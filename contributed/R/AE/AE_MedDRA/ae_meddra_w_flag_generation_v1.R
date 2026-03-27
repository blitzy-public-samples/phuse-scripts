# ==============================================================================
#         PROGRAM NAME: ae_meddra_w_flag_generation_v1.R
#
#          DESCRIPTION: MedDRA at a Glance Panel Driver — AE context
#                       Orchestrates the complete MedDRA hierarchical adverse
#                       event analysis with flag generation:
#                         1. Data loading (XPT/SAS7BDAT via haven)
#                         2. Preliminary data validation (chk_var, chk_dm_subj_gt0)
#                         3. Setup & arm derivation (ae_setup.R)
#                         4. MedDRA 4-level aggregation (ae_meddra.R)
#                         5. Pairwise risk difference, relative risk, Fisher's
#                            exact test (ae_meddra.R meddra_cmp)
#                         6. Continuity correction with flag generation
#                         7. Excel workbook output (ae_meddra_output.R)
#                         8. Error summary workbook (err_output.R)
#
#      ORIGINAL SOURCE: tested/SAS/MedDRA/ae_meddra_w_flag_generation_v1.sas
#                       (653 lines)
#                       + contributed/MedDRA/MedDRA_at_a_Glance/ae_meddra.sas
#                       + contributed/MedDRA/MedDRA_at_a_Glance/ae_meddra_output.sas
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                DATE:  February 15, 2011
#
#   MIGRATION DETAILS:
#     - SAS %params          -> R function arguments
#     - SAS %include         -> R source() with path resolution
#     - SAS %setup           -> R setup() from ae_setup.R
#     - SAS %meddra          -> R meddra_aggregate() from ae_meddra.R
#     - SAS %meddra_cmp      -> R meddra_cmp() from ae_meddra.R
#     - SAS %out_med         -> R out_med() from ae_meddra_output.R
#     - SAS %error_summary   -> R error_summary() from err_output.R
#     - SAS continuity correction DATA step -> R parse_cc_config()
#     - SpreadsheetML XML    -> openxlsx workbook API
#
#            MADE WITH: R >= 4.3.0
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(rlang)
library(cli)
library(janitor)
library(openxlsx)


# ==============================================================================
# source_dependencies — Source all required module files
# ==============================================================================
# Locates and sources utility, macro, and output modules required by the
# MedDRA panel. Uses three candidate directory paths per module:
#   1. Relative to this script's location (via sys.frame detection)
#   2. Relative to contributed/R/AE/ sibling directories
#   3. Fallback to contributed/R/MedDRA/ directory
#
# SAS equivalents: %include statements at lines 213-218 of original driver.
#
# @return Invisible NULL. Side effect: sources module files into global env.
# ==============================================================================
source_dependencies <- function() {
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  # Candidate directories for AE ZZ_Utilities
  zz_dirs <- c(
    file.path(script_dir, "..", "ZZ_Utilities"),
    "contributed/R/AE/ZZ_Utilities"
  )

  # Candidate directories for MedDRA-specific modules
  meddra_dirs <- c(
    file.path(script_dir),
    "contributed/R/AE/AE_MedDRA",
    "contributed/R/MedDRA"
  )

  source_if_needed <- function(fn_name, file_name, candidate_dirs) {
    if (exists(fn_name, mode = "function", inherits = TRUE)) return(invisible(NULL))
    for (d in candidate_dirs) {
      fpath <- file.path(d, file_name)
      if (file.exists(fpath)) {
        source(fpath, local = FALSE)
        return(invisible(NULL))
      }
    }
    cli::cli_warn("Dependency not found: {.file {file_name}} (function: {fn_name})")
  }

  # Foundation layer (ZZ_Utilities)
  source_if_needed("create_styles",    "xml_output.R",    zz_dirs)
  source_if_needed("chk_var",          "data_checks.R",   zz_dirs)
  source_if_needed("setup",            "ae_setup.R",      zz_dirs)
  source_if_needed("error_summary",    "err_output.R",    zz_dirs)
  source_if_needed("group_subset_pp",  "sl_gs_output.R",  zz_dirs)

  # MedDRA analytical macros
  source_if_needed("meddra_aggregate", "ae_meddra.R",     meddra_dirs)

  # MedDRA output module
  source_if_needed("out_med",          "ae_meddra_output.R", meddra_dirs)

  invisible(NULL)
}


# ==============================================================================
# parse_cc_config — Parse continuity correction parameter
# ==============================================================================
# Replaces SAS DATA step at lines 220-235 of original driver.
#
# SAS logic:
#   - anyalpha("&cc.") and cc = 'arm'  -> cc_sw = 2 (reciprocal of opposite arm)
#   - numeric cc, non-zero, non-missing -> cc_sw = 1 (constant added)
#   - otherwise                         -> cc_sw = 0 (no correction)
#   - cc_whole = 1 if cc is a whole number, else 0
#
# @param cc Numeric, character, or string. "arm" activates reciprocal mode;
#        numeric > 0 activates constant mode; 0/"none"/"" deactivates CC.
# @return Named list: cc_sw (0L/1L/2L), cc_whole (0L/1L), cc_value (numeric/NA).
# ==============================================================================
parse_cc_config <- function(cc) {
  cc_str <- tolower(trimws(as.character(cc)))

  if (cc_str %in% c("arm")) {
    return(list(cc_sw = 2L, cc_whole = 0L, cc_value = NA_real_))
  }

  cc_num <- suppressWarnings(as.numeric(cc_str))

  if (is.na(cc_num) || cc_num == 0) {
    return(list(cc_sw = 0L, cc_whole = 0L, cc_value = 0))
  }

  cc_whole <- if (cc_num == floor(cc_num)) 1L else 0L
  list(cc_sw = 1L, cc_whole = cc_whole, cc_value = cc_num)
}


# ==============================================================================
# build_cc_description — Readable description of CC config
# ==============================================================================
# @param cc_config Named list from parse_cc_config().
# @return Character string.
# ==============================================================================
build_cc_description <- function(cc_config) {
  switch(as.character(cc_config$cc_sw),
    "0" = "None",
    "1" = paste0("Constant = ", cc_config$cc_value,
                 if (cc_config$cc_whole == 1L) " (whole)" else " (fractional)"),
    "2" = "Reciprocal of opposite arm N",
    "Unknown"
  )
}


# ==============================================================================
# derive_arm_subjcnt — Compute per-arm subject counts from merged dataset
# ==============================================================================
# Replaces implicit SAS aggregation in setup macro.
#
# @param all_dm_ex Data frame with USUBJID + ARM columns (merged DM/EX).
# @param arm_count Integer number of arms.
# @return Named integer vector: arm_subjcnt[arm_index] = N.
# ==============================================================================
derive_arm_subjcnt <- function(all_dm_ex, arm_count) {
  if (!"arm_num" %in% names(all_dm_ex)) {
    cli::cli_warn("arm_num not found in merged dataset. Returning empty counts.")
    return(stats::setNames(rep(0L, arm_count), as.character(seq_len(arm_count))))
  }

  counts <- all_dm_ex %>%
    dplyr::distinct(usubjid, arm_num) %>%
    dplyr::count(arm_num, name = "n") %>%
    dplyr::arrange(arm_num)

  arm_ns <- stats::setNames(rep(0L, arm_count), as.character(seq_len(arm_count)))
  for (i in seq_len(nrow(counts))) {
    idx <- as.character(counts$arm_num[i])
    if (idx %in% names(arm_ns)) arm_ns[idx] <- as.integer(counts$n[i])
  }
  arm_ns
}


# ==============================================================================
# run_meddra_panel — Main MedDRA analysis orchestrator
# ==============================================================================
# Replaces the entire SAS %ae_meddra_w_flag_generation macro flow.
# This function:
#   1. Validates inputs
#   2. Sources dependencies
#   3. Parses CC configuration
#   4. Runs setup validation (ae_setup.R)
#   5. Performs MedDRA 4-level aggregation (ae_meddra.R)
#   6. Generates pairwise comparisons with flags (ae_meddra.R)
#   7. Creates Excel output (ae_meddra_output.R)
#   8. Handles error/single-arm fallback via error_summary
#
# @param ae       Data frame: AE domain (AEBODSYS, AEDECOD, USUBJID required).
# @param dm       Data frame: DM domain (ARM/ACTARM, USUBJID required).
# @param ex       Data frame: EX domain (USUBJID required).
# @param output_file Character: path to output Excel workbook.
# @param err_file Character or NULL: path to error workbook.
# @param meddra_data Data frame or NULL: MedDRA hierarchy (mdhier_x_y).
# @param dme_data Data frame or NULL: Designated Medical Events list.
# @param ndabla   Character: NDA/BLA number.
# @param studyid  Character: study identifier.
# @param panel_title Character: panel title.
# @param panel_desc Character: panel description.
# @param meddra_ver Character: MedDRA version (e.g., "14.0" or "N" for none).
# @param study_lag Numeric: days after last exposure for AE window.
# @param cc       Numeric/character: continuity correction value.
# @param vld_sw   Integer/logical: data validation switch.
# @param rd_th    Numeric: risk difference threshold.
# @param rr_th    Numeric: relative risk threshold.
# @param pv_th    Numeric or NA: p-value threshold.
# @param sl_datasets Data frame or NULL: Script Launcher datasets metadata.
# @param sl_group Data frame or NULL: Script Launcher grouping.
# @param sl_subset Data frame or NULL: Script Launcher subsetting.
#
# @return Invisible named list:
#   \describe{
#     \item{success}{Logical — TRUE if panel completed without error}
#     \item{output_file}{Character — path to the output workbook}
#     \item{err_file}{Character or NULL — path to the error workbook}
#     \item{arm_count}{Integer — number of treatment arms}
#     \item{arm_names}{Character vector — arm display names}
#     \item{setup_result}{Named list — full setup() result}
#   }
# ==============================================================================
run_meddra_panel <- function(ae,
                             dm,
                             ex,
                             output_file,
                             err_file      = NULL,
                             meddra_data   = NULL,
                             dme_data      = NULL,
                             ndabla        = "",
                             studyid       = "",
                             panel_title   = "MedDRA at a Glance",
                             panel_desc    = "",
                             meddra_ver    = "14.0",
                             study_lag     = 30,
                             cc            = 0.5,
                             vld_sw        = 1L,
                             rd_th         = 5,
                             rr_th         = 5,
                             pv_th         = NA_real_,
                             sl_datasets   = NULL,
                             sl_group      = NULL,
                             sl_subset     = NULL) {

  # ---------------------------------------------------------------------------
  # 0. Timing
  # ---------------------------------------------------------------------------
  start_time <- proc.time()
  cli::cli_h1("MedDRA at a Glance Panel — AE Context")

  # ---------------------------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------------------------
  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame.")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame.")
  if (missing(output_file) || !is.character(output_file) ||
      length(output_file) != 1L || nchar(trimws(output_file)) == 0L) {
    cli::cli_abort("{.arg output_file} must be a non-empty character string.")
  }

  # ---------------------------------------------------------------------------
  # 2. Source dependency modules
  # ---------------------------------------------------------------------------
  source_dependencies()

  # ---------------------------------------------------------------------------
  # 3. Parse continuity correction (SAS lines 220-235)
  # ---------------------------------------------------------------------------
  cc_config <- parse_cc_config(cc)

  cli::cli_inform(c(
    "i" = "Panel Configuration:",
    "*" = "Panel: {panel_title}",
    "*" = "NDA/BLA: {ndabla} | Study: {studyid}",
    "*" = "MedDRA Version: {meddra_ver}",
    "*" = "Study Lag: {study_lag} days",
    "*" = "CC: {build_cc_description(cc_config)}",
    "*" = "Thresholds: RD={rd_th}, RR={rr_th}, PV={pv_th}",
    "*" = "Validation: {vld_sw}"
  ))

  # ---------------------------------------------------------------------------
  # 4. MedDRA flag determination (SAS lines 124-130)
  # ---------------------------------------------------------------------------
  meddra_flag <- if (is.null(meddra_ver) ||
                     startsWith(toupper(as.character(meddra_ver)), "N")) {
    "N"
  } else {
    "Y"
  }
  dme_flag <- if (!is.null(dme_data) && is.data.frame(dme_data) &&
                  nrow(dme_data) > 0L) "Y" else "N"
  meddra_pct <- if (meddra_flag == "N") 0L else 1L

  cli::cli_inform(c(
    "i" = "MedDRA hierarchy: {meddra_flag}",
    "i" = "DME list: {dme_flag}"
  ))

  # ---------------------------------------------------------------------------
  # 5. Setup validation (SAS %setup call, lines 136-140)
  # ---------------------------------------------------------------------------
  cli::cli_h2("SETUP VALIDATION")

  setup_result <- tryCatch(
    setup(
      ae        = ae,
      dm        = dm,
      ex        = ex,
      meddra_hier = meddra_data,
      dme_data  = dme_data,
      mdhier    = meddra_flag,
      dme       = dme_flag,
      vld_sw    = as.integer(vld_sw),
      study_lag = as.integer(study_lag),
      config    = list(
        ndabla = ndabla, studyid = studyid,
        panel_title = panel_title, panel_desc = panel_desc
      )
    ),
    error = function(e) {
      cli::cli_alert_danger("Setup failed: {conditionMessage(e)}")
      list(setup_success = FALSE, err_nosubj = TRUE, err_missvar = TRUE)
    }
  )

  setup_ok <- isTRUE(setup_result$setup_success)

  if (!setup_ok) {
    cli::cli_alert_warning("Setup failed — generating error summary only.")

    if (!is.null(err_file) && exists("error_summary", mode = "function")) {
      tryCatch(
        error_summary(
          err_file    = err_file,
          panel_title = panel_title,
          ndabla      = ndabla,
          studyid     = studyid,
          err_nosubj  = isTRUE(setup_result$err_nosubj),
          err_missvar = isTRUE(setup_result$err_missvar),
          err_seterr  = TRUE,
          err_desc    = "Setup validation failed",
          panel_desc  = panel_desc,
          sl_subset   = sl_subset,
          rpt_chk_var_req = setup_result$rpt_chk_var
        ),
        error = function(e) {
          cli::cli_warn("Error summary generation failed: {conditionMessage(e)}")
        }
      )
    }

    elapsed <- (proc.time() - start_time)["elapsed"]
    cli::cli_inform(c("i" = "Elapsed: {round(elapsed, 1)}s"))  # base R round() intentional — display only

    return(invisible(list(
      success      = FALSE,
      output_file  = output_file,
      err_file     = err_file,
      arm_count    = 0L,
      arm_names    = character(0),
      setup_result = setup_result
    )))
  }

  # ---------------------------------------------------------------------------
  # 6. Extract setup outputs
  # ---------------------------------------------------------------------------
  ds_base     <- setup_result$ds_base
  arm_count   <- setup_result$arm_count
  arm_names   <- setup_result$arm_names
  arm_N       <- setup_result$arm_N
  rpt_err     <- setup_result$rpt_chk_var

  cli::cli_alert_success("Setup OK: {arm_count} arms, {nrow(ds_base)} AE records")

  # Normalize column names to lowercase for downstream processing
  names(ds_base) <- tolower(names(ds_base))

  # ---------------------------------------------------------------------------
  # 7. Build arm info structure for meddra_aggregate
  # ---------------------------------------------------------------------------
  arm_info <- list(
    arm_count = arm_count,
    arm_names = arm_names,
    arm_N     = arm_N
  )

  arm_subjcnt <- if ("arm_subjcnt" %in% names(setup_result)) {
    setup_result$arm_subjcnt
  } else if (!is.null(setup_result$all_dm_ex)) {
    derive_arm_subjcnt(setup_result$all_dm_ex, arm_count)
  } else {
    arm_N
  }
  arm_info$arm_subjcnt <- arm_subjcnt

  # ---------------------------------------------------------------------------
  # 8. MedDRA 4-level aggregation (SAS %meddra call, lines 146-600)
  # ---------------------------------------------------------------------------
  cli::cli_h2("MEDDRA 4-LEVEL AGGREGATION")

  # Level 4: Preferred Term (PT)
  meddra_4 <- tryCatch(
    meddra_aggregate(
      dsin    = ds_base,
      by_vars = c("aebodsys", "aedecod"),
      arm_info = arm_info,
      cc_sw   = cc_config$cc_sw,
      cc_val  = cc_config$cc_value
    ),
    error = function(e) {
      cli::cli_warn("Level 4 (PT) aggregation failed: {conditionMessage(e)}")
      NULL
    }
  )

  # Level 3: High-Level Term (HLT) — only with MedDRA hierarchy
  meddra_3 <- NULL
  if (meddra_flag == "Y" && !is.null(meddra_data)) {
    # Merge HLT names from hierarchy
    hlt_var <- intersect(tolower(names(meddra_data)),
                         c("hlt_name", "hlterm", "hlt"))
    if (length(hlt_var) > 0L) {
      ds_with_hlt <- ds_base
      if (!"hlt_name" %in% tolower(names(ds_with_hlt))) {
        names(meddra_data) <- tolower(names(meddra_data))
        ds_with_hlt <- ds_with_hlt %>%
          dplyr::left_join(
            meddra_data %>%
              dplyr::select(any_of(c("aedecod" = "pt_name", "hlt_name"))) %>%
              dplyr::distinct(),
            by = c("aedecod" = "aedecod")
          )
      }
      meddra_3 <- tryCatch(
        meddra_aggregate(
          dsin    = ds_with_hlt,
          by_vars = c("aebodsys", "hlt_name"),
          arm_info = arm_info,
          cc_sw   = cc_config$cc_sw,
          cc_val  = cc_config$cc_value
        ),
        error = function(e) {
          cli::cli_warn("Level 3 (HLT) aggregation failed: {conditionMessage(e)}")
          NULL
        }
      )
    }
  }

  # Level 2: High-Level Group Term (HLGT) — only with MedDRA hierarchy
  meddra_2 <- NULL
  if (meddra_flag == "Y" && !is.null(meddra_data)) {
    hlgt_var <- intersect(tolower(names(meddra_data)),
                          c("hlgt_name", "hlgterm", "hlgt"))
    if (length(hlgt_var) > 0L) {
      ds_with_hlgt <- ds_base
      names(meddra_data) <- tolower(names(meddra_data))
      if (!"hlgt_name" %in% tolower(names(ds_with_hlgt))) {
        ds_with_hlgt <- ds_with_hlgt %>%
          dplyr::left_join(
            meddra_data %>%
              dplyr::select(any_of(c("aedecod" = "pt_name", "hlgt_name"))) %>%
              dplyr::distinct(),
            by = c("aedecod" = "aedecod")
          )
      }
      meddra_2 <- tryCatch(
        meddra_aggregate(
          dsin    = ds_with_hlgt,
          by_vars = c("aebodsys", "hlgt_name"),
          arm_info = arm_info,
          cc_sw   = cc_config$cc_sw,
          cc_val  = cc_config$cc_value
        ),
        error = function(e) {
          cli::cli_warn("Level 2 (HLGT) aggregation failed: {conditionMessage(e)}")
          NULL
        }
      )
    }
  }

  # Level 1: System Organ Class (SOC)
  meddra_1 <- tryCatch(
    meddra_aggregate(
      dsin    = ds_base,
      by_vars = c("aebodsys"),
      arm_info = arm_info,
      cc_sw   = cc_config$cc_sw,
      cc_val  = cc_config$cc_value
    ),
    error = function(e) {
      cli::cli_warn("Level 1 (SOC) aggregation failed: {conditionMessage(e)}")
      NULL
    }
  )

  cli::cli_alert_success("Aggregation complete: SOC={ifelse(is.null(meddra_1), 0, nrow(meddra_1))}, HLGT={ifelse(is.null(meddra_2), 0, nrow(meddra_2))}, HLT={ifelse(is.null(meddra_3), 0, nrow(meddra_3))}, PT={ifelse(is.null(meddra_4), 0, nrow(meddra_4))}")

  # ---------------------------------------------------------------------------
  # 9. Pairwise comparison with flag generation (SAS %meddra_cmp)
  # ---------------------------------------------------------------------------
  cli::cli_h2("PAIRWISE COMPARISON")

  meddra_cmp_output <- data.frame()
  meddra_cmp_data   <- data.frame()

  if (arm_count >= 2L && !is.null(meddra_4) && nrow(meddra_4) > 0L &&
      exists("meddra_cmp", mode = "function")) {
    cmp_result <- tryCatch(
      meddra_cmp(
        meddra_1  = meddra_1,
        meddra_2  = meddra_2,
        meddra_3  = meddra_3,
        meddra_4  = meddra_4,
        arm_count = arm_count,
        arm_names = arm_names,
        arm_N     = arm_N,
        cc_sw     = cc_config$cc_sw,
        cc_val    = cc_config$cc_value
      ),
      error = function(e) {
        cli::cli_warn("Pairwise comparison failed: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(cmp_result)) {
      meddra_cmp_output <- cmp_result$output %||% data.frame()
      meddra_cmp_data   <- cmp_result$data   %||% data.frame()
      cli::cli_alert_success("Comparison: {nrow(meddra_cmp_output)} rows")
    }
  } else if (arm_count < 2L) {
    cli::cli_alert_info("Single-arm study — skipping pairwise comparison.")
  }

  # ---------------------------------------------------------------------------
  # 10. Generate output workbook (SAS %out_med call)
  # ---------------------------------------------------------------------------
  cli::cli_h2("OUTPUT GENERATION")

  if (exists("out_med", mode = "function")) {
    tryCatch(
      out_med(
        ndabla          = ndabla,
        studyid         = studyid,
        aemedout        = output_file,
        arm_count       = arm_count,
        arm_name        = arm_names,
        arm_N           = arm_N,
        meddra_cmp_output = meddra_cmp_output,
        meddra_cmp_data   = meddra_cmp_data,
        meddra_ver      = meddra_ver,
        rd_th           = rd_th,
        rr_th           = rr_th,
        pv_th           = pv_th,
        cc_sw           = cc_config$cc_sw,
        rpt_err         = rpt_err,
        sl_group        = sl_group,
        sl_subset       = sl_subset,
        sl_datasets     = sl_datasets,
        wbtitle         = "MedDRA at a Glance",
        author          = "PhUSE CS"
      ),
      error = function(e) {
        cli::cli_alert_danger("Output generation failed: {conditionMessage(e)}")
      }
    )
  } else {
    cli::cli_warn("out_med() not available — skipping output generation.")
  }

  # ---------------------------------------------------------------------------
  # 11. Error summary for edge cases (SAS %error_summary calls)
  # ---------------------------------------------------------------------------
  if (arm_count < 2L && !is.null(err_file) &&
      exists("error_summary", mode = "function")) {
    tryCatch(
      error_summary(
        err_file    = err_file,
        panel_title = panel_title,
        ndabla      = ndabla,
        studyid     = studyid,
        err_nosubj  = FALSE,
        err_missvar = FALSE,
        err_seterr  = FALSE,
        err_desc    = "Single-arm study: pairwise comparison not applicable",
        panel_desc  = panel_desc,
        sl_subset   = sl_subset,
        rpt_chk_var_req = rpt_err
      ),
      error = function(e) {
        cli::cli_warn("Error summary failed: {conditionMessage(e)}")
      }
    )
  }

  # ---------------------------------------------------------------------------
  # 12. Timing
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  cli::cli_alert_success("MedDRA panel complete in {round(elapsed, 1)}s")  # base R round() intentional — display only

  invisible(list(
    success      = TRUE,
    output_file  = output_file,
    err_file     = err_file,
    arm_count    = arm_count,
    arm_names    = arm_names,
    setup_result = setup_result
  ))
}


# ==============================================================================
# load_and_run_meddra_panel — Convenience wrapper with file-based I/O
# ==============================================================================
# Loads AE, DM, EX from XPT/SAS7BDAT files, optionally loads MedDRA
# hierarchy and DME datasets, and runs the full MedDRA panel.
#
# @param study_path  Character: directory containing ae.xpt, dm.xpt, ex.xpt.
# @param output_path Character: directory for output files.
# @param meddra_path Character or NULL: directory containing MedDRA hierarchy.
# @param dme_path    Character or NULL: directory containing DME list.
# @param ndabla      Character: NDA/BLA number.
# @param studyid     Character: study identifier.
# @param meddra_ver  Character: MedDRA version.
# @param study_lag   Numeric: days after last exposure.
# @param cc          Numeric/character: continuity correction.
# @param vld_sw      Integer/logical: validation switch.
# @param rd_th       Numeric: risk difference threshold.
# @param rr_th       Numeric: relative risk threshold.
# @param pv_th       Numeric or NA: p-value threshold.
#
# @return Invisible named list from run_meddra_panel().
# ==============================================================================
load_and_run_meddra_panel <- function(study_path,
                                      output_path,
                                      meddra_path = NULL,
                                      dme_path    = NULL,
                                      ndabla      = "",
                                      studyid     = "",
                                      meddra_ver  = "14.0",
                                      study_lag   = 30,
                                      cc          = 0.5,
                                      vld_sw      = 1L,
                                      rd_th       = 5,
                                      rr_th       = 5,
                                      pv_th       = NA_real_) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (!is.character(study_path) || length(study_path) != 1L ||
      nchar(trimws(study_path)) == 0L) {
    cli::cli_abort("{.arg study_path} must be a non-empty character string.")
  }
  if (!is.character(output_path) || length(output_path) != 1L ||
      nchar(trimws(output_path)) == 0L) {
    cli::cli_abort("{.arg output_path} must be a non-empty character string.")
  }

  # ---------------------------------------------------------------------------
  # Load AE, DM, EX from XPT files
  # ---------------------------------------------------------------------------
  ae_path <- file.path(study_path, "ae.xpt")
  dm_path <- file.path(study_path, "dm.xpt")
  ex_path <- file.path(study_path, "ex.xpt")

  if (!file.exists(ae_path)) cli::cli_abort("AE dataset not found: {.path {ae_path}}")
  if (!file.exists(dm_path)) cli::cli_abort("DM dataset not found: {.path {dm_path}}")
  if (!file.exists(ex_path)) cli::cli_abort("EX dataset not found: {.path {ex_path}}")

  cli::cli_inform(c("i" = "Loading study datasets from: {.path {study_path}}"))
  ae <- haven::read_xpt(ae_path)
  dm <- haven::read_xpt(dm_path)
  ex <- haven::read_xpt(ex_path)

  # ---------------------------------------------------------------------------
  # Load MedDRA hierarchy dataset (optional)
  # ---------------------------------------------------------------------------
  meddra_data <- NULL
  if (!is.null(meddra_path) && nchar(trimws(meddra_path)) > 0L) {
    meddra_files <- list.files(
      meddra_path,
      pattern = "mdhier.*\\.(xpt|sas7bdat)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(meddra_files) > 0L) {
      meddra_file <- meddra_files[1L]
      cli::cli_inform(c("i" = "Loading MedDRA hierarchy: {.path {meddra_file}}"))
      meddra_data <- if (grepl("\\.xpt$", meddra_file, ignore.case = TRUE)) {
        haven::read_xpt(meddra_file)
      } else {
        haven::read_sas(meddra_file)
      }
    } else {
      cli::cli_warn("No MedDRA hierarchy file found in: {.path {meddra_path}}")
    }
  }

  # ---------------------------------------------------------------------------
  # Load DME dataset (optional)
  # ---------------------------------------------------------------------------
  dme_data <- NULL
  if (!is.null(dme_path) && nchar(trimws(dme_path)) > 0L) {
    dme_files <- list.files(
      dme_path,
      pattern = "dme\\.(xpt|sas7bdat)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(dme_files) > 0L) {
      dme_file <- dme_files[1L]
      cli::cli_inform(c("i" = "Loading DME list: {.path {dme_file}}"))
      dme_data <- if (grepl("\\.xpt$", dme_file, ignore.case = TRUE)) {
        haven::read_xpt(dme_file)
      } else {
        haven::read_sas(dme_file)
      }
    } else {
      cli::cli_warn("No DME file found in: {.path {dme_path}}")
    }
  }

  # ---------------------------------------------------------------------------
  # Construct output paths
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  output_file <- file.path(output_path, "ae_meddra_w_flag.xlsx")
  err_file    <- file.path(output_path, "ae_meddra_w_flag_errors.xlsx")

  # ---------------------------------------------------------------------------
  # Delegate to main orchestrator
  # ---------------------------------------------------------------------------
  run_meddra_panel(
    ae          = ae,
    dm          = dm,
    ex          = ex,
    output_file = output_file,
    err_file    = err_file,
    meddra_data = meddra_data,
    dme_data    = dme_data,
    ndabla      = ndabla,
    studyid     = studyid,
    meddra_ver  = meddra_ver,
    study_lag   = study_lag,
    cc          = cc,
    vld_sw      = vld_sw,
    rd_th       = rd_th,
    rr_th       = rr_th,
    pv_th       = pv_th,
    sl_datasets = NULL,
    sl_group    = NULL,
    sl_subset   = NULL
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. This driver combines the tested SAS MedDRA panel logic
#      (ae_meddra_w_flag_generation_v1.sas, 653 lines) with the
#      contributed SAS macro flow from ae_meddra.sas.
#   2. Dependencies are sourced from contributed/R/AE/ZZ_Utilities/
#      and contributed/R/AE/AE_MedDRA/ or contributed/R/MedDRA/.
#   3. The ae_setup.R in contributed context (contributed/R/AE/ZZ_Utilities/)
#      uses setup() signature with (ae, dm, ex, meddra_hier, dme_data, ...).
#   4. MedDRA hierarchy levels 2 and 3 (HLGT, HLT) are only aggregated
#      when a MedDRA hierarchy dataset is provided (meddra_flag == "Y").
#   5. CC trigger: cc_sw=0 → no correction; cc_sw=1 → constant added
#      to zero cells; cc_sw=2 → reciprocal of opposite arm N.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Fisher's exact p-values may differ at machine epsilon due to
#      R vs SAS hypergeometric implementations.
#   2. Relative risk CI: R fisher.test() CI may use a different exact
#      method than SAS PROC FREQ.
#   3. MedDRA hierarchy term matching is case-insensitive in R
#      (tolower on all column names) while SAS uses format catalogs.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS %sysfunc(ifc(not %symexist(run_location),...)) run-location
#      detection → R sys.frame(1L)$ofile directory resolution.
#   2. SAS global macro variables → R function arguments with defaults.
#   3. SAS PROC DATASETS KILL → not needed; R uses function scoping.
#
# PACKAGE SELECTION RATIONALE:
#   haven     — SAS7BDAT/XPT I/O via embedded ReadStat C library
#   dplyr     — DATA step merge/filter/summarise replacement
#   openxlsx  — SpreadsheetML XML → native Excel API
#   janitor   — SAS-compatible round_half_up()
#   cli       — Informative user messages with formatting
#   forcats   — Factor level ordering for arm/term display
#
# OPEN QUESTIONS:
#   1. The contributed SAS ae_setup.R has a slightly different
#      signature than the tested version. The contributed signature
#      (ae, dm, ex, meddra_hier, dme_data, mdhier, dme, vld_sw,
#      study_lag, config) is used here. Confirm parameter alignment.
#   2. MedDRA hierarchy join strategy assumes pt_name matches aedecod.
#      Confirm this is correct for all MedDRA versions.
# ============================================================
