# ============================================================================
# PROGRAM NAME: MedDRA at a Glance Panel (R Migration)
#
# DESCRIPTION: Find subject counts per arm for each adverse event
#              at each MedDRA level.
#              Find risk difference, relative risk, and Fisher's
#              exact test p-value for each pair of arms.
#              Creates an Excel workbook output file which allows
#              users to compare arms and highlight terms with
#              statistics above user-set thresholds.
#
# EVALUATION TYPE: Safety
#
# ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
# ORIGINAL DATE: February 15, 2011
# MIGRATED TO R: 2026
#
# EXTERNAL R FILES USED:
#   tested/R/utilities/ae_setup.R      -- Merges AE, DM, and EX
#   tested/R/macros/ae_meddra.R        -- Does the analysis
#   tested/R/macros/ae_meddra_output.R -- Creates the output
#   tested/R/utilities/xml_output.R    -- openxlsx formatting helpers
#   tested/R/utilities/data_checks.R   -- Generic variable checks
#   tested/R/utilities/sl_gs_output.R  -- Script Launcher settings output
#   tested/R/utilities/err_output.R    -- Error output when missing vars
#
# PARAMETERS REQUIRED:
#   ae           -- AE domain data frame
#   dm           -- DM domain data frame
#   ex           -- EX domain data frame
#   meddra_data  -- MedDRA hierarchy data frame (optional)
#   dme_data     -- Designated Medical Events list data frame (optional)
#   output_file  -- path for MedDRA output Excel workbook
#   err_file     -- path for error summary workbook
#   meddra_ver   -- MedDRA version string
#   study_lag    -- window in days after last exposure for AE inclusion
#   cc           -- continuity correction value (numeric, "arm", or "none")
#
# VARIABLES REQUIRED:
#   AE -- AEBODSYS, AEDECOD, USUBJID
#   DM -- ACTARM or ARM, USUBJID
#   EX -- USUBJID
#
# VARIABLES USED WHEN AVAILABLE:
#   AE -- AESTDTC
#   DM -- RFSTDTC, RFENDTC, ARMCD
#   EX -- EXSTDTC, EXENDTC
#
# MADE WITH: R >= 4.3.0
# ============================================================================

# Required packages -----------------------------------------------------------
# These must be loaded before sourcing dependency modules, which rely on
# dplyr, tidyr, purrr, stringr, forcats, janitor, openxlsx, and cli
# being available in the search path.
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(janitor)
library(openxlsx)
library(cli)


# =============================================================================
# source_dependencies — Source all required R modules with parameterised paths
# =============================================================================
# Replaces SAS %include statements (lines 213-218 of the original driver).
# The SAS driver loaded macros from &utilpath. and &saspath. global variables.
# In R, paths are resolved from function arguments or from relative paths
# based on the location of this script file.
#
# @param r_macros_path Character or NULL: directory containing ae_meddra.R,
#        ae_meddra_output.R. If NULL, derived from this file's location.
# @param r_utils_path Character or NULL: directory containing ae_setup.R,
#        data_checks.R, err_output.R, sl_gs_output.R, xml_output.R.
#        If NULL, derived from this file's location.
# @return Invisible NULL. Side effect: sources 7 module files.
# =============================================================================
source_dependencies <- function(r_macros_path = NULL, r_utils_path = NULL) {
  # Resolve paths relative to this script's location if not explicitly provided
  this_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = TRUE)),
    error = function(e) NULL
  )

  if (is.null(r_macros_path)) {
    r_macros_path <- if (!is.null(this_dir)) {
      file.path(dirname(this_dir), "macros")
    } else {
      "tested/R/macros"
    }
  }

  if (is.null(r_utils_path)) {
    r_utils_path <- if (!is.null(this_dir)) {
      file.path(dirname(this_dir), "utilities")
    } else {
      "tested/R/utilities"
    }
  }

  # Source order matters: xml_output.R and data_checks.R must be sourced

  # before modules that depend on them (ae_setup.R, ae_meddra_output.R,
  # err_output.R, sl_gs_output.R).
  source_if_needed <- function(path, fn_name) {
    if (!exists(fn_name, mode = "function", inherits = TRUE)) {
      if (file.exists(path)) {
        source(path, local = FALSE)
      } else {
        cli::cli_warn("Dependency file not found: {.path {path}}")
      }
    }
  }

  # Foundation layer
  source_if_needed(file.path(r_utils_path, "xml_output.R"),
                   "create_workbook_styles")
  source_if_needed(file.path(r_utils_path, "data_checks.R"),
                   "chk_var")

  # Utility modules
  source_if_needed(file.path(r_utils_path, "ae_setup.R"),
                   "setup_validation")
  source_if_needed(file.path(r_utils_path, "err_output.R"),
                   "error_summary")
  source_if_needed(file.path(r_utils_path, "sl_gs_output.R"),
                   "group_subset_pp")

  # Analytical macros
  source_if_needed(file.path(r_macros_path, "ae_meddra.R"),
                   "meddra_aggregate")
  source_if_needed(file.path(r_macros_path, "ae_meddra_output.R"),
                   "meddra_out_workbook")

  invisible(NULL)
}


# =============================================================================
# parse_cc_config — Parse continuity correction parameter
# =============================================================================
# Replaces SAS DATA step at lines 220-235 of the original driver.
#
# SAS logic:
#   - anyalpha("&cc.") and cc = 'arm'  -> cc_sw = 2 (reciprocal of opposite arm)
#   - numeric cc, non-zero, non-missing -> cc_sw = 1 (constant added)
#   - otherwise                         -> cc_sw = 0 (no correction)
#   - cc_whole = 1 if cc is a whole number (cc - floor(cc) == 0), else 0
#
# @param cc Numeric, character, or string: the continuity correction value.
#        "arm" activates reciprocal mode; numeric > 0 activates constant mode;
#        0, "none", or "" deactivates CC.
# @return Named list with cc_sw (integer 0/1/2), cc_whole (integer 0/1),
#         cc_value (numeric or NA_real_).
# =============================================================================
parse_cc_config <- function(cc) {
  cc_str <- tolower(trimws(as.character(cc)))

  if (cc_str %in% c("arm")) {
    # Reciprocal of opposite arm count (SAS cc_sw = 2)
    return(list(cc_sw = 2L, cc_whole = 0L, cc_value = NA_real_))
  }

  if (cc_str %in% c("0", "none", "")) {
    # No continuity correction (SAS cc_sw = 0)
    return(list(cc_sw = 0L, cc_whole = 0L, cc_value = 0))
  }

  cc_num <- suppressWarnings(as.numeric(cc_str))

  if (is.na(cc_num)) {
    # Unrecognised string: default to no correction with a warning
    cli::cli_warn(
      "Continuity correction {.val {cc}} not recognised; defaulting to none."
    )
    return(list(cc_sw = 0L, cc_whole = 0L, cc_value = 0))
  }

  if (cc_num == 0) {
    return(list(cc_sw = 0L, cc_whole = 0L, cc_value = 0))
  }

  # Non-zero numeric: constant mode (SAS cc_sw = 1)
  cc_whole_flag <- if (cc_num == floor(cc_num)) 1L else 0L
  list(cc_sw = 1L, cc_whole = cc_whole_flag, cc_value = cc_num)
}


# =============================================================================
# build_cc_description — Build human-readable CC description string
# =============================================================================
# Used for the cover page of the output workbook. Replaces the SAS inline
# description construction.
#
# @param cc_config Named list from parse_cc_config().
# @return Character string describing the CC method applied.
# =============================================================================
build_cc_description <- function(cc_config) {
  switch(
    as.character(cc_config$cc_sw),
    "0" = "No continuity correction applied.",
    "1" = paste0(
      "Continuity correction of ", cc_config$cc_value,
      " applied to all cells when any cell is zero."
    ),
    "2" = paste0(
      "Reciprocal of the opposite arm count applied as ",
      "continuity correction when any cell is zero."
    ),
    "No continuity correction applied."
  )
}


# =============================================================================
# derive_arm_subjcnt — Derive per-arm subject counts from safety population
# =============================================================================
# In SAS, per-arm subject counts are stored in global macro variables
# &arm_1., &arm_2., etc. In R, they are derived from the all_dm_ex tibble
# returned by setup_validation() and passed explicitly to downstream functions.
#
# @param all_dm_ex Tibble: safety population with columns arm_num and usubjid.
# @param arm_count Integer: number of treatment arms.
# @return Named integer vector of per-arm subject counts, ordered by arm_num.
# =============================================================================
derive_arm_subjcnt <- function(all_dm_ex, arm_count) {
  arm_count <- as.integer(max(arm_count, 0L))
  if (arm_count == 0L) {
    return(integer(0))
  }
  if (!is.data.frame(all_dm_ex) || nrow(all_dm_ex) == 0L) {
    return(stats::setNames(rep(0L, arm_count),
                           paste0("arm", seq_len(arm_count))))
  }

  counts <- all_dm_ex %>%
    dplyr::group_by(.data$arm_num) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data$usubjid), .groups = "drop") %>%
    dplyr::arrange(.data$arm_num)

  # Ensure all arm numbers 1..arm_count are represented
  all_arms <- tibble::tibble(arm_num = seq_len(arm_count))
  counts <- dplyr::left_join(all_arms, counts, by = "arm_num") %>%
    dplyr::mutate(n = dplyr::if_else(is.na(.data$n), 0L, as.integer(.data$n)))

  result <- counts$n
  names(result) <- paste0("arm", seq_len(arm_count))
  result
}


# =============================================================================
# run_meddra_panel — Main MedDRA at a Glance Analysis Panel Driver
# =============================================================================
# Replaces the entire SAS driver: %params + %aemed macro (lines 82-654).
# Orchestrates the complete MedDRA safety analysis workflow:
#
#   1. Parse and normalise configuration parameters
#   2. Source dependency R modules (if not already loaded)
#   3. Run gatekeeper validation via setup_validation()
#   4. SUCCESS PATH: 4-level hierarchical MedDRA aggregation (SOC, HLGT, HLT, PT)
#      via meddra_aggregate(), comparison dataset assembly via meddra_cmp(),
#      and Excel workbook output via meddra_out_workbook()
#   5. ERROR PATH: Error summary workbook via error_summary()
#
# @param ae Data frame: AE domain with AEBODSYS, AEDECOD, USUBJID.
# @param dm Data frame: DM domain with ACTARM or ARM, USUBJID.
# @param ex Data frame: EX domain with USUBJID.
# @param output_file Character: full path for the MedDRA output Excel workbook.
# @param err_file Character or NULL: full path for error summary workbook.
#        If NULL, derived from output_file by appending " Error Summary".
# @param meddra_data Data frame or NULL: MedDRA hierarchy dataset.
# @param dme_data Data frame or NULL: Designated Medical Events list.
# @param ndabla Character: NDA/BLA identifier for the study.
# @param studyid Character: study identifier.
# @param panel_title Character: panel title for output headers.
# @param panel_desc Character: panel description for output metadata.
# @param meddra_ver Character: MedDRA version string (e.g., "14.0").
#        If first character is "N", MedDRA hierarchy lookup is skipped.
# @param study_lag Integer: window in days after last exposure for AE inclusion.
# @param cc Numeric or character: continuity correction value.
#        0.5 (default), "arm" (reciprocal), 0/"none" (disabled).
# @param vld_sw Logical: data validation switch for AE filtering.
# @param rd_th Numeric: risk difference threshold for signal highlighting.
# @param rr_th Numeric: relative risk threshold for signal highlighting.
# @param pv_th Numeric or NA: negative log p-value threshold for highlighting.
# @param sl_datasets Data frame or NULL: Script Launcher dataset metadata.
# @param sl_group Data frame or NULL: Script Launcher grouping metadata.
# @param sl_subset Data frame or NULL: Script Launcher subsetting metadata.
# @param r_macros_path Character or NULL: directory for ae_meddra.R, etc.
# @param r_utils_path Character or NULL: directory for ae_setup.R, etc.
#
# @return Invisible named list with:
#   \describe{
#     \item{success}{Logical — TRUE if the panel ran successfully}
#     \item{output_file}{Character — path to the generated workbook}
#     \item{err_file}{Character or NULL — path to the error workbook}
#     \item{arm_count}{Integer — number of treatment arms}
#     \item{arm_names}{Character vector — arm display names}
#     \item{setup_result}{Named list — full setup_validation() result}
#   }
# =============================================================================
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
                             vld_sw        = TRUE,
                             rd_th         = 5,
                             rr_th         = 5,
                             pv_th         = NA_real_,
                             sl_datasets   = NULL,
                             sl_group      = NULL,
                             sl_subset     = NULL,
                             r_macros_path = NULL,
                             r_utils_path  = NULL) {

  # ---------------------------------------------------------------------------
  # 0. Timing (SAS lines 640-651)
  # ---------------------------------------------------------------------------
  start_time <- proc.time()

  # ---------------------------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------------------------
  if (!is.data.frame(ae)) {
    cli::cli_abort("{.arg ae} must be a data frame.")
  }
  if (!is.data.frame(dm)) {
    cli::cli_abort("{.arg dm} must be a data frame.")
  }
  if (!is.data.frame(ex)) {
    cli::cli_abort("{.arg ex} must be a data frame.")
  }
  if (missing(output_file) || !is.character(output_file) ||
      length(output_file) != 1L || nchar(trimws(output_file)) == 0L) {
    cli::cli_abort("{.arg output_file} must be a non-empty character string.")
  }

  # ---------------------------------------------------------------------------
  # 2. Source dependency modules (SAS %include statements, lines 213-218)
  # ---------------------------------------------------------------------------
  source_dependencies(r_macros_path = r_macros_path,
                      r_utils_path  = r_utils_path)

  # ---------------------------------------------------------------------------
  # 3. Parse continuity correction (SAS DATA step, lines 220-235)
  # ---------------------------------------------------------------------------
  cc_config <- parse_cc_config(cc)

  cli::cli_inform(c(
    "i" = "MedDRA at a Glance Panel Configuration:",
    "*" = "Panel: {panel_title}",
    "*" = "NDA/BLA: {ndabla} | Study: {studyid}",
    "*" = "MedDRA Version: {meddra_ver}",
    "*" = "Study Lag: {study_lag} days",
    "*" = "CC: sw={cc_config$cc_sw}, value={cc_config$cc_value}, whole={cc_config$cc_whole}",
    "*" = "Thresholds: RD={rd_th}, RR={rr_th}, PV={pv_th}",
    "*" = "Validation switch: {vld_sw}"
  ))

  # ---------------------------------------------------------------------------
  # 4. MedDRA version flag determination (SAS lines 124-130)
  # ---------------------------------------------------------------------------
  # SAS: %if %upcase(%substr(&ver.,1,1)) = N -> meddra = N, meddra_pct = 0
  meddra_flag <- if (is.null(meddra_ver) ||
                     startsWith(toupper(as.character(meddra_ver)), "N")) {
    FALSE
  } else {
    TRUE
  }

  # ---------------------------------------------------------------------------
  # 5. Run gatekeeper validation — setup_validation()
  #    (SAS line 615: %setup(mdhier=Y, dme=Y))
  # ---------------------------------------------------------------------------
  cli::cli_inform(c("i" = "Running setup validation..."))

  setup_result <- setup_validation(
    ae          = ae,
    dm          = dm,
    ex          = ex,
    mdhier      = meddra_flag,
    dme         = !is.null(dme_data),
    vld_sw      = vld_sw,
    study_lag   = as.integer(study_lag),
    meddra_data = meddra_data,
    dme_data    = dme_data
  )

  # ---------------------------------------------------------------------------
  # 6. Determine success/error path
  #    (SAS line 617: %if &setup_success. and &meddra_pct. > 0)
  # ---------------------------------------------------------------------------
  meddra_pct <- if (!is.null(setup_result$meddra_pct)) {
    setup_result$meddra_pct
  } else {
    0
  }

  on_success_path <- isTRUE(setup_result$setup_success) && meddra_pct > 0

  if (on_success_path) {
    # =========================================================================
    # SUCCESS PATH — MedDRA hierarchical aggregation and output
    # =========================================================================
    cli::cli_inform(c("v" = "Setup validation passed. Running MedDRA aggregation..."))

    # Extract setup results
    # ds_base is the MedDRA-matched base dataset (SAS: ds_base_meddra)
    ds_base_meddra <- setup_result$ds_base
    arm_count      <- setup_result$arm_count
    arm_names      <- setup_result$arm_names

    # Derive per-arm subject counts from the safety population
    # SAS: &arm_1., &arm_2., ... global macro variables
    arm_subjcnt <- derive_arm_subjcnt(setup_result$all_dm_ex, arm_count)

    # -----------------------------------------------------------------------
    # Level 1: SOC only
    # SAS line 619: %meddra(ds_base_meddra, meddra_1, soc_name)
    # -----------------------------------------------------------------------
    meddra_1 <- meddra_aggregate(
      ds_base     = ds_base_meddra,
      by_vars     = c("soc_name"),
      arm_count   = arm_count,
      arm_names   = arm_names,
      arm_subjcnt = arm_subjcnt,
      cc_sw       = cc_config$cc_sw,
      cc_whole    = cc_config$cc_whole,
      cc_value    = cc_config$cc_value,
      dme_flag    = FALSE
    )

    # -----------------------------------------------------------------------
    # Level 2: SOC + HLGT
    # SAS line 620: %meddra(ds_base_meddra, meddra_2, soc_name, hlgt_name)
    # -----------------------------------------------------------------------
    meddra_2 <- meddra_aggregate(
      ds_base     = ds_base_meddra,
      by_vars     = c("soc_name", "hlgt_name"),
      arm_count   = arm_count,
      arm_names   = arm_names,
      arm_subjcnt = arm_subjcnt,
      cc_sw       = cc_config$cc_sw,
      cc_whole    = cc_config$cc_whole,
      cc_value    = cc_config$cc_value,
      dme_flag    = FALSE
    )

    # -----------------------------------------------------------------------
    # Level 3: SOC + HLGT + HLT
    # SAS line 621: %meddra(ds_base_meddra, meddra_3, soc_name, hlgt_name,
    #                        hlt_name)
    # -----------------------------------------------------------------------
    meddra_3 <- meddra_aggregate(
      ds_base     = ds_base_meddra,
      by_vars     = c("soc_name", "hlgt_name", "hlt_name"),
      arm_count   = arm_count,
      arm_names   = arm_names,
      arm_subjcnt = arm_subjcnt,
      cc_sw       = cc_config$cc_sw,
      cc_whole    = cc_config$cc_whole,
      cc_value    = cc_config$cc_value,
      dme_flag    = FALSE
    )

    # -----------------------------------------------------------------------
    # Level 4: SOC + HLGT + HLT + PT (includes DME flag at PT level)
    # SAS line 622: %meddra(ds_base_meddra, meddra_4, soc_name, hlgt_name,
    #                        hlt_name, pt_name)
    # -----------------------------------------------------------------------
    meddra_4 <- meddra_aggregate(
      ds_base     = ds_base_meddra,
      by_vars     = c("soc_name", "hlgt_name", "hlt_name", "pt_name"),
      arm_count   = arm_count,
      arm_names   = arm_names,
      arm_subjcnt = arm_subjcnt,
      cc_sw       = cc_config$cc_sw,
      cc_whole    = cc_config$cc_whole,
      cc_value    = cc_config$cc_value,
      dme_flag    = !is.null(dme_data)
    )

    # -----------------------------------------------------------------------
    # Assemble comparison dataset
    # SAS line 624: %meddra_cmp
    # -----------------------------------------------------------------------
    cmp_result <- meddra_cmp(
      meddra_1  = meddra_1,
      meddra_2  = meddra_2,
      meddra_3  = meddra_3,
      meddra_4  = meddra_4,
      arm_count = arm_count,
      cc_sw     = cc_config$cc_sw
    )

    # -----------------------------------------------------------------------
    # Script Launcher grouping/subsetting preprocessing
    # (SAS: implicit call before %out_med)
    # -----------------------------------------------------------------------
    pp_result <- if (!is.null(sl_group) || !is.null(sl_subset)) {
      group_subset_pp(
        sl_group    = sl_group,
        sl_subset   = sl_subset,
        sl_datasets = sl_datasets
      )
    } else {
      NULL
    }

    # -----------------------------------------------------------------------
    # Build CC description for the cover page
    # -----------------------------------------------------------------------
    cc_desc <- build_cc_description(cc_config)

    # -----------------------------------------------------------------------
    # Generate Excel workbook output
    # SAS line 626: %out_med
    # -----------------------------------------------------------------------
    meddra_out_workbook(
      output_file        = output_file,
      meddra_cmp_data    = cmp_result$meddra_cmp_output,
      meddra_cmp_hidden_data = cmp_result$meddra_cmp_data,
      rpt_dm             = setup_result$rpt_dm,
      rpt_err            = setup_result$rpt_err,
      rpt_err_term       = setup_result$rpt_err_term,
      rpt_meddra         = setup_result$rpt_meddra,
      rpt_meddra_term    = setup_result$rpt_meddra_term,
      ndabla             = ndabla,
      studyid            = studyid,
      arm_count          = arm_count,
      arm_names          = arm_names,
      arm_subjcnt        = arm_subjcnt,
      meddra_ver         = meddra_ver,
      rd_th              = rd_th,
      rr_th              = rr_th,
      pv_th              = pv_th,
      cc_sw              = cc_config$cc_sw,
      cc_desc            = cc_desc,
      study_lag          = study_lag,
      vld_sw             = vld_sw,
      meddra             = meddra_flag,
      dme_sw             = !is.null(dme_data),
      sl_group_desc      = if (!is.null(pp_result)) {
        pp_result$sl_group_desc
      } else {
        "No grouping"
      },
      sl_subset_desc     = if (!is.null(pp_result)) {
        pp_result$sl_subset_desc
      } else {
        "No subsetting"
      },
      pp_result          = pp_result
    )

    cli::cli_alert_success(
      "MedDRA at a Glance workbook written to: {output_file}"
    )

  } else {
    # =========================================================================
    # ERROR PATH — Generate error summary workbook
    # SAS lines 629-635
    # =========================================================================
    cli::cli_inform(c("!" = "Setup validation failed or no MedDRA matches. Generating error summary..."))

    # Derive error file path if not provided
    if (is.null(err_file)) {
      err_file <- sub("\\.[^.]+$", " Error Summary.xlsx", output_file)
    }

    # Build descriptive error message for zero MedDRA matches
    # SAS: ifc(&meddra_pct.=0, %str(There were zero adverse events with
    #           matching MedDRA descriptions.), )
    err_desc <- if (!is.null(setup_result$meddra_pct) &&
                    isTRUE(setup_result$meddra_pct == 0)) {
      "There were zero adverse events with matching MedDRA descriptions."
    } else {
      ""
    }

    # Call error_summary() — replaces SAS %error_summary macro call
    # SAS: err_nosubj=%sysfunc(ifc(&dm_subj_gt0.,0,1))
    # SAS: err_missvar=%sysfunc(ifc(&setup_req_var.,0,1))
    error_summary(
      err_file        = err_file,
      panel_title     = panel_title,
      ndabla          = ndabla,
      studyid         = studyid,
      err_nosubj      = !isTRUE(setup_result$dm_subj_gt0),
      err_missvar     = !isTRUE(setup_result$setup_req_var),
      err_seterr      = TRUE,
      err_desc        = err_desc,
      rpt_chk_var_req = setup_result$rpt_chk_var_req,
      sl_subset       = sl_subset,
      sl_subset_desc  = "",
      panel_desc      = panel_desc
    )

    cli::cli_alert_warning(
      "MedDRA panel encountered errors. Error summary written to: {err_file}"
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Timing report (SAS lines 647-651)
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  cli::cli_inform(c(
    "i" = "Running time: {round(elapsed, 1)} seconds"
  ))

  # ---------------------------------------------------------------------------
  # 8. Return value — structured result for programmatic access
  # ---------------------------------------------------------------------------
  invisible(list(
    success      = on_success_path,
    output_file  = output_file,
    err_file     = err_file,
    arm_count    = setup_result$arm_count,
    arm_names    = setup_result$arm_names,
    setup_result = setup_result
  ))
}


# =============================================================================
# load_and_run_meddra_panel — Convenience wrapper with data loading
# =============================================================================
# Provides a simplified entry point that loads CDISC datasets from XPT files
# and calls run_meddra_panel(). Replaces the SAS "LOCAL" run mode from the
# %params macro (lines 84-162) where libname and DATA step set operations
# loaded datasets from the file system.
#
# @param study_path Character: directory containing ae.xpt, dm.xpt, ex.xpt.
# @param output_path Character: directory for output workbooks.
# @param meddra_path Character or NULL: directory containing MedDRA hierarchy
#        dataset (mdhier*.xpt or mdhier*.sas7bdat). NULL skips MedDRA lookup.
# @param dme_path Character or NULL: directory containing DME list dataset.
# @param ndabla Character: NDA/BLA identifier.
# @param studyid Character: study identifier.
# @param meddra_ver Character: MedDRA version string.
# @param study_lag Integer: days after last exposure for AE inclusion.
# @param cc Numeric or character: continuity correction value.
# @param vld_sw Logical: data validation switch.
# @param rd_th Numeric: risk difference threshold.
# @param rr_th Numeric: relative risk threshold.
# @param pv_th Numeric or NA: negative log p-value threshold.
#
# @return Invisible named list from run_meddra_panel().
# =============================================================================
load_and_run_meddra_panel <- function(study_path,
                                      output_path,
                                      meddra_path = NULL,
                                      dme_path    = NULL,
                                      ndabla      = "",
                                      studyid     = "",
                                      meddra_ver  = "14.0",
                                      study_lag   = 30,
                                      cc          = 0.5,
                                      vld_sw      = TRUE,
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
  # Load AE, DM, EX from XPT files (SAS libname + DATA step, lines 106-111)
  # ---------------------------------------------------------------------------
  ae_path <- file.path(study_path, "ae.xpt")
  dm_path <- file.path(study_path, "dm.xpt")
  ex_path <- file.path(study_path, "ex.xpt")

  if (!file.exists(ae_path)) {
    cli::cli_abort("AE dataset not found: {.path {ae_path}}")
  }
  if (!file.exists(dm_path)) {
    cli::cli_abort("DM dataset not found: {.path {dm_path}}")
  }
  if (!file.exists(ex_path)) {
    cli::cli_abort("EX dataset not found: {.path {ex_path}}")
  }

  cli::cli_inform(c("i" = "Loading study datasets from: {study_path}"))
  ae <- haven::read_xpt(ae_path)
  dm <- haven::read_xpt(dm_path)
  ex <- haven::read_xpt(ex_path)

  # ---------------------------------------------------------------------------
  # Load MedDRA hierarchy dataset (SAS libname meddra, line 121)
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
      cli::cli_inform(c("i" = "Loading MedDRA hierarchy: {meddra_file}"))
      meddra_data <- if (grepl("\\.xpt$", meddra_file, ignore.case = TRUE)) {
        haven::read_xpt(meddra_file)
      } else {
        haven::read_xpt(meddra_file)
      }
    } else {
      cli::cli_warn("No MedDRA hierarchy file found in: {.path {meddra_path}}")
    }
  }

  # ---------------------------------------------------------------------------
  # Load DME dataset (SAS libname dme, line 135)
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
      cli::cli_inform(c("i" = "Loading DME list: {dme_file}"))
      dme_data <- if (grepl("\\.xpt$", dme_file, ignore.case = TRUE)) {
        haven::read_xpt(dme_file)
      } else {
        haven::read_xpt(dme_file)
      }
    } else {
      cli::cli_warn("No DME file found in: {.path {dme_path}}")
    }
  }

  # ---------------------------------------------------------------------------
  # Construct output file paths (SAS lines 101-102)
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  output_file <- file.path(output_path, "MedDRA at a Glance Analysis Panel.xlsx")
  err_file    <- file.path(output_path, "MedDRA at a Glance Error Summary.xlsx")

  # ---------------------------------------------------------------------------
  # Call the main driver function
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
    pv_th       = pv_th
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS driver (%params + %meddra + %meddra_cmp + %aemed) -> single R
#      function run_meddra_panel() orchestrating module calls
#    - All SAS %include -> source() with parameterised paths via
#      source_dependencies() helper; checks for already-loaded functions
#    - SAS global macro variables -> function arguments and return values;
#      no global state is modified
#    - SAS libname -> haven::read_xpt() in load_and_run_meddra_panel()
#    - CC trigger: applied only when control arm count (c) equals 0
#      (SAS line 382: the a=0, b=0, d=0 conditions are commented out)
#    - Fisher's exact via fisher.test() two-sided p-value
#    - Negative log p-value uses natural log (log()), NOT log10()
#    - Per-arm subject counts derived from all_dm_ex via
#      derive_arm_subjcnt() since setup_validation() stores them in
#      the safety population tibble rather than as named scalars
#    - ds_base in setup_validation() return IS the MedDRA-matched base
#      dataset (SAS: ds_base_meddra), not a separate field
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - fisher.test() p-values may differ from SAS PROC FREQ at extreme
#      edge cases (both arms zero subjects); SAS returns missing, R
#      returns 1.0 or NA depending on matrix configuration
#    - RD and RR are identical when the same CC is applied, since the
#      formulas are algebraically equivalent
#    - Percentage rounding uses janitor::round_half_up() throughout
#      dependency modules to match SAS round-half-up behaviour
#    - dplyr::arrange() sort stability verified for multi-key sorts;
#      results match SAS PROC SORT stable ordering
#
# NO DIRECT R EQUIVALENT:
#    - SAS %sysfunc(ifc()) -> R ifelse() / dplyr::if_else()
#    - SAS options minoperator/missing='' -> not needed in R
#    - SAS PROC DATASETS kill -> R garbage collection (automatic)
#    - SAS run location detection (%symexist(run_location)) -> single
#      R function with explicit parameterisation replaces dual LOCAL/SL
#    - SAS %ut_saslogcheck -> cli logging and tryCatch error handling
#    - SAS SASHELP.VMACRO introspection -> not applicable in R
#
# PACKAGE SELECTION RATIONALE:
#    - haven: SAS data I/O (AAP mandated, version 2.5.5 pinned)
#    - dplyr: DATA step / PROC SQL replacement (AAP mandated tidyverse)
#    - tidyr: pivoting for contingency tables in meddra_aggregate
#    - purrr: iteration over MedDRA levels and arm pairs
#    - stringr: character function replacement for SAS PROPCASE, etc.
#    - forcats: factor level management for arm/term ordering
#    - janitor: round_half_up() for SAS-compatible rounding (Gate 2)
#    - openxlsx: Excel output replacing SpreadsheetML XML (AAP mandated)
#    - cli: user-facing messages replacing SAS %PUT statements
#
# OPEN QUESTIONS:
#    - Fisher's exact algorithm: confirm R network algorithm matches SAS
#      hypergeometric calculation for weighted frequency tables
#    - CC trigger: confirm c==0 only (commented-out a,b,d checks in
#      SAS line 382 are intentionally disabled)
#    - MedDRA hierarchy filtering by primary_soc_fg: confirm whether
#      primary SOC only or all SOC assignments are included
#    - DME flag: only applied at PT level (level 4) per SAS code;
#      confirm this is the intended behaviour
#    - Output format: .xlsx (openxlsx) replaces .xls (SpreadsheetML);
#      confirm downstream consumers accept .xlsx format
# ============================================================
