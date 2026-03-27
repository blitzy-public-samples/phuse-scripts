# ==============================================================================
#         PROGRAM NAME: ae_v1.R — AE Severity Panel Driver
#
#          DESCRIPTION: Adverse Events Severity Analysis Panel.
#                       Find subject counts per arm for each adverse event.
#                       Find event counts per arm and severity level for each
#                       adverse event. Find top adverse events by relative risk
#                       and odds ratio between each pair of arms.
#                       Creates three output files:
#                         - AE severity counts analysis (Excel workbook)
#                         - Odds ratio analysis (Excel workbook)
#                         - Relative risk analysis (Excel workbook)
#
#      EVALUATION TYPE: Safety
#
#      ORIGINAL SOURCE: contributed/AE/AE_Severity/ae.sas (289 lines)
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                       Andreas Anastassopoulos
#                         (andreas.anastassapoulos@us.ibm.com)
#                DATE:  February 7, 2011
#
#  EXTERNAL R FILES USED:
#    AE_Severity/ae_aggregate.R  -- ae_ab(), ae_cd(), build_severity_lookup()
#    AE_Severity/ae_output.R     -- out_ae(), create_ae_styles()
#    AE_Severity/ae_rror.R       -- rror(), outs(), out_ae_rror()
#    ZZ_Utilities/ae_setup.R     -- setup()
#    ZZ_Utilities/data_checks.R  -- chk_var(), chk_val()
#    ZZ_Utilities/err_output.R   -- error_summary()
#    ZZ_Utilities/sl_gs_output.R -- group_subset_pp()
#    ZZ_Utilities/xml_output.R   -- create_workbook_styles()
#
#  VARIABLES REQUIRED:
#    AE -- AEBODSYS, AEDECOD, USUBJID
#    DM -- ACTARM or ARM, USUBJID
#    EX -- USUBJID
#
#  VARIABLES USED WHEN AVAILABLE:
#    AE -- AESTDTC, AESER, AESEV
#    DM -- RFSTDTC, RFENDTC, ARMCD
#    EX -- EXSTDTC, EXENDTC
#
#            MADE WITH: R >= 4.3.0
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(janitor)
library(openxlsx)
library(cli)

# --- Source Internal Dependencies ---------------------------------------------
# Replaces SAS %include statements (SAS lines 198-204).
# Dependency sourcing: uses a guarded local() block to resolve relative paths
# and only source files if their primary export is not already available.
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  # Sibling macros in AE_Severity/
  sev_deps <- list(
    list(file = "ae_aggregate.R", fn = "ae_ab"),
    list(file = "ae_output.R",    fn = "out_ae"),
    list(file = "ae_rror.R",      fn = "rror")
  )
  # Shared utilities in ZZ_Utilities/
  util_deps <- list(
    list(file = "ae_setup.R",     fn = "setup"),
    list(file = "data_checks.R",  fn = "chk_var"),
    list(file = "err_output.R",   fn = "error_summary"),
    list(file = "sl_gs_output.R", fn = "group_subset_pp"),
    list(file = "xml_output.R",   fn = "create_styles")
  )

  sev_dirs <- c(
    script_dir,
    "contributed/R/AE/AE_Severity",
    file.path(".", "contributed", "R", "AE", "AE_Severity")
  )
  util_dirs <- c(
    file.path(script_dir, "..", "ZZ_Utilities"),
    "contributed/R/AE/ZZ_Utilities",
    file.path(".", "contributed", "R", "AE", "ZZ_Utilities")
  )

  source_dep <- function(dep, search_dirs) {
    if (!exists(dep$fn, mode = "function", inherits = TRUE)) {
      for (d in search_dirs) {
        dep_path <- file.path(d, dep$file)
        if (file.exists(dep_path)) {
          source(dep_path, local = FALSE)
          break
        }
      }
    }
  }

  for (dep in util_deps) source_dep(dep, util_dirs)
  for (dep in sev_deps)  source_dep(dep, sev_dirs)
})


# ==============================================================================
# load_data_file — Load data from XPT, SAS7BDAT, or pass through data frames
# ==============================================================================
load_data_file <- function(x, label = "data") {
  if (is.data.frame(x)) {
    df <- x
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    if (grepl("\\.xpt$", x, ignore.case = TRUE)) {
      df <- haven::read_xpt(x)
    } else if (grepl("\\.sas7bdat$", x, ignore.case = TRUE)) {
      df <- haven::read_sas(x)
    } else {
      cli::cli_abort("Unsupported file format for {label}: {.path {x}}")
    }
  } else {
    cli::cli_abort(
      "{label} must be a data frame or a valid file path to .xpt or .sas7bdat."
    )
  }
  names(df) <- tolower(names(df))
  df
}


# ==============================================================================
# ae_severity_panel — Main AE Severity Panel Orchestrator
# ==============================================================================
# Replaces SAS %params (lines 87-191), %ae (lines 228-287), and the
# top-level execution call %ae (line 289) from contributed/AE/AE_Severity/ae.sas.
#
# Workflow:
#   1. Load AE/DM/EX data (from data frames or file paths)
#   2. Normalize continuity correction parameters (SAS lines 208-222)
#   3. Run ae_setup validation via setup() (SAS line 230)
#   4. De-duplicate: one record per subject × AEBODSYS × AEDECOD (SAS lines 236-247)
#   5. Run Analyses A/B via ae_ab() (SAS lines 249-251)
#   6. Run Analyses C/D via ae_cd() (SAS lines 253-257) if AESEV present
#   7. Check AESER values via chk_val() (SAS lines 259-262) if AESER present
#   8. Generate main AE severity Excel output via out_ae() (SAS line 264)
#   9. Run OR/RR analysis via rror(), outs(), out_ae_rror() (SAS lines 267-269)
#  10. Generate error summaries when setup fails or single arm (SAS lines 271-284)
#
# @param ae           Data frame or path to AE domain.
# @param dm           Data frame or path to DM domain.
# @param ex           Data frame or path to EX domain.
# @param output_path  Character path to output directory.
# @param panel_title  Character panel title for cover sheet (default "AE Severity").
# @param panel_desc   Character panel description.
# @param ndabla       Character NDA/BLA number.
# @param studyid      Character study identifier.
# @param study_lag    Integer window in days after last exposure.
# @param cc           Continuity correction: numeric, "arm", "none", or "0".
# @param vld_sw       Logical or 0/1 — perform data validation on AEs.
# @param sl_datasets  Data frame for Script Launcher datasets metadata.
# @param sl_group     Data frame for Script Launcher grouping metadata.
# @param sl_subset    Data frame for Script Launcher subsetting metadata.
# @param verbose      Logical — emit progress messages.
# @return Invisible named list with output file paths and analysis results.
# ==============================================================================
ae_severity_panel <- function(
    ae,
    dm,
    ex,
    output_path  = ".",
    panel_title  = "AE Severity",
    panel_desc   = "",
    ndabla       = NA_character_,
    studyid      = NA_character_,
    study_lag    = 120L,
    cc           = 0,
    vld_sw       = TRUE,
    sl_datasets  = NULL,
    sl_group     = NULL,
    sl_subset    = NULL,
    verbose      = TRUE
) {
  start_time <- proc.time()

  # ---------------------------------------------------------------------------
  # 1. Load input datasets (SAS lines 116-118)
  # ---------------------------------------------------------------------------
  ae_df <- load_data_file(ae, "AE")
  dm_df <- load_data_file(dm, "DM")
  ex_df <- load_data_file(ex, "EX")

  # ---------------------------------------------------------------------------
  # 2. Normalize continuity correction (SAS lines 208-222)
  # ---------------------------------------------------------------------------
  cc_sw    <- 0L
  cc_whole <- 1L
  cc_val   <- 0

  if (is.character(cc)) {
    cc_clean <- tolower(trimws(cc))
    if (cc_clean == "arm") {
      cc_sw <- 2L; cc_whole <- 0L
    } else if (cc_clean %in% c("none", "0")) {
      cc_sw <- 0L
    } else {
      cc_val <- suppressWarnings(as.numeric(cc_clean))
      if (!is.na(cc_val) && cc_val != 0) {
        cc_sw <- 1L
        cc_whole <- as.integer(cc_val == floor(cc_val))
      }
    }
  } else if (is.numeric(cc)) {
    if (cc != 0) {
      cc_sw <- 1L
      cc_val <- cc
      cc_whole <- as.integer(cc == floor(cc))
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Set up output paths (SAS lines 104-109)
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  aeout1 <- file.path(output_path, "AE Severity.xlsx")
  aeout2 <- file.path(output_path, "Adverse Events Odds Ratio Analysis.xlsx")
  aeout3 <- file.path(output_path, "Adverse Events Relative Risk Analysis.xlsx")
  errout <- file.path(output_path, "AE Severity Error Summary.xlsx")

  # ---------------------------------------------------------------------------
  # 4. Build shared configuration object
  # ---------------------------------------------------------------------------
  # Replaces SAS global macro variables with a named list passed to all
  # component functions. Each function receives the config it needs.
  config <- list(
    panel_title = panel_title,
    panel_desc  = panel_desc,
    ndabla      = ndabla,
    studyid     = studyid,
    study_lag   = as.integer(study_lag),
    cc          = cc,
    cc_sw       = cc_sw,
    cc_whole    = cc_whole,
    cc_val      = cc_val,
    vld_sw      = as.integer(vld_sw),
    aeout1      = aeout1,
    aeout2      = aeout2,
    aeout3      = aeout3,
    errout      = errout,
    output_path = output_path,
    sl_datasets = sl_datasets,
    sl_group    = sl_group,
    sl_subset   = sl_subset
  )

  # ---------------------------------------------------------------------------
  # 5. Run AE setup / gatekeeper validation (SAS line 230: %setup)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_h2("Running AE setup validation")

  setup_result <- tryCatch(
    setup(
      dm        = dm_df,
      ae        = ae_df,
      ex        = ex_df,
      study_lag = study_lag,
      vld_sw    = config$vld_sw,
      sl_subset = sl_subset
    ),
    error = function(e) {
      cli::cli_warn("Setup failed: {conditionMessage(e)}")
      list(setup_success = FALSE, dm_subj_gt0 = FALSE,
           setup_req_var = FALSE)
    }
  )

  # Enrich config with setup results
  config$arm_count   <- setup_result$arm_count   %||% 0L
  config$arm_N       <- setup_result$arm_N       %||% integer(0)
  config$arm_names   <- setup_result$arm_names   %||% character(0)
  config$ae_aeser    <- setup_result$ae_aeser    %||% FALSE
  config$ae_aesev    <- setup_result$ae_aesev    %||% FALSE
  config$arm_count   <- setup_result$arm_count   %||% 0L
  config$rpt_chk_var_req <- setup_result$rpt_chk_var_req %||% NULL

  # Collector for analysis results
  results <- list()

  # ---------------------------------------------------------------------------
  # 6. Main analysis block (SAS lines 232-277: %if &setup_success. ...)
  # ---------------------------------------------------------------------------
  setup_ok <- isTRUE(setup_result$setup_success)

  if (setup_ok) {
    ds_base <- setup_result$ds_base

    # --- De-duplicate to one record per subject × AEBODSYS × AEDECOD ----------
    # SAS lines 236-247: PROC SORT NODUPKEY + DATA step first.usubjid
    ds_base_bysubjpt <- ds_base %>%
      dplyr::arrange(aebodsys, aedecod, usubjid,
                     if ("aeser" %in% names(ds_base)) dplyr::desc(aeser)) %>%
      dplyr::distinct(aebodsys, aedecod, usubjid, .keep_all = TRUE)

    # --- Analyses A & B (SAS lines 249-251) -----------------------------------
    if (verbose) cli::cli_h3("Running Analysis A (AE counts by arm)")
    results$ab_a <- tryCatch(
      ae_ab(ds_base_bysubjpt, config, aeser = "no"),
      error = function(e) {
        cli::cli_warn("Analysis A failed: {conditionMessage(e)}")
        NULL
      }
    )

    if (isTRUE(config$ae_aeser)) {
      if (verbose) cli::cli_h3("Running Analysis B (Serious AE counts by arm)")
      results$ab_b <- tryCatch(
        ae_ab(ds_base_bysubjpt, config, aeser = "yes"),
        error = function(e) {
          cli::cli_warn("Analysis B failed: {conditionMessage(e)}")
          NULL
        }
      )
    }

    # --- Analyses C & D (SAS lines 253-257) -----------------------------------
    if (isTRUE(config$ae_aesev)) {
      # Build severity lookup once (shared by C and D)
      sev_lookup <- tryCatch(
        build_severity_lookup(ds_base),
        error = function(e) {
          cli::cli_warn("Severity lookup build failed: {conditionMessage(e)}")
          NULL
        }
      )
      config$sev_lookup <- sev_lookup

      if (verbose) cli::cli_h3("Running Analysis C (AEs by severity)")
      results$cd_c <- tryCatch(
        ae_cd(ds_base, config, aeser = "no", sev_lookup = sev_lookup),
        error = function(e) {
          cli::cli_warn("Analysis C failed: {conditionMessage(e)}")
          NULL
        }
      )

      if (isTRUE(config$ae_aeser)) {
        if (verbose) cli::cli_h3("Running Analysis D (Serious AEs by severity)")
        results$cd_d <- tryCatch(
          ae_cd(ds_base, config, aeser = "yes", sev_lookup = sev_lookup),
          error = function(e) {
            cli::cli_warn("Analysis D failed: {conditionMessage(e)}")
            NULL
          }
        )
      }
    }

    # --- Validation checks on AESER values (SAS lines 259-262) ----------------
    if (isTRUE(config$ae_aeser) && exists("chk_val", mode = "function")) {
      tryCatch({
        chk_val(ds_base, "aeser", "Y")
        chk_val(ds_base, "aeser", "N")
      }, error = function(e) {
        cli::cli_warn("AESER validation check warning: {conditionMessage(e)}")
      })
    }

    # --- Generate main AE severity output (SAS line 264: %out_ae) -------------
    if (verbose) cli::cli_h3("Generating AE Severity Excel output")
    tryCatch(
      out_ae(config, results),
      error = function(e) {
        cli::cli_warn("Output generation failed: {conditionMessage(e)}")
      }
    )

    # --- OR/RR analysis (SAS lines 267-276) -----------------------------------
    if (config$arm_count > 1L) {
      if (verbose) cli::cli_h3("Running Odds Ratio / Relative Risk analysis")
      tryCatch({
        rror_result <- rror(ds_base_bysubjpt, config)
        if (!is.null(rror_result)) {
          or_data <- rror_result$or_data
          rr_data <- rror_result$rr_data
          rror_cc_ind <- rror_result$cc_ind %||% tibble::tibble()

          outs_result <- outs(or_data, rr_data, rror_cc_ind, config)

          out_ae_rror(
            config     = config,
            rr_out     = outs_result$rr_out,
            or_out     = outs_result$or_out,
            rr_abbrev_out = outs_result$rr_abbrev_out,
            or_abbrev_out = outs_result$or_abbrev_out,
            rror_cc_ind   = rror_cc_ind
          )
        }
      }, error = function(e) {
        cli::cli_warn("OR/RR analysis failed: {conditionMessage(e)}")
      })
    } else {
      # Single arm: generate error summary for OR and RR (SAS lines 270-276)
      if (exists("error_summary", mode = "function")) {
        tryCatch({
          error_summary(
            err_file    = aeout2,
            panel_title = panel_title,
            ndabla      = ndabla,
            studyid     = studyid,
            err_seterr  = FALSE,
            err_desc    = paste0(
              "The Adverse Events Odds Ratio Analysis could not be run ",
              "because this study contained only one arm"
            )
          )
          error_summary(
            err_file    = aeout3,
            panel_title = panel_title,
            ndabla      = ndabla,
            studyid     = studyid,
            err_seterr  = FALSE,
            err_desc    = paste0(
              "The Adverse Events Relative Risk Analysis could not be run ",
              "because this study contained only one arm"
            )
          )
        }, error = function(e) {
          cli::cli_warn("Single-arm error output failed: {conditionMessage(e)}")
        })
      }
    }

    if (verbose) cli::cli_alert_success("AE Severity analysis complete.")

  } else {
    # --- Setup failed: generate error summary (SAS lines 280-284) -------------
    if (verbose) cli::cli_alert_warning("Setup validation failed.")
    if (exists("error_summary", mode = "function")) {
      tryCatch(
        error_summary(
          err_file    = errout,
          panel_title = panel_title,
          ndabla      = ndabla,
          studyid     = studyid,
          err_nosubj  = !isTRUE(setup_result$dm_subj_gt0),
          err_missvar = !isTRUE(setup_result$setup_req_var),
          rpt_chk_var_req = setup_result$rpt_chk_var_req
        ),
        error = function(e) {
          cli::cli_warn("Error summary output failed: {conditionMessage(e)}")
        }
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Timing
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  if (verbose) {
    cli::cli_inform(c("i" = paste0(
      "Running time: ",
      sprintf("%02d:%05.2f", as.integer(elapsed) %/% 60, elapsed %% 60)
    )))
  }

  invisible(list(
    output_files = list(aeout1 = aeout1, aeout2 = aeout2,
                        aeout3 = aeout3, errout = errout),
    config       = config,
    setup_result = setup_result,
    results      = results
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. The setup() function from ae_setup.R returns a named list
#      with elements: setup_success, ds_base, arm_count, arm_N,
#      arm_names, ae_aeser, ae_aesev, dm_subj_gt0, setup_req_var,
#      rpt_chk_var_req.
#   2. ae_ab() accepts (ds_base_bysubjpt, config, aeser) and
#      returns a tibble with analysis results.
#   3. ae_cd() accepts (ds_base, config, aeser, sev_lookup) and
#      returns a tibble with severity-stratified results.
#   4. out_ae() accepts (config, results) where results is a named
#      list with ab_a, ab_b, cd_c, cd_d elements.
#   5. rror(), outs(), out_ae_rror() follow the signatures defined
#      in ae_rror.R.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. De-duplication ordering: SAS PROC SORT uses an internal merge
#      sort (stable); R dplyr::arrange() also uses a stable radix sort.
#      Results should be identical when sort keys are the same.
#   2. AESER descending sort for tie-breaking: SAS sorts 'Y' before
#      'N' via descending; R dplyr::desc(aeser) produces the same.
#   3. Fisher's exact p-values in OR/RR analysis may differ at ~1e-5
#      level between SAS PROC FREQ and R fisher.test().
#
# NO DIRECT R EQUIVALENT:
#   1. SAS "options missing=''" → R NA values displayed as blank via
#      explicit NA-to-empty-string conversion in output functions.
#   2. SAS "options minoperator" → R has native %in% operator.
#   3. SAS PROC DATASETS KILL → not needed; R garbage collects.
#   4. SAS x command for file copy (OR/RR templates) → openxlsx
#      generates workbooks natively without template copying.
#
# PACKAGE SELECTION RATIONALE:
#   haven   — SAS data I/O (read_xpt, read_sas)
#   dplyr   — Data manipulation replacing SAS DATA steps and PROC SQL
#   tidyr   — Pivoting and reshaping
#   purrr   — Functional iteration for arm-wise operations
#   forcats — Factor ordering for severity/arm display
#   janitor — SAS-compatible rounding via round_half_up()
#   openxlsx — Excel output replacing SpreadsheetML XML
#   cli     — User-facing informative messages
#
# OPEN QUESTIONS:
#   1. The SAS code copies Excel template files (AE_OR_Template.xls,
#      AE_RR_Template.xls) via shell commands. The R code generates
#      workbooks from scratch via openxlsx, making templates unnecessary.
#      Confirm that template-free generation is acceptable.
#   2. The SAS continuity correction "arm" mode adds the reciprocal
#      of the opposite arm count; verify this matches the study SAP.
# ============================================================
