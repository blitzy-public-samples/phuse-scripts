# =============================================================================
# AE Severity Panel — Main Orchestrator
# =============================================================================
# Program:      ae.R
# Description:  Main entry point for AE Severity analysis panel. Configures the
#               environment, loads CDISC datasets, orchestrates setup/validation,
#               executes Analyses A/B/C/D, generates Excel workbooks, and
#               triggers odds ratio / relative risk (OR/RR) analysis.
# Author:       David Kretch / Andreas Anastassopoulos (original SAS)
#               Migrated to R by Blitzy
# Date:         07FEB2011 (original); migrated Mar 2026
# Evaluation:   Safety
# Source:       contributed/AE/AE_Severity/ae.sas (289 lines)
# =============================================================================
# Functional mapping:
#   SAS %params macro (lines  87-191) -> params()
#   SAS DATA _NULL_ CC (lines 207-222) -> parse_continuity_correction()
#   SAS %ae macro     (lines 228-287) -> ae()
#   Top-level call                     -> run_ae_severity()
# =============================================================================

# ---------------------------------------------------------------------------
# Library Loading
# ---------------------------------------------------------------------------
# Core tidyverse and I/O
library(haven)       # SAS XPT / SAS7BDAT data I/O (replaces libname)
library(dplyr)       # Data manipulation (replaces DATA steps / PROC SQL)
library(tidyr)       # Reshaping — pivot_wider/longer (used in sourced modules)
library(stringr)     # String operations (replaces SAS character functions)
library(forcats)     # Factor level management (replaces SAS format ordering)

# Rounding and output
library(janitor)     # round_half_up() for SAS-compatible rounding
library(openxlsx)    # Excel workbook generation (replaces SpreadsheetML/PCFILES)

# Logging
library(cli)         # Formatted user-facing messages (replaces %put)


# =============================================================================
# source_module — Helper to source dependency modules with robust path handling
# =============================================================================
# Resolves and sources a single R script, searching the provided base path
# first and falling back to the script directory.
#
# @param file_name  Character. Base file name (e.g., "ae_aggregate.R").
# @param base_path  Character. Directory to search first.
# @return Invisible NULL. Side-effect: the file is sourced into the parent
#         environment.
# =============================================================================
source_module <- function(file_name, base_path) {
  candidate <- file.path(base_path, file_name)
  if (file.exists(candidate)) {
    source(candidate, local = FALSE)
    return(invisible(NULL))
  }
  # Fall back to the directory where this script resides
  script_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) "."
  )
  fallback <- file.path(script_dir, file_name)
  if (file.exists(fallback)) {
    source(fallback, local = FALSE)
    return(invisible(NULL))
  }
  stop(
    cli::format_error(
      c("Cannot find module {.file {file_name}}.",
        "i" = "Searched {.path {base_path}} and {.path {script_dir}}.")
    ),
    call. = FALSE
  )
}


# =============================================================================
# source_dependencies — Source all required utility and analysis modules
# =============================================================================
# Mirrors SAS %include statements at lines 198-204 of ae.sas. All functions
# defined in the sourced files become available in the calling environment.
#
# @param saspath  Character. Path to analysis-specific macros directory.
# @param utilpath Character. Path to cross-panel utility directory.
# @return Invisible NULL.
# =============================================================================
source_dependencies <- function(saspath, utilpath) {
  # Order matches SAS %include sequence:
  #   198: ae_setup.sas      (utilpath)
  #   199: ae_aggregate.sas  (saspath)
  #   200: ae_output.sas     (saspath)
  #   201: data_checks.sas   (utilpath)
  #   202: err_output.sas    (utilpath)
  #   203: sl_gs_output.sas  (utilpath)
  #   204: xml_output.sas    (utilpath)
  # ae_rror.sas is sourced in SAS only conditionally inside %ae — in R we
  # source it unconditionally (it only defines functions; execution is gated).

  source_module("ae_setup.R",     utilpath)
  source_module("ae_aggregate.R", saspath)
  source_module("ae_output.R",    saspath)
  source_module("data_checks.R",  utilpath)
  source_module("err_output.R",   utilpath)
  source_module("sl_gs_output.R", utilpath)
  source_module("xml_output.R",   utilpath)
  source_module("ae_rror.R",      saspath)

  invisible(NULL)
}


# =============================================================================
# params — Initialise Panel Configuration
# =============================================================================
# Migrated from SAS %params macro (ae.sas lines 87-191).
#
# Builds a named list (config object) that replaces the SAS global macro
# variable store. Handles two execution modes:
#   LOCAL          — standalone execution with explicit paths and data loading.
#   SCRIPT_LAUNCHER — invoked from Script Launcher with pre-loaded datasets.
#
# @param run_location  Character. "LOCAL" or "SCRIPT_LAUNCHER".
# @param saspath       Character. Path to analysis macro directory.
# @param utilpath      Character. Path to cross-panel utility directory.
# @param studypath     Character. Path to CDISC study datasets.
# @param outpath       Character. Path for output files.
# @param templatepath  Character. Path to template workbooks.
# @param ndabla        Character. NDA/BLA identifier.
# @param studyid       Character. Study identifier.
# @param study_lag     Numeric or character. Analysis lag in days (default 120).
# @param cc            Character. Continuity correction value.
# @param cont_corr     Character. Continuity correction string (SL mode).
# @param panel_title   Character. Panel display title.
# @param panel_desc    Character. Panel description.
#
# @return Named list (config) with resolved paths, datasets, and settings.
# =============================================================================
params <- function(run_location  = "LOCAL",
                   saspath       = NULL,
                   utilpath      = NULL,
                   studypath     = NULL,
                   outpath       = NULL,
                   templatepath  = NULL,
                   ndabla        = "",
                   studyid       = "",
                   study_lag     = 120,
                   cc            = "0",
                   cont_corr     = NULL,
                   panel_title   = "AE Severity",
                   panel_desc    = "") {

  config <- list()

  # -- Common fields (all modes) ------------------------------------------------
  config$panel_title <- panel_title
  config$panel_desc  <- panel_desc
  config$ndabla      <- ndabla
  config$studyid     <- studyid
  config$study_lag   <- as.numeric(study_lag)
  config$run_location <- toupper(stringr::str_trim(run_location))

  cli::cli_alert_info("Initialising AE Severity Panel ({config$run_location} mode)")

  # -- Paths and modules -------------------------------------------------------
  config$saspath  <- saspath
  config$utilpath <- utilpath
  config$outpath  <- outpath

  if (is.null(saspath) || is.null(utilpath)) {
    cli::cli_warn(
      "saspath and/or utilpath not specified; source() calls may fail at runtime."
    )
  }

  # -- Mode-specific initialisation --------------------------------------------
  if (config$run_location == "LOCAL") {
    # ~~~~~ LOCAL mode (SAS lines 90-163) ~~~~~
    cli::cli_alert_info("LOCAL mode: loading datasets from {.path {studypath}}")

    # Output file paths (.xlsx for openxlsx compatibility)
    config$aeout1 <- file.path(outpath, "AE Severity.xlsx")
    config$aeout2 <- file.path(outpath, "Adverse Events Odds Ratio Analysis.xlsx")
    config$aeout3 <- file.path(outpath, "Adverse Events Relative Risk Analysis.xlsx")
    config$errout <- file.path(outpath, "AE Severity Error Summary.xlsx")

    # Study data path and dataset loading (replaces SAS libname)
    config$studypath <- studypath

    # Load CDISC XPT datasets (SAS lines 119-121)
    ae_path <- file.path(studypath, "ae.xpt")
    dm_path <- file.path(studypath, "dm.xpt")
    ex_path <- file.path(studypath, "ex.xpt")

    if (!file.exists(ae_path)) {
      # Try SAS7BDAT format as fallback
      ae_path <- file.path(studypath, "ae.sas7bdat")
    }
    if (!file.exists(dm_path)) {
      dm_path <- file.path(studypath, "dm.sas7bdat")
    }
    if (!file.exists(ex_path)) {
      ex_path <- file.path(studypath, "ex.sas7bdat")
    }

    config$ae <- load_dataset(ae_path)
    config$dm <- load_dataset(dm_path)
    config$ex <- load_dataset(ex_path)

    cli::cli_alert_info(
      "Loaded: AE ({nrow(config$ae)} obs), DM ({nrow(config$dm)} obs), EX ({nrow(config$ex)} obs)"
    )

    # Template workbook handling (SAS lines 139-142: x "copy...")
    config$templatepath <- templatepath
    config$or_template  <- "AE_OR_Template.xls"
    config$rr_template  <- "AE_RR_Template.xls"

    if (!is.null(templatepath) && !is.null(outpath)) {
      or_src <- file.path(templatepath, config$or_template)
      rr_src <- file.path(templatepath, config$rr_template)
      if (file.exists(or_src)) {
        file.copy(or_src, file.path(outpath, config$or_template), overwrite = TRUE)
      }
      if (file.exists(rr_src)) {
        file.copy(rr_src, file.path(outpath, config$rr_template), overwrite = TRUE)
      }
    }

    # Continuity correction (SAS line 126: %let cc = 0;)
    config$cc <- cc

    # Dummy Script Launcher datasets (SAS lines 148-157)
    # In LOCAL mode these are empty tibbles matching the expected column structure
    config$sl_datasets <- dplyr::tibble(
      dset = character(), label = character()
    )
    config$sl_group <- dplyr::tibble(
      group_var = character(), group_val = character()
    )
    config$sl_subset <- dplyr::tibble(
      subset_var = character(), subset_val = character(), subset_op = character()
    )
    config$sl_group_nobs  <- 0L
    config$sl_subset_nobs <- 0L
    config$sl_custom_ds   <- ""

  } else {
    # ~~~~~ SCRIPT LAUNCHER mode (SAS lines 167-189) ~~~~~
    cli::cli_alert_info("SCRIPT_LAUNCHER mode: expecting pre-loaded datasets")

    # study_lag normalisation (SAS line 167: %let study_lag = %sysfunc(trim(...)))
    config$study_lag <- as.numeric(stringr::str_trim(as.character(study_lag)))

    # Continuity correction normalisation (SAS lines 170-189)
    if (!is.null(cont_corr)) {
      cc_val <- toupper(stringr::str_trim(cont_corr))
      config$cc <- dplyr::case_when(
        cc_val == "1"                     ~ "1",
        cc_val %in% c("0.5", "1/2")      ~ "0.5",
        cc_val == "1/OTHERARMCOUNT"       ~ "arm",
        cc_val %in% c("NONE", "0", "")   ~ "0",
        TRUE                              ~ "0"
      )
    } else {
      config$cc <- cc
    }

    # Output file paths (populated by Script Launcher before invocation)
    out_dir <- if (!is.null(outpath) && nzchar(outpath)) outpath else "."
    if (is.null(config$aeout1)) {
      config$aeout1 <- file.path(out_dir, "AE Severity.xlsx")
    }
    if (is.null(config$aeout2)) {
      config$aeout2 <- file.path(out_dir, "Adverse Events Odds Ratio Analysis.xlsx")
    }
    if (is.null(config$aeout3)) {
      config$aeout3 <- file.path(out_dir, "Adverse Events Relative Risk Analysis.xlsx")
    }
    if (is.null(config$errout)) {
      config$errout <- file.path(out_dir, "AE Severity Error Summary.xlsx")
    }
  }

  cli::cli_alert_info("Continuity correction parameter: cc = '{config$cc}'")
  config
}


# =============================================================================
# load_dataset — Load a SAS transport (XPT) or SAS7BDAT dataset
# =============================================================================
# Determines the file type by extension and delegates to the appropriate
# haven reader. Column names are lowercased to match R convention.
#
# @param path Character. Full path to dataset file.
# @return A tibble with lowercase column names.
# =============================================================================
load_dataset <- function(path) {
  if (!file.exists(path)) {
    stop(
      cli::format_error(
        c("Dataset file not found: {.file {path}}.",
          "i" = "Check studypath parameter.")
      ),
      call. = FALSE
    )
  }
  ext <- tolower(tools::file_ext(path))
  ds <- if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext %in% c("sas7bdat", "sas")) {
    haven::read_sas(path)
  } else {
    stop(
      cli::format_error("Unsupported dataset format: {.val {ext}}"),
      call. = FALSE
    )
  }
  # Lowercase column names for consistent downstream processing
  names(ds) <- tolower(names(ds))
  ds
}


# =============================================================================
# parse_continuity_correction — Parse CC Parameter Into Numeric Switches
# =============================================================================
# Migrated from SAS DATA _NULL_ block (ae.sas lines 207-222).
#
# Translates the string continuity correction parameter into two numeric
# indicators consumed by the OR/RR analysis:
#   cc_sw    0 = no CC, 1 = fixed numeric CC, 2 = arm-based CC (1/other-arm N)
#   cc_whole 0 = fractional CC, 1 = whole-number or no CC
#
# @param cc Character. The continuity correction value ("0", "0.5", "1",
#           "arm", or other numeric string).
# @return Named list with elements cc_sw (integer) and cc_whole (integer).
# =============================================================================
parse_continuity_correction <- function(cc) {
  cc <- stringr::str_trim(as.character(cc))

  # SAS: if anyalpha(cc) then cc_sw = 2 — the only alpha value is "arm"
  if (grepl("[A-Za-z]", cc)) {
    cc_sw <- 2L
  } else {
    cc_num <- suppressWarnings(as.numeric(cc))
    if (!is.na(cc_num) && cc_num != 0) {
      cc_sw <- 1L
    } else {
      cc_sw <- 0L
    }
  }

  # SAS: cc_whole determination (lines 216-221)
  if (cc_sw == 2L) {
    # Arm-based CC is always fractional
    cc_whole <- 0L
  } else if (cc_sw == 1L) {
    cc_num <- as.numeric(cc)
    cc_whole <- if (cc_num - floor(cc_num) != 0) 0L else 1L
  } else {
    # No CC — treat as whole (no fractional component)
    cc_whole <- 1L
  }

  list(cc_sw = cc_sw, cc_whole = cc_whole)
}


# =============================================================================
# ae — Execute AE Severity Analysis Pipeline
# =============================================================================
# Migrated from SAS %ae macro (ae.sas lines 228-287).
#
# Orchestrates the complete AE Severity pipeline:
#   1. Call setup() for dataset validation and population merge
#   2. Sort and deduplicate to subject-level preferred terms
#   3. Execute Analyses A/B (AEs per preferred term per arm)
#   4. Execute Analyses C/D (AEs per severity level per arm)
#   5. Run post-analysis data quality checks (AESER values)
#   6. Generate Excel workbook via out_ae()
#   7. If multi-arm: run OR/RR analysis and generate OR/RR workbooks
#   8. Handle error paths (single-arm OR/RR, setup failure)
#
# @param config   Named list from params() + parse_continuity_correction().
# @param ae_data  Tibble. AE domain dataset (from haven::read_xpt).
# @param dm_data  Tibble. DM domain dataset.
# @param ex_data  Tibble. EX domain dataset.
#
# @return Named list with analysis results and output file paths.
# =============================================================================
ae <- function(config, ae_data, dm_data, ex_data) {

  cli::cli_alert_info(strrep("=", 60))
  cli::cli_alert_info("AE Severity Panel — Execution Start")
  cli::cli_alert_info(strrep("=", 60))

  # =========================================================================
  # Step 1: Setup and validation (SAS line 230: %setup)
  # =========================================================================
  cli::cli_alert_info("Running setup and validation ...")
  setup_result <- setup(
    dm        = dm_data,
    ae        = ae_data,
    ex        = ex_data,
    vld_sw    = if (!is.null(config$vld_sw)) config$vld_sw else 1L,
    study_lag = config$study_lag
  )

  # =========================================================================
  # Failure path (SAS lines 280-285)
  # =========================================================================
  if (!isTRUE(setup_result$setup_success)) {
    cli::cli_warn("Setup failed — generating error summary workbook.")

    # Derive error flags from available data (SAS lines 280-285)
    # dm_subj_gt0: TRUE if at least one safety-population subject exists
    dm_subj_gt0 <- tryCatch(
      chk_dm_subj_gt0(dm_data),
      error = function(e) FALSE
    )
    # setup_req_var: TRUE if all required variables are present
    setup_req_var <- is.null(setup_result$rpt_chk_var_req) ||
      all(setup_result$rpt_chk_var_req$ind == 1L)

    # SAS ifc(&dm_subj_gt0.,0,1) inverts: 0 when TRUE -> err_nosubj = !dm_subj_gt0
    error_summary(
      err_file    = config$errout,
      panel_title = config$panel_title,
      ndabla      = config$ndabla,
      studyid     = config$studyid,
      err_nosubj  = !dm_subj_gt0,
      err_missvar = !setup_req_var,
      err_seterr  = TRUE,
      panel_desc  = config$panel_desc,
      sl_subset   = config$sl_subset,
      rpt_chk_var_req = setup_result$rpt_chk_var_req
    )

    return(invisible(list(
      success    = FALSE,
      setup_result = setup_result,
      output_files = list(errout = config$errout)
    )))
  }

  cli::cli_alert_info(
    "Setup complete: {setup_result$arm_count} arms, {setup_result$arm_total} total subjects"
  )

  # =========================================================================
  # Merge setup results into config (SAS call symputx equivalents)
  # =========================================================================
  config$arm_count      <- setup_result$arm_count
  config$arm_total      <- setup_result$arm_total
  config$arm_names      <- setup_result$arm_names
  config$arm_counts     <- setup_result$arm_counts
  config$max_arm_nm_len <- setup_result$max_arm_nm_len
  config$dm_actarm      <- setup_result$arm_var == "actarm"
  config$vld_sw         <- setup_result$vld_sw

  # Build arm_subjcnts: positional numeric vector with "total" element
  # ae_ab() accesses arm_subjcnts[[i]] (position) and arm_subjcnts[["total"]]
  config$arm_subjcnts <- c(
    unname(setup_result$arm_counts),
    total = setup_result$arm_total
  )

  # Build arm_n: named vector keyed by arm_1, arm_2, etc. (for ae_output.R)
  config$arm_n <- setup_result$arm_counts

  # Detect available AE variables from ds_base columns
  # ae_aeser and ae_aesev are local to setup() — we infer from column presence
  config$ae_aeser <- "aeser" %in% names(setup_result$ds_base)
  config$ae_aesev <- "aesev" %in% names(setup_result$ds_base)

  cli::cli_alert_info(
    "Variable detection: ae_aeser={config$ae_aeser}, ae_aesev={config$ae_aesev}"
  )

  # =========================================================================
  # Step 2: Sort and deduplicate (SAS lines 236-247)
  # proc sort data=ds_base out=ds_base_bysubjpt nodupkey;
  #   by aebodsys aedecod usubjid descending aeser;
  # =========================================================================
  cli::cli_alert_info("Sorting and deduplicating to subject-level preferred terms ...")

  ds_base <- setup_result$ds_base

  # Build sort specification (SAS lines 236-237)
  if (config$ae_aeser) {
    ds_base_bysubjpt <- ds_base %>%
      dplyr::arrange(aebodsys, aedecod, usubjid, dplyr::desc(aeser))
  } else {
    ds_base_bysubjpt <- ds_base %>%
      dplyr::arrange(aebodsys, aedecod, usubjid)
  }

  # Deduplicate: one row per subject per preferred term (SAS nodupkey + first.)
  # SAS by aebodsys aedecod usubjid → distinct on these keys
  ds_base_bysubjpt <- ds_base_bysubjpt %>%
    dplyr::distinct(aebodsys, aedecod, usubjid, .keep_all = TRUE)

  # Keep first observation per subject×term group (SAS first.aedecod logic)
  ds_base_bysubjpt <- ds_base_bysubjpt %>%
    dplyr::group_by(aebodsys, aedecod, usubjid) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  # Reorder columns (SAS line 242: retain usubjid arm_num aebodsys aedecod)
  leading_cols <- c("usubjid", "arm_num", "aebodsys", "aedecod")
  remaining_cols <- setdiff(names(ds_base_bysubjpt), leading_cols)
  ds_base_bysubjpt <- ds_base_bysubjpt %>%
    dplyr::select(dplyr::all_of(leading_cols), dplyr::everything())

  cli::cli_alert_info(
    "Deduplicated: {nrow(ds_base_bysubjpt)} subject-term records"
  )

  # =========================================================================
  # Step 3: Analysis A — AEs by Preferred Term per Arm (SAS line 249-250)
  # Always runs.
  # =========================================================================
  cli::cli_alert_info("Running Analysis A: AEs by preferred term per arm ...")
  ab_a <- ae_ab(ds_base_bysubjpt, config)

  # =========================================================================
  # Step 4: Analysis B — Serious AEs by Preferred Term (SAS line 251)
  # Conditional on AESER variable being present.
  # =========================================================================
  ab_b <- NULL
  if (config$ae_aeser) {
    cli::cli_alert_info("Running Analysis B: Serious AEs by preferred term per arm ...")
    ab_b <- ae_ab(ds_base_bysubjpt, config, aeser = "Y")
  } else {
    cli::cli_inform("Analysis B skipped — AESER variable not available.")
  }

  # =========================================================================
  # Step 5: Analysis C — AEs by Severity Level (SAS lines 253-254)
  # Conditional on AESEV variable being present.
  # =========================================================================
  cd_c <- NULL
  cd_d <- NULL
  sev_lookup <- NULL

  if (config$ae_aesev) {
    cli::cli_alert_info("Running Analysis C: AEs by severity level per arm ...")
    cd_c_result <- ae_cd(ds_base, config)
    cd_c        <- cd_c_result
    sev_lookup  <- cd_c_result$sev_lookup

    # Propagate severity metadata into config for output module
    config$sev_count       <- sev_lookup$sev_count
    config$sev_names       <- sev_lookup$sev_names
    config$max_aesev_nm_len <- max(nchar(sev_lookup$sev_names), na.rm = TRUE)

    # =========================================================================
    # Step 6: Analysis D — Serious AEs by Severity (SAS lines 255-257)
    # Conditional on both AESER and AESEV.
    # =========================================================================
    if (config$ae_aeser) {
      cli::cli_alert_info("Running Analysis D: Serious AEs by severity level per arm ...")
      cd_d_result <- ae_cd(ds_base, config, aeser = "Y", sev_lookup = sev_lookup)
      cd_d        <- cd_d_result
    } else {
      cli::cli_inform("Analysis D skipped — AESER variable not available.")
    }
  } else {
    cli::cli_inform("Analyses C and D skipped — AESEV variable not available.")
  }

  # =========================================================================
  # Step 7: Post-analysis data quality checks (SAS lines 259-262)
  # Validate AESER values in merged dataset
  # =========================================================================
  all_ae_dm_ex_aeser_y <- FALSE
  all_ae_dm_ex_aeser_n <- FALSE

  if (config$ae_aeser) {
    cli::cli_alert_info("Running AESER value checks ...")
    # Reconstruct the full merged dataset for checking (SAS: all_ae_dm_ex)
    all_ae_dm_ex <- dplyr::bind_rows(ds_base, setup_result$err_base)

    if ("aeser" %in% names(all_ae_dm_ex)) {
      chk_y <- chk_val(all_ae_dm_ex, "aeser", "Y", "all_ae_dm_ex")
      chk_n <- chk_val(all_ae_dm_ex, "aeser", "N", "all_ae_dm_ex")

      all_ae_dm_ex_aeser_y <- any(chk_y$value_indicators)
      all_ae_dm_ex_aeser_n <- any(chk_n$value_indicators)
    }
  }

  config$all_ae_dm_ex_aeser_y <- all_ae_dm_ex_aeser_y
  config$all_ae_dm_ex_aeser_n <- all_ae_dm_ex_aeser_n

  # =========================================================================
  # Step 8: Build results object for output module (SAS line 264)
  # =========================================================================
  results <- list(
    ab_a_output    = ab_a,
    ab_b_output    = ab_b,
    cd_c_output    = if (!is.null(cd_c)) cd_c$cd_output else NULL,
    cd_d_output    = if (!is.null(cd_d)) cd_d$cd_output else NULL,
    rpt_dm         = setup_result$rpt_dm,
    rpt_err        = setup_result$rpt_err,
    rpt_err_term   = setup_result$rpt_err_term,
    rpt_missing    = if (!is.null(cd_c)) cd_c$rpt_missing else NULL,
    naes_sp        = setup_result$naes_sp,
    naes_spv       = setup_result$naes_spv,
    naes_sp_by_arm = setup_result$naes_sp_by_arm,
    sl_group       = config$sl_group,
    sl_subset      = config$sl_subset,
    sl_datasets    = config$sl_datasets
  )

  # =========================================================================
  # Step 9: Generate AE Severity workbook (SAS line 264: %out_ae)
  # =========================================================================
  cli::cli_alert_info("Generating AE Severity workbook ...")
  out_ae(config, results)

  # =========================================================================
  # Step 10: OR / RR Analysis (SAS lines 266-277)
  # =========================================================================
  output_files <- list(aeout1 = config$aeout1)

  if (config$arm_count > 1L) {
    cli::cli_alert_info("Multi-arm study: running OR/RR analysis ...")

    # Run OR/RR computation (SAS line 267: %rror)
    rror_result <- rror(ds_base_bysubjpt, config)

    # Build forest-plot layout datasets (SAS line 268: %outs)
    outs_result <- outs(
      or_data     = rror_result$or_data,
      rr_data     = rror_result$rr_data,
      rror_cc_ind = rror_result$rror_cc_ind,
      config      = config
    )

    # Build study metadata tables (SAS implicit in %out_ae_rror)
    lib_meta <- build_lib_metadata(config)

    # Generate OR/RR Excel workbooks (SAS line 269: %out_ae_rror)
    out_ae_rror(
      config        = config,
      rr_out        = outs_result$rr_out,
      or_out        = outs_result$or_out,
      rr_abbrev_out = outs_result$rr_abbrev_out,
      or_abbrev_out = outs_result$or_abbrev_out,
      lib_or        = lib_meta$lib_or,
      lib_rr        = lib_meta$lib_rr,
      lib_arm       = lib_meta$lib_arm
    )

    output_files$aeout2 <- config$aeout2
    output_files$aeout3 <- config$aeout3

  } else {
    # Single-arm study: generate error workbooks for OR and RR
    # (SAS lines 271-277)
    cli::cli_warn("Single-arm study: OR/RR analyses not applicable.")

    single_arm_desc <- stringr::str_c(
      "This study contains only one treatment arm. ",
      "Odds ratio and relative risk analyses require at least two arms."
    )

    # OR error workbook (SAS lines 271-273)
    error_summary(
      err_file    = config$aeout2,
      panel_title = config$panel_title,
      ndabla      = config$ndabla,
      studyid     = config$studyid,
      err_nosubj  = FALSE,
      err_missvar = FALSE,
      err_seterr  = FALSE,
      err_desc    = single_arm_desc,
      panel_desc  = config$panel_desc,
      sl_subset   = config$sl_subset
    )

    # RR error workbook (SAS lines 274-277)
    error_summary(
      err_file    = config$aeout3,
      panel_title = config$panel_title,
      ndabla      = config$ndabla,
      studyid     = config$studyid,
      err_nosubj  = FALSE,
      err_missvar = FALSE,
      err_seterr  = FALSE,
      err_desc    = single_arm_desc,
      panel_desc  = config$panel_desc,
      sl_subset   = config$sl_subset
    )

    output_files$aeout2 <- config$aeout2
    output_files$aeout3 <- config$aeout3
  }

  cli::cli_alert_info(strrep("=", 60))
  cli::cli_alert_info("AE Severity Panel — Execution Complete")
  cli::cli_alert_info(strrep("=", 60))

  invisible(list(
    success      = TRUE,
    config       = config,
    setup_result = setup_result,
    results      = results,
    output_files = output_files
  ))
}


# =============================================================================
# run_ae_severity — Top-Level Entry Point
# =============================================================================
# Convenience wrapper that orchestrates the full AE Severity pipeline:
#   1. Build configuration via params()
#   2. Source all dependency modules
#   3. Parse continuity correction
#   4. Execute the ae() analysis pipeline
#
# This is the recommended single entry point for standalone execution.
#
# @param run_location Character. Execution mode ("LOCAL" or "SCRIPT_LAUNCHER").
# @param ...          Additional arguments passed to params().
# @return Named list from ae() with results and output file paths (invisibly).
# =============================================================================
run_ae_severity <- function(run_location = "LOCAL", ...) {

  # Step 1: Build configuration
  config <- params(run_location = run_location, ...)

  # Step 2: Source dependency modules (requires saspath and utilpath in config)
  if (!is.null(config$saspath) && !is.null(config$utilpath)) {
    source_dependencies(config$saspath, config$utilpath)
  } else {
    cli::cli_warn(
      "saspath or utilpath not set — dependency modules must already be loaded."
    )
  }

  # Step 3: Parse continuity correction
  cc_info <- parse_continuity_correction(config$cc)
  config$cc_sw    <- cc_info$cc_sw
  config$cc_whole <- cc_info$cc_whole

  # Step 4: Data validation switch (SAS line 225: %let vld_sw = 1)
  config$vld_sw <- 1L

  # Step 5: Run the analysis pipeline
  result <- ae(
    config  = config,
    ae_data = config$ae,
    dm_data = config$dm,
    ex_data = config$ex
  )

  invisible(result)
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS run_location detection (%symexist) -> R function argument (default LOCAL)
####    - SAS global macro variables -> named elements in config list object
####    - SAS options minoperator/missing='' -> Not applicable in R
####    - SAS PROC DATASETS kill -> Not needed (R garbage collection handles cleanup)
####    - SAS x "copy..." command for templates -> R file.copy()
####    - SAS %include -> R source() calls via source_dependencies()
####    - SAS symputx/symget global state -> Explicit config list passing between functions
####    - Excel output extension changed from .xls to .xlsx for openxlsx compatibility
####    - ae_setup.R returns arm_counts as named vector (arm_1, arm_2, ...);
####      ae_ab() expects arm_subjcnts with positional indexing + "total" key —
####      conversion handled in ae() by combining unname(arm_counts) with total
####    - ae_aeser / ae_aesev are local to setup() and not returned; detection
####      is performed by checking column presence in ds_base
####    - SAS all_ae_dm_ex dataset (merged AE+DM+EX) is reconstructed from
####      ds_base + err_base for post-analysis chk_val() calls
####    - SAS conditional %include of ae_rror.sas -> unconditional source() of
####      ae_rror.R (function definitions only); execution gated by arm_count > 1
####    - ae_aggregate.R exports ae_ab()/ae_cd() (not ab()/cd() as in SAS)
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Continuity correction parsing: SAS anyalpha() vs R grepl() — identical
####      behavior expected for the supported values ("arm", "0", "0.5", "1")
####    - Sort stability: SAS proc sort is stable; R arrange() is stable within
####      groups — verified equivalent for BY aebodsys aedecod usubjid
####    - Deduplication: SAS nodupkey vs R distinct(.keep_all=TRUE) — identical
####      semantics for sorted input
####    - No rounding occurs in ae.R directly; rounding handled downstream by
####      ae_aggregate.R and ae_rror.R using janitor::round_half_up()
#### NO DIRECT R EQUIVALENT:
####    - SAS sashelp.vmacro global macro cleanup -> Not needed in R (function scoping)
####    - SAS PROC DATASETS kill -> Not needed (R environments are isolated)
####    - SAS options noxwait xsync -> N/A (no shell command synchronization needed)
####    - SAS %sysfunc(ifc(...)) conditional -> R ifelse() or dplyr::case_when()
####    - SAS libname engine -> haven::read_xpt() / haven::read_sas()
####    - SAS PCFILES/JET engine -> openxlsx
####    - SAS run_location %symexist -> R function argument with default
####    - SAS call symputx for cross-macro state -> config list passed explicitly
#### PACKAGE SELECTION RATIONALE:
####    - haven: SAS XPT/SAS7BDAT data I/O — standard tidyverse SAS interop
####    - dplyr: Data manipulation replacing DATA steps — idiomatic tidyverse
####    - tidyr: Reshaping operations used by sourced modules
####    - stringr: String operations replacing SAS character functions
####    - forcats: Factor level management for severity ordering
####    - openxlsx: Excel output replacing SpreadsheetML XML and PCFILES
####    - janitor: round_half_up() for SAS-compatible rounding behavior
####    - cli: User-facing messages replacing SAS %put statements
#### OPEN QUESTIONS:
####    - Confirm SAS dataset formats (XPT vs SAS7BDAT) for haven reader selection
####    - Verify template .xls files can be read/modified by openxlsx or if
####      format conversion is needed (openxlsx creates .xlsx natively)
####    - Review whether Script Launcher integration requires additional
####      config fields beyond those mapped from SAS %params
#### ============================================================
