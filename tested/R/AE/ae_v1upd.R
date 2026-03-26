# ==============================================================================
# PROGRAM: ae_v1upd.R
# DESCRIPTION: AE Severity Analysis Panel — Updated Parameterized Version (R Migration)
#   - Find subject counts per arm for each adverse event
#   - Find event counts per arm and severity level for each AE
#   - Find top AEs by relative risk & odds ratio between arm pairs
#   - Canonical entry point with full parameterization
#   - Creates three output workbooks:
#       1. AE Severity counts (aeout1)
#       2. Odds Ratio analysis (aeout2)
#       3. Relative Risk analysis (aeout3)
# MIGRATED FROM: tested/SAS/AE/ae_v1upd.sas (300 lines)
# EVALUATION TYPE: Safety
# AUTHORS: David Kretch, Andreas Anastassopoulos (original SAS)
# DATE: February 7, 2011 (original); R migration 2026
#
# KEY DIFFERENCE FROM ae_v1.R:
#   - SAS ae_v1upd.sas accepted all configuration via %params(in_*=...) keyword
#     arguments, replacing hardcoded LOCAL paths in ae_v1.sas
#   - SAS: in_studyid=CDISC (default), in_studylag=120, in_cc=0
#   - SAS: options mlogic symbolgen for macro debug tracing
#   - In R: both become parameterized functions; distinction preserved via
#     function name (ae_severity_v1upd), studyid default ("CDISC"), and
#     verbose flag (replaces mlogic/symbolgen)
#
# SAS MACROS MIGRATED:
#   %params (lines 87-201)  -> Function arguments with matching defaults
#   %ae     (lines 238-299) -> Function body implementing analyses A-D
#
# EXTERNAL FILES USED (R equivalents):
#   ae_setup.R      -- Merges AE, DM, and EX domains; validates variables
#   ae_aggregate.R  -- Finds subject/event counts per arm (ae_ab, ae_cd)
#   ae_output.R     -- Creates Excel workbook output (ae_out_workbook)
#   ae_rror.R       -- Relative risk/odds ratio analysis + Excel export
#   data_checks.R   -- Generic variable validation (chk_val)
#   err_output.R    -- Error summary workbook (error_summary)
#   sl_gs_output.R  -- Script Launcher grouping/subsetting metadata
#   xml_output.R    -- Foundational Excel output engine (openxlsx-based)
#
# VARIABLES REQUIRED: AE -- AEBODSYS, AEDECOD, USUBJID
#                     DM -- ACTARM or ARM, USUBJID
#                     EX -- USUBJID
#
# VARIABLES USED WHEN AVAILABLE:
#                     AE -- AESTDTC, AESER, AESEV
#                     DM -- RFSTDTC, RFENDTC, ARMCD
#                     EX -- EXSTDTC, EXENDTC
# ==============================================================================

# --- Library loading ----------------------------------------------------------
# Core tidyverse components replacing SAS DATA step / PROC SQL constructs
library(haven)       # SAS data I/O: read_xpt() replacing libname/XPT access
library(dplyr)       # Data manipulation: replacing DATA steps and PROC SQL
library(tidyr)       # Reshaping: pivot_wider/pivot_longer replacing PROC TRANSPOSE
library(stringr)     # String manipulation: replacing SAS character functions
library(cli)         # User-facing messages: replacing SAS PUT/NOTE statements
library(janitor)     # round_half_up(): SAS-compatible rounding (0.5 -> 1)
library(openxlsx)    # Excel output: replacing SpreadsheetML and PCFILES engine

# --- Source internal dependencies ---------------------------------------------
# Replaces SAS: %include "&utilpath.\ae_setup.sas"; etc.
# Uses file.path() with script-relative paths for portability. The driver
# sources all dependencies to make functions available in the R environment.
# Each source() call is guarded to prevent re-sourcing if already loaded.
local({
  # Determine script directory for relative path resolution
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )

  # Build base paths: macros and utilities directories relative to this file
  if (!is.null(this_dir)) {
    macro_dir <- file.path(dirname(this_dir), "macros")
    util_dir  <- file.path(dirname(this_dir), "utilities")
  } else {
    # Fallback: search relative to working directory
    macro_dir <- "tested/R/macros"
    util_dir  <- "tested/R/utilities"
    if (!dir.exists(macro_dir)) macro_dir <- file.path("..", "macros")
    if (!dir.exists(util_dir))  util_dir  <- file.path("..", "utilities")
  }

  # Source utility files first (foundational layer)
  xml_path <- file.path(util_dir, "xml_output.R")
  if (file.exists(xml_path) && !exists("create_workbook", mode = "function")) {
    source(xml_path, local = FALSE)
  }

  dc_path <- file.path(util_dir, "data_checks.R")
  if (file.exists(dc_path) && !exists("chk_val", mode = "function")) {
    source(dc_path, local = FALSE)
  }

  err_path <- file.path(util_dir, "err_output.R")
  if (file.exists(err_path) && !exists("error_summary", mode = "function")) {
    source(err_path, local = FALSE)
  }

  sl_path <- file.path(util_dir, "sl_gs_output.R")
  if (file.exists(sl_path) && !exists("group_subset_pp", mode = "function")) {
    source(sl_path, local = FALSE)
  }

  setup_path <- file.path(util_dir, "ae_setup.R")
  if (file.exists(setup_path) && !exists("setup_validation", mode = "function")) {
    source(setup_path, local = FALSE)
  }

  # Source macro files (analytical layer)
  agg_path <- file.path(macro_dir, "ae_aggregate.R")
  if (file.exists(agg_path) && !exists("ae_ab", mode = "function")) {
    source(agg_path, local = FALSE)
  }

  out_path <- file.path(macro_dir, "ae_output.R")
  if (file.exists(out_path) && !exists("ae_out_workbook", mode = "function")) {
    source(out_path, local = FALSE)
  }

  rror_path <- file.path(macro_dir, "ae_rror.R")
  if (file.exists(rror_path) && !exists("compute_rror", mode = "function")) {
    source(rror_path, local = FALSE)
  }
})


# ==============================================================================
# ae_severity_v1upd: AE Severity Panel — Canonical Parameterized Entry Point
# ==============================================================================
#' Run the complete AE Severity Analysis Panel with full parameterization.
#'
#' This is the canonical, fully parameterized AE Severity Panel driver.
#' Migrated from SAS \code{ae_v1upd.sas}, which accepted all configuration via
#' \code{%params(in_*=...)} keyword arguments (SAS lines 87-201). Produces
#' three Excel workbooks: (1) AE counts by arm and severity, (2) Odds Ratio
#' analysis, and (3) Relative Risk analysis.
#'
#' Distinguished from \code{ae_severity_v1()} in \code{ae_v1.R} by:
#' \itemize{
#'   \item Function name: \code{ae_severity_v1upd} (traceability to SAS ae_v1upd.sas)
#'   \item Default \code{studyid = "CDISC"} (SAS: \code{in_studyid=CDISC})
#'   \item \code{verbose} parameter replacing SAS \code{options mlogic symbolgen}
#' }
#'
#' @param ae Data frame or character file path. AE domain data (CDISC ADaM/SDTM).
#'   If character, treated as path to XPT file and loaded via \code{haven::read_xpt()}.
#' @param dm Data frame or character file path. DM domain data.
#' @param ex Data frame or character file path. EX domain data.
#' @param panel_title Character. Panel title for workbook headers.
#'   Default "AE Severity" (SAS: \code{in_panel_title}).
#' @param panel_desc Character. Panel description text.
#'   Default "" (SAS: \code{in_panel_desc}).
#' @param outpath Character. Output directory path for workbooks.
#'   Default "." (SAS: \code{in_outpath}).
#' @param aeout1 Character. Filename for AE counts workbook.
#'   Default "AE Severity.xlsx".
#' @param aeout2 Character. Filename for Odds Ratio workbook.
#'   Default "Adverse Events Odds Ratio Analysis.xlsx".
#' @param aeout3 Character. Filename for Relative Risk workbook.
#'   Default "Adverse Events Relative Risk Analysis.xlsx".
#' @param errout Character. Filename for error summary workbook.
#'   Default "AE Severity Error Summary.xlsx".
#' @param ndabla Character. NDA/BLA identifier.
#'   Default "" (SAS: \code{in_ndabla}).
#' @param studyid Character. Study identifier.
#'   Default "CDISC" (SAS: \code{in_studyid=CDISC}).
#' @param study_lag Numeric. Days after last exposure to include AEs.
#'   Default 120 (SAS: \code{in_studylag=120}).
#' @param cc Numeric or character. Continuity correction mode:
#'   \itemize{
#'     \item 0 = no correction (default)
#'     \item Positive number = constant correction added to each cell
#'     \item "arm" = add reciprocal of opposite arm count
#'   }
#'   Default 0 (SAS: \code{in_cc=0}).
#' @param vld_sw Logical. Validation switch — if TRUE, perform data validation.
#'   Default TRUE (SAS: \code{vld_sw=1}).
#' @param verbose Logical. If TRUE, emit diagnostic messages to console
#'   (replaces SAS \code{options mlogic symbolgen}). Default FALSE.
#'
#' @return Invisible named list with analysis results:
#'   \describe{
#'     \item{success}{Logical: TRUE if setup and analyses completed.}
#'     \item{ab_a}{Tibble: Analysis A output (AEs by arm, >2\% filter) or NULL.}
#'     \item{ab_b}{Tibble: Analysis B output (serious AEs by arm) or NULL.}
#'     \item{cd_c}{List: Analysis C output (AEs by severity) or NULL.}
#'     \item{cd_d}{List: Analysis D output (serious AEs by severity) or NULL.}
#'     \item{rror}{List: Relative risk/odds ratio results or NULL.}
#'     \item{setup}{List: Complete setup_validation() result.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Using file paths (XPT files)
#' ae_severity_v1upd(
#'   ae = "data/adam/ae.xpt",
#'   dm = "data/adam/dm.xpt",
#'   ex = "data/adam/ex.xpt",
#'   outpath = "output",
#'   studyid = "CDISC-PILOT-01",
#'   verbose = TRUE
#' )
#'
#' # Using pre-loaded data frames
#' ae_severity_v1upd(
#'   ae = ae_data,
#'   dm = dm_data,
#'   ex = ex_data,
#'   studyid = "MY-STUDY",
#'   cc = "arm",
#'   verbose = TRUE
#' )
#' }
#' @export
ae_severity_v1upd <- function(
  # Data inputs — accept either data frames or file paths
  ae,
  dm,
  ex,

  # Panel metadata — from SAS in_panel_title, in_panel_desc
  panel_title = "AE Severity",
  panel_desc  = "",

  # Path configuration — from SAS in_outpath
  # NOTE: SAS in_saspath, in_utilpath, in_templatepath are not carried forward
  # because R handles library/source paths separately from output paths

  outpath = ".",

  # Output file names (SAS used .xls SpreadsheetML; R uses .xlsx openxlsx)
  aeout1 = "AE Severity.xlsx",
  aeout2 = "Adverse Events Odds Ratio Analysis.xlsx",
  aeout3 = "Adverse Events Relative Risk Analysis.xlsx",
  errout = "AE Severity Error Summary.xlsx",

  # Study identifiers — from SAS in_ndabla, in_studyid
  ndabla  = "",
  studyid = "CDISC",

  # Analysis parameters — from SAS in_studylag, in_cc
  study_lag = 120,
  cc        = 0,

  # Validation switch — from SAS vld_sw=1
  vld_sw = TRUE,

  # Verbose/debug flag — replaces SAS options mlogic symbolgen
  verbose = FALSE
) {

  # ============================================================================
  # Phase 2.2: Verbose diagnostic output (replaces SAS options mlogic symbolgen)
  # ============================================================================
  # SAS ae_v1upd.sas line 80: options minoperator mlogic symbolgen;
  # In R, verbose=TRUE emits diagnostic messages via cli package
  if (verbose) {
    cli::cli_h1("AE Severity Analysis (v1upd) - Parameterized Entry Point")
    cli::cli_alert_info("panel_title = {panel_title}")
    cli::cli_alert_info("panel_desc  = {panel_desc}")
    cli::cli_alert_info("outpath     = {outpath}")
    cli::cli_alert_info("ndabla      = {ndabla}")
    cli::cli_alert_info("studyid     = {studyid}")
    cli::cli_alert_info("study_lag   = {study_lag}")
    cli::cli_alert_info("cc          = {cc}")
    cli::cli_alert_info("vld_sw      = {vld_sw}")
  }

  # ============================================================================
  # Phase 2.3: Data loading — accept data frames or XPT file paths
  # ============================================================================
  # SAS ae_v1upd.sas lines 121-128: libname inlib "&studypath."; data ae; set inlib.ae;
  # R equivalent: haven::read_xpt() for file paths, passthrough for data frames
  if (is.character(ae)) {
    if (verbose) cli::cli_alert_info("Loading AE from XPT: {ae}")
    ae <- haven::read_xpt(ae)
  }
  if (is.character(dm)) {
    if (verbose) cli::cli_alert_info("Loading DM from XPT: {dm}")
    dm <- haven::read_xpt(dm)
  }
  if (is.character(ex)) {
    if (verbose) cli::cli_alert_info("Loading EX from XPT: {ex}")
    ex <- haven::read_xpt(ex)
  }

  # Validate inputs are data frames after potential loading
  if (!is.data.frame(ae)) {
    cli::cli_abort("{.arg ae} must be a data frame or path to an XPT file.")
  }
  if (!is.data.frame(dm)) {
    cli::cli_abort("{.arg dm} must be a data frame or path to an XPT file.")
  }
  if (!is.data.frame(ex)) {
    cli::cli_abort("{.arg ex} must be a data frame or path to an XPT file.")
  }

  # Lowercase all column names for case-insensitive processing
  # SAS is case-insensitive; R requires explicit lowercasing
  ae <- ae %>% dplyr::rename_with(tolower)
  dm <- dm %>% dplyr::rename_with(tolower)
  ex <- ex %>% dplyr::rename_with(tolower)

  # ============================================================================
  # Phase 2.4: Continuity correction normalization
  # ============================================================================
  # SAS ae_v1upd.sas lines 154-158 + 217-232:
  #   %let cc = &in_cc;
  #   data _null_;
  #     if anyalpha("&cc.") then cc = "&cc.";
  #     else cc = &cc.;
  #     if anyalpha(cc) and cc = 'arm' then cc_sw = 2;
  #     else if not missing(cc) and cc ne 0 then cc_sw = 1;
  #     else cc_sw = 0;
  #     if cc_sw = 2 then cc_whole = 0;
  #     else if (cc - floor(cc) ne 0) then cc_whole = 0;
  #     else cc_whole = 1;
  #   run;
  #
  # cc_sw modes:
  #   0 = no continuity correction
  #   1 = constant correction (cc_value added to each cell)
  #   2 = arm-based correction (reciprocal of opposite arm count)
  # cc_whole: 1 if cc is a whole number, 0 otherwise (affects display format)
  cc_config <- if (is.character(cc) &&
                   stringr::str_to_lower(stringr::str_trim(cc)) == "arm") {
    # Mode 2: arm-based reciprocal correction
    list(cc_sw = 2L, cc_whole = 0L, cc_value = NA_real_)
  } else {
    cc_num <- suppressWarnings(as.numeric(cc))
    if (is.na(cc_num) || cc_num == 0) {
      # Mode 0: no correction
      list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
    } else {
      # Mode 1: constant correction
      list(
        cc_sw    = 1L,
        cc_whole = as.integer(cc_num == floor(cc_num)),
        cc_value = cc_num
      )
    }
  }

  if (verbose) {
    cli::cli_alert_info("CC config: cc_sw={cc_config$cc_sw}, cc_whole={cc_config$cc_whole}")
  }

  # ============================================================================
  # Phase 2.5: Setup validation (SAS: %setup)
  # ============================================================================
  # SAS ae_v1upd.sas line 240: %setup;
  # Calls the gatekeeper validation function from ae_setup.R which:
  #   - Validates required variables (aebodsys, aedecod, usubjid in AE/DM/EX)
  #   - Merges AE/DM/EX domains into safety population
  #   - Assigns treatment arms, computes arm counts
  #   - Filters screen failures, validates AE dates against treatment window
  #   - Returns comprehensive result list
  setup_result <- setup_validation(
    ae        = ae,
    dm        = dm,
    ex        = ex,
    vld_sw    = vld_sw,
    study_lag = study_lag
  )

  if (verbose) {
    cli::cli_alert_info("Setup validation complete: success={setup_result$setup_success}")
    if (setup_result$setup_success) {
      cli::cli_alert_info("Arms found: {setup_result$arm_count}")
      cli::cli_alert_info("Total subjects: {setup_result$arm_total}")
    }
  }

  # Construct output file paths using file.path() — never hardcoded
  # SAS: %let aeout1 = &outpath.\AE Severity.xls;
  aeout1_path <- file.path(outpath, aeout1)
  aeout2_path <- file.path(outpath, aeout2)
  aeout3_path <- file.path(outpath, aeout3)
  errout_path <- file.path(outpath, errout)

  # Ensure output directory exists
  if (!dir.exists(outpath)) {
    dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
  }

  # ============================================================================
  # Preprocess Script Launcher grouping/subsetting metadata
  # ============================================================================
  # SAS ae_v1upd.sas lines 160-172: dummy sl_datasets, sl_group, sl_subset
  # In the LOCAL run mode, SAS creates empty datasets. In R, we pass NULL/empty
  # tibbles to group_subset_pp() which handles the empty case gracefully.
  pp_result <- group_subset_pp(
    sl_group    = NULL,
    sl_subset   = NULL,
    sl_datasets = NULL
  )

  # Initialize output holders for the return value
  ab_a_output <- NULL
  ab_b_output <- NULL
  cd_c_output <- NULL
  cd_d_output <- NULL
  rror_result <- NULL

  # ============================================================================
  # Phase 2.6: Main analysis block (SAS: %ae macro, lines 238-299)
  # ============================================================================
  if (setup_result$setup_success) {

    # Extract key metadata from setup result
    arm_count         <- setup_result$arm_count
    arm_total         <- setup_result$arm_total
    arm_names         <- setup_result$arm_names
    arm_display_names <- setup_result$arm_display_names
    ds_base           <- setup_result$ds_base
    all_ae_dm_ex      <- setup_result$all_ae_dm_ex
    all_dm_ex         <- setup_result$all_dm_ex

    # Determine optional variable availability from the returned ds_base columns
    # SAS set these as global macro flags (&ae_aeser., &ae_aesev.) in %setup
    ae_aeser <- "aeser" %in% names(ds_base)
    ae_aesev <- "aesev" %in% names(ds_base)

    if (verbose) {
      cli::cli_alert_info("AESER available: {ae_aeser}")
      cli::cli_alert_info("AESEV available: {ae_aesev}")
    }

    # ------------------------------------------------------------------
    # Compute arm_subjcnt vector: per-arm subject counts + total
    # SAS: arm_subjcnt array(1..arm_count) built in %setup
    # R: construct from all_dm_ex grouped by arm_num
    # ------------------------------------------------------------------
    arm_subjcnt_by_arm <- all_dm_ex %>%
      dplyr::group_by(.data$arm_num) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$usubjid), .groups = "drop") %>%
      dplyr::arrange(.data$arm_num) %>%
      dplyr::pull(.data$n)

    # Validate arm_subjcnt length matches arm_count
    if (length(arm_subjcnt_by_arm) != arm_count) {
      cli::cli_warn(
        "arm_subjcnt length ({length(arm_subjcnt_by_arm)}) does not match arm_count ({arm_count}). Padding with zeros."
      )
      arm_subjcnt_by_arm <- c(
        arm_subjcnt_by_arm,
        rep(0L, arm_count - length(arm_subjcnt_by_arm))
      )
    }

    # ae_ab expects arm_subjcnt of length arm_count + 1 (last element = total)
    arm_subjcnt <- c(arm_subjcnt_by_arm, arm_total)

    # ------------------------------------------------------------------
    # Sort and deduplicate ds_base for per-subject-per-term analyses
    # SAS lines 244-257:
    #   proc sort data=ds_base out=ds_base_bysubjpt nodupkey;
    #     by aebodsys aedecod usubjid %if &ae_aeser. %then descending aeser;;
    #   run;
    #   data ds_base_bysubjpt;
    #     retain usubjid arm_num aebodsys aedecod;
    #     set ds_base_bysubjpt;
    #     by aebodsys aedecod usubjid;
    #     if first.usubjid;
    #   run;
    # ------------------------------------------------------------------
    if (ae_aeser) {
      # Sort by aebodsys/aedecod/usubjid with descending aeser
      # so 'Y' sorts before 'N', preserving serious indicator
      ds_base_bysubjpt <- ds_base %>%
        dplyr::arrange(
          .data$aebodsys,
          .data$aedecod,
          .data$usubjid,
          dplyr::desc(.data$aeser)
        ) %>%
        dplyr::distinct(
          .data$aebodsys, .data$aedecod, .data$usubjid,
          .keep_all = TRUE
        )
    } else {
      ds_base_bysubjpt <- ds_base %>%
        dplyr::arrange(
          .data$aebodsys,
          .data$aedecod,
          .data$usubjid
        ) %>%
        dplyr::distinct(
          .data$aebodsys, .data$aedecod, .data$usubjid,
          .keep_all = TRUE
        )
    }

    # ------------------------------------------------------------------
    # Analysis A: All AEs by preferred term per arm (SAS: %ab;)
    # SAS line 260: %ab;
    # Computes subject counts per arm for each PT, filtered to >2%
    # ------------------------------------------------------------------
    if (verbose) cli::cli_alert_info("Running Analysis A: AEs by arm (>2% filter)")
    ab_a_output <- ae_ab(
      ds_base_bysubjpt = ds_base_bysubjpt,
      arm_count         = arm_count,
      arm_names         = arm_display_names,
      arm_subjcnt       = arm_subjcnt,
      aeser             = FALSE,
      filter_pct        = 2.0
    )

    # ------------------------------------------------------------------
    # Analysis B: Serious AEs by arm (SAS: %if &ae_aeser. %then %ab(aeser=y);)
    # SAS line 261: %if &ae_aeser. %then %ab(aeser=y);;
    # Conditional on AESER variable being available
    # ------------------------------------------------------------------
    if (ae_aeser) {
      if (verbose) cli::cli_alert_info("Running Analysis B: Serious AEs by arm")
      ab_b_output <- ae_ab(
        ds_base_bysubjpt = ds_base_bysubjpt,
        arm_count         = arm_count,
        arm_names         = arm_display_names,
        arm_subjcnt       = arm_subjcnt,
        aeser             = TRUE
      )
    }

    # ------------------------------------------------------------------
    # Analysis C: AEs by severity (SAS: %if &ae_aesev. %then %do; %cd;)
    # SAS lines 264-267
    # Conditional on AESEV variable being available
    # ------------------------------------------------------------------
    if (ae_aesev) {
      if (verbose) cli::cli_alert_info("Running Analysis C: AEs by severity")
      cd_c_output <- ae_cd(
        ds_base    = ds_base,
        arm_count  = arm_count,
        arm_names  = arm_display_names,
        arm_subjcnt = arm_subjcnt,
        aeser      = FALSE
      )

      # ------------------------------------------------------------------
      # Analysis D: Serious AEs by severity
      # SAS line 266: %if &ae_aeser. %then %cd(aeser=y);;
      # Conditional on BOTH ae_aesev AND ae_aeser
      # ------------------------------------------------------------------
      if (ae_aeser) {
        if (verbose) cli::cli_alert_info("Running Analysis D: Serious AEs by severity")
        cd_d_output <- ae_cd(
          ds_base    = ds_base,
          arm_count  = arm_count,
          arm_names  = arm_display_names,
          arm_subjcnt = arm_subjcnt,
          aeser      = TRUE,
          all_sev    = if (!is.null(cd_c_output)) cd_c_output$all_sev else NULL
        )
      }
    }

    # ------------------------------------------------------------------
    # AESER validation: check for valid values
    # SAS lines 269-272:
    #   %if &ae_aeser. %then %do;
    #     %chk_val(work,all_ae_dm_ex,aeser,Y);
    #     %chk_val(work,all_ae_dm_ex,aeser,N);
    #   %end;
    # In R, combine both checks into a single chk_val call with vector values
    # ------------------------------------------------------------------
    rpt_aeser_val <- NULL
    if (ae_aeser) {
      if (verbose) cli::cli_alert_info("Validating AESER values (Y/N)")
      rpt_aeser_val <- chk_val(
        data     = all_ae_dm_ex,
        var      = "aeser",
        values   = c("Y", "N"),
        cs       = FALSE,
        ds_name  = "all_ae_dm_ex"
      )
    }

    # ------------------------------------------------------------------
    # Collect missing-data report rows from Analysis C/D severity catalogs
    # for the output workbook's data check summary
    # ------------------------------------------------------------------
    rpt_missing <- NULL
    if (!is.null(cd_c_output) && !is.null(cd_c_output$rpt_missing_row)) {
      rpt_missing <- cd_c_output$rpt_missing_row
    }
    if (!is.null(cd_d_output) && !is.null(cd_d_output$rpt_missing_row)) {
      rpt_missing <- dplyr::bind_rows(rpt_missing, cd_d_output$rpt_missing_row)
    }

    # Extract severity catalog for the output workbook
    sev_count <- if (!is.null(cd_c_output)) cd_c_output$sev_count else 0L
    sev_names <- if (!is.null(cd_c_output)) cd_c_output$sev_names else character(0)

    # ------------------------------------------------------------------
    # Phase 2.7: Primary output workbook (SAS: %out_ae;)
    # SAS line 274: %out_ae;
    # Creates aeout1 with all 4 analyses, data checks, and metadata
    # ------------------------------------------------------------------
    if (verbose) cli::cli_alert_info("Generating primary AE output workbook: {aeout1_path}")
    ae_out_workbook(
      output_file    = aeout1_path,
      ab_a_output    = ab_a_output,
      ab_b_output    = if (ae_aeser) ab_b_output else NULL,
      cd_c_output    = if (ae_aesev && !is.null(cd_c_output)) cd_c_output$cd_output else NULL,
      cd_d_output    = if (ae_aesev && ae_aeser && !is.null(cd_d_output)) cd_d_output$cd_output else NULL,
      rpt_dm         = setup_result$rpt_dm,
      rpt_err        = setup_result$rpt_err,
      rpt_err_term   = setup_result$rpt_err_term,
      rpt_missing    = rpt_missing,
      ndabla         = ndabla,
      studyid        = studyid,
      arm_count      = arm_count,
      arm_names      = arm_display_names,
      arm_subjcnt    = arm_subjcnt_by_arm,
      sev_count      = sev_count,
      sev_names      = sev_names,
      vld_sw         = vld_sw,
      study_lag      = study_lag,
      ae_aeser       = ae_aeser,
      ae_aesev       = ae_aesev,
      sl_group_desc  = if (!is.null(pp_result)) pp_result$sl_group_desc %||% "No grouping" else "No grouping",
      sl_subset_desc = if (!is.null(pp_result)) pp_result$sl_subset_desc %||% "No subsetting" else "No subsetting",
      pp_result      = pp_result
    )

    # ------------------------------------------------------------------
    # Phase 2.8: Relative risk / Odds ratio output
    # SAS lines 276-287:
    #   %if &arm_count. > 1 %then %do;
    #     %include "&saspath.\ae_rror.sas";
    #   %end;
    #   %else %do;
    #     %error_summary(err_file=&aeout2., err_seterr=0, ...);
    #     %error_summary(err_file=&aeout3., err_seterr=0, ...);
    #   %end;
    # ------------------------------------------------------------------
    if (arm_count > 1L) {
      if (verbose) cli::cli_alert_info("Running Relative Risk / Odds Ratio analysis (arm_count={arm_count})")

      # Build continuity correction description for metadata
      cc_description <- dplyr::case_when(
        cc_config$cc_sw == 0L ~ "No continuity correction applied",
        cc_config$cc_sw == 1L ~ paste0("Constant continuity correction: ", cc_config$cc_value),
        cc_config$cc_sw == 2L ~ "Arm-based reciprocal continuity correction (1/opposite arm count)",
        TRUE                  ~ "Unknown continuity correction mode"
      )
      cc_asterisk_note <- if (cc_config$cc_sw > 0L) {
        "* Continuity correction was applied"
      } else {
        ""
      }
      cc_detail_note <- if (cc_config$cc_sw == 1L) {
        paste0("A constant of ", cc_config$cc_value,
               " was added to each cell of the 2x2 table")
      } else if (cc_config$cc_sw == 2L) {
        "The reciprocal of the opposite arm count was added to each cell"
      } else {
        ""
      }

      # Step 1: Compute pairwise RR and OR
      rror_compute <- compute_rror(
        ds_base_bysubjpt = ds_base_bysubjpt,
        arm_count        = arm_count,
        arm_names        = arm_display_names,
        arm_subjcnt      = arm_subjcnt_by_arm,
        cc_sw            = cc_config$cc_sw,
        cc_whole         = cc_config$cc_whole,
        cc               = if (cc_config$cc_sw == 2L) "arm" else cc_config$cc_value
      )

      # Step 2: Reshape results for Excel output
      rror_reshaped <- reshape_rror_for_excel(
        compute_result = rror_compute,
        arm_count      = arm_count,
        arm_names      = arm_display_names,
        arm_subjcnt    = arm_subjcnt_by_arm
      )

      # Step 3: Build library metadata for workbook headers
      # lib_metadata carries study-level and configuration info used by
      # write_rror_workbooks() for workbook cover pages and metadata sheets
      lib_metadata <- list(
        ndabla            = ndabla,
        studyid           = studyid,
        rundate           = format(Sys.time(), "%d%b%Y %H:%M"),
        date_validation   = if (vld_sw) "Yes" else "No",
        cc_description    = cc_description,
        cc_asterisk_note  = cc_asterisk_note,
        cc_detail_note    = cc_detail_note,
        arm_count         = arm_count,
        study_lag         = study_lag,
        sl_custom_ds      = if (!is.null(pp_result)) pp_result$sl_custom_ds else character(0)
      )

      # Step 4: Write OR and RR workbooks
      write_rror_workbooks(
        or_output_file = aeout2_path,
        rr_output_file = aeout3_path,
        reshaped_data  = rror_reshaped,
        lib_metadata   = lib_metadata,
        arm_count      = arm_count,
        arm_names      = arm_display_names,
        arm_display    = arm_display_names,
        pp_result      = pp_result
      )

      rror_result <- list(
        compute  = rror_compute,
        reshaped = rror_reshaped
      )

      if (verbose) cli::cli_alert_info("OR/RR workbooks written successfully")

    } else {
      # Single-arm study: cannot compute pairwise comparisons
      # SAS lines 281-286: %error_summary for both aeout2 and aeout3
      if (verbose) {
        cli::cli_warn("Single-arm study: skipping OR/RR analysis (arm_count={arm_count})")
      }

      error_summary(
        err_file    = aeout2_path,
        panel_title = panel_title,
        ndabla      = ndabla,
        studyid     = studyid,
        err_seterr  = FALSE,
        err_desc    = "The Adverse Events Odds Ratio Analysis could not be run because this study contained only one arm",
        panel_desc  = panel_desc
      )

      error_summary(
        err_file    = aeout3_path,
        panel_title = panel_title,
        ndabla      = ndabla,
        studyid     = studyid,
        err_seterr  = FALSE,
        err_desc    = "The Adverse Events Relative Risk Analysis could not be run because this study contained only one arm",
        panel_desc  = panel_desc
      )
    }

  } else {
    # ============================================================================
    # Phase 2.9: Setup failure path
    # ============================================================================
    # SAS lines 290-295:
    #   %else %do;
    #     %error_summary(err_file=&errout.,
    #                    err_nosubj=%sysfunc(ifc(&dm_subj_gt0.,0,1)),
    #                    err_missvar=%sysfunc(ifc(&setup_req_var.,0,1)));
    #   %end;
    if (verbose) {
      cli::cli_warn("Setup validation failed — writing error summary workbook")
    }

    error_summary(
      err_file        = errout_path,
      panel_title     = panel_title,
      ndabla          = ndabla,
      studyid         = studyid,
      err_nosubj      = !setup_result$dm_subj_gt0,
      err_missvar     = !setup_result$setup_req_var,
      err_seterr      = TRUE,
      rpt_chk_var_req = setup_result$rpt_chk_var_req,
      panel_desc      = panel_desc
    )
  }

  # ============================================================================
  # Phase 2.10: Return value
  # ============================================================================
  invisible(list(
    success = setup_result$setup_success,
    ab_a    = ab_a_output,
    ab_b    = ab_b_output,
    cd_c    = cd_c_output,
    cd_d    = cd_d_output,
    rror    = rror_result,
    setup   = setup_result
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %params(in_*=...) keyword arguments mapped to R function arguments
#      with matching defaults:
#        in_panel_title  -> panel_title = "AE Severity"
#        in_panel_desc   -> panel_desc = ""
#        in_outpath      -> outpath = "."
#        in_ndabla       -> ndabla = ""
#        in_studyid      -> studyid = "CDISC" (key difference from ae_v1.R)
#        in_studylag     -> study_lag = 120
#        in_cc           -> cc = 0
#    - SAS options mlogic symbolgen -> verbose flag with cli diagnostic output
#    - SAS Script Launcher integration path (lines 176-199) removed — R uses
#      direct function calls. If Script Launcher R integration is needed, it
#      would call ae_severity_v1upd(...) directly with named arguments.
#    - SAS in_saspath, in_utilpath, in_templatepath not carried forward — in R,
#      library/source paths are handled by source() calls, not user configuration
#    - SAS run_location detection (line 84: %sysfunc(ifc(not %symexist(run_location)...)))
#      is not needed in R — all parameters are function arguments
#    - SAS template file copying (lines 149-152) not needed — R creates workbooks
#      directly via openxlsx
#    - SAS dummy sl_datasets/sl_group/sl_subset (lines 160-172) replaced by
#      NULL arguments to group_subset_pp()
#    - AE optional variable flags (ae_aeser, ae_aesev) derived from column
#      presence in ds_base returned by setup_validation(), matching SAS global
#      macro variable pattern
#    - arm_subjcnt vector constructed from all_dm_ex grouped by arm_num, with
#      total appended as last element for ae_ab() compatibility
#    - Missing values: NA throughout (never 0, never NaN). NA_character_ for
#      character missing. No implicit zero substitution.
#    - All rounding uses janitor::round_half_up() via downstream dependency
#      functions (ae_aggregate, ae_rror) for SAS-compatible half-up behavior
#    - Sort + dedup via dplyr::arrange() %>% distinct(.keep_all = TRUE) matches
#      SAS PROC SORT NODUPKEY semantics
#    - Excel output uses openxlsx .xlsx format (not SAS SpreadsheetML .xls)
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Rounding: janitor::round_half_up() matches SAS round-half-up behavior
#      but floating-point representation may cause edge-case differences at
#      the 15th decimal place
#    - Fisher's exact p-values: R stats::fisher.test() uses different algorithm
#      than SAS PROC FREQ EXACT FISHER; results may differ at 5+ decimal places
#    - Continuity correction reciprocal: arm-based CC computes 1/arm_count which
#      in R is IEEE 754 double division; SAS uses identical precision but the
#      accumulated product may differ by machine epsilon
#    - Sort stability: dplyr::arrange() is stable within groups; SAS PROC SORT
#      is stable by default — results should be identical for unique keys but
#      may differ for ties on non-key columns
#
# NO DIRECT R EQUIVALENT:
#    - SAS options mlogic symbolgen -> verbose flag with cli diagnostic output
#    - SAS %symexist, %symglobl -> not needed (R function scoping handles this)
#    - SAS Script Launcher mode detection (lines 83-85) -> not applicable in R
#    - SAS PCFILES/JET engine -> openxlsx direct file I/O
#    - SAS SpreadsheetML XML generation -> openxlsx workbook API
#    - SAS template file copying (x "copy ...") -> not needed (direct creation)
#
# PACKAGE SELECTION RATIONALE:
#    - haven: SAS XPT I/O (mandated by AAP for SAS data access)
#    - dplyr: Core tidyverse data manipulation (mandated: no base R equivalents)
#    - tidyr: Reshaping operations (mandated over base R reshape)
#    - stringr: String manipulation (mandated over base R string functions)
#    - cli: User-facing messages (standard R diagnostic package)
#    - janitor: SAS-compatible rounding (round_half_up) — critical for
#      regulatory submission parity
#    - openxlsx: Excel output replacing SAS SpreadsheetML (mandated by AAP)
#
# OPEN QUESTIONS:
#    - Should ae_v1.R and ae_v1upd.R be formally documented as v1->v1upd lineage?
#    - Script Launcher R equivalent: if implemented, how does it pass params?
#    - Is studyid="CDISC" default appropriate for all deployment contexts?
#    - Should the verbose output include timing information for performance
#      profiling of large studies?
# ============================================================
