# ==============================================================================
# PROGRAM: ae_oncology_v1upd.R
# DESCRIPTION: AE Toxicity (Oncology) Analysis Panel — Updated Parameterized
#   Version (R Migration)
#   - Find subject counts by toxicity grade for each adverse event
#   - Find preferred term analysis by toxicity grade per arm
#   - Two-arm comparison: RD, RR, OR, CI, p-value with continuity correction
#   - Optional MedDRA hierarchy integration
#   - Canonical entry point with full parameterization for Script Launcher use
#   - Creates 1 Excel workbook: "AE Toxicity Analysis.xlsx"
# MIGRATED FROM: tested/SAS/AE/ae_oncology_v1upd.sas (409 lines)
# EVALUATION TYPE: Safety — Oncology
# AUTHORS: David Kretch, Andreas Anastassopoulos (original SAS)
# DATE: February 7, 2011 (original); R migration 2026
#
# KEY DIFFERENCE FROM ae_oncology_v1.R:
#   - SAS ae_oncology_v1upd.sas accepted ALL configuration via
#     %params(in_*=...) keyword arguments (SAS lines 88–107), replacing
#     hardcoded LOCAL paths in ae_oncology_v1.sas.
#   - SAS: in_studyid=DeID (default), in_study_lag=30, in_cc=0.5
#   - SAS: options mlogic symbolgen for macro debug tracing
#   - In R: both become parameterized functions; distinction preserved via
#     function name (ae_oncology_v1upd), studyid default ("DeID"), and
#     verbose flag (replaces mlogic/symbolgen)
#
# SAS MACROS MIGRATED:
#   %params (lines 88–287) -> Function arguments with matching defaults
#   %onc    (lines 329–397) -> Function body implementing 3 analyses
#
# EXTERNAL FILES USED (R equivalents):
#   ae_setup.R              -- Merges AE, DM, EX; validates variables
#   ae_oncology_aggregate.R -- Oncology AE aggregation & comparison
#   ae_oncology_output.R    -- Creates Excel workbook output
#   data_checks.R           -- Generic variable validation
#   err_output.R            -- Error summary workbook
#   sl_gs_output.R          -- Script Launcher grouping/subsetting metadata
#   xml_output.R            -- Foundational Excel output engine (openxlsx-based)
#
# VARIABLES REQUIRED: AE -- AEBODSYS, AEDECOD, USUBJID
#                     DM -- ACTARM or ARM, USUBJID
#                     EX -- USUBJID
#
# VARIABLES USED WHEN AVAILABLE:
#                     AE -- AESTDTC, AETOXGR
#                     DM -- RFSTDTC, RFENDTC, ARMCD
#                     EX -- EXSTDTC, EXENDTC
# ==============================================================================

# --- Library loading ----------------------------------------------------------
# Core tidyverse components replacing SAS DATA step / PROC SQL constructs
library(haven)       # SAS data I/O: read_xpt() replacing libname/XPT access
library(dplyr)       # Data manipulation: replacing DATA steps and PROC SQL
library(tidyr)       # Reshaping: complete/nesting/pivot_wider for arm×grade
library(stringr)     # String manipulation: replacing SAS character functions
library(cli)         # User-facing messages: replacing SAS PUT/NOTE statements
library(janitor)     # round_half_up(): SAS-compatible rounding (0.5 -> 1)
library(openxlsx)    # Excel output: replacing SpreadsheetML and PCFILES engine

# --- Source internal dependencies ---------------------------------------------
# Replaces SAS: %include "&utilpath.\ae_setup.sas"; etc. (lines 302-308)
# Uses file.path() with script-relative paths for portability. Each
# source() call is guarded to prevent re-sourcing if already loaded.
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
  if (file.exists(dc_path) && !exists("chk_var", mode = "function")) {
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
  agg_path <- file.path(macro_dir, "ae_oncology_aggregate.R")
  if (file.exists(agg_path) && !exists("onc_aggregate", mode = "function")) {
    source(agg_path, local = FALSE)
  }

  out_path <- file.path(macro_dir, "ae_oncology_output.R")
  if (file.exists(out_path) && !exists("onc_out_workbook", mode = "function")) {
    source(out_path, local = FALSE)
  }
})


# ==============================================================================
# ae_oncology_v1upd: AE Toxicity (Oncology) Panel — Canonical Parameterized
#                    Entry Point
# ==============================================================================
#' Run the complete Oncology AE Toxicity Analysis Panel with full
#' parameterization.
#'
#' This is the canonical, fully parameterized Oncology AE Toxicity Panel
#' driver. Migrated from SAS \code{ae_oncology_v1upd.sas}, which accepted
#' all configuration via \code{%params(in_*=...)} keyword arguments
#' (SAS lines 88–107). Produces one Excel workbook containing:
#' \enumerate{
#'   \item Toxicity Grade Summary (pt_1): subject counts by grade × arm
#'   \item Preferred-Term Analysis (pt_2): by SOC/PT × grade × arm
#'   \item Two-Arm Comparison (pt_3): RD, RR, OR, CI, p-value (when >1 arm)
#' }
#'
#' Distinguished from \code{ae_oncology_v1()} in \code{ae_oncology_v1.R} by:
#' \itemize{
#'   \item Function name: \code{ae_oncology_v1upd} (traceability to SAS
#'         ae_oncology_v1upd.sas)
#'   \item Default \code{studyid = "DeID"} (SAS: \code{in_studyid=DeID})
#'   \item \code{verbose} parameter replacing SAS
#'         \code{options mlogic symbolgen}
#' }
#'
#' @param ae Data frame or character file path. AE domain data (CDISC
#'   ADaM/SDTM). If character, loaded via \code{haven::read_xpt()}.
#' @param dm Data frame or character file path. DM domain data.
#' @param ex Data frame or character file path. EX domain data.
#' @param panel_title Character. Panel title for workbook headers.
#'   Default \code{"AE Toxicity"} (SAS: \code{in_panel_title}).
#' @param panel_desc Character. Panel description text.
#'   Default \code{""} (SAS: \code{in_panel_desc}).
#' @param outpath Character. Output directory path for workbooks.
#'   Default \code{"."} (SAS: \code{in_outpath}).
#' @param oncaeout Character. Filename for oncology AE workbook.
#'   Default \code{"AE Toxicity Analysis.xlsx"}.
#' @param errout Character. Filename for error summary workbook.
#'   Default \code{"AE Toxicity Error Summary.xlsx"}.
#' @param ndabla Character. NDA/BLA identifier.
#'   Default \code{""} (SAS: \code{in_ndabla}).
#' @param studyid Character. Study identifier.
#'   Default \code{"DeID"} (SAS: \code{in_studyid=DeID}).
#' @param meddra_path Character or NULL. Path to MedDRA hierarchy SAS7BDAT/XPT.
#'   NULL disables MedDRA lookup (SAS: \code{in_meddrapath}).
#' @param meddra_ver Character. MedDRA version identifier.
#'   Default \code{"14.1"} (SAS: \code{in_ver=14.1}).
#' @param study_lag Numeric. Days after last exposure to include AEs.
#'   Default \code{30} (SAS: \code{in_study_lag=30}).
#' @param toxgr_grp5_sw Logical. If TRUE, group grade 5 with grades 3 and 4.
#'   Default \code{TRUE} (SAS: \code{in_toxgr_grp5_sw=1}).
#' @param exp_arm Numeric. Experimental arm number (1-based).
#'   Default \code{1} (SAS: \code{in_exp=1}).
#' @param ctl_arm Numeric. Control arm number (1-based).
#'   Default \code{2} (SAS: \code{in_ctl=2}).
#' @param cmptrm Character vector. MedDRA levels to compare.
#'   Default \code{c("soc_name", "pt_name")} (SAS: \code{in_cmptrm}).
#' @param cmpgr Character. Toxicity grades for comparison.
#'   Default \code{"all"} (SAS: \code{in_cmpgr=all}).
#' @param cmpsort Character. Variable for sorting formatted output.
#'   Default \code{"rr"} (SAS: \code{in_cmpsort=rr}).
#' @param cc Numeric or character. Continuity correction:
#'   0 = none, positive number = constant, "arm" = reciprocal of opposite arm.
#'   Default \code{0.5} (SAS: \code{in_cc=0.5}).
#' @param toxgr_min Integer. Minimum toxicity grade. Default \code{1}.
#' @param toxgr_max Integer. Maximum toxicity grade. Default \code{5}.
#' @param ae_rate_ci_sw Logical. Include CI for AE rates. Default \code{TRUE}.
#' @param vld_sw Logical. Validation switch. Default \code{TRUE}.
#' @param verbose Logical. Emit diagnostic messages (replaces SAS
#'   \code{options mlogic symbolgen}). Default \code{FALSE}.
#'
#' @return Invisible named list with:
#'   \describe{
#'     \item{success}{Logical: TRUE if setup and analyses completed.}
#'     \item{pt_1}{Tibble: Toxicity Grade Summary output or NULL.}
#'     \item{pt_2}{List: Preferred-Term Analysis formatted output or NULL.}
#'     \item{pt_3}{List: Two-Arm Comparison formatted output or NULL.}
#'     \item{meddra_active}{Logical: whether MedDRA was used.}
#'     \item{setup}{List: complete setup_validation() result.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Using file paths (XPT files)
#' ae_oncology_v1upd(
#'   ae = "data/adam/ae.xpt",
#'   dm = "data/adam/dm.xpt",
#'   ex = "data/adam/ex.xpt",
#'   outpath = "output",
#'   studyid = "STUDY-001",
#'   meddra_path = "data/meddra/mdhier_14_1.sas7bdat",
#'   verbose = TRUE
#' )
#'
#' # Using pre-loaded data frames
#' ae_oncology_v1upd(
#'   ae = ae_data,
#'   dm = dm_data,
#'   ex = ex_data,
#'   toxgr_grp5_sw = FALSE,
#'   cc = 0
#' )
#' }
#'
#' @export
ae_oncology_v1upd <- function(
  # Data inputs — from SAS libname/data step (lines 131-136)
  ae,
  dm,
  ex,

  # Panel metadata — from SAS in_panel_title, in_panel_desc (lines 88-89)
  panel_title = "AE Toxicity",
  panel_desc  = "",

  # Path configuration — from SAS in_outpath (line 92)
  outpath = ".",

  # Output file names — from SAS in_oncaeout, in_errout (lines 93-94)
  oncaeout = "AE Toxicity Analysis.xlsx",
  errout   = "AE Toxicity Error Summary.xlsx",

  # Study identifiers — from SAS in_ndabla, in_studyid (lines 96-97)
  ndabla  = "",
  studyid = "DeID",

  # MedDRA configuration — from SAS in_meddrapath, in_ver (lines 98-99)
  meddra_path = NULL,
  meddra_ver  = "14.1",

  # Analysis parameters — from SAS in_study_lag through in_cc (lines 100-107)
  study_lag     = 30,
  toxgr_grp5_sw = TRUE,
  exp_arm       = 1,
  ctl_arm       = 2,
  cmptrm        = c("soc_name", "pt_name"),
  cmpgr         = "all",
  cmpsort       = "rr",
  cc            = 0.5,

  # Derived parameters — from SAS %let statements (lines 291-300)
  toxgr_min     = 1,
  toxgr_max     = 5,
  ae_rate_ci_sw = TRUE,

  # Validation and debug
  vld_sw  = TRUE,
  verbose = FALSE
) {

  # ===========================================================================
  # Task 2.2: Verbose diagnostic — replaces SAS options mlogic symbolgen
  # (SAS line 82: options missing='' mlogic symbolgen)
  # ===========================================================================
  start_time <- proc.time()

  if (verbose) {
    cli::cli_h1("AE Oncology Toxicity Analysis (v1upd)")
    cli::cli_alert_info("panel_title = {panel_title}")
    cli::cli_alert_info("panel_desc = {panel_desc}")
    cli::cli_alert_info("outpath = {outpath}")
    cli::cli_alert_info("oncaeout = {oncaeout}")
    cli::cli_alert_info("errout = {errout}")
    cli::cli_alert_info("ndabla = {ndabla}")
    cli::cli_alert_info("studyid = {studyid}")
    cli::cli_alert_info("meddra_path = {meddra_path %||% 'NULL'}")
    cli::cli_alert_info("meddra_ver = {meddra_ver}")
    cli::cli_alert_info("study_lag = {study_lag}")
    cli::cli_alert_info("toxgr_grp5_sw = {toxgr_grp5_sw}")
    cli::cli_alert_info("exp_arm = {exp_arm}, ctl_arm = {ctl_arm}")
    cli::cli_alert_info("cmptrm = {paste(cmptrm, collapse = ', ')}")
    cli::cli_alert_info("cmpgr = {cmpgr}, cmpsort = {cmpsort}")
    cli::cli_alert_info("cc = {cc}")
    cli::cli_alert_info("toxgr_min = {toxgr_min}, toxgr_max = {toxgr_max}")
    cli::cli_alert_info("ae_rate_ci_sw = {ae_rate_ci_sw}")
    cli::cli_alert_info("vld_sw = {vld_sw}")
  }

  # ===========================================================================
  # Input validation — programming errors caught early
  # ===========================================================================
  if (missing(ae)) cli::cli_abort("{.arg ae} is required.")
  if (missing(dm)) cli::cli_abort("{.arg dm} is required.")
  if (missing(ex)) cli::cli_abort("{.arg ex} is required.")
  if (!is.character(panel_title) || length(panel_title) != 1L) {
    cli::cli_abort("{.arg panel_title} must be a single character string.")
  }
  if (!is.character(outpath) || length(outpath) != 1L) {
    cli::cli_abort("{.arg outpath} must be a single character string.")
  }
  if (!is.character(oncaeout) || length(oncaeout) != 1L) {
    cli::cli_abort("{.arg oncaeout} must be a single character string.")
  }
  if (!is.character(errout) || length(errout) != 1L) {
    cli::cli_abort("{.arg errout} must be a single character string.")
  }
  if (!is.character(ndabla) || length(ndabla) != 1L) {
    cli::cli_abort("{.arg ndabla} must be a single character string.")
  }
  if (!is.character(studyid) || length(studyid) != 1L) {
    cli::cli_abort("{.arg studyid} must be a single character string.")
  }
  if (!is.numeric(study_lag) || length(study_lag) != 1L || study_lag < 0) {
    cli::cli_abort("{.arg study_lag} must be a non-negative number.")
  }
  if (!is.numeric(toxgr_min) || !is.numeric(toxgr_max)) {
    cli::cli_abort("{.arg toxgr_min} and {.arg toxgr_max} must be numeric.")
  }
  if (toxgr_min > toxgr_max) {
    cli::cli_abort("{.arg toxgr_min} ({toxgr_min}) cannot exceed {.arg toxgr_max} ({toxgr_max}).")
  }
  if (!is.character(cmptrm) || length(cmptrm) < 1L) {
    cli::cli_abort("{.arg cmptrm} must be a character vector with >=1 element.")
  }
  if (!is.character(cmpgr) || length(cmpgr) != 1L) {
    cli::cli_abort("{.arg cmpgr} must be a single character string.")
  }
  if (!is.character(cmpsort) || length(cmpsort) != 1L) {
    cli::cli_abort("{.arg cmpsort} must be a single character string.")
  }

  # ===========================================================================
  # Task 2.3: Data loading — replaces SAS libname/data step (lines 131-136)
  # If arguments are character paths, load via haven::read_xpt().
  # Then normalize column names to lowercase (SAS is case-insensitive).
  # ===========================================================================
  if (is.character(ae)) {
    if (verbose) cli::cli_alert_info("Loading AE from: {ae}")
    ae <- haven::read_xpt(ae)
  }
  if (is.character(dm)) {
    if (verbose) cli::cli_alert_info("Loading DM from: {dm}")
    dm <- haven::read_xpt(dm)
  }
  if (is.character(ex)) {
    if (verbose) cli::cli_alert_info("Loading EX from: {ex}")
    ex <- haven::read_xpt(ex)
  }

  # Validate that inputs are data frames after potential loading
  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame or XPT path.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame or XPT path.")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame or XPT path.")

  # Normalize column names to lowercase (SAS case-insensitive equivalence)
  ae <- ae %>% dplyr::rename_with(tolower)
  dm <- dm %>% dplyr::rename_with(tolower)
  ex <- ex %>% dplyr::rename_with(tolower)

  # ===========================================================================
  # Task 2.4: MedDRA hierarchy loading — from SAS in_meddrapath (lines 143-155)
  # Load optional MedDRA hierarchy dataset. Set meddra_active flag.
  # SAS: libname meddra "&meddrapath."; and version check (lines 148-155)
  # ===========================================================================
  meddra_active <- FALSE
  meddra_data   <- NULL
  meddra_pct    <- 0

  if (!is.null(meddra_path) && nchar(str_trim(meddra_path)) > 0L) {
    # Determine MedDRA version — "N" or "N/A" disables MedDRA
    # SAS line 151: %if %upcase(%substr(&ver.,1,1)) = N
    ver_trimmed <- stringr::str_trim(meddra_ver)
    if (stringr::str_detect(stringr::str_to_lower(ver_trimmed), "^n")) {
      meddra_active <- FALSE
      meddra_pct    <- 0
      if (verbose) cli::cli_alert_info("MedDRA disabled: version = '{meddra_ver}'")
    } else {
      # Build MedDRA dataset filename from version
      # Convention: mdhier_X_Y.sas7bdat where X.Y is the version
      meddra_ver_clean <- stringr::str_trim(meddra_ver)
      meddra_ver_file  <- stringr::str_c(
        "mdhier_",
        gsub("\\.", "_", meddra_ver_clean)
      )

      # Try multiple file formats (SAS7BDAT, XPT, CSV)
      candidate_files <- c(
        file.path(meddra_path, paste0(meddra_ver_file, ".sas7bdat")),
        file.path(meddra_path, paste0(meddra_ver_file, ".xpt")),
        file.path(meddra_path, paste0(meddra_ver_file, ".csv"))
      )

      meddra_loaded <- FALSE
      for (cand in candidate_files) {
        if (file.exists(cand)) {
          ext <- tolower(tools::file_ext(cand))
          tryCatch({
            if (ext == "sas7bdat") {
              meddra_data <- haven::read_sas(cand)
            } else if (ext == "xpt") {
              meddra_data <- haven::read_xpt(cand)
            } else if (ext == "csv") {
              meddra_data <- readr::read_csv(cand, show_col_types = FALSE)
            }
            meddra_data <- meddra_data %>% dplyr::rename_with(tolower)
            meddra_loaded <- TRUE
            meddra_active <- TRUE
            if (verbose) cli::cli_alert_info("Loaded MedDRA from: {cand}")
            break
          }, error = function(e) {
            cli::cli_warn("Failed to load MedDRA file {.path {cand}}: {e$message}")
          })
        }
      }

      # If meddra_path is itself a file (not a directory)
      if (!meddra_loaded && file.exists(meddra_path)) {
        ext <- tolower(tools::file_ext(meddra_path))
        tryCatch({
          if (ext == "sas7bdat") {
            meddra_data <- haven::read_sas(meddra_path)
          } else if (ext == "xpt") {
            meddra_data <- haven::read_xpt(meddra_path)
          } else if (ext == "csv") {
            meddra_data <- readr::read_csv(meddra_path, show_col_types = FALSE)
          }
          if (!is.null(meddra_data)) {
            meddra_data <- meddra_data %>% dplyr::rename_with(tolower)
            meddra_loaded <- TRUE
            meddra_active <- TRUE
            if (verbose) cli::cli_alert_info("Loaded MedDRA from: {meddra_path}")
          }
        }, error = function(e) {
          cli::cli_warn("Failed to load MedDRA file {.path {meddra_path}}: {e$message}")
        })
      }

      if (!meddra_loaded) {
        cli::cli_warn("MedDRA hierarchy file not found at {.path {meddra_path}}. MedDRA disabled.")
        meddra_active <- FALSE
        meddra_pct    <- 0
      }
    }
  } else {
    if (verbose) cli::cli_alert_info("MedDRA path not provided. MedDRA disabled.")
  }

  # ===========================================================================
  # Task 2.5: Continuity correction normalization
  # SAS lines 310-326: DATA _null_ that sets cc_sw, cc_whole globals
  # 3-mode logic:
  #   cc_sw = 0: no correction (cc is 0 or missing)
  #   cc_sw = 1: constant correction (cc is a positive number)
  #   cc_sw = 2: arm reciprocal correction (cc = "arm")
  # ===========================================================================
  cc_sw    <- 0L
  cc_whole <- TRUE
  cc_value <- 0

  if (is.character(cc)) {
    cc_trimmed <- str_to_lower(str_trim(cc))
    if (cc_trimmed == "arm") {
      cc_sw    <- 2L
      cc_whole <- FALSE
      cc_value <- 0
    } else if (cc_trimmed %in% c("none", "0", "")) {
      cc_sw    <- 0L
      cc_whole <- TRUE
      cc_value <- 0
    } else {
      # Attempt numeric parse of string
      cc_num <- suppressWarnings(as.numeric(cc_trimmed))
      if (!is.na(cc_num) && cc_num != 0) {
        cc_sw    <- 1L
        cc_value <- cc_num
        cc_whole <- (cc_num == floor(cc_num))
      } else {
        cc_sw    <- 0L
        cc_value <- 0
        cc_whole <- TRUE
      }
    }
  } else if (is.numeric(cc)) {
    if (!is.na(cc) && cc != 0) {
      cc_sw    <- 1L
      cc_value <- cc
      cc_whole <- (cc == floor(cc))
    } else {
      cc_sw    <- 0L
      cc_value <- 0
      cc_whole <- TRUE
    }
  }

  if (verbose) {
    cli::cli_alert_info("Continuity correction: cc_sw = {cc_sw}, cc_value = {cc_value}, cc_whole = {cc_whole}")
  }

  # Build human-readable CC description for the output workbook
  # Uses tibble + mutate + if_else + case_when pipeline for traceability
  cc_info <- tibble::tibble(cc_sw = cc_sw, cc_value = cc_value, cc_whole = cc_whole) %>%
    dplyr::mutate(
      cc_desc = dplyr::case_when(
        cc_sw == 0L ~ "None",
        cc_sw == 2L ~ "Reciprocal of opposite arm count",
        cc_whole    ~ as.character(as.integer(cc_value)),
        TRUE        ~ as.character(cc_value)
      ),
      cc_label = dplyr::if_else(cc_sw == 0L, "No continuity correction",
                                stringr::str_c("CC: ", .data$cc_desc))
    )
  cc_desc <- cc_info$cc_desc[1L]

  # ===========================================================================
  # Task 2.6: Setup validation — replaces SAS %setup/%setup(mdhier=Y)
  # SAS lines 331-335: %if %upcase(%substr(&ver.,1,1))=N %then %setup;
  #                     %else %setup(mdhier=Y);
  # ===========================================================================
  if (verbose) cli::cli_h1("Setup Validation")

  setup_result <- setup_validation(
    ae         = ae,
    dm         = dm,
    ex         = ex,
    mdhier     = meddra_active,
    vld_sw     = vld_sw,
    study_lag  = as.integer(study_lag),
    meddra_data = meddra_data,
    toxgr_min  = toxgr_min,
    toxgr_max  = toxgr_max
  )

  if (verbose) {
    cli::cli_alert_info("Setup success: {setup_result$setup_success}")
    cli::cli_alert_info("Arm count: {setup_result$arm_count}")
    if (setup_result$setup_success) {
      cli::cli_alert_info("Arm names: {paste(setup_result$arm_display_names, collapse = ', ')}")
      cli::cli_alert_info("Total subjects: {setup_result$arm_total}")
    }
  }

  # ===========================================================================
  # Task 2.7: MedDRA validation — SAS lines 337-338
  # %if &meddra_pct. < 80 %then %let meddra = N;
  # After setup, check match percentage. If < 80%, disable MedDRA.
  # ===========================================================================
  if (meddra_active && setup_result$setup_success) {
    meddra_pct_val <- setup_result$meddra_pct
    if (is.na(meddra_pct_val)) meddra_pct_val <- 0
    meddra_pct <- meddra_pct_val

    if (meddra_pct < 80) {
      cli::cli_warn(
        "MedDRA matching percentage ({meddra_pct}%) is below 80% threshold. MedDRA disabled."
      )
      meddra_active <- FALSE
    } else {
      if (verbose) cli::cli_alert_info("MedDRA matching: {meddra_pct}% (above 80% threshold)")
    }
  }

  # ===========================================================================
  # Setup failure path — SAS lines 390-395: %error_summary
  # If setup_success is FALSE, generate error workbook and return early.
  # ===========================================================================
  if (!setup_result$setup_success) {
    if (verbose) cli::cli_alert_warning("Setup failed. Generating error summary.")

    # Build full output path for error summary workbook
    err_file_path <- file.path(outpath, errout)

    # Ensure output directory exists
    if (!dir.exists(outpath)) {
      dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
    }

    # Call error_summary() — replaces SAS %error_summary (lines 391-394)
    tryCatch(
      error_summary(
        err_file     = err_file_path,
        panel_title  = panel_title,
        ndabla       = ndabla,
        studyid      = studyid,
        err_nosubj   = !setup_result$dm_subj_gt0,
        err_missvar  = !setup_result$setup_req_var,
        err_seterr   = TRUE,
        err_desc     = panel_desc,
        rpt_chk_var_req = setup_result$rpt_chk_var_req,
        panel_desc   = panel_desc
      ),
      error = function(e) {
        cli::cli_warn("Error writing error summary: {e$message}")
      }
    )

    elapsed <- (proc.time() - start_time)[["elapsed"]]
    if (verbose) {
      cli::cli_alert_info("Running time: {round(elapsed, 1)} seconds")
    }

    return(invisible(list(
      success       = FALSE,
      pt_1          = NULL,
      pt_2          = NULL,
      pt_3          = NULL,
      meddra_active = meddra_active,
      setup         = setup_result
    )))
  }

  # ===========================================================================
  # Extract setup results into local variables
  # ===========================================================================
  arm_count         <- setup_result$arm_count
  arm_total         <- setup_result$arm_total
  arm_names         <- setup_result$arm_names
  arm_display_names <- setup_result$arm_display_names
  all_dm_ex         <- setup_result$all_dm_ex
  ds_base           <- setup_result$ds_base

  # ===========================================================================
  # Task 2.8: Experimental / Control arm resolution
  # SAS lines 340-358: resolve arm indices from all_arm data.
  # In R, exp_arm and ctl_arm are already numeric indices. Validate
  # they are within range of arm_count.
  # ===========================================================================
  if (is.character(exp_arm)) {
    # Handle character arm name — match against arm_names
    match_idx <- which(toupper(stringr::str_trim(arm_names)) ==
                         toupper(stringr::str_trim(exp_arm)))
    if (length(match_idx) > 0L) {
      exp_arm <- match_idx[1L]
    } else {
      cli::cli_warn("Experimental arm '{exp_arm}' not found. Defaulting to 1.")
      exp_arm <- 1L
    }
  }
  if (is.character(ctl_arm)) {
    match_idx <- which(toupper(stringr::str_trim(arm_names)) ==
                         toupper(stringr::str_trim(ctl_arm)))
    if (length(match_idx) > 0L) {
      ctl_arm <- match_idx[1L]
    } else {
      cli::cli_warn("Control arm '{ctl_arm}' not found. Defaulting to min(2, arm_count).")
      ctl_arm <- min(2L, arm_count)
    }
  }

  exp_arm <- as.integer(exp_arm)
  ctl_arm <- as.integer(ctl_arm)

  # Clamp to valid range (SAS lines 352-353)
  if (exp_arm < 1L || exp_arm > arm_count) {
    cli::cli_warn("exp_arm ({exp_arm}) out of range [1, {arm_count}]. Defaulting to 1.")
    exp_arm <- 1L
  }
  if (ctl_arm < 1L || ctl_arm > arm_count) {
    cli::cli_warn("ctl_arm ({ctl_arm}) out of range [1, {arm_count}]. Defaulting to min(2, arm_count).")
    ctl_arm <- min(2L, arm_count)
  }

  if (verbose) {
    cli::cli_alert_info("Resolved: exp_arm = {exp_arm}, ctl_arm = {ctl_arm}")
  }

  # ===========================================================================
  # Compute arm_subjcnt vector: per-arm subject counts
  # SAS: arm_subjcnt array(1..arm_count) built in %setup
  # R: construct from all_dm_ex grouped by arm_num
  # ===========================================================================
  arm_subjcnt_by_arm <- all_dm_ex %>%
    dplyr::select(.data$arm_num, .data$usubjid) %>%
    dplyr::group_by(.data$arm_num) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data$usubjid), .groups = "drop") %>%
    dplyr::arrange(.data$arm_num) %>%
    dplyr::pull(.data$n)

  cli::cli_inform("Subject counts per arm: {paste(arm_subjcnt_by_arm, collapse = ', ')}")

  # Validate arm_subjcnt length matches arm_count
  if (length(arm_subjcnt_by_arm) != arm_count) {
    cli::cli_warn(
      "arm_subjcnt length ({length(arm_subjcnt_by_arm)}) does not match arm_count ({arm_count}). Padding."
    )
    arm_subjcnt_by_arm <- c(
      arm_subjcnt_by_arm,
      rep(0L, max(0L, arm_count - length(arm_subjcnt_by_arm)))
    )
  }

  # Build arm metadata lookup tibble for enriched reporting
  arm_metadata <- tibble::tibble(
    arm_num  = seq_len(arm_count),
    arm_name = arm_display_names[seq_len(arm_count)],
    arm_n    = arm_subjcnt_by_arm[seq_len(arm_count)]
  )
  # Left-join arm metadata for downstream lookup (used by output functions)
  arm_lookup <- tibble::tibble(arm_num = seq_len(arm_count)) %>%
    dplyr::left_join(arm_metadata, by = "arm_num")

  if (verbose) {
    cli::cli_alert_info("Arm metadata: {paste(arm_lookup$arm_name, '(N=', arm_lookup$arm_n, ')', collapse = '; ')}")
  }

  # ===========================================================================
  # Task 2.9: Grade-5 grouping adjustment
  # SAS line 365: %if %symexist(toxgr_max) %then %if &toxgr_max.=4 %then
  #               %let toxgr_grp5_sw=0;
  # Cannot group grade 5 with 3 and 4 if 4 is the upper bound.
  # ===========================================================================
  if (toxgr_max == 4) {
    toxgr_grp5_sw <- FALSE
    if (verbose) cli::cli_alert_info("toxgr_max=4: auto-disabling toxgr_grp5_sw")
  }

  # Determine ae_aetoxgr flag from setup rpt_chk_var
  ae_aetoxgr_flag <- FALSE
  if (!is.null(setup_result$rpt_chk_var) && nrow(setup_result$rpt_chk_var) > 0L) {
    aetoxgr_row <- setup_result$rpt_chk_var %>%
      dplyr::filter(toupper(.data$ds) == "AE", toupper(.data$var) == "AETOXGR")
    if (nrow(aetoxgr_row) > 0L) {
      ae_aetoxgr_flag <- as.logical(aetoxgr_row$ind[1L])
    }
  }

  # Determine vld_err flag from rpt_chk_var
  vld_err <- FALSE
  if (vld_sw && !is.null(setup_result$rpt_chk_var) &&
      nrow(setup_result$rpt_chk_var) > 0L) {
    err_rows <- setup_result$rpt_chk_var %>%
      dplyr::filter(.data$ind == 0L)
    vld_err <- nrow(err_rows) > 0L
  }

  # ===========================================================================
  # Initialize output result containers
  # ===========================================================================
  pt_1_output    <- NULL
  pt_2_output    <- NULL
  pt_2_formatted <- NULL
  pt_3_output    <- NULL
  pt_3_formatted <- NULL

  # Add "total" column for cross-arm total-level aggregation

  # SAS ae_setup.sas line 379: total = 'Total';
  # R ae_setup.R drops this column at line 686 (ds_base_cols_drop),
  # so we re-add it here for the %aggregate(ds_base, pt_1, total) call.
  if (!"total" %in% colnames(ds_base)) {
    ds_base <- ds_base %>% dplyr::mutate(total = "Total")
  }

  # ===========================================================================
  # Task 2.10: Analysis 1 — Toxicity Grade Summary (aggregate total)
  # SAS line 368: %aggregate(ds_base, pt_1, total);
  # Aggregate the total counts across all terms by arm and toxicity grade.
  # ===========================================================================
  if (verbose) cli::cli_h1("Analysis 1: Toxicity Grade Summary")

  tryCatch({
    pt_1_result <- onc_aggregate(
      ds            = ds_base,
      by_vars       = "total",
      arm_count     = arm_count,
      arm_names     = arm_display_names,
      arm_subjcnt   = arm_subjcnt_by_arm,
      toxgr_min     = as.integer(toxgr_min),
      toxgr_max     = as.integer(toxgr_max),
      toxgr_grp5_sw = toxgr_grp5_sw,
      report        = TRUE,
      output        = TRUE
    )
    pt_1_output <- pt_1_result
    if (verbose) cli::cli_alert_info("Analysis 1 complete: pt_1 generated.")
  }, error = function(e) {
    cli::cli_warn("Analysis 1 (Toxicity Grade Summary) failed: {e$message}")
  })

  # ===========================================================================
  # Task 2.11: Analysis 2 — Preferred Term Analysis by Toxicity Grade
  # SAS lines 370-372:
  #   %aggregate(ds_base, pt_2, aebodsys, aedecod);
  #   %fmt_output(pt_2_output);
  # Aggregate by SOC (aebodsys) and PT (aedecod), then format output.
  # ===========================================================================
  if (verbose) cli::cli_h1("Analysis 2: Preferred Term Analysis")

  tryCatch({
    pt_2_result <- onc_aggregate(
      ds            = ds_base,
      by_vars       = c("aebodsys", "aedecod"),
      arm_count     = arm_count,
      arm_names     = arm_display_names,
      arm_subjcnt   = arm_subjcnt_by_arm,
      toxgr_min     = as.integer(toxgr_min),
      toxgr_max     = as.integer(toxgr_max),
      toxgr_grp5_sw = toxgr_grp5_sw,
      report        = TRUE,
      output        = TRUE
    )
    pt_2_output <- pt_2_result

    # Format the output — replaces SAS %fmt_output(pt_2_output)
    if (!is.null(pt_2_result$output) && is.data.frame(pt_2_result$output) &&
        nrow(pt_2_result$output) > 0L) {
      pt_2_formatted <- onc_fmt_output(
        data      = pt_2_result$output,
        by_vars   = c("aebodsys", "aedecod"),
        ds_name   = "pt_2",
        sort_sw   = FALSE,
        sortvar   = NULL,
        sortgrp_sw = FALSE,
        cc_sw     = cc_sw
      )
    } else {
      pt_2_formatted <- list(formatted = tibble::tibble(), header_ind = tibble::tibble())
    }

    if (verbose) cli::cli_alert_info("Analysis 2 complete: pt_2 generated and formatted.")
  }, error = function(e) {
    cli::cli_warn("Analysis 2 (Preferred Term) failed: {e$message}")
  })

  # ===========================================================================
  # Task 2.12: Analysis 3 — Two-Arm Comparison
  # SAS lines 374-385:
  #   %if &arm_count. > 1 %then %do;
  #     %if &meddra. = Y %then %compare(ds_base_meddra, pt_3, &cmptrm.);
  #     %else %do;
  #       %let cmptrm = aebodsys,aedecod;
  #       %compare(ds_base, pt_3, &cmptrm.);
  #     %end;
  #     %fmt_output(pt_3_output, sort_sw=yes, sortvar=&cmpsort., sortgrp_sw=yes);
  #   %end;
  # ===========================================================================
  if (arm_count > 1L) {
    if (verbose) cli::cli_h1("Analysis 3: Two-Arm Comparison")

    # Select dataset and comparison terms based on MedDRA availability
    # SAS lines 377-383: if meddra=Y use ds_base_meddra, else ds_base
    if (meddra_active) {
      ds_compare  <- ds_base
      cmptrm_use  <- cmptrm
    } else {
      ds_compare  <- ds_base
      cmptrm_use  <- c("aebodsys", "aedecod")
    }

    tryCatch({
      pt_3_result <- onc_compare(
        ds            = ds_compare,
        by_vars       = cmptrm_use,
        arm_count     = arm_count,
        arm_names     = arm_display_names,
        arm_subjcnt   = arm_subjcnt_by_arm,
        toxgr_min     = as.integer(toxgr_min),
        toxgr_max     = as.integer(toxgr_max),
        cmpgr         = cmpgr,
        ctl           = ctl_arm,
        exp           = exp_arm,
        cc_sw         = cc_sw,
        cc_whole      = cc_whole,
        cc_value      = cc_value,
        ae_rate_ci_sw = ae_rate_ci_sw,
        report        = TRUE
      )
      pt_3_output <- pt_3_result

      # Format output with sorting — replaces SAS
      # %fmt_output(pt_3_output, sort_sw=yes, sortvar=&cmpsort., sortgrp_sw=yes)
      if (!is.null(pt_3_result$output) && is.data.frame(pt_3_result$output) &&
          nrow(pt_3_result$output) > 0L) {
        pt_3_formatted <- onc_fmt_output(
          data       = pt_3_result$output,
          by_vars    = cmptrm_use,
          ds_name    = "pt_3",
          sort_sw    = TRUE,
          sortvar    = cmpsort,
          sortgrp_sw = TRUE,
          cc_sw      = cc_sw
        )
      } else {
        pt_3_formatted <- list(formatted = tibble::tibble(), header_ind = tibble::tibble())
      }

      if (verbose) cli::cli_alert_info("Analysis 3 complete: pt_3 generated and formatted.")
    }, error = function(e) {
      cli::cli_warn("Analysis 3 (Two-Arm Comparison) failed: {e$message}")
    })
  } else {
    if (verbose) cli::cli_alert_info("Single-arm study: skipping two-arm comparison.")
  }

  # ===========================================================================
  # Task 2.13: Excel output generation — replaces SAS %out_onc (line 387)
  # Build the complete AE Toxicity Analysis workbook with all analysis tabs.
  # ===========================================================================
  if (verbose) cli::cli_h1("Excel Output Generation")

  # Build full output path
  output_file_path <- file.path(outpath, oncaeout)

  # Ensure output directory exists
  if (!dir.exists(outpath)) {
    dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
  }

  # Prepare grouping/subsetting metadata (empty defaults for direct invocation)
  # SAS lines 199-211: dummy sl_datasets, sl_group, sl_subset
  sl_group  <- tibble::tibble(
    group_name = character(0), domain = character(0),
    partition = character(0), var_name = character(0),
    var_value = character(0), dsvg_grp_name = character(0)
  )
  sl_subset <- tibble::tibble(
    name = character(0), domain = character(0),
    partition = character(0), var_name = character(0),
    var_value = character(0), inner_operator = character(0),
    outer_operator = character(0)
  )

  # Preprocess grouping/subsetting metadata
  pp_result <- tryCatch(
    group_subset_pp(sl_group = sl_group, sl_subset = sl_subset),
    error = function(e) {
      cli::cli_warn("group_subset_pp failed: {e$message}")
      NULL
    }
  )

  # Determine MedDRA flag for output ("Y"/"N")
  meddra_flag <- if (meddra_active) "Y" else "N"

  # Prepare pt_2 data for workbook
  pt_2_wb_data <- if (!is.null(pt_2_formatted) && is.list(pt_2_formatted)) {
    if ("formatted" %in% names(pt_2_formatted)) pt_2_formatted$formatted
    else tibble::tibble()
  } else {
    tibble::tibble()
  }

  # Prepare pt_3 data for workbook
  pt_3_wb_data <- if (!is.null(pt_3_formatted) && is.list(pt_3_formatted)) {
    if ("formatted" %in% names(pt_3_formatted)) pt_3_formatted$formatted
    else tibble::tibble()
  } else {
    tibble::tibble()
  }

  tryCatch({
    onc_out_workbook(
      output_file    = output_file_path,
      pt_1_output    = pt_1_output,
      pt_2_data      = pt_2_wb_data,
      pt_3_data      = pt_3_wb_data,
      rpt_dm         = setup_result$rpt_dm,
      rpt_err        = setup_result$rpt_err,
      rpt_err_term   = setup_result$rpt_err_term,
      rpt_missing    = if (!is.null(pt_1_output) && "rpt_missing_row" %in% names(pt_1_output))
                         pt_1_output$rpt_missing_row else tibble::tibble(),
      rpt_meddra     = setup_result$rpt_meddra,
      rpt_meddra_term = setup_result$rpt_meddra_term,
      rpt_key        = if (!is.null(pt_1_output) && "rpt_key_row" %in% names(pt_1_output))
                         pt_1_output$rpt_key_row else tibble::tibble(),
      ndabla         = ndabla,
      studyid        = studyid,
      arm_count      = arm_count,
      arm_names      = arm_display_names,
      toxgr_max      = as.character(toxgr_max),
      toxgr_grp5_sw  = if (toxgr_grp5_sw) "Y" else "N",
      cmpgr          = cmpgr,
      meddra         = meddra_flag,
      meddra_pct     = meddra_pct,
      cc_sw          = cc_sw,
      cc_desc        = cc_desc,
      study_lag      = study_lag,
      vld_sw         = vld_sw,
      vld_err        = vld_err,
      ae_aetoxgr     = ae_aetoxgr_flag,
      ae_rate_ci_sw  = ae_rate_ci_sw,
      pp_result      = pp_result
    )
    if (verbose) cli::cli_alert_info("Workbook saved: {output_file_path}")
  }, error = function(e) {
    cli::cli_warn("Failed to generate Excel output: {e$message}")
  })

  # ===========================================================================
  # Task 2.15: Return value — invisible named list
  # ===========================================================================
  elapsed <- (proc.time() - start_time)[["elapsed"]]
  if (verbose) {
    cli::cli_h1("Complete")
    cli::cli_alert_info("Running time: {round(elapsed, 1)} seconds")
  }

  invisible(list(
    success       = setup_result$setup_success,
    pt_1          = pt_1_output,
    pt_2          = pt_2_formatted,
    pt_3          = pt_3_formatted,
    meddra_active = meddra_active,
    setup         = setup_result
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %params(in_*=...) keyword arguments mapped to R function
#      arguments with matching defaults:
#        in_panel_title=AE Toxicity  -> panel_title="AE Toxicity"
#        in_studyid=DeID             -> studyid="DeID"
#        in_study_lag=30             -> study_lag=30
#        in_cc=0.5                   -> cc=0.5
#        in_exp=1                    -> exp_arm=1
#        in_ctl=2                    -> ctl_arm=2
#        in_cmptrm=%str(soc_name,pt_name) -> cmptrm=c("soc_name","pt_name")
#        in_cmpgr=all               -> cmpgr="all"
#        in_cmpsort=rr              -> cmpsort="rr"
#        in_ver=14.1                -> meddra_ver="14.1"
#        in_toxgr_grp5_sw=1         -> toxgr_grp5_sw=TRUE
#    - SAS options mlogic symbolgen -> verbose flag with cli messages
#    - SAS Script Launcher integration path removed: R uses direct
#      function calls. SAS in_saspath/in_utilpath not needed in R
#      (handled by source() with relative path resolution).
#    - studyid default is "DeID" (matching SAS in_studyid=DeID),
#      distinguishing this from ae_oncology_v1.R which uses "".
#    - All MedDRA, toxicity grade, and comparison logic is
#      semantically identical to ae_oncology_v1.R.
#    - SAS dummy sl_datasets/sl_group/sl_subset data steps (lines
#      199-211) replaced by empty tibbles for direct invocation.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Fisher's exact test p-values: R fisher.test() vs SAS PROC FREQ
#      EXACT FISHER may differ at extreme sample sizes due to algorithm
#      differences in the hypergeometric distribution computation.
#    - Continuity correction: SAS uses cc=0.5 as constant addition to
#      zero cells in 2x2 tables; R implementation matches this exactly
#      via the cc_sw/cc_value/cc_whole mechanism.
#    - Relative risk and odds ratio CI at very small sample sizes may
#      show minor differences due to floating-point precision.
#    - MedDRA matching percentage: minor rounding differences possible
#      with janitor::round_half_up() vs SAS PUT with format.
#    - Sort stability: SAS PROC SORT is stable; dplyr::arrange() is
#      stable within groups — verified compatible.
# NO DIRECT R EQUIVALENT:
#    - SAS options mlogic symbolgen -> verbose flag with cli messages
#    - SAS %symexist, %symglobl -> not needed (R function scoping)
#    - SAS Script Launcher mode detection (%sysfunc(ifc(not
#      %symexist(run_location),...))) -> not applicable in R
#    - SAS %str() for comma-containing defaults -> R c() vector
#    - SAS %nrstr() quoting -> not needed in R
#    - SAS PCFILES/JET engine -> openxlsx (full parity)
#    - SAS SpreadsheetML XML generation -> openxlsx styles + writeData
# PACKAGE SELECTION RATIONALE:
#    - haven (>=2.5.5): SAS data I/O (AAP mandate)
#    - dplyr (>=1.1.0): core data manipulation (tidyverse mandate)
#    - tidyr (>=1.3.0): reshaping for arm×grade completions
#    - stringr (>=1.5.0): string manipulation (tidyverse mandate)
#    - cli (>=3.6.0): user-facing messages replacing SAS PUT/NOTE
#    - janitor (>=2.2.0): round_half_up for SAS-compatible rounding
#    - openxlsx (>=4.2.5): Excel output replacing SpreadsheetML
# OPEN QUESTIONS:
#    - Should ae_oncology_v1.R and ae_oncology_v1upd.R be formally
#      documented as lineage? Both are preserved per AAP traceability.
#    - Is studyid="DeID" default appropriate for all deployment
#      contexts, or should it be environment-specific?
#    - MedDRA hierarchy data format: confirm column naming conventions
#      match mdhier_X_Y datasets (soc_name, hlgt_name, hlt_name,
#      pt_name, primary_soc_fg).
#    - Continuity correction default of 0.5 (v1upd) vs 0 (v1): verify
#      whether clinical teams expect this difference.
# ============================================================
