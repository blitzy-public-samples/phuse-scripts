# ==============================================================================
# PROGRAM: ae_v1.R
# DESCRIPTION: AE Severity Analysis Panel (R Migration)
#   - Find subject counts per arm for each adverse event
#   - Find event counts per arm and severity level for each AE
#   - Find top AEs by relative risk & odds ratio between arm pairs
#   - Creates three output workbooks: AE Counts, Odds Ratio, Relative Risk
# MIGRATED FROM: tested/SAS/AE/ae_v1.sas (289 lines)
# EVALUATION TYPE: Safety
# AUTHORS: David Kretch, Andreas Anastassopoulos (original SAS)
# DATE: February 7, 2011 (original); R migration 2026
#
# SAS MACROS MIGRATED:
#   %params  (lines 87-191)   -> ae_severity_v1() function parameters
#   %ae      (lines 228-289)  -> ae_severity_v1() function body
#
# SAS DEPENDENCIES:
#   ae_setup.sas       -> tested/R/utilities/ae_setup.R   (setup_validation)
#   ae_aggregate.sas   -> tested/R/macros/ae_aggregate.R  (ae_ab, ae_cd)
#   ae_output.sas      -> tested/R/macros/ae_output.R     (ae_out_workbook)
#   ae_rror.sas        -> tested/R/macros/ae_rror.R       (compute_rror, etc.)
#   data_checks.sas    -> tested/R/utilities/data_checks.R(chk_val)
#   err_output.sas     -> tested/R/utilities/err_output.R (error_summary)
#   sl_gs_output.sas   -> tested/R/utilities/sl_gs_output.R (transitive)
#   xml_output.sas     -> tested/R/utilities/xml_output.R   (transitive)
#
# INPUTS:  AE, DM, EX datasets (CDISC ADaM/SDTM via XPT or data frames)
# OUTPUTS: Up to 3 Excel workbooks:
#          1. AE Severity counts (Analyses A-D)
#          2. Adverse Events Odds Ratio Analysis
#          3. Adverse Events Relative Risk Analysis
# ==============================================================================

# --- Load required packages ---------------------------------------------------
# Each package is loaded explicitly for clarity; sourced dependency modules
# may also call library() internally but R safely de-duplicates.
library(haven)      # SAS data I/O: read_xpt() for CDISC XPT transport files
library(dplyr)      # Core data manipulation replacing SAS DATA steps / PROC SQL
library(tidyr)      # Data reshaping and pivoting
library(stringr)    # String manipulation replacing SAS character functions
library(cli)        # User-facing informative messages / error formatting
library(janitor)    # round_half_up() for SAS-compatible rounding (Gate 2)
library(openxlsx)   # Excel output replacing SAS SpreadsheetML XML engine

# --- Source internal dependency modules ---------------------------------------
# Uses robust path resolution: first tries relative to this script's location,
# then falls back to well-known project-root-relative paths. This replaces
# the SAS %include statements at lines 198-204 of ae_v1.sas.
local({
  # Attempt to locate this script's directory via source() frame attribute
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )

  # Build base directories for utilities and macros
  if (!is.null(this_dir)) {
    util_dir  <- file.path(dirname(this_dir), "utilities")
    macro_dir <- file.path(dirname(this_dir), "macros")
  } else {
    util_dir  <- "tested/R/utilities"
    macro_dir <- "tested/R/macros"
  }

  # Source each dependency only if its exported function is not yet available
  dep_files <- list(
    list(dir = util_dir,  file = "xml_output.R",    fn = "create_workbook"),
    list(dir = util_dir,  file = "sl_gs_output.R",  fn = "group_subset_pp"),
    list(dir = util_dir,  file = "data_checks.R",   fn = "chk_val"),
    list(dir = util_dir,  file = "err_output.R",    fn = "error_summary"),
    list(dir = util_dir,  file = "ae_setup.R",      fn = "setup_validation"),
    list(dir = macro_dir, file = "ae_aggregate.R",   fn = "ae_ab"),
    list(dir = macro_dir, file = "ae_output.R",      fn = "ae_out_workbook"),
    list(dir = macro_dir, file = "ae_rror.R",        fn = "compute_rror")
  )

  for (dep in dep_files) {
    dep_path <- file.path(dep$dir, dep$file)
    if (!exists(dep$fn, mode = "function", inherits = TRUE)) {
      if (file.exists(dep_path)) {
        source(dep_path, local = FALSE)
      } else {
        cli::cli_warn(
          "Dependency {.file {dep_path}} not found; function {.fn {dep$fn}} unavailable."
        )
      }
    }
  }
})


# ==============================================================================
# ae_severity_v1: Main AE Severity Panel Driver
# ==============================================================================
#' AE Severity Analysis Panel (Original Version)
#'
#' Entry-point function that orchestrates the complete AE severity analysis
#' workflow. Replaces SAS \code{%params} (lines 87-191) for parameter
#' initialization and \code{%ae} (lines 228-289) for analysis execution.
#'
#' All SAS \code{%let} global variables become named function arguments with
#' matching defaults. Run-location detection (LOCAL vs Script Launcher) is
#' removed because R always runs as a direct function call. SAS macro variable
#' cleanup (lines 92-94) is unnecessary in R due to function scoping.
#'
#' @param ae        Data frame or character path to XPT file for AE domain.
#'                  Required columns: AEBODSYS, AEDECOD, USUBJID.
#'                  Optional: AESTDTC, AESER, AESEV.
#' @param dm        Data frame or character path to XPT file for DM domain.
#'                  Required columns: ACTARM or ARM, USUBJID.
#'                  Optional: RFSTDTC, RFENDTC, ARMCD.
#' @param ex        Data frame or character path to XPT file for EX domain.
#'                  Required columns: USUBJID.
#'                  Optional: EXSTDTC, EXENDTC.
#' @param panel_title Character. Panel title for output headers.
#'                    Default "AE Severity" (SAS line 97).
#' @param panel_desc  Character. Panel description text.
#'                    Default "" (SAS line 98).
#' @param outpath     Character. Output directory path. No hardcoded paths —
#'                    all paths are parameterized via function arguments.
#'                    Default "." (SAS line 105).
#' @param aeout1      Character. Filename for AE Severity counts workbook.
#'                    Default "AE Severity.xlsx" (SAS line 106; .xls -> .xlsx).
#' @param aeout2      Character. Filename for Odds Ratio workbook.
#'                    Default "Adverse Events Odds Ratio Analysis.xlsx"
#'                    (SAS line 107).
#' @param aeout3      Character. Filename for Relative Risk workbook.
#'                    Default "Adverse Events Relative Risk Analysis.xlsx"
#'                    (SAS line 108).
#' @param errout      Character. Filename for error summary workbook.
#'                    Default "AE Severity Error Summary.xlsx" (SAS line 109).
#' @param ndabla      Character. NDA/BLA identifier for output headers.
#'                    Default "" (SAS line 122).
#' @param studyid     Character. Study identifier for output headers.
#'                    Default "" (SAS line 123).
#' @param study_lag   Numeric. Window in days after last exposure where AEs
#'                    should be included in the analysis.
#'                    Default 120 (SAS line 128).
#' @param cc          Numeric, character, or zero. Continuity correction method:
#'                    \itemize{
#'                      \item \code{0} or \code{"0"}: No continuity correction
#'                      \item Numeric > 0: Constant added to all cells
#'                      \item \code{"arm"}: Reciprocal of opposite arm total
#'                    }
#'                    Default 0 (SAS line 148).
#' @param vld_sw      Logical. Validation switch — determines whether to perform
#'                    date-based validation on AEs. Default TRUE (SAS line 196).
#'
#' @return Invisibly returns a named list with:
#'   \describe{
#'     \item{success}{Logical: whether setup validation passed.}
#'     \item{ab_a}{Tibble or NULL: Analysis A output (AEs by arm >2\%).}
#'     \item{ab_b}{Tibble or NULL: Analysis B output (serious AEs by arm).}
#'     \item{cd_c}{Tibble or NULL: Analysis C output (AEs by severity).}
#'     \item{cd_d}{Tibble or NULL: Analysis D output (serious AEs by severity).}
#'     \item{rror}{List or NULL: compute_rror() result if multi-arm study.}
#'     \item{setup}{List: full setup_validation() result.}
#'     \item{ae_aeser}{Logical: whether AESER variable was available.}
#'     \item{ae_aesev}{Logical: whether AESEV variable was available.}
#'   }
#'
#' @export
ae_severity_v1 <- function(ae,
                           dm,
                           ex,
                           panel_title = "AE Severity",
                           panel_desc  = "",
                           outpath     = ".",
                           aeout1      = "AE Severity.xlsx",
                           aeout2      = "Adverse Events Odds Ratio Analysis.xlsx",
                           aeout3      = "Adverse Events Relative Risk Analysis.xlsx",
                           errout      = "AE Severity Error Summary.xlsx",
                           ndabla      = "",
                           studyid     = "",
                           study_lag   = 120,
                           cc          = 0,
                           vld_sw      = TRUE) {

  # ============================================================================
  # 1. INPUT VALIDATION
  # ============================================================================
  # Validate that required arguments have correct types. Data frame content
  # validation is delegated to setup_validation() which performs CDISC-aware
  # checks.
  if (missing(ae)) cli::cli_abort("{.arg ae} is required (AE domain data).")
  if (missing(dm)) cli::cli_abort("{.arg dm} is required (DM domain data).")
  if (missing(ex)) cli::cli_abort("{.arg ex} is required (EX domain data).")

  stopifnot(
    is.character(panel_title) && length(panel_title) == 1L,
    is.character(panel_desc)  && length(panel_desc) == 1L,
    is.character(outpath)     && length(outpath) == 1L,
    is.character(aeout1)      && length(aeout1) == 1L,
    is.character(aeout2)      && length(aeout2) == 1L,
    is.character(aeout3)      && length(aeout3) == 1L,
    is.character(errout)      && length(errout) == 1L,
    is.character(ndabla)      && length(ndabla) == 1L,
    is.character(studyid)     && length(studyid) == 1L,
    is.numeric(study_lag)     && length(study_lag) == 1L,
    is.logical(vld_sw)        && length(vld_sw) == 1L
  )

  # ============================================================================
  # 2. DATA LOADING (replaces SAS lines 113-118: libname + data; set; run;)
  # ============================================================================
  # Accept both data frames and file paths to XPT transport files.
  # SAS: libname inlib "&studypath."; data ae; set inlib.ae; run;
  # R:   haven::read_xpt() for file paths; pass-through for data frames.
  cli::cli_inform("AE Severity Panel: Loading input datasets.")

  if (is.character(ae) && length(ae) == 1L) {
    if (!file.exists(ae)) cli::cli_abort("AE file not found: {.file {ae}}")
    ae <- haven::read_xpt(ae)
  }
  if (is.character(dm) && length(dm) == 1L) {
    if (!file.exists(dm)) cli::cli_abort("DM file not found: {.file {dm}}")
    dm <- haven::read_xpt(dm)
  }
  if (is.character(ex) && length(ex) == 1L) {
    if (!file.exists(ex)) cli::cli_abort("EX file not found: {.file {ex}}")
    ex <- haven::read_xpt(ex)
  }

  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame or XPT path.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame or XPT path.")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame or XPT path.")

  # Normalize column names to lowercase (SAS is case-insensitive)
  ae <- ae %>% dplyr::rename_with(tolower)
  dm <- dm %>% dplyr::rename_with(tolower)
  ex <- ex %>% dplyr::rename_with(tolower)

  # Ensure output directory exists
  if (!dir.exists(outpath)) {
    dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
  }

  # ============================================================================
  # 3. CONTINUITY CORRECTION NORMALIZATION
  #    (replaces SAS DATA _NULL_ step, lines 207-222)
  # ============================================================================
  # SAS derives cc_sw and cc_whole from the cc parameter:
  #   - anyalpha("arm")   -> cc_sw = 2 (reciprocal of opposite arm)
  #   - nonzero numeric   -> cc_sw = 1 (constant correction)
  #   - zero/missing       -> cc_sw = 0 (no correction)
  #   - cc_whole = 0 if cc has fractional part or cc_sw=2; 1 otherwise
  cc_config <- if (is.character(cc) && stringr::str_detect(tolower(cc), "[a-z]")) {
    # Character input containing alphabetic chars -> "arm" mode
    if (stringr::str_to_lower(trimws(cc)) == "arm") {
      list(cc_sw = 2L, cc_whole = 0L, cc_value = NA_real_)
    } else {
      cli::cli_warn(
        "Unrecognized continuity correction value {.val {cc}}; defaulting to no correction."
      )
      list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
    }
  } else {
    cc_num <- suppressWarnings(as.numeric(cc))
    if (is.na(cc_num) || cc_num == 0) {
      list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
    } else {
      # Nonzero numeric: cc_whole = 1 if cc is integer, 0 otherwise
      cc_whole_val <- as.integer(cc_num == floor(cc_num))
      list(cc_sw = 1L, cc_whole = cc_whole_val, cc_value = cc_num)
    }
  }

  # ============================================================================
  # 4. SETUP VALIDATION (replaces SAS %setup call at line 230)
  # ============================================================================
  # Calls the gatekeeper validation function from ae_setup.R. This validates
  # required variables in AE/DM/EX, merges domains, derives arm information,
  # and constructs the analysis-ready base dataset.
  cli::cli_inform("AE Severity Panel: Running setup validation.")

  setup_result <- setup_validation(
    ae        = ae,
    dm        = dm,
    ex        = ex,
    mdhier    = FALSE,
    dme       = FALSE,
    vld_sw    = vld_sw,
    study_lag = as.integer(study_lag)
  )

  # Initialize result holders (populated conditionally below)
  ab_a_output   <- NULL
  ab_b_output   <- NULL
  cd_c_output   <- NULL
  cd_d_output   <- NULL
  rror_result   <- NULL
  sev_count     <- 0L
  sev_names     <- character(0)
  rpt_missing   <- NULL
  ae_aeser      <- FALSE
  ae_aesev      <- FALSE

  # ============================================================================
  # 5. ANALYSES (replaces SAS %ae macro body, lines 232-277)
  # ============================================================================
  if (setup_result$setup_success) {

    cli::cli_alert("Setup validation passed. Running AE analyses.")

    # --------------------------------------------------------------------------
    # 5a. Derive ae_aeser and ae_aesev from ds_base column availability
    # --------------------------------------------------------------------------
    # In SAS, %setup creates global macro variables ae_aeser and ae_aesev
    # based on whether AESER and AESEV columns exist in AE. The R setup
    # function preserves these columns in ds_base only if they exist, so
    # column presence in ds_base serves as the indicator.
    ae_aeser <- "aeser" %in% names(setup_result$ds_base)
    ae_aesev <- "aesev" %in% names(setup_result$ds_base)

    # --------------------------------------------------------------------------
    # 5b. Compute arm subject counts from safety population
    # --------------------------------------------------------------------------
    # SAS: arm_subjcnt array populated during %setup via PROC SQL counting
    # distinct subjects per arm_num from all_dm_ex.
    # R: Compute from all_dm_ex which has arm_num assigned by setup_validation.
    # Uses dplyr::filter + group_by + summarise for idiomatic R (not vapply).
    arm_counts_df <- setup_result$all_dm_ex %>%
      dplyr::filter(!is.na(.data$arm_num)) %>%
      dplyr::mutate(arm_num_int = as.integer(.data$arm_num)) %>%
      dplyr::group_by(.data$arm_num_int) %>%
      dplyr::summarise(n_subj = dplyr::n_distinct(.data$usubjid),
                       .groups = "drop") %>%
      dplyr::arrange(.data$arm_num_int)

    # Build arm_subjcnt vector aligned to arm 1..arm_count
    arm_subjcnt <- integer(setup_result$arm_count)
    for (i in seq_len(nrow(arm_counts_df))) {
      idx <- arm_counts_df$arm_num_int[i]
      if (idx >= 1L && idx <= setup_result$arm_count) {
        arm_subjcnt[idx] <- as.integer(arm_counts_df$n_subj[i])
      }
    }
    arm_subjcnt_total <- as.integer(sum(arm_subjcnt))
    # ae_ab and ae_cd expect arm_count + 1 length (per-arm + total)
    arm_subjcnt_full <- c(arm_subjcnt, arm_subjcnt_total)

    # --------------------------------------------------------------------------
    # 5c. Sort and deduplicate: one record per subject per AE
    #     (replaces SAS PROC SORT + DATA step, lines 236-247)
    # --------------------------------------------------------------------------
    # SAS: proc sort data=ds_base out=ds_base_bysubjpt nodupkey;
    #        by aebodsys aedecod usubjid descending aeser;
    #      run;
    #      data ds_base_bysubjpt;
    #        set ds_base_bysubjpt;
    #        by aebodsys aedecod usubjid;
    #        if first.usubjid;
    #      run;
    #
    # R equivalent: arrange by sort keys then distinct() with .keep_all=TRUE
    # retains the first row per group (matching SAS NODUPKEY + first.usubjid).
    # When AESER is available, sort descending so "Y" precedes "N", ensuring
    # the serious indicator is preserved for each subject's first record.
    sort_cols <- c("aebodsys", "aedecod", "usubjid")
    ds_base_sorted <- setup_result$ds_base

    if (ae_aeser) {
      ds_base_sorted <- ds_base_sorted %>%
        dplyr::arrange(.data$aebodsys, .data$aedecod, .data$usubjid,
                       dplyr::desc(.data$aeser))
    } else {
      ds_base_sorted <- ds_base_sorted %>%
        dplyr::arrange(.data$aebodsys, .data$aedecod, .data$usubjid)
    }

    ds_base_bysubjpt <- ds_base_sorted %>%
      dplyr::distinct(.data$aebodsys, .data$aedecod, .data$usubjid,
                      .keep_all = TRUE)

    # --------------------------------------------------------------------------
    # 5d. Analysis A: AEs by arm >2%  (SAS line 250: %ab;)
    # --------------------------------------------------------------------------
    cli::cli_inform("Running Analysis A: AE subject counts by arm.")
    ab_a_output <- ae_ab(
      ds_base_bysubjpt = ds_base_bysubjpt,
      arm_count         = setup_result$arm_count,
      arm_names         = setup_result$arm_names,
      arm_subjcnt       = arm_subjcnt_full,
      aeser             = FALSE
    )

    # --------------------------------------------------------------------------
    # 5e. Analysis B: Serious AEs by arm (SAS line 251: %if &ae_aeser. %then %ab(aeser=y);)
    # --------------------------------------------------------------------------
    if (ae_aeser) {
      cli::cli_inform("Running Analysis B: Serious AE subject counts by arm.")
      ab_b_output <- ae_ab(
        ds_base_bysubjpt = ds_base_bysubjpt,
        arm_count         = setup_result$arm_count,
        arm_names         = setup_result$arm_names,
        arm_subjcnt       = arm_subjcnt_full,
        aeser             = TRUE
      )
    }

    # --------------------------------------------------------------------------
    # 5f. Analysis C: AEs by severity (SAS lines 254-255: %if &ae_aesev. %then %cd;)
    # --------------------------------------------------------------------------
    cd_result_c <- NULL
    cd_result_d <- NULL

    if (ae_aesev) {
      cli::cli_inform("Running Analysis C: AE event counts by severity.")
      cd_result_c <- ae_cd(
        ds_base    = setup_result$ds_base,
        arm_count  = setup_result$arm_count,
        arm_names  = setup_result$arm_names,
        arm_subjcnt = arm_subjcnt_full,
        aeser      = FALSE
      )
      cd_c_output <- cd_result_c$cd_output
      sev_count   <- cd_result_c$sev_count
      sev_names   <- cd_result_c$sev_names

      # Accumulate rpt_missing from Analysis C
      rpt_missing <- cd_result_c$rpt_missing_row

      # ------------------------------------------------------------------
      # 5g. Analysis D: Serious AEs by severity
      #     (SAS line 256: %if &ae_aeser. %then %cd(aeser=y);)
      # ------------------------------------------------------------------
      if (ae_aeser) {
        cli::cli_inform("Running Analysis D: Serious AE event counts by severity.")
        cd_result_d <- ae_cd(
          ds_base    = setup_result$ds_base,
          arm_count  = setup_result$arm_count,
          arm_names  = setup_result$arm_names,
          arm_subjcnt = arm_subjcnt_full,
          aeser      = TRUE,
          all_sev    = cd_result_c$all_sev
        )
        cd_d_output <- cd_result_d$cd_output

        # Accumulate rpt_missing from Analysis D
        if (!is.null(cd_result_d$rpt_missing_row)) {
          rpt_missing <- dplyr::bind_rows(rpt_missing, cd_result_d$rpt_missing_row)
        }
      }
    }

    # --------------------------------------------------------------------------
    # 5h. AESER value validation (SAS lines 259-262)
    # --------------------------------------------------------------------------
    # SAS: %chk_val(work, all_ae_dm_ex, aeser, Y);
    #      %chk_val(work, all_ae_dm_ex, aeser, N);
    # Validates that AESER values are within expected set {Y, N}.
    if (ae_aeser) {
      chk_val_result <- chk_val(
        data    = setup_result$all_ae_dm_ex,
        var     = "aeser",
        values  = c("Y", "N"),
        ds_name = "all_ae_dm_ex"
      )
      # Audit trail — informational; does not abort analysis
      cli::cli_inform(
        "AESER validation: {nrow(chk_val_result)} checks performed."
      )
    }

    # --------------------------------------------------------------------------
    # 5i. AE Output Workbook Generation (SAS line 264: %out_ae;)
    # --------------------------------------------------------------------------
    cli::cli_inform("Generating AE Severity output workbook.")
    ae_out_workbook(
      output_file = file.path(outpath, aeout1),
      ab_a_output = ab_a_output,
      ab_b_output = ab_b_output,
      cd_c_output = cd_c_output,
      cd_d_output = cd_d_output,
      rpt_dm      = setup_result$rpt_dm,
      rpt_err     = setup_result$rpt_err,
      rpt_err_term = setup_result$rpt_err_term,
      rpt_missing = rpt_missing,
      ndabla      = ndabla,
      studyid     = studyid,
      arm_count   = setup_result$arm_count,
      arm_names   = setup_result$arm_display_names,
      arm_subjcnt = arm_subjcnt,
      sev_count   = sev_count,
      sev_names   = sev_names,
      vld_sw      = vld_sw,
      study_lag   = study_lag,
      ae_aeser    = ae_aeser,
      ae_aesev    = ae_aesev
    )

    # --------------------------------------------------------------------------
    # 5j. Relative Risk / Odds Ratio Analysis (SAS lines 267-277)
    # --------------------------------------------------------------------------
    if (setup_result$arm_count > 1L) {
      cli::cli_inform("Multi-arm study detected: computing OR/RR analysis.")

      # Compute RR and OR for all arm pairs
      rror_result <- compute_rror(
        ds_base_bysubjpt = ds_base_bysubjpt,
        arm_count         = setup_result$arm_count,
        arm_names         = setup_result$arm_names,
        arm_subjcnt       = arm_subjcnt,
        cc_sw             = cc_config$cc_sw,
        cc_whole          = cc_config$cc_whole,
        cc                = cc_config$cc_value
      )

      # Reshape results for Excel output layout
      reshaped <- reshape_rror_for_excel(
        compute_result = rror_result,
        arm_count      = setup_result$arm_count,
        arm_names      = setup_result$arm_names,
        arm_subjcnt    = arm_subjcnt
      )

      # Construct lib_metadata for write_rror_workbooks
      # Contains study identifiers, run info, and CC description
      cc_description <- dplyr::case_when(
        cc_config$cc_sw == 0L ~ "None",
        cc_config$cc_sw == 2L ~ "Reciprocal of opposite arm total (1/N)",
        cc_config$cc_sw == 1L ~ paste0("Constant: ", cc_config$cc_value),
        TRUE                  ~ "None"
      )
      cc_asterisk_note <- if (cc_config$cc_sw > 0L) {
        "* Continuity correction applied"
      } else {
        ""
      }
      cc_detail_note <- if (cc_config$cc_sw == 2L) {
        "Continuity correction: reciprocal of the opposite arm total added to zero cells"
      } else if (cc_config$cc_sw == 1L) {
        paste0("Continuity correction: ", cc_config$cc_value, " added to all cells")
      } else {
        ""
      }

      date_validation_text <- if (vld_sw) {
        paste0("Date-based AE validation applied (study_lag = ", study_lag, " days)")
      } else {
        "No date-based validation"
      }

      lib_metadata <- list(
        ndabla            = ndabla,
        studyid           = studyid,
        rundate           = format(Sys.time(), "%d%b%Y %H:%M"),
        date_validation   = date_validation_text,
        statistic         = "",
        cc_description    = cc_description,
        cc_asterisk_note  = cc_asterisk_note,
        cc_detail_note    = cc_detail_note,
        arm_count         = as.character(setup_result$arm_count),
        sl_custom_ds      = "",
        study_lag         = as.character(study_lag)
      )

      # Write OR and RR workbooks
      write_rror_workbooks(
        or_output_file = file.path(outpath, aeout2),
        rr_output_file = file.path(outpath, aeout3),
        reshaped_data  = reshaped,
        lib_metadata   = lib_metadata,
        arm_count      = setup_result$arm_count,
        arm_names      = setup_result$arm_names,
        arm_display    = setup_result$arm_display_names
      )

    } else {
      # Single arm study — write error summaries (SAS lines 271-277)
      # SAS: %error_summary(err_file=&aeout2., err_seterr=0,
      #        err_desc=%nrstr(The Adverse Events Odds Ratio Analysis ...));
      cli::cli_inform("Single-arm study: writing OR/RR error summaries.")

      error_summary(
        err_file    = file.path(outpath, aeout2),
        panel_title = panel_title,
        ndabla      = ndabla,
        studyid     = studyid,
        err_seterr  = FALSE,
        err_desc    = paste0(
          "The Adverse Events Odds Ratio Analysis could not be run ",
          "because this study contained only one arm"
        ),
        panel_desc  = panel_desc
      )
      error_summary(
        err_file    = file.path(outpath, aeout3),
        panel_title = panel_title,
        ndabla      = ndabla,
        studyid     = studyid,
        err_seterr  = FALSE,
        err_desc    = paste0(
          "The Adverse Events Relative Risk Analysis could not be run ",
          "because this study contained only one arm"
        ),
        panel_desc  = panel_desc
      )
    }

    cli::cli_alert_success("AE Severity Panel analyses completed successfully.")

  } else {
    # ==========================================================================
    # SETUP FAILURE PATH (SAS lines 280-285)
    # ==========================================================================
    # SAS: %error_summary(err_file=&errout.,
    #        err_nosubj=%sysfunc(ifc(&dm_subj_gt0.,0,1)),
    #        err_missvar=%sysfunc(ifc(&setup_req_var.,0,1)));
    cli::cli_warn("AE Severity Panel: Setup validation failed.")

    error_summary(
      err_file        = file.path(outpath, errout),
      panel_title     = panel_title,
      ndabla          = ndabla,
      studyid         = studyid,
      err_nosubj      = !setup_result$dm_subj_gt0,
      err_missvar     = !setup_result$setup_req_var,
      rpt_chk_var_req = setup_result$rpt_chk_var_req,
      panel_desc      = panel_desc
    )
  }

  # ============================================================================
  # 6. RETURN VALUE
  # ============================================================================
  # Returns all computed results invisibly for downstream programmatic use.
  # SAS had no explicit return — results were global datasets and macro vars.
  invisible(list(
    success   = setup_result$setup_success,
    ab_a      = ab_a_output,
    ab_b      = ab_b_output,
    cd_c      = cd_c_output,
    cd_d      = cd_d_output,
    rror      = rror_result,
    setup     = setup_result,
    ae_aeser  = ae_aeser,
    ae_aesev  = ae_aesev
  ))
}


# ==============================================================================
# MIGRATION NOTES
# ==============================================================================
# ASSUMPTIONS:
#    - SAS %params (lines 87-191) + %ae (lines 228-289) macros combined into
#      a single ae_severity_v1() function
#    - All SAS %let global variables become named function arguments with
#      matching defaults
#    - Run-location detection (LOCAL vs Script Launcher, SAS line 84) removed;
#      R always runs as a direct function call
#    - SAS macro variable cleanup (lines 92-94: data macrovar; %symdel) not
#      needed in R due to function scoping
#    - SAS dummy sl_datasets/sl_group/sl_subset (lines 151-162) replaced by
#      NULL defaults — Script Launcher metadata passed via pp_result parameter
#      to ae_out_workbook when available
#    - OR/RR template copy via OS command (lines 139-142: x "copy template")
#      replaced by openxlsx workbook creation from scratch
#    - arm_subjcnt computed from all_dm_ex safety population using arm_num
#      assigned by setup_validation(); ae_ab/ae_cd receive arm_count+1 length
#      (per-arm + total), while ae_out_workbook/compute_rror receive arm_count
#      length (per-arm only) as required by their validation
#    - ae_aeser and ae_aesev derived from column presence in ds_base rather
#      than explicit flags, since setup_validation() conditionally retains
#      these columns based on their existence in the source AE data
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Percentage computation: janitor::round_half_up() available for
#      SAS-compatible rounding; ae_ab/ae_cd compute raw float percentages
#      by default (caller controls rounding via pct_digits parameter)
#    - PROC SORT NODUPKEY behavior: dplyr::arrange() + distinct(.keep_all)
#      preserves first occurrence per group (same as SAS when BY vars match)
#    - Fisher's exact test p-values in OR/RR: R stats::fisher.test() may
#      differ from SAS PROC FREQ EXACT at edge cases (very small cell counts)
#    - Continuity correction reciprocal: 1/opposite_arm_total may produce
#      different precision than SAS due to floating-point representation
#    - Sort stability: dplyr::arrange() is stable (same as SAS PROC SORT);
#      verified that multi-key descending sorts preserve expected ordering
#
# NO DIRECT R EQUIVALENT:
#    - SAS options minoperator (line 80) -> not needed; R has native %in%
#    - SAS options missing='' (line 81) -> NA display handled by openxlsx
#      output formatting functions; NA is never silently converted to blank
#    - SAS %sysfunc(ifc(...)) (line 84, 282-283) -> standard R if/else or
#      dplyr::if_else()
#    - SAS %symexist (line 84) -> not needed; R function scoping ensures
#      all variables are locally available
#    - SAS template copy via x command (lines 139-142) -> openxlsx creates
#      workbooks from scratch without template files
#    - SAS PCFILES/JET engine for Excel writes -> openxlsx direct API
#    - SAS SpreadsheetML XML generation -> openxlsx OOXML format
#
# PACKAGE SELECTION RATIONALE:
#    - haven (2.5.5): SAS/XPT data I/O via embedded ReadStat C library;
#      AAP mandated for all SAS transport file handling
#    - dplyr (>=1.1.0): Core tidyverse data manipulation; AAP mandates
#      tidyverse over base R for all data wrangling operations
#    - tidyr (>=1.3.0): Data reshaping; loaded for transitive dependency
#      support in ae_aggregate.R pivot operations
#    - stringr (>=1.5.0): String manipulation; AAP mandates tidyverse
#      string functions over base R equivalents
#    - cli (>=3.6.0): User-facing messages replacing SAS %put NOTE/WARNING/
#      ERROR; structured formatting with {.val}, {.file}, {.fn} glue syntax
#    - janitor (>=2.2.0): round_half_up() replicates SAS round-half-up
#      behavior; critical for regulatory output parity per Gate 2 audit
#    - openxlsx (>=4.2.5): Excel workbook output replacing SAS SpreadsheetML
#      XML engine and PCFILES/JET engine; AAP mandated
#
# OPEN QUESTIONS:
#    - OR/RR template Excel compatibility: verify openxlsx output is
#      functionally equivalent to SAS PCFILES-written templates
#    - Script Launcher integration: if an R-based Script Launcher exists,
#      panel options should be passed via a configuration list or YAML file
#      rather than SAS macro variable resolution
#    - Sort stability for NODUPKEY deduplication: verified dplyr arrange +
#      distinct matches SAS; however, tie-breaking on unlisted columns may
#      differ if source data has non-deterministic row order
#    - Date validation (vld_sw): SAS uses integer 1/0; R uses logical
#      TRUE/FALSE; confirmed setup_validation handles both via as.logical()
# ==============================================================================
