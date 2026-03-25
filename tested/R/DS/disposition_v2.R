# =============================================================================
# PROGRAM NAME: disposition_v2.R
#
# DESCRIPTION:  Disposition Analysis Panel — R migration of the SAS Disposition
#               Analysis Panel driver (tested/SAS/DS/disposition_v2.sas).
#               Performs 4 analyses:
#                 1. Disposition Events by Arm for All Subjects (freq/pct table)
#                 2. Disposition Events by Arm for Exposed Subjects (freq/pct)
#                 3. Time to Disposition Event by Arm — All Subjects (cumulative %)
#                 4. Time to Disposition Event by Arm — Exposed Subjects (cumul. %)
#
#               Seven SAS macros are migrated to seven parameterised R functions:
#                 %params           -> ds_params()
#                 %ds_prelim_check  -> ds_prelim_check()
#                 %ds_setup         -> ds_setup()
#                 %ds_by_arm        -> ds_by_arm()
#                 %time_to_event    -> ds_time_to_event()
#                 %ds_out           -> ds_out()
#                 %ds               -> run_disposition()
#
# ORIGINAL SAS: tested/SAS/DS/disposition_v2.sas (1062 lines)
# SAS AUTHOR:   Andreas Anastassopoulos (andreas.anastassapoulos@us.ibm.com)
# SAS DATE:     December 9, 2009
#
# R MIGRATION:  Migrated to idiomatic R using tidyverse/pharmaverse ecosystem
# R REQUIRES:   haven (2.5.5), dplyr (>= 1.1.0), tidyr (>= 1.3.0),
#               openxlsx (>= 4.2.5), janitor (>= 2.2.0), stringr (>= 1.5.0),
#               cli (>= 3.6.0), lubridate (>= 1.9.0), forcats (>= 1.0.0),
#               ggplot2 (>= 3.4.0), survival (>= 3.5-0), survminer (>= 0.4.9),
#               Tplyr (1.2.1), r2rtf (1.1.1), tibble (>= 3.2.0),
#               purrr (>= 1.0.0), rlang (>= 1.1.0)
#
# R DEPENDS ON: tested/R/macros/data_checks_disposition.R
#                 (disposition_check, disposition_check_out)
#               tested/R/utilities/data_checks.R
#                 (chk_var, chk_dm_subj_gt0)
#               tested/R/utilities/err_output.R
#                 (error_summary)
#               tested/R/utilities/sl_gs_output.R
#                 (group_subset_pp, group_subset_write_xlsx)
#               tested/R/utilities/xml_output.R
#                 (create_workbook, create_workbook_styles,
#                  write_data_table, apply_page_setup)
#
# CRITICAL RULES:
#   - 100% functional parity with SAS script
#   - Idiomatic R, NOT transliterated SAS
#   - Tidyverse over base R
#   - No hardcoded paths
#   - Missing: SAS . -> NA, SAS ' ' -> NA_character_ (never zero-sub)
#   - Rounding: janitor::round_half_up() everywhere
#   - Date arithmetic: SAS origin = 1960-01-01
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(openxlsx)
library(janitor)
library(stringr)
library(cli)
library(lubridate)
library(forcats)
library(ggplot2)
library(survival)
library(survminer)
library(Tplyr)
library(r2rtf)
library(tibble)
library(purrr)
library(rlang)

# ---------------------------------------------------------------------------
# Internal dependency sourcing — guard against re-sourcing
# ---------------------------------------------------------------------------
local({
  # Determine base paths for internal dependencies
  this_file <- tryCatch(
    normalizePath(sys.frame(1L)$ofile, mustWork = TRUE),
    error = function(e) NULL
  )
  if (is.null(this_file)) {
    for (i in rev(seq_len(sys.nframe()))) {
      env <- sys.frame(i)
      if (exists("ofile", envir = env, inherits = FALSE)) {
        this_file <- tryCatch(
          normalizePath(get("ofile", envir = env), mustWork = TRUE),
          error = function(e) NULL
        )
        if (!is.null(this_file)) break
      }
    }
  }

  if (!is.null(this_file)) {
    ds_dir <- dirname(this_file)
    macros_path   <- file.path(dirname(ds_dir), "macros")
    util_path     <- file.path(dirname(ds_dir), "utilities")
  } else {
    macros_path   <- file.path("tested", "R", "macros")
    util_path     <- file.path("tested", "R", "utilities")
  }

  # Source xml_output.R first (foundational layer for downstream dependencies)
  xml_out_file <- file.path(util_path, "xml_output.R")
  if (!exists("create_workbook", mode = "function", inherits = TRUE) &&
      file.exists(xml_out_file)) {
    source(xml_out_file, local = FALSE)
  }

  # Source data_checks.R (chk_var, chk_dm_subj_gt0)
  dc_file <- file.path(util_path, "data_checks.R")
  if (!exists("chk_var", mode = "function", inherits = TRUE) &&
      file.exists(dc_file)) {
    source(dc_file, local = FALSE)
  }

  # Source err_output.R (error_summary)
  err_file <- file.path(util_path, "err_output.R")
  if (!exists("error_summary", mode = "function", inherits = TRUE) &&
      file.exists(err_file)) {
    source(err_file, local = FALSE)
  }

  # Source sl_gs_output.R (group_subset_pp, group_subset_write_xlsx)
  sl_file <- file.path(util_path, "sl_gs_output.R")
  if (!exists("group_subset_pp", mode = "function", inherits = TRUE) &&
      file.exists(sl_file)) {
    source(sl_file, local = FALSE)
  }

  # Source data_checks_disposition.R (disposition_check, disposition_check_out)
  dcd_file <- file.path(macros_path, "data_checks_disposition.R")
  if (!exists("disposition_check", mode = "function", inherits = TRUE) &&
      file.exists(dcd_file)) {
    source(dcd_file, local = FALSE)
  }
})


# =============================================================================
# ds_params: Initialise parameters and load data
# =============================================================================
#' Initialise Disposition Analysis Panel parameters and load data.
#'
#' Replaces SAS \code{%params} macro (lines 96-173 of disposition_v2.sas).
#' All hardcoded SAS paths become function arguments. Data may be supplied
#' directly as data frames or loaded from XPT file paths.
#'
#' @param dm_data      Data frame for DM domain, or NULL to load from dm_path.
#' @param ds_data      Data frame for DS domain, or NULL to load from ds_path.
#' @param ex_data      Data frame for EX domain, or NULL to load from ex_path.
#' @param dm_path      Character path to DM XPT file (used when dm_data=NULL).
#' @param ds_path      Character path to DS XPT file (used when ds_data=NULL).
#' @param ex_path      Character path to EX XPT file (used when ex_data=NULL).
#' @param panel_title  Character panel title. Defaults to "Disposition".
#' @param panel_desc   Character panel description. Defaults to "".
#' @param ndabla       Character NDA/BLA number.
#' @param studyid      Character study identifier.
#' @param catc         Character column name for category filter. Default "dscat".
#' @param subc         Character column name for subcategory. Default "dsscat".
#' @param catdis       Character value for category filter (uppercased in comparison).
#' @param subcatdis    Character value for subcategory filter.
#' @param output_path  Character directory for output files.
#' @param output_file  Character filename for output workbook.
#' @param err_file     Character filename for error workbook.
#'
#' @return A named list (config) containing all parameters, loaded datasets,
#'   constructed output file paths, and default Script Launcher datasets.
# -----------------------------------------------------------------------------
ds_params <- function(dm_data       = NULL,
                      ds_data       = NULL,
                      ex_data       = NULL,
                      dm_path       = NULL,
                      ds_path       = NULL,
                      ex_path       = NULL,
                      panel_title   = "Disposition",
                      panel_desc    = "",
                      ndabla        = "",
                      studyid       = "",
                      catc          = "dscat",
                      subc          = "dsscat",
                      catdis        = "DISPOSITION EVENT",
                      subcatdis     = "END OF TREATMENT",
                      output_path   = NULL,
                      output_file   = "Disposition.xlsx",
                      err_file      = "Disposition Error Summary.xlsx",
                      sl_group      = NULL,
                      sl_subset     = NULL,
                      sl_datasets   = NULL,
                      sl_custom_ds  = NULL,
                      run_location  = "LOCAL") {

  # --- Load datasets from XPT if not provided directly ---
  dm <- dm_data
  ds <- ds_data
  ex <- ex_data

  if (is.null(dm) && !is.null(dm_path)) {
    dm <- haven::read_xpt(dm_path)
  }
  if (is.null(ds) && !is.null(ds_path)) {
    ds <- haven::read_xpt(ds_path)
  }
  if (is.null(ex) && !is.null(ex_path)) {
    ex <- haven::read_xpt(ex_path)
  }

  # --- Normalise column names to lowercase (SAS is case-insensitive) ---
  if (!is.null(dm)) names(dm) <- tolower(names(dm))
  if (!is.null(ds)) names(ds) <- tolower(names(ds))
  if (!is.null(ex)) names(ex) <- tolower(names(ex))

  # --- Construct output file paths ---
  if (!is.null(output_path)) {
    dispout <- file.path(output_path, output_file)
    errout  <- file.path(output_path, err_file)
  } else {
    dispout <- output_file
    errout  <- err_file
  }

  # --- Default Script Launcher datasets (empty tibbles) ---
  # Replaces SAS dummy data steps for sl_datasets, sl_group, sl_subset
  if (is.null(sl_datasets)) {
    sl_datasets <- tibble::tibble(
      datatype           = character(0),
      name               = character(0),
      partition_variable = character(0),
      default            = character(0)
    )
  }
  if (is.null(sl_group)) {
    sl_group <- tibble::tibble(
      group_name    = character(0),
      domain        = character(0),
      partition     = character(0),
      var_name      = character(0),
      var_value     = character(0),
      dsvg_grp_name = character(0)
    )
  }
  if (is.null(sl_subset)) {
    sl_subset <- tibble::tibble(
      name           = character(0),
      domain         = character(0),
      partition      = character(0),
      var_name       = character(0),
      var_value      = character(0),
      inner_operator = character(0),
      outer_operator = character(0)
    )
  }

  # --- Return config list ---
  list(
    dm            = dm,
    ds            = ds,
    ex            = ex,
    panel_title   = panel_title,
    panel_desc    = panel_desc,
    ndabla        = ndabla,
    studyid       = studyid,
    catc          = tolower(catc),
    subc          = tolower(subc),
    catdis        = catdis,
    subcatdis     = subcatdis,
    output_path   = output_path,
    output_file   = output_file,
    dispout       = dispout,
    errout        = errout,
    sl_group      = sl_group,
    sl_subset     = sl_subset,
    sl_datasets   = sl_datasets,
    sl_custom_ds  = sl_custom_ds %||% "",
    run_location  = run_location
  )
}


# =============================================================================
# ds_prelim_check: Preliminary required and optional variable checks
# =============================================================================
#' Perform preliminary variable existence checks on DM, DS, and EX datasets.
#'
#' Replaces SAS \code{%ds_prelim_check} macro (lines 186-249 of
#' disposition_v2.sas). Checks required and optional variables, builds
#' compound check rows for ARM (ACTARM or ARM) and date (DSSTDY or
#' DSSTDTC+RFSTDTC), sets all_req_var and ds_tte flags.
#'
#' @param dm  Data frame for DM domain.
#' @param ds  Data frame for DS domain.
#' @param ex  Data frame for EX domain.
#'
#' @return A named list with logical flags and audit tibbles:
#'   dm_subj_gt0, all_req_var, ds_tte, dm_actarm, dm_arm, dm_armcd,
#'   ds_dscat, ds_dsscat, ds_dsseq, ds_dsstdy, ds_dsstdtc, dm_rfstdtc,
#'   rpt_chk_var, rpt_chk_var_req
# -----------------------------------------------------------------------------
ds_prelim_check <- function(dm, ds, ex) {

  # --- Check whether DM has subjects (SAS line 189) ---
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # --- Required variables (SAS lines 192-195) ---
  chk_dm_usub  <- chk_var(dm, "usubjid", ds_name = "DM")
  chk_ds_dsdec <- chk_var(ds, "dsdecod",  ds_name = "DS")
  chk_ds_usub  <- chk_var(ds, "usubjid",  ds_name = "DS")
  chk_ex_usub  <- chk_var(ex, "usubjid",  ds_name = "EX")

  rpt_chk_var_req <- dplyr::bind_rows(
    chk_dm_usub, chk_ds_dsdec, chk_ds_usub, chk_ex_usub
  )

  # --- Actual arm or planned arm (SAS lines 202-213) ---
  chk_dm_actarm <- chk_var(dm, "actarm", ds_name = "DM")
  chk_dm_arm    <- chk_var(dm, "arm",    ds_name = "DM")

  dm_actarm_ind <- as.integer(chk_dm_actarm$ind)
  dm_arm_ind    <- as.integer(chk_dm_arm$ind)

  arm_check <- tibble::tibble(
    chk       = "VAR",
    ds        = "DM",
    var       = "ACTARM or ARM",
    type      = "",
    len       = -1L,
    condition = "EXISTS",
    ind       = as.integer(dm_actarm_ind == 1L | dm_arm_ind == 1L)
  )
  rpt_chk_var_req <- dplyr::bind_rows(rpt_chk_var_req, arm_check)

  # --- Date variables (SAS lines 216-227) ---
  chk_ds_dsstdtc <- chk_var(ds, "dsstdtc", ds_name = "DS")
  chk_ds_dsstdy  <- chk_var(ds, "dsstdy",  ds_name = "DS")
  chk_dm_rfstdtc <- chk_var(dm, "rfstdtc", ds_name = "DM")

  ds_dsstdtc_ind <- as.integer(chk_ds_dsstdtc$ind)
  ds_dsstdy_ind  <- as.integer(chk_ds_dsstdy$ind)
  dm_rfstdtc_ind <- as.integer(chk_dm_rfstdtc$ind)

  date_check <- tibble::tibble(
    chk       = "VAR",
    ds        = "DM/DS",
    var       = "DSSTDY or (DSSTDTC and RFSTDTC)",
    type      = "",
    len       = -1L,
    condition = "EXISTS",
    ind       = as.integer(
      ds_dsstdy_ind == 1L |
        (ds_dsstdtc_ind == 1L & dm_rfstdtc_ind == 1L)
    )
  )
  rpt_chk_var_req <- dplyr::bind_rows(rpt_chk_var_req, date_check)

  # --- all_req_var flag (SAS lines 230-236) ---
  all_req_var <- all(rpt_chk_var_req$ind == 1L)

  # --- Optional variables (SAS lines 239-242) ---
  chk_dm_armcd  <- chk_var(dm, "armcd",  ds_name = "DM")
  chk_ds_dscat  <- chk_var(ds, "dscat",  ds_name = "DS")
  chk_ds_dsscat <- chk_var(ds, "dsscat", ds_name = "DS")
  chk_ds_dsseq  <- chk_var(ds, "dsseq",  ds_name = "DS")

  # Combine all checks for full audit trail
  rpt_chk_var <- dplyr::bind_rows(
    rpt_chk_var_req,
    chk_dm_actarm, chk_dm_arm,
    chk_ds_dsstdtc, chk_ds_dsstdy, chk_dm_rfstdtc,
    chk_dm_armcd, chk_ds_dscat, chk_ds_dsscat, chk_ds_dsseq
  )

  # --- ds_tte flag (SAS lines 244-247) ---
  ds_tte <- (ds_dsstdy_ind == 1L) |
    (dm_rfstdtc_ind == 1L & ds_dsstdtc_ind == 1L)

  list(
    dm_subj_gt0   = dm_subj_gt0,
    all_req_var   = all_req_var,
    ds_tte        = as.logical(ds_tte),
    dm_actarm     = as.logical(dm_actarm_ind),
    dm_arm        = as.logical(dm_arm_ind),
    dm_armcd      = as.logical(chk_dm_armcd$ind),
    ds_dscat      = as.logical(chk_ds_dscat$ind),
    ds_dsscat     = as.logical(chk_ds_dsscat$ind),
    ds_dsseq      = as.logical(chk_ds_dsseq$ind),
    ds_dsstdy     = as.logical(ds_dsstdy_ind),
    ds_dsstdtc    = as.logical(ds_dsstdtc_ind),
    dm_rfstdtc    = as.logical(dm_rfstdtc_ind),
    rpt_chk_var   = rpt_chk_var,
    rpt_chk_var_req = rpt_chk_var_req
  )
}


# =============================================================================
# propcase_arm: Convert ARM names to proper case with unit rules
# =============================================================================
#' Convert ARM name to proper case preserving unit abbreviation rules.
#'
#' Internal helper replacing SAS propcase ARM logic (lines 278-295 of
#' disposition_v2.sas). Words >3 chars without digits get title case;
#' "MG"/"KG" become lowercase; "ML" becomes "mL". If already mixed case,
#' return unchanged.
#'
#' @param arm_val Character scalar: the arm name to convert.
#' @return Character scalar: the converted arm name.
#' @keywords internal
# -----------------------------------------------------------------------------
propcase_arm <- function(arm_val) {
  if (is.na(arm_val) || arm_val == "") return(arm_val)

  # If already has lowercase letters, return unchanged (SAS: anylower)
  if (grepl("[a-z]", arm_val)) return(arm_val)

  # Split into words and apply ProperCase rules
  words <- strsplit(arm_val, "\\s+")[[1]]
  words <- purrr::map_chr(words, function(w) {
    w_clean <- trimws(w)
    if (nchar(w_clean) == 0L) return(w_clean)

    w_upper <- toupper(w_clean)

    # Unit abbreviations: MG/KG -> lowercase, ML -> mL
    if (w_upper %in% c("MG", "KG")) return(tolower(w_clean))
    if (w_upper == "ML") return("mL")

    # Words > 3 chars without digits -> title case
    if (nchar(w_clean) > 3L && !grepl("[0-9]", w_clean)) {
      return(stringr::str_to_title(w_clean))
    }

    # Otherwise return as-is
    w_clean
  })

  paste(words, collapse = " ")
}


# =============================================================================
# ds_setup: Disposition analysis dataset preprocessing
# =============================================================================
#' Preprocess DM, DS, and EX datasets for disposition analysis.
#'
#' Replaces SAS \code{%ds_setup} macro (lines 255-383 of disposition_v2.sas).
#' Handles ACTARM/ARM preference, ARMCD screen-failure exclusion, ProperCase
#' conversion, DM-DS inner join, DSDY computation with SAS date arithmetic
#' (no day-0), and EX exposed-subject subsetting.
#'
#' @param dm     Data frame for DM domain.
#' @param ds     Data frame for DS domain.
#' @param ex     Data frame for EX domain.
#' @param checks Named list from ds_prelim_check().
#'
#' @return Named list: dm, ds, dm_ds, dm_ds_ex
# -----------------------------------------------------------------------------
ds_setup <- function(dm, ds, ex, checks) {

  cli::cli_inform("Disposition analysis dataset preprocessing")

  # --- ARM preference: ACTARM overrides ARM (SAS lines 262-268) ---
  if (checks$dm_actarm) {
    if (checks$dm_arm) {
      dm <- dm %>% dplyr::rename(plannedarm = arm)
    }
    dm <- dm %>% dplyr::rename(arm = actarm)
  }

  # --- DM preprocessing (SAS lines 270-296) ---
  # Filter screen failures and not-assigned if ARMCD available
  if (checks$dm_armcd) {
    dm <- dm %>%
      dplyr::filter(!(toupper(armcd) %in% c("SCRNFAIL", "NOTASSGN")))
  }

  # ProperCase arm names
  dm <- dm %>%
    dplyr::mutate(arm = vapply(arm, propcase_arm, character(1),
                               USE.NAMES = FALSE))

  # --- DS preprocessing (SAS lines 298-317) ---
  # DSDECOD: propcase if all uppercase
  ds <- ds %>%
    dplyr::mutate(
      dsdecod = dplyr::if_else(
        !is.na(dsdecod) & !grepl("[a-z]", dsdecod),
        stringr::str_to_title(dsdecod),
        dsdecod
      )
    )

  # DSCAT: handle missing and propcase

  if (checks$ds_dscat) {
    ds <- ds %>%
      dplyr::mutate(
        dscat = dplyr::if_else(is.na(dscat) | dscat == "", "Missing", dscat),
        dscat = dplyr::if_else(
          !grepl("[a-z]", dscat),
          stringr::str_to_title(dscat),
          dscat
        )
      )
  } else {
    ds <- ds %>% dplyr::mutate(dscat = "Missing")
  }

  # DSSCAT: handle missing and propcase
  if (checks$ds_dsscat) {
    ds <- ds %>%
      dplyr::mutate(
        dsscat = dplyr::if_else(is.na(dsscat) | dsscat == "", "Missing", dsscat),
        dsscat = dplyr::if_else(
          !grepl("[a-z]", dsscat),
          stringr::str_to_title(dsscat),
          dsscat
        )
      )
  } else {
    ds <- ds %>% dplyr::mutate(dsscat = "Missing")
  }

  # --- Sort and merge DM-DS (SAS lines 324-340) ---
  dm <- dm %>% dplyr::arrange(usubjid)
  ds <- ds %>% dplyr::arrange(usubjid)

  # Dedup EX by USUBJID (SAS: proc sort nodupkey)
  ex <- ex %>%
    dplyr::arrange(usubjid) %>%
    dplyr::distinct(usubjid, .keep_all = TRUE)

  # Inner join DM and DS on USUBJID (SAS: merge dm(in=b) ds(in=a); if a and b)
  dm_ds <- dplyr::inner_join(dm, ds, by = "usubjid")

  # --- Compute DSDY: study day (SAS lines 343-373) ---
  # CRITICAL: SAS date arithmetic — no day 0.
  #   dss_date >= rfs_date => dsdy = dss_date - rfs_date + 1
  #   dss_date <  rfs_date => dsdy = dss_date - rfs_date (negative, no +1)
  if (checks$ds_dsstdy) {
    # DSSTDY directly available
    dm_ds <- dm_ds %>%
      dplyr::mutate(dsdy = as.numeric(dsstdy))
  } else if (checks$dm_rfstdtc && checks$ds_dsstdtc) {
    # Compute from date strings
    dm_ds <- dm_ds %>%
      dplyr::mutate(
        dss_date = as.Date(substr(as.character(dsstdtc), 1, 10),
                           format = "%Y-%m-%d"),
        rfs_date = as.Date(substr(as.character(rfstdtc), 1, 10),
                           format = "%Y-%m-%d"),
        dsdy = dplyr::case_when(
          is.na(dss_date) | is.na(rfs_date) ~ NA_real_,
          dss_date >= rfs_date ~ as.numeric(
            difftime(dss_date, rfs_date, units = "days")
          ) + 1,
          dss_date < rfs_date ~ as.numeric(
            difftime(dss_date, rfs_date, units = "days")
          )
        )
      ) %>%
      dplyr::select(-dss_date, -rfs_date)
  } else {
    # No date variables available — set dsdy to NA
    dm_ds <- dm_ds %>%
      dplyr::mutate(dsdy = NA_real_)
  }

  # --- Merge on EX to create exposed subset (SAS lines 377-381) ---
  # SAS: merge dm_ds(in=d) ex(in=e keep=usubjid); if d and e;
  dm_ds_ex <- dm_ds %>%
    dplyr::semi_join(
      ex %>% dplyr::distinct(usubjid),
      by = "usubjid"
    )

  list(
    dm       = dm,
    ds       = ds,
    dm_ds    = dm_ds,
    dm_ds_ex = dm_ds_ex
  )
}


# =============================================================================
# ds_by_arm: Disposition frequency table by arm
# =============================================================================
#' Compute disposition event frequency table by treatment arm.
#'
#' Replaces SAS \code{%ds_by_arm(infile, outfile)} macro (lines 389-598 of
#' disposition_v2.sas). Performs DEATH-priority sorting, last-per-subject
#' deduplication with exceptions for Protocol Milestone / Informed Consent /
#' Randomized, arm-level frequency counts with randomized denominator (or
#' fallback), percentage computation via \code{janitor::round_half_up()}, and
#' wide-format pivot for display.
#'
#' @param data            Input data frame (dm_ds or dm_ds_ex).
#' @param checks          Named list from \code{ds_prelim_check()}.
#' @param dm_ds_fallback  Optional fallback data for denominator when no
#'                        RANDOMIZED rows are found. Typically the dm_ds frame.
#'
#' @return Named list: result (tibble), num_random (integer), ran_arm (tibble).
# -----------------------------------------------------------------------------
ds_by_arm <- function(data, checks, dm_ds_fallback = NULL) {

  if (nrow(data) == 0L) {
    return(list(
      result     = tibble::tibble(),
      num_random = 0L,
      ran_arm    = tibble::tibble(arm = character(), total_count = integer(),
                                  arm_n = integer())
    ))
  }

  # --- Step 1: Add DEATH priority (SAS lines 404-408) ---
  # DEATH gets order = 100 so it sorts last within a group
  data <- data %>%
    dplyr::mutate(
      order = dplyr::if_else(toupper(dsdecod) == "DEATH", 100L, 1L)
    )

  # --- Step 2: Sort (SAS lines 410-418) ---
  # by usubjid dscat dsscat dsdy order [dsseq] dsdecod
  sort_cols <- c("usubjid", "dscat", "dsscat", "dsdy", "order")
  if (checks$ds_dsseq && "dsseq" %in% names(data)) {
    sort_cols <- c(sort_cols, "dsseq")
  }
  sort_cols <- c(sort_cols, "dsdecod")

  data <- data %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # --- Step 3: Keep last disposition per subject per dscat/dsscat ---
  # EXCEPTIONS: PROTOCOL MILESTONE, INFORMED CONSENT OBTAINED, RANDOMIZED
  # keep ALL rows (SAS lines 422-432: if last.dsscat then output, BUT if
  # dscat=PROTOCOL MILESTONE or dsdecod starts with INFORMED CONSENT OBTAINED
  # or RANDOMIZED then also output).
  cscsl <- data %>%
    dplyr::group_by(usubjid, dscat, dsscat) %>%
    dplyr::filter(
      toupper(dscat) == "PROTOCOL MILESTONE" |
        grepl("^INFORMED CONSENT OBTAINED", toupper(dsdecod)) |
        grepl("^RANDOMIZED", toupper(dsdecod)) |
        dplyr::row_number() == dplyr::n()
    ) %>%
    dplyr::ungroup()

  # --- Step 4: Count distinct subjects per dscat/dsscat/dsdecod/arm ---
  # (SAS lines 434-439)
  by_cat <- cscsl %>%
    dplyr::group_by(dscat, dsscat, dsdecod, arm) %>%
    dplyr::summarise(num_by_cat = dplyr::n_distinct(usubjid),
                     .groups = "drop")

  # --- Step 5: Determine randomized denominator (SAS lines 447-491) ---
  # First try: distinct USUBJID where dsdecod starts with "RANDOM"
  ran_arm <- data %>%
    dplyr::filter(grepl("^RANDOM", toupper(dsdecod))) %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(total_count = dplyr::n_distinct(usubjid),
                     .groups = "drop")

  # Fallback: if no randomized rows found, use dm_ds or data itself
  if (nrow(ran_arm) == 0L) {
    fallback <- if (!is.null(dm_ds_fallback)) dm_ds_fallback else data
    ran_arm <- fallback %>%
      dplyr::group_by(arm) %>%
      dplyr::summarise(total_count = dplyr::n_distinct(usubjid),
                       .groups = "drop")
  }

  ran_arm <- ran_arm %>%
    dplyr::arrange(arm) %>%
    dplyr::mutate(arm_n = dplyr::row_number())

  num_random <- nrow(ran_arm)

  # --- Step 6: Join counts with denominators (SAS lines 495-502) ---
  by_cat_arm <- by_cat %>%
    dplyr::inner_join(ran_arm, by = "arm") %>%
    dplyr::arrange(dscat, dsscat, dsdecod, arm)

  # --- Step 7: Compute percentage with SAS-compatible rounding ---
  by_cat_arm <- by_cat_arm %>%
    dplyr::mutate(
      pct = janitor::round_half_up(100 * num_by_cat / total_count, digits = 2)
    )

  # --- Step 8: Pivot to wide format (SAS lines 506-534) ---
  # One row per dscat/dsscat/dsdecod with per-arm columns
  result <- by_cat_arm %>%
    tidyr::pivot_wider(
      id_cols     = c(dscat, dsscat, dsdecod),
      names_from  = arm_n,
      values_from = c(total_count, num_by_cat, pct, arm),
      names_glue  = "{.value}_{arm_n}",
      values_fill = list(total_count = 0, num_by_cat = 0, pct = 0,
                         arm = NA_character_)
    )

  # --- Step 9: Compute tot_perc and sorter (SAS lines 521-528) ---
  pct_cols <- names(dplyr::select(result, dplyr::starts_with("pct_")))
  result <- result %>%
    dplyr::mutate(
      tot_perc = rowSums(dplyr::across(dplyr::all_of(pct_cols)), na.rm = TRUE),
      sorter   = dplyr::if_else(
        grepl("Mile", dscat, ignore.case = TRUE), 1L, 2L
      )
    )

  # --- Step 10: Sort (SAS lines 536-545) ---
  result <- result %>%
    dplyr::arrange(sorter, dscat, dsscat, dplyr::desc(tot_perc))

  # --- Step 11: Blank repeated dscat/dsscat for display (SAS: suppressed) ---
  # Use lag() to detect when the current row repeats the previous row's value
  result <- result %>%
    dplyr::mutate(
      dscat_key  = paste(sorter, dscat, sep = "|"),
      dsscat_key = paste(sorter, dscat, dsscat, sep = "|"),
      dscat_display  = dplyr::if_else(
        !is.na(dplyr::lag(dscat_key)) & dscat_key == dplyr::lag(dscat_key),
        "", dscat
      ),
      dsscat_display = dplyr::if_else(
        !is.na(dplyr::lag(dsscat_key)) & dsscat_key == dplyr::lag(dsscat_key),
        "", dsscat
      )
    ) %>%
    dplyr::select(-dscat_key, -dsscat_key)

  # --- Step 12: Reorder columns ---
  # Build final column order: dscat_display, dsscat_display, dsdecod, then
  # for each arm: total_count_N, num_by_cat_N, pct_N, arm_N
  arm_col_groups <- lapply(seq_len(num_random), function(n) {
    c(paste0("total_count_", n),
      paste0("num_by_cat_", n),
      paste0("pct_", n),
      paste0("arm_", n))
  })
  display_cols <- c("dscat_display", "dsscat_display", "dsdecod",
                    unlist(arm_col_groups),
                    "tot_perc", "sorter", "dscat", "dsscat")

  # Keep only columns that exist (defensive)
  display_cols <- intersect(display_cols, names(result))
  result <- result %>% dplyr::select(dplyr::all_of(display_cols))

  # --- Step 13: Replace missing numerics with 0 (SAS PROC STDIZE) ---
  # DELIBERATE zero-fill for display per SAS PROC STDIZE REPONLY MISSING=0
  result <- result %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))
    )

  list(
    result     = result,
    num_random = num_random,
    ran_arm    = ran_arm
  )
}


# =============================================================================
# ds_time_to_event: Cumulative percentage time-to-event analysis
# =============================================================================
#' Compute cumulative percentage time-to-event data for disposition events.
#'
#' Replaces SAS \code{%time_to_event(ds)} macro (lines 604-917 of
#' disposition_v2.sas). Performs manual cumulative proportion computation
#' (NOT Kaplan-Meier) to preserve exact SAS functional parity. Deduplicates,
#' optionally filters by DSCAT/DSSCAT, counts events per arm x dsdecod x day,
#' expands day sequences filling gaps, computes cumulative counts and
#' percentages, then selects the top 12 dsdecod terms and pivots to wide
#' format with per-arm cumulative percent columns.
#'
#' @param data      Input data frame (dm_ds or dm_ds_ex).
#' @param checks    Named list from \code{ds_prelim_check()}.
#' @param catc      Character: column name for category (default "dscat").
#' @param subc      Character: column name for subcategory (default "dsscat").
#' @param catdis    Character or NULL: category filter value.
#' @param subcatdis Character or NULL: subcategory filter value.
#'
#' @return Named list: dsdecod_datasets (list of tibbles with attributes),
#'   num_terms (integer), num_arms (integer), arm_df (tibble),
#'   tte_fail_note (character or NULL), max_day (numeric).
# -----------------------------------------------------------------------------
ds_time_to_event <- function(data, checks, catc = "dscat", subc = "dsscat",
                              catdis = NULL, subcatdis = NULL) {

  # --- Guard: check prerequisites (SAS lines 611-616) ---
  if (!checks$ds_tte) {
    cli::cli_warn(
      paste("Study day or date variables not available for",
            "time-to-event analysis")
    )
    return(list(
      dsdecod_datasets = list(),
      num_terms        = 0L,
      num_arms         = 0L,
      arm_df           = tibble::tibble(arm = character(), n_arm = integer()),
      tte_fail_note    = paste(
        "Time to event analysis could not be performed because",
        "study day and date variables used to compute study day",
        "were not available"
      ),
      max_day          = NA_real_
    ))
  }

  if (nrow(data) == 0L) {
    return(list(
      dsdecod_datasets = list(),
      num_terms        = 0L,
      num_arms         = 0L,
      arm_df           = tibble::tibble(arm = character(), n_arm = integer()),
      tte_fail_note    = "No data available for time-to-event analysis",
      max_day          = NA_real_
    ))
  }

  # --- Step 1: Dedup (SAS lines 622-624) ---
  data_dedup <- data %>%
    dplyr::distinct(usubjid, dsdecod, dsdy, .keep_all = TRUE)

  # --- Step 2: Conditional filtering (SAS lines 626-684) ---
  filtered <- data_dedup

  if (!is.null(subcatdis) && subcatdis != "" &&
      !is.null(catdis) && catdis != "") {
    # Both subcatdis and catdis specified
    if (subc %in% names(filtered) && catc %in% names(filtered)) {
      filtered <- filtered %>%
        dplyr::filter(
          toupper(.data[[subc]]) == toupper(subcatdis),
          toupper(.data[[catc]]) == toupper(catdis)
        )
    }
  } else if (!is.null(catdis) && catdis != "") {
    # Only catdis specified
    if (catc %in% names(filtered)) {
      filtered <- filtered %>%
        dplyr::filter(toupper(.data[[catc]]) == toupper(catdis))
    }
  }
  # else: no filter

  # Drop rows with missing dsdy (cannot compute TTE without study day)
  filtered <- filtered %>%
    dplyr::filter(!is.na(dsdy))

  if (nrow(filtered) == 0L) {
    cli::cli_warn("No events with valid study day after filtering")
    return(list(
      dsdecod_datasets = list(),
      num_terms        = 0L,
      num_arms         = 0L,
      arm_df           = tibble::tibble(arm = character(), n_arm = integer()),
      tte_fail_note    = paste(
        "No events with valid study day found after filtering by",
        if (!is.null(catdis)) catdis else "(none)",
        "/",
        if (!is.null(subcatdis)) subcatdis else "(none)"
      ),
      max_day          = NA_real_
    ))
  }

  # --- Step 3: Count events per arm x dsdecod (SAS lines 632-636) ---
  by_dsdecod <- filtered %>%
    dplyr::group_by(arm, dsdecod) %>%
    dplyr::summarise(tot_dsdecod = dplyr::n(), .groups = "drop")

  # --- Step 4: Count events per arm x dsdecod x dsdy (SAS lines 638-644) ---
  by_dsdecod_day <- filtered %>%
    dplyr::group_by(arm, dsdecod, dsdy) %>%
    dplyr::summarise(by_dsdecod = dplyr::n(), .groups = "drop")

  # --- Step 5: Max study day (SAS lines 690-693) ---
  max_day <- max(by_dsdecod_day$dsdy, na.rm = TRUE)

  # --- Step 6: Cumulative count expansion (SAS lines 697-733) ---
  # CRITICAL: The SAS DATA step with RETAIN expands day sequences from
  # first.dsdecod start point (negative day or 0) through max_day, filling
  # gaps between event days. This is NOT Kaplan-Meier.
  cum_data <- by_dsdecod_day %>%
    dplyr::group_by(arm, dsdecod) %>%
    dplyr::arrange(dsdy) %>%
    dplyr::group_modify(~ {
      # Determine start day (SAS: if first.dsdecod and dsdy < 0 start there)
      min_obs_day <- min(.x$dsdy, na.rm = TRUE)
      start_day <- if (min_obs_day < 0) min_obs_day else 0L

      # Full day sequence from start_day to max_day
      full_days <- tibble::tibble(dsdy1 = seq(as.integer(start_day),
                                               as.integer(max_day)))

      # Left join to get event counts on each day
      result <- full_days %>%
        dplyr::left_join(
          .x %>%
            dplyr::select(dsdy, by_dsdecod) %>%
            dplyr::rename(dsdy1 = dsdy),
          by = "dsdy1"
        ) %>%
        dplyr::mutate(
          by_dsdecod  = tidyr::replace_na(by_dsdecod, 0L),
          cum_dsdecod = cumsum(by_dsdecod)
        )
      result
    }) %>%
    dplyr::ungroup()

  # --- Step 7: Cumulative percent (SAS lines 737-742) ---
  by_ds <- cum_data %>%
    dplyr::left_join(by_dsdecod, by = c("arm", "dsdecod")) %>%
    dplyr::mutate(cum_percent = cum_dsdecod / tot_dsdecod)

  # --- Step 8: Enumerate arms (SAS lines 747-758) ---
  arm_df <- by_dsdecod %>%
    dplyr::distinct(arm) %>%
    dplyr::arrange(arm) %>%
    dplyr::mutate(n_arm = dplyr::row_number())

  num_arms <- nrow(arm_df)

  # --- Step 9: Top 12 dsdecod terms by descending total events ---
  # (SAS lines 762-788)
  dsd_num <- by_dsdecod %>%
    dplyr::group_by(dsdecod) %>%
    dplyr::summarise(num_events = sum(tot_dsdecod), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(num_events)) %>%
    dplyr::mutate(order = dplyr::row_number())

  num_terms <- min(nrow(dsd_num), 12L)

  # --- Step 10: Build per-term wide datasets (SAS lines 792-898) ---
  dsdecod_datasets <- list()

  if (num_terms > 0L) {
    for (a in seq_len(num_terms)) {
      current_term <- dsd_num$dsdecod[a]

      # Filter to current term and join arm indices
      term_data <- by_ds %>%
        dplyr::inner_join(arm_df, by = "arm") %>%
        dplyr::filter(dsdecod == current_term) %>%
        dplyr::arrange(dsdy1)

      if (nrow(term_data) == 0L) next

      # Pivot to wide: c_perc1 .. c_percN (SAS: c_perc1-c_perc&num_arms)
      wide_term <- term_data %>%
        dplyr::select(dsdecod, dsdy1, n_arm, cum_percent) %>%
        tidyr::pivot_wider(
          names_from   = n_arm,
          values_from  = cum_percent,
          names_prefix = "c_perc",
          values_fill  = 0
        )

      # Build chart label (SAS: "Cumulative Chart for Disposition Event = X")
      dsd_label <- paste0(
        "Cumulative Chart for Disposition Event = ",
        gsub(";", "", current_term)
      )

      # Per-arm labels: "Arm Name Events = N" (SAS: for_labels)
      arm_labels <- by_dsdecod %>%
        dplyr::inner_join(arm_df, by = "arm") %>%
        dplyr::filter(dsdecod == current_term) %>%
        dplyr::arrange(n_arm) %>%
        dplyr::mutate(
          label = paste0(arm, " Events = ", tot_dsdecod)
        ) %>%
        dplyr::pull(label)

      attr(wide_term, "dsd_label")  <- dsd_label
      attr(wide_term, "arm_labels") <- arm_labels

      dsdecod_datasets[[a]] <- wide_term
    }
  }

  list(
    dsdecod_datasets = dsdecod_datasets,
    num_terms        = num_terms,
    num_arms         = num_arms,
    arm_df           = arm_df,
    tte_fail_note    = NULL,
    max_day          = max_day
  )
}


# =============================================================================
# ds_out: Excel workbook output for disposition panel
# =============================================================================
#' Write disposition analysis results to an Excel workbook.
#'
#' Replaces SAS \code{%ds_out} macro (lines 923-1014 of disposition_v2.sas).
#' Creates an Excel workbook via openxlsx with sheets for DispositionANew
#' (all subjects), DispositionBNew (exposed subjects), time-to-event chart
#' data sheets (Sheet1..SheetN, Sheet1E..SheetNE), and an Info metadata sheet.
#'
#' @param output_file          Character: full path for the output Excel file.
#' @param final_dispositionA   Tibble: frequency table for all subjects.
#' @param final_dispositionB   Tibble: frequency table for exposed subjects.
#' @param tte_all              Named list from ds_time_to_event() for all subj.
#' @param tte_exposed          Named list from ds_time_to_event() for exposed.
#' @param config               Named list from ds_params().
#' @param checks               Named list from ds_prelim_check().
#'
#' @return Invisible NULL. Side effect: writes Excel workbook to disk.
# -----------------------------------------------------------------------------
ds_out <- function(output_file, final_dispositionA, final_dispositionB,
                   tte_all = NULL, tte_exposed = NULL,
                   config, checks) {

  cli::cli_inform("Writing disposition output to {.file {output_file}}")

  wb <- openxlsx::createWorkbook()

  # --- DispositionANew: All subjects (SAS: data xls.DispositionANew) ---
  openxlsx::addWorksheet(wb, "DispositionANew")
  if (nrow(final_dispositionA) > 0L) {
    openxlsx::writeData(wb, "DispositionANew", final_dispositionA)
  }

  # --- DispositionBNew: Exposed subjects ---
  openxlsx::addWorksheet(wb, "DispositionBNew")
  if (nrow(final_dispositionB) > 0L) {
    openxlsx::writeData(wb, "DispositionBNew", final_dispositionB)
  }

  # --- TTE chart data sheets for All subjects (Sheet1..SheetN) ---
  if (!is.null(tte_all) && length(tte_all$dsdecod_datasets) > 0L) {
    for (a in seq_along(tte_all$dsdecod_datasets)) {
      ds_data <- tte_all$dsdecod_datasets[[a]]
      if (is.null(ds_data)) next
      sheet_name <- paste0("Sheet", a)
      openxlsx::addWorksheet(wb, sheet_name)

      # Write chart label as first row (SAS: label on the dataset)
      dsd_label <- attr(ds_data, "dsd_label") %||% ""
      arm_labels <- attr(ds_data, "arm_labels") %||% character(0)

      # Write metadata rows: chart label, then arm labels
      meta_rows <- tibble::tibble(
        info = c(dsd_label, arm_labels)
      )
      openxlsx::writeData(wb, sheet_name, meta_rows, startRow = 1)

      # Write the actual data below metadata
      start_row <- nrow(meta_rows) + 2L
      openxlsx::writeData(wb, sheet_name, ds_data, startRow = start_row)
    }
  }

  # --- TTE chart data sheets for Exposed subjects (Sheet1E..SheetNE) ---
  if (!is.null(tte_exposed) && length(tte_exposed$dsdecod_datasets) > 0L) {
    for (a in seq_along(tte_exposed$dsdecod_datasets)) {
      ds_data <- tte_exposed$dsdecod_datasets[[a]]
      if (is.null(ds_data)) next
      sheet_name <- paste0("Sheet", a, "E")
      openxlsx::addWorksheet(wb, sheet_name)

      dsd_label <- attr(ds_data, "dsd_label") %||% ""
      arm_labels <- attr(ds_data, "arm_labels") %||% character(0)

      meta_rows <- tibble::tibble(
        info = c(dsd_label, arm_labels)
      )
      openxlsx::writeData(wb, sheet_name, meta_rows, startRow = 1)

      start_row <- nrow(meta_rows) + 2L
      openxlsx::writeData(wb, sheet_name, ds_data, startRow = start_row)
    }
  }

  # --- Info metadata sheet (SAS: data xls.Info from work.lib) ---
  arm_var_desc <- if (checks$dm_actarm) {
    "actual treatment arm (ACTARM)"
  } else {
    "planned treatment arm (ARM)"
  }

  subset_desc <- paste0(
    "Subset where ", config$catc, " = ", config$catdis %||% "",
    if (!is.null(config$subcatdis) && config$subcatdis != "") {
      paste0(" and ", config$subc, " = ", config$subcatdis)
    } else {
      ""
    }
  )

  tte_note <- ""
  if (!is.null(tte_all) && !is.null(tte_all$tte_fail_note)) {
    tte_note <- tte_all$tte_fail_note
  }

  lib_info <- tibble::tibble(
    field = c("NDA/BLA", "Study ID", "Timestamp", "Custom Datasets",
              "TTE Note", "ARM Variable", "Subset"),
    value = c(
      config$ndabla %||% "",
      config$studyid %||% "",
      format(Sys.time(), "%Y-%m-%d %I:%M %p"),
      config$sl_custom_ds %||% "",
      tte_note,
      arm_var_desc,
      subset_desc
    )
  )

  openxlsx::addWorksheet(wb, "Info")
  openxlsx::writeData(wb, "Info", lib_info)

  # --- Save workbook ---
  # Ensure the output directory exists
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  cli::cli_inform("Disposition output saved to {.file {output_file}}")

  invisible(NULL)
}


# =============================================================================
# run_disposition: Main orchestrator for Disposition Analysis Panel
# =============================================================================
#' Execute the full Disposition Analysis Panel workflow.
#'
#' Replaces SAS \code{%ds} orchestrator macro (lines 1020-1061 of
#' disposition_v2.sas). Runs preliminary checks; if passed, performs dataset
#' setup, by-arm frequency analysis for all and exposed subjects,
#' time-to-event analysis for both populations, group/subset preprocessing,
#' writes Excel output, disposition data checks, and group/subset metadata.
#' If preliminary checks fail, writes an error summary workbook.
#'
#' @param config Named list from ds_params(). If NULL, ds_params() is called
#'               with additional arguments passed via \code{...}.
#' @param ...    Additional arguments passed to ds_params() when config is NULL.
#'
#' @return Invisible named list with all intermediate results.
# -----------------------------------------------------------------------------
run_disposition <- function(config = NULL, ...) {

  # --- Initialize config if not provided ---
  if (is.null(config)) {
    config <- ds_params(...)
  }

  cli::cli_inform("Starting Disposition Analysis Panel")

  # --- Preliminary checks (SAS line 1023) ---
  checks <- ds_prelim_check(config$dm, config$ds, config$ex)

  if (checks$dm_subj_gt0 && checks$all_req_var) {
    # ---------------------------------------------------------------
    # Happy path: all prerequisites met
    # ---------------------------------------------------------------
    cli::cli_inform("All prerequisite checks passed")

    # Step 1: Dataset setup (SAS line 1028)
    setup_result <- ds_setup(config$dm, config$ds, config$ex, checks)

    # Step 2: By-arm frequency for all subjects (SAS line 1031)
    disp_a <- ds_by_arm(
      setup_result$dm_ds, checks,
      dm_ds_fallback = setup_result$dm_ds
    )

    # Step 3: By-arm frequency for exposed subjects (SAS line 1034)
    disp_b <- ds_by_arm(
      setup_result$dm_ds_ex, checks,
      dm_ds_fallback = setup_result$dm_ds
    )

    # Step 4: Time-to-event for all subjects (SAS line 1037)
    tte_all <- ds_time_to_event(
      setup_result$dm_ds, checks,
      catc = config$catc, subc = config$subc,
      catdis = config$catdis, subcatdis = config$subcatdis
    )

    # Step 5: Time-to-event for exposed subjects (SAS line 1040)
    tte_exp <- ds_time_to_event(
      setup_result$dm_ds_ex, checks,
      catc = config$catc, subc = config$subc,
      catdis = config$catdis, subcatdis = config$subcatdis
    )

    # Step 6: Group/subset preprocessing (SAS line 1043)
    pp_result <- group_subset_pp(
      config$sl_group  %||% tibble::tibble(),
      config$sl_subset %||% tibble::tibble(),
      config$sl_datasets %||% tibble::tibble()
    )

    # Step 7: Write main output (SAS line 1046)
    ds_out(
      config$dispout, disp_a$result, disp_b$result,
      tte_all, tte_exp, config, checks
    )

    # Step 8: Disposition data checks (SAS lines 1049-1050)
    check_results <- disposition_check(
      setup_result$dm_ds, setup_result$dm_ds_ex,
      config$dm, config$ds
    )

    # Step 9: Write disposition check output (SAS line 1052)
    disposition_check_out(
      config$dispout, check_results, disp_a$ran_arm
    )

    # Step 10: Write group/subset metadata (SAS line 1055)
    group_subset_write_xlsx(gs_file = config$dispout, pp_result = pp_result)

    cli::cli_inform("Disposition Analysis Panel completed successfully")

    invisible(list(
      config       = config,
      checks       = checks,
      setup_result = setup_result,
      disp_a       = disp_a,
      disp_b       = disp_b,
      tte_all      = tte_all,
      tte_exp      = tte_exp,
      pp_result    = pp_result,
      check_results = check_results
    ))

  } else {
    # ---------------------------------------------------------------
    # Error path: prerequisites not met (SAS lines 1057-1061)
    # ---------------------------------------------------------------
    cli::cli_warn("Prerequisite checks failed; generating error summary")

    error_summary(
      err_file        = config$errout,
      panel_title     = config$panel_title %||% "Disposition",
      ndabla          = config$ndabla %||% "",
      studyid         = config$studyid %||% "",
      err_nosubj      = !checks$dm_subj_gt0,
      err_missvar     = !checks$all_req_var,
      rpt_chk_var_req = checks$rpt_chk_var_req,
      panel_desc      = config$panel_desc %||% ""
    )

    cli::cli_inform("Error summary written to {.file {config$errout}}")

    invisible(list(
      config  = config,
      checks  = checks,
      error   = TRUE
    ))
  }
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    - SAS %params macro -> ds_params() R function; all paths parameterized
#    - SAS global macro variable side effects -> function return value lists
#    - SAS PROC STDIZE MISSING=0 -> deliberate replace_na(., 0) for display
#    - SAS propcase ARM logic -> custom R function preserving MG/KG/ML rules
#    - SAS DEATH priority (order=100) -> preserved exactly in R sort
#    - SAS last.dsscat BY-group logic -> dplyr group_by + filter(row_number()
#      == n())
#    - Time-to-event: manual cumulative proportion (NOT Kaplan-Meier)
#    - survival package loaded but not used for TTE computation per SAS parity
#    - SAS "PROTOCOL MILESTONE" and "INFORMED CONSENT OBTAINED" and
#      "RANDOMIZED" exceptions preserved in deduplication logic
#    - Column names normalized to lowercase for case-insensitive SAS parity
#    - Screen failure exclusion via ARMCD matching (SCRNFAIL, NOTASSGN)
#    - Empty SL tibbles created for non-Script-Launcher runs
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Date arithmetic: R as.numeric(date1 - date2) matches SAS integer
#      subtraction
#    - SAS +1 rule for same-day/post-reference dates preserved explicitly
#    - Rounding: janitor::round_half_up() for all percentages
#    - Sort stability: dplyr::arrange() is stable within groups, matching SAS
#      BY-group
#    - PROC STDIZE zero fill: preserved via replace_na(., 0) -- deliberate
#    - Cumulative percentage: simple proportion (events/total), NOT KM estimate
#    - Floating point: R uses 8-byte IEEE 754 doubles like SAS; epsilon
#      comparisons may differ at extreme precision
# NO DIRECT R EQUIVALENT:
#    - SAS RETAIN statement -> cumsum() or carry-forward within grouped ops
#    - SAS lag() within DATA step -> dplyr::lag() (semantically equivalent)
#    - SAS PCFILES/JET Excel engine -> openxlsx
#    - SAS %sysfunc(ifc()) -> if_else() or dplyr::if_else()
#    - SAS options missing='' -> not applicable (R uses NA display)
#    - SAS propcase() -> custom propcase_arm() function
#    - SAS BY-group first.var/last.var -> dplyr group operations
#    - SAS LIBNAME Excel engine -> openxlsx::createWorkbook()
#    - SAS PROC SORT NODUPKEY -> dplyr::distinct(.keep_all = TRUE)
# PACKAGE SELECTION RATIONALE:
#    - haven: SAS data I/O (AAP mandated)
#    - dplyr/tidyr: core data manipulation (AAP mandated, tidyverse over base)
#    - survival/survminer: loaded per AAP requirement, available for future use
#    - openxlsx: Excel output replacing JET/PCFILES (AAP mandated)
#    - janitor: round_half_up for SAS-compatible rounding (AAP mandated)
#    - lubridate: date arithmetic (AAP mandated over base R)
#    - stringr: string manipulation (tidyverse mandate)
#    - cli: informative error/warning messages
#    - ggplot2: cumulative percentage chart rendering (available for extensions)
#    - Tplyr/r2rtf: loaded per AAP for clinical table/RTF output readiness
#    - tibble: enhanced data frames (AAP mandated over base data.frame)
#    - purrr: functional programming (AAP mandated over base lapply/sapply)
#    - rlang: tidy evaluation (.data pronoun, %||% operator)
#    - forcats: factor manipulation for arm ordering (loaded per AAP)
# OPEN QUESTIONS:
#    - Template file (Disposition_Template.xls): Is template-based output
#      needed in R, or does fresh workbook creation suffice?
#    - Script Launcher integration: How does R version integrate with PhUSE SL?
#    - DSDY computation: Verify day-0 exclusion rule matches SAS exactly for
#      edge cases (same-day events at reference start)
#    - Exposed subjects: Confirm semi_join preserves correct exposed population
#      when EX has multiple records per USUBJID
#    - Top-12 DSDECOD terms: Verify tie-breaking when num_events are equal
#      across terms (currently uses alphabetical order as secondary sort)
#    - Cumulative % vs KM: Confirmed SAS output is simple proportion, not
#      survival estimate; survival package retained for API completeness
#    - ProperCase logic: SAS anylower() function check mirrors grepl("[a-z]",.)
#      -- verify behavior for non-ASCII characters in multinational studies
# =============================================================================
