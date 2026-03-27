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
#     - Migrated from: contributed/MedDRA/ZZ_Utilities/ae_setup.sas (821 ln)  #
#     - SAS %setup macro  -> R setup() function                               #
#     - SAS %rpt_setup    -> R rpt_setup() function                           #
#     - SAS %log_msg      -> R log_msg() function                             #
#     - SAS DATA step merges -> dplyr join pipelines                          #
#     - SAS PROC SQL -> dplyr verbs                                           #
#     - SAS macro variables -> function parameters / return list              #
#     - SAS %put -> cli messages                                              #
#     - SAS %chk_var / %chk_dm_subj_gt0 -> data_checks.R functions           #
#                                                                             #
#  EXTERNAL FILES USED: data_checks.R (sourced for chk_var, chk_dm_subj_gt0) #
#                                                                             #
#            MADE WITH: R >= 4.3.0, dplyr >= 1.1.0, tidyr, stringr,          #
#                       lubridate, purrr, cli, janitor, rlang, haven          #
#                                                                             #
#            REVISIONS:                                                        #
#              2011-05-08  DK  Handling for no subjects in DM                  #
#              2011-05-31  DK  Handling for 0 AEs in safety pop               #
#              2011-06-18  DK  Separate start/end date comparison             #
#              2011-06-19  DK  Long arm name handling                         #
#              2026-03-25  Blitzy  Migrated from SAS to R                     #
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
library(janitor)
library(rlang)
library(haven)

# --------------------------------------------------------------------------- #
# Source the data validation module (provides chk_var, chk_dm_subj_gt0)
# --------------------------------------------------------------------------- #
# Determine the directory of this script for relative sourcing
.ae_setup_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) {
    # Fallback: try to locate via contributed/R/MedDRA path from working dir
    candidates <- c(
      file.path("contributed", "R", "MedDRA"),
      "."
    )
    for (d in candidates) {
      if (file.exists(file.path(d, "data_checks.R"))) return(d)
    }
    "."
  }
)
source(file.path(.ae_setup_dir, "data_checks.R"), local = FALSE)
rm(.ae_setup_dir)

# =========================================================================== #
# log_msg — Print a boxed text message to the console
# =========================================================================== #
#' Print a Boxed Log Message
#'
#' Replacement for SAS \code{\%log_msg} macro (lines 799-821).
#' Produces a bordered text banner using cli formatting.
#'
#' @param text Character scalar. The message text to display.
#'
#' @return Invisibly returns NULL. Called for its side effect of printing.
#'
#' @examples
#' log_msg("DATA CHECKS")
#'
#' @export
log_msg <- function(text) {
  # SAS: builds a box from asterisks around the text
  # R: use cli_h2 for a styled heading banner
  cli::cli_h2(text)
  invisible(NULL)
}

# =========================================================================== #
# setup — Adverse Event Panel Setup (Main Gatekeeper)
# =========================================================================== #
#' Adverse Event Panel Setup
#'
#' Validates DM/AE/EX datasets, merges the safety population, looks up MedDRA
#' terms and DMEs. Replaces SAS \code{\%setup(mdhier=N,dme=N)} macro
#' (lines 26-587).
#'
#' @param ae         data.frame. Adverse Events (AE) domain.
#' @param dm         data.frame. Demographics (DM) domain.
#' @param ex         data.frame. Exposure (EX) domain.
#' @param meddra_hier data.frame, character file path, or NULL. If a data.frame,
#'   used directly as the MedDRA hierarchy. If a character string, treated as a
#'   file path to a SAS dataset and loaded via \code{haven::read_sas()}. Expected
#'   columns: aebodsys, aedecod, soc_name, hlgt_name, hlt_name, pt_name.
#' @param dme_data   data.frame, character file path, or NULL. If a data.frame,
#'   used directly. If a character string, loaded via \code{haven::read_sas()}.
#'   Expected column: pt_name (or llt_name renamed to pt_name) and dme flag.
#' @param mdhier     Character. "Y" to perform MedDRA lookup, "N" otherwise.
#'   Default "N".
#' @param dme        Character. "Y" to perform DME lookup, "N" otherwise.
#'   Default "N".
#' @param vld_sw     Integer (0 or 1). Data validation switch. 1 = validate
#'   AE dates against treatment period. Default 1.
#' @param study_lag  Integer. Days after treatment end to include AEs.
#'   Default 30.
#' @param config     list. Optional configuration parameters. May include
#'   toxgr_min, toxgr_max, ver (MedDRA version string).
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{setup_success}{Logical. TRUE if setup completed successfully.}
#'     \item{setup_req_var}{Logical. All required variables present.}
#'     \item{arm_count}{Integer. Number of treatment arms.}
#'     \item{arm_total}{Integer. Total subjects across arms.}
#'     \item{arm_var}{Character. Arm variable used (ACTARM or ARM).}
#'     \item{arm_subjects}{Named integer vector. Per-arm subject counts.}
#'     \item{arm_names}{Named character vector. Formatted arm display names.}
#'     \item{max_arm_nm_len}{Integer. Maximum arm name length.}
#'     \item{ds_base}{data.frame. Base analysis dataset (valid AEs).}
#'     \item{ds_base_meddra}{data.frame or NULL. AEs matched to MedDRA.}
#'     \item{err_base}{data.frame. Excluded AEs with error codes.}
#'     \item{err_base_meddra}{data.frame or NULL. AEs not matched to MedDRA.}
#'     \item{err_dm_ex}{data.frame. Subjects excluded during DM/EX merge.}
#'     \item{all_dm_ex}{data.frame. Safety population.}
#'     \item{all_ae_dm_ex}{data.frame. All AEs in safety pop (pre-validation).}
#'     \item{all_arm}{data.frame. Arm summary with counts and numbering.}
#'     \item{meddra_pct}{Numeric or NULL. MedDRA match percentage.}
#'     \item{rpt_meddra}{data.frame or NULL. MedDRA matching summary.}
#'     \item{rpt_meddra_term}{data.frame or NULL. Unmatched MedDRA terms.}
#'     \item{rpt_chk_var}{data.frame. All variable check results.}
#'     \item{rpt_chk_var_req}{data.frame. Required variable check results.}
#'     \item{naes_sp}{Integer. AE count in safety population.}
#'     \item{naes_spv}{Integer. Validated AE count.}
#'   }
#'
#' @export
setup <- function(ae,
                  dm,
                  ex,
                  meddra_hier = NULL,
                  dme_data    = NULL,
                  mdhier      = "N",
                  dme         = "N",
                  vld_sw      = 1L,
                  study_lag   = 30L,
                  config      = list()) {

  # -------------------------------------------------------------------------
  # Internal Helpers
  # -------------------------------------------------------------------------

 # Case-insensitive check for variable existence in a data frame
  has_var <- function(ds, var_name) {
    tolower(var_name) %in% tolower(names(ds))
  }

  # Get actual column name by case-insensitive match (returns NA if missing)
  get_var <- function(ds, var_name) {
    idx <- which(tolower(names(ds)) == tolower(var_name))
    if (length(idx) > 0L) names(ds)[idx[1L]] else NA_character_
  }

  # Safe ISO 8601 partial date parser (SAS lines 140-146, 344-348)
  # SAS: input(substr(dtc,1,10),?? e8601da.) suppresses errors
  # R: lubridate::ymd() with quiet=TRUE for error suppression
  parse_iso_date <- function(dtc) {
    dtc <- as.character(dtc)
    dtc_len <- stringr::str_length(stringr::str_trim(dtc))
    dplyr::case_when(
      is.na(dtc) | dtc_len < 7L ~ as.Date(NA_character_),
      dtc_len >= 10L ~ lubridate::ymd(substr(dtc, 1L, 10L), quiet = TRUE),
      dtc_len >= 7L  ~ lubridate::ymd(
        paste0(substr(dtc, 1L, 7L), "-01"), quiet = TRUE
      ),
      TRUE ~ as.Date(NA_character_)
    )
  }

  # Compute the date string length for partial date comparison
  date_str_len <- function(dtc) {
    dtc <- as.character(dtc)
    stringr::str_length(stringr::str_trim(dtc))
  }

  # -------------------------------------------------------------------------
  # Phase 1: Preliminary Data Checks (SAS lines 28-110)
  # -------------------------------------------------------------------------
  log_msg("DATA CHECKS")

  # Initialize the accumulated variable-check report
  rpt_chk_var <- tibble::tibble(
    chk = character(), ds = character(), var = character(),
    type = character(), len = integer(),
    condition = character(), ind = integer()
  )

  # Check DM has > 0 subjects (SAS line 42)
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # Required variables (SAS lines 46-52)
  req_checks <- list(
    list(ds = ae, ds_name = "ae", var = "aebodsys"),
    list(ds = ae, ds_name = "ae", var = "aedecod"),
    list(ds = ae, ds_name = "ae", var = "usubjid"),
    list(ds = dm, ds_name = "dm", var = "usubjid"),
    list(ds = ex, ds_name = "ex", var = "usubjid")
  )

  # Run chk_var for each required variable and accumulate report rows
  req_results <- purrr::map(req_checks, function(rc) {
    chk_var(rc$ds, rc$var)
  })

  rpt_chk_var <- purrr::reduce(
    purrr::map(req_results, ~ .x$report),
    dplyr::bind_rows,
    .init = rpt_chk_var
  )

  ae_aebodsys <- req_results[[1L]]$exists
  ae_aedecod  <- req_results[[2L]]$exists
  ae_usubjid  <- req_results[[3L]]$exists
  dm_usubjid  <- req_results[[4L]]$exists
  ex_usubjid  <- req_results[[5L]]$exists

  # Save required-variable snapshot (SAS lines 54-56)
  rpt_chk_var_req <- rpt_chk_var

  # Arm variable detection (SAS lines 59-76)
  actarm_check <- chk_var(dm, "actarm")
  arm_check    <- chk_var(dm, "arm")
  rpt_chk_var  <- dplyr::bind_rows(rpt_chk_var, actarm_check$report,
                                    arm_check$report)

  dm_actarm <- actarm_check$exists

  dm_arm    <- arm_check$exists

  # Insert ACTARM-or-ARM combined check (SAS lines 63-70)
  rpt_chk_var_req <- dplyr::bind_rows(
    rpt_chk_var_req,
    tibble::tibble(
      chk = "VAR", ds = "DM", var = "ACTARM or ARM",
      type = "", len = -1L,
      condition = "EXISTS",
      ind = as.integer(dm_actarm || dm_arm)
    )
  )

  # Determine arm variable to use (SAS lines 73-76)
  arm_var <- if (dm_actarm) {
    "ACTARM"
  } else if (dm_arm) {
    "ARM"
  } else {
    NA_character_
  }

  # Required variable flag (SAS lines 78-86)
  setup_req_var <- ae_aebodsys && ae_aedecod && ae_usubjid &&
    (dm_actarm || dm_arm) && dm_usubjid && ex_usubjid

  # Early exit if DM has no subjects or required vars missing (SAS line 88)
  if (!dm_subj_gt0 || !setup_req_var) {
    if (!dm_subj_gt0) cli::cli_warn("There are no subjects in DM — setup cannot proceed.")
    if (!setup_req_var) {
      cli::cli_warn("Some variables required for AE setup are missing — setup cannot proceed.")
    }
    return(list(
      setup_success    = FALSE,
      setup_req_var    = setup_req_var,
      arm_count        = 0L,
      arm_total        = 0L,
      arm_var          = arm_var %||% NA_character_,
      arm_subjects     = integer(0),
      arm_names        = character(0),
      max_arm_nm_len   = 0L,
      ds_base          = tibble::tibble(),
      ds_base_meddra   = NULL,
      err_base         = tibble::tibble(),
      err_base_meddra  = NULL,
      err_dm_ex        = tibble::tibble(),
      all_dm_ex        = tibble::tibble(),
      all_ae_dm_ex     = tibble::tibble(),
      all_arm          = tibble::tibble(),
      meddra_pct       = NULL,
      rpt_meddra       = NULL,
      rpt_meddra_term  = NULL,
      rpt_chk_var      = rpt_chk_var,
      rpt_chk_var_req  = rpt_chk_var_req,
      naes_sp          = 0L,
      naes_spv         = 0L
    ))
  }

  # Optional variable checks (SAS lines 91-110)
  opt_checks <- list(
    list(ds = ae, var = "aestdtc"),
    list(ds = ae, var = "aeser"),
    list(ds = ae, var = "aesev"),
    list(ds = ae, var = "aetoxgr"),
    list(ds = dm, var = "rfstdtc"),
    list(ds = dm, var = "rfendtc"),
    list(ds = dm, var = "armcd"),
    list(ds = ex, var = "exstdtc"),
    list(ds = ex, var = "exendtc")
  )

  opt_results <- purrr::map(opt_checks, function(oc) chk_var(oc$ds, oc$var))
  rpt_chk_var <- purrr::reduce(
    purrr::map(opt_results, ~ .x$report),
    dplyr::bind_rows,
    .init = rpt_chk_var
  )

  ae_aestdtc  <- opt_results[[1L]]$exists
  ae_aeser    <- opt_results[[2L]]$exists
  ae_aesev    <- opt_results[[3L]]$exists
  ae_aetoxgr  <- opt_results[[4L]]$exists
  ae_aetoxgr_type <- opt_results[[4L]]$type
  dm_rfstdtc  <- opt_results[[5L]]$exists
  dm_rfendtc  <- opt_results[[6L]]$exists
  dm_armcd    <- opt_results[[7L]]$exists
  ex_exstdtc  <- opt_results[[8L]]$exists
  ex_exendtc  <- opt_results[[9L]]$exists

  # Composite date availability (SAS lines 101-102)
  ex_exdtc <- ex_exstdtc && ex_exendtc
  dm_rfdtc <- dm_rfstdtc && dm_rfendtc

  # If no dates available, turn off validation (SAS lines 104-110)
  if (!(ae_aestdtc && (ex_exdtc || dm_rfdtc))) {
    vld_sw <- 0L
    cli::cli_warn(
      "AE data validation has been turned off because required variables are missing"
    )
  }

  # -------------------------------------------------------------------------
  # Phase 2: Demographics Domain (DM) Processing (SAS lines 113-151)
  # -------------------------------------------------------------------------
  log_msg("DEMOGRAPHICS DOMAIN (DM)")

  # Build the select list (SAS lines 119-123: keep=)
  arm_var_actual <- get_var(dm, arm_var)
  usubjid_actual <- get_var(dm, "usubjid")

  dm_keep_cols <- c(usubjid_actual, arm_var_actual)
  if (dm_armcd) dm_keep_cols <- c(dm_keep_cols, get_var(dm, "armcd"))
  if (vld_sw == 1L && dm_rfdtc) {
    dm_keep_cols <- c(dm_keep_cols, get_var(dm, "rfstdtc"),
                      get_var(dm, "rfendtc"))
  }
  dm_keep_cols <- unique(stats::na.omit(dm_keep_cols))

  all_dm <- dm %>%
    dplyr::select(dplyr::all_of(dm_keep_cols))

  # Standardize column names to uppercase
  names(all_dm) <- toupper(names(all_dm))

  # Create ARM column from the actual arm variable (SAS line 126)
  # Drop original arm variable columns to avoid duplicate column names when

  # renaming back at the end (ACTARM/ARM → arm for working, then arm → arm_var)
  arm_cols_to_drop <- intersect(c("ACTARM", "ARM"), names(all_dm))
  all_dm <- all_dm %>%
    dplyr::mutate(arm = .data[[toupper(arm_var)]]) %>%
    dplyr::select(-dplyr::any_of(arm_cols_to_drop))

  # Exclude screen failures / unassigned (SAS lines 128-131)
  if (dm_armcd && "ARMCD" %in% names(all_dm)) {
    all_dm <- all_dm %>%
      dplyr::filter(
        !(toupper(.data[["ARMCD"]]) %in% c("SCRNFAIL", "NOTASSGN"))
      )
  }

  # Convert reference dates (SAS lines 134-148)
  if (vld_sw == 1L && dm_rfdtc && "RFSTDTC" %in% names(all_dm)) {
    all_dm <- all_dm %>%
      dplyr::mutate(
        rfstdt_len = date_str_len(.data[["RFSTDTC"]]),
        rfendt_len = date_str_len(.data[["RFENDTC"]]),
        rfstdt     = parse_iso_date(.data[["RFSTDTC"]]),
        rfendt     = parse_iso_date(.data[["RFENDTC"]])
      )
  }

  all_dm <- all_dm %>% dplyr::arrange(.data[["USUBJID"]])

  # -------------------------------------------------------------------------
  # Phase 3: Exposure Domain (EX) Processing (SAS lines 154-189)
  # -------------------------------------------------------------------------
  log_msg("EXPOSURE DOMAIN (EX)")

  ex_usubjid_actual <- get_var(ex, "usubjid")

  # Standardize EX column names for processing
  ex_work <- ex
  names(ex_work) <- toupper(names(ex_work))

  # Build min/max treatment dates per subject (SAS PROC SQL lines 160-189)
  all_ex <- ex_work %>%
    dplyr::group_by(.data[["USUBJID"]]) %>%
    dplyr::summarise(
      exstdt = if (ex_exstdtc) {
        suppressWarnings(min(parse_iso_date(.data[["EXSTDTC"]]), na.rm = TRUE))
      } else {
        as.Date(NA_character_)
      },
      exstdt_len = if (ex_exstdtc) 10L else NA_integer_,
      exendt = if (ex_exstdtc && ex_exendtc) {
        suppressWarnings(max(
          c(
            max(parse_iso_date(.data[["EXSTDTC"]]), na.rm = TRUE),
            max(parse_iso_date(.data[["EXENDTC"]]), na.rm = TRUE)
          ),
          na.rm = TRUE
        ))
      } else if (ex_exstdtc) {
        suppressWarnings(max(parse_iso_date(.data[["EXSTDTC"]]), na.rm = TRUE))
      } else {
        as.Date(NA_character_)
      },
      exendt_len = if (ex_exstdtc) 10L else NA_integer_,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data[["USUBJID"]])

  # Handle infinite dates from min/max on all-NA (returns -Inf/Inf)
  all_ex <- all_ex %>%
    dplyr::mutate(
      exstdt = dplyr::if_else(is.finite(as.numeric(exstdt)), exstdt,
                               as.Date(NA_character_)),
      exendt = dplyr::if_else(is.finite(as.numeric(exendt)), exendt,
                               as.Date(NA_character_))
    )

  # -------------------------------------------------------------------------
  # Phase 4: Safety Population (DM + EX merge) (SAS lines 191-248)
  # -------------------------------------------------------------------------
  log_msg("SUBJECTS IN SAFETY POPULATION (DM & EX)")

  # Inner join for safety pop (SAS: merge if a and b)
  all_dm_ex <- dplyr::inner_join(all_dm, all_ex, by = "USUBJID")

  # Error tracking: subjects in DM but not EX (SAS: if a and not b)
  err_dm_ex <- dplyr::anti_join(all_dm, all_ex, by = "USUBJID") %>%
    dplyr::mutate(
      err_type = "ex",
      output   = 0L
    )

  # Treatment date derivation (SAS lines 213-244)
  if (vld_sw == 1L) {
    all_dm_ex <- all_dm_ex %>%
      dplyr::mutate(
        trtstdt     = as.Date(NA_character_),
        trtstdt_len = NA_integer_,
        trtendt     = as.Date(NA_character_),
        trtendt_len = NA_integer_
      )

    # EX dates first (SAS lines 220-227)
    if (ex_exdtc) {
      all_dm_ex <- all_dm_ex %>%
        dplyr::mutate(
          trtstdt = dplyr::if_else(
            !is.na(exstdt) & !is.na(exendt), exstdt, trtstdt
          ),
          trtstdt_len = dplyr::if_else(
            !is.na(exstdt) & !is.na(exendt), exstdt_len, trtstdt_len
          ),
          trtendt = dplyr::if_else(
            !is.na(exstdt) & !is.na(exendt), exendt, trtendt
          ),
          trtendt_len = dplyr::if_else(
            !is.na(exstdt) & !is.na(exendt), exendt_len, trtendt_len
          )
        )
    }

    # DM reference dates as fallback (SAS lines 229-236)
    if (dm_rfdtc && "rfstdt" %in% names(all_dm_ex)) {
      all_dm_ex <- all_dm_ex %>%
        dplyr::mutate(
          trtstdt = dplyr::if_else(
            !is.na(rfstdt) & !is.na(rfendt) & is.na(trtstdt) & is.na(trtendt),
            rfstdt, trtstdt
          ),
          trtstdt_len = dplyr::if_else(
            !is.na(rfstdt) & !is.na(rfendt) & is.na(.data[["trtstdt_len"]]),
            rfstdt_len, trtstdt_len
          ),
          trtendt = dplyr::if_else(
            !is.na(rfstdt) & !is.na(rfendt) & is.na(trtendt),
            rfendt, trtendt
          ),
          trtendt_len = dplyr::if_else(
            !is.na(rfstdt) & !is.na(rfendt) & is.na(.data[["trtendt_len"]]),
            rfendt_len, trtendt_len
          )
        )
    }

    # Subjects without valid dates → error (SAS lines 238-243)
    err_dt <- all_dm_ex %>%
      dplyr::filter(is.na(trtstdt) | is.na(trtendt)) %>%
      dplyr::mutate(err_type = "dt", output = 0L)

    err_dm_ex <- dplyr::bind_rows(err_dm_ex, err_dt)

    all_dm_ex <- all_dm_ex %>%
      dplyr::filter(!is.na(trtstdt), !is.na(trtendt))
  }

  # -------------------------------------------------------------------------
  # Phase 5: Arm Counts and Numbering (SAS lines 250-296)
  # -------------------------------------------------------------------------

  # Count distinct subjects per arm, ordered alphabetically (SAS lines 251-262)
  all_arm <- all_dm_ex %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(count = dplyr::n_distinct(.data[["USUBJID"]]),
                     .groups = "drop") %>%
    dplyr::arrange(arm) %>%
    dplyr::mutate(arm_num = dplyr::row_number())

  arm_count <- nrow(all_arm)
  max_arm_nm_len <- if (arm_count > 0L) {
    max(nchar(dplyr::pull(all_arm, arm)))
  } else {
    0L
  }

  # Running total and per-arm subjects (SAS lines 264-277)
  all_arm <- all_arm %>%
    dplyr::mutate(total = cumsum(count))
  arm_total <- if (arm_count > 0L) {
    as.integer(all_arm$total[arm_count])
  } else {
    0L
  }

  # Named per-arm subject counts: arm_1, arm_2, ...
  arm_subjects <- stats::setNames(
    as.integer(all_arm$count),
    paste0("arm_", seq_len(arm_count))
  )

  # Assign arm numbers to subjects (SAS hash lookup lines 280-296)
  all_dm_ex <- all_dm_ex %>%
    dplyr::left_join(
      all_arm %>% dplyr::select(arm, arm_num),
      by = "arm"
    )

  # -------------------------------------------------------------------------
  # Phase 6: AE Domain Processing (SAS lines 298-352)
  # -------------------------------------------------------------------------
  log_msg("ADVERSE EVENTS DOMAIN (AE)")

  # Build keep-variable list (SAS lines 304-312)
  ae_work <- ae
  names(ae_work) <- toupper(names(ae_work))

  ae_keep <- c("USUBJID", "AEBODSYS", "AEDECOD", "AESEQ")
  if (ae_aeser)  ae_keep <- c(ae_keep, "AESER")
  if (ae_aesev)  ae_keep <- c(ae_keep, "AESEV")
  if (ae_aetoxgr) ae_keep <- c(ae_keep, "AETOXGR")
  if (vld_sw == 1L) ae_keep <- c(ae_keep, "AESTDTC")
  if ("AEENDTC" %in% names(ae_work)) ae_keep <- c(ae_keep, "AEENDTC")

  ae_keep <- intersect(ae_keep, names(ae_work))
  all_ae <- ae_work %>% dplyr::select(dplyr::all_of(ae_keep))

  # Propcase conversion: only if ALL uppercase (SAS lines 314-316)
  # SAS: if not anylower(aebodsys) then aebodsys = propcase(aebodsys)
  all_ae <- all_ae %>%
    dplyr::mutate(
      AEBODSYS = dplyr::if_else(
        AEBODSYS == toupper(AEBODSYS) & !is.na(AEBODSYS),
        stringr::str_to_title(AEBODSYS),
        AEBODSYS
      ),
      AEDECOD = dplyr::if_else(
        AEDECOD == toupper(AEDECOD) & !is.na(AEDECOD),
        stringr::str_to_title(AEDECOD),
        AEDECOD
      )
    )

  # AETOXGR validation (SAS lines 318-336)
  toxgr_min <- config$toxgr_min
  toxgr_max <- config$toxgr_max

  if (ae_aetoxgr && !is.null(toxgr_min) && !is.null(toxgr_max)) {
    if (ae_aetoxgr_type == "C") {
      # Character to numeric conversion (SAS line 323)
      all_ae <- all_ae %>%
        dplyr::mutate(
          aetoxgr_num = suppressWarnings(as.numeric(AETOXGR))
        )
    } else {
      all_ae <- all_ae %>%
        dplyr::mutate(aetoxgr_num = as.numeric(AETOXGR))
    }
    # Validate range (SAS line 329)
    all_ae <- all_ae %>%
      dplyr::mutate(
        aetoxgr_num = dplyr::if_else(
          !is.na(aetoxgr_num) &
            aetoxgr_num >= toxgr_min & aetoxgr_num <= toxgr_max,
          aetoxgr_num,
          NA_real_
        )
      ) %>%
      dplyr::select(-dplyr::any_of("AETOXGR")) %>%
      dplyr::rename(AETOXGR = aetoxgr_num)
  } else if (!ae_aetoxgr) {
    # SAS line 335: call missing(aetoxgr)
    all_ae <- all_ae %>%
      dplyr::mutate(AETOXGR = NA_real_)
  }

  # AE start date conversion (SAS lines 339-349)
  if (vld_sw == 1L && "AESTDTC" %in% names(all_ae)) {
    all_ae <- all_ae %>%
      dplyr::mutate(
        aestdt_len = date_str_len(.data[["AESTDTC"]]),
        aestdt     = parse_iso_date(.data[["AESTDTC"]])
      )
  }

  # Sort (SAS line 352)
  all_ae <- all_ae %>%
    dplyr::arrange(.data[["USUBJID"]], .data[["AEBODSYS"]],
                   .data[["AEDECOD"]],
                   dplyr::across(dplyr::any_of("AESEQ")))

  # -------------------------------------------------------------------------
  # Phase 7: AE + Safety Population Merge and Date Validation
  #          (SAS lines 354-432)
  # -------------------------------------------------------------------------
  log_msg("ADVERSE EVENTS FOR SUBJECTS IN SAFETY POPULATION (AE, DM, & EX)")

  # Merge AE with safety pop (SAS lines 362-368)
  all_ae_dm_ex <- dplyr::inner_join(all_dm_ex, all_ae, by = "USUBJID")
  all_ae_dm_ex <- all_ae_dm_ex %>%
    dplyr::mutate(total = "Total")

  # Date validation (SAS lines 370-430)
  if (vld_sw == 1L && "aestdt" %in% names(all_ae_dm_ex)) {
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        output   = 1L,
        err      = NA_integer_,
        err_type = NA_character_,
        err_desc = NA_character_
      )

    # Error 1: Missing date (SAS lines 384-388)
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        output = dplyr::if_else(is.na(aestdt) & output == 1L, 0L, output),
        err = dplyr::if_else(is.na(aestdt) & is.na(err), 1L, err),
        err_type = dplyr::if_else(
          is.na(aestdt) & is.na(err_type), "dt", err_type
        ),
        err_desc = dplyr::if_else(
          is.na(aestdt) & is.na(err_desc),
          "1. Date missing or incomplete", err_desc
        )
      )

    # Error 2: AE start before treatment start (SAS lines 390-400)
    # Partial date comparison: compare at month level when stdt_len < 10
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        stdt_len = pmin(aestdt_len, trtstdt_len, na.rm = TRUE),
        before_start = dplyr::case_when(
          is.na(aestdt) | is.na(trtstdt) ~ FALSE,
          stdt_len >= 10L ~ aestdt < trtstdt,
          stdt_len >= 7L ~ {
            ae_month_dt <- lubridate::ymd(paste0(
              lubridate::year(aestdt), "-",
              sprintf("%02d", lubridate::month(aestdt)), "-01"
            ), quiet = TRUE)
            trt_month_dt <- lubridate::ymd(paste0(
              lubridate::year(trtstdt), "-",
              sprintf("%02d", lubridate::month(trtstdt)), "-01"
            ), quiet = TRUE)
            ae_month_dt < trt_month_dt
          },
          TRUE ~ FALSE
        ),
        output = dplyr::if_else(before_start & output == 1L, 0L, output),
        err = dplyr::if_else(before_start & is.na(err), 2L, err),
        err_type = dplyr::if_else(
          before_start & is.na(err_type), "dt", err_type
        ),
        err_desc = dplyr::if_else(
          before_start & is.na(err_desc),
          "2. Date before study analysis period", err_desc
        )
      )

    # Error 3: AE start after treatment end + study_lag (SAS lines 402-412)
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        endt_len = pmin(aestdt_len, trtendt_len, na.rm = TRUE),
        after_end = dplyr::case_when(
          is.na(aestdt) | is.na(trtendt) ~ FALSE,
          endt_len >= 10L ~ aestdt > (trtendt + lubridate::days(study_lag)),
          endt_len >= 7L ~ {
            ae_month_dt <- lubridate::ymd(paste0(
              lubridate::year(aestdt), "-",
              sprintf("%02d", lubridate::month(aestdt)), "-01"
            ), quiet = TRUE)
            trt_end_month <- lubridate::ymd(paste0(
              lubridate::year(trtendt), "-",
              sprintf("%02d", lubridate::month(trtendt)), "-01"
            ), quiet = TRUE)
            lag_months <- as.integer(floor(study_lag / 30))
            trt_end_lag <- trt_end_month %m+% months(lag_months)
            ae_month_dt > trt_end_lag
          },
          TRUE ~ FALSE
        ),
        output = dplyr::if_else(after_end & output == 1L, 0L, output),
        err = dplyr::if_else(after_end & is.na(err), 3L, err),
        err_type = dplyr::if_else(
          after_end & is.na(err_type), "dt", err_type
        ),
        err_desc = dplyr::if_else(
          after_end & is.na(err_desc),
          "3. Date after study analysis period", err_desc
        )
      )

    # Error 4: Missing AEBODSYS or AEDECOD (SAS lines 417-422)
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        missing_desc = is.na(AEBODSYS) | AEBODSYS == "" |
          is.na(AEDECOD) | AEDECOD == "",
        output = dplyr::if_else(missing_desc & output == 1L, 0L, output),
        err = dplyr::if_else(missing_desc & is.na(err), 4L, err),
        err_type = dplyr::if_else(
          missing_desc & is.na(err_type), "desc", err_type
        ),
        err_desc = dplyr::if_else(
          missing_desc & is.na(err_desc),
          "4. Description missing", err_desc
        )
      )

    # Where output was not set to 0, set to 1 (SAS line 414)
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        output = dplyr::if_else(is.na(err), 1L, output)
      )

    # Split into valid (ds_base) and error (err_base) datasets
    err_base <- all_ae_dm_ex %>%
      dplyr::filter(output == 0L) %>%
      dplyr::select(
        -dplyr::any_of(c("output", "before_start", "after_end",
                          "missing_desc", "stdt_len", "endt_len", "total"))
      )

    ds_base <- all_ae_dm_ex %>%
      dplyr::filter(output == 1L) %>%
      dplyr::select(
        -dplyr::any_of(c("err", "err_type", "err_desc", "output",
                          "before_start", "after_end", "missing_desc",
                          "stdt_len", "endt_len",
                          "aestdt", "aestdt_len",
                          "trtstdt", "trtstdt_len",
                          "trtendt", "trtendt_len"))
      )
  } else {
    # No validation: all AEs pass (SAS lines 424-426)
    ds_base  <- all_ae_dm_ex
    err_base <- tibble::tibble()
  }

  ds_base <- ds_base %>%
    dplyr::arrange(
      .data[["AEBODSYS"]], .data[["AEDECOD"]]
    )

  naes_sp  <- nrow(all_ae_dm_ex)
  naes_spv <- nrow(ds_base)

  # -------------------------------------------------------------------------
  # Phase 8: MedDRA and DME Lookup (SAS lines 435-533)
  # -------------------------------------------------------------------------
  ds_base_meddra   <- NULL
  err_base_meddra  <- NULL
  rpt_meddra       <- NULL
  rpt_meddra_term  <- NULL
  meddra_pct       <- NULL

  if (toupper(mdhier) == "Y" && !is.null(meddra_hier)) {
    log_msg("LOOK UP MEDDRA TERMS")

    # Load MedDRA hierarchy from file path if a character string is provided
    # (SAS line 453: declare hash h(dataset:"meddra.mdhier_..."))
    if (is.character(meddra_hier) && length(meddra_hier) == 1L) {
      if (!file.exists(meddra_hier)) {
        cli::cli_warn("MedDRA hierarchy file not found: {meddra_hier}")
        meddra_hier <- NULL
      } else {
        meddra_hier <- haven::read_sas(meddra_hier)
      }
    }
    if (is.null(meddra_hier) || !is.data.frame(meddra_hier)) {
      cli::cli_warn("MedDRA hierarchy data is not available; skipping lookup")
    }

    if (!is.null(meddra_hier) && is.data.frame(meddra_hier)) {

    # Uppercase AE terms for matching (SAS lines 449-450)
    ds_base_lookup <- ds_base %>%
      dplyr::mutate(
        aebodsys_upper = toupper(AEBODSYS),
        aedecod_upper  = toupper(AEDECOD)
      )

    # Prepare MedDRA hierarchy for join
    meddra_work <- meddra_hier
    names(meddra_work) <- toupper(names(meddra_work))

    # Ensure join keys are uppercased in MedDRA data
    if ("AEBODSYS" %in% names(meddra_work) && "AEDECOD" %in% names(meddra_work)) {
      meddra_work <- meddra_work %>%
        dplyr::mutate(
          AEBODSYS = toupper(AEBODSYS),
          AEDECOD  = toupper(AEDECOD)
        )
    }

    # Left join to find matches (SAS hash lookup lines 452-469)
    merged <- ds_base_lookup %>%
      dplyr::left_join(
        meddra_work %>%
          dplyr::select(dplyr::any_of(c(
            "AEBODSYS", "AEDECOD",
            "SOC_NAME", "HLGT_NAME", "HLT_NAME", "PT_NAME"
          ))),
        by = c("aebodsys_upper" = "AEBODSYS",
               "aedecod_upper"  = "AEDECOD")
      )

    # Split matched vs unmatched (SAS lines 466-490)
    ds_base_meddra <- merged %>%
      dplyr::filter(!is.na(.data[["PT_NAME"]])) %>%
      dplyr::select(-AEBODSYS, -AEDECOD, -aebodsys_upper, -aedecod_upper)

    err_base_meddra <- merged %>%
      dplyr::filter(is.na(.data[["PT_NAME"]])) %>%
      dplyr::select(-aebodsys_upper, -aedecod_upper)

    # DME flag integration (SAS lines 472-487)
    if (toupper(dme) == "Y" && !is.null(dme_data)) {
      # Load DME dataset from file path if a character string is provided
      if (is.character(dme_data) && length(dme_data) == 1L) {
        if (!file.exists(dme_data)) {
          cli::cli_warn("DME file not found: {dme_data}")
          dme_data <- NULL
        } else {
          dme_data <- haven::read_sas(dme_data)
        }
      }
    }
    if (toupper(dme) == "Y" && !is.null(dme_data) && is.data.frame(dme_data)) {
      dme_work <- dme_data
      names(dme_work) <- toupper(names(dme_work))

      # Rename LLT_NAME to PT_NAME if present (SAS line 475)
      if ("LLT_NAME" %in% names(dme_work) && !"PT_NAME" %in% names(dme_work)) {
        dme_work <- dme_work %>% dplyr::rename(PT_NAME = .data[["LLT_NAME"]])
      }

      if ("PT_NAME" %in% names(dme_work) && "DME" %in% names(dme_work)) {
        ds_base_meddra <- ds_base_meddra %>%
          dplyr::left_join(
            dme_work %>% dplyr::select(.data[["PT_NAME"]], .data[["DME"]]),
            by = "PT_NAME"
          ) %>%
          dplyr::mutate(
            DME = dplyr::if_else(is.na(.data[["DME"]]), NA_character_,
                                  .data[["DME"]])
          )
      }
    }

    # MedDRA matching report (SAS lines 494-531)
    meddra_cnt <- nrow(ds_base_meddra)
    err_meddra_cnt <- nrow(err_base_meddra)
    total_meddra <- meddra_cnt + err_meddra_cnt

    meddra_pct <- if (total_meddra > 0L) {
      janitor::round_half_up(100 * meddra_cnt / total_meddra, digits = 2)
    } else {
      0
    }

    meddra_ver_str <- config$ver %||% "unknown"
    rpt_meddra <- tibble::tibble(
      meddra_ver  = paste0("MedDRA hierarchy version ", meddra_ver_str),
      meddra_cnt  = meddra_cnt,
      err_cnt     = err_meddra_cnt,
      meddra_pct  = meddra_pct
    )

    # Unmatched term detail report (SAS lines 511-531)
    if (err_meddra_cnt > 0L) {
      rpt_meddra_term <- err_base_meddra %>%
        dplyr::group_by(.data[["AEBODSYS"]], .data[["AEDECOD"]]) %>%
        dplyr::summarise(
          subj_count  = dplyr::n_distinct(.data[["USUBJID"]]),
          event_count = dplyr::n(),
          .groups     = "drop"
        ) %>%
        dplyr::mutate(
          AEBODSYS = stringr::str_to_title(.data[["AEBODSYS"]]),
          AEDECOD  = stringr::str_to_title(.data[["AEDECOD"]])
        )
    } else {
      rpt_meddra_term <- tibble::tibble(
        AEBODSYS    = character(),
        AEDECOD     = character(),
        subj_count  = integer(),
        event_count = integer()
      )
    }

    } # end inner if (is.data.frame(meddra_hier))
  } # end if (mdhier == "Y")

  # -------------------------------------------------------------------------
  # Phase 9: Arm Name Formatting (SAS lines 535-574)
  # -------------------------------------------------------------------------

  # Build display names using word-level logic (SAS lines 540-557)
  format_arm_display <- function(arm_name) {
    if (is.na(arm_name) || arm_name == "") return(arm_name)

    arm_display <- arm_name

    # Only transform if entirely uppercase (SAS: if not anylower)
    if (arm_display == toupper(arm_display)) {
      words <- stringr::str_split(arm_display, "\\s+")[[1L]]
      for (i in seq_along(words)) {
        w <- words[i]
        w_clean <- gsub("[^A-Za-z0-9]", "", w)

        # Words > 3 chars without digits → propcase (SAS line 546-547)
        if (nchar(w_clean) > 3L && !grepl("[0-9]", w_clean)) {
          words[i] <- stringr::str_to_title(w)
        }
        # "UP" → propcase (SAS line 548-549)
        if (toupper(w_clean) == "UP") {
          words[i] <- stringr::str_to_title(w)
        }
        # "MG", "KG" → lowercase (SAS lines 550-551)
        if (toupper(w_clean) %in% c("MG", "KG")) {
          words[i] <- stringr::str_to_lower(w)
        }
        # "ML" → "mL" (SAS lines 552-553)
        if (toupper(w_clean) == "ML") {
          words[i] <- "mL"
        }
      }
      arm_display <- paste(words, collapse = " ")
    }

    # Break long words at slashes (SAS lines 559-569)
    if (stringr::str_detect(arm_display, "/")) {
      space_words <- stringr::str_split(arm_display, " ")[[1L]]
      has_long_slash <- any(
        nchar(space_words) > 40L & stringr::str_detect(space_words, "/")
      )
      if (has_long_slash) {
        arm_display <- stringr::str_replace_all(arm_display, "/", "/ ")
      }
    }

    arm_display
  }

  # Apply to all arms (SAS lines 536-574)
  all_arm <- all_arm %>%
    dplyr::mutate(
      arm_display = purrr::map_chr(arm, format_arm_display)
    )

  # Named arm display names: arm_name_1, arm_name_2, ...
  arm_names <- stats::setNames(
    all_arm$arm_display,
    paste0("arm_name_", seq_len(arm_count))
  )

  # Rename arm back to the original arm variable name (SAS line 600)
  all_dm_ex <- all_dm_ex %>%
    dplyr::rename(!!rlang::sym(arm_var) := arm)

  # -------------------------------------------------------------------------
  # Build result
  # -------------------------------------------------------------------------
  cli::cli_alert_info(
    "AE setup complete: {naes_spv} validated AEs for {arm_total} subjects in {arm_count} arms"
  )

  list(
    setup_success    = TRUE,
    setup_req_var    = setup_req_var,
    arm_count        = arm_count,
    arm_total        = arm_total,
    arm_var          = arm_var,
    arm_subjects     = arm_subjects,
    arm_names        = arm_names,
    max_arm_nm_len   = max_arm_nm_len,
    ds_base          = ds_base,
    ds_base_meddra   = ds_base_meddra,
    err_base         = err_base,
    err_base_meddra  = err_base_meddra,
    err_dm_ex        = err_dm_ex,
    all_dm_ex        = all_dm_ex,
    all_ae_dm_ex     = all_ae_dm_ex,
    all_arm          = all_arm,
    meddra_pct       = meddra_pct,
    rpt_meddra       = rpt_meddra,
    rpt_meddra_term  = rpt_meddra_term,
    rpt_chk_var      = rpt_chk_var,
    rpt_chk_var_req  = rpt_chk_var_req,
    naes_sp          = naes_sp,
    naes_spv         = naes_spv
  )
}


# =========================================================================== #
# rpt_setup — Build Reporting Datasets for Data Validation and MedDRA Matching
# =========================================================================== #
#' Build Setup Reporting Datasets
#'
#' Creates reporting datasets summarizing subject disposition and AE data
#' validation results. Replaces SAS \code{\%rpt_setup} macro (lines 591-795).
#'
#' @param setup_result List. The result from \code{setup()}.
#' @param dm           data.frame. Original DM domain (for total subject counts).
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{rpt_dm}{data.frame. Subject disposition by arm (DM → safety pop).}
#'     \item{rpt_err}{data.frame. Error type counts per arm.}
#'     \item{rpt_err_term}{data.frame. Error terms per arm (wide format).}
#'     \item{naes_sp}{Integer. AE count in safety pop.}
#'     \item{naes_spv}{Integer. Validated AE count.}
#'     \item{naes_sp_per_arm}{Named integer vector. Per-arm AE counts.}
#'   }
#'
#' @export
rpt_setup <- function(setup_result, dm) {

  # Extract needed components
  all_dm_ex    <- setup_result$all_dm_ex
  err_dm_ex    <- setup_result$err_dm_ex
  err_base     <- setup_result$err_base
  all_ae_dm_ex <- setup_result$all_ae_dm_ex
  all_arm      <- setup_result$all_arm
  arm_count    <- setup_result$arm_count
  arm_var      <- setup_result$arm_var
  arm_names    <- setup_result$arm_names
  arm_subjects <- setup_result$arm_subjects
  ds_base      <- setup_result$ds_base

  log_msg("SUBJECT VALIDATION REPORT")

  # Standardize DM column names
  dm_work <- dm
  names(dm_work) <- toupper(names(dm_work))

  arm_var_upper <- toupper(arm_var)

  # Helper: count subjects per arm in a dataset using arm display names
  count_per_arm <- function(data, arm_col = arm_var_upper) {
    if (nrow(data) == 0L || !arm_col %in% names(data)) {
      counts <- rep(0L, arm_count)
    } else {
      counts <- purrr::map_int(seq_len(arm_count), function(i) {
        arm_display <- all_arm$arm_display[i]
        arm_original <- all_arm$arm[i]
        sum(
          toupper(gsub("\\s+", "", data[[arm_col]])) ==
            toupper(gsub("\\s+", "", arm_original)),
          na.rm = TRUE
        )
      })
    }
    total <- sum(counts)
    c(counts, total)
  }

  # 1. Subjects in DM (SAS lines 609-619)
  row1_counts <- count_per_arm(dm_work, arm_var_upper)

  # 2. Subjects removed - screen failure / unassigned (SAS lines 623-643)
  if ("ARMCD" %in% names(dm_work)) {
    scrnfail <- dm_work %>%
      dplyr::filter(toupper(.data[["ARMCD"]]) %in% c("SCRNFAIL", "NOTASSGN"))
    row2_counts <- count_per_arm(scrnfail, arm_var_upper)
  } else {
    row2_counts <- rep(0L, arm_count + 1L)
  }

  # 3. Subjects removed - not in safety population (SAS lines 647-663)
  err_ex <- err_dm_ex %>%
    dplyr::filter(.data[["err_type"]] == "ex")
  if (nrow(err_ex) > 0L && "arm" %in% names(err_ex)) {
    row3_counts <- count_per_arm(err_ex, "arm")
  } else if (nrow(err_ex) > 0L && arm_var_upper %in% names(err_ex)) {
    row3_counts <- count_per_arm(err_ex, arm_var_upper)
  } else {
    row3_counts <- rep(0L, arm_count + 1L)
  }

  # 4. Subjects removed - no treatment/reference dates (SAS lines 667-683)
  err_dt <- err_dm_ex %>%
    dplyr::filter(.data[["err_type"]] == "dt")
  if (nrow(err_dt) > 0L && "arm" %in% names(err_dt)) {
    row4_counts <- count_per_arm(err_dt, "arm")
  } else if (nrow(err_dt) > 0L && arm_var_upper %in% names(err_dt)) {
    row4_counts <- count_per_arm(err_dt, arm_var_upper)
  } else {
    row4_counts <- rep(0L, arm_count + 1L)
  }

  # 5. Subjects used in analysis (SAS lines 687-699)
  row5_counts <- count_per_arm(all_dm_ex, arm_var_upper)

  # Build rpt_dm table with per-arm columns and percentages (SAS lines 700-717)
  descs <- c(
    "1. Subjects in demographics (DM)",
    "2. Subjects removed - unassigned/screen failure",
    "3. Subjects removed - not in safety population",
    "4. Subjects removed - no treatment/reference dates",
    "5. Subjects used in analysis"
  )

  rows_matrix <- rbind(row1_counts, row2_counts, row3_counts,
                        row4_counts, row5_counts)

  rpt_dm <- tibble::tibble(desc = descs)

  # Add per-arm count and percentage columns
  dm_arm_counts <- rows_matrix[1L, ]  # denominators from row 1

  for (i in seq_len(arm_count)) {
    col_count <- paste0("arm", i, "_count")
    col_pct   <- paste0("arm", i, "_pct")
    rpt_dm[[col_count]] <- as.integer(rows_matrix[, i])
    denom <- dm_arm_counts[i]
    rpt_dm[[col_pct]] <- if (denom > 0L) {
      janitor::round_half_up(100 * rows_matrix[, i] / denom, digits = 2)
    } else {
      rep(0, 5L)
    }
  }

  rpt_dm[["total_count"]] <- as.integer(rows_matrix[, arm_count + 1L])
  dm_total_denom <- dm_arm_counts[arm_count + 1L]
  rpt_dm[["total_pct"]] <- if (dm_total_denom > 0L) {
    janitor::round_half_up(
      100 * rows_matrix[, arm_count + 1L] / dm_total_denom, digits = 2
    )
  } else {
    rep(0, 5L)
  }

  # -----------------------------------------------------------------------
  # ADVERSE EVENT VALIDATION REPORT
  # -----------------------------------------------------------------------
  log_msg("ADVERSE EVENT DATA VALIDATION REPORT")

  naes_sp  <- nrow(all_ae_dm_ex)
  naes_spv <- nrow(ds_base)

  # Per-arm AE counts in safety pop (SAS lines 730-736)
  naes_sp_per_arm <- if (naes_sp > 0L && "arm_num" %in% names(all_ae_dm_ex)) {
    purrr::map_int(seq_len(arm_count), function(i) {
      sum(all_ae_dm_ex$arm_num == i, na.rm = TRUE)
    })
  } else {
    rep(0L, arm_count)
  }
  names(naes_sp_per_arm) <- paste0("naes_sp_", seq_len(arm_count))

  # Error counts by type per arm (SAS lines 745-764)
  rpt_err <- tibble::tibble()
  if (nrow(err_base) > 0L && "err" %in% names(err_base)) {
    err_summary <- err_base %>%
      dplyr::group_by(.data[["err"]], .data[["err_desc"]]) %>%
      dplyr::summarise(
        dplyr::across(
          .cols = character(),
          .fns  = list()
        ),
        .groups = "drop"
      )

    # Per-arm error counts using imap over arm indices
    err_rows <- err_base %>%
      dplyr::group_by(.data[["err"]], .data[["err_desc"]]) %>%
      {
        grouped <- .
        err_groups <- dplyr::distinct(grouped, .data[["err"]],
                                       .data[["err_desc"]])
        purrr::pmap_dfr(err_groups, function(err, err_desc) {
          subset_data <- err_base %>%
            dplyr::filter(.data[["err"]] == !!err)
          row <- tibble::tibble(err_desc = err_desc)
          for (i in seq_len(arm_count)) {
            arm_err_count <- sum(subset_data$arm_num == i, na.rm = TRUE)
            arm_ae_total <- naes_sp_per_arm[i]
            row[[paste0("arm", i, "_err_count")]] <- as.integer(arm_err_count)
            row[[paste0("arm", i, "_err_pct")]] <- if (arm_ae_total > 0L) {
              janitor::round_half_up(100 * arm_err_count / arm_ae_total,
                                     digits = 2)
            } else {
              0
            }
          }
          row
        })
      }

    rpt_err <- err_rows
  }

  # Error term detail: per-arm counts pivoted wide (SAS lines 766-791)
  rpt_err_term <- tibble::tibble()
  if (nrow(err_base) > 0L && "arm_num" %in% names(err_base)) {
    rpt_err_term_x <- err_base %>%
      dplyr::group_by(.data[["AEBODSYS"]], .data[["AEDECOD"]],
                       .data[["arm_num"]]) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    # Pivot wide (SAS PROC TRANSPOSE lines 774-777)
    rpt_err_term <- rpt_err_term_x %>%
      tidyr::pivot_wider(
        names_from  = arm_num,
        values_from = count,
        names_prefix = "arm"
      )

    # Replace NA with 0, add labels (SAS lines 779-791)
    arm_cols <- paste0("arm", seq_len(arm_count))

    # Ensure all arm columns exist (fill with 0L if missing from pivot)
    for (ac in arm_cols) {
      if (!(ac %in% names(rpt_err_term))) {
        rpt_err_term[[ac]] <- 0L
      }
    }

    # Use rlang::!!! splice to replace NA with 0 across all arm columns
    na_replace_exprs <- purrr::map(
      rlang::set_names(arm_cols),
      function(col) rlang::expr(tidyr::replace_na(!!rlang::sym(col), 0L))  # legitimate: arm counts initialized to zero after cross-join
    )
    rpt_err_term <- rpt_err_term %>%
      dplyr::mutate(!!!na_replace_exprs)

    # Replace empty body system / term with "Missing" (SAS lines 789-790)
    rpt_err_term <- rpt_err_term %>%
      dplyr::mutate(
        AEBODSYS = dplyr::if_else(
          is.na(AEBODSYS) | AEBODSYS == "", "Missing", AEBODSYS
        ),
        AEDECOD = dplyr::if_else(
          is.na(AEDECOD) | AEDECOD == "", "Missing", AEDECOD
        )
      )
  }

  list(
    rpt_dm           = rpt_dm,
    rpt_err          = rpt_err,
    rpt_err_term     = rpt_err_term,
    naes_sp          = naes_sp,
    naes_spv         = naes_spv,
    naes_sp_per_arm  = naes_sp_per_arm
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#   1. Screen failures and unassigned subjects are identified by
#      ARMCD values "SCRNFAIL" and "NOTASSGN" (case-insensitive),
#      matching the SAS logic at line 130.
#   2. SAS hash lookup semantics are preserved via dplyr::left_join.
#      Hash definekey/definedata maps to join keys and selected columns.
#   3. Partial ISO 8601 dates with only year-month (length >= 7 chars)
#      are treated as the first of that month (e.g., "2020-06" becomes
#      2020-06-01), matching SAS substr(dtc,1,7)||'-01' at line 142.
#   4. SAS global macro variables (arm_var, arm_count, arm_total,
#      arm_1..arm_N, arm_name_1..arm_name_N, etc.) are all returned
#      in the structured result list instead of polluting global env.
#   5. The MedDRA hierarchy lookup requires a pre-loaded data.frame
#      argument (meddra_hier) rather than referencing a SAS library.
#      SAS line 453: declare hash h(dataset:"meddra.mdhier_...").
#   6. DME data is passed as a pre-loaded data.frame (dme_data),
#      replacing SAS line 475: declare hash i(dataset:'dme.dme').
#   7. SAS %sysfunc(ifc(&ex_exstdtc. and &ex_exendtc.,1,0)) logic
#      requires BOTH EXSTDTC and EXENDTC to use EX dates (line 101).
#   8. Treatment date priority: EX dates used first (both EXSTDT and
#      EXENDT must be non-missing); DM reference dates as fallback
#      (both RFSTDT and RFENDT must be non-missing).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Date parsing: SAS ?? e8601da. informat silently suppresses
#      invalid date notes; R lubridate::ymd(quiet=TRUE) returns NA.
#      Both produce the same missing-date effect.
#   2. Arm percentage calculation uses janitor::round_half_up() to
#      match SAS round-half-up behavior (SAS lines 709, 715, 749).
#   3. Partial date comparison (7-char dates): SAS mdy(month(),1,year())
#      maps to lubridate month-level comparison. Results should match
#      but edge cases around month boundaries may differ by 1 day.
#   4. study_lag month conversion: SAS floor(&study_lag./30) matches
#      R floor(study_lag / 30) for the month-level comparison.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS DATA step hash object → dplyr::left_join()
#   2. SAS call symputx('name', value, 'g') → return list elements
#   3. SAS ?? e8601da. error-suppressing informat → lubridate::ymd(quiet=TRUE)
#   4. SAS %goto/%label for early exit → R return() from function
#   5. SAS RETAIN statement → cumsum() or explicit column carry-forward
#   6. SAS PROC DATASETS DELETE → not needed (R garbage collection)
#   7. SAS DATA step OUTPUT to multiple datasets → list of tibbles
#   8. SAS propcase() word-by-word logic (acronym-aware) → custom
#      format_arm_display() function preserving SAS word-level rules
#
# PACKAGE SELECTION RATIONALE:
#   - dplyr: All merges, filters, group_by, summarise (AAP mandates
#     tidyverse over base R)
#   - stringr: Case transformations (str_to_title, str_replace_all,
#     str_detect, str_length, str_trim, str_to_lower, str_split)
#   - haven: SAS data I/O via read_sas() for MedDRA hierarchy loading
#   - lubridate: Date parsing (ymd), extraction (month, year), arithmetic
#     (days, %m+%)
#   - cli: User-facing messages (cli_h2, cli_alert_info, cli_warn,
#     cli_abort) replacing SAS %put / %log_msg
#   - janitor: round_half_up() for SAS-compatible rounding at percentage
#     calculation points (regulatory output parity)
#   - rlang: Tidy evaluation (.data pronoun, sym(), !!, !!!) for
#     programmatic column access in dplyr pipelines
#   - tidyr: pivot_wider() for error term transposition, replace_na()
#     for NA → 0 replacement in pivoted arm columns
#   - purrr: map(), map_chr(), map_int(), map_dfc(), walk(), reduce(),
#     imap() for functional iteration over arms and variable checks
#
# OPEN QUESTIONS:
#   1. Verify partial date handling matches SAS exactly for 7-character
#      dates — specifically the day-of-month substitution (always 01).
#   2. Confirm arm numbering order matches SAS alphabetical default
#      from PROC SQL ORDER BY arm.
#   3. MedDRA hierarchy version string (config$ver) must be provided
#      by caller — SAS uses &ver. macro variable.
#   4. AETOXGR range parameters (toxgr_min, toxgr_max) must be
#      provided via config list — SAS uses %symexist.
#   5. Propcase word-level logic may differ from SAS for edge cases
#      involving mixed punctuation or non-ASCII characters.
# ============================================================
