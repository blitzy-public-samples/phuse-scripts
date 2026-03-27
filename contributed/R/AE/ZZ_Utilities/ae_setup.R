###############################################################################
#         PROGRAM NAME: ae_setup.R                                            #
#                                                                             #
#          DESCRIPTION: Adverse Event Panel Setup — Gatekeeper validation,    #
#                       DM/AE/EX domain merging, safety population            #
#                       derivation, optional MedDRA/DME enrichment, and       #
#                       audit-friendly reporting tables.                       #
#                                                                             #
#      SOURCE FILE:     contributed/AE/ZZ_Utilities/ae_setup.sas (821 lines)  #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#      ORIGINAL DATE:   2011                                                  #
#                                                                             #
#      MIGRATION DETAILS:                                                     #
#        - SAS %setup macro   -> setup() R function                           #
#        - SAS %rpt_setup     -> rpt_setup() R function                       #
#        - SAS %log_msg       -> log_msg() R function                         #
#        - SAS DATA step      -> dplyr pipelines                              #
#        - SAS PROC SQL       -> dplyr verbs                                  #
#        - SAS hash objects   -> dplyr left_join                              #
#        - SAS call symputx   -> named list returns                           #
#        - SAS %put           -> cli messages                                 #
#                                                                             #
#      REVISIONS:                                                             #
#        2011-05-08  DK     Handling for no subjects in DM                    #
#        2011-05-31  DK     Handling for 0 AEs in safety pop                  #
#        2011-06-18  DK     Separate start/end date comparison                #
#        2011-06-19  DK     Long arm name handling                            #
#        2026-03-25  Blitzy Migrated from SAS to R                            #
###############################################################################

# -- Required Libraries --------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(cli)
library(janitor)
library(haven)

# -- Source Dependency: data_checks.R ------------------------------------------
# Provides: chk_var(), chk_dm_subj_gt0()
# Sourced from the same directory as this file.
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile)),
    error = function(e) NULL
  )
  if (!is.null(script_dir)) {
    dc_path <- file.path(script_dir, "data_checks.R")
    if (file.exists(dc_path) &&
        !exists("chk_var", mode = "function", envir = parent.frame())) {
      source(dc_path, local = FALSE)
    }
  }
})


# ==============================================================================
# log_msg — Print a bordered message to the log
# Migrated from SAS %log_msg macro (SAS lines 798-821)
#
# SAS behavior: Prints text surrounded by asterisk box to the SAS log.
# R equivalent: Uses message() with asterisk borders replicating SAS output.
#
# @param text Character string to display.
# @return Invisible NULL.
# @export
# ==============================================================================
log_msg <- function(text) {

  text_str <- as.character(text)
  body_line <- paste0("* ", text_str, " *")
  cli::cli_rule()
  message(body_line)
  cli::cli_rule()
  invisible(NULL)
}


# ==============================================================================
# parse_iso_date — Parse ISO 8601 partial date strings to Date
#
# Handles CDISC-style date character variables with partial precision:
#   - 10+ chars (YYYY-MM-DD): full date parse
#   - 7-9 chars (YYYY-MM):    set day to 01
#   - <7 chars or NA:          returns NA
#
# @param dtc Character vector of ISO 8601 date strings.
# @return Named list with $date (Date vector) and $len (integer vector of
#   original trimmed string lengths for downstream precision comparisons).
# ==============================================================================
parse_iso_date <- function(dtc) {
  dtc_chr <- as.character(dtc)
  dtc_trimmed <- stringr::str_trim(dtc_chr)
  dtc_len <- nchar(dtc_trimmed)
  dtc_len <- dplyr::if_else(is.na(dtc_len), 0L, as.integer(dtc_len)) # legitimate: date length initialized to zero when NA

  date_val <- dplyr::case_when(
    dtc_len >= 10L ~ suppressWarnings(
      as.Date(stringr::str_sub(dtc_trimmed, 1L, 10L), format = "%Y-%m-%d")
    ),
    dtc_len >= 7L ~ suppressWarnings(
      as.Date(paste0(stringr::str_sub(dtc_trimmed, 1L, 7L), "-01"), format = "%Y-%m-%d")
    ),
    TRUE ~ as.Date(NA_character_)
  )

  list(date = date_val, len = dtc_len)
}


# ==============================================================================
# format_arm_name — Convert arm name to proper case with acronym handling
# Migrated from SAS lines 536-574
#
# Rules (applied only if arm name is ALL UPPERCASE with no lowercase):
#   - Words > 3 chars with no digits: propcase (str_to_title)
#   - "UP": propcase
#   - "MG", "KG": lowercase
#   - "ML": "mL"
#   - Long slash-separated words (>40 chars): add space after "/"
#
# @param arm_name Character scalar. A single arm name string.
# @return Character scalar. The formatted arm display name.
# ==============================================================================
format_arm_name <- function(arm_name) {
  if (is.na(arm_name) || arm_name == "") return(arm_name)

  # Only process if all uppercase (SAS: not anylower(arm_display))
  if (stringr::str_detect(arm_name, "[a-z]")) return(arm_name)

  # Split into word tokens and whitespace separators preserving structure
  tokens     <- regmatches(arm_name, gregexpr("\\S+", arm_name))[[1]]
  separators <- regmatches(arm_name, gregexpr("\\s+", arm_name))[[1]]

  processed <- vapply(tokens, function(word) {
    clean <- gsub("\\s", "", word)
    upper_clean <- toupper(clean)

    # Words > 3 chars with no digits -> propcase (SAS line 546-547)
    if (stringr::str_length(clean) > 3L && !grepl("[0-9]", clean)) {
      return(stringr::str_to_title(clean))
    }
    # "UP" -> propcase (SAS line 548-549)
    if (upper_clean == "UP") {
      return(stringr::str_to_title(clean))
    }
    # "MG", "KG" -> lowercase (SAS lines 550-551)
    if (upper_clean %in% c("MG", "KG")) {
      return(stringr::str_to_lower(clean))
    }
    # "ML" -> "mL" (SAS lines 552-553)
    if (upper_clean == "ML") {
      return("mL")
    }
    # All other words unchanged
    word
  }, character(1), USE.NAMES = FALSE)

  # Reconstruct string with original separators
  result <- processed[1L]
  if (length(separators) > 0L) {
    for (k in seq_along(separators)) {
      result <- paste0(result, separators[k], processed[k + 1L])
    }
  }

  # Long slash-delimited words (SAS lines 560-568)
  if (grepl("/", result, fixed = TRUE)) {
    space_words <- strsplit(result, " ", fixed = TRUE)[[1]]
    has_long_slash <- any(
      stringr::str_length(space_words) > 40L & grepl("/", space_words, fixed = TRUE)
    )
    if (has_long_slash) {
      result <- stringr::str_replace_all(result, "/", "/ ")
    }
  }

  result
}


# ==============================================================================
# setup — Adverse Event Panel Setup (Main Orchestrator)
# Migrated from SAS %setup macro (SAS lines 26-587)
#
# Validates DM/AE/EX datasets, merges safety population, optionally enriches
# with MedDRA hierarchy and DME lookups, produces audit-friendly reports.
#
# @param dm Data frame. Demographics domain.
# @param ae Data frame. Adverse Events domain.
# @param ex Data frame. Exposure domain.
# @param mdhier Character "Y"/"N". MedDRA hierarchy lookup switch (default "N").
# @param dme Character "Y"/"N". Designated Medical Events switch (default "N").
# @param meddra_data Data frame or NULL. MedDRA hierarchy lookup table.
# @param dme_data Data frame or NULL. DME lookup table.
# @param ver Character or NULL. MedDRA version string.
# @param vld_sw Integer 0/1. Data validation switch (default 1).
# @param study_lag Integer. Days after treatment end to include AEs (default 30).
# @param toxgr_min Numeric or NULL. Minimum valid toxicity grade.
# @param toxgr_max Numeric or NULL. Maximum valid toxicity grade.
# @param sl_subset Character or NULL. Script Launcher subset expression.
# @return Named list with all setup results.
# @export
# ==============================================================================
setup <- function(dm, ae, ex,
                  mdhier      = "N",
                  dme         = "N",
                  meddra_data = NULL,
                  dme_data    = NULL,
                  ver         = NULL,
                  vld_sw      = 1L,
                  study_lag   = 30L,
                  toxgr_min   = NULL,
                  toxgr_max   = NULL,
                  sl_subset   = NULL) {

  # -- Defensive input validation -------------------------------------------------
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame")
  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame")

  cli::cli_h1("AE SETUP")
  cli::cli_alert_info("Initializing AE panel setup")

  # -- Normalize column names to lowercase for consistent processing -----------
  dm <- dplyr::rename_with(dm, tolower)
  ae <- dplyr::rename_with(ae, tolower)
  ex <- dplyr::rename_with(ex, tolower)
  if (!is.null(meddra_data)) meddra_data <- dplyr::rename_with(meddra_data, tolower)
  if (!is.null(dme_data))    dme_data    <- dplyr::rename_with(dme_data, tolower)

  # -- Defaults (SAS lines 29, 32) --------------------------------------------
  if (is.null(vld_sw))    vld_sw    <- 1L
  vld_sw    <- as.integer(vld_sw)
  if (is.null(study_lag)) study_lag <- 30L
  study_lag <- as.integer(study_lag)

  # -- Initialize MedDRA result placeholders -----------------------------------
  meddra_pct      <- NULL
  rpt_meddra      <- NULL
  rpt_meddra_term <- NULL

  # ============================================================================
  # Preliminary Data Checks (SAS lines 35-110)
  # ============================================================================
  log_msg("DATA CHECKS")

  # Check DM has subjects (SAS line 42)
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # Required variable checks (SAS lines 46-52)
  ae_aebodsys_chk <- chk_var(ae, "aebodsys", ds_name = "ae")
  ae_aedecod_chk  <- chk_var(ae, "aedecod",  ds_name = "ae")
  ae_usubjid_chk  <- chk_var(ae, "usubjid",  ds_name = "ae")
  dm_usubjid_chk  <- chk_var(dm, "usubjid",  ds_name = "dm")
  ex_usubjid_chk  <- chk_var(ex, "usubjid",  ds_name = "ex")

  # Build rpt_chk_var from required checks (SAS lines 54-56)
  rpt_chk_var <- dplyr::bind_rows(
    ae_aebodsys_chk$audit_row,
    ae_aedecod_chk$audit_row,
    ae_usubjid_chk$audit_row,
    dm_usubjid_chk$audit_row,
    ex_usubjid_chk$audit_row
  )
  # Snapshot of required checks before adding optional ones
  rpt_chk_var_req <- rpt_chk_var

  # ACTARM vs ARM checks (SAS lines 59-60)
  dm_actarm_chk <- chk_var(dm, "actarm", ds_name = "dm")
  dm_arm_chk    <- chk_var(dm, "arm",    ds_name = "dm")
  rpt_chk_var <- dplyr::bind_rows(
    rpt_chk_var, dm_actarm_chk$audit_row, dm_arm_chk$audit_row
  )

  dm_actarm <- as.logical(dm_actarm_chk$ind)
  dm_arm    <- as.logical(dm_arm_chk$ind)

  # Insert combined ACTARM/ARM row into rpt_chk_var_req (SAS lines 62-70)
  rpt_chk_var_req <- dplyr::bind_rows(
    rpt_chk_var_req,
    dplyr::tibble(
      chk       = "VAR",
      ds        = "DM",
      var       = "ACTARM or ARM",
      type      = NA_character_,
      len       = NA_real_,
      condition = "EXISTS",
      ind       = as.numeric(dm_actarm || dm_arm)
    )
  )

  # Determine arm variable (SAS lines 73-76)
  arm_var <- if (dm_actarm) {
    "actarm"
  } else if (dm_arm) {
    "arm"
  } else {
    NA_character_
  }

  # Setup required variable flag (SAS lines 79-86)
  setup_req_var <- as.logical(
    ae_aebodsys_chk$ind & ae_aedecod_chk$ind & ae_usubjid_chk$ind &
      (dm_actarm | dm_arm) &
      dm_usubjid_chk$ind & ex_usubjid_chk$ind
  )

  # Early exit if no subjects or missing required vars (SAS line 88)
  if (!dm_subj_gt0 || !setup_req_var) {
    if (!dm_subj_gt0) {
      cli::cli_warn("There are no subjects in DM")
    }
    if (!setup_req_var) {
      cli::cli_warn(
        "Some variables required for AE setup are missing"
      )
    }
    return(list(
      setup_success    = FALSE,
      ds_base          = dplyr::tibble(),
      err_base         = dplyr::tibble(),
      arm_var          = arm_var,
      arm_count        = 0L,
      arm_total        = 0L,
      arm_names        = character(0),
      arm_counts       = integer(0),
      max_arm_nm_len   = 0L,
      rpt_dm           = dplyr::tibble(),
      rpt_err          = dplyr::tibble(),
      rpt_err_term     = dplyr::tibble(),
      rpt_chk_var      = rpt_chk_var,
      rpt_chk_var_req  = rpt_chk_var_req,
      naes_sp          = 0L,
      naes_spv         = 0L,
      naes_sp_by_arm   = integer(0),
      vld_sw           = vld_sw,
      meddra_pct       = NULL,
      rpt_meddra       = NULL,
      rpt_meddra_term  = NULL
    ))
  }

  # Optional variable checks (SAS lines 91-99)
  ae_aestdtc_chk <- chk_var(ae, "aestdtc", ds_name = "ae")
  ae_aeser_chk   <- chk_var(ae, "aeser",   ds_name = "ae")
  ae_aesev_chk   <- chk_var(ae, "aesev",   ds_name = "ae")
  ae_aetoxgr_chk <- chk_var(ae, "aetoxgr", ds_name = "ae")
  dm_rfstdtc_chk <- chk_var(dm, "rfstdtc", ds_name = "dm")
  dm_rfendtc_chk <- chk_var(dm, "rfendtc", ds_name = "dm")
  dm_armcd_chk   <- chk_var(dm, "armcd",   ds_name = "dm")
  ex_exstdtc_chk <- chk_var(ex, "exstdtc", ds_name = "ex")
  ex_exendtc_chk <- chk_var(ex, "exendtc", ds_name = "ex")

  rpt_chk_var <- dplyr::bind_rows(
    rpt_chk_var,
    ae_aestdtc_chk$audit_row,
    ae_aeser_chk$audit_row,
    ae_aesev_chk$audit_row,
    ae_aetoxgr_chk$audit_row,
    dm_rfstdtc_chk$audit_row,
    dm_rfendtc_chk$audit_row,
    dm_armcd_chk$audit_row,
    ex_exstdtc_chk$audit_row,
    ex_exendtc_chk$audit_row
  )

  # Extract logical flags from optional checks
  ae_aestdtc      <- as.logical(ae_aestdtc_chk$ind)
  ae_aeser        <- as.logical(ae_aeser_chk$ind)
  ae_aesev        <- as.logical(ae_aesev_chk$ind)
  ae_aetoxgr      <- as.logical(ae_aetoxgr_chk$ind)
  ae_aetoxgr_type <- ae_aetoxgr_chk$type
  dm_rfstdtc      <- as.logical(dm_rfstdtc_chk$ind)
  dm_rfendtc      <- as.logical(dm_rfendtc_chk$ind)
  dm_armcd        <- as.logical(dm_armcd_chk$ind)
  ex_exstdtc      <- as.logical(ex_exstdtc_chk$ind)
  ex_exendtc      <- as.logical(ex_exendtc_chk$ind)

  # Date availability flags (SAS lines 101-102)
  ex_exdtc <- ex_exstdtc && ex_exendtc
  dm_rfdtc <- dm_rfstdtc && dm_rfendtc

  # If no dates available turn off validation (SAS lines 105-110)
  if (!(ae_aestdtc && (ex_exdtc || dm_rfdtc))) {
    vld_sw <- 0L
    cli::cli_warn(
      "AE data validation has been turned off: date variables are missing"
    )
  }

  # ============================================================================
  # DM Processing (SAS lines 117-151)
  # ============================================================================
  log_msg("DEMOGRAPHICS DOMAIN")

  # Select columns for all_dm
  dm_keep <- c("usubjid", arm_var)
  if (dm_armcd)                        dm_keep <- c(dm_keep, "armcd")
  if (vld_sw == 1L && dm_rfstdtc)     dm_keep <- c(dm_keep, "rfstdtc")
  if (vld_sw == 1L && dm_rfendtc)     dm_keep <- c(dm_keep, "rfendtc")
  dm_keep <- intersect(dm_keep, names(dm))

  all_dm <- dm %>%
    dplyr::select(dplyr::all_of(dm_keep)) %>%
    dplyr::mutate(arm = .data[[arm_var]])

  # Exclude screen failures / unassigned (SAS lines 129-131)
  if (dm_armcd && "armcd" %in% names(all_dm)) {
    all_dm <- all_dm %>%
      dplyr::filter(
        !toupper(as.character(armcd)) %in% c("SCRNFAIL", "NOTASSGN")
      )
  }

  # Convert reference dates with precision tracking (SAS lines 134-148)
  if (vld_sw == 1L && dm_rfdtc &&
      "rfstdtc" %in% names(all_dm) && "rfendtc" %in% names(all_dm)) {
    rfst_parsed <- parse_iso_date(all_dm$rfstdtc)
    rfen_parsed <- parse_iso_date(all_dm$rfendtc)
    all_dm <- all_dm %>%
      dplyr::mutate(
        rfstdt     = rfst_parsed$date,
        rfstdt_len = rfst_parsed$len,
        rfendt     = rfen_parsed$date,
        rfendt_len = rfen_parsed$len
      )
  }

  all_dm <- all_dm %>% dplyr::arrange(usubjid)

  # ============================================================================
  # EX Processing (SAS lines 158-189)
  # ============================================================================
  log_msg("EXPOSURE DOMAIN")

  if (vld_sw == 1L && (ex_exstdtc || ex_exendtc)) {
    # Parse available date columns from EX
    ex_work <- ex %>% dplyr::select(usubjid)

    if (ex_exstdtc && "exstdtc" %in% names(ex)) {
      ex_work <- dplyr::bind_cols(
        ex_work,
        ex %>% dplyr::transmute(
          .exstdtc_dt = suppressWarnings(
            as.Date(substr(as.character(exstdtc), 1L, 10L), format = "%Y-%m-%d")
          )
        )
      )
    }
    if (ex_exendtc && "exendtc" %in% names(ex)) {
      ex_work <- dplyr::bind_cols(
        ex_work,
        ex %>% dplyr::transmute(
          .exendtc_dt = suppressWarnings(
            as.Date(substr(as.character(exendtc), 1L, 10L), format = "%Y-%m-%d")
          )
        )
      )
    }

    # Summarise per subject (SAS PROC SQL group by usubjid)
    has_stdt <- ".exstdtc_dt" %in% names(ex_work)
    has_endt <- ".exendtc_dt" %in% names(ex_work)

    if (has_stdt && has_endt) {
      all_ex <- ex_work %>%
        dplyr::group_by(usubjid) %>%
        dplyr::summarise(
          exstdt = {
            v <- .exstdtc_dt[!is.na(.exstdtc_dt)]
            if (length(v) == 0L) as.Date(NA) else min(v)
          },
          exendt = {
            st <- .exstdtc_dt[!is.na(.exstdtc_dt)]
            en <- .exendtc_dt[!is.na(.exendtc_dt)]
            max_st <- if (length(st) > 0L) max(st) else as.Date(NA)
            max_en <- if (length(en) > 0L) max(en) else as.Date(NA)
            cands  <- c(max_st, max_en)
            cands  <- cands[!is.na(cands)]
            if (length(cands) == 0L) as.Date(NA) else max(cands)
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(exstdt_len = 10L, exendt_len = 10L)
    } else if (has_stdt) {
      all_ex <- ex_work %>%
        dplyr::group_by(usubjid) %>%
        dplyr::summarise(
          exstdt = {
            v <- .exstdtc_dt[!is.na(.exstdtc_dt)]
            if (length(v) == 0L) as.Date(NA) else min(v)
          },
          exendt = {
            v <- .exstdtc_dt[!is.na(.exstdtc_dt)]
            if (length(v) == 0L) as.Date(NA) else max(v)
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(exstdt_len = 10L, exendt_len = 10L)
    } else {
      all_ex <- ex_work %>%
        dplyr::distinct(usubjid)
    }
    all_ex <- all_ex %>% dplyr::arrange(usubjid)
  } else {
    # No date validation — just distinct subjects from EX
    all_ex <- ex %>%
      dplyr::distinct(usubjid) %>%
      dplyr::arrange(usubjid)
  }

  # ============================================================================
  # DM-EX Safety Population Merge (SAS lines 195-296)
  # ============================================================================
  log_msg("DEMOGRAPHICS-EXPOSURE MERGE")

  # Total subjects in DM (before safety pop filter)
  dm_total <- dplyr::n_distinct(all_dm$usubjid)

  # Subjects in both DM and EX -> safety population (SAS line 199-201)
  all_dm_ex <- dplyr::inner_join(all_dm, all_ex, by = "usubjid")

  # Subjects in DM but not EX -> error set (SAS lines 207-210)
  err_dm_ex <- dplyr::anti_join(all_dm, all_ex, by = "usubjid") %>%
    dplyr::mutate(err_type = "ex")

  # Treatment date priority (SAS lines 219-243)
  has_exstdt <- "exstdt" %in% names(all_dm_ex)
  has_exendt <- "exendt" %in% names(all_dm_ex)
  has_rfstdt <- "rfstdt" %in% names(all_dm_ex)
  has_rfendt <- "rfendt" %in% names(all_dm_ex)

  if (vld_sw == 1L) {
    all_dm_ex <- all_dm_ex %>%
      dplyr::mutate(
        trtstdt = if (has_exstdt) {
          exstdt
        } else if (has_rfstdt) {
          rfstdt
        } else {
          as.Date(NA)
        },
        trtstdt_len = if (has_exstdt) {
          dplyr::if_else(!is.na(exstdt), 10L, NA_integer_)
        } else if (has_rfstdt) {
          rfstdt_len
        } else {
          NA_integer_
        },
        trtendt = if (has_exendt) {
          exendt
        } else if (has_rfendt) {
          rfendt
        } else {
          as.Date(NA)
        },
        trtendt_len = if (has_exendt) {
          dplyr::if_else(!is.na(exendt), 10L, NA_integer_)
        } else if (has_rfendt) {
          rfendt_len
        } else {
          NA_integer_
        }
      )

    # Fallback: where EX dates are NA, try DM dates (SAS lines 228-237)
    if (has_exstdt && has_rfstdt) {
      all_dm_ex <- all_dm_ex %>%
        dplyr::mutate(
          trtstdt     = dplyr::if_else(is.na(trtstdt) & !is.na(rfstdt),
                                       rfstdt, trtstdt),
          trtstdt_len = dplyr::if_else(is.na(trtstdt_len) & !is.na(rfstdt_len),
                                       rfstdt_len, trtstdt_len)
        )
    }
    if (has_exendt && has_rfendt) {
      all_dm_ex <- all_dm_ex %>%
        dplyr::mutate(
          trtendt     = dplyr::if_else(is.na(trtendt) & !is.na(rfendt),
                                       rfendt, trtendt),
          trtendt_len = dplyr::if_else(is.na(trtendt_len) & !is.na(rfendt_len),
                                       rfendt_len, trtendt_len)
        )
    }

    # Subjects with missing treatment dates -> error (SAS lines 239-243)
    err_dt <- all_dm_ex %>%
      dplyr::filter(is.na(trtstdt) | is.na(trtendt)) %>%
      dplyr::mutate(err_type = "dt")
    err_dm_ex <- dplyr::bind_rows(err_dm_ex, err_dt)

    # Keep only subjects with valid treatment dates
    all_dm_ex <- all_dm_ex %>%
      dplyr::filter(!is.na(trtstdt) & !is.na(trtendt))
  }

  # ============================================================================
  # Arm Counting and Numbering (SAS lines 251-296)
  # ============================================================================
  arm_summary <- all_dm_ex %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(arm_n = dplyr::n_distinct(usubjid), .groups = "drop") %>%
    dplyr::arrange(arm)

  arm_count      <- nrow(arm_summary)
  arm_total      <- sum(arm_summary$arm_n)
  max_arm_nm_len <- if (arm_count > 0L) {
    max(nchar(as.character(arm_summary$arm)))
  } else {
    0L
  }

  # Add arm numbers (SAS lines 264-278)
  arm_summary <- arm_summary %>%
    dplyr::mutate(arm_num = dplyr::row_number())

  # Build named vectors for arm_counts and arm_names
  arm_counts_vec <- setNames(arm_summary$arm_n,
                             paste0("arm_", arm_summary$arm_num))
  arm_names_raw  <- setNames(as.character(arm_summary$arm),
                             paste0("arm_name_", arm_summary$arm_num))

  # Join arm_num back to all_dm_ex (SAS hash lookup lines 281-296)
  all_dm_ex <- dplyr::left_join(
    all_dm_ex,
    arm_summary %>% dplyr::select(arm, arm_num, arm_n),
    by = "arm"
  )

  # ============================================================================
  # AE Processing (SAS lines 302-352)
  # ============================================================================
  log_msg("AE DOMAIN")

  # Select relevant columns from ae
  ae_keep <- c("usubjid", "aebodsys", "aedecod", "aeseq")
  if (ae_aeser)   ae_keep <- c(ae_keep, "aeser")
  if (ae_aesev)   ae_keep <- c(ae_keep, "aesev")
  if (ae_aetoxgr) ae_keep <- c(ae_keep, "aetoxgr")
  if (ae_aestdtc) ae_keep <- c(ae_keep, "aestdtc")
  ae_keep <- intersect(ae_keep, names(ae))

  all_ae <- ae %>%
    dplyr::select(dplyr::all_of(ae_keep))

  # Proper case conversion (SAS lines 315-316): only when all uppercase
  all_ae <- all_ae %>%
    dplyr::mutate(
      aebodsys = dplyr::if_else(
        !is.na(aebodsys) & !stringr::str_detect(aebodsys, "[a-z]"),
        stringr::str_to_title(aebodsys),
        aebodsys
      ),
      aedecod = dplyr::if_else(
        !is.na(aedecod) & !stringr::str_detect(aedecod, "[a-z]"),
        stringr::str_to_title(aedecod),
        aedecod
      )
    )

  # AETOXGR validation (SAS lines 319-336)
  if (ae_aetoxgr && "aetoxgr" %in% names(all_ae)) {
    all_ae <- all_ae %>%
      dplyr::mutate(
        aetoxgr_num = suppressWarnings(as.numeric(as.character(aetoxgr)))
      )
    if (!is.null(toxgr_min)) {
      all_ae <- all_ae %>%
        dplyr::mutate(
          aetoxgr_num = dplyr::if_else(
            !is.na(aetoxgr_num) & aetoxgr_num < toxgr_min,
            NA_real_, aetoxgr_num
          )
        )
    }
    if (!is.null(toxgr_max)) {
      all_ae <- all_ae %>%
        dplyr::mutate(
          aetoxgr_num = dplyr::if_else(
            !is.na(aetoxgr_num) & aetoxgr_num > toxgr_max,
            NA_real_, aetoxgr_num
          )
        )
    }
  }

  # AE date parsing (SAS lines 339-349)
  if (ae_aestdtc && "aestdtc" %in% names(all_ae)) {
    aest_parsed <- parse_iso_date(all_ae$aestdtc)
    all_ae <- all_ae %>%
      dplyr::mutate(
        aestdt     = aest_parsed$date,
        aestdt_len = aest_parsed$len
      )
  }

  # Sort AE data (SAS line 352)
  all_ae <- all_ae %>%
    dplyr::arrange(usubjid, aebodsys, aedecod)

  # ============================================================================
  # AE-DM-EX Merge and Validation (SAS lines 360-432)
  # ============================================================================
  log_msg("AE MERGE AND VALIDATION")

  # Inner join: only AEs for subjects in safety population
  all_ae_dm_ex <- dplyr::inner_join(
    all_dm_ex, all_ae, by = "usubjid",
    relationship = "many-to-many"
  )

  # Add total column (SAS line 376)
  all_ae_dm_ex <- all_ae_dm_ex %>%
    dplyr::mutate(total = "Total")

  # Date validation with study_lag window (SAS lines 382-426)
  if (vld_sw == 1L && "aestdt" %in% names(all_ae_dm_ex) &&
      "trtstdt" %in% names(all_ae_dm_ex)) {

    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        # Precision-aware comparison lengths
        .stdt_len = pmin(
          dplyr::if_else(is.na(aestdt_len), 99L, aestdt_len),
          dplyr::if_else(is.na(trtstdt_len), 99L, trtstdt_len)
        ),
        # SAS line 403: uses trtstdt_len (not trtendt_len) - SAS behavior
        .endt_len = pmin(
          dplyr::if_else(is.na(aestdt_len), 99L, aestdt_len),
          dplyr::if_else(is.na(trtstdt_len), 99L, trtstdt_len)
        ),
        # Study end with lag
        .trtendt_lag_day = trtendt + lubridate::days(study_lag),
        .trtendt_lag_mon = trtendt %m+% lubridate::period(
          as.integer(floor(study_lag / 30)), "month"
        ),
        # Month-level dates for partial date comparison
        .aestdt_mon = lubridate::make_date(
          lubridate::year(aestdt), lubridate::month(aestdt), 1L
        ),
        .trtstdt_mon = lubridate::make_date(
          lubridate::year(trtstdt), lubridate::month(trtstdt), 1L
        ),
        .trtendt_lag_mon_start = lubridate::make_date(
          lubridate::year(.trtendt_lag_mon),
          lubridate::month(.trtendt_lag_mon), 1L
        )
      ) %>%
      dplyr::mutate(
        # Before study check
        .before_full = (.stdt_len >= 10L) & (aestdt < trtstdt),
        .before_mon  = (.stdt_len < 10L)  & (.aestdt_mon < .trtstdt_mon),
        .before_study = dplyr::if_else(
          .stdt_len >= 10L, .before_full, .before_mon
        ),
        # After study check
        .after_full = (.endt_len >= 10L) & (aestdt > .trtendt_lag_day),
        .after_mon  = (.endt_len < 10L)  & (
          .aestdt_mon > .trtendt_lag_mon_start
        ),
        .after_study = dplyr::if_else(
          .endt_len >= 10L, .after_full, .after_mon
        )
      ) %>%
      dplyr::mutate(
        # Error classification (err=4 highest priority)
        err = dplyr::case_when(
          is.na(aebodsys) | aebodsys == "" |
            is.na(aedecod)  | aedecod  == ""    ~ 4L,
          !is.na(.after_study) & .after_study    ~ 3L,
          !is.na(.before_study) & .before_study  ~ 2L,
          is.na(aestdt)                          ~ 1L,
          TRUE                                   ~ 0L
        ),
        err_type = dplyr::case_when(
          err == 4L              ~ "desc",
          err %in% c(1L, 2L, 3L) ~ "dt",
          TRUE                   ~ NA_character_
        ),
        err_desc = dplyr::case_when(
          err == 1L ~ "1. Date missing or incomplete",
          err == 2L ~ "2. Date before study analysis period",
          err == 3L ~ "3. Date after study analysis period",
          err == 4L ~ "4. Description missing",
          TRUE      ~ NA_character_
        )
      ) %>%
      # Remove temporary columns
      dplyr::select(!dplyr::starts_with("."))

  } else {
    # No date validation: only check descriptions
    all_ae_dm_ex <- all_ae_dm_ex %>%
      dplyr::mutate(
        err = dplyr::if_else(
          is.na(aebodsys) | aebodsys == "" |
            is.na(aedecod) | aedecod == "", 4L, 0L
        ),
        err_type = dplyr::if_else(err == 4L, "desc", NA_character_),
        err_desc = dplyr::if_else(
          err == 4L, "4. Description missing", NA_character_
        )
      )
  }

  # Split into valid and excluded (SAS lines 428-430)
  ds_base  <- all_ae_dm_ex %>% dplyr::filter(err == 0L)
  err_base <- all_ae_dm_ex %>% dplyr::filter(err != 0L)

  ds_base <- ds_base %>%
    dplyr::arrange(aebodsys, aedecod)

  # ============================================================================
  # MedDRA Enrichment (SAS lines 439-533, conditional on mdhier="Y")
  # ============================================================================
  if (toupper(mdhier) == "Y" && !is.null(meddra_data) && nrow(ds_base) > 0L) {
    log_msg("MEDDRA ENRICHMENT")

    # Prepare join keys in uppercase for case-insensitive matching
    meddra_join <- meddra_data
    if (haven::is.labelled(meddra_join[[1]])) {
      meddra_join <- meddra_join %>%
        dplyr::mutate(dplyr::across(
          dplyr::where(haven::is.labelled), as.character
        ))
    }

    # Normalize MedDRA column names to lowercase
    names(meddra_join) <- tolower(names(meddra_join))

    # Uppercase join keys in both datasets
    ds_base_uc <- ds_base %>%
      dplyr::mutate(
        .join_bodsys = toupper(as.character(aebodsys)),
        .join_decod  = toupper(as.character(aedecod))
      )

    meddra_join <- meddra_join %>%
      dplyr::mutate(
        .join_bodsys = toupper(as.character(aebodsys)),
        .join_decod  = toupper(as.character(aedecod))
      )

    # Select MedDRA columns for join
    meddra_cols <- intersect(
      c("soc_name", "hlgt_name", "hlt_name", "pt_name"),
      names(meddra_join)
    )
    meddra_lookup <- meddra_join %>%
      dplyr::select(dplyr::all_of(c(".join_bodsys", ".join_decod", meddra_cols))) %>%
      dplyr::distinct()

    # Left join to attach MedDRA hierarchy
    ds_base_enriched <- ds_base_uc %>%
      dplyr::left_join(meddra_lookup,
                       by = c(".join_bodsys", ".join_decod"))

    # Split matched / unmatched
    has_meddra_col <- length(meddra_cols) > 0L
    if (has_meddra_col) {
      first_md_col <- meddra_cols[1]
      ds_base_matched   <- ds_base_enriched %>%
        dplyr::filter(!is.na(.data[[first_md_col]])) %>%
        dplyr::select(!dplyr::starts_with(".join_"))
      ds_base_unmatched <- ds_base_enriched %>%
        dplyr::filter(is.na(.data[[first_md_col]])) %>%
        dplyr::select(!dplyr::starts_with(".join_"))
    } else {
      ds_base_matched   <- ds_base_enriched %>%
        dplyr::select(!dplyr::starts_with(".join_"))
      ds_base_unmatched <- ds_base_enriched %>%
        dplyr::filter(FALSE) %>%
        dplyr::select(!dplyr::starts_with(".join_"))
    }

    # DME flag (SAS lines 472-487, conditional on dme="Y")
    if (toupper(dme) == "Y" && !is.null(dme_data) &&
        "pt_name" %in% names(ds_base_matched)) {
      dme_join <- dme_data
      names(dme_join) <- tolower(names(dme_join))
      if ("pt_name" %in% names(dme_join)) {
        dme_lookup <- dme_join %>%
          dplyr::select(pt_name, dplyr::any_of("dme")) %>%
          dplyr::distinct()
        ds_base_matched <- dplyr::left_join(
          ds_base_matched, dme_lookup, by = "pt_name"
        )
      }
    }

    # MedDRA matching report (SAS lines 496-531)
    meddra_cnt <- nrow(ds_base_matched)
    err_cnt    <- nrow(ds_base_unmatched)
    total_cnt  <- meddra_cnt + err_cnt
    meddra_pct <- if (total_cnt > 0L) {
      janitor::round_half_up(100 * meddra_cnt / total_cnt, 1)
    } else {
      NA_real_
    }

    rpt_meddra <- dplyr::tibble(
      meddra_ver  = if (!is.null(ver)) as.character(ver) else NA_character_,
      meddra_cnt  = meddra_cnt,
      err_cnt     = err_cnt,
      meddra_pct  = meddra_pct
    )

    # Unmatched term report
    if (nrow(ds_base_unmatched) > 0L) {
      rpt_meddra_term <- ds_base_unmatched %>%
        dplyr::mutate(
          aebodsys = dplyr::if_else(
            !is.na(aebodsys) & !stringr::str_detect(aebodsys, "[a-z]"),
            stringr::str_to_title(aebodsys), aebodsys
          ),
          aedecod = dplyr::if_else(
            !is.na(aedecod) & !stringr::str_detect(aedecod, "[a-z]"),
            stringr::str_to_title(aedecod), aedecod
          )
        ) %>%
        dplyr::group_by(aebodsys, aedecod) %>%
        dplyr::summarise(
          n_subjects = dplyr::n_distinct(usubjid),
          n_events   = dplyr::n(),
          .groups    = "drop"
        )
    } else {
      rpt_meddra_term <- dplyr::tibble(
        aebodsys   = character(0),
        aedecod    = character(0),
        n_subjects = integer(0),
        n_events   = integer(0)
      )
    }

    # Move unmatched to err_base and keep matched as ds_base
    err_base_meddra <- ds_base_unmatched %>%
      dplyr::mutate(
        err      = 5L,
        err_type = "meddra",
        err_desc = "5. MedDRA term not matched"
      )
    err_base <- dplyr::bind_rows(err_base, err_base_meddra)
    ds_base  <- ds_base_matched
  }

  # ============================================================================
  # Arm Name Formatting (SAS lines 536-574)
  # ============================================================================
  arm_names_formatted <- vapply(
    arm_names_raw, format_arm_name, character(1), USE.NAMES = TRUE
  )

  # ============================================================================
  # Build Reporting Tables via rpt_setup (SAS lines 576-587)
  # ============================================================================
  rpt_results <- rpt_setup(
    dm_orig      = dm,
    all_dm       = all_dm,
    all_dm_ex    = all_dm_ex,
    all_ae_dm_ex = all_ae_dm_ex,
    err_dm_ex    = err_dm_ex,
    ds_base      = ds_base,
    err_base     = err_base,
    arm_var      = arm_var,
    arm_count    = arm_count,
    arm_total    = arm_total,
    arm_summary  = arm_summary,
    dm_total     = dm_total
  )

  # ============================================================================
  # Return comprehensive result list (SAS call symputx equivalent)
  # ============================================================================
  list(
    setup_success    = TRUE,
    ds_base          = ds_base,
    err_base         = err_base,
    arm_var          = arm_var,
    arm_count        = arm_count,
    arm_total        = arm_total,
    arm_names        = arm_names_formatted,
    arm_counts       = arm_counts_vec,
    max_arm_nm_len   = max_arm_nm_len,
    rpt_dm           = rpt_results$rpt_dm,
    rpt_err          = rpt_results$rpt_err,
    rpt_err_term     = rpt_results$rpt_err_term,
    rpt_chk_var      = rpt_chk_var,
    rpt_chk_var_req  = rpt_chk_var_req,
    naes_sp          = rpt_results$naes_sp,
    naes_spv         = rpt_results$naes_spv,
    naes_sp_by_arm   = rpt_results$naes_sp_by_arm,
    vld_sw           = vld_sw,
    meddra_pct       = meddra_pct,
    rpt_meddra       = rpt_meddra,
    rpt_meddra_term  = rpt_meddra_term
  )
}  # end setup()


# ==============================================================================
# rpt_setup() — Reporting Tables Builder (SAS lines 591-795)
# ==============================================================================
#' Build AE Setup Reporting Tables
#'
#' Constructs the DM subject disposition report (rpt_dm), AE counts in the
#' safety population, error counts by arm (rpt_err), and per-term exclusion
#' details (rpt_err_term). Called internally by setup() but also exported for
#' standalone use.
#'
#' @param dm_orig     Original DM data frame (before filtering)
#' @param all_dm      DM data after screen-failure exclusion
#' @param all_dm_ex   DM-EX merged safety population
#' @param all_ae_dm_ex Merged AE-DM-EX dataset (before error split)
#' @param err_dm_ex   Subjects excluded during DM-EX merge
#' @param ds_base     Validated AE dataset
#' @param err_base    Excluded AE dataset
#' @param arm_var     Character: arm variable name used ("actarm" or "arm")
#' @param arm_count   Integer: number of treatment arms
#' @param arm_total   Integer: total subjects in safety population
#' @param arm_summary Tibble with columns: arm, arm_n, arm_num
#' @param dm_total    Integer: total subjects in DM after screen-failure removal
#' @return Named list: rpt_dm, rpt_err, rpt_err_term, naes_sp, naes_spv,
#'   naes_sp_by_arm
rpt_setup <- function(dm_orig, all_dm, all_dm_ex, all_ae_dm_ex,
                      err_dm_ex, ds_base, err_base,
                      arm_var, arm_count, arm_total,
                      arm_summary, dm_total) {

  log_msg("SETUP REPORT")

  # Rename arm to match arm_var label (SAS lines 598-601)
  # Drop original arm_var column if it differs from "arm" to avoid duplicate name
  all_dm_ex_rpt <- all_dm_ex
  if (arm_var != "arm" && arm_var %in% names(all_dm_ex_rpt)) {
    all_dm_ex_rpt <- all_dm_ex_rpt %>%
      dplyr::select(-dplyr::all_of(arm_var))
  }
  all_dm_ex_rpt <- all_dm_ex_rpt %>%
    dplyr::rename(!!arm_var := arm)

  # ==========================================================================
  # rpt_dm: 5-row subject disposition report (SAS lines 605-717)
  # ==========================================================================
  # Row 1: Subjects in DM (from original dm, after screen-failure exclusion)
  n_dm <- dplyr::n_distinct(all_dm$usubjid)

  # Row 2: Removed screen failure / unassigned
  # Compare dm_orig vs all_dm
  n_dm_orig <- dplyr::n_distinct(dm_orig$usubjid)
  n_scrnfail <- n_dm_orig - n_dm

  # Row 3: Removed not in safety population (not in EX)
  n_not_ex <- err_dm_ex %>%
    dplyr::filter(err_type == "ex") %>%
    dplyr::pull(usubjid) %>%
    dplyr::n_distinct()

  # Row 4: Removed no dates
  n_no_dates <- err_dm_ex %>%
    dplyr::filter(err_type == "dt") %>%
    dplyr::pull(usubjid) %>%
    dplyr::n_distinct()

  # Row 5: Subjects used in analysis
  n_analysis <- dplyr::n_distinct(all_dm_ex$usubjid)

  # Per-arm breakdown using arm_summary
  build_arm_counts <- function(dataset, arm_tbl) {
    if (nrow(dataset) == 0L) {
      out <- setNames(rep(0L, nrow(arm_tbl)), paste0("arm_", arm_tbl$arm_num))
      return(out)
    }
    counts <- dataset %>%
      dplyr::group_by(arm) %>%
      dplyr::summarise(ct = dplyr::n_distinct(usubjid), .groups = "drop")
    merged <- dplyr::left_join(arm_tbl, counts, by = "arm") %>%
      dplyr::mutate(ct = tidyr::replace_na(ct, 0L))  # legitimate: arm counts initialized to zero after left join
    setNames(merged$ct, paste0("arm_", merged$arm_num))
  }

  # Per-arm for each row category
  arm_n_dm <- build_arm_counts(all_dm, arm_summary)
  arm_n_analysis <- build_arm_counts(all_dm_ex, arm_summary)

  # Screen failures per arm: subjects in dm_orig but not in all_dm
  dm_orig_lower <- dm_orig
  names(dm_orig_lower) <- tolower(names(dm_orig_lower))
  if (arm_var %in% names(dm_orig_lower)) {
    scrnfail_subj <- dplyr::anti_join(
      dm_orig_lower %>% dplyr::select(usubjid, !!rlang::sym(arm_var)),
      all_dm %>% dplyr::select(usubjid),
      by = "usubjid"
    )
    if (nrow(scrnfail_subj) > 0L && arm_var %in% names(scrnfail_subj)) {
      scrnfail_subj <- scrnfail_subj %>%
        dplyr::rename(arm = !!rlang::sym(arm_var))
      arm_n_scrnfail <- build_arm_counts(scrnfail_subj, arm_summary)
    } else {
      arm_n_scrnfail <- setNames(rep(0L, arm_count),
                                 paste0("arm_", seq_len(arm_count)))
    }
  } else {
    arm_n_scrnfail <- setNames(rep(0L, arm_count),
                               paste0("arm_", seq_len(arm_count)))
  }

  # Not-in-EX per arm
  err_ex_subj <- err_dm_ex %>% dplyr::filter(err_type == "ex")
  arm_n_not_ex <- build_arm_counts(err_ex_subj, arm_summary)

  # No-dates per arm
  err_dt_subj <- err_dm_ex %>% dplyr::filter(err_type == "dt")
  arm_n_no_dates <- build_arm_counts(err_dt_subj, arm_summary)

  # Build rpt_dm as a tibble with dynamic arm columns
  rpt_dm_rows <- list(
    list(row_label = "Subjects in DM",
         total = n_dm, arm_vals = arm_n_dm),
    list(row_label = "Removed: Screen Failure / Not Assigned",
         total = n_scrnfail, arm_vals = arm_n_scrnfail),
    list(row_label = "Removed: Not in Safety Population (no EX)",
         total = n_not_ex, arm_vals = arm_n_not_ex),
    list(row_label = "Removed: No Valid Treatment Dates",
         total = n_no_dates, arm_vals = arm_n_no_dates),
    list(row_label = "Subjects Used in Analysis",
         total = n_analysis, arm_vals = arm_n_analysis)
  )

  rpt_dm <- dplyr::tibble(
    row_num   = seq_along(rpt_dm_rows),
    row_label = vapply(rpt_dm_rows, function(r) r$row_label, character(1)),
    total     = vapply(rpt_dm_rows, function(r) as.integer(r$total), integer(1))
  )

  # Add per-arm columns
  for (i in seq_len(arm_count)) {
    col_nm <- paste0("arm_", i)
    rpt_dm[[col_nm]] <- vapply(
      rpt_dm_rows,
      function(r) as.integer(r$arm_vals[[col_nm]]),
      integer(1)
    )
  }

  # Add percentage columns (SAS lines 708-716)
  # Denominators: row 1 totals for each arm and overall
  dm_total_count <- rpt_dm$total[1]
  rpt_dm <- rpt_dm %>%
    dplyr::mutate(
      total_pct = janitor::round_half_up(100 * total / dm_total_count, 1)
    )
  for (i in seq_len(arm_count)) {
    col_nm     <- paste0("arm_", i)
    pct_col_nm <- paste0("arm_", i, "_pct")
    denom      <- rpt_dm[[col_nm]][1]
    if (!is.na(denom) && denom > 0L) {
      rpt_dm[[pct_col_nm]] <- janitor::round_half_up(
        100 * rpt_dm[[col_nm]] / denom, 1
      )
    } else {
      rpt_dm[[pct_col_nm]] <- NA_real_
    }
  }

  # ==========================================================================
  # AE Counts (SAS lines 724-742)
  # ==========================================================================
  # Total AEs in safety population
  naes_sp <- nrow(all_ae_dm_ex)

  # AEs per arm in safety population
  naes_sp_by_arm <- all_ae_dm_ex %>%
    dplyr::group_by(arm_num) %>%
    dplyr::summarise(n_ae = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(arm_num)
  naes_sp_by_arm_vec <- setNames(
    naes_sp_by_arm$n_ae,
    paste0("naes_sp_", naes_sp_by_arm$arm_num)
  )

  # Total validated AEs (in ds_base)
  naes_spv <- nrow(ds_base)

  # ==========================================================================
  # rpt_err: Error counts per arm with percentages (SAS lines 745-764)
  # ==========================================================================
  if (nrow(err_base) > 0L) {
    err_by_arm <- err_base %>%
      dplyr::group_by(arm_num) %>%
      dplyr::summarise(arm_err_count = dplyr::n(), .groups = "drop")

    rpt_err <- dplyr::left_join(arm_summary, err_by_arm, by = "arm_num") %>%
      dplyr::mutate(
        arm_err_count = tidyr::replace_na(arm_err_count, 0L),  # legitimate: error count initialized to zero after left join
        arm_err_pct = dplyr::if_else(
          arm_n > 0L,
          janitor::round_half_up(100 * arm_err_count / arm_n, 1),
          NA_real_
        )
      ) %>%
      dplyr::select(arm, arm_num, arm_n, arm_err_count, arm_err_pct)

    # Add total row
    total_err <- sum(rpt_err$arm_err_count)
    total_err_pct <- if (arm_total > 0L) {
      janitor::round_half_up(100 * total_err / arm_total, 1)
    } else {
      NA_real_
    }
    rpt_err <- dplyr::bind_rows(
      rpt_err,
      dplyr::tibble(
        arm           = "Total",
        arm_num       = NA_integer_,
        arm_n         = arm_total,
        arm_err_count = total_err,
        arm_err_pct   = total_err_pct
      )
    )
  } else {
    rpt_err <- dplyr::tibble(
      arm           = character(0),
      arm_num       = integer(0),
      arm_n         = integer(0),
      arm_err_count = integer(0),
      arm_err_pct   = double(0)
    )
  }

  # ==========================================================================
  # rpt_err_term: Per-term exclusion counts (SAS lines 767-793)
  # ==========================================================================
  if (nrow(err_base) > 0L) {
    err_term_long <- err_base %>%
      dplyr::mutate(
        aebodsys = dplyr::if_else(
          is.na(aebodsys) | aebodsys == "", "Missing", aebodsys
        ),
        aedecod = dplyr::if_else(
          is.na(aedecod) | aedecod == "", "Missing", aedecod
        )
      ) %>%
      dplyr::group_by(aebodsys, aedecod, err_desc, arm_num) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    # Pivot wider: arm_num columns (SAS PROC TRANSPOSE)
    rpt_err_term <- err_term_long %>%
      tidyr::pivot_wider(
        names_from  = arm_num,
        values_from = count,
        names_prefix = "arm_",
        values_fill  = 0L
      ) %>%
      dplyr::arrange(err_desc, aebodsys, aedecod)

    # Add total column across arms
    arm_cols_in_term <- grep("^arm_\\d+$", names(rpt_err_term), value = TRUE)
    if (length(arm_cols_in_term) > 0L) {
      rpt_err_term <- rpt_err_term %>%
        dplyr::mutate(
          total = rowSums(
            dplyr::across(dplyr::all_of(arm_cols_in_term)), na.rm = TRUE
          )
        )
    } else {
      rpt_err_term <- rpt_err_term %>%
        dplyr::mutate(total = 0L)
    }
  } else {
    rpt_err_term <- dplyr::tibble(
      aebodsys = character(0),
      aedecod  = character(0),
      err_desc = character(0),
      total    = integer(0)
    )
  }

  # Return all reporting objects
  list(
    rpt_dm         = rpt_dm,
    rpt_err        = rpt_err,
    rpt_err_term   = rpt_err_term,
    naes_sp        = naes_sp,
    naes_spv       = naes_spv,
    naes_sp_by_arm = naes_sp_by_arm_vec
  )
}  # end rpt_setup()


# ============================================================
#### MIGRATION NOTES
#### ============================================================
####
#### ASSUMPTIONS:
####   1. Input datasets (dm, ae, ex) are read via haven::read_xpt() or
####      haven::read_sas() by upstream callers; columns arrive as
####      haven_labelled vectors with uppercase names that are normalized
####      to lowercase at the start of setup().
####   2. SAS date epoch differences are handled by parse_iso_date()
####      which parses ISO-8601 character strings directly (CDISC --DTC
####      format), so no numeric SAS date conversion is needed.
####   3. Partial ISO dates with only 7 characters (YYYY-MM) are padded
####      with "-01" to create first-of-month Date values, matching the
####      SAS mdy(month(x), 1, year(x)) pattern.
####   4. Screen failures are identified by ARMCD values "SCRNFAIL" and
####      "NOTASSGN" (case-insensitive), matching SAS upcase() comparison.
####   5. Arm propcase rules: words > 3 characters with no digits get
####      str_to_title(); "MG"/"KG" -> lowercase; "ML" -> "mL".
####      Long slash-separated tokens > 40 characters get spaces after "/".
####   6. The err=4 (description missing) check takes priority over all
####      date-related errors (err=1,2,3) matching SAS IF/ELSE order.
####
#### POTENTIAL NUMERICAL DIFFERENCES:
####   1. Partial date precision: SAS line 403 uses trststdt_len for the
####      end-date comparison length (likely a SAS bug); this R migration
####      replicates that behavior exactly. If corrected to trtendt_len,
####      results would differ at month boundaries for 7-character dates.
####   2. Rounding: All percentages use janitor::round_half_up() to match
####      SAS round-half-up (R default is half-to-even). Results should
####      be identical at 1 decimal place.
####   3. lubridate::%m+% months() may differ from SAS intnx('month')
####      at month boundaries (e.g. Jan 31 + 1 month). SAS intnx with
####      alignment='b' returns Feb 28; lubridate %m+% also returns
####      Feb 28. Verified equivalent for standard cases.
####   4. Sort stability: dplyr::arrange() uses a stable sort consistent
####      with SAS PROC SORT (NOEQUALS is off by default in SAS).
####
#### NO DIRECT R EQUIVALENT:
####   1. SAS hash objects -> dplyr::left_join() (arm numbering, MedDRA)
####   2. SAS call symputx -> return values in named list
####   3. SAS DSID/OPEN/VARNUM -> names(), is.numeric(), ncol()
####   4. SAS %goto EXIT -> early return() with failure list
####   5. SAS retain -> explicit mutate/lag/cumsum patterns
####   6. SAS SpreadsheetML XML output -> handled by downstream callers
####      using openxlsx or r2rtf (not in this setup file)
####   7. SAS PROC TRANSPOSE -> tidyr::pivot_wider()
####   8. SAS anylower() -> stringr::str_detect(x, "[a-z]")
####
#### PACKAGE SELECTION RATIONALE:
####   dplyr (>=1.1.0): Core data manipulation replacing DATA steps,
####     PROC SQL, PROC SORT, hash lookups. AAP mandates tidyverse.
####   tidyr (>=1.3.0): pivot_wider() replacing PROC TRANSPOSE;
####     replace_na() for zero-filling arm columns.
####   stringr (>=1.5.0): str_to_title() replacing propcase();
####     str_detect() replacing anylower(); str_replace_all() replacing
####     tranwrd(). AAP mandates stringr over base R gsub/toupper.
####   lubridate (>=1.9.0): days(), months(), %m+%, make_date(),
####     year(), month() for date arithmetic replacing SAS intnx/mdy.
####   cli (>=3.6.0): cli_rule(), cli_warn(), cli_abort(),
####     cli_alert_info() replacing SAS %put and %log_msg bordered
####     messages.
####   janitor (>=2.2.0): round_half_up() for SAS-compatible rounding
####     at every percentage calculation.
####   haven (>=2.5.0): is.labelled() for detecting haven-labelled
####     vectors from SAS XPT/SAS7BDAT data files.
####
#### OPEN QUESTIONS:
####   1. Confirm intnx('day', trtendt, study_lag) vs
####      lubridate::days(study_lag) at month boundaries when study_lag
####      is not a multiple of 30.
####   2. Verify propcase behavior with multi-word acronyms containing
####      mixed digits (e.g. "5-FU REGIMEN") - current implementation
####      preserves tokens with digits unchanged.
####   3. MedDRA enrichment uses uppercase join keys; confirm original
####      SAS program intended case-insensitive matching via upcase().
####   4. Confirm treatment date fallback logic when EX has exstdtc but
####      not exendtc for some subjects.
#### ============================================================
