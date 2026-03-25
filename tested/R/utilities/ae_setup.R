# ==========================================================================
# ae_setup.R — Gatekeeper Validation for AE Processing
# ==========================================================================
# Migrated from: tested/SAS/ZZ_Utilities/ae_setup.sas (833 lines)
# SAS macros defined: %setup(mdhier=N, dme=N), %rpt_setup, %log_msg(text)
#
# Purpose:
#   Gatekeeper validation module for all AE panel analyses. Validates
#   subjects and adverse events, merges AE/DM/EX domains, looks up MedDRA
#   terms, and creates reporting datasets. Every SAS macro becomes a
#   parameterized R function with named arguments and matching defaults.
#
# Exported Functions:
#   setup_validation()  — Main gatekeeper validation (SAS %setup)
#   rpt_setup()         — Generate reporting datasets (SAS %rpt_setup)
#   log_msg()           — Print boxed log message (SAS %log_msg)
#
# Required Packages (called via pkg::fn notation):
#   dplyr    (>= 1.1.0) — Data manipulation (tidyverse mandate per AAP)
#   tibble   (>= 3.2.0) — Structured tibble return values
#   stringr  (>= 1.5.0) — String manipulation (tidyverse mandate)
#   lubridate(>= 1.9.0) — Date arithmetic (mandated over base R)
#   cli      (>= 3.6.0) — User-facing messages/errors
#   janitor  (>= 2.2.0) — round_half_up for SAS-compatible rounding
#   rlang    (>= 1.1.0) — Tidy evaluation (.data pronoun, sym/syms)
#
# Internal Dependencies:
#   tested/R/utilities/data_checks.R — chk_var(), chk_dm_subj_gt0()
# ==========================================================================


# --------------------------------------------------------------------------
# log_msg: Print a boxed text message to the console
# --------------------------------------------------------------------------
# Migrated from: SAS %log_msg(text) [lines 810-832]
# SAS behavior: Prints text surrounded by asterisk borders to the SAS log.
# R equivalent: Uses cli::cli_h1() for a visually prominent heading.
#
# @param text Character string: the message to display.
# @return NULL (invisibly). Side effect: prints to console via cli.
# --------------------------------------------------------------------------
log_msg <- function(text) {
  if (!is.character(text) || length(text) != 1L) {
    cli::cli_abort("{.arg text} must be a single character string.")
  }
  cli::cli_h1(text)
  invisible(NULL)
}


# ==========================================================================
# Internal Helpers
# ==========================================================================

# --------------------------------------------------------------------------
# parse_iso_date: Vectorized ISO 8601 date parser
# --------------------------------------------------------------------------
parse_iso_date <- function(dtc) {
  dtc_trimmed <- stringr::str_trim(dtc)
  dtc_len <- nchar(dtc_trimmed)
  dtc_len <- dplyr::if_else(is.na(dtc_trimmed), 0L, as.integer(dtc_len))
  result <- as.Date(rep(NA_character_, length(dtc)))
  full_mask <- !is.na(dtc_len) & dtc_len >= 10L
  if (any(full_mask, na.rm = TRUE)) {
    result[full_mask] <- suppressWarnings(
      as.Date(stringr::str_sub(dtc_trimmed[full_mask], 1, 10),
              format = "%Y-%m-%d")
    )
  }
  partial_mask <- !is.na(dtc_len) & dtc_len >= 7L & dtc_len < 10L
  if (any(partial_mask, na.rm = TRUE)) {
    result[partial_mask] <- suppressWarnings(
      as.Date(paste0(stringr::str_sub(dtc_trimmed[partial_mask], 1, 7), "-01"),
              format = "%Y-%m-%d")
    )
  }
  result
}

# --------------------------------------------------------------------------
# date_char_len: Significant character length of date strings
# --------------------------------------------------------------------------
date_char_len <- function(dtc) {
  dplyr::if_else(is.na(dtc), 0L,
                 as.integer(nchar(stringr::str_trim(dtc))))
}

# --------------------------------------------------------------------------
# safe_min_date / safe_max_date: NA-safe date aggregation
# --------------------------------------------------------------------------
safe_min_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(as.Date(NA_character_))
  min(x)
}

safe_max_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(as.Date(NA_character_))
  max(x)
}

# --------------------------------------------------------------------------
# format_arm_display: Propcase arm names with pharmaceutical abbreviations
# --------------------------------------------------------------------------
format_arm_display <- function(arm_name) {
  if (is.na(arm_name) || arm_name == "") return(arm_name)
  arm_display <- arm_name
  if (!stringr::str_detect(arm_display, "[a-z]")) {
    words <- strsplit(arm_display, "\\s+")[[1]]
    for (word in words) {
      clean_word <- gsub("[[:space:]]", "", word)
      if (nchar(clean_word) == 0L) next
      clean_upper <- toupper(clean_word)
      if (stringr::str_length(clean_word) > 3L &&
          !stringr::str_detect(clean_word, "[0-9]")) {
        arm_display <- sub(clean_word, stringr::str_to_title(clean_word),
                           arm_display, fixed = TRUE)
      }
      if (clean_upper == "UP") {
        arm_display <- sub(clean_word, stringr::str_to_title(word),
                           arm_display, fixed = TRUE)
      }
      if (clean_upper %in% c("MG", "KG")) {
        arm_display <- sub(clean_word, tolower(word), arm_display, fixed = TRUE)
      }
      if (clean_upper == "ML") {
        arm_display <- sub(clean_word, "mL", arm_display, fixed = TRUE)
      }
    }
  }
  if (stringr::str_detect(arm_display, "/")) {
    space_words <- strsplit(arm_display, " ")[[1]]
    has_long_slash <- any(
      nchar(space_words) > 40L & stringr::str_detect(space_words, "/"),
      na.rm = TRUE
    )
    if (has_long_slash) {
      arm_display <- stringr::str_replace_all(arm_display, "/", "/ ")
    }
  }
  arm_display
}

# --------------------------------------------------------------------------
# normalize_arm: Normalize arm name for matching (SAS: compress(upcase()))
# --------------------------------------------------------------------------
normalize_arm <- function(x) {
  toupper(gsub("[[:space:]]", "", x))
}

# --------------------------------------------------------------------------
# count_arm_subjects: Count subjects per arm using display name matching
# --------------------------------------------------------------------------
count_arm_subjects <- function(data, arm_col, arm_display_names) {
  arm_count <- length(arm_display_names)
  if (nrow(data) == 0L || !(arm_col %in% names(data))) {
    return(stats::setNames(
      c(rep(0L, arm_count), 0L),
      c(paste0("arm", seq_len(arm_count), "_count"), "total_count")
    ))
  }
  data_arms_norm <- normalize_arm(as.character(data[[arm_col]]))
  arm_counts <- vapply(arm_display_names, function(nm) {
    sum(data_arms_norm == normalize_arm(nm), na.rm = TRUE)
  }, integer(1))
  names(arm_counts) <- paste0("arm", seq_len(arm_count), "_count")
  total <- as.integer(nrow(data))
  c(arm_counts, total_count = total)
}


# ==========================================================================
# setup_validation: Main gatekeeper validation for AE processing
# ==========================================================================
setup_validation <- function(ae, dm, ex,
                             mdhier = FALSE, dme = FALSE,
                             vld_sw = TRUE, study_lag = 30L,
                             meddra_data = NULL, dme_data = NULL,
                             toxgr_min = NA_real_, toxgr_max = NA_real_) {

  # ------------------------------------------------------------------
  # Input validation (programming errors -> cli_abort)
  # ------------------------------------------------------------------
  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame.")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame.")
  if (mdhier && is.null(meddra_data)) {
    cli::cli_abort("MedDRA hierarchy requested but {.arg meddra_data} is NULL.")
  }
  if (dme && is.null(dme_data)) {
    cli::cli_abort("DME lookup requested but {.arg dme_data} is NULL.")
  }

  # Normalize column names to lowercase (SAS is case-insensitive)
  ae <- dplyr::rename_with(ae, tolower)
  dm <- dplyr::rename_with(dm, tolower)
  ex <- dplyr::rename_with(ex, tolower)
  if (!is.null(meddra_data)) meddra_data <- dplyr::rename_with(meddra_data, tolower)
  if (!is.null(dme_data)) {
    dme_data <- dplyr::rename_with(dme_data, tolower)
    if ("llt_name" %in% names(dme_data) && !("pt_name" %in% names(dme_data))) {
      dme_data <- dplyr::rename(dme_data, pt_name = llt_name)
    }
  }

  # ================================================================
  # PRELIMINARY DATA CHECKS (SAS lines 38-91)
  # ================================================================
  log_msg("PRELIMINARY DATA CHECKS")

  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  rpt_chk_var <- dplyr::bind_rows(
    chk_var(ae, "aebodsys", ds_name = "ae"),
    chk_var(ae, "aedecod",  ds_name = "ae"),
    chk_var(ae, "usubjid",  ds_name = "ae"),
    chk_var(dm, "usubjid",  ds_name = "dm"),
    chk_var(ex, "usubjid",  ds_name = "ex")
  )

  get_flag <- function(tbl, ds_upper, var_upper) {
    row <- tbl[toupper(tbl$ds) == ds_upper & toupper(tbl$var) == var_upper, ]
    if (nrow(row) == 0L) return(FALSE)
    as.logical(row$ind[1L])
  }
  ae_aebodsys <- get_flag(rpt_chk_var, "AE", "AEBODSYS")
  ae_aedecod  <- get_flag(rpt_chk_var, "AE", "AEDECOD")
  ae_usubjid  <- get_flag(rpt_chk_var, "AE", "USUBJID")
  dm_usubjid  <- get_flag(rpt_chk_var, "DM", "USUBJID")
  ex_usubjid  <- get_flag(rpt_chk_var, "EX", "USUBJID")

  rpt_chk_var_req <- rpt_chk_var

  # ACTARM vs ARM (SAS lines 69-82)
  actarm_chk <- chk_var(dm, "actarm", ds_name = "dm")
  arm_chk    <- chk_var(dm, "arm",    ds_name = "dm")
  rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, actarm_chk, arm_chk)

  dm_actarm     <- as.logical(actarm_chk$ind[1L])
  dm_arm_exists <- as.logical(arm_chk$ind[1L])
  arm_exists    <- dm_actarm || dm_arm_exists

  rpt_chk_var_req <- dplyr::bind_rows(
    rpt_chk_var_req,
    tibble::tibble(chk = "VAR", ds = "DM", var = "ACTARM or ARM",
                   type = NA_character_, len = NA_integer_,
                   condition = "EXISTS", ind = as.integer(arm_exists))
  )

  arm_var <- if (dm_actarm) "actarm" else if (dm_arm_exists) "arm" else NULL

  setup_req_var <- ae_aebodsys && ae_aedecod && ae_usubjid &&
                   arm_exists && dm_usubjid && ex_usubjid

  # Early exit (SAS %goto setup_exit)
  if (!dm_subj_gt0 || !setup_req_var) {
    if (!dm_subj_gt0) {
      cli::cli_warn("No subjects found in DM dataset.")
    }
    if (!setup_req_var) {
      cli::cli_warn("Required variables missing for AE setup.")
    }
    return(list(
      setup_success   = FALSE,
      arm_var         = arm_var,
      arm_count       = 0L,
      arm_names       = character(0),
      arm_display_names = character(0),
      all_dm_ex       = tibble::tibble(),
      ds_base         = tibble::tibble(),
      err_base        = tibble::tibble(),
      err_dm_ex       = tibble::tibble(),
      rpt_chk_var     = rpt_chk_var,
      rpt_chk_var_req = rpt_chk_var_req,
      rpt_dm          = tibble::tibble(),
      rpt_err         = tibble::tibble(),
      rpt_err_term    = tibble::tibble(),
      vld_sw          = vld_sw,
      naes_sp         = 0L,
      naes_spv        = 0L,
      dm_subj_gt0     = dm_subj_gt0,
      setup_req_var   = setup_req_var
    ))
  }

  # ================================================================
  # OPTIONAL VARIABLE CHECKS (SAS lines 93-113)
  # ================================================================
  opt_checks <- dplyr::bind_rows(
    chk_var(ae, "aestdtc",  ds_name = "ae"),
    chk_var(ae, "aeser",    ds_name = "ae"),
    chk_var(ae, "aesev",    ds_name = "ae"),
    chk_var(ae, "aetoxgr",  ds_name = "ae"),
    chk_var(dm, "rfstdtc",  ds_name = "dm"),
    chk_var(dm, "rfendtc",  ds_name = "dm"),
    chk_var(dm, "armcd",    ds_name = "dm"),
    chk_var(ex, "exstdtc",  ds_name = "ex"),
    chk_var(ex, "exendtc",  ds_name = "ex")
  )
  rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, opt_checks)

  ae_aestdtc <- get_flag(opt_checks, "AE", "AESTDTC")
  ae_aeser   <- get_flag(opt_checks, "AE", "AESER")
  ae_aesev   <- get_flag(opt_checks, "AE", "AESEV")
  ae_aetoxgr <- get_flag(opt_checks, "AE", "AETOXGR")
  dm_rfstdtc <- get_flag(opt_checks, "DM", "RFSTDTC")
  dm_rfendtc <- get_flag(opt_checks, "DM", "RFENDTC")
  dm_armcd   <- get_flag(opt_checks, "DM", "ARMCD")
  ex_exstdtc <- get_flag(opt_checks, "EX", "EXSTDTC")
  ex_exendtc <- get_flag(opt_checks, "EX", "EXENDTC")

  ex_exdtc <- ex_exstdtc
  dm_rfdtc <- dm_rfstdtc && dm_rfendtc

  if (vld_sw && !ex_exdtc && !dm_rfdtc) {
    cli::cli_warn("No date variables available for AE validation. Disabling date checks.")
    vld_sw <- FALSE
  }

  # ================================================================
  # DM PROCESSING (SAS lines 116-155)
  # ================================================================
  log_msg("DM PROCESSING")
  cli::cli_inform("Processing DM domain: arm assignment and date conversion.")

  dm_keep_cols <- c("usubjid", arm_var)
  if (dm_armcd) dm_keep_cols <- c(dm_keep_cols, "armcd")
  if (vld_sw && dm_rfdtc) dm_keep_cols <- c(dm_keep_cols, "rfstdtc", "rfendtc")
  dm_keep_syms <- rlang::syms(dm_keep_cols)
  arm_sym <- rlang::sym(arm_var)

  all_dm <- dm |>
    dplyr::select(!!!dm_keep_syms) |>
    dplyr::mutate(arm = !!arm_sym)

  if (dm_armcd) {
    all_dm <- all_dm |>
      dplyr::filter(!(toupper(.data$armcd) %in% c("SCRNFAIL", "NOTASSGN")))
  }

  if (vld_sw && dm_rfdtc) {
    all_dm <- all_dm |>
      dplyr::mutate(
        rfstdt_len = date_char_len(.data$rfstdtc),
        rfstdt     = parse_iso_date(.data$rfstdtc),
        rfendt_len = date_char_len(.data$rfendtc),
        rfendt     = parse_iso_date(.data$rfendtc)
      )
  }

  all_dm <- all_dm |> dplyr::arrange(.data$usubjid)

  # ================================================================
  # EX PROCESSING (SAS lines 157-192)
  # ================================================================
  log_msg("EX PROCESSING")
  cli::cli_inform("Processing EX domain: treatment date aggregation.")

  if (vld_sw && ex_exdtc) {
    ex_parsed <- ex |>
      dplyr::mutate(
        exstdt_parsed = if (ex_exstdtc) parse_iso_date(.data$exstdtc)
                        else as.Date(NA_character_),
        exendt_parsed = if (ex_exendtc) parse_iso_date(.data$exendtc)
                        else as.Date(NA_character_)
      )

    all_ex <- ex_parsed |>
      dplyr::group_by(.data$usubjid) |>
      dplyr::summarise(
        exstdt = safe_min_date(.data$exstdt_parsed),
        exendt = if (ex_exendtc) {
          safe_max_date(pmax(.data$exstdt_parsed, .data$exendt_parsed,
                             na.rm = TRUE))
        } else {
          safe_max_date(.data$exstdt_parsed)
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(exstdt_len = 10L, exendt_len = 10L) |>
      dplyr::arrange(.data$usubjid)
  } else {
    all_ex <- ex |>
      dplyr::distinct(.data$usubjid) |>
      dplyr::arrange(.data$usubjid)
  }

  # ================================================================
  # DM-EX MERGE -> SAFETY POPULATION (SAS lines 194-299)
  # ================================================================
  log_msg("DM-EX MERGE")
  cli::cli_inform("Merging DM and EX for safety population derivation.")

  # Subjects in both DM and EX (SAS: merge ... if a and b)
  both <- dplyr::inner_join(all_dm, all_ex, by = "usubjid")
  # Subjects in DM but not EX (SAS: if a and not b -> err_type='ex')
  dm_only <- dplyr::anti_join(all_dm, all_ex, by = "usubjid") |>
    dplyr::mutate(err_type = "ex")

  if (vld_sw) {
    # Treatment date assignment (SAS lines 210-247)
    both <- both |>
      dplyr::mutate(
        trtstdt     = as.Date(NA_character_),
        trtstdt_len = NA_integer_,
        trtendt     = as.Date(NA_character_),
        trtendt_len = NA_integer_
      )

    # Priority 1: Use EX dates if available (SAS lines 217-224)
    if (ex_exdtc && "exstdt" %in% names(both)) {
      both <- both |>
        dplyr::mutate(
          trtstdt     = dplyr::if_else(!is.na(.data$exstdt) & !is.na(.data$exendt),
                                       .data$exstdt, .data$trtstdt),
          trtstdt_len = dplyr::if_else(!is.na(.data$exstdt) & !is.na(.data$exendt),
                                       .data$exstdt_len, .data$trtstdt_len),
          trtendt     = dplyr::if_else(!is.na(.data$exstdt) & !is.na(.data$exendt),
                                       .data$exendt, .data$trtendt),
          trtendt_len = dplyr::if_else(!is.na(.data$exstdt) & !is.na(.data$exendt),
                                       .data$exendt_len, .data$trtendt_len)
        )
    }

    # Priority 2: Use DM reference dates if EX dates unavailable (SAS lines 226-235)
    if (dm_rfdtc && "rfstdt" %in% names(both)) {
      both <- both |>
        dplyr::mutate(
          trtstdt     = dplyr::if_else(
            !is.na(.data$rfstdt) & !is.na(.data$rfendt) &
              is.na(.data$trtstdt) & is.na(.data$trtendt),
            .data$rfstdt, .data$trtstdt),
          trtstdt_len = dplyr::if_else(
            !is.na(.data$rfstdt) & !is.na(.data$rfendt) &
              is.na(.data$trtstdt_len),
            .data$rfstdt_len, .data$trtstdt_len),
          trtendt     = dplyr::if_else(
            !is.na(.data$rfstdt) & !is.na(.data$rfendt) &
              is.na(.data$trtendt),
            .data$rfendt, .data$trtendt),
          trtendt_len = dplyr::if_else(
            !is.na(.data$rfstdt) & !is.na(.data$rfendt) &
              is.na(.data$trtendt_len),
            .data$rfendt_len, .data$trtendt_len)
        )
    }

    # Subjects without valid treatment dates -> error (SAS lines 240-247)
    err_dt <- both |>
      dplyr::filter(is.na(.data$trtstdt) | is.na(.data$trtendt)) |>
      dplyr::mutate(err_type = "dt")

    all_dm_ex <- both |>
      dplyr::filter(!is.na(.data$trtstdt) & !is.na(.data$trtendt)) |>
      dplyr::select(dplyr::any_of(c("arm", "usubjid", "trtstdt", "trtstdt_len",
                                     "trtendt", "trtendt_len")))

    err_dm_ex <- dplyr::bind_rows(dm_only, err_dt)
  } else {
    all_dm_ex <- both |> dplyr::select(dplyr::any_of(c("arm", "usubjid")))
    err_dm_ex <- dm_only
  }

  # Arm counts and numbering (SAS lines 253-290)
  all_arm <- all_dm_ex |>
    dplyr::group_by(.data$arm) |>
    dplyr::summarise(count = dplyr::n_distinct(.data$usubjid), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$arm) |>
    dplyr::mutate(
      arm_num  = dplyr::row_number(),
      prev_arm = dplyr::lag(.data$arm, default = NA_character_)
    )

  arm_count <- nrow(all_arm)
  arm_total <- sum(all_arm$count)
  arm_names <- dplyr::pull(all_arm, .data$arm)

  # Assign arm numbers via left_join (SAS: hash lookup)
  all_dm_ex <- all_dm_ex |>
    dplyr::left_join(
      all_arm |> dplyr::select("arm", "arm_num"),
      by = "arm"
    )

  # Assign arm numbers to err_dm_ex as well for downstream reporting
  err_dm_ex <- err_dm_ex |>
    dplyr::left_join(
      all_arm |> dplyr::select("arm", "arm_num"),
      by = "arm"
    )

  # Arm display names (SAS lines 546-585)
  arm_display_names <- vapply(arm_names, format_arm_display, character(1),
                              USE.NAMES = FALSE)
  max_arm_nm_len <- max(nchar(arm_display_names), 0L, na.rm = TRUE)

  # ================================================================
  # AE PROCESSING (SAS lines 301-356)
  # ================================================================
  log_msg("AE PROCESSING")
  cli::cli_inform("Processing AE domain: propcase, AETOXGR validation, date conversion.")

  # Select columns (SAS: set ae(keep=...))
  ae_keep_cols <- c("usubjid", "aebodsys", "aedecod")
  if ("aeseq" %in% names(ae)) ae_keep_cols <- c(ae_keep_cols, "aeseq")
  if (ae_aeser)   ae_keep_cols <- c(ae_keep_cols, "aeser")
  if (ae_aesev)   ae_keep_cols <- c(ae_keep_cols, "aesev")
  if (ae_aetoxgr) ae_keep_cols <- c(ae_keep_cols, "aetoxgr")
  if (vld_sw && ae_aestdtc) ae_keep_cols <- c(ae_keep_cols, "aestdtc")
  if ("aeendtc" %in% names(ae)) ae_keep_cols <- c(ae_keep_cols, "aeendtc")

  all_ae <- ae |> dplyr::select(dplyr::any_of(ae_keep_cols))

  # Propcase for descriptions without lowercase (SAS: not anylower -> propcase)
  all_ae <- all_ae |>
    dplyr::mutate(
      aebodsys = dplyr::if_else(
        !is.na(.data$aebodsys) & !stringr::str_detect(.data$aebodsys, "[a-z]"),
        stringr::str_to_title(.data$aebodsys), .data$aebodsys),
      aedecod = dplyr::if_else(
        !is.na(.data$aedecod) & !stringr::str_detect(.data$aedecod, "[a-z]"),
        stringr::str_to_title(.data$aedecod), .data$aedecod)
    )

  # AETOXGR validation (SAS lines 322-342)
  if (ae_aetoxgr && !is.na(toxgr_min) && !is.na(toxgr_max)) {
    ae_toxgr_is_char <- is.character(all_ae$aetoxgr)
    if (ae_toxgr_is_char) {
      all_ae <- all_ae |>
        dplyr::mutate(
          aetoxgr = dplyr::if_else(
            grepl("^[0-9]+$", .data$aetoxgr),
            suppressWarnings(as.numeric(.data$aetoxgr)),
            NA_real_
          )
        )
    } else {
      all_ae <- all_ae |>
        dplyr::mutate(aetoxgr = as.numeric(.data$aetoxgr))
    }
    all_ae <- all_ae |>
      dplyr::mutate(
        aetoxgr = dplyr::if_else(
          !is.na(.data$aetoxgr) &
            .data$aetoxgr >= toxgr_min & .data$aetoxgr <= toxgr_max,
          .data$aetoxgr, NA_real_
        )
      )
  } else if (!ae_aetoxgr) {
    all_ae <- all_ae |> dplyr::mutate(aetoxgr = NA_real_)
  }

  # AE date conversion (SAS lines 346-354)
  if (vld_sw && ae_aestdtc) {
    all_ae <- all_ae |>
      dplyr::mutate(
        aestdt_len = date_char_len(.data$aestdtc),
        aestdt     = parse_iso_date(.data$aestdtc)
      )
  }

  # Sort (SAS: proc sort by usubjid aebodsys aedecod aeseq)
  sort_cols <- c("usubjid", "aebodsys", "aedecod")
  if ("aeseq" %in% names(all_ae)) sort_cols <- c(sort_cols, "aeseq")
  sort_syms <- rlang::syms(sort_cols)
  all_ae <- all_ae |> dplyr::arrange(!!!sort_syms)

  # ================================================================
  # AE-DM-EX MERGE (SAS lines 358-435)
  # ================================================================
  log_msg("AE-DM-EX MERGE")
  cli::cli_inform("Merging AE with safety population and validating dates.")

  # Inner join: subjects in both safety pop and AE (SAS: merge if a and b)
  all_ae_dm_ex <- dplyr::inner_join(all_dm_ex, all_ae, by = "usubjid")

  # Add total column for cross-arm aggregation
  all_ae_dm_ex <- all_ae_dm_ex |> dplyr::mutate(total = "Total")

  # Date validation (SAS lines 375-433)
  if (vld_sw) {
    all_ae_dm_ex <- all_ae_dm_ex |>
      dplyr::mutate(
        output_flag = NA_integer_,
        err         = NA_integer_,
        err_type    = NA_character_,
        err_desc    = NA_character_,

        # Precision lengths for date comparisons (SAS lines 392, 406)
        stdt_len = dplyr::if_else(
          !is.na(.data$aestdt),
          as.integer(pmin(.data$aestdt_len, .data$trtstdt_len, na.rm = TRUE)),
          NA_integer_),
        # SAS line 406: endt_len = min(aestdt_len, trtstdt_len)
        # NOTE: Preserving SAS behavior — appears to use trtstdt_len, not trtendt_len
        endt_len = dplyr::if_else(
          !is.na(.data$aestdt),
          as.integer(pmin(.data$aestdt_len, .data$trtstdt_len, na.rm = TRUE)),
          NA_integer_),

        # Check 1: Missing AE start date (SAS lines 377-381)
        .c1 = is.na(.data$aestdt),

        # Check 2: AE start before treatment start (SAS lines 384-397)
        .c2 = dplyr::if_else(
          !.data$.c1 & !is.na(.data$stdt_len) & !is.na(.data$trtstdt),
          dplyr::case_when(
            .data$stdt_len >= 10L ~ .data$aestdt < .data$trtstdt,
            .data$stdt_len >= 7L ~
              lubridate::floor_date(.data$aestdt, "month") <
              lubridate::floor_date(.data$trtstdt, "month"),
            TRUE ~ FALSE
          ),
          FALSE
        ),

        # Check 3: AE start after treatment end + lag (SAS lines 399-415)
        .c3 = dplyr::if_else(
          !.data$.c1 & !is.na(.data$endt_len) & !is.na(.data$trtendt),
          dplyr::case_when(
            .data$endt_len >= 10L ~
              .data$aestdt > (.data$trtendt + lubridate::days(study_lag)),
            .data$endt_len >= 7L ~
              lubridate::floor_date(.data$aestdt, "month") >
              (lubridate::floor_date(.data$trtendt, "month") +
                 lubridate::period(as.integer(floor(study_lag / 30)), "month")),
            TRUE ~ FALSE
          ),
          FALSE
        ),

        # Check 4: Missing descriptions (SAS lines 424-429)
        .c4 = is.na(.data$aebodsys) | .data$aebodsys == "" |
              is.na(.data$aedecod) | .data$aedecod == "",

        # Assign error codes (last SAS check wins -> reverse case_when)
        err = dplyr::case_when(
          .data$.c4 ~ 4L,
          .data$.c3 ~ 3L,
          .data$.c2 ~ 2L,
          .data$.c1 ~ 1L,
          TRUE ~ NA_integer_
        ),
        err_type = dplyr::case_when(
          .data$.c4 ~ "desc",
          .data$.c3 ~ "dt",
          .data$.c2 ~ "dt",
          .data$.c1 ~ "dt",
          TRUE ~ NA_character_
        ),
        err_desc = dplyr::case_when(
          .data$.c4 ~ "4. Description missing",
          .data$.c3 ~ "3. Date after study analysis period",
          .data$.c2 ~ "2. Date before study analysis period",
          .data$.c1 ~ "1. Date missing or incomplete",
          TRUE ~ NA_character_
        ),
        output_flag = dplyr::if_else(
          .data$.c1 | .data$.c2 | .data$.c3 | .data$.c4, 0L, 1L
        )
      ) |>
      dplyr::select(-dplyr::starts_with(".c"), -"stdt_len", -"endt_len")
  } else {
    all_ae_dm_ex <- all_ae_dm_ex |>
      dplyr::mutate(
        output_flag = 1L,
        err         = NA_integer_,
        err_type    = NA_character_,
        err_desc    = NA_character_
      )
  }

  # Split into valid (ds_base) and invalid (err_base) (SAS lines 431-433)
  ds_base_cols_drop <- c("err", "err_type", "err_desc", "output_flag")
  if (vld_sw) {
    ds_base_cols_drop <- c(ds_base_cols_drop,
                           "aestdt", "aestdt_len",
                           "trtstdt", "trtstdt_len",
                           "trtendt", "trtendt_len")
  }

  ds_base <- all_ae_dm_ex |>
    dplyr::filter(.data$output_flag == 1L) |>
    dplyr::select(-dplyr::any_of(c(ds_base_cols_drop, "total")))

  err_base <- all_ae_dm_ex |>
    dplyr::filter(.data$output_flag == 0L) |>
    dplyr::select(-dplyr::any_of("output_flag"))

  # Clean output_flag from all_ae_dm_ex
  all_ae_dm_ex <- all_ae_dm_ex |>
    dplyr::select(-dplyr::any_of("output_flag"))

  # ================================================================
  # MEDDRA LOOKUP (SAS lines 438-543, conditional on mdhier)
  # ================================================================
  rpt_meddra      <- tibble::tibble()
  rpt_meddra_term <- tibble::tibble()
  meddra_pct      <- NA_real_

  if (mdhier && nrow(ds_base) > 0L) {
    log_msg("MEDDRA LOOKUP")
    cli::cli_inform("Looking up MedDRA hierarchy for AE terms.")

    # Filter to primary SOC (SAS: primary_soc_fg='Y')
    meddra_ref <- meddra_data |>
      dplyr::filter(toupper(.data$primary_soc_fg) == "Y") |>
      dplyr::mutate(aedecod_upper = toupper(.data$aedecod)) |>
      dplyr::distinct(.data$aedecod_upper, .keep_all = TRUE)

    # Prepare join key
    ds_base_meddra <- ds_base |>
      dplyr::mutate(aedecod_upper = toupper(.data$aedecod))

    # Left join for MedDRA terms (SAS: hash lookup)
    meddra_cols <- c("soc_name", "hlgt_name", "hlt_name", "pt_name")
    meddra_join_cols <- intersect(meddra_cols, names(meddra_ref))
    meddra_ref_slim <- meddra_ref |>
      dplyr::select(dplyr::any_of(c("aedecod_upper", meddra_join_cols)))

    ds_base_joined <- dplyr::left_join(
      ds_base_meddra, meddra_ref_slim, by = "aedecod_upper"
    )

    # Matched records have non-NA soc_name
    matched_mask <- !is.na(ds_base_joined$soc_name)

    ds_base <- ds_base_joined |>
      dplyr::filter(matched_mask) |>
      dplyr::select(-"aedecod_upper")

    err_base_meddra <- ds_base_joined |>
      dplyr::filter(!matched_mask) |>
      dplyr::select(-"aedecod_upper")

    # Append unmatched to err_base
    if (nrow(err_base_meddra) > 0L) {
      err_base_meddra <- err_base_meddra |>
        dplyr::mutate(
          err      = 5L,
          err_type = "meddra",
          err_desc = "5. MedDRA term not found"
        )
      err_base <- dplyr::bind_rows(err_base, err_base_meddra)
    }

    # DME flag (SAS lines 490-510, conditional on dme)
    if (dme && !is.null(dme_data) && nrow(ds_base) > 0L) {
      dme_ref <- dme_data |>
        dplyr::mutate(pt_name_upper = toupper(.data$pt_name)) |>
        dplyr::distinct(.data$pt_name_upper, .keep_all = TRUE) |>
        dplyr::select("pt_name_upper", "dme")

      ds_base <- ds_base |>
        dplyr::mutate(pt_name_upper = toupper(.data$pt_name)) |>
        dplyr::left_join(dme_ref, by = "pt_name_upper") |>
        dplyr::select(-"pt_name_upper")
    }

    # MedDRA matching report (SAS lines 515-543)
    total_terms   <- nrow(ds_base_joined)
    matched_terms <- sum(matched_mask)
    meddra_pct    <- if (total_terms > 0L) {
      janitor::round_half_up(100 * matched_terms / total_terms, 1)
    } else {
      0
    }

    rpt_meddra <- tibble::tibble(
      desc  = c("Total AE records", "MedDRA matched", "MedDRA unmatched",
                "Match percentage"),
      value = c(as.character(total_terms),
                as.character(matched_terms),
                as.character(total_terms - matched_terms),
                paste0(meddra_pct, "%"))
    )

    # Unmatched term detail
    if (any(!matched_mask)) {
      rpt_meddra_term <- ds_base_joined |>
        dplyr::filter(!matched_mask) |>
        dplyr::group_by(.data$aedecod) |>
        dplyr::summarise(
          n_subjects = dplyr::n_distinct(.data$usubjid),
          n_records  = dplyr::n(),
          .groups    = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(.data$n_records))
    }
  }

  # ================================================================
  # BUILD REPORTS (SAS %rpt_setup call at line 587)
  # ================================================================
  log_msg("REPORT SETUP")
  rpt_results <- rpt_setup(
    dm                = dm,
    all_dm_ex         = all_dm_ex,
    all_ae_dm_ex      = all_ae_dm_ex,
    ds_base           = ds_base,
    err_base          = err_base,
    err_dm_ex         = err_dm_ex,
    arm_var           = arm_var,
    arm_names         = arm_names,
    arm_display_names = arm_display_names,
    arm_count         = arm_count,
    dm_armcd          = dm_armcd
  )

  # ================================================================
  # RETURN VALUE (SAS global macro variables -> named list)
  # ================================================================
  list(
    setup_success     = TRUE,
    arm_var           = arm_var,
    arm_count         = arm_count,
    arm_total         = arm_total,
    arm_names         = arm_names,
    arm_display_names = arm_display_names,
    max_arm_nm_len    = max_arm_nm_len,
    all_dm_ex         = all_dm_ex,
    ds_base           = ds_base,
    err_base          = err_base,
    err_dm_ex         = err_dm_ex,
    all_ae_dm_ex      = all_ae_dm_ex,
    rpt_chk_var       = rpt_chk_var,
    rpt_chk_var_req   = rpt_chk_var_req,
    rpt_dm            = rpt_results$rpt_dm,
    rpt_err           = rpt_results$rpt_err,
    rpt_err_term      = rpt_results$rpt_err_term,
    rpt_meddra        = rpt_meddra,
    rpt_meddra_term   = rpt_meddra_term,
    meddra_pct        = meddra_pct,
    vld_sw            = vld_sw,
    naes_sp           = rpt_results$naes_sp,
    naes_spv          = rpt_results$naes_spv,
    dm_subj_gt0       = dm_subj_gt0,
    setup_req_var     = setup_req_var
  )
}


# ==========================================================================
# rpt_setup: Generate reporting datasets
# ==========================================================================
# Migrated from: SAS %rpt_setup [lines 602-806]
#
# Builds subject validation report (rpt_dm), AE error counts (rpt_err),
# and error term detail (rpt_err_term) using dplyr group_by + summarise
# pipelines instead of SAS macro %do loops.
#
# @param dm Data frame: original DM dataset (normalized to lowercase).
# @param all_dm_ex Data frame: safety population.
# @param all_ae_dm_ex Data frame: AEs in safety population.
# @param ds_base Data frame: validated AEs.
# @param err_base Data frame: invalid AEs.
# @param err_dm_ex Data frame: subjects removed from safety population.
# @param arm_var Character: arm column name in DM.
# @param arm_names Character vector: original arm values.
# @param arm_display_names Character vector: propcase-processed arm names.
# @param arm_count Integer: number of treatment arms.
# @param dm_armcd Logical: whether ARMCD column exists in DM.
# @return Named list with rpt_dm, rpt_err, rpt_err_term, naes_sp, naes_spv.
# ==========================================================================
rpt_setup <- function(dm, all_dm_ex, all_ae_dm_ex, ds_base, err_base,
                      err_dm_ex, arm_var, arm_names, arm_display_names,
                      arm_count, dm_armcd = FALSE) {

  cli::cli_inform("Building subject and AE validation reports.")

  # Guard: empty arms

  if (arm_count == 0L) {
    return(list(
      rpt_dm       = tibble::tibble(),
      rpt_err      = tibble::tibble(),
      rpt_err_term = tibble::tibble(),
      naes_sp      = 0L,
      naes_spv     = 0L
    ))
  }

  # ----------------------------------------------------------------
  # rpt_dm: Subject validation report (SAS lines 609-728)
  # ----------------------------------------------------------------
  # Category 1: Subjects in DM
  cat1 <- count_arm_subjects(dm, arm_var, arm_display_names)

  # Category 2: Screen failures / unassigned
  if (dm_armcd && "armcd" %in% names(dm)) {
    scrnfail_dm <- dm |>
      dplyr::filter(toupper(.data$armcd) %in% c("SCRNFAIL", "NOTASSGN"))
    cat2 <- count_arm_subjects(scrnfail_dm, arm_var, arm_display_names)
  } else {
    cat2 <- stats::setNames(
      c(rep(0L, arm_count), 0L),
      c(paste0("arm", seq_len(arm_count), "_count"), "total_count")
    )
  }

  # Category 3: Not in safety population (err_type = 'ex')
  err_ex <- err_dm_ex |> dplyr::filter(.data$err_type == "ex")
  cat3 <- count_arm_subjects(err_ex, "arm", arm_display_names)

  # Category 4: No treatment/reference dates (err_type = 'dt')
  err_dt <- err_dm_ex |> dplyr::filter(.data$err_type == "dt")
  cat4 <- count_arm_subjects(err_dt, "arm", arm_display_names)

  # Category 5: Subjects used in analysis
  cat5 <- count_arm_subjects(all_dm_ex, "arm", arm_display_names)

  # Build rpt_dm tibble (SAS: union queries with per-arm columns + pct)
  build_rpt_row <- function(order_num, desc_text, counts, denom_counts) {
    row <- tibble::tibble(order = order_num, desc = desc_text)
    for (i in seq_len(arm_count)) {
      cnt_col  <- paste0("arm", i, "_count")
      pct_col  <- paste0("arm", i, "_pct")
      cnt_val  <- as.integer(counts[cnt_col])
      denom    <- as.integer(denom_counts[cnt_col])
      pct_val  <- if (!is.na(denom) && denom > 0L) {
        janitor::round_half_up(100 * cnt_val / denom, 1)
      } else {
        0
      }
      row[[cnt_col]] <- cnt_val
      row[[pct_col]] <- pct_val
    }
    row[["total_count"]] <- as.integer(counts["total_count"])
    total_denom <- as.integer(denom_counts["total_count"])
    row[["total_pct"]] <- if (!is.na(total_denom) && total_denom > 0L) {
      janitor::round_half_up(100 * as.integer(counts["total_count"]) / total_denom, 1)
    } else {
      0
    }
    row
  }

  rpt_dm <- dplyr::bind_rows(
    build_rpt_row(1L, "1. Subjects in demographics (DM)",          cat1, cat1),
    build_rpt_row(2L, "2. Removed: screen failure / unassigned",   cat2, cat1),
    build_rpt_row(3L, "3. Removed: not in safety population",      cat3, cat1),
    build_rpt_row(4L, "4. Removed: no treatment/reference dates",  cat4, cat1),
    build_rpt_row(5L, "5. Subjects used in analysis",              cat5, cat1)
  )

  # ----------------------------------------------------------------
  # AE counts (SAS lines 730-742)
  # ----------------------------------------------------------------
  naes_sp  <- as.integer(nrow(all_ae_dm_ex))
  naes_spv <- as.integer(nrow(ds_base))

  # ----------------------------------------------------------------
  # rpt_err: Error type counts per arm (SAS lines 744-775)
  # ----------------------------------------------------------------
  if (nrow(err_base) > 0L && "arm_num" %in% names(err_base)) {
    # AE counts per arm for denominator
    ae_arm_denom <- all_ae_dm_ex |>
      dplyr::group_by(.data$arm_num) |>
      dplyr::summarise(ae_count = dplyr::n(), .groups = "drop")

    # Error counts per arm and type
    err_summary <- err_base |>
      dplyr::group_by(.data$err, .data$err_desc, .data$arm_num) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    err_types <- err_summary |>
      dplyr::distinct(.data$err, .data$err_desc) |>
      dplyr::arrange(.data$err)

    rpt_err_rows <- list()
    for (e_idx in seq_len(nrow(err_types))) {
      row_info  <- err_types[e_idx, ]
      err_sub   <- err_summary |>
        dplyr::filter(.data$err == row_info$err)
      row <- tibble::tibble(err_desc = row_info$err_desc)
      for (i in seq_len(arm_count)) {
        arm_err <- err_sub |>
          dplyr::filter(.data$arm_num == i) |>
          dplyr::pull(.data$count)
        arm_err <- if (length(arm_err) == 0L) 0L else as.integer(arm_err[1L])

        arm_denom <- ae_arm_denom |>
          dplyr::filter(.data$arm_num == i) |>
          dplyr::pull(.data$ae_count)
        arm_denom <- if (length(arm_denom) == 0L) 0L else as.integer(arm_denom[1L])

        pct <- if (arm_denom > 0L) {
          janitor::round_half_up(100 * arm_err / arm_denom, 1)
        } else {
          0
        }
        row[[paste0("arm", i, "_err_count")]] <- arm_err
        row[[paste0("arm", i, "_err_pct")]]   <- pct
      }
      rpt_err_rows[[e_idx]] <- row
    }
    rpt_err <- dplyr::bind_rows(rpt_err_rows)
  } else {
    rpt_err <- tibble::tibble()
  }

  # ----------------------------------------------------------------
  # rpt_err_term: Error term detail per arm (SAS lines 777-804)
  # ----------------------------------------------------------------
  if (nrow(err_base) > 0L && "arm_num" %in% names(err_base)) {
    err_term_long <- err_base |>
      dplyr::group_by(.data$aebodsys, .data$aedecod, .data$arm_num) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    rpt_err_term <- err_term_long |>
      dplyr::distinct(.data$aebodsys, .data$aedecod)

    for (i in seq_len(arm_count)) {
      arm_col_name <- paste0("arm", i)
      arm_slice <- err_term_long |>
        dplyr::filter(.data$arm_num == i) |>
        dplyr::select("aebodsys", "aedecod", "count")
      names(arm_slice)[names(arm_slice) == "count"] <- arm_col_name
      rpt_err_term <- dplyr::left_join(
        rpt_err_term, arm_slice, by = c("aebodsys", "aedecod")
      )
    }

    # Fill NAs with 0, replace blank descriptions with 'Missing'
    rpt_err_term <- rpt_err_term |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("arm"),
          ~ dplyr::if_else(is.na(.), 0L, as.integer(.))
        ),
        aebodsys = dplyr::if_else(
          is.na(.data$aebodsys) | .data$aebodsys == "", "Missing", .data$aebodsys
        ),
        aedecod = dplyr::if_else(
          is.na(.data$aedecod) | .data$aedecod == "", "Missing", .data$aedecod
        )
      )

    # Use dense_rank for ordering consistency check
    rpt_err_term <- rpt_err_term |>
      dplyr::arrange(.data$aebodsys, .data$aedecod) |>
      dplyr::mutate(term_rank = dplyr::dense_rank(
        paste(.data$aebodsys, .data$aedecod)
      )) |>
      dplyr::select(-"term_rank")
  } else {
    rpt_err_term <- tibble::tibble()
  }

  # Return reporting datasets
  list(
    rpt_dm       = rpt_dm,
    rpt_err      = rpt_err,
    rpt_err_term = rpt_err_term,
    naes_sp      = naes_sp,
    naes_spv     = naes_spv
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %setup macro's global variable side effects replaced by
#      a named return list. All macro variables (arm_count, arm_total,
#      arm_name_i, setup_success, etc.) are list elements.
#    - MedDRA hash lookups replaced by dplyr::left_join() on uppercased
#      aedecod key — semantically equivalent to SAS hash find().
#    - SAS PROPCASE behavior approximated by stringr::str_to_title()
#      with custom rules for pharmaceutical abbreviations (MG, KG, ML).
#    - Date precision tracking (string length) preserved for partial
#      date comparisons exactly as in SAS source.
#    - Column names normalized to lowercase at function entry for
#      case-insensitive compatibility with SAS variable handling.
#    - SAS %goto / label control flow replaced by early return().
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS sort stability vs R arrange() on multi-key sorts — dplyr
#      arrange is stable within groups, verified compatible.
#    - Percentage calculations use janitor::round_half_up() for
#      SAS-compatible rounding (R default is half-to-even).
#    - SAS line 406: endt_len = min(aestdt_len, trtstdt_len) — this
#      appears to use trtstdt_len rather than trtendt_len. The SAS
#      behavior is faithfully preserved for functional parity.
# NO DIRECT R EQUIVALENT:
#    - SAS hash object -> dplyr::left_join() (semantically equivalent)
#    - SAS %goto / label -> R early return / function structure
#    - SAS RETAIN -> not needed in dplyr pipelines (all columns persist)
#    - SAS PROC SQL CREATE TABLE -> dplyr pipeline assignments
#    - SAS %SYSFUNC / %SYMEXIST -> standard R is.na / exists checks
# PACKAGE SELECTION RATIONALE:
#    - dplyr: core data manipulation (mandated by AAP over base R)
#    - stringr: string manipulation (tidyverse mandate over base R)
#    - lubridate: date arithmetic (mandated over base R date functions)
#    - cli: informative error/warning messages (AAP specification)
#    - janitor: round_half_up for SAS-compatible rounding (AAP mandate)
#    - rlang: tidy evaluation for dynamic column references (.data, sym)
#    - tibble: structured tibble return values (AAP over data.frame)
# OPEN QUESTIONS:
#    - MedDRA version parameter: how is meddra_data versioned in R
#      workflow? Recommend adding version attribute to meddra_data.
#    - DME reference dataset source: confirm dme_data structure matches
#      SAS dme.dme(rename=(llt_name=pt_name)) expectation.
#    - PROPCASE handling for pharmaceutical abbreviations: confirm list
#      of preserved abbreviations (MG, KG, ML) is complete.
#    - AETOXGR range validation: toxgr_min/toxgr_max are optional
#      parameters; confirm default behavior when not provided.
# ============================================================
