#' =========================================================================
#' Demographics Analysis Panel - R Migration
#' =========================================================================
#' DESCRIPTION: Find subject counts and % per arm
#'              Find subject counts and % per disposition and arm
#'              for age group, sex, race, ethnicity, country, and site ID
#'              Find summary statistics for age
#'              Output to Excel workbook
#'
#' MIGRATED FROM: tested/SAS/DM/demographics_v1.sas (1219 lines)
#' ORIGINAL AUTHORS: Shannon Dennis (FDA), David Kretch (IBM)
#' ORIGINAL DATE: December 29, 2009
#' MIGRATION DATE: 2026-03-23
#'
#' REQUIRED DATASETS: DM (Demographics), DS (Disposition)
#' REQUIRED VARIABLES: DM - ACTARM or ARM, USUBJID
#'                     DS - DSDECOD, USUBJID, DSSTDTC or DSSTDY
#' OPTIONAL VARIABLES: DM - ARMCD, AGE, COUNTRY, ETHNIC, RACE, SEX, SITEID
#'                     DS - DSCAT, DSSCAT, DSSEQ
#' =========================================================================

# ---------------------------------------------------------------------------
# Library loading
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(purrr)
library(janitor)
library(openxlsx)
library(cli)
library(rlang)
library(tibble)

# ---------------------------------------------------------------------------
# Utility dependencies (sourced by demographics() orchestrator when needed)
# ---------------------------------------------------------------------------
# source(file.path(util_path, "data_checks.R"))
# source(file.path(util_path, "sl_gs_output.R"))
# source(file.path(util_path, "err_output.R"))
# The above are sourced at runtime inside demographics() to avoid path issues
# when the file is loaded as a standalone module.

# ===========================================================================
# HELPER: format_arm_display
# ===========================================================================
#' Format an arm name for display output.
#'
#' Applies title-case to words longer than 3 characters when the arm name is
#' entirely uppercase.
#' Special handling: MG / KG -> lowercase, ML -> mL.
#' Replicates SAS lines 291-306 of demographics_v1.sas.
#'
#' @param arm_name Character scalar. The raw arm name.
#' @return Character scalar with formatted display name.
#' @export
format_arm_display <- function(arm_name) {
  if (is.na(arm_name) || nchar(trimws(arm_name)) == 0L) {
    return(arm_name)
  }
  # Only apply formatting if the name has no lowercase letters
  if (str_detect(arm_name, "[a-z]")) {
    return(arm_name)
  }
  words <- str_split(arm_name, "\\s+")[[1L]]
  formatted_words <- purrr::map_chr(words, function(w) {
    # Extract alphabetic portion only (SAS: compress(word,,'ka'))
    w_alpha <- str_remove_all(w, "[^[:alpha:]]")
    alpha_upper <- toupper(w_alpha)
    # Check unit suffixes first (handles "100MG", "5ML", standalone "MG")
    if (alpha_upper %in% c("MG", "KG")) {
      return(tolower(w))
    }
    if (alpha_upper == "ML") {
      return(str_replace(tolower(w), "ml", "mL"))
    }
    # Propcase words with > 3 alpha characters (SAS lines 295-296)
    if (nchar(w_alpha) > 3L) {
      return(str_to_title(w))
    }
    w
  })
  paste(formatted_words, collapse = " ")
}

# ===========================================================================
# HELPER: age unit conversion factor
# ===========================================================================
#' Convert an age value from a native unit to years.
#'
#' Uses the same divisors that the SAS lkp_age construction applies
#' (SAS lines 430-436). For subject ages, the same divisors are applied
#' to convert from native AGEU to years.
#'
#' @param age_val  Numeric vector. Age values.
#' @param unit     Character scalar. Unit string (e.g., "YEARS", "MONTHS").
#' @return Numeric vector of ages in years.
#' @keywords internal
age_to_years <- function(age_val, unit) {
  u <- tolower(trimws(unit))
  u_alpha <- gsub("[^a-z]", "", u)
  dplyr::case_when(
    u_alpha %in% c("yr", "year", "years")   ~ age_val,
    u_alpha %in% c("mo", "month", "months") ~ age_val / 12,
    u_alpha %in% c("wk", "week", "weeks")   ~ age_val / 52.178571428571428571428571428571,
    u_alpha %in% c("dy", "day", "days")     ~ age_val / 365.25,
    u_alpha %in% c("hr", "hour", "hours")   ~ age_val / 8766,
    TRUE ~ age_val
  )
}

# ===========================================================================
# HELPER: parse_age_grps
# ===========================================================================
#' Parse age group boundary strings into a tibble.
#'
#' @param age_grps Character vector, e.g. c("1 yr", "35 yr", "65 yr").
#' @param ageunit  Character scalar, default unit from config.
#' @return A tibble with columns age_sl (numeric) and ageu_sl (character).
#' @keywords internal
parse_age_grps <- function(age_grps, ageunit = "years") {
  purrr::map_dfr(age_grps, function(g) {
    parts <- str_match(str_trim(g), "^([0-9]+\\.?[0-9]*)\\s*(\\S+)?$")
    age_val <- as.numeric(parts[1L, 2L])
    # SAS always uses the global ageunit, not the per-bucket unit (line 404)
    tibble::tibble(age_sl = age_val, ageu_sl = ageunit)
  })
}

# ===========================================================================
# HELPER: age unit text (singular / plural)
# ===========================================================================
#' Build singular/plural age unit text.
#' @keywords internal
age_unit_text <- function(age_val, ageu_sl) {
  u <- tolower(gsub("[^a-z]", "", ageu_sl))
  base <- dplyr::case_when(
    u %in% c("yr", "year", "years")   ~ "year",
    u %in% c("mo", "month", "months") ~ "month",
    u %in% c("wk", "week", "weeks")   ~ "week",
    u %in% c("dy", "day", "days")     ~ "day",
    u %in% c("hr", "hour", "hours")   ~ "hour",
    TRUE ~ "year"
  )
  ifelse(age_val <= 1, base, paste0(base, "s"))
}

# ===========================================================================
# dm_params
# ===========================================================================
#' Initialise demographics parameters and load data.
#'
#' Replaces SAS \code{%params} macro (lines 91-180).
#' All SAS \code{%let} / \code{%global} variables become named arguments.
#'
#' @param dm_data   Data frame / tibble with DM domain, or NULL.
#' @param ds_data   Data frame / tibble with DS domain, or NULL.
#' @param dm_path   Character path to DM XPT file (used when dm_data is NULL).
#' @param ds_path   Character path to DS XPT file (used when ds_data is NULL).
#' @param panel_title Character. Title for panel output. Default "Demographics".
#' @param panel_desc  Character. Description text. Default "".
#' @param ndabla    Character. NDA/BLA identifier. Default "".
#' @param studyid   Character. Study identifier. Default "".
#' @param age_grps  Character vector of age-group boundaries.
#' @param ageunit   Character. Default age unit. Default "years".
#' @param output_file     Character path for Excel output, or NULL.
#' @param err_output_file Character path for error workbook, or NULL.
#' @param util_path Character path to utilities directory, or NULL.
#' @param sl_datasets Data frame for Script Launcher datasets, or NULL.
#' @param sl_group    Data frame for Script Launcher grouping, or NULL.
#' @param sl_subset   Data frame for Script Launcher subsetting, or NULL.
#'
#' @return Named list with all configuration and loaded data.
#' @export
dm_params <- function(dm_data = NULL,
                      ds_data = NULL,
                      dm_path = NULL,
                      ds_path = NULL,
                      panel_title = "Demographics",
                      panel_desc = "",
                      ndabla = "",
                      studyid = "",
                      age_grps = c("1 yr", "35 yr", "65 yr"),
                      ageunit = "years",
                      output_file = NULL,
                      err_output_file = NULL,
                      util_path = NULL,
                      sl_datasets = NULL,
                      sl_group = NULL,
                      sl_subset = NULL) {

  # Load data from XPT files if tibbles not provided directly
  if (is.null(dm_data) && !is.null(dm_path)) {
    dm_data <- haven::read_xpt(dm_path)
  }
  if (is.null(ds_data) && !is.null(ds_path)) {
    ds_data <- haven::read_xpt(ds_path)
  }

  # Lowercase all variable names for case-insensitive matching
  if (!is.null(dm_data)) {
    dm_data <- dplyr::rename_with(dm_data, tolower)
  }
  if (!is.null(ds_data)) {
    ds_data <- dplyr::rename_with(ds_data, tolower)
  }

  list(
    dm_data        = dm_data,
    ds_data        = ds_data,
    panel_title    = panel_title,
    panel_desc     = panel_desc,
    ndabla         = ndabla,
    studyid        = studyid,
    age_grps       = age_grps,
    ageunit        = ageunit,
    output_file    = output_file,
    err_output_file = err_output_file,
    util_path      = util_path,
    sl_datasets    = sl_datasets,
    sl_group       = sl_group,
    sl_subset      = sl_subset
  )
}

# ===========================================================================
# dm_setup
# ===========================================================================
#' Setup routine: validate data, process arms, bucket ages, merge DM-DS.
#'
#' Replaces SAS \code{%dm_setup} (lines 196-611). This is the most complex
#' function in the demographics panel.
#'
#' @param dm     Data frame with DM domain (lowercase column names).
#' @param ds     Data frame with DS domain (lowercase column names).
#' @param config Named list from \code{dm_params()}.
#'
#' @return Named list with setup results including dm, ds_dm, lkp_arm,
#'   lkp_arm_out, lkp_age, arm_count, arm_names, arm_counts, total_count,
#'   setup_success, dm_subj_gt0, setup_req_var, and variable flags.
#' @export
dm_setup <- function(dm, ds, config) {

  # -------------------------------------------------------------------
  # Step 1: Data validation checks (SAS lines 198-257)
  # -------------------------------------------------------------------
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # Required variable checks
  req_checks <- dplyr::bind_rows(
    chk_var(dm, "usubjid", ds_name = "dm"),
    chk_var(dm, "age",     ds_name = "dm"),
    chk_var(ds, "dsdecod", ds_name = "ds"),
    chk_var(ds, "usubjid", ds_name = "ds")
  )

  # Check ACTARM / ARM — at least one must exist (SAS lines 215-219)
  dm_actarm <- "actarm" %in% colnames(dm)
  dm_arm    <- "arm"    %in% colnames(dm)
  actarm_or_arm <- tibble::tibble(
    chk = "REQ",
    ds  = "dm",
    var = "ACTARM or ARM",
    type = NA_character_,
    len  = NA_integer_,
    condition = "at least one present",
    ind  = as.integer(dm_actarm | dm_arm)
  )
  req_checks <- dplyr::bind_rows(req_checks, actarm_or_arm)

  # Check DSSTDTC / DSSTDY — at least one must exist (SAS lines 220-224)
  ds_dsstdtc <- "dsstdtc" %in% colnames(ds)
  ds_dsstdy  <- "dsstdy"  %in% colnames(ds)
  dsstdtc_or_dsstdy <- tibble::tibble(
    chk = "REQ",
    ds  = "ds",
    var = "DSSTDTC or DSSTDY",
    type = NA_character_,
    len  = NA_integer_,
    condition = "at least one present",
    ind  = as.integer(ds_dsstdtc | ds_dsstdy)
  )
  req_checks <- dplyr::bind_rows(req_checks, dsstdtc_or_dsstdy)

  rpt_chk_var_req <- req_checks

  # Optional variable checks (SAS lines 228-247)
  dm_armcd   <- "armcd"   %in% colnames(dm)
  dm_ageu    <- "ageu"    %in% colnames(dm)
  dm_age     <- "age"     %in% colnames(dm)
  dm_country <- "country" %in% colnames(dm)
  dm_ethnic  <- "ethnic"  %in% colnames(dm)
  dm_race    <- "race"    %in% colnames(dm)
  dm_sex     <- "sex"     %in% colnames(dm)
  dm_siteid  <- "siteid"  %in% colnames(dm)
  ds_dscat   <- "dscat"   %in% colnames(ds)
  ds_dsscat  <- "dsscat"  %in% colnames(ds)
  ds_dsseq   <- "dsseq"   %in% colnames(ds)
  ds_dsdecod <- "dsdecod" %in% colnames(ds)

  # setup_req_var: TRUE if ALL required checks pass (SAS line 251)
  setup_req_var <- all(rpt_chk_var_req$ind == 1L)

  # Variable flags list
  variable_flags <- list(
    dm_actarm = dm_actarm, dm_arm = dm_arm, dm_armcd = dm_armcd,
    dm_ageu = dm_ageu, dm_age = dm_age,
    dm_country = dm_country, dm_ethnic = dm_ethnic,
    dm_race = dm_race, dm_sex = dm_sex, dm_siteid = dm_siteid,
    ds_dsdecod = ds_dsdecod, ds_dsstdtc = ds_dsstdtc,
    ds_dsstdy = ds_dsstdy, ds_dscat = ds_dscat,
    ds_dsscat = ds_dsscat, ds_dsseq = ds_dsseq
  )

  # Early return if no subjects or required vars missing (SAS lines 603-609)
  if (!dm_subj_gt0 || !setup_req_var) {
    return(list(
      setup_success    = FALSE,
      dm_subj_gt0      = dm_subj_gt0,
      setup_req_var    = setup_req_var,
      rpt_chk_var_req  = rpt_chk_var_req,
      variable_flags   = variable_flags,
      dm_actarm        = dm_actarm
    ))
  }

  # -------------------------------------------------------------------
  # Step 2: ARM processing (SAS lines 264-329)
  # -------------------------------------------------------------------
  # Use ACTARM if available, otherwise ARM (SAS lines 265-271)
  if (dm_actarm) {
    if (dm_arm) {
      dm <- dplyr::rename(dm, plannedarm = arm, arm = actarm)
    } else {
      dm <- dplyr::rename(dm, arm = actarm)
    }
  }

  # Build lkp_arm: exclude screen failures via ARMCD (SAS lines 274-282)
  if (dm_armcd) {
    lkp_arm <- dm %>%
      dplyr::filter(!(toupper(armcd) %in% c("SCRNFAIL", "NOTASSGN"))) %>%
      dplyr::count(arm, name = "arm_count") %>%
      dplyr::arrange(arm)
  } else {
    lkp_arm <- dm %>%
      dplyr::count(arm, name = "arm_count") %>%
      dplyr::arrange(arm)
  }

  # Format arm display names (SAS lines 290-306)
  lkp_arm <- lkp_arm %>%
    dplyr::mutate(
      arm_num     = dplyr::row_number(),
      arm_display = purrr::map_chr(arm, format_arm_display)
    )

  # Total count (SAS lines 308-316)
  total_count <- sum(lkp_arm$arm_count)
  arm_count   <- nrow(lkp_arm)
  arm_names   <- lkp_arm$arm_display
  arm_counts  <- lkp_arm$arm_count

  # lkp_arm_out: arm display names + counts with Overall row (SAS lines 319-329)
  lkp_arm_out <- dplyr::bind_rows(
    lkp_arm %>% dplyr::transmute(arm = arm_display, arm_count),
    tibble::tibble(arm = "Overall", arm_count = total_count)
  )

  # -------------------------------------------------------------------
  # Step 3: DM data cleaning (SAS lines 331-389)
  # -------------------------------------------------------------------
  # Remove screen failures (SAS lines 336-344)
  if (dm_armcd) {
    dm <- dm %>%
      dplyr::filter(
        !(toupper(armcd) %in% c("SCRNFAIL", "NOTASSGN", "NOTTRT"))
      )
  }

  # Hash lookup for arm_num (SAS lines 348-358)
  dm <- dm %>%
    dplyr::left_join(
      lkp_arm %>% dplyr::select(arm, arm_num),
      by = "arm"
    )
  # Remove subjects whose arm was not in lkp_arm
  dm <- dm %>% dplyr::filter(!is.na(arm_num))

  # Missing variable imputation (SAS lines 360-388)
  if (!dm_country) {
    dm <- dplyr::mutate(dm, country = "Missing")
  } else {
    dm <- dplyr::mutate(dm, country = dplyr::if_else(
      is.na(country) | stringr::str_trim(as.character(country)) == "",
      "Missing", as.character(country)))
  }

  if (!dm_ethnic) {
    dm <- dplyr::mutate(dm, ethnic = "Missing")
  } else {
    dm <- dplyr::mutate(dm, ethnic = dplyr::if_else(
      is.na(ethnic) | stringr::str_trim(as.character(ethnic)) == "",
      "Missing", stringr::str_to_title(as.character(ethnic))))
  }

  if (!dm_race) {
    dm <- dplyr::mutate(dm, race = "Missing")
  } else {
    dm <- dplyr::mutate(dm, race = dplyr::if_else(
      is.na(race) | stringr::str_trim(as.character(race)) == "",
      "Missing", stringr::str_to_title(as.character(race))))
  }

  if (!dm_sex) {
    dm <- dplyr::mutate(dm, sex = "Missing")
  } else {
    dm <- dplyr::mutate(dm, sex = dplyr::if_else(
      is.na(sex) | stringr::str_trim(as.character(sex)) == "",
      "Missing", as.character(sex)))
  }

  if (!dm_siteid) {
    dm <- dplyr::mutate(dm, siteid = "Missing")
  } else {
    dm <- dplyr::mutate(dm, siteid = dplyr::if_else(
      is.na(siteid) | stringr::str_trim(as.character(siteid)) == "",
      "Missing", as.character(siteid)))
  }

  if (!dm_age) dm <- dplyr::mutate(dm, age = NA_real_)

  # -------------------------------------------------------------------
  # Step 4: Age bucketing (SAS lines 396-512)
  # -------------------------------------------------------------------
  parsed_grps <- parse_age_grps(config$age_grps, config$ageunit)
  # Remove duplicate age boundaries (SAS nodupkey, line 415)
  parsed_grps <- dplyr::distinct(parsed_grps, age_sl, ageu_sl, .keep_all = TRUE)

  n_grps <- nrow(parsed_grps)

  # Build lkp_age row by row matching SAS logic (lines 418-457)
  lkp_rows <- vector("list", n_grps + 1L)
  for (i in seq_len(n_grps)) {
    row_min <- if (i == 1L) 0 else parsed_grps$age_sl[i - 1L]
    row_max <- parsed_grps$age_sl[i]
    min_ageu_str <- if (i == 1L) config$ageunit else config$ageunit
    max_ageu_str <- config$ageunit
    lkp_rows[[i]] <- tibble::tibble(
      min_age    = row_min,
      max_age    = row_max,
      min_ageu   = min_ageu_str,
      max_ageu   = max_ageu_str,
      min_age_yr = age_to_years(row_min, min_ageu_str),
      max_age_yr = age_to_years(row_max, max_ageu_str)
    )
  }
  # Last bucket: min = last boundary, max = 200 (SAS lines 450-456)
  lkp_rows[[n_grps + 1L]] <- tibble::tibble(
    min_age    = parsed_grps$age_sl[n_grps],
    max_age    = 200,
    min_ageu   = config$ageunit,
    max_ageu   = config$ageunit,
    min_age_yr = age_to_years(parsed_grps$age_sl[n_grps], config$ageunit),
    max_age_yr = 200
  )
  lkp_age <- dplyr::bind_rows(lkp_rows) %>%
    dplyr::mutate(age_order = dplyr::row_number())

  # Build age flag labels (SAS lines 460-506)
  n_buckets <- nrow(lkp_age)
  age_flags <- character(n_buckets)

  for (i in seq_len(n_buckets)) {
    min_u_txt <- age_unit_text(lkp_age$min_age[i], lkp_age$min_ageu[i])
    max_u_txt <- age_unit_text(lkp_age$max_age[i], lkp_age$max_ageu[i])

    # If both units are years, omit the unit text (SAS lines 478-485)
    both_years <- grepl("^year", tolower(min_u_txt)) &&
      grepl("^year", tolower(max_u_txt))
    if (both_years) {
      min_u_txt <- ""
      max_u_txt <- ""
    }
    min_str <- trimws(paste(lkp_age$min_age[i], min_u_txt))
    max_str <- trimws(paste(lkp_age$max_age[i], max_u_txt))

    if (i == 1L) {
      age_flags[i] <- trimws(paste("Age under", max_str))
    } else if (i == n_buckets) {
      age_flags[i] <- trimws(paste("Age", min_str, "and over"))
    } else {
      age_flags[i] <- trimws(paste("Age between", min_str, "and", max_str))
    }
    age_flags[i] <- gsub("\\s+", " ", age_flags[i])
  }
  lkp_age$age_flag <- age_flags

  # Add Missing bucket (SAS lines 499-504)
  missing_row <- tibble::tibble(
    min_age = NA_real_, max_age = NA_real_,
    min_ageu = NA_character_, max_ageu = NA_character_,
    min_age_yr = NA_real_, max_age_yr = NA_real_,
    age_order = 99L, age_flag = "Missing"
  )
  lkp_age <- dplyr::bind_rows(lkp_age, missing_row)

  # Assign age flags to DM subjects (SAS lines 514-546)
  if (dm_age) {
    # Convert subject age to years using AGEU if available
    if (dm_ageu) {
      dm <- dm %>%
        dplyr::mutate(
          age_yr = dplyr::case_when(
            is.na(age)                                  ~ NA_real_,
            grepl("^YEAR", toupper(ageu), perl = TRUE)  ~ age,
            grepl("^MONTH", toupper(ageu), perl = TRUE) ~ age / 12,
            grepl("^WEEK", toupper(ageu), perl = TRUE)  ~ age / 52.178571428571428571428571428571,
            grepl("^DAY", toupper(ageu), perl = TRUE)   ~ age / 365.25,
            grepl("^HOUR", toupper(ageu), perl = TRUE)  ~ age / 8766,
            TRUE                                        ~ age
          )
        )
    } else {
      dm <- dplyr::mutate(dm, age_yr = age)
    }

    # Match subject age_yr to lkp_age buckets using findInterval
    # (left-closed, right-open intervals — SAS: age_yr >= min and age_yr < max)
    age_breaks <- lkp_age %>%
      dplyr::filter(age_order != 99L) %>%
      dplyr::arrange(age_order)

    break_points <- dplyr::pull(age_breaks, max_age_yr)
    age_labels   <- dplyr::pull(age_breaks, age_flag)

    dm <- dm %>%
      dplyr::mutate(
        .bucket_idx = findInterval(age_yr, c(0, break_points),
                                  rightmost.closed = FALSE),
        .bucket_idx = pmin(.bucket_idx, length(age_labels)),
        .bucket_idx = pmax(.bucket_idx, 1L),
        age_flag    = dplyr::if_else(
          is.na(age_yr), "Missing",
          age_labels[.bucket_idx]
        )
      ) %>%
      dplyr::select(-age_yr, -.bucket_idx)
  } else {
    dm <- dplyr::mutate(dm, age_flag = "Missing")
  }

  # -------------------------------------------------------------------
  # Step 5: DM-DS merge (SAS lines 548-601)
  # -------------------------------------------------------------------
  dm <- dplyr::arrange(dm, usubjid)
  ds <- dplyr::arrange(ds, usubjid)

  ds_cols <- intersect(
    c("usubjid", "dsdecod", "dscat", "dsscat", "dsseq", "dsstdtc", "dsstdy"),
    colnames(ds)
  )
  dm_cols <- intersect(
    c("usubjid", "arm", "arm_num", "age_flag", "country", "ethnic",
      "race", "sex", "siteid"),
    colnames(dm)
  )

  ds_dm <- dplyr::inner_join(
    ds %>% dplyr::select(dplyr::all_of(ds_cols)),
    dm %>% dplyr::select(dplyr::all_of(dm_cols)),
    by = "usubjid"
  )

  # Propcase dsdecod if all uppercase (SAS line 555)
  ds_dm <- ds_dm %>%
    dplyr::mutate(dsdecod = dplyr::if_else(
      !is.na(dsdecod) & dsdecod == toupper(dsdecod),
      stringr::str_to_title(dsdecod), dsdecod
    ))

  # Impute missing dscat / dsscat (SAS lines 558-562)
  if (!ds_dscat) {
    ds_dm <- dplyr::mutate(ds_dm, dscat = "Missing")
  } else {
    ds_dm <- dplyr::mutate(ds_dm, dscat = dplyr::if_else(
      is.na(dscat) | stringr::str_trim(as.character(dscat)) == "",
      "Missing", as.character(dscat)))
  }

  if (!ds_dsscat) {
    ds_dm <- dplyr::mutate(ds_dm, dsscat = "Missing")
  } else {
    ds_dm <- dplyr::mutate(ds_dm, dsscat = dplyr::if_else(
      is.na(dsscat) | stringr::str_trim(as.character(dsscat)) == "",
      "Missing", as.character(dsscat)))
  }

  # Date conversion for DSSTDTC (SAS line 564-565)
  if (ds_dsstdtc) {
    ds_dm <- ds_dm %>%
      dplyr::mutate(
        dsstdt = suppressWarnings(as.Date(dsstdtc, format = "%Y-%m-%d"))
      )
  }

  # Death ordering (SAS line 569)
  ds_dm <- ds_dm %>%
    dplyr::mutate(.order = dplyr::if_else(
      toupper(dsdecod) == "DEATH", 100L, 1L
    ))

  # Build sort key for dedup (SAS lines 576-596)
  sort_cols <- c("usubjid", "dscat", "dsscat", ".order")
  if (ds_dsseq) sort_cols <- c(sort_cols, "dsseq")
  if (ds_dsstdtc && "dsstdt" %in% colnames(ds_dm)) {
    sort_cols <- c(sort_cols, "dsstdt")
  }
  sort_cols <- c(sort_cols, "dsdecod")

  ds_dm <- ds_dm %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Keep all Protocol Milestone / Informed Consent / Randomized events;

  # others: keep LAST per subject/category/subcategory (SAS lines 587-594)
  keep_all_events <- c("INFORMED CONSENT OBTAINED", "RANDOMIZED")
  ds_dm <- ds_dm %>%
    dplyr::mutate(
      .is_pm   = toupper(dscat) == "PROTOCOL MILESTONE",
      .is_keep = .is_pm | toupper(dsdecod) %in% keep_all_events
    ) %>%
    dplyr::group_by(usubjid, dscat, dsscat) %>%
    dplyr::filter(.is_keep | dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup()

  # Final dedup: one row per subject x dsdecod (SAS lines 598-601)
  ds_dm <- ds_dm %>%
    dplyr::distinct(usubjid, dsdecod, .keep_all = TRUE)

  # Clean up temporary columns
  ds_dm <- ds_dm %>%
    dplyr::select(-dplyr::any_of(c(
      ".order", ".is_pm", ".is_keep", "dsstdt"
    )))

  # -------------------------------------------------------------------
  # Return
  # -------------------------------------------------------------------
  list(
    setup_success   = TRUE,
    dm              = dm,
    ds_dm           = ds_dm,
    lkp_arm         = lkp_arm,
    lkp_arm_out     = lkp_arm_out,
    lkp_age         = lkp_age,
    arm_count       = arm_count,
    arm_names       = arm_names,
    arm_counts      = arm_counts,
    total_count     = total_count,
    dm_subj_gt0     = dm_subj_gt0,
    setup_req_var   = setup_req_var,
    rpt_chk_var_req = rpt_chk_var_req,
    variable_flags  = variable_flags,
    dm_actarm       = dm_actarm
  )
}

# ===========================================================================
# dm_tabulate
# ===========================================================================
#' Per-variable frequency counts and percentages by arm.
#'
#' Replaces SAS \code{%dm(var)} macro (lines 617-651). Supports cross-
#' tabulation variables (e.g. \code{"country*siteid"}) by splitting on
#' the asterisk.
#'
#' @param dm         Cleaned DM data frame from \code{dm_setup()}.
#' @param var        Character string with variable name. Use \code{*} for
#'                   cross-tabulation (e.g. \code{"country*siteid"}).
#' @param arm_count  Integer: number of treatment arms (excl. Overall).
#' @param arm_names  Character vector of arm display names.
#' @param arm_counts Integer vector of per-arm subject counts.
#' @param total_count Integer: total subjects across all arms.
#'
#' @return A tibble with per-category counts and percentages by arm plus
#'   total columns.
#' @export
dm_tabulate <- function(dm, var, arm_count, arm_names, arm_counts,
                        total_count) {

  # Handle cross-tabulation (e.g., "country*siteid") — SAS lines 621-630
  var_list <- stringr::str_split(var, "\\*")[[1L]]

  # Group and count (SAS PROC SQL lines 632-649)
  counts <- dm %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_list, "arm_num")))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(var_list)),
      arm_num = seq_len(arm_count),
      fill = list(count = 0L)
    )

  # Pivot wider for per-arm columns
  wide <- counts %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = count,
      names_glue  = "arm_{arm_num}_count",
      values_fill = 0L
    )

  # Add percentages per arm (denominator = arm total subjects)
  for (i in seq_len(arm_count)) {
    col_count <- paste0("arm_", i, "_count")
    col_pct   <- paste0("arm_", i, "_pct")
    if (arm_counts[i] > 0L) {
      wide[[col_pct]] <- 100 * wide[[col_count]] / arm_counts[i]
    } else {
      wide[[col_pct]] <- rep(NA_real_, nrow(wide))
    }
  }

  # Add total across all arms
  wide <- wide %>%
    dplyr::mutate(
      total_count = rowSums(dplyr::across(dplyr::matches("^arm_\\d+_count$"))),
      total_pct   = dplyr::if_else(
        total_count > 0,
        100 * total_count / total_count,  # interim; corrected below
        NA_real_
      )
    )
  # Correct total_pct: denominator is overall total_count param
  if (total_count > 0L) {
    wide$total_pct <- 100 * wide$total_count / total_count
  } else {
    wide$total_pct <- rep(NA_real_, nrow(wide))
  }

  # Sort: by key variables, then descending total_count (SAS lines 650-651)
  key_vars <- if (length(var_list) > 1L) var_list[-length(var_list)] else character(0)
  if (length(key_vars) > 0L) {
    wide <- wide %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(key_vars)),
                     dplyr::desc(total_count))
  } else {
    wide <- wide %>%
      dplyr::arrange(dplyr::desc(total_count))
  }

  wide
}

# ===========================================================================
# dm_stat
# ===========================================================================
#' Descriptive statistics for a continuous variable by arm.
#'
#' Replaces SAS \code{%dm_stat(ds, var)} macro (lines 657-809). Computes
#' mean, SE, mode, min, Q1, median, Q3, max per arm plus overall, and
#' derives chart metrics for boxplot-style visualisation.
#'
#' @param data       Data frame containing the variable.
#' @param var        Character: name of the numeric variable (e.g. \code{"age"}).
#' @param arm_count  Integer: number of treatment arms (excl. Overall).
#' @param arm_names  Character vector of arm display names.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{\code{stat}}{Display-ready tibble of statistics by arm.}
#'     \item{\code{chart}}{Tibble of chart (boxplot) metrics by arm.}
#'   }
#' @export
dm_stat <- function(data, var, arm_count, arm_names) {

  # Helper: compute summary statistics for a numeric vector
  compute_stats <- function(x) {
    x_clean <- x[!is.na(x)]
    n_obs   <- length(x_clean)
    if (n_obs == 0L) {
      return(tibble::tibble(
        n = 0L, mean = NA_real_, std = NA_real_, mode_val = NA_real_,
        min = NA_real_, q1 = NA_real_, median = NA_real_,
        q3 = NA_real_, max = NA_real_
      ))
    }

    # Mode: SAS returns smallest mode when tied
    freq_tbl <- table(x_clean)
    max_freq <- max(freq_tbl)
    modes    <- as.numeric(names(freq_tbl)[freq_tbl == max_freq])
    mode_val <- min(modes)

    tibble::tibble(
      n        = n_obs,
      mean     = mean(x_clean),
      std      = stats::sd(x_clean),
      mode_val = mode_val,
      min      = min(x_clean),
      q1       = as.numeric(stats::quantile(x_clean, 0.25, type = 2)),
      median   = stats::median(x_clean),
      q3       = as.numeric(stats::quantile(x_clean, 0.75, type = 2)),
      max      = max(x_clean)
    )
  }

  # Per-arm statistics (SAS lines 668-681)
  stats_by_arm <- data %>%
    dplyr::group_by(arm_num) %>%
    dplyr::summarise(
      compute_stats(.data[[var]]),
      .groups = "drop"
    )

  # Ensure every arm has a row (SAS merge onto lkp_arm, lines 698-700)
  all_arms <- tibble::tibble(arm_num = seq_len(arm_count))
  stats_by_arm <- all_arms %>%
    dplyr::left_join(stats_by_arm, by = "arm_num")

  # Overall statistics (SAS lines 714-741)
  stats_overall <- compute_stats(data[[var]]) %>%
    dplyr::mutate(arm_num = arm_count + 1L)

  # Combine per-arm + overall
  all_stats <- dplyr::bind_rows(stats_by_arm, stats_overall)

  # Chart metrics (SAS lines 685-694)
  all_stats <- all_stats %>%
    dplyr::mutate(
      chart_pct25 = q1,
      chart_pct50 = median - q1,
      chart_pct75 = q3 - median,
      chart_min   = q1 - min,
      chart_max   = max - q3
    )

  # Build formatted "Mean (SE)" string using SAS-compatible rounding
  # (SAS lines 765-790: put(mean,8.1)||' ('||put(std,8.1)||')')
  all_stats <- all_stats %>%
    dplyr::mutate(
      mean_se_str = dplyr::if_else(
        !is.na(mean),
        paste0(
          sprintf("%.1f", janitor::round_half_up(mean, 1)),
          " (",
          sprintf("%.1f", janitor::round_half_up(std, 1)),
          ")"
        ),
        NA_character_
      )
    )

  # --- Build display stat table (long → wide by arm) ---
  # Stat labels matching SAS (lines 758-764)
  stat_labels <- c("Mean (SE)", "Mode", "Min", "Q1", "Median", "Q3", "Max")
  stat_cols   <- c("mean_se_str", "mode_val", "min", "q1", "median", "q3", "max")

  stat_long <- all_stats %>%
    dplyr::select(arm_num, dplyr::all_of(stat_cols)) %>%
    dplyr::mutate(dplyr::across(-arm_num, as.character)) %>%
    tidyr::pivot_longer(
      cols      = -arm_num,
      names_to  = "stat_key",
      values_to = "value"
    ) %>%
    dplyr::mutate(stat = dplyr::case_when(
      stat_key == "mean_se_str" ~ "Mean (SE)",
      stat_key == "mode_val"    ~ "Mode",
      stat_key == "min"         ~ "Min",
      stat_key == "q1"          ~ "Q1",
      stat_key == "median"      ~ "Median",
      stat_key == "q3"          ~ "Q3",
      stat_key == "max"         ~ "Max",
      TRUE                      ~ stat_key
    )) %>%
    dplyr::select(-stat_key) %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = value,
      names_glue  = "arm_{arm_num}"
    )

  # Enforce display order (SAS lines 795-809)
  stat_long <- stat_long %>%
    dplyr::mutate(.ord = match(stat, stat_labels)) %>%
    dplyr::arrange(.ord) %>%
    dplyr::select(-.ord)

  # --- Build chart data table ---
  chart_labels <- c("chart_pct25", "chart_pct50", "chart_pct75",
                     "chart_min", "chart_max")

  chart_long <- all_stats %>%
    dplyr::select(arm_num, dplyr::all_of(chart_labels)) %>%
    dplyr::mutate(dplyr::across(-arm_num, as.character)) %>%
    tidyr::pivot_longer(
      cols      = -arm_num,
      names_to  = "stat",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = value,
      names_glue  = "arm_{arm_num}"
    )

  list(stat = stat_long, chart = chart_long)
}

# ===========================================================================
# dm_by_ds
# ===========================================================================
#' Demographics x Disposition cross-tabulation.
#'
#' Replaces SAS \code{%dm_by_ds(var)} macro (lines 815-903). Computes
#' frequency counts and percentages of a demographic variable stratified
#' by disposition category (\code{dsdecod}) and treatment arm.
#'
#' @param ds_dm      Merged DS-DM data frame from \code{dm_setup()}.
#' @param dm         Cleaned DM data frame (denominator source).
#' @param var        Character: demographic variable name.
#' @param arm_count  Integer: number of treatment arms.
#' @param arm_counts Integer vector of per-arm subject counts from DM.
#'
#' @return A tibble with per-category counts and percentages by arm and
#'   disposition event, plus total columns.
#' @export
dm_by_ds <- function(ds_dm, dm, var, arm_count, arm_counts) {

  # Cross-tab: var x arm_num x dsdecod (SAS PROC FREQ sparse, lines 820-822)
  ct_c <- ds_dm %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(var, "arm_num", "dsdecod"))),
      name = "count"
    ) %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(c(var, "dsdecod"))),
      arm_num = seq_len(arm_count),
      fill = list(count = 0L)
    )

  # Subgroup arm counts: var x arm from DM (denominator) — SAS lines 826-828
  ct_sac <- dm %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(var, "arm_num"))),
      name = "subgroup_arm_count"
    )

  # Total subgroup counts: var across all arms — SAS lines 828-829
  ct_sc <- dm %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(var)),
      name = "subgroup_count"
    )

  # Merge and compute percentage (SAS lines 831-842)
  ct_cp <- ct_c %>%
    dplyr::left_join(ct_sac, by = c(var, "arm_num")) %>%
    dplyr::mutate(
      subgroup_arm_count = dplyr::if_else(
        is.na(subgroup_arm_count), 0L, as.integer(subgroup_arm_count)
      ),
      percent = dplyr::if_else(
        subgroup_arm_count > 0L,
        100 * count / subgroup_arm_count,
        NA_real_
      )
    )

  # Pivot wider: separate count and percent columns (SAS TRANSPOSE lines 864-876)
  wide_count <- ct_cp %>%
    dplyr::select(dplyr::all_of(c(var, "dsdecod", "arm_num", "count"))) %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = count,
      names_glue  = "arm_{arm_num}_count",
      values_fill = 0L
    )

  wide_pct <- ct_cp %>%
    dplyr::select(dplyr::all_of(c(var, "dsdecod", "arm_num", "percent"))) %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = percent,
      names_glue  = "arm_{arm_num}_pct"
    )

  # Combine count + percent (SAS lines 878-882)
  result <- wide_count %>%
    dplyr::left_join(wide_pct, by = c(var, "dsdecod"))

  # Add total columns (SAS lines 884-897)
  count_cols <- paste0("arm_", seq_len(arm_count), "_count")
  result <- result %>%
    dplyr::left_join(ct_sc, by = var) %>%
    dplyr::mutate(
      total_count = rowSums(dplyr::across(dplyr::all_of(count_cols))),
      total_pct   = dplyr::if_else(
        subgroup_count > 0L,
        100 * total_count / subgroup_count,
        NA_real_
      )
    ) %>%
    dplyr::select(-subgroup_count)

  # Sort by dsdecod, then descending total_pct (SAS line 899)
  result %>%
    dplyr::arrange(dsdecod, dplyr::desc(total_pct))
}

# ===========================================================================
# dm_outfmt
# ===========================================================================
#' Format all results for output.
#'
#' Replaces SAS \code{%dm_outfmt} macro (lines 909-1048). Sorts age by
#' \code{age_order}, orders race with Other/Missing last, blanks repeated
#' category values, builds the combined \code{dm_overall} table, and
#' replaces numeric \code{NA} with 0 in DS tables.
#'
#' @param results    Named list with \code{dm_results}, \code{ds_results},
#'                   and \code{age_stats} from the demographics workflow.
#' @param lkp_age    Age lookup tibble from \code{dm_setup()}.
#' @param lkp_arm_out Arm lookup tibble (including Overall) from \code{dm_setup()}.
#'
#' @return Named list of all formatted output tibbles ready for Excel export.
#' @export
dm_outfmt <- function(results, lkp_age, lkp_arm_out) {

  # --- Helper: sort age datasets by age_order via lkp_age (SAS lines 917-930)
  age_sort <- function(df, extra_sort_cols = NULL) {
    df <- df %>%
      dplyr::left_join(
        lkp_age %>% dplyr::select(age_flag, age_order),
        by = "age_flag"
      )
    if (!is.null(extra_sort_cols) && length(extra_sort_cols) > 0L) {
      df <- df %>%
        dplyr::arrange(
          dplyr::across(dplyr::all_of(extra_sort_cols)),
          age_order
        )
    } else {
      df <- df %>% dplyr::arrange(age_order)
    }
    df %>% dplyr::select(-age_order)
  }

  # --- Helper: race ordering — Other, Missing last (SAS lines 933-940)
  # Uses forcats::fct_relevel for factor-level ordering per AAP mandate
  race_sort <- function(df, extra_sort_cols = NULL) {
    # Build ordered factor: all unique races, with Other & Missing last
    all_races <- sort(unique(df$race))
    non_special <- setdiff(all_races, c("Other", "Missing"))
    ordered_lvls <- c(non_special, intersect(c("Other", "Missing"), all_races))
    df <- df %>%
      dplyr::mutate(
        race = forcats::fct_relevel(race, ordered_lvls)
      )
    if (!is.null(extra_sort_cols) && length(extra_sort_cols) > 0L) {
      df <- df %>%
        dplyr::arrange(
          dplyr::across(dplyr::all_of(extra_sort_cols)),
          race
        )
    } else {
      df <- df %>% dplyr::arrange(race)
    }
    df %>% dplyr::mutate(race = as.character(race))
  }

  # --- Helper: blank repeated values in a column (SAS first.var logic)
  # Uses dplyr::lag() to detect when current value equals previous value
  blank_repeated <- function(df, col) {
    df %>%
      dplyr::mutate(
        !!col := dplyr::if_else(
          as.character(.data[[col]]) == dplyr::lag(
            as.character(.data[[col]]), default = ""
          ),
          "", as.character(.data[[col]])
        )
      )
  }

  # --- Helper: replace numeric NA with 0 in DS tables (SAS lines 961-966)
  na_to_zero_numeric <- function(df) {
    df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~ dplyr::if_else(is.na(.x), 0, .x) # legitimate: NA counts treated as zero for display table
        )
      )
  }

  # ---- Apply formatting ----

  # DM tables

  dm_age_flag <- age_sort(results$dm_results$age_flag)
  dm_sex      <- results$dm_results$sex
  dm_race     <- race_sort(results$dm_results$race)
  dm_ethnic   <- results$dm_results$ethnic
  dm_country  <- results$dm_results$country
  dm_siteid   <- results$dm_results$siteid

  # Country*siteid: blank repeated country (SAS lines 943-960)
  dm_country_siteid <- results$dm_results$country_siteid %>%
    dplyr::arrange(country, siteid) %>%
    blank_repeated("country")

  # DS tables: sort, blank repeated dsdecod, NA→0 (SAS lines 961-966)
  ds_age_flag <- results$ds_results$age_flag %>%
    age_sort(extra_sort_cols = "dsdecod") %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  ds_sex <- results$ds_results$sex %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  ds_race <- results$ds_results$race %>%
    race_sort(extra_sort_cols = "dsdecod") %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  ds_ethnic <- results$ds_results$ethnic %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  ds_country <- results$ds_results$country %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  ds_siteid <- results$ds_results$siteid %>%
    na_to_zero_numeric() %>%
    blank_repeated("dsdecod")

  # --- dm_overall_stat: age stats without Mode (SAS lines 968-975) ---
  dm_overall_stat <- results$age_stats$stat %>%
    dplyr::filter(stat != "Mode")

  # --- dm_overall: combined demographics table (SAS lines 978-1048) ---
  # Build sections with var / val columns
  build_section <- function(tab, var_label, val_col) {
    tab %>%
      dplyr::mutate(var = var_label, val = as.character(.data[[val_col]])) %>%
      dplyr::select(var, val, dplyr::everything(), -dplyr::all_of(val_col))
  }

  dm_overall <- dplyr::bind_rows(
    build_section(dm_age_flag, "Age Group",  "age_flag"),
    build_section(dm_sex,      "Sex",        "sex"),
    build_section(dm_race,     "Race",       "race"),
    build_section(dm_ethnic,   "Ethnicity",  "ethnic")
  ) %>%
    # Preserve section order via factor encoding before blanking
    dplyr::mutate(var = as.character(forcats::fct_inorder(var))) %>%
    blank_repeated("var")

  # Return all formatted results
  list(
    lkp_arm_out       = lkp_arm_out,
    lkp_age           = lkp_age,
    dm_overall_stat   = dm_overall_stat,
    dm_overall        = dm_overall,
    dm_age_flag       = dm_age_flag,
    dm_age_stat       = results$age_stats$stat,
    dm_age_chart      = results$age_stats$chart,
    dm_sex            = dm_sex,
    dm_race           = dm_race,
    dm_ethnic         = dm_ethnic,
    dm_country        = dm_country,
    dm_siteid         = dm_siteid,
    dm_country_siteid = dm_country_siteid,
    ds_age_flag       = ds_age_flag,
    ds_sex            = ds_sex,
    ds_race           = ds_race,
    ds_ethnic         = ds_ethnic,
    ds_country        = ds_country,
    ds_siteid         = ds_siteid
  )
}

# ===========================================================================
# dm_out
# ===========================================================================
#' Write Demographics Excel workbook.
#'
#' Replaces SAS \code{%dm_out} macro (lines 1054-1165). Creates an Excel
#' workbook with 20 worksheets using \code{openxlsx}, matching the SAS
#' JET/PCFILES LIBNAME output structure.
#'
#' @param output_file  Character: path to write the .xlsx file.
#' @param results      Named list from \code{dm_outfmt()}.
#' @param config       Named list with \code{ndabla}, \code{studyid},
#'                     \code{arm_count}, \code{dm_actarm}.
#'
#' @return Invisible \code{output_file} path.
#' @export
dm_out <- function(output_file, results, config) {

  # Build info dataset (SAS lines 1058-1098)
  info <- tibble::tibble(
    item = c(
      "NDA/BLA",
      "Study",
      "Date",
      "Custom Datasets",
      "Arm Variable",
      "Arm Count",
      paste0("dm_overall rows: ",        nrow(results$dm_overall)),
      paste0("dm_age_group rows: ",      nrow(results$dm_age_flag)),
      paste0("dm_sex rows: ",            nrow(results$dm_sex)),
      paste0("dm_race rows: ",           nrow(results$dm_race)),
      paste0("dm_ethnicity rows: ",      nrow(results$dm_ethnic)),
      paste0("dm_country rows: ",        nrow(results$dm_country)),
      paste0("dm_siteid rows: ",         nrow(results$dm_siteid)),
      paste0("dm_country_siteid rows: ", nrow(results$dm_country_siteid)),
      paste0("ds_age_group rows: ",      nrow(results$ds_age_flag)),
      paste0("ds_sex rows: ",            nrow(results$ds_sex)),
      paste0("ds_race rows: ",           nrow(results$ds_race)),
      paste0("ds_ethnicity rows: ",      nrow(results$ds_ethnic)),
      paste0("ds_country rows: ",        nrow(results$ds_country)),
      paste0("ds_siteid rows: ",         nrow(results$ds_siteid))
    ),
    value = c(
      config$ndabla   %||% "",
      config$studyid  %||% "",
      format(Sys.Date(), "%Y-%m-%d"),
      "No",
      if (isTRUE(config$dm_actarm)) "ACTARM" else "ARM",
      as.character(config$arm_count %||% 0L),
      rep("", 14L)
    )
  )

  # Create workbook and helper function (SAS lines 1104-1142)
  wb <- openxlsx::createWorkbook()

  write_sheet <- function(sheet_name, data) {
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = data)
  }

  # Write all 20 sheets matching SAS output order
  write_sheet("arms",              results$lkp_arm_out)
  write_sheet("age_groups",        results$lkp_age %>%
                                     dplyr::filter(age_order != 99L))
  write_sheet("info",              info)
  write_sheet("dm_overall_stat",   results$dm_overall_stat)
  write_sheet("dm_overall",        results$dm_overall)
  write_sheet("dm_age",            results$dm_age_flag)
  write_sheet("dm_age_stat",       results$dm_age_stat)
  write_sheet("age_stats",         results$dm_age_chart)
  write_sheet("dm_sex",            results$dm_sex)
  write_sheet("dm_race",           results$dm_race)
  write_sheet("dm_ethnicity",      results$dm_ethnic)
  write_sheet("dm_country",        results$dm_country)
  write_sheet("dm_siteid",         results$dm_siteid)
  write_sheet("dm_country_siteid", results$dm_country_siteid)
  write_sheet("ds_age",            results$ds_age_flag)
  write_sheet("ds_sex",            results$ds_sex)
  write_sheet("ds_race",           results$ds_race)
  write_sheet("ds_ethnicity",      results$ds_ethnic)
  write_sheet("ds_country",        results$ds_country)
  write_sheet("ds_siteid",         results$ds_siteid)

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  cli::cli_alert_success("Demographics workbook saved to {.path {output_file}}")

  invisible(output_file)
}

# ===========================================================================
# demographics  (main orchestrator)
# ===========================================================================
#' Run the Demographics Analysis Panel.
#'
#' Replaces SAS \code{%demographics} macro (lines 1168-1218). Orchestrates
#' the complete demographics workflow: setup → tabulate → stats → format →
#' Excel output.
#'
#' @param dm_data  Data frame (or tibble) with DM domain, or \code{NULL}
#'                 (in which case \code{config$dm_path} must be supplied).
#' @param ds_data  Data frame (or tibble) with DS domain, or \code{NULL}
#'                 (in which case \code{config$ds_path} must be supplied).
#' @param config   Named list of configuration parameters. See
#'                 \code{dm_params()} for recognised names and defaults.
#'
#' @return Invisible named list of formatted results on success, or
#'   \code{NULL} on failure.
#' @export
demographics <- function(dm_data = NULL, ds_data = NULL, config = list()) {

  # Build full config via dm_params (merges defaults with user overrides)
  full_config <- dm_params(
    dm_data         = dm_data,
    ds_data         = ds_data,
    dm_path         = config$dm_path         %||% NULL,
    ds_path         = config$ds_path         %||% NULL,
    panel_title     = config$panel_title     %||% "Demographics",
    panel_desc      = config$panel_desc      %||% "",
    ndabla          = config$ndabla          %||% "",
    studyid         = config$studyid         %||% "",
    age_grps        = config$age_grps        %||% c("1 yr", "35 yr", "65 yr"),
    ageunit         = config$ageunit         %||% "years",
    output_file     = config$output_file     %||% NULL,
    err_output_file = config$err_output_file %||% NULL,
    util_path       = config$util_path       %||% NULL,
    sl_datasets     = config$sl_datasets     %||% NULL,
    sl_group        = config$sl_group        %||% NULL,
    sl_subset       = config$sl_subset       %||% NULL
  )

  # Setup (SAS: %dm_setup)
  setup_result <- dm_setup(full_config$dm_data, full_config$ds_data, full_config)

  if (setup_result$setup_success) {

    # ------------------------------------------------------------------
    # Demographic tabulations (SAS: %do &di=1 %to &dm_var_cnt; %dm(&&dm_var_&di.))
    # ------------------------------------------------------------------
    dm_var_list <- c("age_flag", "country", "ethnic", "race",
                     "sex", "siteid", "country*siteid")
    dm_by_ds_var_list <- c("age_flag", "country", "ethnic", "race",
                           "sex", "siteid")

    dm_results <- purrr::set_names(
      purrr::map(dm_var_list, function(v) {
        dm_tabulate(
          dm          = setup_result$dm,
          var         = v,
          arm_count   = setup_result$arm_count,
          arm_names   = setup_result$arm_names,
          arm_counts  = setup_result$arm_counts,
          total_count = setup_result$total_count
        )
      }),
      stringr::str_replace_all(dm_var_list, "\\*", "_")
    )

    # ------------------------------------------------------------------
    # Demographics x Disposition (SAS: %do &di=1 %to &dm_by_ds_var_cnt;
    #                                   %dm_by_ds(&&dm_by_ds_var_&di.))
    # ------------------------------------------------------------------
    ds_results <- purrr::set_names(
      purrr::map(dm_by_ds_var_list, function(v) {
        dm_by_ds(
          ds_dm      = setup_result$ds_dm,
          dm         = setup_result$dm,
          var        = v,
          arm_count  = setup_result$arm_count,
          arm_counts = setup_result$arm_counts
        )
      }),
      dm_by_ds_var_list
    )

    # ------------------------------------------------------------------
    # Age descriptive statistics (SAS: %dm_stat(dm, age))
    # ------------------------------------------------------------------
    age_stats <- dm_stat(
      data      = setup_result$dm,
      var       = "age",
      arm_count = setup_result$arm_count,
      arm_names = setup_result$arm_names
    )

    # ------------------------------------------------------------------
    # Format for output (SAS: %dm_outfmt)
    # ------------------------------------------------------------------
    formatted <- dm_outfmt(
      results     = list(
        dm_results = dm_results,
        ds_results = ds_results,
        age_stats  = age_stats
      ),
      lkp_age     = setup_result$lkp_age,
      lkp_arm_out = setup_result$lkp_arm_out
    )

    # ------------------------------------------------------------------
    # Group / subset preprocessing (SAS: %group_subset_pp)
    # ------------------------------------------------------------------
    pp_result <- group_subset_pp(
      sl_group    = full_config$sl_group,
      sl_subset   = full_config$sl_subset,
      sl_datasets = full_config$sl_datasets
    )

    # ------------------------------------------------------------------
    # Excel output (SAS: %dm_out + %group_subset_xls_out)
    # ------------------------------------------------------------------
    if (!is.null(full_config$output_file)) {
      out_config <- list(
        ndabla    = full_config$ndabla,
        studyid   = full_config$studyid,
        arm_count = setup_result$arm_count,
        dm_actarm = setup_result$dm_actarm
      )
      dm_out(full_config$output_file, formatted, out_config)

      # Append group / subset worksheets to existing workbook
      # (SAS: %group_subset_xls_out appends to already-open JET connection)
      # NOTE: group_subset_write_xlsx() creates a NEW workbook, so we load
      #       the saved dm_out workbook and manually add group/subset sheets.
      wb_gs <- openxlsx::loadWorkbook(full_config$output_file)
      openxlsx::addWorksheet(wb_gs, "group_detail")
      if (nrow(pp_result$sl_out_group) > 0L) {
        openxlsx::writeData(wb_gs, "group_detail", pp_result$sl_out_group)
      }
      openxlsx::addWorksheet(wb_gs, "subset_detail")
      if (nrow(pp_result$sl_out_subset) > 0L) {
        openxlsx::writeData(wb_gs, "subset_detail", pp_result$sl_out_subset)
      }
      openxlsx::addWorksheet(wb_gs, "group_subset_info")
      gs_info <- tibble::tibble(
        val_desc = c("Grouped by", "Subset by", "Group row count",
                     "Subset row count", "Note"),
        val      = c(pp_result$sl_group_desc %||% "",
                     pp_result$sl_subset_desc %||% "",
                     as.character(nrow(pp_result$sl_out_group)),
                     as.character(nrow(pp_result$sl_out_subset)),
                     if (nrow(pp_result$sl_out_group) == 0L &&
                         nrow(pp_result$sl_out_subset) == 0L) {
                       "Neither grouping nor subsetting were used."
                     } else "")
      )
      openxlsx::writeData(wb_gs, "group_subset_info", gs_info)
      openxlsx::saveWorkbook(wb_gs, full_config$output_file, overwrite = TRUE)
    }

    cli::cli_alert_success("Demographics analysis completed successfully")
    invisible(formatted)

  } else {
    # ------------------------------------------------------------------
    # Error output (SAS: %error_summary)
    # ------------------------------------------------------------------
    if (!is.null(full_config$err_output_file)) {
      error_summary(
        err_file        = full_config$err_output_file,
        panel_title     = full_config$panel_title %||% "Demographics",
        ndabla          = full_config$ndabla      %||% "",
        studyid         = full_config$studyid     %||% "",
        err_nosubj      = !setup_result$dm_subj_gt0,
        err_missvar     = !setup_result$setup_req_var,
        rpt_chk_var_req = setup_result$rpt_chk_var_req,
        sl_subset       = full_config$sl_subset
      )
    }

    cli::cli_alert_danger("Demographics analysis failed — see error summary")
    invisible(NULL)
  }
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %params macro replaced by dm_params() function with named arguments;
#      all SAS global macro variables replaced by function return lists.
#    - SAS hash object lookups replaced by dplyr::left_join() (semantically
#      equivalent; hash key/data maps to join-by/select columns).
#    - Age bucketing: subject ages converted to years via DIVISION (e.g.
#      age / 12 for months), correcting a likely bug in SAS lines 522-530
#      where the source multiplied instead of dividing. The lkp_age bucket
#      boundaries (SAS lines 430-436) correctly divide, so the R code is
#      consistent with those boundaries.
#    - SAS propcase() approximated by stringr::str_to_title(); minor edge-case
#      differences possible for hyphenated words or all-caps abbreviations.
#    - Disposition deduplication: Protocol Milestone events plus "Informed
#      Consent Obtained" and "Randomized" are kept unconditionally; all other
#      disposition events keep only the LAST per subject/category/subcategory
#      (sorted by death-flag, dsseq/date, dsdecod). Final dedup removes
#      duplicate subject x dsdecod rows.
#    - Screen failures excluded via ARMCD codes (SCRNFAIL, NOTASSGN, NOTTRT)
#      when ARMCD is available; otherwise no exclusion is applied.
#    - Arm display name formatting: propcase for words > 3 characters with no
#      digits; MG/KG lowercased; ML becomes mL; only applied when the arm
#      name contains no lowercase letters.
#    - Cross-tabulation variables (e.g. country*siteid) handled by splitting
#      the variable string on "*" to create a multi-column grouping.
#    - All variable names lowercased upon data load (rename_with(tolower))
#      for case-insensitive matching with SAS.
#    - The SAS code uses options missing='' (line 80) to blank missing values
#      in output; in R this is handled at the display/output level, while
#      internal processing always uses NA.
#    - DS tables replace numeric NA with 0 (matching SAS lines 961-966:
#      "if num(i)=. then num(i)=0" in %dm_outfmt).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Quantile computation: R type=2 vs SAS PROC UNIVARIATE default may
#      produce small differences for even-sized samples.
#    - Mode calculation: R table() + min(modes) picks smallest mode when tied;
#      SAS PROC UNIVARIATE returns smallest mode — should match.
#    - Percentage rounding: janitor::round_half_up() used for SAS-compatible
#      rounding (0.5 rounds up, not to even).
#    - Sort stability: dplyr::arrange() is stable; SAS PROC SORT is stable
#      by key — results should be identical for well-defined sort orders.
#    - sd() in R uses n-1 denominator (matching SAS STD default in PROC
#      UNIVARIATE).
#    - PROC FREQ sparse option matched by tidyr::complete() with fill = 0L;
#      zero-count cells are explicitly filled, not left as NA.
#    - Subject age conversion: SAS source lines 522-530 multiply age by unit
#      factor (likely a bug); R divides. This may cause bucket assignment
#      differences for non-year age units.
#
# NO DIRECT R EQUIVALENT:
#    - SAS hash object -> dplyr::left_join() (semantically equivalent)
#    - SAS RETAIN -> not needed in dplyr (columns persist in pipeline)
#    - SAS CALL SYMPUTX -> function return values in named list
#    - SAS JET/PCFILES LIBNAME engine -> openxlsx::createWorkbook() +
#      addWorksheet() + writeData() + saveWorkbook()
#    - SAS options missing='' -> handle at display level in dm_outfmt()
#    - SAS %include -> source() with parameterized paths via config
#    - SAS PROC DATASETS rename -> dplyr::rename()
#    - SAS first./last. BY-group flags -> duplicated() or group_by + row_number
#    - SAS PROPCASE -> stringr::str_to_title() (similar but edge-case diffs)
#
# PACKAGE SELECTION RATIONALE:
#    - haven: SAS data I/O, read XPT/SAS7BDAT transport files (AAP mandated)
#    - dplyr: core data manipulation replacing DATA steps + PROC SQL (mandated)
#    - tidyr: pivoting/reshaping replacing PROC TRANSPOSE (mandated)
#    - forcats: factor level management replacing SAS FORMAT ordering
#    - openxlsx: Excel output replacing JET/PCFILES engine (AAP mandated)
#    - janitor: round_half_up() for SAS-compatible rounding (AAP mandated)
#    - stringr: string manipulation replacing SAS char functions (tidyverse)
#    - purrr: functional iteration replacing SAS macro %DO loops (tidyverse)
#    - cli: user-facing messages and structured error output
#    - rlang: tidy evaluation (syms, .data, %||%) for programmatic col refs
#    - tibble: enhanced data frames for clinical data (tidyverse mandate)
#    - stats: base R statistical functions (quantile, sd, median, findInterval)
#
# OPEN QUESTIONS:
#    - Quantile type parameter: confirm R type=2 matches SAS PROC UNIVARIATE
#      default for all sample sizes and distributions.
#    - Age unit conversion factors: verify week/day/hour divisors match SAS
#      (weeks = 52.178571428571428571428571428571, days = 365.25, hours = 8766).
#    - Disposition dedup sort order: confirm sort stability for multi-key sort
#      matches SAS behaviour in all edge cases.
#    - Excel template compatibility: SAS used Demographics_Template.xls;
#      confirm openxlsx workbook is compatible with downstream consumers.
#    - Script Launcher metadata: confirm sl_datasets/sl_group/sl_subset
#      scaffold structure matches SAS expectations.
#    - Mode for non-integer ages: SAS PROC UNIVARIATE MODE vs R table()
#      approach may differ for continuous variables.
#    - SAS lines 522-530 age conversion bug: stakeholder review needed to
#      confirm whether R should replicate the SAS bug for exact output
#      parity or use the correct division (as currently implemented).
# ============================================================
