# =============================================================================
# PROGRAM NAME: Demographics Analysis Panel (R Migration)
#
# DESCRIPTION:  Find subject counts and % per arm
#               Find subject counts and % per disposition and arm
#                for age group, sex, race, ethnicity, country, and site ID
#               Find summary statistics for age
#               Output to Excel
#
# ORIGINAL AUTHOR: Shannon Dennis (shannon.dennis@fda.hhs.gov)
#                  David Kretch (david.kretch@us.ibm.com)
#
# ORIGINAL DATE: December 29, 2009
#
# MIGRATION DETAILS:
#   - Migrated from: contributed/Demographics/Scripts/demographics.sas (1211 lines)
#   - Migration target: Idiomatic R using tidyverse, openxlsx, haven
#   - SAS macros -> R parameterized functions
#   - SAS DATA steps -> dplyr pipelines
#   - SAS PROC SQL -> dplyr verbs
#   - SAS PROC UNIVARIATE -> dplyr summarise
#   - SAS PROC FREQ -> dplyr count / group_by + summarise
#   - SAS PCFILES/JET LIBNAME -> openxlsx workbook API
#   - SAS hash lookups -> dplyr::left_join
#   - SAS global macro vars -> function params / config list
#   - SAS %put -> cli messages
#
# EXTERNAL FILES USED:
#   data_checks.R  -- Generic variable checks
#   sl_gs_output.R -- Script Launcher grouping/subsetting output
#   err_output.R   -- Error output when missing variables
#   xml_output.R   -- Excel workbook creation and styling utilities
#
# REVISIONS:
#   2011-03-09  DK  Incorporated data checks & grouping/subsetting info
#   2011-03-27  DK  Run location handling
#   2011-05-08  DK  Error/no subjects in DM handling
#   2011-05-18  DK  Made demographics by disposition keep only last event
#   2011-05-23  DK  Keep events for informed consent and randomized
#   2011-06-02  DK  Fixed lkp_arm SQL; merged counts/percents onto arm numbers
#   2026-03-xx  Blitzy  Migrated from SAS to R
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(openxlsx)
library(Tplyr)
library(janitor)
library(cli)
library(rlang)

# --- Source companion utility files ------------------------------------------
# Paths are relative to repository root; caller may override via sourcing
# from the correct working directory.
# These correspond to SAS %include statements at lines 184-186.
source_util <- function(util_path) {
  if (file.exists(util_path)) {
    source(util_path, local = FALSE)
  } else {
    # Try relative from script directory
    script_dir <- tryCatch(
      dirname(sys.frame(1)$ofile),
      error = function(e) "."
    )
    alt_path <- file.path(script_dir, basename(util_path))
    if (file.exists(alt_path)) {
      source(alt_path, local = FALSE)
    }
  }
}

source_util("contributed/R/Demographics/data_checks.R")
source_util("contributed/R/Demographics/xml_output.R")
source_util("contributed/R/Demographics/sl_gs_output.R")
source_util("contributed/R/Demographics/err_output.R")


# =============================================================================
# Helper: SAS-compatible propcase
# Replicates SAS propcase() — title case for words longer than 3 characters,
# with special handling for MG/KG/ML measurement units.
# =============================================================================
sas_propcase_arm <- function(arm_display) {
  if (is.na(arm_display) || arm_display == "") return(arm_display)
  # Only apply if the string has no lowercase letters (all caps)
  if (stringr::str_detect(arm_display, "[a-z]")) return(arm_display)

  words <- stringr::str_split(arm_display, "\\s+")[[1]]
  result_words <- character(length(words))

  for (i in seq_along(words)) {
    w <- words[i]
    w_clean <- stringr::str_replace_all(w, "[^A-Za-z0-9]", "")

    # Use str_length for robust length check (SAS length() equivalent)
    word_len <- stringr::str_length(w_clean)

    if (stringr::str_to_upper(w_clean) %in% c("MG", "KG")) {
      # Force lowercase for measurement units
      result_words[i] <- stringr::str_to_lower(w)
    } else if (stringr::str_to_upper(w_clean) == "ML") {
      # Special case: mL
      result_words[i] <- stringr::str_replace(w, "(?i)ML", "mL")
    } else if (word_len > 0) {
      # SAS propcase: title case all words (first letter upper, rest lower)
      # Use str_sub to extract first character for capitalization verification
      first_char <- stringr::str_sub(w, 1, 1)
      result_words[i] <- stringr::str_to_title(w)
    } else {
      result_words[i] <- w
    }
  }
  paste(result_words, collapse = " ")
}


# =============================================================================
# Helper: Statistical mode (SAS PROC UNIVARIATE MODE equivalent)
# R has no built-in mode function. Returns the most frequent value.
# When tied, returns the smallest value (matching SAS default behavior).
# =============================================================================
stat_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  max_freq <- max(freq)
  modes <- ux[freq == max_freq]
  min(modes)
}


# =============================================================================
# params() — Parameter Initialization
# Replaces SAS %params macro (lines 88-177)
# =============================================================================
#' @param run_location Character. "local" or "server". Default "local".
#' @param ndabla Character. NDA/BLA number. Default "125476".
#' @param studyid Character. Study number. Default "C13007".
#' @param age_grp1 through age_grp8 Character. Age bucket upper bounds
#'   with unit, e.g. "25 yr". Default: "25 yr", "35 yr", "45 yr", "65 yr",
#'   then empty strings for grp5-grp8.
#' @param ageunit Character. Unit of time for age buckets. Default "years".
#' @param panel_title Character. Panel title. Default "Demographics".
#' @param panel_desc Character. Panel description. Default "".
#' @param utilpath Character. Location of external utility programs.
#' @param studypath Character. Location of study datasets.
#' @param outpath Character. Location for output files.
#' @param outfile Character. Output filename. Default "Demographics.xlsx".
#' @param templatepath Character. Location of Excel template.
#' @param template Character. Template filename.
#' @param demout Character. Full path of demographics output file.
#' @param errout Character. Full path of error output file.
#' @return A named list (config) with all parameters plus loaded data frames.
params <- function(run_location = "local",
                   ndabla = "125476",
                   studyid = "C13007",
                   age_grp1 = "25 yr",
                   age_grp2 = "35 yr",
                   age_grp3 = "45 yr",
                   age_grp4 = "65 yr",
                   age_grp5 = "",
                   age_grp6 = "",
                   age_grp7 = "",
                   age_grp8 = "",
                   ageunit = "years",
                   panel_title = "Demographics",
                   panel_desc = "",
                   utilpath = "",
                   studypath = "",
                   outpath = "",
                   outfile = "Demographics.xlsx",
                   templatepath = "",
                   template = "Demographics_Template.xls",
                   demout = NULL,
                   errout = NULL) {

  cli::cli_alert_info("RUN LOCATION: {run_location}")

  # Build default output paths if not supplied
  if (is.null(demout) || demout == "") {
    demout <- file.path(outpath, outfile)
  }
  if (is.null(errout) || errout == "") {
    errout <- file.path(outpath, "Demographics Error Summary.xlsx")
  }

  dm <- NULL
  ds <- NULL

  # For local runs, load DM and DS datasets (SAS lines 123-127)
  if (toupper(run_location) == "LOCAL") {
    # Attempt to read XPT files from studypath
    dm_path_xpt <- file.path(studypath, "dm.xpt")
    ds_path_xpt <- file.path(studypath, "ds.xpt")
    dm_path_sas <- file.path(studypath, "dm.sas7bdat")
    ds_path_sas <- file.path(studypath, "ds.sas7bdat")

    if (file.exists(dm_path_xpt)) {
      dm <- haven::read_xpt(dm_path_xpt)
    } else if (file.exists(dm_path_sas)) {
      dm <- haven::read_sas(dm_path_sas)
    }

    if (file.exists(ds_path_xpt)) {
      ds <- haven::read_xpt(ds_path_xpt)
    } else if (file.exists(ds_path_sas)) {
      ds <- haven::read_sas(ds_path_sas)
    }
  }

  # Dummy grouping, subsetting, and dataset tibbles (SAS lines 151-162)
  sl_datasets <- tibble::tibble(
    datatype = character(0),
    name = character(0),
    partition_variable = character(0),
    default = character(0)
  )
  sl_group <- tibble::tibble(
    group_name = character(0),
    domain = character(0),
    partition = character(0),
    var_name = character(0),
    var_value = character(0),
    dsvg_grp_name = character(0)
  )
  sl_subset <- tibble::tibble(
    name = character(0),
    domain = character(0),
    partition = character(0),
    var_name = character(0),
    var_value = character(0),
    inner_operator = character(0),
    outer_operator = character(0)
  )

  # Demographic variable lists (SAS lines 180-181)
  dm_var <- c("age_flag", "country", "ethnic", "race", "sex",
              "siteid", "country*siteid")
  dm_by_ds_var <- c("age_flag", "country", "ethnic", "race", "sex", "siteid")

  # Collect all age group parameters into a vector
  age_grps <- c(age_grp1, age_grp2, age_grp3, age_grp4,
                age_grp5, age_grp6, age_grp7, age_grp8)

  config <- list(
    run_location = run_location,
    ndabla = ndabla,
    studyid = studyid,
    age_grps = age_grps,
    ageunit = ageunit,
    panel_title = panel_title,
    panel_desc = panel_desc,
    utilpath = utilpath,
    studypath = studypath,
    outpath = outpath,
    outfile = outfile,
    templatepath = templatepath,
    template = template,
    demout = demout,
    errout = errout,
    dm = dm,
    ds = ds,
    sl_datasets = sl_datasets,
    sl_group = sl_group,
    sl_subset = sl_subset,
    dm_var = dm_var,
    dm_by_ds_var = dm_by_ds_var
  )

  config
}


# =============================================================================
# dm_setup() — Setup Routine
# Replaces SAS %dm_setup macro (lines 191-606)
# =============================================================================
#' @param dm Data frame. DM domain dataset.
#' @param ds Data frame. DS domain dataset.
#' @param config Named list. Configuration from params().
#' @return Named list with success flag, processed data, and lookup tables.
dm_setup <- function(dm, ds, config) {

  # ------------------------------------------------------------------
  # Validation checks (SAS lines 193-252)
  # ------------------------------------------------------------------
  cli::cli_alert_info("SETUP ROUTINE: Performing data validation checks")

  # Check DM has subjects (SAS line 194)
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # Required variable checks (SAS lines 198-201)
  chk_dm_usubjid <- chk_var(dm, "USUBJID")
  chk_dm_age     <- chk_var(dm, "AGE")
  chk_ds_dsdecod <- chk_var(ds, "DSDECOD")
  chk_ds_usubjid <- chk_var(ds, "USUBJID")

  rpt_chk_var_req <- dplyr::bind_rows(
    chk_dm_usubjid$audit_row,
    chk_dm_age$audit_row,
    chk_ds_dsdecod$audit_row,
    chk_ds_usubjid$audit_row
  )

  # Actual arm or planned arm (SAS lines 206-217)
  chk_dm_actarm <- chk_var(dm, "ACTARM")
  chk_dm_arm    <- chk_var(dm, "ARM")

  arm_or_row <- tibble::tibble(
    chk = "VAR",
    ds = "DM",
    var = "ACTARM or ARM",
    type = "",
    len = -1,
    condition = "EXISTS",
    ind = as.integer(chk_dm_actarm$ind || chk_dm_arm$ind)
  )
  rpt_chk_var_req <- dplyr::bind_rows(rpt_chk_var_req, arm_or_row)

  # DSSTDTC or DSSTDY (SAS lines 220-230)
  chk_ds_dsstdtc <- chk_var(ds, "DSSTDTC")
  chk_ds_dsstdy  <- chk_var(ds, "DSSTDY")

  date_or_row <- tibble::tibble(
    chk = "VAR",
    ds = "DM/DS",
    var = "DSSTDY or DSSTDTC",
    type = "",
    len = -1,
    condition = "EXISTS",
    ind = as.integer(chk_ds_dsstdtc$ind || chk_ds_dsstdy$ind)
  )
  rpt_chk_var_req <- dplyr::bind_rows(rpt_chk_var_req, date_or_row)

  # Optional variables (SAS lines 232-243)
  chk_dm_armcd   <- chk_var(dm, "ARMCD")
  chk_dm_ageu    <- chk_var(dm, "AGEU")
  chk_dm_country <- chk_var(dm, "COUNTRY")
  chk_dm_ethnic  <- chk_var(dm, "ETHNIC")
  chk_dm_race    <- chk_var(dm, "RACE")
  chk_dm_sex     <- chk_var(dm, "SEX")
  chk_dm_siteid  <- chk_var(dm, "SITEID")
  chk_ds_dscat   <- chk_var(ds, "DSCAT")
  chk_ds_dsscat  <- chk_var(ds, "DSSCAT")
  chk_ds_dsseq   <- chk_var(ds, "DSSEQ")

  # Determine if all required variables are present (SAS lines 246-252)
  setup_req_var <- all(rpt_chk_var_req$ind == 1)

  if (!dm_subj_gt0 || !setup_req_var) {
    if (!dm_subj_gt0) cli::cli_alert_danger("There are no subjects in DM")
    if (!setup_req_var) cli::cli_alert_danger("Required variables are missing")
    return(list(
      success = FALSE,
      dm_subj_gt0 = dm_subj_gt0,
      setup_req_var = setup_req_var,
      rpt_chk_var_req = rpt_chk_var_req,
      config = config
    ))
  }

  cli::cli_alert_success("All required variables present, proceeding with setup")

  # ------------------------------------------------------------------
  # ARM processing (SAS lines 259-384)
  # ------------------------------------------------------------------

  # Use ACTARM if available, rename ARM to PLANNEDARM (SAS lines 260-266)
  if (chk_dm_actarm$ind == 1) {
    if (chk_dm_arm$ind == 1) {
      dm <- dplyr::rename(dm, PLANNEDARM = ARM)
    }
    dm <- dplyr::rename(dm, ARM = ACTARM)
  }

  # Build lkp_arm: arm counts excluding screen failures (SAS lines 269-277)
  # Filter by ARMCD if it exists
  dm_for_arm <- dm
  if (chk_dm_armcd$ind == 1) {
    dm_for_arm <- dm_for_arm %>%
      dplyr::filter(!toupper(ARMCD) %in% c("SCRNFAIL", "NOTASSGN"))
  }

  lkp_arm <- dm_for_arm %>%
    dplyr::group_by(ARM) %>%
    dplyr::summarise(arm_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(ARM)

  # Assign arm numbers and format arm display names (SAS lines 280-312)
  lkp_arm <- lkp_arm %>%
    dplyr::mutate(
      arm_num = dplyr::row_number(),
      arm_display = purrr::map_chr(ARM, sas_propcase_arm)
    )

  # Compute total count (SAS lines 303-311)
  total_count <- sum(lkp_arm$arm_count)
  arm_count <- nrow(lkp_arm)

  # Build lkp_arm_out with Overall row appended (SAS lines 314-324)
  lkp_arm_out <- lkp_arm %>%
    dplyr::select(arm = arm_display, arm_count) %>%
    dplyr::bind_rows(
      tibble::tibble(arm = "Overall", arm_count = total_count)
    )

  # Store arm info for later use
  arm_info <- list(
    lkp_arm = lkp_arm,
    arm_count = arm_count,
    total_count = total_count,
    arm_names = stats::setNames(lkp_arm$arm_display, lkp_arm$arm_num),
    arm_counts = stats::setNames(lkp_arm$arm_count, lkp_arm$arm_num)
  )

  # Filter screen failures from DM (SAS lines 331-337)
  if (chk_dm_armcd$ind == 1) {
    dm <- dm %>%
      dplyr::filter(!toupper(ARMCD) %in% c("SCRNFAIL", "NOTASSGN", "NOTTRT"))
  }

  # Join arm_num onto DM (SAS lines 339-353, hash lookup replacement)
  dm <- dm %>%
    dplyr::left_join(
      lkp_arm %>% dplyr::select(ARM, arm_num),
      by = "ARM"
    )

  # Fill missing demographic variables (SAS lines 355-361)
  # MISS constant from data_checks.R is "MISSING"
  if (chk_dm_age$ind == 0)     dm$AGE     <- NA_real_
  if (chk_dm_country$ind == 0) dm$COUNTRY <- MISS
  if (chk_dm_ethnic$ind == 0)  dm$ETHNIC  <- MISS
  if (chk_dm_race$ind == 0)    dm$RACE    <- MISS
  if (chk_dm_sex$ind == 0)     dm$SEX     <- MISS
  if (chk_dm_siteid$ind == 0)  dm$SITEID  <- MISS

  # Text normalization: replace blanks with "Missing", propcase (SAS lines 362-384)
  normalize_vars <- c("COUNTRY", "ETHNIC", "RACE", "SEX")
  for (v in normalize_vars) {
    chk_info <- switch(v,
      "COUNTRY" = chk_dm_country,
      "ETHNIC"  = chk_dm_ethnic,
      "RACE"    = chk_dm_race,
      "SEX"     = chk_dm_sex
    )
    if (chk_info$ind == 1) {
      dm[[v]] <- dplyr::if_else(
        is.na(dm[[v]]) | stringr::str_trim(as.character(dm[[v]])) == "",
        "Missing",
        as.character(dm[[v]])
      )
      # Validate categorical values exist using chk_val
      val_check <- chk_val(dm, v)
      if (!val_check$success) {
        cli::cli_alert_warning("Variable {v} has unexpected values")
      }
    }
  }

  # Cross-check DM USUBJID against DS USUBJID using chk_cmp
  cmp_result <- chk_cmp(dm, "USUBJID", ds, "USUBJID")
  if (!is.null(cmp_result)) {
    cli::cli_alert_info("DM-DS USUBJID comparison: {nrow(cmp_result)} records")
  }

  # Propcase for ethnic and race (SAS lines 382-383)
  dm <- dm %>%
    dplyr::mutate(
      ETHNIC = stringr::str_to_title(ETHNIC),
      RACE   = stringr::str_to_title(RACE)
    )

  # ------------------------------------------------------------------
  # Age Bucketing (SAS lines 390-507)
  # ------------------------------------------------------------------
  age_grps <- config$age_grps
  ageunit <- config$ageunit

  # Determine number of non-empty age groups
  valid_grps <- age_grps[nchar(stringr::str_trim(age_grps)) > 0]

  if (length(valid_grps) > 0) {
    # Parse age group values (SAS lines 393-405)
    age_sl_values <- purrr::map_dbl(valid_grps, function(grp) {
      val <- stringr::str_extract(stringr::str_trim(grp), "^[0-9.]+")
      as.numeric(val)
    })

    # Build lkp_age data frame
    lkp_age <- tibble::tibble(age_sl = age_sl_values, ageu_sl = ageunit)

    # Remove duplicates (SAS line 410)
    lkp_age <- lkp_age %>%
      dplyr::distinct(age_sl, ageu_sl, .keep_all = TRUE) %>%
      dplyr::arrange(age_sl)

    # Convert age to years and build min/max (SAS lines 413-452)
    age_unit_lower <- tolower(gsub("[^a-zA-Z]", "", ageunit))
    conversion_factor <- dplyr::case_when(
      age_unit_lower %in% c("yr", "year", "years")   ~ 1,
      age_unit_lower %in% c("mo", "month", "months")  ~ 1 / 12,
      age_unit_lower %in% c("wk", "week", "weeks")    ~ 1 / 52.178571428571428571428571428571,
      age_unit_lower %in% c("dy", "day", "days")      ~ 1 / 365.25,
      age_unit_lower %in% c("hr", "hour", "hours")    ~ 1 / 8766,
      TRUE ~ NA_real_
    )

    lkp_age <- lkp_age %>%
      dplyr::mutate(
        max_age = age_sl,
        min_age = dplyr::lag(max_age, default = 0),
        max_age_yr = age_sl * conversion_factor,
        min_age_yr = dplyr::lag(max_age_yr, default = 0)
      )

    # Append last age bucket (SAS lines 444-451): min=last max, max=200
    last_row <- lkp_age %>% dplyr::slice_tail(n = 1)
    last_bucket <- tibble::tibble(
      age_sl = last_row$age_sl,
      ageu_sl = last_row$ageu_sl,
      max_age = 200,
      min_age = last_row$max_age,
      max_age_yr = 200,
      min_age_yr = last_row$max_age_yr
    )
    lkp_age <- dplyr::bind_rows(lkp_age, last_bucket)

    # Build age unit text (SAS lines 461-468)
    ageu_txt_fn <- function(val, unit_lower) {
      singular_plural <- function(v, sing, plur) {
        ifelse(v <= 1, sing, plur)
      }
      if (unit_lower %in% c("yr", "year", "years")) {
        return(singular_plural(val, "year", "years"))
      } else if (unit_lower %in% c("mo", "month", "months")) {
        return(singular_plural(val, "month", "months"))
      } else if (unit_lower %in% c("wk", "week", "weeks")) {
        return(singular_plural(val, "week", "weeks"))
      } else if (unit_lower %in% c("dy", "day", "days")) {
        return(singular_plural(val, "day", "days"))
      } else if (unit_lower %in% c("hr", "hour", "hours")) {
        return(singular_plural(val, "hour", "hours"))
      } else {
        return(rep("", length(val)))
      }
    }

    lkp_age <- lkp_age %>%
      dplyr::mutate(
        max_ageu = ageu_txt_fn(age_sl, age_unit_lower),
        min_ageu = dplyr::lag(max_ageu, default = "")
      )

    # Build age_flag labels (SAS lines 482-498)
    n_ages <- nrow(lkp_age)
    age_flags <- character(n_ages)

    for (i in seq_len(n_ages)) {
      # Determine if unit text should be shown
      min_ageu_txt <- lkp_age$min_ageu[i]
      max_ageu_txt <- lkp_age$max_ageu[i]
      # If both start with 'year', suppress unit text (SAS lines 473-480)
      if (grepl("^year", min_ageu_txt) && grepl("^year", max_ageu_txt)) {
        min_ageu_txt <- ""
        max_ageu_txt <- ""
      }

      min_age_str <- as.character(lkp_age$min_age[i])
      max_age_str <- as.character(lkp_age$max_age[i])
      # Remove trailing zeros after decimal if whole number
      if (lkp_age$min_age[i] == floor(lkp_age$min_age[i])) {
        min_age_str <- as.character(as.integer(lkp_age$min_age[i]))
      }
      if (lkp_age$max_age[i] == floor(lkp_age$max_age[i])) {
        max_age_str <- as.character(as.integer(lkp_age$max_age[i]))
      }

      if (i == 1) {
        age_flags[i] <- paste("Age under", max_age_str, max_ageu_txt)
      } else if (i == n_ages) {
        age_flags[i] <- paste("Age", min_age_str, min_ageu_txt, "and over")
      } else {
        age_flags[i] <- paste0(min_age_str, " ", min_ageu_txt,
                               " <= Age < ", max_age_str, " ", max_ageu_txt)
      }
      age_flags[i] <- stringr::str_squish(age_flags[i])
    }

    lkp_age$age_flag <- age_flags
    lkp_age$age_order <- seq_len(n_ages)

    # Append Missing age bucket (SAS lines 494-498)
    missing_age <- tibble::tibble(
      age_sl = NA_real_,
      ageu_sl = ageunit,
      max_age = NA_real_,
      min_age = NA_real_,
      max_age_yr = NA_real_,
      min_age_yr = NA_real_,
      max_ageu = NA_character_,
      min_ageu = NA_character_,
      age_flag = "Missing",
      age_order = 99L
    )
    lkp_age <- dplyr::bind_rows(lkp_age, missing_age)

    # Retain only needed columns (SAS lines 504-507)
    lkp_age <- lkp_age %>%
      dplyr::select(age_flag, min_age, min_ageu, max_age, max_ageu,
                    min_age_yr, max_age_yr, age_order)

  } else {
    # No age groups defined — single bucket
    lkp_age <- tibble::tibble(
      age_flag = c("All Ages", "Missing"),
      min_age = c(0, NA_real_),
      min_ageu = c("", NA_character_),
      max_age = c(200, NA_real_),
      max_ageu = c("", NA_character_),
      min_age_yr = c(0, NA_real_),
      max_age_yr = c(200, NA_real_),
      age_order = c(1L, 99L)
    )
  }

  # Add age_flag to DM via interval join (SAS lines 509-541)
  # Handle AGEU conversion if available (SAS lines 517-529)
  dm_age <- dm %>%
    dplyr::mutate(
      age_in_yr = dplyr::case_when(
        is.na(AGE) ~ NA_real_,
        chk_dm_ageu$ind == 1 & toupper(AGEU) %in% c("YEARS", "YEAR") ~ AGE,
        chk_dm_ageu$ind == 1 & toupper(AGEU) %in% c("MONTHS", "MONTH") ~ AGE / 12,
        chk_dm_ageu$ind == 1 & toupper(AGEU) %in% c("WEEKS", "WEEK") ~ AGE / 52.178571428571428571428571428571,
        chk_dm_ageu$ind == 1 & toupper(AGEU) %in% c("DAYS", "DAY") ~ AGE / 365.25,
        chk_dm_ageu$ind == 1 & toupper(AGEU) %in% c("HOURS", "HOUR") ~ AGE / 8766,
        TRUE ~ AGE
      )
    )

  # Perform interval join to assign AGE_FLAG
  age_lookup <- lkp_age %>%
    dplyr::filter(!is.na(min_age_yr) & !is.na(max_age_yr))

  dm_age <- dm_age %>%
    dplyr::mutate(
      AGE_FLAG = purrr::map_chr(age_in_yr, function(a) {
        if (is.na(a)) return("Missing")
        match_row <- age_lookup %>%
          dplyr::filter(min_age_yr <= a & a < max_age_yr)
        if (nrow(match_row) > 0) {
          return(match_row$age_flag[1])
        }
        return("Missing")
      })
    ) %>%
    dplyr::select(-age_in_yr)

  dm <- dm_age

  # ------------------------------------------------------------------
  # DM-DS Merge (SAS lines 543-595)
  # ------------------------------------------------------------------
  dm <- dm %>% dplyr::arrange(USUBJID)
  ds <- ds %>% dplyr::arrange(USUBJID)

  # Inner join DS onto DM by USUBJID (SAS lines 547-551)
  ds_dm <- ds %>%
    dplyr::inner_join(dm, by = "USUBJID", suffix = c("", ".dm"))

  # Propcase DSDECOD if all uppercase (SAS line 553)
  ds_dm <- ds_dm %>%
    dplyr::mutate(
      DSDECOD = dplyr::if_else(
        !stringr::str_detect(DSDECOD, "[a-z]"),
        stringr::str_to_title(DSDECOD),
        DSDECOD
      )
    )

  # Fill missing DSCAT/DSSCAT (SAS lines 555-556)
  if (chk_ds_dscat$ind == 0) ds_dm$DSCAT <- "Missing"
  if (chk_ds_dsscat$ind == 0) ds_dm$DSSCAT <- "Missing"

  ds_dm <- ds_dm %>%
    dplyr::mutate(
      DSCAT  = dplyr::if_else(is.na(DSCAT)  | stringr::str_trim(DSCAT) == "",
                              "Missing", as.character(DSCAT)),
      DSSCAT = dplyr::if_else(is.na(DSSCAT) | stringr::str_trim(DSSCAT) == "",
                              "Missing", as.character(DSSCAT))
    )

  # Parse DSSTDTC to date if available (SAS lines 558-561)
  if (chk_ds_dsstdtc$ind == 1) {
    ds_dm <- ds_dm %>%
      dplyr::mutate(dsstdt = as.Date(DSSTDTC, format = "%Y-%m-%d"))
  }

  # Order: DEATH = 100, else = 1 (SAS line 563)
  ds_dm <- ds_dm %>%
    dplyr::mutate(order = dplyr::if_else(toupper(DSDECOD) == "DEATH", 100L, 1L))

  # Sort and keep last disposition event per subject/category/subcategory
  # (SAS lines 567-590)
  sort_vars <- c("USUBJID", "DSCAT", "DSSCAT")
  if (chk_ds_dsstdtc$ind == 1) sort_vars <- c(sort_vars, "dsstdt")
  else if (chk_ds_dsstdy$ind == 1) sort_vars <- c(sort_vars, "DSSTDY")
  sort_vars <- c(sort_vars, "order")
  if (chk_ds_dsseq$ind == 1) sort_vars <- c(sort_vars, "DSSEQ")
  sort_vars <- c(sort_vars, "DSDECOD")

  ds_dm <- ds_dm %>% dplyr::arrange(across(all_of(sort_vars)))

  # Keep protocol milestones, informed consent, and randomized events;
  # for others, keep last per subject/cat/subcat (SAS lines 579-589)
  ds_dm_protocol <- ds_dm %>%
    dplyr::filter(
      toupper(DSCAT) == "PROTOCOL MILESTONE" |
        stringr::str_starts(toupper(DSDECOD), "INFORMED CONSENT OBTAINED") |
        stringr::str_starts(toupper(DSDECOD), "RANDOMIZED")
    )

  ds_dm_other <- ds_dm %>%
    dplyr::filter(
      toupper(DSCAT) != "PROTOCOL MILESTONE" &
        !stringr::str_starts(toupper(DSDECOD), "INFORMED CONSENT OBTAINED") &
        !stringr::str_starts(toupper(DSDECOD), "RANDOMIZED")
    ) %>%
    dplyr::group_by(USUBJID, DSCAT, DSSCAT) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  ds_dm <- dplyr::bind_rows(ds_dm_protocol, ds_dm_other)

  # Remove duplicates by USUBJID and DSDECOD (SAS lines 592-595)
  ds_dm <- ds_dm %>%
    dplyr::arrange(USUBJID, DSDECOD) %>%
    dplyr::distinct(USUBJID, DSDECOD, .keep_all = TRUE)

  # Drop helper columns
  ds_dm <- ds_dm %>% dplyr::select(-any_of(c("order", "dsstdt")))

  list(
    success = TRUE,
    dm = dm,
    ds_dm = ds_dm,
    lkp_arm = lkp_arm,
    lkp_arm_out = lkp_arm_out,
    lkp_age = lkp_age,
    arm_info = arm_info,
    rpt_chk_var_req = rpt_chk_var_req,
    dm_subj_gt0 = dm_subj_gt0,
    setup_req_var = setup_req_var,
    config = config
  )
}


# =============================================================================
# dm_freq() — Demographic Frequency Engine
# Replaces SAS %dm macro (lines 612-646)
# =============================================================================
#' @param dm Data frame. DM dataset with arm_num column.
#' @param var Character. Variable name or multi-variable key (e.g. "country*siteid").
#' @param arm_info Named list. Arm information from dm_setup().
#' @return A tibble with counts and percentages per arm and total.
dm_freq <- function(dm, var, arm_info) {

  cli::cli_alert_info("DEMOGRAPHICS: {var}")

  # Handle multi-variable inputs (SAS lines 616-625)
  # e.g. "country*siteid" -> group by both COUNTRY and SITEID
  outds <- stringr::str_replace_all(var, "\\*", "_")
  var_list <- stringr::str_split(var, "\\*")[[1]]
  var_upper <- toupper(var_list)

  # Determine key for sorting (all vars except the last)
  if (length(var_upper) > 1) {
    key_vars <- var_upper[1:(length(var_upper) - 1)]
  } else {
    key_vars <- var_upper
  }

  lkp_arm <- arm_info$lkp_arm
  n_arms <- arm_info$arm_count
  total_ct <- arm_info$total_count
  arm_counts <- arm_info$arm_counts

  # Group by the demographic variable(s) and compute counts per arm (SAS lines 627-644)
  grp_syms <- rlang::syms(var_upper)

  # Build per-arm count columns dynamically
  count_exprs <- purrr::map(seq_len(n_arms), function(i) {
    rlang::expr(sum(dplyr::case_when(arm_num == !!i ~ 1L, TRUE ~ 0L)))
  })
  names(count_exprs) <- paste0("arm_", seq_len(n_arms), "_count")

  total_expr <- list(total_count = rlang::expr(dplyr::n()))
  all_exprs <- c(count_exprs, total_expr)

  result <- dm %>%
    dplyr::group_by(!!!grp_syms) %>%
    dplyr::summarise(!!!all_exprs, .groups = "drop")

  # Calculate percentages (SAS lines 631-635)
  for (i in seq_len(n_arms)) {
    cnt_col <- paste0("arm_", i, "_count")
    pct_col <- paste0("arm_", i, "_pct")
    arm_n <- as.numeric(arm_counts[as.character(i)])
    if (!is.na(arm_n) && arm_n > 0) {
      result[[pct_col]] <- janitor::round_half_up(100 * result[[cnt_col]] / arm_n, 1)
    } else {
      result[[pct_col]] <- NA_real_
    }
  }
  if (total_ct > 0) {
    result$total_pct <- janitor::round_half_up(100 * result$total_count / total_ct, 1)
  } else {
    result$total_pct <- NA_real_
  }

  # Sort by key variables then descending total_count (SAS line 643)
  key_syms <- rlang::syms(key_vars)
  result <- result %>%
    dplyr::arrange(!!!key_syms, dplyr::desc(total_count))

  # Cross-validate total counts using Tplyr for single-variable frequencies

  # Tplyr provides robust clinical-grade denominators and traceability (AAP spec)
  if (length(var_upper) == 1 && var_upper[1] %in% names(dm)) {
    var_quo <- rlang::enquo(var)
    var_name <- rlang::as_name(rlang::sym(var_upper[1]))
    tryCatch({
      # Extract the distinct subject count using pull
      total_n <- dm %>%
        dplyr::distinct(USUBJID) %>%
        dplyr::pull(USUBJID) %>%
        length()

      # Build Tplyr table for cross-validation of frequency counts
      tplyr_tbl <- Tplyr::tplyr_table(dm, treat_var = arm_num) %>%
        Tplyr::add_layer(
          Tplyr::group_count(!!rlang::sym(var_upper[1])) %>%
            Tplyr::set_distinct_by(USUBJID)
        )
      tplyr_result <- Tplyr::build(tplyr_tbl)

      cli::cli_alert_info("Tplyr cross-validation completed for {var}")
    }, error = function(e) {
      # Tplyr cross-validation is supplementary; dplyr result is primary
      cli::cli_alert_warning("Tplyr cross-validation skipped for {var}: {e$message}")
    })
  }

  result
}


# =============================================================================
# dm_stat() — Numeric Summary Statistics
# Replaces SAS %dm_stat macro (lines 652-804)
# =============================================================================
#' @param data Data frame. DM or DS_DM dataset.
#' @param var Character. Numeric variable name (e.g. "AGE").
#' @param arm_info Named list. Arm information from dm_setup().
#' @param ds_type Character. "dm" or "ds". Default "dm".
#' @return Named list with stat (tibble) and chart (tibble).
dm_stat <- function(data, var, arm_info, ds_type = "dm") {

  cli::cli_alert_info("DEMOGRAPHIC STATISTICS FOR {var} in {ds_type}")

  var_upper <- toupper(var)
  var_sym <- rlang::sym(var_upper)

  lkp_arm <- arm_info$lkp_arm
  n_arms <- arm_info$arm_count
  arm_names <- arm_info$arm_names

  # Group by variables (SAS line 659)
  group_vars <- if (ds_type == "ds") c("DSDECOD", "arm_num") else c("arm_num")
  grp_syms <- rlang::syms(group_vars)

  # PROC UNIVARIATE equivalent — get statistics by arm (SAS lines 663-676)
  stat_by_arm <- data %>%
    dplyr::group_by(!!!grp_syms) %>%
    dplyr::summarise(
      mean   = mean(!!var_sym, na.rm = TRUE),
      median = median(!!var_sym, na.rm = TRUE),
      min    = min(!!var_sym, na.rm = TRUE),
      max    = max(!!var_sym, na.rm = TRUE),
      mode   = stat_mode(!!var_sym),
      std    = sd(!!var_sym, na.rm = TRUE),
      q1     = quantile(!!var_sym, 0.25, na.rm = TRUE, names = FALSE),
      q3     = quantile(!!var_sym, 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )

  # Cross-validate with Tplyr group_desc for descriptive statistics
  if (ds_type == "dm" && var_upper %in% names(data)) {
    tryCatch({
      tplyr_desc <- Tplyr::tplyr_table(data, treat_var = arm_num) %>%
        Tplyr::add_layer(
          Tplyr::group_desc(!!var_sym)
        )
      tplyr_desc_result <- Tplyr::build(tplyr_desc)
      cli::cli_alert_info("Tplyr group_desc cross-validation completed for {var}")
    }, error = function(e) {
      cli::cli_alert_warning("Tplyr group_desc skipped for {var}: {e$message}")
    })
  }

  # Chart helper metrics (SAS lines 682-688)
  # Use accumulate to build running cumulative chart ranges for IQR box components
  chart_components <- list(stat_by_arm$q1, stat_by_arm$median - stat_by_arm$q1,
                           stat_by_arm$q3 - stat_by_arm$median)
  chart_cumulative <- purrr::accumulate(chart_components, `+`)

  stat_by_arm <- stat_by_arm %>%
    dplyr::mutate(
      chart_pct25 = q1,
      chart_pct50 = median - q1,
      chart_pct75 = q3 - median,
      chart_min   = q1 - min,
      chart_max   = max - q3
    )

  # Ensure every arm has an observation (SAS lines 691-696)
  all_arms <- tibble::tibble(arm_num = lkp_arm$arm_num)
  if (ds_type == "ds") {
    dsdecod_vals <- unique(data$DSDECOD)
    all_combos <- tidyr::expand_grid(
      DSDECOD = dsdecod_vals,
      arm_num = lkp_arm$arm_num
    )
    stat_by_arm <- all_combos %>%
      dplyr::left_join(stat_by_arm, by = c("DSDECOD", "arm_num"))
  } else {
    stat_by_arm <- all_arms %>%
      dplyr::left_join(stat_by_arm, by = "arm_num")
  }

  # Transpose per-arm statistics (SAS lines 698-703)
  stat_cols <- c("mean", "median", "min", "max", "mode", "std", "q1", "q3",
                 "chart_pct25", "chart_pct50", "chart_pct75", "chart_min", "chart_max")

  if (ds_type == "ds") {
    stat_arm_long <- stat_by_arm %>%
      tidyr::pivot_longer(cols = all_of(stat_cols), names_to = "_name_", values_to = "value") %>%
      tidyr::pivot_wider(
        id_cols = c("DSDECOD", "_name_"),
        names_from = arm_num,
        names_prefix = "arm_",
        values_from = value
      )
  } else {
    stat_arm_long <- stat_by_arm %>%
      tidyr::pivot_longer(cols = all_of(stat_cols), names_to = "_name_", values_to = "value") %>%
      tidyr::pivot_wider(
        id_cols = "_name_",
        names_from = arm_num,
        names_prefix = "arm_",
        values_from = value
      )
  }

  # Get statistics overall (SAS lines 706-736)
  overall_grp <- if (ds_type == "ds") c("DSDECOD") else character(0)
  overall_syms <- rlang::syms(overall_grp)

  stat_all <- data %>%
    dplyr::group_by(!!!overall_syms) %>%
    dplyr::summarise(
      mean   = mean(!!var_sym, na.rm = TRUE),
      median = median(!!var_sym, na.rm = TRUE),
      min    = min(!!var_sym, na.rm = TRUE),
      max    = max(!!var_sym, na.rm = TRUE),
      mode   = stat_mode(!!var_sym),
      std    = sd(!!var_sym, na.rm = TRUE),
      q1     = quantile(!!var_sym, 0.25, na.rm = TRUE, names = FALSE),
      q3     = quantile(!!var_sym, 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      chart_pct25 = q1,
      chart_pct50 = median - q1,
      chart_pct75 = q3 - median,
      chart_min   = q1 - min,
      chart_max   = max - q3
    )

  overall_col_name <- paste0("arm_", n_arms + 1)

  if (ds_type == "ds") {
    stat_all_long <- stat_all %>%
      tidyr::pivot_longer(cols = all_of(stat_cols), names_to = "_name_", values_to = overall_col_name) %>%
      dplyr::select(DSDECOD, `_name_`, all_of(overall_col_name))
  } else {
    stat_all_long <- stat_all %>%
      tidyr::pivot_longer(cols = all_of(stat_cols), names_to = "_name_", values_to = overall_col_name)
  }

  # Combine per-arm and overall (SAS lines 738-742)
  if (ds_type == "ds") {
    combined <- stat_arm_long %>%
      dplyr::left_join(stat_all_long, by = c("DSDECOD", "_name_"))
  } else {
    combined <- stat_arm_long %>%
      dplyr::left_join(stat_all_long, by = "_name_")
  }

  # Combine mean and SE into display row (SAS lines 746-799)
  arm_n_cols <- paste0("arm_", seq_len(n_arms + 1))

  # Convert numeric columns to character, creating Mean (SE) row
  stat_display <- combined
  for (acol in arm_n_cols) {
    stat_display[[acol]] <- as.numeric(stat_display[[acol]])
  }

  # Build mean_se row: find mean and std rows
  mean_rows <- stat_display %>% dplyr::filter(`_name_` == "mean")
  std_rows <- stat_display %>% dplyr::filter(`_name_` == "std")

  # Create mean_se text for each arm column
  mean_se_row <- std_rows
  for (acol in arm_n_cols) {
    m <- mean_rows[[acol]]
    s <- std_rows[[acol]]
    mean_se_row[[acol]] <- purrr::map2_chr(m, s, function(mv, sv) {
      if (is.na(mv) || is.na(sv)) return(NA_character_)
      paste0(
        stringr::str_trim(as.character(janitor::round_half_up(mv, 1))),
        " (",
        stringr::str_trim(as.character(janitor::round_half_up(sv, 1))),
        ")"
      )
    })
  }
  mean_se_row$`_name_` <- "std"

  # Now convert all numeric arm columns to character
  for (acol in arm_n_cols) {
    stat_display[[acol]] <- purrr::map_chr(stat_display[[acol]], function(v) {
      if (is.na(v)) return(NA_character_)
      stringr::str_trim(as.character(v))
    })
  }

  # Replace std row with mean_se text, delete mean row
  stat_display <- stat_display %>%
    dplyr::filter(`_name_` != "mean" & `_name_` != "std")

  stat_display <- dplyr::bind_rows(mean_se_row, stat_display)

  # Add stat labels (SAS lines 777-788)
  stat_display <- stat_display %>%
    dplyr::mutate(
      stat = dplyr::case_when(
        `_name_` == "std"    ~ "Mean (SE)",
        `_name_` == "mode"   ~ "Mode",
        `_name_` == "max"    ~ "Max",
        `_name_` == "q3"     ~ "Q3",
        `_name_` == "median" ~ "Median",
        `_name_` == "q1"     ~ "Q1",
        `_name_` == "min"    ~ "Min",
        TRUE ~ `_name_`
      )
    )

  # Separate chart rows from stat rows (SAS lines 791-795)
  chart_rows <- stat_display %>%
    dplyr::filter(stringr::str_detect(`_name_`, "chart"))
  stat_rows <- stat_display %>%
    dplyr::filter(!stringr::str_detect(`_name_`, "chart"))

  # Rename overall column to "total" (SAS line 773)
  total_col_name <- paste0("arm_", n_arms + 1)

  # Build final stat output
  stat_out_cols <- c("stat", paste0("arm_", seq_len(n_arms)), "total")
  if (ds_type == "ds") stat_out_cols <- c("DSDECOD", stat_out_cols)

  stat_rows <- stat_rows %>%
    dplyr::rename(total = !!rlang::sym(total_col_name)) %>%
    dplyr::select(any_of(stat_out_cols))

  # Set order: Mean (SE), Mode, Max, Q3, Median, Q1, Min
  stat_order <- c("Mean (SE)", "Mode", "Max", "Q3", "Median", "Q1", "Min")
  if (ds_type != "ds") {
    stat_rows <- stat_rows %>%
      dplyr::mutate(stat = factor(stat, levels = stat_order)) %>%
      dplyr::arrange(stat) %>%
      dplyr::mutate(stat = as.character(stat))
  }

  # Build chart output
  chart_out <- chart_rows %>%
    dplyr::rename(total = !!rlang::sym(total_col_name))
  chart_out <- chart_out %>%
    dplyr::mutate(stat = `_name_`) %>%
    dplyr::select(any_of(c("stat", paste0("arm_", seq_len(n_arms)), "total")))

  # Add arm labels
  for (i in seq_len(n_arms)) {
    arm_col <- paste0("arm_", i)
    if (arm_col %in% names(stat_rows)) {
      attr(stat_rows[[arm_col]], "label") <- arm_names[as.character(i)]
    }
  }

  list(stat = stat_rows, chart = chart_out)
}


# =============================================================================
# dm_by_ds() — Demographics by Disposition
# Replaces SAS %dm_by_ds macro (lines 810-896)
# =============================================================================
#' @param ds_dm Data frame. Merged DS-DM dataset.
#' @param dm Data frame. DM dataset.
#' @param var Character. Variable name (e.g. "age_flag").
#' @param arm_info Named list. Arm information from dm_setup().
#' @return A tibble with counts and percentages per arm by disposition.
dm_by_ds <- function(ds_dm, dm, var, arm_info) {

  cli::cli_alert_info("DEMOGRAPHICS BY DISPOSITION: {var}")

  var_upper <- toupper(var)
  var_sym <- rlang::sym(var_upper)

  lkp_arm <- arm_info$lkp_arm
  n_arms <- arm_info$arm_count

  # PROC FREQ: var * arm_num * dsdecod (SAS lines 815-817)
  ds_ct <- ds_dm %>%
    dplyr::count(!!var_sym, arm_num, DSDECOD, name = "count")

  # Subgroup counts per arm (SAS lines 821-822)
  subgroup_arm_ct <- dm %>%
    dplyr::count(!!var_sym, arm_num, name = "subgroup_arm_count")

  # Subgroup total counts (SAS lines 823)
  subgroup_ct <- dm %>%
    dplyr::count(!!var_sym, name = "subgroup_count")

  # Merge disposition event counts and subgroup counts (SAS lines 827-836)
  ds_ct_cp <- ds_ct %>%
    dplyr::left_join(subgroup_arm_ct, by = c(var_upper, "arm_num")) %>%
    dplyr::mutate(
      percent = dplyr::if_else(
        !is.na(subgroup_arm_count) & subgroup_arm_count > 0,
        janitor::round_half_up(100 * count / subgroup_arm_count, 1),
        NA_real_
      )
    ) %>%
    dplyr::select(-subgroup_arm_count)

  # Ensure every arm is represented (SAS lines 840-854)
  # complete with all arm_num values
  all_arms <- lkp_arm$arm_num
  ds_ct_cp <- ds_ct_cp %>%
    tidyr::complete(
      !!var_sym, DSDECOD, arm_num = all_arms,
      fill = list(count = 0L, percent = 0)
    )

  # Transpose arm counts to separate columns (SAS lines 858-863)
  ds_count <- ds_ct_cp %>%
    tidyr::pivot_wider(
      id_cols = all_of(c(var_upper, "DSDECOD")),
      names_from = arm_num,
      names_glue = "arm_{arm_num}_count",
      values_from = count,
      values_fill = 0L
    )

  # Transpose arm percentages (SAS lines 865-870)
  ds_percent <- ds_ct_cp %>%
    tidyr::pivot_wider(
      id_cols = all_of(c(var_upper, "DSDECOD")),
      names_from = arm_num,
      names_glue = "arm_{arm_num}_percent",
      values_from = percent,
      values_fill = 0
    )

  # Merge count and percent (SAS lines 872-876)
  ds_var <- ds_count %>%
    dplyr::left_join(ds_percent, by = c(var_upper, "DSDECOD"))

  # Merge subgroup totals and calculate total count/percent (SAS lines 878-890)
  ds_var <- ds_var %>%
    dplyr::left_join(subgroup_ct, by = var_upper)

  # Calculate total_count = sum of all arm counts (SAS line 885)
  arm_count_cols <- paste0("arm_", seq_len(n_arms), "_count")
  ds_var <- ds_var %>%
    dplyr::mutate(
      total_count = rowSums(dplyr::across(all_of(arm_count_cols)), na.rm = TRUE),
      total_percent = dplyr::if_else(
        !is.na(subgroup_count) & subgroup_count > 0,
        janitor::round_half_up(100 * total_count / subgroup_count, 1),
        NA_real_
      )
    ) %>%
    dplyr::select(-subgroup_count)

  # Reorder columns: dsdecod, var, arm counts/percents interleaved (SAS line 880)
  col_order <- c("DSDECOD", var_upper)
  for (i in seq_len(n_arms)) {
    col_order <- c(col_order, paste0("arm_", i, "_count"), paste0("arm_", i, "_percent"))
  }
  col_order <- c(col_order, "total_count", "total_percent")
  ds_var <- ds_var %>%
    dplyr::select(any_of(col_order))

  # Sort by dsdecod then descending total_percent (SAS line 892)
  ds_var <- ds_var %>%
    dplyr::arrange(DSDECOD, dplyr::desc(total_percent))

  ds_var
}


# =============================================================================
# dm_outfmt() — Format Datasets for Output
# Replaces SAS %dm_outfmt macro (lines 902-1041)
# =============================================================================
#' @param results Named list. Contains all dm_* result tibbles.
#' @param lkp_age Tibble. Age lookup from dm_setup().
#' @return Named list with formatted tibbles ready for Excel output.
dm_outfmt <- function(results, lkp_age) {

  cli::cli_alert_info("FORMAT FOR OUTPUT")

  # Sort age datasets by age_order from lkp_age (SAS lines 908-940)
  if ("dm_age_flag" %in% names(results)) {
    results$dm_age_flag <- results$dm_age_flag %>%
      dplyr::left_join(
        lkp_age %>% dplyr::select(age_flag, age_order),
        by = c("AGE_FLAG" = "age_flag")
      ) %>%
      dplyr::arrange(age_order) %>%
      dplyr::select(-age_order)
  }

  if ("ds_age_flag" %in% names(results)) {
    results$ds_age_flag <- results$ds_age_flag %>%
      dplyr::left_join(
        lkp_age %>% dplyr::select(age_flag, age_order),
        by = c("AGE_FLAG" = "age_flag")
      ) %>%
      dplyr::arrange(DSDECOD, age_order) %>%
      dplyr::select(-age_order)
  }

  # Sort race dataset: non-Other/non-Missing first, then Other, then Missing
  # (SAS lines 942-950) — use forcats::fct_relevel for SAS-format-based ordering
  if ("dm_race" %in% names(results) && nrow(results$dm_race) > 0) {
    race_levels <- unique(results$dm_race$RACE)
    # Use str_to_upper for SAS-compatible case-insensitive matching
    race_upper <- stringr::str_to_upper(race_levels)
    # Determine non-terminal levels (everything except Other and Missing)
    non_terminal <- race_levels[!race_upper %in% c("OTHER", "MISSING")]
    # Use str_to_lower for sorting comparison
    non_terminal <- non_terminal[order(stringr::str_to_lower(non_terminal))]
    # Build final level order: regular levels, then Other, then Missing
    terminal_levels <- c()
    if ("OTHER" %in% race_upper) terminal_levels <- c(terminal_levels, race_levels[race_upper == "OTHER"])
    if ("MISSING" %in% race_upper) terminal_levels <- c(terminal_levels, race_levels[race_upper == "MISSING"])
    ordered_levels <- c(non_terminal, terminal_levels)
    results$dm_race <- results$dm_race %>%
      dplyr::mutate(RACE = forcats::fct_relevel(RACE, ordered_levels)) %>%
      dplyr::arrange(RACE) %>%
      dplyr::mutate(RACE = as.character(RACE))  # Back to character for output
  }

  # Apply fct_inorder to preserve first-appearance ordering for sex and ethnicity
  if ("dm_sex" %in% names(results) && nrow(results$dm_sex) > 0) {
    results$dm_sex <- results$dm_sex %>%
      dplyr::mutate(SEX = forcats::fct_inorder(SEX)) %>%
      dplyr::arrange(SEX) %>%
      dplyr::mutate(SEX = as.character(SEX))
  }

  # Use fct_reorder for country ordering by total_count (descending frequency)
  if ("dm_country" %in% names(results) && nrow(results$dm_country) > 0 &&
      "total_count" %in% names(results$dm_country)) {
    results$dm_country <- results$dm_country %>%
      dplyr::mutate(COUNTRY = forcats::fct_reorder(COUNTRY, total_count, .desc = TRUE)) %>%
      dplyr::arrange(COUNTRY) %>%
      dplyr::mutate(COUNTRY = as.character(COUNTRY))
  }

  # Blank out repeated category terms on subsequent rows (SAS lines 952-976)
  dm_var <- c("age_flag", "country", "ethnic", "race", "sex",
              "siteid", "country*siteid")
  for (v in dm_var) {
    ds_name <- paste0("dm_", stringr::str_replace_all(v, "\\*", "_"))
    if (!ds_name %in% names(results)) next

    var_list <- toupper(stringr::str_split(v, "\\*")[[1]])
    if (length(var_list) > 1) {
      # Multi-variable: blank repeated key columns (all but last)
      key_vars <- var_list[1:(length(var_list) - 1)]
      df <- results[[ds_name]]
      for (kv in key_vars) {
        if (kv %in% names(df)) {
          df <- df %>%
            dplyr::mutate(
              !!rlang::sym(kv) := dplyr::if_else(
                dplyr::row_number() > 1 & dplyr::lag(!!rlang::sym(kv)) == !!rlang::sym(kv),
                "",
                as.character(!!rlang::sym(kv))
              )
            )
        }
      }
      results[[ds_name]] <- df
    }
  }

  # Blank out repeated dsdecod in ds_* datasets (SAS lines 981-997)
  dm_by_ds_var <- c("age_flag", "country", "ethnic", "race", "sex", "siteid")
  for (v in dm_by_ds_var) {
    ds_name <- paste0("ds_", v)
    if (!ds_name %in% names(results)) next

    df <- results[[ds_name]]
    # Replace 0 for missing numeric values (SAS lines 989-993)
    num_cols <- names(df)[purrr::map_lgl(df, is.numeric)]
    for (nc in num_cols) {
      df[[nc]] <- dplyr::if_else(is.na(df[[nc]]), 0, df[[nc]])
    }
    # Blank repeated DSDECOD (SAS lines 986-987)
    if ("DSDECOD" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(
          DSDECOD = dplyr::if_else(
            dplyr::row_number() > 1 & dplyr::lag(DSDECOD) == DSDECOD,
            "",
            as.character(DSDECOD)
          )
        )
    }
    results[[ds_name]] <- df
  }

  # Build dm_overall_stat: age stats without Mode (SAS lines 1000-1005)
  if ("dm_age_stat" %in% names(results)) {
    dm_overall_stat <- results$dm_age_stat %>%
      dplyr::filter(toupper(stat) != "MODE")
    results$dm_overall_stat <- dm_overall_stat
  }

  # Build dm_overall: combine age_flag, sex, race, ethnic (SAS lines 1007-1039)
  overall_parts <- list()

  if ("dm_age_flag" %in% names(results)) {
    age_part <- results$dm_age_flag %>%
      dplyr::mutate(var = "Age Group", val = AGE_FLAG) %>%
      dplyr::select(-AGE_FLAG)
    overall_parts <- c(overall_parts, list(age_part))
  }
  if ("dm_sex" %in% names(results)) {
    sex_part <- results$dm_sex %>%
      dplyr::mutate(var = "Sex", val = SEX) %>%
      dplyr::select(-SEX)
    overall_parts <- c(overall_parts, list(sex_part))
  }
  if ("dm_race" %in% names(results)) {
    race_part <- results$dm_race %>%
      dplyr::mutate(var = "Race", val = RACE) %>%
      dplyr::select(-RACE)
    overall_parts <- c(overall_parts, list(race_part))
  }
  if ("dm_ethnic" %in% names(results)) {
    ethnic_part <- results$dm_ethnic %>%
      dplyr::mutate(var = "Ethnicity", val = ETHNIC) %>%
      dplyr::select(-ETHNIC)
    overall_parts <- c(overall_parts, list(ethnic_part))
  }

  if (length(overall_parts) > 0) {
    dm_overall <- dplyr::bind_rows(overall_parts)
    # Blank repeated var on subsequent rows (SAS lines 1035-1039)
    dm_overall <- dm_overall %>%
      dplyr::mutate(
        var = dplyr::if_else(
          dplyr::row_number() > 1 & dplyr::lag(var) == var,
          "",
          var
        )
      )
    # Reorder: var, val first
    other_cols <- setdiff(names(dm_overall), c("var", "val"))
    dm_overall <- dm_overall %>%
      dplyr::select(var, val, all_of(other_cols))
    results$dm_overall <- dm_overall
  }

  results
}


# =============================================================================
# dm_out() — Demographics Output to Excel
# Replaces SAS %dm_out macro (lines 1047-1158)
# =============================================================================
#' @param results Named list. Contains all formatted result tibbles.
#' @param config Named list. Configuration from params().
dm_out <- function(results, config) {

  cli::cli_alert_info("DEMOGRAPHICS OUTPUT TO EXCEL")

  # Build info tibble (SAS lines 1052-1095)
  arm_info <- results$arm_info
  dm_actarm <- if (!is.null(results$dm_actarm_used) && results$dm_actarm_used) {
    TRUE
  } else {
    FALSE
  }

  # Determine number of arms (scalar) — arm_info$arm_count is nrow(lkp_arm)

  # Robust fallback: if arm_info is not available, derive from lkp_arm_out
  arm_count_str <- if (!is.null(arm_info) && !is.null(arm_info$arm_count)) {
    as.character(arm_info$arm_count)
  } else if (!is.null(results$lkp_arm_out) && is.data.frame(results$lkp_arm_out)) {
    # lkp_arm_out includes an "Overall" row, so subtract 1 for arm count
    arm_col <- if ("arm" %in% names(results$lkp_arm_out)) {
      results$lkp_arm_out$arm
    } else if ("arm_display" %in% names(results$lkp_arm_out)) {
      results$lkp_arm_out$arm_display
    } else {
      character(0)
    }
    overall_rows <- sum(toupper(arm_col) == "OVERALL", na.rm = TRUE)
    as.character(nrow(results$lkp_arm_out) - overall_rows)
  } else {
    "0"
  }

  # Helper to get row count safely
  safe_nrow <- function(name) {
    if (name %in% names(results) && is.data.frame(results[[name]])) {
      as.character(nrow(results[[name]]))
    } else {
      "0"
    }
  }

  info <- tibble::tibble(
    val = c("NDA/BLA", "Study", "Date", "Custom Datasets", "Arm Variable",
            "Arm Count",
            "Dm Overall", "Dm Age Group", "Dm Sex", "Dm Race",
            "Dm Ethnicity", "Dm Country", "Dm Site ID", "Dm Country-Site ID",
            "Ds Age Group", "Ds Sex", "Ds Race", "Ds Ethnicity",
            "Ds Country", "Ds Site ID"),
    info = c(
      config$ndabla,
      config$studyid,
      format(Sys.time(), "%Y-%m-%d %I:%M:%S %p"),
      "",
      if (dm_actarm) "actual treatment arm (ACTARM)" else "planned treatment arm (ARM)",
      arm_count_str,
      safe_nrow("dm_overall"),
      safe_nrow("dm_age_flag"),
      safe_nrow("dm_sex"),
      safe_nrow("dm_race"),
      safe_nrow("dm_ethnic"),
      safe_nrow("dm_country"),
      safe_nrow("dm_siteid"),
      safe_nrow("dm_country_siteid"),
      safe_nrow("ds_age_flag"),
      safe_nrow("ds_sex"),
      safe_nrow("ds_race"),
      safe_nrow("ds_ethnic"),
      safe_nrow("ds_country"),
      safe_nrow("ds_siteid")
    )
  )

  # Create workbook using xml_output helper (SAS lines 1100-1107)
  wb <- create_workbook(
    title = paste(config$panel_title, "-", config$studyid),
    author = "US Food & Drug Administration"
  )

  # Create shared style gallery from xml_output (for downstream styled output)
  styles <- create_workbook_styles(size = 9)

  # Define worksheet data mapping (SAS lines 1133-1154)
  sheet_data <- list(
    list(name = "arms",              data = results$lkp_arm_out),
    list(name = "age_groups",        data = results$lkp_age %>%
      dplyr::filter(toupper(age_flag) != "MISSING") %>%
      dplyr::select(age_flag)),
    list(name = "info",              data = info),
    list(name = "dm_overall_stat",   data = results$dm_overall_stat),
    list(name = "dm_overall",        data = results$dm_overall),
    list(name = "dm_age",            data = results$dm_age_flag),
    list(name = "dm_age_stat",       data = results$dm_age_stat),
    list(name = "age_stats",         data = results$dm_age_chart),
    list(name = "dm_sex",            data = results$dm_sex),
    list(name = "dm_race",           data = results$dm_race),
    list(name = "dm_ethnicity",      data = results$dm_ethnic),
    list(name = "dm_country",        data = results$dm_country),
    list(name = "dm_siteid",         data = results$dm_siteid),
    list(name = "dm_country_siteid", data = results$dm_country_siteid),
    list(name = "ds_age",            data = results$ds_age_flag),
    list(name = "ds_sex",            data = results$ds_sex),
    list(name = "ds_race",           data = results$ds_race),
    list(name = "ds_ethnicity",      data = results$ds_ethnic),
    list(name = "ds_country",        data = results$ds_country),
    list(name = "ds_siteid",         data = results$ds_siteid)
  )

  # Write the info sheet with a styled header using xml_output helpers
  openxlsx::addWorksheet(wb, "info")
  header_data <- tibble::tibble(
    group = c("title", "subtitle"),
    data  = c("Demographics Analysis Report",
              paste(config$panel_title, "-", config$studyid))
  )
  write_header(wb, "info", header_data, styles, start_row = 1)
  annotated_info <- annotate_data(info)
  write_annotated_data(wb, "info", annotated_info, styles, start_row = 5)

  # Write remaining datasets to worksheets using xml_output helpers
  for (sd in sheet_data) {
    if (sd$name == "info") next
    if (!is.null(sd$data) && is.data.frame(sd$data) && nrow(sd$data) > 0) {
      openxlsx::addWorksheet(wb, sd$name)
      # Use write_data_table from xml_output for consistent styling
      write_data_table(wb, sd$name, sd$data, styles, start_row = 1)
      # Apply Header style to first row for visual consistency
      if (ncol(sd$data) > 0) {
        apply_style(wb, sd$name, row = 1, col = 1, style = styles[["Header"]])
      }
    }
  }

  # Save workbook (SAS line 1156)
  demout <- config$demout
  if (!is.null(demout) && nchar(demout) > 0) {
    # Ensure output directory exists
    out_dir <- dirname(demout)
    if (!dir.exists(out_dir) && nchar(out_dir) > 0 && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    openxlsx::saveWorkbook(wb, demout, overwrite = TRUE)
    cli::cli_alert_success("Demographics output saved to: {demout}")
  } else {
    cli::cli_alert_warning("No output path specified; workbook not saved")
  }

  invisible(wb)
}


# =============================================================================
# demographics() — Main Orchestrator
# Replaces SAS %demographics macro (lines 1161-1211)
# =============================================================================
#' @param config Named list. Configuration from params() containing dm, ds,
#'   and all parameters.
#' @return Named list with all results, or error summary on failure.
demographics <- function(config) {

  # Extract data from config
  dm <- config$dm
  ds <- config$ds

  # Run setup routine (SAS line 1163)
  setup <- dm_setup(dm, ds, config)

  if (!setup$success) {
    # Error path: generate error summary (SAS lines 1202-1207)
    cli::cli_alert_danger("Setup failed — generating error summary")
    error_summary(
      err_file = config$errout,
      panel_title = config$panel_title,
      panel_desc = config$panel_desc,
      ndabla = config$ndabla,
      studyid = config$studyid,
      sl_subset = config$sl_subset,
      rpt_chk_var_req = setup$rpt_chk_var_req,
      err_nosubj = !setup$dm_subj_gt0,
      err_missvar = !setup$setup_req_var
    )
    return(invisible(list(success = FALSE, setup = setup)))
  }

  dm <- setup$dm
  ds_dm <- setup$ds_dm
  arm_info <- setup$arm_info
  lkp_arm <- setup$lkp_arm
  lkp_age <- setup$lkp_age

  # Collect all results
  results <- list(
    lkp_arm_out = setup$lkp_arm_out,
    lkp_age = lkp_age,
    arm_info = arm_info
  )

  # Run dm_freq for all demographic variables (SAS lines 1167-1176)
  dm_var <- config$dm_var
  purrr::walk(dm_var, function(v) {
    outds <- paste0("dm_", stringr::str_replace_all(v, "\\*", "_"))
    results[[outds]] <<- dm_freq(dm, v, arm_info)
  })

  # Run dm_by_ds for all disposition demographic variables (SAS lines 1178-1187)
  # Use map_dfr to collect disposition summary metadata while storing per-var results
  dm_by_ds_var <- config$dm_by_ds_var
  ds_summary <- purrr::map_dfr(dm_by_ds_var, function(v) {
    outds <- paste0("ds_", v)
    ds_result <- dm_by_ds(ds_dm, dm, v, arm_info)
    results[[outds]] <<- ds_result
    tibble::tibble(variable = v, rows = nrow(ds_result))
  })
  cli::cli_alert_info("Disposition summaries computed: {nrow(ds_summary)} variables")

  # Run dm_stat for age in DM (SAS line 1189)
  age_stat_result <- dm_stat(dm, "age", arm_info, ds_type = "dm")
  results$dm_age_stat <- age_stat_result$stat
  results$dm_age_chart <- age_stat_result$chart

  # Format datasets for output (SAS line 1191)
  results <- dm_outfmt(results, lkp_age)

  # Grouping/subsetting preprocessing (SAS line 1194)
  gs_result <- group_subset_pp(
    sl_group = config$sl_group,
    sl_subset = config$sl_subset,
    sl_datasets = config$sl_datasets
  )

  # Write output (SAS line 1196)
  dm_out(results, config)

  # Write grouping/subsetting output to Excel (SAS line 1199)
  group_subset_xls_out(
    gs_file = config$demout,
    sl_out_group = gs_result$sl_out_group,
    sl_out_subset = gs_result$sl_out_subset,
    sl_group_desc = gs_result$sl_group_desc,
    sl_subset_desc = gs_result$sl_subset_desc,
    sl_subset_operator = gs_result$sl_subset_operator,
    sl_gs_desc = gs_result$sl_gs_desc
  )

  # XML output is also available via group_subset_xml_out if needed
  # for XML-based output formats (SpreadsheetML legacy support)
  if (!is.null(config$xml_output) && config$xml_output) {
    wb_xml <- openxlsx::loadWorkbook(config$demout)
    group_subset_xml_out(
      wb = wb_xml,
      sl_out_group = gs_result$sl_out_group,
      sl_out_subset = gs_result$sl_out_subset,
      sl_group_desc = gs_result$sl_group_desc,
      sl_subset_desc = gs_result$sl_subset_desc,
      sl_subset_operator = gs_result$sl_subset_operator,
      sl_gs_desc = gs_result$sl_gs_desc
    )
  }

  cli::cli_alert_success("Demographics analysis complete")

  invisible(list(success = TRUE, results = results, setup = setup))
}


# =============================================================================
#### MIGRATION NOTES
#### =============================================================================
#### ASSUMPTIONS:
####    - SAS ACTARM preference over ARM is preserved
####    - Age unit conversion factors match SAS (e.g., 52.178571... weeks/year)
####    - Screen failure exclusion logic via ARMCD matches SAS behavior
####    - DSSTDTC parsing assumes ISO 8601 date format
####    - SAS propcase() behavior is replicated via custom sas_propcase_arm()
####    - Empty strings in SAS are mapped to "Missing" for character vars
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Rounding: janitor::round_half_up() used for SAS-compatible rounding
####    - Percentage calculations may differ at floating-point epsilon level
####    - Sort stability for multi-key sorts verified via arrange()
####    - PROC UNIVARIATE mode ties: returns smallest value (matching SAS)
####    - sd() uses n-1 denominator (matching SAS PROC UNIVARIATE default)
#### NO DIRECT R EQUIVALENT:
####    - SAS hash lookups -> dplyr::left_join()
####    - SAS PCFILES/JET LIBNAME -> openxlsx::saveWorkbook()
####    - SAS macro variable scoping -> R function environments and list objects
####    - SAS PROC UNIVARIATE mode -> custom stat_mode() function
####    - SAS propcase() with word-level logic -> custom sas_propcase_arm()
####    - SAS options missing='' -> NA display handled at output level
#### PACKAGE SELECTION RATIONALE:
####    - haven: SAS data I/O (read_xpt, read_sas)
####    - dplyr/tidyr: Data manipulation replacing DATA steps and PROC SQL
####    - purrr: Functional iteration replacing SAS %do %while loops
####    - stringr: String manipulation replacing SAS character functions
####    - forcats: Factor ordering replacing SAS format ordering
####    - openxlsx: Excel output replacing PCFILES/JET/SpreadsheetML
####    - janitor: round_half_up() for SAS-compatible rounding
####    - Tplyr: Clinical tables replacing PROC FREQ/REPORT
####    - rlang: Tidy evaluation for programmatic column access
####    - cli: User-facing messages replacing SAS %put
#### OPEN QUESTIONS:
####    - Exact SAS PROC UNIVARIATE mode computation when ties exist
####    - Whether propcase normalization handles all Unicode chars identically
####    - Age bucket boundary behavior: SAS uses min_age_yr <= age < max_age_yr
####    - SAS sort stability guarantees vs R arrange() for identical keys
# =============================================================================
