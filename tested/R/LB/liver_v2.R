# =============================================================================
# PROGRAM NAME: liver_v2.R
#
# Liver Lab Analysis Panel — R Migration
# Migrated from: tested/SAS/LB/liver_v2.sas (GCR r135)
# Original author: Shannon Dennis, FDA/IBM, 12/29/2009
# Migration: PhUSE WG5 SAS-to-R Migration, 2026
#
# Purpose:
#   Provides an overview of the number and percent of subjects with predefined
#   abnormalities in lab test values for Alanine Aminotransferase (ALT), Aspartate
#   Aminotransferase (AST), Total Bilirubin (TB), and Alkaline Phosphatase (ALP).
#   Reports frequency of lab values at or above upper limit of normal (ULN) at
#   multiple thresholds. Compares max lab values between tests, evaluates
#   baseline vs maximum, and plots max lab values by study day. Implements
#   Hy's Law screening criteria with at-any-visit and during-study analyses.
#
# Analyses:
#   1. Lab value ULN multiple frequency tables (>=2x, >=3x, >=5x, >=10x, >=20x
#      for ALT/AST/ALP; >=1.5x, >=2x, >=3x for BILI; plus Normal/>=HI/Missing
#      for ALP)
#   2. DILI pattern tables — 9 combinations of ALT/AST thresholds x BILI
#      thresholds x ALP status (at-any-visit AND during-study)
#   3. Baseline vs. maximum lab value cross-tabulations (5 bins)
#   4. AST vs BILI and ALT vs BILI maximum value comparisons (Hy's Law scatter)
#   5. Maximum lab values by study day
#
# Required Datasets: LB (Laboratory), DM (Demographics)
# Required LB Variables: USUBJID, LBTESTCD, LBSTRESN, LBSTRESU, LBSTNRHI, LBSTNRLO
# Optional LB Variables: LBBLFL, LBDY, LBDTC, VISITNUM, LBTEST, LBSTAT, LBREASND
# Required DM Variables: USUBJID, ARM (or ACTARM)
# Optional DM Variables: ARMCD, RFSTDTC, RFENDTC
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(purrr)
library(openxlsx)
library(janitor)
library(cli)
library(lubridate)
library(tibble)


# =============================================================================
# Constants — Acceptable LBTESTCD codes for each liver analyte
# Mirrors SAS: %let l_alt = 'ALT','SGPT'; etc. (lines 173-184)
# =============================================================================
LIVER_CODES <- list(
  ALT  = c("ALT", "SGPT"),
  AST  = c("AST", "SGOT"),
  ALP  = c("ALP", "ALKP"),
  BILI = c("BILI", "BILI_TB", "TB", "TBL", "TBIL", "TBILI", "TB_BILI", "BILITOT")
)

ALL_LIVER_CODES <- toupper(unlist(LIVER_CODES, use.names = FALSE))


# =============================================================================
# liver_params — Configuration function
# =============================================================================
#' Migrate SAS %params macro (lines 97-168). Returns a named config list.
#'
#' @param data_path       Character. Path to XPT data directory.
#' @param output_path     Character. Path for output workbooks.
#' @param r_macros_path   Character. Path to R macro functions.
#' @param r_utilities_path Character. Path to R utility functions.
#' @param nda_number      Character. NDA/BLA number.
#' @param study_id        Character. Study identifier.
#' @param run_location    Character. "LOCAL" or "LAUNCHER".
#' @param log_path        Character or NULL. Path for log file output.
#'
#' @return Named list config object.
liver_params <- function(data_path        = ".",
                         output_path      = ".",
                         r_macros_path    = "tested/R/macros",
                         r_utilities_path = "tested/R/utilities",
                         nda_number       = "",
                         study_id         = "",
                         run_location     = "LOCAL",
                         log_path         = NULL) {

  # Build config list — mirrors SAS %let assignments

  config <- list(
    data_path        = data_path,
    output_path      = output_path,
    r_macros_path    = r_macros_path,
    r_utilities_path = r_utilities_path,
    nda_number       = nda_number,
    study_id         = study_id,
    run_location     = toupper(run_location),
    log_path         = log_path,
    sl_group_by      = tibble::tibble(),
    sl_subset_by     = tibble::tibble(),
    panel_title      = "Liver Lab Analysis Panel",
    panel_desc       = paste0(
      "Overview of the number and percent of subjects with predefined ",
      "abnormalities in lab test values for ALT, AST, Total Bilirubin, and ALP."
    )
  )

  # Redirect console output to log file if specified (mirrors SAS PROC PRINTTO)
  if (!is.null(log_path) && nzchar(log_path)) {
    log_dir <- dirname(log_path)
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
    sink(log_path, split = TRUE)
  }

  config
}


# =============================================================================
# liver_prelim — Preliminary data checks
# =============================================================================
#' Migrate SAS %liver_prelim (lines 194-265). Validates DM and LB before
#' processing. Returns a list with ok flag and error details.
#'
#' @param dm     Data frame of DM domain.
#' @param lb     Data frame of LB domain.
#' @param config Named list from liver_params().
#'
#' @return Named list: ok (logical), errors (tibble), dm, lb, flags.
liver_prelim <- function(dm, lb, config) {

  rpt_chk_var <- tibble::tibble()
  rpt_chk_val <- tibble::tibble()

  # --- 1. Check DM has subjects ---
  dm_subj_gt0 <- chk_dm_subj_gt0(dm)

  # --- 2. Check required DM variables ---
  # ACTARM with ARM fallback (SAS: %chk_var(ds=dm, var=ACTARM, cond_ds=dm,
  # cond_var=ARM, ...))
  dm_actarm <- FALSE
  dm_arm    <- FALSE
  if ("ACTARM" %in% names(dm)) {
    dm_actarm <- TRUE
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_var(dm, "ACTARM", ds_name = "DM"))
  } else if ("ARM" %in% names(dm)) {
    dm_arm <- TRUE
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_var(dm, "ARM", ds_name = "DM"))
  }
  arm_available <- dm_actarm || dm_arm

  # USUBJID (required)
  chk_usubjid <- chk_var(dm, "USUBJID", ds_name = "DM")
  rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_usubjid)
  dm_usubjid  <- chk_usubjid$ind == 1L

  # --- 3. Check required LB variables ---
  lb_required <- c("USUBJID", "LBTESTCD", "LBSTNRHI", "LBSTRESN", "LBSTRESU", "LBSTNRLO")
  all_req_var <- dm_usubjid && arm_available

  for (vname in lb_required) {
    chk_result <- chk_var(lb, vname, ds_name = "LB")
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_result)
    if (chk_result$ind != 1L) {
      all_req_var <- FALSE
    }
  }

  # --- 4. Check optional LB variables (Info severity) ---
  lb_optional <- c("ARMCD", "LBBLFL", "LBTEST", "VISITNUM", "LBSTAT", "LBREASND")
  opt_flags <- list()
  for (vname in lb_optional) {
    chk_result <- chk_var(lb, vname, ds_name = "LB")
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_result)
    opt_flags[[tolower(vname)]] <- (chk_result$ind == 1L)
  }

  # --- 5. Special LBDY / LBDTC+RFSTDTC conditional check ---
  lbdy_available <- "LBDY" %in% names(lb)
  if (!lbdy_available) {
    lbdtc_avail  <- "LBDTC" %in% names(lb)
    rfstdtc_avail <- "RFSTDTC" %in% names(dm)
    lbdy_available <- lbdtc_avail && rfstdtc_avail
    chk_result <- chk_var(lb, "LBDY", ds_name = "LB")
    rpt_chk_var <- dplyr::bind_rows(rpt_chk_var, chk_result)
  }

  # --- 6. liver_lbtestcd check ---
  lbtestcd_result <- liver_lbtestcd(
    lb,
    alt_codes  = LIVER_CODES$ALT,
    ast_codes  = LIVER_CODES$AST,
    alp_codes  = LIVER_CODES$ALP,
    bili_codes = LIVER_CODES$BILI
  )
  liver_lbtestcd_ok <- lbtestcd_result$liver_lbtestcd

  # Overall prelim pass
  ok <- dm_subj_gt0 && all_req_var && liver_lbtestcd_ok

  if (!ok) {
    cli::cli_alert_danger("Preliminary data checks FAILED.")
  } else {
    cli::cli_alert_success("Preliminary data checks PASSED.")
  }

  list(
    ok                 = ok,
    dm_subj_gt0        = dm_subj_gt0,
    all_req_var        = all_req_var,
    liver_lbtestcd     = liver_lbtestcd_ok,
    err_liver_lbtest   = lbtestcd_result$err_liver_lbtest,
    rpt_chk_var        = rpt_chk_var,
    dm                 = dm,
    lb                 = lb,
    dm_actarm          = dm_actarm,
    opt_flags          = opt_flags
  )
}


# =============================================================================
# find_null_col — Drop all-missing columns
# =============================================================================
#' Migrate SAS %find_null_col (lines 351-446). Removes columns that are
#' entirely NA from a data frame.
#'
#' @param df A data frame.
#' @return Data frame with all-NA columns removed.
find_null_col <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return(df)
  non_null <- vapply(df, function(col) !all(is.na(col)), logical(1L))
  df[, non_null, drop = FALSE]
}


# =============================================================================
# liver_setup — Data preparation
# =============================================================================
#' Migrate SAS %liver_setup (lines 271-666). Normalizes ARM, filters LB to
#' liver analytes, merges with DM, derives missing variables, computes max
#' and baseline labs.
#'
#' @param dm         Data frame of DM domain.
#' @param lb         Data frame of LB domain.
#' @param config     Named list from liver_params().
#' @param dm_actarm  Logical. TRUE if ACTARM present.
#' @param opt_flags  Named list of optional variable availability flags.
#'
#' @return Named list of analytic datasets.
liver_setup <- function(dm, lb, config, dm_actarm = TRUE, opt_flags = list()) {

  cli::cli_alert_info("Liver setup: ARM normalization and data preparation.")

  # -------------------------------------------------------------------------
  # Sub-step a: ARM normalization (SAS lines 273-331)
  # -------------------------------------------------------------------------
  # Use ACTARM if available, else ARM (SAS: if ACTARM exists rename to ARM)
  arm_col <- if (dm_actarm && "ACTARM" %in% names(dm)) "ACTARM" else "ARM"

  dm <- dm %>%
    dplyr::mutate(
      arm_display = .data[[arm_col]]
    )

  # SAS PROPCASE logic (lines 294-311): word-by-word PROPCASE only if arm is

  # all-uppercase. Short words stay lowercase, "MG"/"KG" -> lowercase,
  # "ML" -> "mL". Replicate with a custom helper.
  propcase_arm <- function(arm_str) {
    if (is.na(arm_str)) return(NA_character_)
    # SAS: only apply if no lowercase letters present (all-uppercase detection)
    if (!grepl("[a-z]", arm_str)) {
      words <- strsplit(arm_str, "\\s+")[[1]]
      words <- purrr::map_chr(words, function(w) {
        wu <- toupper(w)
        if (wu %in% c("MG", "KG")) return(tolower(w))
        if (wu == "ML") return("mL")
        if (nchar(w) <= 3 && grepl("[0-9]", w)) return(w)
        stringr::str_to_title(w)
      })
      return(paste(words, collapse = " "))
    }
    arm_str
  }

  dm <- dm %>%
    dplyr::mutate(
      arm_display = purrr::map_chr(.data$arm_display, propcase_arm)
    )

  # Exclude SCRNFAIL/NOTASSGN if ARMCD exists (SAS lines 288-290)
  if ("ARMCD" %in% names(dm)) {
    dm <- dm %>%
      dplyr::filter(!toupper(.data$ARMCD) %in% c("SCRNFAIL", "NOTASSGN"))
  }

  # Treatment arms: subject counts per arm (SAS lines 315-331)
  treatment_arms <- dm %>%
    dplyr::count(.data$arm_display, name = "n_subjects") %>%
    dplyr::arrange(.data$arm_display)

  # -------------------------------------------------------------------------
  # Sub-step b: Filter LB to 4 liver analytes (SAS line 341)
  # -------------------------------------------------------------------------
  lb_liver <- lb %>%
    dplyr::filter(toupper(.data$LBTESTCD) %in% ALL_LIVER_CODES) %>%
    dplyr::mutate(
      liver_test = dplyr::case_when(
        toupper(.data$LBTESTCD) %in% toupper(LIVER_CODES$ALT)  ~ "ALT",
        toupper(.data$LBTESTCD) %in% toupper(LIVER_CODES$AST)  ~ "AST",
        toupper(.data$LBTESTCD) %in% toupper(LIVER_CODES$ALP)  ~ "ALP",
        toupper(.data$LBTESTCD) %in% toupper(LIVER_CODES$BILI) ~ "BILI",
        TRUE ~ NA_character_
      )
    )

  # -------------------------------------------------------------------------
  # Sub-step c: Remove records with missing/zero LBSTNRHI (SAS line 346)
  # -------------------------------------------------------------------------
  lb_liver <- lb_liver %>%
    dplyr::filter(!is.na(.data$LBSTNRHI), .data$LBSTNRHI > 0)

  # -------------------------------------------------------------------------
  # Sub-step d: Drop all-NA columns (SAS %find_null_col)
  # -------------------------------------------------------------------------
  lb_liver <- find_null_col(lb_liver)

  # -------------------------------------------------------------------------
  # Sub-step e: Merge LB with DM (SAS lines 457-461)
  # Inner join on USUBJID — subjects must be in both DM and LB
  # -------------------------------------------------------------------------
  dm_cols <- c("USUBJID", "arm_display")
  if ("RFSTDTC" %in% names(dm)) dm_cols <- c(dm_cols, "RFSTDTC")
  if ("RFENDTC" %in% names(dm)) dm_cols <- c(dm_cols, "RFENDTC")
  if ("ARMCD" %in% names(dm))   dm_cols <- c(dm_cols, "ARMCD")

  lb_merged <- lb_liver %>%
    dplyr::inner_join(
      dm %>% dplyr::select(dplyr::all_of(dm_cols)),
      by = "USUBJID"
    )

  # -------------------------------------------------------------------------
  # Sub-step f: Derive LBDY if missing (SAS lines 475-496)
  # SAS: LBDY = lbdt - studydate; if LBDY >= 0 then LBDY + 1 (no day 0)
  # -------------------------------------------------------------------------
  if (!"LBDY" %in% names(lb_merged) || all(is.na(lb_merged$LBDY))) {
    if ("LBDTC" %in% names(lb_merged) && "RFSTDTC" %in% names(lb_merged)) {
      lb_merged <- lb_merged %>%
        dplyr::mutate(
          .lbdt   = as.Date(stringr::str_sub(.data$LBDTC, 1, 10)),
          .rfstdt = as.Date(stringr::str_sub(.data$RFSTDTC, 1, 10)),
          LBDY    = as.numeric(.data$.lbdt - .data$.rfstdt),
          LBDY    = dplyr::if_else(.data$LBDY >= 0, .data$LBDY + 1, .data$LBDY)
        ) %>%
        dplyr::select(-".lbdt", -".rfstdt")
    }
  }

  # -------------------------------------------------------------------------
  # Sub-step g: Derive LBBLFL if missing (SAS lines 499-530)
  # SAS: sort by USUBJID descending LBDY where LBDY <= 1, first.USUBJID
  # NOTE: SAS derives per USUBJID (NOT per USUBJID x test)
  # -------------------------------------------------------------------------
  if (!"LBBLFL" %in% names(lb_merged) || all(is.na(lb_merged$LBBLFL))) {
    if ("LBDY" %in% names(lb_merged)) {
      bl_candidates <- lb_merged %>%
        dplyr::filter(!is.na(.data$LBDY), .data$LBDY <= 1) %>%
        dplyr::arrange(.data$USUBJID, dplyr::desc(.data$LBDY)) %>%
        dplyr::group_by(.data$USUBJID) %>%
        dplyr::slice_head(n = 1L) %>%
        dplyr::ungroup() %>%
        dplyr::select("USUBJID", bl_lbdy = "LBDY")

      lb_merged <- lb_merged %>%
        dplyr::left_join(bl_candidates, by = "USUBJID") %>%
        dplyr::mutate(
          LBBLFL = dplyr::if_else(
            !is.na(.data$bl_lbdy) & !is.na(.data$LBDY) &
              .data$LBDY == .data$bl_lbdy,
            "Y",
            NA_character_
          )
        ) %>%
        dplyr::select(-"bl_lbdy")
    }
  }

  # -------------------------------------------------------------------------
  # Sub-step h: Derive VISITNUM if missing (SAS lines 532-582)
  # -------------------------------------------------------------------------
  if (!"VISITNUM" %in% names(lb_merged) || all(is.na(lb_merged$VISITNUM))) {
    if ("LBDY" %in% names(lb_merged)) {
      visit_map <- lb_merged %>%
        dplyr::distinct(.data$LBDY) %>%
        dplyr::filter(!is.na(.data$LBDY)) %>%
        dplyr::arrange(.data$LBDY) %>%
        dplyr::mutate(VISITNUM = dplyr::row_number())
      lb_merged <- lb_merged %>%
        dplyr::select(-dplyr::any_of("VISITNUM")) %>%
        dplyr::left_join(visit_map, by = "LBDY")
    }
  }

  # -------------------------------------------------------------------------
  # Sub-step i: Create subnum per USUBJID (SAS lines 584-604)
  # Sequential subject numbering for output ordering
  # -------------------------------------------------------------------------
  subj_map <- lb_merged %>%
    dplyr::distinct(.data$USUBJID) %>%
    dplyr::arrange(.data$USUBJID) %>%
    dplyr::mutate(subnum = dplyr::row_number())
  lb_merged <- lb_merged %>%
    dplyr::left_join(subj_map, by = "USUBJID")

  # -------------------------------------------------------------------------
  # Sub-step j: Compute max_labs — non-baseline maximums (SAS lines 610-656)
  # PROC MEANS: max LBSTRESN where LBBLFL != "Y" and LBDY >= 1
  # -------------------------------------------------------------------------
  post_bl <- lb_merged %>%
    dplyr::filter(is.na(.data$LBBLFL) | .data$LBBLFL != "Y")

  # Further restrict to post-baseline (LBDY >= 1) if LBDY is available
  if ("LBDY" %in% names(post_bl) && !all(is.na(post_bl$LBDY))) {
    post_bl <- post_bl %>%
      dplyr::filter(!is.na(.data$LBDY), .data$LBDY >= 1)
  }

  max_labs <- post_bl %>%
    dplyr::filter(!is.na(.data$LBSTRESN)) %>%
    dplyr::group_by(.data$USUBJID, .data$liver_test, .data$arm_display) %>%
    dplyr::summarise(
      max_stresn = max(.data$LBSTRESN, na.rm = TRUE),
      max_stnrhi = dplyr::first(.data$LBSTNRHI),
      max_stnrlo = dplyr::first(
        if ("LBSTNRLO" %in% names(.)) .data$LBSTNRLO else NA_real_
      ),
      subnum     = dplyr::first(.data$subnum),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      max_uln_ratio = .data$max_stresn / .data$max_stnrhi
    )

  # Get study day of maximum value for scatter/max-by-day outputs
  max_labs_stday <- post_bl %>%
    dplyr::filter(!is.na(.data$LBSTRESN)) %>%
    dplyr::group_by(.data$USUBJID, .data$liver_test, .data$arm_display) %>%
    dplyr::filter(.data$LBSTRESN == max(.data$LBSTRESN, na.rm = TRUE)) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select("USUBJID", "liver_test", "arm_display",
                  max_stresn = "LBSTRESN",
                  max_stday  = dplyr::any_of("LBDY"),
                  max_stnrhi = "LBSTNRHI",
                  "subnum")

  if (!"max_stday" %in% names(max_labs_stday)) {
    max_labs_stday <- max_labs_stday %>%
      dplyr::mutate(max_stday = NA_real_)
  }

  max_labs_stday <- max_labs_stday %>%
    dplyr::mutate(max_uln_ratio = .data$max_stresn / .data$max_stnrhi)

  # -------------------------------------------------------------------------
  # Sub-step k: Compute baseline_labs (SAS lines 626-634)
  # -------------------------------------------------------------------------
  baseline_labs <- lb_merged %>%
    dplyr::filter(.data$LBBLFL == "Y") %>%
    dplyr::filter(!is.na(.data$LBSTRESN)) %>%
    dplyr::group_by(.data$USUBJID, .data$liver_test, .data$arm_display) %>%
    dplyr::summarise(
      bl_stresn = dplyr::first(.data$LBSTRESN),
      bl_stnrhi = dplyr::first(.data$LBSTNRHI),
      bl_stnrlo = dplyr::first(
        if ("LBSTNRLO" %in% names(.)) .data$LBSTNRLO else NA_real_
      ),
      subnum    = dplyr::first(.data$subnum),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(
      bl_uln_ratio = .data$bl_stresn / .data$bl_stnrhi
    )

  # -------------------------------------------------------------------------
  # Sub-step l: Combine baseline + max (SAS lines 658-664)
  # SAS: deletes if either maxlab or bllab is missing
  # -------------------------------------------------------------------------
  labdata_bl_max <- baseline_labs %>%
    dplyr::full_join(
      max_labs %>% dplyr::select("USUBJID", "liver_test", "arm_display",
                                  "max_stresn", "max_stnrhi", "max_uln_ratio",
                                  "subnum"),
      by = c("USUBJID", "liver_test", "arm_display", "subnum")
    )

  cli::cli_alert_success("Liver setup complete: {nrow(treatment_arms)} treatment arm(s).")

  list(
    treatment_arms  = treatment_arms,
    lb_merged       = lb_merged,
    max_labs        = max_labs,
    max_labs_stday  = max_labs_stday,
    baseline_labs   = baseline_labs,
    labdata_bl_max  = labdata_bl_max,
    dm              = dm,
    config          = config
  )
}


# =============================================================================
# flag_uln_multiples — Create ULN threshold indicator columns
# =============================================================================
#' Migrate SAS ULN flag logic (lines 725-786). SAS uses GE (>=) operator.
#'
#' For ALT/AST/ALP: >=2x, >=3x, >=5x, >=10x, >=20x ULN, plus Normal/>=HI/Missing.
#' For BILI: >=1.5x, >=2x, >=3x ULN.
#'
#' @param data      Data frame with max_uln_ratio column.
#' @param test_type Character: "ALT", "AST", "ALP", or "BILI".
#'
#' @return Data frame with ULN flag columns appended.
flag_uln_multiples <- function(data, test_type) {

  if (nrow(data) == 0L) return(data)

  if (test_type %in% c("ALT", "AST")) {
    # SAS: ALTx2 if ALT_RES >= ALT_HI*2
    # ALTnorm if ALT_LO < ALT_RES < ALT_HI (strict both sides)
    data <- data %>%
      dplyr::mutate(
        gt_2x_uln  = dplyr::if_else(.data$max_uln_ratio >= 2,  1L, 0L, missing = 0L),
        gt_3x_uln  = dplyr::if_else(.data$max_uln_ratio >= 3,  1L, 0L, missing = 0L),
        gt_5x_uln  = dplyr::if_else(.data$max_uln_ratio >= 5,  1L, 0L, missing = 0L),
        gt_10x_uln = dplyr::if_else(.data$max_uln_ratio >= 10, 1L, 0L, missing = 0L),
        gt_20x_uln = dplyr::if_else(.data$max_uln_ratio >= 20, 1L, 0L, missing = 0L)
      )
  } else if (test_type == "ALP") {
    # SAS: ALP uses GE for thresholds; ALPnorm = ALP_RES <= ALP_HI
    # ALP_GEHI = ALP_RES >= ALP_HI, ALP_miss = missing
    data <- data %>%
      dplyr::mutate(
        gt_2x_uln  = dplyr::if_else(.data$max_uln_ratio >= 2,  1L, 0L, missing = 0L),
        gt_3x_uln  = dplyr::if_else(.data$max_uln_ratio >= 3,  1L, 0L, missing = 0L),
        gt_5x_uln  = dplyr::if_else(.data$max_uln_ratio >= 5,  1L, 0L, missing = 0L),
        gt_10x_uln = dplyr::if_else(.data$max_uln_ratio >= 10, 1L, 0L, missing = 0L),
        gt_20x_uln = dplyr::if_else(.data$max_uln_ratio >= 20, 1L, 0L, missing = 0L),
        alp_norm   = dplyr::if_else(
          !is.na(.data$max_uln_ratio) & .data$max_uln_ratio <= 1,
          1L, 0L, missing = 0L
        ),
        alp_gehi   = dplyr::if_else(
          !is.na(.data$max_uln_ratio) & .data$max_uln_ratio >= 1,
          1L, 0L, missing = 0L
        ),
        alp_miss   = dplyr::if_else(is.na(.data$max_uln_ratio), 1L, 0L)
      )
  } else {
    # BILI: >=1.5x, >=2x, >=3x ULN
    data <- data %>%
      dplyr::mutate(
        gt_1_5x_uln = dplyr::if_else(.data$max_uln_ratio >= 1.5, 1L, 0L, missing = 0L),
        gt_2x_uln   = dplyr::if_else(.data$max_uln_ratio >= 2,   1L, 0L, missing = 0L),
        gt_3x_uln   = dplyr::if_else(.data$max_uln_ratio >= 3,   1L, 0L, missing = 0L)
      )
  }

  data
}


# =============================================================================
# compute_uln_counts — N/N/pct for each ULN threshold
# =============================================================================
#' Migrate SAS %counts macro (lines 789-850). Counts subjects meeting each
#' ULN threshold. Denominator is arm subject count from DM.
#'
#' @param data      Data frame with ULN flag columns from flag_uln_multiples().
#' @param arm_n     Integer. Number of subjects in the treatment arm.
#' @param test_type Character: "ALT", "AST", "ALP", or "BILI".
#'
#' @return Tibble with threshold, n, N, pct, display columns.
compute_uln_counts <- function(data, arm_n, test_type) {

  if (test_type %in% c("ALT", "AST")) {
    thresholds <- c("gt_2x_uln", "gt_3x_uln", "gt_5x_uln", "gt_10x_uln", "gt_20x_uln")
    labels     <- c(">=2x ULN", ">=3x ULN", ">=5x ULN", ">=10x ULN", ">=20x ULN")
  } else if (test_type == "ALP") {
    thresholds <- c("gt_2x_uln", "gt_3x_uln", "gt_5x_uln", "gt_10x_uln", "gt_20x_uln",
                     "alp_norm", "alp_gehi", "alp_miss")
    labels     <- c(">=2x ULN", ">=3x ULN", ">=5x ULN", ">=10x ULN", ">=20x ULN",
                     "Normal", ">=1x ULN", "Missing")
  } else {
    thresholds <- c("gt_1_5x_uln", "gt_2x_uln", "gt_3x_uln")
    labels     <- c(">=1.5x ULN", ">=2x ULN", ">=3x ULN")
  }

  denom <- max(arm_n, 1L)

  purrr::map2_dfr(thresholds, labels, function(col, lbl) {
    if (!col %in% names(data)) {
      return(tibble::tibble(threshold = lbl, n = 0L, N = denom, pct = 0,
                            display = paste0("0/", denom, " (0.0%)")))
    }
    n_above <- sum(data[[col]], na.rm = TRUE)
    pct_val <- janitor::round_half_up(100 * n_above / denom, 1)
    tibble::tibble(
      threshold = lbl,
      n         = as.integer(n_above),
      N         = as.integer(denom),
      pct       = pct_val,
      display   = paste0(n_above, "/", denom, " (", format(pct_val, nsmall = 1), "%)")
    )
  })
}


# =============================================================================
# compute_dili_patterns — 9 DILI combinations (at-any-visit + during-study)
# =============================================================================
#' Migrate SAS DILI logic (lines 855-1039).
#'
#' 9 patterns = 3 ALT/AST thresholds x 3 BILI thresholds.
#' Each pattern has 3 ALP states: Normal, >=HI, Missing.
#' Two computation modes: at-any-visit (av) and during-study (ds).
#'
#' @param lb_merged Full merged LB data for this arm (visit-level).
#' @param alt_flagged ALT data with ULN flags (subject-level max).
#' @param ast_flagged AST data with ULN flags (subject-level max).
#' @param alp_flagged ALP data with ULN flags (subject-level max).
#' @param bili_flagged BILI data with ULN flags (subject-level max).
#' @param arm_n     Integer. Arm subject count.
#'
#' @return Named list with dili_av and dili_ds tibbles.
compute_dili_patterns <- function(lb_merged, alt_flagged, ast_flagged,
                                  alp_flagged, bili_flagged, arm_n) {

  alt_ast_mults <- c(3, 5, 10)
  bili_mults    <- c(1.5, 2, 3)
  denom         <- max(arm_n, 1L)

  # ---- During-study (ds): conditions at ANY time (subject-level flags) ----
  compute_ds <- function() {
    results <- list()
    idx <- 0L

    for (aa_mult in alt_ast_mults) {
      for (b_mult in bili_mults) {
        idx <- idx + 1L

        # Subjects with ALT >= aa_mult x ULN OR AST >= aa_mult x ULN
        subj_alt <- alt_flagged$USUBJID[
          !is.na(alt_flagged$max_uln_ratio) & alt_flagged$max_uln_ratio >= aa_mult
        ]
        subj_ast <- ast_flagged$USUBJID[
          !is.na(ast_flagged$max_uln_ratio) & ast_flagged$max_uln_ratio >= aa_mult
        ]
        subj_alt_ast <- unique(c(subj_alt, subj_ast))

        # Subjects with BILI >= b_mult x ULN
        subj_bili <- bili_flagged$USUBJID[
          !is.na(bili_flagged$max_uln_ratio) & bili_flagged$max_uln_ratio >= b_mult
        ]

        # DILI: both conditions met
        dili_subj <- intersect(subj_alt_ast, subj_bili)

        # ALP states for DILI subjects
        subj_alp_norm <- alp_flagged$USUBJID[
          !is.na(alp_flagged$alp_norm) & alp_flagged$alp_norm == 1L
        ]
        subj_alp_gehi <- alp_flagged$USUBJID[
          !is.na(alp_flagged$alp_gehi) & alp_flagged$alp_gehi == 1L
        ]
        subj_alp_miss <- alp_flagged$USUBJID[
          !is.na(alp_flagged$alp_miss) & alp_flagged$alp_miss == 1L
        ]

        n_dili      <- length(dili_subj)
        n_alp_norm  <- length(intersect(dili_subj, subj_alp_norm))
        n_alp_gehi  <- length(intersect(dili_subj, subj_alp_gehi))
        n_alp_miss  <- length(intersect(dili_subj, subj_alp_miss))

        results[[idx]] <- tibble::tibble(
          pattern            = paste0("dili_", idx),
          alt_ast_threshold  = paste0(">=", aa_mult, "x ULN"),
          bili_threshold     = paste0(">=", b_mult, "x ULN"),
          analysis_type      = "during_study",
          n_dili             = n_dili,
          pct_dili           = janitor::round_half_up(100 * n_dili / denom, 1),
          n_alp_norm         = n_alp_norm,
          pct_alp_norm       = janitor::round_half_up(100 * n_alp_norm / denom, 1),
          n_alp_gehi         = n_alp_gehi,
          pct_alp_gehi       = janitor::round_half_up(100 * n_alp_gehi / denom, 1),
          n_alp_miss         = n_alp_miss,
          pct_alp_miss       = janitor::round_half_up(100 * n_alp_miss / denom, 1),
          N                  = as.integer(denom),
          dili_subjects      = list(dili_subj)
        )
      }
    }
    dplyr::bind_rows(results)
  }

  # ---- At-any-visit (av): all conditions on same visit (visit-level) ----
  compute_av <- function() {
    # Build visit-level ULN ratios per subject x visit
    if (!"VISITNUM" %in% names(lb_merged) && !"LBDY" %in% names(lb_merged)) {
      return(compute_ds() %>% dplyr::mutate(analysis_type = "at_any_visit"))
    }

    visit_col <- if ("VISITNUM" %in% names(lb_merged)) "VISITNUM" else "LBDY"

    visit_data <- lb_merged %>%
      dplyr::filter(!is.na(.data$LBSTRESN), !is.na(.data$LBSTNRHI),
                    .data$LBSTNRHI > 0) %>%
      dplyr::mutate(uln_ratio = .data$LBSTRESN / .data$LBSTNRHI) %>%
      dplyr::select("USUBJID", visit = dplyr::all_of(visit_col),
                    "liver_test", "uln_ratio") %>%
      tidyr::pivot_wider(
        names_from  = "liver_test",
        values_from = "uln_ratio",
        values_fn   = max
      )

    results <- list()
    idx <- 0L

    for (aa_mult in alt_ast_mults) {
      for (b_mult in bili_mults) {
        idx <- idx + 1L

        # Same-visit conditions
        av_hits <- visit_data %>%
          dplyr::mutate(
            alt_ast_hit = (
              (if ("ALT" %in% names(.)) !is.na(.data$ALT) & .data$ALT >= aa_mult else FALSE) |
              (if ("AST" %in% names(.)) !is.na(.data$AST) & .data$AST >= aa_mult else FALSE)
            ),
            bili_hit = if ("BILI" %in% names(.)) {
              !is.na(.data$BILI) & .data$BILI >= b_mult
            } else {
              FALSE
            }
          ) %>%
          dplyr::filter(.data$alt_ast_hit & .data$bili_hit)

        dili_subj <- unique(av_hits$USUBJID)
        n_dili    <- length(dili_subj)

        # ALP state at same visit for dili subjects
        av_dili_visits <- av_hits %>%
          dplyr::mutate(
            alp_norm_v = if ("ALP" %in% names(.)) {
              !is.na(.data$ALP) & .data$ALP <= 1
            } else {
              FALSE
            },
            alp_gehi_v = if ("ALP" %in% names(.)) {
              !is.na(.data$ALP) & .data$ALP >= 1
            } else {
              FALSE
            },
            alp_miss_v = if ("ALP" %in% names(.)) {
              is.na(.data$ALP)
            } else {
              TRUE
            }
          )

        subj_alp_norm_av <- unique(av_dili_visits$USUBJID[av_dili_visits$alp_norm_v])
        subj_alp_gehi_av <- unique(av_dili_visits$USUBJID[av_dili_visits$alp_gehi_v])
        subj_alp_miss_av <- unique(av_dili_visits$USUBJID[av_dili_visits$alp_miss_v])

        results[[idx]] <- tibble::tibble(
          pattern            = paste0("dili_", idx),
          alt_ast_threshold  = paste0(">=", aa_mult, "x ULN"),
          bili_threshold     = paste0(">=", b_mult, "x ULN"),
          analysis_type      = "at_any_visit",
          n_dili             = n_dili,
          pct_dili           = janitor::round_half_up(100 * n_dili / denom, 1),
          n_alp_norm         = length(subj_alp_norm_av),
          pct_alp_norm       = janitor::round_half_up(100 * length(subj_alp_norm_av) / denom, 1),
          n_alp_gehi         = length(subj_alp_gehi_av),
          pct_alp_gehi       = janitor::round_half_up(100 * length(subj_alp_gehi_av) / denom, 1),
          n_alp_miss         = length(subj_alp_miss_av),
          pct_alp_miss       = janitor::round_half_up(100 * length(subj_alp_miss_av) / denom, 1),
          N                  = as.integer(denom),
          dili_subjects      = list(dili_subj)
        )
      }
    }
    dplyr::bind_rows(results)
  }

  dili_av <- compute_av()
  dili_ds <- compute_ds()

  list(dili_av = dili_av, dili_ds = dili_ds)
}


# =============================================================================
# compute_bl_max_crosstab — Baseline vs Max cross-tabulation
# =============================================================================
#' Migrate SAS %dili3/%dili4/%dili5 (lines 1072-1207). Creates cross-tab
#' of baseline ULN category x maximum ULN category.
#'
#' SAS uses 5 bins: <2x, 2x-5x, 5x-10x, 10x-20x, >=20x ULN.
#'
#' @param bl_max_data Data frame with bl_uln_ratio and max_uln_ratio.
#' @param test_type   Character: "ALT", "AST", "ALP", or "BILI".
#'
#' @return Tibble cross-tabulation with bl_cat rows and max_cat columns.
compute_bl_max_crosstab <- function(bl_max_data, test_type) {

  # SAS uses 5 bins for ALL tests (lines 1072-1100)
  breaks <- c(-Inf, 2, 5, 10, 20, Inf)
  labels <- c("<2x ULN", "2x-5x ULN", "5x-10x ULN", "10x-20x ULN", ">=20x ULN")

  test_data <- bl_max_data %>%
    dplyr::filter(.data$liver_test == test_type,
                  !is.na(.data$bl_uln_ratio), !is.na(.data$max_uln_ratio))

  if (nrow(test_data) == 0L) {
    # Return empty cross-tab with proper structure
    empty_row <- tibble::tibble(bl_cat = factor(labels, levels = labels))
    for (lbl in labels) {
      empty_row[[lbl]] <- 0L
    }
    return(empty_row)
  }

  test_data <- test_data %>%
    dplyr::mutate(
      bl_cat  = cut(.data$bl_uln_ratio,  breaks = breaks, labels = labels,
                     right = FALSE, include.lowest = TRUE),
      max_cat = cut(.data$max_uln_ratio, breaks = breaks, labels = labels,
                     right = FALSE, include.lowest = TRUE)
    )

  # Build count cross-tab
  ct <- test_data %>%
    dplyr::count(.data$bl_cat, .data$max_cat) %>%
    tidyr::pivot_wider(
      names_from   = "max_cat",
      values_from  = "n",
      values_fill  = 0L
    )

  # Ensure all rows and columns present
  ct <- ct %>%
    dplyr::mutate(bl_cat = forcats::fct_relevel(.data$bl_cat, labels))
  for (lbl in labels) {
    if (!lbl %in% names(ct)) ct[[lbl]] <- 0L
  }
  ct %>% dplyr::arrange(.data$bl_cat)
}


# =============================================================================
# compute_hy_scatter — AST/ALT vs BILI max value comparisons
# =============================================================================
#' Migrate SAS AST/ALT vs BILI scatter data (lines 1209-1221).
#'
#' @param max_labs Data frame of subject-level max labs.
#'
#' @return Named list: ast_bili, alt_bili tibbles.
compute_hy_scatter <- function(max_labs) {

  # Pivot to get test-level max ULN ratios side by side per subject
  hy_wide <- max_labs %>%
    dplyr::filter(.data$liver_test %in% c("ALT", "AST", "BILI")) %>%
    dplyr::select("USUBJID", "arm_display", "liver_test", "max_uln_ratio",
                  dplyr::any_of("subnum")) %>%
    tidyr::pivot_wider(
      names_from  = "liver_test",
      values_from = "max_uln_ratio"
    )

  # AST vs BILI
  ast_bili <- hy_wide %>%
    dplyr::filter(!is.na(.data$AST), !is.na(.data$BILI)) %>%
    dplyr::select("USUBJID", "arm_display", dplyr::any_of("subnum"),
                  AST_max = "AST", BILI_max = "BILI")

  # ALT vs BILI
  alt_bili <- hy_wide %>%
    dplyr::filter(!is.na(.data$ALT), !is.na(.data$BILI)) %>%
    dplyr::select("USUBJID", "arm_display", dplyr::any_of("subnum"),
                  ALT_max = "ALT", BILI_max = "BILI")

  list(ast_bili = ast_bili, alt_bili = alt_bili)
}


# =============================================================================
# liver_arm — Per-arm analysis orchestrator
# =============================================================================
#' Migrate SAS %liver_arm (lines 672-1225). Iterates over treatment arms,
#' computing ULN tables, DILI patterns, baseline vs max cross-tabs, and
#' Hy's Law scatter data for each arm.
#'
#' @param setup_data Named list from liver_setup().
#' @param config     Named list from liver_params().
#'
#' @return List of per-arm result lists.
liver_arm <- function(setup_data, config) {

  cli::cli_alert_info("Liver arm-level analysis: processing {nrow(setup_data$treatment_arms)} arm(s).")

  arm_results <- purrr::map(
    setup_data$treatment_arms$arm_display,
    function(arm_name) {
      arm_n <- setup_data$treatment_arms$n_subjects[
        setup_data$treatment_arms$arm_display == arm_name
      ]
      arm_max      <- setup_data$max_labs %>% dplyr::filter(.data$arm_display == arm_name)
      arm_bl_max   <- setup_data$labdata_bl_max %>% dplyr::filter(.data$arm_display == arm_name)
      arm_max_stdy <- setup_data$max_labs_stday %>% dplyr::filter(.data$arm_display == arm_name)
      arm_merged   <- setup_data$lb_merged %>% dplyr::filter(.data$arm_display == arm_name)

      # Split by liver test and flag ULN multiples
      alt_data  <- arm_max %>% dplyr::filter(.data$liver_test == "ALT")  %>% flag_uln_multiples("ALT")
      ast_data  <- arm_max %>% dplyr::filter(.data$liver_test == "AST")  %>% flag_uln_multiples("AST")
      alp_data  <- arm_max %>% dplyr::filter(.data$liver_test == "ALP")  %>% flag_uln_multiples("ALP")
      bili_data <- arm_max %>% dplyr::filter(.data$liver_test == "BILI") %>% flag_uln_multiples("BILI")

      # ULN count tables
      uln_counts <- list(
        ALT  = compute_uln_counts(alt_data,  arm_n, "ALT"),
        AST  = compute_uln_counts(ast_data,  arm_n, "AST"),
        ALP  = compute_uln_counts(alp_data,  arm_n, "ALP"),
        BILI = compute_uln_counts(bili_data, arm_n, "BILI")
      )

      # DILI patterns (at-any-visit and during-study)
      dili <- compute_dili_patterns(
        arm_merged, alt_data, ast_data, alp_data, bili_data, arm_n
      )

      # Baseline vs max cross-tabs
      bl_max_crosstab <- list(
        ALT  = compute_bl_max_crosstab(arm_bl_max, "ALT"),
        AST  = compute_bl_max_crosstab(arm_bl_max, "AST"),
        ALP  = compute_bl_max_crosstab(arm_bl_max, "ALP"),
        BILI = compute_bl_max_crosstab(arm_bl_max, "BILI")
      )

      # Max lab by study day per test
      maxstdy <- list(
        ALT  = arm_max_stdy %>% dplyr::filter(.data$liver_test == "ALT"),
        AST  = arm_max_stdy %>% dplyr::filter(.data$liver_test == "AST"),
        ALP  = arm_max_stdy %>% dplyr::filter(.data$liver_test == "ALP"),
        BILI = arm_max_stdy %>% dplyr::filter(.data$liver_test == "BILI")
      )

      cli::cli_alert_info("  Arm '{arm_name}' processed (N={arm_n}).")

      list(
        arm_name        = arm_name,
        arm_n           = arm_n,
        uln_counts      = uln_counts,
        dili            = dili,
        bl_max_crosstab = bl_max_crosstab,
        maxstdy         = maxstdy,
        alt_data        = alt_data,
        ast_data        = ast_data,
        alp_data        = alp_data,
        bili_data       = bili_data
      )
    }
  )

  names(arm_results) <- setup_data$treatment_arms$arm_display
  arm_results
}


# =============================================================================
# liver_one — Cross-arm merge per test
# =============================================================================
#' Migrate SAS %liver_one (lines 1231-1307). Merges per-arm results across
#' arms for a given test or result type.
#'
#' @param arm_results List of per-arm result lists from liver_arm().
#' @param test_type   Character: "ALT", "AST", "ALP", or "BILI".
#'
#' @return Named list with combined ULN, DILI, bl_max, and maxstdy data.
liver_one <- function(arm_results, test_type) {

  # Combine ULN count tables across arms
  combined_uln <- purrr::map_dfr(arm_results, function(arm) {
    arm$uln_counts[[test_type]] %>%
      dplyr::mutate(arm = arm$arm_name)
  })

  # Widen: one column per arm for display
  combined_uln_wide <- combined_uln %>%
    tidyr::pivot_wider(
      id_cols     = "threshold",
      names_from  = "arm",
      values_from = c("n", "pct", "display", "N"),
      names_glue  = "{arm}_{.value}"
    )

  # Combine DILI (av + ds) across arms
  combined_dili_av <- purrr::map_dfr(arm_results, function(arm) {
    arm$dili$dili_av %>%
      dplyr::mutate(arm = arm$arm_name) %>%
      dplyr::select(-"dili_subjects")
  })
  combined_dili_ds <- purrr::map_dfr(arm_results, function(arm) {
    arm$dili$dili_ds %>%
      dplyr::mutate(arm = arm$arm_name) %>%
      dplyr::select(-"dili_subjects")
  })

  # Combine baseline vs max cross-tabs across arms
  combined_blmax <- purrr::map_dfr(arm_results, function(arm) {
    arm$bl_max_crosstab[[test_type]] %>%
      dplyr::mutate(arm = arm$arm_name, test = test_type)
  })

  # Combine max-by-study-day across arms
  combined_maxstdy <- purrr::map_dfr(arm_results, function(arm) {
    arm$maxstdy[[test_type]] %>%
      dplyr::mutate(arm = arm$arm_name)
  })

  list(
    uln_table  = combined_uln_wide,
    dili_av    = combined_dili_av,
    dili_ds    = combined_dili_ds,
    bl_max     = combined_blmax,
    maxstdy    = combined_maxstdy
  )
}


# =============================================================================
# liver_outfmt — Output assembly
# =============================================================================
#' Migrate SAS %liver_outfmt (lines 1313-1489). Assembles final output
#' datasets for Excel writing.
#'
#' @param arm_results List from liver_arm().
#' @param setup_data  List from liver_setup().
#' @param config      Named list from liver_params().
#'
#' @return Named list of output datasets.
liver_outfmt <- function(arm_results, setup_data, config) {

  cli::cli_alert_info("Assembling output datasets.")

  # --- 1. Combine all 4 tests via liver_one ---
  merged <- list(
    ALT  = liver_one(arm_results, "ALT"),
    AST  = liver_one(arm_results, "AST"),
    ALP  = liver_one(arm_results, "ALP"),
    BILI = liver_one(arm_results, "BILI")
  )

  # --- 2. lab_tables: stack ULN frequency tables ---
  lab_tables <- dplyr::bind_rows(
    merged$ALT$uln_table  %>% dplyr::mutate(test = "ALT"),
    merged$AST$uln_table  %>% dplyr::mutate(test = "AST"),
    merged$ALP$uln_table  %>% dplyr::mutate(test = "ALP"),
    merged$BILI$uln_table %>% dplyr::mutate(test = "BILI")
  )

  # --- 3. hyslaw: stack DILI at-any-visit + during-study ---
  hyslaw <- dplyr::bind_rows(
    purrr::map_dfr(names(merged), function(tst) {
      dplyr::bind_rows(merged[[tst]]$dili_av, merged[[tst]]$dili_ds) %>%
        dplyr::mutate(test = tst)
    })
  )

  # --- 4. max_bl_tables: stack all 4 test cross-tabs ---
  max_bl_tables <- dplyr::bind_rows(
    merged$ALT$bl_max,
    merged$AST$bl_max,
    merged$ALP$bl_max,
    merged$BILI$bl_max
  )

  # --- 5. Scatter data (AST/ALT vs BILI) ---
  scatter <- compute_hy_scatter(setup_data$max_labs)
  ast_bili_scatter <- scatter$ast_bili
  alt_bili_scatter <- scatter$alt_bili

  # --- 6. Max-by-study-day per test ---
  alt_max_stdy <- merged$ALT$maxstdy
  ast_max_stdy <- merged$AST$maxstdy
  alp_max_stdy <- merged$ALP$maxstdy
  tb_max_stdy  <- merged$BILI$maxstdy

  # --- 7. Hy's Law subject detail (SAS lines 1351-1401) ---
  # Identify subjects meeting ANY dili_ds pattern
  all_dili_subj <- character(0)
  for (arm in arm_results) {
    for (pattern_row in seq_len(nrow(arm$dili$dili_ds))) {
      subjs <- arm$dili$dili_ds$dili_subjects[[pattern_row]]
      all_dili_subj <- unique(c(all_dili_subj, subjs))
    }
  }

  hylawind <- setup_data$dm %>%
    dplyr::filter(.data$USUBJID %in% all_dili_subj) %>%
    dplyr::select("USUBJID", "arm_display", dplyr::any_of(c("ARMCD")))

  # --- 8. Baseline conflict detection (SAS lines 1404-1485) ---
  blconflict <- tibble::tibble()
  if ("LBBLFL" %in% names(setup_data$lb_merged)) {
    bl_records <- setup_data$lb_merged %>%
      dplyr::filter(.data$LBBLFL == "Y")

    dup_bl <- bl_records %>%
      dplyr::group_by(.data$USUBJID, .data$liver_test) %>%
      dplyr::filter(dplyr::n() > 1L) %>%
      dplyr::ungroup()

    if (nrow(dup_bl) > 0L) {
      blconflict <- dup_bl %>%
        dplyr::select("USUBJID", "liver_test", "LBSTRESN", "LBSTNRHI",
                      dplyr::any_of(c("LBDY", "VISITNUM", "arm_display"))) %>%
        dplyr::arrange(.data$USUBJID, .data$liver_test)

      # SAS truncates at 100 rows (obs=100)
      if (nrow(blconflict) > 100L) {
        blconflict <- blconflict %>% dplyr::slice_head(n = 100L)
      }
    }
  }

  # Also count BL conflicts for summary sheet
  bkc_count <- nrow(blconflict)

  list(
    treatment_arms  = setup_data$treatment_arms,
    lab_tables      = lab_tables,
    hyslaw          = hyslaw,
    max_bl_tables   = max_bl_tables,
    ast_bili_scatter = ast_bili_scatter,
    alt_bili_scatter = alt_bili_scatter,
    alt_max_stdy    = alt_max_stdy,
    ast_max_stdy    = ast_max_stdy,
    alp_max_stdy    = alp_max_stdy,
    tb_max_stdy     = tb_max_stdy,
    hylawind        = hylawind,
    blconflict      = blconflict,
    bkc_count       = bkc_count
  )
}


# =============================================================================
# liver_output — Excel workbook generation
# =============================================================================
#' Migrate SAS %liver_output (lines 1495-1627). Writes 13+ worksheet Excel
#' workbook using openxlsx (replacing SAS PCFILES/JET engine).
#'
#' @param outfmt_data Named list from liver_outfmt().
#' @param config      Named list from liver_params().
#'
#' @return Invisible path to the output workbook file.
liver_output <- function(outfmt_data, config) {

  cli::cli_alert_info("Writing liver lab Excel workbook.")

  wb <- openxlsx::createWorkbook()

  # Style gallery for header formatting
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign         = "center",
    border         = "Bottom",
    borderStyle    = "thin"
  )

  # Helper: add a worksheet with data and header styling
  add_sheet <- function(sheet_name, data) {
    if (is.null(data) || !is.data.frame(data)) {
      data <- tibble::tibble(message = "No data available")
    }
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, data)
    if (ncol(data) > 0L && nrow(data) > 0L) {
      openxlsx::addStyle(wb, sheet_name, header_style,
                         rows = 1L, cols = seq_len(ncol(data)),
                         gridExpand = TRUE)
    }
  }

  # Worksheet 1: treatment_arms
  add_sheet("treatment_arms", outfmt_data$treatment_arms)

  # Worksheet 2: data_lab_table — ULN frequency tables
  add_sheet("data_lab_table", outfmt_data$lab_tables)

  # Worksheet 3: data_hy_s_law — DILI/Hy's Law summary
  add_sheet("data_hy_s_law", outfmt_data$hyslaw)

  # Worksheet 4: data_max_vs_bl — Baseline vs Maximum cross-tab
  add_sheet("data_max_vs_bl", outfmt_data$max_bl_tables)

  # Worksheet 5: data_ast_vs_bili — AST vs BILI scatter data
  add_sheet("data_ast_vs_bili", outfmt_data$ast_bili_scatter)

  # Worksheet 6: data_alt_vs_bili — ALT vs BILI scatter data
  add_sheet("data_alt_vs_bili", outfmt_data$alt_bili_scatter)

  # Worksheets 7-10: Max lab by study day (one per test)
  add_sheet("data_alt_max_stdy", outfmt_data$alt_max_stdy)
  add_sheet("data_ast_max_stdy", outfmt_data$ast_max_stdy)
  add_sheet("data_alp_max_stdy", outfmt_data$alp_max_stdy)
  add_sheet("data_tb_max_stdy",  outfmt_data$tb_max_stdy)

  # Worksheet 11: data_Hy — Hy's Law subject detail
  add_sheet("data_Hy", outfmt_data$hylawind)

  # Worksheet 12: data_BLconflict — Baseline conflicts
  add_sheet("data_BLconflict", outfmt_data$blconflict)

  # Worksheet 12b: data_BLconflictS — BL conflict count summary
  blconflict_s <- tibble::tibble(
    data = "bkc_count",
    val  = as.integer(outfmt_data$bkc_count)
  )
  add_sheet("data_BLconflictS", blconflict_s)

  # Worksheet 13: information — Metadata sheet
  info <- tibble::tibble(
    path = c(
      config$nda_number,
      config$study_id,
      paste0(format(Sys.Date(), "%Y-%m-%d"), " ", format(Sys.time(), "%I:%M %p")),
      "",
      if (isTRUE(config$dm_actarm)) "ACTARM" else "ARM"
    )
  )
  add_sheet("information", info)

  # Apply page setup for consistent formatting (from xml_output.R if available)
  for (sn in openxlsx::sheets(wb)) {
    tryCatch(
      apply_page_setup(wb, sn, orientation = "landscape"),
      error = function(e) {
        openxlsx::pageSetup(wb, sn, orientation = "landscape")
      }
    )
  }

  # Save workbook
  output_file <- file.path(
    config$output_path,
    paste0("liver_", gsub("[^A-Za-z0-9_.-]", "", config$study_id), ".xlsx")
  )

  # Ensure output directory exists
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  cli::cli_alert_success("Liver Lab workbook saved to {output_file}")

  invisible(output_file)
}


# =============================================================================
# liver — Main orchestrator
# =============================================================================
#' Migrate SAS %liver (lines 1633-1677). Top-level function orchestrating the
#' entire liver lab analysis pipeline.
#'
#' @param data_path       Character. Path to XPT data directory.
#' @param output_path     Character. Path for output workbooks.
#' @param r_macros_path   Character. Path to R macro functions.
#' @param r_utilities_path Character. Path to R utility functions.
#' @param nda_number      Character. NDA/BLA number.
#' @param study_id        Character. Study identifier.
#' @param run_location    Character. "LOCAL" or "LAUNCHER".
#' @param log_path        Character or NULL. Path for log output.
#'
#' @return Invisible output data list (or NULL on failure).
liver <- function(data_path        = ".",
                  output_path      = ".",
                  r_macros_path    = "tested/R/macros",
                  r_utilities_path = "tested/R/utilities",
                  nda_number       = "",
                  study_id         = "",
                  run_location     = "LOCAL",
                  log_path         = NULL) {

  cli::cli_alert_info("LIVER LAB ANALYSIS PANEL")

  # Step 0: Configuration
 config <- liver_params(
    data_path        = data_path,
    output_path      = output_path,
    r_macros_path    = r_macros_path,
    r_utilities_path = r_utilities_path,
    nda_number       = nda_number,
    study_id         = study_id,
    run_location     = run_location,
    log_path         = log_path
  )

  # Source dependent R utility files (parameterized via config)
  tryCatch({
    source(file.path(config$r_macros_path, "data_checks_liver.R"), local = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not source data_checks_liver.R: {e$message}")
  })
  tryCatch({
    source(file.path(config$r_utilities_path, "data_checks.R"), local = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not source data_checks.R: {e$message}")
  })
  tryCatch({
    source(file.path(config$r_utilities_path, "err_output.R"), local = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not source err_output.R: {e$message}")
  })
  tryCatch({
    source(file.path(config$r_utilities_path, "sl_gs_output.R"), local = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not source sl_gs_output.R: {e$message}")
  })
  tryCatch({
    source(file.path(config$r_utilities_path, "xml_output.R"), local = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not source xml_output.R: {e$message}")
  })

  # Step 1: Load data
  dm_file <- file.path(config$data_path, "dm.xpt")
  lb_file <- file.path(config$data_path, "lb.xpt")

  if (!file.exists(dm_file)) {
    cli::cli_alert_danger("DM dataset not found: {dm_file}")
    return(invisible(NULL))
  }
  if (!file.exists(lb_file)) {
    cli::cli_alert_danger("LB dataset not found: {lb_file}")
    return(invisible(NULL))
  }

  dm <- haven::read_xpt(dm_file)
  lb <- haven::read_xpt(lb_file)

  # Step 2: Preliminary checks
  prelim <- liver_prelim(dm, lb, config)

  if (!prelim$ok) {
    # Error path: write error summary and return
    cli::cli_alert_danger("Preliminary checks failed. Writing error summary.")
    err_file <- file.path(config$output_path,
                          paste0("liver_err_", config$study_id, ".xlsx"))
    tryCatch({
      error_summary(
        err_file        = err_file,
        panel_title     = config$panel_title,
        ndabla          = config$nda_number,
        studyid         = config$study_id,
        err_nosubj      = !prelim$dm_subj_gt0,
        err_missvar     = !prelim$all_req_var,
        err_desc        = if (!is.null(prelim$err_liver_lbtest)) prelim$err_liver_lbtest else "",
        rpt_chk_var_req = prelim$rpt_chk_var
      )
    }, error = function(e) {
      cli::cli_alert_danger("Failed to write error summary: {e$message}")
    })
    # Close log if active
    if (!is.null(config$log_path) && nzchar(config$log_path)) {
      tryCatch(sink(), error = function(e) NULL)
    }
    return(invisible(NULL))
  }

  # Step 3: Data setup
  setup_data <- liver_setup(
    dm         = prelim$dm,
    lb         = prelim$lb,
    config     = config,
    dm_actarm  = prelim$dm_actarm,
    opt_flags  = prelim$opt_flags
  )

  # Step 4: Per-arm analysis
  arm_results <- liver_arm(setup_data, config)

  # Step 5: Output formatting (includes liver_one calls internally)
  outfmt_data <- liver_outfmt(arm_results, setup_data, config)

  # Step 6: Data quality checks (from data_checks_liver.R)
  tryCatch({
    lbstat_avail   <- isTRUE(prelim$opt_flags$lbstat)
    lbreasnd_avail <- isTRUE(prelim$opt_flags$lbreasnd)

    liver_check_results <- liver_check(
      lb         = setup_data$lb_merged,
      dm         = setup_data$dm,
      arm_data   = setup_data$treatment_arms,
      alt_codes  = LIVER_CODES$ALT,
      ast_codes  = LIVER_CODES$AST,
      alp_codes  = LIVER_CODES$ALP,
      bili_codes = LIVER_CODES$BILI,
      lbstat_available   = lbstat_avail,
      lbreasnd_available = lbreasnd_avail
    )
  }, error = function(e) {
    cli::cli_alert_warning("Liver data checks encountered error: {e$message}")
  })

  # Step 7: Script Launcher metadata (from sl_gs_output.R)
  tryCatch({
    gs_result <- group_subset_pp(
      sl_group  = config$sl_group_by,
      sl_subset = config$sl_subset_by
    )
  }, error = function(e) {
    cli::cli_alert_warning("Script Launcher preprocessing skipped: {e$message}")
  })

  # Step 8: Write Excel output
  output_file <- liver_output(outfmt_data, config)

  # Step 9: Script Launcher Excel output (if in Launcher context)
  if (config$run_location == "LAUNCHER") {
    tryCatch({
      gs_file <- file.path(config$output_path,
                           paste0("liver_", config$study_id, ".xlsx"))
      if (exists("gs_result") && exists("group_subset_write_xlsx")) {
        group_subset_write_xlsx(gs_file = gs_file, pp_result = gs_result)
      }
    }, error = function(e) {
      cli::cli_alert_warning("Script Launcher output skipped: {e$message}")
    })
  }

  # Step 10: Close logging if active
  if (!is.null(config$log_path) && nzchar(config$log_path)) {
    tryCatch(sink(), error = function(e) NULL)
  }

  cli::cli_alert_success("Liver Lab Analysis Panel completed successfully.")
  invisible(outfmt_data)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#   - LBTESTCD values are compared case-insensitively (toupper()) to match
#     SAS behavior
#   - LBSTNRHI is used as the upper limit of normal; records with missing or
#     zero LBSTNRHI are excluded from analysis
#   - Baseline flag derivation (when LBBLFL missing) uses last observation
#     with LBDY <= 1 per USUBJID (NOT per USUBJID x test, matching SAS)
#   - Treatment arm assignment uses ACTARM if available, otherwise ARM
#     (mirroring SAS IF-THEN logic)
#   - Denominator for percentage calculations is per-arm subject count from
#     DM, NOT LB record count
#   - ULN threshold flags use >= (GE) matching SAS conditional logic
#     (e.g., ALTx2 = 1 if ALT_RES >= ALT_HI*2)
#   - ALP has three states: Normal (ALP <= ULN), >=HI (ALP >= ULN),
#     Missing (ALP is NA) — not binary
#   - DILI computed two ways: at-any-visit (same-visit conditions) and
#     during-study (conditions at any time). Both are output.
#   - SCRNFAIL/NOTASSGN arms are excluded if ARMCD variable exists
#   - Inner join of DM and LB on USUBJID means orphan lab records are
#     excluded from analysis
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   - Rounding: All percentages use janitor::round_half_up() to match SAS
#     round-half-up behavior. R default round() uses banker's rounding
#     (round-half-to-even) which would produce different results.
#   - Sort stability: R dplyr::arrange() is stable within groups but
#     multi-key sort order may differ from SAS PROC SORT for tied values.
#     This affects which record is selected as baseline when multiple
#     records have the same LBDY.
#   - Floating point: ULN ratio comparisons (e.g., >= 3) may differ at
#     machine epsilon boundaries. SAS and R both use IEEE 754 doubles but
#     comparison operators may handle edge cases differently.
#   - PROC FREQ vs dplyr: SAS PROC FREQ counts exclude missing by default;
#     R sum() with na.rm = TRUE achieves same exclusion via different mechanism.
#   - Cross-tab bins: SAS uses right-open intervals [lower, upper) for
#     baseline vs max bins. R cut() with right=FALSE achieves the same.
#
# NO DIRECT R EQUIVALENT:
#   - SAS PCFILES/JET engine for direct Excel writes -> replaced by
#     openxlsx::saveWorkbook()
#   - SAS SpreadsheetML XML generation (xml_output.sas) -> replaced by
#     openxlsx API
#   - SAS Script Launcher integration (sl_gs_output.sas) -> R wrapper
#     functions that produce equivalent metadata tibbles but do not interact
#     with a SAS Script Launcher
#   - SAS PROC PRINTTO for log redirection -> R sink() for output capture
#   - SAS %find_null_col for dropping all-missing columns -> custom R
#     function using vapply(!all(is.na()))
#   - SAS word-by-word PROPCASE with unit-aware exceptions -> custom R
#     propcase_arm() function
#
# PACKAGE SELECTION RATIONALE:
#   - haven (2.5.5): CRAN standard for SAS data I/O; reads XPT natively
#   - dplyr (>= 1.1.0): Tidyverse core; replaces DATA step merges,
#     filtering, aggregation
#   - tidyr (>= 1.3.0): Pivoting for cross-tabulation assembly and DILI
#     at-any-visit visit-level analysis
#   - openxlsx (>= 4.2.5): Replaces SAS PCFILES/JET and SpreadsheetML
#     for Excel output
#   - janitor (>= 2.2.0): round_half_up() for SAS-compatible rounding
#   - stringr (>= 1.5.0): str_to_title() replacing SAS PROPCASE,
#     str_sub() for date string parsing
#   - purrr (>= 1.0.0): Functional iteration over arms and tests
#     (replacing SAS macro loops)
#   - cli (>= 3.6.0): Informative console messages and error formatting
#   - forcats (>= 1.0.0): Factor level management for ordered ULN
#     categories in cross-tabs
#   - lubridate (>= 1.9.0): Date handling support (loaded for consistency)
#   - tibble (>= 3.2.0): Enhanced data frames for config and metadata
#
# OPEN QUESTIONS:
#   - Confirm whether ALP NA (no ALP data) should qualify as a distinct
#     state for Hy's Law classification (SAS treats as separate ALP_miss
#     category)
#   - Verify baseline derivation logic when multiple records have identical
#     LBDY values — SAS uses first.USUBJID after descending LBDY sort
#   - Confirm ULN threshold boundaries: SAS source uses ">=" (GE), which
#     is preserved here. The AAP agent_prompt initially suggested ">"
#     but the SAS source is authoritative.
#   - Determine handling of subjects appearing in LB but not in DM (orphan
#     lab records) — current inner join excludes them, matching SAS behavior
#   - Review whether LBDY day-1 adjustment (no day 0) matches study
#     protocol conventions
#   - At-any-visit DILI computation requires visit-level data (VISITNUM or
#     LBDY); if neither available, falls back to during-study logic
# ============================================================
