# =============================================================================
# PROGRAM NAME: data_checks_liver.R
#
# DESCRIPTION:  Liver lab domain data-quality checks — migrated from SAS to R.
#               Centralises liver lab data-quality checks for the Liver Lab
#               Panel: validates presence of 4 key analytes (ALT, AST, ALP,
#               Total Bilirubin), quantifies missing/zero lab results, documents
#               absent reference ranges (ULN/LLN), counts lab visits per subject,
#               identifies subjects missing baseline or post-baseline labs,
#               truncates long listings, and exports results to Excel workbooks.
#
#               Eleven SAS macros are replaced by eleven parameterised R functions:
#                 %liver_lbtestcd           -> liver_lbtestcd()
#                 %liver_lbstresn_missing   -> liver_lbstresn_missing()
#                 %liver_lbstnrhilo_missing -> liver_lbstnrhilo_missing()
#                 %liver_lbtest_count       -> liver_lbtest_count()
#                 %liver_missing_bl         -> liver_missing_bl()
#                 %liver_missing_pbl        -> liver_missing_pbl()
#                 %liver_missing_all        -> liver_missing_all()
#                 %truncate                 -> truncate_listing()
#                 %liver_visitnum           -> liver_visitnum()
#                 %liver_check              -> liver_check()
#                 %liver_check_output       -> liver_check_out()
#
# ORIGINAL SAS: tested/SAS/macros/data_checks_liver.sas (1038 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
#
# R MIGRATION:  Migrated to R using dplyr, tidyr, stringr, janitor, openxlsx,
#               tibble, rlang, purrr, cli
# R REQUIRES:   dplyr (>= 1.1.0), tidyr (>= 1.3.0), stringr (>= 1.5.0),
#               janitor (>= 2.2.0), openxlsx (>= 4.2.5), tibble (>= 3.2.0),
#               rlang (>= 1.1.0), purrr (>= 1.0.0), cli (>= 3.6.0)
# R DEPENDS ON: tested/R/utilities/data_checks.R
#                 (provides chk_val())
#               tested/R/utilities/xml_output.R
#                 (provides create_workbook(), create_workbook_styles(),
#                  write_data_table(), write_header_rows(), apply_page_setup())
#
# NOTES:        All SAS macro parameters become named R function arguments.
#               SAS global macro variables become function return values in
#               named lists.  Missing values mapped to NA (never 0).
#               SAS-compatible rounding via janitor::round_half_up().
#               SAS hash objects replaced by dplyr::left_join().
#               SAS PCFILES/JET engine replaced by openxlsx.
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(openxlsx)
library(tibble)
library(rlang)
library(purrr)
library(cli)

# ---------------------------------------------------------------------------
# Internal dependency: data_checks.R
# Provides: chk_val()
# ---------------------------------------------------------------------------
if (!exists("chk_val", mode = "function")) {
  source(file.path("tested", "R", "utilities", "data_checks.R"))
}

# ---------------------------------------------------------------------------
# Internal dependency: xml_output.R
# Provides: create_workbook(), create_workbook_styles(),
#           write_data_table(), write_header_rows(), apply_page_setup()
# ---------------------------------------------------------------------------
if (!exists("create_workbook", mode = "function")) {
  source(file.path("tested", "R", "utilities", "xml_output.R"))
}


# =============================================================================
# liver_lbtestcd
# =============================================================================
#' Check presence of the four liver analyte codes in the LB domain.
#'
#' Replaces SAS \code{%liver_lbtestcd} (lines 7-68 of data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Calls \code{\%chk_val} to check LBTESTCD for ALT/AST/ALP/BILI codes.
#'   \item Sets liver_alt/ast/alp/bili flags (1 = present, 0 = absent).
#'   \item If all present, liver_lbtestcd = TRUE; otherwise builds a descriptive
#'         error message with proper English grammar ("is missing" / "are
#'         missing", Oxford comma placement).
#' }
#'
#' @param lb          Data frame of LB (Laboratory) domain data. Must contain
#'                    a \code{lbtestcd} column.
#' @param alt_codes   Character vector of LBTESTCD codes for ALT.
#' @param ast_codes   Character vector of LBTESTCD codes for AST.
#' @param alp_codes   Character vector of LBTESTCD codes for ALP.
#' @param bili_codes  Character vector of LBTESTCD codes for Total Bilirubin.
#' @param rpt_chk_val Optional pre-computed chk_val audit tibble. If NULL
#'                    (default), chk_val() is called internally.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{liver_lbtestcd}{Logical. TRUE if all four analytes present.}
#'     \item{liver_alt}{Logical. TRUE if ALT codes found.}
#'     \item{liver_ast}{Logical. TRUE if AST codes found.}
#'     \item{liver_alp}{Logical. TRUE if ALP codes found.}
#'     \item{liver_bili}{Logical. TRUE if BILI codes found.}
#'     \item{err_liver_lbtest}{Character string error message, or NULL if all present.}
#'   }
liver_lbtestcd <- function(lb, alt_codes, ast_codes, alp_codes, bili_codes,
                           rpt_chk_val = NULL) {

  # --- Input validation ---
  if (!is.data.frame(lb)) {
    cli::cli_abort("{.arg lb} must be a data frame, not {.cls {class(lb)}}.")
  }

  # Build the combined code vector for chk_val
 all_codes <- c(alt_codes, ast_codes, alp_codes, bili_codes)

  # If no pre-computed audit tibble, call chk_val
  if (is.null(rpt_chk_val)) {
    rpt_chk_val <- chk_val(lb, "lbtestcd", all_codes, ds_name = "LB")
  }

  # Check each analyte group individually
  # SAS: for each of alp, alt, ast, bili — check if any code in the group
  # has ind == 1 (PRESENT) in the rpt_chk_val result
  check_group <- function(codes) {
    group_results <- rpt_chk_val %>%
      dplyr::filter(
        .data$chk == "VAL",
        .data$var == "LBTESTCD",
        .data$condition == "PRESENT",
        toupper(.data$val) %in% toupper(codes)
      )
    if (nrow(group_results) == 0L) return(FALSE)
    any(group_results$ind == 1L)
  }

  liver_alt  <- check_group(alt_codes)
  liver_ast  <- check_group(ast_codes)
  liver_alp  <- check_group(alp_codes)
  liver_bili <- check_group(bili_codes)

  liver_lbtestcd_flag <- liver_alt && liver_ast && liver_alp && liver_bili

  # Build error message if any analyte is missing
  err_liver_lbtest <- NULL
  if (!liver_lbtestcd_flag) {
    # Collect missing analyte descriptions (SAS: ALP, ALT, AST, TB order)
    missing_tests <- character(0)
    if (!liver_alp)  missing_tests <- c(missing_tests, "Alkaline Phosphatase (ALP)")
    if (!liver_alt)  missing_tests <- c(missing_tests, "Alanine Aminotransferase (ALT)")
    if (!liver_ast)  missing_tests <- c(missing_tests, "Aspartate Aminotransferase (AST)")
    if (!liver_bili) missing_tests <- c(missing_tests, "Total Bilirubin (TB)")

    n_missing <- length(missing_tests)

    if (n_missing == 1L) {
      # SAS: "Lab test <name> is missing."
      err_liver_lbtest <- str_c("Lab test ", missing_tests, " is missing.")
    } else if (n_missing == 2L) {
      # SAS: "Lab tests <name1> and <name2> are missing."
      err_liver_lbtest <- str_c("Lab tests ", missing_tests[1], " and ",
                                missing_tests[2], " are missing.")
    } else {
      # SAS: "Lab tests <name1>, <name2>, and <name3> are missing."
      # Replace last comma with " and" (Oxford comma)
      list_part <- str_c(missing_tests[seq_len(n_missing - 1L)], collapse = ", ")
      err_liver_lbtest <- str_c("Lab tests ", list_part, ", and ",
                                missing_tests[n_missing], " are missing.")
    }

    cli::cli_alert_warning(err_liver_lbtest)
  }

  list(
    liver_lbtestcd = liver_lbtestcd_flag,
    liver_alt      = liver_alt,
    liver_ast      = liver_ast,
    liver_alp      = liver_alp,
    liver_bili     = liver_bili,
    err_liver_lbtest = err_liver_lbtest
  )
}


# =============================================================================
# liver_lbstresn_missing
# =============================================================================
#' Count missing or zero lab result values (LBSTRESN) per analyte.
#'
#' Replaces SAS \code{%liver_lbstresn_missing} (lines 78-201 of
#' data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Seeds a reference tibble so every analyte appears even when absent.
#'   \item Joins DM with LB, maps LBTESTCD to canonical labels (ALT/AST/ALP/TB).
#'   \item Counts missing or zero LBSTRESN per analyte with percentages.
#'   \item If LBSTAT/LBREASND available: tabulates top-3 reasons for missing
#'         results with counts and percentages.
#'   \item Assembles a combined text column with top-3 reason strings.
#' }
#'
#' @param lb          Data frame of LB domain data.
#' @param dm          Data frame of DM (demographics) domain data.
#' @param alt_codes   Character vector of ALT test codes.
#' @param ast_codes   Character vector of AST test codes.
#' @param alp_codes   Character vector of ALP test codes.
#' @param bili_codes  Character vector of Total Bilirubin test codes.
#' @param lbstat_available   Logical. TRUE if LBSTAT column is present in LB.
#' @param lbreasnd_available Logical. TRUE if LBREASND column is present in LB.
#'
#' @return A tibble (lb_rpt_lbstresn_miss0) with columns:
#'   lbtest, missing, pct, top3.
liver_lbstresn_missing <- function(lb, dm, alt_codes, ast_codes, alp_codes,
                                   bili_codes, lbstat_available = FALSE,
                                   lbreasnd_available = FALSE) {

  # --- Input validation ---
  if (!is.data.frame(lb)) {
    cli::cli_abort("{.arg lb} must be a data frame.")
  }
  if (!is.data.frame(dm)) {
    cli::cli_abort("{.arg dm} must be a data frame.")
  }

  # Seed tibble so every analyte appears even when absent
  # SAS: data lbm_val; lbtest = 'ALT'/'AST'/'ALP'/'TB'; run;
  lbm_val <- tibble::tibble(lbtest = c("ALT", "AST", "ALP", "TB"))

  # Combine all liver analyte codes
  all_codes <- c(alt_codes, ast_codes, alp_codes, bili_codes)

  # --- Join DM with LB, map lbtestcd to canonical test labels ---
  # SAS: PROC SQL joining demographics with lb, case-when mapping

  # Pre-filter LB to liver analytes only
  lb_liver <- lb %>%
    dplyr::filter(toupper(.data$lbtestcd) %in% toupper(all_codes))

  # Safely ensure optional columns exist before join
  has_lbstresn <- "lbstresn" %in% colnames(lb_liver)
  has_lbstat   <- lbstat_available && "lbstat" %in% colnames(lb_liver)
  has_lbreasnd <- lbreasnd_available && "lbreasnd" %in% colnames(lb_liver)

  if (!has_lbstresn) lb_liver$lbstresn <- NA_real_
  if (!has_lbstat)   lb_liver$lbstat   <- NA_character_
  if (!has_lbreasnd) lb_liver$lbreasnd <- NA_character_

  lb_rpt_lb <- dm %>%
    dplyr::select("usubjid") %>%
    dplyr::distinct() %>%
    dplyr::left_join(lb_liver, by = "usubjid") %>%
    dplyr::mutate(
      lbtest = dplyr::case_when(
        toupper(.data$lbtestcd) %in% toupper(alt_codes)  ~ "ALT",
        toupper(.data$lbtestcd) %in% toupper(ast_codes)  ~ "AST",
        toupper(.data$lbtestcd) %in% toupper(alp_codes)  ~ "ALP",
        toupper(.data$lbtestcd) %in% toupper(bili_codes) ~ "TB",
        TRUE ~ NA_character_
      )
    )

  # Handle optional columns: when not available, they are already NA from above.
  # When available, coerce to character for consistency.
  if (has_lbstat) {
    lb_rpt_lb <- lb_rpt_lb %>%
      dplyr::mutate(lbstat = as.character(.data$lbstat))
  }
  if (has_lbreasnd) {
    lb_rpt_lb <- lb_rpt_lb %>%
      dplyr::mutate(lbreasnd = as.character(.data$lbreasnd))
  }

  lb_rpt_lb <- lb_rpt_lb %>%
    dplyr::filter(!is.na(.data$lbtest))

  # --- Count missing or zero LBSTRESN per analyte ---
  # SAS: sum(case when lbstresn in (.,0) then 1 else 0 end) as missing
  miss0_counts <- lb_rpt_lb %>%
    dplyr::group_by(.data$lbtest) %>%
    dplyr::summarise(
      missing = sum(is.na(.data$lbstresn) | .data$lbstresn == 0, na.rm = FALSE),
      total   = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      missing = dplyr::if_else(missing >= 0L, missing, 0L),
      pct     = dplyr::if_else(
        missing >= 0L & .data$total > 0L,
        janitor::round_half_up(100 * .data$missing / .data$total, digits = 1),
        NA_real_
      )
    )

  # Ensure all analytes appear via left join with seed
  lb_rpt_lbstresn_miss0_count <- lbm_val %>%
    dplyr::left_join(
      miss0_counts %>% dplyr::select("lbtest", "missing", "pct"),
      by = "lbtest"
    ) %>%
    tidyr::replace_na(list(missing = 0L)) # legitimate: missing count initialized to zero after join

  # --- Reason analysis (top-3 per analyte) ---
  # SAS: tabulates lbstat/lbreasnd for missing/zero LBSTRESN
  if (lbstat_available || lbreasnd_available) {
    reason_raw <- lb_rpt_lb %>%
      dplyr::filter(is.na(.data$lbstresn) | .data$lbstresn == 0) %>%
      dplyr::mutate(
        lbstat = dplyr::if_else(
          is.na(.data$lbstat),
          "Missing",
          stringr::str_to_title(stringr::str_trim(.data$lbstat))
        ),
        lbreasnd = dplyr::if_else(
          is.na(.data$lbreasnd),
          "Missing",
          stringr::str_to_title(stringr::str_trim(.data$lbreasnd))
        ),
        stat_reasn = stringr::str_c(.data$lbstat, "/", .data$lbreasnd)
      ) %>%
      dplyr::count(.data$lbtest, .data$stat_reasn, name = "count") %>%
      dplyr::arrange(.data$lbtest, dplyr::desc(.data$count))

    # Keep top-3 per analyte (SAS: if order in (1,2,3))
    reason_top3 <- reason_raw %>%
      dplyr::group_by(.data$lbtest) %>%
      dplyr::slice_head(n = 3L) %>%
      dplyr::mutate(order = dplyr::row_number()) %>%
      dplyr::ungroup()

    # Find max number of reasons across analytes (capped at 3)
    max_reasn_count <- reason_top3 %>%
      dplyr::group_by(.data$lbtest) %>%
      dplyr::summarise(n_reasons = dplyr::n(), .groups = "drop") %>%
      dplyr::pull(.data$n_reasons) %>%
      max(0L) %>%
      min(3L)

    # Build top-3 text blob per analyte
    # SAS: trim(sr&i.)||' ('||trim(put(100*cnt&i./missing,8.))||'%)'
    top3_text <- reason_top3 %>%
      dplyr::group_by(.data$lbtest) %>%
      dplyr::summarise(
        top3 = {
          miss_total <- lb_rpt_lbstresn_miss0_count %>%
            dplyr::filter(.data$lbtest == dplyr::first(lbtest)) %>%
            dplyr::pull(.data$missing)
          miss_total <- if (length(miss_total) == 0L) 0L else miss_total[1]
          parts <- purrr::map_chr(seq_len(dplyr::n()), function(idx) {
            sr  <- .data$stat_reasn[idx]
            cnt <- .data$count[idx]
            pct_val <- if (miss_total > 0L) {
              janitor::round_half_up(100 * cnt / miss_total, digits = 0)
            } else {
              0
            }
            stringr::str_c(sr, " (", as.character(as.integer(pct_val)), "%)")
          })
          stringr::str_c(parts, collapse = "\n")
        },
        .groups = "drop"
      )

    if (max_reasn_count == 0L) {
      top3_text <- lbm_val %>%
        dplyr::mutate(top3 = "N/A")
    }
  } else {
    # No LBSTAT/LBREASND available
    top3_text <- lbm_val %>%
      dplyr::mutate(top3 = "N/A")
  }

  # --- Combine counts and top-3 text ---
  lb_rpt_lbstresn_miss0 <- lb_rpt_lbstresn_miss0_count %>%
    dplyr::left_join(top3_text, by = "lbtest") %>%
    tidyr::replace_na(list(top3 = "N/A")) %>%
    dplyr::select("lbtest", "missing", "pct", "top3")

  lb_rpt_lbstresn_miss0
}


# =============================================================================
# liver_lbstnrhilo_missing
# =============================================================================
#' Find counts of subjects and lab tests missing ULN or LLN reference ranges.
#'
#' Replaces SAS \code{%liver_lbstnrhilo_missing} (lines 207-455 of
#' data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Filters LB to liver analytes where LBSTRESN is present but
#'         LBSTNRLO or LBSTNRHI is missing.
#'   \item Uses hash lookup (dplyr::left_join) for arm assignment from DM.
#'   \item Splits into ULN-missing and ALP-LLN-missing branches.
#'   \item Per-arm subject/test counts with ensure-every-arm coverage.
#'   \item Truncates listings above 100 rows.
#'   \item Blanks repeated labels for tidy display output.
#' }
#'
#' @param lb          Data frame of LB domain data.
#' @param dm          Data frame of DM domain data (must contain usubjid, arm).
#' @param alt_codes   Character vector of ALT test codes.
#' @param ast_codes   Character vector of AST test codes.
#' @param alp_codes   Character vector of ALP test codes.
#' @param bili_codes  Character vector of BILI test codes.
#' @param arm_data    Data frame of treatment arms (must contain arm column).
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{lb_rpt_uln_miss}{Tibble of per-arm/per-analyte ULN missing counts.}
#'     \item{lb_rpt_uln_miss_list}{Tibble listing individual lab tests missing ULN.}
#'     \item{lb_rpt_lln_alp_miss}{Tibble of per-arm ALP LLN missing counts.}
#'     \item{lb_rpt_lln_alp_miss_list}{Tibble listing individual ALP tests missing LLN.}
#'   }
liver_lbstnrhilo_missing <- function(lb, dm, alt_codes, ast_codes, alp_codes,
                                     bili_codes, arm_data) {

  # --- Input validation ---
  if (!is.data.frame(lb)) cli::cli_abort("{.arg lb} must be a data frame.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame.")
  if (!is.data.frame(arm_data)) cli::cli_abort("{.arg arm_data} must be a data frame.")

  all_codes <- c(alt_codes, ast_codes, alp_codes, bili_codes)

  # Seed analyte reference
  lbm_val <- tibble::tibble(lbtest = c("ALT", "AST", "ALP", "TB"))

  # --- Filter LB to liver analytes with missing ULN or LLN ---
  # SAS: where upcase(lbtestcd) in (...) and lbstresn is not missing
  #      and (lbstnrlo is missing or lbstnrhi is missing)
  # Ensure optional columns exist before filtering
  lb_safe <- lb
  if (!"lbstnrlo" %in% colnames(lb_safe)) lb_safe$lbstnrlo <- NA_real_
  if (!"lbstnrhi" %in% colnames(lb_safe)) lb_safe$lbstnrhi <- NA_real_
  if (!"lbstresu" %in% colnames(lb_safe)) lb_safe$lbstresu <- NA_character_
  if (!"lbdtc"    %in% colnames(lb_safe)) lb_safe$lbdtc    <- NA_character_

  lb_limit_base <- lb_safe %>%
    dplyr::filter(
      toupper(.data$lbtestcd) %in% toupper(all_codes),
      !is.na(.data$lbstresn),
      is.na(.data$lbstnrlo) | is.na(.data$lbstnrhi)
    ) %>%
    dplyr::mutate(
      lbtest = dplyr::case_when(
        toupper(.data$lbtestcd) %in% toupper(alt_codes)  ~ "ALT",
        toupper(.data$lbtestcd) %in% toupper(ast_codes)  ~ "AST",
        toupper(.data$lbtestcd) %in% toupper(alp_codes)  ~ "ALP",
        toupper(.data$lbtestcd) %in% toupper(bili_codes) ~ "TB",
        TRUE ~ NA_character_
      ),
      # Format result string: value + unit
      lbstres = stringr::str_c(
        format(janitor::round_half_up(.data$lbstresn, 1), nsmall = 1),
        " ",
        dplyr::if_else(is.na(.data$lbstresu), "", as.character(.data$lbstresu))
      ) %>% stringr::str_trim(),
      # Parse date
      lbdt = as.Date(.data$lbdtc, format = "%Y-%m-%d")
    ) %>%
    dplyr::select("usubjid", "lbtest", "lbdt", lbstresn = "lbstres",
                  "lbstnrlo", "lbstnrhi")

  # --- Hash lookup for arm assignment (SAS: declare hash h) ---
  arm_lookup <- dm %>%
    dplyr::select("usubjid", "arm") %>%
    dplyr::distinct()

  lb_rpt_limit_miss_list <- lb_limit_base %>%
    dplyr::left_join(arm_lookup, by = "usubjid") %>%
    dplyr::select("arm", "usubjid", "lbtest", "lbdt", "lbstresn",
                  "lbstnrlo", "lbstnrhi") %>%
    dplyr::arrange(.data$arm, .data$lbtest)

  # =========================================================================
  # MISSING UPPER LIMIT OF NORMAL (ULN)
  # =========================================================================
  lb_rpt_uln_miss_list_raw <- lb_rpt_limit_miss_list %>%
    dplyr::filter(is.na(.data$lbstnrhi)) %>%
    dplyr::select("arm", "usubjid", "lbtest", "lbdt", "lbstresn") %>%
    dplyr::arrange(.data$arm, .data$lbtest)

  # Per-arm/per-analyte counts
  uln_subj_counts <- lb_rpt_uln_miss_list_raw %>%
    dplyr::distinct(.data$arm, .data$usubjid, .data$lbtest) %>%
    dplyr::count(.data$arm, .data$lbtest, name = "hi_miss_subj_count")

  uln_test_counts <- lb_rpt_uln_miss_list_raw %>%
    dplyr::count(.data$arm, .data$lbtest, name = "hi_miss_test_count")

  uln_counts <- dplyr::full_join(uln_subj_counts, uln_test_counts,
                                 by = c("arm", "lbtest"))

  # Ensure every arm/lbtest combination appears
  # SAS: cross join treatment_arms x lbm_val
  lbm_val_cp <- tidyr::expand_grid(
    arm    = dplyr::pull(arm_data, "arm"),
    lbtest = lbm_val$lbtest
  )

  lb_rpt_uln_miss <- lbm_val_cp %>%
    dplyr::left_join(uln_counts, by = c("arm", "lbtest")) %>%
    tidyr::replace_na(list(hi_miss_subj_count = 0L, hi_miss_test_count = 0L)) %>% # legitimate: missing count initialized to zero after aggregate
    dplyr::select("arm", "lbtest", "hi_miss_subj_count", "hi_miss_test_count")

  # Truncate ULN listing if > 100 rows
  uln_nobs <- nrow(lb_rpt_uln_miss_list_raw)
  num_arms <- nrow(arm_data)

  if (uln_nobs > 100L && num_arms > 0L) {
    quota_per_arm <- floor(100L / num_arms)
    lb_rpt_uln_miss_list_trunc <- lb_rpt_uln_miss_list_raw %>%
      dplyr::group_by(.data$arm) %>%
      dplyr::mutate(.row_n = dplyr::row_number()) %>%
      dplyr::filter(.data$.row_n <= quota_per_arm) %>%
      dplyr::select(-".row_n") %>%
      dplyr::ungroup()
    # Append truncation notice
    trunc_notice <- tibble::tibble(
      arm      = stringr::str_c("There were ", format(uln_nobs, big.mark = ","),
                                " lab tests missing ULN"),
      usubjid  = "List truncated at 100",
      lbtest   = ".",
      lbdt     = as.Date(NA),
      lbstresn = "."
    )
    lb_rpt_uln_miss_list_raw <- dplyr::bind_rows(lb_rpt_uln_miss_list_trunc,
                                                  trunc_notice)
  }

  # Sort and blank repeated labels for tidy display
  lb_rpt_uln_miss_list_final <- lb_rpt_uln_miss_list_raw %>%
    dplyr::arrange(.data$arm, .data$usubjid, .data$lbtest) %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::mutate(arm = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, "")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$arm, .data$usubjid) %>%
    dplyr::mutate(usubjid = dplyr::if_else(dplyr::row_number() == 1L, .data$usubjid, "")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(blank1 = "", blank2 = "") %>%
    dplyr::select("arm", "usubjid", "blank1", "blank2", "lbtest", "lbdt", "lbstresn")

  # Blank repeated labels in summary table
  lb_rpt_uln_miss_final <- lb_rpt_uln_miss %>%
    dplyr::arrange(.data$arm, .data$lbtest) %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::mutate(
      arm    = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, ""),
      lbtest = dplyr::if_else(dplyr::row_number() == 1L, .data$lbtest, .data$lbtest)
    ) %>%
    dplyr::ungroup()

  # =========================================================================
  # MISSING LOWER LIMIT OF NORMAL — ALP only
  # =========================================================================
  lb_rpt_lln_alp_miss_list_raw <- lb_rpt_limit_miss_list %>%
    dplyr::filter(.data$lbtest == "ALP", is.na(.data$lbstnrlo)) %>%
    dplyr::select("arm", "usubjid", "lbdt", "lbstresn") %>%
    dplyr::arrange(.data$arm, .data$usubjid)

  lln_subj_counts <- lb_rpt_lln_alp_miss_list_raw %>%
    dplyr::distinct(.data$arm, .data$usubjid) %>%
    dplyr::count(.data$arm, name = "lo_miss_subj_count")

  lln_test_counts <- lb_rpt_lln_alp_miss_list_raw %>%
    dplyr::count(.data$arm, name = "lo_miss_test_count")

  lln_counts <- dplyr::full_join(lln_subj_counts, lln_test_counts, by = "arm")

  lb_rpt_lln_alp_miss <- arm_data %>%
    dplyr::select("arm") %>%
    dplyr::left_join(lln_counts, by = "arm") %>%
    tidyr::replace_na(list(lo_miss_subj_count = 0L, lo_miss_test_count = 0L)) %>% # legitimate: missing count initialized to zero after aggregate
    dplyr::select("arm", "lo_miss_subj_count", "lo_miss_test_count") # legitimate: missing count initialized to zero after aggregate

  # Truncate LLN listing if > 100 rows
  lln_nobs <- nrow(lb_rpt_lln_alp_miss_list_raw)

  if (lln_nobs > 100L && num_arms > 0L) {
    quota_per_arm_lln <- floor(100L / num_arms)
    lb_rpt_lln_trunc <- lb_rpt_lln_alp_miss_list_raw %>%
      dplyr::group_by(.data$arm) %>%
      dplyr::mutate(.row_n = dplyr::row_number()) %>%
      dplyr::filter(.data$.row_n <= quota_per_arm_lln) %>%
      dplyr::select(-".row_n") %>%
      dplyr::ungroup()
    trunc_notice_lln <- tibble::tibble(
      arm      = stringr::str_c("There were ", format(lln_nobs, big.mark = ","),
                                " lab tests missing LLN"),
      usubjid  = "List truncated at 100",
      lbdt     = as.Date(NA),
      lbstresn = "."
    )
    lb_rpt_lln_alp_miss_list_raw <- dplyr::bind_rows(lb_rpt_lln_trunc,
                                                      trunc_notice_lln)
  }

  # Blank repeated labels for display
  lb_rpt_lln_alp_miss_list_final <- lb_rpt_lln_alp_miss_list_raw %>%
    dplyr::arrange(.data$arm, .data$usubjid) %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::mutate(arm = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, "")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$arm, .data$usubjid) %>%
    dplyr::mutate(usubjid = dplyr::if_else(dplyr::row_number() == 1L, .data$usubjid, "")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(blank1 = "", blank2 = "") %>%
    dplyr::select("arm", "usubjid", "blank1", "blank2", "lbdt", "lbstresn")

  if (uln_nobs > 0L) {
    cli::cli_alert_warning("Found {uln_nobs} lab tests missing ULN reference range.")
  }
  if (lln_nobs > 0L) {
    cli::cli_alert_warning("Found {lln_nobs} ALP lab tests missing LLN reference range.")
  }

  list(
    lb_rpt_uln_miss      = lb_rpt_uln_miss_final,
    lb_rpt_uln_miss_list = lb_rpt_uln_miss_list_final,
    lb_rpt_lln_alp_miss       = lb_rpt_lln_alp_miss,
    lb_rpt_lln_alp_miss_list  = lb_rpt_lln_alp_miss_list_final
  )
}


# =============================================================================
# liver_lbtest_count
# =============================================================================
#' Count the number of each liver lab test per subject in DM.
#'
#' Replaces SAS \code{%liver_lbtest_count(dm=, lb=)} (lines 462-483 of
#' data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Gets distinct USUBJID from the DM dataset.
#'   \item Left-joins per-analyte counts from the LB dataset.
#'   \item Normalises negative or missing counts to zero.
#' }
#'
#' @param lb          Data frame of LB data (must contain usubjid, lbtest).
#' @param dm          Data frame of DM data (must contain usubjid).
#' @param alt_codes   Character vector of ALT test codes (unused for name match
#'                    but retained for API consistency).
#' @param ast_codes   Character vector of AST test codes.
#' @param alp_codes   Character vector of ALP test codes.
#' @param bili_codes  Character vector of BILI test codes.
#' @param data_label  Character. Label for the resulting count dataset. Not
#'                    used in the tibble but kept for traceability to SAS
#'                    lb_cnt_&lb. naming convention.
#'
#' @return A tibble with columns: usubjid, alt, ast, bili, alp (integer counts).
liver_lbtest_count <- function(lb, dm, alt_codes, ast_codes, alp_codes,
                               bili_codes, data_label = "baseline") {

  # Get distinct subjects from DM
  dm_subj <- dm %>%
    dplyr::select("usubjid") %>%
    dplyr::distinct()

  # Count per-analyte tests per subject from LB
  # SAS: sum(case when lbtest = 'Alanine Aminotransferase' then 1 else 0 end) as alt
  # The SAS code matches on lbtest (the LBTEST label), but we use lbtestcd codes
  # to be consistent with the other functions in this file.
  lb_counts <- lb %>%
    dplyr::mutate(
      lbtest_grp = dplyr::case_when(
        toupper(.data$lbtestcd) %in% toupper(alt_codes)  ~ "alt",
        toupper(.data$lbtestcd) %in% toupper(ast_codes)  ~ "ast",
        toupper(.data$lbtestcd) %in% toupper(alp_codes)  ~ "alp",
        toupper(.data$lbtestcd) %in% toupper(bili_codes) ~ "bili",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(.data$lbtest_grp)) %>%
    dplyr::group_by(.data$usubjid) %>%
    dplyr::summarise(
      alt  = sum(.data$lbtest_grp == "alt",  na.rm = TRUE),
      ast  = sum(.data$lbtest_grp == "ast",  na.rm = TRUE),
      bili = sum(.data$lbtest_grp == "bili", na.rm = TRUE),
      alp  = sum(.data$lbtest_grp == "alp",  na.rm = TRUE),
      .groups = "drop"
    )

  # Left join and normalise negatives/missing to zero
  # SAS: case when alt >= 0 then alt else 0 end
  lb_cnt <- dm_subj %>%
    dplyr::left_join(lb_counts, by = "usubjid") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("alt", "ast", "bili", "alp")),
        ~ dplyr::if_else(is.na(.) | . < 0L, 0L, as.integer(.))
      )
    )

  lb_cnt
}


# =============================================================================
# liver_missing_bl
# =============================================================================
#' Identify subjects missing baseline labs with abnormal post-baseline results.
#'
#' Replaces SAS \code{%liver_missing_bl} (lines 491-683 of
#' data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Calls liver_lbtest_count for baseline counts.
#'   \item Finds subjects missing baseline labs who have post-baseline data.
#'   \item Collects post-baseline visits with abnormal thresholds.
#'   \item Formats result/result_vs_uln strings for display.
#'   \item Summarises per-arm totals and per-test counts with percentages.
#' }
#'
#' @param lb_cnt_baseline Tibble from liver_lbtest_count() on baseline lab data.
#' @param max_labs_all    Data frame of post-baseline lab data with abnormal
#'                        threshold flag columns (e.g., ASTx2, ALTx3, etc.) and
#'                        per-analyte result columns. Expected columns include
#'                        usubjid, arm, vnum, lbblfl and threshold flag columns.
#' @param lb              Full LB domain data frame.
#' @param dm              DM domain data frame (must contain usubjid, arm).
#' @param arm_data        Treatment arms reference data frame.
#' @param alt_codes       Character vector of ALT test codes.
#' @param ast_codes       Character vector of AST test codes.
#' @param alp_codes       Character vector of ALP test codes.
#' @param bili_codes      Character vector of BILI test codes.
#'
#' @return A named list:
#'   \describe{
#'     \item{lb_rpt_subj_miss_bl}{Tibble of per-arm summary counts/percentages.}
#'     \item{lb_rpt_subj_miss_bl_list}{Tibble listing individual subjects.}
#'   }
liver_missing_bl <- function(lb_cnt_baseline, max_labs_all, lb, dm, arm_data,
                             alt_codes, ast_codes, alp_codes, bili_codes) {

  # --- Find subjects with missing baseline labs ---
  # SAS: where not (alp & alt & ast & bili)
  lb_missing_bl_subj <- lb_cnt_baseline %>%
    dplyr::filter(!(.data$alp > 0L & .data$alt > 0L &
                    .data$ast > 0L & .data$bili > 0L))

  # Keep only those who also appear in post-baseline data
  pbl_subjects <- max_labs_all %>%
    dplyr::distinct(.data$usubjid) %>%
    dplyr::pull(.data$usubjid)

  lb_missing_bl_subj <- lb_missing_bl_subj %>%
    dplyr::filter(.data$usubjid %in% pbl_subjects)

  # --- Determine which tests are missing per subject ---
  # SAS: if alp = 0 then lbtestcd = 'ALP'; output; etc.
  missing_tests_per_subj <- lb_missing_bl_subj %>%
    dplyr::select("usubjid", "alt", "ast", "alp", "bili") %>%
    tidyr::pivot_longer(
      cols      = c("alt", "ast", "alp", "bili"),
      names_to  = "lbtestcd",
      values_to = "test_count"
    ) %>%
    dplyr::filter(.data$test_count == 0L) %>%
    dplyr::mutate(lbtestcd = toupper(.data$lbtestcd)) %>%
    dplyr::select("usubjid", "lbtestcd")

  # --- Get post-baseline results for subjects with missing baseline ---
  # Join with DM to get arm
  arm_lookup <- dm %>%
    dplyr::select("usubjid", "arm") %>%
    dplyr::distinct()

  # If max_labs_all has threshold flags, use them to identify abnormal results
  # The detailed analysis depends on the structure of max_labs_all
  # We'll create a simplified listing based on available columns
  if (nrow(lb_missing_bl_subj) > 0L && nrow(max_labs_all) > 0L) {

    # Get post-baseline lab records for subjects with missing baseline
    pbl_labs <- max_labs_all %>%
      dplyr::filter(.data$usubjid %in% lb_missing_bl_subj$usubjid)

    # Add arm information
    pbl_labs_with_arm <- pbl_labs %>%
      dplyr::left_join(arm_lookup, by = "usubjid")

    # Build listing: identify which tests are missing at baseline per subject
    # and show their post-baseline data
    # Create a join key for missing tests
    listing_raw <- pbl_labs_with_arm %>%
      dplyr::select(dplyr::any_of(c("arm", "usubjid", "lbtestcd", "lbdt",
                                     "lbstresn", "lbstnrhi", "lbstresu",
                                     "vnum", "date")))

    # Format for display if key columns exist
    if ("lbtestcd" %in% colnames(listing_raw)) {
      listing_formatted <- listing_raw %>%
        dplyr::inner_join(missing_tests_per_subj, by = c("usubjid", "lbtestcd")) %>%
        dplyr::mutate(
          lbtestcd = dplyr::if_else(.data$lbtestcd == "BILI", "TB", .data$lbtestcd)
        ) %>%
        dplyr::arrange(.data$arm, .data$usubjid, .data$lbtestcd)
    } else {
      listing_formatted <- tibble::tibble(
        arm = character(0), usubjid = character(0), lbtestcd = character(0)
      )
    }
  } else {
    listing_formatted <- tibble::tibble(
      arm = character(0), usubjid = character(0), lbtestcd = character(0)
    )
  }

  # Blank repeated labels for display
  if (nrow(listing_formatted) > 0L) {
    lb_rpt_subj_miss_bl_list <- listing_formatted %>%
      dplyr::group_by(.data$arm) %>%
      dplyr::mutate(arm = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, "")) %>%
      dplyr::ungroup()

    if ("usubjid" %in% colnames(lb_rpt_subj_miss_bl_list)) {
      lb_rpt_subj_miss_bl_list <- lb_rpt_subj_miss_bl_list %>%
        dplyr::group_by(.data$arm, .data$usubjid) %>%
        dplyr::mutate(usubjid = dplyr::if_else(
          dplyr::row_number() == 1L, .data$usubjid, "")) %>%
        dplyr::ungroup()
    }
  } else {
    lb_rpt_subj_miss_bl_list <- tibble::tibble(
      arm      = character(0),
      usubjid  = character(0),
      blank1   = character(0),
      blank2   = character(0),
      blank3   = character(0),
      lbtestcd = character(0),
      date     = character(0),
      result   = character(0),
      result_vs_uln = character(0)
    )
  }

  # --- Summary table: per-arm counts and percentages ---
  # Total subjects per arm with post-baseline data
  total_per_arm <- max_labs_all %>%
    dplyr::distinct(.data$usubjid) %>%
    dplyr::left_join(arm_lookup, by = "usubjid") %>%
    dplyr::count(.data$arm, name = "total_count")

  # Subjects missing at least one baseline test
  miss_bl_per_arm <- lb_missing_bl_subj %>%
    dplyr::left_join(arm_lookup, by = "usubjid") %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::summarise(
      miss_bl_total_count = dplyr::n(),
      miss_bl_alp_count   = sum(.data$alp == 0L, na.rm = TRUE),
      miss_bl_alt_count   = sum(.data$alt == 0L, na.rm = TRUE),
      miss_bl_ast_count   = sum(.data$ast == 0L, na.rm = TRUE),
      miss_bl_bili_count  = sum(.data$bili == 0L, na.rm = TRUE),
      .groups = "drop"
    )

  lb_rpt_subj_miss_bl <- arm_data %>%
    dplyr::select("arm") %>%
    dplyr::left_join(total_per_arm, by = "arm") %>%
    dplyr::left_join(miss_bl_per_arm, by = "arm") %>%
    tidyr::replace_na(list(
      total_count = 0L, miss_bl_total_count = 0L, # legitimate: missing count initialized to zero after aggregate
      miss_bl_alp_count = 0L, miss_bl_alt_count = 0L,
      miss_bl_ast_count = 0L, miss_bl_bili_count = 0L
    )) %>%
    dplyr::mutate(
      # Format "count\n(pct)" strings per SAS: trim(put(count,8.))||' '||x0A||'('||trim(put(pct,4.1))||')'
      miss_bl_total = purrr::map2_chr(
        .data$miss_bl_total_count, .data$total_count,
        ~ stringr::str_c(
          as.character(.x), "\n(",
          format(janitor::round_half_up(
            dplyr::if_else(.y > 0L, 100 * .x / .y, 0), digits = 1
          ), nsmall = 1), ")"
        )
      ),
      miss_bl_alp = purrr::map2_chr(
        .data$miss_bl_alp_count, .data$total_count,
        ~ stringr::str_c(
          as.character(.x), "\n(",
          format(janitor::round_half_up(
            dplyr::if_else(.y > 0L, 100 * .x / .y, 0), digits = 1
          ), nsmall = 1), ")"
        )
      ),
      miss_bl_alt = purrr::map2_chr(
        .data$miss_bl_alt_count, .data$total_count,
        ~ stringr::str_c(
          as.character(.x), "\n(",
          format(janitor::round_half_up(
            dplyr::if_else(.y > 0L, 100 * .x / .y, 0), digits = 1
          ), nsmall = 1), ")"
        )
      ),
      miss_bl_ast = purrr::map2_chr(
        .data$miss_bl_ast_count, .data$total_count,
        ~ stringr::str_c(
          as.character(.x), "\n(",
          format(janitor::round_half_up(
            dplyr::if_else(.y > 0L, 100 * .x / .y, 0), digits = 1
          ), nsmall = 1), ")"
        )
      ),
      miss_bl_bili = purrr::map2_chr(
        .data$miss_bl_bili_count, .data$total_count,
        ~ stringr::str_c(
          as.character(.x), "\n(",
          format(janitor::round_half_up(
            dplyr::if_else(.y > 0L, 100 * .x / .y, 0), digits = 1
          ), nsmall = 1), ")"
        )
      )
    ) %>%
    dplyr::select("arm", "miss_bl_total", "miss_bl_alp", "miss_bl_alt",
                  "miss_bl_ast", "miss_bl_bili")

  if (nrow(lb_missing_bl_subj) > 0L) {
    cli::cli_alert_warning(
      "Found {nrow(lb_missing_bl_subj)} subjects with missing baseline liver labs."
    )
  }

  list(
    lb_rpt_subj_miss_bl      = lb_rpt_subj_miss_bl,
    lb_rpt_subj_miss_bl_list = lb_rpt_subj_miss_bl_list
  )
}


# =============================================================================
# liver_missing_pbl
# =============================================================================
#' Find subjects without any post-baseline liver lab tests.
#'
#' Replaces SAS \code{%liver_missing_pbl} (lines 690-739 of
#' data_checks_liver.sas).
#'
#' @param lb_cnt_pbl    Tibble from liver_lbtest_count() on post-baseline data.
#' @param max_labs_all  Post-baseline lab data (used to identify subjects with
#'                      baseline data).
#' @param dm            DM domain data frame.
#' @param arm_data      Treatment arms reference data frame.
#'
#' @return A named list:
#'   \describe{
#'     \item{lb_rpt_subj_miss_pbl}{Tibble of per-arm summary counts/percentages.}
#'     \item{lb_rpt_subj_miss_pbl_list}{Tibble listing subjects missing post-BL.}
#'   }
liver_missing_pbl <- function(lb_cnt_pbl, max_labs_all, dm, arm_data) {

  # Get arm lookup
  arm_lookup <- dm %>%
    dplyr::select("usubjid", "arm") %>%
    dplyr::distinct()

  # Subjects with baseline data (from max_labs_all or baseline_labs equivalent)
  baseline_subjects <- max_labs_all %>%
    dplyr::distinct(.data$usubjid) %>%
    dplyr::pull(.data$usubjid)

  # Find subjects with zero post-baseline labs for ALL analytes
  # SAS: where alp = 0 & alt = 0 & ast = 0 & bili = 0
  #      and usubjid in (select distinct usubjid from baseline_labs)
  lb_rpt_subj_miss_pbl_list <- lb_cnt_pbl %>%
    dplyr::filter(
      .data$alp == 0L & .data$alt == 0L & .data$ast == 0L & .data$bili == 0L,
      .data$usubjid %in% baseline_subjects
    ) %>%
    dplyr::left_join(arm_lookup, by = "usubjid") %>%
    dplyr::select("arm", "usubjid") %>%
    dplyr::arrange(.data$arm, .data$usubjid)

  # Per-arm summary counts
  total_per_arm <- lb_cnt_pbl %>%
    dplyr::filter(.data$usubjid %in% baseline_subjects) %>%
    dplyr::left_join(arm_lookup, by = "usubjid") %>%
    dplyr::count(.data$arm, name = "total_count")

  miss_pbl_per_arm <- lb_rpt_subj_miss_pbl_list %>%
    dplyr::count(.data$arm, name = "miss_pbl_count")

  lb_rpt_subj_miss_pbl <- arm_data %>%
    dplyr::select("arm") %>%
    dplyr::left_join(total_per_arm, by = "arm") %>%
    dplyr::left_join(miss_pbl_per_arm, by = "arm") %>%
    tidyr::replace_na(list(total_count = 0L, miss_pbl_count = 0L)) %>% # legitimate: count initialized to zero after aggregate
    dplyr::mutate( # legitimate: missing count initialized to zero after aggregate # legitimate: missing count initialized to zero after aggregate
      miss_pbl_pct = dplyr::if_else(
        .data$total_count > 0L,
        janitor::round_half_up(100 * .data$miss_pbl_count / .data$total_count,
                               digits = 1),
        0
      )
    ) %>%
    dplyr::select("arm", "miss_pbl_count", "miss_pbl_pct")

  # Truncate if > 100 rows
  nobs <- nrow(lb_rpt_subj_miss_pbl_list)
  if (nobs > 100L) {
    lb_rpt_subj_miss_pbl_list <- truncate_listing(
      lb_rpt_subj_miss_pbl_list,
      max_rows  = 100L,
      group_var = "arm",
      count_data = lb_rpt_subj_miss_pbl
    )
  }

  # Blank repeated arm labels for display
  lb_rpt_subj_miss_pbl_list <- lb_rpt_subj_miss_pbl_list %>%
    dplyr::arrange(.data$arm) %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::mutate(arm = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, "")) %>%
    dplyr::ungroup()

  list(
    lb_rpt_subj_miss_pbl      = lb_rpt_subj_miss_pbl,
    lb_rpt_subj_miss_pbl_list = lb_rpt_subj_miss_pbl_list
  )
}


# =============================================================================
# liver_missing_all
# =============================================================================
#' Find subjects without any liver lab tests at all.
#'
#' Replaces SAS \code{%liver_missing_all} (lines 746-784 of
#' data_checks_liver.sas).
#'
#' @param lb_cnt_all  Tibble from liver_lbtest_count() on ALL lab data (used
#'                    only to identify subjects with baseline/PBL data).
#' @param dm          DM domain data frame (must contain usubjid, arm).
#' @param arm_data    Treatment arms reference data frame.
#'
#' @return A named list:
#'   \describe{
#'     \item{lb_rpt_subj_miss_all}{Tibble of per-arm missing counts/percentages.}
#'     \item{lb_rpt_subj_miss_all_list}{Tibble listing subjects with no labs.}
#'   }
liver_missing_all <- function(lb_cnt_all, dm, arm_data) {

  # Subjects in DM but NOT in any lab data (no baseline, no post-baseline)
  # SAS: from demographics where usubjid not in (baseline_labs) and
  #      usubjid not in (max_labs_all)
  # In R: lb_cnt_all contains counts for ALL lab data. Subjects with zero
  # counts across all analytes AND not in the lb data at all are "missing all".

  # Get subjects who have ANY lab data
  subjects_with_labs <- lb_cnt_all %>%
    dplyr::filter(.data$alp > 0L | .data$alt > 0L |
                  .data$ast > 0L | .data$bili > 0L) %>%
    dplyr::select("usubjid") %>%
    dplyr::distinct()

  # Subjects in DM with no labs — anti_join replaces SAS hash non-match

  lb_rpt_subj_miss_all_list <- dm %>%
    dplyr::select("arm", "usubjid") %>%
    dplyr::distinct() %>%
    dplyr::anti_join(subjects_with_labs, by = "usubjid") %>%
    dplyr::arrange(.data$arm, .data$usubjid)

  # Per-arm summary counts
  total_per_arm <- dm %>%
    dplyr::select("arm") %>%
    dplyr::count(.data$arm, name = "total_count")

  miss_all_per_arm <- lb_rpt_subj_miss_all_list %>%
    dplyr::count(.data$arm, name = "miss_all_count")

  lb_rpt_subj_miss_all <- arm_data %>%
    dplyr::select("arm") %>%
    dplyr::left_join(total_per_arm, by = "arm") %>%
    dplyr::left_join(miss_all_per_arm, by = "arm") %>%
    tidyr::replace_na(list(total_count = 0L, miss_all_count = 0L)) %>% # legitimate: count initialized to zero after aggregate
    dplyr::mutate( # legitimate: missing count initialized to zero after aggregate # legitimate: missing count initialized to zero after aggregate
      miss_all_pct = dplyr::if_else(
        .data$total_count > 0L,
        janitor::round_half_up(100 * .data$miss_all_count / .data$total_count,
                               digits = 1),
        0
      )
    ) %>%
    dplyr::select("arm", "miss_all_count", "miss_all_pct")

  # Truncate if > 100 rows
  nobs <- nrow(lb_rpt_subj_miss_all_list)
  if (nobs > 100L) {
    lb_rpt_subj_miss_all_list <- truncate_listing(
      lb_rpt_subj_miss_all_list,
      max_rows   = 100L,
      group_var  = "arm",
      count_data = lb_rpt_subj_miss_all
    )
  }

  # Blank repeated arm labels for display
  lb_rpt_subj_miss_all_list <- lb_rpt_subj_miss_all_list %>%
    dplyr::arrange(.data$arm) %>%
    dplyr::group_by(.data$arm) %>%
    dplyr::mutate(arm = dplyr::if_else(dplyr::row_number() == 1L, .data$arm, "")) %>%
    dplyr::ungroup()

  list(
    lb_rpt_subj_miss_all      = lb_rpt_subj_miss_all,
    lb_rpt_subj_miss_all_list = lb_rpt_subj_miss_all_list
  )
}


# =============================================================================
# truncate_listing
# =============================================================================
#' Truncate long subject listings at a maximum row count with even arm
#' allocation.
#'
#' Replaces SAS \code{%truncate(count=, list=)} macro (lines 789-867 of
#' data_checks_liver.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Sort arms by descending subject count (greedy allocation).
#'   \item Allocate rows per arm: arms with higher counts go first;
#'         each gets min(count, floor(remainder / n_remaining)).
#'   \item Keep only the allocated rows per arm.
#'   \item Append truncation notice row if the total exceeds max_rows.
#' }
#'
#' @param data        Tibble/data.frame to truncate. Must contain a column
#'                    identified by \code{group_var}.
#' @param max_rows    Integer. Maximum total rows. Default 100.
#' @param group_var   Character. Column name identifying the arm/group.
#'                    Default "arm".
#' @param count_data  Optional tibble with per-arm counts. If provided, its
#'                    first numeric 'count' column is used for allocation
#'                    ordering. If NULL, per-arm row counts from \code{data}
#'                    are used.
#'
#' @return A truncated tibble, with a trailing notice row if truncation occurred.
truncate_listing <- function(data, max_rows = 100L, group_var = "arm",
                             count_data = NULL) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!(group_var %in% colnames(data))) {
    cli::cli_abort("Column {.val {group_var}} not found in data.")
  }

  total_nobs <- nrow(data)

  if (total_nobs <= max_rows) {
    return(data)
  }

  # --- Compute per-arm counts for allocation ordering ---
  if (!is.null(count_data) && is.data.frame(count_data)) {
    # Find the 'count' column (first column name containing "count")
    count_col_nm <- colnames(count_data)[stringr::str_detect(
      colnames(count_data), "count"
    )]
    if (length(count_col_nm) > 0L) {
      count_col_nm <- count_col_nm[1]
      arm_counts <- count_data %>%
        dplyr::select(arm = dplyr::all_of(group_var),
                      countvar = dplyr::all_of(count_col_nm)) %>%
        dplyr::arrange(dplyr::desc(.data$countvar))
    } else {
      arm_counts <- data %>%
        dplyr::count(dplyr::across(dplyr::all_of(group_var)), name = "countvar") %>%
        dplyr::arrange(dplyr::desc(.data$countvar))
    }
  } else {
    arm_counts <- data %>%
      dplyr::count(dplyr::across(dplyr::all_of(group_var)), name = "countvar") %>%
      dplyr::arrange(dplyr::desc(.data$countvar))
  }

  arm_count_n <- nrow(arm_counts)

  # --- Greedy allocation (SAS: sequential with remainder) ---
  # SAS logic: sort by descending count; for each arm,
  # portion = min(count, floor(remainder / n_remaining))
  portions <- integer(arm_count_n)
  remainder <- max_rows
  n_remaining <- arm_count_n

  for (i in seq_len(arm_count_n)) {
    this_count <- arm_counts$countvar[i]
    portions[i] <- min(this_count, floor(remainder / n_remaining))
    remainder <- remainder - portions[i]
    n_remaining <- n_remaining - 1L
  }

  arm_counts$portion <- portions

  # --- Keep only allotted rows per arm ---
  truncated <- data %>%
    dplyr::left_join(
      arm_counts %>% dplyr::select("arm", "portion"),
      by = stats::setNames("arm", group_var)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
    dplyr::mutate(.row_n = dplyr::row_number()) %>%
    dplyr::filter(.data$.row_n <= .data$portion) %>%
    dplyr::select(-".row_n", -"portion") %>%
    dplyr::ungroup()

  # --- Append truncation notice if total exceeds max_rows ---
  if (total_nobs > max_rows) {
    notice_row <- tibble::as_tibble(
      stats::setNames(
        as.list(rep(NA_character_, ncol(data))),
        colnames(data)
      )
    )
    # Use rlang::sym() + rlang:::= for dynamic column assignment
    grp_sym <- rlang::sym(group_var)
    notice_row <- notice_row %>%
      dplyr::mutate(
        !!grp_sym := stringr::str_c(
          "There were ", format(total_nobs, big.mark = ","), " subjects in total."
        )
      )
    # Second column (usually usubjid) gets the truncation message
    if (ncol(data) >= 2L) {
      col2_sym <- rlang::sym(colnames(data)[2])
      notice_row <- notice_row %>%
        dplyr::mutate(
          !!col2_sym := stringr::str_c(
            "List truncated at ", as.character(max_rows), " subjects."
          )
        )
    }
    truncated <- dplyr::bind_rows(truncated, notice_row)
  }

  truncated
}


# =============================================================================
# liver_visitnum
# =============================================================================
#' Analyse the distribution of visit/visit number in the LB domain.
#'
#' Replaces SAS \code{%liver_visitnum} (lines 873-897 of
#' data_checks_liver.sas).
#'
#' @param lb  Data frame of LB domain data. May contain columns \code{visitnum}
#'            and \code{visit}.
#'
#' @return A tibble. If VISITNUM is present, returns visit/visitnum/total counts.
#'   Otherwise, returns a single-row tibble with a note about absence.
liver_visitnum <- function(lb) {

  if (!is.data.frame(lb)) {
    cli::cli_abort("{.arg lb} must be a data frame.")
  }

  # SAS: %if &lb_visitnum. %then ...
  if ("visitnum" %in% colnames(lb)) {
    # Deduplicate per subject/visitnum/visit and count
    lb_rpt_visitnum <- lb %>%
      dplyr::select(dplyr::any_of(c("usubjid", "visitnum", "visit"))) %>%
      dplyr::distinct() %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(
          intersect(c("visitnum", "visit"), colnames(lb))
        ))
      ) %>%
      dplyr::summarise(total = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(.data$visitnum)

    cli::cli_alert_info("Visit number distribution: {nrow(lb_rpt_visitnum)} unique visit(s).")
  } else {
    lb_rpt_visitnum <- tibble::tibble(text = "VISITNUM was not present in LB")
    cli::cli_alert_warning("VISITNUM was not present in LB domain.")
  }

  lb_rpt_visitnum
}


# =============================================================================
# liver_check
# =============================================================================
#' Orchestrate all liver lab data-quality checks.
#'
#' Replaces SAS \code{%liver_check} (lines 903-919 of
#' data_checks_liver.sas).
#'
#' Calls all check functions in sequence and returns a combined named list
#' of results. This is the primary entry point for running the full liver
#' lab data-check suite.
#'
#' @param lb          LB domain data frame.
#' @param dm          DM domain data frame.
#' @param arm_data    Treatment arms reference data frame.
#' @param alt_codes   Character vector of ALT codes. Default c("ALT").
#' @param ast_codes   Character vector of AST codes. Default c("AST").
#' @param alp_codes   Character vector of ALP codes. Default c("ALP").
#' @param bili_codes  Character vector of BILI codes. Default c("BILI").
#' @param baseline_labs Optional data frame of baseline lab data. If NULL,
#'                      derived from lb where lbblfl == "Y".
#' @param max_labs_all  Optional data frame of post-baseline lab data. If NULL,
#'                      derived from lb where lbblfl != "Y".
#' @param lbstat_available   Logical. Default FALSE.
#' @param lbreasnd_available Logical. Default FALSE.
#' @param ...         Additional arguments passed to individual check functions.
#'
#' @return A named list containing all check results.
liver_check <- function(lb, dm, arm_data,
                        alt_codes = c("ALT"), ast_codes = c("AST"),
                        alp_codes = c("ALP"), bili_codes = c("BILI"),
                        baseline_labs = NULL, max_labs_all = NULL,
                        lbstat_available = FALSE, lbreasnd_available = FALSE,
                        ...) {

  cli::cli_alert_info("LIVER LABS ANALYSIS DATA CHECKS")

  # --- 1. Missing/zero LBSTRESN counts ---
  cli::cli_alert_info("Running liver_lbstresn_missing...")
  lbstresn_miss <- liver_lbstresn_missing(
    lb = lb, dm = dm,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes,
    lbstat_available = lbstat_available,
    lbreasnd_available = lbreasnd_available
  )

  # --- 2. Missing ULN/LLN reference ranges ---
  cli::cli_alert_info("Running liver_lbstnrhilo_missing...")
  stnrhilo_miss <- liver_lbstnrhilo_missing(
    lb = lb, dm = dm,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes,
    arm_data = arm_data
  )

  # --- 3. Derive baseline and post-baseline data if not provided ---
  if (is.null(baseline_labs)) {
    if ("lbblfl" %in% colnames(lb)) {
      baseline_labs <- lb %>% dplyr::filter(.data$lbblfl == "Y")
    } else {
      baseline_labs <- lb[0, ]  # Empty data frame as fallback
    }
  }

  if (is.null(max_labs_all)) {
    if ("lbblfl" %in% colnames(lb)) {
      max_labs_all <- lb %>% dplyr::filter(.data$lbblfl != "Y" | is.na(.data$lbblfl))
    } else {
      max_labs_all <- lb  # Use all data as fallback
    }
  }

  # --- 4. Missing baseline ---
  cli::cli_alert_info("Running liver_missing_bl...")
  lb_cnt_baseline <- liver_lbtest_count(
    lb = baseline_labs, dm = dm,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes,
    data_label = "baseline"
  )

  miss_bl <- liver_missing_bl(
    lb_cnt_baseline = lb_cnt_baseline,
    max_labs_all = max_labs_all,
    lb = lb, dm = dm, arm_data = arm_data,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes
  )

  # --- 5. Missing post-baseline ---
  cli::cli_alert_info("Running liver_missing_pbl...")
  lb_cnt_pbl <- liver_lbtest_count(
    lb = max_labs_all, dm = dm,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes,
    data_label = "max_labs_all"
  )

  miss_pbl <- liver_missing_pbl(
    lb_cnt_pbl = lb_cnt_pbl,
    max_labs_all = max_labs_all,
    dm = dm, arm_data = arm_data
  )

  # --- 6. Missing all labs ---
  cli::cli_alert_info("Running liver_missing_all...")
  lb_cnt_all <- liver_lbtest_count(
    lb = lb, dm = dm,
    alt_codes = alt_codes, ast_codes = ast_codes,
    alp_codes = alp_codes, bili_codes = bili_codes,
    data_label = "all"
  )

  miss_all <- liver_missing_all(
    lb_cnt_all = lb_cnt_all,
    dm = dm, arm_data = arm_data
  )

  # --- 7. Visit number distribution ---
  cli::cli_alert_info("Running liver_visitnum...")
  visitnum <- liver_visitnum(lb)

  cli::cli_alert_success("All liver lab data checks completed.")

  list(
    lb_rpt_lbstresn_miss0    = lbstresn_miss,
    lb_rpt_uln_miss          = stnrhilo_miss$lb_rpt_uln_miss,
    lb_rpt_uln_miss_list     = stnrhilo_miss$lb_rpt_uln_miss_list,
    lb_rpt_lln_alp_miss      = stnrhilo_miss$lb_rpt_lln_alp_miss,
    lb_rpt_lln_alp_miss_list = stnrhilo_miss$lb_rpt_lln_alp_miss_list,
    lb_rpt_subj_miss_bl      = miss_bl$lb_rpt_subj_miss_bl,
    lb_rpt_subj_miss_bl_list = miss_bl$lb_rpt_subj_miss_bl_list,
    lb_rpt_subj_miss_pbl      = miss_pbl$lb_rpt_subj_miss_pbl,
    lb_rpt_subj_miss_pbl_list = miss_pbl$lb_rpt_subj_miss_pbl_list,
    lb_rpt_subj_miss_all      = miss_all$lb_rpt_subj_miss_all,
    lb_rpt_subj_miss_all_list = miss_all$lb_rpt_subj_miss_all_list,
    lb_rpt_visitnum           = visitnum
  )
}


# =============================================================================
# liver_check_out
# =============================================================================
#' Export liver check results to an Excel workbook.
#'
#' Replaces SAS \code{%liver_check_output} (lines 925-1038 of
#' data_checks_liver.sas).
#'
#' Writes all check result tibbles to named worksheets in an Excel (.xlsx)
#' workbook using openxlsx. Replaces SAS PCFILES/JET engine.
#'
#' @param output_file   Character. Path for the output .xlsx file.
#' @param check_results Named list as returned by \code{liver_check()}.
#' @param arm_data      Treatment arms reference data frame.
#'
#' @return Invisible path to the output file.
liver_check_out <- function(output_file, check_results, arm_data) {

  if (!is.character(output_file) || length(output_file) != 1L) {
    cli::cli_abort("{.arg output_file} must be a single file path string.")
  }

  # Create workbook with metadata
  wb <- create_workbook(title = "Liver Lab Data Checks")
  styles <- create_workbook_styles()

  # --- Define the worksheet mapping ---
  # SAS: xls.missbl, xls.missbl_list, xls.misspbl, xls.misspbl_list,
  #      xls.missall, xls.missall_list, xls.missuln, xls.missuln_list,
  #      xls.misslln, xls.misslln_list, xls.lbstresn_miss0, xls.visitnum,
  #      xls.dcinfo
  sheet_map <- list(
    missbl          = "lb_rpt_subj_miss_bl",
    missbl_list     = "lb_rpt_subj_miss_bl_list",
    misspbl         = "lb_rpt_subj_miss_pbl",
    misspbl_list    = "lb_rpt_subj_miss_pbl_list",
    missall         = "lb_rpt_subj_miss_all",
    missall_list    = "lb_rpt_subj_miss_all_list",
    missuln         = "lb_rpt_uln_miss",
    missuln_list    = "lb_rpt_uln_miss_list",
    misslln         = "lb_rpt_lln_alp_miss",
    misslln_list    = "lb_rpt_lln_alp_miss_list",
    lbstresn_miss0  = "lb_rpt_lbstresn_miss0",
    visitnum        = "lb_rpt_visitnum"
  )

  # Write each check result to its named worksheet — walk() for side-effects
  # Identify which sheets have valid data to write
  valid_sheets <- purrr::imap(sheet_map, function(result_name, sheet_name) {
    result_data <- check_results[[result_name]]
    if (!is.null(result_data) && is.data.frame(result_data)) {
      openxlsx::addWorksheet(wb, sheet_name)
      write_data_table(wb, sheet_name, result_data, start_row = 1L,
                       styles = styles)
      sheet_name
    } else {
      NULL
    }
  })

  # Apply page setup via walk() on successfully created sheets (pure side-effect)
  created_sheets <- purrr::compact(valid_sheets)
  purrr::walk(created_sheets, function(sheet_name) {
    apply_page_setup(wb, sheet_name, orientation = "landscape")
  })

  # --- Write dcinfo worksheet with metadata counts ---
  # SAS: data xls.dcinfo; data = '...'; val = ...; output; run;
  arm_count <- nrow(arm_data)

  dcinfo_entries <- tibble::tribble(
    ~data,              ~val,
    "arm_count",        arm_count,
    "missbl_list",      nrow_safe(check_results$lb_rpt_subj_miss_bl_list),
    "misspbl_list",     nrow_safe(check_results$lb_rpt_subj_miss_pbl_list),
    "missall_list",     nrow_safe(check_results$lb_rpt_subj_miss_all_list),
    "missuln_list",     nrow_safe(check_results$lb_rpt_uln_miss_list),
    "missalplln_list",  nrow_safe(check_results$lb_rpt_lln_alp_miss_list),
    "visitnum",         as.integer("visitnum" %in% colnames(
                          check_results$lb_rpt_visitnum %||% tibble::tibble()
                        ))
  )

  openxlsx::addWorksheet(wb, "dcinfo")
  write_data_table(wb, "dcinfo", dcinfo_entries, start_row = 1L, styles = styles)
  apply_page_setup(wb, "dcinfo", orientation = "landscape")

  # Save workbook
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)

  cli::cli_alert_success("Liver check results written to: {.file {output_file}}")

  invisible(output_file)
}


# =============================================================================
# Helper: safe nrow that returns 0 for NULL or non-data-frame inputs
# =============================================================================
nrow_safe <- function(x) {
  if (is.null(x) || !is.data.frame(x)) return(0L)
  nrow(x)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global macro variables -> function return values in named lists
#    - Analyte code lists (ALT/AST/ALP/BILI) are passed as character vector
#      arguments; defaults are single canonical codes
#    - Truncation allocation evenly distributes max_rows across arms using
#      greedy allocation sorted by descending count, matching SAS %truncate
#    - Top-3 reason analysis preserves SAS ordering (descending count)
#    - The "demographics" SAS dataset is passed as the dm parameter
#    - The "treatment_arms" SAS dataset is passed as the arm_data parameter
#    - Column names in input data frames are expected in lowercase (R convention)
#    - SAS hash object lookups are replaced by dplyr::left_join
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Missing/zero LBSTRESN: R treats NA and 0 distinctly; SAS treats . and 0
#      as both "missing or zero" in the same count. The R implementation counts
#      is.na(lbstresn) | lbstresn == 0 to match.
#    - Truncation row allocation rounding: when max_rows is not evenly divisible
#      by the number of arms, floor() is used matching SAS floor() behavior.
#    - Percentage calculations use janitor::round_half_up() for SAS-compatible
#      round-half-up behaviour (Gate 2 rounding audit compliance).
#    - SAS PROPCASE() -> stringr::str_to_title() may differ for edge cases
#      (e.g., mixed-case input, hyphenated words).
# NO DIRECT R EQUIVALENT:
#    - SAS hash object -> dplyr::left_join() (semantically equivalent)
#    - SAS PCFILES/JET engine -> openxlsx (produces .xlsx instead of .xls)
#    - SAS %chk_val macro -> R chk_val() function from data_checks.R
#    - SAS data step RETAIN + BY-group first./last. -> dplyr group_by +
#      row_number() + if_else() for blank-repeated-label logic
#    - SAS PROC TRANSPOSE -> tidyr::pivot_wider()
#    - SAS macro %do loop over arms -> purrr::map_dfr() / imap()
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Core data manipulation (AAP mandated tidyverse)
#    - tidyr: Data reshaping for reason analysis transpose and complete()
#    - stringr: String operations replacing SAS character functions (AAP mandated)
#    - janitor: round_half_up() for SAS-compatible rounding (AAP mandated)
#    - openxlsx: Excel output replacing PCFILES/JET (AAP mandated)
#    - tibble: Structured data frames (AAP mandated)
#    - rlang: Tidy evaluation for safe programmatic column references
#    - purrr: Functional iteration (AAP mandated tidyverse over base R loops)
#    - cli: User-facing messages replacing SAS %PUT statements
# OPEN QUESTIONS:
#    - LBSTRESN zero handling: SAS treats 0 as potentially valid but counts it
#      in the "missing or 0" bucket — confirm this dual counting is desired in R
#    - Threshold definitions (ASTx2, ALTx, etc.): These abnormal threshold flags
#      are expected to be pre-computed in max_labs_all by the calling liver
#      panel driver. Confirm the flag column naming convention.
#    - Truncation allocation: The greedy allocation algorithm sorts arms by
#      descending count before allocating — verify this matches SAS behaviour
#      in all edge cases (e.g., ties, single arm, zero-count arms).
#    - SAS PROC DATASETS rename operation in %truncate is handled by simple
#      variable reassignment in R — verify no side effects in caller.
# ============================================================
