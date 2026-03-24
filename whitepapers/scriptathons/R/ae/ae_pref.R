# =============================================================================
# ae_pref.R
# AE Preferred-Term Analysis — Scriptathon Target 24
#
# Migrated from: whitepapers/scriptathons/ae/ae_pref.sas
# Original SAS modification date: 2019-12-23
#
# Description:
#   Counts subjects experiencing treatment-emergent adverse events at three
#   granularity levels:
#     1. Treatment arm   (by TRTAN)
#     2. System Organ Class (by TRTAN, AESOC)
#     3. Preferred Term    (by TRTAN, AESOC, AEDECOD)
#
#   Each level filters the ADAE dataset on the safety population flag (SAFFL)
#   and the appropriate AE occurrence flag (AOCCFL, AOCCSFL, AOCCPFL).
#
# Dependencies:
#   haven   (>= 2.5.5) — SAS XPT transport file I/O
#   dplyr   (>= 1.1.0) — tidyverse data manipulation
#   rlang   (>= 1.1.0) — tidy evaluation for dynamic filter expressions
#   janitor (>= 2.2.0) — SAS-compatible round_half_up (available for future use)
# =============================================================================

library(haven)
library(dplyr)
library(rlang)
library(janitor)

#' AE Preferred-Term Analysis
#'
#' Migrated from SAS macro \code{\%scriptathon_target24} which nests the
#' \code{\%AEstat} macro. Reads ADSL and ADAE datasets in XPT format and
#' produces subject counts at three levels of granularity:
#' \itemize{
#'   \item Treatment arm level (TRTAN)
#'   \item System Organ Class level (TRTAN x AESOC)
#'   \item Preferred Term level (TRTAN x AESOC x AEDECOD)
#' }
#'
#' @param adsl_path Character. File path to the ADSL XPT dataset.
#' @param adae_path Character. File path to the ADAE XPT dataset.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{num_groups}{Integer. Number of distinct treatment groups in ADSL
#'       (from TRT01A).}
#'     \item{stat1}{A tibble. Row counts per treatment arm (TRTAN), filtered on
#'       SAFFL == "Y" and AOCCFL == "Y".}
#'     \item{stat2}{A tibble. Row counts per treatment arm x SOC
#'       (TRTAN, AESOC), filtered on SAFFL == "Y" and AOCCSFL == "Y".}
#'     \item{stat3}{A tibble. Row counts per treatment arm x SOC x preferred
#'       term (TRTAN, AESOC, AEDECOD), filtered on SAFFL == "Y" and
#'       AOCCPFL == "Y".}
#'   }
#'
#' @details
#' The SAS source defined a \code{tclause} parameter (values: trtemfl,
#' AOCCSFL) that was declared in the macro signature but never functionally
#' used in the PROC MEANS body (no VAR statement references it). This
#' parameter is intentionally omitted in the R migration. See MIGRATION NOTES
#' at end of file.
#'
#' @examples
#' \dontrun{
#' results <- ae_pref(
#'   adsl_path = "data/adam/cdisc/adsl.xpt",
#'   adae_path = "data/adam/cdisc/adae.xpt"
#' )
#' results$num_groups
#' results$stat1
#' results$stat2
#' results$stat3
#' }
#'
#' @export
ae_pref <- function(adsl_path, adae_path) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (!is.character(adsl_path) || length(adsl_path) != 1L) {
    stop("adsl_path must be a single character string.", call. = FALSE)
  }
  if (!is.character(adae_path) || length(adae_path) != 1L) {
    stop("adae_path must be a single character string.", call. = FALSE)
  }
  if (!file.exists(adsl_path)) {
    stop("ADSL file not found: ", adsl_path, call. = FALSE)
  }
  if (!file.exists(adae_path)) {
    stop("ADAE file not found: ", adae_path, call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Data loading (SAS lines 6-20)
  #   filename source url "...adsl.xpt"; libname source xport;
  #   data work.adsl; set source.adsl; run;
  #   filename source url "...adae.xpt"; libname source xport;
  #   data work.adae; set source.adae; run;
  # ---------------------------------------------------------------------------
  adsl <- haven::read_xpt(adsl_path)
  adae <- haven::read_xpt(adae_path)

  # ---------------------------------------------------------------------------
  # Count distinct treatment groups (SAS lines 28-36)
  #   proc sql noprint;
  #     select count(distinct trt01a) into: _num_groups from work.adsl;
  #   quit;
  #   %put _num_groups = &_num_groups;
  # ---------------------------------------------------------------------------
  num_groups <- dplyr::n_distinct(adsl$TRT01A)
  message("_num_groups = ", num_groups)

  # ---------------------------------------------------------------------------
  # Inner function: ae_stat
  #
  # Replaces SAS %AEstat macro (SAS lines 40-60).
  # PROC MEANS with BY &byvar, &wclause (WHERE filter), OUTPUT n=n
  # counts total rows per BY group (no VAR statement => row count).
  #
  # Arguments:
  #   data         — data frame (replaces dsin= parameter)
  #   by_vars      — character vector of grouping variables (replaces byvar=)
  #   where_clause — quosure list for filtering (replaces wclause=)
  #
  # The SAS paramcount auto-increment is not needed — results are returned

  # directly rather than written to sequential WORK datasets.
  # The SAS tclause parameter is omitted (see MIGRATION NOTES).
  # ---------------------------------------------------------------------------
  ae_stat <- function(data, by_vars, where_clause = NULL) {
    result <- data

    # Apply WHERE filter if provided (replaces SAS &wclause)
    if (!is.null(where_clause)) {
      result <- result %>%
        dplyr::filter(!!!where_clause)
    }

    # Sort and aggregate (replaces PROC SORT + PROC MEANS BY &byvar; OUTPUT n=n)
    result <- result %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(by_vars))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    return(result)
  }

  # ---------------------------------------------------------------------------
  # Call 1: Treatment arm level (SAS line 65)
  #   %AEstat(dsin=work.adae, byvar=trtan, tclause=trtemfl,
  #           wclause=%NRSTR(WHERE UPCASE(STRIP(SAFFL))='Y'
  #                          AND UPCASE(STRIP(AOCCFL))='Y'));
  # ---------------------------------------------------------------------------
  stat1 <- ae_stat(
    data         = adae,
    by_vars      = c("TRTAN"),
    where_clause = rlang::quos(
      toupper(trimws(SAFFL)) == "Y",
      toupper(trimws(AOCCFL)) == "Y"
    )
  )

  # ---------------------------------------------------------------------------
  # Call 2: System Organ Class level (SAS line 68)
  #   %AEstat(dsin=work.adae, byvar=trtan aesoc, tclause=AOCCSFL,
  #           wclause=%NRSTR(WHERE UPCASE(STRIP(SAFFL))='Y'
  #                          AND UPCASE(STRIP(AOCCSFL))='Y'));
  # ---------------------------------------------------------------------------
  stat2 <- ae_stat(
    data         = adae,
    by_vars      = c("TRTAN", "AESOC"),
    where_clause = rlang::quos(
      toupper(trimws(SAFFL)) == "Y",
      toupper(trimws(AOCCSFL)) == "Y"
    )
  )

  # ---------------------------------------------------------------------------
  # Call 3: Preferred Term level (SAS line 71)
  #   %AEstat(dsin=work.adae, byvar=trtan aesoc aedecod, tclause=AOCCSFL,
  #           wclause=%NRSTR(WHERE UPCASE(STRIP(SAFFL))='Y'
  #                          AND UPCASE(STRIP(AOCCPFL))='Y'));
  # ---------------------------------------------------------------------------
  stat3 <- ae_stat(
    data         = adae,
    by_vars      = c("TRTAN", "AESOC", "AEDECOD"),
    where_clause = rlang::quos(
      toupper(trimws(SAFFL)) == "Y",
      toupper(trimws(AOCCPFL)) == "Y"
    )
  )

  # ---------------------------------------------------------------------------
  # Return results as a named list
  # Replaces SAS sequential WORK datasets: work.stat1, work.stat2, work.stat3
  # ---------------------------------------------------------------------------
  list(
    num_groups = num_groups,
    stat1      = stat1,
    stat2      = stat2,
    stat3      = stat3
  )
}

# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#   - PROC MEANS with n=n output and no VAR statement counts total rows per
#     BY group. This is replicated with dplyr::n().
#   - The SAS `tclause` parameter is declared but not functionally used in the
#     PROC MEANS body (no VAR &tclause statement). It appears to be
#     metadata-only. This parameter is NOT replicated in the R function.
#   - SAS UPCASE(STRIP(...)) is replicated as toupper(trimws(...)) to
#     strip leading/trailing whitespace and uppercase before comparison.
#   - haven::read_xpt() maps SAS numeric missing (.) to NA and SAS character
#     missing (' ') to NA_character_ by default — no additional handling needed.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   - None expected — all outputs are integer row counts.
#   - Row ordering within groups may differ from SAS. SAS PROC SORT followed
#     by PROC MEANS BY guarantees sorted output; R dplyr::arrange() is applied
#     before group_by to match this behaviour.
#
# NO DIRECT R EQUIVALENT:
#   - SAS %EVAL(&paramcount. + 1) auto-increment counter producing sequential
#     WORK datasets (stat1, stat2, stat3) → replaced by named list elements.
#   - SAS sequential WORK datasets → R named list return value.
#   - SAS %NRSTR macro quoting for WHERE clause → rlang::quos() quosure list
#     with !!! splice operator for non-standard evaluation in dplyr::filter().
#   - SAS %LOCAL / %LET / %PUT macro variable scope → standard R function
#     scoping with message() for diagnostics.
#
# PACKAGE SELECTION RATIONALE:
#   - haven: SAS XPT file I/O (tidyverse standard, read_xpt for ADaM datasets)
#   - dplyr: Data manipulation replacing PROC SORT, PROC MEANS BY-group
#     aggregation, and PROC SQL count(distinct)
#   - rlang: Tidy evaluation — quos() constructs quosure lists for the
#     ae_stat() inner function's where_clause parameter, replacing SAS %NRSTR
#     macro quoting
#   - janitor: Loaded for SAS-compatible round_half_up() per migration
#     framework requirement (Gate 2 rounding audit compliance). Not actively
#     used in this script as all outputs are integer counts.
#
# OPEN QUESTIONS:
#   - Confirm AOCCFL vs AOCCSFL vs AOCCPFL variable naming in the target ADAE
#     dataset (three distinct flag variables for overall, SOC, and PT levels).
#   - Verify that PROC MEANS n statistic without a VAR statement matches
#     dplyr::n() row count semantics (both count rows per group).
#   - The `tclause` parameter (values: trtemfl, AOCCSFL) appears unused in the
#     SAS PROC MEANS body — confirm intent with the original author.
# =============================================================================
