# =============================================================================
# util_boxplot_visit_ranges.R
# =============================================================================
# Migrated from: whitepapers/utilities/obsolete_util_boxplot_ranges.sas
# Original Author: Dante Di Tommaso
# Migration: SAS macro %obsolete_util_boxplot_ranges -> R function
#
# Purpose: Visit range computation for boxplot x-axis pagination.
#          Keeps all treatments within each visit block together on a page.
#          Produces range filter strings for subsetting visits per plot page.
#
# SAS Source: 133 lines — macro with RETAIN-based DATA step accumulation,
#             PROC SORT NODUPKEY, %util_count_unique_values, VTYPE/VLENGTH
# R Target:   Parameterized function using dplyr pipelines, rlang tidy eval,
#             cli messaging — returns structured list instead of global symbol
# =============================================================================

# External package imports (per AAP §0.6.1 and §0.8.1 tidyverse mandate)
library(dplyr)
library(rlang)
library(cli)

# -----------------------------------------------------------------------------
#' Compute Boxplot Visit Ranges for Pagination
#'
#' Calculates visit ranges to limit the number of boxes per boxplot page.
#' All treatments within each visit block are kept together on the same page.
#' Either all treatments in Visit X appear on this page, or they all move to
#' the next page.
#'
#' Migrated from SAS macro \code{\%obsolete_util_boxplot_ranges(ds, vvisn=, vtrtn=)}.
#' The SAS macro set a global symbol \code{BOXPLOT_VISIT_RANGES}; this R function
#' returns a structured list instead.
#'
#' @param df Data frame containing measurement data with visit and treatment
#'   columns. Corresponds to the SAS \code{DS} parameter (one-level WORK
#'   dataset).
#' @param visit_var Character string naming the visit variable in \code{df}.
#'   Typically a numeric visit number (e.g., \code{"AVISITN"}) but character
#'   visit identifiers are also supported with appropriate warnings.
#'   Corresponds to the SAS \code{VVISN} parameter.
#' @param trt_var Character string naming the treatment variable in \code{df}.
#'   Typically a numeric treatment number (e.g., \code{"TRTPN"}).
#'   Corresponds to the SAS \code{VTRTN} parameter.
#' @param max_boxes_per_page Integer specifying the maximum number of boxes
#'   allowed per plot page. Corresponds to the SAS global symbol
#'   \code{MAX_BOXES_PER_PAGE}. Must be a positive integer.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{ranges}{Character vector with one element per page, each containing
#'       a filter expression of the form \code{"start <= visit_var <= end"} for
#'       numeric visits or \code{'"start" <= visit_var <= "end"'} for character
#'       visits.}
#'     \item{range_string}{Single character string with all page ranges
#'       concatenated using \code{"|"} as delimiter, matching the SAS global
#'       symbol \code{BOXPLOT_VISIT_RANGES} output format.}
#'   }
#'
#' @details
#' The function replicates the SAS macro's pagination logic:
#' \enumerate{
#'   \item Validates that \code{visit_var} and \code{trt_var} exist in \code{df}
#'   \item Warns about missing values (replacing SAS \code{\%assert_var_nonmissing})
#'   \item Counts unique visits and treatments (replacing SAS
#'     \code{\%util_count_unique_values})
#'   \item Warns if treatment count exceeds \code{max_boxes_per_page}
#'   \item Extracts distinct visit-treatment pairs sorted by visit then treatment
#'     (replacing SAS \code{PROC SORT NODUPKEY})
#'   \item Iterates through visits accumulating box counts, outputting a range
#'     string when the page is full or data is exhausted (replacing SAS
#'     \code{DATA step} with \code{RETAIN})
#' }
#'
#' The conservative paging check uses the total unique treatment count
#' (\code{num_trt}) rather than the next visit's actual treatment count,
#' preserving the SAS macro's worst-case assumption that every treatment
#' could appear at every visit.
#'
#' @examples
#' \dontrun{
#'   # Example with ADaM-style lab data
#'   plot_data <- data.frame(
#'     AVISITN = c(0, 0, 0, 4, 4, 4, 8, 8, 8, 12, 12, 12),
#'     TRTPN   = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1,  2,  3),
#'     AVAL    = rnorm(12)
#'   )
#'   result <- util_boxplot_visit_ranges(
#'     df = plot_data,
#'     visit_var = "AVISITN",
#'     trt_var = "TRTPN",
#'     max_boxes_per_page = 6
#'   )
#'   # result$ranges:       c("0 <= AVISITN <= 4", "8 <= AVISITN <= 12")
#'   # result$range_string: "0 <= AVISITN <= 4|8 <= AVISITN <= 12|"
#' }
#'
#' @export
# -----------------------------------------------------------------------------
util_boxplot_visit_ranges <- function(df,
                                      visit_var,
                                      trt_var,
                                      max_boxes_per_page) {

  # ===========================================================================
  # SECTION 1: Input Validation
  # Replaces SAS lines 33-34: %assert_depend(vars=..., symbols=max_boxes_per_page)
  # ===========================================================================


  # Validate df is a data frame

  if (!is.data.frame(df)) {
    stop(
      "`df` must be a data frame, not ",
      class(df)[1L],
      ".",
      call. = FALSE
    )
  }

  # Validate visit_var is a single character string (replaces SAS positional param)
  if (!rlang::is_character(visit_var) || length(visit_var) != 1L) {
    stop(
      "`visit_var` must be a single character string naming the visit variable.",
      call. = FALSE
    )
  }

  # Validate trt_var is a single character string (replaces SAS positional param)
  if (!rlang::is_character(trt_var) || length(trt_var) != 1L) {
    stop(
      "`trt_var` must be a single character string naming the treatment variable.",
      call. = FALSE
    )
  }

  # Validate max_boxes_per_page is a positive integer
  # rlang::is_scalar_integerish() checks for single integer-like numeric value
  if (!rlang::is_scalar_integerish(max_boxes_per_page) ||
      max_boxes_per_page < 1L) {
    stop(
      "`max_boxes_per_page` must be a positive integer, not ",
      deparse(max_boxes_per_page),
      ".",
      call. = FALSE
    )
  }
  max_boxes_per_page <- as.integer(max_boxes_per_page)

  # Validate that visit_var and trt_var exist in df
  # Replaces SAS %assert_depend(vars=%str(&DS : &vvisn &vtrtn))
  if (!(visit_var %in% names(df))) {
    stop(
      "Variable `", visit_var, "` not found in `df`. ",
      "Available columns: ",
      paste(names(df), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!(trt_var %in% names(df))) {
    stop(
      "Variable `", trt_var, "` not found in `df`. ",
      "Available columns: ",
      paste(names(df), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Validate df has at least one row

  if (nrow(df) == 0L) {
    stop(
      "`df` has zero rows. Cannot compute visit ranges from empty data.",
      call. = FALSE
    )
  }

  # ===========================================================================
  # SECTION 2: Missing Value Warnings
  # Replaces SAS lines 38-39:
  #   %if not %assert_var_nonmissing(&ds, &vvisn) %then %put WARNING: ...
  #   %if not %assert_var_nonmissing(&ds, &vtrtn) %then %put WARNING: ...
  # ===========================================================================

  visit_na_count <- sum(is.na(df[[visit_var]]))
  if (visit_na_count > 0L) {
    cli::cli_warn(c(
      "!" = paste0(
        "Variable {.var ", visit_var, "} has {visit_na_count} missing ",
        "value{?s}. Results may be unexpected."
      ),
      "i" = "Missing values will be excluded from visit range computation."
    ))
  }

  trt_na_count <- sum(is.na(df[[trt_var]]))
  if (trt_na_count > 0L) {
    cli::cli_warn(c(
      "!" = paste0(
        "Variable {.var ", trt_var, "} has {trt_na_count} missing ",
        "value{?s}. Results may be unexpected."
      ),
      "i" = "Missing values will be excluded from visit range computation."
    ))
  }

  # ===========================================================================
  # SECTION 3: Unique Counts
  # Replaces SAS lines 42-43:
  #   %util_count_unique_values(&ds, &vvisn, numvis)
  #   %util_count_unique_values(&ds, &vtrtn, numtrt)
  # ===========================================================================

  num_vis <- dplyr::n_distinct(df[[visit_var]], na.rm = TRUE)
  num_trt <- dplyr::n_distinct(df[[trt_var]], na.rm = TRUE)

  # Handle edge case: all values missing after NA removal

  if (num_vis == 0L) {
    stop(
      "No non-missing values found in `", visit_var,
      "`. Cannot compute visit ranges.",
      call. = FALSE
    )
  }
  if (num_trt == 0L) {
    stop(
      "No non-missing values found in `", trt_var,
      "`. Cannot compute visit ranges.",
      call. = FALSE
    )
  }

  # ===========================================================================
  # SECTION 4: Treatment Count Warning
  # Replaces SAS lines 45-47:
  #   %if &numtrt > &max_boxes_per_page %then %do;
  #     %put WARNING: Treatment count (&NUMTRT) > Max boxes per page ...
  #   %end;
  # ===========================================================================

  if (num_trt > max_boxes_per_page) {
    cli::cli_warn(c(
      "!" = paste0(
        "Treatment count ({num_trt}) is greater than max boxes per page ",
        "({max_boxes_per_page}). Keeping treatments together, max boxes ",
        "per page is effectively {num_trt}."
      )
    ))
  }

  # ===========================================================================
  # SECTION 5: Visit Variable Type Detection
  # Replaces SAS lines 51-59, 71-74:
  #   data _null_; call symput('vistyp', vtype(&vvisn)); ...
  #   %if &vistyp = C %then %put WARNING: ...
  # SAS VTYPE/VLENGTH -> R is.numeric()/is.character()
  # ===========================================================================

  visit_col <- df[[visit_var]]
  is_numeric_visit <- is.numeric(visit_col)

  if (!is_numeric_visit) {
    cli::cli_warn(c(
      "!" = paste0(
        "Expecting numeric visit numbers, not {.cls {class(visit_col)[1L]}}. ",
        "Results may be unexpected."
      ),
      "i" = "Character visit values will be quoted in range strings."
    ))
  }

  # ===========================================================================
  # SECTION 6: Distinct Visit-Treatment Pairs (sorted)
  # Replaces SAS lines 77-80:
  #   proc sort data=&ds (keep=&vvisn &vtrtn) out=temp_vis_trt nodupkey;
  #     by &vvisn &vtrtn;
  #   run;
  # Uses dplyr::distinct() with across(all_of()) and dplyr::arrange()
  # ===========================================================================

  vis_trt <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(visit_var, trt_var)))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(visit_var, trt_var))))

  # Remove rows with NA in visit or treatment variables (SAS handles implicitly)
  # Uses .data pronoun (from rlang, loaded via library(rlang)) for safe

  # programmatic column access within the dplyr data mask context
  vis_trt <- vis_trt %>%
    dplyr::filter(
      !is.na(.data[[visit_var]]) & !is.na(.data[[trt_var]])
    )

  # ===========================================================================
  # SECTION 7: Count Treatments per Visit
  # Uses dplyr::count() to summarize the number of unique treatments
  # appearing at each visit level, for use in the page assignment loop.
  # ===========================================================================

  trt_per_visit <- vis_trt %>%
    dplyr::count(dplyr::across(dplyr::all_of(visit_var)), name = "n_trt") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(visit_var)))

  # Extract ordered visit values and per-visit treatment counts
  visits <- trt_per_visit[[visit_var]]
  n_trts <- trt_per_visit[["n_trt"]]
  n_visits <- length(visits)

  # ===========================================================================
  # SECTION 8: Page Assignment and Range Building
  # Replaces SAS lines 82-124: DATA step with RETAIN
  #
  # SAS Logic (row-by-row through sorted visit-treatment pairs):
  #   RETAIN boxes_on_page 0, start_visit .;
  #   if 0 = boxes_on_page then start_visit = &vvisn;
  #   boxes_on_page + 1;
  #   if last.&vvisn then do;
  #     if NoMore or boxes_on_page + &numtrt > &max_boxes_per_page then do;
  #       ...build range string...
  #       boxes_on_page = 0;
  #     end;
  #   end;
  #
  # R equivalent: iterate through unique visits with per-visit treatment counts.
  # boxes_on_page accumulates actual treatment counts per visit (functionally

  # identical to incrementing once per visit-treatment pair in the SAS step).
  # The paging check uses num_trt (total unique treatments) as a conservative
  # worst-case for the next visit's treatment count, preserving SAS behavior.
  # ===========================================================================

  ranges <- character(0L)
  boxes_on_page <- 0L
  start_visit <- visits[1L]

  for (i in seq_len(n_visits)) {
    # When starting a new page, record the first visit
    # Replaces SAS: if 0 = boxes_on_page then start_visit = &vvisn;
    if (boxes_on_page == 0L) {
      start_visit <- visits[i]
    }

    # Accumulate boxes for this visit (one box per treatment at this visit)
    # Replaces SAS: boxes_on_page + 1; (executed once per visit-treatment row)
    boxes_on_page <- boxes_on_page + n_trts[i]

    # At the end of this visit, check if the next visit would overflow the page
    # Replaces SAS: if last.&vvisn then do;
    #   if NoMore or boxes_on_page + &numtrt > &max_boxes_per_page then do;
    is_last_visit <- (i == n_visits)

    if (is_last_visit ||
        (boxes_on_page + num_trt > max_boxes_per_page)) {
      # Build range string for this page
      end_visit <- visits[i]

      if (is_numeric_visit) {
        # Numeric visit: "start <= visit_var <= end"
        # Replaces SAS: put(start_visit, best8.-L) !!" <= &vvisn <= "
        #               !!put(&vvisn, best8.-L)
        range_str <- paste0(
          format(start_visit, scientific = FALSE, trim = TRUE),
          " <= ", visit_var, " <= ",
          format(end_visit, scientific = FALSE, trim = TRUE)
        )
      } else {
        # Character visit: '"start" <= visit_var <= "end"'
        # Replaces SAS: quote(strip(start_visit)) !!" <= &vvisn <= "
        #               !!quote(strip(&vvisn))
        range_str <- paste0(
          '"', trimws(as.character(start_visit)), '"',
          " <= ", visit_var, " <= ",
          '"', trimws(as.character(end_visit)), '"'
        )
      }

      ranges <- c(ranges, range_str)

      # Reset for next page
      # Replaces SAS: boxes_on_page = 0;
      boxes_on_page <- 0L
    }
  }

  # ===========================================================================
  # SECTION 9: Build Pipe-Delimited Range String
  # Replaces SAS: call symput('boxplot_visit_ranges', strip(boxplot_visit_ranges))
  # SAS format terminates each range with "|", so the full string ends with "|"
  # ===========================================================================

  range_string <- paste0(paste0(ranges, collapse = "|"), "|")

  # ===========================================================================
  # SECTION 10: Informational Messages
  # Replaces SAS lines 128-129:
  #   %put Note: (OBSOLETE_UTIL_BOXPLOT_RANGES) Default visit ranges ...
  #   %put Note: (OBSOLETE_UTIL_BOXPLOT_RANGES) BOXPLOT_VISIT_RANGES set to: ...
  # ===========================================================================

  cli::cli_inform(c(
    "i" = paste0(
      "Visit ranges computed for boxplot pagination, limiting to ",
      "{max_boxes_per_page} boxes max per page."
    ),
    "i" = paste0(
      "BOXPLOT_VISIT_RANGES set to: {range_string}"
    ),
    "i" = paste0(
      "Number of pages: {length(ranges)}"
    )
  ))

  # ===========================================================================
  # SECTION 11: Return Value
  # SAS macro set a global symbol; R returns a structured list.
  # No temp dataset cleanup needed (replaces SAS line 126:
  #   %util_delete_dsets(temp_vis_trt))
  # ===========================================================================

  result <- list(
    ranges       = ranges,
    range_string = range_string
  )

  return(result)
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    - Migrated from obsolete_util_boxplot_ranges.sas (marked obsolete in SAS
#      codebase). Function retains the core pagination logic.
#    - Renamed to util_boxplot_visit_ranges per AAP target design (§0.4.1).
#    - max_boxes_per_page is passed as a function argument, replacing the SAS
#      global macro symbol MAX_BOXES_PER_PAGE.
#    - The global symbol BOXPLOT_VISIT_RANGES is replaced by a list return
#      value with $ranges (character vector) and $range_string (pipe-delimited).
#    - NA values in visit_var or trt_var are excluded from computation with
#      warning, matching the SAS %assert_var_nonmissing advisory behavior.
#    - The conservative paging check (boxes_on_page + num_trt > max) uses
#      the total unique treatment count as a worst-case upper bound, exactly
#      matching the SAS macro's logic on line 101.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Sort order preserved via dplyr::arrange() which provides a stable sort.
#      For numeric visit numbers, results are identical to SAS PROC SORT.
#      For character visits, R uses locale-dependent collation vs. SAS SORTSEQ;
#      ordering differences are possible with special characters.
#    - Numeric formatting: SAS BEST8. format vs R format() — both produce
#      compact representations for integers; differences possible for very
#      large or very small floating-point visit numbers (unlikely in practice).
#
# NO DIRECT R EQUIVALENT:
#    - SAS VTYPE/VLENGTH (line 53-54) -> R is.numeric()/is.character() for
#      type detection. R does not have a fixed-length character concept like
#      SAS $N format; character lengths are dynamic.
#    - SAS global BOXPLOT_VISIT_RANGES symbol -> R function return value (list).
#      Callers must capture the return value instead of referencing a global.
#    - SAS %assert_depend (line 33) -> R stopifnot() / stop() for input
#      validation. The dependency assertion pattern is replaced by explicit
#      parameter checks.
#    - SAS %util_delete_dsets (line 126) -> No R equivalent needed. R garbage
#      collection handles temporary objects automatically.
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Tidyverse data manipulation per AAP §0.8.1 mandate. Replaces
#      SAS DATA step (RETAIN), PROC SORT NODUPKEY, and count operations.
#    - rlang: Tidy evaluation support for programmatic column name access
#      (.data pronoun) and input type validation (is_character, is_scalar_integerish).
#    - cli: User-facing messages replacing SAS %PUT NOTE/WARNING with
#      structured, formatted output.
#
# OPEN QUESTIONS:
#    - This SAS macro was marked obsolete in the SAS codebase (filename prefix
#      "obsolete_"). Should this R version be considered legacy/deprecated?
#    - util_boxplot_block_ranges.R is the more general replacement in the
#      refactored R codebase. Consider whether this function should carry a
#      .Deprecated() notice.
#    - The range string format ("start <= var <= end|") is designed for SAS
#      WHERE clause parsing. R callers may prefer the $ranges vector for
#      direct subsetting via dplyr::filter() expressions.
# =============================================================================
