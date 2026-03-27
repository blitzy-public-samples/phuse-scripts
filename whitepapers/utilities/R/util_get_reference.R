# ============================================================
# util_get_reference.R
# Migration of: whitepapers/utilities/util_get_reference_lines.sas
# ============================================================
# Purpose : Determine which reference lines to include in a
#           graphic based on normal range limits (LOW/HIGH).
#           Returns a numeric vector of reference line positions.
#           Used by WPCT boxplot scripts for overlay reference
#           lines (e.g., geom_hline in ggplot2).
# Author  : Dante Di Tommaso (original SAS macro)
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# Renamed : util_get_reference_lines -> util_get_reference
#           (per AAP section 0.4.1)
# ============================================================

# --- External dependency checks -------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for util_get_reference. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for util_get_reference. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for util_get_reference. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist -------------------
# Mirrors SAS %include for %assert_dset_exist (SAS line 60).
if (!exists("assert_dset_exist", mode = "function", inherits = TRUE)) {
  local({
    script_dir <- tryCatch(
      dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
      error = function(e) NULL
    )
    candidates <- c(
      if (!is.null(script_dir)) file.path(script_dir, "assert_dset_exist.R"),
      "whitepapers/utilities/R/assert_dset_exist.R",
      file.path(".", "assert_dset_exist.R")
    )
    for (f in candidates) {
      if (file.exists(f)) {
        source(f, local = FALSE)
        return(invisible(NULL))
      }
    }
  })
}

# --- Internal dependency: assert_var_exist --------------------
# Mirrors SAS %include for %assert_var_exist (SAS lines 62-66).
if (!exists("assert_var_exist", mode = "function", inherits = TRUE)) {
  local({
    script_dir <- tryCatch(
      dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
      error = function(e) NULL
    )
    candidates <- c(
      if (!is.null(script_dir)) file.path(script_dir, "assert_var_exist.R"),
      "whitepapers/utilities/R/assert_var_exist.R",
      file.path(".", "assert_var_exist.R")
    )
    for (f in candidates) {
      if (file.exists(f)) {
        source(f, local = FALSE)
        return(invisible(NULL))
      }
    }
  })
}

#' Determine Reference Lines for Graphics
#'
#' From input data with normal reference range limits (e.g., lab or vital sign
#' data), determine which reference lines to include in a graphic based on a
#' user-selected rule. This is the R migration of the SAS macro
#' \code{\%util_get_reference_lines(ds, sym, low_var=, high_var=, ref_lines=NONE)}
#' from \code{whitepapers/utilities/util_get_reference_lines.sas}.
#'
#' @param df A data frame containing measurement values and normal range
#'   variables. Replaces SAS \code{DS} positional parameter. Data set options
#'   (e.g., WHERE clause) should be applied before calling this function.
#' @param low_var Character string naming the column with the low value of the
#'   normal range (e.g., \code{"ANRLO"}). \code{NULL} if not applicable.
#'   Replaces SAS \code{LOW_VAR} keyword parameter.
#' @param high_var Character string naming the column with the high value of
#'   the normal range (e.g., \code{"ANRHI"}). \code{NULL} if not applicable.
#'   Replaces SAS \code{HIGH_VAR} keyword parameter.
#' @param ref_lines Control parameter specifying which reference lines to
#'   display. Accepts one of the following:
#'   \describe{
#'     \item{\code{"NONE"}}{(default) No reference lines displayed.}
#'     \item{\code{"UNIFORM"}}{Display reference lines only if all
#'       observations share exactly one set of LOW/HIGH values.}
#'     \item{\code{"NARROW"}}{If multiple reference ranges exist, display
#'       the narrowest band: \code{max(LOW)} and \code{min(HIGH)}.}
#'     \item{\code{"ALL"}}{Display all unique LOW and HIGH values (can be
#'       visually confusing with many ranges).}
#'     \item{Numeric vector}{Explicit reference line positions, e.g.,
#'       \code{c(50, 75, 100)}.}
#'     \item{Character string of numbers}{Space-delimited numeric values,
#'       e.g., \code{"-5 0 5"}, parsed to a numeric vector.}
#'   }
#'   Replaces SAS \code{REF_LINES} keyword parameter.
#'
#' @return A numeric vector of sorted, unique reference line positions, or
#'   \code{NULL} if no reference lines should be drawn. Replaces the SAS
#'   global macro variable assignment (\code{\%let \&sym = ...}).
#'
#' @details
#' \strong{Provide either \code{low_var}, \code{high_var}, or both.} If you
#' provide neither (and \code{ref_lines} is a keyword), the function returns
#' \code{NULL}.
#'
#' The function mirrors the SAS macro logic:
#' \enumerate{
#'   \item Validates the data frame and variables via
#'     \code{\link{assert_dset_exist}} and \code{\link{assert_var_exist}}
#'     (SAS lines 60-66).
#'   \item Determines whether LOW only, HIGH only, or BOTH variables are
#'     provided (SAS lines 69-77, \code{lhb} flag).
#'   \item Validates the \code{ref_lines} parameter: must be a recognized
#'     keyword or numeric values (SAS lines 80-95).
#'   \item If numeric values are provided directly, returns them sorted and
#'     unique (SAS lines 98-100).
#'   \item For keyword modes, extracts distinct LOW/HIGH combinations from
#'     the data (SAS lines 105-111 PROC SQL).
#'   \item Applies mode-specific logic:
#'     \itemize{
#'       \item \strong{UNIFORM}: Only returns values if exactly one reference
#'         range exists (SAS lines 135-138).
#'       \item \strong{NARROW}: Uses \code{max(LOW)} and \code{min(HIGH)} to
#'         define the narrowest band (SAS lines 139-146).
#'       \item \strong{ALL}: Returns all unique LOW and HIGH values.
#'     }
#'   \item Builds sorted, unique return vector (SAS lines 153-168).
#' }
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{\code{whitepapers/utilities/util_get_reference_lines.sas}
#'     (188 lines)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{SAS Output}{Global macro var specified in SYM with numeric values
#'     for reference lines, as required by VREF= option in PROC SHEWHART
#'     BOXCHART statement}
#' }
#'
#' @examples
#' # Example with uniform reference range
#' lab_data <- data.frame(
#'   AVAL  = c(4.2, 5.1, 3.8, 4.9),
#'   ANRLO = c(3.5, 3.5, 3.5, 3.5),
#'   ANRHI = c(5.5, 5.5, 5.5, 5.5)
#' )
#' util_get_reference(lab_data, low_var = "ANRLO", high_var = "ANRHI",
#'                    ref_lines = "UNIFORM")
#' # Returns: c(3.5, 5.5)
#'
#' # Example with explicit numeric values
#' util_get_reference(lab_data, ref_lines = c(3.0, 5.0))
#' # Returns: c(3.0, 5.0)
#'
#' @export
util_get_reference <- function(df,
                               low_var  = NULL,
                               high_var = NULL,
                               ref_lines = "NONE") {

  # ================================================================
  # STEP 0 — Validate data frame existence

  # Mirrors SAS line 60:
  #   %let OK = %assert_dset_exist(%scan(&ds,1,%str( %()));
  # ================================================================
  ok <- tryCatch(
    assert_dset_exist(df),
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_GET_REFERENCE) Unable to determine reference lines ",
        "based on parameters provided. Data frame validation failed."
      )
    )
  }

  # ================================================================
  # STEP 1 — Parse and validate the ref_lines parameter EARLY
  # Mirrors SAS lines 80-95:
  #   If keyword (NONE, UNIFORM, NARROW, ALL) → use keyword logic.
  #   If space-delimited numeric string → parse to numeric.
  #   If any non-numeric token found → ERROR + suppress ref lines.
  # NOTE: This step is performed before variable validation because
  #       numeric ref_lines and NONE do not require low_var/high_var.
  # ================================================================
  ref_nums <- FALSE
  ref_keyword <- NULL

  if (is.numeric(ref_lines)) {
    # Numeric vector provided directly — use as-is after validation
    if (any(is.na(ref_lines))) {
      cli::cli_warn(
        "(UTIL_GET_REFERENCE) NA values removed from ref_lines numeric vector."
      )
      ref_lines <- ref_lines[!is.na(ref_lines)]
    }
    if (length(ref_lines) == 0L) {
      return(invisible(NULL))
    }
    ref_nums <- TRUE

  } else if (is.character(ref_lines) && length(ref_lines) == 1L) {
    # Single character string — may be a keyword or space-delimited numbers
    ref_upper <- toupper(trimws(ref_lines))
    valid_keywords <- c("NONE", "UNIFORM", "NARROW", "ALL")

    if (ref_upper %in% valid_keywords) {
      ref_keyword <- ref_upper
    } else if (nzchar(ref_upper)) {
      # Attempt to parse as space-delimited numeric values
      # Mirrors SAS lines 84-95: scan tokens and check %datatyp
      tokens <- strsplit(trimws(ref_lines), "\\s+")[[1L]]
      parsed <- suppressWarnings(as.numeric(tokens))

      if (any(is.na(parsed))) {
        bad_tokens <- tokens[is.na(parsed)]
        cli::cli_abort(
          paste0(
            "(UTIL_GET_REFERENCE) Expecting numeric values only, not \"",
            paste(bad_tokens, collapse = "\", \""),
            "\". Suppressing reference lines."
          )
        )
      }
      ref_lines <- parsed
      ref_nums  <- TRUE
    } else {
      # Empty string — treat as NONE
      ref_keyword <- "NONE"
    }
  } else if (is.character(ref_lines) && length(ref_lines) > 1L) {
    # Character vector — attempt to parse all elements as numeric
    parsed <- suppressWarnings(as.numeric(ref_lines))
    if (any(is.na(parsed))) {
      bad_vals <- ref_lines[is.na(parsed)]
      cli::cli_abort(
        paste0(
          "(UTIL_GET_REFERENCE) Expecting numeric values only, not \"",
          paste(bad_vals, collapse = "\", \""),
          "\". Suppressing reference lines."
        )
      )
    }
    ref_lines <- parsed
    ref_nums  <- TRUE
  } else {
    cli::cli_abort(
      "(UTIL_GET_REFERENCE) ref_lines must be a character keyword, numeric vector, or space-delimited numeric string."
    )
  }

  # ================================================================
  # STEP 2 — If numeric values were provided directly, return them
  # Mirrors SAS lines 98-100:
  #   %if 1 = &OK and 1 = &ref_nums %then %let &sym = &ref_lines;
  # Numeric ref_lines do NOT require low_var/high_var.
  # ================================================================
  if (ref_nums) {
    result <- sort(unique(ref_lines))
    cli::cli_inform(
      paste0(
        "(UTIL_GET_REFERENCE) Successfully determined reference values: ",
        paste(result, collapse = " "), "."
      )
    )
    return(result)
  }

  # ================================================================
  # STEP 3 — Handle NONE keyword immediately
  # ================================================================
  if (identical(ref_keyword, "NONE")) {
    cli::cli_inform(
      "(UTIL_GET_REFERENCE) ref_lines = NONE. No reference lines requested."
    )
    return(invisible(NULL))
  }

  # ================================================================
  # STEP 4 — Validate low_var and high_var columns
  # (Only reached for keyword modes: UNIFORM, NARROW, ALL)
  # Mirrors SAS lines 62-66:
  #   %if 1 = &OK and %length(&low_var) > 0 %then
  #       %let OK = %assert_var_exist(..., &low_var);
  #   %if 1 = &OK and %length(&high_var) > 0 %then
  #       %let OK = %assert_var_exist(..., &high_var);
  # ================================================================
  if (!rlang::is_null(low_var) && nzchar(low_var)) {
    low_ok <- tryCatch(
      assert_var_exist(df, low_var),
      error = function(e) FALSE
    )
    if (!isTRUE(low_ok)) {
      cli::cli_abort(
        paste0(
          "(UTIL_GET_REFERENCE) Variable '", toupper(low_var),
          "' not found in data frame. See log messages."
        )
      )
    }
  } else {
    low_var <- NULL
  }

  if (!rlang::is_null(high_var) && nzchar(high_var)) {
    high_ok <- tryCatch(
      assert_var_exist(df, high_var),
      error = function(e) FALSE
    )
    if (!isTRUE(high_ok)) {
      cli::cli_abort(
        paste0(
          "(UTIL_GET_REFERENCE) Variable '", toupper(high_var),
          "' not found in data frame. See log messages."
        )
      )
    }
  } else {
    high_var <- NULL
  }

  # ================================================================
  # STEP 5 — Determine LOW/HIGH/BOTH flag (lhb)
  # Mirrors SAS lines 69-77:
  #   Always process 2 vars to keep code simple.
  #   lhb = B (both), L (low only), H (high only)
  #   When only one is supplied, the other is duplicated from it.
  # ================================================================
  if (!rlang::is_null(low_var) && !rlang::is_null(high_var)) {
    lhb <- "B"
    work_low  <- low_var
    work_high <- high_var
  } else if (!rlang::is_null(low_var)) {
    lhb <- "L"
    work_low  <- low_var
    work_high <- low_var
  } else if (!rlang::is_null(high_var)) {
    lhb <- "H"
    work_low  <- high_var
    work_high <- high_var
  } else {
    # Neither low_var nor high_var provided — nothing to compute
    cli::cli_inform(
      "(UTIL_GET_REFERENCE) Neither low_var nor high_var provided. Returning NULL."
    )
    return(invisible(NULL))
  }

  # ================================================================
  # STEP 6 — Extract distinct LOW/HIGH combinations from the data
  # Mirrors SAS lines 105-111:
  #   proc sql noprint;
  #     select distinct &low_var, &high_var,
  #            count(&low_var) + nmiss(&low_var)
  #     from &ds
  #     where n(&low_var, &high_var) > 0
  #     group by &low_var, &high_var;
  #   quit;
  #
  # The SAS WHERE n(low, high) > 0 removes rows where BOTH are
  # missing. In R: filter rows where at least one of the range
  # vars is non-missing.
  # ================================================================
  select_vars <- unique(c(work_low, work_high))

  ranges <- df %>%
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(select_vars),
        ~ !is.na(.)
      )
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(select_vars))) %>%
    dplyr::filter(
      dplyr::if_any(dplyr::everything(), ~ !is.na(.))
    )

  # Count observations per distinct range combination for logging
  # Uses .data pronoun (from rlang, re-exported by dplyr) for safe

  # programmatic column access inside the data mask.
  range_counts <- df %>%
    dplyr::filter(
      !is.na(.data[[work_low]]) | !is.na(.data[[work_high]])
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(select_vars))) %>%
    dplyr::summarise(.n_obs = dplyr::n(), .groups = "drop")

  range_count <- nrow(ranges)

  # ================================================================
  # STEP 7 — Log distinct reference range information
  # Mirrors SAS lines 118-131:
  #   %put NOTE: (UTIL_GET_REFERENCE_LINES) &range_count distinct ...
  #   ... loop logging each range with LOW, HIGH (obs count)
  # ================================================================
  cli::cli_inform(
    paste0(
      "(UTIL_GET_REFERENCE) ", range_count,
      " distinct reference range(s) in the data."
    )
  )

  if (range_count > 0L) {
    cli::cli_inform("(UTIL_GET_REFERENCE) LOW , HIGH (number of observations)")

    for (i in seq_len(nrow(range_counts))) {
      lo_val <- range_counts[[work_low]][i]
      hi_val <- range_counts[[work_high]][i]
      n_obs  <- range_counts[[".n_obs"]][i]

      if (lhb == "L") {
        cli::cli_inform(
          paste0("(UTIL_GET_REFERENCE) ", lo_val, " , --- (", n_obs, ")")
        )
      } else if (lhb == "H") {
        cli::cli_inform(
          paste0("(UTIL_GET_REFERENCE) --- , ", hi_val, " (", n_obs, ")")
        )
      } else {
        cli::cli_inform(
          paste0("(UTIL_GET_REFERENCE) ", lo_val, " , ", hi_val, " (", n_obs, ")")
        )
      }
    }

    cli::cli_inform(
      "(UTIL_GET_REFERENCE) If you see duplicate values, check the precision in your data."
    )
  }

  # ================================================================
  # STEP 8 — Apply mode-specific logic
  # Mirrors SAS lines 134-148
  # ================================================================
  if (range_count == 0L) {
    # No non-missing reference ranges found — suppress
    ref_keyword <- "NONE"
  } else if (identical(ref_keyword, "UNIFORM")) {
    # Mirrors SAS lines 135-138:
    #   If multiple reference ranges, then draw NONE
    if (range_count != 1L) {
      cli::cli_inform(
        paste0(
          "(UTIL_GET_REFERENCE) Non-uniform reference limits detected ",
          "(", range_count, " distinct ranges), so suppressing reference lines."
        )
      )
      return(invisible(NULL))
    }
    # Exactly one range — use its values (fall through to value collection)

  } else if (identical(ref_keyword, "NARROW")) {
    # Mirrors SAS lines 139-146:
    #   If multiple ranges, use max(LOW) and min(HIGH)
    if (range_count > 1L) {
      lo_narrow <- max(ranges[[work_low]], na.rm = TRUE)
      hi_narrow <- min(ranges[[work_high]], na.rm = TRUE)

      # Rebuild ranges as single narrowed row
      narrow_df <- data.frame(
        lo_narrow,
        hi_narrow,
        stringsAsFactors = FALSE
      )
      names(narrow_df) <- c(work_low, work_high)
      ranges <- narrow_df
      range_count <- 1L
    }

  } else if (identical(ref_keyword, "ALL")) {
    # ALL mode — use all distinct values (no filtering)
    # Fall through to value collection

  }

  # Final check after mode logic

  if (identical(ref_keyword, "NONE")) {
    cli::cli_inform(
      "(UTIL_GET_REFERENCE) No valid reference ranges found. Returning NULL."
    )
    return(invisible(NULL))
  }

  # ================================================================
  # STEP 9 — Collect, de-duplicate, and sort reference line values
  # Mirrors SAS lines 153-168:
  #   DATA step creates grl_temp with LOW and/or HIGH values,
  #   PROC SQL SELECT DISTINCT val ... ORDER BY val → sorted unique.
  #
  # Collect LOW and/or HIGH values based on lhb flag:
  #   B → both LOW and HIGH
  #   L → LOW only (but work_high == work_low, so just collect work_low)
  #   H → HIGH only (but work_low == work_high, so just collect work_high)
  # ================================================================
  if (lhb == "B") {
    all_vals <- c(ranges[[work_low]], ranges[[work_high]])
  } else if (lhb == "L") {
    all_vals <- ranges[[work_low]]
  } else {
    # lhb == "H"
    all_vals <- ranges[[work_high]]
  }

  # Remove NAs, sort, and de-duplicate
  result <- sort(unique(stats::na.omit(all_vals)))

  if (length(result) == 0L) {
    cli::cli_inform(
      paste0(
        "(UTIL_GET_REFERENCE) Non-uniform reference limits detected, ",
        "so suppressing reference lines."
      )
    )
    return(invisible(NULL))
  }

  # ================================================================
  # STEP 10 — Report success and return
  # Mirrors SAS line 180:
  #   %put NOTE: (UTIL_GET_REFERENCE_LINES) Successfully created
  #   macro var %upcase(&SYM) with reference values (&&&SYM).;
  # ================================================================
  cli::cli_inform(
    paste0(
      "(UTIL_GET_REFERENCE) Successfully determined reference values: ",
      paste(result, collapse = " "), "."
    )
  )

  return(result)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Function renamed from util_get_reference_lines to
#      util_get_reference per AAP section 0.4.1.
#    - SAS global macro variable (SYM parameter) replaced by R
#      function return value (numeric vector). Callers assign
#      the result to a local variable instead of relying on
#      global side effects.
#    - ref_lines parameter accepts both keyword strings and
#      numeric vectors (or space-delimited numeric strings),
#      matching the full range of SAS REF_LINES behavior.
#    - LOW_VAR/HIGH_VAR can be NULL (optional), matching SAS
#      behavior where %length(&low_var) > 0 gates processing.
#    - SAS "always process 2 vars" simplification preserved:
#      when only LOW or HIGH is provided, the other is aliased
#      to the same column internally.
#    - Input data frame should be pre-filtered before calling
#      (replaces SAS dataset options like WHERE=).
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS PROC SQL min/max vs R min/max with na.rm=TRUE are
#      identical for non-missing numeric data.
#    - SAS line 131 warns about HEX duplicate values; R IEEE 754
#      doubles do not have this SAS-specific formatting issue.
#      The R equivalent message warns about precision instead.
#    - sort(unique()) in R uses standard IEEE 754 comparison;
#      SAS PROC SQL ORDER BY uses the same underlying comparison.
# NO DIRECT R EQUIVALENT:
#    - SAS PROC SQL INTO :sym separated by ' ' for building
#      space-delimited symbol values is replaced by R
#      sort(unique(values)) returning a numeric vector.
#    - SAS %SYMDEL / %GLOBAL for symbol lifecycle management
#      is not needed in R (function scope handles variable
#      lifecycle via garbage collection).
#    - SAS PROC DATASETS DELETE for temp dataset cleanup is not
#      needed in R (temporary data frames are garbage collected
#      when they leave function scope).
#    - SAS %datatyp() for checking if a token is numeric is
#      replaced by suppressWarnings(as.numeric()) + is.na()
#      check in R.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): distinct(), filter(), across(),
#      all_of(), if_any(), summarise(), n(), everything()
#      replacing SAS PROC SQL and DATA step operations for
#      extracting and processing reference range data. Mandated
#      by AAP section 0.8.1 (tidyverse over base R).
#    - rlang (>= 1.1.0): .data pronoun for safe programmatic
#      column access and is_null() for idiomatic NULL checks.
#      Standard tidyverse companion per AAP section 0.8.1.
#    - cli (>= 3.6.0): cli_inform(), cli_warn(), cli_abort()
#      for informative user-facing messages replacing SAS
#      %PUT NOTE/WARNING/ERROR format throughout the function.
# OPEN QUESTIONS:
#    - Should the returned vector include names (e.g., "LOW" /
#      "HIGH") for downstream traceability?
#    - Should the function accept a ggplot2 scale object
#      directly for geom_hline/geom_vline integration?
#    - When NARROW mode narrows to max(LOW) > min(HIGH)
#      (inverted band), should this be flagged as a warning?
# ============================================================
