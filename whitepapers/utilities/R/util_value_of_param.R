# ============================================================
# util_value_of_param.R
# Migration of: whitepapers/utilities/util_value_format.sas
# ============================================================
# Purpose : Determine the appropriate display format (decimal places)
#           for MEAN and STDDEV based on the precision of measured
#           values in a given numeric variable. Returns format
#           specifications as a named list.
#
# SAS Macro: %util_value_format(ds, var, sym=util_value_format, whr=)
# Renamed :  util_value_format -> util_value_of_param per AAP section 0.4.1
# Author  :  Dante Di Tommaso (original SAS macro)
# Migrated:  SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency checks --------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for util_value_of_param. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for util_value_of_param. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for util_value_of_param. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  stop(
    "Package 'stringr' (>= 1.5.0) is required for util_value_of_param. ",
    "Install with: install.packages('stringr')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist & assert_var_exist -
# Mirrors SAS %include for %assert_dset_exist (SAS line 35)
# and %assert_var_exist (SAS line 36). Functions are sourced from
# the same directory if not already available in the current session.
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

#' Determine Display Format for MEAN and STDDEV
#'
#' Scans the precision of measured values in a numeric variable and returns
#' format specifications for MEAN (data precision + 1 decimal) and STDDEV
#' (data precision + 2 decimals). This is the R migration of the SAS macro
#' \code{\%util_value_format(ds, var, sym=, whr=)} from
#' \code{whitepapers/utilities/util_value_format.sas}, renamed to
#' \code{util_value_of_param} per AAP section 0.4.1.
#'
#' @param df A data frame containing the measurement values to summarize.
#'   Replaces SAS positional parameter \code{DS}. Must be a data frame
#'   object (not a string name).
#' @param var A character string specifying the name of the numeric variable
#'   on \code{df} to scan for precision. Replaces SAS positional parameter
#'   \code{VAR}.
#' @param whr An optional character string containing a filter expression to
#'   subset \code{df} before scanning. Replaces SAS keyword parameter
#'   \code{WHR=}. Example: \code{"STUDYID == 'STUDY01'"} or
#'   \code{"AVISITN == 99"}.
#'
#' @return A named list with format specifications:
#'   \describe{
#'     \item{\code{mean_digits}}{Integer — number of decimal places for MEAN
#'       (data precision + 1).}
#'     \item{\code{stddev_digits}}{Integer — number of decimal places for
#'       STDDEV (data precision + 2).}
#'     \item{\code{mean_width}}{Integer — total display width for MEAN in
#'       SAS format notation.}
#'     \item{\code{stddev_width}}{Integer — total display width for STDDEV
#'       in SAS format notation.}
#'     \item{\code{mean_fmt}}{Character — SAS-style format string for MEAN
#'       (e.g., \code{"5.1"}).}
#'     \item{\code{stddev_fmt}}{Character — SAS-style format string for
#'       STDDEV (e.g., \code{"6.2"}).}
#'   }
#'   Returns \code{NULL} invisibly if validation fails.
#'
#' @details
#' The function mirrors the SAS macro logic:
#' \enumerate{
#'   \item Validates the data frame via \code{\link{assert_dset_exist}}
#'     (SAS line 35).
#'   \item Validates that the variable exists via
#'     \code{\link{assert_var_exist}} (SAS line 36).
#'   \item Applies an optional filter expression (SAS line 42:
#'     \code{where \&whr}).
#'   \item Converts each non-missing numeric value to text using
#'     \code{format(x, scientific = FALSE, trim = TRUE)} — equivalent to
#'     SAS \code{put(\&var, best8.-L)} (SAS line 48).
#'   \item Splits each text value at the decimal point (SAS lines 49-50)
#'     and tracks the maximum integer digit count and maximum decimal digit
#'     count across all values (SAS lines 52-58).
#'   \item Computes MEAN format = \code{(max_int + max_dec + 2).(max_dec + 1)}
#'     and STDDEV format = \code{(max_int + max_dec + 3).(max_dec + 2)}
#'     (SAS lines 61-63).
#' }
#'
#' The SAS macro stored the result in a global macro symbol; this R function
#' returns a named list instead, following idiomatic R conventions.
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/util_value_format.sas (80 lines)}
#'   \item{Macro}{\%util_value_format(ds, var, sym=util_value_format, whr=)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{Output}{Space-delimited format pair: e.g., "5.1 6.2"}
#' }
#'
#' @examples
#' # Simple numeric variable with decimals
#' test_df <- data.frame(AVAL = c(12.3, 5.67, 100.1))
#' result <- util_value_of_param(test_df, "AVAL")
#' # result$mean_fmt   == "7.3"  (max_int=3, max_dec=2 -> width=3+2+2=7, dec=2+1=3)
#' # result$stddev_fmt == "8.4"  (width=3+2+3=8, dec=2+2=4)
#'
#' # Integer-only values (no decimals)
#' int_df <- data.frame(COUNT = c(5, 100, 42))
#' result2 <- util_value_of_param(int_df, "COUNT")
#' # result2$mean_fmt   == "5.1"  (max_int=3, max_dec=0 -> width=3+0+2=5, dec=0+1=1)
#' # result2$stddev_fmt == "6.2"  (width=3+0+3=6, dec=0+2=2)
#'
#' # With a filter expression
#' filt_df <- data.frame(STUDYID = c("A", "A", "B"), AVAL = c(1.23, 4.5, 99.999))
#' result3 <- util_value_of_param(filt_df, "AVAL", whr = "STUDYID == 'A'")
#'
#' @export
util_value_of_param <- function(df, var, whr = NULL) {

  # ==================================================================
  # STEP 1 — Validate dataset existence
  # Mirrors SAS line 35: %let OK = %assert_dset_exist(&ds)
  # ==================================================================
  dset_ok <- tryCatch(
    assert_dset_exist(df),
    error = function(e) FALSE
  )

  if (!isTRUE(dset_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_VALUE_OF_PARAM) Result is FAIL. ",
        "Unable to access the specified data frame."
      )
    )
  }

  # ==================================================================
  # STEP 2 — Validate variable existence on the data frame
  # Mirrors SAS line 36: %let OK = %assert_var_exist(&ds, &var)
  # ==================================================================
  var_ok <- tryCatch(
    assert_var_exist(df, var),
    error = function(e) FALSE
  )

  if (!isTRUE(var_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_VALUE_OF_PARAM) Result is FAIL. ",
        "Unable to read values from variable ",
        toupper(var), " on the specified data frame."
      )
    )
  }

  # ==================================================================
  # STEP 2b — Validate that the variable is numeric
  # SAS put(var, best8.) requires a numeric variable; enforce the same
  # constraint in R to prevent meaningless format detection on strings.
  # ==================================================================
  # Resolve actual column name (case-insensitive fallback per
  # assert_var_exist convention)
  col_names <- names(df)
  actual_var <- if (var %in% col_names) {
    var
  } else {
    col_names[toupper(col_names) == toupper(var)][1L]
  }

  if (!is.numeric(df[[actual_var]])) {
    cli::cli_abort(
      paste0(
        "(UTIL_VALUE_OF_PARAM) Result is FAIL. ",
        "Variable ", toupper(var), " is not numeric. ",
        "Format precision detection requires a numeric variable."
      )
    )
  }

  # ==================================================================
  # STEP 3 — Apply optional WHERE filter
  # Mirrors SAS line 42: %if %length(&whr) > 0 %then where &whr
  # Uses rlang::parse_expr() to convert the string filter expression
  # into an evaluable R expression for dplyr::filter().
  # ==================================================================
  working_df <- df
  if (!is.null(whr) && is.character(whr) && nzchar(stringr::str_trim(whr))) {
    filter_expr <- rlang::parse_expr(whr)
    working_df <- dplyr::filter(working_df, !!filter_expr)
  }

  # ==================================================================
  # STEP 3b — Extract numeric vector and remove missing values
  # SAS DATA step implicitly skips missing values in the ifn() logic
  # (SAS lines 52-58): ifn(not missing(int) ...) gates the update.
  # In R, we explicitly remove NAs before text conversion.
  # ==================================================================
  values <- working_df[[actual_var]]
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    cli::cli_abort(
      paste0(
        "(UTIL_VALUE_OF_PARAM) Result is FAIL. ",
        "No non-missing values found for variable ",
        toupper(var), " after applying filters."
      )
    )
  }

  # ==================================================================
  # STEP 4 — Convert values to text and determine precision
  # Mirrors SAS lines 47-58:
  #   valtxt = put(&var, best8.-L)     -> format(x, scientific=FALSE, trim=TRUE)
  #   int = scan(valtxt, 1, '.')       -> str_extract(... before decimal)
  #   dec = scan(valtxt, 2, '.')       -> str_extract(... after decimal)
  #   RETAIN max_int 0, max_dec 0      -> max() across all values
  # ==================================================================
  val_txt <- format(values, scientific = FALSE, trim = TRUE)
  val_txt <- stringr::str_trim(val_txt)

  # Extract integer part (digits before the decimal point, stripping sign)
  # SAS scan(valtxt, 1, '.') returns everything before the decimal.
  # We extract the raw integer portion including any minus sign, then strip
  # the sign for digit-count purposes.
  int_part_raw <- stringr::str_extract(val_txt, "^-?\\d+")
  # Strip the minus sign for digit counting (width calculation focuses on

  # magnitude digits; sign handling is separate in display formatting)
  int_part <- gsub("-", "", int_part_raw)

  # Extract decimal part (digits after the decimal point)
  # SAS scan(valtxt, 2, '.') returns everything after the decimal.
  # For integer values (no decimal point), this is NA.
  dec_part <- stringr::str_extract(val_txt, "(?<=\\.)\\d+")

  # Compute max integer digits across all values
  # Mirrors SAS lines 52-54: max_int = ifn(not missing(int) and
  # length(int) > max_int, length(int), max_int)
  int_lengths <- nchar(int_part)
  int_lengths <- int_lengths[!is.na(int_lengths)]
  max_int <- if (length(int_lengths) > 0L) max(int_lengths) else 1L

  # Compute max decimal digits across all values

  # Mirrors SAS lines 56-58: max_dec = ifn(not missing(dec) and
  # length(dec) > max_dec, length(dec), max_dec)
  # Values with no decimal point produce NA for dec_part; these are
  # excluded (equivalent to SAS missing(dec) check).
  dec_lengths <- nchar(dec_part)
  dec_lengths <- dec_lengths[!is.na(dec_lengths)]
  max_dec <- if (length(dec_lengths) > 0L) max(dec_lengths) else 0L

  # ==================================================================
  # STEP 5 — Compute format specifications
  # Mirrors SAS lines 60-63:
  #   meanfmt = strip(put(max_int+max_dec+2,8.-L)) // '.'
  #             // strip(put(max_dec+1,8.-L))
  #   stdvfmt = strip(put(max_int+max_dec+3,8.-L)) // '.'
  #             // strip(put(max_dec+2,8.-L))
  #
  # SAS format notation: w.d where w = total width, d = decimal places
  #   MEAN:   w = max_int + max_dec + 2,  d = max_dec + 1
  #   STDDEV: w = max_int + max_dec + 3,  d = max_dec + 2
  # ==================================================================
  mean_digits  <- as.integer(max_dec + 1L)
  stddev_digits <- as.integer(max_dec + 2L)
  mean_width   <- as.integer(max_int + max_dec + 2L)
  stddev_width <- as.integer(max_int + max_dec + 3L)

  mean_fmt   <- paste0(mean_width, ".", mean_digits)
  stddev_fmt <- paste0(stddev_width, ".", stddev_digits)

  # ==================================================================
  # STEP 6 — Log success and return
  # Mirrors SAS line 72:
  #   %put NOTE: (UTIL_VALUE_FORMAT) Successfully created symbol
  #              %upcase(&sym) = &&&sym
  # In R, we return a named list instead of setting a global symbol.
  # ==================================================================
  cli::cli_inform(
    paste0(
      "(UTIL_VALUE_OF_PARAM) Format specs: mean=",
      mean_fmt, ", stddev=", stddev_fmt,
      " (max_int=", max_int, ", max_dec=", max_dec, ")"
    )
  )

  result <- list(
    mean_digits   = mean_digits,
    stddev_digits = stddev_digits,
    mean_width    = mean_width,
    stddev_width  = stddev_width,
    mean_fmt      = mean_fmt,
    stddev_fmt    = stddev_fmt
  )

  return(result)
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Renamed from util_value_format to util_value_of_param per
#      AAP section 0.4.1 naming convention.
#    - SAS format notation (w.d) preserved in return list fields
#      mean_fmt and stddev_fmt for traceability back to SAS output.
#    - R format(x, scientific = FALSE, trim = TRUE) mimics SAS
#      put(x, best8.-L) for typical clinical measurement values.
#    - SAS global macro symbol (%global &sym / call symput) replaced
#      by R function return value (named list). The sym= parameter
#      is not needed in R since functions return values directly.
#    - The minus sign is stripped from integer part when counting
#      integer digits (max_int). This differs from SAS where
#      length(int) includes the sign character. The SAS width
#      formula (max_int + max_dec + 2) inherently includes headroom
#      for the sign via the +2 constant.
#    - Variable name resolution uses case-insensitive fallback
#      consistent with assert_var_exist.R behaviour, accommodating
#      datasets imported from SAS where column casing may differ.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS best8.-L may format differently than R format() for very
#      large numbers (> 8 significant digits). SAS truncates to 8
#      characters; R format() preserves full precision. This could
#      cause max_int or max_dec to differ for extreme values.
#    - R format() may produce trailing zeros differently than SAS
#      best8. depending on options(digits) settings; str_trim()
#      is applied but trailing zeros after the decimal are preserved
#      (matching SAS behaviour where precision is determined by the
#      stored value, not the display format).
#    - The stripping of the minus sign from max_int computation
#      means the R width may be 1 narrower than SAS for datasets
#      containing negative values. This is acceptable because R
#      format()/sprintf() handle sign placement independently of
#      width specification.
# NO DIRECT R EQUIVALENT:
#    - SAS format specification (w.d) is a display format concept;
#      R uses format()/sprintf()/formatC() with width and decimal
#      parameters. The returned mean_fmt/stddev_fmt strings preserve
#      SAS notation for traceability and can be used with:
#        sprintf(paste0("%", mean_fmt, "f"), value)
#    - SAS global symbol (%global / call symput) -> R function
#      return value. No global side-effect is produced.
#    - SAS PROC DATASETS cleanup (lines 67-69) is not needed in R;
#      the temporary working data exists only within function scope.
# PACKAGE SELECTION RATIONALE:
#    - stringr (>= 1.5.0): String parsing for decimal point splitting
#      via str_extract() and whitespace trimming via str_trim().
#      Chosen over base R strsplit/regmatches per AAP section 0.8.1
#      tidyverse-over-base-R mandate.
#    - dplyr (>= 1.1.0): For filter() to apply the optional WHERE
#      clause, consistent with tidyverse pipeline patterns.
#    - rlang (>= 1.1.0): For parse_expr() to convert the string
#      filter expression to an evaluable R expression for
#      dplyr::filter(). Required for safe tidy evaluation.
#    - cli (>= 3.6.0): For cli_inform() and cli_abort() providing
#      user-facing messages matching SAS %PUT NOTE/ERROR format.
# OPEN QUESTIONS:
#    - Should the returned digits be used directly with sprintf()
#      or format()? Current return provides both numeric components
#      and SAS-notation strings for flexibility.
#    - Should this integrate with Tplyr f_str() for formatted table
#      output? The mean_digits/stddev_digits values can be fed
#      directly into f_str() format specifications.
#    - For datasets with only negative values, should the width
#      include an explicit +1 for the sign character? The current
#      +2 constant provides sufficient headroom in most cases.
# ============================================================
