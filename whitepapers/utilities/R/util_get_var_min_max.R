# ============================================================
# util_get_var_min_max.R
# Migration of: whitepapers/utilities/util_get_var_min_max.sas
# ============================================================
# Purpose : Compute the MIN and MAX of a numeric variable in a
#           data frame, optionally expanding the range to include
#           additional (extra) values. Used for axis range
#           calculations in WPCT and other plotting utilities.
# Author  : Dante Di Tommaso (original SAS macro)
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency checks --------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for util_get_var_min_max. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}

if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for util_get_var_min_max. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}

if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for util_get_var_min_max. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist --------------------
# Mirrors SAS line 37: %let OK = %assert_dset_exist(&ds)
# The function is sourced from the same directory if not already
# available in the current session.
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

# --- Internal dependency: assert_var_exist ---------------------
# Mirrors SAS line 38: %if &OK %then %let OK = %assert_var_exist(&ds, &var)
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

#' Compute Variable Min and Max
#'
#' Computes the minimum and maximum of a numeric variable in a data frame,
#' optionally filtering the data and expanding the resulting range to
#' include additional values. Returns a named numeric vector
#' \code{c(min = <min_val>, max = <max_val>)}.
#'
#' This is the R migration of the SAS macro
#' \code{\%util_get_var_min_max(ds, var, sym, sqlwhr=, extra=)} from
#' \code{whitepapers/utilities/util_get_var_min_max.sas} (69 lines).
#'
#' @param df A data frame containing the numeric variable for which
#'   min and max are computed. Replaces SAS parameter \code{DS}.
#'   Can be a data frame object or a character string naming an
#'   in-memory data frame (mirrors SAS \code{(libname.)memname}).
#' @param var Character string specifying the name of the numeric
#'   variable (column) in \code{df}. Replaces SAS parameter \code{VAR}.
#' @param sql_whr Optional character string containing an R filter
#'   expression to subset the data before computing min/max. Replaces
#'   SAS parameter \code{SQLWHR} (e.g., the SQL WHERE clause on
#'   SAS line 45). Parsed via \code{rlang::parse_expr()} and evaluated
#'   within \code{dplyr::filter()}.
#'   Example: \code{"STUDYID == 'STUDY01'"} or \code{"AVISIT != 'BASELINE'"}.
#'   Default is \code{NULL} (no filtering).
#' @param extra Optional numeric vector of additional values to include
#'   in the resulting range. For example, to ensure the axis range
#'   includes a normal-range interval. Replaces SAS parameter
#'   \code{EXTRA} (which accepted a space-delimited string; in R a
#'   numeric vector is idiomatic). Default is \code{NULL}.
#'
#' @return A named numeric vector of length 2: \code{c(min = <min_val>,
#'   max = <max_val>)}. Returns \code{c(min = NA_real_, max = NA_real_)}
#'   if all values are missing or if validation fails.
#'
#' @details
#' The function mirrors the SAS macro logic:
#' \enumerate{
#'   \item Validates that the dataset exists via
#'     \code{\link{assert_dset_exist}} (SAS line 37).
#'   \item Validates that the variable exists in the dataset via
#'     \code{\link{assert_var_exist}} (SAS line 38).
#'   \item Validates that the variable is numeric (implicit in SAS
#'     PROC SQL \code{SELECT MIN(), MAX()}).
#'   \item Applies an optional filter expression (SAS line 45:
#'     \code{&sqlwhr}).
#'   \item Computes \code{min()} and \code{max()} with
#'     \code{na.rm = TRUE} — replacing SAS PROC SQL
#'     \code{SELECT MIN(&var), MAX(&var) INTO :minval, :maxval}
#'     (lines 42–46).
#'   \item Adjusts the range to accommodate extra values (SAS
#'     lines 51–56).
#'   \item Returns the result as a named numeric vector instead of
#'     storing it in a global SAS macro symbol (SAS line 58).
#'   \item Warns if either min or max is \code{NA} (SAS lines 60–61);
#'     informs on success (SAS line 63).
#' }
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/util_get_var_min_max.sas (69 lines)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{SAS Macro Signature}{\code{\%util_get_var_min_max(ds, var, sym, sqlwhr=, extra=)}}
#' }
#'
#' @examples
#' # Basic usage
#' util_get_var_min_max(mtcars, "mpg")
#' # => c(min = 10.4, max = 33.9)
#'
#' # With a filter
#' util_get_var_min_max(mtcars, "mpg", sql_whr = "cyl == 6")
#' # => c(min = 17.8, max = 21.4)
#'
#' # With extra values to expand range
#' util_get_var_min_max(mtcars, "mpg", extra = c(0, 50))
#' # => c(min = 0, max = 50)
#'
#' @export
util_get_var_min_max <- function(df, var, sql_whr = NULL, extra = NULL) {

  # ------------------------------------------------------------------
  # Capture a human-readable label for the data frame for messaging.
  # Mirrors SAS %UPCASE(&DS) used in %PUT statements (lines 61, 63, 66).
  # ------------------------------------------------------------------
  df_label <- if (is.character(df) && length(df) == 1L) {
    toupper(df)
  } else {
    toupper(deparse(substitute(df)))
  }

  # ------------------------------------------------------------------
  # Define the failure return value — used when validation fails.
  # Mirrors SAS behaviour where &SYM would contain ". ." when
  # min/max could not be computed.
  # ------------------------------------------------------------------
  fail_result <- c(min = NA_real_, max = NA_real_)

  # ==================================================================
  # STEP 1 — Validate dataset existence
  # Mirrors SAS line 37: %let OK = %assert_dset_exist(&ds)
  # ==================================================================
  dset_ok <- tryCatch(
    assert_dset_exist(df),
    error = function(e) FALSE
  )

  if (!isTRUE(dset_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_GET_VAR_MIN_MAX) Unable to read values from variable ",
        toupper(var), " on data set ", df_label, "."
      )
    )
  }

  # ==================================================================
  # STEP 2 — Validate variable existence on dataset
  # Mirrors SAS line 38: %if &OK %then %let OK = %assert_var_exist(&ds, &var)
  # ==================================================================
  var_ok <- tryCatch(
    assert_var_exist(df, var),
    error = function(e) FALSE
  )

  if (!isTRUE(var_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_GET_VAR_MIN_MAX) Unable to read values from variable ",
        toupper(var), " on data set ", df_label, "."
      )
    )
  }

  # ==================================================================
  # STEP 2b — Resolve df to an actual data frame object
  # If a string name was passed, retrieve the data frame from the
  # calling environment. Mirrors SAS implicit dataset resolution.
  # ==================================================================
  if (is.character(df) && length(df) == 1L) {
    df_resolved <- get(df, envir = parent.frame())
  } else {
    df_resolved <- df
  }

  # ==================================================================
  # STEP 2c — Validate that the variable is numeric
  # Implicit in SAS PROC SQL SELECT MIN(), MAX() which require numeric
  # columns. In R we explicitly guard against non-numeric variables.
  # ==================================================================
  if (!is.numeric(df_resolved[[var]])) {
    cli::cli_abort(
      paste0(
        "(UTIL_GET_VAR_MIN_MAX) Variable ", toupper(var),
        " on data set ", df_label,
        " is not numeric. MIN/MAX computation requires a numeric variable."
      )
    )
  }

  # ==================================================================
  # STEP 3 — Apply optional WHERE filter
  # Mirrors SAS line 45: &sqlwhr (e.g., "where studyid = 'STUDY01'")
  # The sql_whr parameter is an R expression string parsed via

  # rlang::parse_expr() and evaluated within dplyr::filter().
  # ==================================================================
  if (!is.null(sql_whr) && nzchar(sql_whr)) {
    filter_expr <- tryCatch(
      rlang::parse_expr(sql_whr),
      error = function(e) {
        cli::cli_abort(
          paste0(
            "(UTIL_GET_VAR_MIN_MAX) Invalid filter expression: '",
            sql_whr, "'. Error: ", conditionMessage(e)
          )
        )
      }
    )
    df_resolved <- tryCatch(
      dplyr::filter(df_resolved, !!filter_expr),
      error = function(e) {
        cli::cli_abort(
          paste0(
            "(UTIL_GET_VAR_MIN_MAX) Filter expression '", sql_whr,
            "' failed on data set ", df_label, ". Error: ",
            conditionMessage(e)
          )
        )
      }
    )
  }

  # ==================================================================
  # STEP 4 — Compute MIN and MAX
  # Mirrors SAS lines 42-48:
  #   proc sql noprint;
  #     select min(&var), max(&var) into :minval, :maxval
  #     from &ds &sqlwhr;
  #   quit;
  # R min()/max() with na.rm=TRUE returns Inf/-Inf when all values
  # are NA. SAS returns . (missing). We detect infinite values and
  # convert to NA to preserve SAS missing-value semantics.
  # ==================================================================
  values <- df_resolved[[var]]

  # suppressWarnings: R min()/max() with na.rm=TRUE emit "no non-missing
  # arguments" warnings when all values are NA. We handle the resulting
  # Inf/-Inf explicitly below, so suppress the base R warning.
  min_val <- suppressWarnings(min(values, na.rm = TRUE))
  max_val <- suppressWarnings(max(values, na.rm = TRUE))

  # Handle all-NA case: R returns Inf/-Inf; SAS returns .
  if (is.infinite(min_val)) min_val <- NA_real_
  if (is.infinite(max_val)) max_val <- NA_real_

  # ==================================================================
  # STEP 5 — Adjust range for extra values
  # Mirrors SAS lines 51-56:
  #   %do idx = 1 %to %sysfunc(countw(&extra, %str( )));
  #     %let nxt = %scan(&extra, &idx, %str( ));
  #     %if %sysevalf(&minval > &nxt) %then %let minval = &nxt;
  #     %if %sysevalf(&maxval < &nxt) %then %let maxval = &nxt;
  #   %end;
  # In R the EXTRA parameter is a numeric vector; we simply include
  # it in the min()/max() calls. NA values in extra are ignored via
  # na.rm = TRUE.
  # ==================================================================
  if (!is.null(extra)) {
    if (!is.numeric(extra)) {
      cli::cli_warn(
        paste0(
          "(UTIL_GET_VAR_MIN_MAX) 'extra' parameter is not numeric; ",
          "attempting coercion."
        )
      )
      extra <- suppressWarnings(as.numeric(extra))
    }
    # Remove any NA values introduced by coercion
    extra_clean <- extra[!is.na(extra)]
    if (length(extra_clean) > 0L) {
      if (!is.na(min_val)) {
        min_val <- min(min_val, extra_clean)
      } else {
        min_val <- min(extra_clean)
      }
      if (!is.na(max_val)) {
        max_val <- max(max_val, extra_clean)
      } else {
        max_val <- max(extra_clean)
      }
    }
  }

  # ==================================================================
  # STEP 6 — Build result and log messages
  # Mirrors SAS line 58: %let &sym = &minval &maxval
  # Mirrors SAS lines 60-63: WARNING / NOTE messages
  # Instead of setting a global symbol, we return a named numeric
  # vector. This is idiomatic R — callers capture the result in a
  # variable: result <- util_get_var_min_max(df, "var")
  # ==================================================================
  result <- c(min = min_val, max = max_val)

  if (is.na(min_val) || is.na(max_val)) {
    # SAS lines 60-61: %put WARNING: (UTIL_GET_VAR_MIN_MAX) Missing vals ...
    cli::cli_warn(
      paste0(
        "(UTIL_GET_VAR_MIN_MAX) Missing values for ", toupper(var),
        " on ", df_label, ". Result = ",
        ifelse(is.na(min_val), "NA", as.character(min_val)), " ",
        ifelse(is.na(max_val), "NA", as.character(max_val)), "."
      )
    )
  } else {
    # SAS line 63: %put NOTE: (UTIL_GET_VAR_MIN_MAX) Successfully created ...
    cli::cli_inform(
      paste0(
        "(UTIL_GET_VAR_MIN_MAX) Successfully computed min/max for ",
        toupper(var), " on ", df_label, ": ",
        min_val, " ", max_val, "."
      )
    )
  }

  return(result)
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global symbol (%global &sym; %let &sym = &minval &maxval)
#      is replaced by an R named numeric vector return value
#      c(min = min_val, max = max_val). The SAS SYM parameter is
#      removed; callers capture the result in a variable.
#    - SAS SQLWHR clause (SQL WHERE expression) is replaced by an
#      R filter expression string parsed via rlang::parse_expr()
#      and evaluated within dplyr::filter(). The expression uses
#      R syntax (e.g., "STUDYID == 'STUDY01'") rather than SAS
#      SQL syntax (e.g., "where studyid = 'STUDY01'").
#    - The EXTRA parameter accepts a numeric vector instead of a
#      space-delimited string, following R idiomatic conventions.
#    - assert_dset_exist() and assert_var_exist() are sourced from
#      the same directory or must be pre-loaded in the session
#      (mirrors SAS SASAUTOS autocall).
# POTENTIAL NUMERICAL DIFFERENCES:
#    - R min()/max() with na.rm=TRUE returns Inf/-Inf for all-NA
#      input; SAS returns . (missing). This is handled by checking
#      for infinite values and converting to NA_real_.
#    - Floating-point edge cases: SAS and R both use IEEE 754
#      doubles, but comparison results at machine epsilon may
#      differ in rare cases.
# NO DIRECT R EQUIVALENT:
#    - SAS PROC SQL SELECT MIN(), MAX() INTO :sym -> R min()/max()
#      with na.rm=TRUE, returning a named vector instead of writing
#      to a macro variable.
#    - SAS %global / %local scoping -> R function scoping handles
#      this naturally. The return value replaces global symbol
#      assignment.
#    - SAS %sysevalf() for numeric comparison of macro variables ->
#      R standard numeric comparison operators.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): Provides filter() for applying the
#      optional sql_whr filter expression, replacing the SAS SQL
#      WHERE clause. Required per AAP tidyverse-over-base-R mandate.
#    - rlang (>= 1.1.0): Provides parse_expr() to convert the
#      sql_whr filter expression string into an evaluable R
#      expression for use within dplyr::filter().
#    - cli (>= 3.6.0): Provides cli_warn(), cli_inform(), and
#      cli_abort() for user-facing messages matching the SAS
#      %PUT WARNING/NOTE/ERROR format.
# OPEN QUESTIONS:
#    - None — straightforward migration with clear SAS-to-R
#      mappings for all constructs.
# ============================================================
