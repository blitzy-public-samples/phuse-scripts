# ============================================================
# assert_var_nonmissing.R
# Migration of: whitepapers/utilities/assert_var_nonmissing.sas
# ============================================================
# Purpose : Check whether a variable contains ONLY non-missing
#           values, optionally with a WHERE-clause filter.
#           Returns TRUE (PASS) or FALSE (FAIL).
# Author  : Dante Di Tommaso (original SAS macro)
# Ack.    : Inspired by FUTS system from Thotwave
#           http://thotwave.com/resources/futs-framework-unit-testing-sas/
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency checks --------------------------------
# Mirrors SAS runtime expectations: these packages must be
# available for the function to operate correctly.
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for assert_var_nonmissing. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for assert_var_nonmissing. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for assert_var_nonmissing. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist --------------------
# Mirrors SAS line 34: %let OK = %assert_dset_exist(&ds)
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
# Mirrors SAS line 40: %assert_var_exist(&ds, &var)
# The function is sourced from the same directory if not already
# available in the current session.
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

#' Assert Variable Non-Missing
#'
#' Checks whether a variable (column) contains ONLY non-missing values
#' in a dataset (data frame), optionally restricted by a WHERE-clause
#' filter. Returns \code{TRUE} (PASS, all non-missing) or \code{FALSE}
#' (FAIL, has missing values). This is the R migration of the SAS
#' in-line macro \code{\%assert_var_nonmissing(ds, var, whr=)} from
#' \code{whitepapers/utilities/assert_var_nonmissing.sas}.
#'
#' @param df A data frame object OR a character string naming an
#'   in-memory data frame (analogous to a SAS dataset name).
#' @param var A character string specifying the variable (column)
#'   name to check for missing values.
#' @param whr An optional character string containing an R filter
#'   expression (analogous to a SAS WHERE clause). When supplied the
#'   missing-value check is restricted to rows satisfying the filter.
#'   Example: \code{"SITEID == '701'"} or
#'   \code{"toupper(PARAMCD) \%in\% c('DIABP','SYSBP')"}.
#' @param envir Environment in which to look up string names when
#'   \code{df} is a character string. Defaults to the calling
#'   environment (\code{parent.frame()}).
#'
#' @return Logical \code{TRUE} if the variable contains zero missing
#'   values (PASS), \code{FALSE} otherwise (FAIL). The result is
#'   returned invisibly.
#'
#' @details
#' The function mirrors the SAS macro behaviour:
#' \enumerate{
#'   \item Validates that the dataset exists via
#'     \code{\link{assert_dset_exist}} (SAS line 34).
#'   \item Validates that the \code{var} argument is non-empty
#'     (SAS lines 36-39).
#'   \item Validates that the variable exists on the dataset via
#'     \code{\link{assert_var_exist}} (SAS line 40).
#'   \item Builds a filter: if \code{whr} is provided, applies both
#'     the missing-value check and the user filter; otherwise checks
#'     all rows (SAS lines 43-44).
#'   \item Counts the number of missing observations via
#'     \code{sum(is.na())} — replacing SAS
#'     \code{\%sysfunc(attrn(\&dsid, NLOBSF))} (SAS lines 46-50).
#'   \item Logs PASS or FAIL with the count of missing values,
#'     matching SAS \code{\%PUT NOTE/WARNING} format
#'     (SAS lines 52-58).
#' }
#'
#' \strong{Missing value semantics} (per AAP §0.7.3):
#' \itemize{
#'   \item SAS numeric missing (\code{.}) → R \code{NA}
#'   \item SAS character missing (\code{' '}) → R \code{NA_character_}
#'   \item SAS special missing (\code{.A}–\code{.Z}, \code{._}) →
#'     \code{haven::tagged_na()}
#'   \item All of the above are detected by \code{is.na()} in R.
#' }
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/assert_var_nonmissing.sas
#'     (73 lines)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{Acknowledgement}{Inspired by FUTS system from Thotwave}
#' }
#'
#' @examples
#' # All non-missing — PASS
#' assert_var_nonmissing(mtcars, "mpg")
#'
#' # Contains missing — FAIL
#' df_miss <- data.frame(x = c(1, NA, 3))
#' assert_var_nonmissing(df_miss, "x")
#'
#' # With WHERE clause — check only filtered rows
#' assert_var_nonmissing(mtcars, "mpg", whr = "cyl == 4")
#'
#' @export
assert_var_nonmissing <- function(df, var, whr = NULL, envir = parent.frame()) {

  # ------------------------------------------------------------------
  # Capture a human-readable label for the dataset before evaluation.
  # If df is a character string, use it directly; otherwise deparse
  # the expression the caller passed (e.g. "mtcars").
  # ------------------------------------------------------------------
  df_label <- if (is.character(df) && length(df) == 1L) {
    df
  } else {
    deparse(substitute(df))
  }

  # ==================================================================
  # STEP 1 — Validate dataset existence via assert_dset_exist
  # Mirrors SAS line 34: %let OK = %assert_dset_exist(&ds)
  # ==================================================================
  ok <- tryCatch(
    assert_dset_exist(df, envir = envir),
    error = function(e) FALSE
  )

  if (!isTRUE(ok)) {
    return(invisible(FALSE))
  }

  # ==================================================================
  # STEP 2 — Validate that var argument is non-empty
  # Mirrors SAS lines 36-39:
  #   %if 0 = %length(&var) %then %do;
  #     %put ERROR: (ASSERT_VAR_NONMISSING) Please specify a variable ...
  #     %let OK = 0;
  #   %end;
  # ==================================================================
  if (is.null(var) || !is.character(var) || length(var) != 1L || !nzchar(var)) {
    cli::cli_abort(
      paste0(
        "(ASSERT_VAR_NONMISSING) Please specify a variable on data set ",
        toupper(df_label), "."
      )
    )
  }

  # ==================================================================
  # STEP 3 — Validate variable existence via assert_var_exist
  # Mirrors SAS line 40: %else %if &OK %then
  #   %let OK = %assert_var_exist(&ds, &var);
  # ==================================================================
  ok <- tryCatch(
    assert_var_exist(df, var, envir = envir),
    error = function(e) FALSE
  )

  if (!isTRUE(ok)) {
    return(invisible(FALSE))
  }

  # ==================================================================
  # STEP 4 — Resolve df to an actual data frame for value inspection
  # Mirrors SAS line 46: %let dsid = %sysfunc(open( &ds (&where_full) ))
  # In R we need the data frame in memory to check values.
  # ==================================================================
  if (is.data.frame(df)) {
    df_resolved <- df
  } else if (is.character(df) && length(df) == 1L && nzchar(df)) {
    df_resolved <- tryCatch(
      {
        obj <- get(df, envir = envir)
        if (!is.data.frame(obj)) {
          cli::cli_abort(
            paste0(
              "(ASSERT_VAR_NONMISSING) Data set ", toupper(df_label),
              " is not accessible. Abort check for variable ",
              toupper(var), "."
            )
          )
        }
        obj
      },
      error = function(e) {
        # Mirrors SAS lines 64-68: dataset not accessible error path
        cli::cli_abort(
          paste0(
            "(ASSERT_VAR_NONMISSING) Data set ", toupper(df_label),
            " is not accessible. Abort check for variable ",
            toupper(var), "."
          )
        )
      }
    )
  } else {
    cli::cli_abort(
      paste0(
        "(ASSERT_VAR_NONMISSING) Data set ", toupper(df_label),
        " is not accessible. Abort check for variable ",
        toupper(var), "."
      )
    )
  }

  # ==================================================================
  # STEP 5 — Resolve the actual column name (handle case-insensitive)
  # SAS varnum uses %upcase; R names are case-sensitive. Resolve the
  # canonical column name so that df_resolved[[var_resolved]] works
  # even when the user passes a differently-cased name.
  # ==================================================================
  column_names <- names(df_resolved)
  var_resolved <- var
  if (!(var %in% column_names)) {
    # Case-insensitive fallback — mirrors SAS %upcase behaviour
    match_idx <- match(toupper(var), toupper(column_names))
    if (!is.na(match_idx)) {
      var_resolved <- column_names[match_idx]
    }
    # If still no match, assert_var_exist should have caught this,
    # but guard defensively.
  }

  # ==================================================================
  # STEP 6 — Apply optional WHERE-clause filter
  # Mirrors SAS lines 43-44:
  #   %if 0 = %length(&whr) %then
  #     %let where_full = where=(missing(&var));
  #   %else
  #     %let where_full = where=(missing(&var) and (&whr));
  #
  # In R, we first filter by the user WHERE clause (if any), then
  # count missing values on the result. The SAS approach builds a
  # compound WHERE that selects only missing records (optionally
  # restricted by the user clause) and then counts them. Our approach
  # is equivalent: filter to user-subset, then sum(is.na()).
  # ==================================================================
  if (!is.null(whr) && nzchar(whr)) {
    df_filtered <- tryCatch(
      {
        filter_expr <- rlang::parse_expr(whr)
        dplyr::filter(df_resolved, !!filter_expr)
      },
      error = function(e) {
        # Mirrors SAS lines 65-68: bad WHERE clause error path
        cli::cli_abort(
          paste0(
            "(ASSERT_VAR_NONMISSING) Data set ", toupper(df_label),
            " is not accessible. Abort check for variable ",
            toupper(var), ". ",
            "Review your where clause carefully. ",
            "Test the compound clause: is.na(",
            var_resolved, ") & (", whr, ")"
          )
        )
      }
    )
  } else {
    df_filtered <- df_resolved
  }

  # ==================================================================
  # STEP 7 — Count missing values
  # Mirrors SAS line 50: %let rc = %sysfunc(attrn(&dsid, NLOBSF))
  # is.na() detects: NA, NA_character_, NA_real_, NA_integer_,
  # NA_complex_, NaN, and haven::tagged_na() — all SAS missing types.
  # ==================================================================
  n_missing <- sum(is.na(df_filtered[[var_resolved]]))

  # Construct the WHERE display string for log messages
  # Mirrors SAS output format: (where=&whr)

  whr_display <- if (!is.null(whr) && nzchar(whr)) {
    paste0(" (where=", whr, ")")
  } else {
    ""
  }

  # ==================================================================
  # STEP 8 — Log PASS or FAIL and return result
  # Mirrors SAS lines 52-58:
  #   FAIL: %put WARNING: (ASSERT_VAR_NONMISSING) Result is FAIL. ...
  #   PASS: %put NOTE:    (ASSERT_VAR_NONMISSING) Result is PASS. ...
  # ==================================================================
  if (n_missing > 0L) {
    # FAIL — variable contains missing values
    # SAS line 54: %put WARNING
    cli::cli_warn(
      paste0(
        "(ASSERT_VAR_NONMISSING) Result is FAIL. ",
        n_missing, " Missing values for variable \"",
        toupper(var), "\" on data set \"",
        toupper(df_label), "\"",
        whr_display, "."
      )
    )
    return(invisible(FALSE))
  } else {
    # PASS — variable contains only non-missing values
    # SAS line 58: %put NOTE
    cli::cli_inform(
      paste0(
        "(ASSERT_VAR_NONMISSING) Result is PASS. ",
        n_missing, " Missing values for variable \"",
        toupper(var), "\" on data set \"",
        toupper(df_label), "\"",
        whr_display, "."
      )
    )
    return(invisible(TRUE))
  }
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS missing() function -> R is.na() which catches NA, NaN,
#      and haven::tagged_na() — all SAS missing types.
#    - SAS character missing (' ') -> R NA_character_; is.na()
#      detects this correctly.
#    - SAS special missing (._ .m .z) -> haven::tagged_na();
#      is.na() detects these correctly.
#    - SAS WHERE clause -> R dplyr::filter() with
#      rlang::parse_expr(). Users must write R syntax in the whr
#      argument (e.g., "SITEID == '701'") rather than SAS syntax
#      (e.g., "siteid = '701'").
#    - Function returns logical TRUE/FALSE (not in-line 0/1 as
#      in SAS). TRUE maps to SAS 1 (PASS), FALSE maps to SAS 0
#      (FAIL).
#    - Variable name matching uses exact case first, then falls
#      back to case-insensitive matching (SAS convention).
#    - Missing values produce a WARNING (matching SAS behaviour),
#      while structural problems (bad dataset, empty var name,
#      bad WHERE clause) produce an ERROR (matching SAS behaviour).
#    - envir parameter defaults to parent.frame() to check the
#      calling environment, analogous to SAS checking WORK library.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a missing value detection utility with no
#      numerical computation.
# NO DIRECT R EQUIVALENT:
#    - SAS %sysfunc(open/attrn/close) for counting with WHERE ->
#      R dplyr::filter() + sum(is.na()) — much simpler in R.
#    - SAS NLOBSF attribute (filtered observation count) ->
#      R sum(is.na()) provides equivalent count of missing records.
#    - SAS dataset locking (DSID notes on line 49) -> not
#      applicable in R; data frames are not file-locked.
#    - SAS %sysfunc(sysmsg()) -> R tryCatch condition messages.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): For filter() with optional WHERE clause,
#      providing tidy evaluation of filter expressions.
#    - cli (>= 3.6.0): Informative messages matching SAS %PUT
#      NOTE/WARNING/ERROR format for PASS/FAIL assertion logging
#      via cli_inform(), cli_warn(), and cli_abort().
#    - rlang (>= 1.1.0): For parse_expr() to convert user-supplied
#      filter expression strings to evaluable R expressions.
# OPEN QUESTIONS:
#    - Should this distinguish between different tagged_na types
#      (e.g., .A vs .Z) in the log message? Current implementation
#      treats all NA variants uniformly.
#    - Should NaN be counted separately from NA? Currently NaN is
#      included in is.na() count (matches SAS behaviour where NaN
#      is treated as missing).
# ============================================================
