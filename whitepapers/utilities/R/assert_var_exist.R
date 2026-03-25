# ============================================================
# assert_var_exist.R
# Migration of: whitepapers/utilities/assert_var_exist.sas
# ============================================================
# Purpose : Check whether a variable (column) exists in a dataset
#           (data frame). Returns TRUE (PASS) or FALSE (FAIL).
# Author  : Dante Di Tommaso (original SAS macro)
# Ack.    : Inspired by FUTS system from Thotwave
#           http://thotwave.com/resources/futs-framework-unit-testing-sas/
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency check --------------------------------
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for assert_var_exist. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist -------------------
# Mirrors SAS %include for %assert_dset_exist (used on SAS line 14).
# The function is sourced from the same directory if not already
# available in the current session.
if (!exists("assert_dset_exist", mode = "function", inherits = TRUE)) {
  local({
    # Attempt 1: relative to the sourced script location
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
    # If none found, the function will fail later with a clear error
    # when assert_dset_exist() is actually called.
  })
}

#' Assert Variable Existence on a Dataset
#'
#' Checks whether a variable (column) exists in a dataset (data frame).
#' Returns \code{TRUE} (PASS) or \code{FALSE} (FAIL). This is the R
#' migration of the SAS macro \code{\%assert_var_exist(ds, var)} from
#' \code{whitepapers/utilities/assert_var_exist.sas}.
#'
#' @param df A data frame object OR a character string naming an in-memory
#'   data frame (analogous to a SAS dataset name).
#' @param var A character string specifying the variable (column) name to
#'   check for existence.
#' @param envir Environment in which to look up string names when \code{df}
#'   is a character string. Defaults to the calling environment
#'   (\code{parent.frame()}).
#'
#' @return Logical \code{TRUE} if the variable exists on the dataset (PASS),
#'   \code{FALSE} otherwise (FAIL). The result is returned invisibly.
#'
#' @details
#' The function mirrors the SAS macro behaviour:
#' \enumerate{
#'   \item First validates that the dataset exists via
#'     \code{\link{assert_dset_exist}} (SAS line 14).
#'   \item Checks that the \code{var} argument is non-empty (SAS line 16).
#'   \item Resolves the dataset to an in-memory data frame for column
#'     inspection (replaces SAS \code{\%sysfunc(open)}).
#'   \item Checks whether the variable name exists among the dataset's column
#'     names using \code{names()} — replacing SAS
#'     \code{\%sysfunc(varnum(\&dsid, \%upcase(\&var)))}.
#'   \item Logs PASS or FAIL messages matching SAS \code{\%PUT NOTE/ERROR}
#'     format.
#' }
#'
#' Variable name matching uses exact case first (R convention), then falls
#' back to case-insensitive matching (SAS convention, where \code{varnum}
#' uses \code{\%upcase}) to accommodate datasets imported from SAS where
#' column names may have different casing.
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/assert_var_exist.sas (39 lines)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{Acknowledgement}{Inspired by FUTS system from Thotwave}
#' }
#'
#' @examples
#' assert_var_exist(mtcars, "mpg")    # TRUE  -- exact match
#' assert_var_exist(mtcars, "xyz")    # FALSE -- variable not found
#' assert_var_exist("mtcars", "cyl")  # TRUE  -- resolves string name
#'
#' @export
assert_var_exist <- function(df, var, envir = parent.frame()) {

  # ------------------------------------------------------------------
  # Capture a human-readable label for the dataset before any evaluation.
  # If df is a character string, use it directly; otherwise deparse the

  # expression the caller passed (e.g. "mtcars").
  # ------------------------------------------------------------------
  df_label <- if (is.character(df) && length(df) == 1L) {
    df
  } else {
    deparse(substitute(df))
  }

  # ==================================================================
  # STEP 1 — Validate dataset existence via assert_dset_exist

  # Mirrors SAS line 14: %let OK = %assert_dset_exist(&ds)
  # ==================================================================
  # assert_dset_exist may throw (cli_abort) for NULL / invalid input,
  # so wrap in tryCatch to convert errors into a clean FALSE return.
  dset_ok <- tryCatch(
    assert_dset_exist(df, envir = envir),
    error = function(e) FALSE
  )

  if (!isTRUE(dset_ok)) {
    # Dataset validation failed — already logged by assert_dset_exist.
    return(invisible(FALSE))
  }

  # ==================================================================
  # STEP 2 — Validate that var argument is non-empty
  # Mirrors SAS line 16: %if 0 = %length(&var) %then %let OK = 0
  # ==================================================================
  if (is.null(var) || !is.character(var) || length(var) != 1L || !nzchar(var)) {
    cli::cli_warn(
      paste0(
        "(ASSERT_VAR_EXIST) Result is FAIL. \"\" is NOT a variable ",
        "on data set ", toupper(df_label), "."
      )
    )
    return(invisible(FALSE))
  }

  # ==================================================================
  # STEP 3 — Resolve df to an actual data frame for column inspection
  # Mirrors SAS lines 19-32: %sysfunc(open/varnum/close) sequence.
  # In R, column names are available directly via names(); the SAS
  # open/close ceremony is replaced by resolving the data frame object.
  # ==================================================================
  if (is.data.frame(df)) {
    # Data frame passed directly — use as-is
    df_resolved <- df
  } else if (is.character(df) && length(df) == 1L && nzchar(df)) {
    # String name — resolve to in-memory object in the specified envir
    df_resolved <- tryCatch(
      {
        obj <- get(df, envir = envir)
        if (!is.data.frame(obj)) {
          cli::cli_warn(
            paste0(
              "(ASSERT_VAR_EXIST) Result is FAIL. Data set ",
              toupper(df_label),
              " is not accessible as a data frame. ",
              "Abort check for variable ", toupper(var), "."
            )
          )
          NULL
        } else {
          obj
        }
      },
      error = function(e) {
        # Mirrors SAS lines 28-32: dataset not accessible error path
        cli::cli_warn(
          paste0(
            "(ASSERT_VAR_EXIST) Result is FAIL. Data set ",
            toupper(df_label),
            " is not accessible. Abort check for variable ",
            toupper(var), "."
          )
        )
        NULL
      }
    )
    if (is.null(df_resolved)) {
      return(invisible(FALSE))
    }
  } else {
    # Invalid df specification — should not normally reach here after
    # Step 1, but guard defensively.
    cli::cli_warn(
      "(ASSERT_VAR_EXIST) Result is FAIL. Invalid dataset specification."
    )
    return(invisible(FALSE))
  }

  # ==================================================================
  # STEP 4 — Check variable existence in dataset column names
  # Mirrors SAS line 23:
  #   %if %sysfunc(varnum(&dsid, %upcase(&var))) LT 1 %then %let OK = 0
  # SAS varnum is case-insensitive (uses %upcase). R names() is case-
  # sensitive. We first try an exact match (R convention), then fall
  # back to a case-insensitive match (SAS convention).
  # ==================================================================
  column_names <- names(df_resolved)
  var_exists <- var %in% column_names
  if (!var_exists) {
    # Case-insensitive fallback — mirrors SAS %upcase(&var) behaviour
    var_exists <- toupper(var) %in% toupper(column_names)
  }

  # ==================================================================
  # STEP 5 — Log result and return
  # Mirrors SAS lines 35-36:
  #   PASS: %put NOTE: (ASSERT_VAR_EXIST) Result is PASS. ...
  #   FAIL: %put ERROR: (ASSERT_VAR_EXIST) Result is FAIL. ...
  # ==================================================================
  if (var_exists) {
    cli::cli_inform(
      paste0(
        "(ASSERT_VAR_EXIST) Result is PASS. ",
        toupper(var),
        " is a variable on data set ",
        toupper(df_label), "."
      )
    )
    return(invisible(TRUE))
  } else {
    cli::cli_warn(
      paste0(
        "(ASSERT_VAR_EXIST) Result is FAIL. \"",
        toupper(var),
        "\" is NOT a variable on data set ",
        toupper(df_label), "."
      )
    )
    return(invisible(FALSE))
  }
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS dataset open/close/varnum -> R names() column check
#      (dramatically simpler in R; no dataset locking concerns).
#    - SAS case-insensitive variable names -> R case-sensitive by
#      default, with case-insensitive fallback to accommodate SAS-
#      origin datasets where casing may differ.
#    - Function accepts both data frame objects and string names,
#      mirroring SAS two-level dataset references (WORK.name).
#    - assert_dset_exist() is sourced from the same directory or
#      must be pre-loaded in the session (mirrors SAS SASAUTOS).
#    - SAS dataset ID (DSID) locking notes (line 20) do not apply
#      in R; data frames have no file-level lock mechanism.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None -- this is a validation utility with no numerical
#      computation.
# NO DIRECT R EQUIVALENT:
#    - SAS %sysfunc(open/varnum/close) -> R names() check. The
#      entire open-check-close ceremony is unnecessary in R because
#      data frames are in-memory objects with directly accessible
#      column name vectors.
#    - SAS dataset locking issues (lines 20-21, 26) -> not
#      applicable in R; data frames are not file-locked.
#    - SAS %sysfunc(sysmsg()) error message retrieval (line 31) ->
#      R tryCatch condition messages serve the same purpose.
# PACKAGE SELECTION RATIONALE:
#    - cli (>= 3.6.0): Provides informative user-facing messages
#      matching the SAS %PUT NOTE/ERROR format for PASS/FAIL
#      assertion logging via cli_inform() and cli_warn().
#    - Base R names()/is.data.frame()/get()/exists(): Direct
#      column existence checks; no additional packages needed.
# OPEN QUESTIONS:
#    - Should variable name matching be case-sensitive (R default)
#      or case-insensitive (SAS default)? Current implementation
#      tries exact match first, then falls back to case-insensitive.
#      A strict R-only project may want exact-only matching.
#    - Should the function support checking multiple variables at
#      once (character vector)? The SAS macro checks one variable
#      per call; this migration preserves that behaviour.
# ============================================================
