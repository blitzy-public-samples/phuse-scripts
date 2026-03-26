# ============================================================
# assert_unique_keys.R
# Migration of: whitepapers/utilities/assert_unique_keys.sas
# ============================================================
# Purpose : Validate that records in a dataset (data frame) are
#           unique according to specified key variables. Returns
#           TRUE (PASS) when keys are unique, or FALSE (FAIL)
#           with a 'fail_auk' attribute containing the duplicate
#           records when uniqueness is violated.
# Author  : Dante Di Tommaso (original SAS macro)
# Ack.    : Inspired by FUTS system from Thotwave
#           http://thotwave.com/resources/futs-framework-unit-testing-sas/
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency checks --------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for assert_unique_keys. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}

if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for assert_unique_keys. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}

# --- Internal dependency: assert_dset_exist --------------------
# Mirrors SAS line 51: %let continue = %assert_dset_exist(&ds)
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
# Mirrors SAS lines 60-64: %let continue = %assert_var_exist(&ds, &key)
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

#' Assert Unique Keys on a Dataset
#'
#' Validates that records in a dataset (data frame) are unique according to
#' the specified key variables. Returns \code{TRUE} (PASS) when all
#' combinations of key values are unique, or \code{FALSE} (FAIL) when
#' duplicate key combinations are found. On failure, the duplicate records
#' are attached as a \code{"fail_auk"} attribute on the return value.
#'
#' This is the R migration of the SAS macro
#' \code{\%assert_unique_keys(ds, keys, incl=, sqlwhr=)} from
#' \code{whitepapers/utilities/assert_unique_keys.sas}.
#'
#' @param df A data frame object OR a character string naming an in-memory
#'   data frame (analogous to a SAS dataset name such as \code{ANA.ADVS}).
#' @param keys A character vector of key column names that should compose
#'   unique identifiers. Replaces the SAS space-delimited KEYS parameter.
#'   Example: \code{c("USUBJID", "PARAMCD", "ATPTN", "AVISITN")}
#' @param incl An optional character vector of additional column names to
#'   include in the failure output for troubleshooting. Replaces the SAS
#'   space-delimited INCL parameter.
#'   Example: \code{c("PARAM", "ATPT", "AVISIT")}
#' @param sql_whr An optional filter expression string to limit the
#'   uniqueness check to a subset of the data. Replaces the SAS SQLWHR
#'   parameter. Example: \code{"STUDYID == 'STUDY01'"}
#' @param envir Environment in which to look up string names when \code{df}
#'   is a character string. Defaults to the calling environment
#'   (\code{parent.frame()}).
#'
#' @return Logical \code{TRUE} (invisible) if all records have unique key
#'   combinations (PASS), or \code{FALSE} (invisible) if duplicates are
#'   found (FAIL). When \code{FALSE}, the return value carries an attribute
#'   \code{"fail_auk"} containing a data frame of the duplicate records
#'   (analogous to the SAS \code{WORK.FAIL_AUK} dataset).
#'
#' @details
#' The function mirrors the SAS macro logic step by step:
#' \enumerate{
#'   \item Validates that the dataset exists via
#'     \code{\link{assert_dset_exist}} (SAS line 51).
#'   \item Validates that \code{keys} is non-empty (SAS lines 53-56).
#'   \item Validates each key variable exists via
#'     \code{\link{assert_var_exist}} loop (SAS lines 58-67).
#'   \item Validates optional \code{incl} variables exist via
#'     \code{\link{assert_var_exist}} loop (SAS lines 70-80).
#'   \item Applies optional \code{sql_whr} filter (SAS lines 89-91).
#'   \item Detects duplicates using \code{dplyr::group_by} +
#'     \code{dplyr::filter(n() > 1)} — replacing SAS PROC SQL
#'     GROUP BY / HAVING COUNT > 1 (SAS lines 84-97).
#'   \item If duplicates found: logs FAIL, returns FALSE with fail_auk
#'     (SAS lines 99-101).
#'   \item If no duplicates: logs PASS, returns TRUE (SAS lines 103-106).
#' }
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/assert_unique_keys.sas (112 lines)}
#'   \item{SAS macro}{\%assert_unique_keys(ds, keys, incl=, sqlwhr=)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{Acknowledgement}{Inspired by FUTS system from Thotwave}
#' }
#'
#' @examples
#' # Unique keys — PASS
#' df <- data.frame(
#'   USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
#'   PARAMCD = c("ALT", "ALT", "AST"),
#'   AVAL    = c(25, 30, 18)
#' )
#' assert_unique_keys(df, keys = c("USUBJID", "PARAMCD"))
#' # Returns TRUE
#'
#' # Duplicate keys — FAIL
#' df2 <- data.frame(
#'   USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02"),
#'   PARAMCD = c("ALT", "ALT", "AST"),
#'   AVAL    = c(25, 30, 18)
#' )
#' result <- assert_unique_keys(df2, keys = c("USUBJID", "PARAMCD"))
#' # Returns FALSE; attr(result, "fail_auk") contains the duplicate rows
#'
#' @export
assert_unique_keys <- function(df,
                               keys,
                               incl    = NULL,
                               sql_whr = NULL,
                               envir   = parent.frame()) {

  # ------------------------------------------------------------------
  # Capture a human-readable label for the dataset for log messages.
  # If df is a character string, use it directly; otherwise deparse
  # the expression the caller passed.

  # ------------------------------------------------------------------
  df_label <- if (is.character(df) && length(df) == 1L) {
    df
  } else {
    deparse(substitute(df))
  }

  # ==================================================================
  # STEP 1 — Validate dataset existence
  # Mirrors SAS line 51: %let continue = %assert_dset_exist(&ds)
  # ==================================================================
  dset_ok <- tryCatch(
    assert_dset_exist(df, envir = envir),
    error = function(e) FALSE
  )

  if (!isTRUE(dset_ok)) {
    cli::cli_warn(
      paste0(
        "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
        "Dataset '", toupper(df_label), "' is not accessible."
      )
    )
    result <- FALSE
    return(invisible(result))
  }

  # ==================================================================
  # STEP 2 — Validate that keys is non-empty
  # Mirrors SAS lines 53-56: %if %length(&keys) < 1
  # ==================================================================
  if (is.null(keys) || !is.character(keys) || length(keys) == 0L ||
      all(!nzchar(keys))) {
    cli::cli_warn(
      "(ASSERT_UNIQUE_KEYS) Result is FAIL. Please specify a variable name."
    )
    result <- FALSE
    return(invisible(result))
  }

  # Remove any empty-string elements from keys
  keys <- keys[nzchar(keys)]
  if (length(keys) == 0L) {
    cli::cli_warn(
      "(ASSERT_UNIQUE_KEYS) Result is FAIL. Please specify a variable name."
    )
    result <- FALSE
    return(invisible(result))
  }

  # ==================================================================
  # STEP 3 — Resolve df to an actual data frame
  # If a string name was provided, resolve via get() in the calling
  # environment. If a data frame was passed directly, use as-is.
  # ==================================================================
  if (is.data.frame(df)) {
    df_resolved <- df
  } else if (is.character(df) && length(df) == 1L && nzchar(df)) {
    df_resolved <- tryCatch(
      {
        obj <- get(df, envir = envir)
        if (!is.data.frame(obj)) {
          cli::cli_warn(
            paste0(
              "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
              "Dataset '", toupper(df_label),
              "' is not accessible as a data frame."
            )
          )
          NULL
        } else {
          obj
        }
      },
      error = function(e) {
        cli::cli_warn(
          paste0(
            "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
            "Dataset '", toupper(df_label), "' is not accessible."
          )
        )
        NULL
      }
    )
    if (is.null(df_resolved)) {
      result <- FALSE
      return(invisible(result))
    }
  } else {
    cli::cli_warn(
      paste0(
        "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
        "Invalid dataset specification."
      )
    )
    result <- FALSE
    return(invisible(result))
  }

  # ==================================================================
  # STEP 4 — Validate each KEY variable exists via assert_var_exist
  # Mirrors SAS lines 58-67: %do loop over KEYS
  # Short-circuit: stop on first missing key variable (SAS behaviour:
  # %if &continue %then %let continue = %assert_var_exist)
  # ==================================================================
  continue <- TRUE
  for (key_var in keys) {
    if (continue) {
      key_ok <- tryCatch(
        assert_var_exist(df_resolved, key_var, envir = envir),
        error = function(e) FALSE
      )
      if (!isTRUE(key_ok)) {
        continue <- FALSE
      }
    }
  }

  if (!continue) {
    result <- FALSE
    return(invisible(result))
  }

  # ==================================================================
  # STEP 5 — Validate optional INCL variables via assert_var_exist
  # Mirrors SAS lines 70-80: %do loop over INCL
  # ==================================================================
  incl_cols <- character(0L)
  if (!is.null(incl) && is.character(incl)) {
    incl <- incl[nzchar(incl)]
    if (length(incl) > 0L) {
      for (incl_var in incl) {
        if (continue) {
          incl_ok <- tryCatch(
            assert_var_exist(df_resolved, incl_var, envir = envir),
            error = function(e) FALSE
          )
          if (!isTRUE(incl_ok)) {
            continue <- FALSE
          }
        }
      }
      if (!continue) {
        result <- FALSE
        return(invisible(result))
      }
      incl_cols <- incl
    }
  }

  # ==================================================================
  # STEP 6 — Apply optional SQL WHERE filter
  # Mirrors SAS lines 89-91: optional &sqlwhr clause in PROC SQL
  # The sql_whr parameter is an R filter expression string that gets
  # parsed and evaluated within dplyr::filter().
  # ==================================================================
  df_work <- df_resolved
  if (!is.null(sql_whr) && is.character(sql_whr) && nzchar(sql_whr)) {
    df_work <- tryCatch(
      {
        filter_expr <- rlang::parse_expr(sql_whr)
        dplyr::filter(df_work, !!filter_expr)
      },
      error = function(e) {
        cli::cli_warn(
          paste0(
            "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
            "Unable to apply filter expression: ", sql_whr,
            ". Error: ", conditionMessage(e)
          )
        )
        NULL
      }
    )
    if (is.null(df_work)) {
      result <- FALSE
      return(invisible(result))
    }
  }

  # ==================================================================
  # STEP 7 — Detect duplicates
  # Mirrors SAS lines 84-97: PROC SQL GROUP BY HAVING count > 1
  #
  # Strategy: Select keys + incl columns, group by keys, keep groups
  # with more than one row (duplicates), then arrange for readability.
  # Uses dplyr::group_by(), dplyr::filter(), dplyr::select(),
  # dplyr::across(), dplyr::all_of(), dplyr::arrange(), dplyr::ungroup(),
  # and dplyr::n() — all specified in the external_imports schema.
  # ==================================================================
  all_cols <- unique(c(keys, incl_cols))

  fail_auk <- df_work %>%
    dplyr::select(dplyr::all_of(all_cols)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(dplyr::n() > 1L) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(all_cols)))

  # ==================================================================
  # STEP 8 — Return result
  # Mirrors SAS lines 99-106:
  #   If SQLOBS != 0 -> FAIL (ERROR), CONTINUE=0, FAIL_AUK exists
  #   If SQLOBS == 0 -> PASS (NOTE), delete FAIL_AUK
  # ==================================================================
  keys_display <- paste(toupper(keys), collapse = ", ")
  n_dupes <- nrow(fail_auk)

  if (n_dupes > 0L) {
    # FAIL — duplicates found
    # Mirrors SAS line 100: %put ERROR: (ASSERT_UNIQUE_KEYS) Result is FAIL ...
    filter_msg <- ""
    if (!is.null(sql_whr) && is.character(sql_whr) && nzchar(sql_whr)) {
      filter_msg <- paste0(" ", sql_whr)
    }

    cli::cli_warn(
      paste0(
        "(ASSERT_UNIQUE_KEYS) Result is FAIL. ",
        "Unexpected duplicates in ", toupper(df_label),
        " with unique keys ", keys_display,
        filter_msg,
        " (N = ", n_dupes, "). See fail_auk attribute."
      )
    )

    result <- FALSE
    attr(result, "fail_auk") <- fail_auk
    return(invisible(result))
  } else {
    # PASS — no duplicates
    # Mirrors SAS line 104: %put NOTE: (ASSERT_UNIQUE_KEYS) Result is PASS ...
    filter_msg <- ""
    if (!is.null(sql_whr) && is.character(sql_whr) && nzchar(sql_whr)) {
      filter_msg <- paste0(" ", sql_whr)
    }

    cli::cli_inform(
      paste0(
        "(ASSERT_UNIQUE_KEYS) Result is PASS. ",
        toupper(df_label),
        " has unique records for keys ", keys_display,
        filter_msg,
        " (N = 0)."
      )
    )

    result <- TRUE
    return(invisible(result))
  }
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global CONTINUE flag -> R logical return value (TRUE/FALSE).
#      The calling program checks the return value instead of a global
#      macro variable.
#    - SAS WORK.FAIL_AUK dataset -> R tibble attached as "fail_auk"
#      attribute to the FALSE return value. When the assertion passes,
#      no fail_auk attribute exists (mirrors SAS %util_delete_dsets).
#    - SAS space-delimited keys string -> R character vector of key
#      names. E.g., SAS "USUBJID PARAMCD" becomes c("USUBJID", "PARAMCD").
#    - SAS SQLWHR clause (e.g., "where studyid = 'STUDY01'") -> R
#      filter expression string (e.g., "STUDYID == 'STUDY01'") that
#      is parsed via rlang::parse_expr() and evaluated in dplyr::filter().
#    - SAS PROC SQL GROUP BY HAVING count > 1 -> dplyr group_by +
#      filter(n() > 1). Semantically identical duplicate detection.
#    - Variable existence checks use assert_var_exist() with
#      case-insensitive fallback, matching SAS %upcase behaviour.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a validation/assertion utility with no numerical
#      computation. Duplicate detection is purely row-matching logic.
# NO DIRECT R EQUIVALENT:
#    - SAS %GLOBAL CONTINUE -> R function return value. The global
#      side-effect pattern is replaced by an explicit return value
#      that the calling program must capture.
#    - SAS WORK.FAIL_AUK dataset -> R attribute on return value.
#      The calling code accesses it via attr(result, "fail_auk").
#    - SAS %util_delete_dsets(fail_auk) -> Not needed in R; when
#      the assertion passes, no attribute is set (no object to clean up).
#    - SAS SQLOBS automatic macro variable -> nrow(fail_auk) in R.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): group_by + filter(n() > 1) for duplicate
#      detection. Preferred over base R duplicated() to maintain
#      tidyverse consistency and to naturally return the full set of
#      duplicate rows (not just the second occurrence).
#    - cli (>= 3.6.0): Informative user-facing messages matching
#      the SAS %PUT NOTE/ERROR format for PASS/FAIL assertion logging.
#    - rlang: Used for parse_expr() to safely evaluate filter
#      expression strings (sql_whr parameter).
# OPEN QUESTIONS:
#    - Should fail_auk be returned as an attribute on the logical
#      return value (current implementation) or as a separate list
#      element? The attribute approach preserves the simple TRUE/FALSE
#      return contract matching the SAS %GLOBAL CONTINUE pattern.
#    - The SAS HAVING clause uses count(last_key_var) > 1 rather than
#      count(*) > 1. In practice this only differs when the last key
#      variable has NAs; the R implementation uses n() > 1 which
#      counts all rows in the group (equivalent to count(*)).
# ============================================================
