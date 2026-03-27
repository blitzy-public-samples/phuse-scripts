#' Count the Number of Unique Values in a Variable
#'
#' Counts the number of distinct (unique) non-missing values in a specified
#' column of a data frame, with an optional filter expression. This is the
#' R migration of the SAS macro \code{\%util_count_unique_values(ds, var, sym, sqlwhr=)}
#' from \code{whitepapers/utilities/util_count_unique_values.sas}.
#'
#' In SAS, the result was assigned to a global macro variable via
#' \code{PROC SQL count(unique(var)) into: &sym}. In R, the integer count
#' is returned directly as the function's return value.
#'
#' @param df A data frame containing the variable to count.
#'   Replaces the SAS \code{DS} positional parameter (libname.memname).
#' @param var A character string specifying the column name in \code{df}
#'   whose distinct values are to be counted. Replaces the SAS \code{VAR}
#'   positional parameter.
#' @param sql_whr An optional character string containing an R filter
#'   expression to subset \code{df} before counting. The expression is
#'   parsed and evaluated within \code{dplyr::filter()}. Replaces the SAS
#'   \code{SQLWHR} keyword parameter (which accepted a SQL WHERE clause).
#'   Default is \code{NULL} (no filtering).
#'
#' @return An integer: the count of distinct non-missing values of \code{var}
#'   in \code{df} (after optional filtering). Matches the semantics of
#'   SAS \code{PROC SQL count(unique(var))}, which excludes missing values.
#'
#' @details
#' \strong{SAS-to-R Mapping:}
#' \itemize{
#'   \item SAS \code{\%macro util_count_unique_values(ds, var, sym, sqlwhr=)}
#'         \eqn{\rightarrow} R \code{util_count_unique_values(df, var, sql_whr)}
#'   \item SAS \code{PROC SQL count(unique(var))} \eqn{\rightarrow}
#'         R \code{dplyr::n_distinct(df[[var]], na.rm = TRUE)}
#'   \item SAS global symbol assignment (\code{\%let &sym = ...})
#'         \eqn{\rightarrow} R function return value
#'   \item SAS \code{\%assert_dset_exist / \%assert_var_exist}
#'         \eqn{\rightarrow} R input validation with \code{cli::cli_abort()}
#'   \item SAS \code{\%PUT NOTE:} / \code{\%PUT ERROR:}
#'         \eqn{\rightarrow} R \code{cli::cli_inform()} / \code{cli::cli_abort()}
#' }
#'
#' \strong{NA Handling:}
#' SAS \code{count(unique(var))} in PROC SQL excludes missing values
#' (both numeric \code{.} and character blank). This function uses
#' \code{na.rm = TRUE} to match that behaviour. If you need to include
#' \code{NA} as a counted level, use \code{dplyr::n_distinct(df[[var]],
#' na.rm = FALSE)} directly.
#'
#' @examples
#' \dontrun{
#'   # Simple count of distinct treatment arms
#'   adsl <- haven::read_xpt("data/adam/cdisc/adsl.xpt")
#'   n_trt <- util_count_unique_values(adsl, "TRT01A")
#'
#'   # Count with filter expression
#'   n_trt_study <- util_count_unique_values(
#'     adsl, "TRT01A",
#'     sql_whr = "STUDYID == 'CDISCPILOT01'"
#'   )
#' }
#'
#' @author Migrated from SAS by Blitzy (original SAS author: Dante Di Tommaso)
#' @seealso \code{\link[dplyr]{n_distinct}}
#' @export
util_count_unique_values <- function(df, var, sql_whr = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation

  # ---------------------------------------------------------------------------

  # Validate df is a data frame

  if (is.null(df) || !is.data.frame(df)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "(UTIL_COUNT_UNIQUE_VALUES) ",
          "Input {.arg df} must be a non-NULL data frame."
        )
      )
    )
  }

  # Validate var is a non-empty character scalar
  if (is.null(var) || !is.character(var) || length(var) != 1L || nchar(var) == 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "(UTIL_COUNT_UNIQUE_VALUES) ",
          "{.arg var} must be a single non-empty character string."
        )
      )
    )
  }

  # Validate that var exists as a column in df
  # Mirrors SAS lines 33-34: %assert_dset_exist / %assert_var_exist
  if (!var %in% names(df)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "(UTIL_COUNT_UNIQUE_VALUES) ",
          "Unable to read values from variable {toupper(var)} on data set."
        ),
        "i" = "Available columns: {paste(names(df), collapse = ', ')}"
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Optional filtering (replaces SAS SQLWHR= keyword parameter)
  # ---------------------------------------------------------------------------

  if (!is.null(sql_whr)) {
    if (!is.character(sql_whr) || length(sql_whr) != 1L || nchar(sql_whr) == 0L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "(UTIL_COUNT_UNIQUE_VALUES) ",
            "{.arg sql_whr} must be a single non-empty character string or NULL."
          )
        )
      )
    }
    # Parse the expression string and evaluate within dplyr::filter()
    filter_expr <- rlang::parse_expr(sql_whr)
    df <- tryCatch(
      dplyr::filter(df, !!filter_expr),
      error = function(e) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "(UTIL_COUNT_UNIQUE_VALUES) ",
              "Failed to apply filter expression: {sql_whr}"
            ),
            "i" = "Underlying error: {conditionMessage(e)}"
          )
        )
      }
    )
  }

  # ---------------------------------------------------------------------------
  # Count distinct non-missing values
  # ---------------------------------------------------------------------------
  # SAS PROC SQL count(unique(var)) excludes missing values (numeric . and

  # character blank). We use na.rm = TRUE to match this behaviour exactly.
  result <- dplyr::n_distinct(df[[var]], na.rm = TRUE)

  # Ensure the result is a plain integer (matching SAS integer count)
  result <- as.integer(result)

  # ---------------------------------------------------------------------------
  # Informational logging (replaces SAS %PUT NOTE:)
  # ---------------------------------------------------------------------------
  cli::cli_inform(
    c(
      "v" = paste0(
        "(UTIL_COUNT_UNIQUE_VALUES) ",
        "Successfully counted unique values of {toupper(var)}: {result}"
      )
    )
  )

  result
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS count(unique(var)) in PROC SQL excludes missing values;
#      R dplyr::n_distinct(x, na.rm = TRUE) is used to match this
#      behaviour. If downstream consumers require NA to be counted
#      as a distinct level, they should call n_distinct() directly
#      with na.rm = FALSE.
#    - SAS global symbol assignment (%global &sym; %let &sym = ...)
#      is replaced by a direct R return value. Callers assign the
#      result to a local variable (e.g., n <- util_count_unique_values(...)).
#    - The sql_whr parameter accepts R filter expressions
#      (e.g., "STUDYID == 'STUDY01'"), NOT SAS SQL WHERE syntax
#      (e.g., "where studyid = 'STUDY01'"). Callers migrating from
#      SAS must convert their WHERE clauses to R logical expressions.
#    - The SAS SYM parameter (name of macro variable to create) is
#      removed because R functions return values directly.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS count(unique(var)) always excludes missing values.
#      R n_distinct() with na.rm = TRUE replicates this. If na.rm
#      were FALSE, R would count NA as one extra distinct value —
#      verify na.rm setting matches expectations for every call site.
#    - If the data frame has zero rows (e.g., after filtering), R
#      returns 0L; SAS would return 0 in the macro variable.
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC SQL count(unique()) → dplyr::n_distinct() is the
#      idiomatic tidyverse equivalent (functionally identical with
#      na.rm = TRUE).
#    - SAS %symexist / %global → not needed; R uses function return values.
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): n_distinct() is the idiomatic tidyverse
#      equivalent of SAS count(unique()). filter() is used for the
#      optional WHERE clause.
#    - rlang (>= 1.1.0): parse_expr() converts the sql_whr string
#      into an evaluable R expression for dplyr::filter(), replacing
#      SAS's dynamic SQL WHERE clause evaluation.
#    - cli (>= 3.6.0): cli_abort() and cli_inform() provide
#      user-facing messages matching the SAS %PUT ERROR / %PUT NOTE
#      convention used in the original macro.
#
# OPEN QUESTIONS:
#    - Downstream WPCT scripts that called this SAS macro used the
#      global symbol; those R migrations must be updated to capture
#      the return value instead.
#    - If a consumer needs NA counted as a distinct level, they
#      should use n_distinct(df[[var]], na.rm = FALSE) directly.
# ============================================================
