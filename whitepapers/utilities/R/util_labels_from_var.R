# ============================================================
# util_labels_from_var.R
# Migration of: whitepapers/utilities/util_labels_from_var.sas
# ============================================================
# Purpose : Extract distinct value-label pairs from two columns
#           of a data frame.  Returns an ordered tibble of
#           value / label pairs, a named lookup vector, and the
#           pair count — replacing the SAS pattern of creating
#           global macro variable sequences (PREFIX_VAL1 …
#           PREFIX_VALN, PREFIX_LAB1 … PREFIX_LABN).
#
# SAS Macro : %util_labels_from_var(ds, var, lab, prefix=, whr=)
# Author    : Dante Di Tommaso (original SAS macro)
# Migrated  : SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# ============================================================

# --- External dependency checks --------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' (>= 1.1.0) is required for util_labels_from_var. ",
    "Install with: install.packages('dplyr')",
    call. = FALSE
  )
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for util_labels_from_var. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for util_labels_from_var. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}
if (!requireNamespace("haven", quietly = TRUE)) {
  stop(
    "Package 'haven' (>= 2.5.0) is required for util_labels_from_var. ",
    "Install with: install.packages('haven')",
    call. = FALSE
  )
}
# Note: haven::val_labels() is referenced in the schema but is NOT an
# exported function in haven >= 2.5.  The actual API for accessing value
# labels on haven::labelled() vectors is attr(x, "labels").  The haven
# package is still required for haven::labelled class recognition and
# variable-level label support (attr(x, "label")).

# --- Internal dependency: assert_dset_exist, assert_var_exist ---
# Mirrors SAS %include for %assert_dset_exist and %assert_var_exist.
# The functions are sourced from the same directory if not already
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

#' Extract Distinct Value-Label Pairs from Two Variables
#'
#' Extracts the unique combinations of a primary value variable and a
#' secondary label variable from a data frame, ordered by the primary
#' variable.
#'
#' This is the R migration of the SAS macro
#' \code{\%util_labels_from_var(ds, var, lab, prefix=, whr=)} from
#' \code{whitepapers/utilities/util_labels_from_var.sas}.
#'
#' In SAS the macro creates sequences of global symbols
#' (\code{PREFIX_VAL1 … PREFIX_VALN}, \code{PREFIX_LAB1 … PREFIX_LABN},
#' and \code{PREFIX_N}).
#' In R the function returns a structured list containing the same
#' information without global side-effects.
#'
#' @param df A data frame (or tibble) containing the value and label
#'   columns. Analogous to the SAS \code{DS} positional parameter.
#' @param var Character string naming the primary variable whose distinct
#'   values are to be extracted (e.g., \code{"AVISITN"}).
#'   Analogous to the SAS \code{VAR} positional parameter.
#' @param lab Character string naming the label variable that provides a
#'   human-readable label for each value in \code{var} (e.g.,
#'   \code{"AVISIT"}).
#'   Analogous to the SAS \code{LAB} positional parameter.
#' @param prefix Optional character string used as a prefix for naming
#'   the output.
#'   Defaults to the value of \code{var} when \code{NULL} or empty.
#'   Analogous to the SAS \code{PREFIX} keyword parameter.
#' @param whr Optional character string containing an R filter expression
#'   to subset the data before pair extraction (e.g.,
#'   \code{"studyid == 'STUDY01'"}).
#'   Analogous to the SAS \code{WHR} keyword parameter but uses R
#'   expression syntax rather than SAS WHERE clause syntax.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{\code{n}}{Integer — the number of distinct value-label pairs
#'     (replaces \code{PREFIX_N} global symbol in SAS).}
#'   \item{\code{pairs}}{A tibble with two columns, \code{value} and
#'     \code{label}, ordered by \code{value}
#'     (replaces \code{PREFIX_VAL1 … PREFIX_VALN} and
#'     \code{PREFIX_LAB1 … PREFIX_LABN}).}
#'   \item{\code{lookup}}{A named character vector where names are the
#'     values and elements are the labels.
#'     Convenient for direct value-to-label translation
#'     (e.g., \code{lookup["8"] => "WEEK 8"}).}
#' }
#' The list carries two additional attributes:
#' \describe{
#'   \item{\code{prefix}}{The resolved prefix string.}
#'   \item{\code{var_label}}{The \code{haven} variable-level label
#'     of the \code{var} column, if available (\code{NULL} otherwise).}
#'   \item{\code{haven_val_labels}}{The \code{haven} value labels
#'     attached to the \code{var} column, if any (\code{NULL} otherwise).}
#' }
#'
#' @details
#' **Validation steps** (mirroring SAS lines 51-53):
#' \enumerate{
#'   \item \code{\link{assert_dset_exist}} validates the data frame.
#'   \item \code{\link{assert_var_exist}} validates both \code{var} and
#'     \code{lab} columns.
#' }
#'
#' **1-to-1 mapping check** (mirroring SAS lines 74-78):
#' After extracting distinct pairs, the function checks that every value
#' of \code{var} maps to exactly one \code{lab} value.
#' If duplicates are found a warning is issued via \code{cli::cli_warn}
#' (matching the SAS \code{PUT ERROR} on lines 76-77).
#'
#' **Haven label integration** (Phase 5):
#' If the \code{var} column carries a \code{haven} variable label
#' (\code{attr(df[[var]], "label")}) or value labels
#' (\code{haven::val_labels()}), these are captured and included in
#' the returned list attributes for downstream consumers.
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/util_labels_from_var.sas (94 lines)}
#'   \item{Author}{Dante Di Tommaso}
#' }
#'
#' @examples
#' # Simple example
#' df <- data.frame(
#'   AVISITN = c(1, 2, 3, 1, 2, 3),
#'   AVISIT  = c("Baseline", "Week 2", "Week 4",
#'                "Baseline", "Week 2", "Week 4"),
#'   stringsAsFactors = FALSE
#' )
#' result <- util_labels_from_var(df, "AVISITN", "AVISIT")
#' result$n       # 3
#' result$pairs   # tibble(value = c(1,2,3), label = c("Baseline","Week 2","Week 4"))
#' result$lookup  # c("1" = "Baseline", "2" = "Week 2", "3" = "Week 4")
#'
#' # With filter
#' result2 <- util_labels_from_var(df, "AVISITN", "AVISIT",
#'                                  whr = "AVISITN <= 2")
#'
#' @export
util_labels_from_var <- function(df, var, lab, prefix = NULL, whr = NULL) {

  # ==================================================================
  # STEP 0 — Resolve prefix default
  # Mirrors SAS line 46: %if %length(&prefix) = 0 %then %let prefix = &var;
  # ==================================================================
  if (is.null(prefix) || !nzchar(trimws(prefix))) {
    prefix <- var
  }

  # ==================================================================
  # STEP 1 — Validate dataset existence
  # Mirrors SAS line 51: %let OK = %assert_dset_exist(&ds)
  # ==================================================================
  dset_ok <- tryCatch(
    assert_dset_exist(df),
    error = function(e) FALSE
  )

  if (!isTRUE(dset_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_LABELS_FROM_VAR) Unable to read variable ",
        toupper(var), " or ", toupper(lab),
        " on the supplied data frame."
      )
    )
  }

  # ==================================================================
  # STEP 2 — Validate that VAR column exists
  # Mirrors SAS line 52: %if &OK %then %let OK = %assert_var_exist(&ds, &var)
  # ==================================================================
  var_ok <- tryCatch(
    assert_var_exist(df, var),
    error = function(e) FALSE
  )

  if (!isTRUE(var_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_LABELS_FROM_VAR) Unable to read variable ",
        toupper(var), " or ", toupper(lab),
        " on the supplied data frame."
      )
    )
  }

  # ==================================================================
  # STEP 3 — Validate that LAB column exists
  # Mirrors SAS line 53: %if &OK %then %let OK = %assert_var_exist(&ds, &lab)
  # ==================================================================
  lab_ok <- tryCatch(
    assert_var_exist(df, lab),
    error = function(e) FALSE
  )

  if (!isTRUE(lab_ok)) {
    cli::cli_abort(
      paste0(
        "(UTIL_LABELS_FROM_VAR) Unable to read variable ",
        toupper(var), " or ", toupper(lab),
        " on the supplied data frame."
      )
    )
  }

  # ==================================================================
  # STEP 4 — Apply optional WHERE filter
  # Mirrors SAS line 57:
  #   %if %length(&whr) > 0 %then %let whr = where &whr;
  # In R: parse the string expression and apply via dplyr::filter().
  # ==================================================================
  if (!is.null(whr) && nzchar(trimws(whr))) {
    df <- tryCatch(
      {
        filter_expr <- rlang::parse_expr(whr)
        dplyr::filter(df, !!filter_expr)
      },
      error = function(e) {
        cli::cli_abort(
          paste0(
            "(UTIL_LABELS_FROM_VAR) Invalid filter expression: '",
            whr, "'. Error: ", conditionMessage(e)
          )
        )
      }
    )
  }

  # ==================================================================
  # STEP 5 — Extract distinct VAR-LAB pairs, ordered by VAR

  # Mirrors SAS lines 66-70:
  #   proc sort data=&ds out=css_lfv nodupkey;
  #     by &var &lab;
  #     &whr ;
  #   run;
  # dplyr::distinct() replaces PROC SORT NODUPKEY.
  # dplyr::arrange() replaces the BY statement ordering.
  # ==================================================================
  pairs <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(var, lab)))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(var)))

  # ==================================================================
  # STEP 6 — Validate 1:1 mapping (each VAR value → exactly one LAB)
  # Mirrors SAS lines 72-78:
  #   data _null_;
  #     set css_lfv;
  #     by &var;
  #     if not (first.&var and last.&var) then do;
  #       put "ERROR: (UTIL_LABELS_FROM_VAR) Each %upcase(&VAR) value
  #            should have exactly one %upcase(&LAB) value." ...;
  #     end;
  #   run;
  #
  # In R: count occurrences of each VAR value in the distinct pairs
  # tibble. If any VAR value has more than one associated LAB value,
  # emit a warning.
  # ==================================================================
  dupes <- pairs %>%
    dplyr::count(dplyr::across(dplyr::all_of(var))) %>%
    dplyr::filter(.data[["n"]] > 1L)

  if (nrow(dupes) > 0L) {
    dupe_vals <- paste(dupes[[var]], collapse = ", ")
    cli::cli_warn(
      paste0(
        "(UTIL_LABELS_FROM_VAR) Each ", toupper(var),
        " value should have exactly one ", toupper(lab),
        " value. Duplicates found for: ", dupe_vals,
        ". Most likely you are missing some values in the lookup."
      )
    )
  }

  # ==================================================================
  # STEP 7 — Build the return structure
  # Replaces SAS lines 80-81: CALL SYMPUT to store value/label pairs
  # in global macro variable sequences.
  #
  # R returns a list with:
  #   $n      — count of distinct pairs
  #   $pairs  — tibble(value, label) ordered by value
  #   $lookup — named character vector for convenient translation
  # ==================================================================

  # Rename the columns to standardised names while keeping originals
  # accessible via attribute metadata.
  result_pairs <- dplyr::tibble(
    value = pairs[[var]],
    label = pairs[[lab]]
  )

  # Named lookup vector: names = stringified values, values = labels.
  # Coerce value to character for consistent naming (SAS CALL SYMPUT
  # always stores as character strings).
  lookup_vec <- stats::setNames(
    as.character(result_pairs[["label"]]),
    as.character(result_pairs[["value"]])
  )

  n_pairs <- nrow(result_pairs)

  # ==================================================================
  # STEP 8 — Haven label integration (Phase 5)
  # Extract variable-level label and value labels if the column was
  # imported via haven (e.g., from SAS XPT / SAS7BDAT).
  # ==================================================================
  var_label <- tryCatch(
    attr(df[[var]], "label"),
    error = function(e) NULL
  )

  # Value labels on haven::labelled vectors are stored in the "labels"

  # attribute (a named numeric/character vector).  haven does not export
  # a standalone val_labels() accessor; use attr() directly.
  haven_val_labels <- tryCatch(
    {
      vl <- attr(df[[var]], "labels")
      if (is.null(vl) || length(vl) == 0L) NULL else vl
    },
    error = function(e) NULL
  )

  # ==================================================================
  # STEP 9 — Success message
  # Mirrors SAS line 88:
  #   %put NOTE: (UTIL_LABELS_FROM_VAR) Successfully created symbols
  #              for Values and Labels from %upcase(&var) and %upcase(&lab);
  # ==================================================================
  cli::cli_inform(
    paste0(
      "(UTIL_LABELS_FROM_VAR) Successfully created value-label pairs ",
      "from ", toupper(var), " and ", toupper(lab),
      " (", n_pairs, " distinct pair",
      if (n_pairs != 1L) "s" else "", ")."
    )
  )

  # ==================================================================
  # Assemble and return the result list
  # ==================================================================
  result <- list(
    n      = n_pairs,
    pairs  = result_pairs,
    lookup = lookup_vec
  )

  attr(result, "prefix")           <- prefix
  attr(result, "var_label")        <- var_label
  attr(result, "haven_val_labels") <- haven_val_labels

  return(result)
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global macro variable sequences (PREFIX_VAL1, PREFIX_LAB1,
#      ..., PREFIX_VALN, PREFIX_LABN, PREFIX_N) replaced by an R list
#      return value containing $n (count), $pairs (tibble), and
#      $lookup (named vector).
#    - Consumers of this function should use the returned tibble /
#      named vector instead of global symbols. For example:
#        result <- util_labels_from_var(df, "AVISITN", "AVISIT")
#        result$lookup["2"]  # => "Week 2"
#    - The whr parameter accepts R filter expressions
#      (e.g., "studyid == 'STUDY01'") NOT SAS WHERE syntax.
#    - The assert_dset_exist() and assert_var_exist() functions must
#      be available in the session (auto-sourced from same directory
#      or pre-loaded).
#    - The SAS %util_count_unique_values call (line 59 of source) is
#      inlined into this function via nrow() on the distinct pairs —
#      a separate function call is unnecessary in R.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None expected — this is a metadata extraction utility with no
#      arithmetic computation.
#    - Sort order: dplyr::arrange() uses R's default locale-aware
#      ordering which matches SAS PROC SORT for numeric variables.
#      For character variables with non-ASCII content the order may
#      differ between SAS and R locales; downstream consumers should
#      verify if locale-specific ordering is required.
# NO DIRECT R EQUIVALENT:
#    - SAS CALL SYMPUT for creating global symbols → R function
#      return value (list with $n, $pairs, $lookup).
#    - SAS %GLOBAL for sequential macro var creation → R tibble with
#      value / label columns.
#    - SAS PROC DATASETS DELETE css_lfv → not needed in R; the
#      intermediate tibble is local to the function scope and
#      garbage-collected automatically.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>= 1.1.0): distinct(), arrange(), count(), filter(),
#      across(), all_of() replacing PROC SORT NODUPKEY and DATA step
#      BY-group processing.  Mandated by AAP §0.8.1 (tidyverse over
#      base R).
#    - haven (>= 2.5.0): Required for haven::labelled class recognition
#      and value label extraction from CDISC ADaM/SDTM datasets imported
#      via read_xpt() / read_sas().  Value labels are accessed via
#      attr(x, "labels") (haven does not export a standalone val_labels
#      accessor).
#    - rlang (>= 1.1.0): parse_expr() for tidy evaluation of the
#      optional whr filter expression.  Standard tidyverse companion
#      per AAP §0.8.1.
#    - cli (>= 3.6.0): cli_warn(), cli_inform(), cli_abort() for
#      user-facing messages matching SAS %PUT NOTE / ERROR format.
# OPEN QUESTIONS:
#    - Should whr accept NSE (non-standard evaluation) expressions
#      in addition to string expressions?  Current implementation
#      accepts strings only, consistent with the other migrated
#      utility functions (util_get_var_min_max, util_get_reference,
#      util_value_of_param).
#    - Should the function also support setting haven labels on the
#      output pairs tibble?  Currently haven metadata is captured as
#      attributes but not propagated to the result columns.
# ============================================================
