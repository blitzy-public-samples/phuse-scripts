# =============================================================================
# data_checks.R — Generic Data Validation Checks
# Migrated from: contributed/MedDRA/ZZ_Utilities/data_checks.sas (311 lines)
# =============================================================================
# Four validation functions: chk_var, chk_dm_subj_gt0, chk_val, chk_cmp
# Centralized dataset validation framework for MedDRA analysis pipeline.
# This is a foundational standalone module with no internal dependencies.
# Downstream consumer: ae_setup.R imports chk_var() and chk_dm_subj_gt0().
# =============================================================================

# ---------------------------------------------------------------------------
# Library Dependencies
# ---------------------------------------------------------------------------
library(dplyr)
library(tibble)
library(purrr)
library(cli)
library(rlang)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

#' Sentinel token for missing values (SAS line 1: %let miss = MISSING;)
#' Used as a placeholder in the `values` parameter of chk_val() to indicate
#' that NA / missing values should be checked. Preserved from SAS convention.
MISS <- "MISSING"

# ---------------------------------------------------------------------------
# chk_var — Check Variable Existence in a Dataset
# ---------------------------------------------------------------------------
#' Migrated from SAS %macro chk_var (lines 13-59).
#' SAS opens dataset via OPEN(), checks VARNUM(), VARTYPE(), VARLEN().
#' R equivalent: check names(), is.numeric(), max(nchar()).
#'
#' @param ds   data.frame or tibble to inspect.
#' @param var  character scalar: variable name to check (case-insensitive).
#' @param lib  character scalar: unused in R (SAS libname compatibility).
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{report}{tibble with columns: chk, ds, var, type, len, condition, ind}
#'     \item{exists}{logical: TRUE if variable exists in ds}
#'     \item{type}{character: "N" (numeric), "C" (character), or "" (not found)}
#'     \item{len}{integer: variable length (max nchar for C, 8 for N, -1 if not found)}
#'   }
chk_var <- function(ds, var, lib = NULL) {
  # Capture dataset name from caller expression (SAS: &ds. macro variable)
  ds_name <- toupper(deparse(substitute(ds)))
  var_upper <- toupper(var)

  # Case-insensitive variable name matching
  # SAS: varnum(dsid, upcase("&var.")) > 0
  var_match <- names(ds)[toupper(names(ds)) == var_upper]
  var_exists <- length(var_match) > 0L

  if (var_exists) {
    actual_var <- var_match[1L]
    ind <- 1L
    # SAS: vartype(dsid, varnum) returns "N" or "C"
    type <- if (is.numeric(ds[[actual_var]])) "N" else "C"
    # SAS: varlen(dsid, varnum) returns fixed-width length
    # R: numeric columns report 8 (double precision bytes);
    #    character columns report max observed nchar
    len <- if (type == "N") {
      8L
    } else {
      val_lengths <- nchar(as.character(ds[[actual_var]]), type = "bytes")
      max(val_lengths, 0L, na.rm = TRUE)
    }
    condition <- "EXISTS"
  } else {
    ind <- 0L
    type <- ""
    len <- -1L
    condition <- "DOES NOT EXIST"
  }

  # Build report row (SAS: single-row dataset chk_var_&ds._&var.)
  report <- tibble::tibble(
    chk       = "VAR",
    ds        = ds_name,
    var       = var_upper,
    type      = type,
    len       = len,
    condition = condition,
    ind       = ind
  )

  # SAS: call symputx("&ds._&var.", ind, 'g') etc. → returned as list

  list(
    report = report,
    exists = as.logical(ind),
    type   = type,
    len    = len
  )
}

# ---------------------------------------------------------------------------
# chk_dm_subj_gt0 — Check DM Dataset Has > 0 Subjects
# ---------------------------------------------------------------------------
#' Migrated from SAS %macro chk_dm_subj_gt0 (lines 68-80).
#' SAS: open 'dm', check attrn(dsid, 'nobs') and attrn(dsid, 'nvars').
#'
#' @param dm data.frame or tibble: demographics dataset.
#'
#' @return logical: TRUE if dm has > 0 rows and > 0 columns, FALSE otherwise.
chk_dm_subj_gt0 <- function(dm) {
  # Guard: ensure input is a data frame
  if (!is.data.frame(dm)) {
    return(FALSE)
  }
  # SAS: nobs = attrn(dsid, 'nobs'); nvars = attrn(dsid, 'nvars');
  # dm_subj_gt0 = '1' if nobs > 0 and nvars > 0, else '0'
  nrow(dm) > 0L && ncol(dm) > 0L
}

# ---------------------------------------------------------------------------
# chk_val — Check Value Existence / Count in a Dataset Variable
# ---------------------------------------------------------------------------
#' Migrated from SAS %macro chk_val (lines 100-235).
#' SAS accepts up to 25 positional value parameters (val1..val25);
#' R accepts a character vector of unlimited length via `values`.
#'
#' @param ds     data.frame or tibble to inspect.
#' @param var    character scalar: variable name to check.
#' @param values character vector: values to look for. Use the MISS constant
#'               to check for NA / missing values.
#' @param lib    character scalar: unused in R (SAS libname compatibility).
#' @param cs     logical: if TRUE, comparisons are case-sensitive (default FALSE).
#' @param count  logical: if TRUE, return occurrence counts; if FALSE, return
#'               0/1 presence indicators (default FALSE).
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{report}{tibble with columns: chk, ds, var, val, condition, ind}
#'     \item{results}{named list where each key is {DS}_{VAR}_{VAL} and value
#'                    is the indicator or count; if count=TRUE, also includes
#'                    {DS}_{VAR}_{VAL}_CNT keys}
#'   }
chk_val <- function(ds, var, values, lib = NULL, cs = FALSE, count = FALSE) {
  ds_name <- toupper(deparse(substitute(ds)))
  var_upper <- toupper(var)

  # --- Variable existence and type detection (SAS lines 123-140) ---
  var_match <- names(ds)[toupper(names(ds)) == var_upper]
  success <- length(var_match) > 0L

  if (success) {
    actual_var <- var_match[1L]
    var_type <- if (is.numeric(ds[[actual_var]])) "N" else "C"
    var_len <- if (var_type == "N") {
      8L
    } else {
      val_lengths <- nchar(as.character(ds[[actual_var]]), type = "bytes")
      max(val_lengths, 0L, na.rm = TRUE)
    }
  } else {
    # SAS: if open fails, set defaults and warn (line 232)
    actual_var <- var
    var_type <- "C"
    var_len <- 200L
    cli::cli_warn(
      "Variable {.var {var}} does not exist in dataset {.val {ds_name}}."
    )
  }

  # --- MISSING token handling (SAS lines 143-145) ---
  # Identify which requested values represent missing / NA
  is_miss_token <- toupper(as.character(values)) == toupper(MISS)

  # Build lookup vector with NA where MISS token was used
  lookup_values <- as.character(values)
  lookup_values[is_miss_token] <- NA_character_

  # Coerce lookup values to match variable type
  if (success && var_type == "N") {
    lookup_values <- suppressWarnings(as.numeric(lookup_values))
  }

  # --- Per-value lookup (SAS lines 172-208) ---
  # Accumulate results via side-effect in map_dfr (SAS: call symputx per val)
  results_list <- list()

  report <- purrr::map_dfr(seq_along(values), function(i) {
    val_original <- values[i]
    val_lookup <- lookup_values[i]

    # Display value for report (SAS lines 211-216): empty/NA → "MISSING"
    val_display <- if (is.na(val_original) ||
                       toupper(as.character(val_original)) == toupper(MISS) ||
                       nchar(trimws(as.character(val_original))) == 0L) {
      "MISSING"
    } else {
      as.character(val_original)
    }

    if (!success) {
      # Dataset/variable doesn't exist: indicator = -1 (SAS convention)
      ind_val <- -1L
    } else if (is.na(val_lookup)) {
      # Looking for missing / NA values
      na_count <- sum(is.na(ds[[actual_var]]))
      ind_val <- if (count) as.integer(na_count) else as.integer(na_count > 0L)
    } else {
      # Standard value lookup with case sensitivity handling (SAS lines 148-156)
      # Uses dplyr::filter() for subsetting + dplyr::count() for occurrence counting
      if (var_type == "C" && !cs) {
        # Case-insensitive: compare uppercased values
        match_count <- ds %>%
          dplyr::filter(
            !is.na(.data[[actual_var]]),
            toupper(.data[[actual_var]]) == toupper(as.character(val_lookup))
          ) %>%
          dplyr::count() %>%
          dplyr::pull(n)
      } else {
        # Case-sensitive character or numeric: exact comparison
        match_count <- ds %>%
          dplyr::filter(
            !is.na(.data[[actual_var]]),
            .data[[actual_var]] == val_lookup
          ) %>%
          dplyr::count() %>%
          dplyr::pull(n)
      }
      ind_val <- if (count) as.integer(match_count) else as.integer(match_count > 0L)
    }

    # Build result key: sanitize display value for naming
    # SAS: call symputx("&ds._&var._&val{i}.", ind, 'g')
    val_safe <- gsub("[^A-Za-z0-9_]", "_", gsub("\\s+", "_", val_display))
    result_name <- paste(ds_name, var_upper, val_safe, sep = "_")
    results_list[[result_name]] <<- ind_val

    if (count) {
      cnt_name <- paste0(result_name, "_CNT")
      results_list[[cnt_name]] <<- ind_val
    }

    # Build report row (SAS: rpt_chk_val accumulation lines 220-226)
    cond_text <- if (count) {
      paste0("COUNT OF ", var_upper, " = ", val_display)
    } else {
      paste0(var_upper, " = ", val_display)
    }

    tibble::tibble(
      chk       = "VAL",
      ds        = ds_name,
      var       = var_upper,
      val       = val_display,
      condition = cond_text,
      ind       = ind_val
    )
  })

  list(
    report  = report,
    results = results_list
  )
}

# ---------------------------------------------------------------------------
# chk_cmp — Compare Distinct Values Between Two Dataset Variables
# ---------------------------------------------------------------------------
#' Migrated from SAS %macro chk_cmp (lines 242-310).
#' SAS: PROC SQL full outer join on distinct values; reports mismatches.
#'
#' @param ds1  data.frame or tibble: first dataset.
#' @param var1 character scalar: variable name in ds1.
#' @param ds2  data.frame or tibble: second dataset.
#' @param var2 character scalar: variable name in ds2.
#' @param lib  character scalar: unused in R (SAS libname compatibility).
#'
#' @return tibble with columns: ds1, ds2, in_dataset, value.
#'         Rows represent values that exist in one dataset but not the other.
#'         Empty tibble (0 rows) if all distinct values match.
#'
#' @details Aborts with \code{cli::cli_abort()} if variables do not exist or
#'          are not of the same type (SAS lines 304-308).
chk_cmp <- function(ds1, var1, ds2, var2, lib = NULL) {
  ds1_name <- toupper(deparse(substitute(ds1)))
  ds2_name <- toupper(deparse(substitute(ds2)))
  var1_upper <- toupper(var1)
  var2_upper <- toupper(var2)

  # --- Type validation (SAS lines 258-276) ---
  var1_match <- names(ds1)[toupper(names(ds1)) == var1_upper]
  var2_match <- names(ds2)[toupper(names(ds2)) == var2_upper]

  var1_exists <- length(var1_match) > 0L
  var2_exists <- length(var2_match) > 0L

  if (!var1_exists || !var2_exists) {
    # SAS lines 304-308: %put ERROR:
    cli::cli_abort(
      "One or more datasets or variables do not exist: {.var {var1}} in {.val {ds1_name}}, {.var {var2}} in {.val {ds2_name}}."
    )
  }

  actual_var1 <- var1_match[1L]
  actual_var2 <- var2_match[1L]

  # Check both variables are the same type (SAS lines 264-270)
  type1 <- if (is.numeric(ds1[[actual_var1]])) "N" else "C"
  type2 <- if (is.numeric(ds2[[actual_var2]])) "N" else "C"

  if (type1 != type2) {
    # SAS lines 306-307: %put ERROR: not of the same type
    cli::cli_abort(
      "Variables are not of the same type: {.var {var1}} is {.val {type1}}, {.var {var2}} is {.val {type2}}."
    )
  }

  # --- Find mismatched values via full outer join (SAS lines 280-300) ---
  # Get distinct values from each dataset
  vals1 <- ds1 %>%
    dplyr::distinct(.data[[actual_var1]]) %>%
    dplyr::mutate(in_ds1 = TRUE)

  vals2 <- ds2 %>%
    dplyr::distinct(.data[[actual_var2]]) %>%
    dplyr::mutate(in_ds2 = TRUE)

  # Standardize column name for join compatibility
  names(vals1)[1L] <- "value"
  names(vals2)[1L] <- "value"

  # Full outer join to detect mismatches (SAS: PROC SQL full join)
  combined <- dplyr::full_join(vals1, vals2, by = "value")

  # Filter to mismatches only: rows present in one dataset but not both
  # SAS: where not (not missing(a.&var1.) and not missing(b.&var2.))
  mismatches <- combined %>%
    dplyr::filter(
      is.na(.data[["in_ds1"]]) | is.na(.data[["in_ds2"]])
    ) %>%
    dplyr::mutate(
      in_dataset = dplyr::case_when(
        !is.na(.data[["in_ds1"]]) & is.na(.data[["in_ds2"]]) ~ ds1_name,
        is.na(.data[["in_ds1"]]) & !is.na(.data[["in_ds2"]]) ~ ds2_name,
        TRUE ~ "UNKNOWN"
      ),
      value = dplyr::if_else(
        is.na(.data[["value"]]),
        NA_character_,
        as.character(.data[["value"]])
      )
    ) %>%
    dplyr::transmute(
      ds1        = ds1_name,
      ds2        = ds2_name,
      in_dataset = .data[["in_dataset"]],
      value      = .data[["value"]]
    )

  mismatches
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#   1. Datasets are R data frames or tibbles (not file path references).
#      SAS's OPEN()/CLOSE() dataset handle model is replaced by direct
#      data frame introspection via names(), is.numeric(), nrow(), ncol().
#   2. Variable name matching is case-insensitive, consistent with SAS
#      behavior using upcase("&var.") in VARNUM() calls.
#   3. The MISSING token convention is preserved from SAS (%let miss=MISSING).
#      Functions accept the MISS constant as a sentinel value that maps to
#      NA (numeric) or NA_character_ (character).
#   4. Report tibble column structures (chk, ds, var, type, len, condition,
#      ind for chk_var/chk_val; ds1, ds2, in_dataset, value for chk_cmp)
#      follow the SAS output dataset schemas for downstream compatibility.
#   5. SAS global macro variable scoping (call symputx with 'g' scope) is
#      replaced by function return values in named lists.
#   6. SAS %macro chk_val positional val1..val25 parameters (max 25 values)
#      are consolidated into a single R character vector with no cap.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   None — these are data validation checks only; no statistical
#   computations are performed.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS OPEN()/VARNUM()/VARTYPE()/VARLEN()/ATTRN() → R data frame
#      introspection functions: names(), is.numeric(), nchar(), nrow(),
#      ncol(). The R approach operates on in-memory objects rather than
#      SAS library dataset handles.
#   2. SAS VARLEN() returns fixed-width field length; R character vectors
#      are variable-width. We report max(nchar()) as an approximation.
#      Numeric columns always report 8 (matching SAS 8-byte double).
#   3. SAS macro variable scoping (call symputx with 'g' scope) → function
#      return values via named lists. Callers access results as
#      result$results[["KEY"]] instead of &KEY. macro resolution.
#   4. SAS PROC DATASETS delete → not needed in R (garbage collection).
#   5. SAS PROC SQL with INTO :variable → dplyr pipelines returning tibbles.
#
# PACKAGE SELECTION RATIONALE:
#   - dplyr (>=1.1.0): Core tidyverse data manipulation per AAP mandate.
#     Used for filter(), count(), full_join(), mutate(), case_when(),
#     if_else(), transmute(), distinct(). Replaces SAS PROC SQL and
#     DATA step operations.
#   - tibble (>=3.2.0): Structured return values with consistent column
#     types per AAP mandate (tibble over base data.frame).
#   - purrr (>=1.0.0): Functional iteration via map_dfr() replacing SAS
#     %do loops over val1..val25 positional parameters.
#   - cli (>=3.6.0): User-facing error/warning messages with rich
#     formatting, replacing SAS %put ERROR: statements.
#   - rlang (>=1.1.0): .data pronoun for unambiguous column references
#     in dplyr pipelines when variable names are passed as strings.
#
# OPEN QUESTIONS:
#   1. Variable length (nchar) check may not be meaningful for R character
#      vectors since R does not have fixed-width character fields like SAS.
#      Consider whether downstream consumers rely on the len value for
#      field-width validation or SpreadsheetML column sizing.
#   2. SAS positional parameters val1..val25 allowed up to 25 values;
#      R version accepts unlimited values via character vector. Verify
#      downstream callers do not depend on the 25-value cap.
#   3. deparse(substitute(ds)) captures the caller's expression for the
#      dataset name. If functions are called programmatically (e.g., from
#      within lapply), the captured name may not be meaningful. Consider
#      adding an explicit ds_name parameter if this becomes an issue.
#
# ============================================================
