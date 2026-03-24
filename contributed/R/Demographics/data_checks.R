# =============================================================================
# data_checks.R
# Migrated from: contributed/Demographics/Utility Programs/data_checks.sas
# Description: Data validation functions for the Demographics analysis panel.
#              Provides four validation functions (chk_var, chk_dm_subj_gt0,
#              chk_val, chk_cmp) that perform dataset introspection, variable
#              existence checks, value presence/count checks, and cross-dataset
#              value comparison. Returns tibble-based audit trails replacing
#              SAS rpt_chk_* accumulation tables.
#
# Original SAS: 310 lines, 4 macros (%chk_var, %chk_dm_subj_gt0, %chk_val,
#               %chk_cmp) plus global constant %let miss = MISSING;
#
# Usage: source("contributed/R/Demographics/data_checks.R")
#        Called by demographics_v1.R via source()
# =============================================================================

# --- Library Loading ---
library(dplyr)
library(tibble)
library(rlang)
library(cli)

# --- Global Constant ---
# Replaces SAS: %let miss = MISSING; (line 1)
# Sentinel label used in audit trail tibbles when a value is NA/missing
MISS <- "MISSING"


# =============================================================================
# chk_var()
# Migrated from SAS %chk_var (lines 13-59)
# Checks whether variable VAR exists in dataset DS and returns an audit row
# with indicator (1 = exists, 0 = does not exist), type ("C"/"N"), and length.
#
# In SAS, this macro set global variables (&ds._&var., &ds._&var._type,
# &ds._&var._len) via CALL SYMPUTX and accumulated rows into rpt_chk_var
# via merge. In R, we return a named list containing these values plus an
# audit_row tibble for the caller to accumulate via dplyr::bind_rows().
#
# @param ds   A data frame (or tibble) to inspect.
# @param var  Character string: name of the variable (column) to check.
# @param lib  Character string: library name (kept for API compatibility;
#             unused in R since datasets are passed directly).
# @return A named list with components:
#   \item{ind}{Integer: 1 if variable exists, 0 otherwise.}
#   \item{type}{Character: "C" (character), "N" (numeric), or "" (not found).}
#   \item{len}{Numeric: max observed character length, 8 for numeric, or -1.}
#   \item{audit_row}{A one-row tibble for rpt_chk_var accumulation.}
# =============================================================================
chk_var <- function(ds, var, lib = "work") {


  # Capture the dataset name as passed by the caller (for audit trail)
  ds_name <- tryCatch(
    toupper(deparse(substitute(ds))),
    error = function(e) "UNKNOWN"
  )

  var_upper <- toupper(var)

  # Default values for when dataset is NULL or variable is absent

  ind  <- 0L
  type <- ""
  len  <- -1

  # Check if the dataset is a valid data frame with content

  if (!is.null(ds) && is.data.frame(ds)) {
    # Check if the variable exists in the data frame (case-insensitive match
    # against column names to mirror SAS VARNUM which is case-insensitive)
    col_match <- match(toupper(var), toupper(colnames(ds)))

    if (!is.na(col_match)) {
      actual_col <- colnames(ds)[col_match]
      ind <- 1L

      # Determine type: "C" for character, "N" for numeric
      # Mirrors SAS VARTYPE() which returns 'C' or 'N'
      if (is.character(ds[[actual_col]]) || is.factor(ds[[actual_col]])) {
        type <- "C"
        # Character length: max observed nchar, matching SAS VARLEN concept
        # SAS uses fixed-width allocation; R uses variable-width, so we take
        # the maximum observed string length as the closest equivalent
        char_vals <- as.character(ds[[actual_col]])
        len <- if (all(is.na(char_vals))) {
          0
        } else {
          max(nchar(char_vals, type = "bytes"), na.rm = TRUE)
        }
      } else {
        type <- "N"
        # SAS numeric VARLEN is always 8 (bytes for double precision)
        len <- 8
      }
    }
    # If col_match is NA, defaults (ind=0, type="", len=-1) remain
  }
  # If ds is NULL or not a data frame, defaults remain

  # Build audit row tibble matching SAS rpt_chk_var column layout
  # SAS columns: chk $12, ds $32, var $36, type $1, len 8., condition $200, ind 8.
  audit_row <- tibble::tibble(
    chk       = "VAR",
    ds        = ds_name,
    var       = var_upper,
    type      = type,
    len       = len,
    condition = "EXISTS",
    ind       = ind
  )

  list(
    ind       = ind,
    type      = type,
    len       = len,
    audit_row = audit_row
  )
}


# =============================================================================
# chk_dm_subj_gt0()
# Migrated from SAS %chk_dm_subj_gt0 (lines 68-80)
# Checks whether the DM dataset has any subjects (rows) and variables (columns).
#
# In SAS, this macro set the global variable DM_SUBJ_GT0 to '1' or '0' via
# CALL SYMPUTX after opening the dataset and checking NOBS > 0 AND NVARS > 0.
# In R, we return a logical TRUE/FALSE and emit a cli alert when DM is empty.
#
# @param dm A data frame (or tibble) representing the DM domain.
# @return Logical: TRUE if DM has subjects (nrow > 0 and ncol > 0), FALSE
#         otherwise. Side effect: emits cli::cli_alert_danger when FALSE.
# =============================================================================
chk_dm_subj_gt0 <- function(dm) {

  # Validate input is a data frame
  if (is.null(dm) || !is.data.frame(dm)) {
    cli::cli_alert_danger("DM is NULL or not a valid data frame")
    return(FALSE)
  }

  # Mirror SAS logic: nobs > 0 and nvars > 0
  has_subjects <- nrow(dm) > 0 && ncol(dm) > 0

  if (!has_subjects) {
    cli::cli_alert_danger("There are no subjects in DM")
  }

  has_subjects
}


# =============================================================================
# chk_val()
# Migrated from SAS %chk_val (lines 100-235)
# Checks whether dataset DS has specified values in variable VAR.
# Supports up to 25 values via ..., case-insensitive matching (default),
# and count vs indicator mode.
#
# In SAS, this macro:
#   - Accepted val1..val25 positional parameters plus cs= and count= keyword args
#   - Built a PROC SQL with a left join to count value occurrences
#   - Set global macro variables DS_VAR_VAL for each value
#   - Accumulated rows into rpt_chk_val via merge
#   - Replaced the MISSING keyword with actual missing (. or '')
#
# In R, we:
#   - Accept values via ... (up to 25)
#   - Use dplyr::filter() for value counting
#   - Return a list with per-value results and audit_rows tibble
#   - Replace MISS constant with NA for filtering, label as "MISSING" in output
#
# @param ds    A data frame (or tibble) to inspect.
# @param var   Character string: name of the variable (column) to check.
# @param ...   Values to look up in the variable. Up to 25 values supported.
# @param cs    Logical: case-sensitive comparison? Default FALSE (case-insensitive).
# @param count Logical: return counts instead of presence indicators? Default FALSE.
# @return A named list with components:
#   \item{results}{A named list of per-value indicators or counts.}
#   \item{audit_rows}{A tibble with one row per value for rpt_chk_val accumulation.}
#   \item{success}{Logical: TRUE if dataset and variable exist, FALSE otherwise.}
# =============================================================================
chk_val <- function(ds, var, ..., cs = FALSE, count = FALSE) {

  # Capture the dataset name for audit trail
  ds_name <- tryCatch(
    toupper(deparse(substitute(ds))),
    error = function(e) "UNKNOWN"
  )

  var_upper <- toupper(var)

  # Collect values from ... into a list (SAS lines 113-118: find max_arg)
  vals <- list(...)
  max_arg <- length(vals)

  # Guard: no values provided
  if (max_arg == 0) {
    cli::cli_alert_warning("No values provided to check for variable {var} in dataset")
    return(list(
      results    = list(),
      audit_rows = tibble::tibble(
        chk = character(0), ds = character(0), var = character(0),
        val = character(0), condition = character(0), ind = numeric(0)
      ),
      success = FALSE
    ))
  }

  # Guard: too many values (SAS supported up to 25)
  if (max_arg > 25) {
    cli::cli_alert_warning("More than 25 values provided; only the first 25 will be used")
    vals <- vals[1:25]
    max_arg <- 25
  }

  # ---- Determine type and length of VAR in DS (SAS lines 122-140) ----
  success <- FALSE
  var_type <- "C"
  var_len <- 200

  if (!is.null(ds) && is.data.frame(ds)) {
    # Case-insensitive column matching (mirrors SAS VARNUM behavior)
    col_match <- match(toupper(var), toupper(colnames(ds)))
    if (!is.na(col_match)) {
      actual_col <- colnames(ds)[col_match]
      success <- TRUE
      if (is.character(ds[[actual_col]]) || is.factor(ds[[actual_col]])) {
        var_type <- "C"
        char_vals <- as.character(ds[[actual_col]])
        var_len <- if (all(is.na(char_vals))) {
          200
        } else {
          max(nchar(char_vals, type = "bytes"), na.rm = TRUE)
        }
      } else {
        var_type <- "N"
        var_len <- 8
      }
    }
  }

  # ---- Replace MISSING keyword with NA (SAS lines 142-145) ----
  # In SAS: if upcase(val) = &miss then replace with missing ('' for C, . for N)
  # In R: replace MISS constant with NA
  processed_vals <- lapply(vals, function(v) {
    if (is.character(v) && toupper(v) == MISS) {
      return(NA)
    }
    v
  })

  # ---- Resolve actual column name for dplyr operations ----
  actual_col <- if (success) {
    colnames(ds)[match(toupper(var), toupper(colnames(ds)))]
  } else {
    var
  }

  # ---- Compute indicator or count for each value (SAS lines 165-208) ----
  condition_label <- if (count) "COUNT" else "PRESENT"

  results_list <- list()
  audit_row_list <- vector("list", max_arg)

  for (i in seq_along(processed_vals)) {
    val_i <- processed_vals[[i]]
    original_val <- vals[[i]]

    if (success) {
      # Count occurrences using dplyr::filter (replaces SAS PROC SQL WHERE clause)
      if (is.na(val_i)) {
        # Filtering for NA/missing values
        cnt <- ds %>%
          dplyr::filter(is.na(!!rlang::sym(actual_col))) %>%
          nrow()
      } else if (var_type == "C" && !cs) {
        # Case-insensitive character comparison (SAS lines 154, 196)
        cnt <- ds %>%
          dplyr::filter(toupper(as.character(!!rlang::sym(actual_col))) ==
                          toupper(as.character(val_i))) %>%
          nrow()
      } else {
        # Exact match (numeric or case-sensitive character)
        cnt <- ds %>%
          dplyr::filter(!!rlang::sym(actual_col) == val_i) %>%
          nrow()
      }

      # Compute indicator or count (SAS lines 178-189)
      ind_val <- if (count) cnt else if (cnt > 0) 1L else 0L

    } else {
      # Dataset or variable does not exist: indicator = -1 (SAS lines 201-206)
      ind_val <- -1L
    }

    # ---- Normalize display value for audit trail (SAS lines 211-216) ----
    # In SAS: if type='C' and val='' OR type='N' and val in ('','.') then val='MISSING'
    display_val <- if (is.na(val_i)) {
      MISS
    } else if (var_type == "C" && is.character(val_i) && nchar(val_i) == 0) {
      MISS
    } else if (var_type == "N" && is.character(val_i) && val_i %in% c("", ".")) {
      MISS
    } else {
      # For character values in case-insensitive mode, store uppercase (SAS line 168)
      if (var_type == "C" && !cs) {
        toupper(as.character(val_i))
      } else {
        as.character(val_i)
      }
    }

    # Build result name matching SAS macro variable naming convention:
    # compress(ds)||'_'||compress(var)||'_'||compress(translate(trim(val),'_',' '),'_','ak')
    safe_val <- gsub("[^A-Za-z0-9_]", "_", display_val)
    result_name <- paste0(
      toupper(deparse(substitute(ds))), "_",
      toupper(var), "_",
      safe_val,
      if (count) "_cnt" else ""
    )
    results_list[[result_name]] <- ind_val

    # Build audit row (SAS lines 220-226 structure)
    audit_row_list[[i]] <- tibble::tibble(
      chk       = "VAL",
      ds        = ds_name,
      var       = var_upper,
      val       = display_val,
      condition = condition_label,
      ind       = as.numeric(ind_val)
    )
  }

  # Combine all audit rows (SAS lines 220-226: merge into rpt_chk_val)
  audit_rows <- dplyr::bind_rows(audit_row_list)

  # ---- Error logging (SAS lines 230-233) ----
  if (!success) {
    cli::cli_alert_danger("Dataset {ds_name} or variable {var} does not exist")
  }

  list(
    results    = results_list,
    audit_rows = audit_rows,
    success    = success
  )
}


# =============================================================================
# chk_cmp()
# Migrated from SAS %chk_cmp (lines 242-310)
# Determines which values of VAR1 in DS1 are not in VAR2 of DS2 and vice versa.
# Returns a tibble of asymmetric values (values present in only one dataset).
#
# In SAS, this macro:
#   - Opened both datasets and checked variable types matched
#   - Extracted distinct values from each dataset via PROC SQL
#   - Performed a full outer join to find asymmetric rows
#   - Created rpt_cmp_&ds1._&ds2. output table
#
# In R, we:
#   - Check both variables exist and have matching types
#   - Use dplyr::distinct() to extract unique values
#   - Use dplyr::full_join() for cross-dataset comparison
#   - Use dplyr::filter() to identify asymmetric rows
#   - Return a comparison tibble
#
# @param ds1  A data frame (or tibble): first dataset for comparison.
# @param var1 Character string: variable name in ds1 to compare.
# @param ds2  A data frame (or tibble): second dataset for comparison.
# @param var2 Character string: variable name in ds2 to compare.
# @return A tibble with columns: ds1 (name), ds2 (name), in_ds (which dataset
#         has the value), var (the asymmetric value). Returns an empty tibble
#         with the same structure if an error occurs.
# =============================================================================
chk_cmp <- function(ds1, var1, ds2, var2) {

  # Capture dataset names for output labeling
  ds1_name <- tryCatch(
    toupper(deparse(substitute(ds1))),
    error = function(e) "DS1"
  )
  ds2_name <- tryCatch(
    toupper(deparse(substitute(ds2))),
    error = function(e) "DS2"
  )

  # ---- Empty result template (used on error) ----
  empty_result <- tibble::tibble(
    ds1   = character(0),
    ds2   = character(0),
    in_ds = character(0),
    var   = character(0)
  )

  # ---- Determine data types (SAS lines 258-276) ----
  # Check ds1/var1
  type1 <- ""
  if (!is.null(ds1) && is.data.frame(ds1)) {
    col_match1 <- match(toupper(var1), toupper(colnames(ds1)))
    if (!is.na(col_match1)) {
      actual_col1 <- colnames(ds1)[col_match1]
      type1 <- if (is.character(ds1[[actual_col1]]) ||
                    is.factor(ds1[[actual_col1]])) "C" else "N"
    }
  }

  # Check ds2/var2
  type2 <- ""
  if (!is.null(ds2) && is.data.frame(ds2)) {
    col_match2 <- match(toupper(var2), toupper(colnames(ds2)))
    if (!is.na(col_match2)) {
      actual_col2 <- colnames(ds2)[col_match2]
      type2 <- if (is.character(ds2[[actual_col2]]) ||
                    is.factor(ds2[[actual_col2]])) "C" else "N"
    }
  }

  # Determine success: both exist and types match (SAS line 274)
  success <- nchar(type1) > 0 && nchar(type2) > 0 && type1 == type2

  # ---- If not success, log error and return empty (SAS lines 304-308) ----
  if (!success) {
    cli::cli_alert_danger(
      paste0("One or more of dataset ", ds1_name, " or ", ds2_name,
             " or variable ", var1, " or ", var2, " does not exist")
    )
    if (nchar(type1) > 0 && nchar(type2) > 0 && type1 != type2) {
      cli::cli_alert_danger(
        paste0("Variables ", var1, " and ", var2, " are not of the same type")
      )
    }
    return(empty_result)
  }

  # ---- Extract distinct values from each dataset (SAS lines 280-289) ----
  actual_col1 <- colnames(ds1)[match(toupper(var1), toupper(colnames(ds1)))]
  actual_col2 <- colnames(ds2)[match(toupper(var2), toupper(colnames(ds2)))]

  # Distinct values from ds1 with a flag column
  vals_ds1 <- ds1 %>%
    dplyr::transmute(join_val = !!rlang::sym(actual_col1)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ds1_flag = 1L)

  # Distinct values from ds2 with a flag column
  vals_ds2 <- ds2 %>%
    dplyr::transmute(join_val = !!rlang::sym(actual_col2)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ds2_flag = 1L)

  # ---- Full outer join (SAS lines 291-299) ----
  joined <- dplyr::full_join(
    vals_ds1, vals_ds2,
    by = "join_val"
  )

  # ---- Filter to asymmetric rows (SAS line 299: where not (ds1 and ds2)) ----
  asymmetric <- joined %>%
    dplyr::filter(is.na(.data$ds1_flag) | is.na(.data$ds2_flag))

  # ---- Build result tibble (SAS lines 291-299 output columns) ----
  if (nrow(asymmetric) == 0) {
    return(empty_result)
  }

  result <- asymmetric %>%
    dplyr::mutate(
      ds1   = ds1_name,
      ds2   = ds2_name,
      in_ds = dplyr::case_when(
        !is.na(.data$ds1_flag) ~ ds1_name,
        !is.na(.data$ds2_flag) ~ ds2_name,
        TRUE ~ NA_character_
      ),
      var   = as.character(.data$join_val)
    ) %>%
    dplyr::transmute(
      ds1   = .data$ds1,
      ds2   = .data$ds2,
      in_ds = .data$in_ds,
      var   = .data$var
    )

  result
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS DSID/OPEN/CLOSE/VARNUM/VARTYPE/VARLEN functions replaced by R introspection
####      (colnames(), is.character(), is.numeric(), nchar(), nrow(), ncol())
####    - SAS global macro variables for check indicators -> R return values in lists
####    - rpt_chk_var/rpt_chk_val accumulation via SAS merge -> caller uses bind_rows()
####    - SAS case sensitivity behavior: default case-insensitive via toupper()
####    - SAS SYMPUTX global scope -> R function return values (named list elements)
####    - Variable name matching is case-insensitive (matching SAS VARNUM behavior)
####    - Factor columns treated as character type ("C") for type classification
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- these are validation checks, not numerical computations
####    - Character length computation may differ (SAS fixed-width VARLEN vs R
####      variable-width max(nchar())) -- SAS returns allocated storage, R returns
####      maximum observed string length
####    - SAS numeric VARLEN always returns 8 (bytes); R returns 8 for consistency
#### NO DIRECT R EQUIVALENT:
####    - SAS DSID/OPEN/CLOSE dataset introspection -> R colnames(), is.character(), nrow()
####    - SAS global macro variables set by CALL SYMPUTX -> R function return values
####    - SAS macro language %do %while for variable iteration -> R for/lapply loops
####    - SAS PROC SQL with dual table fallback -> R dplyr operations with tryCatch
####    - SAS rpt_chk_var merge accumulation -> caller accumulates via bind_rows()
####    - SAS lib parameter for library.dataset notation -> R passes data frames directly
#### PACKAGE SELECTION RATIONALE:
####    - dplyr: Data manipulation replacing PROC SQL joins and DATA step logic (AAP mandate)
####    - tibble: Structured return values replacing SAS output datasets (AAP mandate)
####    - cli: Error/warning messages with formatting replacing SAS %put ERROR: / %put WARNING:
####    - rlang: Tidy evaluation for programmatic column access (sym(), !!) replacing
####      SAS macro variable resolution (&var.)
#### OPEN QUESTIONS:
####    - SAS VARLEN for numeric variables returns 8 (bytes); R equivalent is less meaningful
####      but we return 8 for consistency
####    - chk_val MISSING keyword handling across different locale settings may need review
####    - Whether tryCatch is sufficient to replicate SAS error continuation behavior
####    - Factor levels vs character values: current implementation treats factors as "C" type
####    - chk_cmp full outer join behavior with NA values may differ from SAS handling
####      of missing values in PROC SQL joins
# ============================================================
