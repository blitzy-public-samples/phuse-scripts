# ==========================================================================
# data_checks.R — Data Validation Utility Functions
# ==========================================================================
# Migrated from: tested/SAS/ZZ_Utilities/data_checks.sas (311 lines)
#
# Purpose:
#   Centralizes all data validation utilities called by ae_setup and other
#   domain panel drivers. Each SAS macro is migrated to a parameterized R
#   function that returns structured tibble audit results instead of creating
#   SAS global macro variable side effects.
#
# Exported Functions:
#   chk_var()          — Check variable existence in a data frame
#   chk_dm_subj_gt0()  — Check DM dataset has subjects (rows > 0, cols > 0)
#   chk_val()          — Check value presence or counts in a variable
#   chk_cmp()          — Compare distinct values across two datasets
#
# External Dependencies:
#   tibble (>= 3.2.0)  — Structured tibble return values
#   dplyr  (>= 1.1.0)  — Data manipulation (full_join, filter, mutate,
#                         case_when, transmute) in chk_cmp pipeline
#   purrr  (>= 1.0.0)  — Functional iteration (map_dfr) in chk_val
#   cli    (>= 3.6.0)  — User-facing error/warning messages
#
# Accumulation Pattern (for callers):
#   SAS accumulates rpt_chk_var via dataset merge. In R, callers must
#   accumulate results using dplyr::bind_rows():
#
#     rpt_chk_var <- dplyr::bind_rows(
#       chk_var(ae, "aebodsys", ds_name = "ae"),
#       chk_var(ae, "aedecod",  ds_name = "ae"),
#       chk_var(dm, "usubjid",  ds_name = "dm")
#     )
#
# SAS Global Variable Equivalent:
#   SAS: %let miss = MISSING;
#   R:   miss_keyword parameter with default "MISSING" in chk_val()
# ==========================================================================


# ==========================================================================
# chk_var: Check variable existence in a data frame
# ==========================================================================
# Migrated from: %chk_var(lib=work, ds=, var=)  [SAS lines 13-59]
#
# SAS behavior:
#   - Opens dataset via open(), checks VARNUM > 0 for existence
#   - Gets VARTYPE (N/C) and VARLEN (declared length)
#   - Creates global macro variable &ds._&var. = 1 if exists, 0 if not
#   - Creates &ds._&var._type and &ds._&var._len globals
#   - Inserts audit row into cumulative rpt_chk_var dataset via merge
#   - Cleans up temp dataset via PROC DATASETS
#
# R equivalent:
#   - Checks var %in% colnames(data)
#   - Maps R class to SAS type code: numeric/logical -> "N", all else -> "C"
#   - Approximates SAS VARLEN: numeric -> 8, character -> max(nchar())
#   - Returns single-row tibble (caller accumulates via bind_rows)
#   - No global state; caller extracts indicator via result$ind
#
# @param data  A data frame or tibble to inspect.
# @param var   Character string: the variable (column) name to check.
# @param ds_name Character string: dataset label for the audit trail.
#   Defaults to the deparsed name of the \code{data} argument.
# @return A single-row tibble with columns:
#   \describe{
#     \item{chk}{Character "VAR" — check type identifier}
#     \item{ds}{Character — uppercased dataset name}
#     \item{var}{Character — uppercased variable name}
#     \item{type}{Character — "N" (numeric) or "C" (character), "" if missing}
#     \item{len}{Integer — 8 for numeric, max nchar for character, -1 if missing}
#     \item{condition}{Character "EXISTS" — the condition being tested}
#     \item{ind}{Integer — 1 if variable exists, 0 otherwise}
#   }
# --------------------------------------------------------------------------
chk_var <- function(data, var, ds_name = deparse(substitute(data))) {

  # --- Input validation ---
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, not {.cls {class(data)}}."
    )
  }
  if (!is.character(var) || length(var) != 1L) {
    cli::cli_abort("{.arg var} must be a single character string.")
  }

  ds_upper  <- toupper(as.character(ds_name))
  var_upper <- toupper(var)

  # --- Check existence ---
  var_exists <- var %in% colnames(data)

  if (var_exists) {
    col <- data[[var]]

    # Map R class to SAS-style type code
    # SAS VARTYPE returns "N" for numeric, "C" for character
    # In R: numeric, integer, logical -> "N"; character, factor, Date, etc. -> "C"
    r_type <- if (is.numeric(col) || is.logical(col)) "N" else "C"

    # Approximate SAS variable length
    # SAS VARLEN returns declared storage length:
    #   numeric always 8 (8-byte double), character = declared $length
    # R approximation: numeric = 8, character = max observed nchar
    if (r_type == "N") {
      r_len <- 8L
    } else {
      char_vals   <- as.character(col)
      nchar_vals  <- nchar(char_vals, allowNA = TRUE)
      # Remove NAs from nchar calculation (NA values have no length)
      non_na_lens <- nchar_vals[!is.na(nchar_vals)]
      if (length(non_na_lens) == 0L) {
        # All values are NA or column is empty — report length 0
        r_len <- 0L
      } else {
        r_len <- as.integer(max(non_na_lens))
      }
    }
  } else {
    # Variable does not exist — SAS returns type='', len=-1
    r_type <- ""
    r_len  <- -1L
  }

  # --- Build audit tibble row ---
  tibble::tibble(
    chk       = "VAR",
    ds        = ds_upper,
    var       = var_upper,
    type      = r_type,
    len       = r_len,
    condition = "EXISTS",
    ind       = as.integer(var_exists)
  )
}


# ==========================================================================
# chk_dm_subj_gt0: Check DM dataset has subjects
# ==========================================================================
# Migrated from: %chk_dm_subj_gt0  [SAS lines 68-80]
#
# SAS behavior:
#   - Opens DM dataset via open()
#   - Gets nobs = attrn(dsid, 'nobs') and nvars = attrn(dsid, 'nvars')
#   - Sets global macro dm_subj_gt0 = 1 if nobs>0 and nvars>0, else 0
#
# R equivalent:
#   - Checks nrow(dm) > 0 and ncol(dm) > 0
#   - Returns logical TRUE/FALSE (callers can coerce to 1/0 via as.integer)
#   - Handles NULL and non-data-frame inputs gracefully
#
# @param dm  A data frame representing the DM (Demographics) domain.
# @return Logical: TRUE if dm has at least one row and one column,
#   FALSE if dm is NULL, not a data frame, or empty.
# --------------------------------------------------------------------------
chk_dm_subj_gt0 <- function(dm) {

  # Handle NULL or non-data-frame gracefully (SAS returns 0 if dataset missing)
  if (is.null(dm) || !is.data.frame(dm)) {
    return(FALSE)
  }

  # SAS: ifc(nobs>0 and nvars>0, '1', '0')
  nrow(dm) > 0L && ncol(dm) > 0L
}


# ==========================================================================
# chk_val: Check value presence or counts in a variable
# ==========================================================================
# Migrated from: %chk_val(lib, ds, var, val1..val25, cs=F, count=F)
#   [SAS lines 100-235]
#
# SAS behavior:
#   - Accepts up to 25 positional value parameters (val1..val25)
#   - Opens dataset, gets VARTYPE and VARLEN for the target variable
#   - For each value: checks if it exists in the variable
#   - Supports MISSING keyword: if value == %let miss, checks for actual NA
#   - Supports case-insensitive matching (cs=F default, uppercases both sides)
#   - Supports count mode (count=T returns match count instead of 1/0)
#   - Creates global macro variables per value (e.g., DS_VAR_VAL = 1)
#   - Accumulates audit rows into rpt_chk_val via merge
#   - Returns ind = -1 if dataset or variable does not exist
#
# R equivalent:
#   - Values parameter is an unlimited-length vector (not limited to 25)
#   - Uses purrr::map_dfr() for functional iteration over values (AAP-mandated)
#   - Returns tibble with one row per value checked
#   - No global state; caller extracts indicators from result tibble
#   - MISSING keyword: default "MISSING" matches SAS %let miss = MISSING
#
# @param data Data frame to inspect.
# @param var Character string: variable name to check.
# @param values Character or numeric vector of values to look for.
# @param cs Logical: case-sensitive matching. Default FALSE (SAS default cs=F).
#   When FALSE, character comparisons are uppercased on both sides.
# @param count Logical: return counts instead of presence indicators.
#   Default FALSE (SAS default count=F).
# @param ds_name Character string: dataset label for audit trail.
# @param miss_keyword Character string: keyword that triggers NA checking.
#   Default "MISSING" (matches SAS %let miss = MISSING).
# @return A tibble with columns:
#   \describe{
#     \item{chk}{Character "VAL" — check type identifier}
#     \item{ds}{Character — uppercased dataset name}
#     \item{var}{Character — uppercased variable name}
#     \item{val}{Character — the value being checked (or "MISSING" for NAs)}
#     \item{condition}{Character — "PRESENT" or "COUNT"}
#     \item{ind}{Integer — 1/0 for presence, count for count mode, -1 if error}
#   }
# --------------------------------------------------------------------------
chk_val <- function(data, var, values, cs = FALSE, count = FALSE,
                    ds_name = deparse(substitute(data)),
                    miss_keyword = "MISSING") {

  # --- Input validation ---
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, not {.cls {class(data)}}."
    )
  }
  if (!is.character(var) || length(var) != 1L) {
    cli::cli_abort("{.arg var} must be a single character string.")
  }
  if (length(values) == 0L) {
    cli::cli_abort("{.arg values} must contain at least one value to check.")
  }

  ds_upper   <- toupper(as.character(ds_name))
  var_upper  <- toupper(var)
  cond_label <- if (count) "COUNT" else "PRESENT"

  # --- Check variable existence ---
  # SAS: if variable/dataset doesn't exist, all indicators = -1
  if (!(var %in% colnames(data))) {
    cli::cli_warn(
      "Dataset {.val {ds_name}} does not contain variable {.val {var}}."
    )
    # Return -1 indicators for all requested values (SAS failure code)
    return(
      tibble::tibble(
        chk       = rep("VAL", length(values)),
        ds        = rep(ds_upper, length(values)),
        var       = rep(var_upper, length(values)),
        val       = purrr::map_chr(values, function(v) {
          if (is.na(v)) "MISSING" else as.character(v)
        }),
        condition = rep(cond_label, length(values)),
        ind       = rep(-1L, length(values))
      )
    )
  }

  col     <- data[[var]]
  is_char <- is.character(col) || is.factor(col)

  # --- Process each value using purrr::map_dfr ---
  # Replaces SAS %do loop over val1..val25 positional parameters
  purrr::map_dfr(values, function(v) {

    # Determine if this value represents a missing/NA check
    check_na    <- FALSE
    val_display <- NA_character_

    if (is.na(v)) {
      # Actual R NA passed directly — treat as missing value check
      check_na    <- TRUE
      val_display <- "MISSING"
    } else {
      v_char <- as.character(v)
      if (nchar(v_char) > 0L && toupper(v_char) == toupper(miss_keyword)) {
        # Value matches the MISSING keyword — check for NA
        # SAS: %if %upcase(&val) = &miss. %then %let val = <actual missing>;
        check_na    <- TRUE
        val_display <- "MISSING"
      } else {
        val_display <- v_char
      }
    }

    # Count matching rows
    if (check_na) {
      # Count NA values in the column
      matches <- sum(is.na(col))
    } else if (is_char) {
      # Character/factor comparison
      col_char <- as.character(col)
      v_str    <- as.character(v)
      if (!cs) {
        # Case-insensitive: upcase both sides (SAS default behavior)
        matches <- sum(toupper(col_char) == toupper(v_str), na.rm = TRUE)
      } else {
        # Case-sensitive comparison
        matches <- sum(col_char == v_str, na.rm = TRUE)
      }
    } else {
      # Numeric comparison
      v_num <- suppressWarnings(as.numeric(v))
      if (is.na(v_num)) {
        # Value could not be coerced to numeric — zero matches
        matches <- 0L
      } else {
        matches <- sum(col == v_num, na.rm = TRUE)
      }
    }

    # SAS post-processing: replace blank/dot val with "MISSING"
    # SAS lines 211-216: if type=C and val='', or type=N and val in ('','.'),
    #   then val = 'MISSING'
    if (is.na(val_display) || val_display == "" || val_display == ".") {
      val_display <- "MISSING"
    }

    tibble::tibble(
      chk       = "VAL",
      ds        = ds_upper,
      var       = var_upper,
      val       = val_display,
      condition = cond_label,
      ind       = if (count) as.integer(matches) else as.integer(matches > 0L)
    )
  })
}


# ==========================================================================
# chk_cmp: Compare distinct values across two datasets
# ==========================================================================
# Migrated from: %chk_cmp(lib=, ds1=, var1=, ds2=, var2=)  [SAS lines 242-310]
#
# SAS behavior:
#   - Determines data types of var1 and var2 via VARTYPE
#   - If types differ or variables missing, sets success=0 and exits
#   - Gets distinct values from each dataset via PROC SQL
#   - Full outer joins on the values
#   - Keeps only unmatched rows (WHERE NOT (ds1 AND ds2))
#   - Creates rpt_cmp_DS1_DS2 table with orphan values
#   - Prints error if validation fails
#
# R equivalent:
#   - Validates variable existence and type compatibility
#   - Gets unique() values from each dataset
#   - dplyr::full_join() + filter for unmatched rows
#   - Returns tibble with orphan values and their source dataset
#   - Uses cli::cli_abort() for hard errors (matching SAS %put ERROR)
#
# @param data1 Data frame 1.
# @param var1 Character string: variable name in data1.
# @param data2 Data frame 2.
# @param var2 Character string: variable name in data2.
# @param ds1_name Character string: label for data1 in output.
# @param ds2_name Character string: label for data2 in output.
# @return A tibble with columns:
#   \describe{
#     \item{ds1}{Character — name of first dataset}
#     \item{ds2}{Character — name of second dataset}
#     \item{in_dataset}{Character — which dataset contains the orphan value}
#     \item{var}{The orphan value itself}
#   }
#   Returns zero-row tibble if all distinct values match between datasets.
# --------------------------------------------------------------------------
chk_cmp <- function(data1, var1, data2, var2,
                    ds1_name = deparse(substitute(data1)),
                    ds2_name = deparse(substitute(data2))) {

  # --- Input validation ---
  if (!is.data.frame(data1)) {
    cli::cli_abort(
      "{.arg data1} must be a data frame, not {.cls {class(data1)}}."
    )
  }
  if (!is.data.frame(data2)) {
    cli::cli_abort(
      "{.arg data2} must be a data frame, not {.cls {class(data2)}}."
    )
  }
  if (!is.character(var1) || length(var1) != 1L) {
    cli::cli_abort("{.arg var1} must be a single character string.")
  }
  if (!is.character(var2) || length(var2) != 1L) {
    cli::cli_abort("{.arg var2} must be a single character string.")
  }

  # --- Validate variable existence ---
  # SAS: if dsid then ... varnum(dsid, var) ...
  if (!(var1 %in% colnames(data1))) {
    cli::cli_abort(
      "Variable {.val {var1}} does not exist in dataset {.val {ds1_name}}."
    )
  }
  if (!(var2 %in% colnames(data2))) {
    cli::cli_abort(
      "Variable {.val {var2}} does not exist in dataset {.val {ds2_name}}."
    )
  }

  # --- Validate same type ---
  # SAS: type1 = vartype(dsid, varnum(dsid, var1))
  # SAS: if type1 ne '' and type2 ne '' and type1 = type2 then success=1
  col1  <- data1[[var1]]
  col2  <- data2[[var2]]
  type1 <- if (is.numeric(col1) || is.logical(col1)) "N" else "C"
  type2 <- if (is.numeric(col2) || is.logical(col2)) "N" else "C"

  if (type1 != type2) {
    cli::cli_abort(
      c(
        paste0("Variables {.val {var1}} (type ", type1,
               ") and {.val {var2}} (type ", type2,
               ") are not of the same type."),
        "i" = "Both variables must be the same type for comparison."
      )
    )
  }

  # --- Get distinct values ---
  # SAS: SELECT DISTINCT var1, 1 AS ds1 FROM lib.ds1
  vals1 <- tibble::tibble(val = unique(data1[[var1]]), .ds1_flag = TRUE)
  vals2 <- tibble::tibble(val = unique(data2[[var2]]), .ds2_flag = TRUE)

  # --- Full join and identify mismatches ---
  # SAS: FULL JOIN ... WHERE NOT (ds1 AND ds2)
  # Keeps only rows present in one dataset but not the other
  result <- dplyr::full_join(vals1, vals2, by = "val") %>%
    dplyr::filter(is.na(.ds1_flag) | is.na(.ds2_flag)) %>%
    dplyr::mutate(
      in_dataset = dplyr::case_when(
        !is.na(.ds1_flag) & is.na(.ds2_flag) ~ as.character(ds1_name),
        is.na(.ds1_flag) & !is.na(.ds2_flag) ~ as.character(ds2_name),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::transmute(
      ds1        = as.character(ds1_name),
      ds2        = as.character(ds2_name),
      in_dataset = in_dataset,
      var        = val
    )

  result
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global macro variable side effects replaced by return tibbles.
#      Callers accumulate rpt_chk_var via dplyr::bind_rows() of chk_var()
#      results. Same pattern applies for chk_val() accumulation.
#    - SAS VARLEN approximated by max(nchar()) for character variables.
#      SAS uses declared storage length; R has no fixed column length.
#    - SAS type mapping: numeric/integer/logical -> "N",
#      character/factor/Date/POSIXct -> "C".
#    - MISSING keyword default "MISSING" matches SAS %let miss = MISSING.
#    - SAS val1..val25 positional limit removed; R accepts unlimited vector.
#    - SAS PROC DATASETS cleanup not needed; R garbage collection handles it.
#    - chk_dm_subj_gt0 returns logical (TRUE/FALSE) rather than integer (1/0).
#      Callers needing integer can use as.integer() on the result.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Variable length calculation: SAS uses declared $length (e.g., $200),
#      R uses max observed nchar(). Results will differ when declared length
#      exceeds actual max string length in data.
#    - Count-based chk_val: SAS counts only non-missing matches via GROUP BY;
#      R sum(..., na.rm=TRUE) is equivalent, but rounding or type coercion
#      edge cases may produce different counts for haven-labelled values.
#    - SAS case-insensitive comparison uses UPCASE(); R uses toupper().
#      These are equivalent for ASCII but may differ for locale-specific
#      characters (e.g., Turkish dotted-I).
#
# NO DIRECT R EQUIVALENT:
#    - SAS open()/close()/varnum()/vartype()/varlen() dataset functions
#      -> colnames(), class(), nchar() approximations.
#    - SAS global macro variables (&ds._&var., &ds._&var._type, etc.)
#      -> Function return tibbles. Callers extract via result$ind.
#    - SAS PROC DATASETS cleanup -> R garbage collection (automatic).
#    - SAS SYMPUTX for creating macro variables at run time
#      -> Not needed; return values carry all information.
#
# PACKAGE SELECTION RATIONALE:
#    - tibble: Structured return values with consistent column types.
#      AAP mandates tibble over base data.frame for clinical data.
#    - dplyr: Core data manipulation for chk_cmp full_join pipeline.
#      AAP mandates dplyr over base merge/match.
#    - purrr: Functional iteration via map_dfr() in chk_val.
#      AAP mandates purrr over base lapply for tidyverse consistency.
#    - cli: User-facing error (cli_abort) and warning (cli_warn) messages
#      with structured formatting for variable names and dataset labels.
#
# OPEN QUESTIONS:
#    - Should chk_var return type as SAS-style "N"/"C" or full R class name?
#      Current: SAS-style for backward compatibility with downstream checks.
#    - Variable length: is exact SAS declared length needed, or is
#      max(nchar()) sufficient for R-side audit trails?
#    - Should chk_val support haven::tagged_na() for SAS special missing
#      values (.A through .Z)? Current: treats all NA uniformly.
#    - Should chk_cmp handle NA values in the comparison? Current: NA is
#      treated as a distinct value that can appear in the mismatch report.
# ============================================================
