# =============================================================================
# data_checks.R
# Migrated from: contributed/AE/ZZ_Utilities/data_checks.sas (310 lines)
#
# Foundational data validation primitives for the PhUSE CS Standard Analyses
# Working Group (WG5) clinical reporting pipeline. Provides four validation
# functions (chk_var, chk_dm_subj_gt0, chk_val, chk_cmp) consumed by
# ae_setup.R and other upstream modules.
#
# SAS macros are replaced by parameterized R functions. SAS DSID/OPEN/VARNUM
# metadata access is replaced by idiomatic R data frame introspection.
# SAS call symputx global macro variables are replaced by named list returns.
# SAS positional parameters (val1..val25) are replaced by R vector arguments.
#
# NO source() dependencies — this is the foundational validation module.
# =============================================================================

# -- Required Libraries --------------------------------------------------------
library(dplyr)
library(cli)

# -- Global Constant -----------------------------------------------------------
# SAS line 1: %let miss = MISSING;
# Sentinel value used in chk_val() to indicate that the caller wants to check
# for missing (NA) values. When a value in the `values` vector equals MISS
# (case-insensitive), it is replaced with NA before matching.
MISS <- "MISSING"


# ==============================================================================
# chk_var — Check whether a variable exists in a dataset
# Migrated from SAS %chk_var macro (lines 13-59)
#
# SAS behavior:
#   Opens dataset via DSID, checks varnum > 0, retrieves vartype and varlen,
#   publishes ind/type/len via call symputx, and appends an audit row to
#   rpt_chk_var.
#
# R equivalent:
#   Inspects data frame column names, determines type via is.numeric(),
#   estimates length, and returns a named list with audit tibble row.
# ==============================================================================
chk_var <- function(ds, var, ds_name = deparse(substitute(ds))) {


  # -- Argument validation ------------------------------------------------------
  if (!is.character(var) || length(var) != 1L) {
    cli::cli_abort("Argument {.arg var} must be a single character string.")
  }

  # -- Validate inputs ---------------------------------------------------------
  # SAS lines 24-43: open dataset, check varnum, handle failure
  if (!is.data.frame(ds)) {
    # Dataset cannot be opened (SAS lines 38-43)
    ind  <- 0L
    type <- NA_character_
    len  <- -1L

    audit_row <- dplyr::tibble(
      chk       = "VAR",
      ds        = toupper(ds_name),
      var       = toupper(var),
      type      = type,
      len       = len,
      condition = "EXISTS",
      ind       = ind
    )

    return(list(ind = ind, type = type, len = len, audit_row = audit_row))
  }

  # -- Check variable existence ------------------------------------------------
  # SAS line 26: ind = ifn(varnum(dsid,upcase("&var."))>0,1,0)
  if (var %in% names(ds)) {
    ind <- 1L

    # SAS line 28: type = vartype(dsid, varnum(dsid, upcase("&var.")))
    # "N" for numeric, "C" for character
    type <- if (is.numeric(ds[[var]])) "N" else "C"

    # SAS line 29: len = varlen(dsid, varnum(dsid, upcase("&var.")))
    # SAS numeric varlen is always 8; character varlen is declared byte length.
    # In R, numeric storage is always 8-byte double. For character, use the
    # maximum observed byte length (or 0 if all NA).
    if (type == "N") {
      len <- 8L
    } else {
      raw_lengths <- nchar(ds[[var]], type = "bytes", allowNA = TRUE)
      len <- if (all(is.na(raw_lengths))) 0L else max(raw_lengths, na.rm = TRUE)
    }
  } else {
    # SAS lines 31-33: variable does not exist
    ind  <- 0L
    type <- NA_character_
    len  <- -1L
  }

  # -- Build audit tibble row --------------------------------------------------
  # SAS lines 17-22: data step creating chk_var_{ds}_{var} observation
  audit_row <- dplyr::tibble(
    chk       = "VAR",
    ds        = toupper(ds_name),
    var       = toupper(var),
    type      = type,
    len       = as.numeric(len),
    condition = "EXISTS",
    ind       = as.numeric(ind)
  )

  # -- Return named list -------------------------------------------------------
  # SAS lines 47-49: call symputx("&ds._&var.", ind, 'g') etc.
  # In R, caller captures these via the returned list.
  list(
    ind       = ind,
    type      = type,
    len       = len,
    audit_row = audit_row
  )
}


# ==============================================================================
# chk_dm_subj_gt0 — Check whether the DM dataset has any subjects
# Migrated from SAS %chk_dm_subj_gt0 macro (lines 68-80)
#
# SAS behavior:
#   Opens the 'dm' dataset, checks nobs > 0 and nvars > 0, publishes
#   dm_subj_gt0 macro variable as '1' or '0'.
#
# R equivalent:
#   Checks nrow(dm) > 0 and ncol(dm) > 0, returns logical TRUE/FALSE.
# ==============================================================================
chk_dm_subj_gt0 <- function(dm) {

  # Guard: if dm is not a valid data frame, return FALSE

  if (!is.data.frame(dm)) {
    return(FALSE)
  }

  # SAS lines 73-76: nobs = attrn(dsid,'nobs'); nvars = attrn(dsid,'nvars')
  # ifc(nobs>0 and nvars>0, '1', '0')
  nrow(dm) > 0L && ncol(dm) > 0L
}


# ==============================================================================
# chk_val — Check whether a dataset has specific values in a variable
# Migrated from SAS %chk_val macro (lines 100-235)
#
# SAS behavior:
#   Accepts up to 25 positional value parameters (val1..val25), opens dataset,
#   counts occurrences via PROC SQL, builds indicator macro variables via
#   call symputx, and accumulates audit rows in rpt_chk_val.
#
# R equivalent:
#   Accepts a single `values` vector (no 25-item limit). Uses dplyr for
#   counting and joining. Returns a named list with audit tibble and
#   per-value indicators.
#
# Parameters:
#   ds       — data frame to validate
#   var      — character string: variable name to check
#   values   — character or numeric vector of values to look up
#   ds_name  — character string: display name for audit trail
#   cs       — logical: case-sensitive matching (default FALSE = case-insensitive)
#   count    — logical: return actual counts instead of 1/0 indicators (default FALSE)
#
# Returns:
#   Named list with:
#     $audit_rows       — tibble of audit records (one per value)
#     $value_indicators — named list of indicator/count values
# ==============================================================================
chk_val <- function(ds,
                    var,
                    values,
                    ds_name = deparse(substitute(ds)),
                    cs      = FALSE,
                    count   = FALSE) {

  # -- Ensure MISS constant is available (SAS line 108) ------------------------
  miss_sentinel <- MISS

  # -- Determine variable type and existence (SAS lines 122-140) ---------------
  success <- FALSE
  type    <- "C"
  len     <- 200L

  if (is.data.frame(ds) && var %in% names(ds)) {
    success <- TRUE
    type    <- if (is.numeric(ds[[var]])) "N" else "C"
    if (type == "N") {
      len <- 8L
    } else {
      raw_lengths <- nchar(ds[[var]], type = "bytes", allowNA = TRUE)
      len <- if (all(is.na(raw_lengths))) 200L else max(raw_lengths, na.rm = TRUE)
    }
  }

  # -- Handle MISS keyword (SAS lines 142-145) ---------------------------------
  # If any value matches the MISS sentinel (case-insensitive), replace with NA.
  # For numeric variables, NA is NA_real_; for character, NA_character_.
  values_proc <- values
  miss_mask <- !is.na(values_proc) & toupper(as.character(values_proc)) == toupper(miss_sentinel)
  if (any(miss_mask)) {
    if (type == "C") {
      values_proc[miss_mask] <- NA_character_
    } else {
      values_proc[miss_mask] <- NA_real_
    }
  }

  # Coerce values to match variable type
  if (type == "N") {
    values_proc <- suppressWarnings(as.numeric(values_proc))
  } else {
    values_proc <- as.character(values_proc)
  }

  # -- Case sensitivity handling (SAS lines 154, 168) --------------------------
  # If not case-sensitive, uppercase both values and column data for matching
  if (type == "C" && !cs) {
    values_compare <- toupper(ifelse(is.na(values_proc), NA_character_, values_proc))
  } else {
    values_compare <- values_proc
  }

  # -- Build lookup tibble (SAS lines 165-170) ---------------------------------
  lookup <- dplyr::tibble(
    lookup_val = values_compare,
    orig_val   = values_proc
  )

  # -- Count matching values (SAS lines 172-208) -------------------------------
  if (success) {
    # Extract column data
    col_data <- ds[[var]]

    # Apply case transformation if needed
    if (type == "C" && !cs) {
      col_data <- toupper(as.character(col_data))
    }

    # Build a tibble with the column values for counting
    col_tbl <- dplyr::tibble(val = col_data)

    # Count occurrences of each distinct value present in the column
    # that is also in our lookup list (including NA)
    counts_tbl <- col_tbl %>%
      dplyr::filter(val %in% values_compare | (is.na(val) & any(is.na(values_compare)))) %>%
      dplyr::count(val, name = "n_count")

    # Left join lookup with counts
    result <- lookup %>%
      dplyr::left_join(counts_tbl, by = c("lookup_val" = "val"))

    # Replace NA count with 0 (value not found)
    result <- result %>%
      dplyr::mutate(n_count = dplyr::if_else(is.na(n_count), 0L, as.integer(n_count))) # legitimate: count initialized to zero after aggregate

    # Compute indicator (SAS lines 178-189)
    if (!count) {
      # PRESENT mode: ind = 1 if found, 0 if not
      result <- result %>%
        dplyr::mutate(ind = dplyr::if_else(n_count > 0L, 1L, 0L))
    } else {
      # COUNT mode: ind = actual count
      result <- result %>%
        dplyr::mutate(ind = n_count)
    }
  } else {
    # Dataset or variable does not exist → all indicators = -1
    result <- lookup %>%
      dplyr::mutate(n_count = 0L, ind = -1L)
  }

  # -- Build display value for missing (SAS lines 211-216) ---------------------
  # Replace empty/NA values with "MISSING" in the output display.
  # SAS line 168: when cs != T, the lookup value is uppercased. The display
  # value should reflect this (lookup_val already has correct case applied).
  result <- result %>%
    dplyr::mutate(
      display_val = dplyr::case_when(
        type == "C" & is.na(lookup_val)                      ~ "MISSING",
        type == "C" & as.character(lookup_val) == ""          ~ "MISSING",
        type == "N" & is.na(lookup_val)                      ~ "MISSING",
        type == "N" & as.character(lookup_val) == ""          ~ "MISSING",
        type == "N" & as.character(lookup_val) == "."         ~ "MISSING",
        TRUE ~ as.character(lookup_val)
      )
    )

  # -- Build audit tibble (SAS lines 173-177) ----------------------------------
  condition_str <- if (!count) "PRESENT" else "COUNT"

  audit_rows <- result %>%
    dplyr::mutate(
      chk       = "VAL",
      ds        = toupper(ds_name),
      var_name  = toupper(var),
      val       = display_val,
      condition = condition_str,
      ind_out   = as.numeric(ind)
    ) %>%
    dplyr::select(chk, ds, var = var_name, val, condition, ind = ind_out) %>%
    dplyr::arrange(ds, var, val, condition)

  # -- Build value_indicators named list (SAS lines 223-225) -------------------
  # SAS: compress(ds)||'_'||compress(var)||'_'||
  #      compress(translate(trim(val),'_',' '),'_','ak')||ifc(count='T','_cnt','')
  #
  # R equivalent: {DS}_{VAR}_{sanitized_val}[_cnt]
  # sanitize: replace spaces with '_', then remove anything not alphanumeric or '_'
  value_indicators <- list()
  for (i in seq_len(nrow(result))) {
    disp_val <- result$display_val[i]

    # Sanitize value name: spaces → '_', keep only alphanum and '_'
    sanitized <- gsub("[^[:alnum:]_]", "", gsub(" ", "_", disp_val))
    key_name  <- paste0(
      toupper(gsub(" ", "", ds_name)), "_",
      toupper(gsub(" ", "", var)), "_",
      sanitized
    )

    if (count) {
      key_name <- paste0(key_name, "_cnt")
    }

    value_indicators[[key_name]] <- result$ind[i]
  }

  # -- Warning if dataset/variable does not exist (SAS lines 231-233) ----------
  if (!success) {
    cli::cli_warn("Dataset {.val {ds_name}} or variable {.val {var}} does not exist.")
  }

  # -- Return ------------------------------------------------------------------
  list(
    audit_rows       = audit_rows,
    value_indicators = value_indicators
  )
}


# ==============================================================================
# chk_cmp — Compare values of a variable between two datasets
# Migrated from SAS %chk_cmp macro (lines 242-310)
#
# SAS behavior:
#   Determines data types of var1 in ds1 and var2 in ds2. If types match,
#   performs PROC SQL FULL JOIN on distinct values, filtering to rows present
#   in only one dataset (asymmetric values).
#
# R equivalent:
#   Checks column existence and type compatibility, then performs dplyr
#   distinct → full_join → filter for asymmetric rows.
#
# Parameters:
#   ds1      — first data frame
#   var1     — character string: variable name in ds1
#   ds2      — second data frame
#   var2     — character string: variable name in ds2
#   ds1_name — character string: display name for ds1
#   ds2_name — character string: display name for ds2
#
# Returns:
#   A tibble with columns: ds1, ds2, in (which dataset), var (the value)
#   containing only values that appear asymmetrically. Returns NULL if
#   validation fails (types mismatch or variables/datasets missing).
# ==============================================================================
chk_cmp <- function(ds1,
                    var1,
                    ds2,
                    var2,
                    ds1_name = deparse(substitute(ds1)),
                    ds2_name = deparse(substitute(ds2))) {

  success <- TRUE

  # -- Validate ds1 and var1 (SAS lines 262-266) ------------------------------
  type1 <- NA_character_
  if (is.data.frame(ds1) && var1 %in% names(ds1)) {
    type1 <- if (is.numeric(ds1[[var1]])) "N" else "C"
  } else {
    success <- FALSE
  }

  # -- Validate ds2 and var2 (SAS lines 268-272) ------------------------------
  type2 <- NA_character_
  if (is.data.frame(ds2) && var2 %in% names(ds2)) {
    type2 <- if (is.numeric(ds2[[var2]])) "N" else "C"
  } else {
    success <- FALSE
  }

  # -- Type agreement check (SAS lines 274-275) --------------------------------
  if (success) {
    if (is.na(type1) || is.na(type2) || type1 != type2) {
      success <- FALSE
    }
  }

  # -- Exit with warning if validation failed (SAS lines 304-308) --------------
  if (!success) {
    cli::cli_warn(paste0(
      "One or more of dataset {.val {ds1_name}} or {.val {ds2_name}} ",
      "or variable {.val {var1}} or {.val {var2}} does not exist, ",
      "or {.val {var1}} and {.val {var2}} are not of the same type."
    ))
    return(NULL)
  }

  # -- Get distinct values from each dataset (SAS lines 281-289) ---------------
  distinct_ds1 <- ds1 %>%
    dplyr::distinct(!!dplyr::sym(var1)) %>%
    dplyr::mutate(ds1_flag = TRUE)

  distinct_ds2 <- ds2 %>%
    dplyr::distinct(!!dplyr::sym(var2)) %>%
    dplyr::mutate(ds2_flag = TRUE)

  # -- Full outer join (SAS lines 291-298) -------------------------------------
  # SAS: full join chk_cmp_ds1 a full join chk_cmp_ds2 b on a.var1 = b.var2
  joined <- dplyr::full_join(
    distinct_ds1,
    distinct_ds2,
    by = stats::setNames(var2, var1)
  )

  # -- Filter to asymmetric rows (SAS line 299) --------------------------------
  # SAS: where not (ds1 and ds2)  →  keep rows where either flag is NA
  # Split into values only-in-ds1 and only-in-ds2, then combine with bind_rows
  # (mirrors the SAS CASE WHEN logic in lines 292-295)

  # Values present only in ds1
  only_ds1 <- joined %>%
    dplyr::filter(!is.na(ds1_flag) & is.na(ds2_flag)) %>%
    dplyr::mutate(
      ds1_col = ds1_name, ds2_col = ds2_name,
      in_ds = ds1_name, var_val = !!dplyr::sym(var1)
    ) %>%
    dplyr::select(ds1 = ds1_col, ds2 = ds2_col, `in` = in_ds, var = var_val)

  # Values present only in ds2
  only_ds2 <- joined %>%
    dplyr::filter(is.na(ds1_flag) & !is.na(ds2_flag)) %>%
    dplyr::mutate(
      ds1_col = ds1_name, ds2_col = ds2_name,
      in_ds = ds2_name, var_val = !!dplyr::sym(var1)
    ) %>%
    dplyr::select(ds1 = ds1_col, ds2 = ds2_col, `in` = in_ds, var = var_val)

  # Combine asymmetric rows from both sides
  result <- dplyr::bind_rows(only_ds1, only_ds2)

  dplyr::as_tibble(result)
}


# ==============================================================================
# MIGRATION NOTES
# ==============================================================================
# ASSUMPTIONS:
#    - SAS DSID/OPEN/VARNUM/VARTYPE/VARLEN replaced by R names(), is.numeric(),
#      nchar() for data frame introspection
#    - SAS positional parameters val1..val25 replaced by single R vector
#      argument `values` (no 25-item limit in R)
#    - SAS call symputx global macro variables replaced by R function return
#      values in named lists
#    - SAS %let miss = MISSING replaced by R constant MISS <- "MISSING"
#    - Variable length semantics differ: SAS has declared lengths; R character
#      vectors are unbounded. max(nchar()) approximates observed content length
#    - SAS lib.ds dataset references replaced by direct data frame arguments
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS varlen returns declared byte length; R max(nchar()) returns actual
#      content length — no functional impact on validation logic
#    - Case sensitivity: SAS upcase comparison depends on session encoding;
#      R toupper follows Unicode rules — may differ for non-ASCII characters
#    - SAS compress(translate(trim(val),'_',' '),'_','ak') sanitization may
#      produce slightly different key names than R gsub equivalent for edge
#      cases involving non-ASCII or special characters
#
# NO DIRECT R EQUIVALENT:
#    - SAS DSID/OPEN/CLOSE for dataset metadata → R is.data.frame/names/ncol/nrow
#    - SAS call symputx for publishing global macro variables → R named list returns
#    - SAS %do %while for val1..val25 iteration → R vector argument
#    - SAS proc datasets delete → not needed (R function-scoped variables)
#    - SAS proc sql with conditional joins → dplyr left_join/filter
#    - SAS attrn(dsid,'nobs')/attrn(dsid,'nvars') → nrow()/ncol()
#    - SAS data _null_ with call symputx → R function return values
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Core tidyverse data manipulation replacing PROC SQL and DATA step
#      logic; tibble(), filter(), count(), left_join(), full_join(), arrange(),
#      mutate(), distinct(), if_else(), case_when(), bind_rows(), sym()
#    - cli: User-facing warning and error messaging replacing SAS %put ERROR
#      statements; cli_warn(), cli_abort()
#
# OPEN QUESTIONS:
#    - Confirm whether callers expect rpt_chk_var/rpt_chk_val as accumulated
#      tibbles or individual rows — current implementation returns per-call
#      results; callers should use dplyr::bind_rows() to accumulate
#    - Verify case sensitivity handling matches SAS behavior for all character
#      encodings in production datasets
# ==============================================================================
