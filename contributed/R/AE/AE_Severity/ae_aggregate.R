# ============================================================
# ae_aggregate.R — AE Aggregation Functions (ab/cd)
# ============================================================
# Migrated from: contributed/AE/AE_Severity/ae_aggregate.sas (234 lines)
#
# Overview:
#   Core analytical computation functions for the AE Severity panel.
#   - ae_ab(): Analyses A/B — AE counts per preferred term, per arm
#   - ae_cd(): Analyses C/D — AE counts per severity level per arm
#   - build_severity_lookup(): Canonical severity ordering utility
#
#   These functions are called by the main ae() driver.
#
# SAS-to-R Migration:
#   %ab macro  (SAS lines 4-67)  → ae_ab() function
#   %cd macro  (SAS lines 72-234) → ae_cd() function
#   Inline severity builder       → build_severity_lookup() function
#
# Original SAS Authors: PhUSE CS Working Group 5
# R Migration: Blitzy Platform
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(rlang)

# ============================================================
# build_severity_lookup() — Build Canonical Severity Lookup Table
# ============================================================
# Migrated from SAS %cd macro lines 78-128
#
# Creates a lookup table mapping each unique severity level found in the
# data to a canonical ordering number (sev_num) and a display name.
#
# Canonical ordering (preserved exactly from SAS):
#   MILD = 1, MODERATE = 2, SEVERE = 3, LIFE THREATENING = 4,
#   FATAL = 5, non-standard = 6, MISSING/blank/NA = 100
#   Order=100 entries are collapsed into a single severity number.
#
# Parameters:
#   ds_base  - data frame containing an 'aesev' column with severity levels
#
# Returns: named list with:
#   all_sev          - tibble: aesev, freq, aesev_display, order, sev_num
#   sev_count        - integer: count of distinct severity levels
#   sev_names        - named character vector (sev_name_1, sev_name_2, ...)
#   max_aesev_nm_len - integer: maximum character length of aesev values
# ============================================================
build_severity_lookup <- function(ds_base) {
  # --- Input validation ---
  if (!is.data.frame(ds_base)) {
    stop("build_severity_lookup: ds_base must be a data frame.", call. = FALSE)
  }
  if (!"aesev" %in% names(ds_base)) {
    stop("build_severity_lookup: ds_base must contain an 'aesev' column.",
         call. = FALSE)
  }

  # --- Step 1: Count occurrences per severity level ---
  # SAS equivalent: PROC SQL SELECT aesev, count(1) FROM ds_base GROUP BY aesev
  all_sev <- ds_base %>%
    count(aesev, name = "freq")

  # --- Step 2: Compute max aesev name length ---
  # SAS equivalent: SELECT max(length(aesev)) INTO :max_aesev_nm_len
  max_aesev_nm_len <- all_sev %>%
    mutate(name_len = nchar(if_else(is.na(aesev), "", aesev))) %>%
    pull(name_len) %>%
    max(na.rm = TRUE)
  if (is.infinite(max_aesev_nm_len) || is.na(max_aesev_nm_len)) {
    max_aesev_nm_len <- 0L
  }

  # --- Step 3: Assign display names and canonical ordering ---
  # SAS equivalent: DATA step with propcase() and SELECT/WHEN
  all_sev <- all_sev %>%
    mutate(
      aesev_display = case_when(
        is.na(aesev)                         ~ "Missing",
        aesev == ""                          ~ "Missing",
        toupper(aesev) == "MISSING"          ~ "Missing",
        TRUE                                 ~ str_to_title(aesev)
      ),
      order = case_when(
        toupper(aesev) == "MILD"             ~ 1L,
        toupper(aesev) == "MODERATE"         ~ 2L,
        toupper(aesev) == "SEVERE"           ~ 3L,
        toupper(aesev) == "LIFE THREATENING" ~ 4L,
        toupper(aesev) == "FATAL"            ~ 5L,
        is.na(aesev) | aesev == "" |
          toupper(aesev) == "MISSING"        ~ 100L,
        TRUE                                 ~ 6L
      )
    ) %>%
    arrange(order, aesev)

  # --- Step 4: Assign sequential severity numbers ---
  # SAS equivalent: DATA step with RETAIN sev_num / _n_ / first.order logic
  # For order = 100 (missing variants), SAS keeps only the FIRST row
  # and DELETEs subsequent duplicates:
  #   `else if order ^= 100 or first.order then sev_num + 1; else delete;`
  # This collapses NA, "", and "MISSING" into a single "Missing" severity row.
  all_sev <- all_sev %>%
    filter(order < 100L | !duplicated(order)) %>%
    mutate(sev_num = row_number())

  # --- Step 5: Derive sev_count and sev_names ---
  # SAS equivalent: CALL SYMPUTX('sev_count', sev_num) at eof
  #                 CALL SYMPUTX('sev_name_'||compress(sev_num), aesev_display)
  sev_count <- max(all_sev$sev_num, na.rm = TRUE)

  unique_sevs <- all_sev %>%
    filter(!duplicated(sev_num)) %>%
    arrange(sev_num)

  sev_names <- setNames(
    unique_sevs$aesev_display,
    paste0("sev_name_", unique_sevs$sev_num)
  )

  list(
    all_sev          = all_sev,
    sev_count        = sev_count,
    sev_names        = sev_names,
    max_aesev_nm_len = max_aesev_nm_len
  )
}


# ============================================================
# ae_ab() — Analysis A/B: Adverse Events Per Preferred Term
# ============================================================
# Migrated from SAS %ab macro (lines 4-67)
#
# Computes per-arm subject counts and percentages for each AE preferred
# term (grouped by body system class).
#   Analysis A (default) = all AEs, filtered to terms > 2% in at least one arm
#   Analysis B (aeser="yes") = serious AEs only, no threshold filter
#
# Parameters:
#   ds_base_bysubjpt - data frame with one row per subject per AE preferred
#                      term. Required columns: aebodsys, aedecod, arm_num.
#                      Column aeser required when aeser parameter = "yes"/"true".
#   config           - named list containing:
#                        arm_count    (integer)  number of treatment arms
#                        arm_subjcnts (numeric vector) subject counts per arm
#                                     indexed 1..N plus named element "total"
#                        arm_names    (character vector) arm display names 1..N
#   aeser            - character: "yes"/"true" for serious AEs (Analysis B),
#                      "no" (default) for all AEs (Analysis A)
#
# Returns: tibble with interleaved columns:
#   aebodsys, aedecod,
#   arm_sum_1, arm_pct_1, ..., arm_sum_N, arm_pct_N,
#   arm_sum_total, arm_pct_total
#
# Sorted by: aebodsys ascending, arm_pct_total descending
# ============================================================
ae_ab <- function(ds_base_bysubjpt, config, aeser = "no") {
  # --- Input validation ---
  if (!is.data.frame(ds_base_bysubjpt)) {
    stop("ab: ds_base_bysubjpt must be a data frame.", call. = FALSE)
  }
  required_cols <- c("aebodsys", "aedecod", "arm_num")
  missing_cols <- setdiff(required_cols, names(ds_base_bysubjpt))
  if (length(missing_cols) > 0L) {
    stop(paste("ab: ds_base_bysubjpt missing required columns:",
               paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(config$arm_count) || is.null(config$arm_subjcnts)) {
    stop("ab: config must contain arm_count and arm_subjcnts.", call. = FALSE)
  }

  arm_count   <- config$arm_count
  arm_subjcnts <- config$arm_subjcnts

  # Normalise aeser parameter
  if (is.null(aeser) || !nzchar(aeser)) aeser <- "no"

  # --- Step 1: Determine analysis type (SAS lines 6-8) ---
  # SAS: %if (%upcase(%substr(&aeser.,1,1)) = Y or = T)
  analysis <- if (toupper(substr(aeser, 1, 1)) %in% c("Y", "T")) "b" else "a"

  # --- Step 2: Filter for serious AEs if Analysis B (SAS lines 15-17) ---
  if (analysis == "b") {
    if (!"aeser" %in% names(ds_base_bysubjpt)) {
      stop("ab: ds_base_bysubjpt must contain 'aeser' column for Analysis B.",
           call. = FALSE)
    }
    working_data <- ds_base_bysubjpt %>%
      filter(.data[["aeser"]] == "Y")
  } else {
    working_data <- ds_base_bysubjpt
  }

  # --- Step 3: Per-arm subject counts per AE term (SAS lines 19-37) ---
  # SAS: RETAIN + arm_sum arrays with BY aebodsys/aedecod processing
  # R:   group_by → summarise → pivot_wider
  ab_data <- working_data %>%
    group_by(aebodsys, aedecod, arm_num) %>%
    summarise(arm_sum = n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = arm_sum,
      names_prefix = "arm_sum_",
      values_fill  = 0L
    )

  # Ensure all arm_sum columns exist (arms with zero counts get 0)
  for (i in seq_len(arm_count)) {
    col_nm <- paste0("arm_sum_", i)
    if (!col_nm %in% names(ab_data)) {
      ab_data[[col_nm]] <- 0L
    }
  }

  # Compute arm_sum_total (SAS: sum(of arm_sum_1 - arm_sum_&arm_count.))
  ab_data <- ab_data %>%
    mutate(arm_sum_total = rowSums(across(starts_with("arm_sum_"))))

  # Compute percentages (SAS: arm_pct(i) = 100 * arm_sum(i) / arm_subjcnt(i))
  for (i in seq_len(arm_count)) {
    sum_col <- paste0("arm_sum_", i)
    pct_col <- paste0("arm_pct_", i)
    subjcnt <- arm_subjcnts[[i]]
    ab_data <- ab_data %>%
      mutate(!!pct_col := 100 * .data[[sum_col]] / subjcnt)
  }
  ab_data <- ab_data %>%
    mutate(arm_pct_total = 100 * arm_sum_total / arm_subjcnts[["total"]])

  # --- Step 4: Apply labels (SAS lines 39-44) ---
  # Store column labels as attributes for traceability
  arm_names <- config$arm_names
  for (i in seq_len(arm_count)) {
    arm_nm <- if (!is.null(arm_names)) arm_names[[i]] else paste0("Arm ", i)
    attr(ab_data[[paste0("arm_sum_", i)]], "label") <-
      paste0(arm_nm, " Subject Count")
    attr(ab_data[[paste0("arm_pct_", i)]], "label") <-
      paste0(arm_nm, " %")
  }
  attr(ab_data[["arm_sum_total"]], "label") <- "Total Subject Count"
  attr(ab_data[["arm_pct_total"]], "label") <- "Total %"

  # --- Step 5: 2% threshold filter for Analysis A (SAS lines 58-60) ---
  # SAS: WHERE NOT (arm_pct_1 <= 2 AND arm_pct_2 <= 2 AND ... AND 1=1)
  # R:   Keep rows where at least one arm percentage exceeds 2%
  if (analysis == "a") {
    pct_cols <- paste0("arm_pct_", seq_len(arm_count))
    ab_output <- ab_data %>%
      filter(if_any(all_of(pct_cols), ~ . > 2))
  } else {
    ab_output <- ab_data
  }

  # --- Step 6: Column ordering (SAS lines 53-56: RETAIN order) ---
  col_order <- c("aebodsys", "aedecod")
  for (i in seq_len(arm_count)) {
    col_order <- c(col_order,
                   paste0("arm_sum_", i),
                   paste0("arm_pct_", i))
  }
  col_order <- c(col_order, "arm_sum_total", "arm_pct_total")
  ab_output <- ab_output %>% select(all_of(col_order))

  # --- Step 7: Sort (SAS lines 63-65) ---
  # SAS: PROC SORT BY aebodsys DESCENDING arm_pct_total
  ab_output <- ab_output %>%
    arrange(aebodsys, desc(arm_pct_total))

  ab_output
}


# ============================================================
# ae_cd() — Analysis C/D: Adverse Events Per Severity Level
# ============================================================
# Migrated from SAS %cd macro (lines 72-234)
#
# Computes per-arm×severity counts for each AE preferred term (grouped by
# body system class), plus a missing severity report.
#   Analysis C (default) = all AEs
#   Analysis D (aeser="yes") = serious AEs only
#
# Parameters:
#   ds_base    - data frame with one row per AE event.
#                Required columns: aebodsys, aedecod, arm_num, aesev.
#                Column aeser required when aeser parameter = "yes"/"true".
#   config     - named list containing:
#                  arm_count  (integer)  number of treatment arms
#                  arm_names  (character vector) arm display names 1..N
#   aeser      - character: "yes"/"true" for serious AEs (Analysis D),
#                "no" (default) for all AEs (Analysis C)
#   sev_lookup - (optional) pre-built severity lookup from build_severity_lookup().
#                If NULL, builds internally. Pass to reuse across calls
#                (mirrors SAS %if %sysfunc(exist(all_sev)) caching).
#
# Returns: named list with:
#   cd_output   - tibble: aebodsys, aedecod, arm1_sev1..armN_sevM, sum_total
#   rpt_missing - single-row tibble: report label + arm*_missing + arm*_missing_pct
#   sev_lookup  - the severity lookup used (for reuse in subsequent calls)
# ============================================================
ae_cd <- function(ds_base, config, aeser = "no", sev_lookup = NULL) {
  # --- Input validation ---
  if (!is.data.frame(ds_base)) {
    stop("cd: ds_base must be a data frame.", call. = FALSE)
  }
  required_cols <- c("aebodsys", "aedecod", "arm_num", "aesev")
  missing_cols <- setdiff(required_cols, names(ds_base))
  if (length(missing_cols) > 0L) {
    stop(paste("cd: ds_base missing required columns:",
               paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(config$arm_count)) {
    stop("cd: config must contain arm_count.", call. = FALSE)
  }

  arm_count <- config$arm_count

  # Normalise aeser parameter
  if (is.null(aeser) || !nzchar(aeser)) aeser <- "no"

  # --- Step 1: Determine analysis type (SAS lines 74-76) ---
  analysis <- if (toupper(substr(aeser, 1, 1)) %in% c("Y", "T")) "d" else "c"

  # --- Step 2: Build severity lookup if not provided (SAS lines 78-128) ---
  # SAS: %if %sysfunc(exist(all_sev)) = 0 → only build when not cached
  if (is.null(sev_lookup)) {
    sev_lookup <- build_severity_lookup(ds_base)
  }
  sev_count <- sev_lookup$sev_count

  # --- Step 3: Filter for serious AEs if Analysis D (SAS lines 136-138) ---
  if (analysis == "d") {
    if (!"aeser" %in% names(ds_base)) {
      stop("cd: ds_base must contain 'aeser' column for Analysis D.",
           call. = FALSE)
    }
    working_data <- ds_base %>%
      filter(.data[["aeser"]] == "Y")
  } else {
    working_data <- ds_base
  }

  # --- Step 4: Join severity lookup to get sev_num (SAS lines 158-166) ---
  # SAS: hash sev_lookup with definekey('aesev') / definedata('sev_num')
  # When hash find() fails (rc != 0), SAS assigns sev_num = sev_count (Missing).
  # This handles NA, empty string, and unknown severity values.
  working_data <- working_data %>%
    left_join(
      sev_lookup$all_sev %>% select(aesev, sev_num),
      by = "aesev"
    ) %>%
    mutate(sev_num = if_else(is.na(sev_num),
                             as.integer(sev_lookup$sev_count),
                             sev_num))

  # --- Step 5: Count per term × arm × severity (SAS lines 168-176) ---
  # SAS: RETAIN + 2D array sum_sev{arm_count, sev_count} with BY processing
  cd_counts <- working_data %>%
    group_by(aebodsys, aedecod, arm_num, sev_num) %>%
    summarise(count = n(), .groups = "drop")

  # --- Step 6: Pivot to wide format (SAS: arm1_sev1..armN_sevM) ---
  cd_wide <- cd_counts %>%
    mutate(col_name = paste0("arm", arm_num, "_sev", sev_num)) %>%
    select(aebodsys, aedecod, col_name, count) %>%
    tidyr::pivot_wider(
      names_from  = col_name,
      values_from = count,
      values_fill = 0L
    )

  # Ensure all expected arm×severity columns exist (zero-fill missing combos)
  expected_cols <- as.vector(outer(
    paste0("arm", seq_len(arm_count)),
    paste0("_sev", seq_len(sev_count)),
    paste0
  ))
  for (col in expected_cols) {
    if (!col %in% names(cd_wide)) {
      cd_wide[[col]] <- 0L
    }
  }

  # Compute sum_total (SAS line 179: sum of all arm×sev columns)
  cd_output <- cd_wide %>%
    mutate(sum_total = rowSums(across(all_of(expected_cols))))

  # --- Step 7: Apply labels (SAS lines 183-189) ---
  arm_names_cfg <- config$arm_names
  sev_names     <- sev_lookup$sev_names
  for (i in seq_len(arm_count)) {
    arm_nm <- if (!is.null(arm_names_cfg)) {
      arm_names_cfg[[i]]
    } else {
      paste0("Arm ", i)
    }
    for (j in seq_len(sev_count)) {
      col <- paste0("arm", i, "_sev", j)
      sev_nm_key <- paste0("sev_name_", j)
      sev_nm <- if (!is.null(sev_names[[sev_nm_key]])) {
        sev_names[[sev_nm_key]]
      } else {
        paste0("Severity ", j)
      }
      if (col %in% names(cd_output)) {
        attr(cd_output[[col]], "label") <- paste(arm_nm, sev_nm)
      }
    }
  }
  attr(cd_output[["sum_total"]], "label") <- "Total"

  # --- Step 8: Keep required columns and sort (SAS lines 191-199) ---
  # SAS: PROC SORT BY descending sum_total (aebodsys is commented out)
  keep_cols <- c("aebodsys", "aedecod", expected_cols, "sum_total")
  cd_output <- cd_output %>%
    select(all_of(keep_cols)) %>%
    arrange(desc(sum_total))

  # --- Step 9: Missing severity report (SAS lines 202-233) ---
  # SAS PROC SQL: sum each arm×sev column across all terms
  rpt_totals <- cd_output %>%
    summarise(across(all_of(expected_cols), sum))

  # Determine report label
  report_label <- if (analysis == "c") {
    "AEs by Severity"
  } else {
    "Serious AEs by Severity"
  }
  rpt_missing <- rpt_totals %>%
    mutate(report = report_label)

  # Calculate missing counts and percentages per arm
  # SAS: arm_i_missing = arm_i_sev_N (last sev = Missing)
  #      arm_i_missing_pct = 100 * arm_i_sev_N / sum(arm_i_sev1..arm_i_sevN)
  for (i in seq_len(arm_count)) {
    missing_col <- paste0("arm", i, "_sev", sev_count)
    sev_cols    <- paste0("arm", i, "_sev", seq_len(sev_count))

    arm_total_val  <- rowSums(rpt_missing[, sev_cols, drop = FALSE])
    arm_missing_val <- rpt_missing[[missing_col]]

    rpt_missing <- rpt_missing %>%
      mutate(
        !!paste0("arm", i, "_missing") := arm_missing_val,
        !!paste0("arm", i, "_missing_pct") := if_else(
          arm_total_val > 0,
          100 * arm_missing_val / arm_total_val,
          NA_real_
        )
      )
  }

  # Keep only report label and missing columns (SAS: keep report arm*_missing:)
  rpt_missing <- rpt_missing %>%
    select(report, matches("_missing"))

  # --- Return results ---
  list(
    cd_output   = cd_output,
    rpt_missing = rpt_missing,
    sev_lookup  = sev_lookup
  )
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS RETAIN + array processing -> dplyr group_by + summarise + pivot_wider
####    - SAS BY-group first.aedecod/last.aedecod processing -> dplyr grouping semantics
####    - SAS hash lookup for severity level -> dplyr left_join
####    - SAS dynamic macro variables (sev_name_1..sev_name_N, sev_count) -> R list elements
####    - SAS arm_subjcnt array initialization from macro variables -> R named vector from config
####    - Severity canonical ordering (MILD=1..FATAL=5, MISSING=100, other=6) preserved exactly
####    - SAS propcase() -> stringr::str_to_title() for standard severity terms
####    - SAS PROC DATASETS cleanup -> not needed in R (function-scoped variables)
####    - 2% threshold filter in Analysis A: SAS "not (all pct <= 2)" -> R if_any(> 2)
####    - SAS %if %sysfunc(exist(all_sev)) caching -> optional sev_lookup parameter
####    - Column names preserved lowercase matching SAS variable names
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Percentage calculation: 100 * count / denominator — identical arithmetic
####    - Rounding: No explicit rounding in aggregation (raw percentages preserved)
####    - Sort stability: SAS PROC SORT is stable; dplyr::arrange() is stable — equivalent
####    - Missing severity detection: SAS checks upcase(aesev) and blank; R checks toupper, NA, and ""
####    - Division by zero: SAS produces missing (.); R returns NA_real_ via if_else guard
#### NO DIRECT R EQUIVALENT:
####    - SAS RETAIN statement -> Not needed; dplyr summarise computes group-level aggregates
####    - SAS array processing (arm_sum{*}, sum_sev{arm,sev}) -> dplyr column operations
####    - SAS hash object for severity lookup -> dplyr left_join
####    - SAS macro %do loops for column generation -> R for loops with rlang :=
####    - SAS PROC DATASETS delete -> Not needed (R garbage collection)
####    - SAS call symputx for sev_name/sev_count -> R list elements returned from function
#### PACKAGE SELECTION RATIONALE:
####    - dplyr: Natural replacement for DATA step BY-group processing and PROC SQL
####    - tidyr: pivot_wider for converting long arm/severity counts to wide format
####    - stringr: str_to_title() for severity display names (replacing SAS propcase)
####    - rlang: := operator and .data pronoun for programmatic column creation
#### OPEN QUESTIONS:
####    - Confirm whether severity levels beyond the canonical 5 (MILD..FATAL) exist in study data
####    - Verify that "MISSING" severity is always the last sev_num (for rpt_missing calculation)
####    - Confirm arm_num in ds_base is 1-indexed integer matching config$arm_count ordering
####    - Verify that ds_base contains both aeser and aesev columns when available
####    - Confirm whether NA aesev and empty string aesev can co-occur in same dataset
#### ============================================================
