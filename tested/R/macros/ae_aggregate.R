# =============================================================================
# ae_aggregate.R - AE Severity Panel Aggregation Functions
# =============================================================================
# Migrated from: tested/SAS/macros/ae_aggregate.sas (252 lines)
# SAS macros:    %ab(aeser=no), %cd(aeser=no)
#
# Purpose:
#   Centralizes two core AE aggregation functions for the AE Severity Panel.
#   ae_ab() computes preferred-term subject counts per treatment arm
#   (Analyses A and B).
#   ae_cd() computes severity-level counts per treatment arm
#   (Analyses C and D).
#
# SAS -> R Migration Pattern:
#   %macro params      -> R named function arguments with matching defaults
#   DATA step RETAIN   -> dplyr::group_by() + dplyr::summarise()
#   2D RETAIN array    -> dplyr::group_by() + tidyr::pivot_wider()
#   SAS hash lookup    -> dplyr::left_join()
#   PROC SORT          -> dplyr::arrange()
#   PROPCASE()         -> stringr::str_to_title()
#   Global macro vars  -> Function return values
#
# Required packages (explicit namespace, no library() calls):
#   dplyr   (>=1.1.0) - Core data manipulation
#   tidyr   (>=1.3.0) - Reshaping (complete, pivot_wider, nesting)
#   stringr (>=1.5.0) - String manipulation (str_to_title)
#   janitor (>=2.2.0) - SAS-compatible rounding (round_half_up)
#   tibble  (>=3.2.0) - Enhanced data frames (tibble)
# =============================================================================

# Import pipe operator for namespace-qualified usage (no library() call needed)
# dplyr re-exports %>% from magrittr; this makes it available in this file's scope
`%>%` <- dplyr::`%>%`


# -----------------------------------------------------------------------------
# Internal helper: Normalize AESEV values
# -----------------------------------------------------------------------------
# Converts NA, empty string, and literal "MISSING" to NA_character_ so that
# all missing-category severity values are represented consistently.
# Non-missing values are preserved as-is (case-sensitive, matching SAS).
# -----------------------------------------------------------------------------
.normalize_aesev <- function(x) {
  # First: NA -> ""
  x_clean <- dplyr::if_else(is.na(x), "", x)
  # Then: "", "MISSING", "Missing", "missing" -> NA_character_
  dplyr::if_else(toupper(x_clean) %in% c("", "MISSING"), NA_character_, x)
}


# -----------------------------------------------------------------------------
# Internal helper: Build severity level catalog
# -----------------------------------------------------------------------------
# Replicates SAS lines 84-132 of ae_aggregate.sas.
# Discovers distinct AESEV values, assigns standard ordering, creates
# proper-case display names, and assigns sequential severity numbers.
# Missing-category entries (NA, blank, "MISSING") are consolidated into
# a single severity level with order = 100 (matching SAS first.order logic).
#
# Parameters:
#   ds_base - Data frame with column 'aesev'
#
# Returns:
#   Named list with:
#     sev_levels  - Tibble: aesev_norm, aesev_upper, order, aesev_display,
#                   sev_num (one row per unique severity level)
#     sev_count   - Integer: number of severity levels
#     sev_names   - Character vector: display names in order
# -----------------------------------------------------------------------------
.build_severity_catalog <- function(ds_base) {

  # Known severity order (SAS lines 107-117)
  known_orders <- tibble::tibble(
    ref_upper = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING", "FATAL"),
    known_order = c(1L, 2L, 3L, 4L, 5L)
  )

 # Get distinct normalised severity values (SAS lines 87-96 PROC SQL)
  catalog <- ds_base %>%
    dplyr::mutate(aesev_norm = .normalize_aesev(.data$aesev)) %>%
    dplyr::distinct(.data$aesev_norm) %>%
    dplyr::mutate(
      aesev_upper = toupper(dplyr::coalesce(.data$aesev_norm, ""))
    ) %>%
    # Join known order table (SAS lines 107-117 SELECT)
    dplyr::left_join(known_orders,
                     by = c("aesev_upper" = "ref_upper")) %>%
    dplyr::mutate(
      # Assign order: known -> known_order; missing -> 100; other -> 6
      order = dplyr::case_when(
        !is.na(.data$known_order)  ~ .data$known_order,
        is.na(.data$aesev_norm)    ~ 100L,
        TRUE                       ~ 6L
      ),
      # Display name: PROPCASE equivalent (SAS line 105, 114)
      aesev_display = dplyr::case_when(
        is.na(.data$aesev_norm) ~ "Missing",
        TRUE ~ stringr::str_to_title(.data$aesev_norm)
      )
    ) %>%
    dplyr::select("aesev_norm", "aesev_upper", "order", "aesev_display") %>%
    # Sort by order then aesev (SAS line 121 PROC SORT)
    dplyr::arrange(.data$order, .data$aesev_norm)

  # Consolidate order = 100: keep only the first entry per order group
  # (SAS lines 127-128: if order < 100 or (order = 100 and first.order))
  sev_levels <- catalog %>%
    dplyr::group_by(.data$order) %>%
    dplyr::filter(.data$order < 100L | dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sev_num = dplyr::row_number())

  list(
    sev_levels = sev_levels,
    sev_count  = nrow(sev_levels),
    sev_names  = sev_levels$aesev_display
  )
}


# =============================================================================
# ae_ab: Adverse Events per Preferred Term (Analyses A and B)
# =============================================================================
# Replaces SAS %ab(aeser=no) macro (lines 9-72 of ae_aggregate.sas).
#
# Analysis A (aeser = FALSE):
#   All AEs by preferred term per arm, filtered to rows where at least one
#   arm exceeds filter_pct threshold (SAS line 64: arm_pct_i <= 2).
#
# Analysis B (aeser = TRUE):
#   Serious AEs only (AESER = 'Y'), no frequency threshold filter applied.
#
# Parameters:
#   ds_base_bysubjpt - Data frame: one row per subject per preferred term.
#                      Required columns: aebodsys, aedecod, arm_num.
#                      For analysis B also requires column aeser.
#   arm_count        - Integer: number of treatment arms (excluding total).
#   arm_names        - Character vector of arm display names (length arm_count).
#   arm_subjcnt      - Integer vector of subject counts per arm PLUS total.
#                      Length must be arm_count + 1; last element is the total
#                      subject count. Matches SAS arm_subjcnt array layout.
#   aeser            - Logical: TRUE = filter to serious AEs (Analysis B).
#                      Default FALSE (Analysis A). NOTE: this parameter name
#                      intentionally shadows the data column 'aeser'; the
#                      column is referenced via .data$aeser inside dplyr verbs.
#   filter_pct       - Numeric: threshold for Analysis A filtering. Rows where
#                      ALL arm percentages are <= filter_pct are removed.
#                      Default 2.0, matching SAS hardcoded 2% (line 64).
#   pct_digits       - Integer or NULL: if non-NULL, apply SAS-compatible
#                      rounding via janitor::round_half_up() to the specified
#                      number of decimal places. Default NULL (raw float
#                      percentages, matching SAS intermediate computation).
#
# Returns:
#   Tibble with columns in SAS RETAIN order (lines 59-61):
#     aebodsys, aedecod,
#     arm_sum_1, arm_pct_1, ..., arm_sum_<N>, arm_pct_<N>,
#     arm_sum_total, arm_pct_total
#   Sorted by aebodsys ascending, arm_pct_total descending (lines 68-70).
# =============================================================================
ae_ab <- function(ds_base_bysubjpt,
                  arm_count,
                  arm_names,
                  arm_subjcnt,
                  aeser = FALSE,
                  filter_pct = 2.0,
                  pct_digits = NULL) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.data.frame(ds_base_bysubjpt),
    is.numeric(arm_count), length(arm_count) == 1L, arm_count >= 1L,
    is.character(arm_names), length(arm_names) == arm_count,
    is.numeric(arm_subjcnt), length(arm_subjcnt) == arm_count + 1L,
    is.logical(aeser), length(aeser) == 1L,
    is.numeric(filter_pct), length(filter_pct) == 1L
  )

  required_cols <- c("aebodsys", "aedecod", "arm_num")
  missing_cols <- setdiff(required_cols, names(ds_base_bysubjpt))
  if (length(missing_cols) > 0L) {
    stop("ds_base_bysubjpt is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (aeser && !"aeser" %in% names(ds_base_bysubjpt)) {
    stop("aeser = TRUE requires column 'aeser' in ds_base_bysubjpt")
  }

  # --- Analysis selection (SAS lines 11-13) -----------------------------------
  analysis <- if (aeser) "b" else "a"

  # --- Apply AESER filter for Analysis B (SAS lines 20-22) --------------------
  # .data$aeser references the DATA COLUMN, not the function parameter
  data <- ds_base_bysubjpt
  if (aeser) {
    data <- data %>%
      dplyr::filter(toupper(.data$aeser) == "Y")
  }

  # --- Handle empty data after filtering --------------------------------------
  if (nrow(data) == 0L) {
    empty_result <- tibble::tibble(
      aebodsys = character(0L),
      aedecod  = character(0L)
    )
    for (i in seq_len(arm_count)) {
      empty_result[[paste0("arm_sum_", i)]] <- integer(0L)
      empty_result[[paste0("arm_pct_", i)]] <- numeric(0L)
    }
    empty_result[["arm_sum_total"]] <- integer(0L)
    empty_result[["arm_pct_total"]] <- numeric(0L)
    return(empty_result)
  }

  # --- Count subjects per arm per SOC/PT (SAS lines 18-42) --------------------
  # Replaces: BY aebodsys aedecod; RETAIN arm_sum_1..arm_sum_n;
  #   first.aedecod -> zero arm_sum; arm_sum(arm_num) += 1; last.aedecod -> output
  term_counts <- data %>%
    dplyr::group_by(.data$aebodsys, .data$aedecod, .data$arm_num) %>%
    dplyr::summarise(arm_sum = dplyr::n(), .groups = "drop") %>%
    # Ensure all arm x term combinations exist with fill = 0
    # (replaces SAS RETAIN zero initialisation, lines 24-25, 30-32)
    tidyr::complete(
      tidyr::nesting(aebodsys, aedecod),
      arm_num = seq_len(arm_count),
      fill = list(arm_sum = 0L)
    )

  # --- Compute total counts per term (SAS line 37) ----------------------------
  term_totals <- term_counts %>%
    dplyr::group_by(.data$aebodsys, .data$aedecod) %>%
    dplyr::summarise(arm_sum_total = sum(.data$arm_sum), .groups = "drop")

  # --- Compute percentages per arm (SAS line 39) -----------------------------
  # SAS: arm_pct(i) = 100 * arm_sum(i) / arm_subjcnt(i)
  term_counts <- term_counts %>%
    dplyr::mutate(
      arm_pct = 100 * .data$arm_sum / arm_subjcnt[.data$arm_num]
    )

  # --- Pivot to wide format (SAS lines 51-53 KEEP) ---------------------------
  wide_output <- term_counts %>%
    tidyr::pivot_wider(
      id_cols     = c("aebodsys", "aedecod"),
      names_from  = "arm_num",
      values_from = c("arm_sum", "arm_pct"),
      names_glue  = "{.value}_{arm_num}",
      values_fill = list(arm_sum = 0L, arm_pct = 0)
    )

  # --- Add total columns (SAS lines 37-39) -----------------------------------
  wide_output <- wide_output %>%
    dplyr::left_join(term_totals, by = c("aebodsys", "aedecod")) %>%
    dplyr::mutate(
      arm_pct_total = 100 * .data$arm_sum_total / arm_subjcnt[arm_count + 1L]
    )

  # --- Apply SAS-compatible rounding (AAP Gate 2 requirement) -----------------
  if (!is.null(pct_digits)) {
    pct_col_names <- c(paste0("arm_pct_", seq_len(arm_count)), "arm_pct_total")
    wide_output <- wide_output %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(pct_col_names),
        ~ janitor::round_half_up(., digits = pct_digits)
      ))
  }

  # --- Column labels (SAS lines 44-49) ---------------------------------------
  for (i in seq_len(arm_count)) {
    attr(wide_output[[paste0("arm_sum_", i)]], "label") <-
      paste(arm_names[i], "Subject Count")
    attr(wide_output[[paste0("arm_pct_", i)]], "label") <-
      paste0(arm_names[i], " %")
  }
  attr(wide_output[["arm_sum_total"]], "label") <- "Total Subject Count"
  attr(wide_output[["arm_pct_total"]], "label")  <- "Total %"

  # --- Output filtering for Analysis A (SAS lines 58-65) ---------------------
  # SAS: where not (arm_pct_1 <= 2 and arm_pct_2 <= 2 and ... and 1=1)
  # Equivalent: keep rows where at least one arm has pct > filter_pct
  if (!aeser) {
    pct_col_names <- paste0("arm_pct_", seq_len(arm_count))
    wide_output <- wide_output %>%
      dplyr::filter(dplyr::if_any(
        dplyr::all_of(pct_col_names),
        ~ . > filter_pct
      ))
  }

  # --- Sort by aebodsys, descending arm_pct_total (SAS lines 68-70) ----------
  wide_output <- wide_output %>%
    dplyr::arrange(.data$aebodsys, dplyr::desc(.data$arm_pct_total))

  # --- Reorder columns to match SAS RETAIN order (SAS lines 59-61) -----------
  # aebodsys, aedecod, arm_sum_1, arm_pct_1, arm_sum_2, arm_pct_2, ...
  # arm_sum_total, arm_pct_total
  col_order <- c("aebodsys", "aedecod")
  for (i in seq_len(arm_count)) {
    col_order <- c(col_order, paste0("arm_sum_", i), paste0("arm_pct_", i))
  }
  col_order <- c(col_order, "arm_sum_total", "arm_pct_total")

  wide_output <- wide_output %>%
    dplyr::select(dplyr::all_of(col_order))

  wide_output
}


# =============================================================================
# ae_cd: Adverse Events per Severity Level (Analyses C and D)
# =============================================================================
# Replaces SAS %cd(aeser=no) macro (lines 77-251 of ae_aggregate.sas).
#
# Analysis C (aeser = FALSE):
#   All AEs cross-tabulated by preferred term, arm, and severity level.
#
# Analysis D (aeser = TRUE):
#   Serious AEs only (AESER = 'Y'), same cross-tabulation.
#
# Parameters:
#   ds_base      - Data frame with one row per AE record.
#                  Required columns: aebodsys, aedecod, arm_num, aesev.
#                  For Analysis D also requires column aeser.
#   arm_count    - Integer: number of treatment arms (excluding total).
#   arm_names    - Character vector of arm display names (length arm_count).
#   arm_subjcnt  - Integer vector of subject counts per arm PLUS total.
#                  Length must be arm_count + 1.
#   aeser        - Logical: TRUE = filter to serious AEs (Analysis D).
#                  Default FALSE (Analysis C).
#   all_sev      - Optional: severity catalog from a previous ae_cd() call
#                  (the $all_sev element of the return value). If NULL,
#                  the catalog is built from ds_base. This mirrors the SAS
#                  %if %sysfunc(exist(all_sev))=0 logic (line 84) that reuses
#                  the catalog across calls for Analyses C and D.
#   pct_digits   - Integer or NULL: if non-NULL, apply SAS-compatible
#                  rounding via janitor::round_half_up() to rpt_missing
#                  percentages. Default NULL (raw float).
#
# Returns:
#   Named list with:
#     cd_output        - Tibble: aebodsys, aedecod, arm<i>_sev<j> columns,
#                        sum_total. Sorted by descending sum_total
#                        (SAS line 204; note aebodsys sort is commented out
#                        in the SAS source).
#     all_sev          - Tibble: severity level catalog (sev_levels) for
#                        reuse across calls.
#     sev_count        - Integer: number of severity levels.
#     sev_names        - Character vector: severity display names in order.
#     rpt_missing_row  - Tibble: single row with report label and per-arm
#                        missing severity counts / percentages. Caller
#                        accumulates via dplyr::bind_rows() across calls.
# =============================================================================
ae_cd <- function(ds_base,
                  arm_count,
                  arm_names,
                  arm_subjcnt,
                  aeser = FALSE,
                  all_sev = NULL,
                  pct_digits = NULL) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.data.frame(ds_base),
    is.numeric(arm_count), length(arm_count) == 1L, arm_count >= 1L,
    is.character(arm_names), length(arm_names) == arm_count,
    is.numeric(arm_subjcnt), length(arm_subjcnt) == arm_count + 1L,
    is.logical(aeser), length(aeser) == 1L
  )

  required_cols <- c("aebodsys", "aedecod", "arm_num", "aesev")
  missing_cols <- setdiff(required_cols, names(ds_base))
  if (length(missing_cols) > 0L) {
    stop("ds_base is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (aeser && !"aeser" %in% names(ds_base)) {
    stop("aeser = TRUE requires column 'aeser' in ds_base")
  }

  # --- Analysis selection (SAS lines 79-81) -----------------------------------
  analysis <- if (aeser) "d" else "c"

  # --- Build severity catalog from UNFILTERED data (SAS lines 84-132) ---------
  # SAS builds all_sev from ds_base BEFORE applying AESER filter.
  # This ensures the catalog is consistent across Analyses C and D.
  if (is.null(all_sev)) {
    sev_info    <- .build_severity_catalog(ds_base)
    sev_levels  <- sev_info$sev_levels
    sev_count   <- sev_info$sev_count
    sev_names   <- sev_info$sev_names
  } else {
    # Reuse provided catalog (mirrors SAS %if exist(all_sev) logic)
    sev_levels <- all_sev
    sev_count  <- nrow(sev_levels)
    sev_names  <- sev_levels$aesev_display
  }

  # --- Apply AESER filter for Analysis D (SAS lines 141-143) ------------------
  data <- ds_base
  if (aeser) {
    data <- data %>%
      dplyr::filter(toupper(.data$aeser) == "Y")
  }

  # --- Handle empty data after filtering --------------------------------------
  if (nrow(data) == 0L) {
    # Build correct column structure with zero rows
    sev_col_names <- character(0L)
    for (i in seq_len(arm_count)) {
      for (j in seq_len(sev_count)) {
        sev_col_names <- c(sev_col_names, paste0("arm", i, "_sev", j))
      }
    }
    empty_cd <- tibble::tibble(
      aebodsys = character(0L),
      aedecod  = character(0L)
    )
    for (cn in sev_col_names) empty_cd[[cn]] <- integer(0L)
    empty_cd[["sum_total"]] <- integer(0L)

    report_label <- if (!aeser) "AEs by Severity" else "Serious AEs by Severity"
    empty_rpt <- tibble::tibble(report = report_label)
    for (i in seq_len(arm_count)) {
      empty_rpt[[paste0("arm", i, "_missing")]]     <- 0L
      empty_rpt[[paste0("arm", i, "_missing_pct")]] <- 0
    }

    return(list(
      cd_output       = empty_cd,
      all_sev         = sev_levels,
      sev_count       = sev_count,
      sev_names       = sev_names,
      rpt_missing_row = empty_rpt
    ))
  }

  # --- Build severity lookup for joining (SAS lines 163-167 hash) ------------
  # Map every distinct aesev in the filtered data to its sev_num via the
  # normalised catalog. Missing-category values all map to the same sev_num.
  missing_sev_num <- sev_levels$sev_num[sev_levels$order == 100L]
  if (length(missing_sev_num) == 0L) missing_sev_num <- NA_integer_

  # Non-missing severity lookup: match on uppercased normalised value
  non_missing_ref <- sev_levels %>%
    dplyr::filter(.data$order < 100L) %>%
    dplyr::mutate(ref_upper = toupper(dplyr::coalesce(.data$aesev_norm, ""))) %>%
    dplyr::select("ref_upper", "sev_num")

  sev_lookup <- data %>%
    dplyr::distinct(.data$aesev) %>%
    dplyr::mutate(
      aesev_norm  = .normalize_aesev(.data$aesev),
      aesev_upper = toupper(dplyr::coalesce(.data$aesev_norm, "")),
      is_missing  = is.na(.data$aesev_norm)
    ) %>%
    dplyr::left_join(non_missing_ref,
                     by = c("aesev_upper" = "ref_upper")) %>%
    dplyr::mutate(
      sev_num = dplyr::case_when(
        !is.na(.data$sev_num) ~ .data$sev_num,
        .data$is_missing      ~ missing_sev_num,
        TRUE                  ~ .data$sev_num
      )
    ) %>%
    dplyr::select("aesev", "sev_num")

  # --- Join severity numbers to data (SAS lines 170-171 hash find) -----------
  data_with_sev <- data %>%
    dplyr::left_join(sev_lookup, by = "aesev")

  # Warn about unmatched (should not happen with consistent data)
  n_unmatched <- sum(is.na(data_with_sev$sev_num))
  if (n_unmatched > 0L) {
    warning(n_unmatched, " AE records could not be mapped to a severity level ",
            "and will be excluded from the cross-tabulation.")
    data_with_sev <- data_with_sev %>%
      dplyr::filter(!is.na(.data$sev_num))
  }

  # --- Cross-tabulation: arm x severity per term (SAS lines 137-186) ---------
  # Replaces: RETAIN sum_sev{arm_count, sev_count};
  #   first.aedecod -> zero all; sum_sev(arm_num, sev_num) += 1;
  #   last.aedecod -> compute sum_total, output
  term_sev <- data_with_sev %>%
    dplyr::group_by(.data$aebodsys, .data$aedecod,
                    .data$arm_num, .data$sev_num) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    # Ensure all arm x sev x term combinations (SAS lines 173-178 zero init)
    tidyr::complete(
      tidyr::nesting(aebodsys, aedecod),
      arm_num = seq_len(arm_count),
      sev_num = seq_len(sev_count),
      fill = list(count = 0L)
    )

  # --- Pivot to wide format (SAS lines 152-158, 196-200) ---------------------
  # Column names: arm1_sev1, arm1_sev2, ..., arm2_sev1, ...
  term_sev <- term_sev %>%
    dplyr::mutate(
      col_name = paste0("arm", .data$arm_num, "_sev", .data$sev_num)
    )

  wide <- term_sev %>%
    tidyr::pivot_wider(
      id_cols     = c("aebodsys", "aedecod"),
      names_from  = "col_name",
      values_from = "count",
      values_fill = 0L
    )

  # --- Compute sum_total (SAS line 184) --------------------------------------
  # Build explicit column name vector for labels and rpt_missing (SAS lines 188-194)
  sev_col_names <- character(0L)
  for (i in seq_len(arm_count)) {
    for (j in seq_len(sev_count)) {
      sev_col_names <- c(sev_col_names, paste0("arm", i, "_sev", j))
    }
  }

  # Use starts_with("arm") to select all arm*_sev* columns for the row total.
  # At this point in the pipeline, the only columns starting with "arm" are the
  # cross-tabulation columns (arm1_sev1, arm1_sev2, ..., arm2_sev1, ...).
  wide <- wide %>%
    dplyr::mutate(
      sum_total = rowSums(dplyr::across(dplyr::starts_with("arm")))
    )

  # --- Column labels (SAS lines 188-194) -------------------------------------
  for (i in seq_len(arm_count)) {
    for (j in seq_len(sev_count)) {
      cn <- paste0("arm", i, "_sev", j)
      if (cn %in% names(wide)) {
        attr(wide[[cn]], "label") <- paste(arm_names[i], sev_names[j])
      }
    }
  }
  attr(wide[["sum_total"]], "label") <- "Total"

  # --- Reorder columns to match SAS KEEP order (SAS lines 196-200) -----------
  col_order <- c("aebodsys", "aedecod", sev_col_names, "sum_total")
  wide <- wide %>%
    dplyr::select(dplyr::all_of(col_order))

  # --- Sort by descending sum_total (SAS line 204) ---------------------------
  # NOTE: In the SAS source, aebodsys is commented out in the BY statement:
  #   by /*aebodsys*/ descending sum_total;
  # We faithfully replicate this as sort by descending sum_total only.
  wide <- wide %>%
    dplyr::arrange(dplyr::desc(.data$sum_total))

  # --- rpt_missing computation (SAS lines 207-247) ---------------------------
  # Step 1: Sum all arm*_sev* columns across all terms (SAS lines 209-221)
  sev_sums <- wide %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(sev_col_names),
      sum
    ))

  # Step 2: Determine report label (SAS lines 211-213)
  report_label <- if (!aeser) "AEs by Severity" else "Serious AEs by Severity"

  # Step 3: Get last severity display name (SAS lines 223-227)
  last_sev <- sev_names[sev_count]

  # Step 4: Compute missing counts and percentages per arm (SAS lines 229-242)
  rpt_missing_row <- tibble::tibble(report = report_label)

  for (i in seq_len(arm_count)) {
    if (identical(last_sev, "Missing")) {
      # Last severity IS "Missing" -> use last sev column (SAS lines 232-234)
      missing_col <- paste0("arm", i, "_sev", sev_count)
      arm_missing <- as.integer(sev_sums[[missing_col]])

      # Total across all severities for this arm
      arm_sev_cols <- paste0("arm", i, "_sev", seq_len(sev_count))
      arm_total <- sum(as.numeric(sev_sums[arm_sev_cols]))

      # SAS: 100 * missing / total (line 234)
      arm_missing_pct <- if (arm_total > 0) {
        100 * arm_missing / arm_total
      } else {
        0
      }
    } else {
      # Last severity is NOT "Missing" -> counts are 0 (SAS lines 236-238)
      arm_missing <- 0L
      arm_missing_pct <- 0
    }

    # Apply SAS-compatible rounding to percentage if requested
    if (!is.null(pct_digits)) {
      arm_missing_pct <- janitor::round_half_up(arm_missing_pct,
                                                 digits = pct_digits)
    }

    rpt_missing_row[[paste0("arm", i, "_missing")]]     <- arm_missing
    rpt_missing_row[[paste0("arm", i, "_missing_pct")]] <- arm_missing_pct
  }

  # --- Return named list (SAS global macros -> return values) -----------------
  list(
    cd_output       = wide,
    all_sev         = sev_levels,
    sev_count       = sev_count,
    sev_names       = sev_names,
    rpt_missing_row = rpt_missing_row
  )
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    - SAS %ab/%cd macros -> R ae_ab()/ae_cd() parameterized functions
#    - arm_subjcnt vector includes total as last element, matching SAS
#      arm_subjcnt array layout: c(arm_1_n, arm_2_n, ..., arm_total)
#    - Severity ordering hardcoded: MILD=1, MODERATE=2, SEVERE=3,
#      LIFE THREATENING=4, FATAL=5, Other=6, Missing=100
#    - rpt_missing accumulation is done by the caller via
#      dplyr::bind_rows() across multiple ae_cd() calls, replacing
#      SAS's global rpt_missing dataset append (lines 244-247)
#    - Analysis A 2% filter: row removed only when ALL arm
#      percentages are <= 2% (SAS line 64: NOT all arm_pct_i <= 2)
#    - ae_cd sort: aebodsys is intentionally commented out in SAS
#      source (line 204); sort is by descending sum_total only
#    - SAS ds_base_bysubjpt contains one row per unique subject x
#      preferred-term; ae_ab() counts rows (dplyr::n()) as subject counts
#    - Missing values in aesev (NA, empty string, "MISSING") are
#      consolidated into a single severity level, matching SAS
#      first.order logic (lines 127-128)
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Percentage computation: 100 * count / total is identical to SAS
#    - Sort stability for descending arm_pct_total: dplyr::arrange()
#      is stable, matching SAS PROC SORT stability
#    - PROPCASE for severity: stringr::str_to_title() matches SAS
#      PROPCASE for standard terms; verify "LIFE THREATENING" maps to
#      "Life Threatening" (confirmed: str_to_title handles multi-word)
#    - Floating-point percentages stored as-is by default (no rounding);
#      caller specifies pct_digits for SAS-compatible output formatting
#
# NO DIRECT R EQUIVALENT:
#    - SAS RETAIN arrays -> dplyr::group_by() + dplyr::summarise()
#      + tidyr::pivot_wider() pipeline
#    - SAS hash lookup (all_sev) -> dplyr::left_join() with a
#      normalised severity lookup table
#    - SAS BY-group first.var / last.var -> implicit in dplyr
#      group operations (group_by + summarise collapses groups)
#    - SAS global macro variables (sev_count, sev_name_*) -> function
#      return values in a named list
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>=1.1.0): Core tidyverse data manipulation; AAP mandated
#      tidyverse over base R for all data wrangling operations
#    - tidyr (>=1.3.0): complete() ensures all arm x term/sev
#      combinations with fill=0, replacing SAS RETAIN zero-init;
#      pivot_wider() reshapes long -> wide replacing SAS array output
#    - stringr (>=1.5.0): str_to_title() for PROPCASE equivalent;
#      AAP mandates stringr over base R tolower/toupper
#    - janitor (>=2.2.0): round_half_up() for SAS-compatible rounding
#      (SAS rounds 0.5 up; R default banker's rounding); required by
#      AAP Gate 2 Rounding Audit
#    - tibble (>=3.2.0): Enhanced data frames; AAP mandates tibble
#      over base data.frame for intermediate/output data
#
# OPEN QUESTIONS:
#    - Severity ordering for non-standard values (order=6): confirm
#      that alphabetical sub-sort within order group is acceptable
#    - Analysis A filter threshold: SAS uses <= 2 (not < 2); row with
#      exactly 2.0% in all arms IS removed. Confirm this is intended.
#    - rpt_missing: confirm that the last severity in the catalog
#      reliably represents "Missing" when blank/MISSING aesev exist
#    - ae_cd sort omits aebodsys (commented out in SAS source line
#      204): confirm this is the intended production behaviour
# =============================================================================
