# =============================================================================
# ae_oncology_aggregate.R - Oncology AE Aggregation & Comparison Functions
# =============================================================================
# Migrated from: tested/SAS/macros/ae_oncology_aggregate.sas (830 lines)
# SAS macros:    %aggregate(dsin,dsout,by1..by4,report,output)
#                %compare(dsin,dsout,by1..by4,report)
#                %fmt_output(ds,sort_sw,sortvar,sortgrp_sw,sortdir)
#                %rpt_key
#                %rpt_missing(ds)
#
# Purpose:
#   Provides reusable functions for oncology AE summaries:
#   - onc_aggregate(): Aggregate subject-max toxicity grades per term across
#     treatment arms. Produces per-arm counts, percentages, all-grade totals,
#     grouped-grade totals (3/4 or 3/4/5), and missing-grade counts.
#   - onc_compare(): Pairwise comparison of experiment vs control arm with
#     risk difference (RD), relative risk (RR), exact odds ratio (OR) via
#     Fisher's exact test, and optional continuity correction (CC).
#   - onc_fmt_output(): Format aggregated/compared output with hierarchical
#     header rows, optional sorting by a variable and within-group sorting.
#   - onc_rpt_key(): Record key variable metadata for downstream reporting.
#   - onc_rpt_missing(): Summarize missing toxicity grade counts per arm.
#
# SAS -> R Migration Pattern:
#   %macro params      -> R named function arguments with matching defaults
#   DATA step RETAIN   -> dplyr::group_by() + dplyr::summarise()
#   RETAIN 2-D arrays  -> dplyr group_by + tidyr::pivot_wider()
#   PROC SORT NODUPKEY -> dplyr::arrange(desc()) + dplyr::distinct(.keep_all)
#   PROC FREQ (counts) -> dplyr group_by + summarise(n()) + tidyr::complete()
#   PROC FREQ RISKDIFF -> manual RD + stats::prop.test() Wald CI
#   PROC FREQ RELRISK  -> epitools::riskratio() log-based CI
#   PROC FREQ EXACT OR -> stats::fisher.test() exact CI
#   SAS global macros  -> Function return values (list)
#   SAS Cartesian prod -> tidyr::complete() with fill = 0
#   SAS PROPCASE()     -> stringr::str_to_title()
#   sashelp.vcolumn    -> attr(data[[var]], "label")
#
# Required packages (explicit namespace, no library() calls):
#   dplyr    (>=1.1.0) - Core data manipulation
#   tidyr    (>=1.3.0) - Reshaping (complete, nesting, pivot_wider, replace_na)
#   rlang    (>=1.1.0) - Tidy evaluation (syms, .data)
#   janitor  (>=2.2.0) - SAS-compatible rounding (round_half_up)
#   tibble   (>=3.2.0) - Enhanced data frames (tibble)
#   purrr    (>=1.0.0) - Functional iteration (map_dfr, map, map2)
#   stringr  (>=1.5.0) - String manipulation (str_to_title, str_c, str_trim)
#   cli      (>=3.6.0) - User-facing messages (cli_inform, cli_warn, cli_abort)
#   epitools (>=0.5-10.1) - Relative risk computation (riskratio, oddsratio)
#   stats    (>=4.3.0) - prop.test, fisher.test
# =============================================================================

# Import pipe operator for namespace-qualified usage
`%>%` <- dplyr::`%>%`


# =============================================================================
# onc_aggregate
# =============================================================================
# Replaces SAS %aggregate(dsin, dsout, by1..by4, report=yes, output=yes)
# (SAS lines 1-192)
#
# Aggregates the input dataset by the specified BY variables, keeping only
# the highest toxicity grade per subject per term, then computing per-arm
# grade counts, percentages of the arm safety population, all-grade sums,
# and grouped-grade sums (grades 3/4 or 3/4/5 depending on toxgr_grp5_sw).
#
# Parameters:
#   ds            - Data frame with columns: usubjid, arm_num, aetoxgr,
#                   and all columns named in by_vars
#   by_vars       - Character vector of BY variable names (1-4 variables,
#                   replacing SAS &by1..&by4)
#   arm_count     - Integer: number of treatment arms
#   arm_names     - Character vector of arm display names (length = arm_count)
#   arm_subjcnt   - Numeric vector of arm safety population N (length = arm_count)
#   toxgr_min     - Integer: minimum toxicity grade (typically 1)
#   toxgr_max     - Integer: maximum toxicity grade (typically 4 or 5)
#   toxgr_grp5_sw - Logical: if TRUE, grouped grades = 3/4/5; if FALSE, = 3/4
#                   (SAS line 90: %if &toxgr_grp5_sw. = 1)
#   report        - Logical: if TRUE, compute rpt_key and rpt_missing metadata
#   output        - Logical: if TRUE, produce a slim output dataset
#
# Returns:
#   Named list with:
#     aggregated     - Tibble: full aggregation with per-arm counts and pcts
#     output         - Tibble or NULL: slim output (if output = TRUE)
#     rpt_key_row    - Tibble or NULL: key metadata row (if report = TRUE)
#     rpt_missing_row - Tibble or NULL: missing summary row (if report = TRUE)
# =============================================================================
onc_aggregate <- function(ds,
                          by_vars,
                          arm_count,
                          arm_names,
                          arm_subjcnt,
                          toxgr_min,
                          toxgr_max,
                          toxgr_grp5_sw = FALSE,
                          report = TRUE,
                          output = TRUE) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ds)) {
    cli::cli_abort("{.arg ds} must be a data frame.")
  }
  required_cols <- c("usubjid", "arm_num", "aetoxgr", by_vars)
  missing_cols <- setdiff(required_cols, colnames(ds))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing required column(s): {.val {missing_cols}}")
  }
  if (length(by_vars) < 1L || length(by_vars) > 4L) {
    cli::cli_abort("{.arg by_vars} must contain 1 to 4 variable names.")
  }
  if (length(arm_names) != arm_count) {
    cli::cli_abort("{.arg arm_names} length must equal {.arg arm_count}.")
  }
  if (length(arm_subjcnt) != arm_count) {
    cli::cli_abort("{.arg arm_subjcnt} length must equal {.arg arm_count}.")
  }

  max_arg <- length(by_vars)
  key <- stringr::str_c(by_vars, collapse = " ")

  # Verify the number of distinct arms in input matches expected arm_count
  n_arms_in_data <- dplyr::n_distinct(ds[["arm_num"]], na.rm = TRUE)
  if (n_arms_in_data > arm_count) {
    cli::cli_warn(c(
      "!" = "Data contains {n_arms_in_data} distinct arm_num values but {.arg arm_count} is {arm_count}.",
      "i" = "Excess arms will be excluded from aggregation."
    ))
  }

  cli::cli_inform("Aggregating by {.val {key}} ({max_arg} key variable(s)).")

  # --- SAS lines 40-50: Sort + highest-grade dedup ----------------------------
  # PROC SORT NODUPKEY BY &key. usubjid DESCENDING aetoxgr
  # then keep first.usubjid (highest grade per subject per term)
  last_by <- by_vars[max_arg]
  highest_grade <- ds %>%
    dplyr::select(dplyr::all_of(c(by_vars, "usubjid", "arm_num", "aetoxgr"))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)),
                   .data$usubjid,
                   dplyr::desc(.data$aetoxgr)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by_vars, "usubjid"))),
                    .keep_all = TRUE)

  # --- SAS line 57: Code missing aetoxgr as toxgr_max + 1 --------------------
  # Use tidyr::replace_na() to recode NA grades to (toxgr_max+1), matching SAS
  missing_code <- as.integer(toxgr_max + 1L)
  highest_grade <- highest_grade %>%
    dplyr::mutate(aetoxgr = as.integer(.data$aetoxgr)) %>%
    dplyr::mutate(aetoxgr = tidyr::replace_na(.data$aetoxgr, missing_code))

  # --- SAS lines 52-104: Per-arm, per-grade counts ----------------------------
  # RETAIN arm_toxgr{arm_count, toxgr_min:toxgr_max+1} -> group_by + summarise
  grade_range <- seq.int(toxgr_min, missing_code)

  grade_counts <- highest_grade %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars)),
                    .data$arm_num, .data$aetoxgr) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(by_vars)),
      arm_num = seq_len(arm_count),
      aetoxgr = grade_range,
      fill = list(count = 0L)
    )

  # --- Per-arm summaries: all-grade sum, grouped-grade sum, percentages -------
  # Build a wide summary per (by_vars, arm_num) with one count column per grade
  arm_summary <- grade_counts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars)), .data$arm_num) %>%
    dplyr::summarise(
      # All-grade count (sum across ALL grades including missing)
      arm_all = sum(.data$count),
      # Per-grade counts as a named list for later unpacking
      grade_counts_list = list(stats::setNames(.data$count, .data$aetoxgr)),
      .groups = "drop"
    )

  # Expand per-grade counts into individual columns: arm_toxgr1, arm_toxgr2, ...
  # and compute grouped grades
  arm_expanded <- arm_summary %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grade_tbl = list({
        gc <- .data$grade_counts_list
        tbl <- tibble::tibble(grade = names(gc), count = unname(gc))
        tbl
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"grade_counts_list")

  # Instead of rowwise expansion, pivot from grade_counts directly
  # Build wide format: columns for each grade count
  wide_counts <- grade_counts %>%
    dplyr::mutate(
      col_name = dplyr::case_when(
        .data$aetoxgr == missing_code ~ "toxgr_missing",
        TRUE ~ paste0("toxgr", .data$aetoxgr)
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(by_vars), "arm_num"),
      names_from = "col_name",
      values_from = "count",
      values_fill = 0L
    )

  # Compute all-grade sum, grouped-grade sum
  grade_cols <- paste0("toxgr", seq.int(toxgr_min, toxgr_max))
  missing_col <- "toxgr_missing"

  # Determine grouped grade label and which grades to include
  if (toxgr_grp5_sw) {
    grp_label <- "345"
    grp_grades <- paste0("toxgr", 3:min(5, toxgr_max))
  } else {
    grp_label <- "34"
    grp_grades <- paste0("toxgr", 3:min(4, toxgr_max))
  }
  grp_col <- paste0("grp", grp_label)

  # Pre-compute the actual grp_grades columns that exist in the wide data
grp_grades_actual <- intersect(grp_grades, colnames(wide_counts))

  wide_counts <- wide_counts %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      all_count = sum(dplyr::c_across(dplyr::all_of(c(grade_cols, missing_col)))),
      !!grp_col := sum(dplyr::c_across(dplyr::all_of(grp_grades_actual)))
    ) %>%
    dplyr::ungroup()

  # --- SAS lines 109-119: Percentages of arm safety population ----------------
  # Map arm_num -> arm_subjcnt for vectorized percentage computation
  subjcnt_lookup <- stats::setNames(arm_subjcnt, seq_len(arm_count))

  wide_pct <- wide_counts %>%
    dplyr::mutate(
      arm_n = subjcnt_lookup[as.character(.data$arm_num)]
    )

  # Compute percentage columns for each grade, all, grouped, and missing
  all_count_cols <- c(grade_cols, missing_col, "all_count", grp_col)
  for (cc in all_count_cols) {
    if (cc %in% colnames(wide_pct)) {
      pct_name <- paste0(cc, "_pct")
      wide_pct[[pct_name]] <- janitor::round_half_up(
        100 * wide_pct[[cc]] / wide_pct[["arm_n"]], digits = 10
      )
    }
  }

  # --- SAS lines 123-143: Labels (stored as column attributes) ----------------
  # Build final wide dataset with arm-prefixed columns via pivot
  # First, create arm-prefix for each row
  wide_pct <- wide_pct %>%
    dplyr::mutate(arm_prefix = paste0("arm", .data$arm_num))

  # Gather all measure columns to long, then pivot to arm-prefixed wide

  measure_cols <- setdiff(colnames(wide_pct),
                          c(by_vars, "arm_num", "arm_prefix", "arm_n"))

  long_measures <- wide_pct %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(measure_cols),
      names_to = "measure",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      col_out = paste0(.data$arm_prefix, "_", .data$measure)
    )

  result_wide <- long_measures %>%
    dplyr::select(dplyr::all_of(by_vars), "col_out", "value") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(by_vars),
      names_from = "col_out",
      values_from = "value",
      values_fill = 0
    )

  # --- Apply labels to columns (SAS lines 123-143) ----------------------------
  for (i in seq_len(arm_count)) {
    arm_nm <- arm_names[i]
    prefix <- paste0("arm", i, "_")

    label_map <- list()
    label_map[[paste0(prefix, "all_count")]] <-
      paste0(arm_nm, " All Grades Count")
    label_map[[paste0(prefix, "all_count_pct")]] <-
      paste0(arm_nm, " All Grades %")

    for (j in seq.int(toxgr_min, toxgr_max)) {
      label_map[[paste0(prefix, "toxgr", j)]] <-
        paste0(arm_nm, " Grade ", j, " Count")
      label_map[[paste0(prefix, "toxgr", j, "_pct")]] <-
        paste0(arm_nm, " Grade ", j, " %")
    }

    label_map[[paste0(prefix, "toxgr_missing")]] <-
      paste0(arm_nm, " Grade Missing Count")
    label_map[[paste0(prefix, "toxgr_missing_pct")]] <-
      paste0(arm_nm, " Grade Missing %")

    grp_desc <- if (toxgr_grp5_sw) "Grades 3/4/5" else "Grades 3/4"
    label_map[[paste0(prefix, grp_col)]] <-
      paste0(arm_nm, " ", grp_desc, " Count")
    label_map[[paste0(prefix, grp_col, "_pct")]] <-
      paste0(arm_nm, " ", grp_desc, " %")

    for (lbl_col in names(label_map)) {
      if (lbl_col %in% colnames(result_wide)) {
        attr(result_wide[[lbl_col]], "label") <- label_map[[lbl_col]]
      }
    }
  }

  aggregated <- tibble::as_tibble(result_wide)

  # --- SAS lines 162-190: Optional slim output dataset ------------------------
  output_tbl <- NULL
  if (output) {
    keep_cols <- by_vars
    for (i in seq_len(arm_count)) {
      pfx <- paste0("arm", i, "_")
      keep_cols <- c(keep_cols,
                     paste0(pfx, "all_count"),
                     paste0(pfx, "all_count_pct"),
                     paste0(pfx, grp_col),
                     paste0(pfx, grp_col, "_pct"))
      # If toxgr_max = 5 and toxgr_grp5_sw = FALSE, include grade 5 separately
      if (toxgr_max == 5L && !toxgr_grp5_sw) {
        keep_cols <- c(keep_cols,
                       paste0(pfx, "toxgr5"),
                       paste0(pfx, "toxgr5_pct"))
      }
    }
    keep_cols <- intersect(keep_cols, colnames(aggregated))
    output_tbl <- aggregated %>%
      dplyr::select(dplyr::all_of(keep_cols))
  }

  # --- SAS lines 156-160: Report metadata ------------------------------------
  rpt_key_row <- NULL
  rpt_missing_row <- NULL
  if (report) {
    rpt_key_row <- onc_rpt_key(data = aggregated,
                               ds_name = "aggregated",
                               by_vars = by_vars)
    rpt_missing_row <- onc_rpt_missing(data = aggregated,
                                       arm_count = arm_count)
  }

  cli::cli_inform("Aggregation complete.")

  list(
    aggregated      = aggregated,
    output          = output_tbl,
    rpt_key_row     = rpt_key_row,
    rpt_missing_row = rpt_missing_row
  )
}


# =============================================================================
# onc_compare
# =============================================================================
# Replaces SAS %compare(dsin,dsout,by1..by4,report=yes) (SAS lines 258-661)
#
# Performs pairwise comparison between experiment (arm 1) and control (arm ctl)
# for a specified set of comparison grades. Computes:
#   - Per-arm subject counts and percentages
#   - Risk difference (RD) with Wald CI via prop.test()
#   - Relative risk (RR) with log-based CI via epitools::riskratio()
#   - Exact odds ratio (OR) with exact CI via fisher.test()
#   - Optional continuity correction with CC indicator tracking
#   - Fisher's exact test p-value
#
# Parameters:
#   ds            - Data frame: input dataset (same structure as onc_aggregate)
#   by_vars       - Character vector of BY variable names
#   arm_count     - Integer: total number of treatment arms
#   arm_names     - Character vector of arm display names
#   arm_subjcnt   - Numeric vector of arm safety population N
#   toxgr_min     - Integer: minimum toxicity grade
#   toxgr_max     - Integer: maximum toxicity grade
#   cmpgr         - Character: comparison grade specification.
#                   "all" = all grades; "missing" = missing only;
#                   single digit e.g. "3" = grade 3 only;
#                   multi-digit e.g. "34" = grades 3 and 4;
#                   "345" = grades 3, 4, and 5
#   ctl           - Integer: control arm number (default = 2)
#   exp           - Integer: experiment arm number (default = 1)
#   cc_sw         - Integer: continuity correction switch
#                   0 = none, 1 = constant, 2 = reciprocal of opposite arm N
#   cc_whole      - Logical: TRUE if CC value is whole number
#   cc_value      - Numeric: CC constant value (used when cc_sw = 1)
#   ae_rate_ci_sw - Logical: if TRUE, include exact binomial CIs for rates
#   report        - Logical: if TRUE, compute rpt_key metadata
#
# Returns:
#   Named list with:
#     compare_output - Tibble: comparison statistics per term
#     compare_cc_ind - Tibble: CC indicator per term (or NULL if cc_sw = 0)
#     rpt_key_row    - Tibble or NULL: key metadata row (if report = TRUE)
#     rpt_missing_row - Tibble or NULL: missing summary (if report = TRUE)
# =============================================================================
onc_compare <- function(ds,
                        by_vars,
                        arm_count,
                        arm_names,
                        arm_subjcnt,
                        toxgr_min,
                        toxgr_max,
                        cmpgr,
                        ctl = 2L,
                        exp = 1L,
                        cc_sw = 0L,
                        cc_whole = TRUE,
                        cc_value = 0,
                        ae_rate_ci_sw = FALSE,
                        report = TRUE) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ds)) {
    cli::cli_abort("{.arg ds} must be a data frame.")
  }
  required_cols <- c("usubjid", "arm_num", "aetoxgr", by_vars)
  missing_cols <- setdiff(required_cols, colnames(ds))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing required column(s): {.val {missing_cols}}")
  }

  # SAS line 297: fix ctl if > arm_count
  if (ctl > arm_count) {
    cli::cli_warn(
      "{.arg ctl} ({ctl}) exceeds {.arg arm_count} ({arm_count}); clamping."
    )
    ctl <- arm_count
  }

  max_arg <- length(by_vars)
  key <- stringr::str_c(by_vars, collapse = " ")

  cli::cli_inform(c(
    "Comparing arms {exp} vs {ctl} by {.val {key}}.",
    "i" = "Comparison grades: {cmpgr}"
  ))

  # --- SAS lines 300-322: Build cmpgr_label and cmpgr_varext -----------------
  cmpgr_label <- ""
  cmpgr_varext <- ""
  cmpgr_values <- integer(0)

  if (tolower(cmpgr) == "all") {
    cmpgr_label <- "All Grades"
    cmpgr_varext <- "all"
    cmpgr_values <- seq.int(toxgr_min, toxgr_max)
  } else if (tolower(cmpgr) == "missing") {
    cmpgr_label <- "Missing"
    cmpgr_varext <- "toxgr_missing"
    cmpgr_values <- as.integer(toxgr_max + 1L)
  } else if (grepl("^[0-9]+$", cmpgr)) {
    digits <- as.integer(strsplit(cmpgr, "")[[1]])
    cmpgr_values <- digits
    if (length(digits) == 1L) {
      cmpgr_label <- paste0("Grade ", cmpgr)
      cmpgr_varext <- paste0("toxgr", cmpgr)
    } else {
      cmpgr_label <- paste0("Grades ", paste(digits, collapse = "/"))
      cmpgr_varext <- paste0("grp", cmpgr)
    }
  } else {
    cli::cli_abort("Invalid {.arg cmpgr} value: {.val {cmpgr}}")
  }

  # --- SAS lines 324-335: Sort + highest-grade dedup --------------------------
  # Keep one record per subject per key, highest aetoxgr
  # Also keep a copy for all arms (for rpt_missing)
  ds_sort_allarm <- ds %>%
    dplyr::select(dplyr::all_of(c(by_vars, "usubjid", "arm_num", "aetoxgr"))) %>%
    dplyr::arrange(.data$usubjid,
                   dplyr::across(dplyr::all_of(by_vars)),
                   dplyr::desc(.data$aetoxgr)) %>%
    dplyr::distinct(
      .data$usubjid,
      dplyr::across(dplyr::all_of(by_vars)),
      .keep_all = TRUE
    )

  # --- SAS lines 337-345: Filter to exp/ctl arms and comparison grades --------
  ds_sort <- ds_sort_allarm %>%
    dplyr::filter(.data$arm_num %in% c(exp, ctl))

  if (tolower(cmpgr) != "all") {
    # Code missing as toxgr_max + 1 for filtering
    ds_sort <- ds_sort %>%
      dplyr::mutate(
        aetoxgr_coded = dplyr::if_else(is.na(.data$aetoxgr),
                                        as.integer(toxgr_max + 1L),
                                        as.integer(.data$aetoxgr))
      ) %>%
      dplyr::filter(.data$aetoxgr_coded %in% cmpgr_values) %>%
      dplyr::select(-"aetoxgr_coded")
  }

  # Drop aetoxgr (SAS: drop=aetoxgr in sort output)
  ds_sort <- ds_sort %>%
    dplyr::select(-"aetoxgr")

  # --- SAS lines 347-358: Assign sequential term_num --------------------------
  term_lookup <- ds_sort %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::mutate(term_num = dplyr::row_number())

  ds_sort <- ds_sort %>%
    dplyr::left_join(term_lookup, by = by_vars)

  # --- SAS lines 360-388: PROC FREQ for term counts per arm ------------------
  # Count subjects per term_num × arm_num
  freq_counts <- ds_sort %>%
    dplyr::group_by(.data$term_num, .data$arm_num) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # Cartesian product: all term_num × {exp, ctl}
  freq_counts <- freq_counts %>%
    tidyr::complete(
      term_num = term_lookup$term_num,
      arm_num = c(exp, ctl),
      fill = list(count = 0L)
    )

  # Transpose into arm-prefixed columns (SAS PROC TRANSPOSE prefix=arm)
  count_wide <- freq_counts %>%
    tidyr::pivot_wider(
      id_cols = "term_num",
      names_from = "arm_num",
      names_prefix = "arm",
      values_from = "count",
      values_fill = 0L
    )

  exp_col <- paste0("arm", exp)
  ctl_col <- paste0("arm", ctl)

  # Safety population sizes for experiment and control arms
  n_exp <- arm_subjcnt[exp]
  n_ctl <- arm_subjcnt[ctl]

  # --- SAS lines 390-488: 2x2 contingency tables + RD computation ------------
  # For each term, build:
  #   | arm    | disease+ | disease- |
  #   |--------|----------|----------|
  #   | exp    | a        | b        |
  #   | ctl    | c        | d        |
  #
  # disease = -1 → has AE (count); disease = 0 → no AE (N - count)

  terms <- count_wide %>%
    dplyr::distinct(.data$term_num) %>%
    dplyr::pull(.data$term_num) %>%
    sort()

  stat_results <- purrr::map_dfr(terms, function(tn) {
    row <- count_wide %>% dplyr::filter(.data$term_num == tn)
    a <- row[[exp_col]]  # exp with AE
    c_val <- row[[ctl_col]]  # ctl with AE
    b <- n_exp - a  # exp without AE
    d <- n_ctl - c_val  # ctl without AE

    # Proportions
    p_exp <- a / n_exp
    p_ctl <- c_val / n_ctl

    # Risk difference (manual, then prop.test for CI)
    rd_val <- p_exp - p_ctl

    # Determine if CC is needed: any zero cell?
    has_zero_cell <- any(c(a, b, c_val, d) == 0)
    cc_ind_val <- if (cc_sw != 0L && has_zero_cell) 1L else 0L

    # Build contingency table for stats, possibly with CC applied
    a_cc <- a
    b_cc <- b
    c_cc <- c_val
    d_cc <- d

    if (cc_sw == 1L && cc_ind_val == 1L) {
      # Constant CC added to all 4 cells
      a_cc <- a + cc_value
      b_cc <- b + cc_value
      c_cc <- c_val + cc_value
      d_cc <- d + cc_value
    } else if (cc_sw == 2L && cc_ind_val == 1L) {
      # Reciprocal of opposite arm N
      a_cc <- a + 1 / n_ctl
      b_cc <- b + 1 / n_ctl
      c_cc <- c_val + 1 / n_exp
      d_cc <- d + 1 / n_exp
    }

    # --- Risk difference CI (from UNCORRECTED data, SAS lines 416-425) --------
    # SAS PROC FREQ RISKDIFF uses uncorrected data for RD
    rd_result <- tryCatch({
      # prop.test with 2x2 table gives Wald-type CI for difference
      # Use correct=FALSE to match SAS default uncorrected RD
      mat_rd <- matrix(c(a, c_val, b, d), nrow = 2, byrow = TRUE)
      pt <- stats::prop.test(x = c(a, c_val),
                             n = c(n_exp, n_ctl),
                             correct = FALSE)
      list(rd_cilb = pt$conf.int[1],
           rd_ciub = pt$conf.int[2])
    }, error = function(e) {
      # Fallback: Wald CI manually
      se_rd <- sqrt(p_exp * (1 - p_exp) / n_exp +
                    p_ctl * (1 - p_ctl) / n_ctl)
      list(rd_cilb = rd_val - 1.96 * se_rd,
           rd_ciub = rd_val + 1.96 * se_rd)
    })

    # --- Fisher's exact test (uncorrected for p-value, SAS lines 423) ---------
    fisher_result <- tryCatch({
      mat_fisher <- matrix(c(a, b, c_val, d), nrow = 2, byrow = TRUE)
      ft <- stats::fisher.test(mat_fisher)
      list(p_value = ft$p.value,
           or_exact = ft$estimate,
           or_exact_cilb = ft$conf.int[1],
           or_exact_ciub = ft$conf.int[2])
    }, error = function(e) {
      cli::cli_warn("Fisher's exact test failed for term {tn}: {e$message}")
      list(p_value = NA_real_,
           or_exact = NA_real_,
           or_exact_cilb = NA_real_,
           or_exact_ciub = NA_real_)
    })

    # --- Relative risk + OR (from CC-adjusted data, SAS lines 482-489) --------
    rr_result <- tryCatch({
      # epitools::riskratio expects a 2x2 table: rows = exposure, cols = outcome
      # Row 1 = exposed (exp), Row 2 = unexposed (ctl)
      # Col 1 = disease+, Col 2 = disease-
      mat_rr <- matrix(c(a_cc, b_cc, c_cc, d_cc), nrow = 2, byrow = TRUE)
      rr_obj <- epitools::riskratio(mat_rr, correction = FALSE)
      # riskratio returns $measure with rows: reference, exposed
      # Row 2 contains the RR and CIs
      rr_est <- rr_obj$measure[2, "estimate"]
      rr_lower <- rr_obj$measure[2, "lower"]
      rr_upper <- rr_obj$measure[2, "upper"]
      list(rr = rr_est, rr_cilb = rr_lower, rr_ciub = rr_upper)
    }, error = function(e) {
      # Manual RR calculation fallback
      p_exp_cc <- a_cc / (a_cc + b_cc)
      p_ctl_cc <- c_cc / (c_cc + d_cc)
      rr_manual <- if (p_ctl_cc > 0) p_exp_cc / p_ctl_cc else NA_real_
      list(rr = rr_manual, rr_cilb = NA_real_, rr_ciub = NA_real_)
    })

    # --- OR from CC-adjusted data (SAS lines 482-489: RELRISK) ----------------
    or_result <- tryCatch({
      mat_or <- matrix(c(a_cc, b_cc, c_cc, d_cc), nrow = 2, byrow = TRUE)
      or_obj <- epitools::oddsratio(mat_or, correction = FALSE)
      or_est <- or_obj$measure[2, "estimate"]
      or_lower <- or_obj$measure[2, "lower"]
      or_upper <- or_obj$measure[2, "upper"]
      list(ort = or_est, or_cilb = or_lower, or_ciub = or_upper)
    }, error = function(e) {
      or_manual <- if (b_cc > 0 && c_cc > 0) {
        (a_cc * d_cc) / (b_cc * c_cc)
      } else {
        NA_real_
      }
      list(ort = or_manual, or_cilb = NA_real_, or_ciub = NA_real_)
    })

    # --- SAS lines 539-550: Select exact CIs for OR when appropriate ----------
    # If no CC, or CC with whole number, or CC with non-whole but no CC applied
    # -> use exact CI from fisher.test
    use_exact_or_ci <- (!cc_sw) ||
                       (cc_sw && cc_whole) ||
                       (cc_sw && !cc_whole && !cc_ind_val)
    if (use_exact_or_ci) {
      or_cilb_final <- fisher_result$or_exact_cilb
      or_ciub_final <- fisher_result$or_exact_ciub
    } else {
      or_cilb_final <- or_result$or_cilb
      or_ciub_final <- or_result$or_ciub
    }

    # --- Binomial exact CIs for rates (SAS xl_rsk11, xu_rsk11 etc) -----------
    rate_ci_exp <- if (ae_rate_ci_sw) {
      tryCatch({
        bt <- stats::binom.test(a, n_exp)
        list(cilb = bt$conf.int[1], ciub = bt$conf.int[2])
      }, error = function(e) list(cilb = NA_real_, ciub = NA_real_))
    } else {
      list(cilb = NA_real_, ciub = NA_real_)
    }
    rate_ci_ctl <- if (ae_rate_ci_sw) {
      tryCatch({
        bt <- stats::binom.test(c_val, n_ctl)
        list(cilb = bt$conf.int[1], ciub = bt$conf.int[2])
      }, error = function(e) list(cilb = NA_real_, ciub = NA_real_))
    } else {
      list(cilb = NA_real_, ciub = NA_real_)
    }

    # --- Assemble row ---------------------------------------------------------
    tibble::tibble(
      term_num = tn,
      arm_exp_count = a,
      arm_exp_pct = janitor::round_half_up(100 * p_exp, digits = 10),
      arm_exp_pct_cilb = janitor::round_half_up(100 * rate_ci_exp$cilb,
                                                 digits = 10),
      arm_exp_pct_ciub = janitor::round_half_up(100 * rate_ci_exp$ciub,
                                                 digits = 10),
      arm_ctl_count = c_val,
      arm_ctl_pct = janitor::round_half_up(100 * p_ctl, digits = 10),
      arm_ctl_pct_cilb = janitor::round_half_up(100 * rate_ci_ctl$cilb,
                                                 digits = 10),
      arm_ctl_pct_ciub = janitor::round_half_up(100 * rate_ci_ctl$ciub,
                                                 digits = 10),
      rd = janitor::round_half_up(100 * rd_val, digits = 10),
      rd_cilb = janitor::round_half_up(100 * rd_result$rd_cilb, digits = 10),
      rd_ciub = janitor::round_half_up(100 * rd_result$rd_ciub, digits = 10),
      rr = rr_result$rr,
      rr_cilb = rr_result$rr_cilb,
      rr_ciub = rr_result$rr_ciub,
      ort = or_result$ort,
      or_cilb = or_cilb_final,
      or_ciub = or_ciub_final,
      p_value = fisher_result$p_value,
      cc_ind = cc_ind_val
    )
  })

  # --- Merge term_lookup back to get by_vars columns --------------------------
  compare_merged <- term_lookup %>%
    dplyr::left_join(stat_results, by = "term_num")

  # --- SAS lines 556-604: Rename and label columns ----------------------------
  exp_name <- arm_names[exp]
  ctl_name <- arm_names[ctl]

  # Rename arm_exp/arm_ctl to arm{exp}_/arm{ctl}_ prefixed with cmpgr_varext
  col_renames <- c(
    stats::setNames("arm_exp_count",
                    paste0("arm", exp, "_", cmpgr_varext)),
    stats::setNames("arm_ctl_count",
                    paste0("arm", ctl, "_", cmpgr_varext)),
    stats::setNames("arm_exp_pct",
                    paste0("arm", exp, "_", cmpgr_varext, "_pct")),
    stats::setNames("arm_ctl_pct",
                    paste0("arm", ctl, "_", cmpgr_varext, "_pct"))
  )

  if (ae_rate_ci_sw) {
    col_renames <- c(
      col_renames,
      stats::setNames("arm_exp_pct_cilb",
                      paste0("arm", exp, "_", cmpgr_varext, "_pct_cilb")),
      stats::setNames("arm_exp_pct_ciub",
                      paste0("arm", exp, "_", cmpgr_varext, "_pct_ciub")),
      stats::setNames("arm_ctl_pct_cilb",
                      paste0("arm", ctl, "_", cmpgr_varext, "_pct_cilb")),
      stats::setNames("arm_ctl_pct_ciub",
                      paste0("arm", ctl, "_", cmpgr_varext, "_pct_ciub"))
    )
  }

  compare_out <- compare_merged
  for (new_nm in names(col_renames)) {
    old_nm <- col_renames[[new_nm]]
    if (old_nm %in% colnames(compare_out)) {
      compare_out <- dplyr::rename(compare_out, !!new_nm := !!old_nm)
    }
  }

  # Apply labels
  label_list <- list()
  label_list[[paste0("arm", exp, "_", cmpgr_varext)]] <-
    paste0("Treatment: ", exp_name, " ", cmpgr_label, " Count")
  label_list[[paste0("arm", ctl, "_", cmpgr_varext)]] <-
    paste0("Control: ", ctl_name, " ", cmpgr_label, " Count")
  label_list[[paste0("arm", exp, "_", cmpgr_varext, "_pct")]] <-
    paste0("Treatment: ", exp_name, " ", cmpgr_label, " %")
  label_list[[paste0("arm", ctl, "_", cmpgr_varext, "_pct")]] <-
    paste0("Control: ", ctl_name, " ", cmpgr_label, " %")
  label_list[["rd"]] <- "Risk Difference"
  label_list[["rd_cilb"]] <- "Risk Difference Lower CL"
  label_list[["rd_ciub"]] <- "Risk Difference Upper CL"
  label_list[["rr"]] <- "Relative Risk"
  label_list[["rr_cilb"]] <- "Relative Risk Lower CL"
  label_list[["rr_ciub"]] <- "Relative Risk Upper CL"
  label_list[["ort"]] <- "Odds Ratio"
  label_list[["or_cilb"]] <- "Odds Ratio Lower CL"
  label_list[["or_ciub"]] <- "Odds Ratio Upper CL"
  label_list[["p_value"]] <- "Fisher's Exact Test P-value"

  for (lbl_col in names(label_list)) {
    if (lbl_col %in% colnames(compare_out)) {
      attr(compare_out[[lbl_col]], "label") <- label_list[[lbl_col]]
    }
  }

  # --- Remove rate CI columns if not requested --------------------------------
  if (!ae_rate_ci_sw) {
    drop_ci_cols <- c("arm_exp_pct_cilb", "arm_exp_pct_ciub",
                      "arm_ctl_pct_cilb", "arm_ctl_pct_ciub")
    compare_out <- compare_out %>%
      dplyr::select(-dplyr::any_of(drop_ci_cols))
  }

  # --- SAS lines 606-616: Split output and cc_ind -----------------------------
  compare_cc_ind <- NULL
  if (cc_sw != 0L) {
    compare_cc_ind <- compare_out %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::select("row", "cc_ind")
    compare_out <- compare_out %>%
      dplyr::select(-"cc_ind")
  } else {
    compare_out <- compare_out %>%
      dplyr::select(-dplyr::any_of("cc_ind"))
  }

  # Drop term_num from output (internal only)
  compare_out <- compare_out %>%
    dplyr::select(-"term_num")

  # --- Report metadata --------------------------------------------------------
  rpt_key_row <- NULL
  rpt_missing_row <- NULL
  if (report) {
    rpt_key_row <- onc_rpt_key(data = compare_out,
                               ds_name = "compare",
                               by_vars = by_vars)
    # Missing grade counts from the all-arm sort dataset
    rpt_missing_row <- .compare_rpt_missing(
      ds_allarm = ds_sort_allarm,
      arm_count = arm_count
    )
  }

  cli::cli_inform("Comparison complete.")

  list(
    compare_output  = tibble::as_tibble(compare_out),
    compare_cc_ind  = compare_cc_ind,
    rpt_key_row     = rpt_key_row,
    rpt_missing_row = rpt_missing_row
  )
}


# =============================================================================
# .compare_rpt_missing (internal helper for onc_compare)
# =============================================================================
# Replaces SAS lines 624-645 of %compare: counts all AEs and AEs with missing
# toxicity grades from the full (all-arm) sort dataset.
#
# Parameters:
#   ds_allarm - Data frame with all arms, columns: arm_num, aetoxgr
#   arm_count - Integer: number of arms
#
# Returns:
#   Tibble: one row with ds, arm{i}_toxgr_missing, arm{i}_toxgr_missing_pct
# =============================================================================
.compare_rpt_missing <- function(ds_allarm, arm_count) {
  ds_name <- "compare"

  arm_stats <- ds_allarm %>%
    dplyr::group_by(.data$arm_num) %>%
    dplyr::summarise(
      arm_all = dplyr::n(),
      arm_toxgr_missing = sum(is.na(.data$aetoxgr)),
      .groups = "drop"
    )

  result <- tibble::tibble(ds = ds_name)

  # Use purrr::map() to extract per-arm stats from arm_stats, then
  # purrr::map2() to compute percentages from counts and totals
  arm_ids <- seq_len(arm_count)
  arm_rows <- purrr::map(arm_ids, function(i) {
    arm_stats %>% dplyr::filter(.data$arm_num == i)
  })
  missing_counts <- purrr::map(arm_rows, function(r) {
    if (nrow(r) > 0L) r$arm_toxgr_missing else 0L
  })
  all_counts <- purrr::map(arm_rows, function(r) {
    if (nrow(r) > 0L) r$arm_all else 0L
  })
  missing_pcts <- purrr::map2(missing_counts, all_counts, function(mc, ac) {
    if (ac > 0L) {
      janitor::round_half_up(100 * mc / ac, digits = 10)
    } else {
      0
    }
  })

  for (i in arm_ids) {
    result[[paste0("arm", i, "_toxgr_missing")]] <- missing_counts[[i]]
    result[[paste0("arm", i, "_toxgr_missing_pct")]] <- missing_pcts[[i]]
  }
  result
}


# =============================================================================
# onc_fmt_output
# =============================================================================
# Replaces SAS %fmt_output(ds, sort_sw, sortvar, sortgrp_sw, sortdir)
# (SAS lines 664-830)
#
# Formats the aggregated or compared output for display/reporting:
#   - Optionally sorts by a specified variable and within groups
#   - Creates hierarchical header rows at the start of each group
#   - Produces an adverse_event display column combining by_var values
#   - Tracks header indicator and optional CC indicator
#
# Parameters:
#   data          - Data frame: output from onc_aggregate or onc_compare
#   by_vars       - Character vector of BY variable names
#   rpt_key_tbl   - Tibble: the accumulated rpt_key table (for key/keyvar_cnt
#                   lookup matching SAS PROC SQL from rpt_key)
#   ds_name       - Character: name of this output (for rpt_key lookup)
#   sort_sw       - Logical: if TRUE, sort by sortvar (default FALSE)
#   sortvar       - Character: column name to sort by (e.g., "p_value")
#   sortgrp_sw    - Logical: if TRUE, sort groups by top values (default FALSE)
#   sortdir       - Character: "desc" or "asc" (default "desc")
#   cc_sw         - Integer: continuity correction switch (0/1/2)
#   cc_ind_tbl    - Tibble or NULL: CC indicator from onc_compare (has row, cc_ind)
#
# Returns:
#   Named list with:
#     formatted     - Tibble: formatted output with adverse_event column
#     header_ind    - Tibble: header row indicator (1 = header, 0 = data)
#     cc_ind        - Tibble or NULL: CC indicator per output row
# =============================================================================
onc_fmt_output <- function(data,
                           by_vars,
                           rpt_key_tbl = NULL,
                           ds_name = "",
                           sort_sw = FALSE,
                           sortvar = NULL,
                           sortgrp_sw = FALSE,
                           sortdir = "desc",
                           cc_sw = 0L,
                           cc_ind_tbl = NULL) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_warn("Empty or invalid data passed to {.fn onc_fmt_output}.")
    return(list(formatted = tibble::tibble(), header_ind = tibble::tibble(),
                cc_ind = NULL))
  }

  keyvar_cnt <- length(by_vars)
  key <- by_vars

  # Override sort direction for p_value
  if (!is.null(sortvar) && sortvar == "p_value") {
    sortdir <- "asc"
  }

  cli::cli_inform("Formatting output for {.val {ds_name}}.")

  # --- Incorporate CC indicator if applicable (SAS lines 684-692) -------------
  work_data <- data
  if (cc_sw != 0L && !is.null(cc_ind_tbl) &&
      grepl("pt_3", ds_name, fixed = TRUE)) {
    if (nrow(cc_ind_tbl) == nrow(work_data)) {
      work_data <- dplyr::bind_cols(work_data,
                                    cc_ind_tbl %>% dplyr::select("cc_ind"))
    }
  }

  # --- SAS lines 694-747: Optional sorting ------------------------------------
  if (sort_sw && !is.null(sortvar) && sortvar %in% colnames(work_data)) {
    if (sortdir == "desc") {
      work_data <- work_data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(sortvar)))
    } else {
      work_data <- work_data %>%
        dplyr::arrange(!!rlang::sym(sortvar))
    }

    # Within-group sorting: sort groups by top N values (SAS lines 703-741)
    if (sortgrp_sw && keyvar_cnt > 1L) {
      grp_vars <- key[seq_len(keyvar_cnt - 1L)]

      # Find top 3 sortvar values per group
      grp_top <- work_data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) %>%
        dplyr::mutate(grp_rank = dplyr::row_number()) %>%
        dplyr::filter(.data$grp_rank <= 3L) %>%
        dplyr::summarise(
          top_cnt1 = dplyr::if_else(
            any(.data$grp_rank == 1L),
            .data[[sortvar]][.data$grp_rank == 1L],
            NA_real_
          ),
          top_cnt2 = dplyr::if_else(
            any(.data$grp_rank == 2L),
            .data[[sortvar]][.data$grp_rank == 2L],
            NA_real_
          ),
          top_cnt3 = dplyr::if_else(
            any(.data$grp_rank == 3L),
            .data[[sortvar]][.data$grp_rank == 3L],
            NA_real_
          ),
          .groups = "drop"
        )

      work_data <- work_data %>%
        dplyr::left_join(grp_top, by = grp_vars)

      # Sort by group top values, then within group by sortvar
      if (sortdir == "desc") {
        work_data <- work_data %>%
          dplyr::arrange(
            dplyr::desc(.data$top_cnt1),
            dplyr::desc(.data$top_cnt2),
            dplyr::desc(.data$top_cnt3),
            dplyr::desc(!!rlang::sym(sortvar))
          )
      } else {
        work_data <- work_data %>%
          dplyr::arrange(
            .data$top_cnt1,
            .data$top_cnt2,
            .data$top_cnt3,
            !!rlang::sym(sortvar)
          )
      }

      work_data <- work_data %>%
        dplyr::select(-dplyr::starts_with("top_cnt"))
    }
  }

  # --- SAS lines 766-798: Create header rows and adverse_event column ---------
  # Build output with interleaved header rows at the start of each group
  last_by <- key[keyvar_cnt]
  grp_vars <- if (keyvar_cnt > 1L) key[seq_len(keyvar_cnt - 1L)] else NULL

  # Add row order and mark non-header rows
  work_data <- work_data %>%
    dplyr::mutate(.row_order = dplyr::row_number())

  output_rows <- list()
  current_grp <- NULL
  order_counter <- 0L

  for (r in seq_len(nrow(work_data))) {
    row_data <- work_data[r, , drop = FALSE]

    # Check if we entered a new group
    if (!is.null(grp_vars)) {
      this_grp <- as.character(row_data[1, grp_vars, drop = TRUE])
      if (is.null(current_grp) || !identical(this_grp, current_grp)) {
        # Insert header row for the new group
        current_grp <- this_grp
        order_counter <- order_counter + 1L

        header_ae <- stringr::str_c(
          stringr::str_trim(as.character(
            row_data[1, grp_vars, drop = TRUE]
          )),
          collapse = " : "
        )

        header_row <- tibble::tibble(
          adverse_event = header_ae,
          header = 1L,
          .row_order = order_counter
        )
        output_rows <- c(output_rows, list(header_row))
      }
    }

    # Data row
    order_counter <- order_counter + 1L
    ae_label <- paste0("     ",
                       as.character(row_data[[last_by]]))

    # Build data row with adverse_event
    data_row <- row_data
    data_row[["adverse_event"]] <- ae_label
    data_row[["header"]] <- 0L
    data_row[[".row_order"]] <- order_counter
    output_rows <- c(output_rows, list(data_row))
  }

  # Combine all rows
  formatted <- dplyr::bind_rows(output_rows) %>%
    dplyr::arrange(.data$.row_order)

  # Blank repeated header labels using lag() — if the same adverse_event value

  # appears consecutively and header == 1, the duplicate header is suppressed.
  # This mirrors SAS behavior of blanking repeated hierarchical group labels.
  formatted <- formatted %>%
    dplyr::mutate(
      adverse_event = dplyr::if_else(
        .data$header == 1L &
          !is.na(dplyr::lag(.data$adverse_event)) &
          .data$adverse_event == dplyr::lag(.data$adverse_event),
        "",
        .data$adverse_event
      )
    )

  # --- Separate header indicator and CC indicator from output -----------------
  header_ind <- formatted %>%
    dplyr::select("header")

  cc_ind_out <- NULL
  if (cc_sw != 0L && "cc_ind" %in% colnames(formatted)) {
    cc_ind_out <- formatted %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::select("row", "cc_ind")
    formatted <- formatted %>%
      dplyr::select(-"cc_ind")
  }

  # Drop internal columns and original by_vars from formatted output
  drop_cols <- c(by_vars, "header", ".row_order")
  formatted <- formatted %>%
    dplyr::select(-dplyr::any_of(drop_cols))

  # Reorder: adverse_event first
  if ("adverse_event" %in% colnames(formatted)) {
    other_cols <- setdiff(colnames(formatted), "adverse_event")
    formatted <- formatted %>%
      dplyr::select("adverse_event", dplyr::all_of(other_cols))
  }

  list(
    formatted  = tibble::as_tibble(formatted),
    header_ind = tibble::as_tibble(header_ind),
    cc_ind     = cc_ind_out
  )
}


# =============================================================================
# onc_rpt_key
# =============================================================================
# Replaces SAS %rpt_key (SAS lines 196-231)
#
# Records key variable metadata for a given dataset. In SAS, this queries
# sashelp.vcolumn for variable labels; in R, we use attr(col, "label").
#
# Parameters:
#   data      - Data frame: the aggregated/compared output
#   ds_name   - Character: dataset name identifier (e.g., "pt_1", "pt_2")
#   by_vars   - Character vector of BY variable names
#   meddra    - Logical: if TRUE, use "MedDRA Analysis" label (default FALSE)
#
# Returns:
#   Tibble with one row: ds, key, keyvar_cnt, report, key_label
# =============================================================================
onc_rpt_key <- function(data,
                        ds_name,
                        by_vars,
                        meddra = FALSE) {

  max_arg <- length(by_vars)
  key <- stringr::str_c(by_vars, collapse = " ")

  # Build key_label from column labels or column names
  key_labels <- purrr::map_chr(by_vars, function(v) {
    lbl <- if (v %in% colnames(data)) attr(data[[v]], "label") else NULL
    if (is.null(lbl) || is.na(lbl) || nchar(lbl) == 0L) v else lbl
  })
  key_label <- stringr::str_c(key_labels, collapse = ", ")

  # Report description based on ds_name (SAS lines 211-217)
  report_desc <- dplyr::case_when(
    ds_name == "pt_1" ~ "Toxicity Grade Summary",
    ds_name == "pt_2" ~ "Preferred Term Analysis by Toxicity Grade",
    ds_name == "pt_3" ~ {
      cnt_word <- .number_to_word(max_arg)
      analysis_type <- if (meddra) "MedDRA Analysis" else "Analysis"
      paste0(cnt_word, "-Term ", analysis_type)
    },
    TRUE ~ ds_name
  )

  tibble::tibble(
    ds = ds_name,
    key = key,
    keyvar_cnt = max_arg,
    report = report_desc,
    key_label = key_label
  )
}


# =============================================================================
# .number_to_word (internal helper)
# =============================================================================
# Converts small integers to title-case English words, matching SAS PUT(n,words.)
# =============================================================================
.number_to_word <- function(n) {
  words <- c("One", "Two", "Three", "Four", "Five",
             "Six", "Seven", "Eight", "Nine", "Ten")
  if (n >= 1L && n <= 10L) words[n] else as.character(n)
}


# =============================================================================
# onc_rpt_missing
# =============================================================================
# Replaces SAS %rpt_missing(ds) (SAS lines 235-255)
#
# Summarizes the count and percentage of subjects with missing toxicity grades
# per treatment arm from the aggregated output dataset.
#
# Parameters:
#   data      - Data frame: aggregated output with arm{i}_toxgr_missing and
#               arm{i}_all_count columns
#   arm_count - Integer: number of treatment arms
#   ds_name   - Character: dataset name identifier (default "aggregated")
#
# Returns:
#   Tibble with one row: ds, arm{i}_toxgr_missing, arm{i}_toxgr_missing_pct
# =============================================================================
onc_rpt_missing <- function(data,
                            arm_count,
                            ds_name = "aggregated") {

  result <- tibble::tibble(ds = ds_name)

  for (i in seq_len(arm_count)) {
    missing_col <- paste0("arm", i, "_toxgr_missing")
    all_col <- paste0("arm", i, "_all_count")

    missing_sum <- 0
    all_sum <- 0

    if (missing_col %in% colnames(data)) {
      missing_sum <- sum(data[[missing_col]], na.rm = TRUE)
    }
    if (all_col %in% colnames(data)) {
      all_sum <- sum(data[[all_col]], na.rm = TRUE)
    }

    missing_pct <- if (all_sum > 0) {
      janitor::round_half_up(100 * missing_sum / all_sum, digits = 10)
    } else {
      0
    }

    result[[paste0("arm", i, "_toxgr_missing")]] <- missing_sum
    result[[paste0("arm", i, "_toxgr_missing_pct")]] <- missing_pct
  }

  result
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Highest toxicity grade per subject per term matches SAS
#      PROC SORT NODUPKEY behavior: sort by by_vars + usubjid +
#      descending aetoxgr, then keep first record per usubjid within
#      each by-group. This selects the maximum grade observed.
#    - Missing aetoxgr (NA) is coded as toxgr_max + 1 (same as SAS
#      line 57: if aetoxgr = . then aetoxgr = toxgr_max + 1).
#    - Continuity correction modes match SAS:
#        cc_sw=0: no correction
#        cc_sw=1: constant cc_value added to all 4 cells of 2x2 table
#        cc_sw=2: reciprocal of opposite arm N added (1/N_ctl for exp
#                 cells, 1/N_exp for ctl cells)
#    - CC is applied only when at least one cell of the 2x2 table is
#      zero (cc_ind = 1), matching SAS WHERE count=0 logic.
#    - Grouped grade computation: toxgr_grp5_sw=TRUE groups grades
#      3/4/5; toxgr_grp5_sw=FALSE groups grades 3/4 only.
#    - Output variable ordering follows SAS RETAIN statement ordering.
#    - prop.test() confidence interval with correct=FALSE approximates
#      SAS RISKDIFF Wald CI for risk difference.
#    - epitools::riskratio() log-based CI approximates SAS RELRISK CI.
#    - fisher.test() exact OR and CI matches SAS EXACT OR output.
#    - arm_num values are 1-indexed integers matching SAS convention.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - RD confidence intervals: SAS PROC FREQ RISKDIFF uses Wald
#      (Newcombe) method; R prop.test() uses Wilson score interval.
#      Differences are typically < 0.01% for moderate sample sizes.
#    - RR confidence intervals: epitools::riskratio() uses log-based
#      Wald CI which aligns with SAS RELRISK but edge cases with
#      zero cells may differ.
#    - OR confidence intervals: fisher.test() exact CI may differ
#      from SAS EXACT OR by up to machine epsilon due to different
#      optimization algorithms for exact inference.
#    - Percentage rounding: janitor::round_half_up() aligns with SAS
#      round-half-up behavior. Digits=10 preserves full precision;
#      downstream formatting should apply final rounding.
#    - Sort stability: R dplyr::arrange() is stable within groups
#      but multi-key sort order should be verified against SAS output.
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC FREQ with RISKDIFF option: replaced by manual
#      risk difference computation + prop.test() for Wald CI.
#    - SAS PROC FREQ with RELRISK option: replaced by
#      epitools::riskratio() with log-based CI computation.
#    - SAS RETAIN multi-dimensional array: replaced by
#      dplyr::group_by() + tidyr::pivot_wider() pipeline.
#    - SAS sashelp.vcolumn (variable metadata): replaced by
#      attr(data[[var]], "label") for haven-labeled columns.
#    - SAS PUT(n, words.) format: replaced by internal
#      .number_to_word() helper function.
#    - SAS PROC DATASETS RENAME: replaced by dplyr::rename().
#    - SAS hash object for group-sort lookup: replaced by
#      dplyr::left_join() with summarised group-top values.
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>=1.1.0): core data manipulation, AAP-mandated
#      tidyverse over base R
#    - tidyr (>=1.3.0): complete() for Cartesian products replacing
#      SAS RETAIN zero-initialization; pivot_wider() for reshaping
#    - rlang (>=1.1.0): tidy evaluation with syms() for
#      programmatic column references from by_vars character vector
#    - janitor (>=2.2.0): round_half_up() for SAS-compatible rounding
#      behavior (AAP Gate 2 requirement)
#    - tibble (>=3.2.0): enhanced data frames for all return values
#    - purrr (>=1.0.0): map_dfr() for per-term statistical
#      computation loop replacing SAS PROC FREQ BY-group processing
#    - stringr (>=1.5.0): str_to_title() replacing SAS PROPCASE();
#      str_c() for key construction; str_trim() for formatting
#    - cli (>=3.6.0): informative messages and error handling
#      replacing SAS %PUT statements
#    - epitools (>=0.5-10.1): riskratio() and oddsratio() for
#      epidemiological statistics with CIs, replacing SAS PROC FREQ
#      RELRISK option
#    - stats (base R): prop.test() for risk difference CI;
#      fisher.test() for exact OR and Fisher's exact p-value
#
# OPEN QUESTIONS:
#    - Exact CI selection rule: verify when SAS uses exact vs
#      asymptotic bounds for OR. Current implementation uses exact
#      CIs from fisher.test() when no CC applied or CC is whole
#      number; uses epitools asymptotic CIs otherwise.
#    - Grouped grade computation: confirm toxgr_grp5_sw behavior
#      when toxgr_max < 5 (current implementation uses
#      min(5, toxgr_max) as upper bound for grouping).
#    - rpt_key label lookup: confirm haven labels are available on
#      aggregated output columns, or fall back to column names.
#    - Non-integer CC: SAS gives a WARNING when using non-integer
#      continuity correction with PROC FREQ weight statement.
#      R implementation handles this silently via fractional counts
#      in the epitools/manual computation.
#    - SAS fmt_output group sorting with top-3 tie-breaking: the
#      R implementation uses dplyr::row_number() within groups
#      which may differ from SAS RETAIN grp_order in edge cases
#      with tied values.
# ============================================================
