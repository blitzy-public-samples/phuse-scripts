# ============================================================
# ae_oncology_aggregate.R
# ============================================================
# Migrated from: contributed/AE/AE_Toxicity/ae_oncology_aggregate.sas
# Purpose:       Aggregation and comparison of oncology AE toxicity data.
#                Provides per-arm toxicity grade aggregation, pairwise
#                comparison with risk difference / relative risk / odds ratio /
#                Fisher's exact test, metadata tracking, and output formatting.
# SAS macros:    %aggregate, %rpt_key, %rpt_missing, %compare, %fmt_output
# ============================================================

# --- Required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(janitor)
library(cli)
library(rlang)
library(purrr)

# --- Internal helpers --------------------------------------------------------

#' Convert small integer to title-case English word (SAS put(n, words.))
#' @param n integer (1-10)
#' @return character English word form
number_to_word <- function(n) {
  words <- c("One", "Two", "Three", "Four", "Five",
             "Six", "Seven", "Eight", "Nine", "Ten")
  if (is.numeric(n) && n >= 1 && n <= length(words)) words[n] else as.character(n)
}

#' Safe wrapper around binom.test for Clopper-Pearson exact CI
#' Returns c(lower, upper); handles n == 0 gracefully.
safe_binom_ci <- function(x, n) {
  if (is.na(x) || is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  x <- max(0L, min(as.integer(round(x)), as.integer(round(n))))
  stats::binom.test(x, as.integer(round(n)))$conf.int[1:2]
}

#' Safe wrapper around fisher.test
#' Returns list with p.value and conf.int; tolerates degenerate tables.
safe_fisher <- function(a, c_val, b, d) {
  mat <- matrix(c(as.integer(round(a)), as.integer(round(c_val)),
                   as.integer(round(b)), as.integer(round(d))), nrow = 2)
  tryCatch(
    stats::fisher.test(mat),
    error = function(e) list(p.value = NA_real_,
                             conf.int = c(NA_real_, NA_real_),
                             estimate = NA_real_)
  )
}

# =============================================================================
# aggregate_ae — Migrate SAS %aggregate (lines 5-192)
# =============================================================================
#' Aggregate oncology AE data by toxicity grade per arm
#'
#' @param dsin        Data frame with at minimum: by_vars columns, usubjid,
#'                    arm_num, aetoxgr.
#' @param dsout_name  Character label for output (e.g. "pt_1", "pt_2").
#' @param by_vars     Character vector of BY variable names.
#' @param arm_count   Integer number of treatment arms.
#' @param arm_subjcnt Numeric vector of subject counts per arm (length arm_count).
#' @param arm_names   Character vector of arm display names (length arm_count).
#' @param toxgr_min   Minimum toxicity grade (default 1).
#' @param toxgr_max   Maximum toxicity grade (default 5).
#' @param toxgr_grp5_sw 1 = group grades 3/4/5; 0 = group grades 3/4 (default 1).
#' @param meddra      "Y" or "N" — passed to rpt_key for report label.
#' @param report      Logical; generate rpt_key / rpt_missing metadata.
#' @param output      Logical; generate a column-subset output data frame.
#' @return Named list: data, output, rpt_key, rpt_missing.
aggregate_ae <- function(dsin, dsout_name, by_vars,
                         arm_count, arm_subjcnt, arm_names,
                         toxgr_min = 1L, toxgr_max = 5L,
                         toxgr_grp5_sw = 1L,
                         meddra = "N",
                         report = TRUE, output = TRUE) {

  # --- 1. Count BY variables and build key (SAS lines 8-21) -----------------
  max_arg   <- length(by_vars)
  key       <- paste(by_vars, collapse = " ")
  toxgr_grp <- if (toxgr_grp5_sw == 1) "345" else "34"

  cli::cli_alert_info("AGGREGATING {dsout_name} BY {key}")


  # --- 2. Sort & deduplicate (SAS lines 40-50) ------------------------------
  #   Keep highest AETOXGR per subject within each BY group.
  sorted_data <- dsin %>%
    dplyr::select(dplyr::all_of(c(by_vars, "usubjid", "arm_num", "aetoxgr"))) %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(by_vars)),
      .data$usubjid,
      dplyr::desc(.data$aetoxgr)
    ) %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(by_vars, "usubjid"))),
      .keep_all = TRUE
    )

  # --- 3. Code missing values as toxgr_max + 1 (SAS line 57) ----------------
  missing_code <- as.numeric(toxgr_max + 1L)
  sorted_data <- sorted_data %>%
    dplyr::mutate(
      aetoxgr = dplyr::if_else(is.na(.data$aetoxgr), missing_code,
                                as.numeric(.data$aetoxgr))
    )

  # --- 4. Count per arm × grade within each BY group (SAS lines 52-120) -----
  grade_levels <- c(seq(toxgr_min, toxgr_max), missing_code)

  counts <- sorted_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars)),
                    .data$arm_num, .data$aetoxgr) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(by_vars)),
      arm_num  = seq_len(arm_count),
      aetoxgr  = grade_levels,
      fill     = list(n = 0L)
    )

  # Create wide-format column name for each (arm, grade)
  counts <- counts %>%
    dplyr::mutate(
      col_name = dplyr::if_else(
        .data$aetoxgr == missing_code,
        paste0("arm", .data$arm_num, "_toxgr_missing"),
        paste0("arm", .data$arm_num, "_toxgr", as.integer(.data$aetoxgr))
      )
    )

  wide <- counts %>%
    dplyr::select(dplyr::all_of(by_vars), "col_name", "n") %>%
    tidyr::pivot_wider(names_from = "col_name", values_from = "n",
                       values_fill = 0L)

  # --- 5. Compute aggregate sums and percentages (SAS lines 88-118) ---------
  for (i in seq_len(arm_count)) {
    grade_cols   <- paste0("arm", i, "_toxgr", seq(toxgr_min, toxgr_max))
    missing_col  <- paste0("arm", i, "_toxgr_missing")
    all_col      <- paste0("arm", i, "_all")
    grp_col      <- paste0("arm", i, "_grp", toxgr_grp)

    # Ensure all expected columns exist
    for (gc in c(grade_cols, missing_col)) {
      if (!gc %in% names(wide)) wide[[gc]] <- 0L
    }

    # All grades = sum of grade + missing counts
    wide[[all_col]] <- rowSums(wide[, c(grade_cols, missing_col), drop = FALSE])

    # Grouped grades (3/4/5 or 3/4)
    grp_end <- if (toxgr_grp5_sw == 1) min(5L, toxgr_max) else min(4L, toxgr_max)
    grp_grades <- paste0("arm", i, "_toxgr", seq(3L, grp_end))
    existing_grp <- grp_grades[grp_grades %in% names(wide)]
    wide[[grp_col]] <- if (length(existing_grp) > 0) {
      rowSums(wide[, existing_grp, drop = FALSE])
    } else {
      0L
    }

    # Percentages (SAS lines 110-118)
    subjcnt_i <- arm_subjcnt[[i]]
    wide[[paste0(all_col, "_pct")]]     <- 100 * wide[[all_col]]     / subjcnt_i
    wide[[paste0(grp_col, "_pct")]]     <- 100 * wide[[grp_col]]     / subjcnt_i
    wide[[paste0(missing_col, "_pct")]] <- 100 * wide[[missing_col]] / subjcnt_i
    for (j in seq(toxgr_min, toxgr_max)) {
      cn <- paste0("arm", i, "_toxgr", j)
      if (cn %in% names(wide)) {
        wide[[paste0(cn, "_pct")]] <- 100 * wide[[cn]] / subjcnt_i
      }
    }
  }

  # --- 6. Apply labels (SAS lines 123-143) -----------------------------------
  grp_lab <- if (toxgr_grp5_sw == 1) "3/4/5" else "3/4"
  # Build per-arm label pairs (col_name, label) using purrr::map2
  arm_indices <- seq_len(arm_count)
  arm_nms     <- purrr::map(arm_indices, ~ arm_names[[.x]])
  label_pairs <- purrr::map2(arm_indices, arm_nms, function(i, arm_nm) {
    grade_pairs <- purrr::map(seq(toxgr_min, toxgr_max), function(j) {
      list(
        c(paste0("arm", i, "_toxgr", j),       paste0(arm_nm, " Grade ", j, " Count")),
        c(paste0("arm", i, "_toxgr", j, "_pct"), paste0(arm_nm, " Grade ", j, " %"))
      )
    })
    c(
      list(
        c(paste0("arm", i, "_all"),     paste0(arm_nm, " All Grades Count")),
        c(paste0("arm", i, "_all_pct"), paste0(arm_nm, " All Grades %"))
      ),
      unlist(grade_pairs, recursive = FALSE),
      list(
        c(paste0("arm", i, "_toxgr_missing"),     paste0(arm_nm, " Grade Missing Count")),
        c(paste0("arm", i, "_toxgr_missing_pct"), paste0(arm_nm, " Grade Missing %")),
        c(paste0("arm", i, "_grp", toxgr_grp),       paste0(arm_nm, " Grades ", grp_lab, " Count")),
        c(paste0("arm", i, "_grp", toxgr_grp, "_pct"), paste0(arm_nm, " Grades ", grp_lab, " %"))
      )
    )
  })
  # Flatten and apply
  for (pair in unlist(label_pairs, recursive = FALSE)) {
    col_nm <- pair[1]; lbl_val <- pair[2]
    if (col_nm %in% names(wide)) attr(wide[[col_nm]], "label") <- lbl_val
  }

  # --- 7. Select & order columns (SAS lines 145-149) -------------------------
  keep_cols <- by_vars
  for (i in seq_len(arm_count)) {
    keep_cols <- c(keep_cols,
      paste0("arm", i, "_all"),     paste0("arm", i, "_all_pct"),
      paste0("arm", i, "_grp", toxgr_grp), paste0("arm", i, "_grp", toxgr_grp, "_pct"),
      paste0("arm", i, "_toxgr", seq(toxgr_min, toxgr_max)),
      paste0("arm", i, "_toxgr", seq(toxgr_min, toxgr_max), "_pct"),
      paste0("arm", i, "_toxgr_missing"), paste0("arm", i, "_toxgr_missing_pct"))
  }
  keep_cols   <- keep_cols[keep_cols %in% names(wide)]
  data_result <- wide[, keep_cols, drop = FALSE]

  # --- 8. Output version (SAS lines 162-189) ---------------------------------
  output_result <- NULL
  if (output) {
    out_cols <- by_vars
    for (i in seq_len(arm_count)) {
      out_cols <- c(out_cols,
        paste0("arm", i, "_all"),     paste0("arm", i, "_all_pct"),
        paste0("arm", i, "_grp", toxgr_grp), paste0("arm", i, "_grp", toxgr_grp, "_pct"))
      if (toxgr_max == 5 && toxgr_grp5_sw == 0) {
        out_cols <- c(out_cols,
          paste0("arm", i, "_toxgr5"), paste0("arm", i, "_toxgr5_pct"))
      }
    }
    out_cols      <- out_cols[out_cols %in% names(data_result)]
    output_result <- data_result[, out_cols, drop = FALSE]
  }

  # --- 9. Reporting metadata (SAS lines 156-160) -----------------------------
  rpt_key_result     <- NULL
  rpt_missing_result <- NULL
  if (report) {
    rpt_key_result <- rpt_key(
      dsout_name = dsout_name, key = key, max_arg = max_arg,
      meddra = meddra, dsin = dsin
    )
    rpt_missing_result <- rpt_missing(
      ds = data_result, ds_name = dsout_name, arm_count = arm_count
    )
  }

  list(data        = data_result,
       output      = output_result,
       rpt_key     = rpt_key_result,
       rpt_missing = rpt_missing_result)
}

# =============================================================================
# rpt_key — Migrate SAS %rpt_key (lines 196-231)
# =============================================================================
#' Build report key metadata row describing an analysis dataset
#'
#' @param dsout_name        Character label for the dataset (e.g. "pt_1").
#' @param key               Character key string (space-separated by-var names).
#' @param max_arg           Integer count of BY variables.
#' @param meddra            "Y" / "N" — include "MedDRA" in the report label.
#' @param dsin              Source data frame (used to look up variable labels).
#' @param existing_rpt_key  Optional existing rpt_key tibble to append to.
#' @return Tibble with columns: ds, key, keyvar_cnt, report, key_label.
rpt_key <- function(dsout_name, key, max_arg, meddra = "N",
                    dsin = NULL, existing_rpt_key = NULL) {

  keyvar_cnt <- max_arg

  # --- Build report description (SAS lines 211-216) --------------------------
  report_desc <- switch(
    dsout_name,
    "pt_1" = "Toxicity Grade Summary",
    "pt_2" = "Preferred Term Analysis by Toxicity Grade",
    "pt_3" = {
      word_cnt  <- number_to_word(keyvar_cnt)
      meddra_lbl <- if (toupper(meddra) == "Y") "MedDRA " else ""
      paste0(word_cnt, "-Term ", meddra_lbl, "Analysis")
    },
    dsout_name
  )

  # --- Build key_label (SAS line 219) ----------------------------------------
  #   Comma-separated variable labels (or variable names if labels absent).
  by_var_names <- strsplit(trimws(key), "\\s+")[[1]]
  labels <- purrr::map_chr(by_var_names, function(v) {
    lbl <- if (!is.null(dsin) && v %in% names(dsin)) {
      attr(dsin[[v]], "label")
    } else {
      NULL
    }
    if (is.null(lbl) || !nzchar(lbl)) v else lbl
  })
  key_label <- paste(labels, collapse = ", ")

  # --- Build result row ------------------------------------------------------
  new_row <- dplyr::tibble(
    ds         = dsout_name,
    key        = key,
    keyvar_cnt = keyvar_cnt,
    report     = report_desc,
    key_label  = key_label
  )

  # --- Append to existing (SAS lines 223-227) --------------------------------
  if (!is.null(existing_rpt_key)) {
    dplyr::bind_rows(existing_rpt_key, new_row)
  } else {
    new_row
  }
}

# =============================================================================
# rpt_missing — Migrate SAS %rpt_missing (lines 235-255)
# =============================================================================
#' Summarise missing toxicity grade counts across the aggregated dataset
#'
#' @param ds                   Aggregated data frame from aggregate_ae().
#' @param ds_name              Character label for this dataset.
#' @param arm_count            Integer number of arms.
#' @param existing_rpt_missing Optional existing rpt_missing tibble to append to.
#' @return Tibble with columns: ds, arm{i}_toxgr_missing, arm{i}_toxgr_missing_pct
rpt_missing <- function(ds, ds_name, arm_count,
                        existing_rpt_missing = NULL) {

  # --- Sum missing counts and compute percentage (SAS lines 237-244) ----------
  # Build per-arm columns using purrr::map_dfc for column-wise construction
  arm_cols <- purrr::map_dfc(seq_len(arm_count), function(i) {
    miss_col <- paste0("arm", i, "_toxgr_missing")
    all_col  <- paste0("arm", i, "_all")

    miss_sum <- if (miss_col %in% names(ds)) sum(ds[[miss_col]], na.rm = TRUE) else 0
    all_sum  <- if (all_col  %in% names(ds)) sum(ds[[all_col]],  na.rm = TRUE) else 0

    pct_val <- if (all_sum > 0) 100 * miss_sum / all_sum else 0
    dplyr::tibble(
      !!miss_col := miss_sum,
      !!paste0(miss_col, "_pct") := pct_val
    )
  })
  new_row <- dplyr::bind_cols(dplyr::tibble(ds = ds_name), arm_cols)

  # --- Append to existing (SAS lines 247-251) --------------------------------
  if (!is.null(existing_rpt_missing)) {
    dplyr::bind_rows(existing_rpt_missing, new_row)
  } else {
    new_row
  }
}

# =============================================================================
# compare_ae — Migrate SAS %compare (lines 260-661)
# =============================================================================
#' Pairwise comparison of oncology AE rates between two arms
#'
#' Computes risk difference, relative risk, odds ratio, Fisher's exact test,
#' and Clopper-Pearson exact CIs for each unique BY-group term. Supports
#' continuity correction for zero-cell terms.
#'
#' @param dsin           Data frame (ds_base or ds_base_meddra).
#' @param dsout_name     Character label for output (e.g. "pt_3").
#' @param by_vars        Character vector of BY variable names.
#' @param exp            Integer arm number for the experimental arm.
#' @param ctl            Integer arm number for the control arm.
#' @param arm_count      Integer total number of arms.
#' @param arm_subjcnt    Numeric vector of per-arm subject counts.
#' @param arm_names      Character vector of arm display names.
#' @param cmpgr          Comparison grade code ("all", "34", "345", "5",
#'                       "missing" or any digit string).
#' @param cc             Numeric continuity correction constant (default 0).
#' @param cc_sw          0 = no CC; 1 = constant CC; 2 = reciprocal CC.
#' @param cc_whole       1 if cc is a whole number; 0 otherwise.
#' @param ae_rate_ci_sw  1 = include Clopper-Pearson CIs in output.
#' @param toxgr_grp5_sw  1 = include grade 5 in grouping (345); 0 = 34 only.
#' @param meddra         "Y"/"N".
#' @param report         Logical; generate rpt_key / rpt_missing.
#' @param all_arm        Optional data frame with arm_num column for Cartesian
#'                       product construction; if NULL, uses c(exp, ctl).
#' @return Named list: data, output, output_cc_ind, rpt_key, rpt_missing.
compare_ae <- function(dsin, dsout_name, by_vars,
                       exp, ctl, arm_count, arm_subjcnt, arm_names,
                       cmpgr = "all", cc = 0, cc_sw = 0, cc_whole = 1,
                       ae_rate_ci_sw = 1, toxgr_grp5_sw = 1,
                       meddra = "N", report = TRUE,
                       all_arm = NULL) {

  # --- 1. Count BY variables and build key (SAS lines 262-276) ---------------
  max_arg <- length(by_vars)
  key     <- paste(by_vars, collapse = " ")
  cli::cli_alert_info("COMPARING {dsout_name} BY {key}  exp={exp} ctl={ctl} cmpgr={cmpgr}")

  # Coerce cc to numeric (SAS macro parameter is string)
  cc <- as.numeric(cc)

  # --- 2. Fix control arm (SAS line 297) -------------------------------------
  if (ctl > arm_count) ctl <- arm_count

  # --- 3. Build cmpgr label & varext (SAS lines 300-322) ---------------------
  cmpgr_is_digits <- grepl("^[0-9]+$", cmpgr)
  if (cmpgr == "all") {
    cmpgr_label  <- "All Grades"
    cmpgr_varext <- "all"
  } else if (cmpgr == "missing") {
    cmpgr_label  <- "Missing"
    cmpgr_varext <- "toxgr_missing"
  } else if (cmpgr_is_digits) {
    digits <- strsplit(cmpgr, "")[[1]]
    if (nchar(cmpgr) == 1L) {
      cmpgr_label  <- paste("Grade", cmpgr)
      cmpgr_varext <- paste0("toxgr", cmpgr)
    } else {
      cmpgr_label  <- paste("Grades", paste(digits, collapse = "/"))
      cmpgr_varext <- paste0("grp", cmpgr)
    }
  } else {
    cmpgr_label  <- cmpgr
    cmpgr_varext <- cmpgr
  }

  # --- 4. Sort & dedup — keep highest AETOXGR per subject-term (SAS 325-335)-
  sorted_data <- dsin %>%
    dplyr::select(dplyr::all_of(c(by_vars, "usubjid", "arm_num", "aetoxgr"))) %>%
    dplyr::arrange(.data$usubjid, dplyr::across(dplyr::all_of(by_vars)),
                   dplyr::desc(.data$aetoxgr)) %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c("usubjid", by_vars))),
      .keep_all = TRUE
    )

  # Preserve all-arm version for rpt_missing (SAS line 334)
  sorted_allarm <- sorted_data

  # --- 5. Filter to exp/ctl arms & comparison grades (SAS lines 339-345) -----
  filtered <- sorted_data %>%
    dplyr::filter(.data$arm_num %in% c(exp, ctl))

  # Apply grade filter if cmpgr is numeric
  if (cmpgr_is_digits) {
    grade_filter <- as.integer(strsplit(cmpgr, "")[[1]])
    filtered <- filtered %>%
      dplyr::filter(.data$aetoxgr %in% grade_filter)
  }

  filtered <- filtered %>%
    dplyr::select(-"aetoxgr") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)))

  # --- 6. Assign sequential term numbers (SAS lines 348-358) -----------------
  if (nrow(filtered) == 0L) {
    # Edge case: no data after filtering — return empty results
    cli::cli_warn("No data remain after filtering for compare_ae({dsout_name}).")
    return(list(data = dplyr::tibble(), output = dplyr::tibble(),
                output_cc_ind = NULL, rpt_key = NULL, rpt_missing = NULL))
  }

  # Assign term_num via cur_group_id() grouped by BY variables
  filtered <- filtered %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::mutate(term_num = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  term_lookup <- filtered %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by_vars, "term_num"))))

  # --- 7. Frequency counts per arm (SAS lines 361-388) -----------------------
  freq_counts <- filtered %>%
    dplyr::count(term_num, arm_num, name = "count") %>%
    tidyr::complete(
      term_num = term_lookup$term_num,
      arm_num  = c(exp, ctl),
      fill     = list(count = 0L)
    )

  # Pivot to wide: arm{exp} and arm{ctl} columns
  freq_wide <- freq_counts %>%
    dplyr::mutate(arm_label = paste0("arm", .data$arm_num)) %>%
    dplyr::select("term_num", "arm_label", "count") %>%
    tidyr::pivot_wider(names_from = "arm_label", values_from = "count",
                       values_fill = 0L)

  # --- 8. Build 2x2 contingency table per term (SAS lines 392-414) -----------
  exp_col <- paste0("arm", exp)
  ctl_col <- paste0("arm", ctl)
  n_exp   <- arm_subjcnt[[exp]]
  n_ctl   <- arm_subjcnt[[ctl]]

  ct <- freq_wide %>%
    dplyr::mutate(
      a = .data[[exp_col]],                 # exp, disease
      b = n_exp - .data[[exp_col]],         # exp, no disease
      c_val = .data[[ctl_col]],             # ctl, disease
      d = n_ctl - .data[[ctl_col]]          # ctl, no disease
    )

  # --- 9. Risk difference & Fisher's exact from ORIGINAL counts (SAS 419-425)
  z_alpha <- stats::qnorm(0.975)

  rd_stats <- ct %>%
    dplyr::select("term_num", "a", "b", "c_val", "d") %>%
    purrr::pmap_dfr(function(term_num, a, b, c_val, d) {
      n1 <- a + b
      n2 <- c_val + d
      p1 <- if (n1 > 0) a / n1 else NA_real_
      p2 <- if (n2 > 0) c_val / n2 else NA_real_

      # Risk difference (Wald asymptotic CI)
      rd <- p1 - p2
      se_rd <- if (!is.na(p1) && !is.na(p2) && n1 > 0 && n2 > 0) {
        sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      } else {
        NA_real_
      }
      l_rdif1 <- rd - z_alpha * se_rd
      u_rdif1 <- rd + z_alpha * se_rd

      # Clopper-Pearson exact CIs for arm proportions
      ci_p1 <- safe_binom_ci(a, n1)
      ci_p2 <- safe_binom_ci(c_val, n2)

      # Fisher's exact p-value
      ft <- safe_fisher(a, c_val, b, d)

      dplyr::tibble(
        term_num  = term_num,
        rsk11     = p1,
        rsk21     = p2,
        rdif1     = rd,
        l_rdif1   = l_rdif1,
        u_rdif1   = u_rdif1,
        xl_rsk11  = ci_p1[1],
        xu_rsk11  = ci_p1[2],
        xl_rsk21  = ci_p2[1],
        xu_rsk21  = ci_p2[2],
        xp2_fish  = ft$p.value
      )
    })

  # --- 10. Continuity correction (SAS lines 430-472) -------------------------
  ct <- ct %>%
    dplyr::mutate(
      cc_ind = dplyr::if_else(.data$a == 0 | .data$b == 0 |
                               .data$c_val == 0 | .data$d == 0, 1L, 0L)
    )

  # CC-adjusted counts for RR/OR computation
  ct_cc <- ct
  if (cc_sw == 1) {
    # Constant correction: add cc to all cells of terms with zero cells
    ct_cc <- ct_cc %>%
      dplyr::mutate(
        a_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$a     + cc, as.numeric(.data$a)),
        b_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$b     + cc, as.numeric(.data$b)),
        c_val_cc = dplyr::if_else(.data$cc_ind == 1L, .data$c_val + cc, as.numeric(.data$c_val)),
        d_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$d     + cc, as.numeric(.data$d))
      )
  } else if (cc_sw == 2) {
    # Reciprocal correction: 1/opposite-arm N
    cc_exp <- 1 / n_ctl
    cc_ctl <- 1 / n_exp
    ct_cc <- ct_cc %>%
      dplyr::mutate(
        a_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$a     + cc_exp, as.numeric(.data$a)),
        b_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$b     + cc_exp, as.numeric(.data$b)),
        c_val_cc = dplyr::if_else(.data$cc_ind == 1L, .data$c_val + cc_ctl, as.numeric(.data$c_val)),
        d_cc     = dplyr::if_else(.data$cc_ind == 1L, .data$d     + cc_ctl, as.numeric(.data$d))
      )
  } else {
    ct_cc <- ct_cc %>%
      dplyr::mutate(
        a_cc     = as.numeric(.data$a),
        b_cc     = as.numeric(.data$b),
        c_val_cc = as.numeric(.data$c_val),
        d_cc     = as.numeric(.data$d)
      )
  }

  # --- 11. Relative risk & odds ratio from CC-adjusted counts (SAS 482-489) --
  rr_or_stats <- ct_cc %>%
    dplyr::select("term_num", "a_cc", "b_cc",
                  "c_val_cc", "d_cc", "cc_ind",
                  a_orig = "a", b_orig = "b",
                  c_orig = "c_val", d_orig = "d") %>%
    purrr::pmap_dfr(function(term_num, a_cc, b_cc, c_val_cc, d_cc,
                             cc_ind, a_orig, b_orig, c_orig, d_orig) {
      n1_cc <- a_cc + b_cc
      n2_cc <- c_val_cc + d_cc
      p1_cc <- if (n1_cc > 0) a_cc / n1_cc else NA_real_
      p2_cc <- if (n2_cc > 0) c_val_cc / n2_cc else NA_real_

      # Relative risk (Katz log-normal CI)
      rr <- if (!is.na(p2_cc) && p2_cc > 0) p1_cc / p2_cc else NA_real_
      if (!is.na(rr) && a_cc > 0 && c_val_cc > 0 && n1_cc > 0 && n2_cc > 0) {
        log_se_rr <- sqrt(1 / a_cc - 1 / n1_cc + 1 / c_val_cc - 1 / n2_cc)
        l_rrc1 <- exp(log(rr) - z_alpha * log_se_rr)
        u_rrc1 <- exp(log(rr) + z_alpha * log_se_rr)
      } else {
        l_rrc1 <- NA_real_
        u_rrc1 <- NA_real_
      }

      # Odds ratio
      denom <- b_cc * c_val_cc
      or_val <- if (!is.na(denom) && denom > 0) (a_cc * d_cc) / denom else NA_real_
      # Asymptotic (Woolf logit) CI
      if (!is.na(or_val) && a_cc > 0 && b_cc > 0 && c_val_cc > 0 && d_cc > 0) {
        log_se_or <- sqrt(1 / a_cc + 1 / b_cc + 1 / c_val_cc + 1 / d_cc)
        l_rror_asym <- exp(log(or_val) - z_alpha * log_se_or)
        u_rror_asym <- exp(log(or_val) + z_alpha * log_se_or)
      } else {
        l_rror_asym <- NA_real_
        u_rror_asym <- NA_real_
      }

      # Exact OR CI from fisher.test on integer-valued counts
      # When CC creates non-integer counts, exact CI is unavailable
      use_exact_counts <- if (cc_sw == 0 || cc_ind == 0) {
        TRUE
      } else if (cc_whole == 1) {
        TRUE
      } else {
        FALSE
      }

      if (use_exact_counts) {
        # Use appropriate integer counts for exact computation
        if (cc_sw == 0 || cc_ind == 0) {
          ft_exact <- safe_fisher(a_orig, c_orig, b_orig, d_orig)
        } else {
          ft_exact <- safe_fisher(a_cc, c_val_cc, b_cc, d_cc)
        }
        xl_rror <- ft_exact$conf.int[1]
        xu_rror <- ft_exact$conf.int[2]
      } else {
        xl_rror <- NA_real_
        xu_rror <- NA_real_
      }

      dplyr::tibble(
        term_num    = term_num,
        rrc1        = rr,
        l_rrc1      = l_rrc1,
        u_rrc1      = u_rrc1,
        rror        = or_val,
        l_rror_asym = l_rror_asym,
        u_rror_asym = u_rror_asym,
        xl_rror     = xl_rror,
        xu_rror     = xu_rror
      )
    })

  # --- 12. Merge all statistics (SAS lines 495-554) --------------------------
  merged <- freq_wide %>%
    dplyr::left_join(rd_stats,   by = "term_num") %>%
    dplyr::left_join(rr_or_stats, by = "term_num") %>%
    dplyr::left_join(
      ct %>% dplyr::select("term_num", "cc_ind"),
      by = "term_num"
    ) %>%
    dplyr::left_join(term_lookup, by = "term_num")

  # Scale proportions to percentages (SAS lines 529-537)
  pct_cols <- c("rsk11", "rsk21", "rdif1", "l_rdif1", "u_rdif1",
                "xl_rsk11", "xu_rsk11", "xl_rsk21", "xu_rsk21")
  merged <- merged %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(pct_cols),
                                 ~ . * 100))

  # Select OR CI source (SAS lines 547-550):
  # Use exact CI when: no CC, CC with whole number, or CC but term not corrected
  # Use asymptotic CI when: non-whole CC and term was corrected
  merged <- merged %>%
    dplyr::mutate(
      l_rror = dplyr::case_when(
        cc_sw == 0                                      ~ .data$xl_rror,
        cc_sw != 0 & cc_whole == 1                      ~ .data$xl_rror,
        cc_sw != 0 & cc_whole == 0 & .data$cc_ind == 0L ~ .data$xl_rror,
        TRUE                                            ~ .data$l_rror_asym
      ),
      u_rror = dplyr::case_when(
        cc_sw == 0                                      ~ .data$xu_rror,
        cc_sw != 0 & cc_whole == 1                      ~ .data$xu_rror,
        cc_sw != 0 & cc_whole == 0 & .data$cc_ind == 0L ~ .data$xu_rror,
        TRUE                                            ~ .data$u_rror_asym
      )
    )

  # --- 13. Rename columns (SAS lines 556-578) --------------------------------
  ve <- cmpgr_varext
  # Build rename mapping (old -> new) using purrr::set_names
  old_names <- c(exp_col, ctl_col,
                 "rsk11", "rsk21",
                 "xl_rsk11", "xu_rsk11", "xl_rsk21", "xu_rsk21",
                 "rdif1", "l_rdif1", "u_rdif1",
                 "rrc1", "l_rrc1", "u_rrc1",
                 "rror", "l_rror", "u_rror",
                 "xp2_fish")
  new_names <- c(paste0("arm", exp, "_", ve), paste0("arm", ctl, "_", ve),
                 paste0("arm", exp, "_", ve, "_pct"), paste0("arm", ctl, "_", ve, "_pct"),
                 paste0("arm", exp, "_", ve, "_pct_cilb"), paste0("arm", exp, "_", ve, "_pct_ciub"),
                 paste0("arm", ctl, "_", ve, "_pct_cilb"), paste0("arm", ctl, "_", ve, "_pct_ciub"),
                 "rd", "rd_cilb", "rd_ciub",
                 "rr", "rr_cilb", "rr_ciub",
                 "ort", "or_cilb", "or_ciub",
                 "p_value")
  rename_map <- purrr::set_names(old_names, new_names)

  # Apply only renames for columns that exist (values are old names)
  existing_renames <- rename_map[rename_map %in% names(merged)]
  merged <- merged %>% dplyr::rename(!!!existing_renames)

  # If ae_rate_ci_sw is off, drop the arm-level CI columns
  if (ae_rate_ci_sw != 1) {
    ci_drop <- c(paste0("arm", exp, "_", ve, "_pct_cilb"),
                 paste0("arm", exp, "_", ve, "_pct_ciub"),
                 paste0("arm", ctl, "_", ve, "_pct_cilb"),
                 paste0("arm", ctl, "_", ve, "_pct_ciub"))
    ci_drop <- ci_drop[ci_drop %in% names(merged)]
    if (length(ci_drop) > 0) {
      merged <- merged %>% dplyr::select(-dplyr::all_of(ci_drop))
    }
  }

  # --- 14. Apply labels (SAS lines 582-603) ----------------------------------
  exp_nm <- arm_names[[exp]]
  ctl_nm <- arm_names[[ctl]]
  label_map <- purrr::set_names(
    c(paste0(exp_nm, " ", cmpgr_label, " Count"),
      paste0(ctl_nm, " ", cmpgr_label, " Count"),
      paste0(exp_nm, " ", cmpgr_label, " %"),
      paste0(ctl_nm, " ", cmpgr_label, " %"),
      "Risk Difference (%)", "Risk Difference Lower CI", "Risk Difference Upper CI",
      "Relative Risk", "Relative Risk Lower CI", "Relative Risk Upper CI",
      "Odds Ratio", "Odds Ratio Lower CI", "Odds Ratio Upper CI",
      "Fisher Exact p-value"),
    c(paste0("arm", exp, "_", ve), paste0("arm", ctl, "_", ve),
      paste0("arm", exp, "_", ve, "_pct"), paste0("arm", ctl, "_", ve, "_pct"),
      "rd", "rd_cilb", "rd_ciub", "rr", "rr_cilb", "rr_ciub",
      "ort", "or_cilb", "or_ciub", "p_value")
  )
  # Use purrr::imap to apply labels dynamically
  purrr::imap(label_map, function(lbl, col_nm) {
    if (col_nm %in% names(merged)) {
      attr(merged[[col_nm]], "label") <<- lbl
    }
  })

  # Drop helper columns not needed in final output
  drop_cols <- c("term_num", "l_rror_asym", "u_rror_asym",
                 "xl_rror", "xu_rror")
  drop_cols <- drop_cols[drop_cols %in% names(merged)]
  merged <- merged %>% dplyr::select(-dplyr::all_of(drop_cols))

  # --- 15. Split output (SAS lines 606-616) ----------------------------------
  # Add row number for cc_ind tracking
  merged <- merged %>% dplyr::mutate(row = dplyr::row_number())

  output_cc_ind <- NULL
  if (cc_sw != 0 && "cc_ind" %in% names(merged)) {
    output_cc_ind <- merged %>%
      dplyr::select("row", "cc_ind")
    output_result <- merged %>%
      dplyr::select(-"row", -"cc_ind")
  } else {
    output_result <- merged %>%
      dplyr::select(-"row") %>%
      { if ("cc_ind" %in% names(.)) dplyr::select(., -"cc_ind") else . }
  }
  data_result <- output_result

  # --- 16. Reporting metadata (SAS lines 620-646) ----------------------------
  rpt_key_result     <- NULL
  rpt_missing_result <- NULL
  if (report) {
    rpt_key_result <- rpt_key(
      dsout_name = dsout_name, key = key, max_arg = max_arg,
      meddra = meddra, dsin = dsin
    )

    # Missing toxicity grades using all-arm deduped data (SAS lines 624-646)
    rpt_missing_result <- dplyr::tibble(ds = dsout_name)
    arm_miss <- sorted_allarm %>%
      dplyr::group_by(.data$arm_num) %>%
      dplyr::summarise(
        arm_all = dplyr::n(),
        arm_toxgr_missing = sum(is.na(.data$aetoxgr)),
        .groups = "drop"
      ) %>%
      tidyr::complete(arm_num = seq_len(arm_count),
                      fill = list(arm_all = 0L, arm_toxgr_missing = 0L))

    for (i in seq_len(arm_count)) {
      row_i <- arm_miss %>% dplyr::filter(.data$arm_num == i)
      miss_n <- if (nrow(row_i) > 0) row_i$arm_toxgr_missing else 0L
      all_n  <- if (nrow(row_i) > 0) row_i$arm_all else 0L
      rpt_missing_result[[paste0("arm", i, "_toxgr_missing")]] <- miss_n
      rpt_missing_result[[paste0("arm", i, "_toxgr_missing_pct")]] <-
        if (all_n > 0) 100 * miss_n / all_n else 0
    }
  }

  list(data           = data_result,
       output         = output_result,
       output_cc_ind  = output_cc_ind,
       rpt_key        = rpt_key_result,
       rpt_missing    = rpt_missing_result)
}

# =============================================================================
# fmt_output — Migrate SAS %fmt_output (lines 669-830)
# =============================================================================
#' Format, sort, and insert header rows for hierarchical AE output
#'
#' Sorts data within and between groups (optionally by a sort variable and its
#' top-3 group totals), reverses order for header-row insertion, generates
#' header rows at group boundaries, and separates output from indicators.
#'
#' @param ds          Data frame to format (output or output_cc_ind from
#'                    compare_ae or aggregate_ae).
#' @param ds_name     Character label matching an entry in rpt_key.
#' @param rpt_key     rpt_key tibble (from rpt_key()).
#' @param cc_sw       Continuity correction switch (0/1/2).
#' @param cc_ind_ds   Optional data frame with row and cc_ind columns.
#' @param sort_sw     Logical; sort within groups by sortvar.
#' @param sortvar     Character column name to sort by.
#' @param sortgrp_sw  Logical; sort groups by top-3 sortvar values.
#' @param sortdir     "desc" or "asc".
#' @return Named list: fmt, fmt_ind, fmt_cc_ind.
fmt_output <- function(ds, ds_name, rpt_key, cc_sw = 0,
                       cc_ind_ds = NULL,
                       sort_sw = FALSE, sortvar = NULL,
                       sortgrp_sw = FALSE, sortdir = "desc") {

  if (nrow(ds) == 0L) {
    return(list(fmt = ds, fmt_ind = dplyr::tibble(), fmt_cc_ind = NULL))
  }

  # --- 1. Retrieve key metadata from rpt_key (SAS lines 676-681) ------------
  rpt_row <- rpt_key %>% dplyr::filter(.data$ds == ds_name)
  if (nrow(rpt_row) == 0L) {
    cli::cli_warn("ds_name '{ds_name}' not found in rpt_key; returning unformatted.")
    return(list(fmt = ds, fmt_ind = dplyr::tibble(), fmt_cc_ind = NULL))
  }
  key_str    <- rpt_row$key[1]
  keyvar_cnt <- rpt_row$keyvar_cnt[1]
  key_vars   <- strsplit(trimws(key_str), "\\s+")[[1]]

  # --- 2. Auto-set sort direction (SAS line 674) -----------------------------
  if (!is.null(sortvar) && sortvar == "p_value") sortdir <- "asc"

  # --- 3. Incorporate cc_ind (SAS lines 684-692) -----------------------------
  if (cc_sw != 0 && !is.null(cc_ind_ds) && grepl("pt_3", ds_name, fixed = TRUE)) {
    ds <- ds %>% dplyr::mutate(row = dplyr::row_number())
    ds <- ds %>% dplyr::left_join(cc_ind_ds, by = "row")
  }

  # --- 4. Sorting (SAS lines 695-747) ----------------------------------------
  working <- ds
  sort_desc <- (sortdir == "desc")

  if (sort_sw && !is.null(sortvar) && sortvar %in% names(working)) {
    # Sort within groups by sortvar
    group_vars <- if (keyvar_cnt > 1) key_vars[seq_len(keyvar_cnt - 1)] else character(0)
    sv_sym <- rlang::sym(sortvar)

    if (sortgrp_sw && length(group_vars) > 0) {
      # --- Top-3 sort values per group (SAS lines 703-718) -------------------
      top_counts <- working %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::arrange(
          if (sort_desc) dplyr::desc(!!sv_sym) else !!sv_sym
        ) %>%
        dplyr::slice(seq_len(min(3, dplyr::n()))) %>%
        dplyr::mutate(.rank = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(group_vars), !!sv_sym, ".rank") %>%
        tidyr::pivot_wider(
          names_from  = ".rank",
          values_from = !!sv_sym,
          names_prefix = "top_cnt"
        )

      # Merge back (SAS hash lookup, lines 721-740)
      working <- working %>%
        dplyr::left_join(top_counts, by = group_vars)

      # Sort: between groups by top counts, within by sortvar
      if (sort_desc) {
        working <- working %>%
          dplyr::arrange(
            dplyr::desc(.data$top_cnt1),
            dplyr::desc(
              dplyr::if_else(is.na(.data$top_cnt2), -Inf, .data$top_cnt2)),
            dplyr::desc(
              dplyr::if_else(is.na(.data$top_cnt3), -Inf, .data$top_cnt3)),
            dplyr::across(dplyr::all_of(group_vars)),
            dplyr::desc(!!sv_sym)
          )
      } else {
        working <- working %>%
          dplyr::arrange(
            .data$top_cnt1,
            dplyr::if_else(is.na(.data$top_cnt2), Inf, .data$top_cnt2),
            dplyr::if_else(is.na(.data$top_cnt3), Inf, .data$top_cnt3),
            dplyr::across(dplyr::all_of(group_vars)),
            !!sv_sym
          )
      }

      # Drop top_cnt helper columns
      working <- working %>%
        dplyr::select(-dplyr::matches("^top_cnt[0-9]+$"))

    } else {
      # Simple sort within groups (no group sorting)
      if (length(group_vars) > 0) {
        if (sort_desc) {
          working <- working %>%
            dplyr::arrange(
              dplyr::across(dplyr::all_of(group_vars)),
              dplyr::desc(!!sv_sym)
            )
        } else {
          working <- working %>%
            dplyr::arrange(
              dplyr::across(dplyr::all_of(group_vars)),
              !!sv_sym
            )
        }
      } else {
        if (sort_desc) {
          working <- working %>% dplyr::arrange(dplyr::desc(!!sv_sym))
        } else {
          working <- working %>% dplyr::arrange(!!sv_sym)
        }
      }
    }
  }

  # --- 5. Generate header rows (SAS lines 767-797) ---------------------------
  #   For keyvar_cnt >= 2: add a header row at the start of each group
  #   defined by the first (keyvar_cnt - 1) key variables.
  #   Group boundaries detected where lag() of the first group var differs
  #   from the current value (SAS first.by processing equivalent).
  if (keyvar_cnt >= 2) {
    group_vars <- key_vars[seq_len(keyvar_cnt - 1)]
    last_key   <- key_vars[keyvar_cnt]

    # Detect group boundaries via dplyr::lag() — used for validation/tracing
    working <- working %>%
      dplyr::mutate(
        .grp_boundary = dplyr::if_else(
          dplyr::row_number() == 1L |
            as.character(.data[[group_vars[1]]]) !=
              dplyr::lag(as.character(.data[[group_vars[1]]]), default = ""),
          TRUE, FALSE
        )
      )

    # Split by group, prepend header, concatenate
    groups <- working %>%
      dplyr::select(-".grp_boundary") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::group_split()

    result_list <- vector("list", length(groups) * 2L)
    idx <- 1L
    for (grp in groups) {
      # Header row: concatenate group variable values with " : "
      grp_vals <- as.character(unlist(grp[1, group_vars, drop = TRUE]))
      header_label <- paste(grp_vals, collapse = " : ")

      # Create header row — same structure as data with NA for numeric columns
      header_row <- grp[1, , drop = FALSE]
      num_cols <- names(header_row)[vapply(header_row, is.numeric, logical(1))]
      for (nc in num_cols) header_row[[nc]] <- NA_real_
      header_row[["adverse_event"]] <- header_label
      header_row[["header"]] <- 1L

      # Data rows: indent last key variable by 5 spaces
      data_rows <- grp %>%
        dplyr::mutate(
          adverse_event = paste0("     ", as.character(.data[[last_key]])),
          header = 0L
        )

      result_list[[idx]]     <- header_row
      result_list[[idx + 1]] <- data_rows
      idx <- idx + 2L
    }

    working <- dplyr::bind_rows(result_list[seq_len(idx - 1)])
  } else {
    # Single key variable: no headers needed; create adverse_event column
    last_key <- key_vars[1]
    working <- working %>%
      dplyr::mutate(
        adverse_event = as.character(.data[[last_key]]),
        header = 0L
      )
  }

  # Add sequential order for downstream use
  working <- working %>% dplyr::mutate(order = dplyr::row_number())

  # --- 6. Separate indicators from output (SAS lines 807-822) ----------------
  fmt_ind <- working %>%
    dplyr::select("order", "header")

  # Extract cc_ind if present
  fmt_cc_ind <- NULL
  if ("cc_ind" %in% names(working)) {
    fmt_cc_ind <- working %>%
      dplyr::select("order", "cc_ind")
  }

  # Final formatted output: drop indicator and row-tracking columns
  drop_final <- c("header", "order", "cc_ind", "row")
  drop_final <- drop_final[drop_final %in% names(working)]
  fmt <- working %>% dplyr::select(-dplyr::all_of(drop_final))

  list(fmt        = fmt,
       fmt_ind    = fmt_ind,
       fmt_cc_ind = fmt_cc_ind)
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS RETAIN+array pattern for per-arm grade counts -> dplyr group_by +
####      summarise + pivot_wider
####    - SAS BY-group processing with first./last. -> dplyr group_by with
####      row operations
####    - SAS PROC FREQ riskdiff output variables (_rsk11_, _rdif1_, etc.)
####      -> manual computation using binom.test() for exact CIs, manual Wald
####      formula for risk difference CI
####    - SAS PROC FREQ relrisk output variables (_rrc1_, _rror_, etc.)
####      -> manual computation using Katz log-normal for RR CI and Woolf logit
####      for OR CI
####    - SAS PROC FREQ EXACT FISHER -> fisher.test()
####    - SAS PROC FREQ EXACT OR -> fisher.test() for exact OR CI
####    - SAS nodupkey -> dplyr::distinct() with .keep_all = TRUE
####    - SAS hash object (DATA step hash lookup) -> dplyr::left_join()
####    - SAS proc datasets rename -> dplyr::rename()
####    - SAS proc datasets modify label -> attr(col, "label")
####    - SAS put(n, words.) -> number_to_word() internal helper
####    - SAS first./last. processing -> dplyr::group_by() + slice(1)
####      or distinct()
####    - SAS sparse option in PROC FREQ -> tidyr::complete() to fill zero cells
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Risk difference CI: SAS uses Wald (asymptotic) method; this R
####      implementation uses the same Wald formula. Results should match to
####      machine precision.
####    - Relative risk CI: SAS uses Katz log-normal approximation; this R
####      implementation uses the identical formula. Results should match.
####    - Odds ratio CI: SAS exact CI from Fisher's exact test; R
####      fisher.test()$conf.int should match; verify edge cases with zero cells
####    - Fisher's exact p-value: should match exactly between SAS and R
####    - Clopper-Pearson exact CI for proportions: binom.test() in R matches
####      SAS exactly
####    - Continuity correction: applied before RR/OR computation, not before
####      RD, matching SAS behavior
####    - Percentage calculations: may differ at 15th+ decimal place due to
####      floating point; use janitor::round_half_up() at final display rounding
#### NO DIRECT R EQUIVALENT:
####    - SAS RETAIN statement -> dplyr group_by + summarise eliminates need
####    - SAS array processing (2D arrays) -> dplyr wide format with rowSums()
####    - SAS %do i=1 %to &arm_count (macro loops) -> for loops or purrr::map()
####    - SAS PROC FREQ riskdiff -> manual computation (no single R function
####      produces all outputs)
####    - SAS PROC FREQ relrisk -> manual computation
####    - SAS proc datasets rename -> dplyr::rename()
####    - SAS hash object -> dplyr::left_join()
####    - SAS proc sort nodupkey -> dplyr::distinct()
####    - SAS options nonotes -> suppressMessages() or invisible()
####    - SAS PROC SQL create table from subquery -> dplyr pipeline
#### PACKAGE SELECTION RATIONALE:
####    - dplyr: Core data manipulation replacing DATA steps, PROC SQL,
####      PROC SORT, PROC FREQ counts
####    - tidyr: complete() for zero-cell filling (PROC FREQ sparse),
####      pivot_wider/longer for reshaping
####    - janitor: round_half_up() for SAS-compatible rounding
####    - purrr: Functional iteration replacing SAS macro %do loops and
####      per-term statistical computation
####    - rlang: Tidy evaluation for dynamic column references from
####      character vectors
####    - cli: Logging messages replacing SAS %put
####    - stats: fisher.test(), binom.test(), prop.test(), qnorm() for
####      statistical computations
#### OPEN QUESTIONS:
####    - Exact method used by SAS PROC FREQ for risk difference CI
####      (Newcombe score vs Wald) -- this code uses Wald to match SAS default
####    - Whether SAS PROC FREQ EXACT OR computes mid-p adjusted CI --
####      if so, R fisher.test may need adjustment
####    - Handling of ties in sort stability: SAS PROC SORT is stable;
####      R arrange() is stable within dplyr but verify for multi-key sorts
####    - Whether the "words" format in SAS (put(n, words.)) needs exact
####      English word form or if a simple lookup suffices for small values
####      (typically 1-4 key variables)
#### ============================================================
