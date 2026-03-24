# ==========================================================================
# ae_meddra.R - MedDRA at a Glance Panel Main Analysis
# ==========================================================================
# Migrated from: tested/SAS/macros/ae_meddra.sas (644 lines)
# SAS macros:    %params, %meddra, %meddra_cmp -> 3 R functions
#
# Purpose: Orchestrates the MedDRA at a Glance safety panel:
#   - meddra_params():     Configuration and parameter normalization
#   - meddra_aggregate():  Hierarchical SOC/HLGT/HLT/PT aggregation with
#                          pairwise risk difference, relative risk, and
#                          Fisher's exact test with continuity correction
#   - meddra_cmp():        Comparison dataset assembly for output
#
# Original Author: David Kretch (david.kretch@us.ibm.com)
# Original Date:   February 15, 2011
# Migration Date:  2026
# ==========================================================================

# --- Package Dependencies -------------------------------------------------
# These packages must be available before sourcing this file.
# They replace SAS DATA step, PROC SQL, PROC FREQ, macro language, and
# SpreadsheetML output constructs from the original ae_meddra.sas.
# --------------------------------------------------------------------------
library(dplyr)    # data manipulation: replaces SAS DATA step + PROC SQL
library(tidyr)    # reshaping: replaces SAS transpose / pivot constructs
library(purrr)    # functional iteration: replaces SAS macro DO loops
library(rlang)    # tidy evaluation: replaces SAS macro variable resolution
library(tibble)   # enhanced data frames: replaces SAS datasets
library(janitor)  # round_half_up: SAS-compatible rounding behaviour
library(cli)      # user-facing messages: replaces SAS %PUT
# stats::fisher.test is called with explicit namespace to avoid
# the mask created by janitor::fisher.test


# ==========================================================================
# meddra_params - Configuration and Parameter Normalization
# ==========================================================================
# Replaces SAS %params macro (lines 80-199 of ae_meddra.sas)
#
# In SAS, %params handles LOCAL and Script Launcher run modes, sets
# global macro variables, reads datasets, and normalises the continuity
# correction parameter.  In R every setting is a named argument and the
# function returns a structured list.  No global state is modified.
#
# Continuity-correction (CC) modes derived from the cc argument:
#   cc = 0 or "none"   -> cc_sw = 0   (no correction)
#   cc = numeric > 0    -> cc_sw = 1   (constant added to all 4 cells)
#   cc = "arm"          -> cc_sw = 2   (reciprocal of opposite arm total)
# ==========================================================================
meddra_params <- function(
  ae              = NULL,
  dm              = NULL,
  ex              = NULL,
  panel_title     = "MedDRA at a Glance",
  panel_desc      = "",
  ndabla          = NA_character_,
  studyid         = NA_character_,
  meddra_ver      = "14.0",
  study_lag       = 120L,
  cc              = 0.5,
  meddra_data     = NULL,
  dme_data        = NULL,
  sl_datasets     = NULL,
  sl_group        = NULL,
  sl_subset       = NULL,
  output_file     = NULL,
  err_output_file = NULL,
  rd_th           = 5,
  rr_th           = 5,
  pv_th           = NA_real_,
  vld_sw          = TRUE
) {

  # --- Input validation ---------------------------------------------------
  if (is.null(ae) || !is.data.frame(ae)) {
    cli::cli_abort("Parameter {.arg ae} must be a non-NULL data frame.")
  }
  if (is.null(dm) || !is.data.frame(dm)) {
    cli::cli_abort("Parameter {.arg dm} must be a non-NULL data frame.")
  }
  if (is.null(ex) || !is.data.frame(ex)) {
    cli::cli_abort("Parameter {.arg ex} must be a non-NULL data frame.")
  }

  # --- MedDRA version flag (SAS lines 124-128) ---------------------------
  # SAS: if the first character of ver is 'N' then meddra = 'N'
  meddra_flag <- !startsWith(toupper(as.character(meddra_ver)), "N")
  meddra_pct  <- if (meddra_flag) 100 else 0

  # --- Continuity correction normalisation (SAS lines 218-233) -----------
  # SAS DATA step reads cc as character or numeric and derives cc_sw:
  #   anyalpha(cc) and cc='arm' -> 2
  #   cc numeric and non-zero   -> 1
  #   otherwise                 -> 0
  # cc_whole is 1 if cc_value is integer, else 0
  cc_str <- tolower(trimws(as.character(cc)))

  cc_config <- if (cc_str %in% c("0", "none", "")) {
    list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
  } else if (cc_str == "arm") {
    list(cc_sw = 2L, cc_whole = 0L, cc_value = NA_real_)
  } else {
    cc_num <- suppressWarnings(as.numeric(cc_str))
    if (is.na(cc_num)) {
      cli::cli_warn(
        "Continuity correction {.val {cc}} not recognised. Defaulting to none."
      )
      list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
    } else if (cc_num == 0) {
      list(cc_sw = 0L, cc_whole = 0L, cc_value = 0)
    } else {
      cc_whole_flag <- if (cc_num == floor(cc_num)) 1L else 0L
      list(cc_sw = 1L, cc_whole = cc_whole_flag, cc_value = cc_num)
    }
  }

  # --- Log configuration (replaces SAS %PUT statements) -------------------
  cli::cli_inform(c(
    "i" = "MedDRA at a Glance Configuration:",
    "*" = "Panel: {panel_title}",
    "*" = "NDA/BLA: {ndabla} | Study: {studyid}",
    "*" = "MedDRA Version: {meddra_ver} (active: {meddra_flag})",
    "*" = "Study Lag: {study_lag} days",
    "*" = "CC: sw={cc_config$cc_sw}, value={cc_config$cc_value}, whole={cc_config$cc_whole}",
    "*" = "Thresholds: RD={rd_th}, RR={rr_th}, PV={pv_th}",
    "*" = "Validation: {vld_sw}"
  ))

  # --- Return structured configuration list -------------------------------
  list(
    ae              = ae,
    dm              = dm,
    ex              = ex,
    panel_title     = panel_title,
    panel_desc      = panel_desc,
    ndabla          = ndabla,
    studyid         = studyid,
    meddra_ver      = meddra_ver,
    meddra_flag     = meddra_flag,
    meddra_pct      = meddra_pct,
    study_lag       = as.integer(study_lag),
    cc_sw           = cc_config$cc_sw,
    cc_whole        = cc_config$cc_whole,
    cc_value        = cc_config$cc_value,
    meddra_data     = meddra_data,
    dme_data        = dme_data,
    sl_datasets     = sl_datasets,
    sl_group        = sl_group,
    sl_subset       = sl_subset,
    output_file     = output_file,
    err_output_file = err_output_file,
    rd_th           = rd_th,
    rr_th           = rr_th,
    pv_th           = pv_th,
    vld_sw          = vld_sw
  )
}


# ==========================================================================
# meddra_aggregate - Hierarchical MedDRA Aggregation with Pairwise Stats
# ==========================================================================
# Replaces SAS %meddra(dsin, dsout, by1, by2, by3, by4) (lines 239-516)
#
# Algorithm:
#   1. Deduplicate to one record per subject per term (PROC SORT NODUPKEY)
#   2. Count subjects per arm per MedDRA term
#   3. Zero-fill missing arm x term combinations
#   4. Compute percentages: 100 * count / arm_total
#   5. Level tagging: cumulative SOC/HLGT/HLT/PT ordinal numbers
#   6. Pairwise RD (from original pcts) and RR (with CC when applicable)
#   7. Fisher's exact test on ORIGINAL integer counts; -ln(p) transform
#
# Arguments:
#   ds_base     - Input tibble with columns: usubjid, arm_num, and by_vars
#   by_vars     - Character vector of hierarchy column names
#                 e.g. c("soc_name") or c("soc_name","hlgt_name","hlt_name","pt_name")
#   arm_count   - Number of treatment arms (integer)
#   arm_names   - Display names for arms (character vector, length = arm_count)
#   arm_subjcnt - Total subject counts per arm (integer vector, length = arm_count)
#   cc_sw       - CC switch: 0=none, 1=constant, 2=arm reciprocal
#   cc_whole    - 1 if cc_value is integer, 0 otherwise
#   cc_value    - Numeric CC constant (used when cc_sw = 1)
#   dme_flag    - Whether to carry the DME column (logical)
#
# Returns:
#   Tibble with one row per term at the specified hierarchy depth.
# ==========================================================================
meddra_aggregate <- function(ds_base,
                             by_vars,
                             arm_count,
                             arm_names,
                             arm_subjcnt,
                             cc_sw     = 0L,
                             cc_whole  = 0L,
                             cc_value  = 0,
                             dme_flag  = FALSE) {

  # --- Input validation ---------------------------------------------------
  if (!is.data.frame(ds_base) || nrow(ds_base) == 0L) {
    cli::cli_abort("{.arg ds_base} must be a non-empty data frame.")
  }
  if (length(by_vars) == 0L) {
    cli::cli_abort("{.arg by_vars} must contain at least one variable name.")
  }
  required_cols <- c(by_vars, "usubjid", "arm_num")
  missing_cols  <- setdiff(required_cols, colnames(ds_base))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing columns in {.arg ds_base}: {.val {missing_cols}}")
  }
  if (length(arm_subjcnt) != arm_count) {
    cli::cli_abort(
      "{.arg arm_subjcnt} length ({length(arm_subjcnt)}) must equal {.arg arm_count} ({arm_count})."
    )
  }

  max_arg  <- length(by_vars)
  last_by  <- by_vars[max_arg]
  key_str  <- paste(by_vars, collapse = ", ")

  # --- Log activity banner (SAS lines 257-271) ----------------------------
  cli::cli_inform(c("i" = "Aggregating by [{key_str}] - Level {max_arg}, {arm_count} arm(s)"))

  # ---- Step 1: Deduplicate (PROC SORT NODUPKEY, SAS lines 275-278) ------
  # Keep one record per subject per MedDRA term combination per arm
  keep_cols <- c(by_vars, "usubjid", "arm_num")
  has_dme   <- dme_flag && last_by == "pt_name" && "dme" %in% colnames(ds_base)
  if (has_dme) keep_cols <- c(keep_cols, "dme")

  ds_dedup <- ds_base %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by_vars, "usubjid"))))

  # ---- Step 2: Per-arm subject counting (SAS RETAIN+array, lines 280-320)
  term_counts <- ds_dedup %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars)), arm_num) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # ---- Step 3: Zero-fill all arm x term combinations --------------------
  # Replaces SAS implicit initialisation of RETAIN sums to zero
  complete_counts <- term_counts %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(by_vars)),
      arm_num = seq_len(arm_count),
      fill    = list(count = 0L)
    )

  # ---- Step 4: Percentages (SAS: 100 * count / arm_total) ---------------
  # janitor::round_half_up for SAS-compatible rounding (Gate 2 requirement)
  with_pct <- complete_counts %>%
    dplyr::mutate(
      pct = dplyr::if_else(
        arm_subjcnt[arm_num] > 0L,
        janitor::round_half_up(100 * count / arm_subjcnt[arm_num], digits = 10),
        NA_real_
      )
    )

  # ---- Step 5: Preserve DME values if applicable -------------------------
  if (has_dme) {
    dme_lookup <- ds_dedup %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
      dplyr::summarise(dme = dplyr::first(.data[["dme"]]), .groups = "drop")
    with_pct <- with_pct %>%
      dplyr::left_join(dme_lookup, by = by_vars)
  }

  # ---- Step 6: Pivot to wide format (SAS lines 280-320 output) ----------
  # Creates arm-specific columns: arm1_count, arm1_pct, arm2_count, ...
  wide <- with_pct %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = c(count, pct),
      names_glue  = "arm{arm_num}_{.value}"
    )

  # ---- Step 7: Column labels for arm statistics --------------------------
  for (ai in seq_len(arm_count)) {
    lbl <- if (ai <= length(arm_names)) arm_names[ai] else paste("Arm", ai)
    attr(wide[[paste0("arm", ai, "_count")]], "label") <- paste(lbl, "Count")
    attr(wide[[paste0("arm", ai, "_pct")]], "label")   <- paste(lbl, "%")
  }

  # ---- Step 8: Level tagging - cumulative numbering ----------------------
  # Replaces SAS RETAIN + first.by_var cumulative sum (lines 336-341)
  # SAS: if first.soc_name then soc = sum(soc, 1);
  # R: cumsum of first-occurrence flags
  wide <- wide %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)))

  for (k in seq_along(by_vars)) {
    bv     <- by_vars[k]
    prefix <- sub("_.*$", "", bv)   # "soc_name" -> "soc"
    vals   <- wide[[bv]]
    first_occ <- c(TRUE, vals[-1] != vals[-length(vals)])
    cum_vals  <- cumsum(first_occ)
    # rlang := (walrus operator) for dynamic column naming
    wide <- wide %>% dplyr::mutate(!!prefix := cum_vals)
  }
  wide <- wide %>% dplyr::mutate(level = as.integer(max_arg))

  # ---- Step 9: Pairwise Risk Difference and Relative Risk ----------------
  # Replaces SAS DATA step at lines 346-409
  # For ALL pairs (i,j) where i != j:
  #   RD = arm_i_pct - arm_j_pct   (from ORIGINAL percentages)
  #   RR = (a/(a+b)) / (c/(c+d))   (from CC-adjusted values when CC applied)
  # CC is applied ONLY when c = 0 (comparator arm count is zero)
  # -- SAS line 377: the a=0, b=0, d=0 checks are commented out in source
  if (arm_count > 1L) {

    all_pairs <- expand.grid(
      i = seq_len(arm_count), j = seq_len(arm_count),
      KEEP.OUT.ATTRS = FALSE
    )
    all_pairs <- all_pairs[all_pairs$i != all_pairs$j, , drop = FALSE]

    # Compute RD, CC, RR for every (i,j) pair using purrr::map2
    pair_cols <- purrr::map2(all_pairs$i, all_pairs$j, function(i, j) {
      ci <- paste0("arm", i, "_count")
      cj <- paste0("arm", j, "_count")
      pi_col <- paste0("arm", i, "_pct")
      pj_col <- paste0("arm", j, "_pct")

      # Risk difference from ORIGINAL percentages (SAS line 354)
      rd_vals <- wide[[pi_col]] - wide[[pj_col]]

      # 2x2 contingency table (SAS lines 357-360)
      a <- as.numeric(wide[[ci]])
      n_i <- arm_subjcnt[i]
      n_j <- arm_subjcnt[j]
      b <- n_i - a
      c_val <- as.numeric(wide[[cj]])
      d_val <- n_j - c_val

      # CC indicator: applied only when c = 0 (SAS line 377)
      needs_cc <- (c_val == 0)
      cc_chr   <- rep(NA_character_, nrow(wide))

      if (cc_sw > 0L && any(needs_cc)) {
        idx <- which(needs_cc)
        if (cc_sw == 1L) {
          # Constant CC: add cc_value to all 4 cells (SAS lines 380-385)
          a[idx]     <- a[idx] + cc_value
          b[idx]     <- b[idx] + cc_value
          c_val[idx] <- c_val[idx] + cc_value
          d_val[idx] <- d_val[idx] + cc_value
        } else if (cc_sw == 2L) {
          # Reciprocal CC: sequential as in SAS (lines 387-392)
          # Step A: modify a, b using ORIGINAL c+d
          old_cd <- c_val[idx] + d_val[idx]
          safe_cd <- dplyr::if_else(old_cd == 0, Inf, old_cd)
          a[idx] <- a[idx] + 1 / safe_cd
          b[idx] <- b[idx] + 1 / safe_cd
          # Step B: modify c, d using MODIFIED a+b
          new_ab <- a[idx] + b[idx]
          c_val[idx] <- c_val[idx] + 1 / new_ab
          d_val[idx] <- d_val[idx] + 1 / new_ab
        }
        cc_chr[idx] <- "*"
      }

      # Relative risk from working (possibly CC-adjusted) values (SAS line 401)
      # RR = (a/(a+b)) / (c/(c+d))  -- only when c != 0
      sum_ab <- a + b
      sum_cd <- c_val + d_val
      p_i <- dplyr::if_else(sum_ab > 0, a / sum_ab, NA_real_)
      p_j <- dplyr::if_else(sum_cd > 0, c_val / sum_cd, NA_real_)
      rr_vals <- dplyr::if_else(c_val != 0, p_i / p_j, NA_real_)

      res <- tibble::tibble(rd = rd_vals, rr = rr_vals, cc = cc_chr)
      names(res) <- c(
        paste0("rd", i, j), paste0("rr", i, j), paste0("cc", i, j)
      )
      res
    })

    wide <- dplyr::bind_cols(wide, dplyr::bind_cols(pair_cols))

    # ---- Step 10: Fisher's exact test, pairs i < j -----------------------
    # Replaces SAS PROC FREQ with EXACT FISHER (lines 437-470)
    # Fisher's test uses ORIGINAL integer counts, NOT CC-adjusted values
    # p-values are symmetric: pv_ij = pv_ji  (SAS lines 464-465)
    fisher_pairs <- expand.grid(
      i = seq_len(arm_count), j = seq_len(arm_count),
      KEEP.OUT.ATTRS = FALSE
    )
    fisher_pairs <- fisher_pairs[fisher_pairs$i < fisher_pairs$j, , drop = FALSE]

    if (nrow(fisher_pairs) > 0L) {
      fisher_cols <- purrr::map2(fisher_pairs$i, fisher_pairs$j, function(i, j) {
        ci <- paste0("arm", i, "_count")
        cj <- paste0("arm", j, "_count")
        n_i <- arm_subjcnt[i]
        n_j <- arm_subjcnt[j]

        cli::cli_inform("  Fisher's exact test: arm {i} vs {j}")

        # Per-row Fisher test using purrr::map_dfr
        pv_results <- purrr::map_dfr(seq_len(nrow(wide)), function(r) {
          a_v <- wide[[ci]][r]
          b_v <- n_i - a_v
          c_v <- wide[[cj]][r]
          d_v <- n_j - c_v

          # stats::fisher.test on 2x2 matrix (replaces PROC FREQ EXACT FISHER)
          ft <- tryCatch(
            stats::fisher.test(matrix(c(a_v, b_v, c_v, d_v), nrow = 2)),
            error = function(e) {
              cli::cli_warn(
                "Fisher test failed at row {r}, arms {i} vs {j}: {conditionMessage(e)}"
              )
              list(p.value = NA_real_)
            }
          )

          # Negative natural log of p-value (SAS lines 464-465: -log(xp2_fish))
          # SAS log() = natural log = R log()
          pv <- if (!is.na(ft$p.value) && ft$p.value > 0) {
            -log(ft$p.value)
          } else {
            NA_real_
          }
          tibble::tibble(pv = pv)
        })

        # Symmetric assignment: pv_ij = pv_ji
        res <- tibble::tibble(pv_ij = pv_results$pv, pv_ji = pv_results$pv)
        names(res) <- c(paste0("pv", i, j), paste0("pv", j, i))
        res
      })

      wide <- dplyr::bind_cols(wide, dplyr::bind_cols(fisher_cols))
    }
  }

  # ---- Step 11: Column reordering (SAS RETAIN, lines 491-505) -----------
  col_order <- by_vars
  if (has_dme) col_order <- c(col_order, "dme")

  # Arm count and pct columns
  for (ai in seq_len(arm_count)) {
    col_order <- c(col_order,
                   paste0("arm", ai, "_count"),
                   paste0("arm", ai, "_pct"))
  }

  # Pairwise stat columns: rd, rr, pv for each i != j pair
  if (arm_count > 1L) {
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i != j) {
          col_order <- c(col_order,
                         paste0("rd", i, j),
                         paste0("rr", i, j),
                         paste0("pv", i, j))
        }
      }
    }
    # CC indicator columns (only when cc_sw > 0)
    if (cc_sw > 0L) {
      for (i in seq_len(arm_count)) {
        for (j in seq_len(arm_count)) {
          if (i != j) {
            ccn <- paste0("cc", i, j)
            if (ccn %in% colnames(wide)) col_order <- c(col_order, ccn)
          }
        }
      }
    }
  }

  # Level and level-number columns
  level_prefixes <- purrr::map_chr(by_vars, ~ stringr::str_replace(.x, "_.*$", ""))
  col_order <- c(col_order, "level", level_prefixes)

  # Select in order, then any remaining columns
  present <- intersect(col_order, colnames(wide))
  extra   <- setdiff(colnames(wide), present)
  wide <- wide %>%
    dplyr::select(dplyr::all_of(present), dplyr::all_of(extra)) %>%
    dplyr::ungroup()

  wide
}


# ==========================================================================
# meddra_cmp - Comparison Dataset Assembly for Output
# ==========================================================================
# Replaces SAS %meddra_cmp macro (lines 522-602 of ae_meddra.sas)
#
# Algorithm:
#   1. Concatenate four hierarchy-level result tibbles
#   2. Add data-order row counter (concatenation order)
#   3. Ensure all hierarchy columns exist (fills NA for absent levels)
#   4. Sort by hierarchy + level for the visible analysis tab
#   5. Construct meddra_cmp_output  — visible analysis scaffold
#   6. Construct meddra_cmp_data    — hidden data scaffold with placeholder
#                                      signal/formula columns
#   7. Build row-mapping datasets   — lvl_nm/lvl_no for formula generation
#
# Arguments:
#   meddra_1  - SOC-level  result from meddra_aggregate (by_vars = soc_name)
#   meddra_2  - HLGT-level result (by_vars = soc_name, hlgt_name)
#   meddra_3  - HLT-level  result (by_vars = soc_name, hlgt_name, hlt_name)
#   meddra_4  - PT-level   result (all 4 hierarchy columns)
#   arm_count - Number of treatment arms
#   cc_sw     - Continuity-correction switch (0/1/2), controls whether
#               the cc column appears in the output scaffold
#
# Returns:
#   Named list with:
#     meddra_cmp_output     - Visible tab scaffold (sorted by hierarchy)
#     meddra_cmp_data       - Hidden tab scaffold  (concatenation order)
#     meddra_cmp_data_row   - Row mapping in concatenation order
#     meddra_cmp_output_row - Row mapping in sorted order
# ==========================================================================
meddra_cmp <- function(meddra_1,
                       meddra_2   = NULL,
                       meddra_3   = NULL,
                       meddra_4   = NULL,
                       arm_count  = 1L,
                       cc_sw      = 0L) {

  # --- Input validation ---------------------------------------------------
  if (is.null(meddra_1) || !is.data.frame(meddra_1) || nrow(meddra_1) == 0L) {
    cli::cli_abort("{.arg meddra_1} must be a non-empty data frame.")
  }

  # --- Step 1: Concatenate (SAS SET meddra_1 .. meddra_4, lines 528-532) -
  # bind_rows naturally fills NA for columns absent in individual datasets,
  # matching SAS RETAIN + missing-variable semantics for concatenation.
  parts <- list(meddra_1, meddra_2, meddra_3, meddra_4)
  parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, parts)
  combined <- dplyr::bind_rows(parts)

  # --- Step 2: Ensure all hierarchy columns exist -------------------------
  # SAS RETAIN creates columns that may not exist in all input datasets.
  hierarchy_chr <- c("soc_name", "hlgt_name", "hlt_name", "pt_name")
  hierarchy_num <- c("soc", "hlgt", "hlt", "pt")
  for (col in hierarchy_chr) {
    if (!col %in% colnames(combined)) combined[[col]] <- NA_character_
  }
  for (col in hierarchy_num) {
    if (!col %in% colnames(combined)) combined[[col]] <- NA_integer_
  }
  if (!"dme" %in% colnames(combined)) combined[["dme"]] <- NA_integer_
  if (!"level" %in% colnames(combined)) combined[["level"]] <- NA_integer_

  # --- Step 3: Data-order row counter (SAS row + 1, line 535) -------------
  combined <- combined %>%
    dplyr::mutate(row_data = dplyr::row_number())

  # --- Step 4: Create meddra_cmp_data (hidden data tab, SAS lines 569-583)
  # Drop name columns and row counter; preserve arm statistics + level nums.
  # Then add placeholder formula columns — all initialised to NA / "".
  drop_for_data <- c("soc_name", "hlgt_name", "hlt_name", "pt_name",
                     "dme", "row_data")

  meddra_data <- combined %>%
    dplyr::select(-dplyr::any_of(drop_for_data)) %>%
    dplyr::mutate(
      rd       = NA_real_,
      rr       = NA_real_,
      pv       = NA_real_,
      cc       = NA_character_,
      sgnl     = NA_real_,
      sgnl_soc = NA_real_,
      sgnl_hlgt = NA_real_,
      sgnl_hlt  = NA_real_,
      sgnl_pt   = NA_real_,
      sgnl_any  = NA_real_
    )

  # Re-order to match SAS RETAIN: level soc hlgt hlt pt rd rr pv cc sgnl ...
  data_lead <- c("level", "soc", "hlgt", "hlt", "pt",
                 "rd", "rr", "pv", "cc",
                 "sgnl", "sgnl_soc", "sgnl_hlgt", "sgnl_hlt",
                 "sgnl_pt", "sgnl_any")
  data_present <- intersect(data_lead, colnames(meddra_data))
  data_rest    <- setdiff(colnames(meddra_data), data_present)
  meddra_data  <- meddra_data %>%
    dplyr::select(dplyr::all_of(data_present), dplyr::all_of(data_rest))

  # --- Step 5: meddra_cmp_data_row (SAS lines 527, 589-599) --------------
  # Row mapping in concatenation (data) order.
  meddra_data_row <- combined %>%
    dplyr::select(level, soc, hlgt, hlt, pt, row_data) %>%
    dplyr::rename(row = row_data)

  # --- Step 6: Sort for visible tab (SAS PROC SORT, lines 538-540) -------
  # SAS sorts missing values BEFORE non-missing.  R arrange() puts NA last.
  # Replace NA with "" for sorting characters, then restore — this gives
  # the SAS collation where level-1 rows (with hlgt_name=NA) appear before
  # deeper levels within the same soc_name.
  sorted <- combined %>%
    dplyr::mutate(
      .sort_soc  = dplyr::if_else(is.na(soc_name),  "", soc_name),
      .sort_hlgt = dplyr::if_else(is.na(hlgt_name), "", hlgt_name),
      .sort_hlt  = dplyr::if_else(is.na(hlt_name),  "", hlt_name),
      .sort_pt   = dplyr::if_else(is.na(pt_name),   "", pt_name)
    ) %>%
    dplyr::arrange(.sort_soc, .sort_hlgt, .sort_hlt, .sort_pt, level) %>%
    dplyr::select(-dplyr::starts_with(".sort_"))

  # --- Step 7: Visible-order row counter (SAS line 558) -------------------
  sorted <- sorted %>%
    dplyr::mutate(row = dplyr::row_number())

  # --- Step 8: meddra_cmp_output (visible analysis scaffold, SAS 544-566)
  # Initialise signal and summary columns to NA (SAS call missing()).
  meddra_output <- sorted %>%
    dplyr::mutate(
      sgnl     = NA_character_,
      sgnl_soc = NA_character_,
      sgnl_hlgt = NA_character_,
      sgnl_hlt  = NA_character_,
      sgnl_pt   = NA_character_,
      arm_exp_count = NA_real_,
      arm_exp_pct   = NA_real_,
      arm_ctl_count = NA_real_,
      arm_ctl_pct   = NA_real_,
      rd = NA_real_,
      rr = NA_real_,
      pv = NA_real_
    )

  if (cc_sw > 0L) {
    meddra_output <- meddra_output %>%
      dplyr::mutate(cc = NA_character_)
  }

  # Keep columns matching SAS meddra_cmp_output structure
  out_keep <- c("level", "soc_name", "hlgt_name", "hlt_name", "pt_name", "dme",
                "sgnl", "sgnl_soc", "sgnl_hlgt", "sgnl_hlt", "sgnl_pt",
                "arm_exp_count", "arm_exp_pct", "arm_ctl_count", "arm_ctl_pct",
                "rd", "rr")
  if (cc_sw > 0L) out_keep <- c(out_keep, "cc")
  out_keep <- c(out_keep, "pv", "row")
  out_keep <- intersect(out_keep, colnames(meddra_output))

  meddra_output <- meddra_output %>%
    dplyr::select(dplyr::all_of(out_keep))

  # --- Step 9: meddra_cmp_output_row (SAS line 548, 589-599) -------------
  meddra_output_row <- sorted %>%
    dplyr::select(level, soc, hlgt, hlt, pt, row)

  # --- Step 10: Add lvl_nm and lvl_no to both row datasets ---------------
  # (SAS lines 587-600: SELECT on level to derive name and ordinal number)
  add_level_info <- function(df) {
    df %>%
      dplyr::filter(level %in% 1L:4L) %>%
      dplyr::mutate(
        lvl_nm = dplyr::case_when(
          level == 1L ~ "soc",
          level == 2L ~ "hlgt",
          level == 3L ~ "hlt",
          level == 4L ~ "pt",
          TRUE        ~ NA_character_
        ),
        lvl_no = dplyr::case_when(
          level == 1L ~ as.numeric(soc),
          level == 2L ~ as.numeric(hlgt),
          level == 3L ~ as.numeric(hlt),
          level == 4L ~ as.numeric(pt),
          TRUE        ~ NA_real_
        )
      ) %>%
      dplyr::select(row, lvl_nm, lvl_no, soc, hlgt, hlt, pt)
  }

  meddra_data_row   <- add_level_info(meddra_data_row)
  meddra_output_row <- add_level_info(meddra_output_row)

  cli::cli_inform(c(
    "v" = "meddra_cmp assembled: {nrow(meddra_output)} output rows, {nrow(meddra_data)} data rows"
  ))

  # --- Return all four datasets -------------------------------------------
  list(
    meddra_cmp_output     = meddra_output,
    meddra_cmp_data       = meddra_data,
    meddra_cmp_data_row   = meddra_data_row,
    meddra_cmp_output_row = meddra_output_row
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %params macro replaced by meddra_params() which returns a
#      configuration list.  No global state is modified.
#    - Continuity-correction (CC) normalisation:
#        "arm"          -> cc_sw = 2 (reciprocal of opposite arm total)
#        numeric > 0    -> cc_sw = 1 (constant added to all 4 cells)
#        "none" / 0 / ""-> cc_sw = 0 (off)
#    - CC is applied ONLY when the comparator-arm count (c) equals zero,
#      matching SAS line 377 where checks for a=0, b=0, d=0 are commented
#      out in the original source.
#    - For cc_sw = 2 (reciprocal), the sequential SAS execution order is
#      preserved: a and b are modified first using original c+d, then c
#      and d are modified using the already-updated a+b.
#    - Fisher's exact test via stats::fisher.test() is called on the
#      ORIGINAL integer counts (not CC-adjusted values), matching SAS
#      PROC FREQ EXACT FISHER behaviour.
#    - Risk Difference (RD) is computed from ORIGINAL percentages
#      (arm_pct columns), not CC-adjusted proportions.
#    - Relative Risk (RR) is computed from CC-adjusted proportions
#      (post-CC a/(a+b) and c/(c+d)), matching SAS line 401.
#    - Negative log p-value uses natural log: -log(p), matching SAS
#      log() function which is the natural logarithm.
#    - Level numbering uses cumsum of first-occurrence flags, matching
#      SAS RETAIN + first.by_var cumulative counter semantics.
#    - meddra_cmp sorting replaces NA with "" for character columns to
#      match SAS collation where missing values sort before non-missing.
#    - RD and RR are computed for ALL i != j arm pairs; Fisher's test
#      is computed only for i < j pairs with symmetric pv assignment.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - fisher.test() p-values may differ from SAS PROC FREQ EXACT FISHER
#      at extreme edge cases due to algorithm implementation differences
#      (network algorithm vs. enumeration).
#    - Risk difference and relative risk are identical when the same CC
#      constants are applied; IEEE 754 double precision is used in both.
#    - Negative log p-value: -Inf arises when p = 0; handled as NA_real_.
#    - Sort stability for level numbering: dplyr::arrange() is stable
#      within groups in R >= 4.0, matching SAS PROC SORT stability.
#    - Percentages use janitor::round_half_up() at 10 decimal places to
#      preserve precision; final rounding to display digits is deferred
#      to the output layer.
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC FREQ EXACT FISHER -> stats::fisher.test()
#      (conceptually identical, implementation details differ)
#    - SAS hash lookup -> dplyr::left_join()
#    - SAS %goto / label -> R early return or if/else branching
#    - SAS global macro variables -> function return list elements
#    - SAS RETAIN across DATA step iterations -> dplyr group operations
#      and cumsum() for cumulative counters
#    - SAS SpreadsheetML XML generation -> handled by ae_meddra_output.R
#      using openxlsx (not in scope of this file)
#
# PACKAGE SELECTION RATIONALE:
#    - stats::fisher.test(): Base R; exact Fisher's test (standard,
#      no external dependency needed).
#    - dplyr: Core data manipulation; AAP mandates tidyverse over base R.
#    - tidyr: pivot_wider() for arm-specific columns; complete() for
#      zero-filling missing arm x term combinations.
#    - purrr: map2() for pairwise arm iteration; map_dfr() for
#      per-row Fisher test accumulation.  AAP mandates purrr over
#      base R lapply/sapply.
#    - rlang: syms() for programmatic column references in group_by();
#      .data pronoun for unambiguous column access; := for dynamic
#      column naming.
#    - tibble: tibble() for intermediate result construction.
#    - janitor: round_half_up() for SAS-compatible rounding (Gate 2).
#    - cli: Informative messages and structured errors replacing %PUT.
#
# OPEN QUESTIONS:
#    - Confirm SAS PROC FREQ EXACT FISHER produces a two-sided p-value
#      by default; R fisher.test() default is alternative = "two.sided"
#      which should match.
#    - Verify cc_sw = 2 reciprocal denominator: SAS uses opposite arm
#      total (c+d for treatment, a+b for comparator) — confirmed from
#      SAS source lines 387-392.
#    - Level numbering reset behaviour: the cumsum approach resets only
#      when the by-variable value changes; if two non-adjacent rows
#      share the same value, they receive the same level number.  This
#      matches SAS first.by_var semantics on already-sorted data.
#    - Fisher test on degenerate 2x2 tables (all zeros or single
#      non-zero cell): R returns p = 1; SAS may return missing.
#      tryCatch handles any error gracefully.
# ============================================================
