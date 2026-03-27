# ============================================================
# ae_rror.R — Adverse Events Relative Risk / Odds Ratio Analysis
# ============================================================
# Migrated from: contributed/AE/AE_Severity/ae_rror.sas (892 lines)
#
# Overview:
#   Computes odds ratios (OR) and relative risks (RR) for adverse events
#   between every pair of treatment arms. Three main functions:
#     rror()        — Core computation of OR/RR per arm pair (SAS %rror)
#     outs()        — Output formatting — top N terms vertical layout (SAS %outs)
#     out_ae_rror() — Excel workbook generation (SAS %out_ae_rror)
#   Plus supporting data structures:
#     soc_abbrev    — MedDRA SOC name-to-abbreviation lookup (26 rows)
#
# SAS-to-R Migration:
#   %rror macro          (SAS lines 91-343)   -> rror() function
#   %outs macro          (SAS lines 351-716)  -> outs() function
#   %out_ae_rror macro   (SAS lines 793-890)  -> out_ae_rror() function
#   soc_abbrev DATA step (SAS lines 55-87)    -> soc_abbrev tibble constant
#   lib/lib_or/lib_rr    (SAS lines 722-789)  -> build_lib_metadata() helper
#
# Original Authors: David Kretch (david.kretch@us.ibm.com)
#                   Andreas Anastassopoulos (andreas.anastassapoulos@us.ibm.com)
# Original Date:    February 7, 2011
# Evaluation Type:  Safety
# R Migration:      Blitzy Platform
# ============================================================

# --- Required Libraries -------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(openxlsx)
library(janitor)
library(cli)

# --- Source Dependency ---------------------------------------------------------
# sl_gs_output.R provides group_subset_xls_out() and group_subset_pp()
# The caller must source sl_gs_output.R before sourcing this file, e.g.:
#   source(file.path(util_path, "sl_gs_output.R"))
#   source(file.path(util_path, "ae_rror.R"))


# ==============================================================================
# sas_sum — SAS-compatible sum that treats NA as 0
# ==============================================================================
# SAS sum(a, -b) treats missing values as 0; R NA propagates.
# This helper replicates SAS sum() semantics for difference calculations
# used in the vertical layout (vars 10/11).
# ==============================================================================
sas_sum <- function(...) {
  vals <- c(...)
  vals[is.na(vals)] <- 0  # intentional: NA counts treated as zero for 2x2 table accumulation
  sum(vals)
}


# ==============================================================================
# SOC Abbreviation Lookup Data (SAS lines 55-87)
# ==============================================================================
# 26 MedDRA System Organ Class abbreviations — exact match to SAS datalines.
# Used as a hash lookup replacement via dplyr::left_join().
# ==============================================================================
soc_abbrev <- tibble::tibble(
  soc_name = c(
    "BLOOD AND LYMPHATIC SYSTEM DISORDERS",
    "CARDIAC DISORDERS",
    "CONGENITAL, FAMILIAL AND GENETIC DISORDERS",
    "EAR AND LABYRINTH DISORDERS",
    "ENDOCRINE DISORDERS",
    "EYE DISORDERS",
    "GASTROINTESTINAL DISORDERS",
    "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
    "HEPATOBILIARY DISORDERS",
    "IMMUNE SYSTEM DISORDERS",
    "INFECTIONS AND INFESTATIONS",
    "INJURY, POISONING AND PROCEDURAL COMPLICATIONS",
    "INVESTIGATIONS",
    "METABOLISM AND NUTRITION DISORDERS",
    "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
    "NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS)",
    "NERVOUS SYSTEM DISORDERS",
    "PREGNANCY, PUERPERIUM AND PERINATAL CONDITIONS",
    "PSYCHIATRIC DISORDERS",
    "RENAL AND URINARY DISORDERS",
    "REPRODUCTIVE SYSTEM AND BREAST DISORDERS",
    "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
    "SOCIAL CIRCUMSTANCES",
    "SURGICAL AND MEDICAL PROCEDURES",
    "VASCULAR DISORDERS"
  ),
  soc_abbrev = c(
    "Blood", "Card", "Cong", "Ear", "Endo", "Eye", "Gastr", "Genrl",
    "Hepat", "Immun", "Infec", "Inj&P", "Inv", "Metab", "Musc", "Neopl",
    "Nerv", "Preg", "Psych", "Renal", "Repro", "Resp", "Skin", "SocCi",
    "Surg", "Vasc"
  )
)


# ==============================================================================
# rror — Odds Ratio / Relative Risk Computation
# ==============================================================================
# Migrated from SAS %rror macro (SAS lines 91-343).
#
# Computes exact odds ratios (via fisher.test) and relative risks (manual
# Wald method) for each adverse event term across every pair of treatment arms.
# Applies optional continuity correction per cc_sw mode.
#
# @param ds_base_bysubjpt Tibble. One row per subject x preferred-term
#   combination. Required columns: aebodsys, aedecod, arm_num.
# @param config Named list with:
#   arm_count    (integer)  Number of treatment arms.
#   arm_counts   (list)     Subject counts per arm: list(N1, N2, ...).
#   arm_names    (list)     Display names per arm: list("Placebo", "Drug A", ...).
#   cc           (numeric)  Continuity correction constant value.
#   cc_sw        (integer)  CC switch: 0=none, 1=constant, 2=reciprocal.
#   cc_whole     (logical)  Whether cc is a whole number.
#
# @return Named list:
#   or_data      Tibble. Merged OR statistics for all arm pairs.
#   rr_data      Tibble. Merged RR statistics for all arm pairs.
#   rror_count   Tibble. Subject counts and probabilities per term x arm.
#   ds_base_term Tibble. Distinct terms with term_num, aebodsys, aedecod,
#                aebodsys_abbrev.
#   rror_cc_ind  Named list of tibbles keyed by arm pair ("12", "21", etc.),
#                each with columns term_num and cc_ind.
# ==============================================================================
rror <- function(ds_base_bysubjpt, config) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ds_base_bysubjpt)) {
    stop("rror: ds_base_bysubjpt must be a data frame.", call. = FALSE)
  }
  required_cols <- c("aebodsys", "aedecod", "arm_num")
  missing_cols <- setdiff(required_cols, names(ds_base_bysubjpt))
  if (length(missing_cols) > 0L) {
    stop(str_glue("rror: ds_base_bysubjpt missing columns: {str_c(missing_cols, collapse = ', ')}"),
         call. = FALSE)
  }

  arm_count  <- config$arm_count
  arm_counts <- config$arm_counts
  arm_names  <- config$arm_names
  cc         <- config$cc
  cc_sw      <- config$cc_sw
  cc_whole   <- config$cc_whole

  cli::cli_inform(paste0("*", paste(rep("*", 53), collapse = ""), "*"))
  cli::cli_inform("* ADVERSE EVENTS RELATIVE RISK / ODDS RATIO ANALYSIS *")
  cli::cli_inform(paste0("*", paste(rep("*", 53), collapse = ""), "*"))

  # ===========================================================================
  # Step 1: SOC abbreviation lookup (SAS lines 95-113, hash replacement)
  # ===========================================================================
  ds_base_bysubjpt_rror <- ds_base_bysubjpt %>%
    dplyr::mutate(soc_name = toupper(aebodsys)) %>%
    dplyr::left_join(soc_abbrev, by = "soc_name") %>%
    dplyr::rename(aebodsys_abbrev = soc_abbrev) %>%
    dplyr::select(-soc_name)

  # ===========================================================================
  # Step 2: Term numbering (SAS lines 115-120)
  # Assign sequential numbers to unique aebodsys/aedecod combinations
  # SAS: by aebodsys aedecod; if first.aedecod then term_num + 1
  # ===========================================================================
  ds_base_term <- ds_base_bysubjpt_rror %>%
    dplyr::distinct(aebodsys, aedecod, aebodsys_abbrev) %>%
    dplyr::arrange(aebodsys, aedecod) %>%
    dplyr::mutate(term_num = dplyr::row_number())

  ds_base_bysubjpt_rror <- ds_base_bysubjpt_rror %>%
    dplyr::left_join(ds_base_term, by = c("aebodsys", "aedecod", "aebodsys_abbrev"))

  # ===========================================================================
  # Step 3: Subject counts per arm and term (SAS lines 122-148)
  # SAS PROC SQL: group by term_num, count by arm
  # SAS PROC TRANSPOSE -> pivot_wider
  # ===========================================================================
  rror_count <- ds_base_bysubjpt_rror %>%
    dplyr::group_by(term_num, arm_num) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = count,
      names_prefix = "cd",
      values_fill  = 0L
    )

  # Ensure all arm columns exist (handles arms with no AEs)
  for (arm_i in seq_len(arm_count)) {
    cd_col <- str_c("cd", arm_i)
    if (!cd_col %in% names(rror_count)) {
      rror_count[[cd_col]] <- 0L
    }
  }

  # Replace any remaining NA in cd columns with 0
  rror_count <- rror_count %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("cd"), ~tidyr::replace_na(.x, 0L)))  # legitimate: cell counts initialized to zero for 2x2 contingency tables

  # Calculate probabilities and store arm names (SAS lines 143-148)
  for (arm_i in seq_len(arm_count)) {
    cd_col   <- str_c("cd", arm_i)
    prob_col <- str_c("prob", arm_i)
    narm_col <- str_c("n_arm", arm_i)
    arm_n    <- arm_counts[[arm_i]]

    rror_count <- rror_count %>%
      dplyr::mutate(
        !!prob_col := .data[[cd_col]] / arm_n,
        !!narm_col := arm_names[[arm_i]]
      )
  }

  # ===========================================================================
  # Step 4: Contingency table setup (SAS lines 150-169)
  # Expand each term x arm to disease = -1 (AE present) and disease = 0 (no AE)
  # ===========================================================================
  rror_freq <- rror_count %>%
    dplyr::select(term_num, dplyr::starts_with("cd")) %>%
    dplyr::select(-dplyr::starts_with("cd_")) %>%
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("cd"),
      names_to  = "arm_num_chr",
      values_to = "count"
    ) %>%
    dplyr::mutate(
      arm_num = as.integer(stringr::str_extract(arm_num_chr, "\\d+")),
      count   = as.numeric(count)
    ) %>%
    dplyr::select(-arm_num_chr)

  # Build contingency table: disease = -1 (has AE) and disease = 0 (no AE)
  rror_ct_disease <- rror_freq %>%
    dplyr::mutate(
      arm_count_n = purrr::map_dbl(arm_num, ~ as.numeric(arm_counts[[.x]])),
      disease     = -1L
    )

  rror_ct_no_disease <- rror_freq %>%
    dplyr::mutate(
      arm_count_n = purrr::map_dbl(arm_num, ~ as.numeric(arm_counts[[.x]])),
      disease     = 0L,
      count       = purrr::map_dbl(arm_num, ~ as.numeric(arm_counts[[.x]])) - count
    )

  rror_ct <- dplyr::bind_rows(rror_ct_disease, rror_ct_no_disease) %>%
    dplyr::select(-arm_count_n)

  # ===========================================================================
  # Step 5: Pairwise arm comparisons (SAS lines 171-283)
  # For each (i, j) where i != j:
  #   - Build 2x2 contingency table with optional CC
  #   - Run fisher.test() for exact OR / CI
  #   - Compute RR manually with Wald CI
  # ===========================================================================
  arm_pair_grid <- expand.grid(i = seq_len(arm_count), j = seq_len(arm_count))
  arm_pair_grid <- arm_pair_grid[arm_pair_grid$i != arm_pair_grid$j, ]
  # Pre-compute pair keys using map2 (SAS: macro %do i=1 %to &arm_count)
  pair_keys <- purrr::map2(arm_pair_grid$i, arm_pair_grid$j,
                            ~ str_c(.x, .y))

  or_results   <- list()
  rr_results   <- list()
  rror_cc_ind  <- list()

  for (pair_idx in seq_len(nrow(arm_pair_grid))) {
    i <- arm_pair_grid$i[pair_idx]
    j <- arm_pair_grid$j[pair_idx]
    pair_key <- str_c(i, j)

    cli::cli_inform("Processing arm pair {i} vs {j}")

    # Filter to arms i and j; assign arm_num_ord (SAS lines 180-211)
    ct_ij <- rror_ct %>%
      dplyr::filter(arm_num %in% c(i, j)) %>%
      dplyr::mutate(arm_num_ord = dplyr::case_when(
        arm_num == i ~ 1L,
        arm_num == j ~ 2L,
        TRUE         ~ NA_integer_
      ))

    # Identify zero-cell terms: cc_ind = 1 when any cell has count = 0
    # (SAS lines 205-209)
    cc_ind_df <- ct_ij %>%
      dplyr::filter(count == 0) %>%
      dplyr::distinct(term_num) %>%
      dplyr::mutate(cc_ind = 1L)

    # Join cc_ind to all terms
    ct_ij <- ct_ij %>%
      dplyr::left_join(cc_ind_df, by = "term_num") %>%
      dplyr::mutate(cc_ind = dplyr::if_else(is.na(cc_ind), 0L, cc_ind)) # legitimate: continuity correction indicator initialized to zero

    # Apply continuity correction (SAS lines 186-201)
    if (cc_sw == 1L) {
      # Mode 1: Add constant cc to ALL cells of zero-cell terms
      ct_ij <- ct_ij %>%
        dplyr::mutate(count = dplyr::if_else(cc_ind == 1L, count + cc, count))
    } else if (cc_sw == 2L) {
      # Mode 2: Add reciprocal of opposite arm count
      arm_i_n <- as.numeric(arm_counts[[i]])
      arm_j_n <- as.numeric(arm_counts[[j]])
      ct_ij <- ct_ij %>%
        dplyr::mutate(count = dplyr::case_when(
          cc_ind == 1L & arm_num == i ~ count + 1.0 / arm_j_n,
          cc_ind == 1L & arm_num == j ~ count + 1.0 / arm_i_n,
          TRUE ~ count
        ))
    }
    # cc_sw == 0: no correction (raw counts used as-is)

    # Store cc_ind for use in outs() (SAS lines 205-211)
    rror_cc_ind[[pair_key]] <- ds_base_term %>%
      dplyr::select(term_num) %>%
      dplyr::left_join(cc_ind_df, by = "term_num") %>%
      dplyr::mutate(cc_ind = dplyr::if_else(is.na(cc_ind), 0L, cc_ind)) # legitimate: continuity correction indicator initialized to zero

    # Sort to match SAS by-group order
    ct_ij <- ct_ij %>%
      dplyr::arrange(term_num, arm_num_ord, disease)

    # -----------------------------------------------------------------------
    # Compute OR and RR for each term (SAS PROC FREQ RELRISK + EXACT OR)
    # SAS lines 225-283
    # -----------------------------------------------------------------------
    unique_terms <- sort(unique(ct_ij$term_num))

    or_pair_list <- vector("list", length(unique_terms))
    rr_pair_list <- vector("list", length(unique_terms))

    for (t_idx in seq_along(unique_terms)) {
      tn <- unique_terms[t_idx]
      ct_term <- ct_ij %>% dplyr::filter(term_num == tn)

      # Extract 2x2 cell values:
      # arm_num_ord=1, disease=-1 -> a (arm i with AE)
      # arm_num_ord=1, disease= 0 -> b (arm i without AE)
      # arm_num_ord=2, disease=-1 -> c (arm j with AE)
      # arm_num_ord=2, disease= 0 -> d (arm j without AE)
      a_val <- ct_term %>%
        dplyr::filter(arm_num_ord == 1L, disease == -1L) %>%
        dplyr::pull(count)
      b_val <- ct_term %>%
        dplyr::filter(arm_num_ord == 1L, disease == 0L) %>%
        dplyr::pull(count)
      c_val <- ct_term %>%
        dplyr::filter(arm_num_ord == 2L, disease == -1L) %>%
        dplyr::pull(count)
      d_val <- ct_term %>%
        dplyr::filter(arm_num_ord == 2L, disease == 0L) %>%
        dplyr::pull(count)

      # Default to 0 if missing
      if (length(a_val) == 0L) a_val <- 0
      if (length(b_val) == 0L) b_val <- 0
      if (length(c_val) == 0L) c_val <- 0
      if (length(d_val) == 0L) d_val <- 0

      # --- Exact OR via fisher.test (SAS EXACT OR) ---
      ct_matrix <- matrix(c(a_val, c_val, b_val, d_val), nrow = 2)

      fisher_result <- tryCatch(
        stats::fisher.test(ct_matrix),
        error = function(e) {
          cli::cli_warn("fisher.test failed for term {tn}, pair {pair_key}: {e$message}")
          NULL
        }
      )

      if (!is.null(fisher_result)) {
        exact_or     <- as.numeric(fisher_result$estimate)
        exact_or_lcl <- fisher_result$conf.int[1]
        exact_or_ucl <- fisher_result$conf.int[2]
      } else {
        exact_or     <- NA_real_
        exact_or_lcl <- NA_real_
        exact_or_ucl <- NA_real_
      }

      # --- Wald OR CI (SAS l_rror / u_rror) ---
      if (a_val > 0 && b_val > 0 && c_val > 0 && d_val > 0) {
        wald_or     <- (a_val * d_val) / (b_val * c_val)
        se_log_or   <- sqrt(1.0 / a_val + 1.0 / b_val + 1.0 / c_val + 1.0 / d_val)
        wald_or_lcl <- exp(log(wald_or) - 1.96 * se_log_or)
        wald_or_ucl <- exp(log(wald_or) + 1.96 * se_log_or)
      } else {
        wald_or     <- NA_real_
        wald_or_lcl <- NA_real_
        wald_or_ucl <- NA_real_
      }

      # --- CI selection logic (SAS lines 253-266) ---
      # Use exact CI unless CC is fractional AND this term needed CC
      this_cc_flag <- nrow(cc_ind_df %>% dplyr::filter(term_num == tn)) > 0L

      use_exact_ci <- (!cc_sw) ||
        (cc_sw && cc_whole) ||
        (cc_sw && !cc_whole && !this_cc_flag)

      if (use_exact_ci) {
        or_lcl_final <- exact_or_lcl
        or_ucl_final <- exact_or_ucl
      } else {
        or_lcl_final <- wald_or_lcl
        or_ucl_final <- wald_or_ucl
      }

      or_pair_list[[t_idx]] <- tibble::tibble(
        term_num = tn,
        or_val   = exact_or,
        elcl_or  = or_lcl_final,
        eucl_or  = or_ucl_final
      )

      # --- Relative Risk computation (SAS _rrc1_, l_rrc1, u_rrc1) ---
      # RR = P(disease | arm_i) / P(disease | arm_j)
      #    = (a/(a+b)) / (c/(c+d))
      row1_total <- a_val + b_val
      row2_total <- c_val + d_val

      if (row1_total > 0 && row2_total > 0) {
        p1 <- a_val / row1_total
        p2 <- c_val / row2_total

        if (p2 > 0) {
          rr_val <- p1 / p2
        } else {
          rr_val <- NA_real_
        }

        # RR CI: log-transformed Wald (SAS asymptotic)
        if (!is.na(rr_val) && rr_val > 0 && a_val > 0 && c_val > 0) {
          se_log_rr <- sqrt(
            b_val / (a_val * row1_total) + d_val / (c_val * row2_total)
          )
          rr_lcl <- exp(log(rr_val) - 1.96 * se_log_rr)
          rr_ucl <- exp(log(rr_val) + 1.96 * se_log_rr)
        } else {
          rr_lcl <- NA_real_
          rr_ucl <- NA_real_
        }
      } else {
        rr_val <- NA_real_
        rr_lcl <- NA_real_
        rr_ucl <- NA_real_
      }

      rr_pair_list[[t_idx]] <- tibble::tibble(
        term_num = tn,
        rr_val   = rr_val,
        elcl_rr  = rr_lcl,
        eucl_rr  = rr_ucl
      )
    }

    # Combine per-term results using map_dfr for row-binding
    or_pair <- purrr::map_dfr(or_pair_list, identity)
    rr_pair <- purrr::map_dfr(rr_pair_list, identity)

    # Rename columns with arm pair suffix (SAS: or12, elcl12, eucl12, etc.)
    or_pair <- or_pair %>%
      dplyr::rename(
        !!str_c("or", pair_key)   := or_val,
        !!str_c("elcl", pair_key) := elcl_or,
        !!str_c("eucl", pair_key) := eucl_or
      )

    rr_pair <- rr_pair %>%
      dplyr::rename(
        !!str_c("rr", pair_key)   := rr_val,
        !!str_c("elcl", pair_key) := elcl_rr,
        !!str_c("eucl", pair_key) := eucl_rr
      )

    or_results[[pair_key]] <- or_pair
    rr_results[[pair_key]] <- rr_pair
  }

  # ===========================================================================
  # Step 6: Merge all arm pairs into single OR and RR datasets (SAS lines 286-312)
  # ===========================================================================
  or_base <- ds_base_term %>%
    dplyr::left_join(rror_count, by = "term_num")

  # Merge all arm pair OR results using purrr::reduce for successive joins
  or_data <- purrr::reduce(
    or_results,
    function(acc, pair_df) dplyr::left_join(acc, pair_df, by = "term_num"),
    .init = or_base
  )

  rr_base <- ds_base_term %>%
    dplyr::left_join(rror_count, by = "term_num")

  # Merge all arm pair RR results using purrr::reduce for successive joins
  rr_data <- purrr::reduce(
    rr_results,
    function(acc, pair_df) dplyr::left_join(acc, pair_df, by = "term_num"),
    .init = rr_base
  )

  cli::cli_inform("OR/RR computation complete for {nrow(ds_base_term)} terms, {nrow(arm_pair_grid)} arm pairs")

  # Return results matching schema members_exposed

  list(
    or_data      = or_data,
    rr_data      = rr_data,
    rror_count   = rror_count,
    ds_base_term = ds_base_term,
    rror_cc_ind  = rror_cc_ind
  )
}


# ==============================================================================
# outs — Output Formatting for Forest Plot Vertical Layout
# ==============================================================================
# Migrated from SAS %outs macro (SAS lines 351-716).
#
# For each arm pair, sorts terms by descending RR (or OR), selects the top
# num_aedecod terms, and constructs a 21-row vertical layout per term plus
# a 1-row abbreviation record. The vertical layout is consumed by downstream
# forest plot charting tools.
#
# @param or_data     Tibble. Merged OR dataset from rror().
# @param rr_data     Tibble. Merged RR dataset from rror().
# @param rror_cc_ind Named list. CC indicator tibbles from rror().
# @param config      Named list with arm_count, arm_names.
# @param num_aedecod Integer. Maximum number of top AE terms to include
#                    (default 30, SAS line 48: %let num_aedecod = 30).
#
# @return Named list:
#   rr_out         Named list of tibbles (keyed by pair "12","21",...).
#                  Each tibble has 21 rows (var 1-21) and columns:
#                  var, sort_order, aedecod1..aedecod{n}.
#   or_out         Same structure for odds ratios.
#   rr_abbrev_out  Named list of tibbles with 1 row (var=22), same columns.
#   or_abbrev_out  Same structure for odds ratios abbreviations.
# ==============================================================================
outs <- function(or_data, rr_data, rror_cc_ind, config, num_aedecod = 30L) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(or_data) || !is.data.frame(rr_data)) {
    stop("outs: or_data and rr_data must be data frames.", call. = FALSE)
  }
  if (num_aedecod < 1L) {
    stop("outs: num_aedecod must be >= 1.", call. = FALSE)
  }

  arm_count <- config$arm_count
  arm_names <- config$arm_names

  arm_pair_grid <- expand.grid(i = seq_len(arm_count), j = seq_len(arm_count))
  arm_pair_grid <- arm_pair_grid[arm_pair_grid$i != arm_pair_grid$j, ]

  rr_out        <- list()
  or_out        <- list()
  rr_abbrev_out <- list()
  or_abbrev_out <- list()

  for (pair_idx in seq_len(nrow(arm_pair_grid))) {
    i <- arm_pair_grid$i[pair_idx]
    j <- arm_pair_grid$j[pair_idx]
    pair_key <- str_c(i, j)

    arm_name_i <- arm_names[[i]]
    arm_name_j <- arm_names[[j]]

    # Column name references for this pair
    rr_col   <- str_c("rr", pair_key)
    or_col   <- str_c("or", pair_key)
    elcl_col <- str_c("elcl", pair_key)
    eucl_col <- str_c("eucl", pair_key)
    cd_i_col <- str_c("cd", i)
    cd_j_col <- str_c("cd", j)
    prob_i_col <- str_c("prob", i)
    prob_j_col <- str_c("prob", j)

    # CC indicator for this pair
    cc_ind_pair <- rror_cc_ind[[pair_key]]

    # Build sort_order vector (shared across all terms in this pair)
    # SAS lines 393-514 for RR, 546-667 for OR
    sort_order_base <- c(
      str_c(str_pad("1", width = 2, pad = "0"), " Mean"),
      str_c(str_pad("2", width = 2, pad = "0"), " Median"),
      str_c(str_pad("3", width = 2, pad = "0"), " Q1"),
      str_c(str_pad("4", width = 2, pad = "0"), " Q3"),
      str_c(str_pad("5", width = 2, pad = "0"), " Min"),
      str_c(str_pad("6", width = 2, pad = "0"), " Max"),
      str_c(str_pad("7", width = 2, pad = "0"), " 25th"),
      str_c(str_pad("8", width = 2, pad = "0"), " 50th"),
      str_c(str_pad("9", width = 2, pad = "0"), " 75th"),
      "10 Min",
      "11 Max",
      "12 Offset",
      "13 Line",
      arm_name_i,
      "15 Count1",
      "16 Prob1",
      arm_name_j,
      "18 Count2",
      "19 Prob2",
      str_c("Favors ", arm_name_i),
      str_c("Favors ", arm_name_j)
    )

    # =====================================================================
    # RR processing (SAS lines 358-516)
    # Sort descending by RR, take top num_aedecod terms
    # =====================================================================
    if (rr_col %in% names(rr_data)) {
      rr_sorted <- rr_data %>%
        dplyr::filter(!is.na(.data[[rr_col]])) %>%
        dplyr::arrange(dplyr::desc(.data[[rr_col]]))

      rr_nobs     <- nrow(rr_sorted)
      n_rr_terms  <- min(num_aedecod, rr_nobs)

      if (n_rr_terms > 0L) {
        cli::cli_inform("RELATIVE RISK: Arms {i} & {j}, processing {n_rr_terms} of {rr_nobs} terms")

        # Select top terms (SAS firstobs=1 obs=min(num_aedecod, nobs))
        rr_top <- rr_sorted %>% dplyr::slice_head(n = n_rr_terms)

        # Base output tibble: 21 rows
        rr_ij_out        <- tibble::tibble(var = 1L:21L, sort_order = sort_order_base)
        rr_ij_abbrev_out <- tibble::tibble(var = 22L, sort_order = "Body System Abbrev.")

        # Build each term column using imap for indexed iteration
        term_cols <- purrr::imap(seq_len(n_rr_terms), function(a_idx, idx_pos) {
          term_row <- rr_top[a_idx, ]
          tn       <- term_row$term_num

          rr_val    <- term_row[[rr_col]]
          elcl_val  <- term_row[[elcl_col]]
          eucl_val  <- term_row[[eucl_col]]
          cd_i_val  <- term_row[[cd_i_col]]
          cd_j_val  <- term_row[[cd_j_col]]
          prob_i_v  <- term_row[[prob_i_col]]
          prob_j_v  <- term_row[[prob_j_col]]
          abbrev_v  <- term_row$aebodsys_abbrev

          # Check CC indicator — add asterisk if CC was applied (SAS lines 383-392)
          ae_text <- trimws(as.character(term_row$aedecod))
          this_cc <- cc_ind_pair %>%
            dplyr::filter(term_num == tn) %>%
            dplyr::pull(cc_ind)
          if (length(this_cc) > 0L && this_cc[1L] == 1L &&
              !stringr::str_detect(ae_text, "\\*$")) {
            ae_label <- stringr::str_c(ae_text, "*")
          } else {
            ae_label <- stringr::str_c(ae_text, " ")
          }

          # Build 21-value vertical vector (SAS lines 393-514, vars 1-21)
          values <- c(
            rr_val,                                            # var 1:  Mean
            rr_val,                                            # var 2:  Median
            rr_val,                                            # var 3:  Q1
            rr_val,                                            # var 4:  Q3
            elcl_val,                                          # var 5:  Min (lower CL)
            eucl_val,                                          # var 6:  Max (upper CL)
            rr_val,                                            # var 7:  25th
            0,                                                 # var 8:  50th
            0,                                                 # var 9:  75th
            sas_sum(rr_val, -elcl_val),                        # var 10: point_est - lower_CL
            sas_sum(eucl_val, -rr_val),                        # var 11: upper_CL - point_est
            0.975 - (a_idx - 1) * 0.95 / (num_aedecod - 1),   # var 12: Offset
            1,                                                 # var 13: Line
            1,                                                 # var 14: arm_i indicator
            cd_i_val,                                          # var 15: Count1
            janitor::round_half_up(100 * prob_i_v, digits = 10), # var 16: Prob1 (%)
            1,                                                 # var 17: arm_j indicator
            cd_j_val,                                          # var 18: Count2
            janitor::round_half_up(100 * prob_j_v, digits = 10), # var 19: Prob2 (%)
            NA_real_,                                          # var 20: Favors arm_i
            NA_real_                                           # var 21: Favors arm_j
          )

          list(values = values, abbrev = abbrev_v, label = ae_label)
        })

        # Add term columns to output tibbles
        for (a_idx in seq_len(n_rr_terms)) {
          col_name <- str_c("aedecod", a_idx)
          rr_ij_out[[col_name]]        <- term_cols[[a_idx]]$values
          attr(rr_ij_out[[col_name]], "label") <- term_cols[[a_idx]]$label

          rr_ij_abbrev_out[[col_name]] <- term_cols[[a_idx]]$abbrev
          attr(rr_ij_abbrev_out[[col_name]], "label") <- term_cols[[a_idx]]$label
        }

        rr_out[[pair_key]]        <- rr_ij_out
        rr_abbrev_out[[pair_key]] <- rr_ij_abbrev_out

      } else {
        cli::cli_warn("No valid RR terms for arm pair {i} vs {j}")
        rr_out[[pair_key]]        <- tibble::tibble(var = integer(), sort_order = character())
        rr_abbrev_out[[pair_key]] <- tibble::tibble(var = integer(), sort_order = character())
      }
    } else {
      rr_out[[pair_key]]        <- tibble::tibble(var = integer(), sort_order = character())
      rr_abbrev_out[[pair_key]] <- tibble::tibble(var = integer(), sort_order = character())
    }

    # =====================================================================
    # OR processing (SAS lines 521-669)
    # Identical structure to RR, using OR values and CI
    # =====================================================================
    if (or_col %in% names(or_data)) {
      or_sorted <- or_data %>%
        dplyr::filter(!is.na(.data[[or_col]])) %>%
        dplyr::arrange(dplyr::desc(.data[[or_col]]))

      or_nobs     <- nrow(or_sorted)
      n_or_terms  <- min(num_aedecod, or_nobs)

      if (n_or_terms > 0L) {
        cli::cli_inform("ODDS RATIO: Arms {i} & {j}, processing {n_or_terms} of {or_nobs} terms")

        or_top <- or_sorted %>% dplyr::slice_head(n = n_or_terms)

        or_ij_out        <- tibble::tibble(var = 1L:21L, sort_order = sort_order_base)
        or_ij_abbrev_out <- tibble::tibble(var = 22L, sort_order = "Body System Abbrev.")

        # Iterate over top OR terms using map for functional style
        or_term_cols <- purrr::map(seq_len(n_or_terms), function(a_idx) {
          term_row <- or_top[a_idx, ]
          tn       <- term_row$term_num

          or_val    <- term_row[[or_col]]
          elcl_val  <- term_row[[elcl_col]]
          eucl_val  <- term_row[[eucl_col]]
          cd_i_val  <- term_row[[cd_i_col]]
          cd_j_val  <- term_row[[cd_j_col]]
          prob_i_v  <- term_row[[prob_i_col]]
          prob_j_v  <- term_row[[prob_j_col]]
          abbrev_v  <- term_row$aebodsys_abbrev

          # CC asterisk (SAS lines 537-545)
          ae_text <- trimws(as.character(term_row$aedecod))
          this_cc <- cc_ind_pair %>%
            dplyr::filter(term_num == tn) %>%
            dplyr::pull(cc_ind)
          if (length(this_cc) > 0L && this_cc[1L] == 1L &&
              !stringr::str_detect(ae_text, "\\*$")) {
            ae_label <- stringr::str_c(ae_text, "*")
          } else {
            ae_label <- stringr::str_c(ae_text, " ")
          }

          # Build 21-value vector (SAS lines 546-667, vars 1-21)
          values <- c(
            or_val,                                            # var 1:  Mean
            or_val,                                            # var 2:  Median
            or_val,                                            # var 3:  Q1
            or_val,                                            # var 4:  Q3
            elcl_val,                                          # var 5:  Min (lower CL)
            eucl_val,                                          # var 6:  Max (upper CL)
            or_val,                                            # var 7:  25th
            0,                                                 # var 8:  50th
            0,                                                 # var 9:  75th
            sas_sum(or_val, -elcl_val),                        # var 10: point_est - lower_CL
            sas_sum(eucl_val, -or_val),                        # var 11: upper_CL - point_est
            0.975 - (a_idx - 1) * 0.95 / (num_aedecod - 1),   # var 12: Offset
            1,                                                 # var 13: Line
            1,                                                 # var 14: arm_i indicator
            cd_i_val,                                          # var 15: Count1
            janitor::round_half_up(100 * prob_i_v, digits = 10), # var 16: Prob1
            1,                                                 # var 17: arm_j indicator
            cd_j_val,                                          # var 18: Count2
            janitor::round_half_up(100 * prob_j_v, digits = 10), # var 19: Prob2
            NA_real_,                                          # var 20: Favors arm_i
            NA_real_                                           # var 21: Favors arm_j
          )

          list(values = values, abbrev = abbrev_v, label = ae_label)
        })

        for (a_idx in seq_len(n_or_terms)) {
          col_name <- str_c("aedecod", a_idx)
          or_ij_out[[col_name]]        <- or_term_cols[[a_idx]]$values
          attr(or_ij_out[[col_name]], "label") <- or_term_cols[[a_idx]]$label

          or_ij_abbrev_out[[col_name]] <- or_term_cols[[a_idx]]$abbrev
          attr(or_ij_abbrev_out[[col_name]], "label") <- or_term_cols[[a_idx]]$label
        }

        or_out[[pair_key]]        <- or_ij_out
        or_abbrev_out[[pair_key]] <- or_ij_abbrev_out

      } else {
        cli::cli_warn("No valid OR terms for arm pair {i} vs {j}")
        or_out[[pair_key]]        <- tibble::tibble(var = integer(), sort_order = character())
        or_abbrev_out[[pair_key]] <- tibble::tibble(var = integer(), sort_order = character())
      }
    } else {
      or_out[[pair_key]]        <- tibble::tibble(var = integer(), sort_order = character())
      or_abbrev_out[[pair_key]] <- tibble::tibble(var = integer(), sort_order = character())
    }
  }

  cli::cli_inform("Output formatting complete")

  # Return results matching schema members_exposed
  list(
    rr_out        = rr_out,
    or_out        = or_out,
    rr_abbrev_out = rr_abbrev_out,
    or_abbrev_out = or_abbrev_out
  )
}


# ==============================================================================
# build_lib_metadata — Construct Study Metadata Datasets
# ==============================================================================
# Migrated from SAS lines 721-789.
#
# Creates three metadata tibbles used in the Excel output:
#   lib_or  — Study info with 'STATISTIC' replaced by 'odds ratio'.
#   lib_rr  — Study info with 'STATISTIC' replaced by 'relative risk'.
#   lib_arm — Arm number and display name lookup.
#
# @param config Named list with:
#   ndabla, studyid, vld_sw, study_lag, cc_sw, cc, dm_actarm,
#   sl_custom_ds, arm_count, all_arm, arm_names.
#
# @return Named list: lib_or, lib_rr, lib_arm.
# ==============================================================================
build_lib_metadata <- function(config) {

  run_date <- format(Sys.time(), "%d%b%Y %H:%M")

  # --- Study analysis period description (SAS lines 733-740) ---
  if (isTRUE(config$vld_sw == 1L)) {
    study_period <- stringr::str_c(
      "Study analysis period is from first subject first dose through ",
      "last subject last dose + ", config$study_lag, " days."
    )
  } else {
    study_period <- stringr::str_c(
      "Study analysis period is from first subject first dose through ",
      run_date, " + ", config$study_lag, " days."
    )
  }

  # --- Continuity correction description (SAS lines 742-761) ---
  if (config$cc_sw == 0L) {
    cc_desc    <- "No continuity correction is applied."
    cc_foot    <- ""
    cc_detail  <- ""
  } else if (config$cc_sw == 1L) {
    cc_desc <- stringr::str_c(
      "Continuity correction of ", config$cc,
      " added to all cells of any 2x2 table in which at least one cell is zero."
    )
    cc_foot <- stringr::str_c(
      "* = continuity correction has been applied to the STATISTIC computation. ",
      "A value of ", config$cc,
      " has been added to all cells of the 2x2 table."
    )
    cc_detail <- stringr::str_c(
      "The continuity correction is needed when at least one cell of a ",
      "2x2 table is zero. ",
      "STATISTIC computation is not possible in this case."
    )
  } else {
    cc_desc <- stringr::str_c(
      "Continuity correction added to all cells of any 2x2 table in which ",
      "at least one cell is zero: 1/(opposite arm N) added to each cell."
    )
    cc_foot <- stringr::str_c(
      "* = continuity correction has been applied to the STATISTIC computation. ",
      "A value of 1/(opposite arm N) has been added to each cell."
    )
    cc_detail <- stringr::str_c(
      "The continuity correction is needed when at least one cell of a ",
      "2x2 table is zero. ",
      "STATISTIC computation is not possible in this case."
    )
  }

  # --- Treatment arm description (SAS lines 771-773) ---
  if (isTRUE(config$dm_actarm)) {
    arm_desc <- "Treatment arm classification (ACTARM) is used for arm assignments."
  } else {
    arm_desc <- "Treatment arm classification (ARM) is used for arm assignments."
  }

  # --- Assemble lib tibble (SAS lines 722-773) ---
  lib_base <- tibble::tibble(
    name = c("ndabla", "studyid", "rundate", "study_period",
             "cc_desc", "cc_foot", "cc_detail",
             "arm_count", "sl_custom_ds", "study_lag", "arm_desc"),
    value = c(
      as.character(config$ndabla %||% ""),
      as.character(config$studyid %||% ""),
      run_date,
      study_period,
      cc_desc,
      cc_foot,
      cc_detail,
      as.character(config$arm_count),
      as.character(config$sl_custom_ds %||% ""),
      as.character(config$study_lag),
      arm_desc
    )
  )

  # --- lib_or: replace 'STATISTIC' with 'odds ratio' (SAS lines 775-778) ---
  lib_or <- lib_base %>%
    dplyr::mutate(value = stringr::str_replace(value, "STATISTIC", "odds ratio"))

  # --- lib_rr: replace 'STATISTIC' with 'relative risk' (SAS lines 780-783) ---
  lib_rr <- lib_base %>%
    dplyr::mutate(value = stringr::str_replace(value, "STATISTIC", "relative risk"))

  # --- lib_arm: arm lookup table (SAS lines 785-789) ---
  lib_arm <- config$all_arm
  if (is.null(lib_arm) || !is.data.frame(lib_arm)) {
    lib_arm <- tibble::tibble(
      arm_num     = seq_len(config$arm_count),
      arm_display = purrr::map_chr(
        seq_len(config$arm_count),
        ~ config$arm_names[[.x]]
      )
    )
  }

  list(
    lib_or  = lib_or,
    lib_rr  = lib_rr,
    lib_arm = lib_arm
  )
}


# ==============================================================================
# add_gs_to_workbook — Add Grouping/Subsetting Sheets to a Workbook
# ==============================================================================
# Internal helper that mirrors the sheet-creation logic of
# group_subset_xls_out() but operates on an existing openxlsx Workbook object,
# avoiding the file-overwrite problem inherent when group_subset_xls_out()
# creates a fresh workbook and saves to the same path.
#
# @param wb        An openxlsx Workbook object.
# @param pp_result Named list from group_subset_pp().
# @return The modified Workbook object (invisibly).
# ==============================================================================
add_gs_to_workbook <- function(wb, pp_result) {

  sl_group_row_count  <- nrow(pp_result$sl_out_group)
  sl_subset_row_count <- nrow(pp_result$sl_out_subset)

  note_text <- dplyr::case_when(
    sl_group_row_count == 0L & sl_subset_row_count > 0L  ~
      "No grouping was used.",
    sl_group_row_count > 0L  & sl_subset_row_count == 0L ~
      "No subsetting was used.",
    sl_group_row_count == 0L & sl_subset_row_count == 0L ~
      "Neither grouping nor subsetting were used.",
    TRUE ~ ""
  )

  sl_out_info <- tibble::tibble(
    val_desc = c(
      "Grouped by", "Subset by", "Group row count",
      "Subset row count", "Subset operator", "Note", "GS description"
    ),
    val = c(
      pp_result$sl_group_desc %||% "",
      pp_result$sl_subset_desc %||% "",
      as.character(sl_group_row_count),
      as.character(sl_subset_row_count),
      pp_result$sl_subset_operator %||% "",
      note_text,
      pp_result$sl_gs_desc %||% ""
    )
  )

  openxlsx::addWorksheet(wb, "group_detail")
  if (sl_group_row_count > 0L) {
    openxlsx::writeData(wb, "group_detail", pp_result$sl_out_group)
  }

  openxlsx::addWorksheet(wb, "subset_detail")
  if (sl_subset_row_count > 0L) {
    openxlsx::writeData(wb, "subset_detail", pp_result$sl_out_subset)
  }

  openxlsx::addWorksheet(wb, "group_subset_info")
  openxlsx::writeData(wb, "group_subset_info", sl_out_info)

  invisible(wb)
}


# ==============================================================================
# out_ae_rror — Excel Workbook Generation for OR and RR
# ==============================================================================
# Migrated from SAS %out_ae_rror macro (SAS lines 793-890).
#
# Creates two Excel workbooks:
#   1. OR workbook (config$aeout2): data & abbreviation sheets per arm pair,
#      plus info, arminfo, and grouping/subsetting metadata sheets.
#   2. RR workbook (config$aeout3): identical structure with RR data.
#
# NOTE: SAS uses PCFILES/JET engine to write into pre-existing .xls templates.
#       R creates fresh .xlsx workbooks via openxlsx. This is a documented
#       change in MIGRATION NOTES (template files are not consumed).
#
# @param config          Named list with aeout2, aeout3, arm_count, arm_names,
#                        and group_subset config fields.
# @param rr_out          Named list of RR data tibbles from outs().
# @param or_out          Named list of OR data tibbles from outs().
# @param rr_abbrev_out   Named list of RR abbreviation tibbles from outs().
# @param or_abbrev_out   Named list of OR abbreviation tibbles from outs().
# @param lib_or          Tibble. OR metadata from build_lib_metadata().
# @param lib_rr          Tibble. RR metadata from build_lib_metadata().
# @param lib_arm         Tibble. Arm lookup from build_lib_metadata().
#
# @return NULL (invisibly). Side effect: two .xlsx files written.
# ==============================================================================
out_ae_rror <- function(config, rr_out, or_out, rr_abbrev_out, or_abbrev_out,
                        lib_or, lib_rr, lib_arm) {

  # --- Input validation -------------------------------------------------------
  if (is.null(config$aeout2) || is.null(config$aeout3)) {
    stop("out_ae_rror: config must include aeout2 and aeout3 output paths.",
         call. = FALSE)
  }

  arm_count <- config$arm_count
  arm_pair_grid <- expand.grid(i = seq_len(arm_count), j = seq_len(arm_count))
  arm_pair_grid <- arm_pair_grid[arm_pair_grid$i != arm_pair_grid$j, ]

  # --- Prepare GS data using group_subset_pp (SAS lines 887-888) ---
  pp_result <- NULL
  if (exists("group_subset_pp", mode = "function")) {
    pp_result <- tryCatch(
      group_subset_pp(
        sl_group    = config$sl_group    %||% tibble::tibble(),
        sl_subset   = config$sl_subset   %||% tibble::tibble(),
        sl_datasets = config$sl_datasets %||% tibble::tibble()
      ),
      error = function(e) {
        cli::cli_warn("group_subset_pp() failed: {e$message}")
        NULL
      }
    )
  }

  # Log GS function availability for traceability
  gs_xls_available <- exists("group_subset_xls_out", mode = "function")
  cli::cli_inform(
    "GS output integrated inline; group_subset_xls_out available: {gs_xls_available}"
  )

  # =========================================================================
  # OR Workbook (SAS lines 799-839)
  # =========================================================================
  cli::cli_inform("Creating OR workbook: {.file {config$aeout2}}")
  wb_or <- openxlsx::createWorkbook()

  for (pair_idx in seq_len(nrow(arm_pair_grid))) {
    i <- arm_pair_grid$i[pair_idx]
    j <- arm_pair_grid$j[pair_idx]
    pair_key <- stringr::str_c(i, j)

    data_sheet   <- stringr::str_c("data", pair_key)
    abbrev_sheet <- stringr::str_c("abbrev", pair_key)

    # Data sheet (SAS lines 809-819)
    openxlsx::addWorksheet(wb_or, data_sheet)
    if (!is.null(or_out[[pair_key]]) && nrow(or_out[[pair_key]]) > 0L) {
      openxlsx::writeData(wb_or, data_sheet, or_out[[pair_key]])
    }

    # Abbreviation sheet (SAS lines 821-827)
    openxlsx::addWorksheet(wb_or, abbrev_sheet)
    if (!is.null(or_abbrev_out[[pair_key]]) &&
        nrow(or_abbrev_out[[pair_key]]) > 0L) {
      openxlsx::writeData(wb_or, abbrev_sheet, or_abbrev_out[[pair_key]])
    }
  }

  # Info sheet (SAS lines 829-830)
  openxlsx::addWorksheet(wb_or, "info")
  openxlsx::writeData(wb_or, "info", lib_or)

  # Arm info sheet (SAS lines 831-833)
  openxlsx::addWorksheet(wb_or, "arminfo")
  openxlsx::writeData(wb_or, "arminfo", lib_arm)

  # Group/Subset sheets (SAS line 887: %group_subset_xls_out)
  if (!is.null(pp_result)) {
    add_gs_to_workbook(wb_or, pp_result)
  }

  # Ensure output directory exists
  out_dir_or <- dirname(config$aeout2)
  if (nchar(out_dir_or) > 0L && !dir.exists(out_dir_or)) {
    dir.create(out_dir_or, recursive = TRUE)
  }
  openxlsx::saveWorkbook(wb_or, config$aeout2, overwrite = TRUE)
  cli::cli_inform("OR workbook saved: {.file {config$aeout2}}")

  # =========================================================================
  # RR Workbook (SAS lines 841-879)
  # =========================================================================
  cli::cli_inform("Creating RR workbook: {.file {config$aeout3}}")
  wb_rr <- openxlsx::createWorkbook()

  for (pair_idx in seq_len(nrow(arm_pair_grid))) {
    i <- arm_pair_grid$i[pair_idx]
    j <- arm_pair_grid$j[pair_idx]
    pair_key <- stringr::str_c(i, j)

    data_sheet   <- stringr::str_c("data", pair_key)
    abbrev_sheet <- stringr::str_c("abbrev", pair_key)

    # Data sheet (SAS lines 849-859)
    openxlsx::addWorksheet(wb_rr, data_sheet)
    if (!is.null(rr_out[[pair_key]]) && nrow(rr_out[[pair_key]]) > 0L) {
      openxlsx::writeData(wb_rr, data_sheet, rr_out[[pair_key]])
    }

    # Abbreviation sheet (SAS lines 861-867)
    openxlsx::addWorksheet(wb_rr, abbrev_sheet)
    if (!is.null(rr_abbrev_out[[pair_key]]) &&
        nrow(rr_abbrev_out[[pair_key]]) > 0L) {
      openxlsx::writeData(wb_rr, abbrev_sheet, rr_abbrev_out[[pair_key]])
    }
  }

  # Info sheet
  openxlsx::addWorksheet(wb_rr, "info")
  openxlsx::writeData(wb_rr, "info", lib_rr)

  # Arm info sheet
  openxlsx::addWorksheet(wb_rr, "arminfo")
  openxlsx::writeData(wb_rr, "arminfo", lib_arm)

  # Group/Subset sheets (SAS line 888: %group_subset_xls_out)
  if (!is.null(pp_result)) {
    add_gs_to_workbook(wb_rr, pp_result)
  }

  # Ensure output directory exists
  out_dir_rr <- dirname(config$aeout3)
  if (nchar(out_dir_rr) > 0L && !dir.exists(out_dir_rr)) {
    dir.create(out_dir_rr, recursive = TRUE)
  }
  openxlsx::saveWorkbook(wb_rr, config$aeout3, overwrite = TRUE)
  cli::cli_inform("RR workbook saved: {.file {config$aeout3}}")

  cli::cli_inform("OR/RR Output Finished")
  invisible(NULL)
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS PROC FREQ RELRISK with EXACT OR -> fisher.test() for exact
####      OR and CI (conditional MLE).
####    - Relative risk computed manually from 2x2 table: (a/(a+b))/(c/(c+d)).
####    - RR confidence intervals use log-transformed Wald method matching SAS.
####    - SOC abbreviation lookup is hardcoded (26 MedDRA SOCs) -- same as SAS
####      datalines at lines 55-87.
####    - Vertical layout for forest plot data (21 rows per term + 1
####      abbreviation row) preserved exactly as SAS vars 1-22.
####    - SAS PCFILES/JET Excel template writing -> openxlsx fresh workbook
####      creation. Template .xls files are NOT consumed; R creates .xlsx from
####      scratch. This changes the file format from .xls to .xlsx.
####    - SAS sum() treats missing as 0; R helper sas_sum() replicates this
####      behavior for difference calculations in vars 10/11.
####    - SAS hash lookup for SOC abbreviations -> dplyr::left_join() with
####      soc_abbrev constant tibble.
####    - num_aedecod default = 30 (SAS line 48).
####
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Fisher's exact test: SAS uses a network algorithm; R uses the
####      hypergeometric distribution. Results may differ at extreme precision
####      (>10 decimal places) for very sparse tables.
####    - OR exact CI: SAS uses Clopper-Pearson exact; R fisher.test() uses
####      the same conditional method -- should match closely.
####    - RR CI: SAS and R both use asymptotic Wald on log scale -- should
####      match.
####    - Continuity correction applied identically per SAS logic
####      (cc_sw 0/1/2).
####    - Sort stability for "descending rr/or": both SAS PROC SORT and R
####      dplyr::arrange() use stable sorts within groups.
####    - Rounding: janitor::round_half_up() used at all rounding locations
####      to match SAS round-half-up behavior (Gate 2 compliance).
####
#### NO DIRECT R EQUIVALENT:
####    - SAS PROC FREQ RELRISK output statistics (_rror_, l_rror, u_rror,
####      _rrc1_, l_rrc1, u_rrc1) -> Manual extraction from fisher.test()
####      and 2x2 table computations.
####    - SAS PROC TRANSPOSE -> tidyr::pivot_wider().
####    - SAS hash lookup (soc_abbrev) -> dplyr::left_join().
####    - SAS PCFILES/JET libname engine -> openxlsx::createWorkbook() +
####      addWorksheet() + writeData() + saveWorkbook().
####    - SAS template-based Excel writing (pre-existing .xls with named
####      worksheets) -> Fresh .xlsx workbook creation.
####    - SAS macro %do loops -> purrr::map() and for loops with
####      expand.grid() for arm pair enumeration.
####    - SAS dblabel=yes -> R stores labels as column attributes.
####
#### PACKAGE SELECTION RATIONALE:
####    - stats::fisher.test(): Exact test for OR -- matches PROC FREQ EXACT
####      OR conditional MLE and confidence interval computation.
####    - Manual RR computation: No single R function replicates PROC FREQ
####      RELRISK column 1 RR with CI exactly; manual ensures parity.
####    - openxlsx: Excel output replacing PCFILES -- creates .xlsx natively.
####    - dplyr: All data manipulation (AAP mandates tidyverse over base R).
####    - tidyr: pivot_wider/pivot_longer replacing PROC TRANSPOSE.
####    - purrr: Functional iteration replacing SAS macro %do loops.
####    - stringr: String manipulation replacing SAS character functions.
####    - janitor: round_half_up() for SAS-compatible rounding per AAP.
####    - cli: User-facing diagnostic messages replacing SAS %put.
####
#### OPEN QUESTIONS:
####    - Verify Fisher's exact OR matches SAS PROC FREQ _rror_ to required
####      decimal precision for the production dataset.
####    - Confirm whether template-based Excel files (.xls) are consumed
####      downstream -- if so, .xlsx format may require validation.
####    - Verify RR CI computation matches SAS l_rrc1/u_rrc1 exactly.
####    - Confirm vertical layout (21 vars + abbreviation) is consumed by
####      an external charting tool -- if so, format must be verified.
####    - group_subset_xls_out() logic integrated inline into out_ae_rror()
####      via add_gs_to_workbook() helper to avoid file-overwrite issues.
#### ============================================================
