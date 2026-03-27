# =============================================================================
# PROGRAM NAME: ae_rror.R
#
# DESCRIPTION:  Adverse Events Relative Risk / Odds Ratio Analysis + Excel Export
#               Computes pairwise relative risk (RR) and odds ratio (OR) for
#               top adverse events across treatment arms, reshapes results into
#               vertical Excel worksheet layout, writes metadata, and exports
#               to two Excel workbooks (OR and RR).
#
# ORIGINAL SAS: tested/SAS/macros/ae_rror.sas (892 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
#               Andreas Anastassopoulos (andreas.anastassapoulos@us.ibm.com)
# SAS DATE:     February 7, 2011
#
# SAS MACROS MIGRATED:
#   %rror          (lines 91-343)  -> compute_rror()
#   %outs          (lines 351-718) -> reshape_rror_for_excel()
#   %out_ae_rror   (lines 793-892) -> write_rror_workbooks()
#   DATA soc_abbrev (lines 55-87)  -> soc_abbreviations()
#
# R REQUIRES:   dplyr (>=1.1.0), tidyr (>=1.3.0), purrr (>=1.0.0),
#               tibble (>=3.2.0), openxlsx (>=4.2.5), janitor (>=2.2.0),
#               stringr (>=1.5.0), rlang (>=1.1.0), cli (>=3.6.0),
#               stats (>=4.3.0)
#
# INTERNAL DEPENDENCY: tested/R/utilities/sl_gs_output.R
#   Used function: group_subset_write_xlsx()
#
# EVALUATION TYPE: Safety
#
# NOTES:        All SAS hash lookups replaced by dplyr::left_join().
#               SAS PROC FREQ RELRISK/EXACT OR replaced by manual OR/RR
#               calculation + stats::fisher.test() for exact CIs.
#               SAS PCFILES/JET engine replaced by openxlsx direct file I/O.
#               SAS macro parameters become named R function arguments.
# =============================================================================

# --- Load required packages ---
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(openxlsx)
library(janitor)
library(stringr)
library(rlang)
library(cli)

# --- Source internal dependency ---
# group_subset_write_xlsx from sl_gs_output.R replaces SAS
# %group_subset_xls_out(gs_file=...) calls at SAS lines 887-888
local({
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )
  gs_path <- if (!is.null(this_dir)) {
    file.path(this_dir, "..", "utilities", "sl_gs_output.R")
  } else {
    fp <- "tested/R/utilities/sl_gs_output.R"
    if (file.exists(fp)) fp else file.path("..", "utilities", "sl_gs_output.R")
  }
  if (file.exists(gs_path) &&
      !exists("group_subset_write_xlsx", mode = "function")) {
    source(gs_path, local = FALSE)
  }
})


# =============================================================================
# soc_abbreviations
# =============================================================================
#' Build MedDRA SOC abbreviation lookup tibble.
#'
#' Replaces the SAS inline DATA step (lines 55-87 of ae_rror.sas) that creates
#' the soc_abbrev dataset mapping MedDRA System Organ Class full names to
#' standard abbreviations. Contains 26 SOC entries.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{soc_name}{Character. Uppercase MedDRA SOC full name.}
#'     \item{soc_abbrev}{Character. Standard SOC abbreviation (<=10 chars).}
#'   }
#'
#' @export
#'
#' @examples
#' soc_tbl <- soc_abbreviations()
#' nrow(soc_tbl)  # 26
soc_abbreviations <- function() {
  tibble::tibble(
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
      "Blood", "Card", "Cong", "Ear", "Endo", "Eye",
      "Gastr", "Genrl", "Hepat", "Immun", "Infec", "Inj&P",
      "Inv", "Metab", "Musc", "Neopl", "Nerv", "Preg",
      "Psych", "Renal", "Repro", "Resp", "Skin", "SocCi",
      "Surg", "Vasc"
    )
  )
}


# =============================================================================
# compute_rror
# =============================================================================
#' Compute pairwise relative risk and odds ratio for adverse events.
#'
#' Replaces SAS \code{%rror} macro (lines 91-343 of ae_rror.sas). For every
#' ordered pair of treatment arms (i, j), builds 2x2 contingency tables per
#' AE term, applies optional continuity correction, and computes:
#'
#' \itemize{
#'   \item Sample Odds Ratio: \code{(a*d)/(b*c)} — matches SAS \code{_rror_}
#'   \item Exact OR CI via \code{fisher.test()} — matches SAS \code{xl_rror/xu_rror}
#'   \item Wald OR CI — matches SAS \code{l_rror/u_rror}
#'   \item Relative Risk: \code{(a/(a+b))/(c/(c+d))} — matches SAS \code{_rrc1_}
#'   \item Wald RR CI — matches SAS \code{l_rrc1/u_rrc1}
#' }
#'
#' @param ds_base_bysubjpt  A data.frame/tibble with one row per subject per
#'   AE term. Required columns: \code{aebodsys}, \code{aedecod}, \code{arm_num}.
#' @param arm_count  Integer. Number of treatment arms.
#' @param arm_names  Character vector of length \code{arm_count}. Arm display names.
#' @param arm_subjcnt  Numeric vector of length \code{arm_count}. Total subjects per arm.
#' @param cc_sw  Integer. Continuity correction switch:
#'   0 = none, 1 = constant, 2 = reciprocal of opposite arm.
#' @param cc_whole  Integer/logical. Whether the CC value is a whole number.
#' @param cc  Numeric. Continuity correction constant (used when \code{cc_sw = 1}).
#' @param num_aedecod  Integer. Number of top AE terms to include (default 30).
#'
#' @return A named list:
#'   \describe{
#'     \item{term_data}{Tibble of term metadata (term_num, aebodsys, aedecod, aebodsys_abbrev).}
#'     \item{arm_counts}{Tibble of per-arm per-term counts and probabilities (wide format).}
#'     \item{arm_names}{Character vector of arm names.}
#'     \item{pair_or}{Named list: each element \code{"i_j"} is a tibble with OR results per term.}
#'     \item{pair_rr}{Named list: each element \code{"i_j"} is a tibble with RR results per term.}
#'     \item{rror_cc_ind}{Named list: each element \code{"i_j"} is a tibble with CC indicator per term.}
#'     \item{or_nobs}{Integer. Total number of distinct AE terms.}
#'     \item{rr_nobs}{Integer. Total number of distinct AE terms.}
#'   }
#'
#' @export
compute_rror <- function(ds_base_bysubjpt,
                         arm_count,
                         arm_names,
                         arm_subjcnt,
                         cc_sw,
                         cc_whole,
                         cc,
                         num_aedecod = 30L) {

  # --- Input validation ---
  stopifnot(
    is.data.frame(ds_base_bysubjpt),
    is.numeric(arm_count) && arm_count >= 1L,
    is.character(arm_names) && length(arm_names) == arm_count,
    is.numeric(arm_subjcnt) && length(arm_subjcnt) == arm_count,
    is.numeric(cc_sw),
    is.numeric(num_aedecod) && num_aedecod >= 1L
  )
  num_aedecod <- as.integer(num_aedecod)
  arm_count <- as.integer(arm_count)

  cli::cli_inform(paste(rep("*", 56), collapse = ""))
  cli::cli_inform("* ADVERSE EVENTS RELATIVE RISK / ODDS RATIO ANALYSIS *")
  cli::cli_inform(paste(rep("*", 56), collapse = ""))

  # -----------------------------------------------------------------------
  # Step 1: Assign sequential term_num by AEBODSYS/AEDECOD (SAS lines 95-120)
  # SAS: by aebodsys aedecod; if first.aedecod then term_num + 1;
  # -----------------------------------------------------------------------
  term_data <- ds_base_bysubjpt %>%
    dplyr::distinct(.data$aebodsys, .data$aedecod) %>%
    dplyr::arrange(.data$aebodsys, .data$aedecod) %>%
    dplyr::mutate(term_num = dplyr::row_number())

  # -----------------------------------------------------------------------
  # Step 2: Join SOC abbreviations (SAS hash lookup, lines 100-113)
  # SAS: declare hash h(dataset:'soc_abbrev'); rc = h.find();
  # R:   dplyr::left_join on uppercase SOC name
  # -----------------------------------------------------------------------
  soc_lookup <- soc_abbreviations()
  term_data <- term_data %>%
    dplyr::mutate(soc_name = stringr::str_to_upper(.data$aebodsys)) %>%
    dplyr::left_join(soc_lookup, by = "soc_name") %>%
    dplyr::rename(aebodsys_abbrev = "soc_abbrev") %>%
    dplyr::select(-"soc_name") %>%
    dplyr::mutate(
      aebodsys_abbrev = dplyr::if_else(
        is.na(.data$aebodsys_abbrev), "", .data$aebodsys_abbrev
      )
    )

  # -----------------------------------------------------------------------
  # Step 3: Count subjects per term and arm (SAS PROC SQL, lines 122-129)
  # SAS: sum(case when arm_num = &i. then 1 else 0 end) as cd&i.
  # -----------------------------------------------------------------------
  ds_work <- ds_base_bysubjpt %>%
    dplyr::left_join(
      term_data %>% dplyr::select("aebodsys", "aedecod", "term_num"),
      by = c("aebodsys", "aedecod")
    )

  rror_freq_raw <- ds_work %>%
    dplyr::group_by(.data$term_num, .data$arm_num) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup()

  # -----------------------------------------------------------------------
  # Step 4: Ensure all term x arm combinations exist (SAS lines 152-169)
  # Fill missing combinations with zero (critical for correct 2x2 tables)
  # -----------------------------------------------------------------------
  rror_freq <- rror_freq_raw %>%
    tidyr::complete(
      tidyr::nesting(term_num = term_data$term_num),
      arm_num = seq_len(arm_count),
      fill = list(count = 0L)
    )

  # -----------------------------------------------------------------------
  # Step 5: Build wide arm counts and probabilities (SAS rror_count, lines 141-148)
  # SAS: prob&i. = cd&i. / &&&arm_&i.; array n_arm{} $100 n_arm1-n_armN ...
  # -----------------------------------------------------------------------
  arm_counts <- rror_freq %>%
    tidyr::pivot_wider(
      names_from  = "arm_num",
      values_from = "count",
      names_prefix = "cd_",
      values_fill  = 0L
    )

  # Add probability columns and ensure integer counts
  arm_counts <- arm_counts %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("cd_"), as.integer))

  for (k in seq_len(arm_count)) {
    cd_col   <- paste0("cd_", k)
    prob_col <- paste0("prob_", k)
    arm_counts[[prob_col]] <- arm_counts[[cd_col]] / arm_subjcnt[k]
  }

  # -----------------------------------------------------------------------
  # Step 6: Pairwise RR/OR computation (SAS lines 171-312)
  # For each ordered pair (i, j) where i != j:
  #   a) Build 2x2 contingency table per term
  #   b) Detect zero cells -> cc_ind

  #   c) Apply continuity correction if needed
  #   d) Compute sample OR, exact OR CI, Wald OR CI, RR, Wald RR CI
  #   e) Select appropriate CI (exact vs Wald) per SAS logic
  # -----------------------------------------------------------------------
  pair_or      <- list()
  pair_rr      <- list()
  rror_cc_ind  <- list()

  if (arm_count >= 2L) {
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i == j) next

        pair_key <- stringr::str_c(as.character(i), "_", as.character(j))
        cli::cli_inform("Computing RR/OR for arm {i} vs arm {j}")

        # --- 6a: Subset to this pair and build event/nonevent ---
        pair_freq <- rror_freq %>%
          dplyr::filter(.data$arm_num %in% c(i, j))

        # --- 6b: Detect zero cells -> cc_ind ---
        # SAS: left join where count = 0 to flag cc_ind = 1
        cc_ind_data <- pair_freq %>%
          dplyr::mutate(
            arm_total = dplyr::if_else(
              .data$arm_num == i, arm_subjcnt[i], arm_subjcnt[j]
            ),
            nonevent = .data$arm_total - .data$count
          ) %>%
          dplyr::group_by(.data$term_num) %>%
          dplyr::summarise(
            cc_ind = as.integer(
              any(.data$count == 0L, na.rm = TRUE) |
              any(.data$nonevent == 0L, na.rm = TRUE)
            ),
            .groups = "drop"
          )

        rror_cc_ind[[pair_key]] <- cc_ind_data

        # --- 6c-e: Compute statistics for each term ---
        all_terms <- sort(unique(pair_freq$term_num))

        term_stats <- purrr::map(all_terms, function(tn) {
          row_i <- pair_freq %>%
            dplyr::filter(.data$term_num == tn, .data$arm_num == i)
          row_j <- pair_freq %>%
            dplyr::filter(.data$term_num == tn, .data$arm_num == j)

          # Raw 2x2 cells: events and non-events per arm
          a_raw <- row_i$count[1]                   # events in arm i
          b_raw <- arm_subjcnt[i] - row_i$count[1]  # non-events in arm i
          c_raw <- row_j$count[1]                    # events in arm j
          d_raw <- arm_subjcnt[j] - row_j$count[1]  # non-events in arm j

          term_cc <- dplyr::pull(
            cc_ind_data %>% dplyr::filter(.data$term_num == tn),
            .data$cc_ind
          )
          if (length(term_cc) == 0L) term_cc <- 0L

          # --- Apply continuity correction (SAS lines 179-211) ---
          a_adj <- a_raw
          b_adj <- b_raw
          c_adj <- c_raw
          d_adj <- d_raw

          if (term_cc == 1L && cc_sw > 0L) {
            if (cc_sw == 1L) {
              # Constant CC: add cc to all 4 cells
              a_adj <- a_raw + cc
              b_adj <- b_raw + cc
              c_adj <- c_raw + cc
              d_adj <- d_raw + cc
            } else if (cc_sw == 2L) {
              # Reciprocal CC: add 1/opposite_arm_total
              a_adj <- a_raw + 1.0 / arm_subjcnt[j]
              b_adj <- b_raw + 1.0 / arm_subjcnt[j]
              c_adj <- c_raw + 1.0 / arm_subjcnt[i]
              d_adj <- d_raw + 1.0 / arm_subjcnt[i]
            }
            cli::cli_warn(
              "Continuity correction applied for term {tn}, pair ({i},{j})"
            )
          }

          # --- Sample Odds Ratio: (a*d)/(b*c) matches SAS _rror_ ---
          or_est <- if (b_adj > 0 && c_adj > 0) {
            (a_adj * d_adj) / (b_adj * c_adj)
          } else {
            NA_real_
          }

          # --- Determine CI selection (SAS lines 257-266) ---
          # Use exact when: no CC, or CC with whole, or CC non-whole but
          # this term does not need CC (cc_ind=0)
          use_exact_or <- (cc_sw == 0L) ||
            as.logical(cc_whole) ||
            (term_cc == 0L)

          # --- Exact OR CI via fisher.test() (SAS EXACT OR) ---
          or_exact_lower <- NA_real_
          or_exact_upper <- NA_real_

          if (use_exact_or) {
            # When exact CI is appropriate, counts are integer (or close to it)
            int_a <- as.integer(janitor::round_half_up(a_adj, 0))
            int_b <- as.integer(janitor::round_half_up(b_adj, 0))
            int_c <- as.integer(janitor::round_half_up(c_adj, 0))
            int_d <- as.integer(janitor::round_half_up(d_adj, 0))

            ft <- tryCatch({
              # Matrix layout: rows=arms, cols=disease(event/nonevent)
              mat <- matrix(c(int_a, int_c, int_b, int_d), nrow = 2L)
              stats::fisher.test(mat)
            }, error = function(e) NULL)

            if (!is.null(ft)) {
              or_exact_lower <- ft$conf.int[1]
              or_exact_upper <- ft$conf.int[2]
            }
          }

          # --- Wald CI for OR (SAS l_rror/u_rror) ---
          or_wald_lower <- NA_real_
          or_wald_upper <- NA_real_
          if (!is.na(or_est) &&
              a_adj > 0 && b_adj > 0 && c_adj > 0 && d_adj > 0) {
            log_or    <- log(or_est)
            se_log_or <- sqrt(1.0 / a_adj + 1.0 / b_adj +
                              1.0 / c_adj + 1.0 / d_adj)
            z_val     <- stats::qnorm(0.975)
            or_wald_lower <- exp(log_or - z_val * se_log_or)
            or_wald_upper <- exp(log_or + z_val * se_log_or)
          }

          # --- Select OR CI ---
          or_lower <- if (use_exact_or) or_exact_lower else or_wald_lower
          or_upper <- if (use_exact_or) or_exact_upper else or_wald_upper

          # --- Relative Risk: (a/(a+b)) / (c/(c+d)) matches SAS _rrc1_ ---
          total_i <- a_adj + b_adj
          total_j <- c_adj + d_adj

          rr_est <- if (total_i > 0 && total_j > 0 && c_adj > 0) {
            (a_adj / total_i) / (c_adj / total_j)
          } else {
            NA_real_
          }

          # --- Wald CI for RR (SAS always uses l_rrc1/u_rrc1) ---
          rr_lower <- NA_real_
          rr_upper <- NA_real_
          if (!is.na(rr_est) &&
              a_adj > 0 && c_adj > 0 && total_i > 0 && total_j > 0) {
            log_rr    <- log(rr_est)
            se_log_rr <- sqrt(1.0 / a_adj - 1.0 / total_i +
                              1.0 / c_adj - 1.0 / total_j)
            z_val     <- stats::qnorm(0.975)
            rr_lower  <- exp(log_rr - z_val * se_log_rr)
            rr_upper  <- exp(log_rr + z_val * se_log_rr)
          }

          tibble::tibble(
            term_num  = tn,
            or_est    = or_est,
            or_lower  = or_lower,
            or_upper  = or_upper,
            rr_est    = rr_est,
            rr_lower  = rr_lower,
            rr_upper  = rr_upper
          )
        }) %>%
          dplyr::bind_rows()

        # Separate OR and RR results for this pair
        pair_or[[pair_key]] <- term_stats %>%
          dplyr::select(
            "term_num",
            estimate = "or_est",
            lower_cl = "or_lower",
            upper_cl = "or_upper"
          )

        pair_rr[[pair_key]] <- term_stats %>%
          dplyr::select(
            "term_num",
            estimate = "rr_est",
            lower_cl = "rr_lower",
            upper_cl = "rr_upper"
          )
      }
    }
  }

  # -----------------------------------------------------------------------
  # Step 7: Assemble return (SAS lines 286-337)
  # -----------------------------------------------------------------------
  or_nobs <- nrow(term_data)
  rr_nobs <- nrow(term_data)

  cli::cli_inform("RR/OR computation complete. {or_nobs} distinct AE terms processed.")

  list(
    term_data   = term_data,
    arm_counts  = arm_counts,
    arm_names   = arm_names,
    pair_or     = pair_or,
    pair_rr     = pair_rr,
    rror_cc_ind = rror_cc_ind,
    or_nobs     = as.integer(or_nobs),
    rr_nobs     = as.integer(rr_nobs)
  )
}


# =============================================================================
# build_vertical_layout  (internal helper — not exported)
# =============================================================================
#' Build the 22-row vertical layout for one top AE term.
#'
#' Replaces the inner DATA step loop within SAS \code{%outs} that creates
#' the \code{vert_rr/vert_or} datasets (SAS lines ~400-600). Each term
#' contributes 21 numeric rows (the chart data) plus 1 character row
#' (Body System Abbreviation).
#'
#' The 21-row layout is designed for the Script Launcher Excel charting
#' template: rows 1-6 carry the boxplot-like estimate and CI bounds,
#' rows 7-9 carry additional percentile positions, rows 10-11 carry
#' offset values for chart error bars, row 12 carries vertical positioning,
#' row 13 is a reference line marker, rows 14-19 carry arm-level counts
#' and probabilities, and rows 20-21 are "Favors" label placeholders.
#'
#' @param estimate   Numeric. Point estimate (RR or OR).
#' @param lower_cl   Numeric. Lower confidence limit.
#' @param upper_cl   Numeric. Upper confidence limit.
#' @param cd_i       Numeric. Subject count in arm i with this AE.
#' @param cd_j       Numeric. Subject count in arm j with this AE.
#' @param prob_i     Numeric. Probability (proportion) in arm i.
#' @param prob_j     Numeric. Probability (proportion) in arm j.
#' @param arm_name_i Character. Display name of arm i.
#' @param arm_name_j Character. Display name of arm j.
#' @param aebodsys_abbrev Character. SOC abbreviation.
#' @param rank       Integer. 1-based rank of this term in the top-N list.
#' @param num_aedecod Integer. Total number of terms being displayed.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{data}{Tibble with 21 rows (var 1-21): numeric data rows.}
#'     \item{abbrev}{Tibble with 1 row (var 22): character abbreviation row.}
#'   }
#' @keywords internal
build_vertical_layout <- function(estimate, lower_cl, upper_cl,
                                  cd_i, cd_j, prob_i, prob_j,
                                  arm_name_i, arm_name_j,
                                  aebodsys_abbrev,
                                  rank, num_aedecod) {

  # Offset for vertical chart positioning (SAS: .975-(A-1)*.95/(num_aedecod-1))
  offset_val <- if (num_aedecod > 1L) {
    0.975 - (rank - 1L) * 0.95 / (num_aedecod - 1L)
  } else {
    0.975
  }

  # Chart error bar offsets (SAS: sum(rr,-elcl) and sum(eucl,-rr))
  min_offset <- if (!is.na(estimate) && !is.na(lower_cl)) {
    estimate - lower_cl
  } else {
    NA_real_
  }
  max_offset <- if (!is.na(estimate) && !is.na(upper_cl)) {
    upper_cl - estimate
  } else {
    NA_real_
  }

  # Probability as percentage — SAS-compatible rounding
  prob_i_pct <- janitor::round_half_up(100.0 * prob_i, digits = 10)
  prob_j_pct <- janitor::round_half_up(100.0 * prob_j, digits = 10)

  # Build 21-row numeric data tibble (var 1-21)
  data_tbl <- tibble::tibble(
    var = 1L:21L,
    sort_order = c(
      "01 Mean",                                       # var  1
      "02 Median",                                     # var  2
      "03 Q1",                                         # var  3
      "04 Q3",                                         # var  4
      "05 Min",                                        # var  5
      "06 Max",                                        # var  6
      "07 25th",                                       # var  7
      "08 50th",                                       # var  8
      "09 75th",                                       # var  9
      "10 Min",                                        # var 10
      "11 Max",                                        # var 11
      "12 Offset",                                     # var 12
      "13 Line",                                       # var 13
      arm_name_i,                                      # var 14
      "15 Count1",                                     # var 15
      "16 Prob1",                                      # var 16
      arm_name_j,                                      # var 17
      "18 Count2",                                     # var 18
      "19 Prob2",                                      # var 19
      stringr::str_c("Favors ", arm_name_i),           # var 20
      stringr::str_c("Favors ", arm_name_j)            # var 21
    ),
    value = c(
      estimate,     # var  1: Mean = point estimate
      estimate,     # var  2: Median = point estimate
      estimate,     # var  3: Q1 = point estimate
      estimate,     # var  4: Q3 = point estimate
      lower_cl,     # var  5: Min = lower CL
      upper_cl,     # var  6: Max = upper CL
      estimate,     # var  7: 25th = point estimate
      0.0,          # var  8: 50th = 0
      0.0,          # var  9: 75th = 0
      min_offset,   # var 10: Min offset = estimate - lower CL
      max_offset,   # var 11: Max offset = upper CL - estimate
      offset_val,   # var 12: Vertical positioning offset
      1.0,          # var 13: Reference line marker
      1.0,          # var 14: Arm i marker
      cd_i,         # var 15: Count in arm i
      prob_i_pct,   # var 16: Probability % in arm i
      1.0,          # var 17: Arm j marker
      cd_j,         # var 18: Count in arm j
      prob_j_pct,   # var 19: Probability % in arm j
      NA_real_,     # var 20: Favors arm i (placeholder)
      NA_real_      # var 21: Favors arm j (placeholder)
    )
  )

  # Abbreviation row (var 22) — character value
  abbrev_tbl <- tibble::tibble(
    var        = 22L,
    sort_order = "Body System Abbrev.",
    value      = as.character(aebodsys_abbrev)
  )

  list(data = data_tbl, abbrev = abbrev_tbl)
}


# =============================================================================
# reshape_rror_for_excel
# =============================================================================
#' Reshape RR/OR results into vertical Excel worksheet layout.
#'
#' Replaces SAS \code{%outs} macro (lines 351-718 of ae_rror.sas). For each
#' arm pair, sorts by descending estimate, takes the top \code{num_aedecod}
#' terms, and builds a wide layout where each column is an AE term and each
#' row is one of 22 variables (21 numeric chart data + 1 abbreviation).
#'
#' @param compute_result  Named list returned by \code{compute_rror()}.
#' @param arm_count       Integer. Number of treatment arms.
#' @param arm_names       Character vector of arm display names.
#' @param arm_subjcnt     Numeric vector of subject counts per arm.
#' @param num_aedecod     Integer. Number of top terms to display (default 30).
#'
#' @return A named list:
#'   \describe{
#'     \item{rr_data}{Named list: \code{"i_j"} -> tibble of RR data (21 rows, wide).}
#'     \item{rr_abbrev}{Named list: \code{"i_j"} -> tibble of RR abbreviations (1 row, wide).}
#'     \item{or_data}{Named list: \code{"i_j"} -> tibble of OR data (21 rows, wide).}
#'     \item{or_abbrev}{Named list: \code{"i_j"} -> tibble of OR abbreviations (1 row, wide).}
#'     \item{rr_labels}{Named list: \code{"i_j"} -> character vector of top RR term labels.}
#'     \item{or_labels}{Named list: \code{"i_j"} -> character vector of top OR term labels.}
#'   }
#'
#' @export
reshape_rror_for_excel <- function(compute_result,
                                   arm_count,
                                   arm_names,
                                   arm_subjcnt,
                                   num_aedecod = 30L) {

  num_aedecod <- as.integer(num_aedecod)
  arm_count   <- as.integer(arm_count)

  # Extract compute_rror components

  term_data   <- compute_result$term_data
  arm_counts  <- compute_result$arm_counts
  pair_or     <- compute_result$pair_or
  pair_rr     <- compute_result$pair_rr
  rror_cc_ind <- compute_result$rror_cc_ind
  or_nobs     <- compute_result$or_nobs
  rr_nobs     <- compute_result$rr_nobs

  rr_data_out   <- list()
  rr_abbrev_out <- list()
  or_data_out   <- list()
  or_abbrev_out <- list()
  rr_labels_out <- list()
  or_labels_out <- list()

  for (i in seq_len(arm_count)) {
    for (j in seq_len(arm_count)) {
      if (i == j) next

      pair_key <- stringr::str_c(as.character(i), "_", as.character(j))

      rr_pair <- pair_rr[[pair_key]]
      or_pair <- pair_or[[pair_key]]
      cc_pair <- rror_cc_ind[[pair_key]]

      if (is.null(rr_pair) || is.null(or_pair)) next

      # Column names for arm-level data
      cd_col_i   <- paste0("cd_", i)
      cd_col_j   <- paste0("cd_", j)
      prob_col_i <- paste0("prob_", i)
      prob_col_j <- paste0("prob_", j)

      # Use rlang::syms for programmatic column selection
      keep_cols <- rlang::syms(c(
        "term_num", "aebodsys", "aedecod", "aebodsys_abbrev",
        cd_col_i, cd_col_j, prob_col_i, prob_col_j
      ))

      term_arm_data <- term_data %>%
        dplyr::left_join(arm_counts, by = "term_num") %>%
        dplyr::select(!!!keep_cols)

      # =========================================================
      # Reshape RR for this pair
      # =========================================================
      rr_merged <- term_arm_data %>%
        dplyr::left_join(rr_pair, by = "term_num") %>%
        dplyr::left_join(cc_pair, by = "term_num")

      actual_rr_count <- min(num_aedecod, rr_nobs)
      rr_top <- rr_merged %>%
        dplyr::arrange(dplyr::desc(.data$estimate)) %>%
        dplyr::slice_head(n = actual_rr_count)

      rr_vert_list <- purrr::map(seq_len(nrow(rr_top)), function(rank) {
        row <- rr_top[rank, ]

        # Label with asterisk if CC applied
        aecd_label <- if (!is.na(row$cc_ind) && row$cc_ind == 1L) {
          stringr::str_c(row$aedecod, "*")
        } else {
          stringr::str_c(row$aedecod, " ")
        }

        vert <- build_vertical_layout(
          estimate       = row$estimate,
          lower_cl       = row$lower_cl,
          upper_cl       = row$upper_cl,
          cd_i           = row[[cd_col_i]],
          cd_j           = row[[cd_col_j]],
          prob_i         = row[[prob_col_i]],
          prob_j         = row[[prob_col_j]],
          arm_name_i     = arm_names[i],
          arm_name_j     = arm_names[j],
          aebodsys_abbrev = row$aebodsys_abbrev,
          rank           = rank,
          num_aedecod    = num_aedecod
        )

        list(data_col = vert$data$value,
             abbrev_val = vert$abbrev$value,
             label = aecd_label)
      })

      # Build wide data tibble: var | sort_order | aedecod1 .. aedecod_N
      base_vert <- build_vertical_layout(
        0, 0, 0, 0, 0, 0, 0,
        arm_names[i], arm_names[j], "", 1L, num_aedecod
      )
      rr_wide <- tibble::tibble(
        var        = base_vert$data$var,
        sort_order = base_vert$data$sort_order
      )
      rr_wide_abbr <- tibble::tibble(
        var        = 22L,
        sort_order = "Body System Abbrev."
      )
      rr_term_labels <- character(0)

      for (a_idx in seq_along(rr_vert_list)) {
        col_name <- paste0("aedecod", a_idx)
        rr_wide[[col_name]]      <- rr_vert_list[[a_idx]]$data_col
        rr_wide_abbr[[col_name]] <- rr_vert_list[[a_idx]]$abbrev_val
        rr_term_labels <- c(rr_term_labels, rr_vert_list[[a_idx]]$label)
      }

      rr_data_out[[pair_key]]   <- rr_wide
      rr_abbrev_out[[pair_key]] <- rr_wide_abbr
      rr_labels_out[[pair_key]] <- rr_term_labels

      # =========================================================
      # Reshape OR for this pair (same logic, different estimates)
      # =========================================================
      or_merged <- term_arm_data %>%
        dplyr::left_join(or_pair, by = "term_num") %>%
        dplyr::left_join(cc_pair, by = "term_num")

      actual_or_count <- min(num_aedecod, or_nobs)
      or_top <- or_merged %>%
        dplyr::arrange(dplyr::desc(.data$estimate)) %>%
        dplyr::slice_head(n = actual_or_count)

      or_vert_list <- purrr::map(seq_len(nrow(or_top)), function(rank) {
        row <- or_top[rank, ]

        aecd_label <- if (!is.na(row$cc_ind) && row$cc_ind == 1L) {
          stringr::str_c(row$aedecod, "*")
        } else {
          stringr::str_c(row$aedecod, " ")
        }

        vert <- build_vertical_layout(
          estimate       = row$estimate,
          lower_cl       = row$lower_cl,
          upper_cl       = row$upper_cl,
          cd_i           = row[[cd_col_i]],
          cd_j           = row[[cd_col_j]],
          prob_i         = row[[prob_col_i]],
          prob_j         = row[[prob_col_j]],
          arm_name_i     = arm_names[i],
          arm_name_j     = arm_names[j],
          aebodsys_abbrev = row$aebodsys_abbrev,
          rank           = rank,
          num_aedecod    = num_aedecod
        )

        list(data_col = vert$data$value,
             abbrev_val = vert$abbrev$value,
             label = aecd_label)
      })

      or_wide <- tibble::tibble(
        var        = base_vert$data$var,
        sort_order = base_vert$data$sort_order
      )
      or_wide_abbr <- tibble::tibble(
        var        = 22L,
        sort_order = "Body System Abbrev."
      )
      or_term_labels <- character(0)

      for (a_idx in seq_along(or_vert_list)) {
        col_name <- paste0("aedecod", a_idx)
        or_wide[[col_name]]      <- or_vert_list[[a_idx]]$data_col
        or_wide_abbr[[col_name]] <- or_vert_list[[a_idx]]$abbrev_val
        or_term_labels <- c(or_term_labels, or_vert_list[[a_idx]]$label)
      }

      or_data_out[[pair_key]]   <- or_wide
      or_abbrev_out[[pair_key]] <- or_wide_abbr
      or_labels_out[[pair_key]] <- or_term_labels
    }
  }

  list(
    rr_data   = rr_data_out,
    rr_abbrev = rr_abbrev_out,
    or_data   = or_data_out,
    or_abbrev = or_abbrev_out,
    rr_labels = rr_labels_out,
    or_labels = or_labels_out
  )
}


# =============================================================================
# write_rror_workbooks
# =============================================================================
#' Write OR and RR results to two Excel workbooks.
#'
#' Replaces SAS \code{%out_ae_rror} macro (lines 793-892 of ae_rror.sas).
#' Creates two Excel workbooks — one for odds ratios, one for relative risks —
#' each containing per-arm-pair data and abbreviation worksheets, plus shared
#' \code{info} and \code{arminfo} metadata worksheets. Optionally adds Script
#' Launcher grouping/subsetting worksheets via \code{group_subset_write_xlsx()}.
#'
#' SAS \code{PCFILES/JET LIBNAME} engine replaced by \code{openxlsx} direct
#' file I/O.
#'
#' @param or_output_file   Character. File path for the OR workbook.
#' @param rr_output_file   Character. File path for the RR workbook.
#' @param reshaped_data    Named list returned by \code{reshape_rror_for_excel()}.
#' @param lib_metadata     Named list with study-level metadata:
#'   \describe{
#'     \item{ndabla}{Character. NDA/BLA identifier.}
#'     \item{studyid}{Character. Study identifier.}
#'     \item{rundate}{Character. Script execution date.}
#'     \item{date_validation}{Character. Date validation description.}
#'     \item{cc_description}{Character. CC method description.}
#'     \item{cc_asterisk_note}{Character. Note about asterisk meaning.}
#'     \item{cc_detail_note}{Character. Detail note about CC method.}
#'     \item{arm_count}{Integer. Number of treatment arms.}
#'     \item{sl_custom_ds}{Character. Script Launcher custom dataset name.}
#'     \item{study_lag}{Character. Study lag identifier.}
#'     \item{actual_or_planned}{Character. 'actual' or 'planned' arm type.}
#'   }
#' @param arm_count        Integer. Number of treatment arms.
#' @param arm_names        Character vector of arm names (length = arm_count).
#' @param arm_display      Tibble or data.frame with columns \code{arm_num} and
#'                         \code{arm_display} for the arm info metadata sheet.
#' @param pp_result        Optional. Result from \code{group_subset_pp()} for
#'                         Script Launcher metadata. Default \code{NULL} (skip).
#'
#' @return Invisible named list:
#'   \describe{
#'     \item{or_file}{Character. Path to saved OR workbook.}
#'     \item{rr_file}{Character. Path to saved RR workbook.}
#'   }
#'
#' @export
write_rror_workbooks <- function(or_output_file,
                                 rr_output_file,
                                 reshaped_data,
                                 lib_metadata,
                                 arm_count,
                                 arm_names,
                                 arm_display,
                                 pp_result = NULL) {

  arm_count <- as.integer(arm_count)

  # -------------------------------------------------------------------
  # 0. If pp_result is provided, write GS sheets first to seed the files
  # -------------------------------------------------------------------
  or_wb <- NULL
  rr_wb <- NULL

  if (!is.null(pp_result)) {
    cli::cli_inform("Writing Script Launcher grouping/subsetting metadata to OR workbook.")
    group_subset_write_xlsx(gs_file = or_output_file, pp_result = pp_result)
    or_wb <- openxlsx::loadWorkbook(or_output_file)

    cli::cli_inform("Writing Script Launcher grouping/subsetting metadata to RR workbook.")
    group_subset_write_xlsx(gs_file = rr_output_file, pp_result = pp_result)
    rr_wb <- openxlsx::loadWorkbook(rr_output_file)
  } else {
    or_wb <- openxlsx::createWorkbook()
    rr_wb <- openxlsx::createWorkbook()
  }

  # -------------------------------------------------------------------
  # 1. Build metadata tibbles (lib_or, lib_rr, lib_arm)
  # -------------------------------------------------------------------
  lib_base <- tibble::tibble(
    row_id = seq_len(11L),
    name = c(
      "ndabla", "studyid", "rundate", "date_validation",
      "statistic", "cc_description", "cc_asterisk_note",
      "cc_detail_note", "arm_count", "sl_custom_ds", "study_lag"
    ),
    value = c(
      as.character(lib_metadata$ndabla %||% ""),
      as.character(lib_metadata$studyid %||% ""),
      # Normalize date separators for consistent SAS-style output
      stringr::str_replace_all(
        as.character(lib_metadata$rundate %||% format(Sys.time(), "%d%b%Y %H:%M")),
        "/", "-"
      ),
      as.character(lib_metadata$date_validation %||% ""),
      "",
      as.character(lib_metadata$cc_description %||% ""),
      as.character(lib_metadata$cc_asterisk_note %||% ""),
      as.character(lib_metadata$cc_detail_note %||% ""),
      as.character(lib_metadata$arm_count %||% arm_count),
      as.character(lib_metadata$sl_custom_ds %||% ""),
      as.character(lib_metadata$study_lag %||% "")
    )
  )

  lib_or <- lib_base
  lib_or$value[lib_or$name == "statistic"] <- "odds ratio"

  lib_rr <- lib_base
  lib_rr$value[lib_rr$name == "statistic"] <- "relative risk"

  # Arm info tibble
  if (is.data.frame(arm_display) && nrow(arm_display) > 0L) {
    lib_arm <- arm_display
  } else {
    lib_arm <- tibble::tibble(
      arm_num     = seq_len(arm_count),
      arm_display = arm_names[seq_len(arm_count)]
    )
  }

  # -------------------------------------------------------------------
  # 2. Write per-arm-pair worksheets
  # -------------------------------------------------------------------
  for (i in seq_len(arm_count)) {
    for (j in seq_len(arm_count)) {
      if (i == j) next

      pair_key    <- stringr::str_c(as.character(i), "_", as.character(j))
      data_name   <- stringr::str_c("data", as.character(i), as.character(j))
      abbrev_name <- stringr::str_c("abbrev", as.character(i), as.character(j))

      # OR workbook
      or_data_tbl   <- reshaped_data$or_data[[pair_key]]
      or_abbrev_tbl <- reshaped_data$or_abbrev[[pair_key]]

      if (!is.null(or_data_tbl)) {
        openxlsx::addWorksheet(or_wb, sheetName = data_name)
        openxlsx::writeData(or_wb, sheet = data_name, x = or_data_tbl)
      }
      if (!is.null(or_abbrev_tbl)) {
        openxlsx::addWorksheet(or_wb, sheetName = abbrev_name)
        openxlsx::writeData(or_wb, sheet = abbrev_name, x = or_abbrev_tbl)
      }

      # RR workbook
      rr_data_tbl   <- reshaped_data$rr_data[[pair_key]]
      rr_abbrev_tbl <- reshaped_data$rr_abbrev[[pair_key]]

      if (!is.null(rr_data_tbl)) {
        openxlsx::addWorksheet(rr_wb, sheetName = data_name)
        openxlsx::writeData(rr_wb, sheet = data_name, x = rr_data_tbl)
      }
      if (!is.null(rr_abbrev_tbl)) {
        openxlsx::addWorksheet(rr_wb, sheetName = abbrev_name)
        openxlsx::writeData(rr_wb, sheet = abbrev_name, x = rr_abbrev_tbl)
      }
    }
  }

  # -------------------------------------------------------------------
  # 3. Add shared metadata worksheets: info and arminfo
  # -------------------------------------------------------------------
  openxlsx::addWorksheet(or_wb, sheetName = "info")
  openxlsx::writeData(or_wb, sheet = "info", x = lib_or)

  openxlsx::addWorksheet(or_wb, sheetName = "arminfo")
  openxlsx::writeData(or_wb, sheet = "arminfo", x = lib_arm)

  openxlsx::addWorksheet(rr_wb, sheetName = "info")
  openxlsx::writeData(rr_wb, sheet = "info", x = lib_rr)

  openxlsx::addWorksheet(rr_wb, sheetName = "arminfo")
  openxlsx::writeData(rr_wb, sheet = "arminfo", x = lib_arm)

  # -------------------------------------------------------------------
  # 4. Save workbooks
  # -------------------------------------------------------------------
  or_dir <- dirname(or_output_file)
  rr_dir <- dirname(rr_output_file)
  if (!dir.exists(or_dir)) dir.create(or_dir, recursive = TRUE)
  if (!dir.exists(rr_dir)) dir.create(rr_dir, recursive = TRUE)

  openxlsx::saveWorkbook(or_wb, file = or_output_file, overwrite = TRUE)
  cli::cli_inform("OR workbook saved: {or_output_file}")

  openxlsx::saveWorkbook(rr_wb, file = rr_output_file, overwrite = TRUE)
  cli::cli_inform("RR workbook saved: {rr_output_file}")

  invisible(list(or_file = or_output_file, rr_file = rr_output_file))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS PROC FREQ RELRISK statement produces relative risk as
#      (a/(a+b)) / (c/(c+d)) where row 1 = arm i, row 2 = arm j.
#      R equivalent is manual calculation matching this formula.
#    - SAS PROC FREQ EXACT OR produces the conditional maximum-
#      likelihood odds ratio; R fisher.test()$estimate also returns
#      the conditional MLE, but we compute sample OR = (a*d)/(b*c)
#      to match the SAS _rror_ column which contains the sample
#      (cross-product) OR. fisher.test() is used only for exact CIs.
#    - Continuity correction logic faithfully preserves SAS behavior:
#      cc_sw=1 adds a constant (cc) to all 4 cells when any cell is
#      zero; cc_sw=2 adds 1/opposite_arm_total reciprocals.
#    - Top num_aedecod terms selected by descending estimate value.
#    - Vertical 21-row layout per term preserved for Excel template
#      compatibility with the Script Launcher charting template.
#    - Offset formula: 0.975 - (A-1)*0.95/(num_aedecod-1) where
#      num_aedecod is the configured max, not the actual count.
#    - SAS worksheet naming convention "data{i}{j}" and "abbrev{i}{j}"
#      (no separators between indices) is preserved.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - fisher.test() exact CI bounds may differ slightly from SAS
#      xl_rror/xu_rror due to different internal algorithms for
#      computing exact confidence intervals on the odds ratio.
#    - Wald CI on log-RR uses qnorm(0.975) = 1.959964; SAS may use
#      slightly different z-quantile precision.
#    - CC reciprocal calculation: R uses 1/arm_subjcnt[opposite_arm];
#      verify SAS uses the same denominator (total N in opposite arm).
#    - Sort stability: R dplyr::arrange() is stable within groups;
#      if two AEs have identical RR/OR estimates, their order may
#      differ from SAS affecting which terms appear in top-N.
#    - Floating point: both SAS and R use IEEE 754 doubles, but
#      accumulation order in sums may produce last-bit differences.
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC FREQ with both RELRISK and EXACT OR in one call
#      -> separate R computations (manual OR/RR + fisher.test).
#    - SAS hash object lookup -> dplyr::left_join() on the
#      soc_abbreviations() tibble.
#    - SAS PCFILES/JET LIBNAME engine -> openxlsx direct file I/O.
#    - SAS DATA step with RETAIN + first./last. dot logic -> dplyr
#      group_by + arrange + row_number + distinct.
#    - SAS OUTPUT statement inside DO loop -> purrr::map_dfr() with
#      tibble construction per iteration.
#
# PACKAGE SELECTION RATIONALE:
#    - stats::fisher.test(): Exact odds ratio CIs (standard R approach;
#      no additional dependency needed). Used for exact CI extraction
#      only; point estimate is sample OR = (a*d)/(b*c).
#    - dplyr: All data manipulation (AAP mandates tidyverse over base R).
#    - tidyr: Reshaping via pivot_wider/complete (tidyverse standard).
#    - purrr: Functional iteration via map_dfr (AAP mandates purrr over
#      base for/apply loops).
#    - openxlsx: Excel output (AAP mandated for all Excel generation).
#    - janitor: round_half_up for SAS-compatible rounding (AAP Gate 2).
#    - stringr: String manipulation (tidyverse over base R string fns).
#    - rlang: Tidy evaluation with .data pronoun for programmatic cols.
#    - cli: Structured user-facing messages replacing SAS %PUT.
#    - epitools was considered for riskratio() but not used; manual RR
#      calculation is simpler and avoids an additional dependency.
#
# OPEN QUESTIONS:
#    - Exact vs asymptotic CI selection rule: the current implementation
#      uses exact CIs (fisher.test) when counts are integer (no CC
#      applied, or CC with whole-number values) and Wald CIs when CC
#      produces non-integer counts. Verify this matches SAS use of
#      xl/xu (exact) vs l/u (asymptotic Wald).
#    - CC reciprocal method: confirm SAS uses 1/opposite_arm_total as
#      the additive correction for cc_sw=2.
#    - Sort stability for top-N term selection: if two terms have
#      identical estimates, the tiebreaker may differ between R and SAS.
#    - SAS _rror_ is sample OR (a*d)/(b*c); fisher.test()$estimate is
#      conditional MLE OR. We use sample OR to match SAS. Confirm this
#      is the correct interpretation for regulatory output.
# ============================================================
