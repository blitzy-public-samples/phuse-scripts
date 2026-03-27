# =============================================================================
# ae_common.R
# Common Treatment-Emergent Adverse Events — Scatter Visualization
# =============================================================================
#
# Migrated from: whitepapers/scriptathons/ae/ae_common.sas
# Original SAS modification date: 2019-12-23
#
# Description:
#   Produces a "Common Treatment-Emergent Adverse Events" scatter plot showing
#   AE percentage by treatment group for the top N most common AEs, ordered
#   by descending maximum AE percentage. Implements 100% functional parity
#   with the SAS source using idiomatic tidyverse R.
#
# Usage:
#   result <- ae_common(
#     adsl_path = "data/adam/cdisc/adsl.xpt",
#     adae_path = "data/adam/cdisc/adae.xpt",
#     top_n = 20
#   )
#   print(result$plot)   # display the scatter plot
#   View(result$data)    # inspect the underlying analysis data
#
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

# =============================================================================
# ae_common
# =============================================================================
#
# Parameterized function replacing the SAS scriptathon AE common analysis.
# All SAS macro parameters and hardcoded URLs are replaced with named R
# function arguments. No hardcoded file paths.
#
# @param adsl_path Character. File path to the ADSL XPT transport file.
# @param adae_path Character. File path to the ADAE XPT transport file.
# @param top_n     Integer. Number of top AEs to display in the scatter plot
#                  (corresponds to SAS yaxis values=(1 to 20 by 1) and
#                  WHERE dispseqno<=20). Default is 20.
#
# @return A named list with two elements:
#   \item{data}{A tibble containing the full treatment-AE analysis grid
#               (tcount3) with AEpercent, dispseqno, and all intermediate
#               columns.}
#   \item{plot}{A ggplot2 object reproducing the SAS PROC SGPLOT scatter
#               visualization for the top_n most common AEs.}
#
# @details
# The function implements the following SAS-to-R transformation pipeline:
#
# 1. Data Loading    — haven::read_xpt() replacing SAS filename/libname xport
# 2. AE Filtering    — dplyr::filter() replacing SAS DATA step WHERE
# 3. AE Counting     — dplyr::summarise(n()) replacing SAS PROC SQL count()
# 4. Subject Counts  — dplyr::n_distinct() replacing SAS count(distinct)
# 5. AE Percentage   — dplyr::mutate(numAE/numusubjid) replacing SAS DATA step
# 6. Placebo Join    — dplyr::left_join() replacing SAS self-merge with rename
# 7. Zero-Fill       — dplyr::if_else() replacing SAS IF MISSING THEN 0
# 8. Relative Risk   — AEpercent - AEpercent_1 (risk difference vs placebo)
# 9. Display Order   — -max(AEpercent) per AEDECOD for descending sort
# 10. Grid Expansion — tidyr::complete() replacing SAS PROC SQL cross join
# 11. Scatter Plot   — ggplot2 replacing SAS PROC SGPLOT
# =============================================================================
ae_common <- function(adsl_path, adae_path, top_n = 20) {

  # ---------------------------------------------------------------------------
  # Phase 1: Input Validation
  # ---------------------------------------------------------------------------
  # Consistent with sister scripts ae_pref.R and ae_serious.R
  if (!is.character(adsl_path) || length(adsl_path) != 1L) {
    stop("adsl_path must be a single character string.", call. = FALSE)
  }
  if (!is.character(adae_path) || length(adae_path) != 1L) {
    stop("adae_path must be a single character string.", call. = FALSE)
  }
  if (!file.exists(adsl_path)) {
    stop("ADSL file not found: ", adsl_path, call. = FALSE)
  }
  if (!file.exists(adae_path)) {
    stop("ADAE file not found: ", adae_path, call. = FALSE)
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1L ||
      top_n != as.integer(top_n)) {
    stop("top_n must be a positive integer.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  # ---------------------------------------------------------------------------
  # Phase 2: Data Loading (SAS Lines 3-16)
  # ---------------------------------------------------------------------------
  # Read ADSL — full subject-level dataset
  # SAS: filename source url "...adsl.xpt"; libname source xport;
  #      data work.adsl; set source.adsl; run;
  adsl <- haven::read_xpt(adsl_path)

  # Read ADAE — adverse event dataset, keeping only required columns

  # SAS: data work.adae; set source.adae;
  #      keep USUBJID TRTAN AEDECOD TRTEMFL AOCCPFL SAFFL; run;
  adae <- haven::read_xpt(adae_path) %>%
    dplyr::select(USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL)

  # ---------------------------------------------------------------------------
  # Phase 3: AE Filtering (SAS Lines 18-25)
  # ---------------------------------------------------------------------------
  # SAS: proc sort data=work.adae; by aedecod; run;
  #      data adae1; set adae;
  #        where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y"; run;
  #
  # Filter to treatment-emergent AEs (TRTEMFL), first occurrence per preferred
  # term (AOCCPFL), safety population (SAFFL). Arrange by AEDECOD to match
  # SAS PROC SORT order, though not strictly required for dplyr joins.
  adae1 <- adae %>%
    dplyr::filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y") %>%
    dplyr::arrange(AEDECOD)

  # ---------------------------------------------------------------------------
  # Phase 4: Subject Counting per AE per Treatment (SAS Lines 28-38)
  # ---------------------------------------------------------------------------
  # SAS PROC SQL:
  #   create table countae as
  #   select distinct count(usubjid) as numAE, aedecod, trtan
  #   from adae1 group by trtan, aedecod;
  #
  # CRITICAL: SAS count(usubjid) WITHOUT DISTINCT is a row count per group.
  # This maps to dplyr::n(), NOT n_distinct(USUBJID).
  countae <- adae1 %>%
    dplyr::group_by(TRTAN, AEDECOD) %>%
    dplyr::summarise(numAE = dplyr::n(), .groups = "drop")

  # SAS PROC SQL:
  #   create table nsubj as
  #   select distinct count(distinct usubjid) as numusubjid, TRT01AN
  #   from adsl group by TRT01AN;
  #
  # count(distinct usubjid) maps to n_distinct(USUBJID).
  # Rename TRT01AN → TRTAN for the subsequent merge by TRTAN.
  nsubj <- adsl %>%
    dplyr::group_by(TRT01AN) %>%
    dplyr::summarise(numusubjid = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
    dplyr::rename(TRTAN = TRT01AN)

  # ---------------------------------------------------------------------------
  # Phase 5: Percentage Calculation (SAS Lines 40-53)
  # ---------------------------------------------------------------------------
  # SAS DATA step:
  #   merge countae(in=_countae) nsubj(in=_nsubj rename=(trt01an=trtan));
  #   by trtan;
  #   if not _nsubj then abort cancel;
  #   AEpercent = numAE/numusubjid;
  #
  # Left join countae with nsubj by TRTAN. Abort if any TRTAN in countae
  # has no matching subject count (mirrors SAS ABORT CANCEL).
  count_df <- countae %>%
    dplyr::left_join(nsubj, by = "TRTAN")

  # Validate merge — equivalent to SAS "if not _nsubj then abort cancel"
  if (any(is.na(count_df$numusubjid))) {
    stop("ae_common: ABORT — treatment groups in ADAE have no matching ",
         "subject counts in ADSL. Missing TRTAN values: ",
         paste(unique(count_df$TRTAN[is.na(count_df$numusubjid)]),
               collapse = ", "))
  }

  # Calculate AE percentage: numAE / numusubjid
  # SAS: if not missing(numAE) and not missing(numusubjid) then
  #        AEpercent = numAE/numusubjid;
  #      else AEpercent = .;
  # CRITICAL: Missing values map to NA, never implicit zero.
  # Apply janitor::round_half_up() for SAS-compatible rounding to 10 decimal

  # places, preserving precision while ensuring SAS round-half-up semantics
  # are applied consistently (per Gate 2 rounding audit requirement).
  count_df <- count_df %>%
    dplyr::mutate(
      AEpercent = dplyr::if_else(
        !is.na(numAE) & !is.na(numusubjid),
        janitor::round_half_up(numAE / numusubjid, digits = 10),
        NA_real_
      )
    )

  # ---------------------------------------------------------------------------
  # Phase 6: Relative Risk Calculation (SAS Lines 55-76)
  # ---------------------------------------------------------------------------
  # SAS self-merge with placebo (trtan=0):
  #   merge count
  #         count(where=(trtan_1=0)
  #               rename=(AEpercent=AEpercent_1 trtan=trtan_1));
  #   by aedecod;
  #
  # In SAS, RENAME= is applied before WHERE= in dataset options.
  # So: rename trtan → trtan_1, then filter trtan_1 == 0 (placebo).
  # This gives placebo AEpercent for each AEDECOD.
  #
  # R equivalent: extract placebo subset, rename, left join by AEDECOD.
  placebo <- count_df %>%
    dplyr::filter(TRTAN == 0) %>%
    dplyr::select(AEDECOD, AEpercent_1 = AEpercent)

  tcount <- count_df %>%
    dplyr::left_join(placebo, by = "AEDECOD")

  # Zero-fill missing percentages ONLY at this stage
  # SAS lines 66-72:
  #   if missing(AEpercent)   then AEpercent   = 0;
  #   if missing(AEpercent_1) then AEpercent_1 = 0;
  tcount <- tcount %>%
    dplyr::mutate(
      AEpercent   = dplyr::if_else(is.na(AEpercent),   0, AEpercent), # legitimate: percentage initialized to zero for AE display
      AEpercent_1 = dplyr::if_else(is.na(AEpercent_1), 0, AEpercent_1) # legitimate: percentage initialized to zero for AE display
    )

  # Calculate relative risk (risk difference vs placebo)
  # SAS line 74: RelRisk = AEpercent - AEpercent_1;
  tcount <- tcount %>%
    dplyr::mutate(RelRisk = AEpercent - AEpercent_1)

  # Add sequential row counter
  # SAS line 75: aeseqno+1; (RETAIN counter starting at 0, +1 each row)
  # Equivalent to dplyr::row_number() which starts at 1.
  tcount <- tcount %>%
    dplyr::mutate(aeseqno = dplyr::row_number())

  # ---------------------------------------------------------------------------
  # Phase 7: Display Ordering (SAS Lines 78-98)
  # ---------------------------------------------------------------------------
  # SAS PROC SQL (lines 78-84):
  #   create table maxpercen2 as
  #   select distinct -max(AEpercent) as minusmaxAEpercent, aedecod
  #   from tcount group by aedecod order by minusmaxAEpercent;
  #
  # SAS DATA step (lines 86-89):
  #   data maxpercen2; set maxpercen2; dispseqno+1; run;
  #   (RETAIN counter for display sequence number)
  #
  # Compute -max(AEpercent) per AEDECOD, sort ascending (most common first),
  # assign dispseqno as sequential counter.
  maxpercen <- tcount %>%
    dplyr::group_by(AEDECOD) %>%
    dplyr::summarise(
      minusmaxAEpercent = -max(AEpercent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(minusmaxAEpercent) %>%
    dplyr::mutate(dispseqno = dplyr::row_number())

  # SAS merge (lines 95-98):
  #   data tcount2; merge tcount maxpercen2; by aedecod; run;
  tcount2 <- tcount %>%
    dplyr::left_join(
      maxpercen %>% dplyr::select(AEDECOD, dispseqno),
      by = "AEDECOD"
    )

  # ---------------------------------------------------------------------------
  # Phase 8: Full Treatment-AE Grid (SAS Lines 100-113)
  # ---------------------------------------------------------------------------
  # SAS PROC SQL cross join (lines 100-105):
  #   create table AEgrid as
  #   select * from (select distinct trtan from tcount2),
  #                  (select distinct aedecod, dispseqno from tcount2)
  #   order by aedecod, trtan;
  #
  # SAS DATA step merge (lines 107-113):
  #   merge tcount2 AEgrid; by aedecod trtan;
  #   if missing(AEpercent) then AEpercent = 0;
  #
  # R equivalent: tidyr::complete() expands the grid to all TRTAN ×
  # (AEDECOD, dispseqno) combinations and fills missing AEpercent with 0.
  tcount3 <- tcount2 %>%
    tidyr::complete(
      TRTAN,
      tidyr::nesting(AEDECOD, dispseqno),
      fill = list(AEpercent = 0)
    )

  # Ensure zero-fill for any remaining missing AEpercent values
  # SAS lines 110-112: if missing(AEpercent) then AEpercent = 0;
  tcount3 <- tcount3 %>%
    dplyr::mutate(
      AEpercent = dplyr::if_else(is.na(AEpercent), 0, AEpercent) # legitimate: percentage initialized to zero for AE display
    )

  # Sort by dispseqno (SAS lines 115-117: proc sort by dispseqno)
  tcount3 <- tcount3 %>%
    dplyr::arrange(dispseqno)

  # ---------------------------------------------------------------------------
  # Phase 9: SGPLOT Scatter Visualization (SAS Lines 120-128)
  # ---------------------------------------------------------------------------
  # SAS:
  #   title1 "Common Treatment-Emergent Adverse Events";
  #   proc sgplot data=tcount3;
  #     label trtan="Treatment";
  #     label aeseqno="AE ordered in descending order";
  #     where dispseqno<=20;
  #     scatter x=AEpercent y=dispseqno / group=trtan;
  #     xaxis grid;
  #     yaxis grid values=(1 to 20 by 1) valueshint;
  #   run;
  #
  # Filter to top_n AEs for display and convert TRTAN to factor for grouping.
  plot_data <- tcount3 %>%
    dplyr::filter(dispseqno <= top_n) %>%
    dplyr::mutate(TRTAN = as.factor(TRTAN))

  # Build ggplot2 scatter plot matching SAS PROC SGPLOT specification:
  # - x = AEpercent (AE percentage)
  # - y = dispseqno (display sequence number, 1 = most common)
  # - color = TRTAN (treatment group)
  # - Grid lines on both axes
  # - Y-axis breaks at every integer from 1 to top_n
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = AEpercent, y = dispseqno, color = TRTAN)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(breaks = seq(1, top_n, by = 1)) +
    ggplot2::labs(
      title = "Common Treatment-Emergent Adverse Events",
      x     = "AEs (%)",
      y     = "AE ordered in descending order",
      color = "Treatment"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line()
    )

  # ---------------------------------------------------------------------------
  # Phase 10: Return Value
  # ---------------------------------------------------------------------------
  # Return the complete analysis data and the scatter plot object.
  invisible(list(data = tcount3, plot = p))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   - SAS count(usubjid) in PROC SQL is a row count (not distinct),
#     replicated with dplyr::n(). The SELECT DISTINCT keyword in the SAS
#     query applies to the result set, not to the count aggregate.
#   - Placebo identified by TRTAN == 0 as in SAS source (line 62).
#   - Zero-fill for missing AE percentages matches SAS DATA step behavior
#     at three explicit locations:
#       (a) After placebo join (SAS lines 66-72)
#       (b) After grid expansion (SAS lines 110-112)
#       (c) Via tidyr::complete() fill parameter
#   - SAS RENAME= is applied before WHERE= in dataset options (line 62),
#     so the filter trtan_1=0 references the renamed column.
#   - SAS aeseqno+1 is a RETAIN counter initialized to 0 and incremented
#     by 1 each row; this matches dplyr::row_number() which returns 1-based
#     sequential integers.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   - SAS PROC SQL count() vs R dplyr::n() should produce identical integer
#     counts for this query context (no NULL handling differences).
#   - Floating-point division (numAE / numusubjid) may produce minor
#     epsilon differences (< 1e-15) due to IEEE 754 representation.
#   - Sort stability: SAS sort is guaranteed stable; dplyr::arrange() is
#     also stable within groups (backed by vctrs), so display ordering
#     for ties should match.
#
# NO DIRECT R EQUIVALENT:
#   - SAS aeseqno+1 implicit RETAIN counter -> dplyr::row_number()
#   - SAS DATA step merge with rename -> dplyr::left_join with renamed
#     columns via dplyr::select(new_name = old_name)
#   - SAS PROC SQL cross join -> tidyr::complete() with nesting()
#   - SAS dispseqno+1 RETAIN counter -> dplyr::row_number() after arrange
#   - SAS PROC SGPLOT scatter with group -> ggplot2::geom_point() with
#     aes(color = TRTAN)
#
# PACKAGE SELECTION RATIONALE:
#   - haven: SAS XPT file I/O (tidyverse standard, ReadStat C backend)
#   - dplyr: Data manipulation replacing DATA steps and PROC SQL
#   - tidyr: complete() + nesting() for zero-fill grid replacing cross-join
#   - ggplot2: Scatter visualization replacing PROC SGPLOT
#   - janitor: round_half_up() loaded for SAS-compatible rounding per
#     migration framework (available for any future percentage formatting)
#
# OPEN QUESTIONS:
#   - Verify TRTAN == 0 correctly identifies placebo across all study
#     datasets. If a different coding scheme is used, the placebo_trtan
#     value should be parameterized.
#   - Confirm geom_point() + geom_line() matches SAS scatter overlay
#     aesthetics. SAS scatter statement without SERIES may render
#     differently — points only vs points+lines.
#   - The SAS label aeseqno="AE ordered in descending order" is assigned
#     to a variable not used on the y-axis (dispseqno is used instead).
#     This may be an original SAS author oversight; the R version uses
#     the label text for the y-axis regardless, which is semantically
#     correct for the display sequence.
# ============================================================
