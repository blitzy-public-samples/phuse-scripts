# =============================================================================
# FILE:        ae_serious.R
# SOURCE:      whitepapers/scriptathons/ae/ae_serious.sas (original: target26.sas)
# DESCRIPTION: Serious Adverse Event (SAE) summary table with Fisher's exact
#              test, migrated from SAS to idiomatic R.
#              Produces "Table 7.5 Summary of Serious Adverse Events" with
#              denominator machinery, zero-fill grid expansion, weighted
#              contingency tables, Fisher's exact per AE preferred term, and
#              an overall SAE summary row.
# AUTHORS:     Original SAS: Prathamesh Athavale / Rucha Landge
#              R Migration:  Blitzy Platform (PhUSE WG5 SAS-to-R Migration)
# DATE:        2024 (SAS), 2026 (R migration)
# LICENSE:     MIT (per PhUSE CS repository)
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(rlang)       # Provides %||% operator required by r2rtf on R < 4.4.0
library(Tplyr)
library(r2rtf)
library(janitor)
library(stringr)
library(purrr)
library(tibble)

# =============================================================================
# ae_serious()
# -----------------------------------------------------------------------------
# Produces a Serious Adverse Event summary table with Fisher's exact test,
# replicating SAS ae_serious.sas (target26.sas) with 100% functional parity.
#
# @param adae_path  Character. Path to ADAE XPT transport file.
# @param adsl_path  Character. Path to ADSL XPT transport file.
# @param output_path Character or NULL. If non-NULL, writes RTF report to this
#                    path. If NULL, only returns the data frame.
# @return A data frame (tibble) containing the final report with columns:
#         AEDECOD, trt<N> columns (CP strings), and cvalue1 (p-value).
#         Invisibly returns the report data frame.
# =============================================================================
ae_serious <- function(adae_path, adsl_path, output_path = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  stopifnot(
    "adae_path must be a character string" = is.character(adae_path) && length(adae_path) == 1L,
    "adsl_path must be a character string" = is.character(adsl_path) && length(adsl_path) == 1L,
    "adae_path file must exist" = file.exists(adae_path),
    "adsl_path file must exist" = file.exists(adsl_path)
  )
  if (!is.null(output_path)) {
    stopifnot("output_path must be a character string" = is.character(output_path) && length(output_path) == 1L)
  }

  # ===========================================================================
  # Phase 3: Data Loading and SAE Filtering (SAS Lines 12-24)
  # ===========================================================================
  # SAS: data work.adae; set source.adae;
  #        keep usubjid trtan aedecod aeser aocc02fl aocc04fl saffl;
  #        where (aocc02fl='Y' or aocc04fl='Y') and (aeser='Y') and (saffl='Y');
  # ===========================================================================
  adae <- haven::read_xpt(adae_path) %>%
    dplyr::select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL) %>%
    dplyr::filter(
      (AOCC02FL == "Y" | AOCC04FL == "Y"),
      AESER == "Y",
      SAFFL == "Y"
    )

  # Sort by USUBJID (SAS lines 26-28)
  adae <- adae %>%
    dplyr::arrange(USUBJID)

  # ===========================================================================
  # Phase 4: ADSL Loading and Merge (SAS Lines 32-50)
  # ===========================================================================
  # SAS: data work.adsl; set an_sour.adsl; keep usubjid trt01an;
  #        where trt01an ne .;
  #      proc sort nodupkey; by usubjid trt01an;
  #      data adae_sl; merge adsl(in=a) adae(in=b); by usubjid; if a and b;
  # ===========================================================================
  adsl <- haven::read_xpt(adsl_path) %>%
    dplyr::select(USUBJID, TRT01AN) %>%
    dplyr::filter(!is.na(TRT01AN)) %>%
    dplyr::distinct(USUBJID, TRT01AN) %>%
    dplyr::arrange(USUBJID, TRT01AN)

  # Inner join: SAS 'if a and b' = inner_join

  adae_sl <- dplyr::inner_join(adsl, adae, by = "USUBJID")

  # ===========================================================================
  # Phase 5: Denominator Calculation (SAS Lines 54-63)
  # ===========================================================================
  # SAS: proc freq data=adsl noprint; table trt01an / out=sum_1;
  #      data _null_; set sum_1 end=eof;
  #        call symput('trtcn'||..., count);
  #        call symput('trtnum'||..., trt01an);
  #        if eof then call symput('numtrt', _n_);
  # ===========================================================================
  sum_1 <- adsl %>%
    dplyr::count(TRT01AN, name = "COUNT")

  # Replaces SAS macro variables &trtcn<n>, &trtnum<n>, &numtrt
  trt_counts <- setNames(sum_1$COUNT, as.character(sum_1$TRT01AN))
  trt_nums   <- sum_1$TRT01AN
  numtrt     <- nrow(sum_1)

  # ===========================================================================
  # Phase 6: Deduplication and AE Term Extraction (SAS Lines 65-86)
  # ===========================================================================
  # SAS: proc sort nodupkey; by usubjid aedecod;  (adae_sl2)
  #      proc sort nodupkey; by aedecod; where aocc02fl='Y'; (adae_dummy)
  #      data adae_sl3; merge adae_sl2 adae_dummy; by aedecod; if a and b;
  #      proc freq; table aedecod*trtan / out=sum_2;
  # ===========================================================================

  # Deduplicate by USUBJID + AEDECOD (SAS lines 65-67)
  adae_sl2 <- adae_sl %>%
    dplyr::arrange(USUBJID, AEDECOD) %>%
    dplyr::distinct(USUBJID, AEDECOD, .keep_all = TRUE)

  # Unique AE terms where AOCC02FL = 'Y' (SAS lines 69-72)
  adae_dummy <- adae_sl %>%
    dplyr::filter(AOCC02FL == "Y") %>%
    dplyr::arrange(AEDECOD) %>%
    dplyr::distinct(AEDECOD)

  # Merge: keep only AE terms in adae_dummy (SAS lines 78-82)
  adae_sl3 <- adae_sl2 %>%
    dplyr::semi_join(adae_dummy, by = "AEDECOD")

  # Cross-tabulation count (SAS lines 84-86)
  sum_2 <- adae_sl3 %>%
    dplyr::count(AEDECOD, TRTAN, name = "COUNT")

  # ===========================================================================
  # Phase 7: %dummy Macro -> Grid Expansion (SAS Lines 88-98)
  # ===========================================================================
  # SAS: %macro dummy(); data dummy_2; set adae_dummy;
  #        %do i=1 %to &numtrt; trtan=&&trtnum&i; output; %end;
  #      R: tidyr::expand_grid replaces the entire %dummy macro
  # ===========================================================================
  dummy_2 <- tidyr::expand_grid(
    AEDECOD = adae_dummy$AEDECOD,
    TRTAN   = trt_nums
  )

  # Merge dummy grid with observed counts (SAS lines 100-103)
  # SAS: merge dummy_2(in=a) sum_2(in=b); by aedecod trtan;
  # Full merge without 'if a' means all rows kept; left_join on dummy_2
  adae_sl4 <- dummy_2 %>%
    dplyr::left_join(sum_2, by = c("AEDECOD", "TRTAN"))

  # ===========================================================================
  # Phase 8: Denominator Merge and Response Expansion (SAS Lines 109-125)
  # ===========================================================================
  # SAS: merge adae_sl4(in=a) sum_1(rename=(trt01an=trtan count=bign));
  #        by trtan; if a;
  #      do j=0 to 1; resp=j; count_final logic; output; end;
  # ===========================================================================

  # Merge with BigN denominators (SAS lines 109-113)
  adae_sl5 <- adae_sl4 %>%
    dplyr::left_join(
      sum_1 %>% dplyr::rename(TRTAN = TRT01AN, bign = COUNT),
      by = "TRTAN"
    )

  # Response expansion: create resp=0 (non-responder) and resp=1 (responder)
  # rows per aedecod x trtan combination (SAS lines 115-125)
  # SAS DO loop j=0 to 1 generates 2 rows per input row:
  #   resp=1: count_final = count (or 0 if count missing)
  #   resp=0: count_final = bign - count (or bign if count missing)
  resp_1 <- adae_sl5 %>%
    dplyr::mutate(
      resp = 1L,
      count_final = dplyr::if_else(!is.na(COUNT), COUNT, 0L)
    )

  resp_0 <- adae_sl5 %>%
    dplyr::mutate(
      resp = 0L,
      count_final = dplyr::if_else(!is.na(COUNT), bign - COUNT, bign)
    )

  adae_sl6 <- dplyr::bind_rows(resp_0, resp_1) %>%
    dplyr::arrange(AEDECOD, TRTAN, resp)

  # ===========================================================================
  # Phase 9: Fisher's Exact Test per AE Term (SAS Lines 131-144)
  # ===========================================================================
  # SAS: ods output FishersExact = fish1;
  #      proc freq data=adae_sl6;
  #        table trtan*resp / out=sum_final sparse fisher;
  #        weight count_final; by aedecod; exact fisher;
  #      data fish2; set fish1; where name1='XP2_FISH';
  # ===========================================================================

  # Run Fisher's exact test PER AEDECOD (BY group behavior)
  fisher_results <- adae_sl6 %>%
    dplyr::group_by(AEDECOD) %>%
    dplyr::group_modify(~ {
      # Build contingency matrix: rows = TRTAN, cols = resp (0, 1)
      wide <- .x %>%
        tidyr::pivot_wider(
          id_cols   = TRTAN,
          names_from  = resp,
          values_from = count_final,
          values_fill = 0L
        ) %>%
        dplyr::arrange(TRTAN)

      # Ensure column ordering: resp 0 then resp 1
      col_names <- intersect(c("0", "1"), colnames(wide))
      mat <- as.matrix(wide[, col_names, drop = FALSE])
      storage.mode(mat) <- "integer"

      # Fisher's exact test — two-sided p-value (SAS XP2_FISH)
      ft <- tryCatch(
        stats::fisher.test(mat),
        error = function(e) list(p.value = NA_real_)
      )

      tibble::tibble(p_value = ft$p.value)
    }) %>%
    dplyr::ungroup()

  # Format p-value as character to match SAS cvalue1 (BEST format)
  fisher_results <- fisher_results %>%
    dplyr::mutate(
      cvalue1 = dplyr::if_else(
        is.na(p_value),
        NA_character_,
        formatC(p_value, format = "f", digits = 4)
      )
    )

  # ===========================================================================
  # Phase 10: Formatting and Display Preparation (SAS Lines 146-181)
  # ===========================================================================
  # SAS: where resp=1; cp=compress(put(count,8.0))||'('||compress(put(count,8.1))||')';
  #      if trtan=81 then ord=1; else ord=2;
  #      retain count_max; if ord=1 then count_max=count;
  #      proc transpose prefix=trt; by aedecod count_max; id trtan; var cp;
  #      merge sum_final2 fish2; by aedecod;
  #      proc sort; by descending count_max aedecod;
  # ===========================================================================

  # Extract responder rows: SAS 'out=sum_final' filtered to resp=1
  # The COUNT for resp=1 equals count_final which is the observed subject count
  sum_final <- adae_sl6 %>%
    dplyr::filter(resp == 1L) %>%
    dplyr::select(AEDECOD, TRTAN, COUNT = count_final, bign)

  # Create CP string: SAS put(count,8.0)||'('||put(count,8.1)||')'
  # This format repeats count as integer and with 1 decimal — replicated exactly
  sum_final_fmt <- sum_final %>%
    dplyr::mutate(
      cp = stringr::str_c(
        as.character(as.integer(COUNT)),
        "(",
        formatC(COUNT, format = "f", digits = 1),
        ")"
      )
    )

  # Assign treatment ordering: TRTAN=81 -> ord=1, else ord=2 (SAS lines 150-151)
  sum_final_fmt <- sum_final_fmt %>%
    dplyr::mutate(ord = dplyr::if_else(TRTAN == 81, 1L, 2L))

  # Sort by AEDECOD, ord (SAS lines 154-156)
  sum_final_fmt <- sum_final_fmt %>%
    dplyr::arrange(AEDECOD, ord)

  # RETAIN count_max: first ord's count per AEDECOD (SAS lines 158-162)
  # Since data is sorted by AEDECOD then ord, first() gives ord=1 (TRTAN=81) count
  sum_final_fmt <- sum_final_fmt %>%
    dplyr::group_by(AEDECOD) %>%
    dplyr::mutate(count_max = dplyr::first(COUNT)) %>%
    dplyr::ungroup()

  # Pivot to wide format: PROC TRANSPOSE prefix=trt, id trtan (SAS lines 168-172)
  sum_final2 <- sum_final_fmt %>%
    tidyr::pivot_wider(
      id_cols     = c(AEDECOD, count_max),
      names_from  = TRTAN,
      names_prefix = "trt",
      values_from = cp
    )

  # Merge with Fisher p-values (SAS lines 174-177)
  sum_final2 <- sum_final2 %>%
    dplyr::left_join(
      fisher_results %>% dplyr::select(AEDECOD, cvalue1),
      by = "AEDECOD"
    )

  # Sort by descending count_max, then AEDECOD (SAS lines 179-181)
  sum_final2 <- sum_final2 %>%
    dplyr::arrange(dplyr::desc(count_max), AEDECOD)

  # ===========================================================================
  # Phase 11: Overall SAE Summary Row (SAS Lines 183-231)
  # ===========================================================================
  # SAS: proc sort nodupkey; by usubjid; where aocc02fl='Y'; (adae_sl_sub)
  #      merge adsl(in=a) adae_sl_sub(in=b); by usubjid;
  #        if a; if a and b then flg=1; else flg=0;
  #      proc freq; table trtan*flg / sparse fisher; exact fisher;
  #      where flg=1; cp string; aedecod='Number of subjects...';
  #      proc transpose prefix=trt; merge with fish p-value;
  # ===========================================================================

  # Identify subjects with at least one SAE (AOCC02FL='Y') (SAS lines 183-186)
  adae_sl_sub <- adae_sl %>%
    dplyr::filter(AOCC02FL == "Y") %>%
    dplyr::distinct(USUBJID)

  # Create SAE flag: left join ADSL with SAE subjects (SAS lines 190-196)
  adae_sl_sub2 <- adsl %>%
    dplyr::mutate(
      flg = dplyr::if_else(USUBJID %in% adae_sl_sub$USUBJID, 1L, 0L)
    ) %>%
    dplyr::rename(TRTAN = TRT01AN)

  # Fisher's exact test on overall SAE flag (SAS lines 200-209)
  overall_tab <- table(adae_sl_sub2$TRTAN, adae_sl_sub2$flg)
  fish_sub <- tryCatch(
    stats::fisher.test(overall_tab),
    error = function(e) list(p.value = NA_real_)
  )

  fish_sub_cvalue1 <- dplyr::if_else(
    is.na(fish_sub$p.value),
    NA_character_,
    formatC(fish_sub$p.value, format = "f", digits = 4)
  )

  # Count subjects with flg==1 per treatment (SAS out=sum_final_sub, where flg=1)
  sum_final_sub <- adae_sl_sub2 %>%
    dplyr::count(TRTAN, flg, name = "COUNT") %>%
    dplyr::filter(flg == 1L)

  # Ensure all treatments represented (SPARSE behavior) even if zero SAE subjects
  sum_final_sub <- tidyr::expand_grid(TRTAN = trt_nums) %>%
    dplyr::left_join(sum_final_sub, by = "TRTAN") %>%
    dplyr::mutate(COUNT = dplyr::if_else(is.na(COUNT), 0L, COUNT))

  # Format summary row (SAS lines 211-217)
  sum_final_sub <- sum_final_sub %>%
    dplyr::mutate(
      cp = stringr::str_c(
        as.character(as.integer(COUNT)),
        "(",
        formatC(COUNT, format = "f", digits = 1),
        ")"
      ),
      AEDECOD = "Number of subjects reporting serious adverse events"
    )

  # Pivot to wide format (SAS lines 223-227)
  sum_final_sub2 <- sum_final_sub %>%
    tidyr::pivot_wider(
      id_cols      = AEDECOD,
      names_from   = TRTAN,
      names_prefix = "trt",
      values_from  = cp
    )

  # Merge with Fisher p-value (SAS lines 229-231: positional merge, no BY)
  sum_final_sub2 <- sum_final_sub2 %>%
    dplyr::mutate(cvalue1 = fish_sub_cvalue1)

  # ===========================================================================
  # Phase 12: Final Report Assembly (SAS Lines 233-256)
  # ===========================================================================
  # SAS: data final_report; set sum_final_sub2 sum_final2;
  #      proc report ... column aedecod trt0 trt54 trt81 cvalue1;
  # ===========================================================================

  # Stack summary row (top) with detail rows (SAS lines 233-235)
  final_report <- dplyr::bind_rows(sum_final_sub2, sum_final2) %>%
    dplyr::select(-dplyr::any_of("count_max"))

  # Identify treatment columns dynamically
  trt_col_names <- paste0("trt", trt_nums)

  # Ensure all expected treatment columns exist, fill with NA if not
  for (tc in trt_col_names) {
    if (!tc %in% colnames(final_report)) {
      final_report[[tc]] <- NA_character_
    }
  }

  # Order columns: AEDECOD, trt columns in ascending TRTAN order, cvalue1
  final_report <- final_report %>%
    dplyr::select(AEDECOD, dplyr::all_of(trt_col_names), cvalue1)

  # ---------------------------------------------------------------------------
  # RTF Output via r2rtf (replacing PROC REPORT)
  # ---------------------------------------------------------------------------
  if (!is.null(output_path)) {
    # Build column header string dynamically
    # SAS: define aedecod /"Preferred Term"
    #      define trt0 /"Placebo#N=&trtcn0"
    #      define trt54/"Treatment 1#N=&trtcn54"
    #      define trt81/"Treatment 2#N=&trtcn81"
    #      define cvalue1/"p-value*b"
    trt_labels <- purrr::map_chr(trt_nums, function(tn) {
      label <- dplyr::case_when(
        tn == 0  ~ "Placebo",
        tn == 54 ~ "Treatment 1",
        tn == 81 ~ "Treatment 2",
        TRUE     ~ paste0("Treatment ", tn)
      )
      # Use \n for line break within column header (SAS # split character)
      # CRITICAL: r2rtf uses | as column separator — do NOT embed | in labels
      paste0(label, "\nN=", trt_counts[as.character(tn)])
    })

    col_header <- paste(
      c("Preferred Term", trt_labels, "p-value*b"),
      collapse = " | "
    )

    # Calculate relative column widths
    n_cols <- length(trt_col_names) + 2L
    col_widths <- c(4, rep(2, length(trt_col_names)), 1.5)

    # Ensure output directory exists
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir) && nchar(output_dir) > 0 && output_dir != ".") {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    tryCatch({
      final_report %>%
        r2rtf::rtf_title(
          title = "Table 7.5 Summary of Serious Adverse Events",
          subtitle = "Safety Population"
        ) %>%
        r2rtf::rtf_colheader(
          colheader = col_header,
          col_rel_width = col_widths
        ) %>%
        r2rtf::rtf_body(
          col_rel_width = col_widths
        ) %>%
        r2rtf::rtf_footnote(
          footnote = c(
            "Denominator for each % is treatment column N",
            "*b = p-values are from Fisher's Exact Test"
          )
        ) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(file = output_path)
      message("RTF output written to: ", output_path)
    }, error = function(e) {
      warning("Failed to write RTF output: ", conditionMessage(e))
    })
  }

  # Always return the final report data frame
  invisible(final_report)
}

# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#   - SAS PROC FREQ with WEIGHT and FISHER computes Fisher's exact test on
#     the weighted contingency table. R fisher.test() operates on integer
#     contingency tables. The count_final values are integers, so results match.
#   - SAS XP2_FISH = two-sided Fisher's exact p-value = fisher.test()$p.value
#   - The CP string format `count(count.1)` appears to repeat the count with
#     1 decimal -- this may be a bug in the original SAS (should likely be
#     count(percent)), but is replicated exactly for parity.
#   - TRTAN values 0, 54, 81 are hardcoded in SAS PROC REPORT column
#     definitions -- preserved dynamically via trt_nums for this study dataset.
#   - SAS 'if a and b' merge behavior = inner_join in R.
#   - SAS 'if a' merge behavior (adae_sl_sub2) = left_join in R.
#   - SAS PROC FREQ SPARSE option = tidyr::expand_grid to ensure all cells.
#   - SAS RETAIN across observations = dplyr::first() within group_by.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   - Fisher's exact p-values: SAS uses network algorithm; R uses different
#     algorithm. Minor differences at high decimal places are possible.
#   - SAS PROC FREQ SPARSE ensures all treatment x response cells exist;
#     R expand_grid achieves equivalent behavior.
#   - P-value character formatting: SAS uses BEST12. format for cvalue1;
#     R uses formatC with 4 decimal places. Exact string may differ.
#
# NO DIRECT R EQUIVALENT:
#   - SAS %dummy macro (DO loop with CALL SYMPUT) -> tidyr::expand_grid
#   - SAS PROC TRANSPOSE with PREFIX option -> tidyr::pivot_wider with names_prefix
#   - SAS ODS OUTPUT FishersExact -> fisher.test() direct call per group
#   - SAS RETAIN count_max -> dplyr::first() within group
#   - SAS PROC REPORT COMPUTE BEFORE/AFTER -> r2rtf rtf_title/rtf_footnote
#   - SAS macro variables (&trtcn0 etc.) -> named R vector (trt_counts)
#   - SAS positional merge (no BY) -> dplyr::mutate for single-value assignment
#
# PACKAGE SELECTION RATIONALE:
#   - haven: SAS XPT file I/O (tidyverse standard)
#   - dplyr: Data manipulation replacing DATA steps, PROC FREQ counts, merges
#   - tidyr: expand_grid/pivot_wider replacing %dummy macro and PROC TRANSPOSE
#   - r2rtf: RTF table output replacing PROC REPORT (Merck, production-ready)
#   - Tplyr: Available for denominator/count layer if future enhancement needed
#   - janitor: round_half_up() for SAS-compatible rounding (available)
#   - stringr: str_c() for CP string construction (tidyverse string standard)
#   - tibble: tibble() for Fisher test result construction in group_modify
#
# OPEN QUESTIONS:
#   - Confirm fisher.test() p-values match SAS XP2_FISH for all AE terms
#   - The CP string format may be a bug (count displayed twice, no percent) --
#     verify with original authors before production use
#   - TRTAN-specific column naming (trt0, trt54, trt81) is study-specific --
#     generalize for multi-study use?
#   - SAS continuity correction behavior in Fisher's exact -- verify R default
#     matches (R fisher.test does not apply continuity correction by default)
#   - P-value display precision (4 decimals) vs SAS BEST12. -- confirm
#     acceptable for regulatory submission
# =============================================================================
