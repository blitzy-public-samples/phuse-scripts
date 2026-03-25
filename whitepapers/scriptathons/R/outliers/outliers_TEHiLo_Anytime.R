# =============================================================================
# Treatment-Emergent High/Low Anytime Table with Fisher's Exact Test
# =============================================================================
#
# SOURCE:    whitepapers/scriptathons/outliers/outliers_TEHiLo_Anytime.sas
# MIGRATION: SAS -> R  (March 2026)
#
# DESCRIPTION:
#   Computes treatment-emergent abnormal high (CRIT2FL) and low (CRIT1FL)
#   at any time for a specified vital sign parameter (default: DIABP),
#   with Fisher's exact p-values and formatted PROC REPORT-style output
#   rendered via r2rtf.
#
#   Analysis flow:
#     1. Read ADVS CDISC ADaM dataset from XPT
#     2. Filter to safety population, post-baseline (ADY > 1), and
#        subjects with at least one non-missing CRIT1FL or CRIT2FL
#     3. Count unique subjects per treatment and direction (Low/High)
#     4. Compute Fisher's exact test p-values per direction
#     5. Merge counts and p-values; format for reporting
#     6. Generate RTF table output via r2rtf
#
#   SAS-to-R Mapping:
#     - PROC SQL count(unique(...)) -> dplyr::n_distinct()
#     - %fisherpval macro           -> compute_fisher_pval() R function
#     - PROC FREQ EXACT FISHER      -> stats::fisher.test()
#     - DATA step merge by tefl     -> dplyr::left_join()
#     - PROC REPORT + ODS RTF       -> r2rtf pipeline
#     - SAS put(pct, 5.1)           -> janitor::round_half_up() + sprintf()
#     - SAS pvalue5.3 format        -> sprintf("%.3f", ...)
#
# REGULATORY:
#   - Rounding: janitor::round_half_up() for SAS-compatible round-half-up
#   - Missing: NA (numeric) / NA_character_ (character) — no zero imputation
#   - Date epoch not relevant for this analysis
#
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)      # XPT data I/O — read_xpt()
library(dplyr)      # Data manipulation — replaces DATA steps + PROC SQL
library(tidyr)      # Data reshaping — loaded per migration framework
library(rlang)      # Tidy evaluation — required by r2rtf internal %||% operator
library(Tplyr)      # Clinical table grammar — loaded per migration framework
library(r2rtf)      # RTF output — replaces PROC REPORT + ODS RTF
library(janitor)    # round_half_up() for SAS-compatible rounding
library(stringr)    # String formatting — str_pad()

# =============================================================================
# compute_fisher_pval
# =============================================================================
#
# Replaces the SAS %fisherpval macro (source lines 93-116):
#   %macro fisherpval(flg=, tefl=);
#     proc sort data=advs; by usubjid &flg; run;
#     data &flg; set advs; by usubjid &flg; if last.usubjid; run;
#     ods output FishersExact=fisher_&flg;
#     proc freq data = &flg; tables param*trtpn*&flg/fisher; run;
#     data fisher_&flg; ... where name1="XP2_FISH"; ...
#   %mend;
#
# Parameters:
#   data       - data.frame/tibble containing USUBJID, TRTPN, and the flag variable
#                (should already be filtered to non-missing flag values)
#   flag_var   - character string: name of the criterion flag column ("CRIT1FL" or "CRIT2FL")
#   tefl_label - character string: direction label ("Low" or "High")
#
# Returns:
#   A tibble with columns: tefl (character), nvalue1 (numeric p-value)
#
# SAS Semantic Preservation:
#   - SAS sorts by USUBJID and flag, then keeps last.usubjid -> R arranges
#     by USUBJID and flag_var then takes slice_tail(n=1) per USUBJID group
#   - SAS PROC FREQ default excludes missing from tables -> R pre-filters
#     to non-missing flag values before calling this function
#   - SAS XP2_FISH = two-sided Fisher's exact p-value -> fisher.test()$p.value
# =============================================================================
compute_fisher_pval <- function(data, flag_var, tefl_label) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("compute_fisher_pval: 'data' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!flag_var %in% colnames(data)) {
    stop(
      paste0("compute_fisher_pval: flag variable '", flag_var, "' not found in data."),
      call. = FALSE
    )
  }
  required_cols <- c("USUBJID", "TRTPN")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "compute_fisher_pval: required column(s) missing: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # --- Deduplicate to last record per subject ---------------------------------
  # Replicates SAS: proc sort by usubjid &flg; data &flg; if last.usubjid;
  # SAS missing sorts LOW; after sort, non-missing flag values come after missing.
  # 'last.usubjid' picks the last row (i.e., the non-missing / highest flag value).
  # Since data is pre-filtered to non-missing flag_var, sorting by flag_var within

  # USUBJID and taking the last row picks the "Y" value when both "Y" and "N" exist
  # (because "Y" > "N" alphabetically).
  deduped <- data %>%
    arrange(USUBJID, .data[[flag_var]]) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # --- Build contingency table: TRTPN x flag_var ------------------------------
  # Replicates SAS: tables param*trtpn*&flg/fisher
  # PROC FREQ generates a contingency table of TRTPN by the flag variable.
  # The table() call creates the 2-way cross-tabulation.
  cont_table <- table(deduped$TRTPN, deduped[[flag_var]])

  # --- Fisher's exact test ----------------------------------------------------
  # Replicates SAS: ods output FishersExact; where name1="XP2_FISH"
  # XP2_FISH is the two-sided Fisher's exact p-value.
  # R fisher.test() returns the two-sided p-value by default.
  # Edge case: if contingency table has < 2 rows or columns, fisher.test fails.
  fisher_p <- tryCatch(
    {
      fisher_result <- stats::fisher.test(cont_table)
      fisher_result$p.value
    },
    error = function(e) {
      warning(
        paste0(
          "compute_fisher_pval: Fisher's exact test failed for direction '",
          tefl_label, "': ", conditionMessage(e),
          ". Returning NA for p-value."
        ),
        call. = FALSE
      )
      NA_real_
    }
  )

  # Return structured result matching SAS fisher_&flg dataset structure

  tibble::tibble(tefl = tefl_label, nvalue1 = fisher_p)
}

# =============================================================================
# outliers_tehilo_anytime
# =============================================================================
#
# Main analysis function — replaces the entire SAS program
# (source lines 1-192 of outliers_TEHiLo_Anytime.sas)
#
# Parameters:
#   data_path   - character: path to directory containing advs.xpt
#                 (replaces SAS: filename source url "..."; libname source xport;)
#   output_path - character: output directory for RTF table
#                 (replaces SAS: ODS RTF FILE=)
#   paramcd     - character: PARAMCD filter value (default "DIABP")
#                 (replaces SAS: WHERE PARAMCD="DIABP")
#   atptn       - numeric: ATPTN filter value (default 815)
#                 (replaces SAS: WHERE ATPTN=815)
#
# Returns:
#   A tibble containing the formatted table data (invisibly).
#   Side effect: writes RTF file to output_path.
#
# SAS Source Cross-Reference:
#   Lines 1-14:   Annotations (embedded as R comments)
#   Lines 16-24:  Data loading and filtering
#   Lines 26-66:  PROC SQL count computations (Low/High totN and n)
#   Lines 68-89:  Data combination, sort, merge, pct calculation
#   Lines 92-123: %fisherpval macro calls for Low and High
#   Lines 125-140: Merge Fisher p-values, format pctc and nc
#   Lines 142-152: Sort and first-p-value display logic
#   Lines 154-187: PROC REPORT output with titles and footnotes
# =============================================================================
outliers_tehilo_anytime <- function(data_path,
                                    output_path,
                                    paramcd = "DIABP",
                                    atptn   = 815) {

  # --- Input validation -------------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L) {
    stop("outliers_tehilo_anytime: 'data_path' must be a single character string.",
         call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L) {
    stop("outliers_tehilo_anytime: 'output_path' must be a single character string.",
         call. = FALSE)
  }
  if (!is.character(paramcd) || length(paramcd) != 1L) {
    stop("outliers_tehilo_anytime: 'paramcd' must be a single character string.",
         call. = FALSE)
  }
  if (!is.numeric(atptn) || length(atptn) != 1L) {
    stop("outliers_tehilo_anytime: 'atptn' must be a single numeric value.",
         call. = FALSE)
  }

  xpt_file <- file.path(data_path, "advs.xpt")
  if (!file.exists(xpt_file)) {
    stop(
      paste0("outliers_tehilo_anytime: ADVS dataset not found at: ", xpt_file),
      call. = FALSE
    )
  }

  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # ===========================================================================
  # STEP 1: Data Loading and Filtering
  # ===========================================================================
  # Replaces SAS lines 16-24:
  #   filename source url "...advs.xpt"; libname source xport;
  #   data advs; set inlib.advs;
  #     WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y"
  #           and (not missing(CRIT1FL) or not missing(CRIT2FL));
  #     keep USUBJID TRTPN PARAM PARAMCD AVAL CRIT1 CRIT1FL CRIT2 CRIT2FL;
  #   run;
  #
  # NOTE: SAS line 21 references 'inlib.advs' which is likely a copy error
  #       from another program; the URL-based libname 'source' (line 19)
  #       is the intended data source. Mapped to haven::read_xpt().
  # ---------------------------------------------------------------------------
  advs_raw <- haven::read_xpt(xpt_file)

  advs <- advs_raw %>%
    filter(
      PARAMCD == paramcd,
      ATPTN   == atptn,
      ADY > 1,
      SAFFL == "Y",
      # CRITICAL: SAS uses OR (not AND) — include if EITHER criterion is non-missing
      !is.na(CRIT1FL) | !is.na(CRIT2FL)
    ) %>%
    select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

  # Validate that filtered data is not empty
  if (nrow(advs) == 0L) {
    warning(
      paste0(
        "outliers_tehilo_anytime: No records after filtering for PARAMCD='",
        paramcd, "', ATPTN=", atptn,
        ". Returning empty result."
      ),
      call. = FALSE
    )
    return(invisible(tibble::tibble()))
  }

  # ===========================================================================
  # STEP 2: Count Computation — Low Direction (CRIT1FL)
  # ===========================================================================
  # Replaces SAS PROC SQL lines 26-45:
  #   - totn_low: count(unique(usubjid)) where not missing(crit1fl) -> n_distinct
  #   - n_low:    count(unique(usubjid)) where crit1fl="Y"          -> n_distinct
  #
  # Direction mapping (per SAS code logic, NOT annotations which are swapped):
  #   CRIT1FL -> "Low"  (treatment-emergent low criterion)
  #   CRIT2FL -> "High" (treatment-emergent high criterion)
  # ---------------------------------------------------------------------------

  # Total N for TE Low: subjects with at least one non-missing CRIT1FL record
  # (SAS lines 28-35)
  totn_low <- advs %>%
    filter(!is.na(CRIT1FL)) %>%
    group_by(TRTPN, PARAM) %>%
    summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(tefl = "Low")

  # n for TE Low: subjects with CRIT1FL = "Y" (SAS lines 38-45)
  n_low <- advs %>%
    filter(CRIT1FL == "Y") %>%
    group_by(TRTPN, PARAM) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(tefl = "Low")

  # ===========================================================================
  # STEP 3: Count Computation — High Direction (CRIT2FL)
  # ===========================================================================
  # Replaces SAS PROC SQL lines 47-65:
  #   - totn_high: count(unique(usubjid)) where not missing(crit2fl)
  #   - n_high:    count(unique(usubjid)) where crit2fl="Y"
  #
  # NOTE: SAS comment at line 57 says "Count number of TE low subjects"
  # but the code uses CRIT2FL which is the HIGH criterion. This is a
  # copy-paste error in the SAS source comments.
  # ---------------------------------------------------------------------------

  # Total N for TE High: subjects with at least one non-missing CRIT2FL record
  # (SAS lines 48-55)
  totn_high <- advs %>%
    filter(!is.na(CRIT2FL)) %>%
    group_by(TRTPN, PARAM) %>%
    summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(tefl = "High")

  # n for TE High: subjects with CRIT2FL = "Y" (SAS lines 58-65)
  n_high <- advs %>%
    filter(CRIT2FL == "Y") %>%
    group_by(TRTPN, PARAM) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(tefl = "High")

  # ===========================================================================
  # STEP 4: Combine Datasets and Compute Percentage
  # ===========================================================================
  # Replaces SAS lines 68-89:
  #   data totn; set totn_high totn_low; run;
  #   data te_n; set n_high n_low; run;
  #   proc sort data=totn; by trtpn param; run;
  #   proc sort data=te_n; by trtpn param; run;
  #   data all_n; merge totn te_n; by trtpn param; pct=n/totn*100; run;
  #
  # NOTE: The SAS merge is by trtpn param (without tefl), which works due to
  # positional row alignment within BY groups. R left_join by all three keys
  # (TRTPN, PARAM, tefl) is semantically more correct and avoids the SAS
  # many-to-many merge pitfall when counts are unbalanced.
  # ---------------------------------------------------------------------------
  totn  <- bind_rows(totn_high, totn_low)
  te_n  <- bind_rows(n_high, n_low)

  all_n <- totn %>%
    left_join(te_n, by = c("TRTPN", "PARAM", "tefl")) %>%
    mutate(
      n   = coalesce(n, 0L),
      pct = n / totn * 100
    )

  # ===========================================================================
  # STEP 5: Fisher's Exact Test — Both Directions
  # ===========================================================================
  # Replaces SAS lines 92-123:
  #   %fisherpval(flg=crit1fl, tefl=Low);
  #   %fisherpval(flg=crit2fl, tefl=High);
  #   data all_fisher; set fisher_crit1fl fisher_crit2fl; run;
  #
  # The SAS %fisherpval macro:
  #   1. Sorts the FULL advs by USUBJID and the flag variable
  #   2. Keeps last.USUBJID (deduplicates to one record per subject)
  #   3. Runs PROC FREQ with FISHER option
  #   4. Extracts XP2_FISH (two-sided Fisher's exact p-value)
  #
  # R equivalent: pre-filter to non-missing flag, then call compute_fisher_pval()
  # Pre-filtering matches SAS behavior because:
  #   - SAS sorts by USUBJID + flag; missing sorts LOW
  #   - last.USUBJID picks non-missing over missing when both exist
  #   - PROC FREQ excludes missing by default
  # ---------------------------------------------------------------------------
  fisher_low  <- compute_fisher_pval(
    data       = advs %>% filter(!is.na(CRIT1FL)),
    flag_var   = "CRIT1FL",
    tefl_label = "Low"
  )

  fisher_high <- compute_fisher_pval(
    data       = advs %>% filter(!is.na(CRIT2FL)),
    flag_var   = "CRIT2FL",
    tefl_label = "High"
  )

  all_fisher <- bind_rows(fisher_low, fisher_high)

  # ===========================================================================
  # STEP 6: Merge Fisher P-values and Format for Reporting
  # ===========================================================================
  # Replaces SAS lines 125-152:
  #   proc sort data=all_fisher; by tefl; run;
  #   proc sort data=all_n; by tefl trtpn; run;
  #   data table_vals;
  #     merge all_n all_fisher; by tefl;
  #     pctc="("||strip(put(pct, 5.1))||")";
  #     nc=strip(put(n, best.));
  #   run;
  #   proc sort data=table_vals; by param descending tefl nvalue1; run;
  #   data table_vals; set table_vals;
  #     by param descending tefl nvalue1;
  #     if first.nvalue1 then nvalue1c=put(nvalue1, pvalue5.3);
  #     else nvalue1c="";
  #   run;
  #
  # Formatting notes:
  #   - SAS put(pct, 5.1): formats with width 5 and 1 decimal place
  #   - strip(): removes leading/trailing blanks
  #   - SAS put(nvalue1, pvalue5.3): p-value with 3 decimal places
  #   - janitor::round_half_up() ensures SAS round-half-up behavior
  # ---------------------------------------------------------------------------

  # Merge Fisher p-values with count data (many-to-one by tefl)
  table_vals <- all_n %>%
    left_join(all_fisher, by = "tefl") %>%
    mutate(
      # Format percentage: "(XX.X)" with right-aligned padding inside parens
      # SAS: pctc = "(" || strip(put(pct, 5.1)) || ")"
      # str_pad(width=5) replicates the SAS 5.1 format width before stripping
      pctc = paste0(
        "(",
        stringr::str_pad(
          sprintf("%.1f", janitor::round_half_up(pct, digits = 1)),
          width = 5
        ),
        ")"
      ),
      # Format n as character: SAS nc = strip(put(n, best.))
      nc = as.character(n)
    )

  # Sort for display: by PARAM, descending tefl, nvalue1
  # SAS: proc sort by param descending tefl nvalue1;
  # desc(tefl): "Low" sorts before "High" (L > H descending)
  table_vals <- table_vals %>%
    arrange(PARAM, desc(tefl), nvalue1)

  # Display p-value only once per direction (first row within each PARAM x tefl)
  # SAS: by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=...
  # Since nvalue1 is constant within each tefl group (one Fisher p per direction),
  # first.nvalue1 = first row per PARAM x tefl group.
  table_vals <- table_vals %>%
    group_by(PARAM, tefl) %>%
    mutate(
      nvalue1c = if_else(
        row_number() == 1,
        sprintf("%.3f", nvalue1),
        ""
      )
    ) %>%
    ungroup()

  # ===========================================================================
  # STEP 7: Report Output — RTF Table
  # ===========================================================================
  # Replaces SAS lines 154-187:
  #   options pageno=1 ... orientation=landscape ...
  #   title1 font="Courier New" height=8pt "Vital Signs";
  #   title2 font="Courier New" height=8pt "Treatment-Emergent ...";
  #   proc report data=table_vals nowd ...
  #     columns param tefl trtpn totn nc pctc nvalue1c;
  #     define param/"Vital Sign|(unit)" ...;
  #     define tefl/"Abnormality|Direction" ...;
  #     define trtpn/"Treatment" ...;
  #     define totn/"N" ...;
  #     define nc/"n" ...;
  #     define pctc/"(%)" ...;
  #     define nvalue1c/"P value*" ...;
  #     compute after _page_; line @2 "Abbreviations: ..."; endcomp;
  #   run;
  #
  # Column widths from SAS DEFINE cellwidth values (inches):
  #   param: 2.75, tefl: 1.25, trtpn: 1.75, totn: 0.75, nc: 0.6, pctc: 0.6,
  #   nvalue1c: 1.0
  # ---------------------------------------------------------------------------

  # Prepare the table data for RTF output
  report_data <- table_vals %>%
    select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c)

  # Column headers matching SAS DEFINE labels
  # SAS uses "|" as split character for multi-line headers
  col_headers <- c(
    "Vital Sign\n(unit)",
    "Abnormality\nDirection",
    "Treatment",
    "N",
    "n",
    "(%)",
    "P value*"
  )

  # Footnotes matching SAS COMPUTE AFTER _PAGE_ block (lines 181-186)
  # NOTE: SAS source line 183 has typo "wtih" -> preserved as-is for parity
  footnotes <- paste(
    "Abbreviations:  N = number of patients with a normal",
    "(i.e., not low if calculating `low` and not high if calculating `high`)",
    "baseline and at least one post-baseline measure,",
    "n = number of patients wtih an abnormal post-baseline result",
    "in the specified category.\n*P values are from Fisher's Exact test."
  )

  # Convert all columns to character for consistent RTF rendering
  report_data <- report_data %>%
    mutate(
      TRTPN = as.character(TRTPN),
      totn  = as.character(totn)
    )

  # Build RTF output pipeline
  # r2rtf pipeline: rtf_body -> rtf_title -> rtf_footnote -> rtf_page -> rtf_encode -> write_rtf
  rtf_output <- report_data %>%
    rtf_body(
      col_rel_width = c(2.75, 1.25, 1.75, 0.75, 0.6, 0.6, 1.0),
      text_font     = 9,        # Courier New (r2rtf font code 9)
      text_font_size = 8        # 8pt matching SAS style(column)=[fontsize=8pt]
    ) %>%
    rtf_title(
      title = "Vital Signs",
      subtitle = "Treatment-Emergent Abnormal High or Low at Any Time",
      text_font = 9,            # Courier New for titles
      text_font_size = 8        # 8pt matching SAS title font size
    ) %>%
    rtf_footnote(
      footnotes,
      text_font = 9,            # Courier New for footnotes
      text_font_size = 8
    ) %>%
    rtf_page(
      orientation = "landscape",
      margin = c(1, 1, 1, 1, 1, 1)  # top, bottom, left, right, header, footer
    ) %>%
    rtf_encode()                # Encode to RTF format before writing

  # Write RTF file
  rtf_file <- file.path(output_path, "outliers_TEHiLo_Anytime.rtf")
  write_rtf(rtf_output, file = rtf_file)

  message("RTF output written to: ", rtf_file)

  # Return the formatted table data invisibly
  invisible(table_vals)
}


# =============================================================================
#### MIGRATION NOTES
# =============================================================================
#### ASSUMPTIONS:
####    1. SAS %fisherpval macro with `if last.usubjid` deduplication is
####       replicated via arrange(USUBJID, flag_var) %>% group_by(USUBJID) %>%
####       slice_tail(n = 1). This picks the last record per subject after
####       sorting by the flag variable, matching SAS last. semantics.
####    2. SAS `inlib.advs` reference (source line 21) is assumed to be
####       equivalent to `source.advs` (the XPT library defined at line 19).
####       This appears to be a copy error from another program.
####    3. CRIT1FL maps to "Low" direction, CRIT2FL maps to "High" direction
####       per the SAS code logic (lines 28-65). Note that the SAS annotations
####       (lines 7-12) appear to have these reversed — the code is authoritative.
####    4. Fisher's exact p-value (XP2_FISH) is the two-tailed exact p-value,
####       which is the default returned by R's fisher.test().
####    5. P-value is displayed only for the first treatment within each
####       direction, matching SAS `if first.nvalue1` logic (line 150).
####    6. SAS merge at line 86 uses BY trtpn param (without tefl). R migration
####       uses left_join by TRTPN, PARAM, and tefl for correctness when
####       count datasets have unbalanced rows per direction.
####    7. When no subjects have the flag = "Y" for a treatment, n is set to 0
####       via coalesce() rather than SAS missing. This is clinically appropriate
####       (zero events) vs SAS artifact (merge produces missing n).
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. Fisher's exact: R fisher.test() vs SAS PROC FREQ EXACT may produce
####       slightly different p-values for large tables due to algorithm
####       differences. SAS uses a network algorithm; R uses direct enumeration
####       or hybrid methods. Differences are typically < 1e-10.
####    2. Percentage rounding: janitor::round_half_up() matches SAS
####       round-half-up behavior. R default round() uses half-to-even
####       (banker's rounding) which was NOT used here.
####    3. Subject deduplication: SAS BY-group `last.` relies on sort order;
####       R slice_tail after arrange should be equivalent but verify sort
####       stability for subjects with identical flag values.
####    4. SAS pvalue5.3 format omits leading zero (".050") while R sprintf
####       includes it ("0.050"). This is a cosmetic difference.
#### NO DIRECT R EQUIVALENT:
####    1. SAS PROC REPORT with COMPUTE AFTER _PAGE_ for page-level footnotes
####       -> r2rtf rtf_footnote() (footnotes appear at document level, not
####       per-page in the same manner).
####    2. SAS ODS output FishersExact with Name1="XP2_FISH" -> fisher.test()$p.value
####       (direct extraction rather than ODS dataset parsing).
####    3. SAS PROC REPORT style(report)/style(column)/style(header) inline
####       CSS-like styling -> r2rtf formatting parameters (text_font,
####       text_font_size, col_rel_width).
####    4. SAS `pvalue5.3` format -> sprintf("%.3f", ...) with leading zero.
####       SAS format omits leading zero for values < 1; R includes it.
#### PACKAGE SELECTION RATIONALE:
####    haven:   SAS XPT file I/O (tidyverse standard, CRAN 2.5.5)
####    dplyr:   Data manipulation replacing DATA steps and PROC SQL
####    tidyr:   Loaded per migration framework for data reshaping availability
####    Tplyr:   Loaded per migration framework for clinical table grammar
####    r2rtf:   RTF output replacing PROC REPORT with ODS (Merck, CRAN 1.1.1+)
####    janitor: round_half_up() for SAS-compatible rounding (CRAN 2.2.0+)
####    stringr: str_pad() for format-width string alignment (CRAN 1.5.0+)
####    stats:   fisher.test() for Fisher's exact test (R base)
####    tibble:  tibble() for structured result data frames (CRAN 3.2.0+)
#### OPEN QUESTIONS:
####    1. Verify that SAS `count(unique(usubjid))` in PROC SQL behaves
####       identically to R n_distinct(USUBJID) for subjects appearing in
####       multiple records with different flag values.
####    2. Confirm Fisher's exact test specification — does SAS PROC FREQ
####       EXACT use mid-p correction? R fisher.test() does not by default.
####    3. Verify font specification (Courier New 8pt) matches SAS output
####       appearance in the generated RTF file.
####    4. Confirm `inlib.advs` vs `source.advs` — which libname is correct
####       in the original SAS? The URL-based 'source' libname is assumed.
####    5. SAS source line 183 contains typo "wtih" (should be "with") —
####       preserved as-is for output parity with original SAS.
# =============================================================================
