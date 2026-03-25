# =============================================================================
# Subject Disposition Table (table7_2b)
# =============================================================================
#
# MIGRATION SOURCE: whitepapers/scriptathons/demographics/disposition_b2.sas
# ORIGINAL NAME:    table7_2b.sas
# AUTHOR:           ddabhi
# DATE CREATED:     20141012 (SAS original)
# MIGRATED TO R:    2024 (SAS-to-R migration)
#
# PURPOSE:
#   Generate a Subject Disposition table from ADSL data. Reads ADSL transport
#   file, filters for Intent-to-Treat (ITT) population, creates a Total column
#   by row duplication, computes Completed / Discontinued / all-reason counts
#   and percentages by treatment arm, formats output with indented sub-reason
#   labels, and produces an RTF file via r2rtf.
#
# REVISION HISTORY:
#   2019-12-23 - SAS source: updated path as data has been moved
#   2024       - Migrated to R using haven, dplyr, tidyr, r2rtf, stringr, janitor
#
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(rlang)
library(r2rtf)
library(stringr)
library(janitor)
library(cli)

# =============================================================================
# Helper Function: disp_count
# =============================================================================
# Replaces SAS %disp macro (SAS lines 69-77).
#
# The original SAS macro:
#   %macro disp(outdata=, where=, var=);
#     proc sql;
#       create table &outdata as
#         select count(distinct usubjid) as cnt,
#                (calculated cnt*100)/tot as per,
#                &var as col1, a.trt01an, trt01a length=50
#           from adsl1 as a, tot as b
#             where &where and a.trt01an = b.trt01an
#               group by a.trt01an, trt01a, tot, col1;
#     quit;
#   %mend disp;
#
# R replacement uses function arguments instead of macro text substitution.
#
# Parameters:
#   data      - Input data frame (filtered ADSL with total arm appended)
#   tot_data  - Treatment arm total counts tibble (TRT01AN, tot)
#   where_fn  - A function(df) returning a filtered data frame
#               Replaces SAS WHERE clause resolved at macro expansion time
#   var_col   - Either a fixed character string for col1 label, or NULL to use
#               DCREASCD column values as labels (for all-reasons breakdown)
#
# Returns: tibble with columns (col1, TRT01AN, TRT01A, cnt, per)
# =============================================================================
disp_count <- function(data, tot_data, where_fn, var_col = NULL) {

  # Apply the WHERE filter (replaces SAS macro WHERE resolution)
  filtered <- where_fn(data)

  if (!is.null(var_col)) {
    # Fixed label mode: e.g. var=%str("Completed the study")
    # Group by treatment arm and count distinct subjects
    result <- filtered %>%
      dplyr::group_by(TRT01AN, TRT01A) %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::mutate(col1 = var_col)
  } else {
    # Variable label mode: var=%str(dcreascd) — use DCREASCD value as label
    # Clean DCREASCD values by collapsing irregular whitespace
    result <- filtered %>%
      dplyr::mutate(DCREASCD = stringr::str_squish(as.character(DCREASCD))) %>%
      dplyr::group_by(TRT01AN, TRT01A, DCREASCD) %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::mutate(col1 = DCREASCD) %>%
      dplyr::select(-DCREASCD)
  }

  # Join with totals and compute percentage
  # SAS: (calculated cnt*100)/tot
  result <- result %>%
    dplyr::left_join(tot_data, by = "TRT01AN") %>%
    dplyr::mutate(
      per = dplyr::if_else(tot > 0L, cnt * 100 / tot, NA_real_)
    ) %>%
    dplyr::select(col1, TRT01AN, TRT01A, cnt, per)

  result
}

# =============================================================================
# Main Function: generate_disposition_b2
# =============================================================================
#' Generate Subject Disposition Table (Table 7.2b)
#'
#' Reads ADSL transport data, filters for ITT population, computes disposition
#' counts (Completed, Discontinued, all discontinuation reasons) by treatment
#' arm with a Total column, formats count/percentage strings, and produces an
#' RTF table via r2rtf. Completes the workflow that was left incomplete in the
#' original SAS source (PROC TRANSPOSE commented out).
#'
#' @param data_path Character string. Path to directory containing adsl.xpt.
#'   Replaces SAS: filename source url "https://..."; libname source xport;
#'   Load via config: config$data_paths$adam_path
#' @param output_path Character string. Full path (including filename) for RTF
#'   output. Defaults to "disposition_b2.rtf". Replaces SAS ODS RTF destination.
#'   Load via config: file.path(config$output_paths$rtf_output_path, "disposition_b2.rtf")
#'
#' @return Invisibly returns the final wide-format disposition tibble.
#'   Side effect: writes an RTF file to output_path.
#'
#' @examples
#' \dontrun{
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   generate_disposition_b2(
#'     data_path   = config$data_paths$adam_path,
#'     output_path = file.path(config$output_paths$rtf_output_path,
#'                             "disposition_b2.rtf")
#'   )
#' }
#' @export
generate_disposition_b2 <- function(data_path, output_path = "disposition_b2.rtf") {

  # ===========================================================================
  # Phase 1: Data Reading and Filtering (SAS lines 21-27)
  # ===========================================================================
  # SAS: filename source url "https://raw.githubusercontent.com/.../adsl.xpt";
  #      libname source xport;
  #      data adsl(keep = usubjid trt01an trt01a dcreascd ittfl);
  #        set source.adsl;
  #        where ittfl = "Y";
  #      run;

  adsl_path <- file.path(data_path, "adsl.xpt")
  if (!file.exists(adsl_path)) {
    cli::cli_abort(
      "ADSL transport file not found: ", adsl_path,
      "\nEnsure data_path points to a directory containing adsl.xpt.",
      call. = FALSE
    )
  }

  adsl_raw <- haven::read_xpt(adsl_path)

  # Normalize column names to uppercase for CDISC-consistent access
  names(adsl_raw) <- toupper(names(adsl_raw))

  # Validate required variables exist
  required_vars <- c("USUBJID", "TRT01AN", "TRT01A", "DCREASCD", "ITTFL")
  missing_vars <- setdiff(required_vars, names(adsl_raw))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      "Required variables missing from ADSL: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Keep required variables and apply ITT population filter (SAS line 26)
  adsl <- adsl_raw %>%
    dplyr::select(USUBJID, TRT01AN, TRT01A, DCREASCD, ITTFL) %>%
    dplyr::filter(ITTFL == "Y")

  if (nrow(adsl) == 0L) {
    cli::cli_abort("No subjects remain after filtering for ITTFL == 'Y'.", call. = FALSE)
  }

  # ===========================================================================
  # Phase 2: Create Total Column (SAS lines 29-36)
  # ===========================================================================
  # SAS: data adsl1;
  #        set adsl;
  #        output;             <- output original row
  #        trt01an = 999;
  #        trt01a = "Total";
  #        output;             <- output duplicated row with Total arm
  #      run;
  #
  # R equivalent: bind original rows with mutated copy using bind_rows()
  # (SAS DATA step output;output; row duplication pattern)

  adsl_total <- adsl %>%
    dplyr::mutate(TRT01AN = 999L, TRT01A = "Total")

  adsl1 <- dplyr::bind_rows(adsl, adsl_total)

  # ===========================================================================
  # Phase 3: Header N Counts (SAS lines 39-44)
  # ===========================================================================
  # SAS: proc sql noprint;
  #        create table tot as select trt01an, count(distinct usubjid) as tot
  #          from adsl1 group by trt01an order by trt01an;
  #        select count(distinct trt01an) into :ntrt from adsl1;
  #        select count(distinct usubjid) into :trt1-:trt%cmpres(&ntrt)
  #          from adsl1 group by trt01an order by trt01an;
  #        select distinct trt01a into :trtlbl1-:trtlbl%cmpres(&ntrt)
  #          from adsl1 group by trt01a order by trt01a;
  #      quit;

  # Per-treatment total subject counts
  tot <- adsl1 %>%
    dplyr::distinct(USUBJID, TRT01AN) %>%
    dplyr::count(TRT01AN, name = "tot") %>%
    dplyr::arrange(TRT01AN)

  # Number of distinct treatment arms (SAS macro variable :ntrt)
  ntrt <- dplyr::n_distinct(adsl1$TRT01AN)

  # Treatment arm metadata — ordered consistently by TRT01AN
  # (SAS source line 43 orders labels by trt01a alphabetically, which could
  #  mismatch line 42 ordering by trt01an — R version uses consistent ordering)
  trt_info <- adsl1 %>%
    dplyr::distinct(TRT01AN, TRT01A) %>%
    dplyr::arrange(TRT01AN)

  # Per-treatment N values (SAS macro variables :trt1 through :trtN)
  trt_ns <- tot$tot
  names(trt_ns) <- trt_info$TRT01A

  # Treatment labels (SAS macro variables :trtlbl1 through :trtlblN)
  trt_labels <- trt_info$TRT01A

  # ===========================================================================
  # Phase 4: Dummy Row Template (SAS lines 46-64)
  # ===========================================================================
  # NOTE: Original SAS misspellings are preserved for exact parity with
  #       SAS output:
  #         "Protocal Violation" (should be "Protocol Violation")
  #         "Withdrawl by Subject" (should be "Withdrawal by Subject")
  #         "Withdrawl by Parent/Guardian" (should be "Withdrawal by Parent/Guardian")

  dummy <- dplyr::tibble(
    col1 = c(
      "Completed the study",                 # ord = 1
      "Discontinued",                         # ord = 2
      " ",                                    # ord = 3, visual separator row
      "  Death",                              # ord = 4
      "  Adverse Event",                      # ord = 5
      "  Lack of Efficacy",                   # ord = 6
      "  Lost to Follow-up",                  # ord = 7
      "  Non-compliance with Study Drug",     # ord = 8
      "  Pregnancy",                          # ord = 9
      "  Protocal Violation",                 # ord = 10 (SAS typo preserved)
      "  Physician Decision",                 # ord = 11
      "  Withdrawl by Subject",              # ord = 12 (SAS typo preserved)
      "  Withdrawl by Parent/Guardian",      # ord = 13 (SAS typo preserved)
      "  Recovery",                           # ord = 14
      "  Technical Problems",                 # ord = 15
      "  Other"                               # ord = 16
    ),
    ord = 1:16
  )

  # ===========================================================================
  # Phase 5: Compute Disposition Counts (SAS lines 78-80 via %disp macro)
  # ===========================================================================

  # Call 1: Completed subjects (SAS line 78)
  # %disp(outdata=comp, where=%str(upcase(dcreascd)="COMPLETED"),
  #        var=%str("Completed the study"))
  comp <- disp_count(
    data     = adsl1,
    tot_data = tot,
    where_fn = function(df) dplyr::filter(df, toupper(DCREASCD) == "COMPLETED"),
    var_col  = "Completed the study"
  )

  # Call 2: Discontinued subjects — aggregate (SAS line 79)
  # %disp(outdata=disc, where=%str(upcase(dcreascd)^="COMPLETED"),
  #        var=%str("Discontinued"))
  disc <- disp_count(
    data     = adsl1,
    tot_data = tot,
    where_fn = function(df) dplyr::filter(df, toupper(DCREASCD) != "COMPLETED"),
    var_col  = "Discontinued"
  )

  # Call 3: All discontinuation reasons — detail breakdown (SAS line 80)
  # %disp(outdata=all, where=%str(upcase(dcreascd)^="COMPLETED"),
  #        var=%str(dcreascd))
  all_reasons <- disp_count(
    data     = adsl1,
    tot_data = tot,
    where_fn = function(df) dplyr::filter(df, toupper(DCREASCD) != "COMPLETED"),
    var_col  = NULL
  )

  # ===========================================================================
  # Phase 6: Stack and Format (SAS lines 82-88)
  # ===========================================================================
  # SAS: data disp;
  #        set comp disc all(in = a);
  #        by trt01an trt01a;
  #        if a then col1 = "  " || trim(left(col1));  <- indent sub-reasons
  #        nper = put(cnt, 3.) || "(" || put(per, 5.1) || ")";
  #      run;

  # Indent sub-reason labels: SAS line 85: "  " || trim(left(col1))
  all_reasons_indented <- all_reasons %>%
    dplyr::mutate(col1 = paste0("  ", stringr::str_trim(col1)))

  # Stack all disposition rows (SAS: set comp disc all)
  disp <- dplyr::bind_rows(comp, disc, all_reasons_indented)

  # Format count/percent string matching SAS:
  #   nper = put(cnt, 3.) || "(" || put(per, 5.1) || ")"
  # put(cnt, 3.)  → right-justified 3-character integer
  # put(per, 5.1) → right-justified 5-character with 1 decimal
  # janitor::round_half_up() ensures SAS-compatible rounding (AAP §0.7.2)
  disp <- disp %>%
    dplyr::mutate(
      per_rounded = janitor::round_half_up(per, digits = 1),
      nper = dplyr::if_else(
        !is.na(cnt),
        paste0(
          stringr::str_pad(as.character(cnt), width = 3, side = "left"),
          "(",
          format(as.numeric(per_rounded), nsmall = 1, width = 5),
          ")"
        ),
        NA_character_
      )
    )

  # ===========================================================================
  # Phase 7: Pivot to Wide Format (SAS lines 91-94: PROC TRANSPOSE commented out)
  # ===========================================================================
  # NOTE: SAS source has PROC TRANSPOSE commented out (lines 91-94):
  #   /* proc transpose data = disp out = disp_t; by run; */
  # R implementation completes the intended wide-format workflow using
  # tidyr::pivot_wider().

  disp_wide <- disp %>%
    dplyr::select(col1, TRT01A, nper) %>%
    tidyr::pivot_wider(
      names_from  = TRT01A,
      values_from = nper,
      values_fill = ""
    )

  # Merge with dummy template for proper ordering and inclusion of all
  # disposition categories (including blank separator row and categories
  # with zero subjects)
  final_table <- dummy %>%
    dplyr::left_join(disp_wide, by = "col1") %>%
    dplyr::arrange(ord) %>%
    dplyr::select(-ord)

  # Replace NA values with empty string for clean RTF display
  # (SAS numeric missing → NA; display as blank for zero-count categories)
  final_table[is.na(final_table)] <- ""

  # Ensure treatment arm columns are ordered by TRT01AN (not alphabetical)
  col_order <- c("col1", trt_labels)
  col_order <- col_order[col_order %in% names(final_table)]
  final_table <- final_table %>%
    dplyr::select(dplyr::all_of(col_order))

  # ===========================================================================
  # Phase 8: RTF Output via r2rtf
  # ===========================================================================
  # Build dynamic column headers with per-arm N counts
  header_labels <- paste0(trt_labels, "\n(N=", trt_ns, ")")
  col_header_str <- paste(
    c("Disposition Category", header_labels),
    collapse = " | "
  )

  # Column widths: wider first column for label text, equal for treatment arms
  n_trt_cols <- length(trt_labels)
  col_widths <- c(3.5, rep(1.5, n_trt_cols))

  # Justification: left for disposition categories, center for counts
  col_just <- c("l", rep("c", n_trt_cols))

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (nchar(output_dir) > 0L && output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Generate RTF output using r2rtf pipeline
  # Pipeline: rtf_body -> rtf_colheader -> rtf_title -> rtf_encode -> write_rtf
  final_table %>%
    r2rtf::rtf_body(
      col_rel_width      = col_widths,
      text_justification = col_just
    ) %>%
    r2rtf::rtf_colheader(
      colheader          = col_header_str,
      col_rel_width      = col_widths,
      text_justification = col_just
    ) %>%
    r2rtf::rtf_title(title = "Subject Disposition") %>%
    r2rtf::rtf_encode() %>%
    r2rtf::write_rtf(file = output_path)

  message("RTF output written to: ", output_path)

  # Return the final table invisibly for programmatic access
  invisible(final_table)
}

# =============================================================================
# MIGRATION NOTES
# =============================================================================
#
# ASSUMPTIONS:
#   1. ADSL XPT file is located at file.path(data_path, "adsl.xpt")
#   2. ITT population identified by ITTFL == "Y"
#   3. DCREASCD values in data match the dummy template categories
#      (after uppercasing for filter, raw value for labels) — category
#      labels are case-sensitive for display matching with dummy template
#   4. Total column generated by row duplication with TRT01AN = 999,
#      matching SAS DATA step output; output; pattern (SAS lines 30-36)
#   5. Column variable names in ADSL are uppercase per CDISC convention
#      (haven::read_xpt preserves case; names normalized to upper)
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. SAS percent formatting put(per, 5.1) uses round-half-up natively;
#      R version uses janitor::round_half_up() for equivalence
#   2. SAS count(distinct usubjid) vs R n_distinct(USUBJID) — both count
#      unique non-missing values; semantically identical
#   3. SAS PROC SQL implicit Cartesian join with WHERE clause vs R
#      left_join — join results are equivalent given unique TRT01AN keys
#      in the tot table
#   4. Sort stability: SAS sort is guaranteed stable by key; R arrange()
#      is stable within groups — verified for multi-key sorts here
#
# NO DIRECT R EQUIVALENT:
#   1. SAS %disp macro with dynamic WHERE and VAR resolved at macro
#      expansion time -> R function disp_count() with function arguments
#      for filtering and character/NULL flag for label mode
#   2. SAS DATA step "output; output;" row duplication -> dplyr::bind_rows()
#      of original rows and mutated copy with TRT01AN=999
#   3. SAS INDENT labels with leading spaces -> preserved as literal leading
#      spaces in character column for r2rtf rendering
#   4. SAS PROC TRANSPOSE was commented out in source (lines 91-94) —
#      R implementation completes the intended workflow using
#      tidyr::pivot_wider() for long-to-wide reshaping
#
# PACKAGE SELECTION RATIONALE:
#   - haven:   SAS XPT transport file reader (read_xpt), CDISC standard I/O
#   - dplyr:   Core data manipulation replacing DATA steps and PROC SQL
#   - tidyr:   pivot_wider() for long-to-wide reshaping (completing the
#              commented-out SAS PROC TRANSPOSE)
#   - r2rtf:   Production-grade RTF output replacing SAS ODS RTF destination
#   - stringr: Tidyverse string manipulation — str_pad (SAS put format),
#              str_trim (SAS trim/left), str_squish (whitespace normalization)
#   - janitor: round_half_up() for SAS-compatible rounding per AAP section 0.7.2
#
# OPEN QUESTIONS:
#   1. SAS source appears incomplete — PROC TRANSPOSE commented out (lines
#      91-94). R version implements full RTF output workflow as intended.
#   2. Verify DCREASCD category values in actual data match the dummy
#      template entries (including original misspellings: "Protocal Violation",
#      "Withdrawl by Subject", "Withdrawl by Parent/Guardian").
#   3. Confirm formatting width (3 chars for count) is sufficient for the
#      actual data range — if subject count exceeds 999, width should increase.
#   4. SAS source line 43 orders treatment labels by trt01a (alphabetical)
#      while line 42 orders counts by trt01an (numeric) — potential mismatch
#      in original SAS. R version consistently orders by TRT01AN throughout.
#   5. Blank separator row (ord=3, col1=" ") is a display-only element that
#      separates "Discontinued" from the sub-reason breakdown. Confirm this
#      visual convention is required for the final output.
#
# =============================================================================
