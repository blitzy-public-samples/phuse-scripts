# ==============================================================================
# Reference R program for target 31 — Subject Disposition
# ==============================================================================
#
# PROGRAM NAME : disposition_b1.R
# DESCRIPTION  : Migrated from whitepapers/scriptathons/demographics/disposition_b1.sas
#                (originally named target31.sas)
#                Generates the Target 31 Subject Disposition table:
#                  - Reads ADSL XPT transport data
#                  - Filters to ITT population (ITTFL = 'Y')
#                  - Counts Completed vs Discontinued subjects by treatment arm
#                  - Counts specific discontinuation reasons by treatment arm
#                  - Formats count/percentage cells with SAS-compatible rounding
#                  - Transposes by treatment arm
#                  - Renders RTF output via r2rtf (replacing SAS ODS + PROC REPORT)
#
# INPUT        : ADSL.xpt (ADaM Subject-Level Analysis Dataset)
#                Dataset Variables: USUBJID, TRT01P, TRT01PN, DCREASCD, ITTFL
#                Record Selection: WHERE ITTFL = 'Y' (for disposition counts)
#
# OUTPUT       : Subject Disposition RTF table
#
# SAS SOURCE   : whitepapers/scriptathons/demographics/disposition_b1.sas
#                SAS V9, original CODE NAME = target31.sas
#
# DEPENDENCIES : haven (>=2.5.5), dplyr (>=1.1.0), tidyr (>=1.3.0),
#                rlang (>=1.1.0), r2rtf (>=1.1.1), stringr (>=1.5.0),
#                janitor (>=2.2.0)
#
# ==============================================================================

# --- Library Loading ----------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(rlang)       # Required: r2rtf 1.3.0 uses rlang's %||% operator internally
library(r2rtf)
library(stringr)
library(janitor)
library(cli)

# ==============================================================================
# generate_disposition_b1
# ==============================================================================
#' Generate Subject Disposition Table (Target 31)
#'
#' Produces an RTF table showing subject disposition counts and percentages
#' by treatment arm. Subjects are classified as Completed or Discontinued,
#' with specific discontinuation reasons listed as subcategories.
#'
#' @param data_path Character string. Path to directory containing adsl.xpt.
#'   Replaces SAS filename/libname xport pattern (SAS lines 20-21).
#' @param output_path Character string. Path for the output RTF file.
#'   Defaults to "disposition_b1.rtf".
#'
#' @return Invisible character string of the output file path.
#'
#' @details
#' The function replicates the SAS Target 31 Subject Disposition table:
#' \itemize{
#'   \item Reads ADSL from XPT transport file
#'   \item Filters to Intent-to-Treat population (ITTFL = 'Y') for counts
#'   \item Counts Completed and Discontinued subjects per treatment arm
#'   \item Counts individual discontinuation reasons per treatment arm
#'   \item Uses total ADSL subjects per arm as denominator for percentages
#'   \item Formats cells as "n (xx.x%)" with SAS-compatible round-half-up
#'   \item Transposes from long to wide by treatment arm
#'   \item Outputs RTF table with title, dynamic headers, and footnotes
#' }
#'
#' @examples
#' \dontrun{
#'   # Using config:
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   generate_disposition_b1(
#'     data_path = config$data_paths$adam_path,
#'     output_path = file.path(config$output_paths$rtf_output_path,
#'                             "disposition_b1.rtf")
#'   )
#' }
#'
generate_disposition_b1 <- function(data_path, output_path = "disposition_b1.rtf") {

  # ============================================================================
  # Phase 1: Data Reading
  # Replaces SAS lines 20-25:
  #   filename source url "https://raw.githubusercontent.com/.../adsl.xpt";
  #   libname source xport;
  #   data work.adsl; set source.adsl; run;
  # ============================================================================
  adsl_path <- file.path(data_path, "adsl.xpt")

  if (!file.exists(adsl_path)) {
    cli::cli_abort(
      "ADSL transport file not found at: ", adsl_path,
      "\nEnsure data_path points to the directory containing adsl.xpt.",
      call. = FALSE
    )
  }

  adsl <- haven::read_xpt(adsl_path)

  # Validate required variables exist
  required_vars <- c("USUBJID", "TRT01P", "TRT01PN", "DCREASCD", "ITTFL")
  missing_vars <- setdiff(required_vars, names(adsl))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      "Required variables missing from ADSL: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # ============================================================================
  # Phase 2: Disposition Counts (replaces SAS PROC SQL, lines 29-53)
  # ============================================================================

  # --- Part 1: Completed vs Discontinued (SAS lines 30-37) ---
  # SAS: SELECT DISTINCT TRT01P, TRT01PN, COUNT(USUBJID) as cnt, 1 as ord,
  #        CASE WHEN DCREASCD='Completed' THEN DCREASCD
  #             ELSE 'Discontinued' END as status
  #      FROM adsl WHERE ITTFL='Y'
  #      GROUP BY TRT01P, calculated status
  completed_disc <- adsl %>%
    dplyr::filter(ITTFL == "Y") %>%
    dplyr::mutate(
      status = dplyr::if_else(
        DCREASCD == "Completed", "Completed", "Discontinued"
      )
    ) %>%
    dplyr::count(TRT01P, TRT01PN, status, name = "cnt") %>%
    dplyr::mutate(ord = 1L)

  # --- Part 2: Specific Discontinuation Reasons (SAS lines 38-43) ---
  # SAS: SELECT DISTINCT TRT01P, TRT01PN, COUNT(USUBJID) as cnt, 2 as ord,
  #        DCREASCD as status
  #      FROM adsl WHERE ITTFL='Y' AND DCREASCD^='Completed'
  #      GROUP BY TRT01P, DCREASCD
  disc_reasons <- adsl %>%
    dplyr::filter(ITTFL == "Y", DCREASCD != "Completed") %>%
    dplyr::count(TRT01P, TRT01PN, DCREASCD, name = "cnt") %>%
    dplyr::rename(status = DCREASCD) %>%
    dplyr::mutate(ord = 2L)

  # --- Stack and Sort (SAS UNION ALL + ORDER BY, lines 38, 44) ---
  disp <- dplyr::bind_rows(completed_disc, disc_reasons) %>%
    dplyr::arrange(TRT01PN, ord, status)

  # --- Frequency Totals (SAS lines 47-52) ---
  # Note: SAS counts ALL subjects per arm (no ITTFL filter) as denominator.
  # SAS: SELECT DISTINCT TRT01PN, COUNT(USUBJID) as tot_cnt FROM adsl
  #      GROUP BY TRT01PN ORDER BY TRT01PN
  freq <- adsl %>%
    dplyr::count(TRT01PN, name = "tot_cnt") %>%
    dplyr::arrange(TRT01PN)

  # ============================================================================
  # Phase 3: Format Count/Percentage Cells (replaces SAS DATA step, lines 57-62)
  # ============================================================================
  # SAS: merge disp freq; by TRT01PN;
  #      treat = strip(TRT01P) || " (N=" || strip(tot_cnt) || ")";
  #      cnt_prcnt = strip(cnt) || " (" || strip(put(cnt/tot_cnt, percent8.1)) || ")";
  disp_2 <- disp %>%
    dplyr::left_join(freq, by = "TRT01PN") %>%
    dplyr::mutate(
      # Treatment header label: "Arm Name (N=XX)"
      treat = paste0(
        stringr::str_trim(TRT01P),
        " (N=",
        stringr::str_trim(as.character(tot_cnt)),
        ")"
      ),
      # Count/percentage cell: "n (xx.x%)"
      # SAS percent8.1 format: fraction -> percentage with 1 decimal + % sign
      # janitor::round_half_up() ensures SAS-compatible rounding (AAP §0.7.2)
      pct_value = janitor::round_half_up(100 * cnt / tot_cnt, digits = 1),
      cnt_prcnt = paste0(
        stringr::str_trim(as.character(cnt)),
        " (",
        stringr::str_trim(sprintf("%.1f%%", pct_value)),
        ")"
      )
    ) %>%
    dplyr::select(-pct_value)

  # ============================================================================
  # Phase 4: Transpose (replaces SAS PROC SORT + PROC TRANSPOSE, lines 64-73)
  # ============================================================================

  # Sort by status, ord (SAS line 64: proc sort data=disp_2; by status ord;)
  disp_sorted <- disp_2 %>%
    dplyr::arrange(status, ord)

  # Extract treatment headers for r2rtf column headers
  # SAS IDLABEL treat (line 69) -> these become the column labels in PROC REPORT
  treat_headers <- disp_sorted %>%
    dplyr::select(TRT01PN, treat) %>%
    dplyr::distinct() %>%
    dplyr::arrange(TRT01PN)

  # Transpose: PROC TRANSPOSE with prefix=tr, by status ord, id TRT01PN
  # (SAS lines 66-71)
  wide <- disp_sorted %>%
    dplyr::select(status, ord, TRT01PN, cnt_prcnt) %>%
    tidyr::pivot_wider(
      names_from  = TRT01PN,
      values_from = cnt_prcnt,
      names_prefix = "tr"
    )

  # Final sort by ord, status (SAS line 73: proc sort data=trans_2; by ord status;)
  trans_2 <- wide %>%
    dplyr::arrange(ord, status)

  # ============================================================================
  # Phase 5: Prepare RTF Output Data
  # ============================================================================

  # Insert blank row between ord groups to replicate SAS "break after ord/skip"
  # (SAS line 86)
  trt_col_names <- setdiff(names(trans_2), c("status", "ord"))
  ord_values <- sort(unique(trans_2$ord))

  output_parts <- list()
  for (i in seq_along(ord_values)) {
    current_group <- trans_2 %>%
      dplyr::filter(ord == ord_values[i])
    output_parts[[length(output_parts) + 1L]] <- current_group

    # Add blank separator row after each group except the last
    if (i < length(ord_values)) {
      blank_row <- dplyr::tibble(status = "", ord = NA_integer_)
      for (col_nm in trt_col_names) {
        blank_row[[col_nm]] <- NA_character_
      }
      output_parts[[length(output_parts) + 1L]] <- blank_row
    }
  }

  output_df <- dplyr::bind_rows(output_parts)

  # Remove ord column (SAS line 82: define ord / group order=data noprint)
  output_df <- output_df %>%
    dplyr::select(-ord)

  # Replace NA with empty string for RTF display
  # SAS PROC REPORT missing option renders missing values as blanks
  output_df <- output_df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.), "", .)
      )
    )

  # ============================================================================
  # Phase 6: RTF Output (replaces SAS TITLE + PROC REPORT, lines 77-93)
  # ============================================================================

  # Build dynamic column header string for r2rtf
  # SAS: define status / group "Subject Disposition" order=data;
  #      define tr: / display;  (uses IDLABEL as column header)
  col_header_parts <- c("Subject Disposition", treat_headers$treat)
  col_header <- paste(col_header_parts, collapse = " | ")

  # Column widths: status column wider, treatment columns equal

  n_trt_cols <- length(trt_col_names)
  col_widths <- c(3.0, rep(2.5, n_trt_cols))

  # Create output directory if it does not exist
  output_dir <- dirname(output_path)
  if (nchar(output_dir) > 0L && output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # RTF pipeline: title -> colheader -> body -> footnote -> encode -> write
  # SAS line 77: title1 font="Courier New" height=8pt "Subject Disposition";
  # SAS lines 79-92: proc report ... compute after _page_ ... endcomp; quit;
  # NOTE: rtf_encode() is required to convert annotated table to RTF markup
  #       before write_rtf() serialises to disk.
  output_df %>%
    rtf_body(
      col_rel_width  = col_widths,
      text_font      = 5L,
      text_font_size = 8
    ) %>%
    rtf_colheader(
      colheader      = col_header,
      col_rel_width  = col_widths,
      text_font      = 5L,
      text_font_size = 8
    ) %>%
    rtf_title(
      title          = "Subject Disposition",
      text_font      = 5L,
      text_font_size = 8
    ) %>%
    rtf_footnote(
      footnote = c(
        paste0(
          "Abbreviations:  N = number of subjects in the population; ",
          "n=number of subjects specified category"
        ),
        "% = Percentage of subjects with N as denominator"
      ),
      text_font      = 5L,
      text_font_size = 8
    ) %>%
    rtf_encode() %>%
    write_rtf(file = output_path)

  invisible(output_path)
}


# ==============================================================================
# MIGRATION NOTES
# ==============================================================================
#
# ASSUMPTIONS:
#   1. ADSL transport file (adsl.xpt) is located at data_path/adsl.xpt
#   2. ITT population is identified by ITTFL = 'Y' (SAS line 36)
#   3. DCREASCD contains disposition reason codes matching expected values
#      (e.g., "Completed" for study completers)
#   4. TRT01P (character) and TRT01PN (numeric) treatment variables are present
#      with a 1:1 mapping between them
#   5. Denominator for percentages is the total number of subjects per
#      treatment arm in the FULL ADSL (not restricted to ITT), matching the
#      SAS source (lines 47-52: no WHERE clause on freq query)
#   6. USUBJID is never missing in a valid CDISC ADSL dataset, so
#      dplyr::count() (which uses n()) produces the same result as
#      SAS COUNT(USUBJID) which counts non-missing values
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. SAS percent8.1 format rounding vs R sprintf with round_half_up —
#      Verified equivalent: janitor::round_half_up() implements round-half-up
#      matching SAS default behavior. R's base round() uses banker's rounding
#      which would differ at 0.5 boundaries.
#   2. SAS PROC SQL COUNT(DISTINCT) vs R n_distinct behavior if duplicate
#      USUBJID records exist — SAS source uses COUNT(USUBJID) (not COUNT
#      DISTINCT), which counts all non-missing rows. R count() counts all
#      rows, matching SAS behavior when USUBJID has no missing values.
#   3. Sort stability: SAS sort is guaranteed stable by key. R dplyr::arrange()
#      is also stable within groups. Multi-key sorts produce identical results
#      for this script's data structure.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS PROC SQL UNION ALL with CALCULATED keyword → dplyr bind_rows()
#      with pre-computed columns via mutate(). The CALCULATED keyword is a
#      SAS PROC SQL extension allowing reference to computed columns in the
#      same SELECT; in R, mutate() computes first, then group_by/count.
#   2. SAS PROC TRANSPOSE with IDLABEL → tidyr::pivot_wider() + manual
#      extraction of treatment headers into a separate tibble for r2rtf
#      column header construction.
#   3. SAS PROC REPORT COMPUTE AFTER _PAGE_ → r2rtf::rtf_footnote() places
#      footnote text at the bottom of each page, matching SAS behavior.
#   4. SAS PROC REPORT "break after ord / skip" → Blank rows manually
#      inserted between ord groups in the output data frame before rendering.
#
# PACKAGE SELECTION RATIONALE:
#   haven   — CDISC XPT transport file I/O (read_xpt), replacing SAS
#             filename/libname xport pattern
#   dplyr   — Core data manipulation (filter, mutate, count, arrange,
#             bind_rows, if_else, left_join, rename, select), replacing
#             SAS PROC SQL and DATA step operations
#   tidyr   — pivot_wider() for long-to-wide reshape, replacing SAS
#             PROC TRANSPOSE
#   r2rtf   — RTF table output (rtf_body, rtf_colheader, rtf_title,
#             rtf_footnote, write_rtf), replacing SAS ODS + PROC REPORT
#   stringr — str_trim() for SAS-compatible STRIP() function replacement,
#             providing tidyverse-idiomatic whitespace trimming per AAP rule
#             (tidyverse over base R)
#   janitor — round_half_up() for SAS-compatible round-half-up rounding
#             at all percentage formatting locations (AAP §0.7.2, §0.8.2)
#
# OPEN QUESTIONS:
#   1. Confirm whether TRT01PN values include all arms or only ITT-relevant
#      arms — the SAS source counts all ADSL subjects as the denominator
#      but only ITT subjects in the numerator.
#   2. Verify alphabetical sort of discontinuation reasons matches SAS
#      default collation — R uses locale-dependent sorting; SAS uses the
#      session encoding. For ASCII-range English values, results are identical.
#   3. The SAS source uses COUNT(USUBJID) which counts non-missing values;
#      R count() counts all rows. If any USUBJID values are missing in the
#      source data, counts would differ — this is unlikely for valid CDISC
#      datasets but should be verified against production data.
#
# ==============================================================================
