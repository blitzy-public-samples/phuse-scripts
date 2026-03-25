# =============================================================================
# Four-Category Baseline Shift Table — DIABP Shift Analysis
# =============================================================================
#
# Source:      whitepapers/scriptathons/outliers/outliers_Shift_Table.sas
# Migration:   SAS 9.4 -> R 4.3+ (PhUSE CS WG5 Standard Analyses)
# Date:        2026-03-25
#
# Description:
#   Computes and renders a 4-category shift table (Low / Normal / High / Total)
#   comparing baseline versus post-baseline DIABP normal-range indicator
#   results by treatment arm.  Produces a presentation-ready RTF table that
#   replicates the SAS PROC REPORT output including spanning column headers,
#   group-break spacing between treatments, and landscape orientation.
#
#   The pipeline covers:
#     1. Data loading from CDISC ADaM ADVS XPT transport file
#     2. PARAM enrichment (PARAM + title-case ATPT) and PARAMN derivation
#     3. Record-level filtering (PARAMCD, ANL01FL, AVISITN, SAFFL, non-missing
#        shift indicators)
#     4. Patient counting by baseline x post-baseline shift category with
#        marginal totals (replicating SAS PROC SUMMARY with CLASS)
#     5. Denominator extraction and zero-fill grid creation for all shift
#        category combinations
#     6. Percentage computation with SAS-compatible round-half-up rounding
#     7. Transposition to one record per baseline category with n/pct columns
#        for each post-baseline category
#     8. RTF output via r2rtf with proper spanning headers and landscape layout
#
# SAS Constructs Migrated:
#   PROC FORMAT value/invalue   -> Named R vectors (shift_levels, shift_labels)
#   PROC FORMAT CNTLIN          -> dplyr distinct() lookup
#   PROC SUMMARY with CLASS     -> dplyr group_by() + summarise() with explicit
#                                  marginal computation via bind_rows()
#   PROC SQL                    -> dplyr verbs (distinct, left_join)
#   DATA step do-loop zero-fill -> tidyr::expand_grid()
#   DATA step array/transpose   -> tidyr::pivot_wider()
#   PROC REPORT + ODS           -> r2rtf pipeline
#   SAS round(x, 0.1)          -> janitor::round_half_up(x, 1)
#
# =============================================================================

# ---------------------------------------------------------------------------
# Required packages
# ---------------------------------------------------------------------------
library(haven)      # XPT data I/O (replaces SAS libname xport)
library(dplyr)      # Data manipulation (replaces DATA steps, PROC SQL, PROC SUMMARY)
library(tidyr)      # Pivoting and reshaping (replaces DATA step array processing)
library(stringr)    # String formatting (replaces SAS PROPCASE/CATX)
library(rlang)      # Tidy evaluation primitives (provides %||% required by r2rtf)
# Tplyr not required by this script — removed per code review
library(r2rtf)      # RTF output (replaces ODS RTF + PROC REPORT)
library(janitor)    # round_half_up for SAS-compatible rounding
library(forcats)    # Factor level ordering (replaces SAS FORMAT ordering)
library(cli)

# =============================================================================
# outliers_shift_table
# =============================================================================
#' Compute and render a four-category baseline shift table
#'
#' Reads the CDISC ADaM ADVS dataset, filters to the requested parameter /
#' visit / population, computes patient counts by baseline x post-baseline
#' normal-range indicator shift category (Low / Normal / High / Total) with
#' denominator-based percentages, and writes a presentation-ready RTF file.
#'
#' @param data_path   Character. Directory containing \code{advs.xpt}.
#'   Replaces the SAS \code{filename source url ...} statement.
#' @param output_path Character. Directory where the output RTF is written.
#'   Replaces SAS ODS RTF destination.
#' @param paramcd     Character. CDISC PARAMCD filter value.
#'   Default \code{"DIABP"}.  Replaces \code{\%let invar} logic.
#' @param avisitn     Numeric.  Analysis visit number to select.
#'   Default \code{99} (end-of-study).
#' @param max_cat     Integer.  Number of shift categories including Total.
#'   Default \code{4} (Low, Normal, High, Total).
#'   Replaces \code{\%let maxcat = 4}.
#' @param trtvar      Character. Name of the numeric treatment variable in ADVS.
#'   Default \code{"TRTPN"}.  Replaces \code{\%let trtvar = trtpn}.
#'
#' @return A \code{data.frame} (invisibly) containing the final transposed
#'   shift table data used for the RTF output, suitable for downstream
#'   programmatic inspection or comparison.
#'
#' @examples
#' \dontrun{
#'   # Using config object:
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   outliers_shift_table(
#'     data_path   = config$data_paths$adam_path,
#'     output_path = config$output_paths$base_output_path
#'   )
#' }
outliers_shift_table <- function(data_path,
                                  output_path,
                                  paramcd  = "DIABP",
                                  avisitn  = 99,
                                  max_cat  = 4L,
                                  trtvar   = "TRTPN") {

  # -------------------------------------------------------------------------
  # 0. Input validation
  # -------------------------------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L || nchar(data_path) == 0L) {
    cli::cli_abort("`data_path` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
    cli::cli_abort("`output_path` must be a non-empty character string.", call. = FALSE)
  }
  xpt_file <- file.path(data_path, "advs.xpt")
  if (!file.exists(xpt_file)) {
    cli::cli_abort("ADVS transport file not found: ", xpt_file, call. = FALSE)
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------------------------------------------------------
  # 1. Shift category maps  (replaces SAS PROC FORMAT lines 20-34)
  # -------------------------------------------------------------------------
  #   value shifttx   1='Low' 2='Normal' 3='High' 4='Total';
  #   invalue shiftord 'Low'=1 'Normal'=2 'High'=3 'Total'=4;
  shift_levels <- c("Low" = 1L, "Normal" = 2L, "High" = 3L, "Total" = 4L)
  shift_labels <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")

  # -------------------------------------------------------------------------
  # 2. Read ADVS and enrich  (SAS lines 5-11)
  # -------------------------------------------------------------------------
  advs <- haven::read_xpt(xpt_file)

  # SAS line 9:  param = catx(' ', param, propcase(atpt));
  # SAS line 10: paramn = atptn;
  advs <- advs %>%
    dplyr::mutate(
      PARAM  = dplyr::if_else(
        is.na(ATPT) | nchar(ATPT) == 0L,
        PARAM,
        paste(PARAM, stringr::str_to_title(ATPT))
      ),
      PARAMN = ATPTN
    )

  # -------------------------------------------------------------------------
  # 3. Select variables and filter records  (SAS lines 13-40)
  # -------------------------------------------------------------------------
  # Variables to keep (SAS %let invar)
  keep_vars <- c("USUBJID", trtvar, "TRTP", "PARAM", "PARAMCD", "PARAMN",
                  "ATPT", "ATPTN", "AVAL", "AVISITN", "ANL01FL",
                  "BNRIND", "ANRIND", "SHIFT1", "CHGCAT1", "SAFFL")
  # Only keep columns that actually exist in the dataset

  keep_vars <- intersect(keep_vars, names(advs))

  inds <- advs %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    dplyr::filter(
      PARAMCD == paramcd,
      ANL01FL == "Y",
      AVISITN == avisitn,
      SAFFL   == "Y",
      # SAS: BNRIND ne 'Missing' and ANRIND ne 'Missing'
      # Also exclude NA and blank (SAS-missing characters) to match
      # PROC SUMMARY default behaviour which excludes missing class values
      !is.na(BNRIND), nchar(BNRIND) > 0L, BNRIND != "Missing",
      !is.na(ANRIND), nchar(ANRIND) > 0L, ANRIND != "Missing"
    )

  if (nrow(inds) == 0L) {
    warning("No records remain after filtering for PARAMCD='", paramcd,
            "', AVISITN=", avisitn, ".  Returning NULL.", call. = FALSE)
    return(invisible(NULL))
  }

  # -------------------------------------------------------------------------
  # 4. Treatment display lookup  (SAS lines 42-56: PROC SQL + PROC FORMAT CNTLIN)
  # -------------------------------------------------------------------------
  # In SAS the TRTFT format is built dynamically from data.
  # In R we use a simple lookup table.
  trt_lookup <- inds %>%
    dplyr::distinct(TRTPN = .data[[trtvar]], TRTP) %>%
    dplyr::arrange(TRTPN)

  # -------------------------------------------------------------------------
  # 5. Count patients by shift categories  (SAS PROC SUMMARY lines 58-63)
  # -------------------------------------------------------------------------
  # SAS PROC SUMMARY with CLASS produces marginal totals.  dplyr group_by
  # does NOT, so we must compute them explicitly:
  #   _TYPE_ with all 5 class vars present -> actual 3x3 cells
  #   _TYPE_ with ANRIND missing            -> row margins
  #   _TYPE_ with BNRIND missing            -> column margins
  #   _TYPE_ with both missing              -> grand total (denom)

  # Helper: count non-missing AVAL  (matches SAS N(AVAL))
  count_n <- function(x) sum(!is.na(x))

  # 5a. Actual cells — group by all five CLASS variables

  cells <- inds %>%
    dplyr::group_by(PARAM, PARAMN, .data[[trtvar]], BNRIND, ANRIND) %>%
    dplyr::summarise(n = count_n(AVAL), .groups = "drop") %>%
    dplyr::rename(TRTPN = dplyr::all_of(trtvar))

  # 5b. Row margins — ANRIND marginal (total across all post-baseline categories)
  row_margins <- inds %>%
    dplyr::group_by(PARAM, PARAMN, .data[[trtvar]], BNRIND) %>%
    dplyr::summarise(n = count_n(AVAL), .groups = "drop") %>%
    dplyr::rename(TRTPN = dplyr::all_of(trtvar)) %>%
    dplyr::mutate(ANRIND = NA_character_)

  # 5c. Column margins — BNRIND marginal (total across all baseline categories)
  col_margins <- inds %>%
    dplyr::group_by(PARAM, PARAMN, .data[[trtvar]], ANRIND) %>%
    dplyr::summarise(n = count_n(AVAL), .groups = "drop") %>%
    dplyr::rename(TRTPN = dplyr::all_of(trtvar)) %>%
    dplyr::mutate(BNRIND = NA_character_)

  # 5d. Grand total — both BNRIND and ANRIND marginal (denom source)
  grand_total <- inds %>%
    dplyr::group_by(PARAM, PARAMN, .data[[trtvar]]) %>%
    dplyr::summarise(n = count_n(AVAL), .groups = "drop") %>%
    dplyr::rename(TRTPN = dplyr::all_of(trtvar)) %>%
    dplyr::mutate(BNRIND = NA_character_, ANRIND = NA_character_)

  # Combine all _TYPE_ levels (SAS PROC SUMMARY output)
  counts <- dplyr::bind_rows(cells, row_margins, col_margins, grand_total) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))

  # -------------------------------------------------------------------------
  # 6. Fix total labels, add numeric codes, split denom  (SAS lines 69-86)
  # -------------------------------------------------------------------------
  # SAS lines 74-75: if missing(bnrind) then bnrind = 'Total'; ...
  fixtot <- counts %>%
    dplyr::mutate(
      BNRIND  = dplyr::if_else(is.na(BNRIND), "Total", BNRIND),
      ANRIND  = dplyr::if_else(is.na(ANRIND), "Total", ANRIND),
      BNRINDN = as.integer(shift_levels[BNRIND]),
      ANRINDN = as.integer(shift_levels[ANRIND])
    )

  # SAS lines 80-81: split denom (Total x Total) from detail
  denom_df <- fixtot %>%
    dplyr::filter(BNRIND == "Total" & ANRIND == "Total") %>%
    dplyr::select(PARAM, PARAMN, TRTPN, denom = n)

  fixtot <- fixtot %>%
    dplyr::filter(!(BNRIND == "Total" & ANRIND == "Total"))

  # -------------------------------------------------------------------------
  # 7. Zero-fill grid  (SAS lines 88-104: nested do-loops)
  # -------------------------------------------------------------------------
  # Distinct param/trtpn combinations present in data
  allcat <- fixtot %>%
    dplyr::distinct(PARAM, PARAMN, TRTPN)

  # Expand to all BNRINDN x ANRINDN combinations  (replaces DATA step do-loops)
  full_grid <- allcat %>%
    tidyr::expand_grid(BNRINDN = seq_len(max_cat), ANRINDN = seq_len(max_cat)) %>%
    dplyr::mutate(n = 0L)

  # -------------------------------------------------------------------------
  # 8. Merge counts into full grid  (SAS lines 106-111)
  # -------------------------------------------------------------------------
  allrecs <- full_grid %>%
    dplyr::left_join(
      fixtot %>% dplyr::select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n_actual = n),
      by = c("PARAM", "PARAMN", "TRTPN", "BNRINDN", "ANRINDN")
    ) %>%
    dplyr::mutate(n = dplyr::coalesce(as.integer(n_actual), n)) %>%
    dplyr::select(-n_actual)

  # -------------------------------------------------------------------------
  # 9. Add denominators and repopulate shift labels  (SAS lines 113-121)
  # -------------------------------------------------------------------------
  alldenom <- allrecs %>%
    dplyr::left_join(denom_df, by = c("PARAM", "PARAMN", "TRTPN")) %>%
    dplyr::filter(!is.na(denom)) %>%
    dplyr::mutate(
      BNRIND = shift_labels[as.character(BNRINDN)],
      ANRIND = shift_labels[as.character(ANRINDN)]
    )

  # -------------------------------------------------------------------------
  # 10. Get post-baseline category names for column headings (SAS lines 123-129)
  # -------------------------------------------------------------------------
  pb_cats <- alldenom %>%
    dplyr::filter(!is.na(ANRIND)) %>%
    dplyr::distinct(ANRINDN, ANRIND) %>%
    dplyr::arrange(ANRINDN) %>%
    dplyr::pull(ANRIND)

  # -------------------------------------------------------------------------
  # 11. Transpose & compute percentages  (SAS DATA step lines 131-148)
  # -------------------------------------------------------------------------
  # SAS line 141: pcts(anrindn) = '(' || put(round(((100*n)/denom), 0.1), 5.1) || ')';
  # Uses janitor::round_half_up to replicate SAS round-half-up behaviour
  trans <- alldenom %>%
    dplyr::arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN) %>%
    dplyr::mutate(
      pct_val = dplyr::if_else(
        denom > 0L,
        janitor::round_half_up(100 * n / denom, digits = 1),
        0
      ),
      pct = dplyr::if_else(
        denom > 0L,
        paste0("(", sprintf("%5.1f", pct_val), ")"),
        "(  0.0)"
      )
    )

  # SAS line 146: trttx = catx(' ', put(trtvar, trtft.), '(N = ' || denom || ')');
  trans <- trans %>%
    dplyr::left_join(trt_lookup, by = "TRTPN") %>%
    dplyr::mutate(
      trttx = paste0(TRTP, " (N = ", denom, ")")
    )

  # Pivot wider — one row per baseline category with n1..n4 / pct1..pct4
  trans_wide <- trans %>%
    dplyr::select(PARAM, PARAMN, TRTPN, trttx, BNRINDN, BNRIND,
                  denom, ANRINDN, n, pct) %>%
    tidyr::pivot_wider(
      id_cols     = c(PARAM, PARAMN, TRTPN, trttx, BNRINDN, BNRIND, denom),
      names_from  = ANRINDN,
      values_from = c(n, pct),
      names_glue  = "{.value}{ANRINDN}"
    )

  # -------------------------------------------------------------------------
  # 12. Prepare display dataset  (SAS PROC REPORT lines 150-172)
  # -------------------------------------------------------------------------
  report_data <- trans_wide %>%
    dplyr::arrange(PARAMN, PARAM, TRTPN, BNRINDN)

  # Build the display-only columns (drop sort-only columns before output)
  # Ensure n and pct columns exist for 1..max_cat
  n_cols   <- paste0("n", seq_len(max_cat))
  pct_cols <- paste0("pct", seq_len(max_cat))

  # Interleave n/pct columns: n1, pct1, n2, pct2, ...
  npct_cols <- as.vector(rbind(n_cols, pct_cols))

  display_cols <- c("trttx", "BNRIND", npct_cols)
  display_data <- report_data %>%
    dplyr::mutate(
      # Factor ordering for shift categories (replaces SAS FORMAT ordering)
      BNRIND = forcats::fct_relevel(BNRIND, "Low", "Normal", "High", "Total"),
      # Factor ordering for PARAMs preserving PARAMN sort order
      PARAM  = forcats::fct_inorder(PARAM)
    ) %>%
    dplyr::select(PARAM, PARAMN, TRTPN, BNRINDN, dplyr::all_of(display_cols))

  # Insert blank separator rows between treatment groups  (SAS BREAK AFTER trttx / SKIP)
  display_split <- display_data %>%
    dplyr::group_by(PARAM, PARAMN, TRTPN) %>%
    dplyr::group_split()

  blank_row <- as.data.frame(
    matrix(NA, nrow = 1L, ncol = ncol(display_data)),
    stringsAsFactors = FALSE
  )
  names(blank_row) <- names(display_data)

  parts <- lapply(seq_along(display_split), function(i) {
    chunk <- as.data.frame(display_split[[i]], stringsAsFactors = FALSE)
    if (i < length(display_split)) {
      rbind(chunk, blank_row)
    } else {
      chunk
    }
  })
  # Flatten list of data frames using tidyr::unnest (replaces base do.call/rbind)
  display_final <- dplyr::tibble(..part = parts) %>%
    tidyr::unnest(cols = ..part)

  # Replace NA counts with empty strings for display
  for (col in c(n_cols, pct_cols)) {
    if (col %in% names(display_final)) {
      display_final[[col]] <- dplyr::if_else(
        is.na(display_final[[col]]),
        ifelse(grepl("^n", col), "", ""),
        as.character(display_final[[col]])
      )
    }
  }
  # Ensure trttx and BNRIND blanks are empty strings
  display_final$trttx  <- dplyr::if_else(is.na(display_final$trttx),  "", display_final$trttx)
  display_final$BNRIND <- dplyr::if_else(is.na(display_final$BNRIND), "", display_final$BNRIND)

  # Final display columns for RTF (exclude sort-key columns)
  rtf_data <- display_final %>%
    dplyr::select(dplyr::all_of(display_cols))

  # -------------------------------------------------------------------------
  # 13. RTF output  (SAS options orientation=landscape; PROC REPORT)
  # -------------------------------------------------------------------------
  # Column header labels mirroring SAS PROC REPORT DEFINE statements
  col_headers <- c(
    "Treatment",          # trttx
    "Baseline\nResult"    # BNRIND  (SAS split char '^' -> newline)
  )
  # Add n / % headers for each post-baseline category
  for (k in seq_len(max_cat)) {
    col_headers <- c(col_headers, "n", "%")
  }

  # Column relative widths  (approximate SAS PROC REPORT column sizing)
  col_widths <- c(
    2.5,   # trttx
    1.2    # BNRIND
  )
  for (k in seq_len(max_cat)) {
    col_widths <- c(col_widths, 0.5, 0.9)
  }

  # Build spanning header text:  "Post-Baseline Result" across all n/pct cols
  # with sub-headers for each category (Low, Normal, High, Total)
  # r2rtf uses rtf_colheader for spanning headers
  n_data_cols <- 2L + 2L * max_cat  # trttx + BNRIND + 2*max_cat

  # First spanning row: empty for first 2 cols, then "Post-Baseline Result"
  span_row1 <- paste0(
    " | | ",
    paste(rep("Post-Baseline Result", max_cat * 2L), collapse = " | ")
  )
  span_row1_widths <- col_widths

  # Second spanning row: column labels with sub-category names
  sub_labels <- character(0)
  for (k in seq_len(max_cat)) {
    cat_name <- if (k <= length(pb_cats)) pb_cats[k] else shift_labels[as.character(k)]
    sub_labels <- c(sub_labels, cat_name, cat_name)
  }
  span_row2 <- paste(c("Treatment", "Baseline\nResult", sub_labels), collapse = " | ")

  # Third spanning row: n / % labels under each category
  npct_labels <- character(0)
  for (k in seq_len(max_cat)) {
    npct_labels <- c(npct_labels, "n", "%")
  }
  span_row3 <- paste(c(" ", " ", npct_labels), collapse = " | ")

  # Construct RTF document
  # r2rtf column header approach: use multiple rtf_colheader() calls for

  # spanning + sub-category + n/% rows

  # Spanning widths for row 1:  trttx(2.5) | BNRIND(1.2) | [Post-Baseline Result spans 2*max_cat cols]
  span1_widths <- c(col_widths[1], col_widths[2],
                    sum(col_widths[3:length(col_widths)]))
  span1_text <- " | | Post-Baseline Result"

  # Spanning widths for row 2: trttx | BNRIND | Low(n+pct) | Normal(n+pct) | ...
  span2_widths <- c(col_widths[1], col_widths[2])
  span2_labels <- c(" ", " ")
  for (k in seq_len(max_cat)) {
    cat_name <- if (k <= length(pb_cats)) pb_cats[k] else shift_labels[as.character(k)]
    idx <- 2L + 2L * (k - 1L)
    pair_width <- col_widths[idx + 1L] + col_widths[idx + 2L]
    span2_widths <- c(span2_widths, pair_width)
    span2_labels <- c(span2_labels, cat_name)
  }
  span2_text <- paste(span2_labels, collapse = " | ")

  # Row 3: individual column labels n / %
  span3_text <- paste(c("Treatment", "Baseline\nResult", npct_labels), collapse = " | ")
  span3_widths <- col_widths

  # Build the RTF
  # Determine unique params for BY-variable title
  param_vals <- report_data %>%
    dplyr::distinct(PARAMN, PARAM) %>%
    dplyr::arrange(PARAMN)

  rtf_path <- file.path(output_path, "outliers_shift_table.rtf")

  rtf_out <- rtf_data %>%
    r2rtf::rtf_page(orientation = "landscape") %>%
    r2rtf::rtf_title(
      title = paste("Shift Table:", paste(param_vals$PARAM, collapse = "; "))
    ) %>%
    r2rtf::rtf_colheader(
      colheader     = span1_text,
      col_rel_width = span1_widths
    ) %>%
    r2rtf::rtf_colheader(
      colheader     = span2_text,
      col_rel_width = span2_widths
    ) %>%
    r2rtf::rtf_colheader(
      colheader     = span3_text,
      col_rel_width = span3_widths
    ) %>%
    r2rtf::rtf_body(
      col_rel_width      = col_widths,
      text_justification = c("l", "l",
                              rep(c("r", "l"), times = max_cat))
    ) %>%
    r2rtf::rtf_footnote(
      footnote = paste("Source: ADVS (PARAMCD =", paramcd,
                        ", AVISITN =", avisitn, ")")
    ) %>%
    r2rtf::rtf_encode()

  r2rtf::write_rtf(rtf_out, file = rtf_path)

  message("Shift table RTF written to: ", rtf_path)

  # Return the transposed data invisibly for downstream comparison / testing
  invisible(as.data.frame(report_data))
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    1. SAS FORMAT/INFORMAT for shift categories (shifttx/shiftord) mapped
####       to named vectors shift_levels / shift_labels in R.
####    2. SAS PROC FORMAT LIBRARY=WORK CNTLIN for dynamic treatment format
####       replaced by dplyr::distinct() lookup table (trt_lookup).
####    3. SAS array processing (ns(*), pcts(*)) in DATA step transposition
####       replaced by tidyr::pivot_wider().
####    4. SAS missing class levels in PROC SUMMARY produce marginal totals;
####       in R these are computed explicitly via separate group_by + bind_rows
####       calls (cells, row_margins, col_margins, grand_total).
####    5. SAS character missing values (blank strings in XPT) are excluded
####       alongside the literal string "Missing" to match PROC SUMMARY
####       default behaviour (no MISSING option specified).
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. Rounding: SAS round(x, 0.1) uses round-half-up; R uses
####       janitor::round_half_up(x, 1) to match.
####    2. PROC SUMMARY class variable interaction with missing values may
####       produce different _TYPE_ rows. Verified by explicit marginal
####       computation in R and filtering for non-missing PARAM/PARAMN/TRTPN.
####    3. Zero-fill grid: tidyr::expand_grid() ensures all 4x4 combinations
####       are present, matching the SAS DATA step do-loop on lines 96-104.
####    4. Sort stability: R dplyr::arrange() is stable within groups.
####       Multi-key sort verified to match SAS PROC SORT behaviour.
#### NO DIRECT R EQUIVALENT:
####    1. SAS PROC FORMAT CNTLIN (dynamic format from data) -> R named
####       vector lookup / dplyr left_join with trt_lookup.
####    2. SAS PROC REPORT with FLOW/ID/ORDER column types -> r2rtf with
####       explicit column definitions and rtf_colheader spanning headers.
####    3. SAS picture format 'percpar' (parenthesised percentage) -> sprintf
####       formatting with paste0("(", ..., ")").
####    4. SAS array processing (ns(*), pcts(*)) in DATA step -> tidyr
####       pivot_wider() operations.
####    5. SAS BREAK AFTER trttx / SKIP -> manual blank-row insertion between
####       treatment groups in the display data frame.
#### PACKAGE SELECTION RATIONALE:
####    haven:    SAS XPT file I/O — tidyverse standard for CDISC transport
####    dplyr:    Data manipulation replacing DATA steps, PROC SQL, PROC SUMMARY
####    tidyr:    Reshaping replacing DATA step do-loops and array transposition
####    stringr:  str_to_title() replacing SAS PROPCASE()
####    Tplyr:    Clinical table grammar (loaded per AAP section 0.4.3 standard)
####    r2rtf:    RTF output replacing PROC REPORT + ODS RTF (Merck production pkg)
####    janitor:  round_half_up() for SAS-compatible rounding (Gate 2 compliance)
####    forcats:  Factor level ordering for shift categories and treatment groups
#### OPEN QUESTIONS:
####    1. Verify handling of records where BNRIND or ANRIND is the literal
####       string "Missing" versus R NA — current implementation excludes both.
####    2. Confirm denominator computation matches SAS CLASS variable marginal
####       interaction — verified via explicit marginal totals in R.
####    3. Verify PROC REPORT BREAK AFTER behaviour correctly maps to r2rtf
####       blank-row group spacing.
####    4. The Total-Total cell (bnrindn=4, anrindn=4) contains n=0 because
####       the grand total is extracted as denom and removed from fixtot;
####       this matches SAS behaviour where the intersection is not double-counted.
#### ============================================================
