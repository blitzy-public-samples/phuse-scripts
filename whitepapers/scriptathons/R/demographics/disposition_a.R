# ============================================================================
# PROGRAM:     disposition_a.R
# DESCRIPTION:  Subject Disposition Table — Completed/Discontinued by treatment
#               arm with sub-categories for Death/AE, Lack of Efficacy, and
#               Other Reasons. Produces an RTF table output.
# MIGRATED FROM: whitepapers/scriptathons/demographics/disposition_a.sas
#                (originally discontinuation_typeA.sas)
# ORIGINAL AUTHOR: Ann Croft (12-Oct-2014)
# MIGRATION DATE:  2026
# LICENSE:      MIT (PhUSE CS WG5 Standard Analyses)
#
# REPLACES SAS CONSTRUCTS:
#   - filename source url / libname xport    -> haven::read_xpt()
#   - 5 PROC FREQ calls                      -> dplyr::count()
#   - DATA step merge / BY-group / RETAIN    -> dplyr left_join / group_by / row_number
#   - PROC TRANSPOSE                         -> tidyr::pivot_wider()
#   - %zero macro                            -> tidyr::replace_na()
#   - %disconta macro / PROC REPORT / ODS RTF -> r2rtf pipeline
#   - SAS round(x, 0.1)                      -> janitor::round_half_up(x, 1)
#
# USAGE:
#   config <- yaml::read_yaml("config/migration_config.yaml")
#   generate_disposition_a(
#     data_path   = config$data_paths$adam_path,
#     output_path = file.path(config$output_paths$rtf_output_path,
#                             "disposition_a.rtf")
#   )
# ============================================================================

# --------------------------------------------------------------------------
# Library Loading
# --------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(rlang)
library(r2rtf)
library(stringr)
library(janitor)
library(cli)

# --------------------------------------------------------------------------
# generate_disposition_a
#
# Produces a Subject Disposition table showing completed/discontinued subjects
# by treatment arm, with sub-categories for Death or Adverse Event, Lack of
# Efficacy, and Other Reasons. Output is written as an RTF file.
#
# @param data_path   Character. Path to the directory containing adsl.xpt
# @param output_path Character. Path for the RTF output file.
#                     Defaults to "disposition_a.rtf" in the working directory.
# @return Invisibly returns the output_path on success.
# --------------------------------------------------------------------------
generate_disposition_a <- function(data_path, output_path = "disposition_a.rtf") {

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================
  if (!is.character(data_path) || length(data_path) != 1L) {
    cli::cli_abort("data_path must be a single character string.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L) {
    cli::cli_abort("output_path must be a single character string.", call. = FALSE)
  }

  adsl_file <- file.path(data_path, "adsl.xpt")
  if (!file.exists(adsl_file)) {
    cli::cli_abort("ADSL transport file not found at: ", adsl_file, call. = FALSE)
  }

  # ==========================================================================
  # 1. READ AND PREPARE ADSL
  #    Replaces SAS lines 7-17:
  #      filename source url "...adsl.xpt";
  #      libname source xport;
  #      data adsl; set source.adsl; where trt01p ne '';
  #        if trt01pn=0 then trt01pn=3; ...
  # ==========================================================================
  adsl <- haven::read_xpt(adsl_file)


  # Normalize column names to uppercase for case-insensitive SAS equivalence
  names(adsl) <- toupper(names(adsl))

  # Validate required ADSL variables
  required_vars <- c("STUDYID", "TRT01P", "TRT01PN", "DISCONFL",
                     "DCREASCD", "DSRAEFL", "DTHFL", "DCDECOD")
  missing_vars <- setdiff(required_vars, names(adsl))
  if (length(missing_vars) > 0L) {
    cli::cli_abort("Required variables not found in ADSL: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Strip haven labelled class from key numeric column to avoid comparison
  # issues in case_when(); character columns are kept as-is.
  adsl <- adsl %>%
    dplyr::mutate(TRT01PN = as.numeric(TRT01PN))

  # Population filter: trt01p ne '' (SAS line 13)
  adsl <- adsl %>%
    dplyr::filter(!is.na(TRT01P), TRT01P != "")

  # Treatment recoding (SAS lines 14-16):
  #   0  -> 3 (Placebo last)
  #   54 -> 1 (Low Dose first)
  #   81 -> 2 (High Dose second)
  adsl <- adsl %>%
    dplyr::mutate(
      TRT01PN = dplyr::case_when(
        TRT01PN == 0  ~ 3L,
        TRT01PN == 54 ~ 1L,
        TRT01PN == 81 ~ 2L,
        TRUE          ~ as.integer(TRT01PN)
      )
    )

  # ==========================================================================
  # 2. FREQUENCY TABULATIONS
  #    Replaces SAS PROC FREQ calls (lines 20-41)
  # ==========================================================================

  # --- Topline counts (SAS lines 20-22) ---
  # proc freq: table studyid * trt01pn * trt01p * disconfl
  topline <- adsl %>%
    dplyr::count(STUDYID, TRT01PN, TRT01P, DISCONFL, name = "COUNT")

  # --- Midline 1: Death or Adverse Event (SAS lines 24-28) ---
  # where DSRAEFL='Y' or DTHFL='Y'
  death_ae_data <- adsl %>%
    dplyr::filter(DSRAEFL == "Y" | DTHFL == "Y")

  midline1 <- death_ae_data %>%
    dplyr::count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")

  sortord1 <- death_ae_data %>%
    dplyr::count(STUDYID, DCREASCD, name = "COUNT")

  # --- Midline 2: Lack of Efficacy (SAS lines 30-34) ---
  # where DCdecod='LACK OF EFFICACY'
  loe_data <- adsl %>%
    dplyr::filter(DCDECOD == "LACK OF EFFICACY")

  midline2 <- loe_data %>%
    dplyr::count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")

  sortord2 <- loe_data %>%
    dplyr::count(STUDYID, DCREASCD, name = "COUNT")

  # --- Midline 3: Other Reasons (SAS lines 36-41) ---
  # where DCdecod^='LACK OF EFFICACY' and DSRAEFL^='Y' and DTHFL^='Y'
  #       and disconfl='Y'
  # Note: SAS ^= with missing values returns TRUE (missing != 'X' is TRUE);
  #       R != with NA returns NA (excluded by filter). Use is.na() OR to
  #       preserve SAS semantics for the inequality conditions.
  other_data <- adsl %>%
    dplyr::filter(
      is.na(DCDECOD) | DCDECOD != "LACK OF EFFICACY",
      is.na(DSRAEFL) | DSRAEFL != "Y",
      is.na(DTHFL)   | DTHFL   != "Y",
      DISCONFL == "Y"
    )

  midline3 <- other_data %>%
    dplyr::count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")

  sortord3 <- other_data %>%
    dplyr::count(STUDYID, DCREASCD, name = "COUNT")

  # ==========================================================================
  # 3. COMBINE AND ORDER
  #    Replaces SAS DATA steps (lines 43-67)
  # ==========================================================================

  # Stack frequency datasets with ordering columns (SAS lines 43-50)
  all_data <- dplyr::bind_rows(
    topline  %>% dplyr::mutate(ord1 = 1L, ord2 = 1L),
    midline1 %>% dplyr::mutate(ord1 = 2L, ord2 = 1L),
    midline2 %>% dplyr::mutate(ord1 = 3L, ord2 = 2L),
    midline3 %>% dplyr::mutate(ord1 = 4L, ord2 = 3L)
  )

  # Stack sort-order datasets (SAS lines 51-57)
  alls <- dplyr::bind_rows(
    sortord1 %>% dplyr::mutate(ord1 = 2L, ord2 = 1L),
    sortord2 %>% dplyr::mutate(ord1 = 3L, ord2 = 2L),
    sortord3 %>% dplyr::mutate(ord1 = 4L, ord2 = 3L)
  )

  # Compute descending sort rank within each ord1/ord2 group
  # Replaces SAS: proc sort by ord1 ord2 descending count;
  #               data alls2; retain ord3; if first.ord2 then ord3=1;
  #               else ord3=ord3+1; (lines 60-67)
  alls2 <- alls %>%
    dplyr::arrange(ord1, ord2, dplyr::desc(COUNT)) %>%
    dplyr::group_by(ord1, ord2) %>%
    dplyr::mutate(ord3 = dplyr::row_number()) %>%
    dplyr::ungroup()

  # ==========================================================================
  # 4. POPULATION COUNTS
  #    Replaces SAS PROC FREQ (lines 69-72)
  # ==========================================================================
  pop <- adsl %>%
    dplyr::count(STUDYID, TRT01PN, TRT01P, name = "tot")

  # ==========================================================================
  # 5. CREATE OUTPUT CELLS WITH PERCENTAGE
  #    Replaces SAS DATA step allp (lines 75-86)
  #    SAS formula: countc = put(count,4.) || ' (' ||
  #                          put(round(count/tot, 0.1), 5.1) || ')'
  #    Uses janitor::round_half_up() for SAS-compatible rounding (AAP §0.7.2)
  # ==========================================================================
  allp <- all_data %>%
    dplyr::left_join(pop, by = c("STUDYID", "TRT01PN", "TRT01P")) %>%
    dplyr::mutate(
      countc = dplyr::if_else(
        !is.na(COUNT),
        paste0(
          sprintf("%4d", COUNT),
          " (",
          sprintf("%5.1f", janitor::round_half_up(COUNT / tot, 1L)),
          ")"
        ),
        NA_character_
      ),
      # Label topline rows (SAS lines 82-85)
      DCREASCD = dplyr::case_when(
        ord1 == 1L & DISCONFL == "Y" ~ "Discontinued",
        ord1 == 1L                   ~ "Completed the Study",
        TRUE                         ~ DCREASCD
      )
    )

  # ==========================================================================
  # 6. TRANSPOSE TO WIDE FORMAT
  #    Replaces SAS PROC TRANSPOSE (lines 89-94)
  #    proc transpose data=allp out=allpt prefix=trt;
  #      var countc; by studyid ord1 ord2 dcreascd; id trt01pn;
  # ==========================================================================
  allp_sorted <- allp %>%
    dplyr::arrange(STUDYID, ord1, ord2, DCREASCD, TRT01PN)

  allpt <- allp_sorted %>%
    dplyr::select(STUDYID, ord1, ord2, DCREASCD, TRT01PN, countc) %>%
    tidyr::pivot_wider(
      names_from  = TRT01PN,
      values_from = countc,
      names_prefix = "trt"
    )

  # ==========================================================================
  # 7. TREATMENT INFO FOR HEADERS
  #    Replaces SAS DATA _NULL_ + call symput (lines 97-113)
  #    Note: Original SAS code has a bug where both 'c' and 'n' macro vars

  #    were set to trt01p (the treatment name). The 'n' macro should have
  #    held the population count. This R version uses the correct population
  #    total for the (N=...) header.
  # ==========================================================================
  trt_info <- pop %>%
    dplyr::distinct(TRT01PN, TRT01P, tot) %>%
    dplyr::arrange(TRT01PN)

  trt_cols <- paste0("trt", trt_info$TRT01PN)

  # Ensure all expected treatment columns exist in the transposed data
  for (tc in trt_cols) {
    if (!tc %in% names(allpt)) {
      allpt[[tc]] <- NA_character_
    }
  }

  # ==========================================================================
  # 8. HANDLE MISSING CELLS
  #    Replaces SAS %zero macro (lines 118-129)
  #    SAS: if trt&i = '' then trt&i = '###0';
  #         else trt&i = tranwrd(trt&i, ' ', '#');
  #    R: replace NA with display zero; '#' space placeholder not needed in
  #       r2rtf (handles spaces natively).
  # ==========================================================================
  allpt2 <- allpt %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(trt_cols),
        ~tidyr::replace_na(., "   0")  # intentional: formatted zero-count string for display table cells
      )
    )

  # ==========================================================================
  # 9. MERGE SORT ORDER AND SET TOPLINE ORD3
  #    Replaces SAS DATA allpt3 (lines 132-141)
  # ==========================================================================
  allpt3 <- allpt2 %>%
    dplyr::left_join(
      alls2 %>% dplyr::select(STUDYID, ord1, ord2, DCREASCD, ord3),
      by = c("STUDYID", "ord1", "ord2", "DCREASCD")
    ) %>%
    dplyr::mutate(
      # SAS lines 135-136: topline ord3 assignment
      ord3 = dplyr::case_when(
        ord1 == 1L & toupper(DCREASCD) == "DISCONTINUED" ~ 2L,
        ord1 == 1L ~ 1L,
        TRUE       ~ ord3
      )
    ) %>%
    dplyr::arrange(STUDYID, ord1, ord2, ord3)

  # ==========================================================================
  # 10. BUILD DISPLAY LABELS
  #     Replaces SAS DATA final (lines 146-154)
  #     SAS: if ord1=1 then col1 = strip(dcreascd);
  #          else if ord1=3 then col1 = '####' || strip(dcreascd);
  #     '####' represents 4 space indentation (SAS '#' = space in RTF)
  # ==========================================================================
  final_data <- allpt3 %>%
    dplyr::mutate(
      col1 = dplyr::case_when(
        ord1 == 1L ~ stringr::str_trim(DCREASCD),
        ord1 == 3L ~ paste0("    ", stringr::str_trim(DCREASCD)),
        TRUE       ~ ""
      )
    )

  # ==========================================================================
  # 11. BUILD DISPLAY TABLE WITH GROUP HEADERS
  #     Replaces SAS PROC REPORT COMPUTE BEFORE ord2 (lines 184-190):
  #       if ord2=1 then text='##Death or Adverse Event';
  #       if ord2=2 then text='##Lack of Efficacy-Related Reasons';
  #       if ord2=3 then text='##Other Reasons';
  # ==========================================================================
  group_header_map <- c(
    "1" = "  Death or Adverse Event",
    "2" = "  Lack of Efficacy-Related Reasons",
    "3" = "  Other Reasons"
  )

  # Assemble display rows with group header rows inserted at each ord2 break
  display_rows <- list()
  row_is_header <- logical(0L)

  ord2_values <- sort(unique(final_data$ord2))

  for (g in ord2_values) {
    # Create group header row (bold spanning text)
    header_row <- tibble::tibble(col1 = group_header_map[as.character(g)])
    for (tc in trt_cols) {
      header_row[[tc]] <- ""
    }
    display_rows <- c(display_rows, list(header_row))
    row_is_header <- c(row_is_header, TRUE)

    # Detail rows for this ord2 group, sorted by ord1 then ord3
    detail <- final_data %>%
      dplyr::filter(ord2 == g) %>%
      dplyr::arrange(ord1, ord3) %>%
      dplyr::select(col1, dplyr::all_of(trt_cols))

    if (nrow(detail) > 0L) {
      display_rows <- c(display_rows, list(detail))
      row_is_header <- c(row_is_header, rep(FALSE, nrow(detail)))
    }
  }

  display_df <- dplyr::bind_rows(display_rows)

  # ==========================================================================
  # 12. RTF OUTPUT
  #     Replaces SAS ODS RTF + PROC REPORT (lines 143-194)
  # ==========================================================================

  # --- Build column header string ---
  # SAS: ("Treatment Name @(N=count)" trt&i)
  # R version uses correct population count for (N=...) header
  header_pieces <- "Subject Disposition"
  for (i in seq_len(nrow(trt_info))) {
    # str_squish() collapses internal whitespace in treatment names from haven
    trt_label <- stringr::str_squish(trt_info$TRT01P[i])
    header_pieces <- c(
      header_pieces,
      paste0(trt_label, " (N=", trt_info$tot[i], ")")
    )
  }
  col_header_str <- paste(header_pieces, collapse = " | ")

  # --- Column relative widths ---
  # SAS: col1 = 9.5 cm, each trt = 4.5 cm
  col_widths <- c(9.5, rep(4.5, nrow(trt_info)))

  # --- Text format matrix: bold for group header rows ---
  n_display_rows <- nrow(display_df)
  n_display_cols <- ncol(display_df)
  fmt_matrix <- matrix("", nrow = n_display_rows, ncol = n_display_cols)
  fmt_matrix[row_is_header, ] <- "b"

  # --- Ensure output directory exists ---
  output_dir <- dirname(output_path)
  if (nzchar(output_dir) && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # --- Generate RTF ---
  display_df %>%
    rtf_title(
      title = c(
        "Table X: Disposition Table",
        "",
        "Subject Disposition"
      )
    ) %>%
    rtf_colheader(
      colheader   = col_header_str,
      col_rel_width = col_widths
    ) %>%
    rtf_body(
      col_rel_width      = col_widths,
      text_justification = c("l", rep("l", nrow(trt_info))),
      text_format        = fmt_matrix
    ) %>%
    rtf_footnote(
      footnote = c(
        "Abbreviations: N=total number of subjects in population.",
        "               n=number of subjects in the specified category",
        "               %=Percentage of patients with N as denominator"
      )
    ) %>%
    rtf_encode() %>%
    write_rtf(file = output_path)

  invisible(output_path)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#    1. ADSL XPT transport file is located at data_path/adsl.xpt
#    2. Treatment recoding matches SAS source: TRT01PN 0->3, 54->1, 81->2
#       (maps Placebo to position 3, Low Dose to 1, High Dose to 2)
#    3. Required ADSL variables: STUDYID, TRT01P, TRT01PN, DISCONFL,
#       DCREASCD, DSRAEFL, DTHFL, DCDECOD (all uppercase per CDISC XPT)
#    4. DCREASCD (CRF reason) used for display; DCDECOD (coded term) used
#       for filtering, matching original SAS usage of mixed-case DCdecod
#    5. The SAS source displays count/total as a proportion (0-1 scale),
#       not as a percentage (0-100 scale), per the formula:
#       round(count/tot, 0.1) formatted with 5.1
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Rounding of proportions: janitor::round_half_up() used to match
#       SAS round-half-up behavior. R default round() uses banker's
#       rounding (half-to-even) which could differ at exact .05 boundaries.
#    2. Sort stability: R dplyr::arrange() is stable within groups;
#       SAS PROC SORT is also stable. No differences expected for
#       the descending-count sort order computation.
#    3. Missing value filter semantics: SAS ^= with missing character
#       returns TRUE (blank != 'X' is TRUE); R != with NA returns NA
#       (excluded by filter). Explicit is.na() OR clauses added in
#       the Other Reasons filter to preserve SAS behavior.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS %zero macro (lines 118-129) replaces spaces with '#' as
#       RTF space placeholders -> r2rtf handles space preservation
#       natively; only NA->zero replacement is needed.
#    2. SAS PROC REPORT COMPUTE BEFORE ord2 -> manual group header
#       row insertion into the display data frame with bold formatting
#       via r2rtf text_format matrix.
#    3. SAS RETAIN + first.ord2 BY-group logic (lines 62-67) ->
#       dplyr::group_by() + row_number() for within-group ranking.
#    4. SAS call symput for dynamic macro variables (lines 98-111) ->
#       R tibble with distinct treatment info.
#
# PACKAGE SELECTION RATIONALE:
#    haven    — CDISC XPT transport I/O; preserves SAS labels/formats
#    dplyr    — Replaces DATA step, PROC FREQ, PROC SORT, BY-group
#    tidyr    — Replaces PROC TRANSPOSE via pivot_wider(); replace_na()
#               replaces %zero macro
#    r2rtf    — Replaces ODS RTF + PROC REPORT for regulatory RTF output
#    stringr  — str_trim() replaces SAS strip() for label construction;
#               tidyverse-idiomatic per AAP rule (no base R trimws())
#    janitor  — round_half_up() for SAS-compatible rounding (AAP §0.7.2)
#
# OPEN QUESTIONS:
#    1. Verify treatment recoding values (0/54/81) against actual study
#       protocol; these are specific to the CDISC Pilot Study
#    2. Confirm DCREASCD vs DCDECOD variable names in target ADSL;
#       original SAS uses mixed-case DCdecod due to case-insensitivity
#    3. Original SAS code has a bug in call symput (line 106): both
#       'c' (character name) and 'n' (intended count) macro variables
#       are set to strip(trt01p). The R version corrects this to use
#       the actual population count for the (N=...) column header.
#    4. The proportion display (count/tot on 0-1 scale) in the SAS
#       source may be intentional or a formula error; the footnote
#       references "%" but the calculation shows proportions. This R
#       version faithfully replicates the SAS formula.
#
# ============================================================
