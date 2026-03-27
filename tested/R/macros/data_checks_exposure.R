# =============================================================================
# PROGRAM NAME: data_checks_exposure.R
#
# DESCRIPTION:  Exposure domain data-quality checks — migrated from SAS to R.
#               Centralises exposure data-quality checks that identify exposures
#               with missing/invalid frequency codes (EXDOSFRQ), detect subjects
#               excluded from Analysis A (missing study days) and Analysis B
#               (missing dose numbers), orchestrate these checks, and write
#               results to an Excel (.xlsx) workbook.
#
#               Five SAS macros are replaced by five parameterised R functions:
#                 %exposure_exdosfrq_missing  -> exposure_exdosfrq_missing()
#                 %exposure_err_a(ds)         -> exposure_err_a()
#                 %exposure_err_b(ds)         -> exposure_err_b()
#                 %exposure_check             -> exposure_check()
#                 %exposure_check_out         -> exposure_check_out()
#
# ORIGINAL SAS: tested/SAS/macros/data_checks_exposure.sas (191 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
#
# R MIGRATION:  Migrated to R using dplyr, stringr, janitor, openxlsx, tibble
# R REQUIRES:   dplyr (>= 1.1.0), stringr (>= 1.5.0), janitor (>= 2.2.0),
#               openxlsx (>= 4.2.5), tibble (>= 3.2.0)
# R DEPENDS ON: tested/R/utilities/xml_output.R
#               (provides create_workbook, create_workbook_styles,
#                write_data_table, apply_page_setup)
#
# NOTES:        All SAS macro parameters become named R function arguments.
#               SAS global macro variables become function return values in
#               named lists.  Missing values mapped to NA (never 0).
#               SAS-compatible rounding via janitor::round_half_up().
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(janitor)
library(openxlsx)
library(tibble)

# ---------------------------------------------------------------------------
# Internal dependency: xml_output.R
# Provides: create_workbook(), create_workbook_styles(),
#           write_data_table(), apply_page_setup()
# ---------------------------------------------------------------------------
if (!exists("create_workbook", mode = "function")) {
  source(file.path("tested", "R", "utilities", "xml_output.R"))
}


# =============================================================================
# exposure_exdosfrq_missing
# =============================================================================
#' Identify exposure events with missing or invalid dosing frequency codes.
#'
#' Replaces SAS \code{%exposure_exdosfrq_missing} (lines 2-23 of
#' data_checks_exposure.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item PROC SQL groups EX_DM by ARM and PROPCASE(EXDOSFRM), counting records
#'         whose EXDOSFRQ is NOT in the valid list (lines 4-12).
#'   \item DATA step retains column order, blanks repeated ARM labels (first.arm
#'         logic), computes exdosfrq_miss_pct = 100 * miss / total, and adds
#'         three blank placeholder columns (lines 14-22).
#' }
#'
#' @param ex_dm         A data.frame / tibble with at least columns
#'                      \code{arm}, \code{exdosfrm}, and \code{exdosfrq}.
#' @param vld_exdosfrq  Character vector of valid EXDOSFRQ codes. Records whose
#'                      \code{exdosfrq} is NOT in this vector are counted as
#'                      missing.
#'
#' @return A tibble with columns: arm, exdosfrm, blank1, blank2, blank3,
#'         exdosfrq_miss, exdosfrq_miss_pct.  Repeated arm values are blanked
#'         for display purposes.
#'
#' @examples
#' \dontrun{
#' result <- exposure_exdosfrq_missing(ex_dm, c("QD", "BID", "TID"))
#' }
exposure_exdosfrq_missing <- function(ex_dm, vld_exdosfrq) {

 # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ex_dm)) {
    stop("'ex_dm' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(vld_exdosfrq)) {
    stop("'vld_exdosfrq' must be a character vector of valid EXDOSFRQ codes.",
         call. = FALSE)
  }

  required_cols <- c("arm", "exdosfrm", "exdosfrq")
  missing_cols <- setdiff(required_cols, colnames(ex_dm))
  if (length(missing_cols) > 0L) {
    stop(
      paste0("'ex_dm' is missing required columns: ",
             paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # --- Handle empty input ----------------------------------------------------
  if (nrow(ex_dm) == 0L) {
    return(tibble::tibble(
      arm              = character(0),
      exdosfrm         = character(0),
      blank1           = numeric(0),
      blank2           = numeric(0),
      blank3           = numeric(0),
      exdosfrq_miss    = integer(0),
      exdosfrq_miss_pct = numeric(0)
    ))
  }

  # --- Core logic (replaces SAS PROC SQL + DATA step) ------------------------

  # PROC SQL equivalent: group by ARM and PROPCASE(EXDOSFRM), count records
  # whose EXDOSFRQ is not in the valid list.
  # SAS propcase() -> stringr::str_to_title()
  result <- ex_dm %>%
    dplyr::mutate(exdosfrm = stringr::str_to_title(exdosfrm)) %>%
    dplyr::group_by(arm, exdosfrm) %>%
    dplyr::summarise(
      exdosfrq_miss = sum(!(exdosfrq %in% vld_exdosfrq)),
      total         = dplyr::n(),
      .groups       = "drop"
    ) %>%
    # Explicitly ungroup to ensure clean state (schema: ungroup)
    dplyr::ungroup() %>%
    # Compute percentage with SAS-compatible rounding (Gate 2)
    # SAS: exdosfrq_miss_pct = 100*exdosfrq_miss/total;
    dplyr::mutate(
      exdosfrq_miss_pct = dplyr::if_else(
        total == 0L,
        0,
        janitor::round_half_up(100 * exdosfrq_miss / total, digits = 1)
      )
    ) %>%
    # Add blank placeholder columns (SAS: blank1 = .; blank2 = .; blank3 = .;)
    dplyr::mutate(
      blank1 = NA_real_,
      blank2 = NA_real_,
      blank3 = NA_real_
    ) %>%
    # Sort to establish BY-group order before blanking
    dplyr::arrange(arm, exdosfrm) %>%
    # Blank repeated arm labels (SAS: if not first.arm then arm = '')
    dplyr::mutate(arm = dplyr::if_else(duplicated(arm), "", arm)) %>%
    # Retain column order: arm exdosfrm blank1 blank2 blank3 exdosfrq_miss
    #                      exdosfrq_miss_pct   (SAS RETAIN + DROP total)
    dplyr::select(arm, exdosfrm, blank1, blank2, blank3,
                  exdosfrq_miss, exdosfrq_miss_pct)

  return(result)
}


# =============================================================================
# exposure_err_a
# =============================================================================
#' Identify subjects excluded from Exposure Analysis A due to missing study days.
#'
#' Replaces SAS \code{%exposure_err_a(ds)} (lines 29-65 of
#' data_checks_exposure.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Identifies ARM/USUBJID combinations where \code{max(studydays)} is
#'         missing — i.e. ALL study-day values for a subject are NA (lines 31-39).
#'   \item Counts total excluded subjects, stored as a global macro variable in
#'         SAS — returned as an integer in R (lines 41-43).
#'   \item Builds a per-arm summary with LEFT JOIN on DM totals.  Arms with no
#'         excluded subjects receive count = 0 and pct = 0 (lines 46-57).
#'   \item Blanks repeated ARM labels in the detail table for display
#'         (lines 59-63).
#' }
#'
#' @param ex_dm  A data.frame / tibble with at least columns \code{arm},
#'               \code{usubjid}, and \code{studydays}.
#' @param dm     A data.frame / tibble (DM domain) with at least column
#'               \code{arm}, used for per-arm denominator totals.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{ex_err_a}{Tibble of excluded subjects (arm, usubjid) with blanked
#'                     repeated ARM values.}
#'     \item{ex_err_a_summary}{Tibble with columns arm, count, pct.}
#'     \item{ex_err_a_count}{Integer — total number of excluded subjects.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- exposure_err_a(ex_dm, dm)
#' result$ex_err_a_count
#' }
exposure_err_a <- function(ex_dm, dm) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ex_dm)) {
    stop("'ex_dm' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.data.frame(dm)) {
    stop("'dm' must be a data.frame or tibble.", call. = FALSE)
  }

  ex_required <- c("arm", "usubjid", "studydays")
  ex_missing <- setdiff(ex_required, colnames(ex_dm))
  if (length(ex_missing) > 0L) {
    stop(
      paste0("'ex_dm' is missing required columns: ",
             paste(ex_missing, collapse = ", ")),
      call. = FALSE
    )
  }

  dm_required <- c("arm")
  dm_missing <- setdiff(dm_required, colnames(dm))
  if (length(dm_missing) > 0L) {
    stop(
      paste0("'dm' is missing required column: ",
             paste(dm_missing, collapse = ", ")),
      call. = FALSE
    )
  }

  # --- Step 1: Identify subjects with ALL studydays missing -------------------
  # SAS: select arm, usubjid from
  #        (select arm, usubjid, max(studydays) as studydays
  #         from &ds. group by arm, usubjid)
  #      where studydays is missing
  #
  # In R, max(na.rm = TRUE) returns -Inf when all values are NA (with warning).
  # We suppress the warning and use !is.finite() to catch both -Inf and NA.
  ex_err_a_detail <- ex_dm %>%
    dplyr::group_by(arm, usubjid) %>%
    dplyr::summarise(
      max_studydays = suppressWarnings(max(studydays, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.finite(max_studydays)) %>%
    dplyr::select(arm, usubjid) %>%
    dplyr::arrange(arm, usubjid)

  # --- Step 2: Count total excluded subjects ----------------------------------
  # SAS: %global ex_err_a; select put(count(1),8. -L) into: ex_err_a
  ex_err_a_count <- nrow(ex_err_a_detail)

  # --- Step 3: Build per-arm summary with DM denominator totals ---------------
  # SAS: LEFT JOIN of DM arm totals with ex_err_a arm counts
  dm_totals <- dm %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(total = dplyr::n(), .groups = "drop")

  err_a_arm_counts <- ex_err_a_detail %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  ex_err_a_summary <- dm_totals %>%
    dplyr::left_join(err_a_arm_counts, by = "arm") %>%
    dplyr::mutate(
      # SAS: case when b.count is missing then 0 else b.count end
      count = dplyr::if_else(is.na(count), 0L, as.integer(count)), # legitimate: count initialized to zero after aggregate join
      # SAS: case when b.count is missing then 0 else b.count/a.total end
      pct   = dplyr::if_else(
        count == 0L,
        0,
        janitor::round_half_up(count / total, digits = 4)
      )
    ) %>%
    dplyr::select(arm, count, pct)

  # --- Step 4: Blank repeated arm labels in detail for display ----------------
  # SAS: data ex_err_a; set ex_err_a; by arm usubjid;
  #      if not first.arm then arm = '';
  ex_err_a_display <- ex_err_a_detail %>%
    dplyr::arrange(arm, usubjid) %>%
    dplyr::mutate(arm = dplyr::if_else(duplicated(arm), "", arm))

  return(list(
    ex_err_a         = ex_err_a_display,
    ex_err_a_summary = ex_err_a_summary,
    ex_err_a_count   = ex_err_a_count
  ))
}


# =============================================================================
# exposure_err_b
# =============================================================================
#' Identify exposure events excluded from Analysis B due to missing dose numbers.
#'
#' Replaces SAS \code{%exposure_err_b(ds)} (lines 68-111 of
#' data_checks_exposure.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Filters EX_DM for rows where DOSES is missing, sorted by ARM and
#'         USUBJID (lines 72-77).
#'   \item Counts total excluded events (lines 79-81).
#'   \item Builds per-arm summary with distinct subject counts and event counts.
#'         Denominators come from DM (arm_tot) and EX_DM (ex_tot) via inner
#'         join.  Arms with no errors receive count = 0 and pct = 0
#'         (lines 84-109).
#' }
#'
#' @param ex_dm  A data.frame / tibble with at least columns \code{arm},
#'               \code{usubjid}, and \code{doses}.
#' @param dm     A data.frame / tibble (DM domain) with at least column
#'               \code{arm}, used for per-arm subject denominator.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{ex_err_b}{Tibble of excluded events (arm, usubjid).}
#'     \item{ex_err_b_summary}{Tibble with columns arm, subject_count,
#'           subject_pct, event_count, event_pct.}
#'     \item{ex_err_b_count}{Integer — total number of excluded events.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- exposure_err_b(ex_dm, dm)
#' result$ex_err_b_count
#' }
exposure_err_b <- function(ex_dm, dm) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ex_dm)) {
    stop("'ex_dm' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.data.frame(dm)) {
    stop("'dm' must be a data.frame or tibble.", call. = FALSE)
  }

  ex_required <- c("arm", "usubjid", "doses")
  ex_missing <- setdiff(ex_required, colnames(ex_dm))
  if (length(ex_missing) > 0L) {
    stop(
      paste0("'ex_dm' is missing required columns: ",
             paste(ex_missing, collapse = ", ")),
      call. = FALSE
    )
  }

  dm_required <- c("arm")
  dm_missing <- setdiff(dm_required, colnames(dm))
  if (length(dm_missing) > 0L) {
    stop(
      paste0("'dm' is missing required column: ",
             paste(dm_missing, collapse = ", ")),
      call. = FALSE
    )
  }

  # --- Step 1: Filter for rows with missing doses ----------------------------
  # SAS: select arm, usubjid from &ds. where doses is missing
  #      order by arm, usubjid
  ex_err_b_detail <- ex_dm %>%
    dplyr::filter(is.na(doses)) %>%
    dplyr::select(arm, usubjid) %>%
    dplyr::arrange(arm, usubjid)

  # --- Step 2: Count total excluded events ------------------------------------
  # SAS: %global ex_err_b; select put(count(1),8. -L) into: ex_err_b
  ex_err_b_count <- nrow(ex_err_b_detail)

  # --- Step 3: Build per-arm summary ------------------------------------------
  # SAS: inner join DM arm_tot and EX_DM ex_tot, then left join with err_b stats

  # DM arm totals (subject denominator)
  dm_arm_totals <- dm %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(arm_tot = dplyr::n(), .groups = "drop")

  # EX_DM arm totals (event denominator)
  ex_arm_totals <- ex_dm %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(ex_tot = dplyr::n(), .groups = "drop")

  # Inner join: only arms present in both DM and EX_DM
  # SAS: from (...) a, (...) b where a.arm = b.arm
  arm_totals <- dm_arm_totals %>%
    dplyr::inner_join(ex_arm_totals, by = "arm")

  # Error stats per arm (subject_count = distinct subjects, event_count = events)
  if (ex_err_b_count > 0L) {
    err_b_stats <- ex_err_b_detail %>%
      dplyr::group_by(arm) %>%
      dplyr::summarise(
        subject_count = dplyr::n_distinct(usubjid),
        event_count   = dplyr::n(),
        .groups       = "drop"
      )
  } else {
    # No errors — create empty stats tibble with correct column types
    err_b_stats <- tibble::tibble(
      arm           = character(0),
      subject_count = integer(0),
      event_count   = integer(0)
    )
  }

  # Left join totals with error stats and compute percentages
  # SAS: CASE WHEN subject_count IS MISSING THEN 0 ELSE subject_count END
  ex_err_b_summary <- arm_totals %>%
    dplyr::left_join(err_b_stats, by = "arm") %>%
    dplyr::mutate(
      subject_count = dplyr::if_else(is.na(subject_count), 0L, # legitimate: subject count initialized to zero after aggregate join
                                     as.integer(subject_count)),
      subject_pct   = dplyr::if_else(
        subject_count == 0L,
        0,
        janitor::round_half_up(subject_count / arm_tot, digits = 4)
      ),
      event_count   = dplyr::if_else(is.na(event_count), 0L, # legitimate: event count initialized to zero after aggregate join
                                     as.integer(event_count)),
      event_pct     = dplyr::if_else(
        event_count == 0L,
        0,
        janitor::round_half_up(event_count / ex_tot, digits = 4)
      )
    ) %>%
    dplyr::select(arm, subject_count, subject_pct, event_count, event_pct)

  return(list(
    ex_err_b         = ex_err_b_detail,
    ex_err_b_summary = ex_err_b_summary,
    ex_err_b_count   = ex_err_b_count
  ))
}


# =============================================================================
# exposure_check
# =============================================================================
#' Run all exposure data-quality checks.
#'
#' Replaces SAS \code{%exposure_check} (lines 116-126 of
#' data_checks_exposure.sas).  Orchestrates the three constituent checks:
#' \code{exposure_exdosfrq_missing()}, \code{exposure_err_a()}, and
#' \code{exposure_err_b()}.
#'
#' @param ex_dm         A data.frame / tibble with the merged EX + DM exposure
#'                      dataset.  Must contain columns required by all three
#'                      sub-checks (arm, usubjid, exdosfrm, exdosfrq,
#'                      studydays, doses).
#' @param dm            A data.frame / tibble (DM domain) with at least column
#'                      \code{arm}.
#' @param vld_exdosfrq  Character vector of valid EXDOSFRQ codes passed to
#'                      \code{exposure_exdosfrq_missing()}.
#'
#' @return A named list combining results from all three checks:
#'   \describe{
#'     \item{ex_exdosfrq_missing}{Tibble from \code{exposure_exdosfrq_missing()}.}
#'     \item{ex_err_a}{Detail tibble from \code{exposure_err_a()}.}
#'     \item{ex_err_a_summary}{Summary tibble from \code{exposure_err_a()}.}
#'     \item{ex_err_a_count}{Integer from \code{exposure_err_a()}.}
#'     \item{ex_err_b}{Detail tibble from \code{exposure_err_b()}.}
#'     \item{ex_err_b_summary}{Summary tibble from \code{exposure_err_b()}.}
#'     \item{ex_err_b_count}{Integer from \code{exposure_err_b()}.}
#'   }
#'
#' @examples
#' \dontrun{
#' checks <- exposure_check(ex_dm, dm, c("QD", "BID", "TID"))
#' checks$ex_err_a_count
#' }
exposure_check <- function(ex_dm, dm, vld_exdosfrq) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(ex_dm)) {
    stop("'ex_dm' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.data.frame(dm)) {
    stop("'dm' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(vld_exdosfrq)) {
    stop("'vld_exdosfrq' must be a character vector.", call. = FALSE)
  }

  # --- Run constituent checks -------------------------------------------------
  # SAS: %exposure_exdosfrq_missing;  (uses global datasets & macro vars)
  exdosfrq_result <- exposure_exdosfrq_missing(ex_dm, vld_exdosfrq)

  # SAS: %exposure_err_a(ex_dm);
  err_a_result <- exposure_err_a(ex_dm, dm)

  # SAS: %exposure_err_b(ex_dm);
  err_b_result <- exposure_err_b(ex_dm, dm)

  # --- Combine results into a single named list -------------------------------
  results <- list(
    ex_exdosfrq_missing = exdosfrq_result,
    ex_err_a            = err_a_result$ex_err_a,
    ex_err_a_summary    = err_a_result$ex_err_a_summary,
    ex_err_a_count      = err_a_result$ex_err_a_count,
    ex_err_b            = err_b_result$ex_err_b,
    ex_err_b_summary    = err_b_result$ex_err_b_summary,
    ex_err_b_count      = err_b_result$ex_err_b_count
  )

  return(results)
}


# =============================================================================
# exposure_check_out
# =============================================================================
#' Write exposure data-check results to an Excel workbook.
#'
#' Replaces SAS \code{%exposure_check_out} (lines 131-190 of
#' data_checks_exposure.sas).  Creates an Excel workbook with up to 6
#' worksheets containing data-check results, optional pre-validation and
#' domain-check error tables, and a metadata sheet for template row control.
#'
#' Worksheet mapping (SAS -> R):
#' \itemize{
#'   \item \code{xls.checka}    -> CHECKA worksheet  (ex_err_a_summary)
#'   \item \code{xls.checkb}    -> CHECKB worksheet  (ex_err_b_summary)
#'   \item \code{xls.missdosfrq}-> MISSDOSFRQ worksheet (ex_exdosfrq_missing)
#'   \item \code{xls.pva_err}   -> PVA_ERR worksheet  (optional)
#'   \item \code{xls.dc_err}    -> DC_ERR worksheet   (optional)
#'   \item \code{xls.dcinfo}    -> DCINFO worksheet   (arm_count metadata)
#' }
#'
#' @param output_file            Character. Full path to the output Excel file
#'                               (.xlsx).  Replaces SAS \code{&expout.} macro
#'                               variable.
#' @param check_results          Named list as returned by
#'                               \code{exposure_check()}.  Must contain
#'                               \code{ex_err_a_summary},
#'                               \code{ex_err_b_summary}, and
#'                               \code{ex_exdosfrq_missing}.
#' @param arm_count              Integer. The number of treatment arms, written
#'                               to the DCINFO worksheet for template row
#'                               control (SAS \code{&num_arms.}).
#' @param final_exposure_err_d   Optional data.frame / tibble of pre-validation
#'                               errors (SAS \code{final_exposureD_err}).  If
#'                               non-NULL, written to the PVA_ERR worksheet.
#' @param final_exposure_err_e   Optional data.frame / tibble of domain-check
#'                               errors (SAS \code{final_exposureE_err}).  If
#'                               non-NULL, written to the DC_ERR worksheet.
#'
#' @return The output file path (invisible), for method chaining.
#'
#' @examples
#' \dontrun{
#' exposure_check_out("output/exposure_checks.xlsx", checks, arm_count = 3)
#' }
exposure_check_out <- function(output_file,
                               check_results,
                               arm_count,
                               final_exposure_err_d = NULL,
                               final_exposure_err_e = NULL) {

  # --- Input validation -------------------------------------------------------
  if (!is.character(output_file) || length(output_file) != 1L ||
      nchar(output_file) == 0L) {
    stop("'output_file' must be a non-empty character string.", call. = FALSE)
  }
  if (!is.list(check_results)) {
    stop("'check_results' must be a named list from exposure_check().",
         call. = FALSE)
  }
  required_names <- c("ex_err_a_summary", "ex_err_b_summary",
                       "ex_exdosfrq_missing")
  missing_names <- setdiff(required_names, names(check_results))
  if (length(missing_names) > 0L) {
    stop(
      paste0("'check_results' is missing required elements: ",
             paste(missing_names, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!is.numeric(arm_count) || length(arm_count) != 1L) {
    stop("'arm_count' must be a single numeric value.", call. = FALSE)
  }

  # --- Create workbook and styles (xml_output.R helpers) ----------------------
  # SAS: libname xls pcfiles path="&expout.";
  wb     <- create_workbook(title = "Exposure Data Check Summary")
  styles <- create_workbook_styles()

  # --- Add worksheets ---------------------------------------------------------
  # SAS: drop table xls.checka, xls.checkb, xls.missdosfrq, xls.pva_err,
  #      xls.dc_err, xls.dcinfo;
  # In R, we start from a fresh workbook so no need to drop.
  openxlsx::addWorksheet(wb, "CHECKA")
  openxlsx::addWorksheet(wb, "CHECKB")
  openxlsx::addWorksheet(wb, "MISSDOSFRQ")
  openxlsx::addWorksheet(wb, "PVA_ERR")
  openxlsx::addWorksheet(wb, "DC_ERR")
  openxlsx::addWorksheet(wb, "DCINFO")

  # --- Write CHECKA worksheet (ex_err_a_summary) ------------------------------
  # SAS: data xls.checka; set ex_err_a_summary; run;
  checka_data <- check_results$ex_err_a_summary
  if (is.data.frame(checka_data) && nrow(checka_data) > 0L) {
    # Write column headers
    openxlsx::writeData(wb, "CHECKA", x = as.data.frame(t(colnames(checka_data))),
                        startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "CHECKA",
                       style = styles$ColumnOutline,
                       rows = 1, cols = seq_len(ncol(checka_data)),
                       gridExpand = TRUE)
    # Write data using styled helper
    write_data_table(wb, "CHECKA", checka_data, start_row = 2, styles = styles)
  }

  # --- Write CHECKB worksheet (ex_err_b_summary) ------------------------------
  # SAS: data xls.checkb; set ex_err_b_summary; run;
  checkb_data <- check_results$ex_err_b_summary
  if (is.data.frame(checkb_data) && nrow(checkb_data) > 0L) {
    openxlsx::writeData(wb, "CHECKB", x = as.data.frame(t(colnames(checkb_data))),
                        startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "CHECKB",
                       style = styles$ColumnOutline,
                       rows = 1, cols = seq_len(ncol(checkb_data)),
                       gridExpand = TRUE)
    write_data_table(wb, "CHECKB", checkb_data, start_row = 2, styles = styles)
  }

  # --- Write MISSDOSFRQ worksheet (ex_exdosfrq_missing) -----------------------
  # SAS: data xls.missdosfrq; set ex_exdosfrq_missing; run;
  missdosfrq_data <- check_results$ex_exdosfrq_missing
  if (is.data.frame(missdosfrq_data) && nrow(missdosfrq_data) > 0L) {
    openxlsx::writeData(wb, "MISSDOSFRQ",
                        x = as.data.frame(t(colnames(missdosfrq_data))),
                        startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "MISSDOSFRQ",
                       style = styles$ColumnOutline,
                       rows = 1, cols = seq_len(ncol(missdosfrq_data)),
                       gridExpand = TRUE)
    write_data_table(wb, "MISSDOSFRQ", missdosfrq_data,
                     start_row = 2, styles = styles)
  }

  # --- Write PVA_ERR worksheet (conditional) ----------------------------------
  # SAS: %if not (&dm_arm. and &ex_extrt. and &ex_exdose. and &ex_exdosu.) ...
  if (!is.null(final_exposure_err_d) && is.data.frame(final_exposure_err_d) &&
      nrow(final_exposure_err_d) > 0L) {
    openxlsx::writeData(wb, "PVA_ERR",
                        x = as.data.frame(t(colnames(final_exposure_err_d))),
                        startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "PVA_ERR",
                       style = styles$ColumnOutline,
                       rows = 1, cols = seq_len(ncol(final_exposure_err_d)),
                       gridExpand = TRUE)
    write_data_table(wb, "PVA_ERR", final_exposure_err_d,
                     start_row = 2, styles = styles)
  }

  # --- Write DC_ERR worksheet (conditional) -----------------------------------
  # SAS: %if not (&ex_extrt. and &ex_exdose. and &ex_exdosu.) ...
  if (!is.null(final_exposure_err_e) && is.data.frame(final_exposure_err_e) &&
      nrow(final_exposure_err_e) > 0L) {
    openxlsx::writeData(wb, "DC_ERR",
                        x = as.data.frame(t(colnames(final_exposure_err_e))),
                        startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "DC_ERR",
                       style = styles$ColumnOutline,
                       rows = 1, cols = seq_len(ncol(final_exposure_err_e)),
                       gridExpand = TRUE)
    write_data_table(wb, "DC_ERR", final_exposure_err_e,
                     start_row = 2, styles = styles)
  }

  # --- Write DCINFO worksheet (arm_count metadata) ----------------------------
  # SAS: data xls.dcinfo; data = 'arm_count'; val = &num_arms.; output; run;
  dcinfo <- tibble::tibble(
    data = "arm_count",
    val  = as.numeric(arm_count)
  )
  openxlsx::writeData(wb, "DCINFO", x = dcinfo, startRow = 1, startCol = 1,
                      colNames = TRUE)

  # --- Apply page setup to all data worksheets --------------------------------
  for (sheet_name in c("CHECKA", "CHECKB", "MISSDOSFRQ")) {
    apply_page_setup(wb, sheet_name, orientation = "landscape")
  }

  # --- Ensure output directory exists -----------------------------------------
  output_dir <- dirname(output_file)
  if (nchar(output_dir) > 0L && output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Save workbook ----------------------------------------------------------
  # SAS: libname xls clear;
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)

  return(invisible(output_file))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global macro variable outputs (ex_err_a, ex_err_b) are returned
#      as integer counts within function return lists instead of injecting
#      values into the global environment.
#    - EXDOSFRQ valid list is passed as a character vector argument
#      (was a SAS macro variable &vld_exdosfrq.).
#    - Missing STUDYDAYS detected via max(na.rm = TRUE) + !is.finite()
#      after per-subject aggregation.  R's max() returns -Inf for all-NA
#      groups; SAS returns missing.  Both are caught by this filter.
#    - Missing DOSES detected via is.na() filter, matching SAS WHERE
#      doses IS MISSING.
#    - Input column names are lowercase (arm, usubjid, exdosfrm, exdosfrq,
#      studydays, doses), consistent with downstream CDISC processing.
#    - SAS PCFILES template copy step (commented out in SAS source) is not
#      reproduced; workbook is created fresh using openxlsx.
#    - Arms with zero excluded subjects receive count = 0 and pct = 0 in
#      summary tables, matching SAS CASE WHEN ... IS MISSING THEN 0 logic.
#      This is NOT implicit zero substitution — it correctly represents
#      "zero excluded subjects" rather than a missing value.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - max(na.rm = TRUE) returns -Inf when ALL study-day values are NA;
#      handled explicitly via !is.finite() to match SAS behaviour.
#    - Percentage rounding uses janitor::round_half_up() for SAS-compatible
#      half-up rounding at every computation location (Gate 2 Rounding Audit).
#    - exdosfrq_miss_pct rounded to 1 decimal place; err_a/err_b fractions
#      rounded to 4 decimal places for display precision.
#    - Sort stability: dplyr::arrange() is stable within groups; SAS PROC
#      SQL ORDER BY is also stable.  No differences expected.
# NO DIRECT R EQUIVALENT:
#    - SAS PCFILES LIBNAME -> openxlsx::createWorkbook + saveWorkbook
#    - SAS %global macro variable -> function return value in named list
#    - SAS first.arm BY-group processing -> dplyr::if_else(duplicated())
#    - SAS propcase() function -> stringr::str_to_title()
#    - SAS PUT(x, 8. -L) -> nrow() returning integer directly
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Core data manipulation (AAP mandated; tidyverse over base R)
#    - stringr: str_to_title() for propcase (AAP mandated; tidyverse)
#    - janitor: round_half_up() for SAS-compatible rounding (AAP mandated)
#    - openxlsx: Excel output replacing PCFILES engine (AAP mandated)
#    - tibble: Enhanced data frames for return values (AAP mandated)
#    - xml_output.R: Shared workbook styling engine (internal dependency)
# OPEN QUESTIONS:
#    - EXDOSFRQ valid list: confirm source of vld_exdosfrq values in the
#      calling context (may come from exposure_exdosfrq.csv reference data
#      in tested/SAS/EX/).
#    - max(studydays, na.rm = TRUE) when ALL values are NA -> R returns
#      -Inf; handled as NA-equivalent for exclusion logic.  Confirm this
#      aligns with statistician intent.
#    - SAS PCFILES template formatting: Excel template formatting is not
#      reproduced; styles from xml_output.R style gallery are applied
#      instead.  Verify layout meets regulatory requirements.
#    - Column ordering of blank1/blank2/blank3 in ex_exdosfrq_missing:
#      preserved from SAS RETAIN statement for template compatibility.
# ============================================================
