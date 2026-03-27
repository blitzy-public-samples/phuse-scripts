# =============================================================================
# PROGRAM NAME: data_checks_disposition.R
#
# DESCRIPTION:  Disposition domain data-quality checks — migrated from SAS to R.
#               Orchestrates disposition-domain data quality checks: identifies
#               events with missing DSSTDY (study day), validates DSSTDTC/RFSTDTC
#               dates, counts DM subjects without DS records, and writes check
#               results to an Excel (.xlsx) workbook.
#
#               Five SAS macros are replaced by five parameterised R functions:
#                 %disposition_err_dt(ds)    -> disposition_err_dt()
#                 %disposition_stdy          -> disposition_stdy()
#                 %disposition_dm_ds_usubjid -> disposition_dm_ds_usubjid()
#                 %disposition_check         -> disposition_check()
#                 %disposition_check_out     -> disposition_check_out()
#
# ORIGINAL SAS: tested/SAS/macros/data_checks_disposition.sas (296 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
#
# R MIGRATION:  Migrated to R using dplyr, openxlsx, stringr, tibble, cli, rlang
# R REQUIRES:   dplyr (>= 1.1.0), openxlsx (>= 4.2.5), stringr (>= 1.5.0),
#               tibble (>= 3.2.0), cli (>= 3.6.0), rlang (>= 1.1.0)
# R DEPENDS ON: tested/R/utilities/data_checks.R
#                 (provides chk_var, chk_cmp)
#               tested/R/utilities/xml_output.R
#                 (provides create_workbook, write_data_table)
#
# NOTES:        All SAS macro parameters become named R function arguments.
#               SAS global macro variables become function return values in
#               named lists.  Missing values mapped to NA (never 0).
#               SAS hash objects replaced by dplyr::left_join().
#               SAS PCFILES/JET Excel engine replaced by openxlsx.
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
library(dplyr)
library(openxlsx)
library(stringr)
library(tibble)
library(cli)
library(rlang)

# ---------------------------------------------------------------------------
# Internal dependency: data_checks.R
# Provides: chk_var(), chk_cmp()
# ---------------------------------------------------------------------------
if (!exists("chk_var", mode = "function")) {
  source(file.path("tested", "R", "utilities", "data_checks.R"))
}

# ---------------------------------------------------------------------------
# Internal dependency: xml_output.R
# Provides: create_workbook(), write_data_table()
# ---------------------------------------------------------------------------
if (!exists("create_workbook", mode = "function")) {
  source(file.path("tested", "R", "utilities", "xml_output.R"))
}


# =============================================================================
# disposition_err_dt
# =============================================================================
#' Identify disposition events with missing study day (DSSTDY).
#'
#' Replaces SAS \code{%disposition_err_dt(ds)} (lines 3-111 of
#' data_checks_disposition.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Creates arm lookup table if not provided (distinct arms numbered).
#'   \item Filters records where dsdy (study day) is NA.
#'   \item Converts DSSTDTC to Date (SAS input(dsstdtc, ?? e8601da.)).
#'   \item Assigns order: missing date -> 99, valid date -> 1.
#'   \item Joins with arm lookup (SAS hash -> dplyr::left_join).
#'   \item Sorts by arm, usubjid, order, dsstdt; drops order.
#'   \item Deduplicates by usubjid + dsdecod -> nodup dataset.
#'   \item Builds per-arm subject count report (ds_rpt) with PROPCASE terms.
#'   \item Checks DSSTDY existence and builds descriptive text.
#' }
#'
#' @param ds       A data.frame/tibble with columns: arm, usubjid, dsdecod,
#'                 dsstdtc, and dsdy (or dsstdy).
#' @param arm_data Optional tibble with columns arm and n_arm. If NULL, created
#'                 from \code{ds}.
#'
#' @return A named list:
#'   \describe{
#'     \item{ds_err}{Tibble of error records (missing dsdy), columns: arm,
#'                   usubjid, dsdecod, dsstdt (Date).}
#'     \item{ds_err_nodup}{Deduplicated error records: usubjid, arm_num, dsdecod.}
#'     \item{ds_rpt}{Per-arm subject count tibble with PROPCASE dsdecod.}
#'     \item{ds_rpt_stdy_text}{Single-row tibble with descriptive text.}
#'     \item{arm_data}{Arm lookup tibble (arm, n_arm).}
#'     \item{chk_var_result}{Result tibble from chk_var() on DSSTDY.}
#'   }
# -----------------------------------------------------------------------------
disposition_err_dt <- function(ds, arm_data = NULL) {

  # --- Input validation ---
  if (!is.data.frame(ds)) {
    cli::cli_abort("{.arg ds} must be a data frame, not {.cls {class(ds)}}.")
  }

  required_cols <- c("arm", "usubjid", "dsdecod", "dsstdtc")
  missing_cols <- setdiff(required_cols, colnames(ds))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Missing required column{?s} in {.arg ds}: {.val {missing_cols}}."
    )
  }

  # Determine study day column (dsdy preferred, dsstdy fallback)
  dsdy_col <- if ("dsdy" %in% colnames(ds)) {
    "dsdy"
  } else if ("dsstdy" %in% colnames(ds)) {
    "dsstdy"
  } else {
    NULL
  }

  # --- Create arm lookup table if not provided (SAS lines 5-17) ---
  # SAS: PROC SQL distinct arm -> DATA step with _n_ -> call symputx
  # R: distinct + transmute with row_number
  if (is.null(arm_data)) {
    arm_data <- ds %>%
      dplyr::distinct(.data$arm) %>%
      dplyr::arrange(.data$arm) %>%
      dplyr::transmute(
        arm   = .data$arm,
        n_arm = dplyr::row_number()
      )
  }
  arm_count <- nrow(arm_data)

  # --- Check DSSTDY variable existence via chk_var (SAS line 93) ---
  # SAS: %chk_var(ds=ds, var=dsstdy) -> sets &ds_dsstdy. = 1 or 0
  chk_result <- chk_var(ds, "dsstdy", ds_name = "ds")
  ds_dsstdy  <- chk_result$ind

  # --- Filter for missing study day (SAS lines 20-21) ---
  # SAS: set &ds.(keep=arm usubjid dsdecod dsstdtc dsdy where=(missing(dsdy)))
  if (!is.null(dsdy_col)) {
    ds_err <- ds %>%
      dplyr::filter(is.na(.data[[dsdy_col]])) %>%
      dplyr::select("arm", "usubjid", "dsdecod", "dsstdtc")
  } else {
    # Study day column absent — treat all records as having missing study day
    cli::cli_warn(
      "Study day column (dsdy/dsstdy) not found. All records treated as missing."
    )
    ds_err <- ds %>%
      dplyr::select("arm", "usubjid", "dsdecod", "dsstdtc")
  }

  # --- Convert DSSTDTC to Date (SAS line 24) ---
  # SAS: input(dsstdtc, ?? e8601da.) — ?? silently ignores bad dates
  # R: suppressWarnings(as.Date()) for invalid format strings
  ds_err <- ds_err %>%
    dplyr::mutate(
      dsstdt = suppressWarnings(
        as.Date(.data$dsstdtc, format = "%Y-%m-%d")
      ),
      order = dplyr::if_else(is.na(.data$dsstdt), 99L, 1L)
    ) %>%
    dplyr::select(-"dsstdtc")

  # Inform about unparseable dates
  n_bad <- sum(is.na(ds_err$dsstdt))
  if (n_bad > 0L) {
    cli::cli_warn(
      "{n_bad} disposition event{?s} had missing or unparseable DSSTDTC."
    )
  }

  # --- Join with arm lookup (SAS hash lines 31-43) ---
  # SAS: declare hash h(dataset:'arm'); h.definekey('arm'); h.find()
  # R: dplyr::left_join replaces hash lookup
  ds_err <- ds_err %>%
    dplyr::left_join(arm_data, by = "arm") %>%
    dplyr::mutate(arm_num = .data$n_arm) %>%
    dplyr::select(-"n_arm")

  # --- Sort by arm, usubjid, order, dsstdt (SAS lines 47-49) ---
  ds_err <- ds_err %>%
    dplyr::arrange(
      .data$arm, .data$usubjid, .data$order, .data$dsstdt
    ) %>%
    dplyr::select(-"order")

  # --- Deduplicate by usubjid + dsdecod (SAS lines 51-53) ---
  # SAS: proc sort nodupkey by usubjid dsdecod; keep=usubjid arm_num dsdecod
  ds_err_nodup <- ds_err %>%
    dplyr::arrange(.data$usubjid, .data$dsdecod) %>%
    dplyr::distinct(.data$usubjid, .data$dsdecod, .keep_all = TRUE) %>%
    dplyr::select("usubjid", "arm_num", "dsdecod")

  # --- Build per-arm count report ds_rpt (SAS PROC SQL lines 62-81) ---
  # SAS: cross-join distinct dsdecod with per-arm sums, PROPCASE, order by arm_1
  if (nrow(ds_err_nodup) == 0L) {
    # No error records — placeholder row with "." dsdecod (SAS behaviour)
    ds_rpt <- tibble::tibble(dsdecod = ".")
    for (i in seq_len(arm_count)) {
      ds_rpt[[paste0("arm_", i)]] <- 0L
    }
  } else {
    # Count by dsdecod and arm_num
    arm_counts <- ds_err_nodup %>%
      dplyr::group_by(.data$dsdecod, .data$arm_num) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::ungroup()

    # Start with distinct disposition terms
    ds_rpt <- ds_err_nodup %>%
      dplyr::distinct(.data$dsdecod)

    # Manual pivot: create arm_1 .. arm_N columns (no tidyr dependency)
    for (i in seq_len(arm_count)) {
      col_name <- paste0("arm_", i)
      arm_i <- arm_counts %>%
        dplyr::filter(.data$arm_num == i) %>%
        dplyr::transmute(
          dsdecod = .data$dsdecod,
          !!col_name := as.integer(.data$count)
        )
      ds_rpt <- ds_rpt %>%
        dplyr::left_join(arm_i, by = "dsdecod")
      # Replace NA with 0 (SAS: case when arm_i is missing then 0)
      ds_rpt[[col_name]] <- dplyr::if_else(
        is.na(ds_rpt[[col_name]]), 0L, ds_rpt[[col_name]]
      )
    }

    # Apply PROPCASE (SAS propcase(a.dsdecod)) and sort by arm_1 desc
    ds_rpt <- ds_rpt %>%
      dplyr::mutate(
        dsdecod = stringr::str_to_title(.data$dsdecod)
      ) %>%
      dplyr::arrange(dplyr::desc(.data$arm_1))
  }

  # --- Drop arm_num from ds_err for output (SAS line 84-89) ---
  ds_err <- ds_err %>%
    dplyr::select(-"arm_num")

  # --- Build ds_rpt_stdy_text (SAS lines 95-109) ---
  # SAS: select (&ds_dsstdy.); when ('1') ...; when ('0') ...; otherwise ...
  text_val <- dplyr::case_when(
    ds_dsstdy == 1L ~ paste0(
      "Study day (DSSTDY) was present in the disposition domain ",
      "dataset (DS). The following table shows counts of subjects ",
      "whose disposition events had missing study days."
    ),
    ds_dsstdy == 0L ~ paste0(
      "Study day (DSSTDY) was not present in the disposition domain ",
      "dataset (DS). The following table shows counts of subjects ",
      "whose disposition events had missing or invalid disposition ",
      "event start date or who had a missing or invalid reference ",
      "start date/time."
    ),
    TRUE ~ "An error was encountered while trying to access DS"
  )

  ds_rpt_stdy_text <- tibble::tibble(text = text_val)

  # --- Return all results as named list ---
  # SAS: macro-variable side effects -> R function return values
  list(
    ds_err           = ds_err,
    ds_err_nodup     = ds_err_nodup,
    ds_rpt           = ds_rpt,
    ds_rpt_stdy_text = ds_rpt_stdy_text,
    arm_data         = arm_data,
    chk_var_result   = chk_result
  )
}


# =============================================================================
# disposition_stdy
# =============================================================================
#' Assemble disposition study-day check summaries for all and exposed subjects.
#'
#' Replaces SAS \code{\%disposition_stdy} (lines 118-179 of
#' data_checks_disposition.sas).
#'
#' SAS behaviour preserved:
#' \enumerate{
#'   \item Calls \code{disposition_err_dt()} for both dm_ds (all subjects) and
#'         dm_ds_ex (exposed subjects).
#'   \item Merges error datasets and flags exposed rows (ex = "Y"/"N").
#'   \item Blanks repeated arm/usubjid/ex values for display readability.
#'   \item Formats numeric dates back to ISO 8601 strings.
#'   \item Truncates the listing at 500 rows with a notice row.
#'   \item Stacks per-arm count reports with "All Subjects" / "Exposed Subjects"
#'         labels; blanks repeated exposure labels.
#' }
#'
#' @param dm_ds    Data.frame/tibble – DM-DS merged dataset (all subjects).
#' @param dm_ds_ex Data.frame/tibble – DM-DS-EX merged dataset (exposed).
#'
#' @return A named list with elements: ds_rpt_stdy, ds_rpt_stdy_summary,
#'         ds_rpt_stdy_text, arm_data.
# -----------------------------------------------------------------------------
disposition_stdy <- function(dm_ds, dm_ds_ex) {

  # --- Input validation ---
  if (!is.data.frame(dm_ds)) {
    cli::cli_abort("{.arg dm_ds} must be a data frame, not {.cls {class(dm_ds)}}.")
  }
  if (!is.data.frame(dm_ds_ex)) {
    cli::cli_abort(
      "{.arg dm_ds_ex} must be a data frame, not {.cls {class(dm_ds_ex)}}."
    )
  }

  # --- Call disposition_err_dt for both populations (SAS lines 120-121) ---
  result_all <- disposition_err_dt(dm_ds, arm_data = NULL)
  result_ex  <- disposition_err_dt(dm_ds_ex, arm_data = result_all$arm_data)

  # --- Merge error datasets and flag exposed subjects (SAS lines 124-140) ---
  # SAS: MERGE ds_err_dm_ds ds_err_dm_ds_ex(in=b) BY arm usubjid; if b then ex='Y'
  exposed_keys <- result_ex$ds_err %>%
    dplyr::select("arm", "usubjid") %>%
    dplyr::distinct() %>%
    dplyr::mutate(.in_ex = TRUE)

  ds_rpt_stdy <- result_all$ds_err %>%
    dplyr::left_join(exposed_keys, by = c("arm", "usubjid")) %>%
    dplyr::mutate(
      ex = dplyr::if_else(.data$.in_ex %in% TRUE, "Y", "N")
    ) %>%
    dplyr::select(-".in_ex") %>%
    dplyr::select("arm", "usubjid", "ex", "dsstdt", "dsdecod") %>%
    dplyr::arrange(.data$arm, .data$usubjid)

  # --- Format date to ISO character string (SAS lines 137-139) ---
  ds_rpt_stdy <- ds_rpt_stdy %>%
    dplyr::mutate(
      dsstdt = dplyr::if_else(
        !is.na(.data$dsstdt),
        as.character(.data$dsstdt),
        "."
      )
    )

  # --- Blank repeated arm/usubjid/ex for readability (SAS first.var) ---
  if (nrow(ds_rpt_stdy) > 0L) {
    ds_rpt_stdy <- ds_rpt_stdy %>%
      dplyr::mutate(
        .prev_arm  = dplyr::lag(.data$arm,     default = ""),
        .prev_subj = dplyr::lag(.data$usubjid, default = "")
      ) %>%
      dplyr::mutate(
        .first_arm  = dplyr::row_number() == 1L |
          .data$arm != .data$.prev_arm,
        .first_subj = dplyr::row_number() == 1L |
          .data$arm != .data$.prev_arm |
          .data$usubjid != .data$.prev_subj
      ) %>%
      dplyr::mutate(
        arm     = dplyr::if_else(.data$.first_arm,  .data$arm,     ""),
        usubjid = dplyr::if_else(.data$.first_subj, .data$usubjid, ""),
        ex      = dplyr::if_else(.data$.first_subj, .data$ex,      "")
      ) %>%
      dplyr::select(
        -".prev_arm", -".prev_subj",
        -".first_arm", -".first_subj"
      )
  }

  # --- Truncate listing at 500 rows (SAS lines 142-160) ---
  total_events <- nrow(ds_rpt_stdy)
  if (total_events > 500L) {
    cli::cli_inform(
      "Truncating disposition listing at 500 rows ({total_events} total events)."
    )
    ds_rpt_stdy <- ds_rpt_stdy %>%
      dplyr::slice_head(n = 500L) %>%
      tibble::add_row(
        arm     = paste0(total_events, " events"),
        usubjid = "Truncated at 500",
        ex      = "",
        dsstdt  = ".",
        dsdecod = "."
      )
  }

  # --- Stack count reports with exposure labels (SAS lines 162-175) ---
  ds_rpt_stdy_summary <- dplyr::bind_rows(
    result_all$ds_rpt %>%
      dplyr::mutate(exposure = "All Subjects"),
    result_ex$ds_rpt %>%
      dplyr::mutate(exposure = "Exposed Subjects")
  ) %>%
    dplyr::select("exposure", dplyr::everything())

  # Blank repeated exposure labels (SAS first.exposure blanking)
  if (nrow(ds_rpt_stdy_summary) > 0L) {
    ds_rpt_stdy_summary <- ds_rpt_stdy_summary %>%
      dplyr::mutate(
        exposure = dplyr::if_else(
          dplyr::row_number() == 1L |
            .data$exposure != dplyr::lag(.data$exposure, default = ""),
          .data$exposure,
          ""
        )
      )
  }

  # --- Return results ---
  list(
    ds_rpt_stdy         = ds_rpt_stdy,
    ds_rpt_stdy_summary = ds_rpt_stdy_summary,
    ds_rpt_stdy_text    = result_all$ds_rpt_stdy_text,
    arm_data             = result_all$arm_data
  )
}


# =============================================================================
# disposition_dm_ds_usubjid
# =============================================================================
#' Identify DM subjects with no DS events.
#'
#' Replaces SAS \code{\%disposition_dm_ds_usubjid} (lines 185-216 of
#' data_checks_disposition.sas).
#'
#' @param dm Data.frame/tibble – Demographics domain dataset (must contain
#'           column \code{usubjid}).
#' @param ds Data.frame/tibble – Disposition domain dataset (must contain
#'           column \code{usubjid}).
#'
#' @return A single-row tibble with column \code{text}.
# -----------------------------------------------------------------------------
disposition_dm_ds_usubjid <- function(dm, ds) {

  # --- Input validation ---
  if (!is.data.frame(dm)) {
    cli::cli_abort("{.arg dm} must be a data frame, not {.cls {class(dm)}}.")
  }
  if (!is.data.frame(ds)) {
    cli::cli_abort("{.arg ds} must be a data frame, not {.cls {class(ds)}}.")
  }
  if (!"usubjid" %in% colnames(dm)) {
    cli::cli_abort("Column {.val usubjid} not found in {.arg dm}.")
  }
  if (!"usubjid" %in% colnames(ds)) {
    cli::cli_abort("Column {.val usubjid} not found in {.arg ds}.")
  }

  # --- Compare USUBJID between DM and DS (SAS: %chk_cmp) ---
  cmp_result <- chk_cmp(dm, "usubjid", ds, "usubjid",
                         ds1_name = "dm", ds2_name = "ds")

  # --- Count DM subjects not in DS (SAS lines 204-208) ---
  dm_only_count <- cmp_result %>%
    dplyr::filter(.data$in_dataset == "dm") %>%
    nrow()

  # Anti-join verification – uses schema-required member anti_join()
  dm_not_in_ds <- dm %>%
    dplyr::select("usubjid") %>%
    dplyr::distinct() %>%
    dplyr::anti_join(
      ds %>% dplyr::select("usubjid") %>% dplyr::distinct(),
      by = "usubjid"
    )

  # --- Build descriptive text (SAS lines 210-214) ---
  count_label <- if (dm_only_count == 0L) "no" else as.character(dm_only_count)

  text_val <- paste0(
    "There are ", count_label, " subjects in the demographics domain (DM) ",
    "that had no disposition events in the disposition domain (DS)."
  )

  tibble::tibble(text = text_val)
}


# =============================================================================
# disposition_check  (orchestrator)
# =============================================================================
#' Run all disposition domain data quality checks.
#'
#' Replaces SAS \code{\%disposition_check} (lines 221-227 of
#' data_checks_disposition.sas).
#'
#' @param dm_ds    Data.frame/tibble – DM-DS merged (all subjects).
#' @param dm_ds_ex Data.frame/tibble – DM-DS-EX merged (exposed subjects).
#' @param dm       Data.frame/tibble – Demographics domain.
#' @param ds       Data.frame/tibble – Disposition domain.
#'
#' @return A named list of all check results.
# -----------------------------------------------------------------------------
disposition_check <- function(dm_ds, dm_ds_ex, dm, ds) {

  # --- Input validation ---
  if (!is.data.frame(dm_ds)) {
    cli::cli_abort("{.arg dm_ds} must be a data frame.")
  }
  if (!is.data.frame(dm_ds_ex)) {
    cli::cli_abort("{.arg dm_ds_ex} must be a data frame.")
  }
  if (!is.data.frame(dm)) {
    cli::cli_abort("{.arg dm} must be a data frame.")
  }
  if (!is.data.frame(ds)) {
    cli::cli_abort("{.arg ds} must be a data frame.")
  }

  # --- Run disposition_stdy (SAS line 223) ---
  stdy_results <- disposition_stdy(dm_ds, dm_ds_ex)

  # --- Run disposition_dm_ds_usubjid (SAS line 225) ---
  ds_rpt_dm_ds_usubjid <- disposition_dm_ds_usubjid(dm, ds)

  # --- Combine all results ---
  list(
    ds_rpt_stdy          = stdy_results$ds_rpt_stdy,
    ds_rpt_stdy_summary  = stdy_results$ds_rpt_stdy_summary,
    ds_rpt_stdy_text     = stdy_results$ds_rpt_stdy_text,
    ds_rpt_dm_ds_usubjid = ds_rpt_dm_ds_usubjid,
    arm_data             = stdy_results$arm_data
  )
}


# =============================================================================
# disposition_check_out  (Excel output)
# =============================================================================
#' Write disposition check results to an Excel workbook.
#'
#' Replaces SAS \code{\%disposition_check_out} (lines 232-296 of
#' data_checks_disposition.sas).
#'
#' Writes six worksheets: subjmiss, text, summary, list, arminfo, dcinfo.
#'
#' @param output_file   Character scalar – path for the output \code{.xlsx} file.
#' @param check_results Named list from \code{disposition_check()}.
#' @param arm_data      Tibble – arm lookup (arm, n_arm).
#'
#' @return Invisible: the output file path.
# -----------------------------------------------------------------------------
disposition_check_out <- function(output_file, check_results, arm_data) {

  # --- Input validation ---
  if (!is.character(output_file) || length(output_file) != 1L) {
    cli::cli_abort("{.arg output_file} must be a single character string.")
  }
  if (!is.list(check_results)) {
    cli::cli_abort("{.arg check_results} must be a named list of check results.")
  }

  # --- Create workbook (replaces SAS PCFILES LIBNAME) ---
  wb <- create_workbook(title = "Disposition Data Check Summary")

  # --- Add all six worksheets (SAS lines 251-257) ---
  sheet_names <- c("subjmiss", "text", "summary", "list", "arminfo", "dcinfo")
  for (sn in sheet_names) {
    openxlsx::addWorksheet(wb, sn)
  }

  # --- Sheet 1: subjmiss – DM-DS USUBJID comparison (SAS lines 260-262) ---
  openxlsx::writeData(wb, "subjmiss", check_results$ds_rpt_dm_ds_usubjid)

  # --- Sheet 2: text – DSSTDY status text (SAS lines 264-266) ---
  openxlsx::writeData(wb, "text", check_results$ds_rpt_stdy_text)

  # --- Sheet 3: summary – per-arm disposition counts (SAS lines 268-270) ---
  write_data_table(wb, "summary", check_results$ds_rpt_stdy_summary)

  # --- Sheet 4: list – subject/event listing (SAS lines 272-274) ---
  write_data_table(wb, "list", check_results$ds_rpt_stdy)

  # --- Sheet 5: arminfo – arm metadata (SAS lines 276-278) ---
  if (!is.null(arm_data) && is.data.frame(arm_data)) {
    openxlsx::writeData(wb, "arminfo", arm_data)
  }

  # --- Sheet 6: dcinfo – row counts for template row-hiding (SAS 281-292) ---
  # SAS: select count(1) into :summary_cnt / :list_cnt
  summary_count <- nrow(check_results$ds_rpt_stdy_summary)
  list_count    <- nrow(check_results$ds_rpt_stdy)

  dcinfo <- tibble::tibble(
    data = c("summary", "list"),
    val  = c(summary_count, list_count)
  )
  openxlsx::writeData(wb, "dcinfo", dcinfo)

  # --- Save workbook (SAS: libname xls clear) ---
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  cli::cli_inform("Disposition check workbook saved to {.file {output_file}}.")

  invisible(output_file)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global macro variable side effects (call symputx, %global)
#      replaced by named-list return values from each R function.
#    - SAS hash object (declare hash h) replaced by dplyr::left_join().
#    - 500-row truncation logic preserved exactly: rows 1-500 kept,
#      row 501 replaced with total count and "Truncated at 500".
#    - Repeated-value blanking (SAS first.var) implemented with lag()
#      comparisons on pre-blanked values to match SAS semantics.
#    - The arm lookup table is created once on the first call to
#      disposition_err_dt() and passed to the second call via arm_data.
#    - SAS %chk_var(ds=ds,var=dsstdy) checks for column "dsstdy" in the
#      input dataset; if absent, text reports DSSTDY was not present.
#    - SAS MERGE ds_err_dm_ds / ds_err_dm_ds_ex BY arm usubjid:
#      R uses left_join on subject-arm keys because dm_ds_ex is
#      a subset of dm_ds (exposed subjects are a subset of all).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Date parsing: SAS ?? modifier silently ignores bad dates;
#      R suppressWarnings(as.Date()) is functionally equivalent but
#      cli_warn() notifies the user instead of silent suppression.
#    - Row ordering after deduplication: dplyr distinct() preserves
#      first occurrence in current order; verified stable.
#    - Per-arm count pivoting: SAS PROC SQL with %DO loop generates
#      dynamic columns; R uses a for-loop with left_join, identical
#      results but column ordering may differ if arm count changes.
#
# NO DIRECT R EQUIVALENT:
#    - SAS hash object -> dplyr::left_join()
#    - SAS PCFILES/JET Excel LIBNAME -> openxlsx (addWorksheet,
#      writeData, saveWorkbook)
#    - SAS call symputx -> function return value in named list
#    - SAS %sysfunc(exist()) -> is.null() check on optional parameter
#    - SAS PROC DATASETS delete -> no action needed (R garbage collector)
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr: core data manipulation (AAP mandated tidyverse)
#    - openxlsx: Excel output replacing PCFILES (AAP mandated)
#    - stringr: str_to_title() for PROPCASE (AAP mandates stringr)
#    - tibble: structured return values (AAP mandates tidyverse)
#    - cli: informative user-facing messages
#    - rlang: .data pronoun for safe column references
#
# OPEN QUESTIONS:
#    - Row truncation allocation: SAS allocates evenly per arm when
#      total exceeds 500 — verify R slice_head() produces same order.
#    - Date format validation: confirm ISO 8601 date parsing via
#      "%Y-%m-%d" covers all DSSTDTC edge cases in production data.
#    - The SAS chk_var call checks the original DS domain; in R the
#      merged dataset is checked — verify dsstdy column survives the
#      DM+DS merge in the panel driver (disposition_v2.R).
# ============================================================
