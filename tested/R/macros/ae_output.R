# =============================================================================
# PROGRAM NAME: ae_output.R
#
# DESCRIPTION:  AE Severity Panel Excel workbook generator. Creates a multi-
#               worksheet Excel (.xlsx) workbook containing:
#                 - Cover/Front Page with study metadata and analysis descriptions
#                 - Analysis A: Adverse Events by Arm >2%
#                 - Analysis B: Serious Adverse Events by Arm (conditional on AESER)
#                 - Analysis C: AEs by Severity (conditional on AESEV)
#                 - Analysis D: Serious AEs by Severity (conditional on AESER+AESEV)
#                 - Note tabs when AESER/AESEV unavailable
#                 - Data Check Summary with subject/error validation tables
#                 - Optional Grouping and Subsetting tab
#
# ORIGINAL SAS: tested/SAS/macros/ae_output.sas (1529 lines)
# SAS AUTHOR:   PhUSE CS Working Group 5
#
# SAS MACROS MIGRATED:
#   %out_cover     (lines 14-198)   -> ae_out_cover()
#   %out_ab        (lines 205-477)  -> ae_out_ab()
#   %out_cd        (lines 483-739)  -> ae_out_cd()
#   %out_err       (lines 745-1292) -> ae_out_err()
#   %out_note      (lines 1298-1360)-> ae_out_note()
#   %ws            (lines 1366-1396)-> ae_out_ws()
#   %out_ae_styles (lines 1400-1443)-> ae_out_styles()
#   %out_ae        (lines 1449-1529)-> ae_out_workbook()
#
# R REQUIRES:   openxlsx (>= 4.2.5), dplyr (>= 1.1.0), janitor (>= 2.2.0),
#               purrr (>= 1.0.0)
#
# INTERNAL DEPENDENCIES:
#   tested/R/utilities/xml_output.R   — create_workbook(), create_workbook_styles(),
#                                       write_formatted_data(), write_header_rows(),
#                                       write_data_table(), apply_page_setup(),
#                                       get_style_by_name()
#   tested/R/utilities/sl_gs_output.R — group_subset_write_ws()
# =============================================================================

# Load required packages
library(openxlsx)
library(dplyr)
library(janitor)
library(purrr)

# Source internal dependencies from the utilities directory
local({
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )
  util_dir <- if (!is.null(this_dir)) {
    file.path(dirname(this_dir), "utilities")
  } else {
    fp <- "tested/R/utilities"
    if (dir.exists(fp)) fp else file.path("..", "utilities")
  }

  xml_path <- file.path(util_dir, "xml_output.R")
  if (file.exists(xml_path) && !exists("create_workbook", mode = "function")) {
    source(xml_path, local = FALSE)
  }

  sl_path <- file.path(util_dir, "sl_gs_output.R")
  if (file.exists(sl_path) && !exists("group_subset_write_ws", mode = "function")) {
    source(sl_path, local = FALSE)
  }
})


# =============================================================================
# ae_out_styles
# =============================================================================
#' Create AE-specific style gallery extending the base xml_output.R styles.
#'
#' Replaces SAS \code{%out_ae_styles} (lines 1400-1443 of ae_output.sas).
#' Generates a named list of openxlsx style objects with:
#'   - Base styles: D (data), D0_R1/D0_R2/D0_R4 (integer right-aligned with
#'     indent), D1_R1/D1_R2 (1-decimal right-aligned), DT (top-aligned wrap),
#'     D0_R1T/D1_R1T (top-aligned numeric)
#'   - Border combinations: For each base style, 7 border variants are created
#'     (_BL, _BR, _BB, _BLR, _BLB, _BRB, _BLRB) matching SAS do-loop
#'     cartesian product of BL, BR, BB flags
#'
#' @param base_size Numeric. Font size in points. Defaults to 9.
#'
#' @return A named list of openxlsx style objects. Includes both AE-specific
#'         styles and base styles from \code{create_workbook_styles()}.
#'
#' @export
ae_out_styles <- function(base_size = 9) {
  # Validate input

if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0) {
    stop("'base_size' must be a single positive number.", call. = FALSE)
  }

  # Start with base style gallery from xml_output.R
  styles <- create_workbook_styles(base_size = base_size)

  # -------------------------------------------------------------------------
  # Define AE-specific parent styles
  # SAS: %xml_style_dcl; ID = 'D'; output; ... (lines 1402-1417)
  # Each parent defines: NumFmt, HA (horizontal align), Indent, VA, Wrap
  # -------------------------------------------------------------------------
  parent_defs <- list(
    D = list(
      fontSize = base_size, valign = "top"
    ),
    D0_R1 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0",
      indent = 1
    ),
    D0_R2 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0",
      indent = 2
    ),
    D0_R4 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0",
      indent = 4
    ),
    D1_R1 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.0",
      indent = 1
    ),
    D1_R2 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.0",
      indent = 2
    ),
    DT = list(
      fontSize = base_size, valign = "top", wrapText = TRUE
    ),
    D0_R1T = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0",
      indent = 1
    ),
    D1_R1T = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.0",
      indent = 1
    )
  )

  # -------------------------------------------------------------------------
  # First, create parent styles without borders and add them to the list
  # so they can be used as fallback or reference styles
  # -------------------------------------------------------------------------
  for (parent_name in names(parent_defs)) {
    base_params <- parent_defs[[parent_name]]
    style_params <- base_params
    style_params$indent <- NULL
    styles[[parent_name]] <- do.call(openxlsx::createStyle, style_params)
  }

  # -------------------------------------------------------------------------
  # Generate border variant cartesian product for each parent
  # SAS: do BL = 0 to 1; do BR = 0 to 1; do BB = 0 to 1;
  #        ID = trim(ParentID)||'_B'; if BL then ID||'L'; ...
  # This produces 7 variants per parent (excluding BL=0,BR=0,BB=0)
  # -------------------------------------------------------------------------
  for (parent_name in names(parent_defs)) {
    base_params <- parent_defs[[parent_name]]

    for (bl in 0:1) {
      for (br in 0:1) {
        for (bb in 0:1) {
          if (bl == 0 && br == 0 && bb == 0) next

          # Build style name: e.g., D_BLR, D0_R2_BL, DT_BLRB
          suffix <- "_B"
          if (bl == 1) suffix <- paste0(suffix, "L")
          if (br == 1) suffix <- paste0(suffix, "R")
          if (bb == 1) suffix <- paste0(suffix, "B")
          style_name <- paste0(parent_name, suffix)

          # Build border specification
          borders <- character(0)
          if (bl == 1) borders <- c(borders, "Left")
          if (br == 1) borders <- c(borders, "Right")
          if (bb == 1) borders <- c(borders, "Bottom")

          # Construct style parameters with borders
          style_params <- base_params
          # Remove indent since openxlsx createStyle does not support indent directly
          style_params$indent <- NULL
          style_params$border <- borders
          style_params$borderStyle <- "thin"

          styles[[style_name]] <- do.call(openxlsx::createStyle, style_params)
        }
      }
    }
  }

  return(styles)
}


# =============================================================================
# ae_out_ws — Helper to add worksheet with column widths
# =============================================================================
#' Add a worksheet to a workbook and configure column widths.
#'
#' Replaces SAS \code{%ws(&ds., keycolwidth=200, colwidth=61.5)} macro
#' (lines 1366-1396 of ae_output.sas). Creates the worksheet, then sets
#' per-column widths.
#'
#' @param wb          An openxlsx workbook object.
#' @param sheet_name  Character. The tab/worksheet name.
#' @param col_widths  Numeric vector. Width of each column in character units.
#'                    If NULL, no explicit widths are set (Excel auto-sizes).
#'
#' @return The worksheet name (invisible), for convenience.
#' @keywords internal
ae_out_ws <- function(wb, sheet_name, col_widths = NULL) {
  if (is.null(wb)) {
    stop("'wb' must be a valid openxlsx workbook object.", call. = FALSE)
  }
  if (!is.character(sheet_name) || length(sheet_name) != 1L) {
    stop("'sheet_name' must be a single character string.", call. = FALSE)
  }

  openxlsx::addWorksheet(wb, sheetName = sheet_name)

  if (!is.null(col_widths) && length(col_widths) > 0) {
    openxlsx::setColWidths(
      wb, sheet = sheet_name,
      cols = seq_along(col_widths),
      widths = col_widths
    )
  }

  return(invisible(sheet_name))
}


# =============================================================================
# ae_out_cover
# =============================================================================
#' Write the AE Severity Panel Cover / Front Page worksheet.
#'
#' Replaces SAS \code{%out_cover} (lines 14-198 of ae_output.sas). Creates
#' a worksheet with study metadata, descriptions of the 4 analyses, method
#' and calculation notes, report settings, and a crossover study note.
#'
#' @param wb              An openxlsx workbook object.
#' @param ndabla          Character. NDA/BLA identifier.
#' @param studyid         Character. Study identifier.
#' @param rundate         Character. Analysis run date string.
#' @param arm_count       Integer. Number of treatment arms.
#' @param arm_names       Character vector of treatment arm display names.
#' @param vld_sw          Logical. Whether date-based validation was performed.
#' @param study_lag       Integer. Days of lag after last exposure for AE window.
#' @param dm_actarm       Logical. Whether ACTARM (TRUE) or ARM (FALSE) was used.
#' @param ae_aeser        Logical. Whether AESER variable is available.
#' @param ae_aesev        Logical. Whether AESEV variable is available.
#' @param sl_group_desc   Character. Grouping description.
#' @param sl_subset_desc  Character. Subsetting description.
#' @param sl_gs_desc      Character. Combined GS description.
#' @param sl_custom_ds    Character. Custom dataset names.
#' @param sl_group_nobs   Integer. Number of group observations.
#' @param sl_subset_nobs  Integer. Number of subset observations.
#' @param styles          Named list of openxlsx style objects.
#'
#' @return Integer. The next available row (invisible).
#' @keywords internal
ae_out_cover <- function(wb, ndabla, studyid, rundate, arm_count, arm_names,
                         vld_sw, study_lag, dm_actarm = FALSE,
                         ae_aeser, ae_aesev,
                         sl_group_desc = "No grouping",
                         sl_subset_desc = "No subsetting",
                         sl_gs_desc = "",
                         sl_custom_ds = "",
                         sl_group_nobs = 0L,
                         sl_subset_nobs = 0L,
                         styles) {
  sheet_name <- "Front Page"

  # Set up worksheet with a single wide key column
  col_widths <- rep(8, 11)  # 11 columns at ~8 char units each
  col_widths[1] <- 50       # key column is wider
  ae_out_ws(wb, sheet_name, col_widths)

  current_row <- 1L

  # --- Header section: Title, NDA/BLA, Study, Run Date ---
  # Blank row
  current_row <- current_row + 1L

  # Title: "AE Severity Panel Front Page"
  openxlsx::writeData(wb, sheet_name, x = "AE Severity Panel Front Page",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Header"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # NDA/BLA, Study, Run date
  info_lines <- c(
    paste0("NDA/BLA: ", ndabla),
    paste0("Study: ", studyid),
    paste0("Analysis run date: ", rundate)
  )
  for (line in info_lines) {
    openxlsx::writeData(wb, sheet_name, x = line,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
    current_row <- current_row + 1L
  }

  # Blank row
  current_row <- current_row + 1L

  # --- Analysis descriptions (1-4) ---
  analyses <- list(
    list(
      title = "1. Adverse Events by Arm Greater than 2%",
      desc = paste0(
        "This analysis shows all adverse events that occur ",
        "in more than 2% of subjects in any treatment arm. Each adverse event is ",
        "counted only once per subject."
      )
    ),
    list(
      title = "2. Serious Adverse Events by Arm",
      desc = paste0(
        "This analysis shows all adverse events that were considered serious. ",
        "Calculations are performed the same as Analysis 1, except ",
        "only adverse events with a 'Y' in the AESER variable from the AE ",
        "dataset are used."
      )
    ),
    list(
      title = "3. Adverse Events by Severity",
      desc = paste0(
        "This analysis shows all adverse events in the study and the ",
        "number of times they occur by arm and severity level, using the ",
        "AESEV variable from the AE dataset if it is available."
      )
    ),
    list(
      title = "4. Serious Adverse Events by Severity",
      desc = paste0(
        "This analysis shows all adverse events that were considered serious ",
        "by arm and severity level. Calculations are performed the same as ",
        "Analysis 1, except only adverse events with a ",
        "'Y' in the AESER variable from the AE dataset are used."
      )
    )
  )

  for (analysis in analyses) {
    # Title line (SubHeader style)
    openxlsx::writeData(wb, sheet_name, x = analysis$title,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "SubHeader"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
    current_row <- current_row + 1L

    # Description line (Default10Wrap style)
    openxlsx::writeData(wb, sheet_name, x = analysis$desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
    # Auto-height based on text length (~100 chars/line)
    height_est <- max(1, ceiling(nchar(analysis$desc) / 100)) * 12.75
    openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                            heights = height_est)
    current_row <- current_row + 1L

    # Blank row
    current_row <- current_row + 1L
  }

  # Extra blank row
  current_row <- current_row + 1L

  # --- Method and Calculations section ---
  openxlsx::writeData(wb, sheet_name, x = "Method and Calculations",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
  current_row <- current_row + 1L

  # Build method text matching SAS logic
  arm_text <- if (isTRUE(dm_actarm)) {
    "actual treatment arm (ACTARM)"
  } else {
    "planned treatment arm (ARM)"
  }

  method_text <- paste0(
    "For all analyses in this report, an adverse event is determined by the body system or ",
    "organ class (AEBODSYS) and dictionary-defined term (AEDECOD) from the ",
    "adverse event (AE) dataset. "
  )

  if (isTRUE(vld_sw)) {
    lag_text <- if (study_lag != 0) paste0(study_lag, " days after ") else ""
    method_text <- paste0(
      method_text,
      "Only adverse events with a start date between subjects' ",
      "first exposure and ", lag_text,
      "subjects' last exposure are included in the analysis. ",
      "Exposure dates are taken from variables EXSTDTC and EXENDTC in the exposure (EX) dataset; ",
      "if these dates are not available, the subjects' reference start and end dates ",
      "(RFSTDTC and RFENDTC) from the demographics (DM) dataset are used instead. "
    )
  }
  method_text <- paste0(method_text, "Treatment arm is determined using the ", arm_text, " from DM.")

  openxlsx::writeData(wb, sheet_name, x = method_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
  height_est <- max(1, ceiling(nchar(method_text) / 100)) * 12.75
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                          heights = height_est)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # --- Report Settings panel ---
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = "Report Settings",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  settings <- list(
    list(desc = "   NDA/BLA:", setting = ndabla),
    list(desc = "   Study:", setting = studyid),
    list(desc = "   Analysis run date:", setting = rundate)
  )
  for (s in settings) {
    openxlsx::writeData(wb, sheet_name, x = s$desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, sheet_name, x = s$setting,
                        startRow = current_row, startCol = 2, colNames = FALSE)
    current_row <- current_row + 1L
  }

  # Blank row
  current_row <- current_row + 1L

  # Custom datasets
  custom_ds_text <- if (length(sl_custom_ds) > 0 && any(nchar(sl_custom_ds) > 0)) {
    paste(sl_custom_ds, collapse = ", ")
  } else {
    "None"
  }
  openxlsx::writeData(wb, sheet_name, x = "   Custom datasets:",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, x = custom_ds_text,
                      startRow = current_row, startCol = 2, colNames = FALSE)
  current_row <- current_row + 1L

  # Grouping/subsetting
  gs_desc_text <- if (nchar(sl_gs_desc) > 0) sl_gs_desc else "None"
  openxlsx::writeData(wb, sheet_name, x = "   Grouping/subsetting:",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, x = gs_desc_text,
                      startRow = current_row, startCol = 2, colNames = FALSE)
  current_row <- current_row + 1L

  if (sl_group_nobs > 0 || sl_subset_nobs > 0) {
    openxlsx::writeData(
      wb, sheet_name,
      x = "For more information, see the Grouping and Subsetting tab at the end of this workbook",
      startRow = current_row, startCol = 2, colNames = FALSE
    )
    current_row <- current_row + 1L
  }

  # Blank row
  current_row <- current_row + 1L

  # Study analysis period
  if (isTRUE(vld_sw)) {
    lag_suffix <- if (study_lag > 0) paste0(" + ", study_lag, " days") else ""
    period_text <- paste0("Subject first exposure date to last exposure date", lag_suffix)
  } else {
    period_text <- "Necessary date variables were not available; all adverse events used in analysis"
  }
  openxlsx::writeData(wb, sheet_name, x = "   Study analysis period: ",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, x = period_text,
                      startRow = current_row, startCol = 2, colNames = FALSE)
  current_row <- current_row + 1L

  # Two blank rows
  current_row <- current_row + 2L

  # Crossover note (red italic)
  crossover_note <- paste0(
    "Note that for crossover studies, the analysis by arm in this report ",
    "can only be used to examine treatment sequences and not individual treatments."
  )
  openxlsx::writeData(wb, sheet_name, x = crossover_note,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10RedWrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = current_row)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 25.5)
  current_row <- current_row + 1L

  # --- Page setup ---
  apply_page_setup(
    wb, sheet_name,
    orientation = "portrait",
    header_left = "AE Severity Front Page",
    header_right = paste0("NDA/BLA ", ndabla, "\nStudy ", studyid),
    footer_center = "Page &P of &N",
    scale = 95
  )

  # Create print area named range
  openxlsx::createNamedRegion(
    wb, sheet_name,
    cols = 1:11, rows = 1:40,
    name = "Print_Area"
  )

  return(invisible(current_row))
}


# =============================================================================
# ae_out_ab
# =============================================================================
#' Write an AE-by-Arm worksheet (Analysis A or B).
#'
#' Replaces SAS \code{%out_ab(rpt=)} (lines 205-477 of ae_output.sas).
#' Creates worksheets for:
#'   - Analysis A: All AEs by Arm (adverse events >2% of subjects in any arm)
#'   - Analysis B: Serious AEs by Arm (AESER = Y subset)
#'
#' Worksheet structure: header section (title, NDA/BLA, study), multi-row
#' column headers (arm names, N=, Subject Count/%), data rows sorted by
#' aebodsys + descending arm_pct_total, footer notes, frozen panes,
#' auto-filter, alternate-row conditional formatting.
#'
#' @param wb             An openxlsx workbook object.
#' @param ab_output      A data.frame/tibble with columns: aebodsys, aedecod,
#'                       arm\{i\}_sum (count per arm), arm\{i\}_pct (% per arm),
#'                       sum_total (total count), pct_total (total %).
#' @param analysis       Character. "a" or "b" controlling which report is made.
#' @param arm_count      Integer. Number of treatment arms.
#' @param arm_names      Character vector. Treatment arm display names.
#' @param arm_subjcnt    Integer vector. Subject count per arm.
#' @param arm_total      Integer. Total subjects across all arms.
#' @param ndabla         Character. NDA/BLA identifier.
#' @param studyid        Character. Study identifier.
#' @param rundate        Character. Analysis run date.
#' @param vld_sw         Logical. Whether date-based validation was performed.
#' @param study_lag      Integer. Study lag days.
#' @param ae_aeser       Logical. Whether AESER is available.
#' @param sl_gs_desc     Character. Combined GS description.
#' @param sl_group_nobs  Integer. Number of grouping observations.
#' @param sl_subset_nobs Integer. Number of subsetting observations.
#' @param all_ae_dm_ex_aeser_y Logical. Whether all AEs marked serious.
#' @param all_ae_dm_ex_aeser_n Logical. Whether no AEs marked non-serious.
#' @param max_arm_nm_len Integer. Max length of arm name strings.
#' @param styles         Named list of openxlsx style objects.
#'
#' @return Integer. The next available row (invisible).
#' @keywords internal
ae_out_ab <- function(wb, ab_output, analysis = "a",
                      arm_count, arm_names, arm_subjcnt, arm_total,
                      ndabla, studyid, rundate,
                      vld_sw = TRUE, study_lag = 0,
                      ae_aeser = TRUE,
                      sl_gs_desc = "", sl_group_nobs = 0L, sl_subset_nobs = 0L,
                      all_ae_dm_ex_aeser_y = FALSE, all_ae_dm_ex_aeser_n = FALSE,
                      max_arm_nm_len = 20L,
                      styles) {

  analysis <- toupper(analysis)

  # Sheet title and long title
  if (analysis == "A") {
    sheet_name <- "1 AEs by Arm"
    title_long <- "Adverse Events by Organ Class and Term"
  } else {
    sheet_name <- "2 Serious AEs by Arm"
    title_long <- "Serious Adverse Events by Organ Class and Term"
  }

  nobs <- nrow(ab_output)
  nvars <- ncol(ab_output)
  nkeycols <- 2L  # aebodsys, aedecod

  # --- Set up worksheet ---
  # Key columns = 200px ~ 28 char, data columns = 61.5px ~ 8.5 char
  key_width <- 28
  data_width <- 8.5
  col_widths <- c(rep(key_width, nkeycols), rep(data_width, nvars - nkeycols))
  ae_out_ws(wb, sheet_name, col_widths)

  current_row <- 1L

  # --- Header section ---
  # Blank row
  current_row <- current_row + 1L

  # Title
  openxlsx::writeData(wb, sheet_name, x = title_long,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Header"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Grouping/subsetting description if available
  if (sl_group_nobs > 0 || sl_subset_nobs > 0) {
    openxlsx::writeData(wb, sheet_name, x = sl_gs_desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10"),
                       rows = current_row, cols = 1)
    current_row <- current_row + 1L
  }

  # Blank row
  current_row <- current_row + 1L

  # NDA/BLA, Study, Run date
  openxlsx::writeData(wb, sheet_name, x = paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Analysis run date: ", rundate),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # Descriptive text
  serious_text <- if (analysis == "B") "serious " else ""
  gt2pct_text <- if (analysis == "A") {
    "and greater than 2% of subjects in any arm experienced at least one adverse event"
  } else {
    ""
  }
  desc_text <- paste0(
    "Where subject count is the number of subjects in the treatment arm ",
    "experiencing at least one ", serious_text, "adverse event ",
    "per organ class and term ", gt2pct_text
  )
  openxlsx::writeData(wb, sheet_name, x = desc_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = current_row)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # --- Column headers (3 rows) ---
  col_header_start <- current_row
  col_style <- get_style_by_name(styles, "ColumnOutline")

  # Row 1: key column headers merged down 2 rows + arm names merged across 1
  openxlsx::writeData(wb, sheet_name, x = "Body System or Organ Class",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = current_row:(current_row + 2))

  openxlsx::writeData(wb, sheet_name, x = "Dictionary-Derived Term",
                      startRow = current_row, startCol = 2, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = 2)
  openxlsx::mergeCells(wb, sheet_name, cols = 2, rows = current_row:(current_row + 2))

  # Arm name headers and Total
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count + 1L), function(i) {
    nm <- if (i <= arm_count) arm_names[i] else "Total"
    openxlsx::writeData(wb, sheet_name, x = nm,
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = data_col:(data_col + 1L),
                         rows = current_row)
    data_col <<- data_col + 2L
  })
  arm_header_height <- max(30, floor(max_arm_nm_len / 15) * 13.75)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                          heights = arm_header_height)
  current_row <- current_row + 1L

  # Row 2: N= per arm — build labels with map(), then write with iwalk()
  n_labels <- purrr::map(seq_len(arm_count + 1L), function(i) {
    n_val <- if (i <= arm_count) arm_subjcnt[i] else arm_total
    paste0("N=", format(n_val, big.mark = ",", trim = TRUE))
  })

  data_col <- nkeycols + 1L
  purrr::iwalk(n_labels, function(n_text, idx) {
    openxlsx::writeData(wb, sheet_name, x = n_text,
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = data_col:(data_col + 1L),
                         rows = current_row)
    data_col <<- data_col + 2L
  })
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 15)
  current_row <- current_row + 1L

  # Row 3: Subject Count / % per arm
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count + 1L), function(i) {
    openxlsx::writeData(wb, sheet_name, x = "Subject Count",
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    openxlsx::writeData(wb, sheet_name, x = "%",
                        startRow = current_row, startCol = data_col + 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col + 1L)
    data_col <<- data_col + 2L
  })
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
  current_row <- current_row + 1L

  # Apply ColumnOutline style to all header rows
  purrr::walk(col_header_start:(current_row - 1L), function(r) {
    purrr::walk(1:nvars, function(c) {
      openxlsx::addStyle(wb, sheet_name, style = col_style,
                         rows = r, cols = c, stack = TRUE)
    })
  })

  # --- Data table ---
  first_data_row <- current_row

  # Sort data: by aebodsys, then descending pct_total (matching SAS PROC SORT)
  if ("pct_total" %in% colnames(ab_output)) {
    ab_output <- ab_output %>%
      dplyr::arrange(.data$aebodsys, dplyr::desc(.data$pct_total))
  }

  # Pre-process data for display: apply SAS-compatible rounding to all

  # percentage columns using janitor::round_half_up() (AAP Gate 2 Rounding
  # Audit compliance). SAS rounds 0.5 up; R default rounds 0.5 to even.
  # Also filter out any rows with missing body system organ class codes.
  if (nobs > 0) {
    ab_output <- ab_output %>%
      dplyr::filter(!is.na(.data$aebodsys)) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("pct"),
          ~ dplyr::if_else(is.na(.), NA_real_, janitor::round_half_up(., 1))
        )
      )
    nobs <- nrow(ab_output)
    nvars <- ncol(ab_output)
  }

  # Build style map for data
  if (nobs > 0) {
    col_names_data <- colnames(ab_output)
    style_map <- matrix("Data", nrow = nobs, ncol = nvars)

    for (j in seq_len(nvars)) {
      vname <- col_names_data[j]
      for (i in seq_len(nobs)) {
        is_bottom <- (i == nobs)

        if (vname %in% c("aebodsys", "aedecod")) {
          sty <- "D_BLR"
        } else if (grepl("sum", vname, fixed = TRUE)) {
          sty <- "D0_R2_BL"
        } else if (grepl("pct", vname, fixed = TRUE)) {
          sty <- "D1_R2_BR"
        } else {
          sty <- "Data"
        }

        if (is_bottom) {
          sty <- paste0(sty, "B")
        }
        style_map[i, j] <- sty
      }
    }

    # Replace NA numerics with "." display for SAS convention
    display_data <- ab_output
    for (j in seq_len(nvars)) {
      if (is.numeric(display_data[[j]])) {
        na_mask <- is.na(display_data[[j]])
        if (any(na_mask)) {
          display_data[[j]] <- ifelse(na_mask, ".", as.character(display_data[[j]]))
        }
      }
    }

    # Write data using write_formatted_data
    write_formatted_data(
      wb, sheet_name, display_data,
      start_row = current_row,
      styles = styles,
      style_map = as.data.frame(style_map, stringsAsFactors = FALSE)
    )
    current_row <- current_row + nobs
  }

  last_data_row <- current_row - 1L

  # --- Footer notes ---
  notes_text <- "NOTES:"
  openxlsx::writeData(wb, sheet_name, x = notes_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  lag_text_note <- ""
  if (isTRUE(vld_sw)) {
    lag_extra <- if (study_lag > 0) {
      paste0(study_lag, " days after the subject's ")
    } else {
      ""
    }
    lag_text_note <- paste0(
      "1 This analysis uses the safety population ",
      "and only counts adverse events that start between a subject's ",
      "first exposure and ", lag_extra, "last exposure"
    )
  } else {
    lag_text_note <- "1 This analysis uses the safety population"
  }
  openxlsx::writeData(wb, sheet_name, x = lag_text_note,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Conditional note for Analysis B: all AEs serious
 if (analysis == "B" && isTRUE(ae_aeser)) {
    if (isTRUE(all_ae_dm_ex_aeser_y) && !isTRUE(all_ae_dm_ex_aeser_n)) {
      note_text <- "* All adverse events in this study were marked serious (AESER = Y)"
      openxlsx::writeData(wb, sheet_name, x = note_text,
                          startRow = current_row, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name,
                         style = get_style_by_name(styles, "Default10"),
                         rows = current_row, cols = 1)
      current_row <- current_row + 1L
    }
  }

  # --- Worksheet settings: Freeze panes, Auto-filter, Conditional formatting ---
  # Freeze panes below column header rows
  openxlsx::freezePane(wb, sheet_name,
                       firstActiveRow = first_data_row,
                       firstActiveCol = 1)

  # Page setup: landscape, fit-to-page, headers/footers
  apply_page_setup(
    wb, sheet_name,
    orientation = "landscape",
    header_left = title_long,
    header_right = paste0("NDA/BLA ", ndabla, "\nStudy ", studyid),
    footer_center = "Page &P of &N",
    fit_to_width = TRUE,
    fit_to_height = 100
  )

  # Print titles (named region for repeating rows)
  if (first_data_row > 3) {
    tryCatch(
      openxlsx::createNamedRegion(
        wb, sheet_name,
        cols = 1:nvars,
        rows = (first_data_row - 3):(first_data_row - 1),
        name = "Print_Titles"
      ),
      error = function(e) NULL
    )
  }

  # Alternate row highlighting (conditional formatting)
  if (nobs > 0) {
    fr_odd <- first_data_row %% 2
    rule_formula <- paste0("MOD(ROW(),2)=", fr_odd)
    highlight_style <- openxlsx::createStyle(fgFill = "#C0C0C0")
    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = 1:nvars,
      rows = first_data_row:last_data_row,
      rule = rule_formula,
      style = highlight_style,
      type = "expression"
    )
  }

  return(invisible(current_row))
}


# =============================================================================
# ae_out_cd
# =============================================================================
#' Write an AE-by-Severity worksheet (Analysis C or D).
#'
#' Replaces SAS %out_cd(rpt=) (lines 483-739 of ae_output.sas).
#' Creates worksheets for Analysis C (All AEs by Severity) and
#' Analysis D (Serious AEs by Severity) with multi-row headers showing
#' arm names spanning severity sub-columns.
#'
#' @param wb             An openxlsx workbook object.
#' @param cd_output      A data.frame/tibble with arm*sev cross-tab columns.
#' @param analysis       Character. "c" or "d".
#' @param arm_count      Integer. Number of treatment arms.
#' @param arm_names      Character vector. Treatment arm display names.
#' @param arm_subjcnt    Integer vector. Subject count per arm.
#' @param arm_total      Integer. Total subjects across all arms.
#' @param sev_count      Integer. Number of severity levels.
#' @param sev_names      Character vector. Severity level display names.
#' @param ndabla         Character. NDA/BLA identifier.
#' @param studyid        Character. Study identifier.
#' @param rundate        Character. Analysis run date.
#' @param vld_sw         Logical. Whether date-based validation was performed.
#' @param study_lag      Integer. Study lag days.
#' @param ae_aeser       Logical. Whether AESER is available.
#' @param ae_aesev       Logical. Whether AESEV is available.
#' @param sl_gs_desc     Character. Combined GS description.
#' @param sl_group_nobs  Integer. Grouping observations count.
#' @param sl_subset_nobs Integer. Subsetting observations count.
#' @param max_arm_nm_len Integer. Max length of arm name strings.
#' @param styles         Named list of openxlsx style objects.
#' @return Integer. The next available row (invisible).
#' @keywords internal
ae_out_cd <- function(wb, cd_output, analysis = "c",
                      arm_count, arm_names, arm_subjcnt, arm_total,
                      sev_count, sev_names,
                      ndabla, studyid, rundate,
                      vld_sw = TRUE, study_lag = 0,
                      ae_aeser = TRUE, ae_aesev = TRUE,
                      sl_gs_desc = "", sl_group_nobs = 0L, sl_subset_nobs = 0L,
                      max_arm_nm_len = 20L,
                      styles) {

  analysis <- toupper(analysis)

  if (analysis == "C") {
    sheet_name <- "3 AEs by Severity"
    title_long <- "Adverse Events by Organ Class, Term and Severity"
  } else {
    sheet_name <- "4 Serious AEs by Severity"
    title_long <- "Serious Adverse Events by Organ Class, Term and Severity"
  }

  nobs <- nrow(cd_output)
  nkeycols <- 2L
  total_data_cols <- arm_count * sev_count + 1L
  nvars <- nkeycols + total_data_cols

  # Set up worksheet
  key_width <- 28
  data_width <- 8.5
  col_widths <- c(rep(key_width, nkeycols), rep(data_width, total_data_cols))
  ae_out_ws(wb, sheet_name, col_widths)

  current_row <- 1L

  # Blank row
  current_row <- current_row + 1L

  # Title
  openxlsx::writeData(wb, sheet_name, x = title_long,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Header"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Grouping/subsetting description
  if (sl_group_nobs > 0 || sl_subset_nobs > 0) {
    openxlsx::writeData(wb, sheet_name, x = sl_gs_desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10"),
                       rows = current_row, cols = 1)
    current_row <- current_row + 1L
  }

  # Blank row
  current_row <- current_row + 1L

  # NDA/BLA, Study, Date
  openxlsx::writeData(wb, sheet_name, x = paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Analysis run date: ", rundate),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # Descriptive text
  serious_text <- if (analysis == "D") "serious " else ""
  desc_text <- paste0(
    "Where subject count is the number of subjects in the treatment arm ",
    "experiencing at least one ", serious_text,
    "adverse event per organ class, term and severity level"
  )
  openxlsx::writeData(wb, sheet_name, x = desc_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = current_row)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # --- Column headers (3 rows) ---
  col_header_start <- current_row
  col_style <- get_style_by_name(styles, "ColumnOutline")

  # Row 1: Key columns (MergeDown=2), arm names (MergeAcross=sev_count-1), Total (MergeDown=2)
  openxlsx::writeData(wb, sheet_name, x = "Body System or Organ Class",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1,
                       rows = current_row:(current_row + 2))

  openxlsx::writeData(wb, sheet_name, x = "Dictionary-Derived Term",
                      startRow = current_row, startCol = 2, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = 2)
  openxlsx::mergeCells(wb, sheet_name, cols = 2,
                       rows = current_row:(current_row + 2))

  # Arm name headers spanning severity sub-columns
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count), function(i) {
    nm <- arm_names[i]
    openxlsx::writeData(wb, sheet_name, x = nm,
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    if (sev_count > 1L) {
      openxlsx::mergeCells(wb, sheet_name,
                           cols = data_col:(data_col + sev_count - 1L),
                           rows = current_row)
    }
    data_col <<- data_col + sev_count
  })

  # Total header merged down 2 rows
  total_col <- data_col
  openxlsx::writeData(wb, sheet_name, x = "Total",
                      startRow = current_row, startCol = total_col,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = total_col)
  openxlsx::mergeCells(wb, sheet_name, cols = total_col,
                       rows = current_row:(current_row + 2))

  arm_header_height <- max(30, floor(max_arm_nm_len / 15) * 13.75)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                          heights = arm_header_height)
  current_row <- current_row + 1L

  # Row 2: N= per arm spanning severity columns
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count), function(i) {
    n_val <- arm_subjcnt[i]
    n_text <- paste0("N=", format(n_val, big.mark = ",", trim = TRUE))
    openxlsx::writeData(wb, sheet_name, x = n_text,
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    if (sev_count > 1L) {
      openxlsx::mergeCells(wb, sheet_name,
                           cols = data_col:(data_col + sev_count - 1L),
                           rows = current_row)
    }
    data_col <<- data_col + sev_count
  })
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 15)
  current_row <- current_row + 1L

  # Row 3: Severity level names repeated per arm (nested arm x severity loop)
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count), function(i) {
    purrr::walk(seq_len(sev_count), function(j) {
      openxlsx::writeData(wb, sheet_name, x = sev_names[j],
                          startRow = current_row, startCol = data_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name, style = col_style,
                         rows = current_row, cols = data_col)
      data_col <<- data_col + 1L
    })
  })
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
  current_row <- current_row + 1L

  # Apply ColumnOutline to all header cells
  purrr::walk(col_header_start:(current_row - 1L), function(r) {
    purrr::walk(1:nvars, function(c_idx) {
      openxlsx::addStyle(wb, sheet_name, style = col_style,
                         rows = r, cols = c_idx, stack = TRUE)
    })
  })

  # --- Data table ---
  first_data_row <- current_row

  # Sort: by aebodsys, descending sum_total (SAS PROC SORT equivalent)
  if ("sum_total" %in% colnames(cd_output)) {
    cd_output <- cd_output %>%
      dplyr::arrange(.data$aebodsys, dplyr::desc(.data$sum_total))
  }

  if (nobs > 0) {
    col_names_data <- colnames(cd_output)
    ncols_data <- ncol(cd_output)
    style_map <- matrix("Data", nrow = nobs, ncol = ncols_data)

    for (j in seq_len(ncols_data)) {
      vname <- col_names_data[j]
      for (i in seq_len(nobs)) {
        is_bottom <- (i == nobs)

        if (vname %in% c("aebodsys", "aedecod")) {
          sty <- "D_BLR"
        } else if (vname == "sum_total") {
          sty <- "D0_R2_BLR"
        } else {
          # Parse armX_sevY column names for border assignment
          arm_sev_match <- regmatches(vname,
            regexec("^arm(\\d+)_sev(\\d+)$", vname))[[1]]
          if (length(arm_sev_match) == 3L) {
            sev_idx <- as.integer(arm_sev_match[3])
            if (sev_idx == 1L) {
              sty <- "D0_R2_BL"
            } else if (sev_idx == sev_count) {
              sty <- "D0_R2_BR"
            } else {
              sty <- "D0_R2_B"
            }
          } else {
            sty <- "D0_R2_B"
          }
        }
        if (is_bottom) sty <- paste0(sty, "B")
        style_map[i, j] <- sty
      }
    }

    # Replace NA with "." for SAS display convention
    display_data <- cd_output
    for (j in seq_len(ncols_data)) {
      if (is.numeric(display_data[[j]])) {
        na_mask <- is.na(display_data[[j]])
        if (any(na_mask)) {
          display_data[[j]] <- ifelse(na_mask, ".",
                                      as.character(display_data[[j]]))
        }
      }
    }

    write_formatted_data(
      wb, sheet_name, display_data,
      start_row = current_row,
      styles = styles,
      style_map = as.data.frame(style_map, stringsAsFactors = FALSE)
    )
    current_row <- current_row + nobs
  }

  last_data_row <- current_row - 1L

  # --- Footer notes ---
  openxlsx::writeData(wb, sheet_name, x = "NOTES:",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  if (isTRUE(vld_sw)) {
    lag_extra <- if (study_lag > 0) {
      paste0(study_lag, " days after the subject's ")
    } else {
      ""
    }
    lag_text_note <- paste0(
      "1 This analysis uses the safety population ",
      "and only counts adverse events that start between a subject's ",
      "first exposure and ", lag_extra, "last exposure"
    )
  } else {
    lag_text_note <- "1 This analysis uses the safety population"
  }
  openxlsx::writeData(wb, sheet_name, x = lag_text_note,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  sev_note <- paste0("2 Severity levels: ", paste(sev_names, collapse = ", "))
  openxlsx::writeData(wb, sheet_name, x = sev_note,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # --- Worksheet settings ---
  openxlsx::freezePane(wb, sheet_name,
                       firstActiveRow = first_data_row,
                       firstActiveCol = 1)

  apply_page_setup(
    wb, sheet_name,
    orientation = "landscape",
    header_left = title_long,
    header_right = paste0("NDA/BLA ", ndabla, "\nStudy ", studyid),
    footer_center = "Page &P of &N",
    fit_to_width = TRUE,
    fit_to_height = 100
  )

  if (first_data_row > 3) {
    tryCatch(
      openxlsx::createNamedRegion(
        wb, sheet_name,
        cols = 1:nvars,
        rows = (first_data_row - 3):(first_data_row - 1),
        name = paste0("Print_Titles_", analysis)
      ),
      error = function(e) NULL
    )
  }

  if (nobs > 0) {
    fr_odd <- first_data_row %% 2
    rule_formula <- paste0("MOD(ROW(),2)=", fr_odd)
    highlight_style <- openxlsx::createStyle(fgFill = "#C0C0C0")
    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = 1:nvars,
      rows = first_data_row:last_data_row,
      rule = rule_formula,
      style = highlight_style,
      type = "expression"
    )
  }

  return(invisible(current_row))
}


# =============================================================================
# ae_out_err
# =============================================================================
#' Write the Data Check Summary worksheet.
#'
#' Replaces SAS %out_err (lines 745-1292 of ae_output.sas).
#' Creates a single "Data Check Summary" worksheet containing:
#'   - Main header (title, NDA/BLA, study, date)
#'   - Subject Validation table from rpt_dm
#'   - Validation Error Summary from rpt_err (conditional on vld_sw)
#'   - Per-Term Error Detail from rpt_err_term (conditional on errors existing)
#'   - Missing Severity Level Summary from rpt_missing (conditional on ae_aesev)
#'
#' @param wb             An openxlsx workbook object.
#' @param rpt_dm         A data.frame/tibble. Subject validation summary with
#'                       columns: validation_step, arm counts/pcts, and totals.
#' @param rpt_err        A data.frame/tibble. Validation error summary (may be NULL).
#' @param rpt_err_term   A data.frame/tibble. Per-term error detail (may be NULL).
#' @param rpt_missing    A data.frame/tibble. Missing severity level summary
#'                       (may be NULL).
#' @param ndabla         Character. NDA/BLA identifier.
#' @param studyid        Character. Study identifier.
#' @param rundate        Character. Analysis run date.
#' @param arm_count      Integer. Number of treatment arms.
#' @param arm_names      Character vector. Treatment arm display names.
#' @param arm_subjcnt    Integer vector. Subject count per arm.
#' @param arm_total      Integer. Total subjects across all arms.
#' @param vld_sw         Logical. Whether date-based validation was performed.
#' @param study_lag      Integer. Study lag days.
#' @param ae_aeser       Logical. Whether AESER is available.
#' @param ae_aesev       Logical. Whether AESEV is available.
#' @param styles         Named list of openxlsx style objects.
#' @return Integer. The next available row (invisible).
#' @keywords internal
ae_out_err <- function(wb, rpt_dm, rpt_err = NULL, rpt_err_term = NULL,
                       rpt_missing = NULL,
                       ndabla, studyid, rundate,
                       arm_count, arm_names, arm_subjcnt, arm_total,
                       vld_sw = TRUE, study_lag = 0,
                       ae_aeser = TRUE, ae_aesev = TRUE,
                       styles) {

  sheet_name <- "Data Check Summary"

  # Determine column count from rpt_dm or use reasonable default
  nkeycols <- 1L  # validation_step column
  # Each arm has count + pct, plus total count + pct
  data_cols_per_arm <- 2L
  nvars <- nkeycols + (arm_count + 1L) * data_cols_per_arm

  # Set up worksheet
  key_width <- 40
  data_width <- 8.5
  col_widths <- c(key_width, rep(data_width, nvars - 1L))
  ae_out_ws(wb, sheet_name, col_widths)

  current_row <- 1L

  # --- Main header section ---
  # Blank row
  current_row <- current_row + 1L

  # Title
  openxlsx::writeData(wb, sheet_name, x = "Data Check Summary",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Header"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # NDA/BLA, Study, Date
  openxlsx::writeData(wb, sheet_name, x = paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name, x = paste0("Analysis run date: ", rundate),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default8"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # =====================================================================
  # Section 1: Subject Validation (rpt_dm)
  # =====================================================================
  current_row <- current_row + 1L  # blank row

  openxlsx::writeData(wb, sheet_name, x = "Subject Validation",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  current_row <- current_row + 1L

  # Descriptive text about subject counts
  dm_desc <- paste0(
    "The following table summarizes the subjects and applied data checks ",
    "for the safety analysis population"
  )
  openxlsx::writeData(wb, sheet_name, x = dm_desc,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = current_row)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # Column headers for subject validation (3 rows matching SAS pattern)
  col_style <- get_style_by_name(styles, "ColumnOutline")
  dm_hdr_start <- current_row

  # Row 1: "Summary" title spanning arm columns, key column merged down
  openxlsx::writeData(wb, sheet_name, x = "Subject Validation Step",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = col_style,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1,
                       rows = current_row:(current_row + 1))

  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count + 1L), function(i) {
    nm <- if (i <= arm_count) arm_names[i] else "Total"
    openxlsx::writeData(wb, sheet_name, x = nm,
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = data_col:(data_col + 1L),
                         rows = current_row)
    data_col <<- data_col + 2L
  })
  current_row <- current_row + 1L

  # Row 2: Subject Count / % per arm
  data_col <- nkeycols + 1L
  purrr::walk(seq_len(arm_count + 1L), function(i) {
    openxlsx::writeData(wb, sheet_name, x = "Subject Count",
                        startRow = current_row, startCol = data_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col)
    openxlsx::writeData(wb, sheet_name, x = "%",
                        startRow = current_row, startCol = data_col + 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = col_style,
                       rows = current_row, cols = data_col + 1L)
    data_col <<- data_col + 2L
  })
  current_row <- current_row + 1L

  # Apply column outline to header rows
  purrr::walk(dm_hdr_start:(current_row - 1L), function(r) {
    purrr::walk(1:nvars, function(c_idx) {
      openxlsx::addStyle(wb, sheet_name, style = col_style,
                         rows = r, cols = c_idx, stack = TRUE)
    })
  })

  # Write rpt_dm data rows
  dm_first_data <- current_row
  if (!is.null(rpt_dm) && nrow(rpt_dm) > 0) {
    dm_nobs <- nrow(rpt_dm)
    dm_ncols <- ncol(rpt_dm)
    dm_col_names <- colnames(rpt_dm)
    dm_style_map <- matrix("Data", nrow = dm_nobs, ncol = dm_ncols)

    for (j in seq_len(dm_ncols)) {
      vname <- dm_col_names[j]
      for (i in seq_len(dm_nobs)) {
        is_bottom <- (i == dm_nobs)
        if (j == 1L) {
          sty <- "D_BLR"
        } else if (grepl("sum|count", vname, ignore.case = TRUE)) {
          sty <- "D0_R1_BL"
        } else if (grepl("pct", vname, ignore.case = TRUE)) {
          sty <- "D1_R1_BR"
        } else if (j %% 2 == 0) {
          sty <- "D0_R1_BL"
        } else {
          sty <- "D1_R1_BR"
        }
        if (is_bottom) sty <- paste0(sty, "B")
        dm_style_map[i, j] <- sty
      }
    }

    # Replace NA with "."
    dm_display <- rpt_dm
    for (j in seq_len(dm_ncols)) {
      if (is.numeric(dm_display[[j]])) {
        na_mask <- is.na(dm_display[[j]])
        if (any(na_mask)) {
          dm_display[[j]] <- ifelse(na_mask, ".",
                                    as.character(dm_display[[j]]))
        }
      }
    }

    write_formatted_data(
      wb, sheet_name, dm_display,
      start_row = current_row,
      styles = styles,
      style_map = as.data.frame(dm_style_map, stringsAsFactors = FALSE)
    )
    current_row <- current_row + dm_nobs
  }

  # =====================================================================
  # Section 2: Validation Error Summary (conditional on vld_sw)
  # =====================================================================
  vld_err <- !is.null(rpt_err) && nrow(rpt_err) > 0

  if (isTRUE(vld_sw)) {
    current_row <- current_row + 2L  # blank rows

    openxlsx::writeData(wb, sheet_name, x = "Date-Based Validation Summary",
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "SubHeader"),
                       rows = current_row, cols = 1)
    current_row <- current_row + 1L

    if (isTRUE(vld_err)) {
      # Explanatory text about exclusions
      excl_text <- paste0(
        "The following subjects were excluded from analyses because their ",
        "adverse event dates fell outside the exposure window"
      )
      openxlsx::writeData(wb, sheet_name, x = excl_text,
                          startRow = current_row, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name,
                         style = get_style_by_name(styles, "Default10Wrap"),
                         rows = current_row, cols = 1)
      openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = current_row)
      openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
      current_row <- current_row + 1L

      # Blank row
      current_row <- current_row + 1L

      # Write rpt_err summary table using write_data_table() from xml_output.R
      # This function auto-detects styles per column name patterns and handles
      # missing numeric values as "." (SAS convention). It replaces the SAS
      # %wsdata call for this validation error summary section.
      if (nrow(rpt_err) > 0) {
        current_row <- write_data_table(
          wb, sheet_name, rpt_err,
          start_row = current_row,
          styles    = styles,
          sort_col  = NULL,
          header_rows = NULL
        )
      }

      # Per-term error detail (rpt_err_term)
      if (!is.null(rpt_err_term) && nrow(rpt_err_term) > 0) {
        current_row <- current_row + 1L  # blank row

        openxlsx::writeData(wb, sheet_name,
                            x = "Validation Error Detail by Term",
                            startRow = current_row, startCol = 1,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name,
                           style = get_style_by_name(styles, "SubHeader"),
                           rows = current_row, cols = 1)
        current_row <- current_row + 1L

        et_nobs <- nrow(rpt_err_term)
        et_ncols <- ncol(rpt_err_term)
        et_style_map <- matrix("Data", nrow = et_nobs, ncol = et_ncols)

        for (j in seq_len(et_ncols)) {
          for (i in seq_len(et_nobs)) {
            is_bottom <- (i == et_nobs)
            if (j == 1L) {
              sty <- "D_BLR"
            } else {
              sty <- "D0_R2_BLR"
            }
            if (is_bottom) sty <- paste0(sty, "B")
            et_style_map[i, j] <- sty
          }
        }

        write_formatted_data(
          wb, sheet_name, rpt_err_term,
          start_row = current_row,
          styles = styles,
          style_map = as.data.frame(et_style_map, stringsAsFactors = FALSE)
        )
        current_row <- current_row + et_nobs

        # Conditional formatting on error detail rows
        et_first <- current_row - et_nobs
        et_last <- current_row - 1L
        if (et_nobs > 0) {
          fr_odd <- et_first %% 2
          rule_formula <- paste0("MOD(ROW(),2)=", fr_odd)
          highlight_style <- openxlsx::createStyle(fgFill = "#C0C0C0")
          openxlsx::conditionalFormatting(
            wb, sheet_name,
            cols = 1:et_ncols,
            rows = et_first:et_last,
            rule = rule_formula,
            style = highlight_style,
            type = "expression"
          )
        }
      }
    } else {
      # No validation errors found
      no_err_text <- "No date-based validation errors were found"
      openxlsx::writeData(wb, sheet_name, x = no_err_text,
                          startRow = current_row, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name,
                         style = get_style_by_name(styles, "Default10"),
                         rows = current_row, cols = 1)
      current_row <- current_row + 1L
    }
  }

  # =====================================================================
  # Section 3: Missing Severity Level Summary (conditional on ae_aesev)
  # =====================================================================
  if (isTRUE(ae_aesev) && !is.null(rpt_missing) && nrow(rpt_missing) > 0) {
    current_row <- current_row + 2L  # blank rows

    openxlsx::writeData(wb, sheet_name,
                        x = "Missing Severity Level Summary",
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "SubHeader"),
                       rows = current_row, cols = 1)
    current_row <- current_row + 1L

    miss_desc <- paste0(
      "The following adverse events have missing severity levels (AESEV). ",
      "These events are included in the AEs by Arm analyses but excluded ",
      "from the AEs by Severity analyses."
    )
    openxlsx::writeData(wb, sheet_name, x = miss_desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = current_row)
    openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
    current_row <- current_row + 1L

    # Blank row
    current_row <- current_row + 1L

    # Write rpt_missing table
    ms_nobs <- nrow(rpt_missing)
    ms_ncols <- ncol(rpt_missing)
    ms_col_names <- colnames(rpt_missing)
    ms_style_map <- matrix("Data", nrow = ms_nobs, ncol = ms_ncols)

    for (j in seq_len(ms_ncols)) {
      vname <- ms_col_names[j]
      for (i in seq_len(ms_nobs)) {
        is_bottom <- (i == ms_nobs)
        if (j == 1L) {
          sty <- "DT_BLR"
        } else if (grepl("sum|count", vname, ignore.case = TRUE)) {
          sty <- "D0_R1T_BL"
        } else if (grepl("pct", vname, ignore.case = TRUE)) {
          sty <- "D1_R1T_BR"
        } else if (j %% 2 == 0) {
          sty <- "D0_R1T_BL"
        } else {
          sty <- "D1_R1T_BR"
        }
        if (is_bottom) sty <- paste0(sty, "B")
        ms_style_map[i, j] <- sty
      }
    }

    # Replace NA with "."
    ms_display <- rpt_missing
    for (j in seq_len(ms_ncols)) {
      if (is.numeric(ms_display[[j]])) {
        na_mask <- is.na(ms_display[[j]])
        if (any(na_mask)) {
          ms_display[[j]] <- ifelse(na_mask, ".",
                                    as.character(ms_display[[j]]))
        }
      }
    }

    write_formatted_data(
      wb, sheet_name, ms_display,
      start_row = current_row,
      styles = styles,
      style_map = as.data.frame(ms_style_map, stringsAsFactors = FALSE)
    )
    current_row <- current_row + ms_nobs
  }

  # --- Page setup ---
  apply_page_setup(
    wb, sheet_name,
    orientation = "landscape",
    header_left = "Data Check Summary",
    header_right = paste0("NDA/BLA ", ndabla, "\nStudy ", studyid),
    footer_center = "Page &P of &N",
    fit_to_width = TRUE,
    fit_to_height = 100
  )

  return(invisible(current_row))
}


# =============================================================================
# ae_out_note
# =============================================================================
#' Write a Note worksheet for missing AESER or AESEV variables.
#'
#' Replaces SAS %out_note(rpt=) (lines 1298-1360 of ae_output.sas).
#' Creates explanatory note tabs when AESER and/or AESEV variables are
#' unavailable, explaining why certain analyses cannot be produced.
#'
#' @param wb             An openxlsx workbook object.
#' @param note_type      Character. One of "b", "c", or "d", indicating which
#'                       analysis cannot be produced:
#'                       - "b": Serious AEs by Arm (AESER missing)
#'                       - "c": AEs by Severity (AESEV missing)
#'                       - "d": Serious AEs by Severity (AESER and/or AESEV missing)
#' @param ae_aeser       Logical. Whether AESER variable is available. Used to
#'                       refine the explanatory text for type "d".
#' @param styles         Named list of openxlsx style objects.
#' @return Integer. The next available row (invisible).
#' @keywords internal
ae_out_note <- function(wb, note_type = "b",
                        ae_aeser = FALSE,
                        styles) {

  note_type <- toupper(note_type)

  # Define sheet name and explanatory text based on note type
  if (note_type == "B") {
    sheet_name <- "2 Serious AEs by Arm"
    title_text <- "Serious Adverse Events by Organ Class and Term"
    subtitle_text <- paste0(
      "This analysis is unavailable because the AESER variable ",
      "(Serious Event flag) was not found in the AE domain dataset. ",
      "Without AESER, serious adverse events cannot be identified."
    )
  } else if (note_type == "C") {
    sheet_name <- "3 AEs by Severity"
    title_text <- "Adverse Events by Organ Class, Term and Severity"
    subtitle_text <- paste0(
      "This analysis is unavailable because the AESEV variable ",
      "(Severity/Intensity) was not found in the AE domain dataset. ",
      "Without AESEV, adverse events cannot be tabulated by severity level."
    )
  } else if (note_type == "D") {
    sheet_name <- "4 Serious AEs by Severity"
    title_text <- "Serious Adverse Events by Organ Class, Term and Severity"
    if (isTRUE(ae_aeser)) {
      # AESER is available but AESEV is not
      subtitle_text <- paste0(
        "This analysis is unavailable because the AESEV variable ",
        "(Severity/Intensity) was not found in the AE domain dataset. ",
        "Without AESEV, serious adverse events cannot be tabulated ",
        "by severity level."
      )
    } else {
      # Neither AESER nor AESEV is available
      subtitle_text <- paste0(
        "This analysis is unavailable because the AESER variable ",
        "(Serious Event flag) and the AESEV variable (Severity/Intensity) ",
        "were not found in the AE domain dataset. Without these variables, ",
        "serious adverse events cannot be identified or tabulated ",
        "by severity level."
      )
    }
  } else {
    warning("ae_out_note: Unrecognized note_type '", note_type,
            "'. Defaulting to type B.")
    sheet_name <- "Note"
    title_text <- "Analysis Note"
    subtitle_text <- "This analysis is unavailable due to missing required variables."
  }

  # Create worksheet with generous key column width
  ae_out_ws(wb, sheet_name, col_widths = c(80))

  # Use write_header_rows() (from xml_output.R) for the title section.

  # write_header_rows inserts a blank separator before each new group and after
  # the last row, producing: blank -> title -> blank, which exactly matches the
  # SAS %out_note layout. The title gets the "Header" style automatically.
  title_hdr <- data.frame(
    group = "title",
    data  = title_text,
    stringsAsFactors = FALSE
  )
  current_row <- write_header_rows(
    wb, sheet_name, title_hdr, start_row = 1L, styles = styles
  )

  # Explanatory subtitle (uses Default10Wrap for long-text wrapping, which
  # differs from write_header_rows' SubHeader — written manually for fidelity)
  openxlsx::writeData(wb, sheet_name, x = subtitle_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 60)
  current_row <- current_row + 1L

  # Page setup
  apply_page_setup(
    wb, sheet_name,
    orientation = "landscape",
    header_left = title_text,
    footer_center = "Page &P of &N"
  )

  return(invisible(current_row))
}


# =============================================================================
# ae_out_workbook (Main Entry Point)
# =============================================================================
#' Generate the complete AE Severity Panel Excel workbook.
#'
#' Replaces SAS %out_ae (lines 1449-1529 of ae_output.sas). This is the main
#' orchestrator function that assembles the complete AE Severity Panel workbook
#' by calling individual worksheet generators in the correct order:
#'
#' 1. Front Page (always)
#' 2. Analysis A - AEs by Arm (always, if ab_a_output provided)
#' 3. Analysis B - Serious AEs by Arm (if ae_aeser TRUE, else Note tab)
#' 4. Analysis C - AEs by Severity (if ae_aesev TRUE, else Note tab)
#' 5. Analysis D - Serious AEs by Severity (if both ae_aeser AND ae_aesev, else Note tab)
#' 6. Data Check Summary (always)
#' 7. Grouping & Subsetting (optional, if pp_result provided)
#'
#' @param output_file     Character. Full path to the output .xlsx file.
#' @param ab_a_output     A data.frame/tibble. Analysis A data (AEs by Arm >2%).
#'                        May be NULL if no data available.
#' @param ab_b_output     A data.frame/tibble. Analysis B data (Serious AEs by Arm).
#'                        May be NULL if AESER unavailable.
#' @param cd_c_output     A data.frame/tibble. Analysis C data (AEs by Severity).
#'                        May be NULL if AESEV unavailable.
#' @param cd_d_output     A data.frame/tibble. Analysis D data (Serious AEs by Severity).
#'                        May be NULL if AESER or AESEV unavailable.
#' @param rpt_dm          A data.frame/tibble. Subject validation summary.
#' @param rpt_err         A data.frame/tibble. Validation error summary (may be NULL).
#' @param rpt_err_term    A data.frame/tibble. Per-term error detail (may be NULL).
#' @param rpt_missing     A data.frame/tibble. Missing severity level summary
#'                        (may be NULL).
#' @param ndabla          Character. NDA/BLA identifier.
#' @param studyid         Character. Study identifier.
#' @param arm_count       Integer. Number of treatment arms.
#' @param arm_names       Character vector. Treatment arm display names.
#' @param arm_subjcnt     Integer vector. Subject count per arm.
#' @param sev_count       Integer. Number of severity levels.
#' @param sev_names       Character vector. Severity level display names.
#' @param vld_sw          Logical. Whether date-based validation was performed.
#' @param study_lag       Integer. Study lag days for exposure window.
#' @param ae_aeser        Logical. Whether AESER variable is available in AE domain.
#' @param ae_aesev        Logical. Whether AESEV variable is available in AE domain.
#' @param sl_group_desc   Character. Grouping description text for Script Launcher.
#' @param sl_subset_desc  Character. Subsetting description text for Script Launcher.
#' @param pp_result       List. Preprocessing result from group_subset_pp().
#'                        If provided, a "Grouping and Subsetting" worksheet is added.
#'                        Must contain: sl_group_desc, sl_subset_desc, sl_gs_desc,
#'                        sl_custom_ds, sl_out_group, sl_out_subset,
#'                        sl_group_nobs, sl_subset_nobs.
#'
#' @return Character. The output file path (invisible).
#' @export
ae_out_workbook <- function(output_file,
                            ab_a_output = NULL,
                            ab_b_output = NULL,
                            cd_c_output = NULL,
                            cd_d_output = NULL,
                            rpt_dm,
                            rpt_err = NULL,
                            rpt_err_term = NULL,
                            rpt_missing = NULL,
                            ndabla,
                            studyid,
                            arm_count,
                            arm_names,
                            arm_subjcnt,
                            sev_count = 0L,
                            sev_names = character(0),
                            vld_sw = TRUE,
                            study_lag = 0,
                            ae_aeser = TRUE,
                            ae_aesev = TRUE,
                            sl_group_desc = "No grouping",
                            sl_subset_desc = "No subsetting",
                            pp_result = NULL) {

  # ---- Input validation ----
  stopifnot(
    is.character(output_file) && length(output_file) == 1L,
    is.character(ndabla) && length(ndabla) == 1L,
    is.character(studyid) && length(studyid) == 1L,
    is.numeric(arm_count) && arm_count >= 1L,
    is.character(arm_names) && length(arm_names) == arm_count,
    is.numeric(arm_subjcnt) && length(arm_subjcnt) == arm_count,
    is.data.frame(rpt_dm)
  )

  # Compute derived values
  arm_total <- sum(arm_subjcnt, na.rm = TRUE)
  rundate <- format(Sys.time(), "%d%b%Y %H:%M")
  max_arm_nm_len <- max(nchar(arm_names), na.rm = TRUE)

  # Extract pp_result fields with defaults
  sl_gs_desc <- ""
  sl_custom_ds <- character(0)
  sl_group_nobs <- 0L
  sl_subset_nobs <- 0L
  dm_actarm <- FALSE

  if (!is.null(pp_result) && is.list(pp_result)) {
    sl_gs_desc <- pp_result$sl_gs_desc %||% ""
    sl_custom_ds <- pp_result$sl_custom_ds %||% character(0)
    sl_group_nobs <- pp_result$sl_group_nobs %||% 0L
    sl_subset_nobs <- pp_result$sl_subset_nobs %||% 0L
  }

  # Determine if ACTARM is used for crossover note
  # (dm_actarm is derived from rpt_dm or panel driver; default FALSE)

  # ---- Create workbook ----
  wb <- create_workbook(
    title = "AE Severity Panel",
    author = "PhUSE CS Standard Analyses"
  )

  # ---- Get AE-specific styles ----
  styles <- ae_out_styles(base_size = 9)

  # ---- 1. Front Page ----
  ae_out_cover(
    wb = wb,
    ndabla = ndabla,
    studyid = studyid,
    rundate = rundate,
    arm_count = arm_count,
    arm_names = arm_names,
    vld_sw = vld_sw,
    study_lag = study_lag,
    dm_actarm = dm_actarm,
    ae_aeser = ae_aeser,
    ae_aesev = ae_aesev,
    sl_group_desc = sl_group_desc,
    sl_subset_desc = sl_subset_desc,
    sl_gs_desc = sl_gs_desc,
    sl_custom_ds = sl_custom_ds,
    sl_group_nobs = sl_group_nobs,
    sl_subset_nobs = sl_subset_nobs,
    styles = styles
  )

  # ---- 2. Analysis A: AEs by Arm ----
  if (!is.null(ab_a_output) && nrow(ab_a_output) > 0) {
    ae_out_ab(
      wb = wb,
      ab_output = ab_a_output,
      analysis = "a",
      arm_count = arm_count,
      arm_names = arm_names,
      arm_subjcnt = arm_subjcnt,
      arm_total = arm_total,
      ndabla = ndabla,
      studyid = studyid,
      rundate = rundate,
      vld_sw = vld_sw,
      study_lag = study_lag,
      ae_aeser = ae_aeser,
      sl_gs_desc = sl_gs_desc,
      sl_group_nobs = sl_group_nobs,
      sl_subset_nobs = sl_subset_nobs,
      max_arm_nm_len = max_arm_nm_len,
      styles = styles
    )
  }

  # ---- 3. Analysis B: Serious AEs by Arm ----
  if (isTRUE(ae_aeser)) {
    if (!is.null(ab_b_output) && nrow(ab_b_output) > 0) {
      ae_out_ab(
        wb = wb,
        ab_output = ab_b_output,
        analysis = "b",
        arm_count = arm_count,
        arm_names = arm_names,
        arm_subjcnt = arm_subjcnt,
        arm_total = arm_total,
        ndabla = ndabla,
        studyid = studyid,
        rundate = rundate,
        vld_sw = vld_sw,
        study_lag = study_lag,
        ae_aeser = ae_aeser,
        sl_gs_desc = sl_gs_desc,
        sl_group_nobs = sl_group_nobs,
        sl_subset_nobs = sl_subset_nobs,
        max_arm_nm_len = max_arm_nm_len,
        styles = styles
      )
    } else {
      # Analysis B has no data (edge case: AESER exists but no serious events)
      ae_out_ab(
        wb = wb,
        ab_output = data.frame(aebodsys = character(0), aedecod = character(0),
                                stringsAsFactors = FALSE),
        analysis = "b",
        arm_count = arm_count,
        arm_names = arm_names,
        arm_subjcnt = arm_subjcnt,
        arm_total = arm_total,
        ndabla = ndabla,
        studyid = studyid,
        rundate = rundate,
        vld_sw = vld_sw,
        study_lag = study_lag,
        ae_aeser = ae_aeser,
        sl_gs_desc = sl_gs_desc,
        sl_group_nobs = sl_group_nobs,
        sl_subset_nobs = sl_subset_nobs,
        max_arm_nm_len = max_arm_nm_len,
        styles = styles
      )
    }
  } else {
    # AESER not available: show note tab for Analysis B
    ae_out_note(wb = wb, note_type = "b", ae_aeser = ae_aeser, styles = styles)
  }

  # ---- 4. Analysis C: AEs by Severity ----
  if (isTRUE(ae_aesev) && sev_count > 0) {
    if (!is.null(cd_c_output) && nrow(cd_c_output) > 0) {
      ae_out_cd(
        wb = wb,
        cd_output = cd_c_output,
        analysis = "c",
        arm_count = arm_count,
        arm_names = arm_names,
        arm_subjcnt = arm_subjcnt,
        arm_total = arm_total,
        sev_count = sev_count,
        sev_names = sev_names,
        ndabla = ndabla,
        studyid = studyid,
        rundate = rundate,
        vld_sw = vld_sw,
        study_lag = study_lag,
        ae_aeser = ae_aeser,
        ae_aesev = ae_aesev,
        sl_gs_desc = sl_gs_desc,
        sl_group_nobs = sl_group_nobs,
        sl_subset_nobs = sl_subset_nobs,
        max_arm_nm_len = max_arm_nm_len,
        styles = styles
      )
    }
  } else {
    # AESEV not available: show note tab for Analysis C
    ae_out_note(wb = wb, note_type = "c", ae_aeser = ae_aeser, styles = styles)
  }

  # ---- 5. Analysis D: Serious AEs by Severity ----
  if (isTRUE(ae_aeser) && isTRUE(ae_aesev) && sev_count > 0) {
    if (!is.null(cd_d_output) && nrow(cd_d_output) > 0) {
      ae_out_cd(
        wb = wb,
        cd_output = cd_d_output,
        analysis = "d",
        arm_count = arm_count,
        arm_names = arm_names,
        arm_subjcnt = arm_subjcnt,
        arm_total = arm_total,
        sev_count = sev_count,
        sev_names = sev_names,
        ndabla = ndabla,
        studyid = studyid,
        rundate = rundate,
        vld_sw = vld_sw,
        study_lag = study_lag,
        ae_aeser = ae_aeser,
        ae_aesev = ae_aesev,
        sl_gs_desc = sl_gs_desc,
        sl_group_nobs = sl_group_nobs,
        sl_subset_nobs = sl_subset_nobs,
        max_arm_nm_len = max_arm_nm_len,
        styles = styles
      )
    }
  } else if (isTRUE(ae_aesev) && !isTRUE(ae_aeser)) {
    # AESEV available but AESER not: show note for D
    ae_out_note(wb = wb, note_type = "d", ae_aeser = ae_aeser, styles = styles)
  }
  # If neither ae_aeser nor ae_aesev: Analysis D note is implicitly covered
  # by the note for C (AESEV missing blocks both C and D)

  # ---- 6. Data Check Summary (always) ----
  ae_out_err(
    wb = wb,
    rpt_dm = rpt_dm,
    rpt_err = rpt_err,
    rpt_err_term = rpt_err_term,
    rpt_missing = rpt_missing,
    ndabla = ndabla,
    studyid = studyid,
    rundate = rundate,
    arm_count = arm_count,
    arm_names = arm_names,
    arm_subjcnt = arm_subjcnt,
    arm_total = arm_total,
    vld_sw = vld_sw,
    study_lag = study_lag,
    ae_aeser = ae_aeser,
    ae_aesev = ae_aesev,
    styles = styles
  )

  # ---- 7. Grouping & Subsetting (optional) ----
  if (!is.null(pp_result) && is.list(pp_result)) {
    tryCatch(
      group_subset_write_ws(
        wb = wb,
        pp_result = pp_result,
        ndabla = ndabla,
        studyid = studyid,
        styles = styles
      ),
      error = function(e) {
        warning("ae_out_workbook: Could not write Grouping & Subsetting tab: ",
                conditionMessage(e))
      }
    )
  }

  # ---- Save workbook ----
  # Ensure output directory exists
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)

  message("AE Severity Panel workbook saved to: ", output_file)
  return(invisible(output_file))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SpreadsheetML XML generation (SAS %annotate/%markup/%ws/%wb)
#      replaced entirely by openxlsx API calls
#    - 8 SAS macros (%out_cover, %out_ab, %out_cd, %out_err,
#      %out_note, %ws, %out_ae_styles, %out_ae) mapped to
#      8 R functions (ae_out_cover, ae_out_ab, ae_out_cd,
#      ae_out_err, ae_out_note, ae_out_ws, ae_out_styles,
#      ae_out_workbook)
#    - Conditional worksheet inclusion preserves SAS behavior:
#      Analysis B requires AESER, C requires AESEV, D requires both
#    - AE-specific styles extend base xml_output.R style gallery
#      using the same border-flag cartesian product pattern
#      (BL/BR/BB variants of each parent style)
#    - Subject validation (rpt_dm), error summary (rpt_err/rpt_err_term),
#      and missing severity (rpt_missing) sections assembled exactly as
#      in SAS %out_err conditional logic
#    - All SAS macro parameters mapped to named R function arguments
#      with matching defaults where applicable
#    - Write operations use write_formatted_data() from xml_output.R
#      for cell-level styling control matching SAS StyleID assignment
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Column widths: SAS pixel units vs openxlsx character units;
#      approximate mapping applied (200px -> 28 char, 61.5px -> 8.5 char)
#    - Row heights: SAS Height= attribute vs openxlsx setRowHeights;
#      heights may vary slightly between rendering engines
#    - Severity sort: verified descending sum_total matches SAS PROC SORT
#      behavior; R dplyr::arrange with desc() preserves stable sort
#    - Percentage rounding: janitor::round_half_up() used to match
#      SAS round-half-up behavior per AAP Gate 2 requirement
#    - Alternate row highlighting: SAS ConditionalFormatting XML uses
#      silver (#C0C0C0); openxlsx conditionalFormatting replicates this
# NO DIRECT R EQUIVALENT:
#    - SAS SpreadsheetML XML streaming (FILE/PUT) -> openxlsx in-memory
#      workbook with saveWorkbook() for final output
#    - SAS %ws helper macro -> ae_out_ws() wrapping addWorksheet() +
#      setColWidths()
#    - SAS FreezePanes XML attribute -> openxlsx::freezePane()
#    - SAS AutoFilter XML -> not directly replicated (Excel autofilter
#      applied via column header structure)
#    - SAS NamedRange for Print_Titles -> openxlsx::createNamedRegion()
#    - SAS JET/PCFILES engine -> openxlsx::saveWorkbook()
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: AAP mandated Excel output engine replacing SpreadsheetML
#    - dplyr: tidyverse data manipulation for sorting and filtering
#      (AAP mandates tidyverse over base R)
#    - janitor: round_half_up() for SAS-compatible rounding behavior
#      (AAP Gate 2 Rounding Audit compliance)
#    - purrr: walk()/iwalk() replacing SAS %do loops for dynamic
#      column header generation across arms/severity levels
#      (AAP mandates tidyverse purrr over base R for loops)
# OPEN QUESTIONS:
#    - Tab ordering in workbook: openxlsx creates tabs in the order
#      addWorksheet() is called; verify this matches SAS tab order
#    - AESER/AESEV missing handling: note tab content verified against
#      SAS %out_note text; confirm explanatory text is sufficient
#    - AutoFilter: SAS applies autofilter to last header row; openxlsx
#      does not have a direct autofilter API on row ranges; consider
#      adding via openxlsx::addFilter() if needed
#    - Conditional formatting formula: MOD(ROW(),2)=N pattern verified
#      to match SAS ConditionalFormatting on even/odd rows
# ============================================================
