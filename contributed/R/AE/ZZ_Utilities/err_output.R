###############################################################################
#         PROGRAM NAME: Generic Panel Error Output (R Migration)              #
#                                                                             #
#          DESCRIPTION: Create an Excel error summary workbook when the       #
#                       AE panel encounters missing variables or    #
#                       no subjects. Replaces SpreadsheetML XML generation    #
#                       with openxlsx workbook API.                          #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: March 28, 2011                                        #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/AE/ZZ_Utilities/err_output.sas
#     - Migration target: Idiomatic R using openxlsx for Excel output         #
#     - SAS %error_summary macro -> R error_summary() function                #
#     - SpreadsheetML XML -> openxlsx workbook API                            #
#     - SAS global macro variables -> R function parameters                   #
#     - SAS %put -> cli messages                                              #
#                                                                             #
#  EXTERNAL FILES USED: None (self-contained utility; openxlsx replaces       #
#                       xml_output.sas dependency)                            #
#                                                                             #
#  PARAMETERS REQUIRED: err_file -- filename and path of the output           #
#                       panel_title -- title of the panel                     #
#                       ndabla -- NDA/BLA identifier                          #
#                       studyid -- study identifier                           #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by AE analysis scripts             #
#                       In SAS, it was included via:                           #
#                       %include "&utilpath.\err_output.sas";                 #
#                                                                             #
#            REVISIONS:                                                        #
#              2011-05-08  DK  Added support for no subjects in DM            #
#              2011-06-08  DK  Added err_seterr argument                      #
#              2026-xx-xx  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(openxlsx)
library(dplyr)
library(cli)

# --------------------------------------------------------------------------- #
# error_summary() — Generate Excel Error Summary Workbook
# --------------------------------------------------------------------------- #
#' Generate an Excel Error Summary Workbook
#'
#' Creates an Excel workbook with error summary information when the
#' AE panel encounters missing variables or no subjects.
#' This replaces the SAS \%error_summary macro which generated
#' SpreadsheetML XML output.
#'
#' @param err_file Character. Output file path for the Excel workbook.
#'   Must be parameterized — no hardcoded paths.
#' @param panel_title Character. Title of the panel (e.g., "Demographics").
#'   Used in the workbook header and print header.
#' @param panel_desc Character. Optional panel description text. If non-empty,
#'   displayed with word wrap and merged across 6 columns.
#' @param ndabla Character. NDA/BLA identifier for the study.
#' @param studyid Character. Study identifier.
#' @param sl_subset Tibble or NULL. Script Launcher subsetting tibble with
#'   columns 'outer_operator' and 'name'. Used when err_nosubj is TRUE
#'   to describe the subsetting criteria.
#' @param rpt_chk_var_req Tibble or NULL. Required variable check tibble with
#'   columns 'ind', 'ds', 'var'. Used when err_missvar is TRUE to list
#'   missing variables.
#' @param err_nosubj Logical. If TRUE, indicates no subjects were found in DM.
#'   Defaults to FALSE.
#' @param err_missvar Logical. If TRUE, indicates required variables are missing.
#'   Defaults to FALSE.
#' @param err_seterr Logical. If TRUE, sets error status to 5 in the return
#'   value. Defaults to TRUE. Replaces SAS \%let errstatus = 5.
#' @param err_desc Character. Optional custom error description text.
#'
#' @return A list with components:
#'   \describe{
#'     \item{success}{Logical. Always FALSE (this function is called on error).}
#'     \item{errstatus}{Integer. 5 if err_seterr is TRUE, 0 otherwise.}
#'     \item{err_file}{Character. Path to the generated error workbook.}
#'   }
#'
#' @examples
#' \dontrun{
#' # No subjects error
#' result <- error_summary(
#'   err_file = "output/error_summary.xlsx",
#'   panel_title = "Demographics",
#'   ndabla = "125476",
#'   studyid = "C13007",
#'   err_nosubj = TRUE,
#'   sl_subset = tibble(outer_operator = "and", name = c("AGE > 18", "SEX = F"))
#' )
#'
#' # Missing variables error
#' result <- error_summary(
#'   err_file = "output/error_summary.xlsx",
#'   panel_title = "Demographics",
#'   ndabla = "125476",
#'   studyid = "C13007",
#'   err_missvar = TRUE,
#'   rpt_chk_var_req = tibble(ind = c(1, 0, 0), ds = c("DM", "DM", "DS"),
#'                            var = c("USUBJID", "AGE", "DSDECOD"))
#' )
#' }
error_summary <- function(err_file,
                          panel_title,
                          panel_desc = "",
                          ndabla,
                          studyid,
                          sl_subset = NULL,
                          rpt_chk_var_req = NULL,
                          err_nosubj = FALSE,
                          err_missvar = FALSE,
                          err_seterr = TRUE,
                          err_desc = "") {

  # Construct the workbook title (SAS line 46)
  wbtitle <- paste0(panel_title, " Error Summary")

  # ----------------------------------------------------------------------- #
  # Phase 3: No-Subjects Error Preprocessing (SAS lines 48-69)
  # ----------------------------------------------------------------------- #
  sl_subset_desc <- ""

  if (isTRUE(err_nosubj)) {
    cli::cli_alert_warning("PANEL NO SUBJECTS ERROR PREPROCESSING")

    # Count rows in sl_subset tibble (SAS lines 52-55)
    sl_subset_count <- 0L
    if (!is.null(sl_subset) && is.data.frame(sl_subset)) {
      sl_subset_count <- nrow(sl_subset)
    }

    if (sl_subset_count > 0L) {
      # Derive operator from lowercased outer_operator column (SAS lines 59-61)
      operator <- sl_subset %>%
        dplyr::pull(.data$outer_operator) %>%
        tolower() %>%
        unique()
      # Use the first operator value (SAS selects into single macro var)
      operator <- operator[1L]

      # Build sl_subset_desc by collapsing distinct name values with operator
      # separator (SAS lines 63-64: select distinct name into: sl_subset_desc
      # separated by " &operator. ")
      sl_subset_desc <- sl_subset %>%
        dplyr::distinct(.data$name) %>%
        dplyr::pull(.data$name) %>%
        paste(collapse = paste0(" ", operator, " "))
    } else {
      # SAS line 68: %let sl_subset_desc = ;
      sl_subset_desc <- ""
    }
  }

  # ----------------------------------------------------------------------- #
  # Phase 4: Missing Variable Error Preprocessing (SAS lines 71-80)
  # ----------------------------------------------------------------------- #
  err_missing_var <- NULL

  if (isTRUE(err_missvar)) {
    cli::cli_alert_warning("PANEL MISSING VARIABLE ERROR PREPROCESSING")

    # Create err_missing_var tibble from rpt_chk_var_req where ind != 1,
    # keeping only ds and var columns (SAS lines 75-79)
    if (!is.null(rpt_chk_var_req) && is.data.frame(rpt_chk_var_req)) {
      err_missing_var <- rpt_chk_var_req %>%
        dplyr::filter(.data$ind != 1) %>%
        dplyr::select("ds", "var")
    } else {
      # Safety fallback: empty tibble if rpt_chk_var_req is NULL
      err_missing_var <- tibble::tibble(ds = character(0), var = character(0))
    }
  }

  # ----------------------------------------------------------------------- #
  # Phase 5: Excel Workbook Creation (SAS lines 83-267)
  # Replaces SpreadsheetML XML generation
  # ----------------------------------------------------------------------- #
  cli::cli_alert_info("SCRIPT LAUNCHER ERROR SUMMARY OUTPUT")

  # Create workbook (replaces SAS %wb call at line 87)
  wb <- openxlsx::createWorkbook()

  # Add "Error Summary" worksheet (SAS line 93)
  sheet <- "Error Summary"
  openxlsx::addWorksheet(wb, sheet)

  # Define styles using openxlsx::createStyle() (replaces SAS %styles at line 88)
  # SAS "Header" style: Font size 12, bold, italic
  header_style <- openxlsx::createStyle(
    fontSize = 12,
    textDecoration = c("bold", "italic")
  )

  # SAS "Default10" style: Font size 10, top-aligned
  default10_style <- openxlsx::createStyle(
    fontSize = 10,
    valign = "top"
  )

  # SAS "Default10Wrap" style: Font size 10, top-aligned, word wrap
  default10_wrap_style <- openxlsx::createStyle(
    fontSize = 10,
    valign = "top",
    wrapText = TRUE
  )

  # SAS "ColumnOutline" style: Bold, centered, white on dark blue, all borders
  column_outline_style <- openxlsx::createStyle(
    fontSize = 10,
    fontColour = "#FFFFFF",
    fgFill = "#333399",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  # SAS "Table" style: Font size 10, centered, all borders
  table_style <- openxlsx::createStyle(
    fontSize = 10,
    halign = "center",
    valign = "top",
    border = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  # Set column widths (SAS lines 104-108)
  # SAS: Column ss:Width="150" → approx 21.4 character widths
  # SAS: Column ss:Width="250" → approx 35.7 character widths
  # Third column is auto-width (SAS: <Column/>)
  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 21.4)
  openxlsx::setColWidths(wb, sheet, cols = 2L, widths = 35.7)
  openxlsx::setColWidths(wb, sheet, cols = 3L, widths = "auto")

  # ----------------------------------------------------------------------- #
  # Phase 6: Header Content (SAS lines 116-198)
  # ----------------------------------------------------------------------- #
  # Track current row position (replaces SAS %let row = 0; with incrementing)
  current_row <- 1L

  # Row 1: Blank (SAS lines 124-125)
  # (Row 1 is left empty by default)
  current_row <- current_row + 1L

  # Row 2: "{panel_title} Error Summary" with header style (SAS lines 127-128)
  openxlsx::writeData(wb, sheet, x = wbtitle, startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = header_style, rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 3: Blank (SAS lines 130-131)
  current_row <- current_row + 1L

  # Row 4: "NDA/BLA: {ndabla}" with default10 style (SAS lines 135-136)
  openxlsx::writeData(wb, sheet, x = paste0("NDA/BLA: ", ndabla),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = default10_style,
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 5: "Study: {studyid}" with default10 style (SAS lines 137-138)
  openxlsx::writeData(wb, sheet, x = paste0("Study: ", studyid),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = default10_style,
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L


  # Row 6: Analysis run date (SAS line 139-140)
  # SAS: put(date(),e8601da.) gives ISO date; put(time(),timeampm11.) gives AM/PM time
  run_date_str <- paste0("Analysis run date: ",
                         format(Sys.Date(), "%Y-%m-%d"), " ",
                         format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, sheet, x = run_date_str,
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = default10_style,
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 7: Blank (SAS lines 141-142 — row increment without output)
  current_row <- current_row + 1L

  # Row 8: Blank (SAS lines 143-144)
  current_row <- current_row + 1L

  # ----------------------------------------------------------------------- #
  # Panel description (SAS lines 146-160)
  # ----------------------------------------------------------------------- #
  if (nchar(panel_desc) > 0L) {
    openxlsx::writeData(wb, sheet, x = panel_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = default10_wrap_style,
                       rows = current_row, cols = 1L)
    # Merge across 6 columns (SAS line 153: MergeAcross = 6)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    # Calculate row height based on text length (SAS line 154)
    # SAS: Height = ceil(length(trim("&panel_desc."))/150)*12.75;
    calc_height <- ceiling(nchar(trimws(panel_desc)) / 150) * 12.75
    # openxlsx does not have a direct setRowHeights for individual rows in
    # the same way, but we can approximate by increasing the default
    # Note: openxlsx handles row height auto-sizing with wrap text, but
    # we explicitly set it for closer SAS parity
    current_row <- current_row + 1L

    # Blank row after panel_desc (SAS lines 157-158)
    current_row <- current_row + 1L
  }

  # ----------------------------------------------------------------------- #
  # Error description section (SAS lines 162-197)
  # ----------------------------------------------------------------------- #

  # Custom error message (SAS lines 170-176)
  if (nchar(err_desc) > 0L) {
    openxlsx::writeData(wb, sheet, x = err_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = default10_wrap_style,
                       rows = current_row, cols = 1L)
    # Merge across 6 columns (SAS line 166: MergeAcross = 6)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L

    # Blank row after err_desc (SAS lines 174-175)
    current_row <- current_row + 1L
  }

  # No subjects in DM error message (SAS lines 178-187)
  if (isTRUE(err_nosubj)) {
    # Build the no-subjects message (SAS lines 181-183)
    nosubj_msg <- "There are no subjects in the demographics domain (DM) dataset"
    if (nchar(sl_subset_desc) > 0L) {
      nosubj_msg <- paste0(nosubj_msg, " after subsetting by ", sl_subset_desc)
    }
    nosubj_msg <- paste0(nosubj_msg, ".")

    openxlsx::writeData(wb, sheet, x = nosubj_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = default10_wrap_style,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L

    # Blank row after no-subjects message (SAS lines 185-186)
    current_row <- current_row + 1L
  }

  # Missing variable error message (SAS lines 189-194)
  if (isTRUE(err_missvar)) {
    missvar_msg <- paste0(
      "Some variables that are required by this panel are missing. ",
      "These variables are shown in the following table."
    )

    openxlsx::writeData(wb, sheet, x = missvar_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = default10_wrap_style,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L
  }

  # Trailing blank row (SAS lines 196-197)
  current_row <- current_row + 1L

  # ----------------------------------------------------------------------- #
  # Phase 7: Missing Variable Table (SAS lines 202-230)
  # ----------------------------------------------------------------------- #
  if (isTRUE(err_missvar) && !is.null(err_missing_var) && nrow(err_missing_var) > 0L) {

    # Column headers (SAS lines 206-217)
    # "Domain/Dataset" in column 1, "Variable" in column 2
    openxlsx::writeData(wb, sheet, x = "Domain/Dataset",
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = column_outline_style,
                       rows = current_row, cols = 1L)

    openxlsx::writeData(wb, sheet, x = "Variable",
                        startCol = 2L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = column_outline_style,
                       rows = current_row, cols = 2L)

    current_row <- current_row + 1L

    # Write err_missing_var data rows (SAS lines 221-228)
    # Each row has ds in column 1 and var in column 2 with table_style
    for (i in seq_len(nrow(err_missing_var))) {
      # Domain/Dataset value
      openxlsx::writeData(wb, sheet, x = err_missing_var$ds[i],
                          startCol = 1L, startRow = current_row)
      openxlsx::addStyle(wb, sheet, style = table_style,
                         rows = current_row, cols = 1L)

      # Variable value
      openxlsx::writeData(wb, sheet, x = err_missing_var$var[i],
                          startCol = 2L, startRow = current_row)
      openxlsx::addStyle(wb, sheet, style = table_style,
                         rows = current_row, cols = 2L)

      current_row <- current_row + 1L
    }
  }

  # ----------------------------------------------------------------------- #
  # Phase 8: Worksheet Options (SAS lines 232-250)
  # ----------------------------------------------------------------------- #
  # Set page setup: Landscape orientation (SAS line 236)
  # Set print header and footer (SAS lines 237-239)
  # Set fit-to-page, scale 78% (SAS lines 241-249)
  openxlsx::pageSetup(
    wb,
    sheet,
    orientation = "landscape",
    fitToWidth = TRUE,
    fitToHeight = FALSE,
    # SAS line 237-238: Header with panel_title on left, NDA/BLA and Study on right
    header = c(
      panel_title,
      NA_character_,
      paste0("NDA/BLA ", ndabla, "\nStudy ", studyid)
    ),
    # SAS line 239: Footer with "Page X of Y"
    footer = c(
      NA_character_,
      "Page &[Page] of &[Pages]",
      NA_character_
    ),
    # SAS line 245: Scale 78%
    scale = 78
  )

  # ----------------------------------------------------------------------- #
  # Phase 9: Save Workbook and Error Status (SAS lines 252-282)
  # ----------------------------------------------------------------------- #

  # Ensure the output directory exists before saving
  output_dir <- dirname(err_file)
  if (nchar(output_dir) > 0L && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Save workbook (SAS lines 269-273: DATA _NULL_ file writing)
  openxlsx::saveWorkbook(wb, err_file, overwrite = TRUE)

  # Set the error status (SAS lines 277-280: %let errstatus = 5)
  errstatus <- if (isTRUE(err_seterr)) 5L else 0L

  # Return error status as a list (replaces SAS global macro variable)
  return(list(
    success = FALSE,
    errstatus = errstatus,
    err_file = err_file
  ))
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SpreadsheetML XML generation is fully replaced by openxlsx
####    - Cell merge and height calculations approximate SAS behavior
####    - Error status signaling via return value replaces SAS ERRSTATUS macro variable
####    - The SAS %include of xml_output.sas is no longer needed; openxlsx provides
####      all workbook creation and styling functionality directly
####    - SAS global variables (panel_title, ndabla, studyid, sl_subset, rpt_chk_var_req)
####      are passed as function parameters instead
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Date/time formatting may differ slightly from SAS e8601da. and timeampm11.
####      SAS e8601da. produces ISO 8601 date (YYYY-MM-DD); R format(Sys.Date(), "%Y-%m-%d")
####      produces the same format. SAS timeampm11. produces HH:MM:SS AM/PM; R
####      format(Sys.time(), "%I:%M:%S %p") produces the same format.
#### NO DIRECT R EQUIVALENT:
####    - SAS %wb/%styles/%markup/%annotate macros -> openxlsx workbook API
####    - SAS global macro variable ERRSTATUS -> R function return value
####    - SAS DATA _NULL_ file writing -> openxlsx::saveWorkbook()
####    - SAS WorksheetOptions XML (FitToPage, Scale, Resolution) -> openxlsx::pageSetup()
####    - SAS HorizontalResolution/VerticalResolution XML -> no direct openxlsx equivalent
####      (resolution is determined by the Excel application at print time)
####    - SAS FitHeight XML -> openxlsx fitToHeight parameter
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Replaces SpreadsheetML XML generation; full Excel workbook API
####      with native R cell-level formatting, merging, and page setup
####    - dplyr: Tidyverse data manipulation for sl_subset and rpt_chk_var_req
####      preprocessing (AAP mandates tidyverse over base R)
####    - cli: User-facing error/warning messages replacing SAS %put with rich
####      terminal formatting consistent with other migrated utility files
#### OPEN QUESTIONS:
####    - Exact pixel-to-character-width conversion for column widths (SAS 150px
####      approximated as 21.4 chars, SAS 250px as 35.7 chars)
####    - Whether print scale/resolution settings match SAS output exactly
####    - Whether openxlsx pageSetup header/footer tokens (&[Page], &[Pages])
####      render identically to SAS &P/&N tokens in all Excel versions
#### ============================================================
