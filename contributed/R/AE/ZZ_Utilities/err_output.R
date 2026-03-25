###############################################################################
#         PROGRAM NAME: Generic Panel Error Output (R Migration)              #
#                                                                             #
#          DESCRIPTION: Create an Excel error summary workbook when a panel   #
#                       encounters missing variables or no subjects.           #
#                       Replaces SAS SpreadsheetML XML generation with         #
#                       openxlsx workbook API.                                #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: March 28, 2011                                        #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/AE/ZZ_Utilities/err_output.sas (282 lines) #
#     - Migration target: Idiomatic R using openxlsx for Excel output         #
#     - SAS %error_summary macro -> R error_summary() function                #
#     - SpreadsheetML XML -> openxlsx workbook API                            #
#     - SAS global macro variables -> R function parameters                   #
#     - SAS %put -> cli::cli_inform() messages                                #
#     - SAS %wb/%styles -> create_workbook()/create_styles() from xml_output.R#
#                                                                             #
#  EXTERNAL FILES USED: xml_output.R -- Excel workbook/style functions        #
#                                                                             #
#  PARAMETERS REQUIRED: err_file -- filename and path of the output           #
#                       panel_title -- title of the panel                     #
#                       ndabla -- NDA/BLA identifier                          #
#                       studyid -- study identifier                           #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by AE analysis scripts.       #
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
# Source xml_output.R dependency
# Replaces SAS: %include "&utilpath.\xml_output.sas"; (SAS line 42)
# Provides: create_workbook() and create_styles()
# --------------------------------------------------------------------------- #
if (!exists("create_workbook", mode = "function") ||
    !exists("create_styles", mode = "function")) {
  local({
    # Determine the directory containing this script
    this_dir <- tryCatch(
      dirname(sys.frame(2)$ofile),
      error = function(e) NULL
    )
    # Fallback: try known relative path from project root
    if (is.null(this_dir) || !nzchar(this_dir)) {
      this_dir <- "contributed/R/AE/ZZ_Utilities"
    }
    xml_path <- file.path(this_dir, "xml_output.R")
    if (file.exists(xml_path)) {
      source(xml_path, local = FALSE)
    } else {
      cli::cli_warn(paste0(
        "xml_output.R not found at {.path ", xml_path, "}. ",
        "Ensure create_workbook() and create_styles() are available."
      ))
    }
  })
}


# --------------------------------------------------------------------------- #
# error_summary() -- Generate Excel Error Summary Workbook
# --------------------------------------------------------------------------- #
#' Generate an Excel Error Summary Workbook
#'
#' Creates an Excel workbook with error summary information when a panel
#' encounters missing variables or no subjects. This replaces the SAS
#' \code{\%error_summary} macro (SAS lines 44-282) which generated
#' SpreadsheetML XML output.
#'
#' @param err_file Character. Output file path for the Excel workbook.
#'   Must be parameterized -- no hardcoded paths.
#' @param panel_title Character. Title of the panel (e.g., "Adverse Events").
#'   Used in the workbook header and print header.
#' @param ndabla Character. NDA/BLA identifier for the study.
#' @param studyid Character. Study identifier.
#' @param err_nosubj Logical. If TRUE, indicates no subjects were found in DM.
#'   Defaults to FALSE. Replaces SAS macro param err_nosubj=0.
#' @param err_missvar Logical. If TRUE, indicates required variables are missing.
#'   Defaults to FALSE. Replaces SAS macro param err_missvar=0.
#' @param err_seterr Logical. If TRUE, sets error status to 5 in the return
#'   value. Defaults to TRUE. Replaces SAS macro param err_seterr=1.
#' @param err_desc Character. Optional custom error description text.
#'   Defaults to "". Replaces SAS macro param err_desc=.
#' @param panel_desc Character. Optional panel description text. If non-empty,
#'   displayed with word wrap and merged across 7 columns.
#'   Replaces SAS global macro variable panel_desc.
#' @param sl_subset Data frame or NULL. Script Launcher subsetting data with
#'   columns \code{outer_operator} and \code{name}. Used when err_nosubj is
#'   TRUE to describe the subsetting criteria.
#'   Replaces SAS dataset sl_subset.
#' @param rpt_chk_var_req Data frame or NULL. Required variable check data with
#'   columns \code{ind}, \code{ds}, \code{var}. Used when err_missvar is TRUE
#'   to list missing variables.
#'   Replaces SAS dataset rpt_chk_var_req.
#'
#' @return A named list with component:
#'   \describe{
#'     \item{errstatus}{Integer. 5 if err_seterr is TRUE, 0 otherwise.
#'       Replaces SAS global macro variable \code{errstatus} (SAS lines 278-280).}
#'   }
#'
#' @examples
#' \dontrun{
#' # No subjects error
#' result <- error_summary(
#'   err_file    = "output/error_summary.xlsx",
#'   panel_title = "Demographics",
#'   ndabla      = "125476",
#'   studyid     = "C13007",
#'   err_nosubj  = TRUE,
#'   sl_subset   = data.frame(
#'     outer_operator = c("and", "and"),
#'     name = c("AGE > 18", "SEX = F"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#'
#' # Missing variables error
#' result <- error_summary(
#'   err_file        = "output/error_summary.xlsx",
#'   panel_title     = "Demographics",
#'   ndabla          = "125476",
#'   studyid         = "C13007",
#'   err_missvar     = TRUE,
#'   rpt_chk_var_req = data.frame(
#'     ind = c(1, 0, 0),
#'     ds  = c("DM", "DM", "DS"),
#'     var = c("USUBJID", "AGE", "DSDECOD"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' }
error_summary <- function(err_file,
                          panel_title,
                          ndabla,
                          studyid,
                          err_nosubj = FALSE,
                          err_missvar = FALSE,
                          err_seterr = TRUE,
                          err_desc = "",
                          panel_desc = "",
                          sl_subset = NULL,
                          rpt_chk_var_req = NULL) {

  # ----------------------------------------------------------------------- #
  # Input validation
  # ----------------------------------------------------------------------- #
  if (!is.character(err_file) || length(err_file) != 1L || !nzchar(err_file)) {
    cli::cli_warn("{.arg err_file} must be a non-empty character string.")
    return(list(errstatus = if (isTRUE(err_seterr)) 5L else 0L))
  }
  if (!is.character(panel_title) || length(panel_title) != 1L) {
    cli::cli_warn("{.arg panel_title} must be a single character string.")
    panel_title <- as.character(panel_title)[1L]
  }
  if (!is.character(ndabla)) ndabla <- as.character(ndabla)
  if (!is.character(studyid)) studyid <- as.character(studyid)
  if (!is.character(err_desc)) err_desc <- as.character(err_desc)
  if (!is.character(panel_desc)) panel_desc <- as.character(panel_desc)

  # Construct the workbook title (SAS line 46: %let wbtitle = &panel_title. Error Summary)
  wbtitle <- paste(panel_title, "Error Summary")

  # Sheet name constant

  sheet <- "Error Summary"

  # ----------------------------------------------------------------------- #
  # No-Subjects Preprocessing (SAS lines 48-69)
  # Conditional on err_nosubj
  # ----------------------------------------------------------------------- #
  sl_subset_desc <- ""

  if (isTRUE(err_nosubj)) {
    # SAS line 50: %put PANEL NO SUBJECTS ERROR PREPROCESSING
    cli::cli_inform("PANEL NO SUBJECTS ERROR PREPROCESSING")

    # Count rows in sl_subset (SAS lines 52-55: select count(1) into: sl_subset_count)
    sl_subset_count <- 0L
    if (!is.null(sl_subset) && is.data.frame(sl_subset)) {
      sl_subset_count <- nrow(sl_subset)
    }

    # Build subset description (SAS lines 57-68)
    if (sl_subset_count > 0L) {
      # SAS lines 59-61: select lowcase(outer_operator) into: operator
      operator <- sl_subset %>%
        dplyr::pull(.data$outer_operator) %>%
        tolower() %>%
        unique()
      # SAS selects into a single macro variable -- use first value
      operator <- operator[1L]

      # SAS lines 63-64: select distinct name into: sl_subset_desc separated by " &operator. "
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
  # Missing-Variable Preprocessing (SAS lines 71-80)
  # Conditional on err_missvar
  # ----------------------------------------------------------------------- #
  err_missing_var <- NULL

  if (isTRUE(err_missvar)) {
    # SAS line 73: %put PANEL MISSING VARIABLE ERROR PREPROCESSING
    cli::cli_inform("PANEL MISSING VARIABLE ERROR PREPROCESSING")

    # SAS lines 75-79: data err_missing_var; set rpt_chk_var_req; where ind ne 1; keep ds var;
    if (!is.null(rpt_chk_var_req) && is.data.frame(rpt_chk_var_req)) {
      err_missing_var <- rpt_chk_var_req %>%
        dplyr::filter(.data$ind != 1) %>%
        dplyr::select("ds", "var")
    } else {
      # Safety fallback: empty data frame if rpt_chk_var_req is NULL
      err_missing_var <- data.frame(ds = character(0), var = character(0),
                                    stringsAsFactors = FALSE)
    }
  }

  # ----------------------------------------------------------------------- #
  # Create Workbook (SAS lines 83-113)
  # ----------------------------------------------------------------------- #
  # SAS line 83: %put SCRIPT LAUNCHER ERROR SUMMARY OUTPUT
  cli::cli_inform("SCRIPT LAUNCHER ERROR SUMMARY OUTPUT")

  # SAS line 87: %wb -- create workbook via xml_output.R's create_workbook()
  wb <- create_workbook(title = wbtitle)

  # SAS line 88: %styles -- create style gallery via xml_output.R's create_styles()
  styles <- create_styles()

  # SAS lines 91-93: add "Error Summary" worksheet
  openxlsx::addWorksheet(wb, sheet)

  # SAS lines 104-108: set column widths
  # Column 1: ss:Width="150" -> 150/7 ≈ 21.4 character widths
  # Column 2: ss:Width="250" -> 250/7 ≈ 35.7 character widths
  # Column 3: auto width (SAS: <Column/>)
  openxlsx::setColWidths(wb, sheet, cols = 1:3,
                         widths = c(150 / 7, 250 / 7, "auto"))

  # ----------------------------------------------------------------------- #
  # Header Section (SAS lines 116-198)
  # Track current row position (replaces SAS %let row = 0 with increment)
  # ----------------------------------------------------------------------- #
  current_row <- 0L

  # Row 1: Blank (SAS lines 124-125: Row=1; Data=''; output)
  current_row <- current_row + 1L
  # (left empty by default)

  # Row 2: "{panel_title} Error Summary" with Header style (SAS lines 127-128)
  current_row <- current_row + 1L
  openxlsx::writeData(wb, sheet, x = wbtitle,
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles$Header,
                     rows = current_row, cols = 1L)

  # Row 3: Blank (SAS lines 130-131)
  current_row <- current_row + 1L

  # Row 4: "NDA/BLA: {ndabla}" with Default10 style (SAS lines 133-136)
  current_row <- current_row + 1L
  openxlsx::writeData(wb, sheet, x = paste0("NDA/BLA: ", ndabla),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles$Default10,
                     rows = current_row, cols = 1L)

  # Row 5: "Study: {studyid}" with Default10 style (SAS lines 137-138)
  current_row <- current_row + 1L
  openxlsx::writeData(wb, sheet, x = paste0("Study: ", studyid),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles$Default10,
                     rows = current_row, cols = 1L)

  # Row 6: "Analysis run date: YYYY-MM-DD HH:MM:SS AM/PM" with Default10
  # SAS line 140: put(date(),e8601da.) || ' ' || put(time(),timeampm11.)
  current_row <- current_row + 1L
  run_date_str <- paste("Analysis run date:",
                        format(Sys.Date(), "%Y-%m-%d"),
                        format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, sheet, x = run_date_str,
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles$Default10,
                     rows = current_row, cols = 1L)

  # Row 7: Blank (SAS lines 141-142: row increment without output)
  current_row <- current_row + 1L

  # Row 8: Blank (SAS lines 143-144: Data=''; output)
  current_row <- current_row + 1L

  # ----------------------------------------------------------------------- #
  # Panel Description (SAS lines 146-160)
  # Conditional: only if panel_desc is non-empty
  # ----------------------------------------------------------------------- #
  if (nchar(panel_desc) > 0L) {
    current_row <- current_row + 1L

    # Write panel_desc with Default10Wrap style (SAS line 149: StyleID = 'Default10Wrap')
    openxlsx::writeData(wb, sheet, x = panel_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$Default10Wrap,
                       rows = current_row, cols = 1L)

    # Merge across 7 columns (SAS line 153: MergeAcross = 6 means span 7 total)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)

    # Row height based on text length (SAS line 154:
    # Height = ceil(length(trim("&panel_desc."))/150)*12.75)
    pd_height <- ceiling(nchar(trimws(panel_desc)) / 150) * 12.75
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = pd_height)

    # Blank row after panel_desc (SAS lines 157-158)
    current_row <- current_row + 1L
  }

  # ----------------------------------------------------------------------- #
  # Error Description Section (SAS lines 162-197)
  # ----------------------------------------------------------------------- #

  # Custom error message (SAS lines 170-176)
  if (nchar(err_desc) > 0L) {
    current_row <- current_row + 1L

    openxlsx::writeData(wb, sheet, x = err_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$Default10Wrap,
                       rows = current_row, cols = 1L)

    # Merge across 7 columns (SAS line 166: MergeAcross = 6)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)

    # Row height based on text length (SAS line 167:
    # Height = ceil(length(trim(Data))/130)*12.75)
    ed_height <- ceiling(nchar(trimws(err_desc)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = ed_height)

    # Blank row after err_desc (SAS lines 174-175)
    current_row <- current_row + 1L
  }

  # No-subjects error message (SAS lines 178-187)
  if (isTRUE(err_nosubj)) {
    current_row <- current_row + 1L

    # Build the no-subjects message (SAS lines 181-183)
    nosubj_msg <- "There are no subjects in the demographics domain (DM) dataset"
    if (nchar(sl_subset_desc) > 0L) {
      nosubj_msg <- paste0(nosubj_msg, " after subsetting by ", sl_subset_desc)
    }
    nosubj_msg <- paste0(nosubj_msg, ".")

    openxlsx::writeData(wb, sheet, x = nosubj_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$Default10Wrap,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)

    # Row height: SAS line 167 pattern — estimate from message length
    ns_height <- ceiling(nchar(trimws(nosubj_msg)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = ns_height)

    # Blank row after no-subjects message (SAS lines 185-186)
    current_row <- current_row + 1L
  }

  # Missing variable error message (SAS lines 190-194)
  if (isTRUE(err_missvar)) {
    current_row <- current_row + 1L

    missvar_msg <- paste0(
      "Some variables that are required by this panel are missing. ",
      "These variables are shown in the following table."
    )
    openxlsx::writeData(wb, sheet, x = missvar_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$Default10Wrap,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)

    # Row height for missing variable message
    mv_height <- ceiling(nchar(trimws(missvar_msg)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = mv_height)
  }

  # Trailing blank row (SAS lines 196-197)
  current_row <- current_row + 1L

  # ----------------------------------------------------------------------- #
  # Missing Variable Table (SAS lines 202-230)
  # Conditional on err_missvar
  # ----------------------------------------------------------------------- #
  if (isTRUE(err_missvar) && !is.null(err_missing_var) &&
      nrow(err_missing_var) > 0L) {

    # Column headers (SAS lines 206-217)
    # "Domain/Dataset" in col 1, "Variable" in col 2 with ColumnOutline style
    openxlsx::writeData(wb, sheet, x = "Domain/Dataset",
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$ColumnOutline,
                       rows = current_row, cols = 1L)

    openxlsx::writeData(wb, sheet, x = "Variable",
                        startCol = 2L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles$ColumnOutline,
                       rows = current_row, cols = 2L)

    # Row height: 30 (SAS line 214: Height = 30)
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = 30)

    current_row <- current_row + 1L

    # Data rows from err_missing_var (SAS lines 221-228)
    # Write ds and var columns with Table style
    for (i in seq_len(nrow(err_missing_var))) {
      # Domain/Dataset value
      openxlsx::writeData(wb, sheet, x = err_missing_var$ds[i],
                          startCol = 1L, startRow = current_row)
      openxlsx::addStyle(wb, sheet, style = styles$Table,
                         rows = current_row, cols = 1L)

      # Variable value
      openxlsx::writeData(wb, sheet, x = err_missing_var$var[i],
                          startCol = 2L, startRow = current_row)
      openxlsx::addStyle(wb, sheet, style = styles$Table,
                         rows = current_row, cols = 2L)

      current_row <- current_row + 1L
    }
  }

  # ----------------------------------------------------------------------- #
  # Page Setup (SAS lines 232-250)
  # ----------------------------------------------------------------------- #
  # Landscape orientation, fit-to-page (SAS lines 236, 241, 243)
  openxlsx::pageSetup(wb, sheet,
                      orientation = "landscape",
                      fitToWidth = TRUE,
                      fitToHeight = FALSE)

  # Header and Footer (SAS lines 237-239)
  # SAS header: &L panel_title  &R NDA/BLA ndabla \n Study studyid
  # SAS footer: Page &P of &N
  openxlsx::setHeaderFooter(wb, sheet,
    header = c(
      panel_title,                                          # left section
      NA_character_,                                        # center section
      paste0("NDA/BLA ", ndabla, "\nStudy ", studyid)       # right section
    ),
    footer = c(
      NA_character_,                                        # left section
      "Page &[Page] of &[Pages]",                           # center section
      NA_character_                                         # right section
    )
  )

  # ----------------------------------------------------------------------- #
  # Save Workbook (SAS lines 252-273)
  # ----------------------------------------------------------------------- #
  # Ensure the output directory exists before saving
  output_dir <- dirname(err_file)
  if (nzchar(output_dir) && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Save workbook (replaces SAS DATA _NULL_; file "&err_file." ls=32767; put string;)
  tryCatch(
    openxlsx::saveWorkbook(wb, err_file, overwrite = TRUE),
    error = function(e) {
      cli::cli_warn("Failed to save error workbook to {.path {err_file}}: {e$message}")
    }
  )

  # ----------------------------------------------------------------------- #
  # Error Status (SAS lines 277-280)
  # SAS: %if &err_seterr. = 1 %then %do; %let errstatus = 5; %end;
  # ----------------------------------------------------------------------- #
  errstatus <- if (isTRUE(err_seterr)) 5L else 0L

  # Return error status as a named list (replaces SAS global macro variable)
  list(errstatus = errstatus)
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - openxlsx column width conversion from SpreadsheetML: SAS Width
####      in points divided by 7 gives approximate character widths
####      (150/7 ~= 21.4, 250/7 ~= 35.7)
####    - err_nosubj and err_missvar are logical (TRUE/FALSE) in R,
####      replacing SAS numeric 0/1 macro variables
####    - err_seterr is logical TRUE (default) replacing SAS numeric 1
####    - SAS global macro variables (panel_title, ndabla, studyid,
####      panel_desc) are passed as explicit function parameters
####    - create_workbook() and create_styles() from xml_output.R replace
####      the inline SAS %wb and %styles macros
####    - SAS MergeAcross=6 means merge 7 columns (cols 1:7 in openxlsx)
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- this is an error output module producing text only
####    - Date/time formatting: SAS e8601da. -> R "%Y-%m-%d" (identical);
####      SAS timeampm11. -> R "%I:%M:%S %p" (identical format)
#### NO DIRECT R EQUIVALENT:
####    - SAS SpreadsheetML streaming XML generation -> openxlsx API
####    - SAS %markup/%annotate macros -> direct openxlsx writeData/addStyle
####    - SAS &strlen. length variable -> not needed (openxlsx handles internally)
####    - SAS WorksheetOptions XML -> openxlsx::pageSetup()
####    - SAS HorizontalResolution/VerticalResolution XML -> no openxlsx
####      equivalent (resolution is determined by Excel at print time)
####    - SAS FitHeight=100 -> openxlsx fitToHeight=FALSE (unlimited pages)
####    - SAS Scale=78 -> omitted because fitToWidth overrides scale in Excel
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Replaces SpreadsheetML XML generation with native R
####      Excel workbook API providing cell-level formatting, merging,
####      page setup, and header/footer support
####    - dplyr: Tidyverse data manipulation for filtering rpt_chk_var_req
####      (AAP mandates tidyverse over base R)
####    - cli: User-facing diagnostic messages replacing SAS %put with
####      rich terminal formatting via cli_inform()/cli_warn()
#### OPEN QUESTIONS:
####    - Verify openxlsx setHeaderFooter() tokens (&[Page], &[Pages])
####      render identically to SAS &P/&N tokens in all Excel versions
####    - Confirm column width mapping accuracy (SAS points / 7)
####    - Whether openxlsx newline in header right section (\n between
####      NDA/BLA and Study) renders correctly in all Excel versions
#### ============================================================
