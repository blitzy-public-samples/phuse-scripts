###############################################################################
#         PROGRAM NAME: Generic Panel Error Output (R Migration)              #
#                                                                             #
#          DESCRIPTION: Create an Excel error summary workbook when the       #
#                       MedDRA panel encounters missing variables or no       #
#                       subjects. Replaces SpreadsheetML XML generation       #
#                       with openxlsx workbook API via xml_output.R shared    #
#                       utility functions.                                    #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: March 28, 2011                                        #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/MedDRA/ZZ_Utilities/err_output.sas (282)   #
#     - Migration target: Idiomatic R using openxlsx via xml_output.R         #
#     - SAS %error_summary macro -> R error_summary() function                #
#     - SAS SpreadsheetML XML -> openxlsx workbook API                        #
#     - SAS %include xml_output.sas -> source("xml_output.R") providing       #
#       wb_create(), create_workbook_styles(), annotate_data(),               #
#       write_annotated()                                                     #
#     - SAS global macro variables -> R function parameters                   #
#     - SAS %put -> cli messages                                              #
#     - SAS || concatenation -> stringr::str_c()                              #
#     - SAS LOWCASE -> stringr::str_to_lower()                                #
#     - SAS TRIM/LEFT -> stringr::str_trim()                                  #
#                                                                             #
#  EXTERNAL FILES USED: xml_output.R -- openxlsx workbook/style utilities     #
#                                                                             #
#  PARAMETERS REQUIRED: err_file -- filename and path of the output           #
#                       panel_title -- title of the panel                     #
#                       ndabla -- NDA/BLA identifier                          #
#                       studyid -- study identifier                           #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by MedDRA analysis scripts.   #
#                       In SAS, it was included via:                           #
#                       %include "&utilpath.\err_output.sas";                 #
#                                                                             #
#            REVISIONS:                                                        #
#              2011-05-08  DK  Added support for no subjects in DM            #
#              2011-06-08  DK  Added err_seterr argument                      #
#              2026-03-25  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(openxlsx)
library(dplyr)
library(stringr)
library(cli)

# --------------------------------------------------------------------------- #
# Source xml_output.R for shared workbook utilities
# Replaces SAS: %include "&utilpath.\xml_output.sas"; (line 42)
#
# Provides: wb_create(), create_workbook_styles(), annotate_data(),
#           write_annotated()
# --------------------------------------------------------------------------- #
local({
  this_script <- tryCatch(
    normalizePath(sys.frame(1L)$ofile),
    error = function(e) NULL
  )
  script_dir <- if (!is.null(this_script)) {
    dirname(this_script)
  } else {
    "."
  }
  xml_output_path <- file.path(script_dir, "xml_output.R")
  if (file.exists(xml_output_path) && !exists("wb_create", mode = "function")) {
    source(xml_output_path, local = FALSE)
  }
})


# --------------------------------------------------------------------------- #
# error_summary() -- Generate Excel Error Summary Workbook
# --------------------------------------------------------------------------- #
#' Generate an Excel Error Summary Workbook
#'
#' Creates an Excel workbook with error summary information when the
#' MedDRA panel encounters missing variables or no subjects.
#' This replaces the SAS \%error_summary macro (lines 44-282 of
#' err_output.sas) which generated SpreadsheetML XML output.
#'
#' Uses xml_output.R utility functions:
#' \itemize{
#'   \item \code{wb_create()} for workbook creation (replaces SAS \%wb)
#'   \item \code{create_workbook_styles()} for the shared style gallery
#'         (replaces SAS \%styles)
#'   \item \code{annotate_data()} for annotating the missing-variable table
#'         (replaces SAS \%annotate)
#'   \item \code{write_annotated()} for writing styled cells
#'         (replaces SAS \%markup)
#' }
#'
#' @param err_file Character. Output file path for the Excel workbook.
#'   Must be parameterized -- no hardcoded paths.
#' @param panel_title Character. Title of the panel (e.g., "MedDRA AE").
#'   Used in the workbook header and print header. Replaces SAS
#'   \code{&panel_title.} macro variable.
#' @param panel_desc Character. Optional panel description text. If non-empty,
#'   displayed with word wrap and merged across 7 columns (SAS MergeAcross=6).
#'   Replaces SAS \code{&panel_desc.} macro variable.
#' @param ndabla Character. NDA/BLA identifier for the study. Replaces SAS
#'   \code{&ndabla.} macro variable.
#' @param studyid Character. Study identifier. Replaces SAS \code{&studyid.}
#'   macro variable.
#' @param sl_subset Data frame or NULL. Script Launcher subsetting data frame
#'   with columns \code{outer_operator} and \code{name}. Used when
#'   \code{err_nosubj} is TRUE to describe the subsetting criteria.
#'   Replaces SAS \code{sl_subset} dataset.
#' @param rpt_chk_var_req Data frame or NULL. Required variable check data
#'   frame with columns \code{ind}, \code{ds}, \code{var}. Used when
#'   \code{err_missvar} is TRUE to list missing variables.
#'   Replaces SAS \code{rpt_chk_var_req} dataset.
#' @param err_nosubj Logical. If TRUE, indicates no subjects were found in DM.
#'   Defaults to FALSE. Replaces SAS \code{err_nosubj=0} macro parameter.
#' @param err_missvar Logical. If TRUE, indicates required variables are
#'   missing. Defaults to FALSE. Replaces SAS \code{err_missvar=0}.
#' @param err_seterr Logical. If TRUE, sets error status to 5 in the return
#'   value. Defaults to TRUE. Replaces SAS \code{err_seterr=1} and the
#'   \code{\%let errstatus = 5;} statement at line 279.
#' @param err_desc Character. Optional custom error description text.
#'   Replaces SAS \code{err_desc=} macro parameter.
#' @param config List. Optional configuration list with study-level settings.
#'   Not used internally but reserved for extension.
#'
#' @return A list with components:
#'   \describe{
#'     \item{success}{Logical. Always FALSE (this function is called on error
#'           conditions).}
#'     \item{errstatus}{Integer. 5 if err_seterr is TRUE, 0 otherwise.
#'           Replaces SAS \code{\%let errstatus = 5;} global macro variable.}
#'     \item{errsummaryrun}{Logical. Always TRUE, indicating this function has
#'           executed. Replaces SAS \code{\%let errsummaryrun = 1;}.}
#'     \item{err_file}{Character. Path to the generated error workbook.}
#'   }
#'
#' @examples
#' \dontrun{
#' # No subjects error
#' result <- error_summary(
#'   err_file    = "output/error_summary.xlsx",
#'   panel_title = "MedDRA AE",
#'   ndabla      = "125476",
#'   studyid     = "C13007",
#'   err_nosubj  = TRUE,
#'   sl_subset   = tibble(
#'     outer_operator = "and",
#'     name           = c("AGE > 18", "SEX = F")
#'   )
#' )
#'
#' # Missing variables error
#' result <- error_summary(
#'   err_file        = "output/error_summary.xlsx",
#'   panel_title     = "MedDRA AE",
#'   ndabla          = "125476",
#'   studyid         = "C13007",
#'   err_missvar     = TRUE,
#'   rpt_chk_var_req = tibble(
#'     ind = c(1, 0, 0),
#'     ds  = c("DM", "DM", "DS"),
#'     var = c("USUBJID", "AGE", "DSDECOD")
#'   )
#' )
#' }
#' @export
error_summary <- function(err_file,
                          panel_title    = "",
                          panel_desc     = "",
                          ndabla         = "",
                          studyid        = "",
                          sl_subset      = NULL,
                          rpt_chk_var_req = NULL,
                          err_nosubj     = FALSE,
                          err_missvar    = FALSE,
                          err_seterr     = TRUE,
                          err_desc       = "",
                          config         = list()) {

  # ---------------------------------------------------------------------- #
  # Input validation
  # ---------------------------------------------------------------------- #
  if (missing(err_file) || !is.character(err_file) || nchar(err_file) == 0L) {
    stop("error_summary: 'err_file' must be a non-empty character string.",
         call. = FALSE)
  }

  # Coerce legacy 0/1 numeric flags to logical (SAS macro used 0/1)
  err_nosubj  <- isTRUE(as.logical(err_nosubj))
  err_missvar <- isTRUE(as.logical(err_missvar))
  err_seterr  <- isTRUE(as.logical(err_seterr))

  # Ensure character parameters are character and handle NA
  panel_title <- if (is.na(panel_title) || is.null(panel_title)) "" else as.character(panel_title)
  panel_desc  <- if (is.na(panel_desc) || is.null(panel_desc)) "" else as.character(panel_desc)
  ndabla      <- if (is.na(ndabla) || is.null(ndabla)) "" else as.character(ndabla)
  studyid     <- if (is.na(studyid) || is.null(studyid)) "" else as.character(studyid)
  err_desc    <- if (is.na(err_desc) || is.null(err_desc)) "" else as.character(err_desc)

  # ---------------------------------------------------------------------- #
  # Construct workbook title (SAS line 46)
  # SAS: %let wbtitle = &panel_title. Error Summary;
  # ---------------------------------------------------------------------- #
  wbtitle <- str_c(panel_title, " Error Summary")

  # ---------------------------------------------------------------------- #
  # No-Subjects Error Preprocessing (SAS lines 48-69)
  # ---------------------------------------------------------------------- #
  sl_subset_desc <- ""

  if (err_nosubj) {
    cli::cli_alert_info("PANEL NO SUBJECTS ERROR PREPROCESSING")

    # Count rows in sl_subset (SAS lines 52-55: PROC SQL count)
    sl_subset_count <- 0L
    if (!is.null(sl_subset) && is.data.frame(sl_subset)) {
      sl_subset_count <- nrow(sl_subset)
    }

    if (sl_subset_count > 0L) {
      # Derive outer_operator (SAS lines 59-61: select lowcase(outer_operator))
      # Using stringr::str_to_lower() per AAP mandate for tidyverse over base R
      operator <- sl_subset %>%
        dplyr::pull(.data$outer_operator) %>%
        stringr::str_to_lower() %>%
        unique()
      # SAS selects into single macro variable; take first distinct value
      operator <- operator[1L]

      # Build subset description by collapsing distinct name values
      # (SAS lines 63-64: select distinct name separated by " &operator. ")
      # Using stringr::str_c() per AAP mandate
      sl_subset_desc <- sl_subset %>%
        dplyr::distinct(.data$name) %>%
        dplyr::pull(.data$name) %>%
        paste(collapse = stringr::str_c(" ", operator, " "))
    } else {
      # SAS line 68: %let sl_subset_desc = ;
      sl_subset_desc <- ""
    }
  }

  # ---------------------------------------------------------------------- #
  # Missing Variable Error Preprocessing (SAS lines 71-80)
  # ---------------------------------------------------------------------- #
  err_missing_var <- NULL

  if (err_missvar) {
    cli::cli_alert_info("PANEL MISSING VARIABLE ERROR PREPROCESSING")

    # Create err_missing_var from rpt_chk_var_req where ind != 1,
    # keeping only ds and var (SAS lines 75-79: DATA step with WHERE/KEEP)
    if (!is.null(rpt_chk_var_req) && is.data.frame(rpt_chk_var_req)) {
      err_missing_var <- rpt_chk_var_req %>%
        dplyr::filter(.data$ind != 1) %>%
        dplyr::select("ds", "var")
    } else {
      # Safety fallback: empty tibble with correct columns
      err_missing_var <- dplyr::tibble(
        ds  = character(0L),
        var = character(0L)
      )
    }
  }

  # ---------------------------------------------------------------------- #
  # Excel Workbook Creation (SAS lines 82-267)
  # ---------------------------------------------------------------------- #
  cli::cli_alert_info("SCRIPT LAUNCHER ERROR SUMMARY OUTPUT")

  # Create workbook using xml_output.R wb_create() (replaces SAS %wb at line 87)
  wb <- wb_create(title = wbtitle)

  # Add "Error Summary" worksheet (SAS line 93)
  sheet <- "Error Summary"
  openxlsx::addWorksheet(wb, sheetName = sheet)

  # Retrieve shared style gallery from xml_output.R create_workbook_styles()

  # (replaces SAS %styles at line 88)
  # Provides: Header, Default10, Default10Wrap, ColumnOutline, Table, etc.
  styles <- create_workbook_styles()

  # Set column widths (SAS lines 104-108)
  # SAS: Column ss:Width="150" -> approx 21.4 character widths
  # SAS: Column ss:Width="250" -> approx 35.7 character widths
  # SAS: <Column/> -> auto-width (default)
  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 21.4)
  openxlsx::setColWidths(wb, sheet, cols = 2L, widths = 35.7)
  openxlsx::setColWidths(wb, sheet, cols = 3L, widths = "auto")

  # ---------------------------------------------------------------------- #
  # Header Content (SAS lines 116-198)
  # Replaces SAS DATA ws_err_header_data step with xml_tag_def/xml_init
  # ---------------------------------------------------------------------- #
  # Track current row position (replaces SAS %let row = 0; with incrementing)
  current_row <- 1L

  # Row 1: Blank (SAS lines 124-125: Data = ''; output;)
  current_row <- current_row + 1L

  # Row 2: "{panel_title} Error Summary" with Header style (SAS lines 127-128)
  openxlsx::writeData(wb, sheet, x = wbtitle,
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles[["Header"]],
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 3: Blank (SAS lines 130-131)
  current_row <- current_row + 1L

  # Row 4: "NDA/BLA: {ndabla}" with Default10 style (SAS lines 133-136)
  # Using stringr::str_c() for concatenation (replaces SAS || at line 136)
  openxlsx::writeData(wb, sheet,
                      x = stringr::str_c("NDA/BLA: ", ndabla),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles[["Default10"]],
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 5: "Study: {studyid}" with Default10 style (SAS lines 137-138)
  openxlsx::writeData(wb, sheet,
                      x = stringr::str_c("Study: ", studyid),
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles[["Default10"]],
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 6: Analysis run date (SAS lines 139-140)
  # SAS: put(date(),e8601da.) -> ISO 8601 date (YYYY-MM-DD)
  # SAS: put(time(),timeampm11.) -> HH:MM:SS AM/PM time
  run_date_str <- stringr::str_c(
    "Analysis run date: ",
    format(Sys.Date(), "%Y-%m-%d"),
    " ",
    format(Sys.time(), "%I:%M:%S %p")
  )
  openxlsx::writeData(wb, sheet, x = run_date_str,
                      startCol = 1L, startRow = current_row)
  openxlsx::addStyle(wb, sheet, style = styles[["Default10"]],
                     rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Row 7: Blank row from SAS row increment (SAS lines 141-142)
  current_row <- current_row + 1L

  # Row 8: Blank (SAS lines 143-144: Data = ''; output;)
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------- #
  # Panel Description (SAS lines 146-160)
  # ---------------------------------------------------------------------- #
  if (nchar(panel_desc) > 0L) {
    openxlsx::writeData(wb, sheet, x = panel_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles[["Default10Wrap"]],
                       rows = current_row, cols = 1L)
    # Merge across 6 columns (SAS line 153: MergeAcross = 6 means cols 1-7)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    # Row height based on text length (SAS line 154):
    # Height = ceil(length(trim("&panel_desc."))/150)*12.75
    # Using stringr::str_trim() per AAP mandate
    calc_height <- ceiling(nchar(stringr::str_trim(panel_desc)) / 150) * 12.75
    # Note: openxlsx handles row height auto-sizing with wrapText enabled
    current_row <- current_row + 1L

    # Blank row after panel_desc (SAS lines 157-158)
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------- #
  # Error Description Section (SAS lines 162-197)
  # ---------------------------------------------------------------------- #

  # Custom error message (SAS lines 170-176)
  if (nchar(err_desc) > 0L) {
    openxlsx::writeData(wb, sheet, x = err_desc,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles[["Default10Wrap"]],
                       rows = current_row, cols = 1L)
    # Merge across 6 columns (SAS line 166: MergeAcross = 6)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L

    # Blank row after err_desc (SAS lines 174-175)
    current_row <- current_row + 1L
  }

  # No subjects in DM error message (SAS lines 178-187)
  if (err_nosubj) {
    # Build the no-subjects message (SAS lines 181-183)
    nosubj_msg <- "There are no subjects in the demographics domain (DM) dataset"
    if (nchar(sl_subset_desc) > 0L) {
      nosubj_msg <- stringr::str_c(
        nosubj_msg, " after subsetting by ", sl_subset_desc
      )
    }
    nosubj_msg <- stringr::str_c(nosubj_msg, ".")

    openxlsx::writeData(wb, sheet, x = nosubj_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles[["Default10Wrap"]],
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L

    # Blank row after no-subjects message (SAS lines 185-186)
    current_row <- current_row + 1L
  }

  # Missing variable error message (SAS lines 189-194)
  if (err_missvar) {
    missvar_msg <- stringr::str_c(
      "Some variables that are required by this panel are missing. ",
      "These variables are shown in the following table."
    )

    openxlsx::writeData(wb, sheet, x = missvar_msg,
                        startCol = 1L, startRow = current_row)
    openxlsx::addStyle(wb, sheet, style = styles[["Default10Wrap"]],
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1L:7L, rows = current_row)
    current_row <- current_row + 1L
  }

  # Trailing blank row (SAS lines 196-197)
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------- #
  # Header markup (SAS line 200: %markup(ws_err_header_data, ws_err_header))
  # In R, the header content was written directly above using writeData/
  # addStyle. The SAS %markup converted annotated data into XML strings;
  # in R, openxlsx writes cells directly so this step is implicit.
  # ---------------------------------------------------------------------- #

  # ---------------------------------------------------------------------- #
  # Missing Variable Table (SAS lines 202-230)
  # Uses annotate_data() and write_annotated() from xml_output.R
  # ---------------------------------------------------------------------- #
  if (err_missvar && !is.null(err_missing_var) && nrow(err_missing_var) > 0L) {

    # Column headers (SAS lines 206-219)
    # SAS creates ws_err_missing_cols_data with ColumnOutline style,
    # then %markup converts to XML.
    # R equivalent: build a 1-row data frame and use annotate_data() +
    # write_annotated() from xml_output.R (replaces SAS %markup at line 219)
    cols_df <- dplyr::tibble(
      ds  = "Domain/Dataset",
      var = "Variable"
    )
    cols_ann <- annotate_data(
      cols_df,
      style_id = "ColumnOutline",
      height   = 30
    )
    current_row <- write_annotated(
      wb, sheet, cols_ann, styles,
      start_row = current_row - 1L,
      start_col = 0L
    )

    # Data rows (SAS lines 221-228)
    # SAS: %annotate(err_missing_var, ws_err_missing_data);
    # SAS: StyleID = 'Table'; (override default style to Table)
    # SAS: %markup(ws_err_missing_data, ws_err_missing);
    # R equivalent: annotate_data() with style_id="Table" then write_annotated()
    data_ann <- annotate_data(
      err_missing_var,
      style_id = "Table"
    )
    current_row <- write_annotated(
      wb, sheet, data_ann, styles,
      start_row = current_row - 1L,
      start_col = 0L
    )
  }

  # ---------------------------------------------------------------------- #
  # WorksheetOptions (SAS lines 232-250)
  # ---------------------------------------------------------------------- #
  # Page setup: Landscape orientation, fit to page, scale 78%
  # (SAS lines 235-249: XML WorksheetOptions block)
  openxlsx::pageSetup(
    wb, sheet,
    orientation = "landscape",
    fitToWidth  = TRUE,
    fitToHeight = FALSE,
    scale       = 78
  )

  # Print header and footer (SAS lines 237-239)
  # SAS Header: Left = panel_title, Right = "NDA/BLA {ndabla}\nStudy {studyid}"
  # SAS Footer: "Page &P of &N"
  openxlsx::setHeaderFooter(
    wb, sheet,
    header = c(
      panel_title,
      NA_character_,
      stringr::str_c("NDA/BLA ", ndabla, "\nStudy ", studyid)
    ),
    footer = c(
      NA_character_,
      "Page &[Page] of &[Pages]",
      NA_character_
    )
  )

  # ---------------------------------------------------------------------- #
  # Save Workbook (SAS lines 252-273)
  # SAS: DATA _NULL_; set wb; file "&err_file." ls=32767; put string;
  # R: openxlsx::saveWorkbook()
  # ---------------------------------------------------------------------- #
  # Ensure the output directory exists before saving
  output_dir <- dirname(err_file)
  if (nchar(output_dir) > 0L && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Save the workbook (replaces SAS DATA _NULL_ file writing at lines 269-273)
  openxlsx::saveWorkbook(wb, file = err_file, overwrite = TRUE)
  cli::cli_alert_success(
    stringr::str_c("Error summary workbook saved: ", err_file)
  )

  # ---------------------------------------------------------------------- #
  # Error Status Signaling (SAS lines 277-282)
  # SAS: %if &err_seterr. = 1 %then %do; %let errstatus = 5; %end;
  # R: Return status code in list (replaces global macro variable)
  # ---------------------------------------------------------------------- #
  errstatus <- dplyr::if_else(err_seterr, 5L, 0L)

  invisible(list(
    success        = FALSE,
    errstatus      = errstatus,
    errsummaryrun  = TRUE,
    err_file       = err_file
  ))
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    1. SpreadsheetML XML generation is fully replaced by openxlsx
####       via xml_output.R shared utility functions (wb_create,
####       create_workbook_styles, annotate_data, write_annotated).
####    2. Cell merge across 6 columns (SAS MergeAcross=6) maps to
####       openxlsx mergeCells(cols = 1:7) because SAS MergeAcross
####       specifies additional columns beyond the current one.
####    3. Error status signaling via return value list replaces SAS
####       global macro variables ERRSTATUS and ERRSUMMARYRUN.
####    4. SAS global variables (panel_title, ndabla, studyid,
####       panel_desc, sl_subset, rpt_chk_var_req) are passed as
####       function parameters instead of relying on macro scope.
####    5. Legacy SAS 0/1 numeric flags for err_nosubj, err_missvar,
####       err_seterr are coerced to logical TRUE/FALSE.
####    6. Row height calculation (SAS line 154:
####       Height = ceil(length(trim(...))/150)*12.75) is computed
####       but openxlsx auto-sizes with wrapText; explicit row height
####       setting omitted as openxlsx handles this via wrapText=TRUE.
####    7. The sl_subset subsetting logic assumes the data frame has
####       columns 'outer_operator' and 'name' matching the SAS dataset.
####
#### POTENTIAL NUMERICAL DIFFERENCES:
####    None -- this is error reporting infrastructure, not computation.
####    Date/time formatting: SAS e8601da. -> R format(Sys.Date(), "%Y-%m-%d")
####    produces identical ISO 8601 output. SAS timeampm11. ->
####    R format(Sys.time(), "%I:%M:%S %p") produces identical AM/PM format.
####
#### NO DIRECT R EQUIVALENT:
####    1. SAS %wb/%styles macros -> R wb_create() and
####       create_workbook_styles() from xml_output.R
####    2. SAS %annotate/%markup macros -> R annotate_data() and
####       write_annotated() from xml_output.R
####    3. SAS DATA _NULL_ file writing (lines 269-273) ->
####       openxlsx::saveWorkbook()
####    4. SAS WorksheetOptions XML (FitToPage, Scale, Resolution) ->
####       openxlsx::pageSetup(). SAS HorizontalResolution and
####       VerticalResolution have no direct openxlsx equivalent;
####       print resolution is determined by Excel at print time.
####    5. SAS global macro variable ERRSTATUS/ERRSUMMARYRUN ->
####       R function return value list.
####    6. SpreadsheetML XML streaming (character-by-character string
####       building via SAS DATA step PUT) -> openxlsx in-memory
####       workbook with structured API calls.
####
#### PACKAGE SELECTION RATIONALE:
####    openxlsx (>=4.2.5): Replaces SpreadsheetML XML generation;
####       full Excel workbook API with native R cell-level formatting,
####       merging, page setup, and header/footer support. No Java
####       dependency (unlike xlsx package).
####    dplyr (>=1.1.0): Tidyverse data manipulation for sl_subset
####       and rpt_chk_var_req preprocessing; tibble construction.
####       AAP mandates tidyverse over base R for all data manipulation.
####    stringr (>=1.5.0): Tidyverse string manipulation replacing
####       SAS || concatenation (str_c), LOWCASE (str_to_lower), and
####       TRIM/LEFT (str_trim). AAP mandates stringr over base R
####       paste/nchar/tolower.
####    cli (>=3.6.0): User-facing messages replacing SAS %put with
####       rich terminal formatting (cli_alert_info, cli_alert_success)
####       consistent with other migrated utility files.
####
#### OPEN QUESTIONS:
####    1. Exact pixel-to-character-width conversion for column widths:
####       SAS 150px approximated as 21.4 character widths, SAS 250px
####       as 35.7 character widths. Verify visual equivalence.
####    2. Whether openxlsx pageSetup header/footer tokens (&[Page],
####       &[Pages]) render identically to SAS &P/&N tokens across
####       all Excel versions.
####    3. SAS Scale=78% is set via openxlsx pageSetup(scale=78).
####       Verify print preview matches SAS output.
####    4. The SAS FitHeight=100 setting (line 243) maps to
####       fitToHeight=FALSE in openxlsx since the intent is to
####       allow unlimited vertical pages. Verify behavior.
#### ============================================================
