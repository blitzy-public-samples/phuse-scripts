# =============================================================================
# PROGRAM NAME: err_output.R
#
# DESCRIPTION:  Error Summary Workbook Generation — creates an Excel (.xlsx)
#               error summary workbook when panel processing encounters errors
#               (no subjects in DM, missing required variables, custom error
#               messages). Replaces the SAS macro %error_summary from
#               err_output.sas which built SpreadsheetML XML via xml_output.sas.
#
# ORIGINAL SAS: tested/SAS/ZZ_Utilities/err_output.sas (283 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
# SAS DATE:     March 28, 2011
#
# R MIGRATION:  Migrated to R using openxlsx package
# R REQUIRES:   openxlsx (>= 4.2.5), dplyr (>= 1.1.0), cli (>= 3.6.0)
# R DEPENDS:    tested/R/utilities/xml_output.R (create_workbook_styles,
#               apply_page_setup)
#
# NOTES:        This utility is called by domain panel drivers when processing
#               encounters an unrecoverable error. The generated workbook serves
#               as a user-facing diagnostic document for Script Launcher.
# =============================================================================

# Load required packages
library(openxlsx)
library(dplyr)
library(cli)

# Source the foundational workbook output engine for shared style gallery
# and page setup utilities. Replaces SAS: %include "&utilpath.\xml_output.sas";
# Uses robust self-location to find xml_output.R relative to this script.
local({
  # Attempt 1: locate via the calling script's file path (source() sets ofile)
  this_file <- tryCatch(
    normalizePath(sys.frame(1L)$ofile, mustWork = TRUE),
    error = function(e) NULL
  )

  # Attempt 2: fall back to the 'source' attribute attached by source()
  if (is.null(this_file)) {
    for (i in rev(seq_len(sys.nframe()))) {
      env <- sys.frame(i)
      if (exists("ofile", envir = env, inherits = FALSE)) {
        this_file <- tryCatch(
          normalizePath(get("ofile", envir = env), mustWork = TRUE),
          error = function(e) NULL
        )
        if (!is.null(this_file)) break
      }
    }
  }

  # Build the path to xml_output.R relative to this file
  if (!is.null(this_file)) {
    xml_output_path <- file.path(dirname(this_file), "xml_output.R")
  } else {
    # Attempt 3: fall back to well-known relative path from project root
    xml_output_path <- "tested/R/utilities/xml_output.R"
  }

  # Only source if the required functions are not yet available
  if (!exists("create_workbook_styles", mode = "function", inherits = TRUE) ||
      !exists("apply_page_setup", mode = "function", inherits = TRUE)) {
    if (file.exists(xml_output_path)) {
      source(xml_output_path, local = FALSE)
    }
  }
})

# =============================================================================
# error_summary
# =============================================================================
#' Create an Excel error summary workbook for panel processing failures.
#'
#' Migrated from SAS \code{%error_summary(err_file=, err_nosubj=0,
#' err_missvar=0, err_seterr=1, err_desc=)} macro (lines 44-282 of
#' err_output.sas). Generates a formatted Excel workbook containing:
#' \itemize{
#'   \item Panel title and study identification header
#'   \item NDA/BLA and study identifiers
#'   \item Analysis run date/time stamp
#'   \item Optional panel description text
#'   \item Optional custom error description
#'   \item Optional "no subjects in DM" error message (with subsetting detail)
#'   \item Optional missing required variables table
#' }
#'
#' In SAS, the macro built SpreadsheetML XML by invoking \code{%wb}, \code{%styles},
#' \code{%markup}, and \code{%annotate} macros from xml_output.sas. In R, all
#' workbook construction uses the \code{openxlsx} package API, with shared styles
#' from \code{create_workbook_styles()} in xml_output.R.
#'
#' @param err_file         Character. Full file path for the output Excel
#'                         workbook. No hardcoded paths — must be provided by
#'                         the caller. Replaces SAS \code{err_file=} parameter.
#' @param panel_title      Character. The panel title used in the workbook title,
#'                         header section, and page header. Replaces SAS global
#'                         macro variable \code{&panel_title.}.
#' @param ndabla           Character. NDA/BLA identifier displayed in the header
#'                         and page header. Replaces SAS \code{&ndabla.}.
#' @param studyid          Character. Study identifier displayed in the header
#'                         and page header. Replaces SAS \code{&studyid.}.
#' @param err_nosubj       Logical. If TRUE, include the "no subjects in DM"
#'                         error message. SAS integer flag (0/1) migrated to R
#'                         logical. Defaults to FALSE.
#' @param err_missvar      Logical. If TRUE, include the missing required
#'                         variables table. SAS integer flag (0/1) migrated to
#'                         R logical. Defaults to FALSE.
#' @param err_seterr       Logical. If TRUE, set the error status to 5 in the
#'                         return value so Script Launcher can detect the error.
#'                         Defaults to TRUE. Replaces SAS \code{err_seterr=1}.
#' @param err_desc         Character. Optional custom error description text.
#'                         If non-empty, written as a merged-cell row in the
#'                         workbook. Defaults to empty string.
#' @param rpt_chk_var_req  Data frame or NULL. The required variable check
#'                         dataset with columns \code{ind}, \code{ds}, and
#'                         \code{var}. Variables where \code{ind != 1} are
#'                         flagged as missing. Required when \code{err_missvar}
#'                         is TRUE. Replaces SAS dataset \code{rpt_chk_var_req}.
#' @param sl_subset        Data frame or NULL. The Script Launcher subsetting
#'                         dataset with columns \code{outer_operator} and
#'                         \code{name}. Used to build the subset description
#'                         when \code{err_nosubj} is TRUE. Replaces SAS dataset
#'                         \code{sl_subset}.
#' @param sl_subset_desc   Character. Pre-built subset description string. If
#'                         non-empty and \code{err_nosubj} is TRUE, used directly
#'                         instead of building from \code{sl_subset}. Defaults to
#'                         empty string.
#' @param panel_desc       Character. Optional panel description text displayed
#'                         below the study identifiers. If non-empty, written as
#'                         a merged, wrapped row. Replaces SAS \code{&panel_desc.}.
#'                         Defaults to empty string.
#' @param styles           Named list of openxlsx style objects or NULL. If NULL,
#'                         calls \code{create_workbook_styles()} from xml_output.R
#'                         to generate the standard style gallery. Allows callers
#'                         to inject pre-built styles for consistency.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{errstatus}{Integer. 5L if \code{err_seterr} is TRUE, 0L otherwise.
#'       Replaces SAS global \code{&errstatus.} variable.}
#'     \item{workbook}{The openxlsx workbook object for further manipulation
#'       if needed.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic error: no subjects in DM
#' result <- error_summary(
#'   err_file    = file.path(tempdir(), "ae_error.xlsx"),
#'   panel_title = "Adverse Events",
#'   ndabla      = "12345",
#'   studyid     = "STUDY-001",
#'   err_nosubj  = TRUE
#' )
#' cat("Error status:", result$errstatus, "\n")
#'
#' # Error: missing required variables
#' chk <- data.frame(
#'   ind = c(1, 0, 0),
#'   ds  = c("ADSL", "ADAE", "ADAE"),
#'   var = c("USUBJID", "AESEV", "AESER"),
#'   stringsAsFactors = FALSE
#' )
#' result <- error_summary(
#'   err_file        = file.path(tempdir(), "ae_error.xlsx"),
#'   panel_title     = "Adverse Events",
#'   ndabla          = "12345",
#'   studyid         = "STUDY-001",
#'   err_missvar     = TRUE,
#'   rpt_chk_var_req = chk
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
                          rpt_chk_var_req = NULL,
                          sl_subset = NULL,
                          sl_subset_desc = "",
                          panel_desc = "",
                          styles = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation — comprehensive checks for all arguments

  # ---------------------------------------------------------------------------

  if (missing(err_file) || !is.character(err_file) || length(err_file) != 1L ||
      nchar(trimws(err_file)) == 0L) {
    cli::cli_abort(
      "{.arg err_file} must be a non-empty character string specifying the output file path."
    )
  }
  if (missing(panel_title) || !is.character(panel_title) || length(panel_title) != 1L) {
    cli::cli_abort("{.arg panel_title} must be a single character string.")
  }
  if (missing(ndabla) || !is.character(ndabla) || length(ndabla) != 1L) {
    cli::cli_abort("{.arg ndabla} must be a single character string.")
  }
  if (missing(studyid) || !is.character(studyid) || length(studyid) != 1L) {
    cli::cli_abort("{.arg studyid} must be a single character string.")
  }
  if (!is.logical(err_nosubj) || length(err_nosubj) != 1L) {
    cli::cli_abort("{.arg err_nosubj} must be a single logical value (TRUE/FALSE).")
  }
  if (!is.logical(err_missvar) || length(err_missvar) != 1L) {
    cli::cli_abort("{.arg err_missvar} must be a single logical value (TRUE/FALSE).")
  }
  if (!is.logical(err_seterr) || length(err_seterr) != 1L) {
    cli::cli_abort("{.arg err_seterr} must be a single logical value (TRUE/FALSE).")
  }
  if (!is.character(err_desc) || length(err_desc) != 1L) {
    cli::cli_abort("{.arg err_desc} must be a single character string.")
  }
  if (!is.character(sl_subset_desc) || length(sl_subset_desc) != 1L) {
    cli::cli_abort("{.arg sl_subset_desc} must be a single character string.")
  }
  if (!is.character(panel_desc) || length(panel_desc) != 1L) {
    cli::cli_abort("{.arg panel_desc} must be a single character string.")
  }

  # Validate rpt_chk_var_req when err_missvar is requested
  if (err_missvar && !is.null(rpt_chk_var_req)) {
    if (!is.data.frame(rpt_chk_var_req)) {
      cli::cli_abort(
        "{.arg rpt_chk_var_req} must be a data.frame with columns {.val ind}, {.val ds}, {.val var}."
      )
    }
    required_cols <- c("ind", "ds", "var")
    missing_cols <- setdiff(required_cols, colnames(rpt_chk_var_req))
    if (length(missing_cols) > 0L) {
      cli::cli_abort(
        "{.arg rpt_chk_var_req} is missing required column(s): {.val {missing_cols}}."
      )
    }
  }

  # Validate sl_subset when err_nosubj is requested
  if (err_nosubj && !is.null(sl_subset) && !is.data.frame(sl_subset)) {
    cli::cli_abort("{.arg sl_subset} must be a data.frame or NULL.")
  }

  # ---------------------------------------------------------------------------
  # Phase 1: No-subjects error preprocessing (SAS lines 48-69)
  # SAS: queries sl_subset count, builds subset description from
  #      outer_operator and name columns
  # ---------------------------------------------------------------------------
  if (err_nosubj) {
    cli::cli_inform("PANEL NO SUBJECTS ERROR PREPROCESSING")

    if (nchar(trimws(sl_subset_desc)) == 0L &&
        !is.null(sl_subset) && is.data.frame(sl_subset) && nrow(sl_subset) > 0L) {
      # Build subset description from the sl_subset data frame
      # SAS: select lowcase(outer_operator) into: operator from sl_subset;
      #      select distinct name into: sl_subset_desc separated by " &operator. "
      operator <- unique(tolower(as.character(sl_subset$outer_operator)))
      if (length(operator) == 0L || all(is.na(operator))) {
        operator <- "and"
      } else {
        operator <- operator[1L]
      }
      subset_names <- unique(as.character(sl_subset$name))
      subset_names <- subset_names[!is.na(subset_names)]
      sl_subset_desc <- paste(subset_names, collapse = paste0(" ", operator, " "))
    }
  }

  # ---------------------------------------------------------------------------
  # Phase 2: Missing variable preprocessing (SAS lines 71-80)
  # SAS: data err_missing_var; set rpt_chk_var_req; where ind ne 1; keep ds var;
  # R: dplyr::filter(ind != 1) %>% dplyr::select(ds, var)
  # Note: dplyr::filter() handles NA safely — rows where ind is NA are excluded
  # ---------------------------------------------------------------------------
  err_missing_var <- NULL

  if (err_missvar) {
    cli::cli_inform("PANEL MISSING VARIABLE ERROR PREPROCESSING")

    if (!is.null(rpt_chk_var_req) && is.data.frame(rpt_chk_var_req) &&
        nrow(rpt_chk_var_req) > 0L) {
      err_missing_var <- rpt_chk_var_req %>%
        dplyr::filter(!is.na(.data$ind) & .data$ind != 1) %>%
        dplyr::select("ds", "var")
    } else {
      cli::cli_warn(
        "Missing variable error requested but {.arg rpt_chk_var_req} is NULL or empty."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Phase 3: Create workbook and styles (SAS lines 85-88)
  # SAS: %wb; %styles;
  # R: openxlsx::createWorkbook() + create_workbook_styles() from xml_output.R
  # ---------------------------------------------------------------------------
  cli::cli_inform("SCRIPT LAUNCHER ERROR SUMMARY OUTPUT")

  wb_title <- paste0(panel_title, " Error Summary")
  wb <- openxlsx::createWorkbook(creator = "US Food & Drug Administration",
                                 title   = wb_title)

  # Obtain the shared style gallery from xml_output.R
  if (is.null(styles) || !is.list(styles) || length(styles) == 0L) {
    styles <- create_workbook_styles()
  }

  # ---------------------------------------------------------------------------
  # Phase 4: Add worksheet and set column widths (SAS lines 91-113)
  # SAS: <Column ss:Width="150"/>, <Column ss:Width="250"/>, <Column/>
  # openxlsx widths are in character units (approx. pixels / 7)
  # ---------------------------------------------------------------------------
  sheet_name <- "Error Summary"
  openxlsx::addWorksheet(wb, sheetName = sheet_name, gridLines = FALSE)

  # Column widths: SAS uses pixel-based widths 150, 250, auto
  # Conversion: approximately pixels / 7 for openxlsx character units
  openxlsx::setColWidths(wb, sheet = sheet_name,
                         cols   = 1:3,
                         widths = c(150 / 7, 250 / 7, "auto"))

  # ---------------------------------------------------------------------------
  # Phase 5: Header section (SAS lines 116-198)
  # Build header rows using openxlsx::writeData() + addStyle()
  # Track current row with a counter variable, incrementing per row written
  # ---------------------------------------------------------------------------
  current_row <- 1L

  # Row 1: blank (SAS Row 1: Data = '')
  current_row <- current_row + 1L

  # Row 2: "{panel_title} Error Summary" — Header style (12pt, bold, italic)
  openxlsx::writeData(wb, sheet_name,
                      x        = wb_title,
                      startRow = current_row,
                      startCol = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = styles[["Header"]],
                     rows  = current_row,
                     cols  = 1)
  current_row <- current_row + 1L

  # Row 3: blank
  current_row <- current_row + 1L

  # Row 4: "NDA/BLA: {ndabla}" — Default10 style
  openxlsx::writeData(wb, sheet_name,
                      x        = paste0("NDA/BLA: ", ndabla),
                      startRow = current_row,
                      startCol = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = styles[["Default10"]],
                     rows  = current_row,
                     cols  = 1)
  current_row <- current_row + 1L

  # Row 5: "Study: {studyid}" — Default10 style
  openxlsx::writeData(wb, sheet_name,
                      x        = paste0("Study: ", studyid),
                      startRow = current_row,
                      startCol = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = styles[["Default10"]],
                     rows  = current_row,
                     cols  = 1)
  current_row <- current_row + 1L

  # Row 6: "Analysis run date: {date} {time}" — Default10 style
  # SAS: put(date(),e8601da.) -> format(Sys.Date(), "%Y-%m-%d")
  # SAS: put(time(),timeampm11.) -> format(Sys.time(), "%I:%M %p")
  run_date_str <- paste0(
    "Analysis run date: ",
    format(Sys.Date(), "%Y-%m-%d"),
    " ",
    format(Sys.time(), "%I:%M %p")
  )
  openxlsx::writeData(wb, sheet_name,
                      x        = run_date_str,
                      startRow = current_row,
                      startCol = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name,
                     style = styles[["Default10"]],
                     rows  = current_row,
                     cols  = 1)
  current_row <- current_row + 1L

  # Row 7: blank (SAS: Row = row+1 with no output but row counter advances)
  current_row <- current_row + 1L

  # Row 8: blank
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Conditional: Panel description (SAS lines 147-160)
  # SAS: %if %length(&panel_desc.) > 0
  # Written as a merged, wrapped row with auto-calculated height
  # ---------------------------------------------------------------------------
  if (nchar(trimws(panel_desc)) > 0L) {
    openxlsx::writeData(wb, sheet_name,
                        x        = panel_desc,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = styles[["Default10Wrap"]],
                       rows  = current_row,
                       cols  = 1)
    # MergeAcross = 6 means merge column 1 through column 7
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:7,
                         rows = current_row)
    # Auto-calculate row height based on text length
    # SAS: ceil(length(trim("&panel_desc."))/150)*12.75
    calc_height <- ceiling(nchar(trimws(panel_desc)) / 150) * 12.75
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = max(calc_height, 12.75))
    current_row <- current_row + 1L

    # Blank row after panel description
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # Conditional: Custom error description (SAS lines 170-176)
  # SAS: %if %length(&err_desc.) > 0
  # ---------------------------------------------------------------------------
  if (nchar(trimws(err_desc)) > 0L) {
    openxlsx::writeData(wb, sheet_name,
                        x        = err_desc,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = styles[["Default10Wrap"]],
                       rows  = current_row,
                       cols  = 1)
    # MergeAcross = 6 → merge columns 1 through 7
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:7,
                         rows = current_row)
    # SAS height formula: ceil(length(trim(Data))/130)*12.75
    calc_height <- ceiling(nchar(trimws(err_desc)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = max(calc_height, 12.75))
    current_row <- current_row + 1L

    # Blank row after custom error description (SAS: Row = row+1; Data = ''; Height = 12.75)
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = 12.75)
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # Conditional: No subjects in DM error (SAS lines 179-187)
  # SAS: %if &err_nosubj. %then %do;
  # ---------------------------------------------------------------------------
  if (err_nosubj) {
    nosubj_msg <- "There are no subjects in the demographics domain (DM) dataset"
    if (nchar(trimws(sl_subset_desc)) > 0L) {
      nosubj_msg <- paste0(nosubj_msg, " after subsetting by ", sl_subset_desc)
    }
    nosubj_msg <- paste0(nosubj_msg, ".")

    openxlsx::writeData(wb, sheet_name,
                        x        = nosubj_msg,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = styles[["Default10Wrap"]],
                       rows  = current_row,
                       cols  = 1)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:7,
                         rows = current_row)
    calc_height <- ceiling(nchar(trimws(nosubj_msg)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = max(calc_height, 12.75))
    current_row <- current_row + 1L

    # Blank row after no-subjects message (SAS: Height = 12.75)
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = 12.75)
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # Conditional: Missing variable error message (SAS lines 190-194)
  # SAS: %if &err_missvar. %then %do;
  # ---------------------------------------------------------------------------
  if (err_missvar) {
    missvar_msg <- paste0(
      "Some variables that are required by this panel are missing. ",
      "These variables are shown in the following table."
    )

    openxlsx::writeData(wb, sheet_name,
                        x        = missvar_msg,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = styles[["Default10Wrap"]],
                       rows  = current_row,
                       cols  = 1)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:7,
                         rows = current_row)
    calc_height <- ceiling(nchar(trimws(missvar_msg)) / 130) * 12.75
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = max(calc_height, 12.75))
    current_row <- current_row + 1L
  }

  # Blank row before the table or end of content (SAS: Row = row+1; Data = ''; Height = 12.75)
  openxlsx::setRowHeights(wb, sheet_name,
                          rows    = current_row,
                          heights = 12.75)
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Phase 6: Missing variable table (SAS lines 202-230)
  # SAS: Column headers with ColumnOutline style; data rows with Table style
  # ---------------------------------------------------------------------------
  if (err_missvar && !is.null(err_missing_var) && nrow(err_missing_var) > 0L) {

    # Column headers: "Domain/Dataset", "Variable" — ColumnOutline style
    # SAS: StyleID = 'ColumnOutline'; Height = 30;
    col_headers <- data.frame(
      `Domain/Dataset` = "Domain/Dataset",
      Variable         = "Variable",
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    openxlsx::writeData(wb, sheet_name,
                        x        = col_headers,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = styles[["ColumnOutline"]],
                       rows  = current_row,
                       cols  = 1:2,
                       gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet_name,
                            rows    = current_row,
                            heights = 30)
    current_row <- current_row + 1L

    # Data rows from err_missing_var tibble — Table style (centered, bordered)
    # SAS: %annotate(err_missing_var, ws_err_missing_data); StyleID = 'Table';
    n_data_rows <- nrow(err_missing_var)

    # Rename columns to match the display labels
    display_data <- err_missing_var
    colnames(display_data) <- c("Domain/Dataset", "Variable")

    openxlsx::writeData(wb, sheet_name,
                        x        = display_data,
                        startRow = current_row,
                        startCol = 1,
                        colNames = FALSE)

    # Apply Table style to all data cells in the missing variable table
    if (n_data_rows > 0L) {
      data_rows <- seq(current_row, current_row + n_data_rows - 1L)
      openxlsx::addStyle(wb, sheet_name,
                         style      = styles[["Table"]],
                         rows       = data_rows,
                         cols       = 1:2,
                         gridExpand = TRUE)
    }

    current_row <- current_row + n_data_rows
  }

  # ---------------------------------------------------------------------------
  # Phase 7: Worksheet page setup and headers/footers (SAS lines 232-250)
  # Use apply_page_setup() from xml_output.R which wraps openxlsx::pageSetup()
  # and openxlsx::setHeaderFooter()
  # SAS: <Layout x:Orientation="Landscape"/>
  #      <Header x:Data="&L panel_title. &R NDA/BLA ndabla &#10; Study studyid."/>
  #      <Footer x:Data="Page &P of &N"/>
  #      <FitToPage/>, <FitHeight>100</FitHeight>, <Scale>78</Scale>
  # ---------------------------------------------------------------------------
  apply_page_setup(
    wb,
    sheet          = sheet_name,
    orientation    = "landscape",
    header_left    = panel_title,
    header_right   = paste0("NDA/BLA ", ndabla, "\nStudy ", studyid),
    footer_center  = "Page &P of &N",
    fit_to_width   = TRUE,
    fit_to_height  = 100,
    scale          = 78
  )

  # ---------------------------------------------------------------------------
  # Phase 8: Save workbook (SAS lines 252-273)
  # SAS: data _null_; set wb; file "&err_file." ls=32767; put string; run;
  # R: openxlsx::saveWorkbook(wb, err_file, overwrite = TRUE)
  # ---------------------------------------------------------------------------

  # Ensure the output directory exists before saving
  output_dir <- dirname(err_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, file = err_file, overwrite = TRUE)
  cli::cli_inform("Error summary workbook saved to: {.file {err_file}}")

  # ---------------------------------------------------------------------------
  # Phase 9: Error status (SAS lines 278-280)
  # SAS: %if &err_seterr. = 1 %then %do; %let errstatus = 5; %end;
  # R: Return list with errstatus and workbook object
  # ---------------------------------------------------------------------------
  errstatus <- if (err_seterr) 5L else 0L

  return(list(
    errstatus = errstatus,
    workbook  = wb
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS SpreadsheetML XML output replaced by openxlsx workbook
#    - SAS macro variables (panel_title, ndabla, studyid) -> function arguments
#    - SAS integer flag arguments (0/1) -> R logical (TRUE/FALSE)
#    - SAS errstatus global variable -> return value in list
#    - SAS panel_desc global macro variable -> explicit function argument
#    - SAS sl_subset dataset -> data.frame argument with same column names
#    - SAS rpt_chk_var_req dataset -> data.frame argument with ind/ds/var columns
#    - dplyr::filter() handles NA safely for ind != 1 comparison
#    - Column width conversion uses pixels / 7 approximation for openxlsx
#    - MergeAcross = 6 in SAS maps to mergeCells(cols = 1:7) in openxlsx
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Row height auto-calculation may differ slightly from SAS formula
#      SAS: ceil(length(trim(Data))/150)*12.75 or /130)*12.75
#      R: ceiling(nchar(trimws(text)) / 150) * 12.75 or /130
#    - Excel formatting may vary between SpreadsheetML (.xml) and OOXML (.xlsx)
#    - Column width rendering depends on default font and display settings
# NO DIRECT R EQUIVALENT:
#    - SAS FILE/PUT streaming -> openxlsx::saveWorkbook()
#    - SAS %wb/%styles/%markup XML pipeline -> openxlsx API
#    - SAS %xml_tag_def/%xml_init variable declarations -> not needed in R
#    - SAS %annotate (dataset decomposition) -> openxlsx::writeData()
#    - SAS dataset concatenation (data wb; set wb_start ...) -> single workbook
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: Excel output (AAP mandated replacement for SpreadsheetML)
#    - dplyr: data manipulation per AAP tidyverse-over-base-R mandate
#    - cli: user-facing informative messages replacing SAS %PUT
#    - xml_output.R: shared style gallery and page setup (internal dependency)
# OPEN QUESTIONS:
#    - Exact pixel-to-Excel-width conversion factors may need calibration
#    - Row height calculation precision alignment with SAS rendering
#    - Scale=78 print setting: verify rendering parity between formats
#    - SAS %errsummaryrun macro flag: not migrated as it was SAS-specific
#      state management; R callers should use the return value instead
# ============================================================
