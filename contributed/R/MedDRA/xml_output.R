# ============================================================================
# PROGRAM: xml_output.R — Excel Workbook Creation Utilities
# DESCRIPTION: Core Excel output engine replacing SAS SpreadsheetML XML
#   generation with openxlsx in-memory workbook API. Provides workbook
#   scaffolding, a comprehensive style gallery (66 named styles), and
#   data-writing helpers for clinical report Excel output.
#
# ORIGINAL: contributed/MedDRA/ZZ_Utilities/xml_output.sas (896 lines)
# AUTHOR: David Kretch (original SAS), migrated to R
# DATE: February 15, 2011 (original SAS), migrated 2024
#
# MIGRATION: SAS SpreadsheetML XML streaming (DATA _NULL_ + PUT) is
#   replaced entirely by openxlsx in-memory workbook operations. Style
#   inheritance (ss:Parent) is flattened into complete style definitions
#   because openxlsx does not support style inheritance. The global
#   strlen buffer-sizing concept is removed — openxlsx handles string
#   lengths internally.
#
# EXPORTS: wb_create, create_workbook_styles, ws_header, ws_data, ws_rowcount,
#          annotate_data, write_annotated, style_from_spec
# ============================================================================

library(openxlsx)
library(dplyr)

# ============================================================================
# wb_create — Create a new Excel workbook
# Replaces SAS %wb macro (lines 41-66 of xml_output.sas)
#
# The SAS macro wrote XML preamble, document properties (Title, Author,
# Created), and workbook open/close tags. openxlsx handles document
# properties directly via createWorkbook().
#
# @param title       Character. Workbook title (stored in document properties).
# @param author      Character. Document author. Default matches SAS source.
# @param sheet       Character or NULL. If provided, adds an initial worksheet
#                    and configures page setup and optional header/footer.
# @param orientation Character. Page orientation for the initial worksheet.
#                    One of "portrait" or "landscape". Default "portrait".
# @param header      Character vector of length 3 (left, center, right) or
#                    NULL. Page header strings for the initial worksheet.
# @param footer      Character vector of length 3 (left, center, right) or
#                    NULL. Page footer strings for the initial worksheet.
# @param save_path   Character or NULL. If provided, saves workbook to this
#                    file path immediately after creation.
# @return An openxlsx workbook object.
# ============================================================================
wb_create <- function(title = "",
                      author = "US Food & Drug Administration",
                      sheet = NULL,
                      orientation = "portrait",
                      header = NULL,
                      footer = NULL,
                      save_path = NULL) {

  # Create workbook with document properties
  wb <- createWorkbook(creator = author, title = title)

  # Optionally add an initial worksheet with page configuration
  if (!is.null(sheet)) {
    addWorksheet(wb, sheetName = sheet)
    pageSetup(wb, sheet = sheet, orientation = orientation, fitToWidth = TRUE)

    # Set header/footer if provided
    if (!is.null(header) || !is.null(footer)) {
      hdr <- if (!is.null(header)) header else c(NA, NA, NA)
      ftr <- if (!is.null(footer)) footer else c(NA, NA, NA)
      setHeaderFooter(wb, sheet = sheet, header = hdr, footer = ftr)
    }
  }

  # Optionally save the workbook
  if (!is.null(save_path)) {
    saveWorkbook(wb, file = save_path, overwrite = TRUE)
  }

  return(wb)
}


# ============================================================================
# create_workbook_styles — Comprehensive style gallery for Excel output
# Replaces SAS %styles macro (lines 72-558 of xml_output.sas)
#
# Returns a named list of 66 openxlsx Style objects corresponding to every
# SpreadsheetML style ID in the SAS output engine. Style inheritance
# (ss:Parent) has been flattened — each style includes all properties from
# its full parent chain so openxlsx can apply them independently.
#
# Color codes, font sizes, number formats, and border specifications are
# preserved exactly from the SAS source.
#
# @param size Numeric. Base font size in points (default 9).
# @return Named list of openxlsx Style objects.
# ============================================================================
create_workbook_styles <- function(size = 9) {

  # Common border definitions for reuse
  border_lr   <- c("left", "right")
  border_lrb  <- c("left", "right", "bottom")
  border_all  <- "TopBottomLeftRight"
  border_tlr  <- c("top", "left", "right")

  styles <- list(

    # ================================================================
    # Default styles (SAS lines 78-125)
    # ================================================================

    Default = createStyle(fontSize = size),

    DefaultLeft = createStyle(
      fontSize = size, halign = "left", valign = "top"
    ),

    DefaultRight = createStyle(
      fontSize = size, halign = "right", valign = "top"
    ),

    DefaultWhite = createStyle(
      fontSize = size, fontColour = "#FFFFFF"
    ),

    Default10 = createStyle(fontSize = 10, valign = "top"),

    Default10Wrap = createStyle(
      fontSize = 10, valign = "top", wrapText = TRUE
    ),

    Default10RedWrap = createStyle(
      fontSize = 10, fontColour = "#FF0000",
      textDecoration = "italic", valign = "top", wrapText = TRUE
    ),

    Default10Right = createStyle(
      fontSize = 10, halign = "right", valign = "top"
    ),

    Default8 = createStyle(fontSize = 8, valign = "top"),

    # ================================================================
    # Header styles (SAS lines 127-137)
    # ================================================================

    Header = createStyle(
      fontSize = 12, textDecoration = c("bold", "italic")
    ),

    SubHeader = createStyle(
      fontSize = 10, textDecoration = "bold", valign = "top"
    ),

    # ================================================================
    # Column header styles (SAS lines 139-174)
    # White bold text on #333399 (dark blue) background
    # ================================================================

    Column = createStyle(
      fontSize = 10, fontColour = "#FFFFFF", textDecoration = "bold",
      fgFill = "#333399", halign = "center", valign = "center",
      wrapText = TRUE
    ),

    # ColumnOutline: Column + all four thin borders
    ColumnOutline = createStyle(
      fontSize = 10, fontColour = "#FFFFFF", textDecoration = "bold",
      fgFill = "#333399", halign = "center", valign = "center",
      wrapText = TRUE,
      border = border_all, borderStyle = "thin"
    ),

    # ColumnOutlineSmall: ColumnOutline with 8pt font
    ColumnOutlineSmall = createStyle(
      fontSize = 8, fontColour = "#FFFFFF", textDecoration = "bold",
      fgFill = "#333399", halign = "center", valign = "center",
      wrapText = TRUE,
      border = border_all, borderStyle = "thin"
    ),

    # ColumnOutlineItalic: ColumnOutline with bold+italic
    ColumnOutlineItalic = createStyle(
      fontSize = 10, fontColour = "#FFFFFF",
      textDecoration = c("bold", "italic"),
      fgFill = "#333399", halign = "center", valign = "center",
      wrapText = TRUE,
      border = border_all, borderStyle = "thin"
    ),

    # ColumnOutlineRotateTop: ColumnOutline + 90-deg rotation, top-aligned
    ColumnOutlineRotateTop = createStyle(
      fontSize = 10, fontColour = "#FFFFFF", textDecoration = "bold",
      fgFill = "#333399", halign = "center", valign = "top",
      wrapText = TRUE, textRotation = 90,
      border = border_all, borderStyle = "thin"
    ),

    # ColumnOutlineRotateCtr: ColumnOutline + 90-deg rotation, center-aligned
    ColumnOutlineRotateCtr = createStyle(
      fontSize = 10, fontColour = "#FFFFFF", textDecoration = "bold",
      fgFill = "#333399", halign = "center", valign = "center",
      wrapText = TRUE, textRotation = 90,
      border = border_all, borderStyle = "thin"
    ),

    # ================================================================
    # DataHeader and Table styles (SAS lines 176-199)
    # ================================================================

    # DataHeader: bold 10pt on gray (#C0C0C0) with all borders
    DataHeader = createStyle(
      fontSize = 10, textDecoration = "bold",
      fgFill = "#C0C0C0", halign = "left", valign = "center",
      border = border_all, borderStyle = "thin"
    ),

    # Table: centered 10pt with all borders
    Table = createStyle(
      fontSize = 10, halign = "center", valign = "center",
      border = border_all, borderStyle = "thin"
    ),

    # ================================================================
    # Data styles — non-bottom rows (SAS lines 201-303)
    # Base Data style: left+right borders only
    # ================================================================

    Data = createStyle(
      valign = "top", border = border_lr, borderStyle = "thin"
    ),

    DataWrap = createStyle(
      valign = "top", wrapText = TRUE,
      border = border_lr, borderStyle = "thin"
    ),

    DataRight = createStyle(
      halign = "right", valign = "top",
      border = border_lr, borderStyle = "thin"
    ),

    DataRightHighlight = createStyle(
      halign = "right", valign = "top", fgFill = "#CCCCFF",
      border = border_lr, borderStyle = "thin"
    ),

    DataCenter = createStyle(
      halign = "center", valign = "top",
      border = border_lr, borderStyle = "thin"
    ),

    # Decimal-0 styles (integer display)
    DataDec0 = createStyle(
      halign = "right", valign = "top", numFmt = "0",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec0Center = createStyle(
      halign = "center", valign = "top", numFmt = "0",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec0Highlight = createStyle(
      halign = "right", valign = "top", numFmt = "0",
      fgFill = "#CCCCFF", border = border_lr, borderStyle = "thin"
    ),

    # Decimal-1 styles (one decimal place)
    DataDec1 = createStyle(
      halign = "right", valign = "top", numFmt = "0.0",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec1Center = createStyle(
      halign = "center", valign = "top", numFmt = "0.0",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec1Highlight = createStyle(
      halign = "right", valign = "top", numFmt = "0.0",
      fgFill = "#CCCCFF", border = border_lr, borderStyle = "thin"
    ),

    # Decimal-2 styles (two decimal places)
    DataDec2 = createStyle(
      halign = "right", valign = "top", numFmt = "0.00",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec2Center = createStyle(
      halign = "center", valign = "top", numFmt = "0.00",
      border = border_lr, borderStyle = "thin"
    ),

    DataDec2Highlight = createStyle(
      halign = "right", valign = "top", numFmt = "0.00",
      fgFill = "#CCCCFF", border = border_lr, borderStyle = "thin"
    ),

    # Scientific notation styles
    DataSN = createStyle(
      halign = "right", valign = "top", numFmt = "0.00E+00",
      border = border_lr, borderStyle = "thin"
    ),

    DataSNCenter = createStyle(
      halign = "center", valign = "top", numFmt = "0.00E+00",
      border = border_lr, borderStyle = "thin"
    ),

    DataSNHighlight = createStyle(
      halign = "right", valign = "top", numFmt = "0.00E+00",
      fgFill = "#CCCCFF", border = border_lr, borderStyle = "thin"
    ),

    # Percentage styles
    DataPct = createStyle(
      halign = "right", valign = "top", numFmt = "0%",
      border = border_lr, borderStyle = "thin"
    ),

    DataPctHighlight = createStyle(
      halign = "right", valign = "top", numFmt = "0%",
      fgFill = "#CCCCFF", border = border_lr, borderStyle = "thin"
    ),

    # ---------------------------------------------------------------
    # Bottom-row variants: same as above but with bottom border added
    # These apply to the last row in each data block for visual closure
    # ---------------------------------------------------------------

    DataBottom = createStyle(
      valign = "top", border = border_lrb, borderStyle = "thin"
    ),

    DataWrapBottom = createStyle(
      valign = "top", wrapText = TRUE,
      border = border_lrb, borderStyle = "thin"
    ),

    DataRightBottom = createStyle(
      halign = "right", valign = "top",
      border = border_lrb, borderStyle = "thin"
    ),

    DataRightHighlightBottom = createStyle(
      halign = "right", valign = "top",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    DataCenterBottom = createStyle(
      halign = "center", valign = "top",
      border = border_lrb, borderStyle = "thin"
    ),

    # Decimal-0 bottom variants
    DataDec0Bottom = createStyle(
      halign = "right", valign = "top", numFmt = "0",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec0CenterBottom = createStyle(
      halign = "center", valign = "top", numFmt = "0",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec0HighlightBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    # Decimal-1 bottom variants
    DataDec1Bottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.0",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec1CenterBottom = createStyle(
      halign = "center", valign = "top", numFmt = "0.0",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec1HighlightBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.0",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    # Decimal-2 bottom variants
    DataDec2Bottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.00",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec2CenterBottom = createStyle(
      halign = "center", valign = "top", numFmt = "0.00",
      border = border_lrb, borderStyle = "thin"
    ),

    DataDec2HighlightBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.00",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    # Scientific notation bottom variants
    DataSNBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.00E+00",
      border = border_lrb, borderStyle = "thin"
    ),

    DataSNCenterBottom = createStyle(
      halign = "center", valign = "top", numFmt = "0.00E+00",
      border = border_lrb, borderStyle = "thin"
    ),

    DataSNHighlightBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0.00E+00",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    # Percentage bottom variants
    DataPctBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0%",
      border = border_lrb, borderStyle = "thin"
    ),

    DataPctHighlightBottom = createStyle(
      halign = "right", valign = "top", numFmt = "0%",
      fgFill = "#CCCCFF", border = border_lrb, borderStyle = "thin"
    ),

    # ---------------------------------------------------------------
    # Special color styles
    # Used for visual indicators in summary/flag worksheets
    # ---------------------------------------------------------------

    # Gray cell: solid gray fill with matching font (visually hidden text)
    Gray = createStyle(
      fontSize = 8, fontColour = "#808080", fgFill = "#808080",
      halign = "center", valign = "top",
      border = border_lrb, borderStyle = "thin"
    ),

    # Red cell: solid red fill with matching font (visually hidden text)
    Red = createStyle(
      fontSize = 8, fontColour = "#FF0000", fgFill = "#FF0000",
      halign = "center", valign = "top",
      border = border_lrb, borderStyle = "thin"
    ),

    # Peach cell: solid peach fill with matching font
    Peach = createStyle(
      fontSize = 8, fontColour = "#FFCC99", fgFill = "#FFCC99",
      halign = "center", valign = "top",
      border = border_lrb, borderStyle = "thin"
    ),

    # Bold red text with decimal-1 formatting and bottom border
    BoldRedText = createStyle(
      fontSize = 8, fontColour = "#FF0000", textDecoration = "bold",
      halign = "right", valign = "top", numFmt = "0.0",
      border = border_lrb, borderStyle = "thin"
    ),

    # ---------------------------------------------------------------
    # Grouping/Subsetting (GS) styles
    # Used by Script Launcher grouping/subsetting metadata worksheets
    # ---------------------------------------------------------------

    # GS with all four borders
    GS_BTLRB = createStyle(
      fontSize = 10, valign = "top", wrapText = TRUE,
      border = border_all, borderStyle = "thin"
    ),

    # GS centered with all four borders
    GSC_BTLRB = createStyle(
      fontSize = 10, halign = "center", valign = "top", wrapText = TRUE,
      border = border_all, borderStyle = "thin"
    ),

    # GS with top, left, right borders (no bottom)
    GS_BTLR = createStyle(
      fontSize = 10, valign = "top", wrapText = TRUE,
      border = border_tlr, borderStyle = "thin"
    ),

    # GS with left and right borders only
    GS_BLR = createStyle(
      fontSize = 10, valign = "top", wrapText = TRUE,
      border = border_lr, borderStyle = "thin"
    ),

    # GS with left, right, and bottom borders
    GS_BLRB = createStyle(
      fontSize = 10, valign = "top", wrapText = TRUE,
      border = border_lrb, borderStyle = "thin"
    )
  )

  return(styles)
}


# =============================================================================
# ws_header — Write Header Rows to Worksheet
# =============================================================================
#' Write header/subtitle/footnote rows to a worksheet
#'
#' Migrated from SAS %wsheader macro (lines 565-592).
#' Writes grouped header rows (Header, SubHeader, or Default style) to a
#' worksheet. Groups are separated by blank rows. Each row is written with
#' the appropriate style based on its group classification.
#'
#' @param wb An openxlsx workbook object.
#' @param sheet Character string or integer identifying the worksheet.
#' @param header_df A data.frame with columns: \code{group} (character grouping
#'   variable, e.g. "Header", "SubHeader", "Default"), and \code{text}
#'   (character content to write).
#' @param styles Named list of openxlsx Style objects from \code{create_workbook_styles()}.
#' @param start_row Integer row number to begin writing (default 1).
#' @param start_col Integer column number to begin writing (default 1).
#'
#' @return Integer: the next available row after all header rows are written.
#' @export
ws_header <- function(wb, sheet, header_df, styles, start_row = 1L,
                      start_col = 1L) {
  # Validate inputs
  if (!inherits(wb, "Workbook")) {
    stop("ws_header: 'wb' must be an openxlsx Workbook object.", call. = FALSE)
  }
  if (is.null(header_df) || nrow(header_df) == 0L) {
    return(start_row)
  }
  if (!all(c("group", "text") %in% names(header_df))) {
    stop("ws_header: 'header_df' must contain columns 'group' and 'text'.",
         call. = FALSE)
  }

  current_row <- start_row
  prev_group <- ""

  for (i in seq_len(nrow(header_df))) {
    row_group <- as.character(header_df$group[i])
    row_text  <- as.character(header_df$text[i])

    # Insert a blank row between groups (mirrors SAS by group notsorted behavior)
    if (nzchar(prev_group) && row_group != prev_group) {
      current_row <- current_row + 1L
    }

    # Write the text content
    writeData(wb, sheet, x = row_text, startRow = current_row,
              startCol = start_col, colNames = FALSE)

    # Apply the appropriate style based on group value
    style_name <- switch(
      row_group,
      "Header"    = "Header",
      "SubHeader" = "SubHeader",
      "Default"
    )
    if (!is.null(styles[[style_name]])) {
      addStyle(wb, sheet, style = styles[[style_name]],
               rows = current_row, cols = start_col)
    }

    prev_group  <- row_group
    current_row <- current_row + 1L
  }

  # Add trailing blank row after all headers (mirrors SAS EOF blank Row)
  current_row <- current_row + 1L

  return(current_row)
}


# =============================================================================
# ws_data — Write Data Rows with Auto-Detected Styles
# =============================================================================
#' Write data rows to a worksheet with automatic style detection
#'
#' Migrated from SAS %wsdata macro (lines 596-689).
#' Writes a data.frame to a worksheet, automatically selecting the
#' appropriate style for each column based on variable name patterns and
#' data type. The last row receives bottom-border variants for visual closure.
#'
#' Supports an optional "fmt" mode that adds header rows with merged cells,
#' applies highlight styles to sort columns, and indents the first column.
#'
#' @param wb An openxlsx workbook object.
#' @param sheet Character string or integer identifying the worksheet.
#' @param data_df A data.frame of values to write.
#' @param styles Named list of openxlsx Style objects from \code{create_workbook_styles()}.
#' @param start_row Integer row number to begin writing (default 1).
#' @param start_col Integer column number to begin writing (default 1).
#' @param fmt Logical; if TRUE, enable format mode with header row and
#'   highlight/indent logic (default FALSE).
#' @param sort_col Optional character vector of column names to apply
#'   "Highlight" style variants to (used when \code{fmt = TRUE}).
#' @param header_merge Integer; number of columns to merge for the header
#'   row in fmt mode (default is \code{ncol(data_df)}).
#' @param col_widths Optional numeric vector of column widths. If provided,
#'   sets column widths via \code{openxlsx::setColWidths()}.
#'
#' @return Integer: the next available row after all data rows are written.
#' @export
ws_data <- function(wb, sheet, data_df, styles, start_row = 1L,
                    start_col = 1L, fmt = FALSE, sort_col = NULL,
                    header_merge = NULL, col_widths = NULL) {
  # Validate inputs
  if (!inherits(wb, "Workbook")) {
    stop("ws_data: 'wb' must be an openxlsx Workbook object.", call. = FALSE)
  }
  if (is.null(data_df) || nrow(data_df) == 0L) {
    return(start_row)
  }

  col_names <- names(data_df)
  n_cols    <- length(col_names)
  n_rows    <- nrow(data_df)

  if (is.null(header_merge)) {
    header_merge <- n_cols
  }

  # Apply column widths if provided
  if (!is.null(col_widths) && length(col_widths) > 0L) {
    width_cols <- seq(start_col, start_col + min(length(col_widths), n_cols) - 1L)
    setColWidths(wb, sheet, cols = width_cols,
                 widths = col_widths[seq_along(width_cols)])
  }

  current_row <- start_row

  # --- Format mode: write a DataHeader row with merged cells ---
  if (isTRUE(fmt) && n_cols > 0L) {
    # Write column names as a header row
    for (ci in seq_len(n_cols)) {
      writeData(wb, sheet, x = col_names[ci],
                startRow = current_row, startCol = start_col + ci - 1L,
                colNames = FALSE)
      if (!is.null(styles[["DataHeader"]])) {
        addStyle(wb, sheet, style = styles[["DataHeader"]],
                 rows = current_row, cols = start_col + ci - 1L)
      }
    }
    # Merge the header row if requested
    if (header_merge > 1L) {
      mergeCells(wb, sheet, cols = start_col:(start_col + header_merge - 1L),
                 rows = current_row)
    }
    current_row <- current_row + 1L
  }

  # --- Determine the style name for each column based on name patterns ---
  col_styles <- vapply(col_names, function(cname) {
    cname_lower <- tolower(cname)

    # Default style base
    style_base <- "Data"

    # Rule: variables containing 'pct' -> Dec1
    if (grepl("pct", cname_lower, fixed = TRUE)) {
      style_base <- "DataDec1"
    }
    # Rule: variables named rd, rr, ort, fd -> Dec1
    else if (cname_lower %in% c("rd", "rr", "ort", "fd")) {
      style_base <- "DataDec1"
    }
    # Rule: variables starting with rd, rr, or or named p_value -> Dec2
    else if (grepl("^(rd|rr|or)", cname_lower) ||
             cname_lower == "p_value") {
      style_base <- "DataDec2"
    }

    return(style_base)
  }, character(1L), USE.NAMES = TRUE)

  # --- Detect sort/highlight columns for fmt mode ---
  highlight_set <- character(0L)
  if (isTRUE(fmt) && !is.null(sort_col)) {
    highlight_set <- tolower(sort_col)
  }

  # --- Write each data row ---
  for (ri in seq_len(n_rows)) {
    is_last_row <- (ri == n_rows)

    for (ci in seq_len(n_cols)) {
      cname       <- col_names[ci]
      cell_value  <- data_df[[cname]][ri]
      style_base  <- col_styles[cname]
      cell_col    <- start_col + ci - 1L

      # Determine if this column is a highlight column
      is_highlight <- tolower(cname) %in% highlight_set

      # Handle missing numeric values: display as "." with DataRight style
      if (is.numeric(cell_value) && is.na(cell_value)) {
        cell_value <- "."
        style_base <- "DataRight"
      }
      # Handle very large numeric values: switch to scientific notation
      else if (is.numeric(cell_value) && !is.na(cell_value) &&
               abs(cell_value) > 1e6) {
        style_base <- "DataSN"
      }

      # Apply highlight suffix for sort columns in fmt mode
      if (is_highlight && !grepl("Highlight", style_base, fixed = TRUE)) {
        style_base <- paste0(style_base, "Highlight")
      }

      # Apply bottom suffix for the last row
      if (is_last_row) {
        style_base <- paste0(style_base, "Bottom")
      }

      # Indent first column in fmt mode (5-space prefix)
      if (isTRUE(fmt) && ci == 1L && is.character(cell_value)) {
        cell_value <- paste0("     ", cell_value)
      }

      # Write cell value
      writeData(wb, sheet, x = cell_value,
                startRow = current_row, startCol = cell_col,
                colNames = FALSE)

      # Apply the resolved style
      resolved_style <- styles[[style_base]]
      if (is.null(resolved_style)) {
        # Fallback: try without highlight/bottom suffixes
        fallback <- col_styles[cname]
        if (is_last_row) fallback <- paste0(fallback, "Bottom")
        resolved_style <- styles[[fallback]]
        if (is.null(resolved_style)) {
          resolved_style <- if (is_last_row) styles[["DataBottom"]] else styles[["Data"]]
        }
      }
      if (!is.null(resolved_style)) {
        addStyle(wb, sheet, style = resolved_style,
                 rows = current_row, cols = cell_col)
      }
    }

    current_row <- current_row + 1L
  }

  return(current_row)
}


# =============================================================================
# ws_rowcount — Count Rows Up To a Stop Point
# =============================================================================
#' Count rows in a data.frame up to a stop value
#'
#' Migrated from SAS %ws_rowcount macro (lines 694-708).
#' Returns the position of the first occurrence of \code{stop_value} in the
#' specified column, or the total row count if \code{stop_value} is not found.
#' Also provides the index of the first and last data rows.
#'
#' @param data_df A data.frame to scan.
#' @param col_name Character name of the column to search.
#' @param stop_value Character or numeric value to find.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{first_row}{Integer: index of the first row (always 1 if data exists).}
#'     \item{last_row}{Integer: index of the row matching \code{stop_value}, or
#'       \code{nrow(data_df)} if not found.}
#'     \item{row_count}{Integer: number of rows up to and including
#'       \code{last_row}.}
#'   }
#' @export
ws_rowcount <- function(data_df, col_name, stop_value = NULL) {
  if (is.null(data_df) || nrow(data_df) == 0L) {
    return(list(first_row = 0L, last_row = 0L, row_count = 0L))
  }

  total_rows <- nrow(data_df)

  if (is.null(stop_value) || !(col_name %in% names(data_df))) {
    return(list(first_row = 1L, last_row = total_rows, row_count = total_rows))
  }

  # Find the first occurrence of stop_value in the specified column
  match_idx <- which(data_df[[col_name]] == stop_value)

  if (length(match_idx) == 0L) {
    return(list(first_row = 1L, last_row = total_rows, row_count = total_rows))
  }

  stop_row <- match_idx[1L]
  return(list(first_row = 1L, last_row = stop_row, row_count = stop_row))
}


# =============================================================================
# annotate_data — Create Formatting Annotations for a Data Frame
# =============================================================================
#' Create formatting annotations for a data frame
#'
#' Migrated from SAS %annotate macro (lines 713-742).
#' Transposes a dataset so that each variable-value pair becomes a row in an
#' annotation tibble. Each row is annotated with metadata for cell-level
#' formatting: row number, column index, data type (String/Number), style ID,
#' merge specifications, and formula/comment/name attributes.
#'
#' The annotation tibble serves as an intermediate representation that
#' \code{write_annotated()} consumes to write styled cells to a workbook.
#'
#' @param data_df A data.frame to annotate.
#' @param style_id Character; default StyleID for all cells (e.g. "Data").
#' @param height Numeric or NULL; row height in points.
#' @param merge_across Integer or NULL; number of columns to merge across.
#' @param merge_down Integer or NULL; number of rows to merge down.
#' @param formula Character or NULL; Excel formula for the cell.
#' @param comment Character or NULL; Excel comment/note text.
#' @param name Character or NULL; named range identifier.
#' @param array_range Character or NULL; array formula range.
#' @param col_styles Named character vector mapping column names to style IDs.
#'   Overrides the default \code{style_id} for specific columns.
#'
#' @return A tibble with one row per cell, containing columns:
#'   \code{row_num}, \code{col_num}, \code{var_name}, \code{value},
#'   \code{data_type}, \code{style_id}, \code{height}, \code{merge_across},
#'   \code{merge_down}, \code{formula}, \code{comment}, \code{name},
#'   \code{array_range}, \code{is_bottom}.
#' @export
annotate_data <- function(data_df, style_id = "Data", height = NULL,
                          merge_across = NULL, merge_down = NULL,
                          formula = NULL, comment = NULL, name = NULL,
                          array_range = NULL, col_styles = NULL) {
  if (is.null(data_df) || nrow(data_df) == 0L) {
    return(tibble(
      row_num      = integer(0L),
      col_num      = integer(0L),
      var_name     = character(0L),
      value        = character(0L),
      data_type    = character(0L),
      style_id     = character(0L),
      height       = numeric(0L),
      merge_across = integer(0L),
      merge_down   = integer(0L),
      formula      = character(0L),
      comment      = character(0L),
      name         = character(0L),
      array_range  = character(0L),
      is_bottom    = logical(0L)
    ))
  }

  col_names <- names(data_df)
  n_rows    <- nrow(data_df)
  n_cols    <- length(col_names)

  # Build a column metadata tibble for type detection using dplyr
  col_meta <- tibble(
    var_name  = col_names,
    col_num   = seq_along(col_names),
    is_numeric = vapply(data_df, is.numeric, logical(1L))
  ) %>%
    mutate(
      base_type = if_else(.data$is_numeric, "Number", "String")
    ) %>%
    select("var_name", "col_num", "base_type")

  # Build per-column style assignment using dplyr case_when
  # Pre-compute col_style_vec safely (col_styles may be NULL)
  has_overrides <- !is.null(col_styles) && length(col_styles) > 0L
  if (has_overrides) {
    # Build a lookup vector: for each column, check if it has an override
    override_vec <- vapply(col_names, function(cn) {
      if (cn %in% names(col_styles)) col_styles[[cn]] else NA_character_
    }, character(1L))

    col_style_map <- col_meta %>%
      mutate(
        override = override_vec[.data$var_name],
        col_style = case_when(
          !is.na(.data$override) ~ .data$override,
          TRUE                   ~ style_id
        )
      ) %>%
      select("var_name", "col_num", "base_type", "col_style")
  } else {
    col_style_map <- col_meta %>%
      mutate(
        col_style = style_id
      ) %>%
      select("var_name", "col_num", "base_type", "col_style")
  }

  # Pre-allocate vectors for the annotation tibble
  total_cells <- n_rows * n_cols
  out_row_num      <- integer(total_cells)
  out_col_num      <- integer(total_cells)
  out_var_name     <- character(total_cells)
  out_value        <- character(total_cells)
  out_data_type    <- character(total_cells)
  out_style_id     <- character(total_cells)
  out_is_bottom    <- logical(total_cells)

  idx <- 0L
  for (ri in seq_len(n_rows)) {
    is_last <- (ri == n_rows)
    for (ci in seq_len(n_cols)) {
      idx <- idx + 1L
      cname      <- col_names[ci]
      cell_value <- data_df[[cname]][ri]
      cell_type  <- col_style_map$base_type[ci]

      # Handle NA values: type becomes "String", value becomes "." for numeric
      if (is.na(cell_value)) {
        cell_str  <- if_else(cell_type == "Number", ".", "")
        cell_type <- "String"
      } else {
        cell_str <- as.character(cell_value)
      }

      # Replace ~! placeholder with space (SAS convention)
      cell_str <- gsub("~!", " ", cell_str, fixed = TRUE)

      out_row_num[idx]   <- ri
      out_col_num[idx]   <- ci
      out_var_name[idx]  <- cname
      out_value[idx]     <- cell_str
      out_data_type[idx] <- cell_type
      out_style_id[idx]  <- col_style_map$col_style[ci]
      out_is_bottom[idx] <- is_last
    }
  }

  result <- tibble(
    row_num      = out_row_num,
    col_num      = out_col_num,
    var_name     = out_var_name,
    value        = out_value,
    data_type    = out_data_type,
    style_id     = out_style_id,
    height       = if (!is.null(height)) height else NA_real_,
    merge_across = if (!is.null(merge_across)) as.integer(merge_across) else NA_integer_,
    merge_down   = if (!is.null(merge_down)) as.integer(merge_down) else NA_integer_,
    formula      = if (!is.null(formula)) formula else NA_character_,
    comment      = if (!is.null(comment)) comment else NA_character_,
    name         = if (!is.null(name)) name else NA_character_,
    array_range  = if (!is.null(array_range)) array_range else NA_character_,
    is_bottom    = out_is_bottom
  )

  # Apply filter to remove any rows with empty var_name (defensive)
  result <- filter(result, nzchar(.data$var_name))

  return(result)
}


# =============================================================================
# write_annotated — Write Annotated Cells to a Workbook
# =============================================================================
#' Write annotated cells to an openxlsx workbook
#'
#' Migrated from SAS %markup macro (lines 746-785).
#' Consumes an annotation tibble produced by \code{annotate_data()} and
#' writes each cell to the specified worksheet with its resolved style,
#' merge specifications, formula, comment, and named range attributes.
#'
#' This function replaces the SAS DATA _NULL_ XML string building approach
#' with direct openxlsx API calls for cell-level formatting.
#'
#' @param wb An openxlsx workbook object.
#' @param sheet Character string or integer identifying the worksheet.
#' @param annotations A tibble from \code{annotate_data()} with cell metadata.
#' @param styles Named list of openxlsx Style objects from \code{create_workbook_styles()}.
#' @param start_row Integer row offset to add to all row numbers (default 0).
#' @param start_col Integer column offset to add to all column numbers (default 0).
#'
#' @return Integer: the next available row after all annotated cells are written.
#' @export
write_annotated <- function(wb, sheet, annotations, styles,
                            start_row = 0L, start_col = 0L) {
  # Validate inputs
  if (!inherits(wb, "Workbook")) {
    stop("write_annotated: 'wb' must be an openxlsx Workbook object.",
         call. = FALSE)
  }
  if (is.null(annotations) || nrow(annotations) == 0L) {
    return(start_row + 1L)
  }

  required_cols <- c("row_num", "col_num", "value", "data_type", "style_id")
  missing_cols <- setdiff(required_cols, names(annotations))
  if (length(missing_cols) > 0L) {
    stop("write_annotated: annotations missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  max_row <- 0L

  for (i in seq_len(nrow(annotations))) {
    ann       <- annotations[i, ]
    cell_row  <- ann$row_num + start_row
    cell_col  <- ann$col_num + start_col
    cell_val  <- ann$value
    cell_type <- ann$data_type
    sid       <- ann$style_id

    # Track the maximum row written
    if (cell_row > max_row) {
      max_row <- cell_row
    }

    # Determine if bottom style variant should be applied
    is_bottom <- if ("is_bottom" %in% names(ann)) {
      isTRUE(ann$is_bottom)
    } else {
      FALSE
    }

    # Resolve the style name with optional Bottom suffix
    style_name <- sid
    if (is_bottom && !grepl("Bottom", sid, fixed = TRUE)) {
      style_name_bottom <- paste0(sid, "Bottom")
      if (!is.null(styles[[style_name_bottom]])) {
        style_name <- style_name_bottom
      }
    }

    # Write the cell value based on data type
    if (cell_type == "Number" && !is.na(cell_val) && cell_val != ".") {
      numeric_val <- suppressWarnings(as.numeric(cell_val))
      if (!is.na(numeric_val)) {
        writeData(wb, sheet, x = numeric_val,
                  startRow = cell_row, startCol = cell_col, colNames = FALSE)
      } else {
        writeData(wb, sheet, x = cell_val,
                  startRow = cell_row, startCol = cell_col, colNames = FALSE)
      }
    } else {
      # String type or missing numeric displayed as "."
      writeData(wb, sheet, x = cell_val,
                startRow = cell_row, startCol = cell_col, colNames = FALSE)
    }

    # Apply style
    resolved_style <- styles[[style_name]]
    if (is.null(resolved_style)) {
      resolved_style <- styles[[sid]]
    }
    if (!is.null(resolved_style)) {
      addStyle(wb, sheet, style = resolved_style,
               rows = cell_row, cols = cell_col)
    }

    # Handle merge across
    if ("merge_across" %in% names(ann) && !is.na(ann$merge_across) &&
        ann$merge_across > 0L) {
      end_col <- cell_col + as.integer(ann$merge_across)
      mergeCells(wb, sheet, cols = cell_col:end_col, rows = cell_row)
    }

    # Handle merge down
    if ("merge_down" %in% names(ann) && !is.na(ann$merge_down) &&
        ann$merge_down > 0L) {
      end_row <- cell_row + as.integer(ann$merge_down)
      mergeCells(wb, sheet, cols = cell_col, rows = cell_row:end_row)
    }

    # Handle formula (write formula instead of value if present)
    if ("formula" %in% names(ann) && !is.na(ann$formula) &&
        nzchar(ann$formula)) {
      writeFormula(wb, sheet, x = ann$formula,
                   startRow = cell_row, startCol = cell_col)
    }

    # Handle comment
    if ("comment" %in% names(ann) && !is.na(ann$comment) &&
        nzchar(ann$comment)) {
      comment_obj <- createComment(comment = ann$comment, visible = FALSE)
      writeComment(wb, sheet, col = cell_col, row = cell_row,
                   comment = comment_obj)
    }
  }

  return(max_row + 1L)
}


# =============================================================================
# style_from_spec — Create an openxlsx Style from a Specification
# =============================================================================
#' Create an openxlsx Style object from a named specification
#'
#' Migrated from SAS %xml_style_dcl / %xml_style_markup macros (lines 820-896).
#' Accepts a named list (or single-row data.frame) of style properties and
#' returns an openxlsx \code{createStyle()} object. This allows dynamic style
#' creation from metadata specifications.
#'
#' The property names mirror the SAS SpreadsheetML attribute conventions
#' but are mapped to openxlsx parameter names.
#'
#' @param spec A named list or single-row data.frame with optional elements:
#'   \describe{
#'     \item{HA}{Horizontal alignment: "Left", "Center", "Right".}
#'     \item{VA}{Vertical alignment: "Top", "Center", "Bottom".}
#'     \item{Indent}{Integer indent level (not directly supported by openxlsx;
#'       documented in MIGRATION NOTES).}
#'     \item{Wrap}{Logical; TRUE to enable text wrapping.}
#'     \item{Rotate}{Numeric text rotation angle (0-180).}
#'     \item{BT, BL, BR, BB}{Border positions: Top, Left, Right, Bottom.
#'       Set to "Continuous" or "thin" to enable.}
#'     \item{BWt}{Border weight (SAS Weight="1" mapped to "thin").}
#'     \item{BLS}{Border line style (default "Continuous" mapped to "thin").}
#'     \item{IntClr}{Interior/fill color as hex string (e.g. "#C0C0C0").}
#'     \item{FontSize}{Numeric font size in points.}
#'     \item{FontColor}{Font color as hex string.}
#'     \item{Bold}{Logical or 0/1; TRUE for bold text.}
#'     \item{Italic}{Logical or 0/1; TRUE for italic text.}
#'     \item{NumFmt}{Number format string (e.g. "0.00", "0%").}
#'   }
#'
#' @return An openxlsx Style object created via \code{createStyle()}.
#' @export
style_from_spec <- function(spec) {
  if (is.data.frame(spec)) {
    if (nrow(spec) != 1L) {
      stop("style_from_spec: 'spec' data.frame must have exactly 1 row.",
           call. = FALSE)
    }
    spec <- as.list(spec)
  }
  if (!is.list(spec)) {
    stop("style_from_spec: 'spec' must be a named list or single-row data.frame.",
         call. = FALSE)
  }

  # --- Alignment ---
  halign <- NULL
  if (!is.null(spec$HA) && nzchar(spec$HA)) {
    halign <- tolower(spec$HA)
  }

  valign <- NULL
  if (!is.null(spec$VA) && nzchar(spec$VA)) {
    valign <- tolower(spec$VA)
  }

  # --- Text wrapping ---
  wrap_text <- FALSE
  if (!is.null(spec$Wrap) && (isTRUE(spec$Wrap) || spec$Wrap == 1L)) {
    wrap_text <- TRUE
  }

  # --- Text rotation ---
  text_rotation <- NULL
  if (!is.null(spec$Rotate) && !is.na(spec$Rotate)) {
    text_rotation <- as.numeric(spec$Rotate)
  }

  # --- Borders ---
  border_positions <- character(0L)
  border_style_val <- "thin"

  if (!is.null(spec$BWt)) {
    border_style_val <- switch(
      as.character(spec$BWt),
      "1" = "thin",
      "2" = "medium",
      "3" = "thick",
      "thin"
    )
  }
  if (!is.null(spec$BLS) && nzchar(spec$BLS)) {
    border_style_val <- switch(
      tolower(spec$BLS),
      "continuous" = "thin",
      "dash"       = "dashed",
      "dot"        = "dotted",
      "thin"
    )
  }

  if (!is.null(spec$BT) && nzchar(spec$BT)) border_positions <- c(border_positions, "Top")
  if (!is.null(spec$BB) && nzchar(spec$BB)) border_positions <- c(border_positions, "Bottom")
  if (!is.null(spec$BL) && nzchar(spec$BL)) border_positions <- c(border_positions, "Left")
  if (!is.null(spec$BR) && nzchar(spec$BR)) border_positions <- c(border_positions, "Right")

  # --- Fill color ---
  fg_fill <- NULL
  if (!is.null(spec$IntClr) && nzchar(spec$IntClr)) {
    fg_fill <- spec$IntClr
    # Ensure hex prefix
    if (!grepl("^#", fg_fill)) {
      fg_fill <- paste0("#", fg_fill)
    }
  }

  # --- Font ---
  font_size <- NULL
  if (!is.null(spec$FontSize) && !is.na(spec$FontSize)) {
    font_size <- as.numeric(spec$FontSize)
  }

  font_colour <- NULL
  if (!is.null(spec$FontColor) && nzchar(spec$FontColor)) {
    font_colour <- spec$FontColor
    if (!grepl("^#", font_colour)) {
      font_colour <- paste0("#", font_colour)
    }
  }

  # --- Text decoration ---
  text_decoration <- NULL
  is_bold   <- !is.null(spec$Bold) && (isTRUE(spec$Bold) || spec$Bold == 1L)
  is_italic <- !is.null(spec$Italic) && (isTRUE(spec$Italic) || spec$Italic == 1L)
  if (is_bold && is_italic) {
    text_decoration <- c("bold", "italic")
  } else if (is_bold) {
    text_decoration <- "bold"
  } else if (is_italic) {
    text_decoration <- "italic"
  }

  # --- Number format ---
  num_fmt <- NULL
  if (!is.null(spec$NumFmt) && nzchar(spec$NumFmt)) {
    num_fmt <- spec$NumFmt
  }

  # --- Build createStyle() arguments ---
  style_args <- list()

  if (!is.null(font_size))       style_args$fontSize       <- font_size
  if (!is.null(font_colour))     style_args$fontColour     <- font_colour
  if (!is.null(text_decoration)) style_args$textDecoration <- text_decoration
  if (!is.null(halign))          style_args$halign         <- halign
  if (!is.null(valign))          style_args$valign         <- valign
  if (isTRUE(wrap_text))         style_args$wrapText       <- TRUE
  if (!is.null(text_rotation))   style_args$textRotation   <- text_rotation
  if (!is.null(fg_fill))         style_args$fgFill         <- fg_fill
  if (!is.null(num_fmt))         style_args$numFmt         <- num_fmt

  if (length(border_positions) > 0L) {
    style_args$border      <- border_positions
    style_args$borderStyle <- border_style_val
  }

  # Create and return the Style object
  style_obj <- do.call(createStyle, style_args)
  return(style_obj)
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ============================================================
#### MIGRATION NOTES
#### ============================================================
#
#### ASSUMPTIONS:
####   1. openxlsx createStyle() supports all formatting features used by
####      SpreadsheetML styles. Style inheritance (ss:Parent) in SAS is
####      flattened into complete self-contained style definitions since
####      openxlsx does not support style inheritance chains.
####   2. SAS border Weight="1" maps to openxlsx borderStyle = "thin".
####      SAS border LineStyle="Continuous" maps to openxlsx "thin".
####   3. SAS date/number format strings (e.g. "0.00E+00") are compatible
####      with Excel number format strings used by openxlsx numFmt parameter.
####   4. The ~! placeholder convention from SAS (representing a space) is
####      converted to an actual space character via gsub in annotate_data().
####   5. SAS strlen buffer sizing (%sysfunc(ifc(not %symexist(strlen),...)))
####      is not needed in R; openxlsx handles string lengths internally.
####   6. Font families default to Excel's standard Calibri/Arial via openxlsx
####      defaults; SAS SpreadsheetML did not specify explicit font families
####      in most styles.
#
#### POTENTIAL NUMERICAL DIFFERENCES:
####   None — this is formatting/output infrastructure only. No statistical
####   computations are performed. All numerical precision is controlled by
####   the calling analytical scripts via numFmt format strings.
#
#### NO DIRECT R EQUIVALENT:
####   1. SpreadsheetML XML streaming (SAS DATA _NULL_ + PUT statements) is
####      replaced by openxlsx in-memory workbook API. The SAS approach built
####      XML strings character by character; openxlsx uses structured API calls.
####   2. SAS DATA step XML string building (%annotate/%markup macros) is
####      replaced by a two-step workflow: annotate_data() creates a metadata
####      tibble, then write_annotated() applies it via openxlsx writeData +
####      addStyle cell-level API calls.
####   3. SAS style inheritance via ss:Parent attribute must be flattened —
####      each child style includes all parent properties explicitly since
####      openxlsx does not support style inheritance.
####   4. SAS strlen buffer sizing is not needed in R.
####   5. Cell indentation (SAS ss:Indent) has no direct openxlsx support.
####      Workaround: string padding with spaces in annotate_data() and
####      ws_data(). This is documented as a known limitation.
####   6. SAS xml_tag_def / xml_init macros (variable declarations for XML tag
####      attributes) have no R equivalent — openxlsx handles cell types and
####      attributes through its structured API rather than string variables.
#
#### PACKAGE SELECTION RATIONALE:
####   1. openxlsx (>=4.2.5) chosen as sole Excel output engine — supports
####      styles, formulas, named ranges, data validation, conditional
####      formatting, merged cells, page setup. Replaces SpreadsheetML XML
####      generation entirely. No Java dependency (unlike the xlsx package).
####   2. dplyr (>=1.1.0) used for data manipulation in annotate_data() and
####      ws_data() — tibble creation, type detection, conditional logic.
####      Consistent with the project-wide tidyverse-first policy.
#
#### OPEN QUESTIONS:
####   1. Verify openxlsx textRotation property produces identical visual
####      result to SAS ss:Rotate="90". openxlsx uses 0-180 degree range;
####      SAS uses the same convention. Visual verification recommended.
####   2. Confirm border weight mapping: SAS Weight="1" corresponds to
####      openxlsx "thin" (0.5pt). SAS Weight="2" corresponds to "medium"
####      (1pt). Verify visual equivalence in generated Excel output.
####   3. Cell indentation: openxlsx does not natively support the Indent
####      property from SpreadsheetML. The current workaround uses string
####      padding with spaces. If pixel-exact indentation is required,
####      consider post-processing the .xlsx XML or using a different library.
####   4. Named ranges and array formulas: write_annotated() supports these
####      via openxlsx writeFormula() and createNamedRegion(). Verify that
####      formulas written this way behave identically to SAS-generated
####      SpreadsheetML formula cells.
# ============================================================
