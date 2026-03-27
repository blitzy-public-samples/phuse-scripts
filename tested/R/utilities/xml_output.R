# =============================================================================
# PROGRAM NAME: xml_output.R
#
# DESCRIPTION:  Excel workbook output engine — foundational layer for all domain
#               panel drivers and utility files that generate formatted Excel
#               (.xlsx) workbooks. Replaces the SAS SpreadsheetML XML generation
#               engine (xml_output.sas) with the openxlsx package.
#
#               Provides functions for:
#               - Workbook creation with metadata (title, author, timestamp)
#               - A comprehensive style gallery (50+ named styles) for cell-level
#                 formatting matching the original SAS SpreadsheetML style IDs
#               - Writing formatted data tables with column-type-aware styling
#               - Writing header/subtitle/footnote rows
#               - Cell merging, page setup, and style lookup utilities
#
# ORIGINAL SAS: tested/SAS/ZZ_Utilities/xml_output.sas (897 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
# SAS DATE:     February 15, 2011
#
# R MIGRATION:  Migrated to R using openxlsx package
# R REQUIRES:   openxlsx (>= 4.2.5)
#
# NOTES:        This is the foundational output layer. Other utility files
#               (err_output.R, md_output.R, sl_gs_output.R) and all domain
#               panel drivers depend on these functions.
# =============================================================================

# Load required package
library(openxlsx)

# =============================================================================
# create_workbook
# =============================================================================
#' Create a new Excel workbook with metadata.
#'
#' Replaces SAS \code{%wb(title)} macro (lines 41-66 of xml_output.sas).
#' In SAS, this created XML preamble/epilogue strings for SpreadsheetML.
#' In R, this creates an openxlsx workbook object with creator and title
#' metadata.
#'
#' @param title   Character. The document title stored in workbook properties.
#'                Defaults to empty string.
#' @param author  Character. The document author stored in workbook properties.
#'                Defaults to "US Food & Drug Administration" to match the
#'                original SAS author metadata.
#'
#' @return An openxlsx workbook object ready for worksheet additions.
#'
#' @examples
#' wb <- create_workbook(title = "Adverse Events Summary")
#' wb <- create_workbook(title = "Demographics", author = "PhUSE CS")
create_workbook <- function(title = "", author = "US Food & Drug Administration") {
  # Validate inputs
  if (!is.character(title) || length(title) != 1L) {
    stop("'title' must be a single character string.", call. = FALSE)
  }
  if (!is.character(author) || length(author) != 1L) {
    stop("'author' must be a single character string.", call. = FALSE)
  }

  # Create workbook with openxlsx — replaces SAS XML preamble construction

  # SAS built: <?xml ...?>, <Workbook ...>, <DocumentProperties>...</DocumentProperties>
  # openxlsx handles all of this internally via the OOXML format
  wb <- openxlsx::createWorkbook(creator = author, title = title)

  return(wb)
}


# =============================================================================
# create_workbook_styles
# =============================================================================
#' Create the complete style gallery for Excel workbook formatting.
#'
#' Replaces SAS \code{%styles(size=9)} macro (lines 72-558 of xml_output.sas).
#' Generates 50+ named styles that mirror the original SAS SpreadsheetML style
#' IDs. Each style is an \code{openxlsx::createStyle()} object stored in a
#' named list. Downstream functions reference styles by name (e.g., "Data",
#' "DataDec1Bottom", "ColumnOutline") for backward compatibility.
#'
#' @param base_size Numeric. Base font size in points. Defaults to 9, matching
#'                  the SAS \code{%styles(size=9)} default.
#'
#' @return A named list of \code{openxlsx} style objects.
#'
#' @examples
#' styles <- create_workbook_styles()
#' styles <- create_workbook_styles(base_size = 10)
create_workbook_styles <- function(base_size = 9) {
  # Validate input
  if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0) {
    stop("'base_size' must be a single positive number.", call. = FALSE)
  }

  styles <- list()

  # -------------------------------------------------------------------------
  # DEFAULT FAMILY — Base styles with varying alignment and font sizes
  # SAS: Default (ID="Default", ss:Name="Normal"), DefaultLeft, DefaultRight,
  #      DefaultWhite, Default10, Default10Wrap, Default10RedWrap,
  #      Default10Right, Default8
  # -------------------------------------------------------------------------
  styles$Default <- openxlsx::createStyle(
    fontSize = base_size,
    valign   = "top"
  )

  styles$DefaultLeft <- openxlsx::createStyle(
    fontSize = base_size,
    halign   = "left",
    valign   = "top"
  )

  styles$DefaultRight <- openxlsx::createStyle(
    fontSize = base_size,
    halign   = "right",
    valign   = "top"
  )

  styles$DefaultWhite <- openxlsx::createStyle(
    fontSize   = base_size,
    fontColour = "#FFFFFF"
  )

  styles$Default10 <- openxlsx::createStyle(
    fontSize = 10,
    valign   = "top"
  )

  styles$Default10Wrap <- openxlsx::createStyle(
    fontSize = 10,
    valign   = "top",
    wrapText = TRUE
  )

  styles$Default10RedWrap <- openxlsx::createStyle(
    fontSize       = 10,
    valign         = "top",
    wrapText       = TRUE,
    fontColour     = "#FF0000",
    textDecoration = "italic"
  )

  styles$Default10Right <- openxlsx::createStyle(
    fontSize = 10,
    halign   = "right",
    valign   = "top"
  )

  styles$Default8 <- openxlsx::createStyle(
    fontSize = 8,
    valign   = "top"
  )

  # -------------------------------------------------------------------------
  # HEADER FAMILY — Title and subtitle styles
  # SAS: Header (12pt, bold+italic), SubHeader (10pt, bold)
  # -------------------------------------------------------------------------
  styles$Header <- openxlsx::createStyle(
    fontSize       = 12,
    textDecoration = c("bold", "italic")
  )

  styles$SubHeader <- openxlsx::createStyle(
    fontSize       = 10,
    valign         = "top",
    textDecoration = "bold"
  )

  # -------------------------------------------------------------------------
  # COLUMN HEADER FAMILY — Dark blue background (#333399), white bold text
  # SAS: Column, ColumnOutline, ColumnOutlineSmall, ColumnOutlineItalic,
  #      ColumnOutlineRotateTop, ColumnOutlineRotateCtr
  # -------------------------------------------------------------------------
  styles$Column <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = "bold"
  )

  styles$ColumnOutline <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = "bold",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin"
  )

  styles$ColumnOutlineSmall <- openxlsx::createStyle(
    fontSize       = 8,
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = "bold",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin"
  )

  styles$ColumnOutlineItalic <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = c("bold", "italic"),
    border         = "TopBottomLeftRight",
    borderStyle    = "thin"
  )

  styles$ColumnOutlineRotateTop <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "center",
    valign         = "top",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = "bold",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin",
    textRotation   = 90
  )

  styles$ColumnOutlineRotateCtr <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#333399",
    fontColour     = "#FFFFFF",
    textDecoration = "bold",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin",
    textRotation   = 90
  )

  # -------------------------------------------------------------------------
  # DATA HEADER — Gray background (#C0C0C0), bold, all borders
  # SAS: DataHeader
  # -------------------------------------------------------------------------
  styles$DataHeader <- openxlsx::createStyle(
    fontSize       = 10,
    halign         = "left",
    valign         = "center",
    fgFill         = "#C0C0C0",
    textDecoration = "bold",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin"
  )

  # -------------------------------------------------------------------------
  # TABLE STYLE — Centered, all borders, 10pt
  # SAS: Table
  # -------------------------------------------------------------------------
  styles$Table <- openxlsx::createStyle(
    fontSize    = 10,
    halign      = "center",
    valign      = "center",
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  # -------------------------------------------------------------------------
  # DATA FAMILY — Left+Right borders, various alignments
  # SAS: Data (parent), DataWrap, DataRight, DataRightHighlight, DataCenter
  # -------------------------------------------------------------------------
  styles$Data <- openxlsx::createStyle(
    fontSize    = base_size,
    valign      = "top",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataWrap <- openxlsx::createStyle(
    fontSize    = base_size,
    valign      = "top",
    wrapText    = TRUE,
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataRight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataRightHighlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataCenter <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "center",
    valign      = "top",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # -------------------------------------------------------------------------
  # DECIMAL FORMAT FAMILIES — 0, 1, 2 decimal places
  # SAS: DataDec0/Center/Highlight, DataDec1/Center/Highlight,
  #      DataDec2/Center/Highlight
  # -------------------------------------------------------------------------

  # Zero decimal places
  styles$DataDec0 <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec0Center <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "center",
    valign      = "top",
    numFmt      = "0",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec0Highlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # One decimal place
  styles$DataDec1 <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.0",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec1Center <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "center",
    valign      = "top",
    numFmt      = "0.0",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec1Highlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.0",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # Two decimal places
  styles$DataDec2 <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.00",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec2Center <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "center",
    valign      = "top",
    numFmt      = "0.00",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataDec2Highlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.00",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # -------------------------------------------------------------------------
  # SCIENTIFIC NOTATION — Large numbers (> 10^6)
  # SAS: DataSN, DataSNCenter, DataSNHighlight
  # -------------------------------------------------------------------------
  styles$DataSN <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.00E+00",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataSNCenter <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "center",
    valign      = "top",
    numFmt      = "0.00E+00",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataSNHighlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0.00E+00",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # -------------------------------------------------------------------------
  # PERCENT FORMAT
  # SAS: DataPct, DataPctHighlight
  # -------------------------------------------------------------------------
  styles$DataPct <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0%",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  styles$DataPctHighlight <- openxlsx::createStyle(
    fontSize    = base_size,
    halign      = "right",
    valign      = "top",
    numFmt      = "0%",
    fgFill      = "#CCCCFF",
    border      = "LeftRight",
    borderStyle = "thin"
  )

  # -------------------------------------------------------------------------
  # BOTTOM-ROW VARIANTS — Add bottom border to each base data style
  # SAS defines these explicitly (lines 305-474); in R we build them

  # programmatically using a helper that replicates each base style's
  # properties but changes the border to include Bottom+Left+Right.
  # -------------------------------------------------------------------------

  # Helper: build a bottom-row variant by specifying the same properties as

  # the base style but with border = c("Bottom","Left","Right")
  .make_bottom <- function(base_style_params) {
    base_style_params$border      <- c("Bottom", "Left", "Right")
    base_style_params$borderStyle <- "thin"
    do.call(openxlsx::createStyle, base_style_params)
  }

  # Define base style parameters for each style that gets a Bottom variant
  # This matches the SAS explicit bottom-row style definitions exactly
  bottom_defs <- list(
    Data = list(
      fontSize = base_size, valign = "top"
    ),
    DataWrap = list(
      fontSize = base_size, valign = "top", wrapText = TRUE
    ),
    DataRight = list(
      fontSize = base_size, halign = "right", valign = "top"
    ),
    DataRightHighlight = list(
      fontSize = base_size, halign = "right", valign = "top",
      fgFill = "#CCCCFF"
    ),
    DataCenter = list(
      fontSize = base_size, halign = "center", valign = "top"
    ),
    DataDec0 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0"
    ),
    DataDec0Center = list(
      fontSize = base_size, halign = "center", valign = "top", numFmt = "0"
    ),
    DataDec0Highlight = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0",
      fgFill = "#CCCCFF"
    ),
    DataDec1 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.0"
    ),
    DataDec1Center = list(
      fontSize = base_size, halign = "center", valign = "top", numFmt = "0.0"
    ),
    DataDec1Highlight = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.0",
      fgFill = "#CCCCFF"
    ),
    DataDec2 = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.00"
    ),
    DataDec2Center = list(
      fontSize = base_size, halign = "center", valign = "top", numFmt = "0.00"
    ),
    DataDec2Highlight = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0.00",
      fgFill = "#CCCCFF"
    ),
    DataSN = list(
      fontSize = base_size, halign = "right", valign = "top",
      numFmt = "0.00E+00"
    ),
    DataSNCenter = list(
      fontSize = base_size, halign = "center", valign = "top",
      numFmt = "0.00E+00"
    ),
    DataSNHighlight = list(
      fontSize = base_size, halign = "right", valign = "top",
      numFmt = "0.00E+00", fgFill = "#CCCCFF"
    ),
    DataPct = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0%"
    ),
    DataPctHighlight = list(
      fontSize = base_size, halign = "right", valign = "top", numFmt = "0%",
      fgFill = "#CCCCFF"
    )
  )

  for (nm in names(bottom_defs)) {
    bottom_name <- paste0(nm, "Bottom")
    styles[[bottom_name]] <- .make_bottom(bottom_defs[[nm]])
  }

  # -------------------------------------------------------------------------
  # AE-SPECIFIC COLOR STYLES — For MedDRA report cover page
  # SAS: Gray, Red, Peach, BoldRedText (lines 476-499)
  # Note: These inherit from DataCenterBottom / DataDec1Bottom in SAS, so
  # they include all four borders.
  # -------------------------------------------------------------------------
  styles$Gray <- openxlsx::createStyle(
    fontSize    = 8,
    halign      = "center",
    valign      = "top",
    fontColour  = "#808080",
    fgFill      = "#808080",
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  styles$Red <- openxlsx::createStyle(
    fontSize    = 8,
    halign      = "center",
    valign      = "top",
    fontColour  = "#FF0000",
    fgFill      = "#FF0000",
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  styles$Peach <- openxlsx::createStyle(
    fontSize    = 8,
    halign      = "center",
    valign      = "top",
    fontColour  = "#FFCC99",
    fgFill      = "#FFCC99",
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  styles$BoldRedText <- openxlsx::createStyle(
    fontSize       = 8,
    halign         = "right",
    valign         = "top",
    fontColour     = "#FF0000",
    textDecoration = "bold",
    numFmt         = "0.0",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin"
  )

  # -------------------------------------------------------------------------
  # GROUPING/SUBSETTING STYLES — For Script Launcher metadata
  # SAS: GS_BTLRB, GSC_BTLRB, GS_BTLR, GS_BLR, GS_BLRB (lines 501-552)
  # -------------------------------------------------------------------------
  styles$GS_BTLRB <- openxlsx::createStyle(
    fontSize    = 10,
    valign      = "top",
    wrapText    = TRUE,
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  styles$GSC_BTLRB <- openxlsx::createStyle(
    fontSize    = 10,
    halign      = "center",
    valign      = "top",
    wrapText    = TRUE,
    border      = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  styles$GS_BTLR <- openxlsx::createStyle(
    fontSize    = 10,
    valign      = "top",
    wrapText    = TRUE,
    border      = c("Top", "Left", "Right"),
    borderStyle = "thin"
  )

  styles$GS_BLR <- openxlsx::createStyle(
    fontSize    = 10,
    valign      = "top",
    wrapText    = TRUE,
    border      = c("Left", "Right"),
    borderStyle = "thin"
  )

  styles$GS_BLRB <- openxlsx::createStyle(
    fontSize    = 10,
    valign      = "top",
    wrapText    = TRUE,
    border      = c("Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  return(styles)
}


# =============================================================================
# get_style_by_name
# =============================================================================
#' Retrieve a style object from the style gallery by name.
#'
#' Provides safe lookup into the style gallery with fallback logic. If the
#' requested style name is not found, attempts to find the base variant (without
#' "Bottom" suffix). If neither exists, returns the Default style. This avoids
#' errors when callers request styles that may not be defined.
#'
#' @param styles A named list of openxlsx style objects as returned by
#'               \code{create_workbook_styles()}.
#' @param name   Character. The style name to look up (e.g., "DataDec1Bottom").
#'
#' @return An openxlsx style object.
#'
#' @examples
#' styles <- create_workbook_styles()
#' s <- get_style_by_name(styles, "DataDec1Bottom")
#' s <- get_style_by_name(styles, "NonExistent")  # returns Default
get_style_by_name <- function(styles, name) {
  # Validate inputs
  if (!is.list(styles) || length(styles) == 0L) {
    stop("'styles' must be a non-empty named list of style objects.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string.", call. = FALSE)
  }


  # Direct lookup — most common case
  if (name %in% names(styles)) {
    return(styles[[name]])
  }

  # Fallback 1: strip "Bottom" suffix and try the base style
  base_name <- sub("Bottom$", "", name)
  if (base_name != name && base_name %in% names(styles)) {
    return(styles[[base_name]])
  }

  # Fallback 2: strip "Highlight" suffix and try the base style
  highlight_base <- sub("Highlight$", "", name)
  if (highlight_base != name && highlight_base %in% names(styles)) {
    return(styles[[highlight_base]])
  }

  # Fallback 3: strip both "HighlightBottom" and try
  compound_base <- sub("HighlightBottom$", "", name)
  if (compound_base != name && compound_base %in% names(styles)) {
    return(styles[[compound_base]])
  }

  # Ultimate fallback — return Default
  if ("Default" %in% names(styles)) {
    return(styles[["Default"]])
  }

  # If even Default is missing, return a bare style
  return(openxlsx::createStyle())
}


# =============================================================================
# determine_cell_style
# =============================================================================
#' Determine the appropriate style name for a data cell.
#'
#' Replaces the style-selection logic in SAS \code{%wsdata(ds)} (lines 596-689
#' of xml_output.sas). Examines column name, value, numeric flag, row position,
#' and optional highlight column to choose the correct style ID.
#'
#' Style selection logic (mirrors SAS):
#' \enumerate{
#'   \item Base style starts as "Data"
#'   \item If numeric and column name contains "pct" -> "DataDec1"
#'   \item If numeric and column name is rd/rr/ort/fd -> "DataDec1"
#'   \item If numeric and column name starts with rd/rr/or, or is p_value -> "DataDec2"
#'   \item If numeric and value > 1e6 -> "DataSN" (overrides decimal format)
#'   \item If numeric and value is NA -> "DataRight" (display as ".")
#'   \item If sort_col matches column -> append "Highlight"
#'   \item If last row -> append "Bottom"
#' }
#'
#' @param col_name    Character. The column/variable name (lowercase).
#' @param value       The cell value (numeric or character).
#' @param is_numeric  Logical. Whether the column is numeric.
#' @param is_last_row Logical. Whether this is the last row of data.
#' @param sort_col    Character or NULL. The name of the highlight/sort column.
#' @param styles      A named list of style objects from
#'                    \code{create_workbook_styles()}.
#'
#' @return An openxlsx style object for the cell, or NULL if styles are not
#'         provided (in which case the caller should use the style name).
#'
#' @examples
#' styles <- create_workbook_styles()
#' s <- determine_cell_style("pct_ae", 45.2, TRUE, FALSE, NULL, styles)
determine_cell_style <- function(col_name, value, is_numeric, is_last_row,
                                 sort_col = NULL, styles = NULL) {
  # Build style name string using SAS %wsdata logic
  style_name <- "Data"

  if (isTRUE(is_numeric)) {
    # Check column name patterns for decimal formatting
    col_lower <- tolower(col_name)

    if (grepl("pct", col_lower, fixed = TRUE)) {
      style_name <- "DataDec1"
    } else if (col_lower %in% c("rd", "rr", "ort", "fd")) {
      style_name <- "DataDec1"
    } else if (grepl("^(rd|rr|or)", col_lower) || col_lower == "p_value") {
      style_name <- "DataDec2"
    }

    # Scientific notation for extremely large numbers (> 10^6)
    if (!is.na(value) && is.numeric(value) && value > 1e6) {
      style_name <- "DataSN"
    }

    # Missing numeric: SAS displays "." and uses DataRight style
    if (is.na(value)) {
      style_name <- "DataRight"
    }
  }

  # Highlight column for sorted data (SAS: &fmt.=Y branch)
  if (!is.null(sort_col) && is.character(sort_col) && length(sort_col) == 1L) {
    if (tolower(col_name) == tolower(sort_col)) {
      style_name <- paste0(style_name, "Highlight")
    }
  }

  # Bottom row adds "Bottom" suffix for bottom-border styling
  if (isTRUE(is_last_row)) {
    style_name <- paste0(style_name, "Bottom")
  }

  # If styles are provided, return the actual style object
  if (!is.null(styles) && is.list(styles)) {
    return(get_style_by_name(styles, style_name))
  }

  # Otherwise return just the style name string
  return(style_name)
}


# =============================================================================
# write_header_rows
# =============================================================================
#' Write header, subtitle, and footnote rows to a worksheet.
#'
#' Replaces SAS \code{%wsheader(dsin, dsout)} macro (lines 565-592 of
#' xml_output.sas). The SAS macro reads a dataset with \code{group} and
#' \code{data} columns and emits styled XML rows. This function writes directly
#' to an openxlsx worksheet.
#'
#' Row styling follows SAS logic:
#' \itemize{
#'   \item group == "title"    -> Header style (12pt bold italic)
#'   \item group == "subtitle" -> SubHeader style (10pt bold)
#'   \item other               -> Default style
#' }
#'
#' A blank separator row is inserted before each new group and after the last
#' row, matching SAS behavior (\code{first.group -> <Row/>; eof -> <Row/>}).
#'
#' @param wb          An openxlsx workbook object.
#' @param sheet       Character or integer. The worksheet name or index.
#' @param header_data A data.frame with columns \code{group} (character:
#'                    "title", "subtitle", or other) and \code{data} (character:
#'                    the text content). Rows are written in order.
#' @param start_row   Integer. The starting row number in the worksheet.
#'                    Defaults to 1.
#' @param styles      A named list of style objects. If NULL, calls
#'                    \code{create_workbook_styles()} internally.
#'
#' @return Integer. The next available row after the written headers (invisible).
#'
#' @examples
#' wb <- create_workbook("Report")
#' openxlsx::addWorksheet(wb, "Sheet1")
#' hdr <- data.frame(
#'   group = c("title", "subtitle", "default"),
#'   data  = c("Main Title", "Sub Title", "Footnote text"),
#'   stringsAsFactors = FALSE
#' )
#' next_row <- write_header_rows(wb, "Sheet1", hdr)
write_header_rows <- function(wb, sheet, header_data, start_row = 1,
                              styles = NULL) {
  # Validate inputs
  if (is.null(wb)) {
    stop("'wb' must be a valid openxlsx workbook object.", call. = FALSE)
  }
  if (!is.data.frame(header_data)) {
    stop("'header_data' must be a data.frame with 'group' and 'data' columns.",
         call. = FALSE)
  }
  required_cols <- c("group", "data")
  missing_cols <- setdiff(required_cols, colnames(header_data))
  if (length(missing_cols) > 0L) {
    stop(
      paste0("'header_data' is missing required columns: ",
             paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Ensure styles are available
  if (is.null(styles)) {
    styles <- create_workbook_styles()
  }

  current_row <- as.integer(start_row)
  n_rows <- nrow(header_data)

  if (n_rows == 0L) {
    return(invisible(current_row))
  }

  # Convert group to character to handle factors
  groups <- as.character(header_data$group)
  texts  <- as.character(header_data$data)

  prev_group <- ""

  for (i in seq_len(n_rows)) {
    grp  <- groups[i]
    txt  <- texts[i]

    # Insert blank separator row at the start of each new group
    # SAS: if first.group then do; string = '<Row/>'; output; end;
    if (grp != prev_group) {
      current_row <- current_row + 1L
      prev_group  <- grp
    }

    # Select style based on group value
    if (tolower(grp) == "title") {
      cell_style <- get_style_by_name(styles, "Header")
    } else if (tolower(grp) == "subtitle") {
      cell_style <- get_style_by_name(styles, "SubHeader")
    } else {
      cell_style <- get_style_by_name(styles, "Default")
    }

    # Write text and apply style
    openxlsx::writeData(wb, sheet, x = txt, startRow = current_row,
                        startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = cell_style,
                       rows = current_row, cols = 1)

    current_row <- current_row + 1L
  }

  # Insert blank separator row after the last header row (SAS: eof -> <Row/>)
  current_row <- current_row + 1L

  return(invisible(current_row))
}


# =============================================================================
# write_formatted_data
# =============================================================================
#' Write a data frame to a worksheet with cell-level formatting.
#'
#' Replaces the combined SAS \code{%annotate(dsin, dsout)} and
#' \code{%markup(dsin, dsout)} macros (lines 713-785 of xml_output.sas).
#'
#' In SAS, \code{%annotate} decomposed each observation into row/column/type
#' metadata, and \code{%markup} converted that metadata into XML strings.
#' In R, this function writes data directly to the worksheet using
#' \code{openxlsx::writeData()} and applies cell-level styles using
#' \code{openxlsx::addStyle()}.
#'
#' Key behaviors preserved from SAS:
#' \itemize{
#'   \item Cell merging via \code{merge_info} parameter
#'   \item Cell-level style control via \code{style_map} matrix/data.frame
#'   \item Missing numeric values displayed as "." (SAS convention)
#'   \item String vs Number type detection per column
#' }
#'
#' @param wb         An openxlsx workbook object.
#' @param sheet      Character or integer. The worksheet name or index.
#' @param data       A data.frame or tibble to write.
#' @param start_row  Integer. First row to write data. Defaults to 1.
#' @param start_col  Integer. First column to write data. Defaults to 1.
#' @param styles     A named list of style objects from
#'                   \code{create_workbook_styles()}. If NULL, creates default
#'                   styles internally.
#' @param style_map  A data.frame or matrix with the same dimensions as
#'                   \code{data}, containing style names (character strings) for
#'                   each cell. If NULL, the Default style is applied uniformly.
#' @param merge_info A data.frame specifying cells to merge, with columns:
#'                   \code{row} (integer), \code{col} (integer),
#'                   \code{merge_across} (integer, number of columns to merge),
#'                   \code{merge_down} (integer, number of rows to merge).
#'                   If NULL, no merging is performed.
#'
#' @return Integer. The next available row after the written data (invisible).
#'
#' @examples
#' wb <- create_workbook("Demo")
#' openxlsx::addWorksheet(wb, "Data")
#' df <- data.frame(label = c("A", "B"), count = c(10, 20))
#' styles <- create_workbook_styles()
#' write_formatted_data(wb, "Data", df, styles = styles)
write_formatted_data <- function(wb, sheet, data, start_row = 1, start_col = 1,
                                 styles = NULL, style_map = NULL,
                                 merge_info = NULL) {
  # Validate inputs
  if (is.null(wb)) {
    stop("'wb' must be a valid openxlsx workbook object.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  if (n_rows == 0L || n_cols == 0L) {
    return(invisible(as.integer(start_row)))
  }

  # Ensure styles are available
  if (is.null(styles)) {
    styles <- create_workbook_styles()
  }

  # Prepare a display copy of data where NA numerics become "."
  # SAS convention: missing numeric values display as "."
  display_data <- data

  for (j in seq_len(n_cols)) {
    col_vals <- data[[j]]
    if (is.numeric(col_vals)) {
      na_mask <- is.na(col_vals)
      if (any(na_mask)) {
        # Convert entire column to character so NA can become "."
        display_data[[j]] <- ifelse(na_mask, ".", as.character(col_vals))
      }
    }
  }

  # Write data to worksheet
  openxlsx::writeData(wb, sheet, x = display_data,
                      startRow = as.integer(start_row),
                      startCol = as.integer(start_col),
                      colNames = FALSE)

  # Apply cell-level styles
  if (!is.null(style_map)) {
    # Validate style_map dimensions
    if (is.data.frame(style_map) || is.matrix(style_map)) {
      if (nrow(style_map) != n_rows || ncol(style_map) != n_cols) {
        stop("'style_map' dimensions must match 'data' dimensions.", call. = FALSE)
      }
    }

    for (i in seq_len(n_rows)) {
      for (j in seq_len(n_cols)) {
        style_name <- if (is.data.frame(style_map)) {
          as.character(style_map[i, j])
        } else {
          style_map[i, j]
        }

        if (!is.na(style_name) && nchar(style_name) > 0L) {
          cell_style <- get_style_by_name(styles, style_name)
          openxlsx::addStyle(
            wb, sheet, style = cell_style,
            rows = as.integer(start_row) + i - 1L,
            cols = as.integer(start_col) + j - 1L
          )
        }
      }
    }
  } else {
    # Apply Default style to all cells if no style_map provided
    default_style <- get_style_by_name(styles, "Default")
    for (i in seq_len(n_rows)) {
      for (j in seq_len(n_cols)) {
        openxlsx::addStyle(
          wb, sheet, style = default_style,
          rows = as.integer(start_row) + i - 1L,
          cols = as.integer(start_col) + j - 1L
        )
      }
    }
  }

  # Apply cell merging if requested
  # SAS: MergeAcross and MergeDown XML attributes
  if (!is.null(merge_info) && is.data.frame(merge_info) && nrow(merge_info) > 0L) {
    for (k in seq_len(nrow(merge_info))) {
      m_row   <- merge_info$row[k]
      m_col   <- merge_info$col[k]
      m_across <- if ("merge_across" %in% names(merge_info)) {
        merge_info$merge_across[k]
      } else {
        0L
      }
      m_down <- if ("merge_down" %in% names(merge_info)) {
        merge_info$merge_down[k]
      } else {
        0L
      }

      abs_row <- as.integer(start_row) + m_row - 1L
      abs_col <- as.integer(start_col) + m_col - 1L

      if (m_across > 0L) {
        openxlsx::mergeCells(
          wb, sheet,
          cols = abs_col:(abs_col + m_across),
          rows = abs_row
        )
      }
      if (m_down > 0L) {
        openxlsx::mergeCells(
          wb, sheet,
          cols = abs_col,
          rows = abs_row:(abs_row + m_down)
        )
      }
    }
  }

  next_row <- as.integer(start_row) + n_rows
  return(invisible(next_row))
}


# =============================================================================
# write_data_table
# =============================================================================
#' Write a formatted data table to a worksheet with automatic style detection.
#'
#' Replaces SAS \code{%wsdata(ds)} macro (lines 596-689 of xml_output.sas).
#' This is the primary function for writing clinical data tables to Excel.
#' It automatically determines the appropriate style for each cell based on
#' column name patterns, data type, and row position.
#'
#' The function:
#' \enumerate{
#'   \item Optionally writes data header rows (DataHeader style with merge)
#'   \item Writes each data row with column-type-aware styles
#'   \item Applies bottom-border styles to the last row
#'   \item Handles missing numeric values as "." (SAS convention)
#'   \item Supports sort/highlight column marking
#' }
#'
#' @param wb          An openxlsx workbook object.
#' @param sheet       Character or integer. The worksheet name or index.
#' @param data        A data.frame or tibble containing the data to write.
#' @param start_row   Integer. The first row to begin writing. Defaults to 1.
#' @param styles      A named list of style objects. If NULL, creates defaults.
#' @param sort_col    Character or NULL. Column name to highlight (applies
#'                    Highlight style variant). Corresponds to SAS
#'                    \code{&fmt.=Y} and \code{&sort.} macro variables.
#' @param header_rows A data.frame with a single column identifying rows that
#'                    should be rendered as DataHeader spans (merged across all
#'                    columns). Each row value is 0 (normal) or 1 (header).
#'                    Must have the same number of rows as \code{data}. If NULL,
#'                    all rows are treated as normal data.
#'
#' @return Integer. The next available row after the written table (invisible).
#'
#' @examples
#' wb <- create_workbook("AE Summary")
#' openxlsx::addWorksheet(wb, "AE")
#' ae_data <- data.frame(
#'   term   = c("Headache", "Nausea"),
#'   n      = c(15, 8),
#'   pct_ae = c(30.0, 16.0)
#' )
#' styles <- create_workbook_styles()
#' write_data_table(wb, "AE", ae_data, styles = styles)
write_data_table <- function(wb, sheet, data, start_row = 1, styles = NULL,
                             sort_col = NULL, header_rows = NULL) {
  # Validate inputs
  if (is.null(wb)) {
    stop("'wb' must be a valid openxlsx workbook object.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  if (n_rows == 0L || n_cols == 0L) {
    return(invisible(as.integer(start_row)))
  }

  # Ensure styles are available
  if (is.null(styles)) {
    styles <- create_workbook_styles()
  }

  # Validate header_rows if provided
  if (!is.null(header_rows)) {
    if (is.data.frame(header_rows)) {
      hdr_flags <- header_rows[[1]]
    } else if (is.vector(header_rows)) {
      hdr_flags <- header_rows
    } else {
      stop("'header_rows' must be a vector or single-column data.frame.",
           call. = FALSE)
    }
    if (length(hdr_flags) != n_rows) {
      stop("'header_rows' length must equal the number of rows in 'data'.",
           call. = FALSE)
    }
  } else {
    hdr_flags <- rep(0L, n_rows)
  }

  # Pre-compute column metadata
  col_names  <- colnames(data)
  col_is_num <- vapply(data, is.numeric, logical(1L))

  current_row <- as.integer(start_row)

  for (i in seq_len(n_rows)) {
    is_last <- (i == n_rows)

    # --- DataHeader row: merge across all columns with bold gray style ---
    # SAS: if header = 1 then do;
    #        <Cell MergeAcross="nvars-1" StyleID="DataHeader">...
    if (hdr_flags[i] == 1L) {
      # Write the first column value as the header label
      header_text <- as.character(data[i, 1])
      openxlsx::writeData(wb, sheet, x = header_text,
                          startRow = current_row, startCol = 1,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet,
                         style = get_style_by_name(styles, "DataHeader"),
                         rows = current_row, cols = 1)

      # Merge across all columns
      if (n_cols > 1L) {
        openxlsx::mergeCells(wb, sheet,
                             cols = 1:n_cols,
                             rows = current_row)
      }

      # Set row height to 18 (matching SAS: ss:Height="18")
      openxlsx::setRowHeights(wb, sheet,
                              rows = current_row,
                              heights = 18)

      current_row <- current_row + 1L
      next
    }

    # --- Normal data row ---
    for (j in seq_len(n_cols)) {
      cell_val    <- data[i, j]
      col_nm      <- col_names[j]
      is_num      <- col_is_num[j]
      display_val <- cell_val

      # Handle missing numeric: display "." per SAS convention
      if (is_num && is.na(cell_val)) {
        display_val <- "."
      }

      # Write cell value
      openxlsx::writeData(wb, sheet, x = display_val,
                          startRow = current_row,
                          startCol = j,
                          colNames = FALSE)

      # Determine and apply style
      cell_style <- determine_cell_style(
        col_name    = col_nm,
        value       = cell_val,
        is_numeric  = is_num,
        is_last_row = is_last,
        sort_col    = sort_col,
        styles      = styles
      )

      openxlsx::addStyle(wb, sheet, style = cell_style,
                         rows = current_row, cols = j)
    }

    current_row <- current_row + 1L
  }

  return(invisible(current_row))
}


# =============================================================================
# apply_page_setup
# =============================================================================
#' Configure page layout, headers, and footers for a worksheet.
#'
#' Replaces SAS ODS page setup and the page configuration aspects of the
#' SpreadsheetML \code{<WorksheetOptions>} block. Configures print orientation,
#' fit-to-page scaling, and header/footer strings for the worksheet.
#'
#' @param wb             An openxlsx workbook object.
#' @param sheet          Character or integer. The worksheet name or index.
#' @param orientation    Character. Page orientation: "landscape" or "portrait".
#'                       Defaults to "landscape" (matching SAS default for
#'                       clinical reports).
#' @param header_left    Character. Left-aligned header text. Defaults to "".
#' @param header_right   Character. Right-aligned header text. Defaults to "".
#' @param footer_center  Character. Centered footer text. Use \code{"&P"} for
#'                       page number and \code{"&N"} for total pages. Defaults
#'                       to "Page &P of &N".
#' @param fit_to_width   Logical. Whether to fit printout to page width.
#'                       Defaults to TRUE.
#' @param fit_to_height  Integer. Maximum number of pages for height fitting.
#'                       Defaults to 100 (effectively unlimited vertical pages).
#' @param scale          Integer. Print scale percentage (1-400). Defaults to
#'                       78, matching the SAS SpreadsheetML default scaling.
#'
#' @return The workbook object (invisible), for method chaining.
#'
#' @examples
#' wb <- create_workbook("Report")
#' openxlsx::addWorksheet(wb, "Sheet1")
#' apply_page_setup(wb, "Sheet1", orientation = "landscape")
apply_page_setup <- function(wb, sheet, orientation = "landscape",
                             header_left = "", header_right = "",
                             footer_center = "Page &P of &N",
                             fit_to_width = TRUE, fit_to_height = 100,
                             scale = 78) {
  # Validate inputs
  if (is.null(wb)) {
    stop("'wb' must be a valid openxlsx workbook object.", call. = FALSE)
  }
  if (!orientation %in% c("landscape", "portrait")) {
    stop("'orientation' must be 'landscape' or 'portrait'.", call. = FALSE)
  }

  # Apply page setup: orientation, fit-to-page, and scale
  # SAS SpreadsheetML: <PageSetup>, <FitToPage/>, <Layout Orientation="..."/>
  openxlsx::pageSetup(
    wb, sheet,
    orientation = orientation,
    fitToWidth  = fit_to_width,
    fitToHeight = fit_to_height,
    scale       = scale
  )

  # Apply header and footer
  # SAS SpreadsheetML: <Header>, <Footer> elements in <PageSetup>
  openxlsx::setHeaderFooter(
    wb, sheet,
    header = c(header_left, NA, header_right),
    footer = c(NA, footer_center, NA)
  )

  return(invisible(wb))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SpreadsheetML XML generation fully replaced by openxlsx package
#    - Style gallery names preserved for backward compatibility with callers
#    - SAS %strlen global (buffer size) has no R equivalent - not needed
#    - Bottom-row style variants created by adding bottom border to base style
#    - Missing numeric values displayed as "." string per SAS convention
#    - SAS %xml_tag_def/%xml_init variable declarations have no R equivalent
#      and are not needed — R variables are declared implicitly
#    - SAS %xml_style_dcl/%xml_style_markup replaced by create_workbook_styles()
#      which creates all styles up front rather than via a dataset pipeline
#    - SAS %ws_rowcount (row counting utility) is not needed in R because
#      openxlsx tracks row positions internally
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Excel column widths: SAS SpreadsheetML uses pixel-based widths while
#      openxlsx uses character-unit widths; callers may need to adjust
#    - Font rendering may differ between SpreadsheetML (.xml) and OOXML (.xlsx)
#    - Border rendering may differ slightly between Excel XML and OOXML
#    - Row heights: SAS Height="18" converted to openxlsx setRowHeights(18)
#      but rendering may vary between formats
# NO DIRECT R EQUIVALENT:
#    - SAS %annotate/%markup XML pipeline -> openxlsx writeData/addStyle
#    - SAS FILE/PUT text streaming -> openxlsx::saveWorkbook()
#    - SAS %xml_tag_def/%xml_init variable declarations -> not needed in R
#    - SAS %xml_style_dcl/%xml_style_markup -> openxlsx::createStyle()
#    - SAS %ws_rowcount (XML row counting) -> not needed; openxlsx tracks rows
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: AAP mandated replacement for SpreadsheetML XML generation
#    - Produces modern OOXML (.xlsx) format instead of legacy SpreadsheetML
#    - No Java dependency (unlike xlsx package)
#    - Mature, actively maintained CRAN package with comprehensive API
# OPEN QUESTIONS:
#    - Column width conversion factor: SAS pixels to openxlsx character units
#      may require calibration per font and display settings
#    - Exact color matching: verify hex codes render identically across
#      SpreadsheetML and OOXML formats
#    - Cell comment support: openxlsx::writeComment() available but not
#      fully exercised in this migration (SAS XML Comment tag rarely used)
#    - Named cell ranges: openxlsx::createNamedRegion() available but
#      SAS NamedCell usage is limited to specific panels
# ============================================================
