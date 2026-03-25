#' ============================================================================
#' PROGRAM NAME: Excel Output Utilities (xml_output.R)
#'
#' DESCRIPTION:  Reusable openxlsx workbook creation utilities for generating
#'               Excel workbook output. Migrated from SAS SpreadsheetML XML
#'               generation macros to idiomatic R using the openxlsx package.
#'
#'               Functions provided:
#'                 create_workbook   -- create an openxlsx workbook object
#'                 create_workbook_styles     -- define ~66 named cell styles
#'                 annotate_data     -- convert data frame to long format
#'                 write_annotated_data -- write annotated data to worksheet
#'                 write_header      -- write title/subtitle header rows
#'                 write_data_table  -- write formatted data table
#'                 count_rows        -- count rows in a data frame
#'                 apply_style       -- convenience wrapper for addStyle
#'                 create_dynamic_styles -- build styles from specification tibble
#'
#' ORIGINAL SAS: contributed/Demographics/Utility Programs/xml_output.sas
#'               (896 lines, 11 macros)
#'
#' SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
#' SAS DATE:     February 15, 2011
#'
#' R MIGRATION:  Blitzy Platform automated migration
#'
#' NOTES:        Included by err_output.R and sl_gs_output.R via source().
#'               In the original SAS, this was included via:
#'                 %include "&utilpath.\xml_output.sas";
#' ============================================================================

# --- Library Loading ---------------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)


# =============================================================================
# create_workbook() — Replaces SAS %wb macro (lines 41-66)
# =============================================================================
#' Create an openxlsx Workbook Object
#'
#' Initializes a new openxlsx workbook with document properties matching the
#' SAS SpreadsheetML workbook skeleton (title, author, creation date/time).
#'
#' @param title Character string for the workbook title (DocumentProperties/Title)
#' @param author Character string for the workbook author
#'   (default: "US Food & Drug Administration")
#' @param sheet Optional character string. If provided, an initial worksheet
#'   with this name is added to the workbook (default: NULL — no sheet added).
#' @return An openxlsx Workbook object
create_workbook <- function(title, author = "US Food & Drug Administration",
                            sheet = NULL) {
  # Validate input
  if (missing(title) || is.null(title) || !is.character(title)) {
    stop("'title' must be a non-NULL character string", call. = FALSE)
  }

  # Create workbook with document properties
  # SAS equivalent: XML DocumentProperties with Title, Author, Created date/time
  wb <- openxlsx::createWorkbook(creator = author, title = title)

  # Set the base font to match SAS default workbook font
  # SAS Normal/Default style uses the panel-specified font size (typically 9pt)
  openxlsx::modifyBaseFont(wb, fontSize = 9, fontName = "Calibri")

  # Optionally add an initial worksheet
  if (!is.null(sheet) && is.character(sheet) && nchar(sheet) > 0) {
    openxlsx::addWorksheet(wb, sheetName = sheet)
  }

  return(wb)
}


# =============================================================================
# create_workbook_styles() — Replaces SAS %styles macro (lines 72-558)
# =============================================================================
#' Create the Complete Style Gallery
#'
#' Builds a named list of ~66 openxlsx style objects that replicate every
#' SpreadsheetML style defined in the SAS %styles macro. Style inheritance
#' (SAS ss:Parent) is flattened into explicit property specification since
#' openxlsx styles are self-contained.
#'
#' @param size Integer font size for the default style (default: 9).
#'   SAS equivalent: %styles(size=9)
#' @return Named list of openxlsx Style objects
create_workbook_styles <- function(size = 9) {

  styles <- list()

  # -----------------------------------------------------------------------
  # Standard Styles (SAS lines 78-137)
  # -----------------------------------------------------------------------

  # Default: Font size `size`, solid interior (SAS ss:Name="Normal")
  styles[["Default"]] <- openxlsx::createStyle(fontSize = size)

  # DefaultLeft: Left-aligned, top-aligned, no wrap
  styles[["DefaultLeft"]] <- openxlsx::createStyle(
    fontSize = size, halign = "left", valign = "top", wrapText = FALSE
  )

  # DefaultRight: Right-aligned, top-aligned, no wrap
  styles[["DefaultRight"]] <- openxlsx::createStyle(
    fontSize = size, halign = "right", valign = "top", wrapText = FALSE
  )

  # DefaultWhite: White font color
  styles[["DefaultWhite"]] <- openxlsx::createStyle(
    fontSize = size, fontColour = "#FFFFFF"
  )

  # Default10: Font size 10, top-aligned
  styles[["Default10"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = FALSE
  )

  # Default10Wrap: Font size 10, top-aligned, word wrap
  styles[["Default10Wrap"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE
  )

  # Default10RedWrap: Font size 10, red italic, word wrap
  styles[["Default10RedWrap"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FF0000", textDecoration = "italic",
    valign = "top", wrapText = TRUE
  )

  # Default10Right: Font size 10, right-aligned, top-aligned
  styles[["Default10Right"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "right", valign = "top", wrapText = FALSE
  )

  # Default8: Font size 8, top-aligned
  styles[["Default8"]] <- openxlsx::createStyle(
    fontSize = 8, valign = "top", wrapText = FALSE
  )

  # Header: Font size 12, bold, italic
  styles[["Header"]] <- openxlsx::createStyle(
    fontSize = 12, textDecoration = c("bold", "italic")
  )

  # SubHeader: Font size 10, bold, top-aligned
  styles[["SubHeader"]] <- openxlsx::createStyle(
    fontSize = 10, textDecoration = "bold", valign = "top"
  )

  # -----------------------------------------------------------------------
  # Column Header Styles (SAS lines 139-174)
  # -----------------------------------------------------------------------

  # Column: Center-aligned, #333399 bg, white bold font 10, word wrap
  styles[["Column"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = "bold"
  )

  # ColumnOutline: Column + all borders (thin continuous)
  styles[["ColumnOutline"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = "bold",
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # ColumnOutlineSmall: ColumnOutline + font size 8
  styles[["ColumnOutlineSmall"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = "bold",
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # ColumnOutlineItalic: ColumnOutline + italic
  styles[["ColumnOutlineItalic"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = c("bold", "italic"),
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # ColumnOutlineRotateTop: ColumnOutline + 90-degree rotation, top-aligned

  styles[["ColumnOutlineRotateTop"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "top", wrapText = TRUE,
    textDecoration = "bold", textRotation = 90,
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # ColumnOutlineRotateCtr: ColumnOutline + 90-degree rotation, center-aligned
  styles[["ColumnOutlineRotateCtr"]] <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = "bold", textRotation = 90,
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # -----------------------------------------------------------------------
  # Data Styles (SAS lines 176-228)
  # -----------------------------------------------------------------------

  # DataHeader: Left-aligned, center vertical, gray bg, all borders, bold 10
  styles[["DataHeader"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "left", valign = "center", wrapText = FALSE,
    fgFill = "#C0C0C0", textDecoration = "bold",
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # Table: Center-aligned, center vertical, all borders, font 10
  styles[["Table"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "center", valign = "center", wrapText = FALSE,
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # Data: Top-aligned, left+right borders only (no explicit font — inherits size)
  styles[["Data"]] <- openxlsx::createStyle(
    valign = "top", wrapText = FALSE,
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataWrap: Data + word wrap
  styles[["DataWrap"]] <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE,
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataRight: Data + right-aligned
  styles[["DataRight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataRightHighlight: DataRight + #CCCCFF background
  styles[["DataRightHighlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataCenter: Data + center-aligned
  styles[["DataCenter"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # -----------------------------------------------------------------------
  # Number Format Styles (SAS lines 230-303)
  # -----------------------------------------------------------------------

  # DataDec0: Right-aligned, format "0" (zero decimals)
  styles[["DataDec0"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec0Center: Centered, format "0"
  styles[["DataDec0Center"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec0Highlight: DataDec0 + #CCCCFF background
  styles[["DataDec0Highlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0",
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec1: Right-aligned, format "0.0" (one decimal)
  styles[["DataDec1"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.0",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec1Center: Centered, format "0.0"
  styles[["DataDec1Center"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.0",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec1Highlight: DataDec1 + #CCCCFF background
  styles[["DataDec1Highlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.0",
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec2: Right-aligned, format "0.00" (two decimals)
  styles[["DataDec2"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec2Center: Centered, format "0.00"
  styles[["DataDec2Center"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.00",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataDec2Highlight: DataDec2 + #CCCCFF background
  styles[["DataDec2Highlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00",
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataSN: Right-aligned, format "0.00E+00" (scientific notation)
  styles[["DataSN"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00E+00",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataSNCenter: Centered, format "0.00E+00"
  styles[["DataSNCenter"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.00E+00",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataSNHighlight: DataSN + #CCCCFF background
  styles[["DataSNHighlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00E+00",
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataPct: Right-aligned, format "0%"
  styles[["DataPct"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0%",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # DataPctHighlight: DataPct + #CCCCFF background
  styles[["DataPctHighlight"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0%",
    fgFill = "#CCCCFF",
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # -----------------------------------------------------------------------
  # Bottom Row Variants (SAS lines 305-474) — add bottom border
  # All Bottom variants have Bottom+Left+Right borders
  # -----------------------------------------------------------------------

  bottom_borders <- c("Bottom", "Left", "Right")

  # DataBottom
  styles[["DataBottom"]] <- openxlsx::createStyle(
    valign = "top", wrapText = FALSE,
    border = bottom_borders, borderStyle = "thin"
  )

  # DataWrapBottom
  styles[["DataWrapBottom"]] <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE,
    border = bottom_borders, borderStyle = "thin"
  )

  # DataRightBottom
  styles[["DataRightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    border = bottom_borders, borderStyle = "thin"
  )

  # DataRightHighlightBottom
  styles[["DataRightHighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataCenterBottom
  styles[["DataCenterBottom"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec0Bottom
  styles[["DataDec0Bottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec0CenterBottom
  styles[["DataDec0CenterBottom"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec0HighlightBottom
  styles[["DataDec0HighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0",
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec1Bottom
  styles[["DataDec1Bottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.0",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec1CenterBottom
  styles[["DataDec1CenterBottom"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.0",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec1HighlightBottom
  styles[["DataDec1HighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.0",
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec2Bottom
  styles[["DataDec2Bottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec2CenterBottom
  styles[["DataDec2CenterBottom"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.00",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataDec2HighlightBottom
  styles[["DataDec2HighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00",
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataSNBottom
  styles[["DataSNBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00E+00",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataSNCenterBottom
  styles[["DataSNCenterBottom"]] <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE, numFmt = "0.00E+00",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataSNHighlightBottom
  styles[["DataSNHighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0.00E+00",
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataPctBottom
  styles[["DataPctBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0%",
    border = bottom_borders, borderStyle = "thin"
  )

  # DataPctHighlightBottom
  styles[["DataPctHighlightBottom"]] <- openxlsx::createStyle(
    halign = "right", valign = "top", numFmt = "0%",
    fgFill = "#CCCCFF",
    border = bottom_borders, borderStyle = "thin"
  )

  # -----------------------------------------------------------------------
  # Special Styles for AE MedDRA Report Cover Page (SAS lines 476-499)
  # These inherit from DataCenterBottom or DataDec1Bottom
  # -----------------------------------------------------------------------

  # Gray: DataCenterBottom + gray bg/font, font size 8
  styles[["Gray"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#808080",
    halign = "center", valign = "top", wrapText = FALSE,
    fgFill = "#808080",
    border = bottom_borders, borderStyle = "thin"
  )

  # Red: DataCenterBottom + red bg/font, font size 8
  styles[["Red"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FF0000",
    halign = "center", valign = "top", wrapText = FALSE,
    fgFill = "#FF0000",
    border = bottom_borders, borderStyle = "thin"
  )

  # Peach: DataCenterBottom + peach bg/font, font size 8
  styles[["Peach"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FFCC99",
    halign = "center", valign = "top", wrapText = FALSE,
    fgFill = "#FFCC99",
    border = bottom_borders, borderStyle = "thin"
  )

  # BoldRedText: DataDec1Bottom + bold red font size 8, numFmt "0.0"
  styles[["BoldRedText"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FF0000", textDecoration = "bold",
    halign = "right", valign = "top", numFmt = "0.0",
    border = bottom_borders, borderStyle = "thin"
  )

  # -----------------------------------------------------------------------
  # Grouping/Subsetting Styles (SAS lines 501-552)
  # -----------------------------------------------------------------------

  # GS_BTLRB: Top+Left+Right+Bottom borders, font 10, word wrap, top-aligned
  styles[["GS_BTLRB"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE,
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # GSC_BTLRB: Same as GS_BTLRB + center-aligned
  styles[["GSC_BTLRB"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "center", valign = "top", wrapText = TRUE,
    border = c("Top", "Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  # GS_BTLR: Top+Left+Right borders only (no bottom)
  styles[["GS_BTLR"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE,
    border = c("Top", "Left", "Right"),
    borderStyle = "thin"
  )

  # GS_BLR: Left+Right borders only
  styles[["GS_BLR"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE,
    border = c("Left", "Right"),
    borderStyle = "thin"
  )

  # GS_BLRB: Left+Right+Bottom borders
  styles[["GS_BLRB"]] <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE,
    border = c("Bottom", "Left", "Right"),
    borderStyle = "thin"
  )

  return(styles)
}


# =============================================================================
# annotate_data() — Replaces SAS %annotate macro (lines 713-742)
# NOTE: Intentional divergence from tested/R/utilities/xml_output.R which uses
# get_style_by_name() for style lookups. annotate_data() serves a different
# purpose: converting wide-format data to cell-level long format for Excel writing.
# These are not counterpart functions — they address distinct SAS macro translations.
# =============================================================================
#' Annotate a Data Frame for Cell-Level Excel Writing
#'
#' Converts a wide-format data frame into a long-format tibble with one row per
#' cell, suitable for detailed cell-level formatting and writing to Excel.
#' Each variable becomes a separate row with Row number, column name (varname),
#' Data value, Type indicator (String/Number), and a bottom flag for the last
#' row. Replaces the SAS %annotate macro which iterates through columns using
#' %sysfunc(attrn) to build the row-per-variable layout.
#'
#' @param df A data frame to annotate
#' @return A tibble with columns: Row, varname, Data, Type, bottom
annotate_data <- function(df) {
  # Validate input
  if (is.null(df) || !is.data.frame(df)) {
    stop("'df' must be a non-NULL data frame", call. = FALSE)
  }
  if (nrow(df) == 0L) {
    return(
      tibble::tibble(
        Row = integer(0), varname = character(0), Data = character(0),
        Type = character(0), bottom = logical(0)
      )
    )
  }

  # Store original column types for type detection
  col_types <- vapply(df, is.numeric, logical(1))

  # Convert all columns to character for uniform pivot, preserving original values
  df_char <- df
  for (col_name in names(df)) {
    df_char[[col_name]] <- as.character(df[[col_name]])
  }

  # Add Row number and bottom flag
  df_annotated <- df_char %>%
    dplyr::mutate(
      Row = dplyr::row_number(),
      bottom = (dplyr::row_number() == dplyr::n())
    )

  # Pivot to long format: one row per variable per observation
  result <- df_annotated %>%
    tidyr::pivot_longer(
      cols = -c(Row, bottom),
      names_to = "varname",
      values_to = "Data"
    ) %>%
    dplyr::mutate(
      # Determine type based on original column type
      Type = dplyr::if_else(
        col_types[varname],
        "Number",
        "String"
      ),
      # Lowercase varname to match SAS behavior
      varname = tolower(varname)
    ) %>%
    dplyr::select(Row, varname, Data, Type, bottom)

  return(result)
}


# =============================================================================
# write_annotated_data() — Replaces SAS %markup macro (lines 746-785)
# =============================================================================
#' Write Annotated Data to an openxlsx Worksheet
#'
#' Takes an annotated dataset (as produced by annotate_data or with additional
#' formatting columns) and writes cell-by-cell to an openxlsx worksheet.
#' Supports StyleID, MergeAcross, MergeDown, Height, Index, and Comment
#' columns for fine-grained cell formatting.
#'
#' @param wb An openxlsx Workbook object
#' @param sheet Sheet name or index
#' @param annotated_data A tibble/data frame with columns: Row, varname, Data.
#'   Optional columns: StyleID, MergeAcross, MergeDown, Height, Index, Type.
#' @param styles Named list of openxlsx Style objects (from create_workbook_styles)
#' @param start_row Integer row offset for writing (default: 1)
#' @return The workbook object (invisibly), modified in place
write_annotated_data <- function(wb, sheet, annotated_data, styles, start_row = 1) {
  # Validate inputs
  if (is.null(wb)) stop("'wb' must be a non-NULL Workbook object", call. = FALSE)
  if (is.null(annotated_data) || nrow(annotated_data) == 0L) {
    return(invisible(wb))
  }

  # Ensure optional columns exist with defaults
  if (!"StyleID" %in% names(annotated_data)) {
    annotated_data$StyleID <- NA_character_
  }
  if (!"MergeAcross" %in% names(annotated_data)) {
    annotated_data$MergeAcross <- NA_real_
  }
  if (!"MergeDown" %in% names(annotated_data)) {
    annotated_data$MergeDown <- NA_real_
  }
  if (!"Height" %in% names(annotated_data)) {
    annotated_data$Height <- NA_real_
  }
  if (!"Index" %in% names(annotated_data)) {
    annotated_data$Index <- NA_real_
  }

  # Get unique variable names to determine column positions
  all_varnames <- unique(annotated_data$varname)

  # Process each row group
  unique_rows <- sort(unique(annotated_data$Row))

  for (row_val in unique_rows) {
    row_data <- annotated_data %>% dplyr::filter(.data$Row == row_val)
    excel_row <- start_row + row_val - 1L

    # Set row height if specified — extract non-NA heights via pull
    height_vals <- row_data %>%
      dplyr::filter(!is.na(.data$Height)) %>%
      dplyr::pull(.data$Height)
    if (length(height_vals) > 0L) {
      openxlsx::setRowHeights(wb, sheet, rows = excel_row, heights = height_vals[1])
    }

    for (i in seq_len(nrow(row_data))) {
      cell <- row_data[i, ]

      # Determine column position: use Index if provided, else variable order
      if (!is.na(cell$Index) && cell$Index > 0) {
        col_pos <- as.integer(cell$Index)
      } else {
        col_pos <- match(cell$varname, all_varnames)
        if (is.na(col_pos)) col_pos <- i
      }

      # Write cell data
      cell_value <- cell$Data
      if (!is.na(cell_value) && nchar(cell_value) > 0) {
        # Attempt numeric conversion for Number-typed cells
        if (!is.na(cell$Type) && cell$Type == "Number") {
          num_val <- suppressWarnings(as.numeric(cell_value))
          if (!is.na(num_val)) {
            openxlsx::writeData(wb, sheet, x = num_val,
                                startRow = excel_row, startCol = col_pos)
          } else {
            openxlsx::writeData(wb, sheet, x = cell_value,
                                startRow = excel_row, startCol = col_pos)
          }
        } else {
          # Replace SAS space indicator '~!' with a space
          cell_value <- gsub("~!", " ", cell_value, fixed = TRUE)
          openxlsx::writeData(wb, sheet, x = cell_value,
                              startRow = excel_row, startCol = col_pos)
        }
      }

      # Apply style if specified
      if (!is.na(cell$StyleID) && nchar(cell$StyleID) > 0) {
        style_obj <- styles[[cell$StyleID]]
        if (!is.null(style_obj)) {
          openxlsx::addStyle(wb, sheet, style = style_obj,
                             rows = excel_row, cols = col_pos, stack = TRUE)
        }
      }

      # Handle cell merges
      if (!is.na(cell$MergeAcross) && cell$MergeAcross > 0) {
        openxlsx::mergeCells(
          wb, sheet,
          cols = col_pos:(col_pos + as.integer(cell$MergeAcross)),
          rows = excel_row
        )
      }
      if (!is.na(cell$MergeDown) && cell$MergeDown > 0) {
        openxlsx::mergeCells(
          wb, sheet,
          cols = col_pos,
          rows = excel_row:(excel_row + as.integer(cell$MergeDown))
        )
      }
    }
  }

  return(invisible(wb))
}


# =============================================================================
# write_header() — Replaces SAS %wsheader macro (lines 565-592)
# =============================================================================
#' Write Title/Subtitle Header Rows to a Worksheet
#'
#' Reads a data frame with group (title/subtitle) and data columns, writes
#' each row to the worksheet with appropriate Header or SubHeader styling.
#' Inserts blank rows before the first group and after the last row, matching
#' the SAS %wsheader macro behavior.
#'
#' @param wb An openxlsx Workbook object
#' @param sheet Sheet name or index
#' @param header_data A data frame with columns: group (character), data (character).
#'   group values: "title" → Header style, "subtitle" → SubHeader style,
#'   anything else → Default style
#' @param styles Named list of openxlsx Style objects (from create_workbook_styles)
#' @param start_row Integer row to begin writing (default: 1)
#' @return Integer: the next available row after the header section
write_header <- function(wb, sheet, header_data, styles, start_row = 1) {
  # Validate inputs
  if (is.null(wb)) stop("'wb' must be a non-NULL Workbook object", call. = FALSE)
  if (is.null(header_data) || nrow(header_data) == 0L) {
    return(start_row)
  }

  current_row <- start_row
  prev_group <- ""

  for (i in seq_len(nrow(header_data))) {
    row_group <- as.character(header_data$group[i])
    row_data  <- as.character(header_data$data[i])

    # Insert blank row before first item in each group
    if (row_group != prev_group) {
      current_row <- current_row + 1L
      prev_group <- row_group
    }

    # Determine style based on group
    style_name <- dplyr::case_when(
      tolower(row_group) == "title"    ~ "Header",
      tolower(row_group) == "subtitle" ~ "SubHeader",
      TRUE                             ~ "Default"
    )

    # Write the header text
    openxlsx::writeData(wb, sheet, x = row_data,
                        startRow = current_row, startCol = 1)

    # Apply the corresponding style
    if (!is.null(styles[[style_name]])) {
      openxlsx::addStyle(wb, sheet, style = styles[[style_name]],
                         rows = current_row, cols = 1, stack = TRUE)
    }

    current_row <- current_row + 1L
  }

  # Insert trailing blank row
  current_row <- current_row + 1L

  return(current_row)
}


# =============================================================================
# write_data_table() — Replaces SAS %wsdata macro (lines 596-689)
# =============================================================================
#' Write a Data Frame as a Formatted Excel Table
#'
#' Loops through all columns in the data frame, determines data types, and
#' assigns styles based on column name patterns. Replicates the SAS %wsdata
#' macro logic for automatic style selection based on variable semantics.
#'
#' Style Assignment Rules (from SAS lines 644-667):
#' - Columns containing "pct" in name → DataDec1 style (one decimal)
#' - Columns named "rd", "rr", "ort", "fd" → DataDec1 style
#' - Columns starting with "rd", "rr", "or" OR named "p_value" → DataDec2 style
#' - Numbers > 10^6 → DataSN (scientific notation)
#' - Missing numeric → DataRight with "." displayed
#' - Last row → "Bottom" variant of each style
#' - fmt=TRUE and sort_var column → "Highlight" variant
#'
#' @param wb An openxlsx Workbook object
#' @param sheet Sheet name or index
#' @param df Data frame to write
#' @param styles Named list of openxlsx Style objects (from create_workbook_styles)
#' @param start_row Integer row to begin writing (default: 1)
#' @param sort_var Character name of the sort variable column (for Highlight);
#'   NULL if not applicable
#' @param fmt Logical: if TRUE, apply Highlight variant to sort_var column
#'   (default: FALSE)
#' @return Integer: the next available row after the data table
write_data_table <- function(wb, sheet, df, styles, start_row = 1,
                             sort_var = NULL, fmt = FALSE) {
  # Validate inputs
  if (is.null(wb)) stop("'wb' must be a non-NULL Workbook object", call. = FALSE)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    return(start_row)
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  col_names <- names(df)

  for (row_idx in seq_len(n_rows)) {
    excel_row <- start_row + row_idx - 1L
    is_last_row <- (row_idx == n_rows)

    for (col_idx in seq_len(n_cols)) {
      varname <- col_names[col_idx]
      cell_val <- df[[col_idx]][row_idx]
      is_numeric_col <- is.numeric(df[[col_idx]])

      # --- Determine base style ---
      style_name <- "Data"
      miss_num <- FALSE

      if (is_numeric_col) {
        varname_lower <- tolower(varname)

        # Check column name patterns for number formatting
        if (grepl("pct", varname_lower, fixed = TRUE)) {
          style_name <- "DataDec1"
        } else if (varname_lower %in% c("rd", "rr", "ort", "fd")) {
          style_name <- "DataDec1"
        } else if (
          substr(varname_lower, 1, min(nchar(varname_lower), 2)) %in% c("rd", "rr", "or") ||
          varname_lower == "p_value"
        ) {
          style_name <- "DataDec2"
        }

        # Scientific notation for extremely large numbers
        if (!is.na(cell_val) && is.numeric(cell_val) && cell_val > 1e6) {
          style_name <- "DataSN"
        }

        # Handle missing numeric data: display a dot "."
        if (is.na(cell_val)) {
          miss_num <- TRUE
          style_name <- "DataRight"
        }
      }

      # Apply Highlight variant for sort variable column when fmt=TRUE
      if (isTRUE(fmt) && !is.null(sort_var) &&
          toupper(varname) == toupper(sort_var)) {
        style_name <- paste0(style_name, "Highlight")
      }

      # Apply Bottom variant for last row
      if (is_last_row) {
        style_name <- paste0(style_name, "Bottom")
      }

      # --- Write cell value ---
      if (miss_num) {
        # Missing numeric → write "." as string
        openxlsx::writeData(wb, sheet, x = ".",
                            startRow = excel_row, startCol = col_idx)
      } else if (is.na(cell_val)) {
        # Missing character → write empty string (SAS: empty cell for missing string)
        # No data written — cell remains blank
      } else {
        # Write actual value
        openxlsx::writeData(wb, sheet, x = cell_val,
                            startRow = excel_row, startCol = col_idx)
      }

      # --- Apply style ---
      style_obj <- styles[[style_name]]
      if (is.null(style_obj)) {
        # Fallback: try without Bottom/Highlight suffixes
        fallback_name <- gsub("(Highlight|Bottom)", "", style_name)
        style_obj <- styles[[fallback_name]]
        if (is.null(style_obj)) {
          style_obj <- styles[["Data"]]
        }
      }
      openxlsx::addStyle(wb, sheet, style = style_obj,
                         rows = excel_row, cols = col_idx, stack = TRUE)
    }
  }

  # Auto-set column widths based on data content for readability
  openxlsx::setColWidths(wb, sheet, cols = seq_len(n_cols), widths = "auto")

  return(start_row + n_rows)
}


# =============================================================================
# count_rows() — Replaces SAS %ws_rowcount macro (lines 694-708)
# =============================================================================
#' Count Rows in a Data Frame
#'
#' Returns the number of rows in a data frame. Simplified from the SAS
#' %ws_rowcount macro which counted XML <Row> tags in a string dataset.
#' In R, this is trivially nrow(), but the function is preserved for API
#' parity with the SAS macro interface used by callers.
#'
#' @param df A data frame
#' @return Integer row count
count_rows <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(0L)
  }
  return(nrow(df))
}


# =============================================================================
# apply_style() — Convenience wrapper for openxlsx::addStyle()
# =============================================================================
#' Apply a Style to a Worksheet Cell
#'
#' Convenience wrapper for openxlsx::addStyle() that accepts row/col as
#' single integers. Provides a simpler interface matching the cell-level
#' style application pattern used throughout the Demographics panel.
#'
#' @param wb An openxlsx Workbook object
#' @param sheet Sheet name or index
#' @param row Integer row number
#' @param col Integer column number
#' @param style An openxlsx Style object
#' @return The workbook object (invisibly), modified in place
apply_style <- function(wb, sheet, row, col, style) {
  if (is.null(wb)) stop("'wb' must be a non-NULL Workbook object", call. = FALSE)
  if (is.null(style)) {
    return(invisible(wb))
  }

  openxlsx::addStyle(
    wb, sheet,
    style = style,
    rows = row,
    cols = col,
    stack = TRUE
  )

  return(invisible(wb))
}


# =============================================================================
# create_dynamic_styles() — Replaces SAS %xml_style_dcl + %xml_style_markup
#                            (lines 820-896)
# =============================================================================
#' Create Dynamic Styles from a Specification Tibble
#'
#' Accepts a tibble where each row defines a style via property columns, and
#' returns a named list of openxlsx Style objects. Replaces the SAS
#' %xml_style_dcl (variable declarations) and %xml_style_markup (data-driven
#' XML style generation) macros.
#'
#' @param style_df A tibble/data frame with columns:
#'   \describe{
#'     \item{ID}{Character: unique style identifier (required)}
#'     \item{HA}{Character: horizontal alignment ("Left", "Center", "Right")}
#'     \item{VA}{Character: vertical alignment ("Top", "Center", "Bottom")}
#'     \item{Indent}{Numeric: indentation level}
#'     \item{Wrap}{Numeric: 1 for wrap text, 0 or NA for no wrap}
#'     \item{Rotate}{Numeric: text rotation in degrees (e.g., 90)}
#'     \item{BT}{Numeric: 1 if top border present, 0 or NA otherwise}
#'     \item{BL}{Numeric: 1 if left border present}
#'     \item{BR}{Numeric: 1 if right border present}
#'     \item{BB}{Numeric: 1 if bottom border present}
#'     \item{BWt}{Numeric: border weight (SAS Weight=1 → "thin")}
#'     \item{BLS}{Character: border line style (default: "Continuous" → "thin")}
#'     \item{IntClr}{Character: interior/fill color as hex (e.g., "C0C0C0")}
#'     \item{FontSize}{Numeric: font size in points}
#'     \item{FontColor}{Character: font color as hex (e.g., "FF0000")}
#'     \item{Bold}{Numeric: 1 for bold, 0 or NA for normal}
#'     \item{Italic}{Numeric: 1 for italic, 0 or NA for normal}
#'     \item{NumFmt}{Character: number format string (e.g., "0.00", "0%")}
#'   }
#' @return Named list of openxlsx Style objects, keyed by ID
create_dynamic_styles <- function(style_df) {
  # Validate input
  if (is.null(style_df) || !is.data.frame(style_df) || nrow(style_df) == 0L) {
    return(list())
  }
  if (!"ID" %in% names(style_df)) {
    stop("'style_df' must contain an 'ID' column", call. = FALSE)
  }

  # Helper: safely get column value or default
  safe_get <- function(row, col, default = NA) {
    if (col %in% names(style_df)) {
      val <- style_df[[col]][row]
      if (is.na(val) || (is.character(val) && nchar(trimws(val)) == 0)) {
        return(default)
      }
      return(val)
    }
    return(default)
  }

  # Map SAS border line style to openxlsx border style
  map_border_style <- function(bls, bwt) {
    if (!is.na(bls) && nchar(trimws(as.character(bls))) > 0) {
      bls_lower <- tolower(trimws(as.character(bls)))
      if (bls_lower == "continuous") {
        if (!is.na(bwt) && bwt >= 2) return("medium")
        return("thin")
      } else if (bls_lower == "dash") {
        return("dashed")
      } else if (bls_lower == "dot") {
        return("dotted")
      } else if (bls_lower == "dashdot") {
        return("dashDot")
      } else if (bls_lower == "dashdotdot") {
        return("dashDotDot")
      } else if (bls_lower == "double") {
        return("double")
      }
    }
    # Default: thin for weight 1, medium for weight >= 2
    if (!is.na(bwt) && bwt >= 2) return("medium")
    return("thin")
  }

  dynamic_styles <- list()

  for (row_idx in seq_len(nrow(style_df))) {
    style_id <- as.character(style_df$ID[row_idx])

    # --- Alignment ---
    ha_val  <- safe_get(row_idx, "HA", NA)
    va_val  <- safe_get(row_idx, "VA", NA)
    wrap_val    <- safe_get(row_idx, "Wrap", NA)
    rotate_val  <- safe_get(row_idx, "Rotate", NA)
    indent_val  <- safe_get(row_idx, "Indent", NA)

    # --- Borders ---
    bt_val <- safe_get(row_idx, "BT", NA)
    bl_val <- safe_get(row_idx, "BL", NA)
    br_val <- safe_get(row_idx, "BR", NA)
    bb_val <- safe_get(row_idx, "BB", NA)
    bwt_val <- safe_get(row_idx, "BWt", NA)
    bls_val <- safe_get(row_idx, "BLS", NA)

    # --- Interior ---
    intclr_val <- safe_get(row_idx, "IntClr", NA)

    # --- Font ---
    fontsize_val  <- safe_get(row_idx, "FontSize", NA)
    fontcolor_val <- safe_get(row_idx, "FontColor", NA)
    bold_val      <- safe_get(row_idx, "Bold", NA)
    italic_val    <- safe_get(row_idx, "Italic", NA)

    # --- Number Format ---
    numfmt_val <- safe_get(row_idx, "NumFmt", NA)

    # Build createStyle arguments dynamically
    style_args <- list()

    # Horizontal alignment
    if (!is.na(ha_val) && nchar(trimws(as.character(ha_val))) > 0) {
      style_args$halign <- tolower(trimws(as.character(ha_val)))
    }

    # Vertical alignment
    if (!is.na(va_val) && nchar(trimws(as.character(va_val))) > 0) {
      style_args$valign <- tolower(trimws(as.character(va_val)))
    }

    # Wrap text
    if (!is.na(wrap_val) && as.numeric(wrap_val) == 1) {
      style_args$wrapText <- TRUE
    }

    # Text rotation
    if (!is.na(rotate_val) && as.numeric(rotate_val) != 0) {
      style_args$textRotation <- as.numeric(rotate_val)
    }

    # Indent
    if (!is.na(indent_val) && as.numeric(indent_val) > 0) {
      style_args$indent <- as.numeric(indent_val)
    }

    # Borders
    border_positions <- character(0)
    if (!is.na(bt_val) && as.numeric(bt_val) > 0) {
      border_positions <- c(border_positions, "Top")
    }
    if (!is.na(bl_val) && as.numeric(bl_val) > 0) {
      border_positions <- c(border_positions, "Left")
    }
    if (!is.na(br_val) && as.numeric(br_val) > 0) {
      border_positions <- c(border_positions, "Right")
    }
    if (!is.na(bb_val) && as.numeric(bb_val) > 0) {
      border_positions <- c(border_positions, "Bottom")
    }
    if (length(border_positions) > 0) {
      border_style <- map_border_style(bls_val, bwt_val)
      style_args$border <- border_positions
      style_args$borderStyle <- border_style
    }

    # Interior/fill color
    if (!is.na(intclr_val) && nchar(trimws(as.character(intclr_val))) > 0) {
      color_hex <- trimws(as.character(intclr_val))
      if (!startsWith(color_hex, "#")) color_hex <- paste0("#", color_hex)
      style_args$fgFill <- color_hex
    }

    # Font size
    if (!is.na(fontsize_val) && as.numeric(fontsize_val) > 0) {
      style_args$fontSize <- as.numeric(fontsize_val)
    }

    # Font color
    if (!is.na(fontcolor_val) && nchar(trimws(as.character(fontcolor_val))) > 0) {
      fcolor_hex <- trimws(as.character(fontcolor_val))
      if (!startsWith(fcolor_hex, "#")) fcolor_hex <- paste0("#", fcolor_hex)
      style_args$fontColour <- fcolor_hex
    }

    # Text decoration (bold and/or italic)
    decorations <- character(0)
    if (!is.na(bold_val) && as.numeric(bold_val) > 0) {
      decorations <- c(decorations, "bold")
    }
    if (!is.na(italic_val) && as.numeric(italic_val) > 0) {
      decorations <- c(decorations, "italic")
    }
    if (length(decorations) > 0) {
      style_args$textDecoration <- decorations
    }

    # Number format
    if (!is.na(numfmt_val) && nchar(trimws(as.character(numfmt_val))) > 0) {
      style_args$numFmt <- trimws(as.character(numfmt_val))
    }

    # Create the style object
    dynamic_styles[[style_id]] <- do.call(openxlsx::createStyle, style_args)
  }

  return(dynamic_styles)
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - Complete SpreadsheetML XML generation replaced by openxlsx API
####    - All ~66 named styles mapped to openxlsx::createStyle() equivalents
####    - Style inheritance (SAS ss:Parent) flattened into explicit property specification
####    - SAS global $strlen variable (XML string buffer length) is not needed in R
####    - Deprecated macros (%wsheader, %wsdata, %ws_rowcount) included for completeness
####    - SAS default font size (9pt via Normal style) set via modifyBaseFont in create_workbook
####    - The SAS space indicator '~!' is replaced with actual space in write_annotated_data
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected — this module handles formatting, not computation
####    - Number format strings (0, 0.0, 0.00, 0.00E+00, 0%) are preserved exactly
#### NO DIRECT R EQUIVALENT:
####    - SAS SpreadsheetML XML string concatenation → openxlsx workbook object API
####    - SAS %wb workbook skeleton → openxlsx::createWorkbook()
####    - SAS %styles style gallery → named list of createStyle() objects
####    - SAS %annotate row-per-variable conversion → tidyr::pivot_longer()
####    - SAS %markup XML tag assembly → openxlsx::writeData() + addStyle()
####    - SAS %xml_style_dcl/%xml_style_markup dynamic style creation → createStyle() from spec tibble
####    - SAS DATA _NULL_ file writing → openxlsx::saveWorkbook()
####    - SAS MergeDown/MergeAcross XML attributes → openxlsx::mergeCells()
####    - SAS ss:Parent style inheritance → explicit property repetition in each createStyle()
####    - SAS %xml_tag_def/%xml_init tag variable declarations → not needed in R (native data types)
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Full replacement for SpreadsheetML XML generation; native R Excel API
####    - dplyr: Data manipulation for annotate/markup operations (AAP mandates tidyverse)
####    - tidyr: pivot_longer() for data annotation replacing SAS column iteration
#### OPEN QUESTIONS:
####    - Whether openxlsx font rendering matches SAS SpreadsheetML defaults exactly
####    - Whether ss:Rotate="90" maps directly to openxlsx textRotation = 90
####    - Border weight mapping: SAS Weight="1" → openxlsx borderStyle = "thin"
####    - Whether all ~66 styles are actively used or some are legacy/unused
####    - Exact pixel-to-character-width mapping for column widths set externally
# ============================================================
