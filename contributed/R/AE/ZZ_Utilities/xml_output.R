# ==============================================================================
#
#         PROGRAM NAME: Excel XML Output Functions (R Migration)
#
#          DESCRIPTION: R functions for creating Excel workbooks using openxlsx,
#                       migrated from SAS SpreadsheetML XML generation macros.
#
#                       create_workbook()       -- create an openxlsx workbook
#                       create_styles()         -- a collection of 50+ named styles
#                       write_header()          -- write headers/footers to worksheet
#                       write_data_table()      -- write styled data table to worksheet
#                       annotate_data()         -- annotate a data frame for markup
#                       write_annotated_data()  -- write annotated data to worksheet
#                       create_dynamic_styles() -- create styles from specification
#
#                       Migrated from SAS macros:
#                       %wb, %styles, %wsheader, %wsdata, %annotate, %markup,
#                       %xml_tag_def, %xml_init, %xml_style_dcl, %xml_style_markup
#
#                       The SAS macros %xml_tag_def and %xml_init have no direct
#                       R equivalent as the tibble structure in annotate_data()
#                       replaces their role of declaring/initializing XML variables.
#
#               AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                       Migrated to R by Blitzy
#
#         ORIGINAL DATE: February 15, 2011
#         MIGRATION DATE: 2026
#
#            MADE WITH: R 4.3+ / openxlsx
#
#            REVISIONS: SAS -> R migration (openxlsx replaces SpreadsheetML XML)
#
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(openxlsx)
library(dplyr)
library(cli)


# ==============================================================================
# create_workbook
# ------------------------------------------------------------------------------
# Replaces SAS %wb macro (SAS lines 41-66)
# Creates an openxlsx workbook object with document properties.
# SAS generated SpreadsheetML XML header tags; openxlsx handles this internally.
#
# @param title  Character. Workbook title (default "").
# @param author Character. Workbook author
#               (default "US Food & Drug Administration").
# @return An openxlsx workbook object.
# ==============================================================================
create_workbook <- function(title = "",
                            author = "US Food & Drug Administration") {
  if (!is.character(title)) {
    cli::cli_abort("{.arg title} must be a character string.")
  }
  if (!is.character(author)) {
    cli::cli_abort("{.arg author} must be a character string.")
  }

  wb <- openxlsx::createWorkbook(creator = author, title = title)

  cli::cli_inform(
    "Workbook created with title {.val {title}} and author {.val {author}}."
  )

  wb
}


# ==============================================================================
# create_styles
# ------------------------------------------------------------------------------
# Replaces SAS %styles macro (SAS lines 72-558)
# Creates a comprehensive named list of 50+ openxlsx Style objects mirroring
# the SpreadsheetML style gallery from the original SAS program.
#
# SAS used style inheritance via ss:Parent; openxlsx does not support style
# inheritance, so every property is specified explicitly on each style.
#
# @param size Numeric. Base font size in points (default 9, matching SAS).
# @return A named list of openxlsx Style objects.
# ==============================================================================
create_styles <- function(size = 9) {
  if (!is.numeric(size) || length(size) != 1 || size <= 0) {
    cli::cli_abort("{.arg size} must be a positive numeric scalar.")
  }

  # --- Color constants (preserved from SAS) ---
  col_header_bg  <- "#333399"
  col_silver     <- "#C0C0C0"
  col_highlight  <- "#CCCCFF"
  col_red        <- "#FF0000"
  col_gray       <- "#808080"
  col_peach      <- "#FFCC99"
  col_white      <- "#FFFFFF"

  # --- Border shorthand helpers ---
  border_lr    <- "LeftRight"
  border_all   <- "TopBottomLeftRight"
  border_blr   <- c("Bottom", "Left", "Right")
  border_tlr   <- c("Top", "Left", "Right")
  border_lr_v  <- c("Left", "Right")
  border_lrb   <- c("Left", "Right", "Bottom")

  bstyle_thin  <- "thin"

  styles <- list()

  # ---- Base Styles ----


  # Default (SAS lines 79-82): base font size, solid pattern
  styles$Default <- openxlsx::createStyle(fontSize = size)

  # DefaultLeft (SAS lines 85-87): left/top alignment, no wrap

  styles$DefaultLeft <- openxlsx::createStyle(
    fontSize = size, halign = "left", valign = "top", wrapText = FALSE
  )

  # DefaultRight (SAS lines 90-92): right/top alignment, no wrap
  styles$DefaultRight <- openxlsx::createStyle(
    fontSize = size, halign = "right", valign = "top", wrapText = FALSE
  )

  # DefaultWhite (SAS lines 95-97): white font color
  styles$DefaultWhite <- openxlsx::createStyle(
    fontSize = size, fontColour = col_white
  )

  # Default10 (SAS lines 100-103): 10pt font, top alignment, no wrap
  styles$Default10 <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = FALSE
  )

  # Default10Wrap (SAS lines 106-108): 10pt font, top alignment, wrap
  styles$Default10Wrap <- openxlsx::createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE
  )

  # Default10RedWrap (SAS lines 111-114): 10pt red italic, top alignment, wrap
  styles$Default10RedWrap <- openxlsx::createStyle(
    fontSize = 10, fontColour = col_red, textDecoration = "italic",
    valign = "top", wrapText = TRUE
  )

  # Default10Right (SAS lines 117-119): 10pt, right/top, no wrap
  styles$Default10Right <- openxlsx::createStyle(
    fontSize = 10, halign = "right", valign = "top", wrapText = FALSE
  )

  # Default8 (SAS lines 122-125): 8pt font, top alignment, no wrap
  styles$Default8 <- openxlsx::createStyle(
    fontSize = 8, valign = "top", wrapText = FALSE
  )

  # ---- Header Styles ----

  # Header (SAS lines 128-131): 12pt, bold italic
  styles$Header <- openxlsx::createStyle(
    fontSize = 12, textDecoration = c("bold", "italic")
  )

  # SubHeader (SAS lines 134-137): 10pt, bold, top alignment
  styles$SubHeader <- openxlsx::createStyle(
    fontSize = 10, textDecoration = "bold", valign = "top"
  )

  # ---- Column Header Styles ----

  # Column (SAS lines 140-144): centered, dark blue bg, white bold 10pt
  styles$Column <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 10, textDecoration = "bold"
  )

  # ColumnOutline (SAS lines 147-154): Column + all 4 thin borders
  styles$ColumnOutline <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 10, textDecoration = "bold",
    border = border_all, borderStyle = bstyle_thin
  )

  # ColumnOutlineSmall (SAS lines 157-159): ColumnOutline + 8pt
  styles$ColumnOutlineSmall <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 8, textDecoration = "bold",
    border = border_all, borderStyle = bstyle_thin
  )

  # ColumnOutlineItalic (SAS lines 162-164): ColumnOutline + italic
  styles$ColumnOutlineItalic <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 10, textDecoration = c("bold", "italic"),
    border = border_all, borderStyle = bstyle_thin
  )

  # ColumnOutlineRotateTop (SAS lines 167-169): ColumnOutline + 90deg + top
  styles$ColumnOutlineRotateTop <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 10, textDecoration = "bold",
    border = border_all, borderStyle = bstyle_thin,
    textRotation = 90
  )

  # ColumnOutlineRotateCtr (SAS lines 172-174): ColumnOutline + 90deg + center
  styles$ColumnOutlineRotateCtr <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = TRUE,
    fgFill = col_header_bg, fontColour = col_white,
    fontSize = 10, textDecoration = "bold",
    border = border_all, borderStyle = bstyle_thin,
    textRotation = 90
  )

  # ---- DataHeader Style ----

  # DataHeader (SAS lines 177-187): left/center, silver bg, bold 10pt, all borders
  styles$DataHeader <- openxlsx::createStyle(
    halign = "left", valign = "center", wrapText = FALSE,
    fgFill = col_silver, fontSize = 10, textDecoration = "bold",
    border = border_all, borderStyle = bstyle_thin
  )

  # ---- Table Style ----

  # Table (SAS lines 190-199): center/center, 10pt, all borders
  styles$Table <- openxlsx::createStyle(
    halign = "center", valign = "center", wrapText = FALSE,
    fontSize = 10,
    border = border_all, borderStyle = bstyle_thin
  )

  # ---- Data Parent Style ----

  # Data (SAS lines 202-208): top alignment, left+right borders
  styles$Data <- openxlsx::createStyle(
    valign = "top", wrapText = FALSE,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # ---- Data Variant Styles ----

  # DataWrap (SAS lines 211-213): Data + wrapText
  styles$DataWrap <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataRight (SAS lines 216-218): Data + right alignment
  styles$DataRight <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataRightHighlight (SAS lines 221-223): DataRight + highlight fill
  styles$DataRightHighlight <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataCenter (SAS lines 226-228): Data + center alignment
  styles$DataCenter <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # ---- Number Format Styles ----

  # DataDec0 (SAS lines 231-234): Data + right + "0" format
  styles$DataDec0 <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec0Center (SAS lines 237-239): DataDec0 + center
  styles$DataDec0Center <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec0Highlight (SAS lines 242-244): DataDec0 + highlight
  styles$DataDec0Highlight <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0", fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec1 (SAS lines 247-250): Data + right + "0.0" format
  styles$DataDec1 <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.0",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec1Center (SAS lines 253-255): DataDec1 + center
  styles$DataDec1Center <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.0",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec1Highlight (SAS lines 258-260): DataDec1 + highlight
  styles$DataDec1Highlight <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.0", fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec2 (SAS lines 263-266): Data + right + "0.00" format
  styles$DataDec2 <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec2Center (SAS lines 269-271): DataDec2 + center
  styles$DataDec2Center <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.00",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataDec2Highlight (SAS lines 274-276): DataDec2 + highlight
  styles$DataDec2Highlight <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00", fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataSN (SAS lines 279-282): Data + right + scientific notation
  styles$DataSN <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00E+00",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataSNCenter (SAS lines 285-287): DataSN + center
  styles$DataSNCenter <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.00E+00",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataSNHighlight (SAS lines 290-292): DataSN + highlight
  styles$DataSNHighlight <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00E+00", fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataPct (SAS lines 295-298): Data + right + percent format
  styles$DataPct <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0%",
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # DataPctHighlight (SAS lines 301-303): DataPct + highlight
  styles$DataPctHighlight <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0%", fgFill = col_highlight,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # ---- Bottom Row Variants ----
  # All bottom variants add a bottom border (total: bottom + left + right)

  # DataBottom (SAS lines 306-312): Data + bottom+left+right borders
  styles$DataBottom <- openxlsx::createStyle(
    valign = "top", wrapText = FALSE,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataWrapBottom (SAS lines 315-321): DataWrap + bottom+left+right borders
  styles$DataWrapBottom <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataRightBottom (SAS lines 324-330): DataRight + bottom border
  styles$DataRightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataRightHighlightBottom (SAS lines 333-339): DataRightHighlight + bottom
  styles$DataRightHighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top", wrapText = FALSE,
    fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataCenterBottom (SAS lines 342-348): DataCenter + bottom
  styles$DataCenterBottom <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec0Bottom (SAS lines 351-357): DataDec0 + bottom
  styles$DataDec0Bottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec0CenterBottom (SAS lines 360-366): DataDec0Center + bottom
  styles$DataDec0CenterBottom <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec0HighlightBottom (SAS lines 369-375): DataDec0Highlight + bottom
  styles$DataDec0HighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0", fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec1Bottom (SAS lines 378-384): DataDec1 + bottom
  styles$DataDec1Bottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.0",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec1CenterBottom (SAS lines 387-393): DataDec1Center + bottom
  styles$DataDec1CenterBottom <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.0",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec1HighlightBottom (SAS lines 396-402): DataDec1Highlight + bottom
  styles$DataDec1HighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.0", fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec2Bottom (SAS lines 405-411): DataDec2 + bottom
  styles$DataDec2Bottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec2CenterBottom (SAS lines 414-420): DataDec2Center + bottom
  styles$DataDec2CenterBottom <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.00",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataDec2HighlightBottom (SAS lines 423-429): DataDec2Highlight + bottom
  styles$DataDec2HighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00", fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataSNBottom (SAS lines 432-438): DataSN + bottom
  styles$DataSNBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00E+00",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataSNCenterBottom (SAS lines 441-447): DataSNCenter + bottom
  styles$DataSNCenterBottom <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    numFmt = "0.00E+00",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataSNHighlightBottom (SAS lines 450-456): DataSNHighlight + bottom
  styles$DataSNHighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.00E+00", fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataPctBottom (SAS lines 459-465): DataPct + bottom
  styles$DataPctBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0%",
    border = border_blr, borderStyle = bstyle_thin
  )

  # DataPctHighlightBottom (SAS lines 468-474): DataPctHighlight + bottom
  styles$DataPctHighlightBottom <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0%", fgFill = col_highlight,
    border = border_blr, borderStyle = bstyle_thin
  )

  # ---- Special Styles (AE MedDRA cover page) ----

  # Gray (SAS lines 479-482): DataCenterBottom base + 8pt gray on gray
  styles$Gray <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    fontSize = 8, fontColour = col_gray, fgFill = col_gray,
    border = border_blr, borderStyle = bstyle_thin
  )

  # Red (SAS lines 485-488): DataCenterBottom base + 8pt red on red
  styles$Red <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    fontSize = 8, fontColour = col_red, fgFill = col_red,
    border = border_blr, borderStyle = bstyle_thin
  )

  # Peach (SAS lines 491-494): DataCenterBottom base + 8pt peach on peach
  styles$Peach <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = FALSE,
    fontSize = 8, fontColour = col_peach, fgFill = col_peach,
    border = border_blr, borderStyle = bstyle_thin
  )

  # BoldRedText (SAS lines 497-499): DataDec1Bottom base + 8pt bold red
  styles$BoldRedText <- openxlsx::createStyle(
    halign = "right", valign = "top",
    numFmt = "0.0",
    fontSize = 8, fontColour = col_red, textDecoration = "bold",
    border = border_blr, borderStyle = bstyle_thin
  )

  # ---- Grouping & Subsetting Styles ----

  # GS_BTLRB (SAS lines 502-511): 10pt, top, wrap, all 4 borders
  styles$GS_BTLRB <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE, fontSize = 10,
    border = border_all, borderStyle = bstyle_thin
  )

  # GSC_BTLRB (SAS lines 514-523): GS_BTLRB + center horizontal
  styles$GSC_BTLRB <- openxlsx::createStyle(
    halign = "center", valign = "top", wrapText = TRUE, fontSize = 10,
    border = border_all, borderStyle = bstyle_thin
  )

  # GS_BTLR (SAS lines 525-533): 10pt, top, wrap, top+left+right borders
  styles$GS_BTLR <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE, fontSize = 10,
    border = border_tlr, borderStyle = bstyle_thin
  )

  # GS_BLR (SAS lines 535-542): 10pt, top, wrap, left+right borders only
  styles$GS_BLR <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE, fontSize = 10,
    border = border_lr_v, borderStyle = bstyle_thin
  )

  # GS_BLRB (SAS lines 544-552): 10pt, top, wrap, left+right+bottom borders
  styles$GS_BLRB <- openxlsx::createStyle(
    valign = "top", wrapText = TRUE, fontSize = 10,
    border = border_lrb, borderStyle = bstyle_thin
  )

  cli::cli_inform(
    "Style gallery created with {.val {length(styles)}} named styles."
  )

  styles
}


# ==============================================================================
# write_header
# ------------------------------------------------------------------------------
# Replaces SAS %wsheader macro (SAS lines 565-592)
# Writes header rows (title, subtitle, footnotes) to an openxlsx worksheet.
# Header data is grouped by the 'group' column; each group is preceded by a
# blank row. Style is selected based on group value:
#   "title"    -> Header style
#   "subtitle" -> SubHeader style
#   other      -> Default style
#
# @param wb          An openxlsx workbook object.
# @param sheet       Character or integer. Target worksheet name or index.
# @param header_data A data frame / tibble with columns 'group' and 'data'.
#                    'group' categorizes rows; 'data' holds text content.
# @param styles      Named list of openxlsx Style objects from create_styles().
# @param start_row   Integer. First row to write to (default 1).
# @return The next available row number (integer) after the last header row.
# ==============================================================================
write_header <- function(wb, sheet, header_data, styles, start_row = 1) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(header_data)) {
    cli::cli_abort("{.arg header_data} must be a data frame.")
  }
  if (!all(c("group", "data") %in% names(header_data))) {
    cli::cli_abort("{.arg header_data} must contain columns {.val group} and {.val data}.")
  }

  current_row <- as.integer(start_row)

  # Track group transitions to insert blank rows before each new group
  # (SAS: by group notsorted; if first.group then blank row)
  header_data <- header_data %>%
    dplyr::mutate(
      .row_idx = dplyr::row_number(),
      .grp_id  = cumsum(c(1L, as.integer(group[-dplyr::n()] != group[-1L])))
    )

  prev_grp <- NA_integer_

  for (i in seq_len(nrow(header_data))) {
    row_data <- header_data[i, ]
    grp_id   <- row_data$.grp_id

    # Blank row before each new group (SAS lines 572-574)
    if (is.na(prev_grp) || grp_id != prev_grp) {
      current_row <- current_row + 1L
      prev_grp <- grp_id
    }

    # Select style based on group value (SAS lines 576-578)
    style_name <- dplyr::case_when(
      row_data$group == "title"    ~ "Header",
      row_data$group == "subtitle" ~ "SubHeader",
      TRUE                         ~ "Default"
    )

    selected_style <- styles[[style_name]]
    if (is.null(selected_style)) {
      cli::cli_warn("Style {.val {style_name}} not found; using no style.")
    }

    # Write cell value with style (SAS lines 580-583)
    openxlsx::writeData(wb, sheet, x = as.character(row_data$data),
                        startRow = current_row, startCol = 1)
    if (!is.null(selected_style)) {
      openxlsx::addStyle(wb, sheet, style = selected_style,
                         rows = current_row, cols = 1, stack = TRUE)
    }

    current_row <- current_row + 1L
  }

  # Blank row after last entry (SAS lines 585-587)
  current_row <- current_row + 1L

  current_row
}


# ==============================================================================
# write_data_table
# ------------------------------------------------------------------------------
# Replaces SAS %wsdata macro (SAS lines 596-689)
# Writes a data frame to a worksheet with automatic style detection based on
# column names and data characteristics.
#
# Style auto-detection rules (from SAS):
#   - Column name contains "pct"                      -> DataDec1
#   - Column name in ("rd","rr","ort","fd")            -> DataDec1
#   - Column name starts with "rd"/"rr"/"or" or
#     equals "p_value"                                 -> DataDec2
#   - Numeric value > 1e6                              -> DataSN
#   - Missing numeric                                  -> "." with DataRight
#   - fmt=TRUE and column matches sort_var             -> Highlight variant
#   - Last row                                         -> Bottom variant
#
# If a row has a column named 'header' with value 1, it is written as a
# DataHeader row spanning all columns.
#
# @param wb        An openxlsx workbook object.
# @param sheet     Character or integer. Target worksheet name or index.
# @param data      A data frame to write.
# @param styles    Named list of openxlsx Style objects from create_styles().
# @param start_row Integer. First row to write to.
# @param fmt       Logical. If TRUE, apply highlight and indentation formatting
#                  (default FALSE; replaces SAS %let fmt = N).
# @param sort_var  Character or NULL. Column name for highlight formatting
#                  when fmt = TRUE (default NULL).
# @return The next available row number (integer) after the last data row.
# ==============================================================================
write_data_table <- function(wb, sheet, data, styles, start_row,
                             fmt = FALSE, sort_var = NULL) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0) {
    cli::cli_warn("No data rows to write.")
    return(as.integer(start_row))
  }

  current_row <- as.integer(start_row)
  n_rows <- nrow(data)

  # Identify if 'header' column exists for header-row detection
  has_header_col <- "header" %in% names(data)

  # Determine data columns (exclude the 'header' indicator if present)
  data_col_names <- setdiff(names(data), "header")
  n_data_cols <- length(data_col_names)

  for (ri in seq_len(n_rows)) {
    is_last <- (ri == n_rows)
    row_vals <- data[ri, , drop = FALSE]

    # Check if this is a header row (SAS lines 617-624)
    is_header_row <- has_header_col && !is.na(row_vals[["header"]]) &&
      row_vals[["header"]] == 1

    if (is_header_row) {
      # Write header label spanning all data columns
      header_text <- as.character(row_vals[[data_col_names[1]]])
      openxlsx::writeData(wb, sheet, x = header_text,
                          startRow = current_row, startCol = 1)
      # Merge across all columns
      if (n_data_cols > 1) {
        openxlsx::mergeCells(wb, sheet,
                             cols = seq_len(n_data_cols),
                             rows = current_row)
      }
      # Apply DataHeader style
      dh_style <- styles[["DataHeader"]]
      if (!is.null(dh_style)) {
        openxlsx::addStyle(wb, sheet, style = dh_style,
                           rows = current_row,
                           cols = seq_len(n_data_cols),
                           stack = TRUE)
      }
      # Set row height to 18 as in SAS (line 618)
      openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = 18)

    } else {
      # Write ordinary data columns with auto-detected styles
      for (ci in seq_along(data_col_names)) {
        col_name  <- data_col_names[ci]
        col_lower <- tolower(col_name)
        cell_val  <- row_vals[[col_name]]
        is_num    <- is.numeric(cell_val)
        miss_num  <- FALSE

        # Base style determination (SAS lines 640-661)
        style_name <- "Data"

        if (is_num) {
          # Column name contains "pct" (SAS lines 645-646)
          if (grepl("pct", col_lower, fixed = TRUE)) {
            style_name <- "DataDec1"
          # Column name in (rd, rr, ort, fd) (SAS lines 647-648)
          } else if (col_lower %in% c("rd", "rr", "ort", "fd")) {
            style_name <- "DataDec1"
          # Column starts with rd/rr/or or is p_value (SAS lines 649-651)
          } else if (grepl("^(rd|rr|or)", col_lower) ||
                     col_lower == "p_value") {
            style_name <- "DataDec2"
          }

          # Scientific notation for very large numbers (SAS lines 653-654)
          if (!is.na(cell_val) && cell_val > 1e6) {
            style_name <- "DataSN"
          }

          # Missing numeric -> display as "." (SAS lines 656-660)
          if (is.na(cell_val)) {
            miss_num   <- TRUE
            style_name <- "DataRight"
          }
        }

        # Highlight variant for sort variable (SAS lines 664-666)
        if (isTRUE(fmt) && !is.null(sort_var) &&
            toupper(col_name) == toupper(sort_var)) {
          style_name <- paste0(style_name, "Highlight")
        }

        # Bottom variant for last row (SAS line 667)
        if (is_last) {
          style_name <- paste0(style_name, "Bottom")
        }

        # Resolve style object
        resolved_style <- styles[[style_name]]
        if (is.null(resolved_style)) {
          cli::cli_warn(
            "Style {.val {style_name}} not found in styles list."
          )
        }

        # Prepare cell value for writing
        if (miss_num) {
          # SAS writes "." for missing numerics (SAS line 673)
          write_val <- "."
        } else if (is_num && is.na(cell_val)) {
          write_val <- NA
        } else if (!is_num && is.na(cell_val)) {
          # Missing character: write empty string (SAS: missing string -> "")
          write_val <- NA
        } else if (isTRUE(fmt) && ci == 1 && !miss_num) {
          # SAS fmt=Y prepends 5 spaces to first column (SAS line 672)
          write_val <- paste0("     ", as.character(cell_val))
        } else {
          write_val <- cell_val
        }

        # Write cell
        openxlsx::writeData(wb, sheet, x = write_val,
                            startRow = current_row, startCol = ci)

        # Apply style
        if (!is.null(resolved_style)) {
          openxlsx::addStyle(wb, sheet, style = resolved_style,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
    }

    current_row <- current_row + 1L
  }

  current_row
}


# ==============================================================================
# annotate_data
# ------------------------------------------------------------------------------
# Replaces SAS %annotate macro (SAS lines 713-742) and incorporates the role of
# %xml_tag_def (SAS lines 789-800) and %xml_init (SAS lines 804-812).
#
# Converts a wide data frame into a long-format tibble where each cell becomes
# one row, with metadata columns for XML/openxlsx markup.
#
# Columns in the returned tibble:
#   Row          - Original row number
#   Data         - Cell value as character
#   Type         - "String" or "Number"
#   varname      - Lowercase column name
#   bottom       - 1 for last row, 0 otherwise
#   Height       - Row height (NA = default)
#   Index        - Cell index override (NA = auto)
#   MergeAcross  - Number of columns to merge right (NA = none)
#   MergeDown    - Number of rows to merge down (NA = none)
#   StyleID      - Style name to apply (NA = none)
#   Formula      - Cell formula (NA = none)
#   Comment      - Cell comment text (NA = none)
#   Name         - Named range reference (NA = none)
#   ArrayRange   - Array formula range (NA = none)
#
# @param data A data frame to annotate.
# @return A tibble in long format with one row per cell.
# ==============================================================================
annotate_data <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0) {
    cli::cli_warn("Empty data frame provided to {.fn annotate_data}.")
    return(
      dplyr::tibble(
        Row = integer(), Data = character(), Type = character(),
        varname = character(), bottom = integer(),
        Height = numeric(), Index = numeric(),
        MergeAcross = numeric(), MergeDown = numeric(),
        StyleID = character(), Formula = character(),
        Comment = character(), Name = character(),
        ArrayRange = character()
      )
    )
  }

  n_rows <- nrow(data)
  col_names <- names(data)
  n_cols <- length(col_names)

  # Build a list of row-tibbles for efficiency, then bind
  result_list <- vector("list", n_rows * n_cols)
  idx <- 0L

  for (ri in seq_len(n_rows)) {
    is_bottom <- dplyr::if_else(ri == n_rows, 1L, 0L)

    for (ci in seq_along(col_names)) {
      cname <- col_names[ci]
      cval  <- data[[cname]][ri]
      idx   <- idx + 1L

      # Determine type (SAS lines 729-732)
      if (is.numeric(cval)) {
        cell_type <- "Number"
        cell_data <- dplyr::if_else(is.na(cval), NA_character_,
                                    as.character(cval))
      } else {
        cell_type <- "String"
        cell_data <- dplyr::if_else(is.na(cval), NA_character_,
                                    as.character(cval))
      }

      result_list[[idx]] <- dplyr::tibble(
        Row         = ri,
        Data        = cell_data,
        Type        = cell_type,
        varname     = tolower(cname),
        bottom      = is_bottom,
        Height      = NA_real_,
        Index       = NA_real_,
        MergeAcross = NA_real_,
        MergeDown   = NA_real_,
        StyleID     = NA_character_,
        Formula     = NA_character_,
        Comment     = NA_character_,
        Name        = NA_character_,
        ArrayRange  = NA_character_
      )
    }
  }

  dplyr::bind_rows(result_list)
}


# ==============================================================================
# write_annotated_data
# ------------------------------------------------------------------------------
# Replaces SAS %markup macro (SAS lines 746-785)
# Writes annotated (long-format) data to an openxlsx worksheet, applying
# cell-level styles, merges, row heights, and formula/comment attributes.
#
# Processes the annotated tibble row by row, grouped by the Row column.
# For each cell:
#   - Writes the Data value (replacing "~!" with " " per SAS line 773)
#   - Applies the StyleID from the styles list
#   - Merges cells if MergeAcross or MergeDown are set
#   - Sets row height if Height is specified
#
# @param wb             An openxlsx workbook object.
# @param sheet          Character or integer. Target worksheet name or index.
# @param annotated_data A tibble from annotate_data(), optionally modified.
# @param styles         Named list of openxlsx Style objects.
# @param start_row      Integer. First worksheet row to write to.
# @return The next available row number (integer) after the last written row.
# ==============================================================================
write_annotated_data <- function(wb, sheet, annotated_data, styles, start_row) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(annotated_data)) {
    cli::cli_abort("{.arg annotated_data} must be a data frame.")
  }
  if (nrow(annotated_data) == 0) {
    cli::cli_warn("No annotated data rows to write.")
    return(as.integer(start_row))
  }

  current_row <- as.integer(start_row)

  # Group by Row to process one worksheet row at a time
  # (SAS: by row notsorted)
  unique_rows <- unique(annotated_data$Row)

  for (row_val in unique_rows) {
    row_cells <- annotated_data %>%
      dplyr::filter(Row == row_val)

    # Set row height if specified for any cell in this row (SAS line 754)
    heights <- row_cells$Height[!is.na(row_cells$Height)]
    if (length(heights) > 0) {
      openxlsx::setRowHeights(wb, sheet, rows = current_row,
                              heights = heights[1])
    }

    col_cursor <- 1L

    for (ci in seq_len(nrow(row_cells))) {
      cell <- row_cells[ci, ]

      # Determine column position (SAS Index attribute)
      if (!is.na(cell$Index) && cell$Index > 0) {
        col_cursor <- as.integer(cell$Index)
      }

      # Prepare cell value
      cell_value <- cell$Data
      if (!is.na(cell_value)) {
        # Replace space indicator "~!" with " " (SAS line 773)
        cell_value <- gsub("~!", " ", cell_value, fixed = TRUE)

        # Write as numeric if Type is Number and value is parseable
        if (!is.na(cell$Type) && cell$Type == "Number") {
          numeric_val <- suppressWarnings(as.numeric(cell_value))
          if (!is.na(numeric_val)) {
            cell_value <- numeric_val
          }
        }
      } else {
        # Missing data: write empty cell (SAS line 764)
        cell_value <- ""
      }

      # Write cell value
      openxlsx::writeData(wb, sheet, x = cell_value,
                          startRow = current_row, startCol = col_cursor)

      # Apply style (SAS StyleID attribute)
      if (!is.na(cell$StyleID) && nchar(cell$StyleID) > 0) {
        resolved_style <- styles[[cell$StyleID]]
        if (!is.null(resolved_style)) {
          openxlsx::addStyle(wb, sheet, style = resolved_style,
                             rows = current_row, cols = col_cursor,
                             stack = TRUE)
        } else {
          cli::cli_warn(
            "Style {.val {cell$StyleID}} not found for Row {.val {row_val}}, Col {.val {col_cursor}}."
          )
        }
      }

      # Handle MergeAcross (SAS MergeAcross attribute)
      merge_across <- cell$MergeAcross
      merge_down   <- cell$MergeDown

      if (!is.na(merge_across) && merge_across > 0) {
        end_col <- col_cursor + as.integer(merge_across)
        if (!is.na(merge_down) && merge_down > 0) {
          end_row <- current_row + as.integer(merge_down)
          openxlsx::mergeCells(wb, sheet,
                               cols = col_cursor:end_col,
                               rows = current_row:end_row)
        } else {
          openxlsx::mergeCells(wb, sheet,
                               cols = col_cursor:end_col,
                               rows = current_row)
        }
      } else if (!is.na(merge_down) && merge_down > 0) {
        end_row <- current_row + as.integer(merge_down)
        openxlsx::mergeCells(wb, sheet,
                             cols = col_cursor,
                             rows = current_row:end_row)
      }

      col_cursor <- col_cursor + 1L
    }

    current_row <- current_row + 1L
  }

  current_row
}


# ==============================================================================
# create_dynamic_styles
# ------------------------------------------------------------------------------
# Replaces SAS %xml_style_dcl (SAS lines 820-831) and %xml_style_markup
# (SAS lines 834-896).
#
# Creates openxlsx Style objects from a specification tibble where each row
# defines one style with named attributes. This allows downstream panels to
# define custom styles programmatically rather than relying on the fixed
# gallery from create_styles().
#
# Specification columns (matching SAS variable names from %xml_style_dcl):
#   ID        - Style name / identifier (required)
#   HA        - Horizontal alignment ("Left","Center","Right" or "")
#   VA        - Vertical alignment ("Top","Center","Bottom" or "")
#   Indent    - Indentation level (NA = none)
#   Wrap      - Wrap text: 1 = yes, 0/NA = no
#   Rotate    - Text rotation in degrees (NA = none)
#   BT        - Border top: 1 = yes, 0/NA = no
#   BL        - Border left: 1 = yes, 0/NA = no
#   BR        - Border right: 1 = yes, 0/NA = no
#   BB        - Border bottom: 1 = yes, 0/NA = no
#   BWt       - Border weight (NA => default thin)
#   BLS       - Border line style (e.g., "Continuous" or ""; SAS default)
#   IntClr    - Interior fill color (6-char hex WITHOUT #, or "")
#   FontSize  - Font size in points (NA = default)
#   FontColor - Font color (up to 8-char hex WITHOUT #, or "")
#   Bold      - Bold: 1 = yes, 0/NA = no
#   Italic    - Italic: 1 = yes, 0/NA = no
#   NumFmt    - Number format string (e.g., "0", "0.00", or "")
#
# @param style_spec A data frame / tibble with the columns listed above.
# @return A named list of openxlsx Style objects, named by the ID column.
# ==============================================================================
create_dynamic_styles <- function(style_spec) {
  if (!is.data.frame(style_spec)) {
    cli::cli_abort("{.arg style_spec} must be a data frame.")
  }
  if (!"ID" %in% names(style_spec)) {
    cli::cli_abort("{.arg style_spec} must contain an {.val ID} column.")
  }
  if (nrow(style_spec) == 0) {
    cli::cli_warn("Empty style specification provided.")
    return(list())
  }

  # Helper: safely get column value, defaulting to NA if column missing
  safe_get <- function(row, col, default = NA) {
    if (col %in% names(row)) {
      val <- row[[col]]
      if (is.null(val) || (length(val) == 1 && is.na(val))) {
        return(default)
      }
      if (is.character(val) && nchar(trimws(val)) == 0) {
        return(default)
      }
      return(val)
    }
    default
  }

  result <- list()

  for (ri in seq_len(nrow(style_spec))) {
    row <- style_spec[ri, ]
    style_id <- as.character(row$ID)

    # --- Alignment parameters (SAS lines 842-849) ---
    ha_val     <- safe_get(row, "HA", NA_character_)
    va_val     <- safe_get(row, "VA", NA_character_)
    indent_val <- safe_get(row, "Indent", NA_real_)
    wrap_val   <- safe_get(row, "Wrap", NA_real_)
    rotate_val <- safe_get(row, "Rotate", NA_real_)

    halign_arg <- if (!is.na(ha_val)) tolower(ha_val) else NULL
    valign_arg <- if (!is.na(va_val)) tolower(va_val) else NULL
    wrap_arg   <- if (!is.na(wrap_val) && wrap_val == 1) TRUE else FALSE
    rotate_arg <- if (!is.na(rotate_val)) as.integer(rotate_val) else NULL
    indent_arg <- if (!is.na(indent_val)) as.integer(indent_val) else NULL

    # --- Border parameters (SAS lines 852-867) ---
    bt_val  <- safe_get(row, "BT", 0)
    bl_val  <- safe_get(row, "BL", 0)
    br_val  <- safe_get(row, "BR", 0)
    bb_val  <- safe_get(row, "BB", 0)
    bwt_val <- safe_get(row, "BWt", NA_real_)
    bls_val <- safe_get(row, "BLS", NA_character_)

    border_positions <- character(0)
    if (isTRUE(bt_val > 0)) border_positions <- c(border_positions, "Top")
    if (isTRUE(bl_val > 0)) border_positions <- c(border_positions, "Left")
    if (isTRUE(br_val > 0)) border_positions <- c(border_positions, "Right")
    if (isTRUE(bb_val > 0)) border_positions <- c(border_positions, "Bottom")

    border_arg <- if (length(border_positions) > 0) border_positions else NULL

    # Map SAS border weight to openxlsx border style
    border_style_arg <- NULL
    if (!is.null(border_arg)) {
      if (!is.na(bwt_val) && bwt_val >= 2) {
        border_style_arg <- "medium"
      } else {
        border_style_arg <- "thin"
      }
    }

    # --- Font parameters (SAS lines 869-876) ---
    font_size_val  <- safe_get(row, "FontSize", NA_real_)
    font_color_val <- safe_get(row, "FontColor", NA_character_)
    bold_val       <- safe_get(row, "Bold", 0)
    italic_val     <- safe_get(row, "Italic", 0)

    font_size_arg  <- if (!is.na(font_size_val)) as.integer(font_size_val) else NULL
    font_color_arg <- if (!is.na(font_color_val)) {
      # Prepend # if not already present (SAS stores without #)
      fc <- as.character(font_color_val)
      if (!startsWith(fc, "#")) paste0("#", fc) else fc
    } else {
      NULL
    }

    text_dec <- character(0)
    if (isTRUE(bold_val > 0))   text_dec <- c(text_dec, "bold")
    if (isTRUE(italic_val > 0)) text_dec <- c(text_dec, "italic")
    text_dec_arg <- if (length(text_dec) > 0) text_dec else NULL

    # --- Interior color (SAS lines 878-880) ---
    int_clr_val <- safe_get(row, "IntClr", NA_character_)
    fg_fill_arg <- if (!is.na(int_clr_val)) {
      ic <- as.character(int_clr_val)
      if (!startsWith(ic, "#")) paste0("#", ic) else ic
    } else {
      NULL
    }

    # --- Number format (SAS lines 882-884) ---
    num_fmt_val <- safe_get(row, "NumFmt", NA_character_)
    num_fmt_arg <- if (!is.na(num_fmt_val)) as.character(num_fmt_val) else NULL

    # --- Build the style ---
    style_args <- list()
    if (!is.null(font_size_arg))   style_args$fontSize       <- font_size_arg
    if (!is.null(font_color_arg))  style_args$fontColour     <- font_color_arg
    if (!is.null(text_dec_arg))    style_args$textDecoration  <- text_dec_arg
    if (!is.null(halign_arg))      style_args$halign          <- halign_arg
    if (!is.null(valign_arg))      style_args$valign          <- valign_arg
    if (isTRUE(wrap_arg))          style_args$wrapText        <- TRUE
    if (!is.null(rotate_arg))      style_args$textRotation    <- rotate_arg
    if (!is.null(indent_arg))      style_args$indent          <- indent_arg
    if (!is.null(border_arg))      style_args$border          <- border_arg
    if (!is.null(border_style_arg)) style_args$borderStyle    <- border_style_arg
    if (!is.null(fg_fill_arg))     style_args$fgFill          <- fg_fill_arg
    if (!is.null(num_fmt_arg))     style_args$numFmt          <- num_fmt_arg

    result[[style_id]] <- do.call(openxlsx::createStyle, style_args)
  }

  cli::cli_inform(
    "Dynamic styles created: {.val {length(result)}} style(s)."
  )

  result
}


# ==============================================================================
# MIGRATION NOTES
# ==============================================================================
# ASSUMPTIONS:
#    - openxlsx createStyle() maps all SAS SpreadsheetML style attributes
#      that are functionally relevant for Excel rendering.
#    - SAS "Solid" pattern -> openxlsx fgFill with color (pattern is implicit
#      in openxlsx; setting fgFill automatically fills the cell).
#    - SAS border Weight=1 -> openxlsx borderStyle="thin" (Weight=2 -> "medium").
#    - SAS font Size in points maps directly to openxlsx fontSize.
#    - SpreadsheetML column widths (in characters) are approximately equivalent
#      to openxlsx widths (openxlsx uses character-width units natively).
#    - SAS &strlen. global string length variable is not needed in R
#      (no fixed-width string buffers).
#    - SAS style inheritance (ss:Parent) is not supported by openxlsx;
#      all properties are specified explicitly on each style definition.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None. This module handles styling and formatting only; no statistical
#      computations are performed.
#
# NO DIRECT R EQUIVALENT:
#    - SAS SpreadsheetML XML generation via DATA _NULL_ / PUT statements
#      -> replaced entirely by openxlsx API (createWorkbook, writeData, etc.)
#    - SAS XML tag variables (Row, Data, Type, Formula, Height, Index,
#      MergeAcross, MergeDown, StyleID, Comment, Name, ArrayRange)
#      -> replaced by annotate_data() tibble columns and openxlsx direct
#         cell manipulation in write_annotated_data()
#    - SAS %markup: XML string concatenation with conditional ifc() attributes
#      -> replaced by openxlsx writeData/addStyle/mergeCells calls
#    - SAS %annotate: row-by-row long-format conversion via macro DO loop
#      -> replaced by R for-loop over columns producing a tibble
#    - SAS %xml_tag_def / %xml_init: variable declaration/initialization
#      -> not needed; tibble structure provides typed columns with NA defaults
#    - SAS tranwrd(compbl(string),' >','>') XML cleanup
#      -> not needed; openxlsx generates valid XML internally
#    - SAS %ws_rowcount: count XML <Row> tags in generated markup
#      -> not needed; row tracking is done via current_row counter in R
#
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: Direct Excel workbook manipulation replacing SpreadsheetML
#      XML generation; supports styles, merged cells, page setup, formulas.
#      AAP-mandated replacement for the SAS xml_output.sas macro system.
#    - dplyr: Data manipulation for annotated data processing, group_by,
#      filter, mutate, bind_rows. AAP mandates tidyverse over base R.
#    - cli: User-facing messages and error formatting replacing SAS %PUT
#      statements; provides consistent, informative diagnostic output.
#
# OPEN QUESTIONS:
#    - Verify openxlsx border style "thin" visually matches SAS Weight=1
#      in rendered Excel files.
#    - Confirm textRotation in openxlsx matches SAS Rotate="90" orientation
#      (both specify degrees counter-clockwise from horizontal).
#    - Verify fgFill behavior in openxlsx applies solid cell fill equivalent
#      to SAS Interior ss:Pattern="Solid" with ss:Color.
#    - Whether openxlsx supports all SpreadsheetML number formats identically
#      (e.g., "0.00E+00" scientific notation).
# ==============================================================================
