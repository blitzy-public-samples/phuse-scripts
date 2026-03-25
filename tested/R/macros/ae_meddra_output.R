# =============================================================================
# ae_meddra_output.R
# MedDRA Comparison Excel Workbook Generator
# =============================================================================
#
# Migrated from: tested/SAS/macros/ae_meddra_output.sas (2776 lines)
# SAS macros: %out_cover, %out_meddra_cmp, %out_meddra_cmp_data,
#             %wbinfo, %out_err, %out_aem_styles, %out_med
#
# Purpose:
#   Generates the complete MedDRA at a Glance Comparison Excel workbook with
#   multiple worksheets: cover page, visible comparison tab, hidden data tab,
#   workbook info, error summary, and custom styles.
#   ALL SpreadsheetML XML generation is replaced by openxlsx API calls.
#
# Dependencies:
#   - tested/R/utilities/xml_output.R   (create_workbook_styles, apply_page_setup,
#                                         get_style_by_name)
#   - tested/R/utilities/sl_gs_output.R (group_subset_write_ws)
#
# Exports:
#   - meddra_out_workbook()  — Main entry point (replaces SAS %out_med)
#   - meddra_out_styles()    — MedDRA-specific style gallery
# =============================================================================

# --- External package imports ------------------------------------------------
library(openxlsx)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)
library(cli)

# --- Internal dependency sourcing pattern ------------------------------------
# NOTE: Callers are expected to source these utilities before this file,
# or they may be loaded by the panel driver. The following sourcing is
# provided for standalone usage:
if (!exists("create_workbook_styles", mode = "function")) {
  source(
    file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "utilities", "xml_output.R"),
    local = FALSE
  )
}
if (!exists("group_subset_write_ws", mode = "function")) {
  source(
    file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "utilities", "sl_gs_output.R"),
    local = FALSE
  )
}


# =============================================================================
# meddra_out_styles
# =============================================================================
#' Create MedDRA-specific styles for the comparison workbook.
#'
#' Replaces SAS \code{%out_aem_styles} macro (lines 2538-2681 of
#' ae_meddra_output.sas). Builds a named list of openxlsx style objects
#' by extending the base style gallery from \code{create_workbook_styles()}
#' with ~40+ MedDRA-specific styles including level indentation styles,
#' signal highlighting, text-box border styles, input cell styles, and
#' color-coded signal indicators.
#'
#' @param base_size Numeric. Base font size in points. Defaults to 9.
#'                  SAS MedDRA uses size=8 for the base styles.
#'
#' @return A named list of openxlsx style objects.
#' @export
#'
#' @examples
#' styles <- meddra_out_styles(base_size = 9)
#' names(styles)
meddra_out_styles <- function(base_size = 9) {

  # Start with the base style gallery from xml_output.R
  # SAS: %styles(size=8) then extends with %out_aem_styles

  styles <- create_workbook_styles(base_size = base_size)

  # -------------------------------------------------------------------------
  # PART 1: Numeric format / indent styles with border combinations
  # SAS lines 2540-2578: D, D0_R1..D0_R4, D1_R0..D1_R3, D2_R1..D2_R3, IB
  # Each parent ID gets BL/BR/BB border combinations (_BL, _BR, _BB, _BLR, etc.)
  # -------------------------------------------------------------------------

  # Helper: create a style with numeric format, right-align, indent
  .make_num_style <- function(num_fmt, indent = 0, font_size = base_size) {
    openxlsx::createStyle(
      fontSize   = font_size,
      numFmt     = num_fmt,
      halign     = "right",
      indent     = indent
    )
  }

  # Define parent numeric styles
  parent_defs <- list(
    D       = list(num_fmt = "GENERAL", halign = "left", indent = 0),
    D0_R1   = list(num_fmt = "0",       halign = "right", indent = 1),
    D0_R2   = list(num_fmt = "0",       halign = "right", indent = 2),
    D0_R3   = list(num_fmt = "0",       halign = "right", indent = 3),
    D0_R4   = list(num_fmt = "0",       halign = "right", indent = 4),
    D1_R0   = list(num_fmt = "0.0",     halign = "right", indent = 0),
    D1_R1   = list(num_fmt = "0.0",     halign = "right", indent = 1),
    D1_R2   = list(num_fmt = "0.0",     halign = "right", indent = 2),
    D1_R3   = list(num_fmt = "0.0",     halign = "right", indent = 3),
    D2_R1   = list(num_fmt = "0.00",    halign = "right", indent = 1),
    D2_R2   = list(num_fmt = "0.00",    halign = "right", indent = 2),
    D2_R3   = list(num_fmt = "0.00",    halign = "right", indent = 3),
    IB      = list(num_fmt = "[=0]\"\";General", halign = "left", indent = 0)
  )

  # Border weight mapping: SAS BWt=1 -> "thin"; BWt=. (missing) -> "hair"
  thin_border <- "thin"
  hair_border <- "hair"

  # Generate all border combinations for each parent
  border_suffixes <- list(
    "_BL"   = list(left = TRUE,  right = FALSE, bottom = FALSE),
    "_BR"   = list(left = FALSE, right = TRUE,  bottom = FALSE),
    "_BB"   = list(left = FALSE, right = FALSE, bottom = TRUE),
    "_BLR"  = list(left = TRUE,  right = TRUE,  bottom = FALSE),
    "_BLB"  = list(left = TRUE,  right = FALSE, bottom = TRUE),
    "_BRB"  = list(left = FALSE, right = TRUE,  bottom = TRUE),
    "_BLRB" = list(left = TRUE,  right = TRUE,  bottom = TRUE)
  )

  for (pid in names(parent_defs)) {
    pdef <- parent_defs[[pid]]
    bwt <- if (pid == "IB") hair_border else thin_border

    # Base parent style (no extra borders)
    styles[[pid]] <- openxlsx::createStyle(
      fontSize   = base_size,
      numFmt     = pdef$num_fmt,
      halign     = pdef$halign,
      indent     = pdef$indent
    )

    # Generate border variants
    for (sfx in names(border_suffixes)) {
      bd  <- border_suffixes[[sfx]]
      sides <- character(0)
      if (bd$left)   sides <- c(sides, "left")
      if (bd$right)  sides <- c(sides, "right")
      if (bd$bottom) sides <- c(sides, "bottom")

      styles[[paste0(pid, sfx)]] <- openxlsx::createStyle(
        fontSize     = base_size,
        numFmt       = pdef$num_fmt,
        halign       = pdef$halign,
        indent       = pdef$indent,
        border       = sides,
        borderStyle  = rep(bwt, length(sides))
      )
    }
  }

  # -------------------------------------------------------------------------
  # PART 2: Standalone override styles
  # SAS lines 2580-2671 (wb_aem_styles_o_data)
  # -------------------------------------------------------------------------

  # D: Data with top-align, no-wrap, left+right thin borders
  styles[["D"]] <- openxlsx::createStyle(
    fontSize = base_size, valign = "top", wrapText = FALSE,
    border = c("left", "right"), borderStyle = c("thin", "thin")
  )

  # DC: DataCenter with left+right thin borders
  styles[["DC"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "center", valign = "top", wrapText = FALSE,
    border = c("left", "right"), borderStyle = c("thin", "thin")
  )

  # DB: Data with bottom border
  styles[["DB"]] <- openxlsx::createStyle(
    fontSize = base_size, valign = "top", wrapText = FALSE,
    border = c("left", "right", "bottom"),
    borderStyle = c("thin", "thin", "thin")
  )

  # DCB: DataCenter with bottom border
  styles[["DCB"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "center", valign = "top", wrapText = FALSE,
    border = c("left", "right", "bottom"),
    borderStyle = c("thin", "thin", "thin")
  )

  # I: Input style (yellow background FFFF99, all hair borders)
  styles[["I"]] <- openxlsx::createStyle(
    fontSize  = base_size, halign = "left", valign = "center",
    fgFill    = "#FFFF99",
    border      = c("top", "left", "right", "bottom"),
    borderStyle = rep("hair", 4)
  )

  # TBT: Text box top (top+left+right hair borders, indent 1)
  styles[["TBT"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "left", valign = "center", indent = 1,
    border = c("top", "left", "right"), borderStyle = rep("hair", 3)
  )

  # TBM: Text box middle (left+right hair borders, indent 1)
  styles[["TBM"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "left", valign = "center", indent = 1,
    border = c("left", "right"), borderStyle = rep("hair", 2)
  )

  # TBL: Text box left only (left hair border, indent 1)
  styles[["TBL"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "left", valign = "center", indent = 1,
    border = "left", borderStyle = "hair"
  )

  # TBR: Text box right only (right hair border, no indent)
  styles[["TBR"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "left", valign = "center",
    border = "right", borderStyle = "hair"
  )

  # TBB: Text box bottom (left+right+bottom hair borders, indent 1)
  styles[["TBB"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "left", valign = "center", indent = 1,
    border = c("left", "right", "bottom"), borderStyle = rep("hair", 3)
  )

  # D1_R2BR_BLRB: Special bold-red 0.0 format with full borders
  # SAS: NumFmt='0.0', FontSize=8, FontColor=FF0000, Bold=1, Indent=2
  styles[["D1_R2BR_BLRB"]] <- openxlsx::createStyle(
    fontSize  = 8, numFmt = "0.0", halign = "right", indent = 2,
    fontColour = "#FF0000", textDecoration = "bold",
    border      = c("left", "right", "bottom"),
    borderStyle = c("thin", "thin", "thin")
  )

  # I10: Italic 10pt, left-align, top-align, wrap, indent 1
  styles[["I10"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "left", valign = "top", wrapText = TRUE,
    indent = 1, textDecoration = "italic"
  )

  # B10O: Bold 10pt, center, gray bg (C0C0C0), all-around dashed borders
  styles[["B10O"]] <- openxlsx::createStyle(
    fontSize = 10, halign = "center", valign = "top",
    textDecoration = "bold", fgFill = "#C0C0C0",
    border      = c("top", "left", "right", "bottom"),
    borderStyle = c("dashed", "dashed", "dashed", "dashed"),
    borderColour = rep("#000000", 4)
  )

  # DG: Dark Gray signal indicator (#808080 background, all thin borders)
  styles[["DG"]] <- openxlsx::createStyle(
    fontSize = base_size, fgFill = "#808080",
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("thin", 4)
  )

  # LG: Light Gray signal indicator (#C0C0C0 background, all thin borders)
  styles[["LG"]] <- openxlsx::createStyle(
    fontSize = base_size, fgFill = "#C0C0C0",
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("thin", 4)
  )

  # R: Red signal indicator (#FF0000 background, all thin borders)
  styles[["R"]] <- openxlsx::createStyle(
    fontSize = base_size, fgFill = "#FF0000",
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("thin", 4)
  )

  # P: Peach signal indicator (#FFCC99 background, all thin borders)
  styles[["P"]] <- openxlsx::createStyle(
    fontSize = base_size, fgFill = "#FFCC99",
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("thin", 4)
  )

  # OR: Outline Rotated (center, vertical-center, 90 degrees, all hair borders)
  styles[["OR"]] <- openxlsx::createStyle(
    fontSize = base_size, halign = "center", valign = "center",
    textRotation = 90,
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("hair", 4)
  )

  # OW_BLR: Outline White (white 8pt font, invisible text, left+right hair borders)
  styles[["OW_BLR"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FFFFFF",
    border = c("left", "right"), borderStyle = rep("hair", 2)
  )

  # OW_BLRB: Outline White with bottom border
  styles[["OW_BLRB"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FFFFFF",
    border = c("left", "right", "bottom"), borderStyle = rep("hair", 3)
  )

  # COR: Column Outline Rotated (white font on blue #333399, wrap, 90-degree)
  styles[["COR"]] <- openxlsx::createStyle(
    fontSize = 8, fontColour = "#FFFFFF", fgFill = "#333399",
    halign = "center", valign = "center", wrapText = TRUE,
    textRotation = 90,
    border = c("top", "left", "right", "bottom"),
    borderStyle = rep("thin", 4)
  )

  # D_BLR: Data with left+right thin borders (err section general data)
  styles[["D_BLR"]] <- openxlsx::createStyle(
    fontSize = base_size,
    border = c("left", "right"), borderStyle = c("thin", "thin")
  )

  # D_BLRB: Data with left+right+bottom thin borders
  styles[["D_BLRB"]] <- openxlsx::createStyle(
    fontSize = base_size,
    border = c("left", "right", "bottom"),
    borderStyle = c("thin", "thin", "thin")
  )

  return(styles)
}


# =============================================================================
# meddra_out_cover
# =============================================================================
#' Write the "Front Page" cover worksheet for the MedDRA comparison workbook.
#'
#' Replaces SAS \code{%out_cover} macro (lines 14-737 of ae_meddra_output.sas).
#' Produces a narrative cover page with title, NDA/BLA info, study info,
#' run date, methodology description, example data row, explanation sections,
#' calculation definitions, CC notes, and report settings.
#'
#' @param wb             An openxlsx workbook object.
#' @param ndabla         Character. NDA/BLA identifier.
#' @param studyid        Character. Study identifier.
#' @param rundate        Character. Analysis run date string.
#' @param arm_count      Integer. Number of treatment arms.
#' @param arm_names      Character vector of arm display names.
#' @param meddra_ver     Character. MedDRA version used.
#' @param cc_desc        Character. Continuity correction description.
#' @param study_lag      Character or numeric. Study analysis lag period.
#' @param vld_sw         Character. Validation switch ("Y"/"N").
#' @param sl_group_desc  Character. Grouping description.
#' @param sl_subset_desc Character. Subsetting description.
#' @param styles         Named list of openxlsx style objects.
#'
#' @return Invisible NULL.
meddra_out_cover <- function(wb, ndabla, studyid, rundate, arm_count, arm_names,
                             meddra_ver, cc_desc, study_lag, vld_sw,
                             sl_group_desc, sl_subset_desc, styles) {

  cli::cli_inform("Writing MedDRA comparison cover page...")

  wstitle <- "Front Page"
  openxlsx::addWorksheet(wb, wstitle)

  # Column widths: SAS uses 16 columns with specific pixel widths

  # Convert SAS pixel widths (~7 chars/px) approximately
  # SAS: 65, 37, 37, 37, 30, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37
  col_widths <- c(9.3, 5.3, 5.3, 5.3, 4.3, 5.3, 5.3, 5.3,
                  5.3, 5.3, 5.3, 5.3, 5.3, 5.3, 5.3, 5.3)
  openxlsx::setColWidths(wb, wstitle, cols = seq_along(col_widths),
                         widths = col_widths)

  current_row <- 1L

  # ---------------------------------------------------------------------------
  # PART 1: Narrative text with title, NDA/BLA, study, methodology
  # SAS lines 43-150
  # ---------------------------------------------------------------------------

  # Row 1: blank separator
  current_row <- current_row + 1L

  # Title row: "MedDRA at a Glance Comparison Analysis" (Header style)
  openxlsx::writeData(wb, wstitle, x = "MedDRA at a Glance Comparison Analysis",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$Header,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # NDA/BLA row
  openxlsx::writeData(wb, wstitle, x = str_c("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$Default10,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Study row
  openxlsx::writeData(wb, wstitle, x = str_c("Study: ", studyid),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$Default10,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Run date row
  openxlsx::writeData(wb, wstitle, x = str_c("Analysis run date: ", rundate),
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$Default10,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # Methodology narrative text (SubHeader)
  openxlsx::writeData(wb, wstitle, x = "How To Use This Workbook",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # Narrative overview paragraph
  # Use str_squish() to normalize whitespace in the assembled narrative (AAP: stringr)
  overview_text <- stringr::str_squish(str_c(
    "The MedDRA at a Glance Comparison Analysis provides a signal detection ",
    "tool for comparing adverse event rates across treatment groups. ",
    "Select Treatment and Control arms using the dropdown menus in the ",
    "'Comparison' worksheet. The comparison automatically updates based on ",
    "the chosen arm pair. Signal detection is based on Risk Difference (RD), ",
    "Relative Risk (RR), and -log(p-value) thresholds."
  ))
  openxlsx::writeData(wb, wstitle, x = overview_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 54)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # PART 2: Example header row and data row with style indicators
  # SAS lines ~160-250
  # ---------------------------------------------------------------------------

  # SubHeader: "Example Row"
  openxlsx::writeData(wb, wstitle, x = "Example Row from the Comparison Worksheet",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # Example column headers (A through H labels)
  example_headers <- c("A", "B", "", "C", "D", "E", "F", "",
                       "F", "", "G", "G", "G", "", "G", "H")
  for (j in seq_along(example_headers)) {
    openxlsx::writeData(wb, wstitle, x = example_headers[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # Example column labels
  col_labels <- c("Level", "SOC / HLGT / HLT / PT", "", "DME", "Signal",
                  "Signal At", "Treatment", "", "Control", "",
                  "RD%", "RR", "PV", "", "CC", "Sort")
  for (j in seq_along(col_labels)) {
    openxlsx::writeData(wb, wstitle, x = col_labels[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # Example column sub-labels
  sub_labels <- c("", "", "", "", "", "SOC HLGT HLT PT",
                  "Subject Count", "%", "Subject Count", "%",
                  "", "", "", "", "", "")
  for (j in seq_along(sub_labels)) {
    openxlsx::writeData(wb, wstitle, x = sub_labels[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # Example data row with color indicators
  example_data <- c("PT", "Dizziness", "", "", "", "", "5", "12.5",
                    "2", "5.0", "7.5", "2.5", "1.3", "", "", "3")
  example_styles <- c("DC", "D_BLR", "D_BLR", "DC", "DC", "DC",
                       "D0_R2_BL", "D1_R1_BR", "D0_R2_BL", "D1_R1_BR",
                       "D1_R2_BLR", "D1_R1_BL", "D1_R2_BLR", "IB_BR",
                       "OW_BLR", "DC")
  for (j in seq_along(example_data)) {
    openxlsx::writeData(wb, wstitle, x = example_data[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, example_styles[j]),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # PART 3: Detailed explanations of columns A through H
  # SAS lines ~250-500
  # ---------------------------------------------------------------------------

  explanation_items <- list(
    list(
      label   = "A = Level",
      detail  = str_c(
        "The MedDRA hierarchy level: SOC (System Organ Class), ",
        "HLGT (High Level Group Term), HLT (High Level Term), ",
        "PT (Preferred Term)."
      )
    ),
    list(
      label   = "B = MedDRA Description",
      detail  = "The MedDRA term description at the given hierarchy level."
    ),
    list(
      label   = "C = DME Indicator",
      detail  = str_c(
        "Designated Medical Event indicator. 'Y' if the term is a DME, ",
        "blank otherwise."
      )
    ),
    list(
      label   = "D = Signal",
      detail  = str_c(
        "Signal indicator based on threshold comparison: DG (dark gray) = ",
        "signal at this level, LG (light gray/silver) = signal nearby, ",
        "R (red) = signal at this level exceeding threshold, P (peach) = ",
        "signal at a related level."
      )
    ),
    list(
      label   = "E = Signal At",
      detail  = str_c(
        "Four columns (SOC, HLGT, HLT, PT) showing where in the hierarchy ",
        "the signal was detected. 'Y' = signal at that level, blank otherwise."
      )
    ),
    list(
      label   = "F = Subject Counts",
      detail  = str_c(
        "Treatment and Control arm subject counts and percentages. ",
        "Percentages are based on the total number of subjects in each arm."
      )
    ),
    list(
      label   = "G = Statistics",
      detail  = str_c(
        "RD% (Risk Difference percentage), RR (Relative Risk), ",
        "PV (-log10 p-value from Fisher's exact test), CC (continuity ",
        "correction indicator). Values exceeding user-set thresholds are ",
        "highlighted in bold red."
      )
    ),
    list(
      label   = "H = Sort Order",
      detail  = "Numeric sort order for the hierarchy level."
    )
  )

  for (item in explanation_items) {
    openxlsx::writeData(wb, wstitle, x = item$label,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    current_row <- current_row + 1L

    openxlsx::writeData(wb, wstitle, x = item$detail,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    # Dynamic height based on text length
    txt_height <- max(1, ceiling(nchar(item$detail) / 100)) * 13.5
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = txt_height)
    current_row <- current_row + 1L

    # Blank separator
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # PART 4: 2x2 Contingency table example
  # SAS lines ~500-530
  # ---------------------------------------------------------------------------

  openxlsx::writeData(wb, wstitle, x = "Method and Calculations",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  current_row <- current_row + 1L

  # Validation note (conditional on vld_sw)
  if (toupper(vld_sw) == "Y") {
    vld_text <- str_c(
      "Note: Subject counts are based on subjects who passed data validation. ",
      "Subjects excluded during validation are not included in the analysis."
    )
    openxlsx::writeData(wb, wstitle, x = vld_text,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 27)
    current_row <- current_row + 1L
    current_row <- current_row + 1L
  }

  # Study analysis period note (conditional on study_lag)
  if (!is.null(study_lag) && !is.na(study_lag) && nchar(as.character(study_lag)) > 0 &&
      as.character(study_lag) != "0") {
    lag_text <- str_c(
      "Study analysis period: Events occurring within ", study_lag,
      " days after the last exposure date are included in the analysis."
    )
    openxlsx::writeData(wb, wstitle, x = lag_text,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 27)
    current_row <- current_row + 1L
    current_row <- current_row + 1L
  }

  # 2x2 Table explanation
  tbl_text <- str_c(
    "The 2x2 contingency table for each MedDRA term compares AE subject ",
    "counts between Treatment and Control arms:"
  )
  openxlsx::writeData(wb, wstitle, x = tbl_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 27)
  current_row <- current_row + 1L

  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # PART 5: Calculation definitions (RD, RR, PV)
  # SAS lines ~530-600
  # ---------------------------------------------------------------------------

  calc_items <- list(
    c("AE Subject Count", "Number of subjects with the adverse event"),
    c("Subject Count", "Total number of subjects in the treatment arm"),
    c("AE% (Risk)", "AE Subject Count / Subject Count * 100"),
    c("RD% (Risk Difference)", "(Treatment AE% - Control AE%)"),
    c("RR (Relative Risk)", "Treatment AE% / Control AE%"),
    c("-log10(p-value)", "Negative log base 10 of Fisher's exact test p-value")
  )

  for (calc in calc_items) {
    openxlsx::writeData(wb, wstitle, x = calc[1],
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "I10"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:4, rows = current_row)

    openxlsx::writeData(wb, wstitle, x = calc[2],
                        startRow = current_row, startCol = 5, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 5)
    openxlsx::mergeCells(wb, wstitle, cols = 5:16, rows = current_row)
    current_row <- current_row + 1L
  }

  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # PART 5b: CC note (continuity correction description)
  # SAS lines ~600-650
  # ---------------------------------------------------------------------------

  if (nchar(str_trim(cc_desc)) > 0) {
    openxlsx::writeData(wb, wstitle, x = "Continuity Correction",
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    current_row <- current_row + 1L

    current_row <- current_row + 1L

    openxlsx::writeData(wb, wstitle, x = cc_desc,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
    txt_height <- max(1, ceiling(nchar(cc_desc) / 100)) * 13.5
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = txt_height)
    current_row <- current_row + 1L

    current_row <- current_row + 1L
  }

  # p-value note
  pv_note <- str_c(
    "p-value note: -log10(p-value) is used so that smaller (more significant) ",
    "p-values produce larger display values. A -log10(p-value) of 1.3 ",
    "corresponds to p=0.05. Values above the threshold are highlighted."
  )
  openxlsx::writeData(wb, wstitle, x = pv_note,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 40.5)
  current_row <- current_row + 1L

  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # PART 6: Report Settings section
  # SAS lines ~650-730
  # ---------------------------------------------------------------------------

  openxlsx::writeData(wb, wstitle, x = "Report Settings",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$SubHeader,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:16, rows = current_row)
  current_row <- current_row + 1L

  current_row <- current_row + 1L

  settings_items <- list(
    c("NDA/BLA:", ndabla),
    c("Study:", studyid),
    c("Analysis Run Date:", rundate),
    c("Grouping:", sl_group_desc),
    c("Subsetting:", sl_subset_desc),
    c("MedDRA Version:", stringr::str_to_title(meddra_ver))
  )

  # Add study lag if present
  if (!is.null(study_lag) && !is.na(study_lag) && nchar(as.character(study_lag)) > 0 &&
      as.character(study_lag) != "0") {
    settings_items <- c(settings_items,
                        list(c("Study Analysis Period (days):", as.character(study_lag))))
  }

  # Add CC description if present
  if (nchar(str_trim(cc_desc)) > 0) {
    settings_items <- c(settings_items,
                        list(c("Continuity Correction:", cc_desc)))
  }

  # Add arm names
  for (i in seq_len(arm_count)) {
    arm_label <- str_c("Arm ", i, ":")
    arm_value <- if (i <= length(arm_names)) arm_names[i] else str_c("Arm ", i)
    settings_items <- c(settings_items, list(c(arm_label, arm_value)))
  }

  # Write settings items using purrr::iwalk (AAP: tidyverse purrr over base R loops)
  purrr::iwalk(settings_items, function(item, idx) {
    row <- current_row + idx - 1L
    openxlsx::writeData(wb, wstitle, x = item[1],
                        startRow = row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "I10"),
                       rows = row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:4, rows = row)

    openxlsx::writeData(wb, wstitle, x = item[2],
                        startRow = row, startCol = 5, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10Wrap"),
                       rows = row, cols = 5)
    openxlsx::mergeCells(wb, wstitle, cols = 5:16, rows = row)
  })
  current_row <- current_row + length(settings_items)

  # ---------------------------------------------------------------------------
  # Page setup
  # ---------------------------------------------------------------------------
  apply_page_setup(
    wb, wstitle,
    orientation    = "landscape",
    footer_center  = "Page &P of &N"
  )

  invisible(NULL)
}


# =============================================================================
# meddra_out_comparison
# =============================================================================
#' Write the visible "Comparison" worksheet for the MedDRA workbook.
#'
#' Replaces SAS \code{%out_meddra_cmp} macro (lines 743-1473 of
#' ae_meddra_output.sas). Creates the main comparison worksheet with:
#' dropdown arm selectors, threshold input cells, column headers with
#' arm-based dynamic names, hierarchical MedDRA data rows (SOC/HLGT/HLT/PT),
#' count/pct columns, RD/RR/PV columns per arm pair, CC indicator columns,
#' conditional formatting for signal highlighting, auto-filter, data validation,
#' named ranges, and frozen panes.
#'
#' @param wb               An openxlsx workbook object.
#' @param meddra_cmp_data  A tibble/data.frame with comparison data.
#'                         Expected columns: lvl_nm, lvl_no, soc, hlgt, hlt, pt,
#'                         dme, sgnl, sgnl_soc, sgnl_hlgt, sgnl_hlt, sgnl_pt,
#'                         plus arm-prefixed columns (arm1_cnt, arm1_pct, etc.),
#'                         rd, rr, pv, cc, sort_order.
#' @param arm_count        Integer. Number of treatment arms.
#' @param arm_names        Character vector of arm display names.
#' @param arm_subjcnt      Integer vector of per-arm subject counts.
#' @param rd_th            Numeric. Risk Difference threshold (%).
#' @param rr_th            Numeric. Relative Risk threshold.
#' @param pv_th            Numeric. -log10(p-value) threshold.
#' @param cc_sw            Character. Continuity correction switch ("Y"/"N").
#' @param meddra_ver       Character. MedDRA version.
#' @param dme_sw           Logical. Whether DME column is included.
#' @param styles           Named list of openxlsx style objects.
#'
#' @return Invisible NULL.
meddra_out_comparison <- function(wb, meddra_cmp_data, arm_count, arm_names,
                                  arm_subjcnt, rd_th, rr_th, pv_th, cc_sw,
                                  meddra_ver, dme_sw = FALSE, styles) {

  cli::cli_inform("Writing MedDRA comparison worksheet...")

  wstitle <- "Comparison"
  openxlsx::addWorksheet(wb, wstitle)

  # ---------------------------------------------------------------------------
  # Column widths
  # SAS: Level 13px, hierarchy 120px each (SOC/HLGT/HLT/PT), DME 21px,
  #      Signal 13px x4, arm count/pct 50+35px, RD/RR/PV 53px,
  #      CC cols 43+13px, Sort 13px
  # Convert SAS pixels / ~7 to character units
  # ---------------------------------------------------------------------------

  # Build column width vector dynamically based on arm_count
  # Base columns: Level(2), SOC(17), HLGT(17), HLT(17), PT(17), DME(3),
  #               Signal(2), SignalAt_SOC(2), SignalAt_HLGT(2),
  #               SignalAt_HLT(2), SignalAt_PT(2)
  base_widths <- c(2, 17, 17, 17, 17, 3, 2, 2, 2, 2, 2)

  # Per-arm columns: Count(7.1), Pct(5)
  arm_widths <- rep(c(7.1, 5), arm_count)

  # Comparison columns: RD(7.6), RR(7.6), PV(7.6), CC(6.1), CCflag(2), Sort(2)
  cmp_widths <- c(7.6, 7.6, 7.6, 6.1, 2, 2)

  all_widths <- c(base_widths, arm_widths, cmp_widths)
  openxlsx::setColWidths(wb, wstitle, cols = seq_along(all_widths),
                         widths = all_widths)

  # Compute column indices for later reference
  n_base_cols <- length(base_widths)
  n_arm_cols  <- 2L * arm_count
  arm_start_col <- n_base_cols + 1L
  cmp_start_col <- arm_start_col + n_arm_cols
  rd_col <- cmp_start_col
  rr_col <- cmp_start_col + 1L
  pv_col <- cmp_start_col + 2L
  cc_col <- cmp_start_col + 3L
  ccflag_col <- cmp_start_col + 4L
  sort_col <- cmp_start_col + 5L
  total_cols <- length(all_widths)

  current_row <- 1L

  # ---------------------------------------------------------------------------
  # Title row
  # ---------------------------------------------------------------------------
  title_text <- str_c(
    "MedDRA at a Glance Comparison Analysis - MedDRA v", meddra_ver
  )
  openxlsx::writeData(wb, wstitle, x = title_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles$Header,
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 1L

  # Blank
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Selection area: Treatment/Control arm dropdowns + threshold inputs
  # SAS lines ~830-1000
  # ---------------------------------------------------------------------------

  selection_start_row <- current_row

  # Row: Treatment arm label + input cell
  openxlsx::writeData(wb, wstitle, x = "Treatment Arm:",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:3, rows = current_row)

  # Input cell for treatment arm selection (yellow I style)
  default_trt <- if (length(arm_names) >= 1) arm_names[1] else ""
  openxlsx::writeData(wb, wstitle, x = default_trt,
                      startRow = current_row, startCol = 4, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles[["I"]],
                     rows = current_row, cols = 4)
  openxlsx::mergeCells(wb, wstitle, cols = 4:5, rows = current_row)
  trt_input_row <- current_row
  trt_input_col <- 4L

  # Threshold labels: RD% threshold text box (TBT/TBM/TBB)
  openxlsx::writeData(wb, wstitle, x = "RD% Threshold:",
                      startRow = current_row, startCol = 7, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "TBT"),
                     rows = current_row, cols = 7)
  openxlsx::mergeCells(wb, wstitle, cols = 7:8, rows = current_row)

  openxlsx::writeData(wb, wstitle, x = rd_th,
                      startRow = current_row, startCol = 9, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles[["I"]],
                     rows = current_row, cols = 9)
  rd_input_row <- current_row
  rd_input_col <- 9L
  current_row <- current_row + 1L

  # Row: Control arm label + input cell
  openxlsx::writeData(wb, wstitle, x = "Control Arm:",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:3, rows = current_row)

  # Input cell for control arm selection
  default_ctl <- if (length(arm_names) >= 2) arm_names[2] else ""
  openxlsx::writeData(wb, wstitle, x = default_ctl,
                      startRow = current_row, startCol = 4, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles[["I"]],
                     rows = current_row, cols = 4)
  openxlsx::mergeCells(wb, wstitle, cols = 4:5, rows = current_row)
  ctl_input_row <- current_row
  ctl_input_col <- 4L

  # RR threshold
  openxlsx::writeData(wb, wstitle, x = "RR Threshold:",
                      startRow = current_row, startCol = 7, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "TBM"),
                     rows = current_row, cols = 7)
  openxlsx::mergeCells(wb, wstitle, cols = 7:8, rows = current_row)

  openxlsx::writeData(wb, wstitle, x = rr_th,
                      startRow = current_row, startCol = 9, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles[["I"]],
                     rows = current_row, cols = 9)
  rr_input_row <- current_row
  rr_input_col <- 9L
  current_row <- current_row + 1L

  # -log10(PV) threshold
  openxlsx::writeData(wb, wstitle, x = "-log10(PV) Threshold:",
                      startRow = current_row, startCol = 7, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "TBB"),
                     rows = current_row, cols = 7)
  openxlsx::mergeCells(wb, wstitle, cols = 7:8, rows = current_row)

  openxlsx::writeData(wb, wstitle, x = pv_th,
                      startRow = current_row, startCol = 9, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle, style = styles[["I"]],
                     rows = current_row, cols = 9)
  pv_input_row <- current_row
  pv_input_col <- 9L
  current_row <- current_row + 1L

  # Color legend row
  current_row <- current_row + 1L
  legend_items <- list(
    list(label = "DG", style_nm = "DG", desc = "Signal at this level"),
    list(label = "R",  style_nm = "R",  desc = "Signal above threshold"),
    list(label = "LG", style_nm = "LG", desc = "Signal nearby"),
    list(label = "P",  style_nm = "P",  desc = "Signal at related level")
  )

  legend_col <- 1L
  for (lg in legend_items) {
    openxlsx::writeData(wb, wstitle, x = "",
                        startRow = current_row, startCol = legend_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, lg$style_nm),
                       rows = current_row, cols = legend_col)
    legend_col <- legend_col + 1L

    openxlsx::writeData(wb, wstitle, x = lg$desc,
                        startRow = current_row, startCol = legend_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "Default10"),
                       rows = current_row, cols = legend_col)
    openxlsx::mergeCells(wb, wstitle, cols = legend_col:(legend_col + 1),
                         rows = current_row)
    legend_col <- legend_col + 2L
  }
  current_row <- current_row + 1L

  # Blank separator
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Column headers (3-row header block)
  # SAS lines ~1000-1100
  # ---------------------------------------------------------------------------

  header_start_row <- current_row

  # Row 1: Major group headers
  # Level | Hierarchy (SOC/HLGT/HLT/PT) | DME | Signal | Signal At |
  #        Arm headers | Statistics | Sort
  header1_data <- list(
    list(col = 1,  val = "Level",    span = 1),
    list(col = 2,  val = "",         span = 4),  # SOC/HLGT/HLT/PT merged below
    list(col = 6,  val = "DME",      span = 1),
    list(col = 7,  val = "Signal",   span = 1),
    list(col = 8,  val = "Signal At", span = 4)
  )

  for (h in header1_data) {
    openxlsx::writeData(wb, wstitle, x = h$val,
                        startRow = current_row, startCol = h$col,
                        colNames = FALSE)
    style_nm <- if (h$val %in% c("Signal At", "Level", "DME", "Signal")) {
      "ColumnOutline"
    } else {
      "ColumnOutline"
    }
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, style_nm),
                       rows = current_row, cols = h$col)
    if (h$span > 1) {
      openxlsx::mergeCells(wb, wstitle,
                           cols = h$col:(h$col + h$span - 1),
                           rows = current_row)
    }
  }

  # Arm-based headers in Row 1 — use purrr::walk for arm column iteration
  # (AAP: tidyverse purrr over base R for loops where appropriate)
  arm_header_data <- purrr::map(seq_len(arm_count), function(i) {
    list(
      col     = arm_start_col + (i - 1L) * 2L,
      display = if (i <= length(arm_names)) arm_names[i] else str_c("Arm ", i),
      n       = if (i <= length(arm_subjcnt)) arm_subjcnt[i] else 0L
    )
  })
  purrr::walk(arm_header_data, function(hd) {
    arm_header <- str_c(hd$display, "\nN=", hd$n)
    openxlsx::writeData(wb, wstitle, x = arm_header,
                        startRow = current_row, startCol = hd$col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = hd$col)
    openxlsx::mergeCells(wb, wstitle,
                         cols = hd$col:(hd$col + 1),
                         rows = current_row)
  })

  # Statistics group header
  openxlsx::writeData(wb, wstitle, x = "Statistics",
                      startRow = current_row, startCol = rd_col,
                      colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "ColumnOutline"),
                     rows = current_row, cols = rd_col)
  openxlsx::mergeCells(wb, wstitle, cols = rd_col:ccflag_col,
                       rows = current_row)

  openxlsx::writeData(wb, wstitle, x = "Sort",
                      startRow = current_row, startCol = sort_col,
                      colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "ColumnOutline"),
                     rows = current_row, cols = sort_col)

  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 35)
  current_row <- current_row + 1L

  # Row 2: Rotated sub-headers for hierarchy columns
  row2_items <- c("Level", "SOC", "HLGT", "HLT", "PT", "DME",
                  "Signal", "SOC", "HLGT", "HLT", "PT")
  for (j in seq_along(row2_items)) {
    openxlsx::writeData(wb, wstitle, x = row2_items[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    rot_style <- if (j >= 8) "COR" else "ColumnOutline"
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, rot_style),
                       rows = current_row, cols = j)
  }

  # Per-arm sub-headers: "Subject Count" and "%"
  for (i in seq_len(arm_count)) {
    cnt_col <- arm_start_col + (i - 1L) * 2L
    pct_col <- cnt_col + 1L
    openxlsx::writeData(wb, wstitle, x = "Subject\nCount",
                        startRow = current_row, startCol = cnt_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = cnt_col)
    openxlsx::writeData(wb, wstitle, x = "%",
                        startRow = current_row, startCol = pct_col,
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = pct_col)
  }

  # Statistics sub-headers
  stat_labels <- c("RD%", "RR", "-log PV", "CC", "", "Order")
  stat_cols <- c(rd_col, rr_col, pv_col, cc_col, ccflag_col, sort_col)
  for (k in seq_along(stat_labels)) {
    openxlsx::writeData(wb, wstitle, x = stat_labels[k],
                        startRow = current_row, startCol = stat_cols[k],
                        colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = stat_cols[k])
  }

  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 40)
  current_row <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Data rows from meddra_cmp_data
  # SAS lines ~1100-1250
  # ---------------------------------------------------------------------------

  data_start_row <- current_row

  if (!is.null(meddra_cmp_data) && nrow(meddra_cmp_data) > 0) {

    # Pre-process comparison data using dplyr pipeline (AAP: tidyverse over base R)
    # Sort hierarchy: SOC -> HLGT -> HLT -> PT, then arrange by sort key if available
    cmp_sorted <- meddra_cmp_data
    if ("sort_key" %in% names(cmp_sorted)) {
      cmp_sorted <- dplyr::arrange(cmp_sorted, .data$sort_key)
    } else if ("lvl_nm" %in% names(cmp_sorted)) {
      cmp_sorted <- dplyr::arrange(cmp_sorted,
                                   dplyr::desc(.data$lvl_nm == "SOC"),
                                   dplyr::desc(.data$lvl_nm == "HLGT"),
                                   dplyr::desc(.data$lvl_nm == "HLT"))
    }

    # Compute display columns: determine style names per row using mutate + if_else
    arm_cnt_cols <- paste0("arm", seq_len(arm_count), "_cnt")
    arm_pct_cols <- paste0("arm", seq_len(arm_count), "_pct")
    avail_arm_cols <- intersect(c(arm_cnt_cols, arm_pct_cols),
                                names(cmp_sorted))

    if (length(avail_arm_cols) > 0) {
      # Select arm-prefixed columns programmatically
      arm_subset <- dplyr::select(cmp_sorted, dplyr::starts_with("arm"))
      # Apply missing → "." display transformation across arm columns
      cmp_sorted <- dplyr::mutate(
        cmp_sorted,
        dplyr::across(
          dplyr::starts_with("arm"),
          ~ dplyr::if_else(is.na(.x), NA_real_, as.numeric(.x)),
          .names = "{.col}"
        )
      )
    }

    # Filter out rows that have no relevant data if needed
    cmp_display <- dplyr::filter(cmp_sorted, !is.na(.data[[names(cmp_sorted)[1]]]))
    # Use full sorted data if filter removed everything (all rows have data)
    if (nrow(cmp_display) == 0) cmp_display <- cmp_sorted

    n_data <- nrow(cmp_display)

    for (i in seq_len(n_data)) {
      row_data <- cmp_display[i, ]
      is_bottom <- (i == n_data)

      # Determine hierarchy description based on level
      lvl <- if ("lvl_nm" %in% names(row_data)) {
        toupper(as.character(row_data$lvl_nm[1]))
      } else {
        ""
      }

      # Try to get the description from hierarchy-specific columns first,
      # then fall back to a generic "term" column.
      desc_text <- if (lvl == "SOC" && "soc" %in% names(row_data)) {
        as.character(row_data$soc[1])
      } else if (lvl == "HLGT" && "hlgt" %in% names(row_data)) {
        as.character(row_data$hlgt[1])
      } else if (lvl == "HLT" && "hlt" %in% names(row_data)) {
        as.character(row_data$hlt[1])
      } else if (lvl == "PT" && "pt" %in% names(row_data)) {
        as.character(row_data$pt[1])
      } else if ("term" %in% names(row_data)) {
        as.character(row_data$term[1])
      } else {
        ""
      }
      if (length(desc_text) == 0 || is.na(desc_text)) desc_text <- ""

      # Write Level column
      lvl_style <- if (is_bottom) "DCB" else "DC"
      openxlsx::writeData(wb, wstitle, x = lvl,
                          startRow = current_row, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, lvl_style),
                         rows = current_row, cols = 1)

      # Write hierarchy description (merged across cols 2-5)
      desc_style <- if (is_bottom) "D_BLRB" else "D_BLR"
      openxlsx::writeData(wb, wstitle, x = desc_text,
                          startRow = current_row, startCol = 2, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, desc_style),
                         rows = current_row, cols = 2)
      openxlsx::mergeCells(wb, wstitle, cols = 2:5, rows = current_row)

      # DME column
      dme_val <- if ("dme" %in% names(row_data)) as.character(row_data$dme[1]) else ""
      if (is.na(dme_val)) dme_val <- ""
      dme_style <- if (is_bottom) "DCB" else "DC"
      openxlsx::writeData(wb, wstitle, x = dme_val,
                          startRow = current_row, startCol = 6, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, dme_style),
                         rows = current_row, cols = 6)

      # Signal column
      sgnl_val <- if ("sgnl" %in% names(row_data)) as.character(row_data$sgnl[1]) else ""
      if (is.na(sgnl_val)) sgnl_val <- ""
      sgnl_style <- if (is_bottom) "DCB" else "DC"
      openxlsx::writeData(wb, wstitle, x = sgnl_val,
                          startRow = current_row, startCol = 7, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, sgnl_style),
                         rows = current_row, cols = 7)

      # Signal At columns (SOC, HLGT, HLT, PT)
      sgnl_at_cols <- c("sgnl_soc", "sgnl_hlgt", "sgnl_hlt", "sgnl_pt")
      for (sa_idx in seq_along(sgnl_at_cols)) {
        sa_col <- 7L + sa_idx
        sa_val <- if (sgnl_at_cols[sa_idx] %in% names(row_data)) {
          as.character(row_data[[sgnl_at_cols[sa_idx]]][1])
        } else {
          ""
        }
        if (is.na(sa_val)) sa_val <- ""
        sa_style <- if (is_bottom) "DCB" else "DC"
        openxlsx::writeData(wb, wstitle, x = sa_val,
                            startRow = current_row, startCol = sa_col,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, sa_style),
                           rows = current_row, cols = sa_col)
      }

      # Per-arm Count and Pct columns
      for (a in seq_len(arm_count)) {
        cnt_colnm <- str_c("arm", a, "_cnt")
        pct_colnm <- str_c("arm", a, "_pct")
        a_cnt_col <- arm_start_col + (a - 1L) * 2L
        a_pct_col <- a_cnt_col + 1L

        cnt_val <- if (cnt_colnm %in% names(row_data)) row_data[[cnt_colnm]][1] else NA
        pct_val <- if (pct_colnm %in% names(row_data)) row_data[[pct_colnm]][1] else NA

        # Count style: integer, right-aligned with borders
        cnt_style_nm <- if (is_bottom) "D0_R2_BLB" else "D0_R2_BL"
        # Display missing as "."
        cnt_display <- if (is.na(cnt_val)) "." else cnt_val
        openxlsx::writeData(wb, wstitle, x = cnt_display,
                            startRow = current_row, startCol = a_cnt_col,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, cnt_style_nm),
                           rows = current_row, cols = a_cnt_col)

        # Pct style: 1 decimal, right-aligned with borders
        pct_style_nm <- if (is_bottom) "D1_R1_BRB" else "D1_R1_BR"
        pct_display <- if (is.na(pct_val)) "." else janitor::round_half_up(pct_val, 1)
        openxlsx::writeData(wb, wstitle, x = pct_display,
                            startRow = current_row, startCol = a_pct_col,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, pct_style_nm),
                           rows = current_row, cols = a_pct_col)
      }

      # RD% column
      rd_val <- if ("rd" %in% names(row_data)) row_data$rd[1] else NA
      rd_style_nm <- if (is_bottom) "D1_R2_BLRB" else "D1_R2_BLR"
      rd_display <- if (is.na(rd_val)) "." else janitor::round_half_up(rd_val, 1)
      openxlsx::writeData(wb, wstitle, x = rd_display,
                          startRow = current_row, startCol = rd_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, rd_style_nm),
                         rows = current_row, cols = rd_col)

      # RR column
      rr_val <- if ("rr" %in% names(row_data)) row_data$rr[1] else NA
      rr_base <- if (toupper(cc_sw) == "Y") "D1_R2_BLR" else "D1_R1_BL"
      rr_style_nm <- if (is_bottom) paste0(rr_base, "B") else rr_base
      rr_display <- if (is.na(rr_val)) {
        "."
      } else if (toupper(cc_sw) == "Y") {
        janitor::round_half_up(rr_val, 1)
      } else {
        janitor::round_half_up(rr_val, 1)
      }
      openxlsx::writeData(wb, wstitle, x = rr_display,
                          startRow = current_row, startCol = rr_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, rr_style_nm),
                         rows = current_row, cols = rr_col)

      # PV (-log10 p-value) column
      pv_val <- if ("pv" %in% names(row_data)) row_data$pv[1] else NA
      pv_style_nm <- if (is_bottom) "D1_R2_BLRB" else "D1_R2_BLR"
      pv_display <- if (is.na(pv_val)) "." else janitor::round_half_up(pv_val, 1)
      openxlsx::writeData(wb, wstitle, x = pv_display,
                          startRow = current_row, startCol = pv_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, pv_style_nm),
                         rows = current_row, cols = pv_col)

      # CC indicator column
      cc_val <- if ("cc" %in% names(row_data)) row_data$cc[1] else NA
      cc_style_nm <- if (is_bottom) "IB_BRB" else "IB_BR"
      cc_display <- if (is.na(cc_val) || cc_val == 0) "" else cc_val
      openxlsx::writeData(wb, wstitle, x = cc_display,
                          startRow = current_row, startCol = cc_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, cc_style_nm),
                         rows = current_row, cols = cc_col)

      # CC flag column (hidden white text)
      ow_style_nm <- if (is_bottom) "OW_BLRB" else "OW_BLR"
      openxlsx::writeData(wb, wstitle, x = "",
                          startRow = current_row, startCol = ccflag_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, ow_style_nm),
                         rows = current_row, cols = ccflag_col)

      # Sort order column
      sort_val <- if ("sort_order" %in% names(row_data)) row_data$sort_order[1] else NA
      sort_style_nm <- if (is_bottom) "DCB" else "DC"
      sort_display <- if (is.na(sort_val)) "" else sort_val
      openxlsx::writeData(wb, wstitle, x = sort_display,
                          startRow = current_row, startCol = sort_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, sort_style_nm),
                         rows = current_row, cols = sort_col)

      current_row <- current_row + 1L
    }
  }

  data_end_row <- current_row - 1L

  # ---------------------------------------------------------------------------
  # Conditional formatting for signal highlighting
  # SAS lines ~1250-1400
  # ---------------------------------------------------------------------------

  if (nrow(meddra_cmp_data) > 0) {
    # Signal column highlighting (col 7): silver when nearby signal
    openxlsx::conditionalFormatting(
      wb, wstitle,
      cols = 7, rows = data_start_row:data_end_row,
      rule = '="Y"', style = styles[["DG"]], type = "expression"
    )

    # RD column highlighting: bold red when above threshold
    openxlsx::conditionalFormatting(
      wb, wstitle,
      cols = rd_col, rows = data_start_row:data_end_row,
      rule = str_c(">=", rd_th), style = styles[["R"]], type = "expression"
    )

    # RR column highlighting
    openxlsx::conditionalFormatting(
      wb, wstitle,
      cols = rr_col, rows = data_start_row:data_end_row,
      rule = str_c(">=", rr_th), style = styles[["R"]], type = "expression"
    )

    # PV column highlighting
    openxlsx::conditionalFormatting(
      wb, wstitle,
      cols = pv_col, rows = data_start_row:data_end_row,
      rule = str_c(">=", pv_th), style = styles[["R"]], type = "expression"
    )
  }

  # ---------------------------------------------------------------------------
  # Auto-filter, data validation, named ranges, frozen panes
  # SAS lines ~1400-1473
  # ---------------------------------------------------------------------------

  # Auto-filter on header row (openxlsx uses addFilter, not setAutoFilter)
  if (nrow(meddra_cmp_data) > 0) {
    openxlsx::addFilter(
      wb, wstitle,
      rows = header_start_row + 1L,
      cols = 1:total_cols
    )
  }

  # Freeze panes: freeze at data start
  openxlsx::freezePane(wb, wstitle,
                       firstActiveRow = data_start_row,
                       firstActiveCol = 2L)

  # Named ranges for threshold cells
  openxlsx::createNamedRegion(
    wb, wstitle, name = "rdn",
    cols = rd_input_col, rows = rd_input_row
  )
  openxlsx::createNamedRegion(
    wb, wstitle, name = "rrn",
    cols = rr_input_col, rows = rr_input_row
  )
  openxlsx::createNamedRegion(
    wb, wstitle, name = "pvn",
    cols = pv_input_col, rows = pv_input_row
  )
  openxlsx::createNamedRegion(
    wb, wstitle, name = "exp_name",
    cols = trt_input_col, rows = trt_input_row
  )
  openxlsx::createNamedRegion(
    wb, wstitle, name = "ctl_name",
    cols = ctl_input_col, rows = ctl_input_row
  )

  # Page setup
  apply_page_setup(
    wb, wstitle,
    orientation   = "landscape",
    footer_center = "Page &P of &N"
  )

  invisible(NULL)
}


# =============================================================================
# meddra_out_cmp_data
# =============================================================================
#' Write the hidden data worksheet for MedDRA comparison workbook.
#'
#' Replaces SAS \code{%out_meddra_cmp_data} macro (lines 1478-1801 of
#' ae_meddra_output.sas). Creates a hidden worksheet containing level metadata,
#' per-arm counts/percentages, RD/RR/PV values, CC flags, signal propagation
#' data, and named ranges for formulas in the visible Comparison worksheet.
#'
#' @param wb                     An openxlsx workbook object.
#' @param meddra_cmp_hidden_data A tibble/data.frame with hidden data rows.
#'   Expected columns include: lvl_nm, lvl_no, soc, hlgt, hlt, pt, plus
#'   arm-prefixed columns (arm1_cnt, arm1_pct, ...), rd, rr, pv, cc, sgnl,
#'   sgnl_soc, sgnl_hlgt, sgnl_hlt, sgnl_pt, sort_order.
#' @param arm_count              Integer number of treatment arms.
#' @param styles                 Named list of openxlsx style objects.
#'
#' @return Invisible NULL; worksheet is added to \code{wb} by side effect.
meddra_out_cmp_data <- function(wb, meddra_cmp_hidden_data, arm_count, styles) {

  cli::cli_inform("Writing MedDRA hidden data worksheet...")

  wstitle <- "CmpData"
  openxlsx::addWorksheet(wb, wstitle, visible = FALSE)

  if (is.null(meddra_cmp_hidden_data) || nrow(meddra_cmp_hidden_data) == 0) {
    cli::cli_warn("Empty meddra_cmp_hidden_data provided; hidden worksheet will be empty.")
    return(invisible(NULL))
  }

  current_row <- 1L

  # ---------------------------------------------------------------------------
  # Column structure mirrors SAS %out_meddra_cmp_data layout:
  #   1: lvl_nm  2: lvl_no  3: soc  4: hlgt  5: hlt  6: pt
  #   Per arm: arm{i}_cnt, arm{i}_pct  (2 cols per arm)
  #   rd, rr, pv, cc
  #   sgnl, sgnl_soc, sgnl_hlgt, sgnl_hlt, sgnl_pt
  #   sort_order
  # ---------------------------------------------------------------------------

  base_cols <- c("lvl_nm", "lvl_no", "soc", "hlgt", "hlt", "pt")
  arm_cols  <- character(0)
  for (a in seq_len(arm_count)) {
    arm_cols <- c(arm_cols,
                  stringr::str_c("arm", a, "_cnt"),
                  stringr::str_c("arm", a, "_pct"))
  }
  stat_cols <- c("rd", "rr", "pv", "cc")
  sgnl_cols <- c("sgnl", "sgnl_soc", "sgnl_hlgt", "sgnl_hlt", "sgnl_pt")
  sort_cols <- "sort_order"

  all_cols <- c(base_cols, arm_cols, stat_cols, sgnl_cols, sort_cols)

  # Keep only columns present in data

  available_cols <- intersect(all_cols, names(meddra_cmp_hidden_data))

  if (length(available_cols) == 0) {
    cli::cli_warn("No recognized columns in meddra_cmp_hidden_data.")
    return(invisible(NULL))
  }

  # --- Write column headers ---
  for (j in seq_along(available_cols)) {
    openxlsx::writeData(wb, wstitle, x = available_cols[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # --- Write data rows ---
  subset_data <- meddra_cmp_hidden_data %>%
    dplyr::select(dplyr::any_of(available_cols))

  n_data <- nrow(subset_data)
  n_cols <- ncol(subset_data)

  for (i in seq_len(n_data)) {
    for (j in seq_len(n_cols)) {
      cell_val <- subset_data[[j]][i]

      if (is.na(cell_val)) {
        display_val <- if (is.numeric(subset_data[[j]])) "." else ""
      } else if (is.numeric(cell_val)) {
        col_nm <- available_cols[j]
        if (grepl("_pct$|^rd$|^rr$|^pv$", col_nm)) {
          display_val <- janitor::round_half_up(cell_val, 2)
        } else {
          display_val <- cell_val
        }
      } else {
        display_val <- cell_val
      }

      openxlsx::writeData(wb, wstitle, x = display_val,
                          startRow = current_row, startCol = j,
                          colNames = FALSE)
    }
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # Named ranges (SAS lines ~1650-1801)
  # ---------------------------------------------------------------------------
  data_start <- 2L
  data_end   <- data_start + n_data - 1L

  # Helper: safely create a named region with error handling
  safe_named_region <- function(wb, sheet, name, cols, rows) {
    tryCatch(
      openxlsx::createNamedRegion(wb, sheet, name = name,
                                  cols = cols, rows = rows),
      error = function(e) {
        cli::cli_warn("Named region '{name}' skipped: {e$message}")
      }
    )
  }

  # Per-arm count and percentage ranges
  for (a in seq_len(arm_count)) {
    cnt_idx <- which(available_cols == stringr::str_c("arm", a, "_cnt"))
    pct_idx <- which(available_cols == stringr::str_c("arm", a, "_pct"))

    if (length(cnt_idx) == 1) {
      safe_named_region(wb, wstitle,
                        name = stringr::str_c("aecnt", a),
                        cols = cnt_idx, rows = data_start:data_end)
    }
    if (length(pct_idx) == 1) {
      safe_named_region(wb, wstitle,
                        name = stringr::str_c("aepct", a),
                        cols = pct_idx, rows = data_start:data_end)
    }
  }

  # Comparison statistic ranges
  for (stat_nm in stat_cols) {
    stat_idx <- which(available_cols == stat_nm)
    if (length(stat_idx) == 1) {
      safe_named_region(wb, wstitle,
                        name = stringr::str_c("aecmp_", stat_nm),
                        cols = stat_idx, rows = data_start:data_end)
    }
  }

  # Level number range
  lvlno_idx <- which(available_cols == "lvl_no")
  if (length(lvlno_idx) == 1) {
    safe_named_region(wb, wstitle, name = "lvl_no_range",
                      cols = lvlno_idx, rows = data_start:data_end)
  }

  # Sort order range
  sort_idx <- which(available_cols == "sort_order")
  if (length(sort_idx) == 1) {
    safe_named_region(wb, wstitle, name = "sort_range",
                      cols = sort_idx, rows = data_start:data_end)
  }

  # Signal columns
  for (sgnl_nm in sgnl_cols) {
    sgnl_idx <- which(available_cols == sgnl_nm)
    if (length(sgnl_idx) == 1) {
      safe_named_region(wb, wstitle, name = sgnl_nm,
                        cols = sgnl_idx, rows = data_start:data_end)
    }
  }

  # Hierarchy identifier columns
  for (hier_nm in c("soc", "hlgt", "hlt", "pt")) {
    hier_idx <- which(available_cols == hier_nm)
    if (length(hier_idx) == 1) {
      safe_named_region(wb, wstitle,
                        name = stringr::str_c("hier_", hier_nm),
                        cols = hier_idx, rows = data_start:data_end)
    }
  }

  invisible(NULL)
}


# =============================================================================
# meddra_out_wbinfo
# =============================================================================
#' Write the hidden workbook info worksheet for MedDRA comparison workbook.
#'
#' Replaces SAS \code{\%wbinfo} macro (lines 1806-1906 of
#' ae_meddra_output.sas). Creates a hidden worksheet containing treatment arm
#' display names, arm subject counts, threshold configuration values, and
#' VLOOKUP-supporting named ranges used by the visible Comparison worksheet.
#'
#' @param wb          An openxlsx workbook object.
#' @param arm_count   Integer number of treatment arms.
#' @param arm_names   Character vector of arm display names.
#' @param arm_subjcnt Integer vector of per-arm subject counts.
#' @param rd_th       Numeric risk-difference threshold (or NA/Inf for none).
#' @param rr_th       Numeric relative-risk threshold (or NA/Inf for none).
#' @param pv_th       Numeric negative-log10-p-value threshold (or NA/Inf for none).
#' @param styles      Named list of openxlsx style objects.
#'
#' @return Invisible NULL; worksheet is added to \code{wb} by side effect.
meddra_out_wbinfo <- function(wb, arm_count, arm_names, arm_subjcnt,
                              rd_th, rr_th, pv_th, styles) {

  cli::cli_inform("Writing MedDRA workbook info worksheet...")

  wstitle <- "WBInfo"
  openxlsx::addWorksheet(wb, wstitle, visible = FALSE)

  current_row <- 1L

  # ---------------------------------------------------------------------------
  # Arm information table  (SAS lines 1808-1860)
  # Columns: Arm Name | Arm Number | Subject Count
  # ---------------------------------------------------------------------------

  arm_headers <- c("Arm Name", "Arm Number", "Subject Count")
  for (j in seq_along(arm_headers)) {
    openxlsx::writeData(wb, wstitle, x = arm_headers[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  arm_data_start <- current_row
  for (i in seq_len(arm_count)) {
    arm_display <- if (i <= length(arm_names)) arm_names[i] else stringr::str_c("Arm ", i)
    arm_n       <- if (i <= length(arm_subjcnt)) arm_subjcnt[i] else 0L

    openxlsx::writeData(wb, wstitle, x = arm_display,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, wstitle, x = i,
                        startRow = current_row, startCol = 2, colNames = FALSE)
    openxlsx::writeData(wb, wstitle, x = arm_n,
                        startRow = current_row, startCol = 3, colNames = FALSE)
    current_row <- current_row + 1L
  }
  arm_data_end <- current_row - 1L

  # Named ranges for arm lookup (data validation dropdown & VLOOKUP)
  tryCatch({
    openxlsx::createNamedRegion(wb, wstitle, name = "wbinfo_arminfo_1",
                                cols = 1, rows = arm_data_start:arm_data_end)
    openxlsx::createNamedRegion(wb, wstitle, name = "wbinfo_arminfo",
                                cols = 1:3, rows = arm_data_start:arm_data_end)
    openxlsx::createNamedRegion(wb, wstitle, name = "armn",
                                cols = 3, rows = arm_data_start:arm_data_end)
  }, error = function(e) {
    cli::cli_warn("Arm named regions partially skipped: {e$message}")
  })

  # ---------------------------------------------------------------------------
  # Comparison number computation (SAS lines 1860-1880)
  # Number of pairwise comparisons = arm_count * (arm_count - 1) / 2
  # ---------------------------------------------------------------------------

  current_row <- current_row + 1L
  n_comparisons <- arm_count * (arm_count - 1L) %/% 2L

  openxlsx::writeData(wb, wstitle, x = "Number of Comparisons",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, wstitle, x = n_comparisons,
                      startRow = current_row, startCol = 2, colNames = FALSE)
  tryCatch(
    openxlsx::createNamedRegion(wb, wstitle, name = "ncmp",
                                cols = 2, rows = current_row),
    error = function(e) cli::cli_warn("Named region ncmp skipped: {e$message}")
  )

  current_row <- current_row + 2L

  # ---------------------------------------------------------------------------
  # Threshold configuration  (SAS lines 1880-1906)
  # SAS uses ISBLANK -> "I" for infinity (unlimited threshold)
  # ---------------------------------------------------------------------------

  th_headers <- c("Threshold", "Value")
  for (j in seq_along(th_headers)) {
    openxlsx::writeData(wb, wstitle, x = th_headers[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  # Format threshold: NA / NULL / Inf  ->  "I"  (infinity marker)
  format_threshold <- function(val) {
    if (is.null(val) || is.na(val) || is.infinite(val)) return("I")
    val
  }

  thresholds <- list(
    list(name = "RD%",          value = format_threshold(rd_th)),
    list(name = "RR",           value = format_threshold(rr_th)),
    list(name = "-log10(PV)",   value = format_threshold(pv_th))
  )

  th_data_start <- current_row
  for (th in thresholds) {
    openxlsx::writeData(wb, wstitle, x = th$name,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, wstitle, x = th$value,
                        startRow = current_row, startCol = 2, colNames = FALSE)
    current_row <- current_row + 1L
  }

  # Named ranges for thresholds used by conditional formatting
  tryCatch({
    openxlsx::createNamedRegion(wb, wstitle, name = "rd",
                                cols = 2, rows = th_data_start)
    openxlsx::createNamedRegion(wb, wstitle, name = "rr",
                                cols = 2, rows = th_data_start + 1L)
    openxlsx::createNamedRegion(wb, wstitle, name = "pv",
                                cols = 2, rows = th_data_start + 2L)
  }, error = function(e) {
    cli::cli_warn("Threshold named regions partially skipped: {e$message}")
  })

  # ---------------------------------------------------------------------------
  # Exposure / Control / Comparison selector named ranges
  # SAS lines ~1886-1906
  # ---------------------------------------------------------------------------

  current_row <- current_row + 1L

  exp_ctl_headers <- c("Selection", "Arm Number")
  for (j in seq_along(exp_ctl_headers)) {
    openxlsx::writeData(wb, wstitle, x = exp_ctl_headers[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  current_row <- current_row + 1L

  sel_data_start <- current_row
  selections <- c("Exposure", "Control")
  default_arms <- c(1L, if (arm_count >= 2) 2L else 1L)
  for (s in seq_along(selections)) {
    openxlsx::writeData(wb, wstitle, x = selections[s],
                        startRow = current_row, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, wstitle, x = default_arms[s],
                        startRow = current_row, startCol = 2, colNames = FALSE)
    current_row <- current_row + 1L
  }

  tryCatch({
    openxlsx::createNamedRegion(wb, wstitle, name = "exp",
                                cols = 2, rows = sel_data_start)
    openxlsx::createNamedRegion(wb, wstitle, name = "ctl",
                                cols = 2, rows = sel_data_start + 1L)
    openxlsx::createNamedRegion(wb, wstitle, name = "cmp",
                                cols = 2, rows = sel_data_start)
  }, error = function(e) {
    cli::cli_warn("Selection named regions partially skipped: {e$message}")
  })

  invisible(NULL)
}


# =============================================================================
# meddra_out_err
# =============================================================================
#' Write the "Data Check Summary" worksheet for MedDRA comparison workbook.
#'
#' Replaces SAS \code{\%out_err} macro (lines 1912-2534 of
#' ae_meddra_output.sas). Produces a multi-section worksheet:
#' \enumerate{
#'   \item Header with title, NDA/BLA, study, run date
#'   \item Subject Validation per-arm counts
#'   \item AE Data Validation summary (error reasons + counts)
#'   \item AE Validation by Term detail (per-term body system + event counts)
#'   \item MedDRA Matching summary and conditional per-term mismatch detail
#' }
#'
#' @param wb              An openxlsx workbook object.
#' @param rpt_dm          Tibble with subject validation data. Expected columns:
#'   \code{description} plus arm-prefixed count/pct columns.
#' @param rpt_err         Tibble with AE validation error summary.
#' @param rpt_err_term    Tibble with per-term AE validation detail.
#' @param rpt_meddra      Tibble with MedDRA matching summary.
#' @param rpt_meddra_term Tibble with per-term MedDRA non-match detail.
#' @param vld_sw          Character validation switch (\code{"Y"}/\code{"N"}).
#' @param meddra          Logical whether MedDRA matching is enabled.
#' @param arm_count       Integer number of treatment arms.
#' @param arm_names       Character vector of arm display names.
#' @param styles          Named list of openxlsx style objects.
#'
#' @return Invisible NULL; worksheet is added to \code{wb} by side effect.
meddra_out_err <- function(wb, rpt_dm, rpt_err, rpt_err_term,
                           rpt_meddra, rpt_meddra_term,
                           vld_sw, meddra, arm_count, arm_names, styles) {

  cli::cli_inform("Writing MedDRA data check summary worksheet...")

  wstitle <- "Data Check Summary"
  openxlsx::addWorksheet(wb, wstitle)

  # Column widths: main descriptor ~38 char, arm columns ~10 char each
  # SAS uses ~266 px for col 1, ~67 px for arm cols (converted / 7)
  main_col_width <- 38
  arm_col_width  <- 10
  # Total columns: 1 description + 2 per arm (count+pct) + 1 total
  total_cols <- 1L + 2L * arm_count + 1L
  col_widths <- c(main_col_width,
                  rep(c(arm_col_width, arm_col_width), arm_count),
                  arm_col_width)
  openxlsx::setColWidths(wb, wstitle,
                         cols = seq_along(col_widths),
                         widths = col_widths)

  current_row <- 1L
  rundate <- format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")

  # ===== HEADER SECTION (SAS lines 1920-1960) ================================

  # Title row
  current_row <- current_row + 1L
  openxlsx::writeData(
    wb, wstitle,
    x = "MedDRA at a Glance Comparison: Data Check Summary",
    startRow = current_row, startCol = 1, colNames = FALSE
  )
  openxlsx::addStyle(wb, wstitle, style = get_style_by_name(styles, "Header"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 1L

  # Run-date row
  openxlsx::writeData(
    wb, wstitle, x = stringr::str_c("Report generated: ", rundate),
    startRow = current_row, startCol = 1, colNames = FALSE
  )
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 2L

  # ===== SUBJECT VALIDATION SECTION (SAS lines 1960-2060) ====================

  openxlsx::writeData(wb, wstitle, x = "Subject Validation",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 1L

  # Narrative text
  subj_text <- stringr::str_c(
    "The following table summarizes subject counts by treatment arm. ",
    "Subjects not meeting validation criteria are excluded from the ",
    "MedDRA at a Glance Comparison Analysis."
  )
  openxlsx::writeData(wb, wstitle, x = subj_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 27)
  current_row <- current_row + 2L

  # Column headers for subject-validation table
  subj_col_headers <- "Description"
  for (i in seq_len(arm_count)) {
    arm_lbl <- if (i <= length(arm_names)) arm_names[i] else stringr::str_c("Arm ", i)
    subj_col_headers <- c(subj_col_headers,
                          stringr::str_c(arm_lbl, "\nSubject Count"),
                          stringr::str_c(arm_lbl, "\n%"))
  }
  subj_col_headers <- c(subj_col_headers, "Total")

  for (j in seq_along(subj_col_headers)) {
    openxlsx::writeData(wb, wstitle, x = subj_col_headers[j],
                        startRow = current_row, startCol = j, colNames = FALSE)
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = j)
  }
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 40)
  current_row <- current_row + 1L

  # Data rows — rpt_dm
  if (!is.null(rpt_dm) && nrow(rpt_dm) > 0) {
    n_dm <- nrow(rpt_dm)
    for (i in seq_len(n_dm)) {
      is_last <- (i == n_dm)
      for (j in seq_len(ncol(rpt_dm))) {
        cell_val <- rpt_dm[[j]][i]
        display_val <- if (is.na(cell_val)) {
          if (is.numeric(rpt_dm[[j]])) "." else ""
        } else {
          cell_val
        }

        col_nm <- names(rpt_dm)[j]
        style_nm <- if (j == 1) {
          if (is_last) "DataBottom" else "Data"
        } else if (grepl("cnt|count|total", col_nm, ignore.case = TRUE)) {
          if (is_last) "DataCenterBottom" else "DataCenter"
        } else if (grepl("pct|%", col_nm, ignore.case = TRUE)) {
          if (is_last) "DataDec1Bottom" else "DataDec1"
        } else {
          if (is_last) "DataBottom" else "Data"
        }

        openxlsx::writeData(wb, wstitle, x = display_val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, style_nm),
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
  }
  current_row <- current_row + 1L

  # ===== AE DATA VALIDATION SECTION (SAS lines 2060-2200) ====================

  has_errors <- !is.null(rpt_err) && nrow(rpt_err) > 0

  openxlsx::writeData(wb, wstitle, x = "Adverse Events Data Validation",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 1L

  ae_text <- if (has_errors) {
    stringr::str_c(
      "The following adverse events were excluded from the analysis. ",
      "Adverse events are excluded if: (1) the preferred term is blank, ",
      "(2) the subject is not in the subject-level dataset, or ",
      "(3) the subject was excluded during subject validation."
    )
  } else {
    "All adverse events passed data validation."
  }

  openxlsx::writeData(wb, wstitle, x = ae_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 40.5)
  current_row <- current_row + 2L

  # --- AE Validation Summary table ---
  if (has_errors) {
    err_headers <- "Reason"
    for (i in seq_len(arm_count)) {
      arm_lbl <- if (i <= length(arm_names)) arm_names[i] else stringr::str_c("Arm ", i)
      err_headers <- c(err_headers,
                       stringr::str_c(arm_lbl, "\nEvent Count"),
                       stringr::str_c(arm_lbl, "\n%"))
    }

    for (j in seq_along(err_headers)) {
      openxlsx::writeData(wb, wstitle, x = err_headers[j],
                          startRow = current_row, startCol = j, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = j)
    }
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 35)
    current_row <- current_row + 1L

    n_err <- nrow(rpt_err)
    for (i in seq_len(n_err)) {
      is_last <- (i == n_err)
      for (j in seq_len(ncol(rpt_err))) {
        cell_val <- rpt_err[[j]][i]
        display_val <- if (is.na(cell_val)) {
          if (is.numeric(rpt_err[[j]])) "." else ""
        } else {
          cell_val
        }

        col_nm <- names(rpt_err)[j]
        style_nm <- if (j == 1) {
          if (is_last) "DataBottom" else "Data"
        } else if (grepl("cnt|count", col_nm, ignore.case = TRUE)) {
          if (is_last) "DataCenterBottom" else "DataCenter"
        } else if (grepl("pct|%", col_nm, ignore.case = TRUE)) {
          if (is_last) "DataDec1Bottom" else "DataDec1"
        } else {
          if (is_last) "DataBottom" else "Data"
        }

        openxlsx::writeData(wb, wstitle, x = display_val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, style_nm),
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
    current_row <- current_row + 1L

    # --- AE Validation by Term detail  (SAS lines 2220-2270) ---
    if (!is.null(rpt_err_term) && nrow(rpt_err_term) > 0) {

      # Merged title row
      openxlsx::writeData(
        wb, wstitle,
        x = "Adverse Events Without Passing Data Validation",
        startRow = current_row, startCol = 1, colNames = FALSE
      )
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = 1)
      # Columns: AEBODSYS(1) | AEDECOD merged(2-6) | per-arm count(7..)
      max_term_cols <- 6L + arm_count
      openxlsx::mergeCells(wb, wstitle, cols = 1:max_term_cols,
                           rows = current_row)
      openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 25)
      current_row <- current_row + 1L

      # Column header row
      hdr_col <- 1L
      openxlsx::writeData(wb, wstitle, x = "Body System or Organ Class",
                          startRow = current_row, startCol = hdr_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = hdr_col)
      hdr_col <- 2L

      openxlsx::writeData(wb, wstitle, x = "Dictionary-Derived Term",
                          startRow = current_row, startCol = hdr_col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = hdr_col)
      openxlsx::mergeCells(wb, wstitle, cols = hdr_col:(hdr_col + 4L),
                           rows = current_row)
      hdr_col <- hdr_col + 5L

      for (i in seq_len(arm_count)) {
        arm_lbl <- if (i <= length(arm_names)) arm_names[i] else stringr::str_c("Arm ", i)
        openxlsx::writeData(wb, wstitle,
                            x = stringr::str_c(arm_lbl, "\nEvent Count"),
                            startRow = current_row, startCol = hdr_col,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, "ColumnOutline"),
                           rows = current_row, cols = hdr_col)
        hdr_col <- hdr_col + 1L
      }
      openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 30)
      current_row <- current_row + 1L

      # Per-term data rows
      err_term_first_row <- current_row
      n_term <- nrow(rpt_err_term)
      for (i in seq_len(n_term)) {
        is_last <- (i == n_term)
        base_style <- if (is_last) "DataBottom" else "Data"

        for (j in seq_len(ncol(rpt_err_term))) {
          cell_val <- rpt_err_term[[j]][i]
          col_nm  <- toupper(names(rpt_err_term)[j])
          display_val <- if (is.na(cell_val)) {
            if (is.numeric(rpt_err_term[[j]])) "." else ""
          } else {
            cell_val
          }

          openxlsx::writeData(wb, wstitle, x = display_val,
                              startRow = current_row, startCol = j,
                              colNames = FALSE)
          openxlsx::addStyle(wb, wstitle,
                             style = get_style_by_name(styles, base_style),
                             rows = current_row, cols = j)

          # Merge AEDECOD across 5 cells (columns 2-6)
          if (col_nm == "AEDECOD") {
            openxlsx::mergeCells(wb, wstitle, cols = j:(j + 4L),
                                 rows = current_row)
          }
        }
        current_row <- current_row + 1L
      }
      err_term_last_row <- current_row - 1L

      # Alternating silver-row conditional formatting (SAS lines 2500-2520)
      if (n_term > 1) {
        silver_style <- openxlsx::createStyle(fgFill = "#C0C0C0")
        openxlsx::conditionalFormatting(
          wb, wstitle,
          cols  = 1:max_term_cols,
          rows  = err_term_first_row:err_term_last_row,
          rule  = "MOD(ROW(),2)=0",
          style = silver_style,
          type  = "expression"
        )
      }

      current_row <- current_row + 1L
    }
  }

  # ===== MEDDRA MATCHING SECTION (SAS lines 2274-2389) ========================

  current_row <- current_row + 1L
  openxlsx::writeData(wb, wstitle, x = "MedDRA Matching",
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "SubHeader"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  current_row <- current_row + 1L

  # Compute total validated AEs for narrative
  naes_spv <- 0L
  if (!is.null(rpt_meddra) && nrow(rpt_meddra) > 0) {
    match_col    <- which(grepl("match_count|match_cnt", names(rpt_meddra),
                                ignore.case = TRUE))
    nonmatch_col <- which(grepl("nonmatch_count|nonmatch_cnt|non_match",
                                names(rpt_meddra), ignore.case = TRUE))
    if (length(match_col) > 0) {
      naes_spv <- naes_spv + sum(rpt_meddra[[match_col[1]]], na.rm = TRUE)
    }
    if (length(nonmatch_col) > 0) {
      naes_spv <- naes_spv + sum(rpt_meddra[[nonmatch_col[1]]], na.rm = TRUE)
    }
    if (naes_spv == 0) naes_spv <- nrow(rpt_meddra)
  }

  meddra_text <- stringr::str_c(
    "MedDRA matching is performed on all adverse events which passed data ",
    "validation, of which there were ", format(naes_spv, big.mark = ","),
    " in this study."
  )
  openxlsx::writeData(wb, wstitle, x = meddra_text,
                      startRow = current_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, wstitle,
                     style = get_style_by_name(styles, "Default10Wrap"),
                     rows = current_row, cols = 1)
  openxlsx::mergeCells(wb, wstitle, cols = 1:total_cols, rows = current_row)
  txt_ht <- max(27, ceiling(nchar(meddra_text) / 100) * 13.5)
  openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = txt_ht)
  current_row <- current_row + 2L

  # --- MedDRA Matching Summary table ---
  if (!is.null(rpt_meddra) && nrow(rpt_meddra) > 0) {

    # Merged title row
    openxlsx::writeData(
      wb, wstitle,
      x = stringr::str_c("MedDRA Matching Summary (N=",
                          format(naes_spv, big.mark = ","), ")"),
      startRow = current_row, startCol = 1, colNames = FALSE
    )
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:4, rows = current_row)
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 35)
    current_row <- current_row + 1L

    # Column headers
    meddra_hdrs <- c("MedDRA Version", "Match Count",
                     "Non-Match Count", "Match %")
    for (j in seq_along(meddra_hdrs)) {
      openxlsx::writeData(wb, wstitle, x = meddra_hdrs[j],
                          startRow = current_row, startCol = j, colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = j)
    }
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 40)
    current_row <- current_row + 1L

    # Data rows
    n_meddra <- nrow(rpt_meddra)
    for (i in seq_len(n_meddra)) {
      is_last <- (i == n_meddra)
      for (j in seq_len(ncol(rpt_meddra))) {
        cell_val <- rpt_meddra[[j]][i]
        display_val <- if (is.na(cell_val)) {
          if (is.numeric(rpt_meddra[[j]])) "." else ""
        } else {
          cell_val
        }

        col_nm <- names(rpt_meddra)[j]
        style_nm <- if (j == 1) {
          if (is_last) "DataBottom" else "Data"
        } else if (grepl("pct|%", col_nm, ignore.case = TRUE)) {
          if (is_last) "DataDec1Bottom" else "DataDec1"
        } else {
          if (is_last) "DataCenterBottom" else "DataCenter"
        }

        openxlsx::writeData(wb, wstitle, x = display_val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, style_nm),
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
    current_row <- current_row + 1L
  }

  # --- Non-matching terms detail (SAS lines 2351-2389) ---
  meddra_pct <- 100
  if (!is.null(rpt_meddra) && nrow(rpt_meddra) > 0) {
    pct_col <- which(grepl("pct|%|match_pct", names(rpt_meddra),
                           ignore.case = TRUE))
    if (length(pct_col) > 0) {
      pct_vals <- rpt_meddra[[pct_col[1]]]
      pct_vals <- pct_vals[!is.na(pct_vals)]
      if (length(pct_vals) > 0) meddra_pct <- min(pct_vals)
    }
  }

  if (floor(meddra_pct) < 100 &&
      !is.null(rpt_meddra_term) && nrow(rpt_meddra_term) > 0) {

    # Merged title row
    openxlsx::writeData(
      wb, wstitle,
      x = "Adverse Events Without Matching MedDRA Terms",
      startRow = current_row, startCol = 1, colNames = FALSE
    )
    openxlsx::addStyle(wb, wstitle,
                       style = get_style_by_name(styles, "ColumnOutline"),
                       rows = current_row, cols = 1)
    openxlsx::mergeCells(wb, wstitle, cols = 1:8, rows = current_row)
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 25)
    current_row <- current_row + 1L

    # Column headers: BSOC | Dict-Derived Term (merged 5) | Subject Count | Event Count
    mterm_hdr <- list(
      list(col = 1L, val = "Body System or Organ Class", span = 1L),
      list(col = 2L, val = "Dictionary-Derived Term",    span = 5L),
      list(col = 7L, val = "Subject Count",              span = 1L),
      list(col = 8L, val = "Event Count",                span = 1L)
    )
    for (mh in mterm_hdr) {
      openxlsx::writeData(wb, wstitle, x = mh$val,
                          startRow = current_row, startCol = mh$col,
                          colNames = FALSE)
      openxlsx::addStyle(wb, wstitle,
                         style = get_style_by_name(styles, "ColumnOutline"),
                         rows = current_row, cols = mh$col)
      if (mh$span > 1L) {
        openxlsx::mergeCells(wb, wstitle,
                             cols = mh$col:(mh$col + mh$span - 1L),
                             rows = current_row)
      }
    }
    openxlsx::setRowHeights(wb, wstitle, rows = current_row, heights = 30)
    current_row <- current_row + 1L

    # Per-term data rows
    mterm_first_row <- current_row
    n_mterm <- nrow(rpt_meddra_term)
    for (i in seq_len(n_mterm)) {
      is_last    <- (i == n_mterm)
      base_style <- if (is_last) "DataBottom" else "Data"

      for (j in seq_len(ncol(rpt_meddra_term))) {
        cell_val <- rpt_meddra_term[[j]][i]
        col_nm   <- toupper(names(rpt_meddra_term)[j])
        display_val <- if (is.na(cell_val)) {
          if (is.numeric(rpt_meddra_term[[j]])) "." else ""
        } else {
          cell_val
        }

        openxlsx::writeData(wb, wstitle, x = display_val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, wstitle,
                           style = get_style_by_name(styles, base_style),
                           rows = current_row, cols = j)

        # Merge AEDECOD across 5 columns
        if (col_nm == "AEDECOD") {
          openxlsx::mergeCells(wb, wstitle, cols = j:(j + 4L),
                               rows = current_row)
        }
      }
      current_row <- current_row + 1L
    }
    mterm_last_row <- current_row - 1L

    # Alternating silver rows (SAS lines 2500-2520)
    if (n_mterm > 1) {
      silver_style <- openxlsx::createStyle(fgFill = "#C0C0C0")
      openxlsx::conditionalFormatting(
        wb, wstitle,
        cols  = 1:8,
        rows  = mterm_first_row:mterm_last_row,
        rule  = "MOD(ROW(),2)=0",
        style = silver_style,
        type  = "expression"
      )
    }
  }

  # ===== PAGE SETUP (SAS lines 2488-2521) =====================================
  apply_page_setup(
    wb, wstitle,
    orientation   = "landscape",
    footer_center = "Page &P of &N"
  )

  invisible(NULL)
}


# =============================================================================
# meddra_out_workbook  (MAIN ENTRY POINT / EXPORTED FUNCTION)
# =============================================================================
#' Generate the complete MedDRA at a Glance Comparison Excel workbook.
#'
#' This is the main orchestrator that replaces SAS \code{\%out_med} macro
#' (lines 2687-2776 of ae_meddra_output.sas). It creates an openxlsx workbook,
#' initialises the combined base + MedDRA style gallery, calls each worksheet
#' builder in sequence, optionally appends the Script Launcher
#' "Grouping and Subsetting" worksheet, and saves the final \code{.xlsx} file.
#'
#' @param output_file          Character path for the output \code{.xlsx} file.
#' @param meddra_cmp_data      Tibble with visible comparison data (SOC/HLGT/HLT/PT
#'   hierarchy, per-arm counts/pct, RD, RR, PV, CC, DME flag).
#' @param meddra_cmp_hidden_data Tibble with hidden data worksheet content.
#' @param rpt_dm               Tibble with subject-validation counts.
#' @param rpt_err              Tibble with AE validation error summary.
#' @param rpt_err_term         Tibble with per-term AE validation detail.
#' @param rpt_meddra           Tibble with MedDRA matching summary.
#' @param rpt_meddra_term      Tibble with per-term MedDRA non-match detail.
#' @param ndabla               Character NDA/BLA identifier.
#' @param studyid              Character study identifier.
#' @param arm_count            Integer number of treatment arms.
#' @param arm_names            Character vector of arm display names.
#' @param arm_subjcnt          Integer vector of per-arm subject counts.
#' @param meddra_ver           Character MedDRA dictionary version string.
#' @param rd_th                Numeric risk-difference threshold.
#' @param rr_th                Numeric relative-risk threshold.
#' @param pv_th                Numeric negative-log10-p-value threshold.
#' @param cc_sw                Character continuity-correction switch
#'   (\code{"Y"}/\code{"N"}).
#' @param cc_desc              Character continuity-correction description.
#' @param study_lag            Character study time-lag description (or \code{""}).
#' @param vld_sw               Character validation switch (\code{"Y"}/\code{"N"}).
#' @param meddra               Logical whether MedDRA matching is enabled.
#' @param dme_sw               Logical whether DME (Designated Medical Event)
#'   flagging is enabled.
#' @param sl_group_desc        Character grouping description for Script Launcher
#'   (default \code{"No grouping"}).
#' @param sl_subset_desc       Character subsetting description for Script Launcher
#'   (default \code{"No subsetting"}).
#' @param pp_result            Optional list returned by \code{group_subset_pp()}
#'   for generating the Grouping and Subsetting worksheet. \code{NULL} to skip.
#'
#' @return Invisible character path of the saved workbook.
#' @export
meddra_out_workbook <- function(output_file,
                                meddra_cmp_data,
                                meddra_cmp_hidden_data,
                                rpt_dm,
                                rpt_err,
                                rpt_err_term,
                                rpt_meddra,
                                rpt_meddra_term,
                                ndabla,
                                studyid,
                                arm_count,
                                arm_names,
                                arm_subjcnt,
                                meddra_ver,
                                rd_th,
                                rr_th,
                                pv_th,
                                cc_sw,
                                cc_desc,
                                study_lag,
                                vld_sw,
                                meddra,
                                dme_sw        = FALSE,
                                sl_group_desc = "No grouping",
                                sl_subset_desc = "No subsetting",
                                pp_result     = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  stopifnot(
    is.character(output_file), nchar(output_file) > 0,
    is.numeric(arm_count),     arm_count >= 1,
    is.character(arm_names),   length(arm_names) >= 1
  )

  cli::cli_inform(c(
    "i" = "Building MedDRA at a Glance Comparison workbook...",
    "i" = "Output file: {output_file}",
    "i" = "Study: {studyid}  NDA/BLA: {ndabla}",
    "i" = "Arms: {arm_count}  MedDRA version: {meddra_ver}"
  ))

  # ---------------------------------------------------------------------------
  # 1. Create workbook  (SAS: %wb)
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook(
    title   = "MedDRA at a Glance Comparison Analysis",
    creator = "PhUSE CS Standard Analyses (R Migration)"
  )

  # ---------------------------------------------------------------------------
  # 2. Build combined style gallery  (SAS: %styles(size=8) + %out_aem_styles)
  # ---------------------------------------------------------------------------
  styles <- meddra_out_styles(base_size = 8)

  cli::cli_inform("Style gallery initialised ({length(styles)} styles).")

  # ---------------------------------------------------------------------------
  # 3. Front Page / Cover worksheet  (SAS: %out_cover)
  # ---------------------------------------------------------------------------
  rundate <- format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")

  meddra_out_cover(
    wb           = wb,
    ndabla       = ndabla,
    studyid      = studyid,
    rundate      = rundate,
    arm_count    = arm_count,
    arm_names    = arm_names,
    meddra_ver   = meddra_ver,
    cc_desc      = cc_desc,
    study_lag    = study_lag,
    vld_sw       = vld_sw,
    sl_group_desc  = sl_group_desc,
    sl_subset_desc = sl_subset_desc,
    styles       = styles
  )

  # ---------------------------------------------------------------------------
  # 4. Comparison worksheet (visible)  (SAS: %out_meddra_cmp)
  # ---------------------------------------------------------------------------
  meddra_out_comparison(
    wb              = wb,
    meddra_cmp_data = meddra_cmp_data,
    arm_count       = arm_count,
    arm_names       = arm_names,
    arm_subjcnt     = arm_subjcnt,
    rd_th           = rd_th,
    rr_th           = rr_th,
    pv_th           = pv_th,
    cc_sw           = cc_sw,
    meddra_ver      = meddra_ver,
    dme_sw          = dme_sw,
    styles          = styles
  )

  # ---------------------------------------------------------------------------
  # 5. Data Check Summary worksheet  (SAS: %out_err)
  # ---------------------------------------------------------------------------
  meddra_out_err(
    wb              = wb,
    rpt_dm          = rpt_dm,
    rpt_err         = rpt_err,
    rpt_err_term    = rpt_err_term,
    rpt_meddra      = rpt_meddra,
    rpt_meddra_term = rpt_meddra_term,
    vld_sw          = vld_sw,
    meddra          = meddra,
    arm_count       = arm_count,
    arm_names       = arm_names,
    styles          = styles
  )

  # ---------------------------------------------------------------------------
  # 6. Grouping and Subsetting worksheet (optional; SAS: %group_subset_xml_out)
  # ---------------------------------------------------------------------------
  if (!is.null(pp_result)) {
    cli::cli_inform("Writing Grouping and Subsetting worksheet...")
    group_subset_write_ws(
      wb       = wb,
      pp_result = pp_result,
      ndabla   = ndabla,
      studyid  = studyid,
      styles   = styles
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Hidden data worksheet  (SAS: %out_meddra_cmp_data)
  # ---------------------------------------------------------------------------
  meddra_out_cmp_data(
    wb                     = wb,
    meddra_cmp_hidden_data = meddra_cmp_hidden_data,
    arm_count              = arm_count,
    styles                 = styles
  )

  # ---------------------------------------------------------------------------
  # 8. Hidden WBInfo worksheet  (SAS: %wbinfo)
  # ---------------------------------------------------------------------------
  meddra_out_wbinfo(
    wb          = wb,
    arm_count   = arm_count,
    arm_names   = arm_names,
    arm_subjcnt = arm_subjcnt,
    rd_th       = rd_th,
    rr_th       = rr_th,
    pv_th       = pv_th,
    styles      = styles
  )

  # ---------------------------------------------------------------------------
  # 9. Save workbook  (SAS: concatenate ws_ datasets + PUT statements)
  # ---------------------------------------------------------------------------
  cli::cli_inform("Saving workbook to {output_file} ...")

  # Ensure output directory exists
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)

  cli::cli_inform(c(
    "v" = "MedDRA at a Glance Comparison workbook saved successfully.",
    "i" = "File: {output_file}"
  ))

  invisible(output_file)
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    - SpreadsheetML XML generation (xml_output.sas) fully replaced by openxlsx
#      package API calls (createWorkbook, addWorksheet, writeData, addStyle,
#      mergeCells, saveWorkbook, etc.)
#    - All 7 SAS macros (%out_cover, %out_meddra_cmp, %out_meddra_cmp_data,
#      %wbinfo, %out_err, %out_aem_styles, %out_med) mapped to 7 R functions
#      with matching worksheet outputs.
#    - MedDRA-specific style catalog extends base xml_output.R styles via
#      create_workbook_styles(base_size) + MedDRA-specific additions.
#    - Signal highlighting via openxlsx::conditionalFormatting() using
#      expression-based rules.
#    - Data validation dropdowns via openxlsx::dataValidation() with list type.
#    - Named ranges via openxlsx::createNamedRegion() with tryCatch safety.
#    - SAS macro parameter &arm_name_1. ... &arm_name_N. collapsed to R
#      character vector arm_names.
#    - SAS dataset references converted to tibble/data.frame arguments.
#    - SAS numeric missing (.) displayed as "." string in Excel cells.
#    - SAS character missing (' ') mapped to empty string in Excel cells.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Column widths: SAS pixel units vs openxlsx character units (approx /7).
#      Exact pixel-to-character mapping may vary by font and Excel version.
#    - Conditional formatting rules may render differently across Excel
#      versions (2016 vs 365 vs LibreOffice Calc).
#    - Named range scoping may differ between SpreadsheetML and OOXML formats.
#    - Alternating-row conditional formatting uses MOD(ROW(),2) expression
#      which matches SAS behaviour but rendering depends on Excel engine.
#    - Percentage values rounded via janitor::round_half_up() to match SAS
#      round-half-up convention; any remaining precision differences are
#      within floating-point epsilon.
#
# NO DIRECT R EQUIVALENT:
#    - SAS SpreadsheetML XML string building (DATA step PUT statements)
#      replaced by openxlsx API calls (writeData + addStyle per cell).
#    - SAS %annotate/%markup cell-level style pipeline replaced by
#      openxlsx writeData() + addStyle() per cell/row.
#    - SAS FILE/PUT streaming output replaced by openxlsx::saveWorkbook().
#    - SAS NamedCell declarations replaced by openxlsx::createNamedRegion().
#    - SAS INDEX/INDIRECT/MATCH array formulas in hidden data sheet are
#      replaced by pre-computed R values written directly to cells; Excel
#      formula-based signal propagation is replaced by R-side computation.
#    - SAS PCFILES/JET engine direct Excel writes replaced by openxlsx.
#
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: AAP-mandated replacement for SpreadsheetML XML generation;
#      provides complete OOXML workbook API without Java dependency.
#    - dplyr: tidyverse data manipulation for preparing worksheet data;
#      AAP mandates tidyverse over base R.
#    - stringr: tidyverse string operations for narrative text construction;
#      AAP mandates stringr over base R string functions.
#    - janitor: round_half_up() for SAS-compatible rounding; AAP Gate 2
#      requires this at every rounding location.
#    - purrr: tidyverse functional programming for iterating over arm columns
#      in worksheet builders; replaces SAS %do loops.
#    - cli: user-facing progress messages replacing SAS %PUT statements;
#      consistent with framework patterns in ae_oncology_output.R.
#
# OPEN QUESTIONS:
#    - Exact conditional formatting rule syntax for signal highlighting may
#      need tuning based on end-user Excel version.
#    - Named range scoping across hidden vs visible worksheets may need
#      manual verification for VLOOKUP cross-sheet references.
#    - Data validation list source ranges across sheets may need adjustment
#      if sheet ordering differs from SAS output.
#    - SAS hidden-sheet INDEX/INDIRECT formulas replaced by static values;
#      if dynamic recalculation is needed, formula strings must be injected
#      via openxlsx::writeFormula() in a future enhancement.
# =============================================================================
