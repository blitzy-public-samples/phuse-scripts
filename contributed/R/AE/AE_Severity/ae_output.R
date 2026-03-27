# ==============================================================================
#         PROGRAM NAME: AE Severity Panel Output (R Migration)
#
#          DESCRIPTION: Generates an Excel (.xlsx) workbook for the Adverse
#                       Events Severity Panel containing:
#                         - Front Page (cover sheet)
#                         - Analysis A: AEs by Arm (>2%)
#                         - Analysis B: Serious AEs by Arm
#                         - Analysis C: AEs by Severity
#                         - Analysis D: Serious AEs by Severity
#                         - Data Check Summary
#                         - Grouping and Subsetting (conditional)
#
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#        ORIGINAL DATE: 2011
#
#   MIGRATION DETAILS:
#     - Source: contributed/AE/AE_Severity/ae_output.sas (1529 lines)
#     - SAS %out_cover       -> R out_cover()
#     - SAS %out_ab(rpt=)    -> R out_ab()
#     - SAS %out_cd(rpt=)    -> R out_cd()
#     - SAS %out_err         -> R out_err()
#     - SAS %out_note(rpt=)  -> R out_note()
#     - SAS %out_ae_styles   -> R create_ae_styles()
#     - SAS %ws macro        -> openxlsx::addWorksheet()
#     - SAS %out_ae          -> R out_ae()
#     - SpreadsheetML XML    -> openxlsx workbook API
#     - SAS DATA _NULL_ file -> openxlsx::saveWorkbook()
#
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5
#            REVISIONS: 2026 -- Blitzy -- Migrated from SAS to R
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(openxlsx)
library(dplyr)
library(stringr)
library(cli)
library(janitor)

# --- Source Dependency ---------------------------------------------------------
# sl_gs_output.R provides group_subset_pp() and group_subset_xml_out()
# The caller must source sl_gs_output.R before sourcing this file, e.g.:
#   source(file.path(util_path, "sl_gs_output.R"))
#   source(file.path(util_path, "ae_output.R"))


# ==============================================================================
# create_ae_styles -- Style Gallery for AE Severity Workbook
# ------------------------------------------------------------------------------
# Replaces SAS %out_ae_styles macro (SAS lines 1400-1443).
# Creates a named list of openxlsx Style objects for all cell formatting used
# across the AE severity workbook. Base styles (D, D0_R1, D0_R2, D0_R4,
# D1_R1, D1_R2, DT, D0_R1T, D1_R1T) are each expanded with border
# combinations: _BL, _BR, _BB, _BLR, _BLB, _BRB, _BLRB.
#
# @return Named list of openxlsx::Style objects.
# ==============================================================================
create_ae_styles <- function() {
  styles <- list()

  # Base style definitions with number formats, alignment, indentation
  # Corresponding to SAS lines 1402-1416
  base_defs <- list(
    D      = list(),
    D0_R1  = list(numFmt = "0",   halign = "right", indent = 1),
    D0_R2  = list(numFmt = "0",   halign = "right", indent = 2),
    D0_R4  = list(numFmt = "0",   halign = "right", indent = 4),
    D1_R1  = list(numFmt = "0.0", halign = "right", indent = 1),
    D1_R2  = list(numFmt = "0.0", halign = "right", indent = 2),
    DT     = list(valign = "top", wrapText = TRUE),
    D0_R1T = list(numFmt = "0",   halign = "right", valign = "top", indent = 1),
    D1_R1T = list(numFmt = "0.0", halign = "right", valign = "top", indent = 1)
  )

  # Generate border combinations for each base style (SAS lines 1419-1436)
  # Combinations: _BL, _BR, _BB, _BLR, _BLB, _BRB, _BLRB
  for (nm in names(base_defs)) {
    for (bl in c(FALSE, TRUE)) {
      for (br in c(FALSE, TRUE)) {
        for (bb in c(FALSE, TRUE)) {
          if (bl || br || bb) {
            suffix <- "_B"
            if (bl) suffix <- str_c(suffix, "L")
            if (br) suffix <- str_c(suffix, "R")
            if (bb) suffix <- str_c(suffix, "B")

            border_sides <- c()
            if (bl) border_sides <- c(border_sides, "left")
            if (br) border_sides <- c(border_sides, "right")
            if (bb) border_sides <- c(border_sides, "bottom")

            style_args <- c(
              base_defs[[nm]],
              list(border = border_sides, borderStyle = "thin")
            )
            styles[[str_c(nm, suffix)]] <- do.call(openxlsx::createStyle, style_args)
          }
        }
      }
    }
  }

  # Non-border base variants

  for (nm in names(base_defs)) {
    if (length(base_defs[[nm]]) > 0L) {
      styles[[nm]] <- do.call(openxlsx::createStyle, base_defs[[nm]])
    } else {
      styles[[nm]] <- openxlsx::createStyle()
    }
  }

  # Standard styles used across the workbook
  styles$Header <- openxlsx::createStyle(
    fontSize = 14, textDecoration = "bold"
  )
  styles$SubHeader <- openxlsx::createStyle(
    fontSize = 11, textDecoration = "bold"
  )
  styles$Default8 <- openxlsx::createStyle(fontSize = 8)
  styles$Default10 <- openxlsx::createStyle(fontSize = 10)
  styles$Default10Wrap <- openxlsx::createStyle(
    fontSize = 10, wrapText = TRUE
  )
  styles$Default10RedWrap <- openxlsx::createStyle(
    fontSize = 10, wrapText = TRUE, fontColour = "#FF0000"
  )
  styles$ColumnOutline <- openxlsx::createStyle(
    textDecoration = "bold",
    border = c("top", "bottom", "left", "right"),
    borderStyle = "thin"
  )
  # Data and DataBottom for error term table (SAS uses 'Data' / 'DataBottom')
  styles$Data <- openxlsx::createStyle(fontSize = 10)
  styles$DataBottom <- openxlsx::createStyle(
    fontSize = 10,
    border = "bottom", borderStyle = "thin"
  )
  # Bare default style (no special formatting)
  styles$Default <- openxlsx::createStyle()

  # Silver fill for alternating rows
  styles$AltRowFill <- openxlsx::createStyle(fgFill = "#C0C0C0")

  styles
}


# ==============================================================================
# out_cover -- Front Page (Cover Sheet) Worksheet
# ------------------------------------------------------------------------------
# Replaces SAS %out_cover macro (SAS lines 14-198).
# Writes the "Front Page" worksheet with analysis descriptions and panel
# settings to the openxlsx workbook.
#
# @param wb      An openxlsx Workbook object.
# @param config  Named list with study configuration:
#                ndabla, studyid, rundate, vld_sw, study_lag, dm_actarm,
#                sl_group_nobs, sl_subset_nobs, sl_gs_desc, sl_custom_ds
# @param results Named list with analysis results (used for conditional text).
# @return Invisible NULL. The workbook is modified in place.
# ==============================================================================
out_cover <- function(wb, config, results) {
  cli::cli_inform("Writing cover sheet: Front Page")

  sheet_name <- "Front Page"
  openxlsx::addWorksheet(wb, sheet_name)

  # ---- Header data (SAS lines 26-87) ----
  row_idx <- 1L

  # Row 1: blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Row 2: Title
  openxlsx::writeData(wb, sheet_name, x = "AE Severity Panel Front Page",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name,
                     style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
  row_idx <- row_idx + 1L

  # Row 3: blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Rows 4-6: NDA/BLA, Study, Run Date
  info_lines <- c(
    str_c("NDA/BLA: ", config$ndabla),
    str_c("Study: ", config$studyid),
    str_c("Analysis run date: ", config$rundate)
  )
  for (line in info_lines) {
    openxlsx::writeData(wb, sheet_name, x = line,
                        startRow = row_idx, startCol = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # Row 7: blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Analysis descriptions, each with subheader + wrapped description
  analysis_descs <- list(
    list(
      title = "1. Adverse Events by Arm Greater than 2%",
      desc  = str_c(
        "This analysis shows all adverse events that occur ",
        "in more than 2% of subjects in any treatment arm. Each adverse event is ",
        "counted only once per subject."
      )
    ),
    list(
      title = "2. Serious Adverse Events by Arm",
      desc  = str_c(
        "This analysis shows all adverse events that were considered serious. ",
        "Calculations are performed the same as Analysis 1, except ",
        "only adverse events with a 'Y' in the AESER variable from the AE ",
        "dataset are used."
      )
    ),
    list(
      title = "3. Adverse Events by Severity",
      desc  = str_c(
        "This analysis shows all adverse events in the study and the ",
        "number of times they occur by arm and severity level, using the ",
        "AESEV variable from the AE dataset if it is available."
      )
    ),
    list(
      title = "4. Serious Adverse Events by Severity",
      desc  = str_c(
        "This analysis shows all adverse events that were considered serious ",
        "by arm and severity level. Calculations are performed the same as ",
        "Analysis 1, except only adverse events with a ",
        "'Y' in the AESER variable from the AE dataset are used."
      )
    )
  )

  sub_style <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold")
  wrap_style <- openxlsx::createStyle(fontSize = 10, wrapText = TRUE)

  for (ad in analysis_descs) {
    # Subheader row
    openxlsx::writeData(wb, sheet_name, x = ad$title,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = sub_style,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
    row_idx <- row_idx + 1L

    # Description row
    openxlsx::writeData(wb, sheet_name, x = ad$desc,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
    row_height <- max(1, ceiling(nchar(str_trim(ad$desc)) / 100)) * 12.75
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                            heights = row_height)
    row_idx <- row_idx + 1L

    # Blank separator
    openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
    row_idx <- row_idx + 1L
  }

  # Extra blank row
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Method and Calculations (SAS lines 70-84) ----
  openxlsx::writeData(wb, sheet_name, x = "Method and Calculations",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = sub_style,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
  row_idx <- row_idx + 1L

  # Build method text with conditional exposure clause (SAS lines 71-84)
  method_text <- str_c(
    "For all analyses in this report, an adverse event is determined by the ",
    "body system or organ class (AEBODSYS) and dictionary-defined term ",
    "(AEDECOD) from the adverse event (AE) dataset. "
  )

  if (isTRUE(config$vld_sw)) {
    lag_text <- if (config$study_lag != 0) {
      str_c(config$study_lag, " days after ")
    } else {
      ""
    }
    method_text <- str_c(
      method_text,
      "Only adverse events with a start date between subjects' ",
      "first exposure and ", lag_text,
      "subjects' last exposure are included in the analysis. ",
      "Exposure dates are taken from variables EXSTDTC and EXENDTC in the ",
      "exposure (EX) dataset; if these dates are not available, the subjects' ",
      "reference start and end dates (RFSTDTC and RFENDTC) from the ",
      "demographics (DM) dataset are used instead. "
    )
  }

  arm_desc <- dplyr::if_else(
    isTRUE(config$dm_actarm),
    "actual treatment arm (ACTARM)",
    "planned treatment arm (ARM)"
  )
  method_text <- str_c(method_text, "Treatment arm is determined using the ",
                       arm_desc, " from DM.")

  openxlsx::writeData(wb, sheet_name, x = method_text,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
  method_height <- max(1, ceiling(nchar(str_trim(method_text)) / 100)) * 12.75
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = method_height)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Panel settings (SAS lines 107-143) ----
  settings_start <- row_idx

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Report Settings header
  openxlsx::writeData(wb, sheet_name, x = "Report Settings",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = sub_style,
                     rows = row_idx, cols = 1)
  row_idx <- row_idx + 1L

  # Setting rows: desc in col 1, setting in col 2
  setting_rows <- dplyr::tibble(
    desc = c(
      "   NDA/BLA:",
      "   Study:",
      "   Analysis run date:",
      "",
      "   Custom datasets:",
      "   Grouping/subsetting:"
    ),
    setting = c(
      config$ndabla,
      config$studyid,
      config$rundate,
      "",
      dplyr::if_else(
        !is.null(config$sl_custom_ds) && nchar(config$sl_custom_ds) > 0,
        config$sl_custom_ds, "None"
      ),
      dplyr::if_else(
        !is.null(config$sl_gs_desc) && nchar(config$sl_gs_desc) > 0,
        config$sl_gs_desc, "None"
      )
    )
  )

  for (i in seq_len(nrow(setting_rows))) {
    openxlsx::writeData(wb, sheet_name, x = setting_rows$desc[i],
                        startRow = row_idx, startCol = 1)
    openxlsx::writeData(wb, sheet_name, x = setting_rows$setting[i],
                        startRow = row_idx, startCol = 2)
    row_idx <- row_idx + 1L
  }

  # Additional grouping/subsetting info row (SAS lines 122-125)
  sl_group_nobs  <- if (!is.null(config$sl_group_nobs)) config$sl_group_nobs else 0L
  sl_subset_nobs <- if (!is.null(config$sl_subset_nobs)) config$sl_subset_nobs else 0L

  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {
    openxlsx::writeData(
      wb, sheet_name,
      x = str_c("For more information, see the Grouping and Subsetting tab ",
                "at the end of this workbook"),
      startRow = row_idx, startCol = 2
    )
    row_idx <- row_idx + 1L
  }

  # Blank rows
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Study analysis period (SAS lines 129-136)
  openxlsx::writeData(wb, sheet_name, x = "   Study analysis period: ",
                      startRow = row_idx, startCol = 1)
  if (isTRUE(config$vld_sw)) {
    period_text <- str_c(
      "Subject first exposure date to last exposure date",
      dplyr::if_else(config$study_lag > 0,
                     str_c(" + ", config$study_lag, " days"), "")
    )
  } else {
    period_text <- str_c(
      "Necessary date variables were not available; ",
      "all adverse events used in analysis"
    )
  }
  openxlsx::writeData(wb, sheet_name, x = period_text,
                      startRow = row_idx, startCol = 2)
  row_idx <- row_idx + 1L

  # Blank rows
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Crossover study note (SAS lines 141-142)
  crossover_text <- str_c(
    "Note that for crossover studies, the analysis by arm in this report ",
    "can only be used to examine treatment sequences and not individual ",
    "treatments."
  )
  openxlsx::writeData(wb, sheet_name, x = crossover_text,
                      startRow = row_idx, startCol = 1)
  red_wrap <- openxlsx::createStyle(
    fontSize = 10, wrapText = TRUE, fontColour = "#FF0000"
  )
  openxlsx::addStyle(wb, sheet_name, style = red_wrap,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:11, rows = row_idx)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 25.5)

  # Column width
  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 50)

  # Page setup (SAS lines 168-183)
  openxlsx::pageSetup(wb, sheet_name, orientation = "portrait",
                      scale = 95, printTitleRows = 1:2)
  header_text <- str_c("&LAE Severity Front Page",
                       "&RNDA/BLA ", config$ndabla,
                       "\nStudy ", config$studyid)
  openxlsx::setHeaderFooter(wb, sheet_name,
                            header = c(NA, NA, header_text),
                            footer = c(NA, "Page &P of &N", NA))

  invisible(NULL)
}


# ==============================================================================
# out_ab -- Analyses A/B Worksheets (AEs by Arm / Serious AEs by Arm)
# ------------------------------------------------------------------------------
# Replaces SAS %out_ab(rpt=) macro (SAS lines 205-477).
# Writes a worksheet with a 3-row column header structure, data table with
# per-arm subject counts and percentages, and footer notes.
#
# @param wb      An openxlsx Workbook object.
# @param rpt     Character "A" or "B".
# @param ab_data Data frame with columns: aebodsys, aedecod,
#                arm_sum_1..arm_sum_N, arm_pct_1..arm_pct_N,
#                arm_sum_total, arm_pct_total.
# @param config  Named list with study configuration including:
#                ndabla, studyid, rundate, arm_count, arm_names (character
#                vector), arm_n (numeric vector of N per arm), arm_total,
#                vld_sw, study_lag, ae_aeser, all_ae_dm_ex_aeser_y,
#                all_ae_dm_ex_aeser_n, sl_group_nobs, sl_subset_nobs,
#                sl_gs_desc, max_arm_nm_len.
# @param styles  Named list of openxlsx Style objects from create_ae_styles().
# @return Invisible NULL. The workbook is modified in place.
# ==============================================================================
out_ab <- function(wb, rpt, ab_data, config, styles) {
  rpt <- toupper(rpt)
  sheet_name <- dplyr::if_else(rpt == "A", "1 AEs by Arm", "2 Serious AEs by Arm")

  cli::cli_inform("Writing worksheet: {sheet_name}")

  openxlsx::addWorksheet(wb, sheet_name)

  arm_count   <- config$arm_count
  arm_names   <- config$arm_names
  arm_n       <- config$arm_n
  arm_total   <- config$arm_total
  nobs        <- nrow(ab_data)
  nvars       <- ncol(ab_data)
  nkeycols    <- 2L
  max_arm_len <- if (!is.null(config$max_arm_nm_len)) config$max_arm_nm_len else 20L

  # ---- Header section (SAS lines 222-282) ----
  row_idx <- 1L

  # Blank row
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Title
  title_text <- dplyr::if_else(
    rpt == "A",
    "Adverse Events by Organ Class and Term",
    "Serious Adverse Events by Organ Class and Term"
  )
  openxlsx::writeData(wb, sheet_name, x = title_text,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Header,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
  row_idx <- row_idx + 1L

  # Grouping/subsetting description (SAS lines 246-249)
  sl_group_nobs  <- if (!is.null(config$sl_group_nobs)) config$sl_group_nobs else 0L
  sl_subset_nobs <- if (!is.null(config$sl_subset_nobs)) config$sl_subset_nobs else 0L

  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {
    gs_desc <- if (!is.null(config$sl_gs_desc)) config$sl_gs_desc else ""
    openxlsx::writeData(wb, sheet_name, x = gs_desc,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default10,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # NDA/BLA, Study, Run Date (Default8 style)
  for (line in c(str_c("NDA/BLA: ", config$ndabla),
                 str_c("Study: ", config$studyid),
                 str_c("Analysis run date: ", config$rundate))) {
    openxlsx::writeData(wb, sheet_name, x = line,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default8,
                       rows = row_idx, cols = 1)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Methodology note (SAS lines 268-277)
  serious_text <- dplyr::if_else(rpt == "B", "serious ", "")
  method_note <- str_c(
    "Where subject count is the number of subjects in the treatment arm ",
    "experiencing at least one ", serious_text, "adverse event ",
    "per organ class and term"
  )
  if (rpt == "A") {
    method_note <- str_c(
      method_note,
      " and greater than 2% of subjects in any arm experienced at least one ",
      "adverse event"
    )
  }

  openxlsx::writeData(wb, sheet_name, x = method_note,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = row_idx)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 30)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Column headers (SAS lines 312-362) — 3-row structure ----
  col_header_start <- row_idx

  # Row 1: Body System, Dict Term, per-arm names spanning 2 cols, Total spanning 2
  col_header_height <- max(30, floor(max_arm_len / 15) * 13.75)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = col_header_height)

  openxlsx::writeData(wb, sheet_name, x = "Body System or Organ Class",
                      startRow = row_idx, startCol = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = row_idx:(row_idx + 2L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 1)

  openxlsx::writeData(wb, sheet_name, x = "Dictionary-Derived Term",
                      startRow = row_idx, startCol = 2)
  openxlsx::mergeCells(wb, sheet_name, cols = 2, rows = row_idx:(row_idx + 2L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 2)

  col_offset <- nkeycols + 1L
  for (i in seq_len(arm_count)) {
    openxlsx::writeData(wb, sheet_name, x = arm_names[i],
                        startRow = row_idx, startCol = col_offset)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = col_offset:(col_offset + 1L),
                         rows = row_idx)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset + 1L)
    col_offset <- col_offset + 2L
  }
  # Total spanning 2 cols
  openxlsx::writeData(wb, sheet_name, x = "Total",
                      startRow = row_idx, startCol = col_offset)
  openxlsx::mergeCells(wb, sheet_name,
                       cols = col_offset:(col_offset + 1L),
                       rows = row_idx)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset + 1L)

  row_idx <- row_idx + 1L

  # Row 2: N=xxx per arm (SAS lines 338-350)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 15)
  col_offset <- nkeycols + 1L
  for (i in seq_len(arm_count)) {
    n_label <- str_c("N=", format(arm_n[i], big.mark = ",", trim = TRUE))
    openxlsx::writeData(wb, sheet_name, x = n_label,
                        startRow = row_idx, startCol = col_offset)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = col_offset:(col_offset + 1L),
                         rows = row_idx)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset + 1L)
    col_offset <- col_offset + 2L
  }
  # Total N
  n_total_label <- str_c("N=", format(arm_total, big.mark = ",", trim = TRUE))
  openxlsx::writeData(wb, sheet_name, x = n_total_label,
                      startRow = row_idx, startCol = col_offset)
  openxlsx::mergeCells(wb, sheet_name,
                       cols = col_offset:(col_offset + 1L),
                       rows = row_idx)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset + 1L)

  row_idx <- row_idx + 1L

  # Row 3: "Subject Count" / "%" alternating (SAS lines 352-361)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 30)
  col_offset <- nkeycols + 1L
  for (i in seq_len(arm_count + 1L)) {
    openxlsx::writeData(wb, sheet_name, x = "Subject Count",
                        startRow = row_idx, startCol = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    col_offset <- col_offset + 1L
    openxlsx::writeData(wb, sheet_name, x = "%",
                        startRow = row_idx, startCol = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    col_offset <- col_offset + 1L
  }
  row_idx <- row_idx + 1L

  # ---- Data table (SAS lines 367-380) ----
  data_start_row <- row_idx

  if (nobs > 0L) {
    openxlsx::writeData(wb, sheet_name, x = ab_data,
                        startRow = data_start_row, startCol = 1,
                        colNames = FALSE)

    # Apply styles column by column (SAS lines 370-378)
    col_names_vec <- names(ab_data)
    for (c_idx in seq_along(col_names_vec)) {
      cn <- col_names_vec[c_idx]
      style_base <- dplyr::case_when(
        cn %in% c("aebodsys", "aedecod") ~ "D_BLR",
        str_detect(cn, "sum")             ~ "D0_R2_BL",
        str_detect(cn, "pct")             ~ "D1_R2_BR",
        TRUE                              ~ "D_BLR"
      )
      # Apply non-bottom style to all rows except last
      if (nobs > 1L) {
        s <- styles[[style_base]]
        if (!is.null(s)) {
          openxlsx::addStyle(wb, sheet_name, style = s,
                             rows = data_start_row:(data_start_row + nobs - 2L),
                             cols = c_idx, gridExpand = TRUE)
        }
      }
      # Bottom row gets 'B' suffix (SAS line 377)
      bottom_style_name <- str_c(style_base, "B")
      sb <- styles[[bottom_style_name]]
      if (!is.null(sb)) {
        openxlsx::addStyle(wb, sheet_name, style = sb,
                           rows = data_start_row + nobs - 1L,
                           cols = c_idx)
      }
    }

    # Alternating row fill (SAS lines 452-459)
    for (r in seq(from = data_start_row, to = data_start_row + nobs - 1L, by = 2L)) {
      openxlsx::addStyle(wb, sheet_name, style = styles$AltRowFill,
                         rows = r, cols = seq_len(nvars),
                         gridExpand = TRUE, stack = TRUE)
    }

    row_idx <- data_start_row + nobs
  }

  # ---- Footer / Notes (SAS lines 288-306) ----
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  openxlsx::writeData(wb, sheet_name, x = "NOTES:",
                      startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  note1 <- "1 This analysis uses the safety population "
  if (isTRUE(config$vld_sw)) {
    lag_text <- dplyr::if_else(
      config$study_lag > 0,
      str_c(config$study_lag, " days after the subject's "), ""
    )
    note1 <- str_c(
      note1,
      "and only counts adverse events that start between a subject's ",
      "first exposure and ", lag_text, "last exposure"
    )
  }
  openxlsx::writeData(wb, sheet_name, x = note1,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
  row_idx <- row_idx + 1L

  # Conditional note for report B when all AEs are serious (SAS lines 301-305)
  if (rpt == "B" && isTRUE(config$ae_aeser)) {
    if (isTRUE(config$all_ae_dm_ex_aeser_y) &&
        !isTRUE(config$all_ae_dm_ex_aeser_n)) {
      all_serious_note <- str_c(
        "* All adverse events in this study were marked serious (AESER = Y)"
      )
      openxlsx::writeData(wb, sheet_name, x = all_serious_note,
                          startRow = row_idx, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                         rows = row_idx, cols = 1)
      openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
      row_idx <- row_idx + 1L
    }
  }

  # ---- Freeze panes (SAS lines 428-434) ----
  openxlsx::freezePane(wb, sheet_name,
                       firstActiveRow = data_start_row,
                       firstActiveCol = nkeycols + 1L)

  # ---- Auto filter (SAS lines 447-450) ----
  openxlsx::addFilter(wb, sheet_name,
                      rows = data_start_row - 1L,
                      cols = seq_len(nvars))

  # ---- Column widths ----
  openxlsx::setColWidths(wb, sheet_name, cols = 1:nkeycols, widths = 200 / 7)
  if (nvars > nkeycols) {
    openxlsx::setColWidths(wb, sheet_name,
                           cols = (nkeycols + 1L):nvars,
                           widths = 61.5 / 7)
  }

  # ---- Page setup (SAS lines 412-460) ----
  openxlsx::pageSetup(wb, sheet_name, orientation = "landscape",
                      fitToWidth = TRUE, fitToHeight = FALSE)
  header_text <- str_c("&L", title_text,
                       "&RNDA/BLA ", config$ndabla,
                       "\nStudy ", config$studyid)
  openxlsx::setHeaderFooter(wb, sheet_name,
                            header = c(NA, NA, header_text),
                            footer = c(NA, "Page &P of &N", NA))

  invisible(NULL)
}


# ==============================================================================
# out_cd -- Analyses C/D Worksheets (AEs by Severity / Serious AEs by Severity)
# ------------------------------------------------------------------------------
# Replaces SAS %out_cd(rpt=) macro (SAS lines 483-739).
# Writes a worksheet with arm x severity column headers, data table showing
# severity counts per body system/term, and footer notes.
#
# @param wb      An openxlsx Workbook object.
# @param rpt     Character "C" or "D".
# @param cd_data Data frame with columns: aebodsys, aedecod, then per-arm
#                severity columns (arm1_sev1..arm1_sevN, arm2_sev1..etc.),
#                sum_total.
# @param config  Named list with study configuration including:
#                ndabla, studyid, rundate, arm_count, arm_names, sev_count,
#                sev_names (character vector of severity level names),
#                vld_sw, study_lag, ae_aeser, all_ae_dm_ex_aeser_y,
#                all_ae_dm_ex_aeser_n, sl_group_nobs, sl_subset_nobs,
#                sl_gs_desc, max_arm_nm_len, max_aesev_nm_len.
# @param styles  Named list of openxlsx Style objects from create_ae_styles().
# @return Invisible NULL. The workbook is modified in place.
# ==============================================================================
out_cd <- function(wb, rpt, cd_data, config, styles) {
  rpt <- toupper(rpt)
  sheet_name <- dplyr::if_else(rpt == "C",
                               "3 AEs by Severity",
                               "4 Serious AEs by Severity")

  cli::cli_inform("Writing worksheet: {sheet_name}")

  openxlsx::addWorksheet(wb, sheet_name)

  arm_count    <- config$arm_count
  arm_names    <- config$arm_names
  sev_count    <- config$sev_count
  sev_names    <- config$sev_names
  nobs         <- nrow(cd_data)
  nvars        <- ncol(cd_data)
  nkeycols     <- 2L
  max_arm_len  <- if (!is.null(config$max_arm_nm_len)) config$max_arm_nm_len else 20L
  max_sev_len  <- if (!is.null(config$max_aesev_nm_len)) config$max_aesev_nm_len else 10L

  # ---- Header section (SAS lines 500-555) ----
  row_idx <- 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Title
  title_text <- dplyr::if_else(
    rpt == "C",
    "Adverse Events by Severity Level",
    "Serious Adverse Events by Severity Level"
  )
  openxlsx::writeData(wb, sheet_name, x = title_text,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Header,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
  row_idx <- row_idx + 1L

  # Grouping/subsetting description
  sl_group_nobs  <- if (!is.null(config$sl_group_nobs)) config$sl_group_nobs else 0L
  sl_subset_nobs <- if (!is.null(config$sl_subset_nobs)) config$sl_subset_nobs else 0L

  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {
    gs_desc <- if (!is.null(config$sl_gs_desc)) config$sl_gs_desc else ""
    openxlsx::writeData(wb, sheet_name, x = gs_desc,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default10,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # NDA/BLA, Study, Run Date
  for (line in c(str_c("NDA/BLA: ", config$ndabla),
                 str_c("Study: ", config$studyid),
                 str_c("Analysis run date: ", config$rundate))) {
    openxlsx::writeData(wb, sheet_name, x = line,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default8,
                       rows = row_idx, cols = 1)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Methodology note (SAS lines 544-554)
  serious_cd_text <- dplyr::if_else(rpt == "D", "serious ", "")
  method_cd_note <- str_c(
    "Where the number in each column is the number of ", serious_cd_text,
    "adverse events per treatment arm at the stated severity level."
  )
  openxlsx::writeData(wb, sheet_name, x = method_cd_note,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                     rows = row_idx, cols = 1)
  merge_span <- min(nvars, nkeycols + 5L)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:merge_span, rows = row_idx)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 12.75)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Column headers — 3-row structure (SAS lines 580-622) ----
  col_header_start <- row_idx

  # Row 1: Body System, Dict Term (span 3 rows), per-arm (span sev_count),
  #         Total (span 3 rows)
  header_height <- max(20, floor(max_arm_len / (sev_count * 6.5)) * 13.75)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = header_height)

  openxlsx::writeData(wb, sheet_name, x = "Body System or Organ Class",
                      startRow = row_idx, startCol = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = row_idx:(row_idx + 2L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 1)

  openxlsx::writeData(wb, sheet_name, x = "Dictionary-Derived Term",
                      startRow = row_idx, startCol = 2)
  openxlsx::mergeCells(wb, sheet_name, cols = 2, rows = row_idx:(row_idx + 2L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 2)

  col_offset <- nkeycols + 1L
  for (i in seq_len(arm_count)) {
    openxlsx::writeData(wb, sheet_name, x = arm_names[i],
                        startRow = row_idx, startCol = col_offset)
    end_col <- col_offset + sev_count - 1L
    if (sev_count > 1L) {
      openxlsx::mergeCells(wb, sheet_name,
                           cols = col_offset:end_col,
                           rows = row_idx:(row_idx + 1L))
    }
    for (cc in col_offset:end_col) {
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = cc)
    }
    col_offset <- end_col + 1L
  }
  # Total column — spans 3 rows
  openxlsx::writeData(wb, sheet_name, x = "Total",
                      startRow = row_idx, startCol = col_offset)
  openxlsx::mergeCells(wb, sheet_name, cols = col_offset,
                       rows = row_idx:(row_idx + 2L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset)

  row_idx <- row_idx + 1L

  # Row 2: spacer (SAS lines 602-609) — blank for total column area
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 15)
  row_idx <- row_idx + 1L

  # Row 3: Severity level names under each arm (SAS lines 612-621)
  sev_row_height <- max(30, floor(max_sev_len / 5) * 13.75)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = sev_row_height)

  col_offset <- nkeycols + 1L
  for (i in seq_len(arm_count)) {
    for (j in seq_len(sev_count)) {
      openxlsx::writeData(wb, sheet_name, x = sev_names[j],
                          startRow = row_idx, startCol = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      col_offset <- col_offset + 1L
    }
  }
  row_idx <- row_idx + 1L

  # ---- Data table (SAS lines 627-643) ----
  data_start_row <- row_idx

  if (nobs > 0L) {
    openxlsx::writeData(wb, sheet_name, x = cd_data,
                        startRow = data_start_row, startCol = 1,
                        colNames = FALSE)

    # Apply styles column by column (SAS lines 631-641)
    col_names_vec <- names(cd_data)
    for (c_idx in seq_along(col_names_vec)) {
      cn <- col_names_vec[c_idx]
      style_base <- dplyr::case_when(
        cn %in% c("aebodsys", "aedecod") ~ "D_BLR",
        cn == "sum_total"                 ~ "D0_R2_BLR",
        str_detect(cn, "sev1")            ~ "D0_R2_BL",
        str_detect(cn, str_c("sev", sev_count)) ~ "D0_R2_BR",
        TRUE                              ~ "D0_R2_B"
      )

      if (nobs > 1L) {
        s <- styles[[style_base]]
        if (!is.null(s)) {
          openxlsx::addStyle(wb, sheet_name, style = s,
                             rows = data_start_row:(data_start_row + nobs - 2L),
                             cols = c_idx, gridExpand = TRUE)
        }
      }
      # Bottom row (SAS line 640)
      bottom_style_name <- str_c(style_base, "B")
      sb <- styles[[bottom_style_name]]
      if (!is.null(sb)) {
        openxlsx::addStyle(wb, sheet_name, style = sb,
                           rows = data_start_row + nobs - 1L,
                           cols = c_idx)
      }
    }

    # Alternating row fill
    for (r in seq(from = data_start_row, to = data_start_row + nobs - 1L, by = 2L)) {
      openxlsx::addStyle(wb, sheet_name, style = styles$AltRowFill,
                         rows = r, cols = seq_len(nvars),
                         gridExpand = TRUE, stack = TRUE)
    }

    row_idx <- data_start_row + nobs
  }

  # ---- Footer / Notes (SAS lines 559-574) ----
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  openxlsx::writeData(wb, sheet_name, x = "NOTES:",
                      startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  lag_text <- dplyr::if_else(
    config$study_lag > 0,
    str_c(config$study_lag, " days after the subject's "), ""
  )
  note_cd <- str_c(
    "1 This analysis uses the safety population ",
    "and only counts adverse events that are treatment emergent between ",
    "a subject's first exposure and ", lag_text, "last exposure"
  )
  openxlsx::writeData(wb, sheet_name, x = note_cd,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                     rows = row_idx, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
  row_idx <- row_idx + 1L

  # Conditional note for D when all AEs are serious (SAS lines 571-573)
  if (rpt == "D" &&
      isTRUE(config$all_ae_dm_ex_aeser_y) &&
      !isTRUE(config$all_ae_dm_ex_aeser_n)) {
    openxlsx::writeData(
      wb, sheet_name,
      x = "* All adverse events in this study were marked serious (AESER = Y)",
      startRow = row_idx, startCol = 1
    )
    openxlsx::addStyle(wb, sheet_name, style = styles$Default10Wrap,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:nvars, rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # ---- Freeze panes ----
  openxlsx::freezePane(wb, sheet_name,
                       firstActiveRow = data_start_row,
                       firstActiveCol = nkeycols + 1L)

  # ---- Auto filter ----
  openxlsx::addFilter(wb, sheet_name,
                      rows = data_start_row - 1L,
                      cols = seq_len(nvars))

  # ---- Column widths ----
  openxlsx::setColWidths(wb, sheet_name, cols = 1:nkeycols, widths = 200 / 7)
  if (nvars > nkeycols) {
    openxlsx::setColWidths(wb, sheet_name,
                           cols = (nkeycols + 1L):nvars,
                           widths = 50 / 7)
  }

  # ---- Page setup ----
  openxlsx::pageSetup(wb, sheet_name, orientation = "landscape",
                      fitToWidth = TRUE, fitToHeight = FALSE)
  header_text <- str_c("&L", title_text,
                       "&RNDA/BLA ", config$ndabla,
                       "\nStudy ", config$studyid)
  openxlsx::setHeaderFooter(wb, sheet_name,
                            header = c(NA, NA, header_text),
                            footer = c(NA, "Page &P of &N", NA))

  invisible(NULL)
}


# ==============================================================================
# out_err -- Data Check Summary Worksheet
# ------------------------------------------------------------------------------
# Replaces SAS %out_err macro (SAS lines 745-1289).
# Writes the "Data Check Summary" worksheet containing:
#   - Header with title, NDA/BLA, Study, Run Date
#   - Subject Validation section (rpt_dm table)
#   - AE Data Validation section (rpt_err summary + rpt_err_term by-term)
#   - Missing Severity Levels section (rpt_missing, conditional on ae_aesev)
#
# @param wb      An openxlsx Workbook object.
# @param config  Named list with study configuration.
# @param results Named list with analysis results including:
#                rpt_dm, rpt_err, rpt_err_term, rpt_missing, naes_sp,
#                naes_sp_by_arm (numeric vector).
# @param styles  Named list of openxlsx Style objects from create_ae_styles().
# @return Invisible NULL. The workbook is modified in place.
# ==============================================================================
out_err <- function(wb, config, results, styles) {
  cli::cli_inform("Writing worksheet: Data Check Summary")

  sheet_name <- "Data Check Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  arm_count  <- config$arm_count
  arm_names  <- config$arm_names
  max_arm_len <- if (!is.null(config$max_arm_nm_len)) config$max_arm_nm_len else 20L

  # Column width (SAS line 761: Width=266)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 266 / 7)

  row_idx <- 1L

  # ---- Worksheet Header (SAS lines 769-806) ----
  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Title
  openxlsx::writeData(wb, sheet_name, x = "Adverse Events Data Check Summary",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$Header,
                     rows = row_idx, cols = 1)
  row_idx <- row_idx + 1L

  # Grouping/subsetting description
  sl_group_nobs  <- if (!is.null(config$sl_group_nobs)) config$sl_group_nobs else 0L
  sl_subset_nobs <- if (!is.null(config$sl_subset_nobs)) config$sl_subset_nobs else 0L

  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {
    gs_desc <- if (!is.null(config$sl_gs_desc)) config$sl_gs_desc else ""
    openxlsx::writeData(wb, sheet_name, x = gs_desc,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default10,
                       rows = row_idx, cols = 1)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # NDA/BLA, Study, Run Date
  for (line in c(str_c("NDA/BLA: ", config$ndabla),
                 str_c("Study: ", config$studyid),
                 str_c("Analysis run date: ", config$rundate))) {
    openxlsx::writeData(wb, sheet_name, x = line,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$Default8,
                       rows = row_idx, cols = 1)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ===================================================================
  # Subject Validation section (SAS lines 817-913)
  # ===================================================================
  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Section header
  openxlsx::writeData(wb, sheet_name, x = "Subject Validation",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$SubHeader,
                     rows = row_idx, cols = 1)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Description text (SAS lines 840-846)
  sv_desc <- str_c(
    "Subjects are validated before their adverse events are used in this ",
    "report's analysis. Subjects are excluded if they fail screening or ",
    "are unassigned to an arm, are not in the safety population, or are ",
    "missing treatment and reference start and end dates. The following ",
    "table shows how many subjects were in the demographics (DM) dataset, ",
    "how many were removed for each of these reasons, and how many ",
    "remained whose adverse events were used in the analysis."
  )
  openxlsx::writeData(wb, sheet_name, x = sv_desc,
                      startRow = row_idx, startCol = 1)
  wrap_style <- openxlsx::createStyle(fontSize = 10, wrapText = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                     rows = row_idx, cols = 1)
  desc_height <- max(1, ceiling(nchar(str_trim(sv_desc)) / 130)) * 12.75
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = desc_height)
  total_data_cols <- 2 * (arm_count + 1L) + 1L
  openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols, rows = row_idx)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Subject Validation Column Headers (SAS lines 860-895) ----
  sv_col_start <- row_idx

  # Row 1: spanning title
  openxlsx::writeData(wb, sheet_name, x = "Subject Validation Summary",
                      startRow = row_idx, startCol = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols, rows = row_idx)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 1)
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 35)
  row_idx <- row_idx + 1L

  # Row 2: "Subject Validation Step" (MergeDown=1), per-arm names spanning 2
  arm_header_height <- 13.75 + max(1, round(max_arm_len / 12, 1)) * 13.75  # intentional base R round for layout
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                          heights = arm_header_height)

  openxlsx::writeData(wb, sheet_name, x = "Subject Validation Step",
                      startRow = row_idx, startCol = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = row_idx:(row_idx + 1L))
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = 1)

  col_offset <- 2L
  for (i in seq_len(arm_count)) {
    openxlsx::writeData(wb, sheet_name, x = arm_names[i],
                        startRow = row_idx, startCol = col_offset)
    openxlsx::mergeCells(wb, sheet_name,
                         cols = col_offset:(col_offset + 1L),
                         rows = row_idx)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset + 1L)
    col_offset <- col_offset + 2L
  }
  openxlsx::writeData(wb, sheet_name, x = "Total",
                      startRow = row_idx, startCol = col_offset)
  openxlsx::mergeCells(wb, sheet_name,
                       cols = col_offset:(col_offset + 1L),
                       rows = row_idx)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset)
  openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                     rows = row_idx, cols = col_offset + 1L)

  row_idx <- row_idx + 1L

  # Row 3: "Subject Count" / "%" under each arm+total
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 27)
  col_offset <- 2L
  for (i in seq_len(arm_count + 1L)) {
    openxlsx::writeData(wb, sheet_name, x = "Subject Count",
                        startRow = row_idx, startCol = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    col_offset <- col_offset + 1L
    openxlsx::writeData(wb, sheet_name, x = "%",
                        startRow = row_idx, startCol = col_offset)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = col_offset)
    col_offset <- col_offset + 1L
  }
  row_idx <- row_idx + 1L

  # ---- Subject Validation Data Table (SAS lines 899-913) ----
  rpt_dm <- results$rpt_dm
  if (!is.null(rpt_dm) && is.data.frame(rpt_dm) && nrow(rpt_dm) > 0L) {
    dm_start_row <- row_idx
    openxlsx::writeData(wb, sheet_name, x = rpt_dm,
                        startRow = dm_start_row, startCol = 1,
                        colNames = FALSE)

    dm_nobs <- nrow(rpt_dm)
    dm_cols <- names(rpt_dm)
    for (c_idx in seq_along(dm_cols)) {
      cn <- dm_cols[c_idx]
      style_base <- dplyr::case_when(
        cn == "desc"               ~ "D_BLR",
        str_detect(cn, "count")    ~ "D0_R1_BL",
        str_detect(cn, "pct")      ~ "D1_R1_BR",
        TRUE                       ~ "D_BLR"
      )
      if (dm_nobs > 1L) {
        s <- styles[[style_base]]
        if (!is.null(s)) {
          openxlsx::addStyle(wb, sheet_name, style = s,
                             rows = dm_start_row:(dm_start_row + dm_nobs - 2L),
                             cols = c_idx, gridExpand = TRUE)
        }
      }
      bottom_name <- str_c(style_base, "B")
      sb <- styles[[bottom_name]]
      if (!is.null(sb)) {
        openxlsx::addStyle(wb, sheet_name, style = sb,
                           rows = dm_start_row + dm_nobs - 1L,
                           cols = c_idx)
      }
    }
    row_idx <- dm_start_row + dm_nobs
  }

  # ===================================================================
  # AE Data Validation section (SAS lines 916-1104)
  # ===================================================================
  # Blank row separator
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Section header
  openxlsx::writeData(wb, sheet_name, x = "Adverse Events Data Validation",
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = styles$SubHeader,
                     rows = row_idx, cols = 1)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Description text (SAS lines 948-952)
  naes_sp <- if (!is.null(results$naes_sp)) results$naes_sp else 0L
  naes_sp_formatted <- format(naes_sp, big.mark = ",", trim = TRUE)
  vld_desc <- str_c(
    "Data validation is performed on all adverse events experienced by ",
    "validated subjects, of which there were ", naes_sp_formatted,
    " in this study. Adverse events can be excluded for the following reasons:"
  )
  openxlsx::writeData(wb, sheet_name, x = vld_desc,
                      startRow = row_idx, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                     rows = row_idx, cols = 1)
  vld_height <- max(1, ceiling(nchar(str_trim(vld_desc)) / 125)) * 12.75
  openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = vld_height)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols, rows = row_idx)
  row_idx <- row_idx + 1L

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # Exclusion reasons list (SAS lines 957-966)
  study_lag <- config$study_lag
  lag_suffix <- dplyr::if_else(study_lag != 0,
                               str_c(" + ", study_lag, " days"), "")
  excl_reasons <- c(
    str_c("   1. The start date of the adverse event was missing or could ",
          "not be interpreted as a date with at least month and year"),
    str_c("   2. The start date of the adverse event was not between the ",
          "subject's first exposure date and last exposure date", lag_suffix),
    str_c("   3. The body system or organ class (AEBODSYS) or dictionary-",
          "derived term (AEDECOD) was blank")
  )
  for (reason in excl_reasons) {
    openxlsx::writeData(wb, sheet_name, x = reason,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                       rows = row_idx, cols = 1)
    rea_height <- max(1, ceiling(nchar(str_trim(reason)) / 125)) * 12.75
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = rea_height)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols, rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # Determine if there were excluded AEs (SAS lines 921-924)
  rpt_err <- results$rpt_err
  vld_err <- !is.null(rpt_err) && is.data.frame(rpt_err) && nrow(rpt_err) > 0L

  if (isTRUE(config$vld_sw)) {
    if (!vld_err) {
      # No excluded AEs (SAS lines 969-973)
      openxlsx::writeData(wb, sheet_name, x = "",
                          startRow = row_idx, startCol = 1)
      row_idx <- row_idx + 1L
      openxlsx::writeData(
        wb, sheet_name,
        x = "No adverse events were excluded during data validation.",
        startRow = row_idx, startCol = 1
      )
      openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                         rows = row_idx, cols = 1)
      openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols,
                           rows = row_idx)
      row_idx <- row_idx + 1L
    } else {
      # Context note about excluded AEs (SAS lines 976-983)
      openxlsx::writeData(wb, sheet_name, x = "",
                          startRow = row_idx, startCol = 1)
      row_idx <- row_idx + 1L

      excl_note <- str_c(
        "The counts in the following two tables are counts of events and ",
        "not of subjects. A subject can have for example an adverse event ",
        "anaemia that passed validation and two that did not. That subject ",
        "is counted in the subject count for anaemia on prior tabs, and ",
        "the two adverse events that were excluded are counted here ",
        "individually."
      )
      openxlsx::writeData(wb, sheet_name, x = excl_note,
                          startRow = row_idx, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                         rows = row_idx, cols = 1)
      excl_h <- max(1, ceiling(nchar(str_trim(excl_note)) / 125)) * 12.75
      openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                              heights = excl_h)
      openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols,
                           rows = row_idx)
      row_idx <- row_idx + 1L
    }
  } else {
    # Validation not done (SAS lines 986-991)
    openxlsx::writeData(wb, sheet_name, x = "",
                        startRow = row_idx, startCol = 1)
    row_idx <- row_idx + 1L
    no_vld_text <- str_c(
      "Data validation was not done because necessary date variables ",
      "were not available. All adverse events were used in the analysis."
    )
    openxlsx::writeData(wb, sheet_name, x = no_vld_text,
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                       rows = row_idx, cols = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:total_data_cols,
                         rows = row_idx)
    row_idx <- row_idx + 1L
  }

  # Blank
  openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
  row_idx <- row_idx + 1L

  # ---- Excluded AE Summary and By-Term tables (SAS lines 1005-1104) ----
  if (vld_err) {
    naes_sp_by_arm <- if (!is.null(results$naes_sp_by_arm)) {
      results$naes_sp_by_arm
    } else {
      rep(0L, arm_count)
    }

    # Summary column headers (SAS lines 1010-1043)
    err_summary_cols <- 1L + 2L * arm_count
    openxlsx::writeData(
      wb, sheet_name,
      x = "Adverse Events Data Validation Summary",
      startRow = row_idx, startCol = 1
    )
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:err_summary_cols, rows = row_idx)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = 1)
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 35)
    row_idx <- row_idx + 1L

    # Row 2: Reason for Exclusion + per-arm N= headers
    err_arm_h <- 13.75 + max(1, round(max_arm_len / 13.25, 1)) * 13.75  # intentional base R round for layout
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                            heights = err_arm_h)

    openxlsx::writeData(wb, sheet_name, x = "Reason for Exclusion",
                        startRow = row_idx, startCol = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = row_idx:(row_idx + 1L))
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = 1)

    col_offset <- 2L
    for (i in seq_len(arm_count)) {
      n_fmt <- format(naes_sp_by_arm[i], big.mark = ",", trim = TRUE)
      arm_label <- str_c(arm_names[i], "\nN=", n_fmt)
      openxlsx::writeData(wb, sheet_name, x = arm_label,
                          startRow = row_idx, startCol = col_offset)
      openxlsx::mergeCells(wb, sheet_name,
                           cols = col_offset:(col_offset + 1L),
                           rows = row_idx)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset + 1L)
      col_offset <- col_offset + 2L
    }
    row_idx <- row_idx + 1L

    # Row 3: Event Count / %
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 27)
    col_offset <- 2L
    for (i in seq_len(arm_count)) {
      openxlsx::writeData(wb, sheet_name, x = "Event Count",
                          startRow = row_idx, startCol = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      col_offset <- col_offset + 1L
      openxlsx::writeData(wb, sheet_name, x = "%",
                          startRow = row_idx, startCol = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      col_offset <- col_offset + 1L
    }
    row_idx <- row_idx + 1L

    # Summary data table (SAS lines 1047-1060)
    err_nobs <- nrow(rpt_err)
    if (err_nobs > 0L) {
      err_start_row <- row_idx
      openxlsx::writeData(wb, sheet_name, x = rpt_err,
                          startRow = err_start_row, startCol = 1,
                          colNames = FALSE)

      err_cols <- names(rpt_err)
      for (c_idx in seq_along(err_cols)) {
        cn <- err_cols[c_idx]
        style_base <- dplyr::case_when(
          cn == "err_desc"            ~ "D_BLR",
          str_detect(cn, "count")     ~ "D0_R1_BL",
          str_detect(cn, "pct")       ~ "D1_R1_BR",
          TRUE                        ~ "D_BLR"
        )
        if (err_nobs > 1L) {
          s <- styles[[style_base]]
          if (!is.null(s)) {
            openxlsx::addStyle(wb, sheet_name, style = s,
                               rows = err_start_row:(err_start_row + err_nobs - 2L),
                               cols = c_idx, gridExpand = TRUE)
          }
        }
        bottom_name <- str_c(style_base, "B")
        sb <- styles[[bottom_name]]
        if (!is.null(sb)) {
          openxlsx::addStyle(wb, sheet_name, style = sb,
                             rows = err_start_row + err_nobs - 1L,
                             cols = c_idx)
        }
      }
      row_idx <- err_start_row + err_nobs
    }

    # Blank separator
    openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
    row_idx <- row_idx + 1L

    # ---- By-Term table (SAS lines 1062-1103) ----
    rpt_err_term <- results$rpt_err_term
    if (!is.null(rpt_err_term) && is.data.frame(rpt_err_term) &&
        nrow(rpt_err_term) > 0L) {
      term_total_cols <- 6L + 2L * arm_count

      # Title row
      openxlsx::writeData(
        wb, sheet_name,
        x = "Adverse Events Data Validation by Term",
        startRow = row_idx, startCol = 1
      )
      openxlsx::mergeCells(wb, sheet_name,
                           cols = 1:term_total_cols, rows = row_idx)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = 1)
      openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 25)
      row_idx <- row_idx + 1L

      # Column headers: Body System, Dict Term (span 5), per-arm Event Count
      term_arm_h <- 13.75 + max(1, round(max_arm_len / 13.25, 1)) * 13.75  # intentional base R round for layout
      openxlsx::setRowHeights(wb, sheet_name, rows = row_idx,
                              heights = term_arm_h)

      openxlsx::writeData(wb, sheet_name, x = "Body System or Organ Class",
                          startRow = row_idx, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = 1)

      openxlsx::writeData(wb, sheet_name, x = "Dictionary-Derived Term",
                          startRow = row_idx, startCol = 2)
      openxlsx::mergeCells(wb, sheet_name, cols = 2:6, rows = row_idx)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = 2)

      col_offset <- 7L
      for (i in seq_len(arm_count)) {
        arm_ec_label <- str_c(arm_names[i], "\nEvent Count")
        openxlsx::writeData(wb, sheet_name, x = arm_ec_label,
                            startRow = row_idx, startCol = col_offset)
        openxlsx::mergeCells(wb, sheet_name,
                             cols = col_offset:(col_offset + 1L),
                             rows = row_idx)
        openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                           rows = row_idx, cols = col_offset)
        openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                           rows = row_idx, cols = col_offset + 1L)
        col_offset <- col_offset + 2L
      }
      row_idx <- row_idx + 1L

      # By-term data
      term_start_row <- row_idx
      term_nobs <- nrow(rpt_err_term)
      openxlsx::writeData(wb, sheet_name, x = rpt_err_term,
                          startRow = term_start_row, startCol = 1,
                          colNames = FALSE)

      # Apply styles (SAS lines 1093-1101)
      term_cols <- names(rpt_err_term)
      for (c_idx in seq_along(term_cols)) {
        cn <- toupper(term_cols[c_idx])
        base_s <- "Data"
        s <- styles[[base_s]]
        if (!is.null(s)) {
          if (term_nobs > 1L) {
            openxlsx::addStyle(wb, sheet_name, style = s,
                               rows = term_start_row:(term_start_row + term_nobs - 2L),
                               cols = c_idx, gridExpand = TRUE)
          }
        }
        # Bottom row
        sb <- styles$DataBottom
        if (!is.null(sb)) {
          openxlsx::addStyle(wb, sheet_name, style = sb,
                             rows = term_start_row + term_nobs - 1L,
                             cols = c_idx)
        }
      }

      # Alternating row fill for by-term table
      for (r in seq(from = term_start_row, to = term_start_row + term_nobs - 1L, by = 2L)) {
        openxlsx::addStyle(wb, sheet_name, style = styles$AltRowFill,
                           rows = r, cols = seq_len(term_total_cols),
                           gridExpand = TRUE, stack = TRUE)
      }

      row_idx <- term_start_row + term_nobs
    }
  }

  # ===================================================================
  # Missing Severity Levels section (SAS lines 1107-1197)
  # ===================================================================
  if (isTRUE(config$ae_aesev)) {
    # Blank separator
    openxlsx::writeData(wb, sheet_name, x = "", startRow = row_idx, startCol = 1)
    row_idx <- row_idx + 1L

    # Section header
    openxlsx::writeData(wb, sheet_name, x = "Missing Severity Levels",
                        startRow = row_idx, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, style = styles$SubHeader,
                       rows = row_idx, cols = 1)
    row_idx <- row_idx + 1L

    # Notes
    missing_notes <- c(
      str_c("Adverse events with missing severity levels appear ",
            "in their own columns in the severity level reports."),
      str_c("Below, the total number of adverse events missing severity ",
            "levels in each report is shown for each arm.")
    )
    for (mn in missing_notes) {
      openxlsx::writeData(wb, sheet_name, x = mn,
                          startRow = row_idx, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, style = wrap_style,
                         rows = row_idx, cols = 1)
      mn_h <- max(1, ceiling(nchar(str_trim(mn)) / 125)) * 12.75
      openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = mn_h)
      miss_total_cols <- 1L + 2L * arm_count
      openxlsx::mergeCells(wb, sheet_name,
                           cols = 1:miss_total_cols, rows = row_idx)
      row_idx <- row_idx + 1L
    }

    # ---- Missing Severity Column Headers (SAS lines 1130-1159) ----
    # Row 1: spanning title
    openxlsx::writeData(
      wb, sheet_name,
      x = "Missing Severity Level Summary",
      startRow = row_idx, startCol = 1
    )
    openxlsx::mergeCells(wb, sheet_name,
                         cols = 1:miss_total_cols, rows = row_idx)
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = 1)
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 25)
    row_idx <- row_idx + 1L

    # Row 2: Report (MergeDown=1), per-arm names spanning 2
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 40)
    openxlsx::writeData(wb, sheet_name, x = "Report",
                        startRow = row_idx, startCol = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1,
                         rows = row_idx:(row_idx + 1L))
    openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                       rows = row_idx, cols = 1)

    col_offset <- 2L
    for (i in seq_len(arm_count)) {
      openxlsx::writeData(wb, sheet_name, x = arm_names[i],
                          startRow = row_idx, startCol = col_offset)
      openxlsx::mergeCells(wb, sheet_name,
                           cols = col_offset:(col_offset + 1L),
                           rows = row_idx)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset + 1L)
      col_offset <- col_offset + 2L
    }
    row_idx <- row_idx + 1L

    # Row 3: Missing Count / %
    openxlsx::setRowHeights(wb, sheet_name, rows = row_idx, heights = 30)
    col_offset <- 2L
    for (i in seq_len(arm_count)) {
      openxlsx::writeData(wb, sheet_name, x = "Missing Count",
                          startRow = row_idx, startCol = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      col_offset <- col_offset + 1L
      openxlsx::writeData(wb, sheet_name, x = "%",
                          startRow = row_idx, startCol = col_offset)
      openxlsx::addStyle(wb, sheet_name, style = styles$ColumnOutline,
                         rows = row_idx, cols = col_offset)
      col_offset <- col_offset + 1L
    }
    row_idx <- row_idx + 1L

    # ---- Missing Severity Data Table (SAS lines 1163-1179) ----
    rpt_missing <- results$rpt_missing
    if (!is.null(rpt_missing) && is.data.frame(rpt_missing) &&
        nrow(rpt_missing) > 0L) {
      miss_start_row <- row_idx
      miss_nobs <- nrow(rpt_missing)
      openxlsx::writeData(wb, sheet_name, x = rpt_missing,
                          startRow = miss_start_row, startCol = 1,
                          colNames = FALSE)

      miss_cols <- names(rpt_missing)
      for (c_idx in seq_along(miss_cols)) {
        cn <- miss_cols[c_idx]
        style_base <- dplyr::case_when(
          cn == "report"              ~ "DT_BLR",
          str_detect(cn, "missing")   ~ "D0_R1T_BL",
          str_detect(cn, "pct")       ~ "D1_R1T_BR",
          TRUE                        ~ "DT_BLR"
        )
        if (miss_nobs > 1L) {
          s <- styles[[style_base]]
          if (!is.null(s)) {
            openxlsx::addStyle(wb, sheet_name, style = s,
                               rows = miss_start_row:(miss_start_row + miss_nobs - 2L),
                               cols = c_idx, gridExpand = TRUE)
          }
        }
        bottom_name <- str_c(style_base, "B")
        sb <- styles[[bottom_name]]
        if (!is.null(sb)) {
          openxlsx::addStyle(wb, sheet_name, style = sb,
                             rows = miss_start_row + miss_nobs - 1L,
                             cols = c_idx)
        }
      }

      # Row heights for missing data
      for (r in miss_start_row:(miss_start_row + miss_nobs - 1L)) {
        openxlsx::setRowHeights(wb, sheet_name, rows = r, heights = 12.75)
      }

      row_idx <- miss_start_row + miss_nobs
    }
  }

  # ---- Page Setup (SAS lines 1255-1278) ----
  openxlsx::pageSetup(wb, sheet_name, orientation = "landscape",
                      fitToWidth = TRUE, fitToHeight = FALSE)
  header_text <- str_c("&LAdverse Events Data Check Summary",
                       "&RNDA/BLA ", config$ndabla,
                       "\nStudy ", config$studyid)
  openxlsx::setHeaderFooter(wb, sheet_name,
                            header = c(NA, NA, header_text),
                            footer = c(NA, "Page &P of &N", NA))

  invisible(NULL)
}


# ==============================================================================
# out_note -- Placeholder Worksheet for Unavailable Analyses
# ------------------------------------------------------------------------------
# Replaces SAS %out_note(rpt=) macro (SAS lines 1293-1360).
# Writes a simple worksheet with a title and explanation when an analysis
# (B, C, or D) cannot be generated because the required variable (AESER
# and/or AESEV) was not used in the study.
#
# @param wb     An openxlsx Workbook object.
# @param rpt    Character "B", "C", or "D".
# @param config Named list with ae_aeser (logical).
# @return Invisible NULL. The workbook is modified in place.
# ==============================================================================
out_note <- function(wb, rpt, config) {
  rpt <- toupper(rpt)

  # Determine worksheet name and content (SAS lines 1305-1341)
  ws_info <- dplyr::case_when(
    rpt == "B" ~ "Serious AEs by Arm",
    rpt == "C" ~ "AEs by Severity",
    rpt == "D" ~ "Serious AEs by Severity",
    TRUE       ~ "Note"
  )

  # Sheet tab names: prepend analysis number
  sheet_name <- dplyr::case_when(
    rpt == "B" ~ "2 Serious AEs by Arm",
    rpt == "C" ~ "3 AEs by Severity",
    rpt == "D" ~ "4 Serious AEs by Severity",
    TRUE       ~ "Note"
  )

  cli::cli_warn("Analysis {rpt} not available; writing note worksheet: {sheet_name}")

  openxlsx::addWorksheet(wb, sheet_name)

  # Title (SAS lines 1311-1325)
  title_text <- dplyr::case_when(
    rpt == "B" ~ "Serious Adverse Events by Organ Class and Term",
    rpt == "C" ~ "Adverse Events by Severity Level",
    rpt == "D" ~ "Serious Adverse Events by Severity Level",
    TRUE       ~ ""
  )

  # Subtitle explaining why unavailable (SAS lines 1329-1341)
  subtitle_text <- dplyr::case_when(
    rpt == "B" ~ "AESER, the serious event variable, was not used in this study.",
    rpt == "C" ~ "AESEV, the severity level variable, was not used in this study.",
    rpt == "D" && isTRUE(config$ae_aeser) ~
      "AESEV, the severity level variable, was not used in this study.",
    rpt == "D" ~
      str_c("AESER, the serious event variable, and AESEV, ",
            "the severity level variable, were not used in this study."),
    TRUE ~ ""
  )

  # Write title
  title_style <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold")
  openxlsx::writeData(wb, sheet_name, x = title_text,
                      startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = title_style,
                     rows = 1, cols = 1)

  # Write subtitle
  sub_style <- openxlsx::createStyle(fontSize = 10, wrapText = TRUE)
  openxlsx::writeData(wb, sheet_name, x = subtitle_text,
                      startRow = 2, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, style = sub_style,
                     rows = 2, cols = 1)

  invisible(NULL)
}


# ==============================================================================
# out_ae -- Main Orchestrator for AE Severity Panel Output
# ------------------------------------------------------------------------------
# Replaces SAS %out_ae macro (SAS lines 1449-1529).
# Creates the complete Excel workbook by orchestrating calls to the individual
# worksheet functions in the correct order, then saves to disk.
#
# @param config  Named list with all study configuration parameters:
#                ndabla, studyid, aeout1 (output file path),
#                vld_sw, study_lag, dm_actarm,
#                arm_count, arm_names (character vector),
#                arm_n (numeric vector), arm_total,
#                ae_aeser (logical), ae_aesev (logical),
#                all_ae_dm_ex_aeser_y, all_ae_dm_ex_aeser_n,
#                sev_count, sev_names,
#                sl_group_nobs, sl_subset_nobs, sl_gs_desc, sl_custom_ds,
#                max_arm_nm_len, max_aesev_nm_len.
# @param results Named list with analysis output data frames:
#                ab_a_output (always), ab_b_output (if ae_aeser),
#                cd_c_output (if ae_aesev), cd_d_output (if ae_aeser & ae_aesev),
#                rpt_dm, rpt_err, rpt_err_term, rpt_missing, naes_sp,
#                naes_sp_by_arm.
#                Optionally: sl_group, sl_subset, sl_datasets for G/S.
# @return Character string: path to saved workbook.
# ==============================================================================
out_ae <- function(config, results) {

  # ---- Log banner (SAS lines 1452-1465) ----
  cli::cli_alert_info(strrep("*", 49))
  cli::cli_alert_info("MAKING EXCEL OUTPUT FOR ADVERSE EVENTS REPORT")
  cli::cli_alert_info(strrep("*", 49))

  # ---- Run date (SAS lines 1467-1470) ----
  config$rundate <- format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")

  # ---- Create workbook (SAS line 1476: %wb) ----
  wb <- openxlsx::createWorkbook()

  # ---- Create styles (SAS lines 1477-1478: %styles; %out_ae_styles) ----
  styles <- create_ae_styles()

  # ---- Grouping & subsetting preprocessing (SAS lines 1472-1474) ----
  sl_group_nobs  <- if (!is.null(config$sl_group_nobs)) config$sl_group_nobs else 0L
  sl_subset_nobs <- if (!is.null(config$sl_subset_nobs)) config$sl_subset_nobs else 0L
  pp_result <- NULL

  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {
    tryCatch({
      pp_result <- group_subset_pp(
        sl_group   = results$sl_group,
        sl_subset  = results$sl_subset,
        sl_datasets = results$sl_datasets
      )
      # Update config with preprocessing results
      if (!is.null(pp_result$sl_gs_desc)) {
        config$sl_gs_desc <- pp_result$sl_gs_desc
      }
      if (!is.null(pp_result$sl_group_nobs)) {
        config$sl_group_nobs <- pp_result$sl_group_nobs
      }
      if (!is.null(pp_result$sl_subset_nobs)) {
        config$sl_subset_nobs <- pp_result$sl_subset_nobs
      }
    }, error = function(e) {
      cli::cli_warn("Grouping/subsetting preprocessing failed: {e$message}")
    })
  }

  # ---- Cover sheet (SAS line 1489) ----
  cli::cli_inform("Writing workbook cover sheet")
  out_cover(wb, config, results)

  # ---- Analysis A: AEs by Arm (always runs) (SAS line 1492) ----
  cli::cli_inform("Writing Analysis A: AEs by Arm")
  out_ab(wb, "A", results$ab_a_output, config, styles)

  # ---- Analysis B: Serious AEs by Arm (SAS lines 1493-1494) ----
  if (isTRUE(config$ae_aeser)) {
    cli::cli_inform("Writing Analysis B: Serious AEs by Arm")
    out_ab(wb, "B", results$ab_b_output, config, styles)
  } else {
    out_note(wb, "B", config)
  }

  # ---- Analyses C & D: AEs/Serious AEs by Severity (SAS lines 1497-1505) ----
  if (isTRUE(config$ae_aesev)) {
    cli::cli_inform("Writing Analysis C: AEs by Severity")
    out_cd(wb, "C", results$cd_c_output, config, styles)

    if (isTRUE(config$ae_aeser)) {
      cli::cli_inform("Writing Analysis D: Serious AEs by Severity")
      out_cd(wb, "D", results$cd_d_output, config, styles)
    } else {
      out_note(wb, "D", config)
    }
  } else {
    out_note(wb, "C", config)
    out_note(wb, "D", config)
  }

  # ---- Error summary sheet (SAS line 1506) ----
  cli::cli_inform("Writing Data Check Summary")
  out_err(wb, config, results, styles)

  # ---- Grouping/subsetting sheet (SAS lines 1517) ----
  if (!is.null(pp_result)) {
    tryCatch({
      group_subset_xml_out(
        wb        = wb,
        pp_result = pp_result,
        ndabla    = config$ndabla,
        studyid   = config$studyid,
        styles    = styles
      )
      cli::cli_inform("Added Grouping and Subsetting worksheet")
    }, error = function(e) {
      cli::cli_warn("Grouping/subsetting worksheet skipped: {e$message}")
    })
  }

  # ---- Save workbook (SAS lines 1523-1527) ----
  output_path <- config$aeout1
  if (is.null(output_path) || !nzchar(output_path)) {
    output_path <- "ae_severity_output.xlsx"
    cli::cli_warn("No output path specified in config$aeout1; defaulting to {output_path}")
  }

  # Ensure directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && nzchar(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  cli::cli_alert_info("Workbook saved to: {output_path}")

  output_path
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SpreadsheetML XML format -> openxlsx native .xlsx format
####    - All SAS style IDs (D0_R1_BL, D1_R2_BR, etc.) mapped to
####      openxlsx createStyle objects
####    - SAS %xml_tag_def/%xml_init/%annotate/%markup XML helper
####      macros -> openxlsx writeData + addStyle
####    - Excel conditional formatting (MOD/SUBTOTAL) -> manual
####      alternating row fill via addStyle with AltRowFill
####    - SAS worksheet settings (FreezePanes, AutoFilter,
####      PrintSetup) -> openxlsx equivalents
####    - Output is .xlsx (not .xls SpreadsheetML XML) --
####      functionally equivalent
####    - SAS strlen=1000 XML string buffer -> not applicable in R
####    - MergeAcross/MergeDown -> openxlsx::mergeCells()
####    - SAS DATA _NULL_ file writing -> openxlsx::saveWorkbook()
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- this file handles output formatting only,
####      not computation
####    - Percentage display: verify decimal formatting matches SAS
####      "0.0" format via openxlsx numFmt
####    - janitor::round_half_up() available for any rounding needed
#### NO DIRECT R EQUIVALENT:
####    - SAS SpreadsheetML XML generation -> openxlsx workbook API
####      (fundamentally different approach)
####    - SAS %ws macro (worksheet XML scaffolding) -> addWorksheet()
####    - SAS %annotate/%markup macros -> direct writeData() calls
####    - SAS XML conditional formatting with MOD/SUBTOTAL formula ->
####      manual row styling with AltRowFill
####    - SAS WorksheetOptions (PageSetup, Print, FrozenPanes) ->
####      pageSetup(), freezePane()
####    - SAS PCFILES/JET engine -> N/A (using openxlsx directly)
####    - SAS NamedRange for Print_Titles -> limited openxlsx
####      support for print areas
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Full Excel workbook creation with styles, merging,
####      freeze panes -- replaces SpreadsheetML XML
####    - Alternative considered: writexl -- rejected because it lacks
####      styling/merging support
####    - Alternative considered: xlsx (rJava-based) -- rejected due
####      to Java dependency
####    - dplyr: AAP mandates tidyverse over base R for data
####      manipulation
####    - stringr: AAP mandates tidyverse over base paste/sprintf
####    - cli: Structured user-facing messages for audit trail
####    - janitor: SAS-compatible round_half_up() for Gate 2
####      compliance
#### OPEN QUESTIONS:
####    - Verify alternating row conditional formatting renders
####      correctly in Excel
####    - Confirm freeze pane positioning matches SAS SplitHorizontal
####      setting
####    - Verify print setup (landscape, fit-to-page) works via
####      openxlsx pageSetup
####    - Determine if downstream consumers require SpreadsheetML XML
####      format specifically
####    - Column width calculations may differ -- verify visual
####      layout matches SAS output
#### ============================================================
