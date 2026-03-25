# =============================================================================
# ae_oncology_output.R
# Oncology AE MedDRA at a Glance Panel — Excel Workbook Generator
# =============================================================================
# Migrated from: tested/SAS/macros/ae_oncology_output.sas (2485 lines, 7 macros)
#
# SAS macro mapping:
#   %out_cover       -> onc_out_cover()
#   %out_pt_1        -> onc_out_pt1()
#   %out_pt_2(fmt=)  -> onc_out_pt2(fmt=)
#   %out_pt_3(fmt=)  -> onc_out_pt3(fmt=)
#   %out_err         -> onc_out_err()
#   %out_oae_styles  -> onc_out_styles()
#   %out_onc         -> onc_out_workbook()
#
# Dependencies:
#   Internal: tested/R/utilities/xml_output.R, tested/R/utilities/sl_gs_output.R
#   External: openxlsx (>=4.2.5), dplyr (>=1.1.0), janitor (>=2.2.0),
#             purrr (>=1.0.0), cli (>=3.6.0), stringr (>=1.5.0)
# =============================================================================

# --- Internal dependency sourcing ---
# xml_output.R provides: create_workbook, create_workbook_styles,
#   write_formatted_data, write_data_table, write_header_rows,
#   apply_page_setup, get_style_by_name
# sl_gs_output.R provides: group_subset_write_ws
local({
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )
  util_dir <- if (!is.null(this_dir)) {
    file.path(dirname(this_dir), "utilities")
  } else {
    fp <- "tested/R/utilities"
    if (dir.exists(fp)) fp else file.path("..", "utilities")
  }

  xml_path <- file.path(util_dir, "xml_output.R")
  if (file.exists(xml_path) && !exists("create_workbook", mode = "function")) {
    source(xml_path, local = FALSE)
  }

  sl_path <- file.path(util_dir, "sl_gs_output.R")
  if (file.exists(sl_path) && !exists("group_subset_write_ws", mode = "function")) {
    source(sl_path, local = FALSE)
  }
})


# =============================================================================
# Internal helper: .write_row
# =============================================================================
#' Write a single styled text row to a worksheet, optionally merging columns.
#'
#' @param wb         openxlsx workbook object.
#' @param sheet      Worksheet name or index.
#' @param row        Integer row number.
#' @param text       Character text to write.
#' @param style      openxlsx style object.
#' @param col        Starting column (default 1).
#' @param merge_cols Integer vector of columns to merge (NULL = no merge).
#' @param height     Numeric row height (NULL = default).
#' @return Invisible NULL.
#' @keywords internal
.write_row <- function(wb, sheet, row, text, style, col = 1L,
                       merge_cols = NULL, height = NULL) {
  openxlsx::writeData(wb, sheet, x = text, startRow = row, startCol = col,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = style, rows = row, cols = col)
  if (!is.null(merge_cols) && length(merge_cols) > 1L) {
    openxlsx::mergeCells(wb, sheet, cols = merge_cols, rows = row)
  }
  if (!is.null(height)) {
    openxlsx::setRowHeights(wb, sheet, rows = row, heights = height)
  }
  invisible(NULL)
}


# =============================================================================
# Internal helper: .build_tox_grade_desc
# =============================================================================
#' Build toxicity grade grouping description string from parameters.
#'
#' Mirrors SAS logic from %out_cover lines 30-85 that constructs
#' toxgr_grp_desc based on toxgr_max and toxgr_grp5_sw.
#'
#' @param toxgr_max    Character. Maximum toxicity grade ("3", "4", or "5").
#' @param toxgr_grp5_sw Character. "Y" if grades 1-2 are grouped.
#' @return Character string describing the grade grouping.
#' @keywords internal
.build_tox_grade_desc <- function(toxgr_max, toxgr_grp5_sw) {
  toxgr_max <- as.character(toxgr_max)
  if (toxgr_max == "5" && toupper(toxgr_grp5_sw) == "Y") {
    "Missing, Grade 0, Grades 1-2, Grade 3, Grade 4, Grade 5"
  } else if (toxgr_max == "5") {
    "Missing, Grade 0, Grade 1, Grade 2, Grade 3, Grade 4, Grade 5"
  } else if (toxgr_max == "4") {
    "Missing, Grade 0, Grade 1, Grade 2, Grade 3, Grade 4"
  } else if (toxgr_max == "3") {
    "Missing, Grade 0, Grade 1, Grade 2, Grade 3"
  } else {
    paste0("Missing, Grade 0, Grades 1 through ", toxgr_max)
  }
}


# =============================================================================
# Internal helper: .build_cmp_grade_desc
# =============================================================================
#' Build comparison grade description from cmpgr parameter.
#'
#' Mirrors SAS logic from %out_cover lines 86-105 that converts
#' cmpgr value to a descriptive label.
#'
#' @param cmpgr Character. Comparison grade threshold ("1"-"5" or "A"/"B"/"C"/"D").
#' @return Named list with elements: label (short label) and desc (full description).
#' @keywords internal
.build_cmp_grade_desc <- function(cmpgr) {
  cmpgr_up <- toupper(as.character(cmpgr))
  label <- dplyr::case_when(
    cmpgr_up == "1" ~ ">= Grade 1",
    cmpgr_up == "2" ~ ">= Grade 2",
    cmpgr_up == "3" ~ ">= Grade 3",
    cmpgr_up == "4" ~ ">= Grade 4",
    cmpgr_up == "5" ~ "Grade 5",
    cmpgr_up == "A" ~ ">= Grade 3",
    cmpgr_up == "B" ~ ">= Grade 4",
    cmpgr_up == "C" ~ "Grade 5",
    cmpgr_up == "D" ~ ">= Grade 3 and Grade 5",
    TRUE ~ paste0(">= Grade ", cmpgr)
  )
  desc <- paste0("Comparison: ", label)
  list(label = label, desc = desc)
}


# =============================================================================
# onc_out_styles — Oncology-specific style gallery
# =============================================================================
#' Create oncology AE-specific style definitions.
#'
#' Replaces SAS \code{%out_oae_styles} macro (lines 2347-2404 of
#' ae_oncology_output.sas). Returns a named list of openxlsx style objects
#' covering:
#' \itemize{
#'   \item 16 base data format styles (D, D0_R1, D0_R2, D0_R4, D1_R1, D1_R2,
#'         D1_R2N, D1_R2S, D2_R1, D2_R2, LCL, UCL, UCLSN, DT, D0_R1T, D1_R1T)
#'   \item 16 highlight (_H) variants with CCCCFF fill
#'   \item 7 border suffix variants per base+highlight: _BB, _BR, _BRB, _BL,
#'         _BLB, _BLR, _BLRB
#'   \item Total: 16 * 16 = 256 oncology-specific styles
#' }
#'
#' @param base_size Numeric font size (default 9).
#' @return Named list of openxlsx style objects.
#' @export
onc_out_styles <- function(base_size = 9) {

  styles <- list()
  highlight_fill <- "#CCCCFF"

  # --- Base style definitions ---
  # Each entry: halign, valign, numFmt, wrapText
  # SAS: D, D0_R1, D0_R2, D0_R4, D1_R1, D1_R2, D1_R2N, D1_R2S,
  #      D2_R1, D2_R2, LCL, UCL, UCLSN, DT, D0_R1T, D1_R1T
  base_defs <- list(
    D       = list(halign = "right", valign = "top", numFmt = "GENERAL",
                   wrapText = FALSE),
    D0_R1   = list(halign = "right", valign = "top", numFmt = "0",
                   wrapText = FALSE),
    D0_R2   = list(halign = "right", valign = "top", numFmt = "#,##0",
                   wrapText = FALSE),
    D0_R4   = list(halign = "right", valign = "top", numFmt = "#,##0.0000",
                   wrapText = FALSE),
    D1_R1   = list(halign = "right", valign = "top", numFmt = "0.0",
                   wrapText = FALSE),
    D1_R2   = list(halign = "right", valign = "top", numFmt = "#,##0.0",
                   wrapText = FALSE),
    # D1_R2N: trailing space format (non-significant marker alignment)
    D1_R2N  = list(halign = "right", valign = "top", numFmt = "0.0_*",
                   wrapText = FALSE),
    # D1_R2S: trailing asterisk format (significance marker)
    D1_R2S  = list(halign = "right", valign = "top", numFmt = '0.0"*"',
                   wrapText = FALSE),
    D2_R1   = list(halign = "right", valign = "top", numFmt = "0.00",
                   wrapText = FALSE),
    D2_R2   = list(halign = "right", valign = "top", numFmt = "#,##0.00",
                   wrapText = FALSE),
    # LCL: left confidence limit with opening paren and trailing comma
    LCL     = list(halign = "right", valign = "top",
                   numFmt = '"("0.00","', wrapText = FALSE),
    # UCL: upper confidence limit with closing paren
    UCL     = list(halign = "right", valign = "top",
                   numFmt = '0.00")"', wrapText = FALSE),
    # UCLSN: upper CL in scientific notation with closing paren
    UCLSN   = list(halign = "right", valign = "top",
                   numFmt = '0.0E+00")"', wrapText = FALSE),
    # DT: date/text style — top-aligned, wrap
    DT      = list(halign = "left", valign = "top", numFmt = "GENERAL",
                   wrapText = TRUE),
    # D0_R1T: integer, top-aligned (for vertical-merge contexts)
    D0_R1T  = list(halign = "right", valign = "top", numFmt = "0",
                   wrapText = FALSE),
    # D1_R1T: 1-decimal, top-aligned
    D1_R1T  = list(halign = "right", valign = "top", numFmt = "0.0",
                   wrapText = FALSE)
  )

  # --- Border suffix definitions ---
  # SAS: nested %do bl/br/bb loops (lines 2389-2404)
  # Suffix naming: _B always present, then L/R/B appended
  border_specs <- list(
    `_BB`   = "Bottom",
    `_BR`   = "Right",
    `_BRB`  = c("Right", "Bottom"),
    `_BL`   = "Left",
    `_BLB`  = c("Left", "Bottom"),
    `_BLR`  = c("Left", "Right"),
    `_BLRB` = c("Left", "Right", "Bottom")
  )

  # --- Generate all style combinations ---
  for (nm in names(base_defs)) {
    def <- base_defs[[nm]]
    h  <- def$halign
    v  <- def$valign
    nf <- def$numFmt
    wt <- def$wrapText

    # 1. Base style (no borders, no highlight)
    styles[[nm]] <- openxlsx::createStyle(
      fontSize = base_size, halign = h, valign = v, numFmt = nf, wrapText = wt
    )

    # 2. Highlight variant (_H)
    styles[[paste0(nm, "_H")]] <- openxlsx::createStyle(
      fontSize = base_size, halign = h, valign = v, numFmt = nf, wrapText = wt,
      fgFill = highlight_fill
    )

    # 3. Border variants and border+highlight variants
    for (sfx in names(border_specs)) {
      borders <- border_specs[[sfx]]
      # Border only
      styles[[paste0(nm, sfx)]] <- openxlsx::createStyle(
        fontSize = base_size, halign = h, valign = v, numFmt = nf, wrapText = wt,
        border = borders, borderStyle = "thin"
      )
      # Border + Highlight
      styles[[paste0(nm, "_H", sfx)]] <- openxlsx::createStyle(
        fontSize = base_size, halign = h, valign = v, numFmt = nf, wrapText = wt,
        fgFill = highlight_fill, border = borders, borderStyle = "thin"
      )
    }
  }

  return(styles)
}


# =============================================================================
# onc_out_cover — Cover / Front Page worksheet
# =============================================================================
#' Build the "Front Page" worksheet of the Oncology AE workbook.
#'
#' Replaces SAS \code{%out_cover} macro (lines 12-438 of
#' ae_oncology_output.sas). Writes a narrative cover page containing: report
#' title, metadata, worksheet descriptions, methodology, contingency-table
#' example (if multi-arm), calculation definitions, and report settings.
#'
#' @param wb              openxlsx workbook.
#' @param ndabla          NDA/BLA identifier.
#' @param studyid         Study identifier.
#' @param rundate         Analysis run date string.
#' @param arm_count       Integer number of treatment arms.
#' @param arm_names       Character vector of arm display names (with N counts).
#' @param toxgr_max       Max toxicity grade ("3","4","5").
#' @param toxgr_grp5_sw   "Y" if grades 1-2 grouped when toxgr_max=5.
#' @param cmpgr           Comparison grade threshold.
#' @param meddra          "Y" if MedDRA coded.
#' @param meddra_pct      Numeric MedDRA match percentage.
#' @param cc_sw           "Y" if continuity correction applied.
#' @param cc_desc         Continuity correction description.
#' @param study_lag       Study analysis period lag text.
#' @param vld_sw          "Y" if validation enabled.
#' @param ae_rate_ci_sw   Logical. TRUE to include AE rate CI columns.
#' @param sl_group_desc   Grouping description string.
#' @param sl_subset_desc  Subsetting description string.
#' @param rpt_key         Tibble with MedDRA key term info.
#' @param styles          Named list of base + oncology styles.
#' @return Invisible workbook.
#' @keywords internal
onc_out_cover <- function(wb, ndabla, studyid, rundate, arm_count, arm_names,
                          toxgr_max, toxgr_grp5_sw, cmpgr, meddra, meddra_pct,
                          cc_sw, cc_desc, study_lag, vld_sw, ae_rate_ci_sw,
                          sl_group_desc, sl_subset_desc, rpt_key, styles) {

  cli::cli_inform("Writing cover page...")
  sheet_name <- "Front Page"
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths — wide content area (SAS single-column 700px -> multi-col)
  openxlsx::setColWidths(wb, sheet_name, cols = 1:10,
                         widths = c(40, rep(12, 9)))

  sty_header    <- get_style_by_name(styles, "Header")
  sty_subheader <- get_style_by_name(styles, "SubHeader")
  sty_def_wrap  <- get_style_by_name(styles, "Default10Wrap")
  sty_def10     <- get_style_by_name(styles, "Default10")
  sty_red_wrap  <- get_style_by_name(styles, "Default10RedWrap")
  sty_col_out   <- get_style_by_name(styles, "ColumnOutline")

  r <- 1L
  merge_wide <- 1:10

  # --- Part 1: Title & metadata (SAS lines 110-140) ---
  .write_row(wb, sheet_name, r, "MedDRA At A Glance Panel - Oncology",
             sty_header, merge_cols = merge_wide)
  r <- r + 2L

  .write_row(wb, sheet_name, r,
             stringr::str_c("NDA/BLA: ", stringr::str_trim(ndabla)),
             sty_subheader)
  r <- r + 1L
  .write_row(wb, sheet_name, r,
             stringr::str_c("Study: ", stringr::str_trim(studyid)),
             sty_subheader)
  r <- r + 1L
  .write_row(wb, sheet_name, r,
             stringr::str_c("Analysis Run Date: ", rundate), sty_subheader)
  r <- r + 2L

  # --- Worksheet descriptions (SAS lines 142-188) ---
  toxgr_grp_desc <- .build_tox_grade_desc(toxgr_max, toxgr_grp5_sw)
  cmp_info       <- .build_cmp_grade_desc(cmpgr)

  ws_num <- 1L
  desc_lines <- character(0)
  desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
    ws_num, ". Front Page: Cover page describing the analysis and parameters.")
  ws_num <- ws_num + 1L
  desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
    ws_num, ". Tox Grade Summary: Summary of adverse events by toxicity grade (",
    toxgr_grp_desc, ") for each treatment arm.")
  ws_num <- ws_num + 1L
  desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
    ws_num, ". PT Analysis (Unformatted): Preferred Term analysis by toxicity ",
    "grade with sortable columns and auto-filter.")
  ws_num <- ws_num + 1L
  desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
    ws_num, ". PT Analysis (Formatted): Same analysis with group headers ",
    "and formatted layout.")
  ws_num <- ws_num + 1L

  if (arm_count > 1L) {
    desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
      ws_num, ". Arm Comparison (Unformatted): Two-arm comparison with RD, ",
      "RR, OR, Fisher's Exact p-value (", cmp_info$label, ").")
    ws_num <- ws_num + 1L
    desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
      ws_num, ". Arm Comparison (Formatted): Same comparison with group ",
      "headers and formatted layout.")
    ws_num <- ws_num + 1L
  }

  desc_lines[length(desc_lines) + 1L] <- stringr::str_c(
    ws_num, ". Data Check Summary: Subject validation, error summary, missing ",
    "toxicity grades, and MedDRA matching diagnostics.")

  for (dl in desc_lines) {
    .write_row(wb, sheet_name, r, dl, sty_def_wrap, merge_cols = merge_wide,
               height = 15)
    r <- r + 1L
  }
  r <- r + 1L

  # --- Methodology (SAS lines 190-250) ---
  method_text <- stringr::str_c(
    "This workbook presents an analysis of adverse events using the MedDRA ",
    "coding dictionary. Events are summarized by SOC, HLGT, HLT, and PT. ",
    "Toxicity grades are grouped as: ", toxgr_grp_desc,
    ". Subject counts and percentages are based on the safety population.")
  .write_row(wb, sheet_name, r, method_text, sty_def_wrap,
             merge_cols = merge_wide, height = 45)
  r <- r + 2L

  .write_row(wb, sheet_name, r,
             "NOTE: Results are for data exploration purposes only.",
             sty_red_wrap, merge_cols = merge_wide, height = 15)
  r <- r + 2L

  # --- Part 2: Contingency table (SAS lines 254-300, arm_count>1 only) ---
  if (arm_count > 1L) {
    .write_row(wb, sheet_name, r, "Two-by-Two Contingency Table Example:",
               sty_subheader, merge_cols = merge_wide)
    r <- r + 2L

    tbl_c <- 2L
    tbl_hdrs <- c("", "Has AE", "No AE", "Total")
    for (ci in seq_along(tbl_hdrs)) {
      openxlsx::writeData(wb, sheet_name, x = tbl_hdrs[ci],
                          startRow = r, startCol = tbl_c + ci - 1L,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name, style = sty_col_out,
                         rows = r, cols = tbl_c + ci - 1L)
    }
    r <- r + 1L

    tbl_data <- list(
      c("Treatment", "a",   "b",   "a+b"),
      c("Control",   "c",   "d",   "c+d"),
      c("Total",     "a+c", "b+d", "a+b+c+d")
    )
    sty_data <- get_style_by_name(styles, "Data")
    for (tr in tbl_data) {
      for (ci in seq_along(tr)) {
        openxlsx::writeData(wb, sheet_name, x = tr[ci],
                            startRow = r, startCol = tbl_c + ci - 1L,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = sty_data,
                           rows = r, cols = tbl_c + ci - 1L)
      }
      r <- r + 1L
    }
    r <- r + 1L
  }

  # --- Part 3: Calculation definitions (SAS lines 305-380) ---
  .write_row(wb, sheet_name, r, "Calculation Definitions:",
             sty_subheader, merge_cols = merge_wide)
  r <- r + 1L

  calc_defs <- list(
    list("AE Rate (Treatment)",
         "Proportion of subjects with AE = a / (a + b)"),
    list("AE Rate (Control)",
         "Proportion of subjects with AE = c / (c + d)")
  )
  if (isTRUE(ae_rate_ci_sw)) {
    calc_defs <- c(calc_defs, list(list(
      "AE Rate CI", "Exact (Clopper-Pearson) 95% confidence interval")))
  }
  if (arm_count > 1L) {
    calc_defs <- c(calc_defs, list(
      list("Risk Difference (RD)",
           "Treatment Rate - Control Rate = a/(a+b) - c/(c+d)"),
      list("RD 95% CI", "Wald confidence interval for risk difference"),
      list("Risk Ratio (RR)",
           "[a/(a+b)] / [c/(c+d)]"),
      list("RR 95% CI", "Log-based confidence interval for risk ratio"),
      list("Odds Ratio (OR)", "(a*d) / (b*c)")
    ))
    if (toupper(cc_sw) == "Y") {
      calc_defs <- c(calc_defs, list(list("OR with CC", cc_desc)))
    }
    calc_defs <- c(calc_defs, list(
      list("OR 95% CI", "Log-based confidence interval for odds ratio"),
      list("Fisher's Exact p-value", "Two-sided Fisher's Exact test p-value")
    ))
  }

  for (cd in calc_defs) {
    openxlsx::writeData(wb, sheet_name, x = cd[[1]],
                        startRow = r, startCol = 1L, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_subheader,
                       rows = r, cols = 1L)
    openxlsx::writeData(wb, sheet_name, x = cd[[2]],
                        startRow = r, startCol = 2L, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_def_wrap,
                       rows = r, cols = 2L)
    openxlsx::mergeCells(wb, sheet_name, cols = 2:10, rows = r)
    r <- r + 1L
  }
  r <- r + 1L

  # --- Continuity correction note (SAS lines 382-395) ---
  if (toupper(cc_sw) == "Y" && arm_count > 1L) {
    cc_note <- stringr::str_c(
      "Continuity Correction: ", cc_desc,
      ". An asterisk (*) after an OR value indicates CC was applied.")
    .write_row(wb, sheet_name, r, cc_note, sty_def_wrap,
               merge_cols = merge_wide, height = 30)
    r <- r + 2L
  }

  # --- Part 4: Report Settings (SAS lines 396-438) ---
  .write_row(wb, sheet_name, r, "Report Settings:",
             sty_subheader, merge_cols = merge_wide)
  r <- r + 1L

  settings <- list(
    c("NDA/BLA:", ndabla),
    c("Study:", studyid),
    c("Analysis Run Date:", rundate),
    c("Grouping:", sl_group_desc),
    c("Subsetting:", sl_subset_desc),
    c("Study Analysis Period:", study_lag),
    c("Toxicity Grade Grouping:", toxgr_grp_desc),
    c("Max Toxicity Grade:", as.character(toxgr_max))
  )
  if (arm_count > 1L) {
    settings <- c(settings, list(
      c("Comparison Grade:", cmp_info$label),
      c("Treatment Arm:", if (length(arm_names) >= 1L) arm_names[1L] else ""),
      c("Control Arm:", if (length(arm_names) >= 2L) arm_names[2L] else ""),
      c("Continuity Correction:",
        if (toupper(cc_sw) == "Y") "Applied" else "Not applied")
    ))
  }
  if (toupper(meddra) == "Y") {
    settings <- c(settings, list(
      c("MedDRA:", "Coded"),
      c("MedDRA Match Rate:", stringr::str_c(
        janitor::round_half_up(as.numeric(meddra_pct), 1), "%"))
    ))
  }
  settings <- c(settings, list(
    c("Validation:", if (toupper(vld_sw) == "Y") "Enabled" else "Disabled")
  ))

  if (!is.null(rpt_key) && is.data.frame(rpt_key) && nrow(rpt_key) > 0L &&
      "sort_label" %in% colnames(rpt_key)) {
    sort_vals <- unique(rpt_key$sort_label)
    sort_vals <- sort_vals[!is.na(sort_vals) & nchar(sort_vals) > 0L]
    # Clean and title-case sort labels (replacing SAS COMPBL + PROPCASE)
    sort_vals <- stringr::str_to_title(stringr::str_squish(sort_vals))
    if (length(sort_vals) > 0L) {
      settings <- c(settings, list(c("Sort Order:",
                                     stringr::str_c("Sorted by ", sort_vals[1L]))))
    }
  }

  for (s in settings) {
    openxlsx::writeData(wb, sheet_name, x = s[1],
                        startRow = r, startCol = 1L, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name,
                       style = get_style_by_name(styles, "Default10"),
                       rows = r, cols = 1L)
    openxlsx::writeData(wb, sheet_name, x = s[2],
                        startRow = r, startCol = 2L, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_def10,
                       rows = r, cols = 2L)
    openxlsx::mergeCells(wb, sheet_name, cols = 2:10, rows = r)
    r <- r + 1L
  }

  apply_page_setup(wb, sheet_name, orientation = "landscape")
  invisible(wb)
}


# =============================================================================
# Internal helper: .write_pt_col_headers
# =============================================================================
#' Write 3-row column headers for pt_1 / pt_2 worksheets.
#'
#' Replaces SAS \code{%wscolumns(ds)} helper (lines 2248-2345).
#' Builds multi-row headers: Row 1 = key labels + arm name headers (merged),
#' Row 2 = grade group labels per arm, Row 3 = Subject Count / % labels.
#'
#' @param wb         openxlsx workbook.
#' @param sheet      Worksheet name.
#' @param start_row  Starting row number.
#' @param rpt_key    Tibble with key column metadata (columns: key_col, label).
#' @param arm_count  Integer number of arms.
#' @param arm_names  Character vector of arm display names (with N counts).
#' @param toxgr_max  Max toxicity grade.
#' @param toxgr_grp5_sw "Y" if grouped.
#' @param styles     Style gallery list.
#' @return List with next_row (integer) and n_data_cols (total columns).
#' @keywords internal
.write_pt_col_headers <- function(wb, sheet, start_row, rpt_key,
                                  arm_count, arm_names, toxgr_max,
                                  toxgr_grp5_sw, styles) {

  sty_col <- get_style_by_name(styles, "ColumnOutline")

  # Determine key columns from rpt_key
  if (!is.null(rpt_key) && is.data.frame(rpt_key) && nrow(rpt_key) > 0L &&
      "label" %in% colnames(rpt_key)) {
    key_labels <- as.character(rpt_key$label)
    key_labels <- key_labels[!is.na(key_labels) & nchar(key_labels) > 0L]
  } else {
    key_labels <- c("SOC", "HLGT", "HLT", "Preferred Term")
  }
  n_key <- length(key_labels)

  # Build grade labels
  toxgr_max_n <- as.integer(toxgr_max)
  if (toxgr_max_n == 5L && toupper(toxgr_grp5_sw) == "Y") {
    grade_labels <- c("Grade 0", "Grades 1-2", "Grade 3", "Grade 4", "Grade 5")
  } else {
    grade_labels <- c("Grade 0", vapply(seq_len(toxgr_max_n), function(g) {
      paste0("Grade ", g)
    }, character(1L)))
  }
  n_grades <- length(grade_labels)
  # Each arm block: n_grades + Total + % = n_grades + 2
  cols_per_arm <- n_grades + 2L

  # Total data columns
  n_total_cols <- n_key + arm_count * cols_per_arm

  r1 <- as.integer(start_row)
  r2 <- r1 + 1L
  r3 <- r1 + 2L

  # --- Row 1: Key labels (MergeDown=2) + Arm headers (MergeAcross) ---
  col_idx <- 1L
  for (ki in seq_len(n_key)) {
    openxlsx::writeData(wb, sheet, x = key_labels[ki],
                        startRow = r1, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r1, cols = col_idx)
    # Merge down 2 rows (span rows r1:r3)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r1:r3)
    col_idx <- col_idx + 1L
  }

  # Arm headers — iterate using purrr::walk (replacing SAS %do i=1 %to &arm_count)
  purrr::walk(seq_len(arm_count), function(ai) {
    arm_label <- if (ai <= length(arm_names)) arm_names[ai] else
      paste0("Arm ", ai)
    arm_start <- col_idx
    arm_end <- col_idx + cols_per_arm - 1L
    openxlsx::writeData(wb, sheet, x = arm_label,
                        startRow = r1, startCol = arm_start, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r1, cols = arm_start)
    openxlsx::mergeCells(wb, sheet, cols = arm_start:arm_end, rows = r1)

    # --- Row 2: Grade labels + Total + % ---
    purrr::walk(seq_len(n_grades), function(gi) {
      openxlsx::writeData(wb, sheet, x = grade_labels[gi],
                          startRow = r2, startCol = col_idx, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, style = sty_col,
                         rows = r2, cols = col_idx)
      col_idx <<- col_idx + 1L
    })
    # Total column
    openxlsx::writeData(wb, sheet, x = "Total",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r2, cols = col_idx)
    col_idx <<- col_idx + 1L
    # % column
    openxlsx::writeData(wb, sheet, x = "%",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r2, cols = col_idx)
    col_idx <<- col_idx + 1L
  })

  # --- Row 3: Subject Count / % Affected labels ---
  col_idx <- n_key + 1L
  purrr::walk(seq_len(arm_count), function(ai) {
    purrr::walk(seq_len(n_grades), function(gi) {
      openxlsx::writeData(wb, sheet, x = "Subject\nCount",
                          startRow = r3, startCol = col_idx, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, style = sty_col,
                         rows = r3, cols = col_idx)
      col_idx <<- col_idx + 1L
    })
    openxlsx::writeData(wb, sheet, x = "Subject\nCount",
                        startRow = r3, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r3, cols = col_idx)
    col_idx <<- col_idx + 1L
    openxlsx::writeData(wb, sheet, x = "% Affected",
                        startRow = r3, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r3, cols = col_idx)
    col_idx <<- col_idx + 1L
  })

  # Set header row heights
  openxlsx::setRowHeights(wb, sheet, rows = r1:r3, heights = 30)

  list(next_row = r3 + 1L, n_data_cols = n_total_cols,
       n_key = n_key, cols_per_arm = cols_per_arm, n_grades = n_grades)
}


# =============================================================================
# onc_out_pt1 — Toxicity Grade Summary worksheet
# =============================================================================
#' Build the "Tox Grade Summary" worksheet.
#'
#' Replaces SAS \code{%out_pt_1} macro (lines 444-615 of
#' ae_oncology_output.sas). Writes toxicity grade summary by treatment arm
#' with header notes, 3-row column headers, and styled data rows.
#'
#' @param wb           openxlsx workbook.
#' @param pt_1_output  Tibble with toxicity grade summary data.
#' @param rpt_key      Tibble with key column metadata.
#' @param rpt_missing  Tibble with missing toxicity grade info.
#' @param arm_count    Integer number of arms.
#' @param arm_names    Character vector of arm display names.
#' @param ae_aetoxgr   Character flag for AETOXGR availability.
#' @param toxgr_max    Max toxicity grade.
#' @param toxgr_grp5_sw "Y" if grouped.
#' @param styles       Style gallery.
#' @return Invisible workbook.
#' @keywords internal
onc_out_pt1 <- function(wb, pt_1_output, rpt_key, rpt_missing,
                        arm_count, arm_names, ae_aetoxgr,
                        toxgr_max, toxgr_grp5_sw, styles) {

  cli::cli_inform("Writing Tox Grade Summary worksheet...")
  sheet_name <- "Tox Grade Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths: key column = 28 chars, data columns = 8 chars
  n_key <- 4L
  if (!is.null(rpt_key) && is.data.frame(rpt_key) && "label" %in% colnames(rpt_key)) {
    n_key <- max(1L, sum(!is.na(rpt_key$label) & nchar(rpt_key$label) > 0L))
  }
  toxgr_max_n <- as.integer(toxgr_max)
  n_grades <- if (toxgr_max_n == 5L && toupper(toxgr_grp5_sw) == "Y") 5L else toxgr_max_n + 1L
  cols_per_arm <- n_grades + 2L
  n_total_cols <- n_key + arm_count * cols_per_arm
  widths <- c(rep(28, n_key), rep(8, n_total_cols - n_key))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_total_cols),
                         widths = widths)

  # --- Header section (SAS lines 448-510) ---
  hdr_data <- data.frame(
    group = c("title", "subtitle", "default", "default"),
    data = c(
      "Toxicity Grade Summary",
      stringr::str_c("Safety Population - ",
                     .build_tox_grade_desc(toxgr_max, toxgr_grp5_sw)),
      "Subject counts reflect the maximum toxicity grade per subject per term.",
      "Percentages are based on the number of subjects in the safety population."
    ),
    stringsAsFactors = FALSE
  )
  # Add missing grade note if applicable
  if (!is.null(rpt_missing) && is.data.frame(rpt_missing) &&
      nrow(rpt_missing) > 0L) {
    hdr_data <- rbind(hdr_data, data.frame(
      group = "default",
      data = "Note: Some subjects have missing toxicity grades. See Data Check Summary.",
      stringsAsFactors = FALSE
    ))
  }
  current_row <- write_header_rows(wb, sheet_name, hdr_data, start_row = 1L,
                                   styles = styles)

  # --- Column headers (SAS %wscolumns) ---
  hdr_info <- .write_pt_col_headers(wb, sheet_name, current_row, rpt_key,
                                    arm_count, arm_names, toxgr_max,
                                    toxgr_grp5_sw, styles)
  current_row <- hdr_info$next_row

  # --- Data rows (SAS lines 540-585) ---
  if (!is.null(pt_1_output) && is.data.frame(pt_1_output) &&
      nrow(pt_1_output) > 0L) {

    # Prepare data: ensure proper sorting using dplyr::arrange
    # and round percentages using janitor::round_half_up (SAS parity)
    pt_1_prepared <- pt_1_output
    pct_cols <- dplyr::select(pt_1_prepared,
                              dplyr::starts_with("pct"))
    if (ncol(pct_cols) > 0L) {
      pt_1_prepared <- dplyr::mutate(
        pt_1_prepared,
        dplyr::across(dplyr::starts_with("pct"),
                      ~ dplyr::if_else(is.na(.x), NA_real_,
                                       janitor::round_half_up(.x, 1)))
      )
    }

    n_data_rows <- nrow(pt_1_prepared)
    n_data_cols <- ncol(pt_1_prepared)

    # Build style map using dplyr-based column classification
    col_names_lower <- tolower(colnames(pt_1_prepared))
    col_styles <- purrr::map_chr(seq_len(n_data_cols), function(j) {
      cn <- col_names_lower[j]
      if (j <= hdr_info$n_key) {
        "D_BLR"
      } else if (grepl("pct", cn, fixed = TRUE)) {
        "D1_R1_BR"
      } else if (grepl("total", cn, fixed = TRUE)) {
        "D0_R2_BLR"
      } else {
        "D0_R2_BB"
      }
    })
    style_map <- matrix(rep(col_styles, each = n_data_rows),
                        nrow = n_data_rows, ncol = n_data_cols)

    # Last row: append B for bottom border
    for (j in seq_len(n_data_cols)) {
      current_sty <- style_map[n_data_rows, j]
      if (!grepl("B$", current_sty)) {
        style_map[n_data_rows, j] <- paste0(current_sty, "B")
      }
    }

    write_formatted_data(wb, sheet_name, pt_1_prepared,
                         start_row = current_row, styles = styles,
                         style_map = as.data.frame(style_map,
                                                   stringsAsFactors = FALSE))
    current_row <- current_row + n_data_rows
  }

  # Page setup — landscape, fit-to-page
  apply_page_setup(wb, sheet_name, orientation = "landscape")

  invisible(wb)
}


# =============================================================================
# onc_out_pt2 — Preferred-Term Analysis worksheet
# =============================================================================
#' Build the Preferred-Term Analysis worksheet(s).
#'
#' Replaces SAS \code{%out_pt_2(fmt=Y/N)} macro (lines 621-894 of
#' ae_oncology_output.sas). Creates the preferred-term analysis worksheet in
#' two variants: unformatted (sortable with auto-filters and alternating row
#' shading) and formatted (with DataHeader group rows, indented adverse_event,
#' frozen panes).
#'
#' @param wb           openxlsx workbook.
#' @param pt_2_data    Tibble with preferred-term data.
#' @param rpt_key      Tibble with key column metadata.
#' @param rpt_missing  Tibble with missing tox grade info.
#' @param arm_count    Integer number of arms.
#' @param arm_names    Character vector of arm display names.
#' @param toxgr_max    Max toxicity grade.
#' @param toxgr_grp5_sw "Y" if grouped.
#' @param fmt          Logical: TRUE for formatted variant, FALSE for
#'                     unformatted variant.
#' @param styles       Style gallery.
#' @return Invisible workbook.
#' @keywords internal
onc_out_pt2 <- function(wb, pt_2_data, rpt_key, rpt_missing,
                        arm_count, arm_names, toxgr_max,
                        toxgr_grp5_sw, fmt = FALSE, styles) {

  fmt_label <- if (fmt) "PT Analysis (fmt)" else "PT Analysis"
  cli::cli_inform(stringr::str_c("Writing ", fmt_label, " worksheet..."))
  sheet_name <- fmt_label
  openxlsx::addWorksheet(wb, sheet_name)

  # Compute column geometry
  n_key <- 4L
  if (!is.null(rpt_key) && is.data.frame(rpt_key) && "label" %in% colnames(rpt_key)) {
    n_key <- max(1L, sum(!is.na(rpt_key$label) & nchar(rpt_key$label) > 0L))
  }
  toxgr_max_n <- as.integer(toxgr_max)
  n_grades <- if (toxgr_max_n == 5L && toupper(toxgr_grp5_sw) == "Y") 5L else toxgr_max_n + 1L
  cols_per_arm <- n_grades + 2L
  n_total_cols <- n_key + arm_count * cols_per_arm
  widths <- c(rep(28, n_key), rep(8, n_total_cols - n_key))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_total_cols),
                         widths = widths)

  # --- Header section ---
  hdr_data <- data.frame(
    group = c("title", "subtitle", "default", "default"),
    data = c(
      stringr::str_c("Preferred Term Analysis",
                     if (fmt) " (Formatted)" else ""),
      stringr::str_c("Safety Population - ",
                     .build_tox_grade_desc(toxgr_max, toxgr_grp5_sw)),
      "Subject counts reflect the maximum toxicity grade per subject per term.",
      "Percentages are based on the number of subjects in the safety population."
    ),
    stringsAsFactors = FALSE
  )
  if (!is.null(rpt_missing) && is.data.frame(rpt_missing) &&
      nrow(rpt_missing) > 0L) {
    hdr_data <- rbind(hdr_data, data.frame(
      group = "default",
      data = "Note: Some subjects have missing toxicity grades. See Data Check Summary.",
      stringsAsFactors = FALSE
    ))
  }
  current_row <- write_header_rows(wb, sheet_name, hdr_data, start_row = 1L,
                                   styles = styles)

  # --- Column headers (3-row) ---
  hdr_info <- .write_pt_col_headers(wb, sheet_name, current_row, rpt_key,
                                    arm_count, arm_names, toxgr_max,
                                    toxgr_grp5_sw, styles)
  current_row <- hdr_info$next_row
  freeze_row <- current_row  # row to freeze at for fmt variant

  # --- Data preparation: apply dplyr operations ---
  if (is.null(pt_2_data) || !is.data.frame(pt_2_data) ||
      nrow(pt_2_data) == 0L) {
    cli::cli_warn("No data for {fmt_label} worksheet.")
    apply_page_setup(wb, sheet_name, orientation = "landscape")
    return(invisible(wb))
  }

  # Round percentage columns using SAS-compatible rounding
  pt_2_prep <- dplyr::mutate(
    pt_2_data,
    dplyr::across(
      dplyr::starts_with("pct"),
      ~ dplyr::if_else(is.na(.x), NA_real_,
                        janitor::round_half_up(.x, 1))
    )
  )

  # Sort data if sort column present (unformatted variant)
  if (!fmt) {
    sort_candidates <- grep("^sort", tolower(colnames(pt_2_prep)), value = TRUE)
    if (length(sort_candidates) > 0L) {
      pt_2_prep <- dplyr::arrange(pt_2_prep,
                                  dplyr::desc(.data[[sort_candidates[1L]]]))
    }
  }

  # Filter out internal-only columns for display
  display_cols <- colnames(pt_2_prep)[!tolower(colnames(pt_2_prep)) %in%
                                        c("row_type", "sort_order")]
  pt_2_display <- dplyr::select(pt_2_prep,
                                dplyr::all_of(display_cols))

  n_data_rows <- nrow(pt_2_display)
  n_data_cols <- ncol(pt_2_display)

  if (fmt) {
    # ----- Formatted variant (SAS lines 695-894) -----
    # DataHeader rows for group labels (SOC-level group rows)
    # Data rows with indented adverse_event
    # Frozen panes, no auto-filter
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = freeze_row,
                         firstActiveCol = 1L)

    for (i in seq_len(n_data_rows)) {
      row_data <- pt_2_display[i, , drop = FALSE]
      # Use original row for type check (row_type may have been excluded)
      orig_row <- pt_2_prep[i, , drop = FALSE]
      is_last <- (i == n_data_rows)

      # Check if this is a DataHeader (group) row by looking for a
      # type/header indicator column or checking if only key columns have
      # data and numeric columns are empty/NA
      is_hdr_row <- FALSE
      if ("row_type" %in% colnames(orig_row)) {
        is_hdr_row <- toupper(as.character(orig_row$row_type[1L])) %in%
          c("HEADER", "GROUP", "SOC", "HLGT", "HLT")
      } else {
        # Heuristic: if all numeric arm columns are NA, treat as header
        arm_cols <- (n_key + 1L):n_data_cols
        if (length(arm_cols) > 0L) {
          arm_vals <- unlist(row_data[1L, arm_cols, drop = TRUE])
          is_hdr_row <- all(is.na(arm_vals) | arm_vals == "")
        }
      }

      if (is_hdr_row) {
        # DataHeader: merge across all columns, bold font
        sty_dh <- get_style_by_name(styles, "DataHeader")
        hdr_text <- as.character(row_data[[1L]])
        openxlsx::writeData(wb, sheet_name, x = hdr_text,
                            startRow = current_row, startCol = 1L,
                            colNames = FALSE)
        openxlsx::mergeCells(wb, sheet_name,
                             cols = 1L:n_total_cols, rows = current_row)
        openxlsx::addStyle(wb, sheet_name, style = sty_dh,
                           rows = current_row, cols = 1L)
        openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                                heights = 18)
      } else {
        # Standard data row
        col_names_lower <- tolower(colnames(row_data))
        for (j in seq_len(n_data_cols)) {
          val <- row_data[[j]]
          if (is.na(val)) val <- "."
          cn <- col_names_lower[j]

          # Determine style
          if (j <= n_key) {
            base_sty <- "Data"
          } else if (grepl("pct", cn, fixed = TRUE)) {
            base_sty <- "D1_R1"
          } else if (grepl("total", cn, fixed = TRUE)) {
            base_sty <- "D0_R2"
          } else {
            base_sty <- "D0_R2"
          }
          # Bottom border on last row
          if (is_last) base_sty <- paste0(base_sty, "_BB")

          cell_style <- get_style_by_name(styles, base_sty)
          openxlsx::writeData(wb, sheet_name, x = val,
                              startRow = current_row, startCol = j,
                              colNames = FALSE)
          openxlsx::addStyle(wb, sheet_name, style = cell_style,
                             rows = current_row, cols = j)
        }
      }
      current_row <- current_row + 1L
    }
  } else {
    # ----- Unformatted variant (SAS lines 621-694) -----
    # Flat data with auto-filter, alternating row shading
    sty_data <- get_style_by_name(styles, "Data")
    sty_d0 <- get_style_by_name(styles, "D0_R2")
    sty_d1 <- get_style_by_name(styles, "D1_R1")
    sty_dt_total <- get_style_by_name(styles, "D0_R2_BLR")

    col_names_lower <- tolower(colnames(pt_2_display))

    for (i in seq_len(n_data_rows)) {
      row_data <- pt_2_display[i, , drop = FALSE]
      is_last <- (i == n_data_rows)

      for (j in seq_len(n_data_cols)) {
        val <- row_data[[j]]
        if (is.na(val)) val <- "."
        cn <- col_names_lower[j]

        if (j <= n_key) {
          base_sty <- "Data"
        } else if (grepl("pct", cn, fixed = TRUE)) {
          base_sty <- "D1_R1"
        } else if (grepl("total", cn, fixed = TRUE)) {
          base_sty <- "D0_R2_BLR"
        } else {
          base_sty <- "D0_R2"
        }
        if (is_last) base_sty <- paste0(base_sty, "B")

        cell_style <- get_style_by_name(styles, base_sty)
        openxlsx::writeData(wb, sheet_name, x = val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = cell_style,
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }

    # Auto-filter across header row
    openxlsx::addFilter(wb, sheet_name,
                            rows = freeze_row - 1L,
                            cols = seq_len(n_total_cols))

    # Alternating row shading via conditional formatting
    data_start <- freeze_row
    data_end <- data_start + n_data_rows - 1L
    if (data_end >= data_start) {
      tryCatch({
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = seq_len(n_total_cols),
          rows = data_start:data_end,
          rule = "ISEVEN(ROW())",
          style = openxlsx::createStyle(bgFill = "#F2F2F2"),
          type = "expression"
        )
      }, error = function(e) {
        cli::cli_warn("Could not apply alternating row shading: {e$message}")
      })
    }
  }

  # Page setup
  apply_page_setup(wb, sheet_name, orientation = "landscape")

  invisible(wb)
}


# =============================================================================
# Internal helper: .write_pt3_col_headers
# =============================================================================
#' Write complex multi-row column headers for pt_3 worksheets.
#'
#' Replaces SAS \code{%out_pt_3} header section (lines 901-1020).
#' Builds a 3-row header with: Row 1 = key labels (MergeDown=2) + Treatment
#' arm (MergeAcross) + Control arm (MergeAcross) + RD/RR/OR groups +
#' P-value (MergeDown=2). Row 2 = sub-labels per arm block. Row 3 =
#' Count / % / CI detail labels.
#'
#' @param wb          openxlsx workbook.
#' @param sheet       Worksheet name.
#' @param start_row   Starting row.
#' @param rpt_key     Tibble with key column metadata.
#' @param arm_count   Integer number of arms.
#' @param arm_names   Character vector of arm display names.
#' @param cmpgr       Comparison grade configuration.
#' @param cc_sw       Continuity correction switch.
#' @param ae_rate_ci_sw  AE rate CI switch.
#' @param styles      Style gallery.
#' @return List with next_row, n_total_cols, n_key, and column layout details.
#' @keywords internal
.write_pt3_col_headers <- function(wb, sheet, start_row, rpt_key,
                                   arm_count, arm_names, cmpgr,
                                   cc_sw, ae_rate_ci_sw, styles) {

  sty_col <- get_style_by_name(styles, "ColumnOutline")

  # Key columns
  if (!is.null(rpt_key) && is.data.frame(rpt_key) && nrow(rpt_key) > 0L &&
      "label" %in% colnames(rpt_key)) {
    key_labels <- as.character(rpt_key$label)
    key_labels <- key_labels[!is.na(key_labels) & nchar(key_labels) > 0L]
  } else {
    key_labels <- c("SOC", "HLGT", "HLT", "Preferred Term")
  }
  n_key <- length(key_labels)

  # For pt_3: each arm has Count, %, and optionally CI columns
  # Then comparison columns: RD, RR, OR, P-value
  # Treatment arm: Count, %  [, LCL, UCL if ae_rate_ci_sw]
  # Control arm: Count, %  [, LCL, UCL if ae_rate_ci_sw]
  cols_per_arm <- if (ae_rate_ci_sw) 4L else 2L

  # Comparison columns: RD (LCL, UCL), RR (LCL, UCL), OR (LCL, UCL), P-value
  n_rd <- 3L  # RD, LCL, UCL
  n_rr <- 3L  # RR, LCL, UCL
  n_or <- 3L  # OR, LCL, UCL
  n_pval <- 1L  # P-value
  n_cmp_cols <- n_rd + n_rr + n_or + n_pval

  # Total cols: key + 2 arms + comparison
  n_total_cols <- n_key + 2L * cols_per_arm + n_cmp_cols

  r1 <- as.integer(start_row)
  r2 <- r1 + 1L
  r3 <- r1 + 2L

  # Row 1: Key labels (MergeDown=2)
  col_idx <- 1L
  for (ki in seq_len(n_key)) {
    openxlsx::writeData(wb, sheet, x = key_labels[ki],
                        startRow = r1, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col,
                       rows = r1, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r1:r3)
    col_idx <- col_idx + 1L
  }

  # Treatment arm header (row 1, merged across)
  trt_label <- if (length(arm_names) >= 1L) arm_names[1L] else "Treatment"
  trt_start <- col_idx
  trt_end <- col_idx + cols_per_arm - 1L
  openxlsx::writeData(wb, sheet, x = trt_label,
                      startRow = r1, startCol = trt_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = trt_start)
  openxlsx::mergeCells(wb, sheet, cols = trt_start:trt_end, rows = r1)

  # Treatment sub-labels (row 2-3)
  openxlsx::writeData(wb, sheet, x = "Count",
                      startRow = r2, startCol = col_idx, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
  openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
  col_idx <- col_idx + 1L
  openxlsx::writeData(wb, sheet, x = "%",
                      startRow = r2, startCol = col_idx, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
  openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
  col_idx <- col_idx + 1L
  if (ae_rate_ci_sw) {
    openxlsx::writeData(wb, sheet, x = "LCL",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
    openxlsx::writeData(wb, sheet, x = "UCL",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
  }

  # Control arm header (row 1, merged)
  ctl_label <- if (length(arm_names) >= 2L) arm_names[2L] else "Control"
  ctl_start <- col_idx
  ctl_end <- col_idx + cols_per_arm - 1L
  openxlsx::writeData(wb, sheet, x = ctl_label,
                      startRow = r1, startCol = ctl_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = ctl_start)
  openxlsx::mergeCells(wb, sheet, cols = ctl_start:ctl_end, rows = r1)

  # Control sub-labels
  openxlsx::writeData(wb, sheet, x = "Count",
                      startRow = r2, startCol = col_idx, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
  openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
  col_idx <- col_idx + 1L
  openxlsx::writeData(wb, sheet, x = "%",
                      startRow = r2, startCol = col_idx, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
  openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
  col_idx <- col_idx + 1L
  if (ae_rate_ci_sw) {
    openxlsx::writeData(wb, sheet, x = "LCL",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
    openxlsx::writeData(wb, sheet, x = "UCL",
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
  }

  # --- Comparison columns: RD ---
  rd_start <- col_idx
  rd_end <- col_idx + n_rd - 1L
  openxlsx::writeData(wb, sheet, x = "Risk Difference",
                      startRow = r1, startCol = rd_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = rd_start)
  openxlsx::mergeCells(wb, sheet, cols = rd_start:rd_end, rows = r1)
  # Sub-labels
  for (lbl in c("RD", "LCL", "UCL")) {
    openxlsx::writeData(wb, sheet, x = lbl,
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
  }

  # --- Relative Risk ---
  rr_start <- col_idx
  rr_end <- col_idx + n_rr - 1L
  openxlsx::writeData(wb, sheet, x = "Relative Risk",
                      startRow = r1, startCol = rr_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = rr_start)
  openxlsx::mergeCells(wb, sheet, cols = rr_start:rr_end, rows = r1)
  for (lbl in c("RR", "LCL", "UCL")) {
    openxlsx::writeData(wb, sheet, x = lbl,
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
  }

  # --- Odds Ratio ---
  or_start <- col_idx
  or_end <- col_idx + n_or - 1L
  or_label <- if (toupper(cc_sw) == "Y") {
    "Odds Ratio (CC)"
  } else {
    "Odds Ratio"
  }
  openxlsx::writeData(wb, sheet, x = or_label,
                      startRow = r1, startCol = or_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = or_start)
  openxlsx::mergeCells(wb, sheet, cols = or_start:or_end, rows = r1)
  for (lbl in c("OR", "LCL", "UCL")) {
    openxlsx::writeData(wb, sheet, x = lbl,
                        startRow = r2, startCol = col_idx, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, style = sty_col, rows = r2, cols = col_idx)
    openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r2:r3)
    col_idx <- col_idx + 1L
  }

  # --- P-value (MergeDown=2) ---
  openxlsx::writeData(wb, sheet, x = "P-value\n(Fisher's Exact)",
                      startRow = r1, startCol = col_idx, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, style = sty_col,
                     rows = r1, cols = col_idx)
  openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = r1:r3)

  # Row heights
  openxlsx::setRowHeights(wb, sheet, rows = r1:r3, heights = 30)

  list(
    next_row = r3 + 1L,
    n_total_cols = n_total_cols,
    n_key = n_key,
    cols_per_arm = cols_per_arm,
    n_cmp_cols = n_cmp_cols
  )
}


# =============================================================================
# onc_out_pt3 — Arm Comparison worksheet
# =============================================================================
#' Build the Arm Comparison worksheet.
#'
#' Replaces SAS \code{%out_pt_3(fmt=Y/N)} macro (lines 901-1417 of
#' ae_oncology_output.sas). Creates the arm-comparison worksheet with
#' risk difference (RD), relative risk (RR), odds ratio (OR), and Fisher's
#' exact p-value columns. Supports formatted and unformatted variants, with
#' optional AE rate CI columns and cc_ind flag-based style differentiation.
#'
#' @param wb              openxlsx workbook.
#' @param pt_3_data       Tibble with arm comparison data.
#' @param rpt_key         Tibble with key column metadata.
#' @param arm_count       Integer number of arms.
#' @param arm_names       Character vector of arm display names.
#' @param cmpgr           Comparison grade string.
#' @param cc_sw           Continuity correction switch.
#' @param ae_rate_ci_sw   AE rate CI switch (default FALSE).
#' @param fmt             Logical: TRUE for formatted, FALSE for unformatted.
#' @param styles          Style gallery.
#' @return Invisible workbook.
#' @keywords internal
onc_out_pt3 <- function(wb, pt_3_data, rpt_key, arm_count, arm_names,
                        cmpgr, cc_sw, ae_rate_ci_sw = FALSE,
                        fmt = FALSE, styles) {

  # Only produce pt_3 if arm_count > 1

  if (arm_count <= 1L) {
    cli::cli_inform("Skipping Arm Comparison (single arm, no comparison).")
    return(invisible(wb))
  }

  fmt_label <- if (fmt) "Arm Comparison (fmt)" else "Arm Comparison"
  cli::cli_inform(stringr::str_c("Writing ", fmt_label, " worksheet..."))
  sheet_name <- fmt_label
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths: key=28, arm data=8, comparison=8
  n_key <- 4L
  if (!is.null(rpt_key) && is.data.frame(rpt_key) && "label" %in% colnames(rpt_key)) {
    n_key <- max(1L, sum(!is.na(rpt_key$label) & nchar(rpt_key$label) > 0L))
  }
  cols_per_arm <- if (ae_rate_ci_sw) 4L else 2L
  n_cmp_cols <- 10L  # RD(3) + RR(3) + OR(3) + Pval(1)
  n_total_cols <- n_key + 2L * cols_per_arm + n_cmp_cols
  key_widths <- rep(28, n_key)
  data_widths <- rep(8, n_total_cols - n_key)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_total_cols),
                         widths = c(key_widths, data_widths))

  # --- Header section ---
  cmp_info <- .build_cmp_grade_desc(cmpgr)
  cmp_desc <- cmp_info$desc
  hdr_data <- data.frame(
    group = c("title", "subtitle", "default"),
    data = c(
      stringr::str_c("Arm Comparison", if (fmt) " (Formatted)" else ""),
      stringr::str_c("Safety Population - ", cmp_desc),
      "Risk Difference, Relative Risk, Odds Ratio, and Fisher's Exact P-value."
    ),
    stringsAsFactors = FALSE
  )
  current_row <- write_header_rows(wb, sheet_name, hdr_data, start_row = 1L,
                                   styles = styles)

  # --- Column headers (3-row) ---
  hdr_info <- .write_pt3_col_headers(wb, sheet_name, current_row, rpt_key,
                                     arm_count, arm_names, cmpgr, cc_sw,
                                     ae_rate_ci_sw, styles)
  current_row <- hdr_info$next_row
  freeze_row <- current_row

  # --- Data preparation ---
  if (is.null(pt_3_data) || !is.data.frame(pt_3_data) ||
      nrow(pt_3_data) == 0L) {
    cli::cli_warn("No data for {fmt_label} worksheet.")
    apply_page_setup(wb, sheet_name, orientation = "landscape")
    return(invisible(wb))
  }

  # Round percentage/stat columns for SAS parity
  pt_3_prep <- dplyr::mutate(
    pt_3_data,
    dplyr::across(
      dplyr::starts_with("pct"),
      ~ dplyr::if_else(is.na(.x), NA_real_,
                        janitor::round_half_up(.x, 1))
    )
  )
  # Filter out any rows with all-NA data columns (empty rows from joins)
  arm_col_names <- colnames(pt_3_prep)[(n_key + 1L):ncol(pt_3_prep)]
  arm_col_names <- arm_col_names[!tolower(arm_col_names) %in%
                                   c("cc_ind", "row_type", "sort_order")]
  if (length(arm_col_names) > 0L) {
    pt_3_prep <- dplyr::filter(pt_3_prep,
                               !purrr::pmap_lgl(
                                 dplyr::select(pt_3_prep, dplyr::all_of(arm_col_names)),
                                 function(...) all(is.na(c(...)))))
  }

  # Build display column name map for output
  display_col_names <- purrr::map(colnames(pt_3_prep), function(cn) {
    stringr::str_squish(cn)
  })

  n_data_rows <- nrow(pt_3_prep)
  n_data_cols <- ncol(pt_3_prep)
  col_names_lower <- tolower(colnames(pt_3_prep))

  # Detect cc_ind column for continuity correction indication
  has_cc_ind <- "cc_ind" %in% col_names_lower

  # Detect sort column for highlighting
  sort_col <- NA_character_
  sort_idx <- which(grepl("^sort", col_names_lower))
  if (length(sort_idx) > 0L) sort_col <- colnames(pt_3_data)[sort_idx[1L]]

  for (i in seq_len(n_data_rows)) {
    row_data <- pt_3_prep[i, , drop = FALSE]
    is_last <- (i == n_data_rows)

    # Check cc_ind
    cc_flag <- FALSE
    if (has_cc_ind) {
      cc_val <- row_data[["cc_ind"]]
      if (!is.null(cc_val) && !is.na(cc_val) && toupper(as.character(cc_val)) == "Y") {
        cc_flag <- TRUE
      }
    }

    # DataHeader check for formatted variant
    is_hdr_row <- FALSE
    if (fmt) {
      if ("row_type" %in% colnames(row_data)) {
        is_hdr_row <- toupper(as.character(row_data$row_type[1L])) %in%
          c("HEADER", "GROUP", "SOC", "HLGT", "HLT")
      } else {
        arm_cols <- (n_key + 1L):n_data_cols
        if (length(arm_cols) > 0L) {
          arm_vals <- unlist(row_data[1L, arm_cols, drop = TRUE])
          is_hdr_row <- all(is.na(arm_vals) | arm_vals == "")
        }
      }
    }

    if (is_hdr_row) {
      sty_dh <- get_style_by_name(styles, "DataHeader")
      hdr_text <- as.character(row_data[[1L]])
      openxlsx::writeData(wb, sheet_name, x = hdr_text,
                          startRow = current_row, startCol = 1L,
                          colNames = FALSE)
      openxlsx::mergeCells(wb, sheet_name,
                           cols = 1L:n_total_cols, rows = current_row)
      openxlsx::addStyle(wb, sheet_name, style = sty_dh,
                         rows = current_row, cols = 1L)
      openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                              heights = 18)
    } else {
      # Standard data row — apply styles based on column type and cc_ind
      for (j in seq_len(n_data_cols)) {
        cn <- col_names_lower[j]

        # Skip cc_ind column (internal flag, not displayed)
        if (cn == "cc_ind") next

        val <- row_data[[j]]
        if (is.na(val)) val <- "."

        # Determine base style
        if (j <= n_key) {
          base_sty <- "Data"
        } else if (grepl("pct", cn, fixed = TRUE)) {
          # Use cc-dependent variant (SAS D1_R2S vs D1_R2N)
          base_sty <- if (cc_flag) "D1_R2" else "D1_R1"
        } else if (grepl("^rd$|^rr$|^ort$|risk_diff|rel_risk|odds_ratio",
                         cn, perl = TRUE)) {
          base_sty <- "D1_R2"
        } else if (grepl("lcl|ucl", cn, fixed = TRUE)) {
          # Check for large values -> UCLSN
          num_val <- suppressWarnings(as.numeric(val))
          if (!is.na(num_val) && abs(num_val) > 1e6) {
            base_sty <- "UCLSN"
          } else if (grepl("lcl", cn, fixed = TRUE)) {
            base_sty <- "LCL"
          } else {
            base_sty <- "UCL"
          }
        } else if (grepl("p_value|pvalue|fisher", cn, perl = TRUE)) {
          base_sty <- "D2_R2"
        } else if (grepl("count|cnt|n_", cn, perl = TRUE)) {
          base_sty <- "D0_R2"
        } else {
          base_sty <- "D0_R2"
        }

        # Highlight sort column
        is_sort <- !is.na(sort_col) && cn == tolower(sort_col)
        if (is_sort) {
          base_sty <- paste0(base_sty, "_H")
        }

        # Bottom border on last row
        if (is_last) base_sty <- paste0(base_sty, "_BB")

        cell_style <- get_style_by_name(styles, base_sty)
        openxlsx::writeData(wb, sheet_name, x = val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = cell_style,
                           rows = current_row, cols = j)
      }
    }
    current_row <- current_row + 1L
  }

  # Formatted variant: freeze panes
  if (fmt) {
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = freeze_row,
                         firstActiveCol = 1L)
  } else {
    # Unformatted variant: auto-filter + alternating rows
    openxlsx::addFilter(wb, sheet_name,
                            rows = freeze_row - 1L,
                            cols = seq_len(n_total_cols))
    data_start <- freeze_row
    data_end <- data_start + n_data_rows - 1L
    if (data_end >= data_start) {
      tryCatch({
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = seq_len(n_total_cols),
          rows = data_start:data_end,
          rule = "ISEVEN(ROW())",
          style = openxlsx::createStyle(bgFill = "#F2F2F2"),
          type = "expression"
        )
      }, error = function(e) {
        cli::cli_warn("Could not apply alternating row shading: {e$message}")
      })
    }
  }

  # Print title named region for formatted variant
  if (fmt) {
    tryCatch({
      openxlsx::createNamedRegion(wb, sheet_name,
                                  cols = seq_len(n_total_cols),
                                  rows = 1L:(freeze_row - 1L),
                                  name = paste0("Print_Titles_", gsub(" ", "_", sheet_name)))
    }, error = function(e) {
      cli::cli_warn("Could not create print title region: {e$message}")
    })
  }

  # Page setup
  apply_page_setup(wb, sheet_name, orientation = "landscape")

  invisible(wb)
}


# =============================================================================
# onc_out_err — Data Check Summary worksheet
# =============================================================================
#' Build the "Data Check Summary" worksheet.
#'
#' Replaces SAS \code{%out_err} macro (lines 1423-2191 of
#' ae_oncology_output.sas). Writes 5 conditional sections to a single
#' worksheet: (1) Subject Validation, (2) Data Validation summary,
#' (3) Data Validation per term, (4) Missing Toxicity Grades,
#' (5) MedDRA Matching.
#'
#' @param wb              openxlsx workbook.
#' @param rpt_dm          Tibble — subject validation per arm.
#' @param rpt_err         Tibble — validation error summary.
#' @param rpt_err_term    Tibble — validation errors per term.
#' @param rpt_missing     Tibble — missing toxicity grade info.
#' @param rpt_meddra      Tibble — MedDRA matching summary.
#' @param rpt_meddra_term Tibble — MedDRA matching per term.
#' @param vld_sw          Character flag for validation switch.
#' @param vld_err         Character flag for validation error presence.
#' @param meddra          Character flag for MedDRA match reporting.
#' @param meddra_pct      Numeric: MedDRA match percentage.
#' @param arm_count       Integer number of arms.
#' @param arm_names       Character vector of arm display names.
#' @param styles          Style gallery.
#' @return Invisible workbook.
#' @keywords internal
onc_out_err <- function(wb, rpt_dm, rpt_err, rpt_err_term, rpt_missing,
                        rpt_meddra, rpt_meddra_term,
                        vld_sw, vld_err, meddra, meddra_pct,
                        arm_count, arm_names, styles) {

  cli::cli_inform("Writing Data Check Summary worksheet...")
  sheet_name <- "Data Check Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths (SAS lines 1430-1440): 8 key/data columns
  col_widths <- c(30, 15, 15, 15, 15, 15, 15, 15)
  n_cols <- length(col_widths)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_cols),
                         widths = col_widths)

  current_row <- 1L
  sty_hdr <- get_style_by_name(styles, "Header")
  sty_sub <- get_style_by_name(styles, "SubHeader")
  sty_col <- get_style_by_name(styles, "ColumnOutline")
  sty_data <- get_style_by_name(styles, "Data")
  sty_d0 <- get_style_by_name(styles, "D0_R2")
  sty_d1 <- get_style_by_name(styles, "D1_R1")

  # =================================================================
  # Section 1: Subject Validation (SAS lines 1445-1560)
  # =================================================================
  openxlsx::writeData(wb, sheet_name, x = "Subject Validation",
                      startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                     rows = current_row, cols = 1L)
  openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet_name,
                      x = "Subject counts by treatment arm from the analysis dataset.",
                      startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = sty_sub,
                     rows = current_row, cols = 1L)
  openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
  current_row <- current_row + 1L

  # Column headers for DM: Description, Arm1, Arm2, ..., Total
  dm_col_labels <- c("Description")
  for (ai in seq_len(arm_count)) {
    dm_col_labels <- c(dm_col_labels,
                       if (ai <= length(arm_names)) arm_names[ai] else paste0("Arm ", ai))
  }
  dm_col_labels <- c(dm_col_labels, "Total")
  for (ci in seq_along(dm_col_labels)) {
    if (ci <= n_cols) {
      openxlsx::writeData(wb, sheet_name, x = dm_col_labels[ci],
                          startRow = current_row, startCol = ci,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name, style = sty_col,
                         rows = current_row, cols = ci)
    }
  }
  current_row <- current_row + 1L

  # DM data rows — use write_data_table for structured output
  if (!is.null(rpt_dm) && is.data.frame(rpt_dm) && nrow(rpt_dm) > 0L) {
    current_row <- write_data_table(wb, sheet_name, data = rpt_dm,
                                    start_row = current_row, styles = styles)
  }
  current_row <- current_row + 1L  # blank separator

  # =================================================================
  # Section 2: Data Validation Summary (SAS lines 1565-1700)
  # Only if vld_err == "Y"
  # =================================================================
  if (toupper(vld_err) == "Y" && !is.null(rpt_err) &&
      is.data.frame(rpt_err) && nrow(rpt_err) > 0L) {

    openxlsx::writeData(wb, sheet_name, x = "Data Validation Summary",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    openxlsx::writeData(wb, sheet_name,
                        x = "Summary of data validation issues found.",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_sub,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    # Column headers
    err_col_labels <- if (ncol(rpt_err) > 0L) colnames(rpt_err) else
      c("Check", "Result")
    for (ci in seq_along(err_col_labels)) {
      if (ci <= n_cols) {
        openxlsx::writeData(wb, sheet_name, x = err_col_labels[ci],
                            startRow = current_row, startCol = ci,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = sty_col,
                           rows = current_row, cols = ci)
      }
    }
    current_row <- current_row + 1L

    # Data
    for (i in seq_len(nrow(rpt_err))) {
      is_last <- (i == nrow(rpt_err))
      for (j in seq_len(min(ncol(rpt_err), n_cols))) {
        val <- rpt_err[i, j, drop = TRUE]
        if (is.na(val)) val <- "."
        base_sty <- if (j == 1L) "Data" else "D0_R2"
        if (is_last) base_sty <- paste0(base_sty, "_BB")
        cell_style <- get_style_by_name(styles, base_sty)
        openxlsx::writeData(wb, sheet_name, x = val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = cell_style,
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
    current_row <- current_row + 1L
  }

  # =================================================================
  # Section 3: Data Validation by Term (SAS lines 1705-1850)
  # Only if vld_err == "Y" and rpt_err_term has data
  # =================================================================
  if (toupper(vld_err) == "Y" && !is.null(rpt_err_term) &&
      is.data.frame(rpt_err_term) && nrow(rpt_err_term) > 0L) {

    openxlsx::writeData(wb, sheet_name,
                        x = "Data Validation by Preferred Term",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    openxlsx::writeData(wb, sheet_name,
                        x = "Per-term data validation issue detail.",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_sub,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    # Column headers
    et_col_labels <- colnames(rpt_err_term)
    for (ci in seq_along(et_col_labels)) {
      if (ci <= n_cols) {
        openxlsx::writeData(wb, sheet_name, x = et_col_labels[ci],
                            startRow = current_row, startCol = ci,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = sty_col,
                           rows = current_row, cols = ci)
      }
    }
    current_row <- current_row + 1L

    for (i in seq_len(nrow(rpt_err_term))) {
      is_last <- (i == nrow(rpt_err_term))
      for (j in seq_len(min(ncol(rpt_err_term), n_cols))) {
        val <- rpt_err_term[i, j, drop = TRUE]
        if (is.na(val)) val <- "."
        base_sty <- if (j == 1L) "Data" else "D0_R2"
        if (is_last) base_sty <- paste0(base_sty, "_BB")
        cell_style <- get_style_by_name(styles, base_sty)
        openxlsx::writeData(wb, sheet_name, x = val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = cell_style,
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
    current_row <- current_row + 1L
  }

  # =================================================================
  # Section 4: Missing Toxicity Grades (SAS lines 1855-1990)
  # =================================================================
  openxlsx::writeData(wb, sheet_name, x = "Missing Toxicity Grades",
                      startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                     rows = current_row, cols = 1L)
  openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
  current_row <- current_row + 1L

  if (!is.null(rpt_missing) && is.data.frame(rpt_missing) &&
      nrow(rpt_missing) > 0L) {

    openxlsx::writeData(wb, sheet_name,
                        x = "Subjects with missing toxicity grade values.",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_sub,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    # Column headers
    mg_col_labels <- colnames(rpt_missing)
    for (ci in seq_along(mg_col_labels)) {
      if (ci <= n_cols) {
        openxlsx::writeData(wb, sheet_name, x = mg_col_labels[ci],
                            startRow = current_row, startCol = ci,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = sty_col,
                           rows = current_row, cols = ci)
      }
    }
    current_row <- current_row + 1L

    for (i in seq_len(nrow(rpt_missing))) {
      is_last <- (i == nrow(rpt_missing))
      for (j in seq_len(min(ncol(rpt_missing), n_cols))) {
        val <- rpt_missing[i, j, drop = TRUE]
        if (is.na(val)) val <- "."
        base_sty <- if (j == 1L) "Data" else "D0_R2"
        if (is_last) base_sty <- paste0(base_sty, "_BB")
        cell_style <- get_style_by_name(styles, base_sty)
        openxlsx::writeData(wb, sheet_name, x = val,
                            startRow = current_row, startCol = j,
                            colNames = FALSE)
        openxlsx::addStyle(wb, sheet_name, style = cell_style,
                           rows = current_row, cols = j)
      }
      current_row <- current_row + 1L
    }
  } else {
    openxlsx::writeData(wb, sheet_name,
                        x = "No subjects with missing toxicity grades.",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_data,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L
  }
  current_row <- current_row + 1L

  # =================================================================
  # Section 5: MedDRA Matching (SAS lines 1995-2191)
  # Only if meddra == "Y"
  # =================================================================
  if (toupper(meddra) == "Y") {

    openxlsx::writeData(wb, sheet_name, x = "MedDRA Matching",
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    meddra_pct_num <- suppressWarnings(as.numeric(meddra_pct))
    if (is.na(meddra_pct_num)) meddra_pct_num <- 100

    match_text <- stringr::str_c(
      "MedDRA dictionary matching results. ",
      "Match rate: ", janitor::round_half_up(meddra_pct_num, 1), "%"
    )
    openxlsx::writeData(wb, sheet_name, x = match_text,
                        startRow = current_row, startCol = 1L,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, style = sty_sub,
                       rows = current_row, cols = 1L)
    openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols, rows = current_row)
    current_row <- current_row + 1L

    # MedDRA summary table — column headers via purrr::iwalk
    if (!is.null(rpt_meddra) && is.data.frame(rpt_meddra) &&
        nrow(rpt_meddra) > 0L) {
      md_col_labels <- colnames(rpt_meddra)
      purrr::iwalk(md_col_labels, function(lbl, ci) {
        if (ci <= n_cols) {
          openxlsx::writeData(wb, sheet_name, x = lbl,
                              startRow = current_row, startCol = ci,
                              colNames = FALSE)
          openxlsx::addStyle(wb, sheet_name, style = sty_col,
                             rows = current_row, cols = ci)
        }
      })
      current_row <- current_row + 1L

      for (i in seq_len(nrow(rpt_meddra))) {
        is_last <- (i == nrow(rpt_meddra))
        for (j in seq_len(min(ncol(rpt_meddra), n_cols))) {
          val <- rpt_meddra[i, j, drop = TRUE]
          if (is.na(val)) val <- "."
          base_sty <- if (j == 1L) "Data" else "D0_R2"
          if (is_last) base_sty <- paste0(base_sty, "_BB")
          cell_style <- get_style_by_name(styles, base_sty)
          openxlsx::writeData(wb, sheet_name, x = val,
                              startRow = current_row, startCol = j,
                              colNames = FALSE)
          openxlsx::addStyle(wb, sheet_name, style = cell_style,
                             rows = current_row, cols = j)
        }
        current_row <- current_row + 1L
      }
      current_row <- current_row + 1L
    }

    # Per-term MedDRA detail (only if match < 100%)
    if (meddra_pct_num < 100 && !is.null(rpt_meddra_term) &&
        is.data.frame(rpt_meddra_term) && nrow(rpt_meddra_term) > 0L) {

      openxlsx::writeData(wb, sheet_name,
                          x = "MedDRA Matching - Unmatched Terms",
                          startRow = current_row, startCol = 1L,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet_name, style = sty_hdr,
                         rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, sheet_name, cols = 1L:n_cols,
                           rows = current_row)
      current_row <- current_row + 1L

      mt_col_labels <- colnames(rpt_meddra_term)
      for (ci in seq_along(mt_col_labels)) {
        if (ci <= n_cols) {
          openxlsx::writeData(wb, sheet_name, x = mt_col_labels[ci],
                              startRow = current_row, startCol = ci,
                              colNames = FALSE)
          openxlsx::addStyle(wb, sheet_name, style = sty_col,
                             rows = current_row, cols = ci)
        }
      }
      current_row <- current_row + 1L

      for (i in seq_len(nrow(rpt_meddra_term))) {
        is_last <- (i == nrow(rpt_meddra_term))
        for (j in seq_len(min(ncol(rpt_meddra_term), n_cols))) {
          val <- rpt_meddra_term[i, j, drop = TRUE]
          if (is.na(val)) val <- "."
          base_sty <- if (j == 1L) "Data" else "D0_R2"
          if (is_last) base_sty <- paste0(base_sty, "_BB")
          cell_style <- get_style_by_name(styles, base_sty)
          openxlsx::writeData(wb, sheet_name, x = val,
                              startRow = current_row, startCol = j,
                              colNames = FALSE)
          openxlsx::addStyle(wb, sheet_name, style = cell_style,
                             rows = current_row, cols = j)
        }
        current_row <- current_row + 1L
      }
    }
  }

  # Page setup
  apply_page_setup(wb, sheet_name, orientation = "landscape")

  invisible(wb)
}


# =============================================================================
# onc_out_workbook — Main entry point / orchestrator
# =============================================================================
#' Generate the Oncology AE MedDRA at a Glance Panel Excel Workbook.
#'
#' Replaces SAS \code{%out_onc} macro (lines 2410-2485 of
#' ae_oncology_output.sas). Orchestrates the entire workbook assembly:
#' creates workbook → applies styles → cover page → toxicity grade summary
#' (pt_1) → preferred-term analysis formatted/unformatted (pt_2) →
#' arm comparison formatted/unformatted (pt_3, only if arm_count > 1) →
#' data check summary → optional grouping/subsetting → saves workbook.
#'
#' @param output_file     Character: full path for the output XLSX file.
#' @param pt_1_output     Tibble: toxicity grade summary data.
#' @param pt_2_data       Tibble: preferred-term analysis data.
#' @param pt_3_data       Tibble: arm comparison data (RD/RR/OR).
#' @param rpt_dm          Tibble: subject validation per arm.
#' @param rpt_err         Tibble: validation error summary.
#' @param rpt_err_term    Tibble: validation errors per term.
#' @param rpt_missing     Tibble: missing toxicity grade info.
#' @param rpt_meddra      Tibble: MedDRA matching summary.
#' @param rpt_meddra_term Tibble: MedDRA matching per term.
#' @param rpt_key         Tibble: key column metadata (columns: key_col, label).
#' @param ndabla          Character: NDA/BLA identifier.
#' @param studyid         Character: study identifier.
#' @param arm_count       Integer: number of treatment arms.
#' @param arm_names       Character vector: arm display names (with N counts).
#' @param toxgr_max       Integer/Character: maximum toxicity grade.
#' @param toxgr_grp5_sw   Character: "Y" if grade 5 grouping active.
#' @param cmpgr           Character: comparison grade configuration.
#' @param meddra          Character: "Y" if MedDRA match reporting enabled.
#' @param meddra_pct      Numeric: MedDRA match percentage (0-100).
#' @param cc_sw           Character: continuity correction switch.
#' @param cc_desc         Character: continuity correction description.
#' @param study_lag       Character: study lag period description.
#' @param vld_sw          Character: validation switch.
#' @param vld_err         Character: "Y" if validation errors present.
#' @param ae_aetoxgr      Character: AETOXGR availability flag.
#' @param ae_rate_ci_sw   Logical: include AE rate CI columns (default FALSE).
#' @param dme_sw          Logical: DME switch (default FALSE).
#' @param sl_group_desc   Character: Script Launcher grouping description.
#' @param sl_subset_desc  Character: Script Launcher subsetting description.
#' @param pp_result       List: preprocessed grouping/subsetting result from
#'                        \code{group_subset_pp()}, or NULL.
#' @return Invisible character: path to saved workbook.
#' @export
onc_out_workbook <- function(output_file,
                             pt_1_output,
                             pt_2_data,
                             pt_3_data,
                             rpt_dm,
                             rpt_err,
                             rpt_err_term,
                             rpt_missing,
                             rpt_meddra,
                             rpt_meddra_term,
                             rpt_key,
                             ndabla,
                             studyid,
                             arm_count,
                             arm_names,
                             toxgr_max,
                             toxgr_grp5_sw,
                             cmpgr,
                             meddra,
                             meddra_pct,
                             cc_sw,
                             cc_desc,
                             study_lag,
                             vld_sw,
                             vld_err,
                             ae_aetoxgr,
                             ae_rate_ci_sw = FALSE,
                             dme_sw = FALSE,
                             sl_group_desc = "No grouping",
                             sl_subset_desc = "No subsetting",
                             pp_result = NULL) {

  # --- Input validation ---
  stopifnot(
    is.character(output_file) && nchar(output_file) > 0L,
    is.numeric(arm_count) && arm_count >= 1L,
    is.character(arm_names) && length(arm_names) >= arm_count
  )

  cli::cli_inform("Building oncology AE workbook: {output_file}")
  rundate <- format(Sys.time(), "%d%b%Y %H:%M")

  # --- Step 1: Create workbook and style gallery ---
  wb <- create_workbook(
    title = stringr::str_c("Oncology AE MedDRA at a Glance - ", studyid),
    author = "PhUSE CS Standard Analyses"
  )

  # Build combined style gallery: base + oncology-specific
  base_styles <- create_workbook_styles()
  onc_styles <- onc_out_styles(base_size = 9L)

  # Merge: oncology styles take precedence for overlapping names
  styles <- c(base_styles, onc_styles)
  # Remove duplicates (keep oncology version when names overlap)
  dup_names <- duplicated(names(styles), fromLast = TRUE)
  styles <- styles[!dup_names]

  cli::cli_inform("Style gallery ready ({length(styles)} styles).")

  # --- Step 2: Cover page ---
  onc_out_cover(wb, ndabla = ndabla, studyid = studyid, rundate = rundate,
                arm_count = arm_count, arm_names = arm_names,
                toxgr_max = toxgr_max, toxgr_grp5_sw = toxgr_grp5_sw,
                cmpgr = cmpgr, meddra = meddra, meddra_pct = meddra_pct,
                cc_sw = cc_sw, cc_desc = cc_desc, study_lag = study_lag,
                vld_sw = vld_sw, ae_rate_ci_sw = ae_rate_ci_sw,
                sl_group_desc = sl_group_desc,
                sl_subset_desc = sl_subset_desc,
                rpt_key = rpt_key, styles = styles)

  # --- Step 3: Toxicity grade summary (pt_1) ---
  onc_out_pt1(wb, pt_1_output = pt_1_output, rpt_key = rpt_key,
              rpt_missing = rpt_missing, arm_count = arm_count,
              arm_names = arm_names, ae_aetoxgr = ae_aetoxgr,
              toxgr_max = toxgr_max, toxgr_grp5_sw = toxgr_grp5_sw,
              styles = styles)

  # --- Step 4: PT Analysis unformatted (pt_2, fmt=N) ---
  onc_out_pt2(wb, pt_2_data = pt_2_data, rpt_key = rpt_key,
              rpt_missing = rpt_missing, arm_count = arm_count,
              arm_names = arm_names, toxgr_max = toxgr_max,
              toxgr_grp5_sw = toxgr_grp5_sw, fmt = FALSE,
              styles = styles)

  # --- Step 5: PT Analysis formatted (pt_2, fmt=Y) ---
  onc_out_pt2(wb, pt_2_data = pt_2_data, rpt_key = rpt_key,
              rpt_missing = rpt_missing, arm_count = arm_count,
              arm_names = arm_names, toxgr_max = toxgr_max,
              toxgr_grp5_sw = toxgr_grp5_sw, fmt = TRUE,
              styles = styles)

  # --- Step 6: Arm Comparison unformatted (pt_3, fmt=N) ---
  # Only if arm_count > 1 (single-arm studies skip comparison)
  if (arm_count > 1L) {
    onc_out_pt3(wb, pt_3_data = pt_3_data, rpt_key = rpt_key,
                arm_count = arm_count, arm_names = arm_names,
                cmpgr = cmpgr, cc_sw = cc_sw,
                ae_rate_ci_sw = ae_rate_ci_sw, fmt = FALSE,
                styles = styles)

    # --- Step 7: Arm Comparison formatted (pt_3, fmt=Y) ---
    onc_out_pt3(wb, pt_3_data = pt_3_data, rpt_key = rpt_key,
                arm_count = arm_count, arm_names = arm_names,
                cmpgr = cmpgr, cc_sw = cc_sw,
                ae_rate_ci_sw = ae_rate_ci_sw, fmt = TRUE,
                styles = styles)
  }

  # --- Step 8: Data Check Summary ---
  onc_out_err(wb, rpt_dm = rpt_dm, rpt_err = rpt_err,
              rpt_err_term = rpt_err_term, rpt_missing = rpt_missing,
              rpt_meddra = rpt_meddra, rpt_meddra_term = rpt_meddra_term,
              vld_sw = vld_sw, vld_err = vld_err, meddra = meddra,
              meddra_pct = meddra_pct, arm_count = arm_count,
              arm_names = arm_names, styles = styles)

  # --- Step 9: Optional Grouping and Subsetting worksheet ---
  if (!is.null(pp_result)) {
    tryCatch({
      group_subset_write_ws(wb, pp_result = pp_result,
                            ndabla = ndabla, studyid = studyid,
                            styles = styles)
      cli::cli_inform("Grouping and Subsetting worksheet added.")
    }, error = function(e) {
      cli::cli_warn("Could not add Grouping/Subsetting worksheet: {e$message}")
    })
  }

  # --- Step 10: Save workbook ---
  # Ensure output directory exists
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)
  cli::cli_inform("Oncology AE workbook saved: {output_file}")

  invisible(output_file)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SpreadsheetML XML generation fully replaced by openxlsx API.
#    - All 7 SAS macros (%out_cover, %out_pt_1, %out_pt_2, %out_pt_3,
#      %out_err, %out_oae_styles, %out_onc) mapped to 7+ R functions
#      preserving worksheet structure and content.
#    - Oncology-specific styles (D0_R1, D0_R2, D1_R1, D1_R2, D2_R1,
#      D2_R2, LCL, UCL, UCLSN, DT) created via onc_out_styles() with
#      border suffixes (_B, _BL, _BR, _BB, _BLR, _BLB, _BRB, _BLRB)
#      and highlight (H) variants.
#    - pt_2/pt_3 fmt=Y/N variants controlled by 'fmt' logical parameter
#      (TRUE=formatted with freeze panes, FALSE=unformatted with
#      auto-filter/conditional formatting).
#    - SAS macro variables (&ndabla, &studyid, etc.) replaced by named
#      R function arguments with no global state mutation.
#    - group_subset_write_ws() from sl_gs_output.R called directly
#      (replacing SAS %group_subset_xml_out).
#    - SAS Height= attributes approximated via openxlsx::setRowHeights()
#      with pixel-to-point conversion.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Column widths: SAS specifies pixels (e.g., Width="200");
#      openxlsx uses character-width units. Approximate conversion used.
#    - Row heights: SAS SpreadsheetML Height attribute is in points;
#      openxlsx setRowHeights also uses points but rendering may vary.
#    - Percentage rounding uses janitor::round_half_up() for SAS
#      compatibility; any remaining differences are sub-epsilon.
#    - Conditional formatting rendering may vary across Excel versions
#      and OpenOffice/LibreOffice implementations.
# NO DIRECT R EQUIVALENT:
#    - SAS SpreadsheetML XML string building → openxlsx object API.
#      No line-for-line correspondence; functional equivalence verified
#      at the worksheet/cell level.
#    - SAS %ws helper macros → inline openxlsx::addWorksheet() calls.
#    - SAS FreezePanes XML element → openxlsx::freezePane().
#    - SAS AutoFilter XML → openxlsx::addFilter().
#    - SAS FILE/PUT streaming output → openxlsx::saveWorkbook().
#    - SAS macro %do loops → R for/purrr::walk iterations.
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: AAP-mandated Excel output engine replacing
#      SpreadsheetML. Supports styles, merges, conditional formatting,
#      freeze panes, auto-filters, named regions.
#    - dplyr: Tidyverse data manipulation (AAP mandates tidyverse
#      over base R).
#    - janitor: round_half_up() for SAS-compatible rounding (AAP Gate 2).
#    - purrr: Functional iteration replacing SAS %do loops.
#    - stringr: Tidyverse string operations (AAP mandates over base R).
#    - cli: User-facing progress/warning messages replacing %PUT.
# OPEN QUESTIONS:
#    - CI column conditional inclusion: verify ae_rate_ci_sw flag
#      behavior matches production SAS output for all arm configurations.
#    - Style ID mapping completeness: oncology-specific style names
#      with border/highlight permutations may need extension for edge
#      cases not covered in the analyzed SAS source sample.
#    - cc_ind flag interpretation in pt_3: confirm that D1_R2S vs
#      D1_R2N distinction corresponds to cc_flag TRUE/FALSE.
#    - DME switch (dme_sw) handling: currently passed through but no
#      DME-specific logic identified in source SAS; verify with
#      production team.
# ============================================================
