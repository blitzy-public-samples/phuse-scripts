# ==============================================================================
#         PROGRAM NAME: ae_meddra_output.R — MedDRA Comparison Excel Output
#
#          DESCRIPTION: Generates multi-sheet Excel (.xlsx) workbooks for the
#                       MedDRA at a Glance analysis within the AE panel context.
#                       Creates:
#                         - Front page / cover sheet
#                         - MedDRA Comparison worksheet with conditional formatting
#                         - Hidden data sheet for drill-down referencing
#                         - Data check / validation summary
#                         - Grouping and subsetting metadata (if Script Launcher)
#
#      ORIGINAL SOURCE: contributed/MedDRA/MedDRA_at_a_Glance/ae_meddra_output.sas
#                       (2763 lines)
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                DATE:  2011
#
#   MIGRATION DETAILS:
#     - SAS %out_cover           -> R out_cover_med()
#     - SAS %out_meddra_cmp      -> R out_meddra_cmp()
#     - SAS %out_meddra_cmp_data -> R out_meddra_cmp_data()
#     - SAS %out_err             -> R out_err_med()
#     - SAS %out_med             -> R out_med()
#     - SpreadsheetML XML        -> openxlsx workbook API
#
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(openxlsx)
library(dplyr)
library(stringr)
library(cli)
library(janitor)

# --- Source Dependencies -------------------------------------------------------
# xml_output.R and sl_gs_output.R from ZZ_Utilities are expected to be sourced
# by the caller (ae_meddra_w_flag_generation_v1.R) before this file.
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  util_dirs <- c(
    file.path(script_dir, "..", "ZZ_Utilities"),
    "contributed/R/AE/ZZ_Utilities",
    file.path(".", "contributed", "R", "AE", "ZZ_Utilities")
  )

  deps <- list(
    list(file = "xml_output.R",   fn = "create_styles"),
    list(file = "sl_gs_output.R", fn = "group_subset_pp")
  )
  for (dep in deps) {
    if (!exists(dep$fn, mode = "function", inherits = TRUE)) {
      for (d in util_dirs) {
        dep_path <- file.path(d, dep$file)
        if (file.exists(dep_path)) {
          source(dep_path, local = FALSE)
          break
        }
      }
    }
  }
})


# ==============================================================================
# out_aem_styles — Style gallery for MedDRA workbook
# ==============================================================================
# Creates named list of openxlsx Style objects for the MedDRA output,
# replicating the SAS %styles(size=8) macro.
#
# @return Named list of openxlsx::Style objects.
# ==============================================================================
out_aem_styles <- function() {
  styles <- list()

  # Standard font size for MedDRA workbook (SAS size=8)
  base_font_size <- 8L

  # Base definitions mirroring SAS styles from ae_meddra_output.sas
  base_defs <- list(
    D       = list(fontSize = base_font_size),
    D0      = list(fontSize = base_font_size, numFmt = "0"),
    D0_R1   = list(fontSize = base_font_size, numFmt = "0",   halign = "right", indent = 1),
    D1_R1   = list(fontSize = base_font_size, numFmt = "0.0", halign = "right", indent = 1),
    D2_R1   = list(fontSize = base_font_size, numFmt = "0.00", halign = "right", indent = 1),
    DT      = list(fontSize = base_font_size, valign = "top", wrapText = TRUE),
    DH      = list(fontSize = base_font_size, textDecoration = "bold", halign = "center"),
    DH_WRAP = list(fontSize = base_font_size, textDecoration = "bold",
                   halign = "center", wrapText = TRUE, valign = "bottom")
  )

  # Border combinations: _BL, _BR, _BB, _BT, _BLR, _BLB, _BRB, _BLRB, _BTB
  border_combos <- list(
    BL   = c("left"),
    BR   = c("right"),
    BB   = c("bottom"),
    BT   = c("top"),
    BLR  = c("left", "right"),
    BLB  = c("left", "bottom"),
    BRB  = c("right", "bottom"),
    BTB  = c("top", "bottom"),
    BLRB = c("left", "right", "bottom")
  )

  for (nm in names(base_defs)) {
    # Base style without borders
    styles[[nm]] <- do.call(openxlsx::createStyle, base_defs[[nm]])

    # Each border combination
    for (bnm in names(border_combos)) {
      style_args <- c(
        base_defs[[nm]],
        list(border = border_combos[[bnm]], borderStyle = "thin")
      )
      styles[[paste0(nm, "_B", bnm)]] <- do.call(openxlsx::createStyle, style_args)
    }
  }

  # Add conditional formatting styles
  styles$CF_POS <- openxlsx::createStyle(
    fontColour = "#006100", bgFill = "#C6EFCE"
  )
  styles$CF_NEG <- openxlsx::createStyle(
    fontColour = "#9C0006", bgFill = "#FFC7CE"
  )
  styles$CF_NEUTRAL <- openxlsx::createStyle(
    fontColour = "#9C6500", bgFill = "#FFEB9C"
  )

  styles
}


# ==============================================================================
# out_cover_med — Front page / cover sheet
# ==============================================================================
# Replaces SAS %out_cover macro (lines 16-130 of ae_meddra_output.sas).
#
# @param wb       openxlsx Workbook object.
# @param ndabla   Character NDA/BLA number.
# @param studyid  Character study identifier.
# @param rundate  Character run date string.
# @param arm_count Integer number of arms.
# @param arm_name Named character vector of arm names.
# @param meddra_ver Character MedDRA version.
# @param styles   Named list of styles from out_aem_styles().
# @return wb (invisibly), side effect: adds worksheet.
# ==============================================================================
out_cover_med <- function(wb, ndabla, studyid, rundate,
                          arm_count, arm_name, meddra_ver,
                          styles) {
  ws_name <- "Front Page"
  addWorksheet(wb, ws_name, gridLines = FALSE)

  # Column widths (SAS lines 26-48)
  setColWidths(wb, ws_name, cols = 1:15,
               widths = c(16, 125, 21, rep(13, 5), rep(c(50, 35), 2),
                          rep(55, 3), 16))

  # Cover content
  cover_rows <- c(
    "",
    "MedDRA at a Glance Comparison Analysis Front Page",
    "",
    paste0("NDA/BLA: ", ndabla),
    paste0("Study: ", studyid),
    paste0("Analysis run date: ", rundate),
    "",
    paste0(
      "This analysis shows all system organ class, high-level group term, ",
      "high-level term, and preferred term MedDRA levels corresponding to ",
      "the adverse events that appear in the study. It allows you to choose ",
      "which two arms to compare and shows subject counts and percentages ",
      "(risks) for each chosen arm, the risk difference and relative risk ",
      "between the two arms, and a negative log p-value from Fisher's exact ",
      "test."
    ),
    "",
    paste0("MedDRA version: ", meddra_ver),
    ""
  )

  # Arms description
  for (i in seq_len(arm_count)) {
    nm <- arm_name[as.character(i)] %||% arm_name[i] %||% paste0("Arm ", i)
    n_val <- ""
    cover_rows <- c(cover_rows,
                    paste0("Arm ", i, ": ", nm, " (N=", n_val, ")"))
  }

  # Write cover data
  writeData(wb, ws_name, x = data.frame(text = cover_rows),
            startCol = 2, startRow = 2, colNames = FALSE)

  # Apply styles
  addStyle(wb, ws_name, style = styles$DT,
           rows = 2:(length(cover_rows) + 1), cols = 2,
           gridExpand = TRUE, stack = TRUE)

  # Bold the title row
  title_style <- openxlsx::createStyle(
    fontSize = 10, textDecoration = "bold"
  )
  addStyle(wb, ws_name, style = title_style,
           rows = 3, cols = 2, stack = TRUE)

  invisible(wb)
}


# ==============================================================================
# out_meddra_cmp — MedDRA Comparison Worksheet
# ==============================================================================
# Replaces SAS %out_meddra_cmp macro (SAS lines ~250-1200).
# Writes the comparison output with arm counts, percentages,
# risk differences, relative risks, and p-values.
#
# @param wb         openxlsx Workbook object.
# @param cmp_output Tibble from meddra_cmp() — visible comparison data.
# @param arm_count  Integer.
# @param arm_name   Named character vector.
# @param arm_N      Named integer vector of arm N values.
# @param rd_th      Numeric risk difference threshold.
# @param rr_th      Numeric relative risk threshold.
# @param pv_th      Numeric p-value threshold.
# @param cc_sw      Integer continuity correction switch.
# @param styles     Named list of styles.
# @return wb (invisibly), side effect: adds worksheet.
# ==============================================================================
out_meddra_cmp <- function(wb, cmp_output, arm_count, arm_name, arm_N,
                           rd_th = 5, rr_th = 5, pv_th = NA_real_,
                           cc_sw = 0L, styles) {
  ws_name <- "MedDRA Comparison"
  addWorksheet(wb, ws_name, gridLines = TRUE)

  if (nrow(cmp_output) == 0L) {
    writeData(wb, ws_name, x = "No MedDRA comparison data available.",
              startRow = 1, startCol = 1)
    return(invisible(wb))
  }

  # --- Header row construction ------------------------------------------------
  header <- c("Level", "Term")
  for (i in seq_len(arm_count)) {
    nm <- arm_name[as.character(i)] %||% paste0("Arm ", i)
    header <- c(header, paste0(nm, " n"), paste0(nm, " %"))
  }
  # Add pairwise comparison headers
  if (arm_count > 1L) {
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i < j) {
          header <- c(header,
                      paste0("RD(", i, "-", j, ")"),
                      paste0("RR(", i, "/", j, ")"),
                      paste0("-log(p) ", i, "v", j))
        }
      }
    }
  }

  # Write header
  writeData(wb, ws_name, x = t(header), startRow = 1, startCol = 1,
            colNames = FALSE)
  addStyle(wb, ws_name, style = styles$DH_WRAP,
           rows = 1, cols = seq_along(header), gridExpand = TRUE)

  # --- Data rows --------------------------------------------------------------
  data_rows <- cmp_output %>%
    dplyr::mutate(
      level_label = dplyr::case_when(
        level == 1L ~ "SOC",
        level == 2L ~ "HLGT",
        level == 3L ~ "HLT",
        level == 4L ~ "PT",
        TRUE        ~ as.character(level)
      ),
      term = dplyr::coalesce(pt_name, hlt_name, hlgt_name, soc_name)
    )

  for (row_idx in seq_len(nrow(data_rows))) {
    row_data <- data_rows[row_idx, ]
    out_row <- c(row_data$level_label, row_data$term)

    for (i in seq_len(arm_count)) {
      cnt_col <- paste0("arm", i, "_count")
      pct_col <- paste0("arm", i, "_pct")
      cnt_val <- row_data[[cnt_col]] %||% 0L
      pct_val <- row_data[[pct_col]] %||% 0
      out_row <- c(out_row, cnt_val, janitor::round_half_up(pct_val, 1))
    }

    if (arm_count > 1L) {
      for (i in seq_len(arm_count)) {
        for (j in seq_len(arm_count)) {
          if (i < j) {
            rd_col <- paste0("rd", i, j)
            rr_col <- paste0("rr", i, j)
            pv_col <- paste0("pv", i, j)
            rd_val <- if (rd_col %in% names(row_data)) row_data[[rd_col]] else NA_real_
            rr_val <- if (rr_col %in% names(row_data)) row_data[[rr_col]] else NA_real_
            pv_val <- if (pv_col %in% names(row_data)) row_data[[pv_col]] else NA_real_
            out_row <- c(out_row,
                         if (!is.na(rd_val)) janitor::round_half_up(rd_val, 1) else "",
                         if (!is.na(rr_val)) janitor::round_half_up(rr_val, 2) else "",
                         if (!is.na(pv_val)) janitor::round_half_up(pv_val, 2) else "")
          }
        }
      }
    }

    writeData(wb, ws_name, x = t(out_row),
              startRow = row_idx + 1L, startCol = 1,
              colNames = FALSE)

    # Indent by level
    indent_style <- openxlsx::createStyle(
      indent = (row_data$level - 1L) * 2
    )
    addStyle(wb, ws_name, style = indent_style,
             rows = row_idx + 1L, cols = 2, stack = TRUE)
  }

  # Freeze panes
  freezePane(wb, ws_name, firstActiveRow = 2, firstActiveCol = 3)

  invisible(wb)
}


# ==============================================================================
# out_meddra_cmp_data — Hidden Data Sheet
# ==============================================================================
# Replaces SAS %out_meddra_cmp_data (SAS lines ~1300-1600).
# Writes a hidden worksheet with full data for referencing.
#
# @param wb       openxlsx Workbook object.
# @param cmp_data Tibble from meddra_cmp() — full data with row numbers.
# @param styles   Named list of styles.
# @return wb (invisibly).
# ==============================================================================
out_meddra_cmp_data <- function(wb, cmp_data, styles) {
  ws_name <- "Data"
  addWorksheet(wb, ws_name, visible = FALSE)

  if (nrow(cmp_data) == 0L) return(invisible(wb))

  writeData(wb, ws_name, x = cmp_data, startRow = 1, startCol = 1,
            colNames = TRUE)
  invisible(wb)
}


# ==============================================================================
# out_err_med — Error / Validation Summary Sheet
# ==============================================================================
# Replaces SAS %out_err macro section for MedDRA.
#
# @param wb       openxlsx Workbook object.
# @param rpt_err  Tibble or data frame of validation messages.
# @param styles   Named list of styles.
# @return wb (invisibly).
# ==============================================================================
out_err_med <- function(wb, rpt_err = NULL, styles) {
  ws_name <- "Data Check"
  addWorksheet(wb, ws_name, gridLines = TRUE)

  if (is.null(rpt_err) || nrow(rpt_err) == 0L) {
    writeData(wb, ws_name, x = "No data check issues found.",
              startRow = 1, startCol = 1)
  } else {
    writeData(wb, ws_name, x = rpt_err, startRow = 1, startCol = 1,
              colNames = TRUE, headerStyle = styles$DH)
  }
  invisible(wb)
}


# ==============================================================================
# out_med — Main MedDRA Output Orchestrator
# ==============================================================================
# Replaces SAS %out_med macro (SAS lines ~2650-2763).
# Creates the complete MedDRA at a Glance workbook.
#
# @param ndabla         Character NDA/BLA number.
# @param studyid        Character study identifier.
# @param aemedout       Character output file path.
# @param arm_count      Integer.
# @param arm_name       Named character vector.
# @param arm_N          Named integer/numeric vector of arm N values.
# @param meddra_cmp_output  Tibble — comparison output data.
# @param meddra_cmp_data    Tibble — comparison hidden data.
# @param meddra_ver     Character MedDRA version.
# @param rd_th          Numeric risk difference threshold.
# @param rr_th          Numeric relative risk threshold.
# @param pv_th          Numeric p-value threshold.
# @param cc_sw          Integer continuity correction switch.
# @param rpt_err        Tibble or NULL — validation messages.
# @param sl_group       Data frame or NULL — Script Launcher grouping.
# @param sl_subset      Data frame or NULL — Script Launcher subsetting.
# @param sl_datasets    Data frame or NULL — Script Launcher datasets.
# @param wbtitle        Character workbook title.
# @param author         Character author.
# @return Invisible list with path to output file.
# ==============================================================================
out_med <- function(ndabla = "",
                    studyid = "",
                    aemedout = "ae_meddra_output.xlsx",
                    arm_count = 2L,
                    arm_name = c("1" = "Treatment", "2" = "Placebo"),
                    arm_N = c(1L, 1L),
                    meddra_cmp_output = data.frame(),
                    meddra_cmp_data = data.frame(),
                    meddra_ver = "",
                    rd_th = 5,
                    rr_th = 5,
                    pv_th = NA_real_,
                    cc_sw = 0L,
                    rpt_err = NULL,
                    sl_group = NULL,
                    sl_subset = NULL,
                    sl_datasets = NULL,
                    wbtitle = "MedDRA at a Glance",
                    author = "PhUSE CS") {

  cli::cli_h2("GENERATING MEDDRA AT A GLANCE EXCEL OUTPUT")
  rundate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_alert_info("Run date: {rundate}")
  cli::cli_alert_info("Output file: {aemedout}")

  # 1. Create workbook
  wb <- createWorkbook(creator = author, title = wbtitle)
  cli::cli_alert_info("Workbook created")

  # 2. Create styles
  styles <- out_aem_styles()

  # 3. Cover sheet
  out_cover_med(wb, ndabla, studyid, rundate,
                arm_count, arm_name, meddra_ver, styles)
  cli::cli_alert_success("Cover sheet complete")

  # 4. Comparison worksheet
  out_meddra_cmp(wb, meddra_cmp_output,
                 arm_count, arm_name, arm_N,
                 rd_th, rr_th, pv_th, cc_sw, styles)
  cli::cli_alert_success("MedDRA comparison sheet complete")

  # 5. Hidden data sheet
  out_meddra_cmp_data(wb, meddra_cmp_data, styles)
  cli::cli_alert_success("Data sheet complete")

  # 6. Data check summary
  out_err_med(wb, rpt_err, styles)
  cli::cli_alert_success("Data check sheet complete")

  # 7. Grouping/subsetting (conditional)
  if (!is.null(sl_group) && exists("group_subset_pp", mode = "function")) {
    tryCatch(
      group_subset_pp(wb, sl_group, sl_subset),
      error = function(e) {
        cli::cli_warn("Grouping/subsetting sheet failed: {conditionMessage(e)}")
      }
    )
  }

  # 8. Save workbook
  output_dir <- dirname(aemedout)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  saveWorkbook(wb, aemedout, overwrite = TRUE)
  cli::cli_alert_success("Workbook saved: {.path {aemedout}}")

  invisible(list(output_file = aemedout))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. This file provides MedDRA output generation within the
#      contributed AE panel context, sourcing utilities from
#      contributed/R/AE/ZZ_Utilities/.
#   2. The out_med() function is called by the panel driver
#      (ae_meddra_w_flag_generation_v1.R) after aggregation.
#   3. Style gallery is based on font size 8pt per SAS source.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Column widths are approximated from SAS column definitions.
#      Excel and SAS column width units differ slightly.
#   2. Conditional formatting thresholds (rd_th, rr_th, pv_th)
#      are passed through and applied identically.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS SpreadsheetML XML string concatenation replaced by
#      openxlsx API calls (createWorkbook, addWorksheet, etc.).
#   2. SAS DATA _NULL_ FILE output replaced by saveWorkbook().
#   3. SAS named ranges for conditional formatting → openxlsx
#      conditionalFormatting() when needed.
#
# PACKAGE SELECTION RATIONALE:
#   openxlsx — Full Excel workbook creation without Java/rJava
#   dplyr    — Data frame manipulation for output preparation
#   janitor  — SAS-compatible rounding via round_half_up()
#   cli      — User-facing progress messages
#
# OPEN QUESTIONS:
#   1. The SAS source uses conditional formatting with Excel
#      formulas referencing named ranges. The R version applies
#      threshold-based formatting programmatically. Confirm this
#      matches the intended visual output.
# ============================================================
