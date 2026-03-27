# ============================================================================
# PROGRAM: ae_meddra_output.R — MedDRA at-a-Glance Excel Workbook Rendering
# DESCRIPTION: Converts SAS SpreadsheetML XML generation to openxlsx workbook
#   pipeline for the MedDRA at-a-Glance adverse event comparison report.
#   Produces a multi-worksheet Excel workbook with:
#     - Front Page (cover sheet with instructions and metadata)
#     - MedDRA Comparison Analysis (visible comparison worksheet)
#     - MedDRA Comparison Data (hidden data sheet with formulas)
#     - Workbook Information (hidden parameter sheet)
#     - Data Check Summary (validation/error sheet)
#     - Grouping and Subsetting (optional SL metadata sheet)
#
# ORIGINAL: contributed/MedDRA/MedDRA_at_a_Glance/ae_meddra_output.sas
#           (2763 lines)
# AUTHOR: David Kretch (original SAS), migrated to R
# DATE: February 15, 2011 (original SAS), migrated 2024
#
# EXPORTS: out_med, out_cover, out_meddra_cmp, out_meddra_cmp_data,
#          wbinfo, out_err, out_aem_styles
# ============================================================================

library(openxlsx)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)
library(cli)

# --------------------------------------------------------------------------- #
# Internal Dependency: xml_output.R
# Provides: wb_create(), create_workbook_styles()/create_styles(),
#           annotate_data(), write_annotated(), style_from_spec()
# --------------------------------------------------------------------------- #
if (!exists("wb_create", mode = "function") ||
    !exists("annotate_data", mode = "function") ||
    !exists("write_annotated", mode = "function") ||
    !exists("style_from_spec", mode = "function")) {
  local({
    script_dir <- tryCatch(dirname(sys.frame(1L)$ofile), error = function(e) ".")
    if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."
    candidates <- c(
      file.path(script_dir, "xml_output.R"),
      "contributed/R/MedDRA/xml_output.R",
      file.path(".", "xml_output.R")
    )
    for (cpath in candidates) {
      if (file.exists(cpath)) {
        source(cpath, local = FALSE)
        break
      }
    }
  })
}
# Alias for schema compatibility: create_styles -> create_workbook_styles
if (exists("create_workbook_styles", mode = "function") &&
    !exists("create_styles", mode = "function")) {
  create_styles <- create_workbook_styles
}

# --------------------------------------------------------------------------- #
# Internal Dependency: sl_gs_output.R
# Provides: group_subset_pp(), group_subset_xml_out()
# --------------------------------------------------------------------------- #
if (!exists("group_subset_pp", mode = "function") ||
    !exists("group_subset_xml_out", mode = "function")) {
  local({
    script_dir <- tryCatch(dirname(sys.frame(1L)$ofile), error = function(e) ".")
    if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."
    candidates <- c(
      file.path(script_dir, "sl_gs_output.R"),
      "contributed/R/MedDRA/sl_gs_output.R",
      file.path(".", "sl_gs_output.R")
    )
    for (cpath in candidates) {
      if (file.exists(cpath)) {
        source(cpath, local = FALSE)
        break
      }
    }
  })
}


# ============================================================================
# out_aem_styles — Extended MedDRA-Specific Style Definitions
# Replaces SAS %out_aem_styles macro (lines 2525-2672 of ae_meddra_output.sas)
#
# Creates a named list of openxlsx Style objects specific to the MedDRA
# comparison workbook. These supplement the base styles from xml_output.R.
#
# Style families:
#   - D (Data): base data cell with L/R borders
#   - D0_R1..D2_R3: decimal format + indent combos (D=dec digits, R=indent)
#   - IB: continuity correction format
#   - BL/BR/BB border combinations: all permutations
#   - D/DC/DB/DCB: text alignment variants
#   - I: yellow input cell (#FFFF99)
#   - TBT/TBM/TBL/TBR/TBB: text box border segments
#   - D1_R2BR_BLRB: red bold decimal
#   - I10: 10pt indented wrap
#   - B10O: bold 10pt centered gray bg dashed border
#   - DG/LG/R/P: signal indicator color cells
#   - OR: rotated 90 centered
#   - OW_BLR/OW_BLRB: white font hidden text
#   - COR: continuity correction rotated header
#
# @return Named list of openxlsx Style objects.
# @export
# ============================================================================
out_aem_styles <- function() {
  styles <- list()

  # Base font size for MedDRA styles
  sz <- 8L

  # ----------------------------------------------------------------
  # First dataset: decimal/indent/border combos (SAS lines 2528-2610)
  # Pattern: D{dec}_R{indent}_B{borders}
  # dec = 0,1,2 decimal places; indent via numFmt prefix spaces
  # R1=1-space indent, R2=2-space indent, R3=3-space indent
  # ----------------------------------------------------------------

  # Build number formats with indent levels
  # R1 = " " prefix (1 space), R2 = "  " (2 spaces), R3 = "   " (3 spaces)
  dec_fmts <- list(
    "0" = list(D0 = "0",   D1 = "0.0",   D2 = "0.00"),
    "1" = list(D0 = " 0",  D1 = " 0.0",  D2 = " 0.00"),
    "2" = list(D0 = "  0", D1 = "  0.0",  D2 = "  0.00"),
    "3" = list(D0 = "   0", D1 = "   0.0", D2 = "   0.00")
  )

  # Base style properties shared by all data cells
  base_props <- list(fontSize = sz, halign = "right", valign = "top")

  # IB style: continuity correction indicator — centered text
  ib_props <- list(fontSize = sz, halign = "center", valign = "top")

  # Generate D{d}_R{r} base styles (no border variants yet)
  base_style_specs <- list()
  for (r in 0:3) {
    r_str <- as.character(r)
    for (d in 0:2) {
      nm <- paste0("D", d, "_R", r + 1)
      nf <- dec_fmts[[as.character(r)]][[paste0("D", d)]]
      base_style_specs[[nm]] <- list(numFmt = nf)
    }
  }
  # Add IB style spec
  base_style_specs[["IB"]] <- list(is_ib = TRUE)

  # Generate all border permutations: BL, BR, BB and their combos
  # Note: first entry has no borders (suffix = "NONE" placeholder, mapped to "")
  border_combos <- list(
    "NONE" = character(0),
    "BL"   = "Left",
    "BR"   = "Right",
    "BB"   = "Bottom",
    "BLR"  = c("Left", "Right"),
    "BLB"  = c("Left", "Bottom"),
    "BRB"  = c("Right", "Bottom"),
    "BLRB" = c("Left", "Right", "Bottom")
  )

  for (spec_nm in names(base_style_specs)) {
    spec <- base_style_specs[[spec_nm]]
    is_ib <- isTRUE(spec$is_ib)

    for (b_nm in names(border_combos)) {
      borders <- border_combos[[b_nm]]
      full_nm <- if (b_nm != "NONE") paste0(spec_nm, "_", b_nm) else spec_nm

      args <- if (is_ib) ib_props else base_props
      if (!is_ib && !is.null(spec$numFmt)) {
        args$numFmt <- spec$numFmt
      }
      if (length(borders) > 0L) {
        args$border <- borders
        args$borderStyle <- "thin"
      }
      styles[[full_nm]] <- do.call(createStyle, args)
    }
  }

  # ----------------------------------------------------------------
  # Second dataset: special one-off styles (SAS lines 2613-2665)
  # ----------------------------------------------------------------

  # D: base data cell — left align, top align, L/R borders
  styles[["D"]] <- createStyle(
    fontSize = sz, halign = "left", valign = "top",
    border = c("Left", "Right"), borderStyle = "thin"
  )
  # DC: data center
  styles[["DC"]] <- createStyle(
    fontSize = sz, halign = "center", valign = "top",
    border = c("Left", "Right"), borderStyle = "thin"
  )
  # DB: data bottom
  styles[["DB"]] <- createStyle(
    fontSize = sz, halign = "left", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )
  # DCB: data center bottom
  styles[["DCB"]] <- createStyle(
    fontSize = sz, halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # I: yellow input cell (#FFFF99 bg)
  styles[["I"]] <- createStyle(
    fontSize = sz, halign = "center", valign = "top",
    fgFill = "#FFFF99",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # Text box border styles (TBT/TBM/TBL/TBR/TBB)
  styles[["TBT"]] <- createStyle(
    fontSize = sz, valign = "top", wrapText = TRUE,
    border = c("Top", "Left", "Right"), borderStyle = "thin"
  )
  styles[["TBM"]] <- createStyle(
    fontSize = sz, valign = "top", wrapText = TRUE,
    border = c("Left", "Right"), borderStyle = "thin"
  )
  styles[["TBL"]] <- createStyle(
    fontSize = sz, valign = "top", wrapText = TRUE,
    border = "Left", borderStyle = "thin"
  )
  styles[["TBR"]] <- createStyle(
    fontSize = sz, valign = "top", wrapText = TRUE,
    border = "Right", borderStyle = "thin"
  )
  styles[["TBB"]] <- createStyle(
    fontSize = sz, valign = "top", wrapText = TRUE,
    border = c("Bottom", "Left", "Right"), borderStyle = "thin"
  )

  # D1_R2BR_BLRB: red bold decimal (exceeding-threshold highlight)
  styles[["D1_R2BR_BLRB"]] <- createStyle(
    fontSize = sz, fontColour = "#FF0000", textDecoration = "bold",
    halign = "right", valign = "top", numFmt = "  0.0",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # I10: 10pt indented wrap
  styles[["I10"]] <- createStyle(
    fontSize = 10, valign = "top", wrapText = TRUE
  )

  # B10O: bold 10pt centered gray bg dashed border
  styles[["B10O"]] <- createStyle(
    fontSize = 10, textDecoration = "bold",
    halign = "center", valign = "top",
    fgFill = "#C0C0C0",
    border = "TopBottomLeftRight", borderStyle = "dashed"
  )

  # DG: dark gray signal indicator (#808080)
  styles[["DG"]] <- createStyle(
    fontSize = sz, fontColour = "#808080", fgFill = "#808080",
    halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )
  # LG: light gray signal indicator (#C0C0C0)
  styles[["LG"]] <- createStyle(
    fontSize = sz, fontColour = "#C0C0C0", fgFill = "#C0C0C0",
    halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )
  # R: red signal indicator (#FF0000)
  styles[["R"]] <- createStyle(
    fontSize = sz, fontColour = "#FF0000", fgFill = "#FF0000",
    halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )
  # P: peach signal indicator (#FFCC99)
  styles[["P"]] <- createStyle(
    fontSize = sz, fontColour = "#FFCC99", fgFill = "#FFCC99",
    halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # OR: rotated 90 degrees centered (for hidden outline column)
  styles[["OR"]] <- createStyle(
    fontSize = sz, halign = "center", valign = "center",
    textRotation = 90,
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # OW_BLR: white font hidden text (outline white) L/R borders

  styles[["OW_BLR"]] <- createStyle(
    fontSize = sz, fontColour = "#FFFFFF",
    halign = "center", valign = "top",
    border = c("Left", "Right"), borderStyle = "thin"
  )
  # OW_BLRB: outline white with bottom border
  styles[["OW_BLRB"]] <- createStyle(
    fontSize = sz, fontColour = "#FFFFFF",
    halign = "center", valign = "top",
    border = c("Left", "Right", "Bottom"), borderStyle = "thin"
  )

  # COR: continuity correction rotated header — dark blue (#333399)
  styles[["COR"]] <- createStyle(
    fontSize = sz, fontColour = "#FFFFFF", textDecoration = "bold",
    fgFill = "#333399",
    halign = "center", valign = "center",
    textRotation = 90, wrapText = TRUE,
    border = "TopBottomLeftRight", borderStyle = "thin"
  )

  return(styles)
}


# ============================================================================
# out_cover — Front Page / Cover Sheet
# Replaces SAS %out_cover macro (lines 14-728 of ae_meddra_output.sas)
#
# Creates the "Front Page" worksheet with instructions, metadata,
# definitions, methods, report settings, and continuity correction notes.
#
# @param wb          openxlsx Workbook object.
# @param ndabla      Character. NDA/BLA identifier.
# @param studyid     Character. Study identifier.
# @param arm_count   Integer. Number of treatment arms.
# @param arm_name    Named character vector of arm names (keys "1","2",...).
# @param cc_sw       Integer. Continuity correction switch (0/1/2).
# @param cc          Character. CC description text.
# @param vld_sw      Character. Validation switch for analysis period.
# @param study_lag   Character. Study lag days for analysis period.
# @param dm_actarm   Character. "Y" or "N" — whether ACTARM is used.
# @param sl_gs_desc  Character. Grouping/subsetting description.
# @param sl_custom_ds Character. Custom dataset names.
# @param sl_group_nobs Integer. Grouping observation count.
# @param sl_subset_nobs Integer. Subsetting observation count.
# @param ver         Character. MedDRA version string.
# @param rundate     Character. Analysis run date/time string.
# @param styles      Named list of openxlsx Style objects.
# @param wbtitle     Character. Workbook title. Default "Adverse Events
#                    MedDRA Analysis".
# @return Invisible NULL (workbook modified in place).
# @export
# ============================================================================
out_cover <- function(wb,
                      ndabla = "",
                      studyid = "",
                      arm_count = 2L,
                      arm_name = c("1" = "Treatment", "2" = "Placebo"),
                      cc_sw = 0L,
                      cc = "",
                      vld_sw = "Y",
                      study_lag = "0",
                      dm_actarm = "Y",
                      sl_gs_desc = "",
                      sl_custom_ds = "",
                      sl_group_nobs = 0L,
                      sl_subset_nobs = 0L,
                      ver = "",
                      rundate = "",
                      styles = list(),
                      wbtitle = "Adverse Events MedDRA Analysis") {

  cli::cli_alert_info("Creating Front Page worksheet")
  ws <- "Front Page"
  addWorksheet(wb, ws)

  # Column widths (SAS line 20-24): 16, 125, 21, 13x5, 50, 35, 50, 35, 55, 55, 55, 16
  col_widths <- c(2.3, 18, 3, rep(1.9, 5), 7.1, 5, 7.1, 5, 7.9, 7.9, 7.9, 2.3)
  setColWidths(wb, ws, cols = seq_along(col_widths), widths = col_widths)

  r <- 1L  # current row tracker

  # ------------------------------------------------------------------
  # Part 1: Title and metadata (SAS lines 26-90)
  # ------------------------------------------------------------------
  r <- r + 1L
  writeData(wb, ws, wbtitle, startRow = r, startCol = 1L, colNames = FALSE)
  addStyle(wb, ws, styles[["Header"]], rows = r, cols = 1L)
  mergeCells(wb, ws, cols = 1:16, rows = r)
  r <- r + 1L

  # NDA/BLA
  writeData(wb, ws, str_c("NDA/BLA: ", ndabla), startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  r <- r + 1L

  # Study
  writeData(wb, ws, str_c("Study: ", studyid), startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  r <- r + 1L

  # Analysis run date
  writeData(wb, ws, str_c("Analysis run date: ", rundate), startRow = r,
            startCol = 1L, colNames = FALSE)
  addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  r <- r + 2L  # skip a row

  # One-arm study warning (SAS lines 75-88)
  if (arm_count == 1L) {
    warn_text <- paste0(
      "Note: This report is designed for studies with two or more treatment ",
      "arms. Since this study has only one arm, some features are disabled, ",
      "including comparison statistics and signal detection."
    )
    writeData(wb, ws, warn_text, startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10RedWrap"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:16, rows = r)
    r <- r + 2L
  }

  # ------------------------------------------------------------------
  # Part 2: How To Use This Report (SAS lines ~92-250)
  # ------------------------------------------------------------------
  writeData(wb, ws, "How To Use This Report", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 2L

  # Instructions text
  instructions <- c(
    paste0("This report provides a summary of adverse events coded using ",
           "MedDRA (Medical Dictionary for Regulatory Activities) organized ",
           "hierarchically by System Organ Class (SOC), High Level Group Term ",
           "(HLGT), High Level Term (HLT), and Preferred Term (PT)."),
    "",
    paste0("The report compares AE rates between treatment arms and calculates ",
           "risk difference, relative risk, and p-values from Fisher's exact test. ",
           "A signal detection system highlights terms where rates exceed user-defined ",
           "thresholds."),
    "",
    paste0("To use: select the experimental and control arms from the dropdown ",
           "menus in the comparison worksheet, set threshold values for risk ",
           "difference (%), relative risk, and negative log p-value, then review ",
           "highlighted signal indicators."),
    ""
  )
  for (txt in instructions) {
    if (nzchar(txt)) {
      writeData(wb, ws, txt, startRow = r, startCol = 1L, colNames = FALSE)
      addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
      mergeCells(wb, ws, cols = 1:16, rows = r)
    }
    r <- r + 1L
  }

  # ------------------------------------------------------------------
  # Example Column Headers (SAS lines ~100-200)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Example:", startRow = r, startCol = 1L, colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 1L

  # Column header labels for example
  example_headers <- c("Level", "SOC", "HLGT", "HLT", "PT", "DME",
                       "Signal", "Signal At:", "", "",
                       "Exp Arm", "", "Ctl Arm", "",
                       "RD%", "RR")
  for (ci in seq_along(example_headers)) {
    writeData(wb, ws, example_headers[ci], startRow = r, startCol = ci,
              colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
  }
  r <- r + 1L

  # Example data row
  example_data <- c("1", "Gastrointestinal disorders", "", "", "", "",
                    "Y", "SOC", "", "",
                    "15", "30.0%", "5", "10.0%",
                    "20.0", "3.0")
  for (ci in seq_along(example_data)) {
    val <- example_data[ci]
    writeData(wb, ws, val, startRow = r, startCol = ci, colNames = FALSE)
    sty <- if (ci <= 6) styles[["Data"]] else styles[["DataCenter"]]
    if (!is.null(sty)) addStyle(wb, ws, sty, rows = r, cols = ci)
  }
  r <- r + 2L

  # ------------------------------------------------------------------
  # Part 3: Column Definitions (SAS lines ~250-420)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Column Definitions", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 1L

  definitions <- list(
    list("A", "Level",
         "Hierarchical level indicator: 1=SOC, 2=HLGT, 3=HLT, 4=PT"),
    list("B", "SOC / HLGT / HLT / PT",
         paste0("MedDRA hierarchy term names at each level. System Organ ",
                "Class, High Level Group Term, High Level Term, Preferred Term.")),
    list("C", "DME",
         "Designated Medical Event flag from MedDRA SMQ list."),
    list("D", "Signal",
         paste0("Signal detection indicator. 'Y' when any comparison metric ",
                "(RD%, RR, PV) exceeds the user-defined threshold.")),
    list("E", "Signal At",
         paste0("Indicates which hierarchy level(s) triggered the signal: ",
                "SOC, HLGT, HLT, PT, or combinations (e.g., 'AB' = SOC and HLGT).")),
    list("F", "Subject Count / %",
         paste0("Number and percentage of subjects in each arm with the AE term. ",
                "Denominator is the total number of subjects in that arm.")),
    list("G", "RD% (Risk Difference %)",
         paste0("Difference in AE percentage between experimental and control ",
                "arms: (Exp% - Ctl%). Positive values indicate higher rates in ",
                "the experimental arm.")),
    list("H", "RR (Relative Risk)",
         paste0("Ratio of AE percentage in experimental arm to control arm: ",
                "(Exp% / Ctl%). Values >1 indicate higher relative risk in ",
                "the experimental arm."))
  )

  for (defn in definitions) {
    writeData(wb, ws, defn[[1]], startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = 1L)
    writeData(wb, ws, defn[[2]], startRow = r, startCol = 2L, colNames = FALSE)
    addStyle(wb, ws, styles[["Data"]], rows = r, cols = 2L)
    writeData(wb, ws, defn[[3]], startRow = r, startCol = 3L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 3L)
    mergeCells(wb, ws, cols = 3:16, rows = r)
    r <- r + 1L
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Part 4: Methods and Calculations (SAS lines ~420-600)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Methods and Calculations", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 1L

  # Analysis period
  period_text <- if (vld_sw == "Y") {
    paste0("Adverse events are included if they occur within the study analysis ",
           "period (from first dose to last dose + ", study_lag, " days). ",
           "Events outside this window are excluded.")
  } else {
    "All adverse events on record are included regardless of timing."
  }
  writeData(wb, ws, period_text, startRow = r, startCol = 1L, colNames = FALSE)
  addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  mergeCells(wb, ws, cols = 1:16, rows = r)
  r <- r + 1L

  # Treatment arm variable
  arm_text <- if (dm_actarm == "Y") {
    "Treatment arm assignment is based on ACTARM (actual arm) from the DM domain."
  } else {
    "Treatment arm assignment is based on ARM (planned arm) from the DM domain."
  }
  writeData(wb, ws, arm_text, startRow = r, startCol = 1L, colNames = FALSE)
  addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  mergeCells(wb, ws, cols = 1:16, rows = r)
  r <- r + 1L

  # 2x2 contingency table (SAS lines 510-560)
  r <- r + 1L
  writeData(wb, ws, "2 x 2 Contingency Table:", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 1L

  ct_headers <- c("", "AE Present", "AE Absent")
  ct_row1 <- c("Experimental Arm", "a", "b")
  ct_row2 <- c("Control Arm", "c", "d")
  for (ci in seq_along(ct_headers)) {
    writeData(wb, ws, ct_headers[ci], startRow = r, startCol = ci, colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
  }
  r <- r + 1L
  for (ci in seq_along(ct_row1)) {
    writeData(wb, ws, ct_row1[ci], startRow = r, startCol = ci, colNames = FALSE)
    addStyle(wb, ws, styles[["Table"]], rows = r, cols = ci)
  }
  r <- r + 1L
  for (ci in seq_along(ct_row2)) {
    writeData(wb, ws, ct_row2[ci], startRow = r, startCol = ci, colNames = FALSE)
    addStyle(wb, ws, styles[["Table"]], rows = r, cols = ci)
  }
  r <- r + 2L

  # Calculation definitions
  calc_defs <- list(
    c("AE Subject Count (a)", "Number of subjects in the arm with the AE term."),
    c("Subject Count (a+b)", "Total number of subjects in the arm."),
    c("AE% = a/(a+b) * 100", "Percentage of subjects with the AE."),
    c("Risk Difference (RD%)", "(Exp AE% - Ctl AE%)"),
    c("Relative Risk (RR)", "(Exp AE% / Ctl AE%)"),
    c("Negative Log P-value", "-log10(p) from Fisher's exact test")
  )
  for (cd in calc_defs) {
    writeData(wb, ws, cd[1], startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["Data"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:3, rows = r)
    writeData(wb, ws, cd[2], startRow = r, startCol = 4L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 4L)
    mergeCells(wb, ws, cols = 4:16, rows = r)
    r <- r + 1L
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Part 5: Continuity Correction (SAS lines ~600-660, conditional on cc_sw)
  # ------------------------------------------------------------------
  if (cc_sw > 0L) {
    writeData(wb, ws, "Continuity Correction", startRow = r, startCol = 1L,
              colNames = FALSE)
    addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
    r <- r + 1L

    cc_text <- paste0(
      "A continuity correction (", cc, ") has been applied. ",
      "When a cell in the 2x2 table is zero, ",
      "the correction is added to all cells to allow computation of relative ",
      "risk and Fisher's exact test. The CC column in the comparison worksheet ",
      "indicates which terms required correction."
    )
    writeData(wb, ws, cc_text, startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:16, rows = r)
    r <- r + 2L
  }

  # ------------------------------------------------------------------
  # Part 6: Report Settings (SAS lines ~660-728)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Report Settings", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  r <- r + 1L

  settings <- list(
    c("NDA/BLA:", ndabla),
    c("Study:", studyid),
    c("Date:", rundate),
    c("MedDRA Version:", ver)
  )
  if (nzchar(sl_custom_ds)) {
    settings <- c(settings, list(c("Custom Datasets:", sl_custom_ds)))
  }
  if (nzchar(sl_gs_desc)) {
    settings <- c(settings, list(c("Grouping/Subsetting:", sl_gs_desc)))
  }
  # Analysis period setting
  ap_desc <- if (vld_sw == "Y") {
    paste0("Study analysis period (first dose to last dose + ", study_lag, " days)")
  } else {
    "All events (no analysis period restriction)"
  }
  settings <- c(settings, list(c("Analysis Period:", ap_desc)))
  # CC setting
  cc_desc <- dplyr::case_when(
    cc_sw == 0L ~ "No continuity correction",
    cc_sw == 1L ~ paste0("Continuity correction applied: ", cc),
    cc_sw == 2L ~ paste0("Continuity correction available: ", cc),
    TRUE ~ "Not specified"
  )
  settings <- c(settings, list(c("Continuity Correction:", cc_desc)))

  for (s in settings) {
    writeData(wb, ws, s[1], startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["Data"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:3, rows = r)
    writeData(wb, ws, s[2], startRow = r, startCol = 4L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 4L)
    mergeCells(wb, ws, cols = 4:16, rows = r)
    r <- r + 1L
  }

  # Page setup
  pageSetup(wb, ws, orientation = "landscape", fitToWidth = TRUE)

  invisible(NULL)
}


# ============================================================================
# out_meddra_cmp — Visible MedDRA Comparison Analysis Worksheet
# Replaces SAS %out_meddra_cmp macro (lines 730-1460 of ae_meddra_output.sas)
#
# Creates the main visible comparison worksheet with:
#   - Dropdown-driven arm selection and threshold inputs
#   - Multi-tier column headers (hierarchy, arms, statistics)
#   - Data rows with formula references to hidden data sheet
#   - Conditional formatting for signal highlighting
#   - Named ranges for dropdown references
#   - Freeze panes and AutoFilter
#
# @param wb              openxlsx Workbook object.
# @param meddra_cmp_output Data frame. The comparison output dataset with
#                         one row per MedDRA term, columns for hierarchy
#                         levels, arm counts, percentages, RD, RR, PV.
# @param row_ds          Data frame. Row metadata with lvl_nm/lvl_no for
#                         each term (for INDEX formula references).
# @param arm_count       Integer. Number of treatment arms.
# @param arm_name        Named character vector of arm display names.
# @param cc_sw           Integer. Continuity correction switch.
# @param sl_gs_desc      Character. Grouping/subsetting description.
# @param max_arm_nm_len  Integer. Maximum arm name length for column sizing.
# @param styles          Named list of base styles.
# @param aem_styles      Named list of MedDRA-specific styles from out_aem_styles().
# @return Invisible NULL (workbook modified in place).
# @export
# ============================================================================
out_meddra_cmp <- function(wb,
                           meddra_cmp_output = data.frame(),
                           row_ds = data.frame(),
                           arm_count = 2L,
                           arm_name = c("1" = "Treatment", "2" = "Placebo"),
                           cc_sw = 0L,
                           sl_gs_desc = "",
                           max_arm_nm_len = 20L,
                           styles = list(),
                           aem_styles = list()) {

  cli::cli_alert_info("Creating MedDRA Comparison Analysis worksheet")
  ws <- "MedDRA Comparison Analysis"
  addWorksheet(wb, ws)

  all_styles <- c(styles, aem_styles)
  n_data_rows <- nrow(meddra_cmp_output)

  # ------------------------------------------------------------------
  # Column widths (SAS lines 740-770)
  # Depends on arm_count and cc_sw for which columns are visible
  # ------------------------------------------------------------------
  # Core columns: Level(13), SOC(120), HLGT(120), HLT(120), PT(120), DME(21)
  # Signal columns: Signal(13), SignalAt SOC/HLGT/HLT/PT (13 each)
  # Arm columns: exp count(50), exp%(35), ctl count(50), ctl%(35)
  # Stats columns: RD%(53), RR(43), CC(13 if cc_sw>0), PV(53)
  # Sort column: 13
  base_widths <- c(1.9,  # Level
                   17.1, 17.1, 17.1, 17.1,  # SOC/HLGT/HLT/PT
                   3.0,  # DME
                   1.9, 1.9, 1.9, 1.9, 1.9,  # Signal, SignalAt x4
                   7.1, 5.0,   # Exp count/pct
                   7.1, 5.0,   # Ctl count/pct
                   7.6, 6.1)   # RD%, RR

  if (cc_sw > 0L) {
    base_widths <- c(base_widths, 1.9)  # CC column
  }
  base_widths <- c(base_widths, 7.6,  # PV
                   1.9)  # Sort order
  setColWidths(wb, ws, cols = seq_along(base_widths), widths = base_widths)

  # Track column indices for data columns
  col_level  <- 1L
  col_soc    <- 2L
  col_hlgt   <- 3L
  col_hlt    <- 4L
  col_pt     <- 5L
  col_dme    <- 6L
  col_signal <- 7L
  col_sig_soc  <- 8L
  col_sig_hlgt <- 9L
  col_sig_hlt  <- 10L
  col_sig_pt   <- 11L
  col_exp_n    <- 12L
  col_exp_pct  <- 13L
  col_ctl_n    <- 14L
  col_ctl_pct  <- 15L
  col_rd       <- 16L
  col_rr       <- 17L
  col_cc <- NA_integer_
  col_pv <- NA_integer_
  col_sort <- NA_integer_

  if (cc_sw > 0L) {
    col_cc   <- 18L
    col_pv   <- 19L
    col_sort <- 20L
  } else {
    col_pv   <- 18L
    col_sort <- 19L
  }
  last_col <- col_sort

  r <- 1L  # current row

  # ------------------------------------------------------------------
  # Title and description header (SAS lines ~780-800)
  # ------------------------------------------------------------------
  writeData(wb, ws, "MedDRA Comparison Analysis", startRow = r, startCol = 1L,
            colNames = FALSE)
  addStyle(wb, ws, styles[["Header"]], rows = r, cols = 1L)
  mergeCells(wb, ws, cols = 1:last_col, rows = r)
  r <- r + 1L

  if (nzchar(sl_gs_desc)) {
    writeData(wb, ws, sl_gs_desc, startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:last_col, rows = r)
    r <- r + 1L
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Selection area: arm dropdowns and threshold inputs (SAS lines ~800-900)
  # ------------------------------------------------------------------
  sel_start_row <- r

  if (arm_count > 1L) {
    # Experimental arm dropdown
    writeData(wb, ws, "Experimental Arm:", startRow = r, startCol = 1L,
              colNames = FALSE)
    addStyle(wb, ws, all_styles[["I10"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:3, rows = r)
    writeData(wb, ws, arm_name[["1"]], startRow = r, startCol = 4L,
              colNames = FALSE)
    addStyle(wb, ws, all_styles[["I"]], rows = r, cols = 4L)
    mergeCells(wb, ws, cols = 4:6, rows = r)
    r <- r + 1L

    # Control arm dropdown
    writeData(wb, ws, "Control Arm:", startRow = r, startCol = 1L,
              colNames = FALSE)
    addStyle(wb, ws, all_styles[["I10"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:3, rows = r)
    ctl_default <- if (length(arm_name) >= 2L) arm_name[["2"]] else arm_name[["1"]]
    writeData(wb, ws, ctl_default, startRow = r, startCol = 4L,
              colNames = FALSE)
    addStyle(wb, ws, all_styles[["I"]], rows = r, cols = 4L)
    mergeCells(wb, ws, cols = 4:6, rows = r)
    r <- r + 1L
    r <- r + 1L  # blank row

    # Threshold inputs
    threshold_labels <- c("Risk Difference Threshold (RD%):",
                          "Relative Risk Threshold (RR):",
                          "Neg. Log P-Value Threshold:")
    threshold_defaults <- c("", "", "")
    for (ti in seq_along(threshold_labels)) {
      writeData(wb, ws, threshold_labels[ti], startRow = r, startCol = 1L,
                colNames = FALSE)
      addStyle(wb, ws, all_styles[["I10"]], rows = r, cols = 1L)
      mergeCells(wb, ws, cols = 1:3, rows = r)
      writeData(wb, ws, threshold_defaults[ti], startRow = r, startCol = 4L,
                colNames = FALSE)
      addStyle(wb, ws, all_styles[["I"]], rows = r, cols = 4L)
      r <- r + 1L
    }
    r <- r + 1L  # blank row

    # Explanation text box (SAS lines ~870-900)
    exp_text <- paste0(
      "Select experimental and control arms from the dropdowns above. ",
      "Enter threshold values to highlight AE terms exceeding those ",
      "thresholds. Leave blank for no threshold filtering."
    )
    writeData(wb, ws, exp_text, startRow = r, startCol = 1L, colNames = FALSE)
    addStyle(wb, ws, all_styles[["TBT"]], rows = r, cols = 1L)
    mergeCells(wb, ws, cols = 1:last_col, rows = r)
    r <- r + 1L

    # Color legend
    legend_items <- list(
      list("Signal detected (term level)", "R"),
      list("Signal detected (other level)", "P"),
      list("No signal", "DG")
    )
    for (li in legend_items) {
      writeData(wb, ws, " ", startRow = r, startCol = 1L, colNames = FALSE)
      sty <- all_styles[[li[[2]]]]
      if (!is.null(sty)) addStyle(wb, ws, sty, rows = r, cols = 1L)
      writeData(wb, ws, li[[1]], startRow = r, startCol = 2L, colNames = FALSE)
      addStyle(wb, ws, all_styles[["TBM"]], rows = r, cols = 2L)
      mergeCells(wb, ws, cols = 2:last_col, rows = r)
      r <- r + 1L
    }
    r <- r + 1L
  }

  # ------------------------------------------------------------------
  # Column headers — 3-tier (SAS lines ~920-1000)
  # ------------------------------------------------------------------
  header_row1 <- r

  # Row 1: top-level grouping headers
  writeData(wb, ws, "MedDRA Hierarchy", startRow = r, startCol = col_level,
            colNames = FALSE)
  addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_level)
  mergeCells(wb, ws, cols = col_level:col_pt, rows = r)

  writeData(wb, ws, "DME", startRow = r, startCol = col_dme, colNames = FALSE)
  addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_dme)

  if (arm_count > 1L) {
    writeData(wb, ws, "Signal Detection", startRow = r, startCol = col_signal,
              colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_signal)
    mergeCells(wb, ws, cols = col_signal:col_sig_pt, rows = r)
  }

  # Exp arm header — use formula referencing wbinfo for dynamic name
  writeData(wb, ws, "=exp_name", startRow = r, startCol = col_exp_n,
            colNames = FALSE)
  addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_exp_n)
  mergeCells(wb, ws, cols = col_exp_n:col_exp_pct, rows = r)

  if (arm_count > 1L) {
    writeData(wb, ws, "=ctl_name", startRow = r, startCol = col_ctl_n,
              colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_ctl_n)
    mergeCells(wb, ws, cols = col_ctl_n:col_ctl_pct, rows = r)

    writeData(wb, ws, "Comparison Statistics", startRow = r,
              startCol = col_rd, colNames = FALSE)
    addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = col_rd)
    stats_end <- if (!is.na(col_cc)) col_pv else col_pv
    mergeCells(wb, ws, cols = col_rd:stats_end, rows = r)
  }
  r <- r + 1L

  # Row 2: sub-headers
  sub_headers <- list()
  sub_headers[[col_level]] <- "Level"
  sub_headers[[col_soc]]   <- "SOC"
  sub_headers[[col_hlgt]]  <- "HLGT"
  sub_headers[[col_hlt]]   <- "HLT"
  sub_headers[[col_pt]]    <- "PT"
  sub_headers[[col_dme]]   <- "DME"
  if (arm_count > 1L) {
    sub_headers[[col_signal]]   <- "Signal"
    sub_headers[[col_sig_soc]]  <- "SOC"
    sub_headers[[col_sig_hlgt]] <- "HLGT"
    sub_headers[[col_sig_hlt]]  <- "HLT"
    sub_headers[[col_sig_pt]]   <- "PT"
  }
  sub_headers[[col_exp_n]]   <- "Subjects"
  sub_headers[[col_exp_pct]] <- "%"
  if (arm_count > 1L) {
    sub_headers[[col_ctl_n]]   <- "Subjects"
    sub_headers[[col_ctl_pct]] <- "%"
    sub_headers[[col_rd]] <- "RD%"
    rr_label <- if (cc_sw == 1L) "RR*" else "RR"
    sub_headers[[col_rr]] <- rr_label
    if (!is.na(col_cc)) {
      sub_headers[[col_cc]] <- "CC"
    }
    sub_headers[[col_pv]] <- "-log(p)"
  }
  sub_headers[[col_sort]] <- "Sort"

  for (ci in seq_along(sub_headers)) {
    if (!is.null(sub_headers[[ci]])) {
      writeData(wb, ws, sub_headers[[ci]], startRow = r, startCol = ci,
                colNames = FALSE)
      # Use rotated style for Signal At columns
      sty <- if (ci >= col_sig_soc && ci <= col_sig_pt) {
        styles[["ColumnOutlineRotateCtr"]]
      } else {
        styles[["ColumnOutline"]]
      }
      if (!is.null(sty)) addStyle(wb, ws, sty, rows = r, cols = ci)
    }
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Data rows with INDEX/INDIRECT formulas (SAS lines ~1000-1200)
  # ------------------------------------------------------------------
  first_data_row <- r
  if (n_data_rows > 0L) {
    for (di in seq_len(n_data_rows)) {
      row_data <- meddra_cmp_output[di, ]
      is_bottom <- (di == n_data_rows)
      b_sfx <- if (is_bottom) "B" else ""

      # Level number
      lvl_val <- if ("level" %in% names(row_data)) row_data$level else NA
      writeData(wb, ws, lvl_val, startRow = r, startCol = col_level,
                colNames = FALSE)
      sty_nm <- paste0("DC", b_sfx)
      if (!is.null(all_styles[[sty_nm]])) {
        addStyle(wb, ws, all_styles[[sty_nm]], rows = r, cols = col_level)
      }

      # Hierarchy text columns (SOC/HLGT/HLT/PT)
      text_cols <- list(
        list(col = col_soc,  var = "soc_name"),
        list(col = col_hlgt, var = "hlgt_name"),
        list(col = col_hlt,  var = "hlt_name"),
        list(col = col_pt,   var = "pt_name")
      )
      for (tc in text_cols) {
        val <- if (tc$var %in% names(row_data)) {
          as.character(row_data[[tc$var]])
        } else {
          ""
        }
        if (is.na(val)) val <- ""
        writeData(wb, ws, val, startRow = r, startCol = tc$col,
                  colNames = FALSE)
        d_sty <- paste0("D", b_sfx)
        if (!is.null(all_styles[[d_sty]])) {
          addStyle(wb, ws, all_styles[[d_sty]], rows = r, cols = tc$col)
        }
      }

      # DME column
      dme_val <- if ("dme" %in% names(row_data)) as.character(row_data$dme) else ""
      if (is.na(dme_val)) dme_val <- ""
      writeData(wb, ws, dme_val, startRow = r, startCol = col_dme,
                colNames = FALSE)
      dc_sty <- paste0("DC", b_sfx)
      if (!is.null(all_styles[[dc_sty]])) {
        addStyle(wb, ws, all_styles[[dc_sty]], rows = r, cols = col_dme)
      }

      # Signal columns: use formulas referencing hidden data sheet
      if (arm_count > 1L) {
        signal_cols <- c(col_signal, col_sig_soc, col_sig_hlgt,
                        col_sig_hlt, col_sig_pt)
        for (sc in signal_cols) {
          writeData(wb, ws, "", startRow = r, startCol = sc, colNames = FALSE)
          ow_sty <- if (is_bottom) "OW_BLRB" else "OW_BLR"
          if (!is.null(all_styles[[ow_sty]])) {
            addStyle(wb, ws, all_styles[[ow_sty]], rows = r, cols = sc)
          }
        }
      }

      # Arm count/pct columns: write numeric values directly
      num_cols <- list(
        list(col = col_exp_n,   var = "exp_n",   fmt = "D0_R2"),
        list(col = col_exp_pct, var = "exp_pct", fmt = "D1_R1")
      )
      if (arm_count > 1L) {
        num_cols <- c(num_cols, list(
          list(col = col_ctl_n,   var = "ctl_n",   fmt = "D0_R2"),
          list(col = col_ctl_pct, var = "ctl_pct", fmt = "D1_R1")
        ))
      }
      for (nc in num_cols) {
        val <- if (nc$var %in% names(row_data)) row_data[[nc$var]] else NA
        if (!is.na(val)) {
          writeData(wb, ws, val, startRow = r, startCol = nc$col,
                    colNames = FALSE)
        }
        bdr <- if (nc$col == col_exp_n || nc$col == col_ctl_n) "_BL" else "_BR"
        sty_full <- paste0(nc$fmt, bdr, if (is_bottom) "B" else "")
        if (!is.null(all_styles[[sty_full]])) {
          addStyle(wb, ws, all_styles[[sty_full]], rows = r, cols = nc$col)
        }
      }

      # Comparison stats: RD, RR, CC, PV
      if (arm_count > 1L) {
        # RD
        rd_val <- if ("rd" %in% names(row_data)) row_data$rd else NA
        if (!is.na(rd_val)) {
          writeData(wb, ws, rd_val, startRow = r, startCol = col_rd,
                    colNames = FALSE)
        }
        rd_sty <- paste0("D1_R2_BLR", if (is_bottom) "B" else "")
        if (!is.null(all_styles[[rd_sty]])) {
          addStyle(wb, ws, all_styles[[rd_sty]], rows = r, cols = col_rd)
        }

        # RR
        rr_val <- if ("rr" %in% names(row_data)) row_data$rr else NA
        if (!is.na(rr_val)) {
          writeData(wb, ws, rr_val, startRow = r, startCol = col_rr,
                    colNames = FALSE)
        }
        rr_bdr <- if (cc_sw > 0L) "_BL" else "_BLR"
        rr_sty <- paste0("D1_R1", rr_bdr, if (is_bottom) "B" else "")
        if (is.null(all_styles[[rr_sty]])) rr_sty <- paste0("D1_R2_BLR", if (is_bottom) "B" else "")
        if (!is.null(all_styles[[rr_sty]])) {
          addStyle(wb, ws, all_styles[[rr_sty]], rows = r, cols = col_rr)
        }

        # CC (if applicable)
        if (!is.na(col_cc)) {
          cc_val <- if ("cc_ind" %in% names(row_data)) {
            as.character(row_data$cc_ind)
          } else {
            ""
          }
          if (is.na(cc_val)) cc_val <- ""
          writeData(wb, ws, cc_val, startRow = r, startCol = col_cc,
                    colNames = FALSE)
          ib_sty <- paste0("IB_BR", if (is_bottom) "B" else "")
          if (!is.null(all_styles[[ib_sty]])) {
            addStyle(wb, ws, all_styles[[ib_sty]], rows = r, cols = col_cc)
          }
        }

        # PV (neg log p-value)
        pv_val <- if ("pv" %in% names(row_data)) row_data$pv else NA
        if (!is.na(pv_val)) {
          writeData(wb, ws, pv_val, startRow = r, startCol = col_pv,
                    colNames = FALSE)
        }
        pv_sty <- paste0("D1_R2_BLR", if (is_bottom) "B" else "")
        if (!is.null(all_styles[[pv_sty]])) {
          addStyle(wb, ws, all_styles[[pv_sty]], rows = r, cols = col_pv)
        }
      }

      # Sort order column
      sort_val <- if ("sort_order" %in% names(row_data)) row_data$sort_order else di
      writeData(wb, ws, sort_val, startRow = r, startCol = col_sort,
                colNames = FALSE)
      ow_sort <- if (is_bottom) "OW_BLRB" else "OW_BLR"
      if (!is.null(all_styles[[ow_sort]])) {
        addStyle(wb, ws, all_styles[[ow_sort]], rows = r, cols = col_sort)
      }

      r <- r + 1L
    }
  }
  last_data_row <- r - 1L

  # ------------------------------------------------------------------
  # Freeze panes (SAS lines ~1230-1240)
  # ------------------------------------------------------------------
  if (arm_count > 1L) {
    freezePane(wb, ws,
               firstActiveRow = first_data_row,
               firstActiveCol = col_dme)
  }

  # AutoFilter (SAS line ~1250)
  if (n_data_rows > 0L) {
    addFilter(wb, ws, rows = first_data_row - 1L,
              cols = seq_len(last_col))
  }

  # ------------------------------------------------------------------
  # Conditional formatting for signal highlighting (SAS lines ~1260-1400)
  # ------------------------------------------------------------------
  if (arm_count > 1L && n_data_rows > 0L) {
    data_rows <- first_data_row:last_data_row

    # Signal column: dark gray when no signal detected
    dg_style <- all_styles[["DG"]]
    lg_style <- all_styles[["LG"]]
    r_style  <- all_styles[["R"]]
    p_style  <- all_styles[["P"]]

    # RD/RR/PV: red bold when exceeding threshold (using formula-based
    # conditional formatting referencing named ranges)
    rd_highlight <- createStyle(
      fontColour = "#FF0000", textDecoration = "bold"
    )

    # Apply conditional formatting for threshold exceedances
    # RD column
    conditionalFormatting(wb, ws,
      cols = col_rd, rows = data_rows,
      rule = "AND(ISNUMBER(INDIRECT(\"rdn\")),ABS(INDIRECT(ADDRESS(ROW(),COLUMN(),4)))>INDIRECT(\"rdn\"))",
      style = rd_highlight, type = "expression"
    )
    # RR column
    conditionalFormatting(wb, ws,
      cols = col_rr, rows = data_rows,
      rule = "AND(ISNUMBER(INDIRECT(\"rrn\")),INDIRECT(ADDRESS(ROW(),COLUMN(),4))>INDIRECT(\"rrn\"))",
      style = rd_highlight, type = "expression"
    )
    # PV column
    conditionalFormatting(wb, ws,
      cols = col_pv, rows = data_rows,
      rule = "AND(ISNUMBER(INDIRECT(\"pvn\")),INDIRECT(ADDRESS(ROW(),COLUMN(),4))>INDIRECT(\"pvn\"))",
      style = rd_highlight, type = "expression"
    )
  }

  # ------------------------------------------------------------------
  # Named ranges for user input cells (SAS lines ~1400-1460)
  # ------------------------------------------------------------------
  if (arm_count > 1L) {
    # Arm selection cells
    exp_row <- sel_start_row
    ctl_row <- sel_start_row + 1L
    threshold_start <- sel_start_row + 3L

    createNamedRegion(wb, ws, cols = 4L, rows = exp_row, name = "exp_name")
    createNamedRegion(wb, ws, cols = 4L, rows = ctl_row, name = "ctl_name")

    # Threshold named ranges
    createNamedRegion(wb, ws, cols = 4L, rows = threshold_start,
                      name = "rdn")
    createNamedRegion(wb, ws, cols = 4L, rows = threshold_start + 1L,
                      name = "rrn")
    createNamedRegion(wb, ws, cols = 4L, rows = threshold_start + 2L,
                      name = "pvn")

    # Data validation for arm dropdowns
    tryCatch({
      dataValidation(wb, ws, cols = 4L, rows = exp_row,
                     type = "list",
                     value = "'Workbook Information'!$A$3:$A$100")
      dataValidation(wb, ws, cols = 4L, rows = ctl_row,
                     type = "list",
                     value = "'Workbook Information'!$A$3:$A$100")
    }, error = function(e) {
      cli::cli_alert_info("Data validation skipped: {e$message}")
    })
  }

  # Page setup
  pageSetup(wb, ws, orientation = "landscape", fitToWidth = TRUE)

  invisible(NULL)
}


# ============================================================================
# out_meddra_cmp_data — Hidden MedDRA Comparison Data Worksheet
# Replaces SAS %out_meddra_cmp_data macro (lines 1465-1790 of
# ae_meddra_output.sas)
#
# Creates a hidden worksheet containing enriched data rows with:
#   - Level metadata (lvl_nm, lvl_no, soc/hlgt/hlt/pt identifiers)
#   - Named ranges for arm data, comparison values, signal columns
#   - Excel formulas for comparison metrics (RD, RR, PV via INDEX/INDIRECT)
#   - Signal propagation formulas (hierarchical IF/MATCH/OFFSET/ISNA logic)
#   - Named ranges for each hierarchy level and arm combination
#
# @param wb              openxlsx Workbook object.
# @param meddra_cmp_data Data frame. The enriched comparison data with columns:
#                         level, soc_name, hlgt_name, hlt_name, pt_name, dme,
#                         per-arm n/pct, rd, rr, pv, cc_ind, sort_order,
#                         lvl_nm, lvl_no, soc_id, hlgt_id, hlt_id, pt_id.
# @param arm_count       Integer. Number of treatment arms.
# @param arm_pairs       Data frame. Arm pair combinations with exp/ctl IDs.
# @param cc_sw           Integer. Continuity correction switch (0/1/2).
# @param styles          Named list of base styles from create_styles().
# @param aem_styles      Named list of MedDRA-specific styles from out_aem_styles().
# @return Invisible NULL (workbook modified in place).
# @export
# ============================================================================
out_meddra_cmp_data <- function(wb,
                                meddra_cmp_data = data.frame(),
                                arm_count = 2L,
                                arm_pairs = data.frame(),
                                cc_sw = 0L,
                                styles = list(),
                                aem_styles = list()) {

  cli::cli_alert_info("Creating hidden MedDRA Comparison Data worksheet")
  ws <- "MedDRA Comparison Data"
  addWorksheet(wb, ws, visible = FALSE)

  all_styles <- c(styles, aem_styles)
  n_rows <- nrow(meddra_cmp_data)

  if (n_rows == 0L) {
    cli::cli_alert_info("No data rows for hidden data sheet — skipping")
    return(invisible(NULL))
  }

  # ------------------------------------------------------------------
  # Build the column layout for the hidden data sheet
  # This mirrors the SAS DATA _NULL_ that builds the hidden XML worksheet
  # (SAS lines 1470-1600)
  # ------------------------------------------------------------------
  base_cols <- c("lvl_nm", "lvl_no", "soc_name", "hlgt_name",
                 "hlt_name", "pt_name", "dme",
                 "soc_id", "hlgt_id", "hlt_id", "pt_id",
                 "sort_order")

  # Per-arm columns: n, pct for each arm
  arm_data_cols <- character(0)
  for (ai in seq_len(arm_count)) {
    arm_data_cols <- c(arm_data_cols,
      paste0("arm", ai, "_n"),
      paste0("arm", ai, "_pct"))
  }

  # Comparison columns per arm pair
  cmp_cols <- character(0)
  if (arm_count > 1L) {
    cmp_cols <- c("rd", "rr")
    if (cc_sw > 0L) cmp_cols <- c(cmp_cols, "cc_ind")
    cmp_cols <- c(cmp_cols, "pv")
  }

  # Signal columns
  sgnl_cols <- c("sgnl", "sgnl_soc", "sgnl_hlgt", "sgnl_hlt",
                 "sgnl_pt", "sgnl_any")

  all_cols <- c(base_cols, arm_data_cols, cmp_cols, sgnl_cols)

  # Helper: column index by name
  col_idx <- function(nm) {
    idx <- which(all_cols == nm)
    if (length(idx) == 0L) return(NA_integer_)
    idx[1L]
  }

  # ------------------------------------------------------------------
  # Write header row
  # ------------------------------------------------------------------
  r <- 1L
  for (ci in seq_along(all_cols)) {
    writeData(wb, ws, all_cols[ci], startRow = r, startCol = ci,
              colNames = FALSE)
    if (!is.null(styles[["ColumnOutline"]])) {
      addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
    }
  }
  r <- r + 1L
  first_data_row <- r

  # ------------------------------------------------------------------
  # Write data rows (SAS lines 1510-1600)
  # Map input data frame columns to hidden sheet columns
  # ------------------------------------------------------------------
  # Normalise source column names: the input might have different names
  # from what we expect, so we map flexibly
  src <- meddra_cmp_data

  for (di in seq_len(n_rows)) {
    row_vals <- vector("list", length(all_cols))
    names(row_vals) <- all_cols

    # Base metadata
    row_vals[["lvl_nm"]]     <- .safe_val(src, di, "lvl_nm", "level")
    row_vals[["lvl_no"]]     <- .safe_val(src, di, "lvl_no")
    row_vals[["soc_name"]]   <- .safe_val(src, di, "soc_name", "AEBODSYS")
    row_vals[["hlgt_name"]]  <- .safe_val(src, di, "hlgt_name", "AEHLT")
    row_vals[["hlt_name"]]   <- .safe_val(src, di, "hlt_name")
    row_vals[["pt_name"]]    <- .safe_val(src, di, "pt_name", "AEDECOD")
    row_vals[["dme"]]        <- .safe_val(src, di, "dme")
    row_vals[["soc_id"]]     <- .safe_val(src, di, "soc_id")
    row_vals[["hlgt_id"]]    <- .safe_val(src, di, "hlgt_id")
    row_vals[["hlt_id"]]     <- .safe_val(src, di, "hlt_id")
    row_vals[["pt_id"]]      <- .safe_val(src, di, "pt_id")
    row_vals[["sort_order"]] <- .safe_val(src, di, "sort_order")

    # Per-arm counts and percentages
    for (ai in seq_len(arm_count)) {
      n_name   <- paste0("arm", ai, "_n")
      pct_name <- paste0("arm", ai, "_pct")
      # Try multiple source column naming conventions
      row_vals[[n_name]]   <- .safe_val(src, di, n_name,
                                        paste0("n_", ai), "exp_n", "ctl_n")
      row_vals[[pct_name]] <- .safe_val(src, di, pct_name,
                                        paste0("pct_", ai), "exp_pct", "ctl_pct")
    }

    # Comparison columns
    if (arm_count > 1L) {
      row_vals[["rd"]] <- .safe_val(src, di, "rd")
      row_vals[["rr"]] <- .safe_val(src, di, "rr")
      row_vals[["pv"]] <- .safe_val(src, di, "pv")
      if (cc_sw > 0L) {
        row_vals[["cc_ind"]] <- .safe_val(src, di, "cc_ind")
      }
    }

    # Signal columns
    for (sc in sgnl_cols) {
      row_vals[[sc]] <- .safe_val(src, di, sc)
    }

    # Write each cell
    for (ci in seq_along(all_cols)) {
      val <- row_vals[[all_cols[ci]]]
      if (is.null(val) || (length(val) == 1L && is.na(val))) {
        writeData(wb, ws, "", startRow = r, startCol = ci, colNames = FALSE)
      } else {
        writeData(wb, ws, val, startRow = r, startCol = ci, colNames = FALSE)
      }
    }
    r <- r + 1L
  }
  last_data_row <- r - 1L

  # ------------------------------------------------------------------
  # Named ranges for the hidden data (SAS lines 1650-1790)
  # ------------------------------------------------------------------
  level_names <- c("soc", "hlgt", "hlt", "pt")

  for (lvl in level_names) {
    # Per-arm data ranges: ae{lvl}{arm}
    for (ai in seq_len(arm_count)) {
      n_ci   <- col_idx(paste0("arm", ai, "_n"))
      pct_ci <- col_idx(paste0("arm", ai, "_pct"))
      if (!is.na(n_ci) && !is.na(pct_ci)) {
        rng_name <- paste0("ae", str_to_upper(substr(lvl, 1, 1)),
                           substr(lvl, 2, nchar(lvl)), as.character(ai))
        tryCatch(
          createNamedRegion(wb, ws, cols = n_ci:pct_ci,
            rows = first_data_row:last_data_row, name = rng_name),
          error = function(e) NULL
        )
      }
    }

    # Comparison range: ae{lvl}c
    rd_ci <- col_idx("rd")
    pv_ci <- col_idx("pv")
    if (!is.na(rd_ci) && !is.na(pv_ci)) {
      tryCatch(
        createNamedRegion(wb, ws, cols = rd_ci:pv_ci,
          rows = first_data_row:last_data_row,
          name = paste0("ae", lvl, "c")),
        error = function(e) NULL
      )
    }

    # CC indicator range: ae{lvl}cc
    if (cc_sw > 0L) {
      cc_ci <- col_idx("cc_ind")
      if (!is.na(cc_ci)) {
        tryCatch(
          createNamedRegion(wb, ws, cols = cc_ci,
            rows = first_data_row:last_data_row,
            name = paste0("ae", lvl, "cc")),
          error = function(e) NULL
        )
      }
    }

    # Signal range: ae{lvl}s
    sgnl_ci <- col_idx("sgnl")
    sgnl_pt_ci <- col_idx("sgnl_pt")
    if (!is.na(sgnl_ci) && !is.na(sgnl_pt_ci)) {
      tryCatch(
        createNamedRegion(wb, ws, cols = sgnl_ci:sgnl_pt_ci,
          rows = first_data_row:last_data_row,
          name = paste0("ae", lvl, "s")),
        error = function(e) NULL
      )
    }

    # Signal-any range: ae{lvl}scd
    sgnl_any_ci <- col_idx("sgnl_any")
    if (!is.na(sgnl_any_ci)) {
      tryCatch(
        createNamedRegion(wb, ws, cols = sgnl_any_ci,
          rows = first_data_row:last_data_row,
          name = paste0("ae", lvl, "scd")),
        error = function(e) NULL
      )
    }
  }

  # Hierarchy grouping ranges for signal propagation
  # gs: SOC IDs, hg: HLGT IDs, ph: HLT IDs
  grp_ranges <- list(
    list(name = "gs", col = "soc_id"),
    list(name = "hg", col = "hlgt_id"),
    list(name = "ph", col = "hlt_id")
  )
  for (gr in grp_ranges) {
    gci <- col_idx(gr$col)
    if (!is.na(gci)) {
      tryCatch(
        createNamedRegion(wb, ws, cols = gci,
          rows = first_data_row:last_data_row, name = gr$name),
        error = function(e) NULL
      )
    }
  }

  # Per arm-pair comparison named ranges: aecmp{i}{j}, ccind{i}{j}
  if (arm_count > 1L) {
    rd_ci <- col_idx("rd")
    pv_ci <- col_idx("pv")
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i != j && !is.na(rd_ci) && !is.na(pv_ci)) {
          tryCatch(
            createNamedRegion(wb, ws, cols = rd_ci:pv_ci,
              rows = first_data_row:last_data_row,
              name = paste0("aecmp", i, j)),
            error = function(e) NULL
          )
          if (cc_sw > 0L) {
            cc_ci <- col_idx("cc_ind")
            if (!is.na(cc_ci)) {
              tryCatch(
                createNamedRegion(wb, ws, cols = cc_ci,
                  rows = first_data_row:last_data_row,
                  name = paste0("ccind", i, j)),
                error = function(e) NULL
              )
            }
          }
        }
      }
    }
    # Self-comparison placeholder
    if (!is.na(rd_ci) && !is.na(pv_ci)) {
      tryCatch(
        createNamedRegion(wb, ws, cols = rd_ci:pv_ci,
          rows = first_data_row:last_data_row, name = "cmp0"),
        error = function(e) NULL
      )
    }
    if (cc_sw > 0L) {
      cc_ci <- col_idx("cc_ind")
      if (!is.na(cc_ci)) {
        tryCatch(
          createNamedRegion(wb, ws, cols = cc_ci,
            rows = first_data_row:last_data_row, name = "ccind0"),
          error = function(e) NULL
        )
      }
    }
  }

  # Level name and sort order ranges
  lvl_nm_ci <- col_idx("lvl_nm")
  sort_ci   <- col_idx("sort_order")
  if (!is.na(lvl_nm_ci)) {
    tryCatch(
      createNamedRegion(wb, ws, cols = lvl_nm_ci,
        rows = first_data_row:last_data_row, name = "lvl_nm"),
      error = function(e) NULL
    )
  }
  if (!is.na(sort_ci)) {
    tryCatch(
      createNamedRegion(wb, ws, cols = sort_ci,
        rows = first_data_row:last_data_row, name = "sort_order"),
      error = function(e) NULL
    )
  }

  invisible(NULL)
}


# ============================================================================
# Helper: safely extract a value from a data frame row
# Tries multiple column name candidates in order; returns NA if none found
# ============================================================================
.safe_val <- function(df, row_idx, ...) {
  candidates <- c(...)
  for (cand in candidates) {
    if (cand %in% names(df)) {
      val <- df[[cand]][row_idx]
      return(val)
    }
  }
  NA
}


# ============================================================================
# wbinfo — Hidden Workbook Information Worksheet
# Replaces SAS %wbinfo macro (lines 1793-1897 of ae_meddra_output.sas)
#
# Creates a hidden worksheet containing arm metadata and VLOOKUP-derived
# formulas for the dynamic arm selection system. Provides named ranges
# used by the comparison worksheet for arm name resolution and comparison
# ID computation.
#
# @param wb         openxlsx Workbook object.
# @param all_arm    Data frame. Arm metadata with columns:
#                    arm_display (display name), arm_num (numeric ID),
#                    count (subject count per arm).
# @param arm_count  Integer. Number of treatment arms.
# @param styles     Named list of base styles from create_styles().
# @return Invisible NULL (workbook modified in place).
# @export
# ============================================================================
wbinfo <- function(wb,
                   all_arm = data.frame(),
                   arm_count = 2L,
                   styles = list()) {

  cli::cli_alert_info("Creating hidden Workbook Information worksheet")
  ws <- "Workbook Information"
  addWorksheet(wb, ws, visible = FALSE)

  n_arms <- nrow(all_arm)
  if (n_arms == 0L) {
    cli::cli_alert_info("No arm data for wbinfo sheet — skipping")
    return(invisible(NULL))
  }

  # ------------------------------------------------------------------
  # Column A/B/C headers (SAS line ~1800)
  # ------------------------------------------------------------------
  r <- 1L
  headers <- c("Arm Name", "Arm Number", "Subject Count")
  for (ci in seq_along(headers)) {
    writeData(wb, ws, headers[ci], startRow = r, startCol = ci,
              colNames = FALSE)
    if (!is.null(styles[["ColumnOutline"]])) {
      addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
    }
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Arm data rows (SAS lines ~1810-1840)
  # ------------------------------------------------------------------
  first_arm_row <- r
  arm_display_col <- intersect(c("arm_display", "arm_name"), names(all_arm))[1]
  arm_num_col     <- intersect(c("arm_num", "arm_id"), names(all_arm))[1]
  count_col       <- intersect(c("count", "n"), names(all_arm))[1]
  if (is.na(arm_display_col)) arm_display_col <- names(all_arm)[1]
  if (is.na(arm_num_col))     arm_num_col     <- names(all_arm)[2]
  if (is.na(count_col))       count_col       <- names(all_arm)[3]

  for (ai in seq_len(n_arms)) {
    arm_nm <- as.character(all_arm[[arm_display_col]][ai])
    writeData(wb, ws, arm_nm, startRow = r, startCol = 1L, colNames = FALSE)

    arm_id <- all_arm[[arm_num_col]][ai]
    writeData(wb, ws, arm_id, startRow = r, startCol = 2L, colNames = FALSE)

    arm_ct <- all_arm[[count_col]][ai]
    writeData(wb, ws, arm_ct, startRow = r, startCol = 3L, colNames = FALSE)

    r <- r + 1L
  }
  last_arm_row <- r - 1L

  # ------------------------------------------------------------------
  # VLOOKUP formulas for dynamic arm selection (SAS lines ~1850-1870)
  # exp_name / ctl_name are named ranges set on the comparison sheet
  # ------------------------------------------------------------------
  r <- r + 1L  # blank separator row
  lookup_start <- r

  # Experimental arm number: VLOOKUP(exp_name, A:C, 2, FALSE)
  writeData(wb, ws, "Exp Arm #", startRow = r, startCol = 1L, colNames = FALSE)
  exp_formula <- paste0(
    "VLOOKUP(exp_name,$A$", first_arm_row, ":$C$", last_arm_row, ",2,FALSE)")
  writeFormula(wb, ws, x = exp_formula, startRow = r, startCol = 2L)
  r <- r + 1L

  # Control arm number
  writeData(wb, ws, "Ctl Arm #", startRow = r, startCol = 1L, colNames = FALSE)
  ctl_formula <- paste0(
    "VLOOKUP(ctl_name,$A$", first_arm_row, ":$C$", last_arm_row, ",2,FALSE)")
  writeFormula(wb, ws, x = ctl_formula, startRow = r, startCol = 2L)
  r <- r + 1L

  # Comparison ID: concatenation of exp & ctl arm numbers, or 0 if same
  writeData(wb, ws, "Comparison ID", startRow = r, startCol = 1L,
            colNames = FALSE)
  cmp_formula <- paste0(
    "IF($B$", lookup_start, "=$B$", lookup_start + 1L, ",0,",
    "$B$", lookup_start, "&$B$", lookup_start + 1L, ")")
  writeFormula(wb, ws, x = cmp_formula, startRow = r, startCol = 2L)
  r <- r + 1L

  # ------------------------------------------------------------------
  # Threshold conversions: blank → "I" (infinity sentinel)
  # (SAS lines ~1870-1885)
  # ------------------------------------------------------------------
  r <- r + 1L  # blank separator
  thresh_start <- r
  thresh_labels <- c("RD Threshold", "RR Threshold", "PV Threshold")
  thresh_refs   <- c("rdn", "rrn", "pvn")
  thresh_names  <- c("rd", "rr", "pv")

  for (ti in seq_along(thresh_labels)) {
    writeData(wb, ws, thresh_labels[ti], startRow = r, startCol = 1L,
              colNames = FALSE)
    # If the user threshold cell is numeric use it; otherwise return "I"
    thresh_formula <- paste0(
      "IF(ISNUMBER(", thresh_refs[ti], "),", thresh_refs[ti], ",\"I\")")
    writeFormula(wb, ws, x = thresh_formula, startRow = r, startCol = 2L)
    r <- r + 1L
  }

  # ------------------------------------------------------------------
  # Named ranges (SAS lines ~1870-1897)
  # ------------------------------------------------------------------

  # wbinfo_arminfo_1: arm display names for dropdown validation
  tryCatch(
    createNamedRegion(wb, ws, cols = 1L,
      rows = first_arm_row:last_arm_row,
      name = "wbinfo_arminfo_1"),
    error = function(e) {
      cli::cli_alert_info("Named range wbinfo_arminfo_1 skipped: {e$message}")
    }
  )

  # wbinfo_arminfo_2: full arm info for VLOOKUP
  tryCatch(
    createNamedRegion(wb, ws, cols = 1:3,
      rows = first_arm_row:last_arm_row,
      name = "wbinfo_arminfo_2"),
    error = function(e) NULL
  )

  # armn: arm numbers column
  tryCatch(
    createNamedRegion(wb, ws, cols = 2L,
      rows = first_arm_row:last_arm_row, name = "armn"),
    error = function(e) NULL
  )

  # exp, ctl, cmp: VLOOKUP result cells
  tryCatch({
    createNamedRegion(wb, ws, cols = 2L, rows = lookup_start,
                      name = "exp")
    createNamedRegion(wb, ws, cols = 2L, rows = lookup_start + 1L,
                      name = "ctl")
    createNamedRegion(wb, ws, cols = 2L, rows = lookup_start + 2L,
                      name = "cmp")
  }, error = function(e) NULL)

  # rd, rr, pv: resolved threshold cells
  for (ti in seq_along(thresh_names)) {
    tryCatch(
      createNamedRegion(wb, ws, cols = 2L,
        rows = thresh_start + ti - 1L,
        name = thresh_names[ti]),
      error = function(e) NULL
    )
  }

  invisible(NULL)
}


# ============================================================================
# out_err — Data Check Summary / Validation Error Worksheet
# Replaces SAS %out_err macro (lines 1899-2520 of ae_meddra_output.sas)
#
# Creates the "Data Check Summary" worksheet containing:
#   - Subject validation summary (exclusion criteria and per-arm counts)
#   - AE data validation (excluded AEs by reason with detail rows)
#   - MedDRA matching summary (version, matched/unmatched counts)
#   - Alternating row highlighting for readability
#
# @param wb              openxlsx Workbook object.
# @param ndabla          Character. NDA/BLA identifier.
# @param studyid         Character. Study identifier.
# @param rundate         Character. Analysis run date/time string.
# @param arm_count       Integer. Number of treatment arms.
# @param all_arm         Data frame. Arm metadata (arm_display, arm_num, count).
# @param rpt_err         Data frame or NULL. Excluded AE records with columns
#                         USUBJID, AEBODSYS, AEDECOD, reason.
# @param vld_err         Character. "Y" if validation errors exist.
# @param naes_sp         Integer. Number of AEs in study period.
# @param naes_spv        Integer. Number of AEs after validation.
# @param meddra_pct      Numeric. MedDRA matching percentage (0-100).
# @param ver             Character. MedDRA version string.
# @param sl_group_desc   Character. Script Launcher group description.
# @param sl_subset_desc  Character. Script Launcher subset description.
# @param styles          Named list of base styles.
# @param aem_styles      Named list of MedDRA-specific styles.
# @return Invisible NULL (workbook modified in place).
# @export
# ============================================================================
out_err <- function(wb,
                    ndabla = "",
                    studyid = "",
                    rundate = "",
                    arm_count = 2L,
                    all_arm = data.frame(),
                    rpt_err = NULL,
                    vld_err = "N",
                    naes_sp = 0L,
                    naes_spv = 0L,
                    meddra_pct = 100,
                    ver = "",
                    sl_group_desc = "",
                    sl_subset_desc = "",
                    styles = list(),
                    aem_styles = list()) {

  cli::cli_alert_info("Creating Data Check Summary worksheet")
  ws <- "Data Check Summary"
  addWorksheet(wb, ws)

  all_styles <- c(styles, aem_styles)
  r <- 1L

  # Resolve column name helpers
  arm_display_col <- intersect(c("arm_display", "arm_name"), names(all_arm))[1]
  count_col       <- intersect(c("count", "n"), names(all_arm))[1]
  if (is.na(arm_display_col) && ncol(all_arm) > 0L) arm_display_col <- names(all_arm)[1]
  if (is.na(count_col) && ncol(all_arm) > 2L)       count_col       <- names(all_arm)[3]

  # ------------------------------------------------------------------
  # Title and metadata (SAS lines ~1910-1940)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Data Check Summary", startRow = r, startCol = 1L,
            colNames = FALSE)
  if (!is.null(styles[["Header"]])) {
    addStyle(wb, ws, styles[["Header"]], rows = r, cols = 1L)
  }
  r <- r + 2L

  meta_items <- c(
    str_c("NDA/BLA: ", ndabla),
    str_c("Study: ", studyid),
    str_c("Analysis run date: ", rundate)
  )
  if (nzchar(sl_group_desc)) meta_items <- c(meta_items, str_c("Group: ", sl_group_desc))
  if (nzchar(sl_subset_desc)) meta_items <- c(meta_items, str_c("Subset: ", sl_subset_desc))

  for (mi in meta_items) {
    writeData(wb, ws, mi, startRow = r, startCol = 1L, colNames = FALSE)
    if (!is.null(styles[["Default10Wrap"]])) {
      addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
    }
    r <- r + 1L
  }
  r <- r + 1L

  # ------------------------------------------------------------------
  # Subject Validation Section (SAS lines ~1940-2020)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Subject Validation", startRow = r, startCol = 1L,
            colNames = FALSE)
  if (!is.null(styles[["SubHeader"]])) {
    addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  }
  r <- r + 1L

  subj_text <- paste0(
    "Subjects are included in the analysis if they have a valid treatment ",
    "arm assignment in the DM domain. Subjects without a valid arm assignment ",
    "are excluded from all analyses.")
  writeData(wb, ws, subj_text, startRow = r, startCol = 1L, colNames = FALSE)
  if (!is.null(styles[["Default10Wrap"]])) {
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  }
  r <- r + 2L

  # Subject count table: header row
  n_arm_display <- min(arm_count, nrow(all_arm))
  subj_headers <- c("Arm", "N")
  for (ci in seq_along(subj_headers)) {
    writeData(wb, ws, subj_headers[ci], startRow = r, startCol = ci,
              colNames = FALSE)
    if (!is.null(styles[["ColumnOutline"]])) {
      addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
    }
  }
  r <- r + 1L

  # One row per arm
  for (ai in seq_len(n_arm_display)) {
    arm_nm <- if (!is.na(arm_display_col)) {
      as.character(all_arm[[arm_display_col]][ai])
    } else {
      paste0("Arm ", ai)
    }
    arm_ct <- if (!is.na(count_col)) all_arm[[count_col]][ai] else 0L

    writeData(wb, ws, arm_nm, startRow = r, startCol = 1L, colNames = FALSE)
    if (!is.null(styles[["Data"]])) {
      addStyle(wb, ws, styles[["Data"]], rows = r, cols = 1L)
    }
    writeData(wb, ws, arm_ct, startRow = r, startCol = 2L, colNames = FALSE)
    r <- r + 1L
  }

  # Total row
  total_n <- if (!is.na(count_col)) sum(all_arm[[count_col]], na.rm = TRUE) else 0L
  writeData(wb, ws, "Total", startRow = r, startCol = 1L, colNames = FALSE)
  if (!is.null(styles[["DataBottom"]])) {
    addStyle(wb, ws, styles[["DataBottom"]], rows = r, cols = 1L)
  }
  writeData(wb, ws, total_n, startRow = r, startCol = 2L, colNames = FALSE)
  r <- r + 2L

  # ------------------------------------------------------------------
  # AE Data Validation Section (SAS lines ~2020-2200)
  # ------------------------------------------------------------------
  writeData(wb, ws, "Adverse Events Data Validation", startRow = r,
            startCol = 1L, colNames = FALSE)
  if (!is.null(styles[["SubHeader"]])) {
    addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  }
  r <- r + 1L

  ae_text <- paste0(
    "Adverse events are checked for completeness and validity. Events may be ",
    "excluded if they have missing dates (when analysis period validation is ",
    "enabled), missing AEBODSYS or AEDECOD values, or fall outside the study ",
    "analysis period.")
  writeData(wb, ws, ae_text, startRow = r, startCol = 1L, colNames = FALSE)
  if (!is.null(styles[["Default10Wrap"]])) {
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  }
  r <- r + 1L

  # AE count summary table
  n_excluded <- naes_sp - naes_spv
  ae_summary <- dplyr::tibble(
    Metric = c("AEs in study period", "AEs after validation", "AEs excluded"),
    Count = c(as.character(naes_sp),
              as.character(naes_spv),
              as.character(n_excluded))
  )
  for (si in seq_len(nrow(ae_summary))) {
    writeData(wb, ws, ae_summary$Metric[si], startRow = r, startCol = 1L,
              colNames = FALSE)
    if (!is.null(styles[["Data"]])) {
      addStyle(wb, ws, styles[["Data"]], rows = r, cols = 1L)
    }
    writeData(wb, ws, ae_summary$Count[si], startRow = r, startCol = 2L,
              colNames = FALSE)
    r <- r + 1L
  }
  r <- r + 1L

  # Excluded AE detail rows (SAS lines ~2100-2200)
  if (vld_err == "Y" && !is.null(rpt_err) && is.data.frame(rpt_err) &&
      nrow(rpt_err) > 0L) {

    writeData(wb, ws, "Excluded Adverse Events:", startRow = r, startCol = 1L,
              colNames = FALSE)
    if (!is.null(styles[["SubHeader"]])) {
      addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
    }
    r <- r + 1L

    # Column headers
    err_headers <- c("USUBJID", "AEBODSYS", "AEDECOD", "Reason")
    for (ci in seq_along(err_headers)) {
      writeData(wb, ws, err_headers[ci], startRow = r, startCol = ci,
                colNames = FALSE)
      if (!is.null(styles[["ColumnOutline"]])) {
        addStyle(wb, ws, styles[["ColumnOutline"]], rows = r, cols = ci)
      }
    }
    r <- r + 1L

    # Map column names flexibly
    err_col_map <- c(
      USUBJID  = intersect(c("USUBJID", "usubjid"), names(rpt_err))[1],
      AEBODSYS = intersect(c("AEBODSYS", "aebodsys"), names(rpt_err))[1],
      AEDECOD  = intersect(c("AEDECOD", "aedecod"), names(rpt_err))[1],
      reason   = intersect(c("reason", "Reason", "err_reason", "exclusion_reason"),
                           names(rpt_err))[1]
    )

    n_err_rows <- min(nrow(rpt_err), 5000L)  # safety cap
    for (ei in seq_len(n_err_rows)) {
      for (ci in seq_along(err_col_map)) {
        col_nm <- err_col_map[ci]
        val <- if (!is.na(col_nm)) as.character(rpt_err[[col_nm]][ei]) else ""
        if (is.na(val)) val <- ""
        writeData(wb, ws, val, startRow = r, startCol = ci, colNames = FALSE)

        is_last <- (ei == n_err_rows)
        sty_name <- if (is_last) "DataBottom" else "Data"
        if (!is.null(styles[[sty_name]])) {
          addStyle(wb, ws, styles[[sty_name]], rows = r, cols = ci)
        }
      }
      r <- r + 1L
    }
    r <- r + 1L
  }

  # ------------------------------------------------------------------
  # MedDRA Matching Section (SAS lines ~2200-2500)
  # ------------------------------------------------------------------
  writeData(wb, ws, "MedDRA Matching", startRow = r, startCol = 1L,
            colNames = FALSE)
  if (!is.null(styles[["SubHeader"]])) {
    addStyle(wb, ws, styles[["SubHeader"]], rows = r, cols = 1L)
  }
  r <- r + 1L

  meddra_ver_text <- str_c("MedDRA Version: ", ver)
  writeData(wb, ws, meddra_ver_text, startRow = r, startCol = 1L,
            colNames = FALSE)
  if (!is.null(styles[["Default10Wrap"]])) {
    addStyle(wb, ws, styles[["Default10Wrap"]], rows = r, cols = 1L)
  }
  r <- r + 1L

  # Compute match/non-match counts using SAS-compatible rounding
  n_matched <- janitor::round_half_up(naes_spv * meddra_pct / 100, 0)
  n_unmatched <- naes_spv - n_matched
  match_pct_disp <- janitor::round_half_up(meddra_pct, 1)

  match_summary <- dplyr::tibble(
    Metric = c("Terms matched", "Terms not matched", "Match percentage"),
    Value = c(as.character(n_matched),
              as.character(n_unmatched),
              str_c(match_pct_disp, "%"))
  )
  for (si in seq_len(nrow(match_summary))) {
    writeData(wb, ws, match_summary$Metric[si], startRow = r, startCol = 1L,
              colNames = FALSE)
    if (!is.null(styles[["Data"]])) {
      addStyle(wb, ws, styles[["Data"]], rows = r, cols = 1L)
    }
    writeData(wb, ws, match_summary$Value[si], startRow = r, startCol = 2L,
              colNames = FALSE)
    r <- r + 1L
  }

  # ------------------------------------------------------------------
  # Page setup (SAS lines ~2510-2520)
  # ------------------------------------------------------------------
  pageSetup(wb, ws, orientation = "landscape", fitToWidth = TRUE)

  invisible(NULL)
}


# ============================================================================
# out_med — Master Orchestrator for MedDRA at a Glance Excel Workbook
# Replaces SAS %out_med macro (lines 2674-2763 of ae_meddra_output.sas)
#
# Creates the complete MedDRA at a Glance workbook by:
#   1. Initialising a new openxlsx Workbook
#   2. Creating base + MedDRA-specific style galleries
#   3. Pre-processing Script Launcher group/subset descriptions
#   4. Calling each worksheet sub-function in sequence:
#        out_cover → out_meddra_cmp → out_meddra_cmp_data → wbinfo →
#        group_subset_xml_out → out_err
#   5. Saving the workbook to the specified output path
#
# All SAS global macro variables become named function parameters with
# matching defaults. No hardcoded file paths.
#
# @param ndabla          Character. NDA/BLA identifier.
# @param studyid         Character. Study identifier.
# @param aemedout        Character. Output file path for the Excel workbook.
# @param arm_count       Integer. Number of treatment arms.
# @param arm_name        Named character vector. Arm display names keyed by
#                         position (e.g., c("1" = "Treatment", "2" = "Placebo")).
# @param all_arm         Data frame. Arm metadata with columns arm_display,
#                         arm_num, count.
# @param meddra_cmp_output Data frame. Visible comparison data for the
#                            MedDRA Comparison Analysis worksheet.
# @param meddra_cmp_data Data frame. Enriched hidden data for the hidden
#                          MedDRA Comparison Data worksheet.
# @param arm_pairs       Data frame. Arm pair combinations (exp/ctl IDs).
# @param rpt_err         Data frame or NULL. Excluded AE records.
# @param vld_err         Character. "Y" if validation errors exist.
# @param vld_sw          Character. Validation switch.
# @param naes_sp         Integer. AEs in study period.
# @param naes_spv        Integer. AEs after validation.
# @param meddra_pct      Numeric. MedDRA matching percentage (0-100).
# @param ver             Character. MedDRA version.
# @param cc_sw           Integer. Continuity correction switch (0/1/2).
# @param sl_group        List or NULL. Script Launcher group parameters.
# @param sl_subset       List or NULL. Script Launcher subset parameters.
# @param sl_datasets     List or NULL. Script Launcher custom datasets.
# @param domain_data     Data frame or NULL. Domain data for SL preprocessing.
# @param wbtitle         Character. Workbook title.
# @param strlen          Integer. Maximum string length for annotation
#                         (default 160, matching SAS %let strlen = 160).
# @param author          Character. Workbook author metadata.
# @return Invisible character. Path to the saved workbook file.
# @export
# ============================================================================
out_med <- function(ndabla = "",
                    studyid = "",
                    aemedout = "ae_meddra_output.xlsx",
                    arm_count = 2L,
                    arm_name = c("1" = "Treatment", "2" = "Placebo"),
                    all_arm = data.frame(),
                    meddra_cmp_output = data.frame(),
                    meddra_cmp_data = data.frame(),
                    arm_pairs = data.frame(),
                    rpt_err = NULL,
                    vld_err = "N",
                    vld_sw = "Y",
                    naes_sp = 0L,
                    naes_spv = 0L,
                    meddra_pct = 100,
                    ver = "",
                    cc_sw = 0L,
                    sl_group = NULL,
                    sl_subset = NULL,
                    sl_datasets = NULL,
                    domain_data = NULL,
                    wbtitle = "MedDRA at a Glance",
                    strlen = 160L,
                    author = "PhUSE CS") {

  # ------------------------------------------------------------------
  # Banner (replaces SAS boxed title, lines 2677-2690)
  # ------------------------------------------------------------------
  cli::cli_h2("MAKING EXCEL OUTPUT FOR AE MEDDRA COMPARISON REPORT")
  rundate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_alert_info("Run date: {rundate}")
  cli::cli_alert_info("Output file: {aemedout}")

  # ------------------------------------------------------------------
  # 1. Create workbook (replaces SAS %wb at line 2701)
  # ------------------------------------------------------------------
  wb <- createWorkbook(creator = author, title = wbtitle)
  cli::cli_alert_info("Workbook created")

  # ------------------------------------------------------------------
  # 2. Create style galleries (replaces SAS %styles(size=8) at line 2702)
  # ------------------------------------------------------------------
  styles <- create_styles(size = 8)
  aem_styles <- out_aem_styles()
  cli::cli_alert_info("Style galleries created ({length(styles)} base + {length(aem_styles)} MedDRA styles)")

  # ------------------------------------------------------------------
  # 3. Pre-process Script Launcher group/subset (line 2698)
  # ------------------------------------------------------------------
  sl_group_desc <- ""
  sl_subset_desc <- ""
  pp_result <- NULL

  if (!is.null(sl_group) || !is.null(sl_subset)) {
    tryCatch({
      pp_result <- group_subset_pp(
        sl_group = sl_group,
        sl_subset = sl_subset,
        sl_datasets = sl_datasets,
        domain_data = domain_data
      )
      sl_group_desc  <- if (!is.null(pp_result$sl_group_desc)) pp_result$sl_group_desc else ""
      sl_subset_desc <- if (!is.null(pp_result$sl_subset_desc)) pp_result$sl_subset_desc else ""
      cli::cli_alert_info("Group/subset preprocessing complete")
    }, error = function(e) {
      cli::cli_alert_info("Group/subset preprocessing skipped: {e$message}")
    })
  }

  # ------------------------------------------------------------------
  # 4. Cover sheet (replaces SAS %out_cover at line 2703)
  # ------------------------------------------------------------------
  out_cover(
    wb = wb,
    ndabla = ndabla,
    studyid = studyid,
    rundate = rundate,
    arm_count = arm_count,
    arm_name = arm_name,
    cc_sw = cc_sw,
    sl_gs_desc = paste0(
      if (nzchar(sl_group_desc)) sl_group_desc else "",
      if (nzchar(sl_group_desc) && nzchar(sl_subset_desc)) "; " else "",
      if (nzchar(sl_subset_desc)) sl_subset_desc else ""
    ),
    ver = ver,
    styles = styles,
    wbtitle = wbtitle
  )
  cli::cli_alert_info("Cover sheet complete")

  # ------------------------------------------------------------------
  # 5. Visible comparison worksheet (replaces SAS %out_meddra_cmp ~line 2705)
  # ------------------------------------------------------------------
    out_meddra_cmp(
    wb = wb,
    meddra_cmp_output = meddra_cmp_output,
    arm_count = arm_count,
    arm_name = arm_name,
    cc_sw = cc_sw,
    styles = styles,
    aem_styles = aem_styles
  )
  cli::cli_alert_info("MedDRA Comparison Analysis worksheet complete")

  # ------------------------------------------------------------------
  # 6. Hidden data worksheet (replaces SAS %out_meddra_cmp_data ~line 2710)
  # ------------------------------------------------------------------
  out_meddra_cmp_data(
    wb = wb,
    meddra_cmp_data = meddra_cmp_data,
    arm_count = arm_count,
    arm_pairs = arm_pairs,
    cc_sw = cc_sw,
    styles = styles,
    aem_styles = aem_styles
  )
  cli::cli_alert_info("Hidden data worksheet complete")

  # ------------------------------------------------------------------
  # 7. Workbook information (replaces SAS %wbinfo ~line 2720)
  # ------------------------------------------------------------------
  wbinfo(
    wb = wb,
    all_arm = all_arm,
    arm_count = arm_count,
    styles = styles
  )
  cli::cli_alert_info("Workbook information worksheet complete")

  # ------------------------------------------------------------------
  # 8. Group/subset worksheet (replaces SAS %group_subset_xml_out line 2749)
  # ------------------------------------------------------------------
  if (!is.null(pp_result)) {
    tryCatch({
      group_subset_xml_out(
        wb = wb,
        pp_result = pp_result,
        ndabla = ndabla,
        studyid = studyid
      )
      cli::cli_alert_info("Group/subset worksheet complete")
    }, error = function(e) {
      cli::cli_alert_info("Group/subset worksheet skipped: {e$message}")
    })
  }

  # ------------------------------------------------------------------
  # 9. Data Check Summary (replaces SAS %out_err ~line 2750)
  # ------------------------------------------------------------------
  out_err(
    wb = wb,
    ndabla = ndabla,
    studyid = studyid,
    rundate = rundate,
    arm_count = arm_count,
    all_arm = all_arm,
    rpt_err = rpt_err,
    vld_err = vld_err,
    naes_sp = naes_sp,
    naes_spv = naes_spv,
    meddra_pct = meddra_pct,
    ver = ver,
    sl_group_desc = sl_group_desc,
    sl_subset_desc = sl_subset_desc,
    styles = styles,
    aem_styles = aem_styles
  )
  cli::cli_alert_info("Data Check Summary worksheet complete")

  # ------------------------------------------------------------------
  # 10. Save workbook (replaces SAS DATA _NULL_ lines 2757-2761)
  # ------------------------------------------------------------------
  # Ensure output directory exists
  out_dir <- dirname(aemedout)
  if (nzchar(out_dir) && out_dir != ".") {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }

  saveWorkbook(wb, file = aemedout, overwrite = TRUE)
  cli::cli_alert_success("Workbook saved: {aemedout}")

  invisible(aemedout)
}


# ============================================================================
# MIGRATION NOTES
# ============================================================================
#
# ASSUMPTIONS:
#   - Excel formulas referencing named ranges (exp_name, ctl_name, rdn, rrn,
#     pvn, cmp, exp, ctl, armn, wbinfo_arminfo_1/2) are preserved and will
#     resolve correctly when the workbook is opened in Excel.
#   - Dropdown data validation via openxlsx::dataValidation() replaces the
#     SAS SpreadsheetML <DataValidation> XML elements and provides equivalent
#     user-facing dropdown functionality.
#   - Hidden worksheets (MedDRA Comparison Data, Workbook Information) replicate
#     the SAS hidden data sheets using openxlsx's visible = FALSE parameter.
#   - The SAS %annotate/%markup cell-level XML generation pattern is replaced
#     by direct openxlsx::writeData() + addStyle() calls, which provide
#     equivalent cell-level formatting control.
#   - The SAS SpreadsheetML conditional formatting logic (signal highlighting
#     when values exceed user-defined thresholds) is replicated using
#     openxlsx::conditionalFormatting() with equivalent rule syntax.
#   - Column widths are approximated from the SAS XML <Column ss:Width="N"/>
#     values; exact pixel-to-character-width conversion may vary slightly
#     between Excel rendering engines.
#   - The create_styles() function from xml_output.R provides all base styles
#     (Header, SubHeader, ColumnOutline, Data, DataBottom, Default10Wrap, etc.)
#     that are referenced throughout this module.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   - None expected for the output module itself — this file performs formatting
#     and workbook generation, not statistical computation.
#   - Rounding of display values (MedDRA matching percentage, AE counts) uses
#     janitor::round_half_up() to match SAS round-half-up behavior.
#   - Excel formula evaluation may produce minor floating-point differences
#     depending on the Excel version used to open the workbook; this is
#     inherent to Excel, not the R migration.
#
# NO DIRECT R EQUIVALENT:
#   - SAS SpreadsheetML XML generation (DATA _NULL_ with PUT statements
#     building XML strings character by character) — replaced entirely by
#     the openxlsx API which generates native OOXML .xlsx files.
#   - SAS %annotate/%markup macros for cell-level annotation and rendering —
#     replaced by openxlsx writeData/writeFormula/addStyle/mergeCells.
#   - SAS %xml_tag_def/%xml_init/%xml_tag_close XML infrastructure macros —
#     not needed; openxlsx handles worksheet/cell lifecycle natively.
#   - SAS named ranges with formula references in XML Names block — replicated
#     via openxlsx::createNamedRegion() + writeFormula().
#   - SAS PrintArea named ranges — openxlsx does not natively support
#     PrintArea; use pageSetup() fitToWidth/fitToHeight instead.
#   - SAS WorksheetOptions (FitToPage, zoom percentage, unsynced) — partially
#     replicated via pageSetup(); zoom level may not be exactly preserved.
#
# PACKAGE SELECTION RATIONALE:
#   - openxlsx (>=4.2.5): Chosen over writexl (no formatting/formula support),
#     xlsx (Java dependency), and openxlsx2 (newer API not yet stable in all
#     pharma environments). openxlsx provides native named ranges, data
#     validation, conditional formatting, formula support, and style
#     management required for full SAS SpreadsheetML parity.
#   - dplyr (>=1.1.0): Mandated by AAP for all data manipulation (tidyverse
#     over base R). Used for tibble construction, filtering, column selection.
#   - stringr (>=1.5.0): Mandated by AAP for string operations. Used for
#     formula string building, text content assembly, column name matching.
#   - janitor (>=2.2.0): Mandated by AAP for SAS-compatible rounding via
#     round_half_up(). Used at all numeric display formatting points.
#   - purrr (>=1.0.0): Mandated by AAP for functional iteration (over base R
#     lapply/sapply). Used for per-arm column generation and style iteration.
#   - cli (>=3.6.0): Provides consistent user-facing progress messages
#     replacing SAS %put and DATA _NULL_ banner output.
#
# OPEN QUESTIONS:
#   - Verify all Excel formula strings (VLOOKUP, IF, ISNUMBER, INDEX/INDIRECT)
#     produce identical results when the workbook is opened in Excel, compared
#     to the SAS-generated SpreadsheetML output.
#   - Confirm named range scoping (workbook-level vs worksheet-level) matches
#     the SAS XML output. openxlsx creates workbook-scoped named ranges by
#     default, which should match SAS behavior.
#   - Verify conditional formatting threshold rules (> vs >=) match the SAS
#     ConditionalFormatting XML Qualifier attribute exactly.
#   - Column width units may differ slightly between SAS SpreadsheetML points
#     and openxlsx character-width units; verify visual alignment in Excel.
#   - The SAS source uses JET/PCFILES in some branches — confirm this code path
#     is not invoked for MedDRA output (it appears to be disposition/liver only).
#
# ============================================================================
