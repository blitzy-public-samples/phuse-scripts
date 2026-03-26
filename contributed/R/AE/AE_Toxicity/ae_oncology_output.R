# ==============================================================================
# ae_oncology_output.R
# ==============================================================================
# Migrated from: contributed/AE/AE_Toxicity/ae_oncology_output.sas (2485 lines)
# Purpose: Oncology AE workbook generation — all SpreadsheetML XML output is
#          replaced by openxlsx API calls. Generates formatted Excel workbooks
#          with cover sheet, toxicity grade summary, preferred term analysis
#          (filterable V1 and printable V2), MedDRA comparison analysis (V1/V2),
#          and data check summary worksheets.
#
# Required dependencies (must be sourced before this file):
#   Internal: contributed/R/AE/ZZ_Utilities/xml_output.R
#               (create_styles, create_dynamic_styles, write_header,
#                write_data_table, annotate_data, write_annotated_data)
#             contributed/R/AE/ZZ_Utilities/sl_gs_output.R
#               (group_subset_pp, group_subset_xml_out)
#   External: openxlsx (>=4.2.5), dplyr (>=1.1.0), tidyr (>=1.3.0),
#             stringr (>=1.5.0), cli (>=3.6.0)
# ==============================================================================

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(cli)
library(rlang)  # provides %||% (null-coalescing operator) used throughout

# --- Module-level constants (SAS globals: lines 8-9) ---
WBTITLE <- "AE Toxicity Panel"


# ==============================================================================
# create_oae_styles
# ------------------------------------------------------------------------------
# Replaces SAS %out_oae_styles macro (SAS lines 2347-2404).
# Creates all oncology-specific openxlsx style objects used across worksheets.
# Produces base styles, highlighted variants (fill #CCCCFF), and all border
# permutations (Left, Right, Bottom, and their combinations).
#
# @param wb An openxlsx Workbook object (reserved for future use).
# @return Named list of openxlsx Style objects including standard styles from
#         xml_output plus all oncology-specific permutations.
# ==============================================================================
create_oae_styles <- function(wb) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }

  # Retrieve standard styles from the xml_output module
  base_styles <- create_styles()

  # ------------------------------------------------------------------
  # Define oncology base style parameters (SAS lines 2349-2373)
  # Each entry maps to openxlsx::createStyle() arguments.
  # ------------------------------------------------------------------
  oae_defs <- list(
    D      = list(),
    D0_R1  = list(halign = "right", numFmt = "0", indent = 1L),
    D0_R2  = list(halign = "right", numFmt = "0", indent = 2L),
    D0_R4  = list(halign = "right", numFmt = "0", indent = 4L),
    D1_R1  = list(halign = "right", numFmt = "0.0", indent = 1L),
    D1_R2  = list(halign = "right", numFmt = "0.0", indent = 2L),
    D1_R2N = list(halign = "right", numFmt = "0.0", indent = 2L),
    D1_R2S = list(halign = "right", numFmt = "0.0", indent = 2L),
    D2_R1  = list(halign = "right", numFmt = "0.00", indent = 1L),
    D2_R2  = list(halign = "right", numFmt = "0.00", indent = 2L),
    LCL    = list(halign = "right"),
    UCL    = list(halign = "left"),
    UCLSN  = list(halign = "left"),
    DT     = list(valign = "top", wrapText = TRUE),
    D0_R1T = list(halign = "right", numFmt = "0", indent = 1L, valign = "top"),
    D1_R1T = list(halign = "right", numFmt = "0.0", indent = 1L, valign = "top")
  )

  # ------------------------------------------------------------------
  # Border suffix definitions (SAS lines 2386-2397)
  # Underscore-prefixed for explicit placement; "B" for bottom-row append.
  # ------------------------------------------------------------------
  border_map <- list(
    "_BL"   = c("Left"),
    "_BR"   = c("Right"),
    "_BLR"  = c("Left", "Right"),
    "B"     = c("Bottom"),
    "_BLB"  = c("Left", "Bottom"),
    "_BRB"  = c("Right", "Bottom"),
    "_BLRB" = c("Left", "Right", "Bottom")
  )

  highlight_fill <- "#CCCCFF"

  # Helper: build one openxlsx style from params + optional borders + fill

  make_oae <- function(params, borders = NULL, fill = NULL) {
    args <- params
    if (!is.null(borders) && length(borders) > 0L) {
      args$border      <- borders
      args$borderStyle <- "thin"
    }
    if (!is.null(fill)) {
      args$fgFill <- fill
    }
    do.call(openxlsx::createStyle, args)
  }

  oae_styles <- list()

  for (base_id in names(oae_defs)) {
    p <- oae_defs[[base_id]]

    # Base style (no borders, no highlight)
    oae_styles[[base_id]] <- make_oae(p)

    # Highlighted variant (SAS lines 2375-2378)
    h_id <- paste0(base_id, "H")
    oae_styles[[h_id]] <- make_oae(p, fill = highlight_fill)

    # Border permutations for both base and highlighted
    for (sfx in names(border_map)) {
      brd <- border_map[[sfx]]
      oae_styles[[paste0(base_id, sfx)]]  <- make_oae(p, borders = brd)
      oae_styles[[paste0(h_id, sfx)]]     <- make_oae(p, borders = brd,
                                                        fill = highlight_fill)
    }
  }

  # Generate additional dynamic styles from a tibble specification using

  # create_dynamic_styles() (replaces SAS %xml_style_dcl / %xml_style_markup)
  dynamic_spec <- dplyr::tibble(
    ID        = c("DynDefault", "DynBold", "DynRed"),
    HA        = c("left", "left", "left"),
    VA        = c("top", "center", "center"),
    Indent    = c(0L, 0L, 0L),
    Wrap      = c(FALSE, FALSE, TRUE),
    Rotate    = c(0L, 0L, 0L),
    BT        = c(FALSE, FALSE, FALSE),
    BL        = c(FALSE, FALSE, FALSE),
    BR        = c(FALSE, FALSE, FALSE),
    BB        = c(FALSE, FALSE, FALSE),
    BWt       = c(1L, 1L, 1L),
    BLS       = c("thin", "thin", "thin"),
    IntClr    = c(NA_character_, NA_character_, NA_character_),
    FontSize  = c(10L, 10L, 10L),
    FontColor = c("#000000", "#000000", "#FF0000"),
    Bold      = c(FALSE, TRUE, FALSE),
    Italic    = c(FALSE, FALSE, FALSE),
    NumFmt    = c("General", "General", "General")
  )
  dyn_styles <- create_dynamic_styles(dynamic_spec)
  for (nm in names(dyn_styles)) {
    oae_styles[[nm]] <- dyn_styles[[nm]]
  }

  # Merge standard + oncology + dynamic styles (oncology takes precedence)
  all_styles <- base_styles
  for (nm in names(oae_styles)) {
    all_styles[[nm]] <- oae_styles[[nm]]
  }

  cli::cli_alert_info(
    "Created {length(oae_styles)} oncology styles, {length(all_styles)} total."
  )
  all_styles
}


# ==============================================================================
# build_column_headers
# ------------------------------------------------------------------------------
# Replaces SAS %wscolumns macro (SAS lines ~2240-2343).
# Writes three-tier column headings for the toxicity-grade analysis worksheets
# (pt_1 and pt_2). Structure:
#   Row 1: Key labels (merge down 2), arm-name headers (merge across grade cols)
#   Row 2: Grade-group sub-headers (All Grades, Grades 3/4, etc.)
#   Row 3: Individual column labels (Subject Count, %)
#
# @param wb          An openxlsx Workbook object.
# @param sheet       Character. Worksheet name.
# @param params_list Named list of study parameters.
# @param rpt_key_row Single-row data frame from rpt_key for this report.
# @param styles      Named list of openxlsx Style objects.
# @param start_row   Integer. First row to write headers.
# @return Integer. Next available row after column headers.
# ==============================================================================
build_column_headers <- function(wb, sheet, params_list, rpt_key_row, styles,
                                 start_row) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.character(sheet) || length(sheet) != 1L) {
    cli::cli_abort("{.arg sheet} must be a single character string.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  arm_count     <- as.integer(params_list$arm_count)
  arm_names     <- params_list$arm_names
  arm_ns        <- params_list$arm_ns
  toxgr_grp5_sw <- toupper(params_list$toxgr_grp5_sw %||% "N")

  # Parse pipe-delimited key labels from rpt_key
  key_labels <- stringr::str_trim(unlist(strsplit(as.character(rpt_key_row$key_label),
                                                  "\\|")))
  key_count  <- length(key_labels)

  # Calculate number of grade columns per arm:
  #   Grouped: All Grades (count + %) = 2
  #            Grades 3/4 or 3/4/5 (count + %) = 2
  #            [if grp5] Grade 5 (count + %) = 2
  #   Individual: Grade 1..5 (count each) = 5
  #   Missing: 1
  n_grp_hdrs <- if (toxgr_grp5_sw == "Y") 3L else 2L
  cols_per_arm <- n_grp_hdrs * 2L + 5L + 1L  # grouped pairs + individual + missing

  col_style <- styles[["ColumnOutline"]]
  r1 <- as.integer(start_row)
  r2 <- r1 + 1L
  r3 <- r1 + 2L

  # Max arm name length for row height calculation
  max_nm <- max(nchar(arm_names), na.rm = TRUE)
  row1_ht <- max(25, ceiling(max_nm / 13.25) * 13.75)
  openxlsx::setRowHeights(wb, sheet, rows = r1, heights = row1_ht)
  openxlsx::setRowHeights(wb, sheet, rows = r2, heights = 25)
  openxlsx::setRowHeights(wb, sheet, rows = r3, heights = 30)

  # --- Row 1: Key labels (merge down 2) + Arm name banners ---
  col_cursor <- 1L
  for (ki in seq_along(key_labels)) {
    openxlsx::writeData(wb, sheet, key_labels[ki],
                        startRow = r1, startCol = col_cursor)
    openxlsx::mergeCells(wb, sheet, cols = col_cursor, rows = r1:r3)
    if (!is.null(col_style)) {
      openxlsx::addStyle(wb, sheet, col_style,
                         rows = r1, cols = col_cursor, stack = TRUE)
    }
    col_cursor <- col_cursor + 1L
  }

  for (ai in seq_len(arm_count)) {
    arm_text <- paste0(arm_names[ai], "\n(N=", arm_ns[ai], ")")
    openxlsx::writeData(wb, sheet, arm_text,
                        startRow = r1, startCol = col_cursor)
    end_col <- col_cursor + cols_per_arm - 1L
    openxlsx::mergeCells(wb, sheet, cols = col_cursor:end_col, rows = r1)
    if (!is.null(col_style)) {
      openxlsx::addStyle(wb, sheet, col_style,
                         rows = r1, cols = col_cursor:end_col, stack = TRUE)
    }

    # --- Row 2: Grade-group sub-headers ---
    c2 <- col_cursor

    # All Grades (merge across 2 cols: count + %)
    openxlsx::writeData(wb, sheet, "All Grades", startRow = r2, startCol = c2)
    openxlsx::mergeCells(wb, sheet, cols = c2:(c2 + 1L), rows = r2)
    if (!is.null(col_style)) {
      openxlsx::addStyle(wb, sheet, col_style,
                         rows = r2, cols = c2:(c2 + 1L), stack = TRUE)
    }
    c2 <- c2 + 2L

    # Grades 3/4 or 3/4/5 (merge across 2)
    grp_label <- dplyr::if_else(toxgr_grp5_sw == "Y",
                                "Grades 3/4", "Grades 3/4/5")
    openxlsx::writeData(wb, sheet, grp_label, startRow = r2, startCol = c2)
    openxlsx::mergeCells(wb, sheet, cols = c2:(c2 + 1L), rows = r2)
    if (!is.null(col_style)) {
      openxlsx::addStyle(wb, sheet, col_style,
                         rows = r2, cols = c2:(c2 + 1L), stack = TRUE)
    }
    c2 <- c2 + 2L

    # Conditional Grade 5 header
    if (toxgr_grp5_sw == "Y") {
      openxlsx::writeData(wb, sheet, "Grade 5", startRow = r2, startCol = c2)
      openxlsx::mergeCells(wb, sheet, cols = c2:(c2 + 1L), rows = r2)
      if (!is.null(col_style)) {
        openxlsx::addStyle(wb, sheet, col_style,
                           rows = r2, cols = c2:(c2 + 1L), stack = TRUE)
      }
      c2 <- c2 + 2L
    }

    # Individual grades 1-5 (merge down 1 each)
    for (g in 1:5) {
      g_label <- paste0("Grade ", g)
      openxlsx::writeData(wb, sheet, g_label, startRow = r2, startCol = c2)
      openxlsx::mergeCells(wb, sheet, cols = c2, rows = r2:r3)
      if (!is.null(col_style)) {
        openxlsx::addStyle(wb, sheet, col_style,
                           rows = r2:r3, cols = c2, stack = TRUE)
      }
      c2 <- c2 + 1L
    }

    # Missing (merge down 1)
    openxlsx::writeData(wb, sheet, "Missing", startRow = r2, startCol = c2)
    openxlsx::mergeCells(wb, sheet, cols = c2, rows = r2:r3)
    if (!is.null(col_style)) {
      openxlsx::addStyle(wb, sheet, col_style,
                         rows = r2:r3, cols = c2, stack = TRUE)
    }
    c2 <- c2 + 1L

    # --- Row 3: Sub-labels under grouped headers ---
    c3 <- col_cursor
    for (gi in seq_len(n_grp_hdrs)) {
      openxlsx::writeData(wb, sheet, "Subject\nCount",
                          startRow = r3, startCol = c3)
      if (!is.null(col_style)) {
        openxlsx::addStyle(wb, sheet, col_style,
                           rows = r3, cols = c3, stack = TRUE)
      }
      c3 <- c3 + 1L
      openxlsx::writeData(wb, sheet, "%", startRow = r3, startCol = c3)
      if (!is.null(col_style)) {
        openxlsx::addStyle(wb, sheet, col_style,
                           rows = r3, cols = c3, stack = TRUE)
      }
      c3 <- c3 + 1L
    }

    # Skip individual grade + missing cols (already merged down from Row 2)
    col_cursor <- col_cursor + cols_per_arm
  }

  # Return the row after the three header rows
  r3 + 1L
}

# ==============================================================================
# out_cover
# ------------------------------------------------------------------------------
# Replaces SAS %out_cover macro (SAS lines 12-438).
# Adds the "Front Page" worksheet containing narrative descriptions of each
# analysis section, a 2x2 contingency table example, calculation definitions,
# continuity-correction notes, and report settings.
#
# @param wb          An openxlsx Workbook object.
# @param params_list Named list of study parameters.
# @param rpt_key     Data frame of report key metadata.
# @param rpt_missing Data frame of missing toxicity grade counts.
# @param styles      Named list of openxlsx Style objects.
# @return Invisible NULL. Worksheet is added as a side effect.
# ==============================================================================
out_cover <- function(wb, params_list, rpt_key, rpt_missing, styles) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.list(params_list)) {
    cli::cli_abort("{.arg params_list} must be a named list.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  sheet <- "Front Page"
  openxlsx::addWorksheet(wb, sheet)

  # Column widths (SAS cover page: wide single-column layout with merges)
  openxlsx::setColWidths(wb, sheet, cols = 1:12, widths = c(30, rep(10, 11)))

  ndabla    <- params_list$ndabla %||% ""
  studyid   <- params_list$studyid %||% ""
  rundate   <- params_list$rundate %||% format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")
  arm_count <- as.integer(params_list$arm_count %||% 1L)

  # Toxicity grade grouping description (SAS lines 26-48)
  toxgr_grp5_sw <- toupper(params_list$toxgr_grp5_sw %||% "N")
  if (toxgr_grp5_sw == "Y") {
    toxgr_grp_desc <- "Grades 3/4 and Grade 5 reported separately"
    grades_label   <- "Grades 3/4"
  } else {
    toxgr_grp_desc <- "Grades 3/4/5 reported together"
    grades_label   <- "Grades 3/4/5"
  }

  # MedDRA key description (SAS lines 50-58)
  meddra_sw <- toupper(params_list$meddra %||% "N")
  if (meddra_sw == "Y") {
    key_desc <- "Body System or Organ Class and Dictionary-Derived Term (MedDRA)"
  } else {
    key_desc <- "Body System or Organ Class and Dictionary-Derived Term"
  }

  # ---- Helpers for building rows ----------------------------------------
  current_row <- 1L

  write_text_row <- function(text, style_name, merge_cols = 11L, ht = NULL) {
    if (is.null(ht)) {
      ht <- max(1, ceiling(nchar(text) / 110.5)) * 13.5
    }
    openxlsx::writeData(wb, sheet, text,
                        startRow = current_row, startCol = 1L)
    if (merge_cols > 1L) {
      openxlsx::mergeCells(wb, sheet, cols = 1:merge_cols, rows = current_row)
    }
    st <- styles[[style_name]]
    if (!is.null(st)) {
      openxlsx::addStyle(wb, sheet, st,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = ht)
    current_row <<- current_row + 1L
  }

  # ===================================================================
  # Part 1: Front page narrative text (SAS lines 59-157)
  # ===================================================================
  write_text_row("", "Default", ht = 13.5)
  write_text_row(WBTITLE, "Header", ht = 18)
  write_text_row("", "Default", ht = 13.5)
  write_text_row(stringr::str_glue("NDA/BLA: {ndabla}"), "Default8", ht = 13.5)
  write_text_row(stringr::str_glue("Study: {studyid}"), "Default8", ht = 13.5)
  write_text_row(stringr::str_glue("Analysis run date: {rundate}"), "Default8", ht = 13.5)
  write_text_row("", "Default", ht = 13.5)

  # Section descriptions
  write_text_row(paste0(
    "1.  Toxicity Grade Summary by treatment arm. Counts and percentages of ",
    "subjects with adverse events summarized by All Grades, ", grades_label,
    ", individual grades 1-5, and missing toxicity grades."
  ), "Default10Wrap")

  write_text_row(paste0(
    "2 & 3.  Preferred Term (PT) Analysis by Toxicity Grade provides the ",
    "same columns as the Toxicity Grade Summary, with one row for each ",
    "preferred term. Section 2 is filterable. Section 3 is formatted for ",
    "printing, grouped by ", key_desc, "."
  ), "Default10Wrap")

  if (arm_count > 1L) {
    cmpvar_labels <- params_list$cmplabel %||% "comparison term"
    write_text_row(paste0(
      "4 & 5.  Comparison analysis of adverse events by ", cmpvar_labels,
      " across treatment arms, including risk difference, relative risk, ",
      "odds ratio, and Fisher's exact p-value. Section 4 is filterable. ",
      "Section 5 is formatted for printing."
    ), "Default10Wrap")
  }

  write_text_row("", "Default", ht = 13.5)

  # AETOXGR availability warning (SAS lines 110-114)
  aetoxgr_sw <- toupper(params_list$aetoxgr_sw %||% "Y")
  if (aetoxgr_sw != "Y") {
    write_text_row(paste0(
      "NOTE: The AETOXGR variable was not available in the AE dataset. ",
      "All toxicity grade columns will show zero counts."
    ), "Default10RedWrap")
    write_text_row("", "Default", ht = 13.5)
  }

  # Method and Calculations header (SAS lines 116-138)
  write_text_row("Method and Calculations", "SubHeader", ht = 16)
  write_text_row("", "Default", ht = 13.5)
  write_text_row(paste0(
    "Subject counts are the number of subjects who experienced at least one ",
    "adverse event at the stated level (e.g., preferred term, body system, ",
    "or overall). Percentages are based on the number of subjects in the ",
    "safety population for each treatment arm."
  ), "Default10Wrap")
  write_text_row("", "Default", ht = 13.5)

  # ===================================================================
  # Part 2: 2x2 contingency table example (SAS lines 162-198)
  # ===================================================================
  write_text_row("2x2 Contingency Table", "SubHeader", ht = 16)
  write_text_row("", "Default", ht = 13.5)
  write_text_row(paste0(
    "The following 2x2 table shows the layout used for risk calculations ",
    "in the comparison analysis:"
  ), "Default10Wrap")
  write_text_row("", "Default", ht = 13.5)

  tbl_style <- styles[["Table"]]
  # Header row of contingency table
  openxlsx::writeData(wb, sheet, "", startRow = current_row, startCol = 2L)
  openxlsx::writeData(wb, sheet, "Arm 1 (Treatment)",
                      startRow = current_row, startCol = 3L)
  openxlsx::writeData(wb, sheet, "Arm 2 (Control)",
                      startRow = current_row, startCol = 4L)
  for (cc in 2:4) {
    if (!is.null(tbl_style)) {
      openxlsx::addStyle(wb, sheet, tbl_style,
                         rows = current_row, cols = cc, stack = TRUE)
    }
  }
  current_row <- current_row + 1L

  # Data rows of contingency table
  openxlsx::writeData(wb, sheet, "Adverse event",
                      startRow = current_row, startCol = 2L)
  openxlsx::writeData(wb, sheet, "a", startRow = current_row, startCol = 3L)
  openxlsx::writeData(wb, sheet, "b", startRow = current_row, startCol = 4L)
  for (cc in 2:4) {
    if (!is.null(tbl_style)) {
      openxlsx::addStyle(wb, sheet, tbl_style,
                         rows = current_row, cols = cc, stack = TRUE)
    }
  }
  current_row <- current_row + 1L

  openxlsx::writeData(wb, sheet, "No adverse event",
                      startRow = current_row, startCol = 2L)
  openxlsx::writeData(wb, sheet, "c", startRow = current_row, startCol = 3L)
  openxlsx::writeData(wb, sheet, "d", startRow = current_row, startCol = 4L)
  for (cc in 2:4) {
    if (!is.null(tbl_style)) {
      openxlsx::addStyle(wb, sheet, tbl_style,
                         rows = current_row, cols = cc, stack = TRUE)
    }
  }
  current_row <- current_row + 1L
  write_text_row("", "Default", ht = 13.5)

  # ===================================================================
  # Part 3: Calculation definitions (SAS lines 208-261)
  # ===================================================================
  write_text_row("Calculations", "SubHeader", ht = 16)
  write_text_row("", "Default", ht = 13.5)

  calcs <- list(
    list("AE Subject Count (a)", "Number of subjects with the AE in this arm"),
    list("No-AE Subject Count (c)", "N - a, where N is the safety population"),
    list("AE Percentage (%)", "100 * a / N"),
    list("Risk (Arm 1)", "a / (a + c)"),
    list("Risk (Arm 2)", "b / (b + d)"),
    list("Risk Difference", "Risk(Arm1) - Risk(Arm2)"),
    list("Risk Difference CI",
         "Wald confidence interval: RD +/- z * sqrt(a*c/(a+c)^3 + b*d/(b+d)^3)"),
    list("Relative Risk", "Risk(Arm1) / Risk(Arm2)"),
    list("Relative Risk CI",
         "exp(ln(RR) +/- z * sqrt(1/a - 1/(a+c) + 1/b - 1/(b+d)))"),
    list("Odds Ratio", "(a*d) / (b*c)")
  )

  # Odds ratio CI source depends on cc_sw (SAS lines 250-260)
  cc_sw    <- as.integer(params_list$cc_sw %||% 0L)
  cc_whole <- params_list$cc_whole %||% ""
  if (cc_sw > 0L) {
    calcs <- append(calcs, list(
      list("Odds Ratio CI", "Exact conditional CI (mid-p adjusted)")
    ))
  } else {
    calcs <- append(calcs, list(
      list("Odds Ratio CI",
           "exp(ln(OR) +/- z * sqrt(1/a + 1/b + 1/c + 1/d))")
    ))
  }
  calcs <- append(calcs, list(
    list("P-value", "Fisher's exact test (two-sided)")
  ))

  for (calc in calcs) {
    openxlsx::writeData(wb, sheet, calc[[1]],
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1:3, rows = current_row)
    openxlsx::writeData(wb, sheet, calc[[2]],
                        startRow = current_row, startCol = 4L)
    openxlsx::mergeCells(wb, sheet, cols = 4:11, rows = current_row)
    st_wrap <- styles[["Default10Wrap"]]
    if (!is.null(st_wrap)) {
      openxlsx::addStyle(wb, sheet, st_wrap,
                         rows = current_row, cols = 1L, stack = TRUE)
      openxlsx::addStyle(wb, sheet, st_wrap,
                         rows = current_row, cols = 4L, stack = TRUE)
    }
    ht <- max(1, ceiling(nchar(calc[[2]]) / 80)) * 13.5
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = ht)
    current_row <- current_row + 1L
  }
  write_text_row("", "Default", ht = 13.5)

  # ===================================================================
  # Continuity correction note (SAS lines 273-309)
  # ===================================================================
  if (cc_sw > 0L) {
    write_text_row("Continuity Correction", "SubHeader", ht = 16)
    write_text_row("", "Default", ht = 13.5)
    if (cc_sw == 1L) {
      cc_text <- paste0(
        "A continuity correction of ", cc_whole,
        " was added to each cell of the 2x2 table when a cell had a count ",
        "of zero, enabling calculation of the odds ratio and its confidence ",
        "interval. Affected rows are marked with an asterisk (*)."
      )
    } else {
      cc_text <- paste0(
        "A continuity correction equal to the reciprocal of the opposite ",
        "arm's subject count was added to each cell of the 2x2 table when ",
        "a cell had a count of zero, enabling calculation of the odds ratio ",
        "and its confidence interval. Affected rows are marked with an ",
        "asterisk (*)."
      )
    }
    write_text_row(cc_text, "Default10Wrap")
    write_text_row("", "Default", ht = 13.5)
  }

  # ===================================================================
  # Part 4: Report settings (SAS lines 312-376)
  # ===================================================================
  write_text_row("Report Settings", "SubHeader", ht = 16)
  write_text_row("", "Default", ht = 13.5)

  settings <- list(
    list("NDA/BLA", ndabla),
    list("Study", studyid),
    list("Analysis run date", rundate),
    list("Custom datasets", params_list$custom_ds %||% "None"),
    list("Grouping / Subsetting", params_list$sl_gs_desc %||% "None"),
    list("Study analysis period", params_list$study_period %||% "N/A"),
    list("Toxicity grade grouping", toxgr_grp_desc)
  )

  if (arm_count > 1L) {
    settings <- append(settings, list(
      list("Treatment arm", params_list$trt_arm %||% ""),
      list("Control arm", params_list$ctrl_arm %||% ""),
      list("MedDRA version", params_list$ver %||% "N/A"),
      list("Comparison terms", params_list$cmplabel %||% ""),
      list("Comparison grades", params_list$cmpgr_label %||% ""),
      list("Continuity correction",
           if (cc_sw == 0L) "None"
           else if (cc_sw == 1L) paste0("Constant: ", cc_whole)
           else "Reciprocal of opposite arm"),
      list("Sort by", params_list$sortlabel %||% "N/A")
    ))
  }

  for (setting in settings) {
    openxlsx::writeData(wb, sheet, setting[[1]],
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = 1:3, rows = current_row)
    openxlsx::writeData(wb, sheet, setting[[2]],
                        startRow = current_row, startCol = 4L)
    openxlsx::mergeCells(wb, sheet, cols = 4:11, rows = current_row)
    st10 <- styles[["Default10Wrap"]]
    if (!is.null(st10)) {
      openxlsx::addStyle(wb, sheet, st10,
                         rows = current_row, cols = 1L, stack = TRUE)
      openxlsx::addStyle(wb, sheet, st10,
                         rows = current_row, cols = 4L, stack = TRUE)
    }
    current_row <- current_row + 1L
  }

  # Cross-over study note in red (SAS lines 363-376)
  cross_sw <- toupper(params_list$cross_sw %||% "N")
  if (cross_sw == "Y") {
    write_text_row("", "Default", ht = 13.5)
    write_text_row(paste0(
      "NOTE: This is a cross-over study design. Subjects may appear in ",
      "more than one treatment arm. Risk statistics and p-values should ",
      "be interpreted with caution."
    ), "Default10RedWrap")
  }

  # ===================================================================
  # Print setup (SAS lines 394-418)
  # ===================================================================
  openxlsx::pageSetup(wb, sheet, orientation = "portrait",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  openxlsx::setHeaderFooter(
    wb, sheet,
    footer = c(NA, "Page &P of &N", NA)
  )

  invisible(NULL)
}

# ==============================================================================
# out_pt_1
# ------------------------------------------------------------------------------
# Replaces SAS %out_pt_1 macro (SAS lines 444-615).
# Adds the "1 Toxicity Grade Summary" worksheet with header, 3-tier column
# headers, styled data rows, footer notes, and print settings.
#
# @param wb          An openxlsx Workbook object.
# @param pt_1_output Data frame of toxicity grade summary data.
# @param params_list Named list of study parameters.
# @param rpt_key     Data frame of report key metadata.
# @param rpt_missing Data frame of missing toxicity grade counts per arm.
# @param styles      Named list of openxlsx Style objects.
# @return Invisible NULL. Worksheet is added as a side effect.
# ==============================================================================
out_pt_1 <- function(wb, pt_1_output, params_list, rpt_key, rpt_missing, styles) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(pt_1_output) || nrow(pt_1_output) == 0L) {
    cli::cli_abort("{.arg pt_1_output} must be a non-empty data frame.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  sheet <- "1 Toxicity Grade Summary"
  openxlsx::addWorksheet(wb, sheet)

  ndabla    <- params_list$ndabla %||% ""
  studyid   <- params_list$studyid %||% ""
  rundate   <- params_list$rundate %||% format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")
  arm_count <- as.integer(params_list$arm_count %||% 1L)
  arm_names <- params_list$arm_names %||% character(0)
  arm_ns    <- params_list$arm_ns %||% integer(0)

  # ---- Column widths (SAS %ws keycolwidth pattern) -----------------------
  # Key column (total) is wide; numeric columns are narrow
  n_data_cols <- ncol(pt_1_output) - 1L
  openxlsx::setColWidths(wb, sheet,
                         cols = seq_len(ncol(pt_1_output)),
                         widths = c(35, rep(8, n_data_cols)))

  # ---- Header section (SAS lines 465-515) --------------------------------
  current_row <- 1L

  header_data <- dplyr::tibble(
    group = c("title", "blank", "subtitle", "subtitle", "subtitle", "blank",
              "default", "default", "blank"),
    data  = c(
      "Toxicity Grade Summary",
      "",
      paste0("NDA/BLA: ", ndabla),
      paste0("Study: ", studyid),
      paste0("Analysis run date: ", rundate),
      "",
      paste0(
        "Subjects with adverse events summarized by toxicity grade. ",
        "Subject counts are the number of subjects with at least one ",
        "adverse event at the overall level. Percentages are based on ",
        "the safety population for each treatment arm."
      ),
      "",
      ""
    )
  )
  current_row <- write_header(wb, sheet, header_data, styles, start_row = current_row)

  # ---- Three-tier column headers via build_column_headers -----------------
  current_row <- build_column_headers(
    wb, sheet, params_list, rpt_key, styles, start_row = current_row
  )

  # ---- Data table (SAS lines 561-579) ------------------------------------
  # Prepare display data: format missing values as "." using dplyr pipelines
  # (tidyverse mandate: dplyr over base R for all data manipulation)
  pt_1_display <- pt_1_output |>
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {
      dplyr::if_else(is.na(x), ".", as.character(x))
    }))

  # Use write_data_table() from xml_output.R for auto-styled data writing.
  # This replaces the SAS %wsdata macro pattern by leveraging the shared
  # utility module's style-detection logic.
  data_start_row <- current_row
  current_row <- tryCatch(
    write_data_table(wb, sheet, pt_1_display, styles,
                     start_row = data_start_row, fmt = FALSE, sort_var = NULL),
    error = function(e) {
      # Fallback: manual cell-by-cell writing if utility is incompatible
      cli::cli_warn("write_data_table() fallback: {e$message}")
      cur_row <- data_start_row
      col_names_fb <- colnames(pt_1_output)
      n_rows_fb <- nrow(pt_1_output)
      for (ri in seq_len(n_rows_fb)) {
        is_bottom <- (ri == n_rows_fb)
        for (ci in seq_along(col_names_fb)) {
          cname <- col_names_fb[ci]
          val <- pt_1_output[[cname]][ri]
          if (is.numeric(val) && is.na(val)) {
            val <- "."
          } else if (is.character(val) && !is.na(val) && val %in% c("", "I")) {
            val <- "."
          }
          openxlsx::writeData(wb, sheet, val,
                              startRow = cur_row, startCol = ci)
          sid <- dplyr::case_when(
            ci == 1L ~ "D_BLR",
            stringr::str_detect(cname, "(?i)^pct") ~ "D1_R1_BR",
            TRUE ~ "D0_R2_BL"
          )
          if (is_bottom) sid <- stringr::str_c(sid, "B")
          st <- styles[[sid]]
          if (!is.null(st)) {
            openxlsx::addStyle(wb, sheet, st,
                               rows = cur_row, cols = ci, stack = TRUE)
          }
        }
        cur_row <- cur_row + 1L
      }
      cur_row
    }
  )

  # ---- Footer / NOTES section (SAS lines 534-551) -----------------------
  current_row <- current_row + 1L

  # Build notes as tibble rows, then combine with bind_rows (tidyverse pattern)
  base_notes <- dplyr::tibble(
    note = c("NOTES:",
             "Subjects are from the safety population.",
             "Subject counts represent subjects with at least one adverse event.")
  )

  # Check rpt_missing for arms with missing toxicity grades
  has_missing <- FALSE
  if (!is.null(rpt_missing) && nrow(rpt_missing) > 0L) {
    miss_sums <- rpt_missing$missing_count
    if (any(!is.na(miss_sums) & miss_sums > 0L)) {
      has_missing <- TRUE
    }
  }

  missing_notes <- if (has_missing) {
    dplyr::tibble(
      note = c("Some subjects had adverse events with missing toxicity grades.",
               "Missing toxicity grade counts are shown in the last column.")
    )
  } else {
    dplyr::tibble(note = character(0))
  }

  all_notes <- dplyr::bind_rows(base_notes, missing_notes)
  notes <- all_notes$note

  for (note in notes) {
    openxlsx::writeData(wb, sheet, note,
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(ncol(pt_1_output)),
                         rows = current_row)
    st_note <- styles[["Default8"]]
    if (!is.null(st_note)) {
      openxlsx::addStyle(wb, sheet, st_note,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    current_row <- current_row + 1L
  }

  # ---- Worksheet settings (SAS lines 584-598) ----------------------------
  openxlsx::pageSetup(wb, sheet, orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  openxlsx::setHeaderFooter(
    wb, sheet,
    footer = c(NA, "Page &P of &N", NA)
  )

  invisible(NULL)
}

# ==============================================================================
# out_pt_2
# ------------------------------------------------------------------------------
# Replaces SAS %out_pt_2 macro (SAS lines 621-894).
# Adds a "PT Analysis by Tox. Grade" worksheet. Called twice:
#   fmt=FALSE  -> "2 PT Analysis by Tox. Grade V1"  (filterable, autofilter)
#   fmt=TRUE   -> "3 PT Analysis by Tox. Grade V2"  (formatted for printing)
#
# @param wb             An openxlsx Workbook object.
# @param pt_2_output    Data frame of PT analysis data (flat, filterable).
# @param pt_2_output_fmt Data frame of PT analysis data (formatted with header rows).
#                        Only used when fmt=TRUE.
# @param params_list    Named list of study parameters.
# @param rpt_key        Data frame of report key metadata.
# @param rpt_missing    Data frame of missing toxicity grade counts per arm.
# @param styles         Named list of openxlsx Style objects.
# @param fmt            Logical. FALSE = filterable V1, TRUE = formatted V2.
# @return Invisible NULL. Worksheet is added as a side effect.
# ==============================================================================
out_pt_2 <- function(wb, pt_2_output, pt_2_output_fmt = NULL, params_list,
                     rpt_key, rpt_missing, styles, fmt = FALSE) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(pt_2_output) || nrow(pt_2_output) == 0L) {
    cli::cli_abort("{.arg pt_2_output} must be a non-empty data frame.")
  }
  if (isTRUE(fmt) && !is.null(pt_2_output_fmt) && !is.data.frame(pt_2_output_fmt)) {
    cli::cli_abort("{.arg pt_2_output_fmt} must be a data frame when fmt=TRUE.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  sheet <- if (fmt) "3 PT Analysis by Tox. Grade V2" else "2 PT Analysis by Tox. Grade V1"
  openxlsx::addWorksheet(wb, sheet)

  ndabla    <- params_list$ndabla %||% ""
  studyid   <- params_list$studyid %||% ""
  rundate   <- params_list$rundate %||% format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")

  # Choose data source based on fmt flag
  data_out <- if (fmt && !is.null(pt_2_output_fmt)) pt_2_output_fmt else pt_2_output

  # ---- Column widths ----
  n_cols <- ncol(data_out)
  openxlsx::setColWidths(wb, sheet,
                         cols = seq_len(n_cols),
                         widths = c(35, rep(8, n_cols - 1L)))

  # ---- Header (SAS lines 644-700) ----
  current_row <- 1L

  header_data <- dplyr::tibble(
    group = c("title", "blank", "subtitle", "subtitle", "subtitle", "blank",
              "default", "blank"),
    data  = c(
      "Preferred Term Analysis by Toxicity Grade",
      "",
      paste0("NDA/BLA: ", ndabla),
      paste0("Study: ", studyid),
      paste0("Analysis run date: ", rundate),
      "",
      paste0(
        "Adverse events by preferred term and toxicity grade. ",
        "Subject counts are the number of subjects with at least one ",
        "adverse event at the stated preferred term. Percentages are ",
        "based on the safety population for each treatment arm."
      ),
      ""
    )
  )
  current_row <- write_header(wb, sheet, header_data, styles, start_row = current_row)

  # ---- Three-tier column headers ----
  col_header_start <- current_row
  current_row <- build_column_headers(
    wb, sheet, params_list, rpt_key, styles, start_row = current_row
  )

  # ---- Data table (SAS lines 750-850) ----
  data_start_row <- current_row
  col_names <- colnames(data_out)
  n_rows <- nrow(data_out)

  if (fmt && !is.null(pt_2_output_fmt)) {
    # Formatted V2: Use annotate_data() + write_annotated_data() from xml_output.R.
    # This replaces the SAS %annotate (data-to-annotation conversion) and %markup
    # (annotation-to-Excel rendering) macro pattern. annotate_data() creates a
    # long-format tibble with Row/Data/Type/varname/bottom/Height/StyleID/
    # MergeAcross/MergeDown columns. write_annotated_data() writes it with styles,
    # merged cells, row heights, and indent handling (replacing ~!~!~! markers).
    annotated <- tryCatch(
      annotate_data(data_out),
      error = function(e) {
        cli::cli_warn("annotate_data() failed: {e$message}; using manual write")
        NULL
      }
    )
    if (!is.null(annotated)) {
      current_row <- tryCatch(
        write_annotated_data(wb, sheet, annotated, styles,
                             start_row = data_start_row),
        error = function(e) {
          cli::cli_warn("write_annotated_data() failed: {e$message}; using manual write")
          NULL
        }
      )
    }
    # Fallback to manual write if annotation pipeline failed
    if (is.null(annotated) || is.null(current_row)) {
      current_row <- data_start_row
      for (ri in seq_len(n_rows)) {
        is_bottom <- (ri == n_rows)
        row_data <- data_out[ri, ]
        is_header_row <- ("header" %in% col_names && isTRUE(row_data$header == 1L))
        if (is_header_row) {
          header_val <- row_data[[1]]
          openxlsx::writeData(wb, sheet, header_val,
                              startRow = current_row, startCol = 1L)
          openxlsx::mergeCells(wb, sheet, cols = seq_len(n_cols), rows = current_row)
          st_dh <- styles[["DataHeader"]]
          if (!is.null(st_dh)) {
            openxlsx::addStyle(wb, sheet, st_dh,
                               rows = current_row, cols = 1L, stack = TRUE)
          }
        } else {
          for (ci in seq_along(col_names)) {
            cname <- col_names[ci]
            if (cname == "header") next
            val <- row_data[[cname]]
            if (is.numeric(val) && is.na(val)) val <- "."
            if (is.character(val) && !is.na(val) && val %in% c("", "I")) val <- "."
            if (ci == 1L && is.character(val) && !is.na(val)) {
              val <- stringr::str_pad(val, nchar(val) + 5L, side = "left")
            }
            openxlsx::writeData(wb, sheet, val,
                                startRow = current_row, startCol = ci)
            sid <- dplyr::case_when(
              ci == 1L ~ "D_BLR",
              stringr::str_detect(cname, "(?i)^pct") ~ "D1_R1_BR",
              TRUE ~ "D0_R2_BL"
            )
            if (is_bottom) sid <- stringr::str_c(sid, "B")
            st <- styles[[sid]]
            if (!is.null(st)) {
              openxlsx::addStyle(wb, sheet, st,
                                 rows = current_row, cols = ci, stack = TRUE)
            }
          }
        }
        current_row <- current_row + 1L
      }
    }
  } else {
    # V1 (filterable): Use write_data_table() from xml_output.R for auto-styled
    # data writing, replacing the SAS %wsdata macro pattern.
    pt_2_display <- data_out |>
      dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {
        dplyr::if_else(is.na(x), ".", as.character(x))
      }))
    current_row <- tryCatch(
      write_data_table(wb, sheet, pt_2_display, styles,
                       start_row = data_start_row, fmt = FALSE, sort_var = NULL),
      error = function(e) {
        cli::cli_warn("write_data_table() V1 fallback: {e$message}")
        cur_row <- data_start_row
        for (ri in seq_len(n_rows)) {
          is_bottom <- (ri == n_rows)
          for (ci in seq_along(col_names)) {
            cname <- col_names[ci]
            val <- data_out[[cname]][ri]
            if (is.numeric(val) && is.na(val)) val <- "."
            if (is.character(val) && !is.na(val) && val %in% c("", "I")) val <- "."
            openxlsx::writeData(wb, sheet, val,
                                startRow = cur_row, startCol = ci)
            sid <- dplyr::case_when(
              ci == 1L ~ "D_BLR",
              stringr::str_detect(cname, "(?i)^pct") ~ "D1_R1_BR",
              TRUE ~ "D0_R2_BL"
            )
            if (is_bottom) sid <- stringr::str_c(sid, "B")
            st <- styles[[sid]]
            if (!is.null(st)) {
              openxlsx::addStyle(wb, sheet, st,
                                 rows = cur_row, cols = ci, stack = TRUE)
            }
          }
          cur_row <- cur_row + 1L
        }
        cur_row
      }
    )
  }
  data_end_row <- current_row - 1L

  # ---- Footer / NOTES (identical to pt_1) ----
  current_row <- current_row + 1L
  notes <- c(
    "NOTES:",
    "Subjects are from the safety population.",
    "Subject counts represent subjects with at least one adverse event at the stated preferred term."
  )
  has_missing <- FALSE
  if (!is.null(rpt_missing) && nrow(rpt_missing) > 0L) {
    miss_sums <- rpt_missing$missing_count
    if (any(!is.na(miss_sums) & miss_sums > 0L)) {
      has_missing <- TRUE
    }
  }
  if (has_missing) {
    notes <- c(notes,
      "Some subjects had adverse events with missing toxicity grades.",
      "Missing toxicity grade counts are shown in the last column."
    )
  }
  for (note in notes) {
    openxlsx::writeData(wb, sheet, note,
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_cols), rows = current_row)
    st_note <- styles[["Default8"]]
    if (!is.null(st_note)) {
      openxlsx::addStyle(wb, sheet, st_note,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    current_row <- current_row + 1L
  }

  # ---- Worksheet settings ----
  if (!fmt) {
    # V1: autofilter, conditional formatting, freeze panes
    openxlsx::addFilter(wb, sheet, rows = data_start_row - 1L,
                        cols = seq_len(n_cols))
    openxlsx::freezePane(wb, sheet, firstActiveRow = data_start_row,
                         firstActiveCol = 2L)
    # Alternating row shading for filterable view
    if (data_end_row >= data_start_row) {
      openxlsx::conditionalFormatting(
        wb, sheet,
        cols  = seq_len(n_cols),
        rows  = data_start_row:data_end_row,
        rule  = "MOD(ROW(),2)=0",
        type  = "expression",
        style = openxlsx::createStyle(fgFill = "#C0C0C0")
      )
    }
  } else {
    # V2: freeze panes only
    openxlsx::freezePane(wb, sheet, firstActiveRow = data_start_row,
                         firstActiveCol = 2L)
  }

  openxlsx::pageSetup(wb, sheet, orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  openxlsx::setHeaderFooter(
    wb, sheet,
    footer = c(NA, "Page &P of &N", NA)
  )

  invisible(NULL)
}

# ==============================================================================
# out_pt_3
# ------------------------------------------------------------------------------
# Replaces SAS %out_pt_3 macro (SAS lines 901-1417).
# Adds a MedDRA comparison analysis worksheet. Called twice:
#   fmt=FALSE -> "4 {report} V1" (filterable)
#   fmt=TRUE  -> "5 {report} V2" (formatted for printing)
# Only invoked when arm_count > 1.
#
# @param wb                An openxlsx Workbook object.
# @param pt_3_output       Data frame of comparison analysis data (flat).
# @param pt_3_output_fmt   Data frame of comparison analysis data (formatted).
# @param pt_3_output_cc_ind Vector/column indicating continuity correction rows.
# @param params_list       Named list of study parameters.
# @param rpt_key           Data frame of report key metadata.
# @param rpt_missing       Data frame of missing toxicity grade counts.
# @param styles            Named list of openxlsx Style objects.
# @param fmt               Logical. FALSE = filterable V1, TRUE = formatted V2.
# @return Invisible NULL. Worksheet is added as a side effect.
# ==============================================================================
out_pt_3 <- function(wb, pt_3_output, pt_3_output_fmt = NULL,
                     pt_3_output_cc_ind = NULL, params_list,
                     rpt_key, rpt_missing, styles, fmt = FALSE) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.data.frame(pt_3_output) || nrow(pt_3_output) == 0L) {
    cli::cli_abort("{.arg pt_3_output} must be a non-empty data frame.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  # Determine worksheet name from report label (SAS lines 905-920)
  report <- params_list$cmp_report %||% "N-Term Analysis"
  sheet_num <- if (fmt) "5" else "4"
  sheet_ver <- if (fmt) "V2" else "V1"
  sheet <- paste0(sheet_num, " ", report, " ", sheet_ver)
  openxlsx::addWorksheet(wb, sheet)

  ndabla    <- params_list$ndabla %||% ""
  studyid   <- params_list$studyid %||% ""
  rundate   <- params_list$rundate %||% format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")
  arm_count <- as.integer(params_list$arm_count %||% 2L)
  cc_sw     <- as.integer(params_list$cc_sw %||% 0L)
  ae_rate_ci_sw <- toupper(params_list$ae_rate_ci_sw %||% "N")

  # Key title and subtitle from comparison parameters (SAS lines 930-960)
  key_title <- params_list$key_title %||% "Preferred Term"
  key_subtitle <- params_list$key_subtitle %||% ""
  cmpgr_label <- params_list$cmpgr_label %||% ""
  sortlabel <- params_list$sortlabel %||% ""

  # Treatment / control arm info
  trt_arm  <- params_list$trt_arm %||% "Treatment"
  ctrl_arm <- params_list$ctrl_arm %||% "Control"
  trt_n    <- params_list$trt_n %||% ""
  ctrl_n   <- params_list$ctrl_n %||% ""

  # Choose data source
  data_out <- if (fmt && !is.null(pt_3_output_fmt)) pt_3_output_fmt else pt_3_output
  col_names <- colnames(data_out)
  n_cols <- ncol(data_out)

  # ---- Column widths ----
  openxlsx::setColWidths(wb, sheet,
                         cols = seq_len(n_cols),
                         widths = c(35, rep(8, n_cols - 1L)))

  # ---- Header (SAS lines 979-1035) ----
  current_row <- 1L

  title_text <- paste0("Adverse Events ", report)
  subtitle_text <- paste0("by ", key_title)
  if (fmt && nchar(sortlabel) > 0L) {
    subtitle_text <- paste0(subtitle_text, "; Sorted by ", sortlabel)
  }

  header_data <- dplyr::tibble(
    group = c("title", "blank", "subtitle", "subtitle", "subtitle",
              "subtitle", "blank", "default", "blank"),
    data  = c(
      title_text,
      "",
      subtitle_text,
      paste0("NDA/BLA: ", ndabla),
      paste0("Study: ", studyid),
      paste0("Analysis run date: ", rundate),
      "",
      paste0(
        "Comparison of adverse events between treatment arms. Subject ",
        "counts are the number of subjects with at least one adverse event. ",
        "Percentages are based on the safety population for each arm."
      ),
      ""
    )
  )
  current_row <- write_header(wb, sheet, header_data, styles, start_row = current_row)

  # ---- Custom three-tier column headers (SAS lines 1128-1223) ----
  # Unlike pt_1/pt_2, pt_3 has a unique header structure for comparison stats

  hdr_row_1 <- current_row
  hdr_row_2 <- current_row + 1L
  hdr_row_3 <- current_row + 2L
  co_style  <- styles[["ColumnOutline"]]
  coi_style <- styles[["ColumnOutlineItalic"]]

  # Identify comparison columns from the data frame
  # Expected column groups: key cols, trt arm cols, ctrl arm cols, RD, RR, OR, P-value
  # The exact columns depend on data_out structure. We introspect from col_names.

  # Build a mapping of column positions by semantic role
  col_idx <- setNames(seq_along(col_names), col_names)

  # Key columns (first 1-2 columns: adverse_event or body_system + adverse_event)
  key_cols <- which(col_names %in% c("adverse_event", "body_system", "report_key",
                                      "soc", "hlgt", "hlt", "pt", "key1", "key2"))
  if (length(key_cols) == 0L) key_cols <- 1L
  key_end <- max(key_cols)

  # Locate treatment arm columns
  trt_cols  <- which(grepl("^trt_", col_names, ignore.case = TRUE))
  ctrl_cols <- which(grepl("^ctrl_", col_names, ignore.case = TRUE))

  # Locate statistic columns
  rd_col    <- which(col_names == "rd")
  rd_lb_col <- which(col_names %in% c("rd_cilb", "rd_lb"))
  rd_ub_col <- which(col_names %in% c("rd_ciub", "rd_ub"))
  rr_col    <- which(col_names == "rr")
  rr_lb_col <- which(col_names %in% c("rr_cilb", "rr_lb"))
  rr_ub_col <- which(col_names %in% c("rr_ciub", "rr_ub"))
  or_col    <- which(col_names %in% c("ort", "or"))
  or_lb_col <- which(col_names %in% c("or_cilb", "ort_cilb", "or_lb", "ort_lb"))
  or_ub_col <- which(col_names %in% c("or_ciub", "ort_ciub", "or_ub", "ort_ub"))
  p_col     <- which(col_names == "p_value")
  sort_col  <- which(col_names %in% c("sort_var", "sort_col", "sortvar"))
  cc_ind_col <- which(col_names == "cc_ind")

  # Row 1: Key labels with MergeDown=2, arm headers, stat group headers
  for (kc in key_cols) {
    lbl <- if (kc == 1L) key_title else col_names[kc]
    openxlsx::writeData(wb, sheet, lbl, startRow = hdr_row_1, startCol = kc)
    openxlsx::mergeCells(wb, sheet, cols = kc, rows = hdr_row_1:hdr_row_3)
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = kc, stack = TRUE)
    }
  }

  # Treatment arm header — merge across trt columns
  if (length(trt_cols) > 0L) {
    trt_label <- paste0(trt_arm, " (N=", trt_n, ")")
    openxlsx::writeData(wb, sheet, trt_label,
                        startRow = hdr_row_1, startCol = min(trt_cols))
    if (length(trt_cols) > 1L) {
      openxlsx::mergeCells(wb, sheet,
                           cols = min(trt_cols):max(trt_cols),
                           rows = hdr_row_1)
    }
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = min(trt_cols), stack = TRUE)
    }
  }

  # Control arm header — merge across ctrl columns
  if (length(ctrl_cols) > 0L) {
    ctrl_label <- paste0(ctrl_arm, " (N=", ctrl_n, ")")
    openxlsx::writeData(wb, sheet, ctrl_label,
                        startRow = hdr_row_1, startCol = min(ctrl_cols))
    if (length(ctrl_cols) > 1L) {
      openxlsx::mergeCells(wb, sheet,
                           cols = min(ctrl_cols):max(ctrl_cols),
                           rows = hdr_row_1)
    }
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = min(ctrl_cols), stack = TRUE)
    }
  }

  # Risk Difference header
  rd_all <- sort(c(rd_col, rd_lb_col, rd_ub_col))
  if (length(rd_all) > 0L) {
    openxlsx::writeData(wb, sheet, "Risk Difference",
                        startRow = hdr_row_1, startCol = min(rd_all))
    if (length(rd_all) > 1L) {
      openxlsx::mergeCells(wb, sheet,
                           cols = min(rd_all):max(rd_all), rows = hdr_row_1)
    }
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = min(rd_all), stack = TRUE)
    }
  }

  # Relative Risk header
  rr_all <- sort(c(rr_col, rr_lb_col, rr_ub_col))
  if (length(rr_all) > 0L) {
    openxlsx::writeData(wb, sheet, "Relative Risk",
                        startRow = hdr_row_1, startCol = min(rr_all))
    if (length(rr_all) > 1L) {
      openxlsx::mergeCells(wb, sheet,
                           cols = min(rr_all):max(rr_all), rows = hdr_row_1)
    }
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = min(rr_all), stack = TRUE)
    }
  }

  # Odds Ratio header
  or_all <- sort(c(or_col, or_lb_col, or_ub_col))
  if (length(or_all) > 0L) {
    openxlsx::writeData(wb, sheet, "Odds Ratio",
                        startRow = hdr_row_1, startCol = min(or_all))
    if (length(or_all) > 1L) {
      openxlsx::mergeCells(wb, sheet,
                           cols = min(or_all):max(or_all), rows = hdr_row_1)
    }
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = min(or_all), stack = TRUE)
    }
  }

  # P-value header (MergeDown=2)
  if (length(p_col) > 0L) {
    openxlsx::writeData(wb, sheet, "P-value",
                        startRow = hdr_row_1, startCol = p_col[1])
    openxlsx::mergeCells(wb, sheet,
                         cols = p_col[1], rows = hdr_row_1:hdr_row_3)
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = hdr_row_1, cols = p_col[1], stack = TRUE)
    }
  }

  # Sort column header (MergeDown=2, Italic if fmt)
  if (length(sort_col) > 0L && fmt) {
    openxlsx::writeData(wb, sheet, "Sort",
                        startRow = hdr_row_1, startCol = sort_col[1])
    openxlsx::mergeCells(wb, sheet,
                         cols = sort_col[1], rows = hdr_row_1:hdr_row_3)
    sort_style <- if (!is.null(coi_style)) coi_style else co_style
    if (!is.null(sort_style)) {
      openxlsx::addStyle(wb, sheet, sort_style,
                         rows = hdr_row_1, cols = sort_col[1], stack = TRUE)
    }
  }

  # Row 2 sub-headers: comparison grade labels for arm groups, individual stat headers
  # Treatment arm sub-headers
  if (length(trt_cols) > 0L) {
    trt_sub_labels <- stringr::str_replace(col_names[trt_cols], "^trt_", "")
    for (i in seq_along(trt_cols)) {
      openxlsx::writeData(wb, sheet, trt_sub_labels[i],
                          startRow = hdr_row_2, startCol = trt_cols[i])
      openxlsx::mergeCells(wb, sheet,
                           cols = trt_cols[i], rows = hdr_row_2:hdr_row_3)
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = hdr_row_2, cols = trt_cols[i], stack = TRUE)
      }
    }
  }

  # Control arm sub-headers
  if (length(ctrl_cols) > 0L) {
    ctrl_sub_labels <- stringr::str_replace(col_names[ctrl_cols], "^ctrl_", "")
    for (i in seq_along(ctrl_cols)) {
      openxlsx::writeData(wb, sheet, ctrl_sub_labels[i],
                          startRow = hdr_row_2, startCol = ctrl_cols[i])
      openxlsx::mergeCells(wb, sheet,
                           cols = ctrl_cols[i], rows = hdr_row_2:hdr_row_3)
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = hdr_row_2, cols = ctrl_cols[i], stack = TRUE)
      }
    }
  }

  # Stat sub-headers (Row 3 level labels: Estimate, Lower CL, Upper CL)
  stat_label_map <- list(
    rd  = "Estimate", rr = "Estimate", ort = "Estimate", or = "Estimate",
    rd_cilb = "%Lower CL", rd_ciub = "%Upper CL",
    rd_lb = "Lower CL", rd_ub = "Upper CL",
    rr_cilb = "Lower CL", rr_ciub = "Upper CL",
    rr_lb = "Lower CL", rr_ub = "Upper CL",
    or_cilb = "Lower CL", or_ciub = "Upper CL",
    ort_cilb = "Lower CL", ort_ciub = "Upper CL",
    or_lb = "Lower CL", or_ub = "Upper CL",
    ort_lb = "Lower CL", ort_ub = "Upper CL"
  )

  for (cn in col_names) {
    ci <- col_idx[[cn]]
    lbl <- stat_label_map[[cn]]
    if (!is.null(lbl)) {
      openxlsx::writeData(wb, sheet, lbl,
                          startRow = hdr_row_3, startCol = ci)
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = hdr_row_3, cols = ci, stack = TRUE)
      }
    }
  }

  current_row <- hdr_row_3 + 1L

  # ---- Data table (SAS lines 1242-1317) ----
  data_start_row <- current_row
  n_rows <- nrow(data_out)

  for (ri in seq_len(n_rows)) {
    is_bottom <- (ri == n_rows)
    row_data <- data_out[ri, ]

    # Detect header rows in formatted version
    is_header_row <- FALSE
    if (fmt && "header" %in% col_names) {
      is_header_row <- isTRUE(row_data$header == 1L)
    }

    if (is_header_row) {
      header_val <- row_data[[1]]
      openxlsx::writeData(wb, sheet, header_val,
                          startRow = current_row, startCol = 1L)
      openxlsx::mergeCells(wb, sheet, cols = seq_len(n_cols), rows = current_row)
      st_dh <- styles[["DataHeader"]]
      if (!is.null(st_dh)) {
        openxlsx::addStyle(wb, sheet, st_dh,
                           rows = current_row, cols = 1L, stack = TRUE)
      }
    } else {
      # Check cc_ind for this row (continuity correction indicator)
      row_cc_ind <- FALSE
      if (!is.null(pt_3_output_cc_ind) && ri <= length(pt_3_output_cc_ind)) {
        row_cc_ind <- isTRUE(pt_3_output_cc_ind[ri] == 1L |
                             pt_3_output_cc_ind[ri] == "*")
      } else if ("cc_ind" %in% col_names) {
        cc_val <- row_data$cc_ind
        row_cc_ind <- isTRUE(!is.na(cc_val) && cc_val %in% c(1L, "*", "1"))
      }

      for (ci in seq_along(col_names)) {
        cname <- col_names[ci]
        if (cname %in% c("header", "cc_ind")) next

        val <- row_data[[cname]]

        # Handle missing values
        if (is.numeric(val) && is.na(val)) {
          val <- "."
        } else if (is.character(val) && !is.na(val) && val %in% c("", "I")) {
          val <- "."
        }

        # Indent text in formatted version (SAS ~!~!~! markers → str_pad)
        if (fmt && ci == 1L && !is_header_row && is.character(val) && !is.na(val)) {
          val <- stringr::str_pad(val, nchar(val) + 5L, side = "left")
        }

        openxlsx::writeData(wb, sheet, val,
                            startRow = current_row, startCol = ci)

        # Style determination (SAS lines 1242-1317)
        sid <- determine_pt3_style(
          cname, ci, key_cols, trt_cols, ctrl_cols,
          rd_col, rd_lb_col, rd_ub_col,
          rr_col, rr_lb_col, rr_ub_col,
          or_col, or_lb_col, or_ub_col,
          p_col, sort_col,
          row_cc_ind, val, fmt
        )

        # Bottom-row border suffix
        if (is_bottom && !grepl("B$", sid)) {
          sid <- paste0(sid, "B")
        }

        # Highlighted sort column in formatted version
        if (fmt && length(sort_col) > 0L && ci %in% sort_col) {
          sid <- gsub("^(D[^_]*)", "\\1H", sid)
          if (!grepl("H", sid)) sid <- paste0(sid, "H")
        }

        st <- styles[[sid]]
        if (is.null(st)) {
          # Fallback: try without highlight
          sid_clean <- stringr::str_replace(sid, "H", "")
          st <- styles[[sid_clean]]
        }
        if (!is.null(st)) {
          openxlsx::addStyle(wb, sheet, st,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
    }
    current_row <- current_row + 1L
  }
  data_end_row <- current_row - 1L

  # ---- Footer / NOTES (SAS lines 1052-1093) ----
  current_row <- current_row + 1L
  notes <- c(
    "NOTES:",
    "Results are for data exploration purposes only.",
    "Subjects are from the safety population."
  )
  has_missing <- FALSE
  if (!is.null(rpt_missing) && nrow(rpt_missing) > 0L) {
    miss_sums <- rpt_missing$missing_count
    if (any(!is.na(miss_sums) & miss_sums > 0L)) has_missing <- TRUE
  }
  if (has_missing) {
    notes <- c(notes,
      "Some subjects had adverse events with missing toxicity grades."
    )
  }
  notes <- c(notes,
    "Confidence intervals are at the 95% level.",
    "P-value is from Fisher's exact test (two-sided)."
  )
  if (cc_sw > 0L) {
    notes <- c(notes,
      "* Rows marked with an asterisk used continuity correction for odds ratio calculation."
    )
  }
  for (note in notes) {
    openxlsx::writeData(wb, sheet, note,
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_cols), rows = current_row)
    st_note <- styles[["Default8"]]
    if (!is.null(st_note)) {
      openxlsx::addStyle(wb, sheet, st_note,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    current_row <- current_row + 1L
  }

  # ---- Worksheet settings ----
  if (!fmt) {
    openxlsx::addFilter(wb, sheet, rows = data_start_row - 1L,
                        cols = seq_len(n_cols))
    openxlsx::freezePane(wb, sheet, firstActiveRow = data_start_row,
                         firstActiveCol = key_end + 1L)
    if (data_end_row >= data_start_row) {
      openxlsx::conditionalFormatting(
        wb, sheet,
        cols  = seq_len(n_cols),
        rows  = data_start_row:data_end_row,
        rule  = "MOD(ROW(),2)=0",
        type  = "expression",
        style = openxlsx::createStyle(fgFill = "#C0C0C0")
      )
    }
  } else {
    openxlsx::freezePane(wb, sheet, firstActiveRow = data_start_row,
                         firstActiveCol = key_end + 1L)
  }

  openxlsx::pageSetup(wb, sheet, orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  openxlsx::setHeaderFooter(
    wb, sheet,
    footer = c(NA, "Page &P of &N", NA)
  )

  invisible(NULL)
}

# ==============================================================================
# determine_pt3_style
# ------------------------------------------------------------------------------
# Internal helper to determine the openxlsx style ID for a cell in the pt_3
# data table based on column semantics, cc_ind, and value magnitude.
# ==============================================================================
determine_pt3_style <- function(cname, ci, key_cols, trt_cols, ctrl_cols,
                                 rd_col, rd_lb_col, rd_ub_col,
                                 rr_col, rr_lb_col, rr_ub_col,
                                 or_col, or_lb_col, or_ub_col,
                                 p_col, sort_col,
                                 row_cc_ind, val, fmt) {
  # Key / text columns
  if (ci %in% key_cols) {
    return("D_BLR")
  }

  # Treatment/control arm count columns
  if (ci %in% trt_cols || ci %in% ctrl_cols) {
    if (stringr::str_detect(cname, "(?i)^(trt_|ctrl_)pct")) {
      return("D1_R1_BR")
    }
    if (stringr::str_detect(cname, "(?i)^(trt_|ctrl_)ci?lb")) {
      return("LCL_BL")
    }
    if (stringr::str_detect(cname, "(?i)^(trt_|ctrl_)ci?ub")) {
      # Scientific notation for very large values
      if (is.numeric(val) && !is.na(val) && abs(val) > 1e6) {
        return("UCLSN_BR")
      }
      return("UCL_BR")
    }
    return("D0_R2_BL")
  }

  # Risk Difference
  if (ci %in% rd_col)    return("D1_R2_BL")
  if (ci %in% rd_lb_col) return("LCL_BL")
  if (ci %in% rd_ub_col) return("UCL_BR")

  # Relative Risk
  if (ci %in% rr_col) {
    if (row_cc_ind) return("D1_R2S_BL")
    return("D1_R2_BL")
  }
  if (ci %in% rr_lb_col) return("LCL_BL")
  if (ci %in% rr_ub_col) {
    if (is.numeric(val) && !is.na(val) && abs(val) > 1e6) return("UCLSN_BR")
    return("UCL_BR")
  }

  # Odds Ratio
  if (ci %in% or_col) {
    if (row_cc_ind) return("D1_R2S_BL")
    return("D1_R2_BL")
  }
  if (ci %in% or_lb_col) return("LCL_BL")
  if (ci %in% or_ub_col) {
    if (is.numeric(val) && !is.na(val) && abs(val) > 1e6) return("UCLSN_BR")
    return("UCL_BR")
  }

  # P-value
  if (ci %in% p_col) return("D2_R2_BLR")

  # Sort column
  if (ci %in% sort_col) {
    if (fmt) return("D1_R2_BLR")
    return("D1_R2_BLR")
  }

  # Default fallback
  "D0_R2_BL"
}

# ==============================================================================
# out_err
# ------------------------------------------------------------------------------
# Replaces SAS %out_err macro (SAS lines 1423-2082).
# Adds the "Data Check Summary" worksheet containing:
#   - Header with study metadata
#   - Subject Validation table (DM subject counts per arm)
#   - Data Validation section (excluded AEs, conditional)
#   - "By Term" detail table (body system/term + arm event counts)
#   - MedDRA Matching section (conditional on meddra == "Y")
#   - Missing Toxicity Grades section
#   - Conditional formatting for alternating rows
#
# @param wb                An openxlsx Workbook object.
# @param params_list       Named list of study parameters.
# @param dm_validation_data Data frame of DM subject validation data.
# @param ae_matching_data  Optional data frame of MedDRA matching data.
# @param styles            Named list of openxlsx Style objects.
# @return Invisible NULL. Worksheet is added as a side effect.
# ==============================================================================
out_err <- function(wb, params_list, dm_validation_data,
                    ae_matching_data = NULL, styles) {
  if (!inherits(wb, "Workbook")) {
    cli::cli_abort("{.arg wb} must be an openxlsx Workbook object.")
  }
  if (!is.list(params_list)) {
    cli::cli_abort("{.arg params_list} must be a named list.")
  }
  if (!is.list(styles)) {
    cli::cli_abort("{.arg styles} must be a named list of openxlsx Style objects.")
  }

  sheet <- "Data Check Summary"
  openxlsx::addWorksheet(wb, sheet)

  ndabla    <- params_list$ndabla %||% ""
  studyid   <- params_list$studyid %||% ""
  rundate   <- params_list$rundate %||% format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")
  arm_count <- as.integer(params_list$arm_count %||% 1L)
  arm_names <- params_list$arm_names %||% character(0)
  arm_ns    <- params_list$arm_ns %||% integer(0)
  meddra_sw <- toupper(params_list$meddra %||% "N")
  vld_sw    <- toupper(params_list$vld_sw %||% "N")

  # Column widths for Data Check Summary
  openxlsx::setColWidths(wb, sheet, cols = 1:20,
                         widths = c(35, rep(10, 19)))

  current_row <- 1L

  # ---- Helper: write merged text row ----
  write_err_text <- function(text, style_name, merge_cols = 12L, ht = NULL) {
    if (is.null(ht)) {
      ht <- max(1, ceiling(nchar(text) / 110.5)) * 13.5
    }
    openxlsx::writeData(wb, sheet, text,
                        startRow = current_row, startCol = 1L)
    if (merge_cols > 1L) {
      openxlsx::mergeCells(wb, sheet, cols = 1:merge_cols, rows = current_row)
    }
    st <- styles[[style_name]]
    if (!is.null(st)) {
      openxlsx::addStyle(wb, sheet, st,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    openxlsx::setRowHeights(wb, sheet, rows = current_row, heights = ht)
    current_row <<- current_row + 1L
  }

  # ===================================================================
  # Header (SAS lines 1449-1484)
  # ===================================================================
  write_err_text("", "Default", ht = 13.5)
  write_err_text("Adverse Events Data Check Summary", "Header", ht = 18)
  write_err_text("", "Default", ht = 13.5)
  write_err_text(paste0("NDA/BLA: ", ndabla), "Default8", ht = 13.5)
  write_err_text(paste0("Study: ", studyid), "Default8", ht = 13.5)
  write_err_text(paste0("Analysis run date: ", rundate), "Default8", ht = 13.5)
  write_err_text("", "Default", ht = 13.5)

  # ===================================================================
  # Subject Validation Table (SAS lines 1498-1710)
  # ===================================================================
  write_err_text("Subject Validation", "SubHeader", ht = 16)
  write_err_text("", "Default", ht = 13.5)
  write_err_text(paste0(
    "Summary of DM domain subjects by treatment arm. Counts include all ",
    "subjects randomized, screening failures, unassigned subjects, subjects ",
    "not in the safety population, and subjects with missing start/end dates."
  ), "Default10Wrap")
  write_err_text("", "Default", ht = 13.5)

  # Build Subject Validation columns: Description + per-arm (Count, %) + Total
  if (!is.null(dm_validation_data) && nrow(dm_validation_data) > 0L) {
    # Prepare DM validation data: filter valid rows, select display columns
    dm_validation_data <- dm_validation_data |>
      dplyr::filter(!is.na(dm_validation_data[[1]])) |>
      dplyr::select(dplyr::everything())

    dm_col_names <- colnames(dm_validation_data)
    dm_n_cols <- ncol(dm_validation_data)

    # Column headers - Row 1: section title merged across all
    openxlsx::writeData(wb, sheet, "Subject Validation Summary",
                        startRow = current_row, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(dm_n_cols),
                         rows = current_row)
    co_style <- styles[["ColumnOutline"]]
    if (!is.null(co_style)) {
      openxlsx::addStyle(wb, sheet, co_style,
                         rows = current_row, cols = 1L, stack = TRUE)
    }
    current_row <- current_row + 1L

    # Column headers - Row 2: arm names
    for (ci in seq_along(dm_col_names)) {
      openxlsx::writeData(wb, sheet, dm_col_names[ci],
                          startRow = current_row, startCol = ci)
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = current_row, cols = ci, stack = TRUE)
      }
    }
    current_row <- current_row + 1L

    # Data rows
    dm_n_rows <- nrow(dm_validation_data)
    for (ri in seq_len(dm_n_rows)) {
      is_bottom <- (ri == dm_n_rows)
      for (ci in seq_along(dm_col_names)) {
        val <- dm_validation_data[[ci]][ri]
        if (is.numeric(val) && is.na(val)) val <- "."

        openxlsx::writeData(wb, sheet, val,
                            startRow = current_row, startCol = ci)

        # Style: description col -> D_BLR, count cols -> D0_R1_BL, pct cols -> D1_R1_BR
        sid <- dplyr::case_when(
          ci == 1L ~ "D_BLR",
          stringr::str_detect(dm_col_names[ci], "(?i)pct") ~ "D1_R1_BR",
          TRUE ~ "D0_R1_BL"
        )
        if (is_bottom) sid <- stringr::str_c(sid, "B")

        st <- styles[[sid]]
        if (!is.null(st)) {
          openxlsx::addStyle(wb, sheet, st,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
      current_row <- current_row + 1L
    }
  }

  write_err_text("", "Default", ht = 13.5)

  # ===================================================================
  # Data Validation / Excluded AEs (SAS lines ~1710-1785)
  # ===================================================================
  vld_err_data <- params_list$vld_err_data
  vld_term_data <- params_list$vld_term_data

  if (!is.null(vld_err_data) && is.data.frame(vld_err_data) &&
      nrow(vld_err_data) > 0L) {
    write_err_text("Data Validation", "SubHeader", ht = 16)
    write_err_text("", "Default", ht = 13.5)
    write_err_text(paste0(
      "The following adverse events were excluded from the analysis due to ",
      "data validation checks. Events may be excluded if the subject is not ",
      "in the safety population, the event falls outside the analysis period, ",
      "or the event fails other validation criteria."
    ), "Default10Wrap")
    write_err_text("", "Default", ht = 13.5)

    # Validation summary table
    vld_col_names <- colnames(vld_err_data)
    vld_n_cols <- ncol(vld_err_data)

    # Column headers
    for (ci in seq_along(vld_col_names)) {
      openxlsx::writeData(wb, sheet, vld_col_names[ci],
                          startRow = current_row, startCol = ci)
      co_style <- styles[["ColumnOutline"]]
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = current_row, cols = ci, stack = TRUE)
      }
    }
    current_row <- current_row + 1L

    # Data rows
    vld_n_rows <- nrow(vld_err_data)
    for (ri in seq_len(vld_n_rows)) {
      is_bottom <- (ri == vld_n_rows)
      for (ci in seq_along(vld_col_names)) {
        val <- vld_err_data[[ci]][ri]
        if (is.numeric(val) && is.na(val)) val <- "."

        openxlsx::writeData(wb, sheet, val,
                            startRow = current_row, startCol = ci)

        sid <- if (ci == 1L) "D_BLR" else "D0_R1_BL"
        if (is_bottom) sid <- paste0(sid, "B")
        st <- styles[[sid]]
        if (!is.null(st)) {
          openxlsx::addStyle(wb, sheet, st,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
      current_row <- current_row + 1L
    }

    write_err_text("", "Default", ht = 13.5)

    # "By Term" detail table (SAS lines ~1750-1785)
    if (!is.null(vld_term_data) && is.data.frame(vld_term_data) &&
        nrow(vld_term_data) > 0L) {
      vld_term_start <- current_row

      vt_col_names <- colnames(vld_term_data)
      vt_n_cols <- ncol(vld_term_data)

      # Column headers for term detail
      for (ci in seq_along(vt_col_names)) {
        openxlsx::writeData(wb, sheet, vt_col_names[ci],
                            startRow = current_row, startCol = ci)
        co_style <- styles[["ColumnOutline"]]
        if (!is.null(co_style)) {
          openxlsx::addStyle(wb, sheet, co_style,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
      current_row <- current_row + 1L

      vt_n_rows <- nrow(vld_term_data)
      for (ri in seq_len(vt_n_rows)) {
        is_bottom <- (ri == vt_n_rows)
        for (ci in seq_along(vt_col_names)) {
          val <- vld_term_data[[ci]][ri]
          if (is.numeric(val) && is.na(val)) val <- "."

          openxlsx::writeData(wb, sheet, val,
                              startRow = current_row, startCol = ci)

          sid <- if (ci <= 2L) "D_BLR" else "D0_R1_BL"
          if (is_bottom) sid <- paste0(sid, "B")
          st <- styles[[sid]]
          if (!is.null(st)) {
            openxlsx::addStyle(wb, sheet, st,
                               rows = current_row, cols = ci, stack = TRUE)
          }
        }
        current_row <- current_row + 1L
      }

      vld_term_end <- current_row - 1L
      # Alternating row shading for term detail table
      if (vld_term_end >= vld_term_start + 1L) {
        openxlsx::conditionalFormatting(
          wb, sheet,
          cols  = seq_len(vt_n_cols),
          rows  = (vld_term_start + 1L):vld_term_end,
          rule  = "MOD(ROW(),2)=0",
          type  = "expression",
          style = openxlsx::createStyle(fgFill = "#C0C0C0")
        )
      }

      write_err_text("", "Default", ht = 13.5)
    }
  }

  # ===================================================================
  # MedDRA Matching Section (SAS lines 1785-1925, conditional)
  # ===================================================================
  if (meddra_sw == "Y" && !is.null(ae_matching_data)) {
    write_err_text("MedDRA Matching", "SubHeader", ht = 16)
    write_err_text("", "Default", ht = 13.5)

    meddra_ver <- params_list$ver %||% ""
    ae_event_count <- params_list$ae_event_count %||% ""
    write_err_text(paste0(
      "MedDRA version ", meddra_ver,
      " matching summary. Total AE events evaluated: ", ae_event_count, "."
    ), "Default10Wrap")
    write_err_text("", "Default", ht = 13.5)

    if (is.data.frame(ae_matching_data) && nrow(ae_matching_data) > 0L) {
      md_col_names <- colnames(ae_matching_data)
      md_n_cols <- ncol(ae_matching_data)

      # Column headers
      for (ci in seq_along(md_col_names)) {
        openxlsx::writeData(wb, sheet, md_col_names[ci],
                            startRow = current_row, startCol = ci)
        co_style <- styles[["ColumnOutline"]]
        if (!is.null(co_style)) {
          openxlsx::addStyle(wb, sheet, co_style,
                             rows = current_row, cols = ci, stack = TRUE)
        }
      }
      current_row <- current_row + 1L

      # Data rows
      md_n_rows <- nrow(ae_matching_data)
      for (ri in seq_len(md_n_rows)) {
        is_bottom <- (ri == md_n_rows)
        for (ci in seq_along(md_col_names)) {
          val <- ae_matching_data[[ci]][ri]
          if (is.numeric(val) && is.na(val)) val <- "."

          openxlsx::writeData(wb, sheet, val,
                              startRow = current_row, startCol = ci)

          # Style: description -> D_BLR, counts -> D0_R1_BL, pct -> D1_R1_BR
          sid <- dplyr::case_when(
            ci == 1L ~ "D_BLR",
            stringr::str_detect(md_col_names[ci], "(?i)pct") ~ "D1_R1_BR",
            TRUE ~ "D0_R1_BL"
          )
          if (is_bottom) sid <- stringr::str_c(sid, "B")
          st <- styles[[sid]]
          if (!is.null(st)) {
            openxlsx::addStyle(wb, sheet, st,
                               rows = current_row, cols = ci, stack = TRUE)
          }
        }
        current_row <- current_row + 1L
      }

      write_err_text("", "Default", ht = 13.5)

      # Non-matching terms detail table (conditional on meddra_pct < 100)
      meddra_pct <- as.numeric(params_list$meddra_pct %||% 100)
      meddra_term_data <- params_list$meddra_term_data

      if (meddra_pct < 100 && !is.null(meddra_term_data) &&
          is.data.frame(meddra_term_data) && nrow(meddra_term_data) > 0L) {

        meddra_term_start <- current_row
        mt_col_names <- colnames(meddra_term_data)
        mt_n_cols <- ncol(meddra_term_data)

        # Column headers
        for (ci in seq_along(mt_col_names)) {
          openxlsx::writeData(wb, sheet, mt_col_names[ci],
                              startRow = current_row, startCol = ci)
          co_style <- styles[["ColumnOutline"]]
          if (!is.null(co_style)) {
            openxlsx::addStyle(wb, sheet, co_style,
                               rows = current_row, cols = ci, stack = TRUE)
          }
        }
        current_row <- current_row + 1L

        mt_n_rows <- nrow(meddra_term_data)
        for (ri in seq_len(mt_n_rows)) {
          is_bottom <- (ri == mt_n_rows)
          for (ci in seq_along(mt_col_names)) {
            val <- meddra_term_data[[ci]][ri]
            if (is.numeric(val) && is.na(val)) val <- "."

            openxlsx::writeData(wb, sheet, val,
                                startRow = current_row, startCol = ci)
            sid <- if (ci <= 2L) "D_BLR" else "D0_R1_BL"
            if (is_bottom) sid <- paste0(sid, "B")
            st <- styles[[sid]]
            if (!is.null(st)) {
              openxlsx::addStyle(wb, sheet, st,
                                 rows = current_row, cols = ci, stack = TRUE)
            }
          }
          current_row <- current_row + 1L
        }

        meddra_term_end <- current_row - 1L
        # Alternating row shading
        if (meddra_term_end >= meddra_term_start + 1L) {
          openxlsx::conditionalFormatting(
            wb, sheet,
            cols  = seq_len(mt_n_cols),
            rows  = (meddra_term_start + 1L):meddra_term_end,
            rule  = "MOD(ROW(),2)=0",
            type  = "expression",
            style = openxlsx::createStyle(fgFill = "#C0C0C0")
          )
        }
        write_err_text("", "Default", ht = 13.5)
      }
    }
  }

  # ===================================================================
  # Missing Toxicity Grades Section (SAS lines 1927-2040)
  # ===================================================================
  write_err_text("Missing Toxicity Grades", "SubHeader", ht = 16)
  write_err_text("", "Default", ht = 13.5)
  write_err_text(paste0(
    "Summary of adverse events with missing toxicity grades (AETOXGR). ",
    "Missing toxicity grades may indicate data entry omissions or events ",
    "where toxicity grading was not applicable."
  ), "Default10Wrap")
  write_err_text(paste0(
    "If subjects had adverse events with missing toxicity grades, those ",
    "subjects are included in the 'All Grades' count and may not appear in ",
    "any individual grade column."
  ), "Default10Wrap")
  write_err_text("", "Default", ht = 13.5)

  if (!is.null(params_list$rpt_missing) || !is.null(params_list$missing_tox_data)) {
    missing_data <- params_list$missing_tox_data
    if (is.null(missing_data) && !is.null(params_list$rpt_missing)) {
      # Reshape rpt_missing from wide to long and back to standard display layout
      # using tidyr pivot_longer/pivot_wider for tidy data transformation
      missing_data <- tryCatch({
        rpt_miss <- params_list$rpt_missing
        if (is.data.frame(rpt_miss) && ncol(rpt_miss) > 1L) {
          long_form <- tidyr::pivot_longer(
            rpt_miss,
            cols = -1L,
            names_to = "metric",
            values_to = "value"
          )
          # Pivot back to wide format (standardizes column structure)
          tidyr::pivot_wider(
            long_form,
            names_from = "metric",
            values_from = "value"
          )
        } else {
          rpt_miss
        }
      }, error = function(e) {
        params_list$rpt_missing
      })
    }

    if (!is.null(missing_data) && is.data.frame(missing_data) &&
        nrow(missing_data) > 0L) {

      ms_col_names <- colnames(missing_data)
      ms_n_cols <- ncol(missing_data)

      # Column header row 1: "Analysis" merged down + arm names merged across 2
      openxlsx::writeData(wb, sheet, "Analysis",
                          startRow = current_row, startCol = 1L)
      openxlsx::mergeCells(wb, sheet, cols = 1L,
                           rows = current_row:(current_row + 1L))
      co_style <- styles[["ColumnOutline"]]
      if (!is.null(co_style)) {
        openxlsx::addStyle(wb, sheet, co_style,
                           rows = current_row, cols = 1L, stack = TRUE)
      }

      # Arm name headers (merge across 2 for count + pct)
      col_offset <- 2L
      for (ai in seq_along(arm_names)) {
        arm_lbl <- paste0(arm_names[ai], " (N=", arm_ns[ai], ")")
        openxlsx::writeData(wb, sheet, arm_lbl,
                            startRow = current_row, startCol = col_offset)
        openxlsx::mergeCells(wb, sheet,
                             cols = col_offset:(col_offset + 1L),
                             rows = current_row)
        if (!is.null(co_style)) {
          openxlsx::addStyle(wb, sheet, co_style,
                             rows = current_row, cols = col_offset, stack = TRUE)
        }
        col_offset <- col_offset + 2L
      }
      current_row <- current_row + 1L

      # Column header row 2: Missing Count / % per arm
      col_offset <- 2L
      for (ai in seq_along(arm_names)) {
        openxlsx::writeData(wb, sheet, "Missing Count",
                            startRow = current_row, startCol = col_offset)
        if (!is.null(co_style)) {
          openxlsx::addStyle(wb, sheet, co_style,
                             rows = current_row, cols = col_offset, stack = TRUE)
        }
        openxlsx::writeData(wb, sheet, "%",
                            startRow = current_row, startCol = col_offset + 1L)
        if (!is.null(co_style)) {
          openxlsx::addStyle(wb, sheet, co_style,
                             rows = current_row, cols = col_offset + 1L,
                             stack = TRUE)
        }
        col_offset <- col_offset + 2L
      }
      current_row <- current_row + 1L

      # Data rows
      ms_n_rows <- nrow(missing_data)
      for (ri in seq_len(ms_n_rows)) {
        is_bottom <- (ri == ms_n_rows)
        for (ci in seq_along(ms_col_names)) {
          val <- missing_data[[ci]][ri]
          if (is.numeric(val) && is.na(val)) val <- "."

          openxlsx::writeData(wb, sheet, val,
                              startRow = current_row, startCol = ci)

          # Style: report_key -> DT_BLR, missing count -> D0_R1T_BL,
          #        pct -> D1_R1T_BR
          sid <- dplyr::case_when(
            ci == 1L ~ "DT_BLR",
            stringr::str_detect(ms_col_names[ci], "(?i)pct") ~ "D1_R1T_BR",
            TRUE ~ "D0_R1T_BL"
          )
          if (is_bottom) sid <- stringr::str_c(sid, "B")

          st <- styles[[sid]]
          # Fallback without T variant if not available
          if (is.null(st)) {
            sid_fallback <- gsub("T_", "_", sid)
            st <- styles[[sid_fallback]]
          }
          if (!is.null(st)) {
            openxlsx::addStyle(wb, sheet, st,
                               rows = current_row, cols = ci, stack = TRUE)
          }
        }
        current_row <- current_row + 1L
      }
    }
  }

  # ===================================================================
  # Worksheet settings (SAS lines 2043-2082)
  # ===================================================================
  openxlsx::pageSetup(wb, sheet, orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  openxlsx::setHeaderFooter(
    wb, sheet,
    footer = c(NA, "Page &P of &N", NA)
  )

  invisible(NULL)
}

# ==============================================================================
# out_onc
# ------------------------------------------------------------------------------
# Replaces SAS %out_onc macro (SAS lines 2410-2485).
# Main orchestrator that creates the complete oncology AE workbook by calling
# each worksheet-generating function in sequence, then saves the workbook.
#
# @param params_list        Named list of study parameters.
# @param pt_1_output        Data frame for Toxicity Grade Summary.
# @param pt_2_output        Data frame for PT Analysis (flat/filterable).
# @param pt_2_output_fmt    Data frame for PT Analysis (formatted/printable).
# @param pt_3_output        Data frame for comparison analysis (flat). NULL if
#                            arm_count <= 1.
# @param pt_3_output_fmt    Data frame for comparison analysis (formatted).
#                            NULL if arm_count <= 1.
# @param pt_3_output_cc_ind Vector of continuity correction indicators. NULL if
#                            arm_count <= 1 or cc_sw == 0.
# @param rpt_key            Data frame of report key metadata.
# @param rpt_missing        Data frame of missing toxicity grade counts.
# @param dm_validation_data Data frame of DM subject validation data.
# @param ae_matching_data   Optional data frame of MedDRA matching data.
# @return Character string: path to the saved workbook.
# ==============================================================================
out_onc <- function(params_list, pt_1_output, pt_2_output, pt_2_output_fmt,
                    pt_3_output = NULL, pt_3_output_fmt = NULL,
                    pt_3_output_cc_ind = NULL,
                    rpt_key, rpt_missing, dm_validation_data,
                    ae_matching_data = NULL) {

  # --- Input validation ---
  if (!is.list(params_list)) {
    cli::cli_abort("{.arg params_list} must be a named list.")
  }
  if (!is.data.frame(pt_1_output) || nrow(pt_1_output) == 0L) {
    cli::cli_abort("{.arg pt_1_output} must be a non-empty data frame.")
  }
  if (!is.data.frame(pt_2_output) || nrow(pt_2_output) == 0L) {
    cli::cli_abort("{.arg pt_2_output} must be a non-empty data frame.")
  }
  if (!is.data.frame(rpt_key)) {
    cli::cli_abort("{.arg rpt_key} must be a data frame.")
  }
  if (!is.null(pt_3_output) && !is.data.frame(pt_3_output)) {
    cli::cli_abort("{.arg pt_3_output} must be a data frame or NULL.")
  }
  if (!is.null(ae_matching_data) && !is.data.frame(ae_matching_data)) {
    cli::cli_abort("{.arg ae_matching_data} must be a data frame or NULL.")
  }

  # Log activity (SAS lines 2413-2426)
  cli::cli_alert_info("MAKING EXCEL OUTPUT FOR ONCOLOGY AE REPORT")

  # Create run date (SAS lines 2429-2431)
  if (is.null(params_list$rundate) || !nzchar(params_list$rundate)) {
    params_list$rundate <- format(Sys.time(), "%Y-%m-%d %I:%M:%S %p")
  }

  arm_count <- as.integer(params_list$arm_count %||% 1L)

  # Preprocessing: grouping & subsetting (SAS lines 2434-2435)
  sl_group   <- params_list$sl_group
  sl_subset  <- params_list$sl_subset
  sl_datasets <- params_list$sl_datasets
  sl_subset_outer <- params_list$sl_subset_outer

  pp_result <- NULL
  has_gs <- FALSE
  if (!is.null(sl_group) || !is.null(sl_subset)) {
    tryCatch({
      pp_result <- group_subset_pp(
        sl_group        = sl_group,
        sl_subset       = sl_subset,
        sl_datasets     = sl_datasets,
        sl_subset_outer = sl_subset_outer
      )
      has_gs <- TRUE
    }, error = function(e) {
      cli::cli_warn("Grouping/subsetting preprocessing failed: {e$message}")
    })
  }

  # If pp_result produced a description, add to params_list
  if (has_gs && !is.null(pp_result$sl_gs_desc)) {
    params_list$sl_gs_desc <- pp_result$sl_gs_desc
  }

  # Create workbook (SAS lines 2437)
  wb <- openxlsx::createWorkbook()

  # Create styles (SAS lines 2438-2445)
  styles <- create_oae_styles(wb)

  # Add worksheets in sequence (SAS lines 2450-2458)
  # 1. Front Page (Cover Sheet)
  out_cover(wb, params_list, rpt_key, rpt_missing, styles)

  # 2. Toxicity Grade Summary
  out_pt_1(wb, pt_1_output, params_list, rpt_key, rpt_missing, styles)

  # 3. PT Analysis V1 (filterable)
  out_pt_2(wb, pt_2_output, pt_2_output_fmt, params_list,
           rpt_key, rpt_missing, styles, fmt = FALSE)

  # 4. PT Analysis V2 (formatted for printing)
  out_pt_2(wb, pt_2_output, pt_2_output_fmt, params_list,
           rpt_key, rpt_missing, styles, fmt = TRUE)

  # 5 & 6. Comparison Analysis V1 and V2 (conditional on arm_count > 1)
  if (arm_count > 1L) {
    if (!is.null(pt_3_output)) {
      out_pt_3(wb, pt_3_output, pt_3_output_fmt, pt_3_output_cc_ind,
               params_list, rpt_key, rpt_missing, styles, fmt = FALSE)
    }
    if (!is.null(pt_3_output_fmt) || !is.null(pt_3_output)) {
      out_pt_3(wb, pt_3_output, pt_3_output_fmt, pt_3_output_cc_ind,
               params_list, rpt_key, rpt_missing, styles, fmt = TRUE)
    }
  } else {
    cli::cli_warn(paste0(
      "Comparison analysis worksheets skipped: arm_count = ", arm_count,
      " (requires > 1 arm)"
    ))
  }

  # 7. Data Check Summary
  out_err(wb, params_list, dm_validation_data, ae_matching_data, styles)

  # 8. Grouping and Subsetting worksheet (conditional)
  if (has_gs && !is.null(pp_result)) {
    tryCatch({
      group_subset_xml_out(
        wb        = wb,
        pp_result = pp_result,
        ndabla    = params_list$ndabla %||% "",
        studyid   = params_list$studyid %||% "",
        styles    = styles
      )
    }, error = function(e) {
      cli::cli_warn("Grouping/subsetting worksheet failed: {e$message}")
    })
  }

  # Save workbook (SAS lines 2479-2483)
  output_path <- params_list$oncaeout
  if (is.null(output_path) || !nzchar(output_path)) {
    output_path <- file.path(tempdir(), "oncology_ae_report.xlsx")
    cli::cli_warn("No output path specified (params_list$oncaeout). Using: {output_path}")
  }

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  cli::cli_alert_success("Workbook saved to: {output_path}")

  invisible(output_path)
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - All SpreadsheetML XML generation is replaced by openxlsx API calls
####    - Style IDs from SAS XML are mapped to named openxlsx style objects
####    - MergeAcross/MergeDown XML attributes -> openxlsx::mergeCells() calls
####    - XML Row height -> openxlsx::setRowHeights()
####    - XML Column width -> openxlsx::setColWidths()
####    - SAS %annotate macro (data-to-XML conversion) -> direct writeData() calls
####    - SAS %markup macro (note-to-XML rendering) -> direct writeData() with styles
####    - Font defaults: SAS ODS uses Times New Roman; openxlsx defaults to Calibri --
####      explicitly set font to match SAS output if exact layout parity required
####    - Print area, freeze panes, autofilter settings -> openxlsx equivalents
####    - Named ranges for print titles -> openxlsx print title support
####    - Conditional formatting (alternating row shading) -> openxlsx::conditionalFormatting()
####    - SAS DATA _NULL_ file writing -> openxlsx::saveWorkbook()
####    - Worksheet tab names preserved exactly from SAS XML
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- this is output generation, not computation
####    - Cell formatting (decimal places, alignment) should match visually
####    - Scientific notation threshold (> 10^6) for upper confidence limits preserved
#### NO DIRECT R EQUIVALENT:
####    - SAS SpreadsheetML XML engine -> openxlsx workbook API
####    - SAS %xml_tag_def/%xml_init macros -> not needed; openxlsx handles cell creation
####    - SAS %annotate macro (SAS dataset -> XML annotation) -> R: writeData() with formatting
####    - SAS %markup macro (annotated data -> XML rows) -> R: writeData() with styles
####    - SAS %ws macro (worksheet scaffold XML) -> R: addWorksheet() + setColWidths()
####    - SAS DATA _NULL_ PUT statement for XML lines -> openxlsx API calls
####    - SAS ~!~!~! indent markers -> paste0("     ", text) for 5-space indent
####    - SAS &#10; line breaks in XML -> openxlsx handles line breaks via wrapText
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Direct replacement for SpreadsheetML XML generation; supports
####      styles, merged cells, conditional formatting, freeze panes, print settings,
####      autofilter -- covers all XML features used in the SAS source
####    - dplyr: Data preparation for worksheet content (tidyverse mandate)
####    - tidyr: Data reshaping for column header construction
####    - stringr: String manipulation for text formatting and indentation
####    - cli: Logging and diagnostic messages replacing SAS PUT statements
#### OPEN QUESTIONS:
####    - Whether exact SpreadsheetML font metrics (character width) match openxlsx defaults
####    - Confirm row height calculations (SAS ceil(nchar/110.5)*13.5) translate to
####      openxlsx row height units
####    - Verify conditional formatting formula syntax compatibility
####    - Whether SAS "~!~!~!" indent markers need exact pixel-equivalent spacing
#### ============================================================
