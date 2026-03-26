# ==============================================================================
#
#         PROGRAM NAME: Generic Panel Metadata Output (R Migration)
#
#          DESCRIPTION: Create an Excel workbook with the settings the user
#                       chose for a panel. Contains one exported function:
#                         metadata_summary() -- Build and save an openxlsx
#                           workbook with a "Metadata Summary" worksheet
#                           containing panel title, NDA/BLA, study ID,
#                           run date, panel description, custom datasets,
#                           and grouping/subsetting descriptions, plus an
#                           optional "Grouping and Subsetting" detail
#                           worksheet via group_subset_xml_out().
#
#                       Migrated from SAS macro %metadata_summary
#                       (contributed/AE/ZZ_Utilities/md_output.sas, 173 lines).
#
#               AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                       Migrated to R by Blitzy
#
#         ORIGINAL DATE: March 14, 2011
#         MIGRATION DATE: 2026
#
#  EXTERNAL FILES USED: xml_output.R   -- Workbook/style/annotated-data helpers
#                       sl_gs_output.R -- Grouping & subsetting preprocessing
#
#  PARAMETERS REQUIRED: md_file      -- output file path (function argument)
#                       panel_title  -- panel title (function argument)
#                       ndabla       -- NDA/BLA identifier (function argument)
#                       studyid      -- study identifier (function argument)
#
#            MADE WITH: R 4.3+ / openxlsx >= 4.2.5
#
#                NOTES: SpreadsheetML XML generation replaced by openxlsx API.
#                       SAS %include replaced by documented source() pattern.
#                       All file paths parameterized via function arguments.
#
#            REVISIONS: 2026 -- Blitzy -- SAS -> R migration
#
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(openxlsx)
library(dplyr)
library(cli)

# --- Source Dependencies (replaces SAS %include at lines 33-34) ---------------
# xml_output.R and sl_gs_output.R must be sourced before this file.
# They provide:
#   create_workbook()       -- Initialize openxlsx workbook (replaces SAS %wb)
#   create_styles()         -- Named style gallery (replaces SAS %styles)
#   write_annotated_data()  -- Write styled cell data (replaces SAS %markup)
#   group_subset_pp()       -- Preprocess grouping/subsetting metadata
#   group_subset_xml_out()  -- Add formatted G&S worksheet to workbook
#
# Example usage:
#   util_path <- "contributed/R/AE/ZZ_Utilities"
#   source(file.path(util_path, "xml_output.R"))
#   source(file.path(util_path, "sl_gs_output.R"))
#   source(file.path(util_path, "md_output.R"))


# ==============================================================================
# make_header_row — Build a single annotated-data row for write_annotated_data()
# ------------------------------------------------------------------------------
# Internal helper that constructs a one-row tibble matching the annotated-data
# schema consumed by write_annotated_data() from xml_output.R.  Each row
# represents one cell in the "Metadata Summary" worksheet.
#
# @param row_num      Integer.   Logical row number (grouping key).
# @param data         Character. Cell text content (default "").
# @param style_id     Character. Style name from create_styles() gallery, or
#                     NA_character_ for no style.
# @param merge_across Numeric.   Number of additional columns to merge right,
#                     or NA_real_ for no merge (default).
# @param height       Numeric.   Explicit row height in points, or NA_real_
#                     for default height (default).
# @return A one-row dplyr::tibble with annotated-data columns.
# ==============================================================================
make_header_row <- function(row_num,
                            data = "",
                            style_id = NA_character_,
                            merge_across = NA_real_,
                            height = NA_real_) {
  dplyr::tibble(
    Row         = as.integer(row_num),
    Data        = as.character(data),
    Type        = "String",
    varname     = NA_character_,
    bottom      = 0L,
    Height      = as.numeric(height),
    Index       = NA_real_,
    MergeAcross = as.numeric(merge_across),
    MergeDown   = NA_real_,
    StyleID     = as.character(style_id),
    Formula     = NA_character_,
    Comment     = NA_character_,
    Name        = NA_character_,
    ArrayRange  = NA_character_
  )
}


# ==============================================================================
# metadata_summary — Generic Panel Metadata Output
# ------------------------------------------------------------------------------
# Replaces SAS %metadata_summary macro (SAS lines 39-173).
# Creates an Excel workbook containing:
#   1. A "Metadata Summary" worksheet with panel title, NDA/BLA, study ID,
#      analysis run date, panel description (conditional), custom datasets,
#      grouping description, and subsetting description.
#   2. A "Grouping and Subsetting" detail worksheet (via group_subset_xml_out)
#      with formatted grouping and subsetting tables.
#
# @param md_file       Character. Output file path for the Excel workbook.
#                      Replaces SAS macro parameter md_file= (SAS line 39).
# @param panel_title   Character. Panel title text displayed in header.
#                      Replaces SAS global macro variable &panel_title.
# @param ndabla        Character. NDA/BLA identifier.
#                      Replaces SAS global macro variable &ndabla.
# @param studyid       Character. Study identifier.
#                      Replaces SAS global macro variable &studyid.
# @param panel_desc    Character. Panel description text for conditional
#                      description section (default ""). Replaces SAS global
#                      macro variable &panel_desc (SAS line 107).
# @param panel_message Character or NULL. Display-friendly version of
#                      panel_desc; if NULL, panel_desc is used (SAS line 111).
#                      Replaces SAS global macro variable &panel_message.
# @param sl_group      Data frame or NULL. Grouping metadata for
#                      group_subset_pp() (default NULL).
# @param sl_subset     Data frame or NULL. Subsetting metadata for
#                      group_subset_pp() (default NULL).
# @param sl_datasets   Data frame or NULL. Datasets metadata for
#                      group_subset_pp() (default NULL).
# @param sl_custom_ds  Character. Custom datasets description; if "" (default),
#                      the value returned by group_subset_pp() is used.
#                      Replaces SAS global macro variable &sl_custom_ds
#                      (SAS line 121).
#
# @return Invisibly returns the openxlsx Workbook object.
# ==============================================================================
metadata_summary <- function(md_file,
                             panel_title,
                             ndabla,
                             studyid,
                             panel_desc = "",
                             panel_message = NULL,
                             sl_group = NULL,
                             sl_subset = NULL,
                             sl_datasets = NULL,
                             sl_custom_ds = "") {

  # --- Status logging (replaces SAS %put at line 41) --------------------------

  cli::cli_inform("SCRIPT LAUNCHER METADATA OUTPUT")

  # --- Parameter validation ---------------------------------------------------
  if (missing(md_file) || !is.character(md_file) || nchar(md_file) == 0L) {
    cli::cli_abort("{.arg md_file} must be a non-empty character string.")
  }
  if (missing(panel_title) || !is.character(panel_title) ||
      length(panel_title) != 1L) {
    cli::cli_abort("{.arg panel_title} must be a single character string.")
  }
  if (missing(ndabla) || !is.character(ndabla) || length(ndabla) != 1L) {
    cli::cli_abort("{.arg ndabla} must be a single character string.")
  }
  if (missing(studyid) || !is.character(studyid) || length(studyid) != 1L) {
    cli::cli_abort("{.arg studyid} must be a single character string.")
  }

  # Coerce NULLs and NAs to safe defaults
  if (is.null(panel_desc) || is.na(panel_desc)) panel_desc <- ""
  if (is.null(sl_custom_ds) || is.na(sl_custom_ds)) sl_custom_ds <- ""

  # ---------------------------------------------------------------------------
  # 1. Create workbook (replaces SAS %wb at line 45)
  # ---------------------------------------------------------------------------
  wbtitle <- paste(panel_title, "Metadata Summary")
  wb <- create_workbook(title = wbtitle)

  # ---------------------------------------------------------------------------
  # 2. Create style gallery (replaces SAS %styles at line 46)
  # ---------------------------------------------------------------------------
  styles <- create_styles()

  # ---------------------------------------------------------------------------
  # 3. Preprocess grouping/subsetting metadata
  #    (replaces SAS %group_subset_pp at line 73)
  # ---------------------------------------------------------------------------
  pp_result <- group_subset_pp(
    sl_group    = sl_group,
    sl_subset   = sl_subset,
    sl_datasets = sl_datasets
  )

  sl_group_desc  <- pp_result$sl_group_desc
  sl_subset_desc <- pp_result$sl_subset_desc

  # Use sl_custom_ds from pp_result when not explicitly provided
  effective_custom_ds <- dplyr::if_else(
    nchar(trimws(sl_custom_ds)) > 0L,
    sl_custom_ds,
    pp_result$sl_custom_ds
  )

  # ---------------------------------------------------------------------------
  # 4. Add Grouping & Subsetting detail worksheet
  #    (replaces SAS %group_subset_xml_out(delete_im=N) at line 74)
  # ---------------------------------------------------------------------------
  group_subset_xml_out(wb       = wb,
                       pp_result = pp_result,
                       ndabla    = ndabla,
                       studyid   = studyid,
                       styles    = styles)

  # ===========================================================================
  # 5. Build "Metadata Summary" worksheet
  #    (replaces SAS ws_metadata_* DATA steps, lines 49-71, 77-148)
  # ===========================================================================
  sheet_name <- "Metadata Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  # --- Column widths (SAS lines 62-65) ----------------------------------------

  # SAS SpreadsheetML Column ss:Width="150" is in points.
  # openxlsx setColWidths uses Excel character-width units (~1/7 of a point).
  openxlsx::setColWidths(wb, sheet_name,
                         cols   = 1:3,
                         widths = c(150 / 7, 150 / 7, "auto"))

  # ===========================================================================
  # 6. Header section — direct openxlsx calls for simple rows,
  #    write_annotated_data() for complex panel-description merge/height
  #    (replaces SAS DATA ws_metadata_header_data, lines 77-126,
  #     and SAS %markup at line 128)
  # ===========================================================================

  # --- Simple header rows via direct openxlsx calls ---------------------------

  # Row 1: blank (SAS lines 85-86) — default blank, nothing to write

  # Row 2: Panel title + " Metadata Summary" with Header style (SAS line 89)
  title_text <- paste(panel_title, "Metadata Summary")
  openxlsx::writeData(wb, sheet_name, title_text, startRow = 2, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Header, rows = 2, cols = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:3, rows = 2)
  openxlsx::setRowHeights(wb, sheet_name, rows = 2, heights = 20)

  # Row 3: blank (SAS lines 91-92) — default blank

  # Row 4: NDA/BLA with Default10 style (SAS lines 96-97)
  openxlsx::writeData(wb, sheet_name, paste0("NDA/BLA: ", ndabla),
                       startRow = 4, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = 4, cols = 1)

  # Row 5: Study with Default10 style (SAS lines 98-99)
  openxlsx::writeData(wb, sheet_name, paste0("Study: ", studyid),
                       startRow = 5, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = 5, cols = 1)

  # Row 6: Analysis run date with Default10 style (SAS lines 100-101)
  # SAS: put(date(), e8601da.) -> format(Sys.Date(), "%Y-%m-%d")
  # SAS: put(time(), timeampm11.) -> format(Sys.time(), "%I:%M:%S %p")
  run_date_str <- paste("Analysis run date:",
                        format(Sys.Date(), "%Y-%m-%d"),
                        format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, sheet_name, run_date_str, startRow = 6, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = 6, cols = 1)

  # Rows 7-8: blank — SAS increments counter without output (SAS lines 102-105)
  # Default blank rows, nothing to write

  r <- 8L

  # --- Panel description section (conditional, SAS lines 107-118) -------------
  # Uses write_annotated_data() for the complex merge/height formatting
  if (nchar(trimws(panel_desc)) > 0L) {
    # Use panel_message if provided, otherwise fall back to panel_desc
    # Convert NULL to empty string first to satisfy dplyr::if_else length requirement
    panel_msg_safe <- if (is.null(panel_message)) "" else as.character(panel_message)
    desc_display <- dplyr::if_else(
      nchar(trimws(panel_msg_safe)) > 0L,
      panel_msg_safe,
      as.character(panel_desc)
    )

    # Height calculation mirrors SAS: ceil(length(trim(panel_desc))/150)*12.75
    desc_height <- ceiling(nchar(trimws(panel_desc)) / 150) * 12.75

    r <- r + 1L
    desc_annotated <- make_header_row(
      r, desc_display, "Default10Wrap",
      merge_across = 6, height = desc_height
    )
    write_annotated_data(wb, sheet_name, desc_annotated, styles, start_row = r)

    # Blank row after panel description (SAS lines 116-117)
    r <- r + 1L
  }

  # --- Metadata details via direct openxlsx calls (SAS lines 120-125) ---------

  # Custom datasets (SAS line 121):
  #   ifc("&sl_custom_ds." ne '', "&sl_custom_ds.", 'No custom datasets')
  r <- r + 1L
  custom_ds_label <- paste0(
    "Custom datasets: ",
    dplyr::if_else(
      nchar(trimws(effective_custom_ds)) > 0L,
      effective_custom_ds,
      "No custom datasets"
    )
  )
  openxlsx::writeData(wb, sheet_name, custom_ds_label, startRow = r, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = r, cols = 1)

  # Grouping description (SAS lines 122-123)
  r <- r + 1L
  openxlsx::writeData(wb, sheet_name, paste0("Grouping: ", sl_group_desc),
                       startRow = r, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = r, cols = 1)

  # Subsetting description (SAS lines 124-125)
  r <- r + 1L
  openxlsx::writeData(wb, sheet_name, paste0("Subsetting: ", sl_subset_desc),
                       startRow = r, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, styles$Default10, rows = r, cols = 1)

  # ===========================================================================
  # 7. Page setup (replaces SAS WorksheetOptions, lines 130-148)
  # ===========================================================================

  # Landscape orientation + fit-to-page (SAS lines 132-139)
  openxlsx::pageSetup(wb, sheet_name,
                       orientation  = "landscape",
                       fitToWidth   = TRUE,
                       fitToHeight  = FALSE)

  # Header: panel title on left; NDA/BLA and Study on right (SAS lines 135-136)
  # Footer: "Page X of Y" centered (SAS line 137)
  # openxlsx header/footer codes: &[Page] = page number, &[Pages] = total pages
  header_left  <- panel_title
  header_right <- paste0("NDA/BLA ", ndabla, "\nStudy ", studyid)
  footer_center <- "Page &[Page] of &[Pages]"

  openxlsx::setHeaderFooter(wb, sheet_name,
                             header = c(header_left, NA, header_right),
                             footer = c(NA, footer_center, NA))

  # ===========================================================================
  # 8. Save workbook (replaces SAS DATA _NULL_ output, lines 165-169)
  # ===========================================================================
  tryCatch(
    {
      openxlsx::saveWorkbook(wb, md_file, overwrite = TRUE)
      cli::cli_inform("Metadata workbook saved to {.file {md_file}}")
    },
    error = function(e) {
      cli::cli_warn(
        "Failed to save metadata workbook to {.file {md_file}}: {e$message}"
      )
    }
  )

  # Return workbook object invisibly (SAS had no return; R adds for chaining)
  invisible(wb)
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    1. SpreadsheetML XML column widths (ss:Width="150" in points)
####       map approximately to openxlsx character-width units via a
####       divide-by-7 factor (150/7 ~ 21.4 character widths).
####    2. Header/footer markup in openxlsx uses &[Page] / &[Pages]
####       codes rather than SAS &amp;P / &amp;N XML entities.
####    3. The SAS macro assembled grouping/subsetting table data
####       WITHIN the single "Metadata Summary" worksheet via XML
####       fragment concatenation.  In R, group_subset_xml_out()
####       adds a separate "Grouping and Subsetting" worksheet,
####       producing a two-worksheet workbook instead of one.
####       All content is preserved; only the worksheet layout differs.
####    4. SAS %let ds = metadata local prefix variable is not needed
####       in R -- descriptive variable names are used directly.
####    5. SAS &strlen. global for string length is unnecessary in R
####       since character vectors have no fixed length constraint.
####
#### POTENTIAL NUMERICAL DIFFERENCES:
####    None expected -- this is metadata/descriptive output only,
####    with no statistical computations or rounding operations.
####
#### NO DIRECT R EQUIVALENT:
####    1. SAS SpreadsheetML DATA _NULL_ streaming (lines 165-169)
####       -> openxlsx::saveWorkbook() API call.
####    2. SAS %include (lines 33-34) -> source() pattern documented
####       in file header; caller must source dependencies.
####    3. SAS %markup macro (line 128) -> write_annotated_data()
####       from xml_output.R with annotated tibble input.
####    4. SAS %xml_tag_def / %xml_init -> not needed; tibble
####       structure in make_header_row() replaces their role.
####    5. SAS PROC DATASETS cleanup (line 171) -> R garbage
####       collection handles temporary object cleanup automatically.
####
#### PACKAGE SELECTION RATIONALE:
####    openxlsx (>=4.2.5) -- CRAN package for Excel workbook
####       creation, chosen per AAP to replace SpreadsheetML XML
####       string concatenation and PCFILES/JET engine writes.
####    dplyr (>=1.1.0) -- Core tidyverse package mandated by AAP
####       for all data manipulation; provides if_else(), tibble(),
####       and bind_rows() used in annotated-data construction.
####    cli (>=3.6.0) -- Diagnostic message formatting replacing
####       SAS %PUT statements; provides cli_inform() and cli_warn()
####       for structured, user-facing audit-trail messages.
####
#### OPEN QUESTIONS:
####    1. Confirm the divide-by-7 column width conversion factor
####       between SpreadsheetML points and openxlsx character units
####       produces visually equivalent column widths.
####    2. Verify openxlsx header/footer newline (\n) renders
####       correctly in all target Excel versions (2016+).
####    3. Confirm that the two-worksheet layout (Metadata Summary +
####       Grouping and Subsetting) is acceptable to downstream
####       consumers that previously expected a single worksheet.
#### ============================================================
