# =============================================================================
# PROGRAM NAME: md_output.R
#
# DESCRIPTION:  Metadata Summary Output — generates the "Metadata Summary"
#               worksheet for the Script Launcher. Creates an Excel (.xlsx)
#               workbook containing panel title, NDA/BLA context, study info,
#               run-date timestamps, custom dataset descriptions,
#               grouping/subsetting description sentences, and (conditionally)
#               a detailed grouping/subsetting worksheet.
#
# ORIGINAL SAS: tested/SAS/ZZ_Utilities/md_output.sas (173 lines)
# SAS AUTHOR:   David Kretch (david.kretch@us.ibm.com)
# SAS DATE:     March 14, 2011
#
# SAS MACROS MIGRATED:
#   %metadata_summary(md_file=) (lines 39-173) -> metadata_summary()
#
# R REQUIRES:   openxlsx (>= 4.2.5), cli (>= 3.6.0), stringr (>= 1.5.0)
#
# INTERNAL DEPENDENCIES:
#   tested/R/utilities/xml_output.R
#     Used functions: create_workbook(), create_workbook_styles(),
#                     apply_page_setup(), write_formatted_data()
#   tested/R/utilities/sl_gs_output.R
#     Used functions: group_subset_pp(), group_subset_write_ws()
#
# NOTES:        All SAS SpreadsheetML XML generation replaced by openxlsx API.
#               SAS macro variables become explicit R function arguments.
#               SAS %markup header pipeline replaced by write_formatted_data()
#               with per-row style_map and merge_info tibbles.
# =============================================================================

# Load required packages
library(openxlsx)
library(cli)
library(stringr)

# Internal dependencies: xml_output.R and sl_gs_output.R from the same directory.
# Source them if their exported functions are not yet available in the session.
local({
  this_dir <- tryCatch(
    dirname(sys.frame(1L)$ofile),
    error = function(e) NULL
  )

  # --- xml_output.R ---
  xml_path <- if (!is.null(this_dir)) {
    file.path(this_dir, "xml_output.R")
  } else {
    fp <- "tested/R/utilities/xml_output.R"
    if (file.exists(fp)) fp else file.path("..", "utilities", "xml_output.R")
  }
  if (file.exists(xml_path) && !exists("create_workbook", mode = "function")) {
    source(xml_path, local = FALSE)
  }

  # --- sl_gs_output.R ---
  sl_path <- if (!is.null(this_dir)) {
    file.path(this_dir, "sl_gs_output.R")
  } else {
    fp <- "tested/R/utilities/sl_gs_output.R"
    if (file.exists(fp)) fp else file.path("..", "utilities", "sl_gs_output.R")
  }
  if (file.exists(sl_path) && !exists("group_subset_pp", mode = "function")) {
    source(sl_path, local = FALSE)
  }
})


# =============================================================================
# metadata_summary
# =============================================================================
#' Generate the Metadata Summary Excel workbook for the Script Launcher.
#'
#' Migrates SAS \code{%metadata_summary(md_file=)} macro (lines 39-173 of
#' md_output.sas). Creates an openxlsx workbook with:
#' \enumerate{
#'   \item A "Metadata Summary" worksheet containing the panel title, NDA/BLA
#'         identifier, study identifier, analysis run date, optional panel
#'         message, custom dataset list, and grouping/subsetting description
#'         sentences.
#'   \item (Conditionally) a "Grouping and Subsetting" detail worksheet when
#'         active grouping or subsetting rules exist, produced by
#'         \code{group_subset_write_ws()} from sl_gs_output.R.
#' }
#'
#' The workbook is saved to \code{md_file} and the workbook object is returned
#' invisibly for downstream chaining.
#'
#' @param md_file       Character. Output file path for the Excel workbook.
#'                      Directories are created automatically if they do not
#'                      exist. Corresponds to the SAS \code{md_file=} macro
#'                      parameter (line 39).
#' @param panel_title   Character. Panel title displayed in the header row
#'                      and page header. Replaces SAS \code{&panel_title.}
#'                      global macro variable (line 36).
#' @param ndabla        Character. NDA/BLA identifier displayed in the header
#'                      section and page header. Replaces SAS \code{&ndabla.}
#'                      (line 97).
#' @param studyid       Character. Study identifier displayed in the header
#'                      section and page header. Replaces SAS \code{&studyid.}
#'                      (line 99).
#' @param panel_desc    Character. Panel description text used to calculate the
#'                      row height for the panel_message cell. An empty string
#'                      (default) suppresses the panel_message row. Replaces
#'                      SAS \code{&panel_desc.} (line 107/113).
#' @param panel_message Character. Descriptive message text written into a
#'                      merged cell spanning 7 columns with word-wrap. Only
#'                      displayed when \code{panel_desc} is non-empty. Replaces
#'                      SAS \code{&panel_message.} (line 111).
#' @param sl_group      Data.frame/tibble or NULL. Script Launcher grouping
#'                      metadata table. Passed to \code{group_subset_pp()}.
#' @param sl_subset     Data.frame/tibble or NULL. Script Launcher subsetting
#'                      metadata table. Passed to \code{group_subset_pp()}.
#' @param sl_datasets   Data.frame/tibble or NULL. Script Launcher datasets
#'                      metadata. Passed to \code{group_subset_pp()}.
#' @param sl_custom_ds  Character or NULL. Pre-computed custom dataset string.
#'                      If NULL, derived from \code{group_subset_pp()} result.
#' @param styles        Named list of openxlsx style objects, or NULL. If NULL,
#'                      the full style gallery is created via
#'                      \code{create_workbook_styles()} from xml_output.R.
#'
#' @return The openxlsx workbook object (invisible).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' metadata_summary(
#'   md_file     = "output/metadata_summary.xlsx",
#'   panel_title = "Adverse Events",
#'   ndabla      = "12345",
#'   studyid     = "STUDY-001"
#' )
#' }
metadata_summary <- function(md_file,
                             panel_title,
                             ndabla,
                             studyid,
                             panel_desc    = "",
                             panel_message = "",
                             sl_group      = NULL,
                             sl_subset     = NULL,
                             sl_datasets   = NULL,
                             sl_custom_ds  = NULL,
                             styles        = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (missing(md_file) || !is.character(md_file) || length(md_file) != 1L ||
      nchar(md_file) == 0L) {
    stop("'md_file' must be a non-empty single character string specifying the ",
         "output file path.", call. = FALSE)
  }
  if (missing(panel_title) || !is.character(panel_title) ||
      length(panel_title) != 1L) {
    stop("'panel_title' must be a single character string.", call. = FALSE)
  }
  if (missing(ndabla) || !is.character(ndabla) || length(ndabla) != 1L) {
    stop("'ndabla' must be a single character string.", call. = FALSE)
  }
  if (missing(studyid) || !is.character(studyid) || length(studyid) != 1L) {
    stop("'studyid' must be a single character string.", call. = FALSE)
  }
  if (!is.character(panel_desc) || length(panel_desc) != 1L) {
    stop("'panel_desc' must be a single character string.", call. = FALSE)
  }
  if (!is.character(panel_message) || length(panel_message) != 1L) {
    stop("'panel_message' must be a single character string.", call. = FALSE)
  }
  if (!is.null(sl_custom_ds) && (!is.character(sl_custom_ds) ||
      length(sl_custom_ds) != 1L)) {
    stop("'sl_custom_ds' must be a single character string or NULL.",
         call. = FALSE)
  }
  if (!is.null(styles) && !is.list(styles)) {
    stop("'styles' must be a named list of openxlsx style objects or NULL.",
         call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Progress message (SAS line 41: %put SCRIPT LAUNCHER METADATA OUTPUT)
  # ---------------------------------------------------------------------------
  cli::cli_inform("SCRIPT LAUNCHER METADATA OUTPUT")

  # ---------------------------------------------------------------------------
  # Task 2.1: Create workbook (SAS line 45: %wb)
  # Replaces SAS SpreadsheetML XML preamble via create_workbook() from

  # xml_output.R
  # ---------------------------------------------------------------------------
  wbtitle <- stringr::str_c(panel_title, " Metadata Summary")
  wb <- create_workbook(title = wbtitle)

  # ---------------------------------------------------------------------------
  # Task 2.1b: Create style gallery (SAS line 46: %styles)
  # Replaces SAS SpreadsheetML style definitions via create_workbook_styles()
  # from xml_output.R. Provides Header, Default10, Default10Wrap, etc.
  # ---------------------------------------------------------------------------
  if (is.null(styles)) {
    styles <- create_workbook_styles()
  }

  # ---------------------------------------------------------------------------
  # Task 2.3: Group/subset preprocessing (SAS lines 73-74)
  # Replaces SAS %group_subset_pp which creates macro variables sl_group_desc,
  # sl_subset_desc, sl_custom_ds. Uses group_subset_pp() from sl_gs_output.R.
  # ---------------------------------------------------------------------------
  pp_result <- group_subset_pp(
    sl_group    = sl_group,
    sl_subset   = sl_subset,
    sl_datasets = sl_datasets
  )

  # Extract description strings from preprocessing result
  sl_group_desc  <- pp_result$sl_group_desc
  sl_subset_desc <- pp_result$sl_subset_desc

  # Use caller-provided sl_custom_ds if given; otherwise fall back to the
  # value computed by group_subset_pp()
  if (is.null(sl_custom_ds)) {
    sl_custom_ds_val <- pp_result$sl_custom_ds
    # Guard against NULL return from pp_result
    if (is.null(sl_custom_ds_val)) {
      sl_custom_ds_val <- ""
    }
  } else {
    sl_custom_ds_val <- sl_custom_ds
  }

  # ---------------------------------------------------------------------------
  # Task 2.2: Create "Metadata Summary" worksheet (SAS lines 48-70)
  # Replaces SAS ws_metadata_start, ws_metadata_table_start datasets.
  # ---------------------------------------------------------------------------
  sheet_name <- "Metadata Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths: SAS SpreadsheetML pixel widths → openxlsx character units.
  # SAS: <Column ss:Width="150"/> <Column ss:Width="150"/> <Column/>
  # Approximate conversion: SAS px / 7 ≈ openxlsx character units.
  # 150px ≈ 21.4 character units; third column auto-sized.
  openxlsx::setColWidths(wb, sheet_name, cols = 1:2,
                         widths = c(150 / 7, 150 / 7))

  # ---------------------------------------------------------------------------
  # Task 2.4: Build header data tibble (SAS lines 76-126)
  # Build header as a single-column data frame for write_formatted_data().
  # Each row maps to a SAS DATA step output observation with Data, StyleID,
  # and optional MergeAcross / Height attributes.
  # ---------------------------------------------------------------------------

  # Capture current date/time for the "Analysis run date" row.
  # SAS: put(date(), e8601da.) || ' ' || put(time(), timeampm11.)
  run_date <- format(Sys.Date(), "%Y-%m-%d")
  run_time <- format(Sys.time(), "%I:%M %p")
  run_datetime_str <- stringr::str_c("Analysis run date: ", run_date, " ",
                                     run_time)

  # Custom datasets text (SAS line 121)
  custom_ds_text <- if (nchar(sl_custom_ds_val) > 0L) {
    stringr::str_c("Custom datasets: ", sl_custom_ds_val)
  } else {
    "Custom datasets: No custom datasets"
  }

  # Determine if panel_message should be included (SAS line 107)
  include_panel_msg <- stringr::str_length(panel_desc) > 0L

  # ---- Assemble header rows ----
  # SAS row counter starts at 0 and increments with each output statement.
  # We build parallel vectors for content, style names, and special attributes.

  header_texts  <- character(0L)
  header_styles <- character(0L)

  # Row 1: blank (SAS lines 85-86)
  header_texts  <- c(header_texts, "")
  header_styles <- c(header_styles, "")

  # Row 2: Title (SAS lines 88-89)
  header_texts  <- c(header_texts,
                     stringr::str_c(panel_title, " Metadata Summary"))
  header_styles <- c(header_styles, "Header")

  # Row 3: blank (SAS lines 91-92)
  header_texts  <- c(header_texts, "")
  header_styles <- c(header_styles, "")

  # Row 4: NDA/BLA (SAS lines 96-97)
  header_texts  <- c(header_texts, stringr::str_c("NDA/BLA: ", ndabla))
  header_styles <- c(header_styles, "Default10")

  # Row 5: Study (SAS lines 98-99)
  header_texts  <- c(header_texts, stringr::str_c("Study: ", studyid))
  header_styles <- c(header_styles, "Default10")

  # Row 6: Analysis run date (SAS lines 100-101)
  header_texts  <- c(header_texts, run_datetime_str)
  header_styles <- c(header_styles, "Default10")

  # Row 7: blank increment (SAS line 102-103 — row counter advances)
  # SAS increments row but no output; we add a blank row
  header_texts  <- c(header_texts, "")
  header_styles <- c(header_styles, "")

  # Row 8: blank (SAS lines 104-105)
  header_texts  <- c(header_texts, "")
  header_styles <- c(header_styles, "")

  # Track which rows need special handling (merge, height)
  merge_info_rows <- list()

  # Conditional panel_message row (SAS lines 107-118)
  if (include_panel_msg) {
    header_texts  <- c(header_texts, panel_message)
    header_styles <- c(header_styles, "Default10Wrap")

    # This row needs MergeAcross=6 and calculated Height.
    # SAS: MergeAcross = 6; Height = ceil(length(trim(panel_desc))/150)*12.75
    trimmed_desc <- stringr::str_trim(panel_desc)
    msg_height   <- ceiling(stringr::str_length(trimmed_desc) / 150) * 12.75
    msg_row_idx  <- length(header_texts)

    merge_info_rows[[length(merge_info_rows) + 1L]] <- list(
      row          = msg_row_idx,
      col          = 1L,
      merge_across = 6L,
      merge_down   = 0L,
      height       = msg_height
    )

    # Blank row after panel_message (SAS lines 116-117)
    header_texts  <- c(header_texts, "")
    header_styles <- c(header_styles, "")
  }

  # Custom datasets row (SAS line 120-121)
  header_texts  <- c(header_texts, custom_ds_text)
  header_styles <- c(header_styles, "Default10")

  # Grouping description row (SAS lines 122-123)
  header_texts  <- c(header_texts,
                     stringr::str_c("Grouping: ", sl_group_desc))
  header_styles <- c(header_styles, "Default10")

  # Subsetting description row (SAS lines 124-125)
  header_texts  <- c(header_texts,
                     stringr::str_c("Subsetting: ", sl_subset_desc))
  header_styles <- c(header_styles, "Default10")

  # ---------------------------------------------------------------------------
  # Write header data using write_formatted_data() from xml_output.R
  # Replaces SAS %markup(ws_&ds._header_data, ws_&ds._header) at line 128.
  # Build a single-column data.frame and a matching style_map.
  # ---------------------------------------------------------------------------
  header_df       <- data.frame(col1 = header_texts, stringsAsFactors = FALSE)
  header_style_df <- data.frame(col1 = header_styles, stringsAsFactors = FALSE)

  # Build merge_info data.frame from collected merge rows
  if (length(merge_info_rows) > 0L) {
    merge_info_df <- data.frame(
      row          = vapply(merge_info_rows, `[[`, integer(1L), "row"),
      col          = vapply(merge_info_rows, `[[`, integer(1L), "col"),
      merge_across = vapply(merge_info_rows, `[[`, integer(1L), "merge_across"),
      merge_down   = vapply(merge_info_rows, `[[`, integer(1L), "merge_down"),
      stringsAsFactors = FALSE
    )
  } else {
    merge_info_df <- NULL
  }

  # Write the header section; returns the next available row
  next_row <- write_formatted_data(
    wb        = wb,
    sheet     = sheet_name,
    data      = header_df,
    start_row = 1L,
    start_col = 1L,
    styles    = styles,
    style_map = header_style_df,
    merge_info = merge_info_df
  )

  # Apply row height for panel_message rows (openxlsx setRowHeights is
  # separate from write_formatted_data merge_info)
  if (length(merge_info_rows) > 0L) {
    for (mi in merge_info_rows) {
      if (!is.null(mi$height) && mi$height > 0) {
        openxlsx::setRowHeights(wb, sheet_name,
                                rows    = mi$row,
                                heights = mi$height)
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Task 2.6: Page setup / worksheet settings (SAS lines 130-148)
  # Replaces SAS WorksheetOptions XML block via apply_page_setup() from
  # xml_output.R. Configures landscape orientation, page header/footer with
  # panel title, NDA/BLA, study ID, and fit-to-page printing.
  # ---------------------------------------------------------------------------
  # SAS header: &L panel_title &R NDA/BLA ndabla \n Study studyid
  header_right_str <- stringr::str_c("NDA/BLA ", ndabla, "\nStudy ", studyid)

  apply_page_setup(
    wb            = wb,
    sheet         = sheet_name,
    orientation   = "landscape",
    header_left   = panel_title,
    header_right  = header_right_str,
    footer_center = "Page &P of &N",
    fit_to_width  = TRUE,
    fit_to_height = 100,
    scale         = 78
  )

  # ---------------------------------------------------------------------------
  # Task 2.5: Conditionally add grouping/subsetting detail worksheet
  # (SAS lines 150-162)
  #
  # SAS: %if %upcase(%substr(&sl_group_desc.,1,1)) ne N %then ws_sl_gs_group;
  #      %if %upcase(%substr(&sl_subset_desc.,1,1)) ne N %then ws_sl_gs_subset;
  #
  # In SAS these detail rows were assembled into the same worksheet. In R, the
  # group_subset_write_ws() function from sl_gs_output.R creates a dedicated
  # "Grouping and Subsetting" worksheet within the same workbook, providing
  # richer formatting (explanatory paragraphs, column headers, merged cells).
  # We invoke it when at least one of grouping or subsetting is active.
  # ---------------------------------------------------------------------------
  has_grouping   <- !stringr::str_starts(toupper(sl_group_desc), "N")
  has_subsetting <- !stringr::str_starts(toupper(sl_subset_desc), "N")

  if (has_grouping || has_subsetting) {
    group_subset_write_ws(
      wb        = wb,
      pp_result = pp_result,
      ndabla    = ndabla,
      studyid   = studyid,
      styles    = styles
    )
  }

  # ---------------------------------------------------------------------------
  # Task 2.7: Save workbook (SAS lines 164-169)
  # Replaces SAS data _null_; set wb; file "&md_file."; put string; pattern.
  # openxlsx::saveWorkbook() writes the modern OOXML (.xlsx) format directly.
  # ---------------------------------------------------------------------------
  output_dir <- dirname(md_file)
  if (nchar(output_dir) > 0L && output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  openxlsx::saveWorkbook(wb, md_file, overwrite = TRUE)
  cli::cli_inform("Metadata summary saved to: {md_file}")

  # ---------------------------------------------------------------------------
  # Task 2.8: Cleanup (SAS line 171)
  # SAS: proc datasets library=work nolist nodetails; delete ws_&ds.: ws_sl_gs:;
  # R: No explicit cleanup needed — R garbage collection handles temporaries.
  # ---------------------------------------------------------------------------

  return(invisible(wb))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS SpreadsheetML XML output replaced by openxlsx workbook.
#    - SAS macro variables (panel_title, ndabla, studyid, etc.) become
#      explicit named R function arguments with no global side-effects.
#    - Column widths converted from SAS pixel units (150px) to approximate
#      openxlsx character-width units (150/7 ≈ 21.4).
#    - SAS %group_subset_pp and %group_subset_xml_out(delete_im=N) are
#      replaced by group_subset_pp() and group_subset_write_ws() from
#      sl_gs_output.R respectively.
#    - In SAS, grouping/subsetting detail rows were assembled into the same
#      "Metadata Summary" worksheet. In R, group_subset_write_ws() creates a
#      dedicated "Grouping and Subsetting" worksheet within the same workbook,
#      providing equivalent content in a cleaner multi-sheet structure.
#    - SAS %markup header pipeline replaced by write_formatted_data() from
#      xml_output.R with a per-row style_map data.frame and merge_info.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Row height calculation for panel_message: SAS formula
#      ceil(length(trim(panel_desc))/150)*12.75 is replicated exactly, but
#      font rendering may cause slight visual differences between
#      SpreadsheetML (.xml) and OOXML (.xlsx) formats.
#    - Excel column width units differ between SpreadsheetML (pixel-based)
#      and openxlsx (character-unit-based); the divisor of 7 is approximate.
#    - Date/time formatting: SAS put(time(),timeampm11.) uses 11-char width
#      with leading space; R format(Sys.time(), "%I:%M %p") may differ in
#      leading-zero behaviour for hours (R pads with zero, SAS pads with
#      space).
# NO DIRECT R EQUIVALENT:
#    - SAS SpreadsheetML XML string building → openxlsx API calls
#      (functionally equivalent output).
#    - SAS FILE/PUT text streaming → openxlsx::saveWorkbook().
#    - SAS %wb/%styles macro pair → create_workbook() + create_workbook_styles().
#    - SAS ws_metadata_start/end datasets → openxlsx::addWorksheet().
#    - SAS ws_metadata_table_start/end datasets → implicit in openxlsx.
#    - SAS %xml_tag_def/%xml_init variable declarations → not needed in R.
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: AAP-mandated replacement for SAS SpreadsheetML XML engine.
#      Produces modern OOXML (.xlsx) format with no Java dependency.
#    - cli: User-facing informative messages replacing SAS %PUT statements,
#      per AAP mandate for cli-based messaging.
#    - stringr: Tidyverse string manipulation replacing SAS character functions,
#      per AAP mandate (tidyverse over base R).
# OPEN QUESTIONS:
#    - Page header/footer rendering: confirm exact layout requirements match
#      SAS ODS behaviour across Excel versions.
#    - Merged cell height auto-calculation: verify the ceil(len/150)*12.75
#      formula produces acceptable visual results in OOXML format.
#    - Multi-sheet vs single-sheet layout: SAS assembled everything in one
#      worksheet; R uses two worksheets when grouping/subsetting is active.
#      Confirm this is acceptable for downstream Script Launcher consumers.
# ============================================================
