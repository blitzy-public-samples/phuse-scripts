# ============================================================================
# PROGRAM: md_output.R — Generic Panel Metadata Output (Metadata Summary)
# DESCRIPTION: Create an Excel workbook with the settings the user chose for
#   a panel. Generates a "Metadata Summary" worksheet containing panel title,
#   NDA/BLA identifier, study ID, analysis run date, panel description, and
#   grouping/subsetting descriptions. Optionally adds a detailed "Grouping
#   and Subsetting" worksheet when grouping or subsetting metadata is present.
#
# ORIGINAL: contributed/MedDRA/ZZ_Utilities/md_output.sas (173 lines)
# AUTHOR: David Kretch (original SAS), migrated to R
# DATE: March 14, 2011 (original SAS), migrated 2026
#
# MIGRATION: SAS SpreadsheetML XML streaming (DATA _NULL_ + PUT) is replaced
#   by openxlsx in-memory workbook operations. The SAS %metadata_summary macro
#   becomes a parameterized R function. SAS global macro variables (&panel_title,
#   &ndabla, &studyid, &panel_desc, &panel_message) become named function
#   arguments with matching defaults. SAS %include of xml_output.sas and
#   sl_gs_output.sas is replaced by source() calls. The SAS %wb/%styles/%markup
#   macro calls are replaced by wb_create(), create_styles(), and
#   write_annotated() from xml_output.R.
#
# EXTERNAL FILES USED: xml_output.R  -- workbook creation, style gallery,
#                                       annotated cell writer
#                      sl_gs_output.R -- grouping/subsetting preprocessing
#                                        and worksheet output
#
# PARAMETERS REQUIRED: md_file     -- filename and path of the output
#                      panel_title -- title of the analysis panel
#                      ndabla      -- NDA/BLA identifier
#                      studyid     -- study identifier
#
# EXPORTS: metadata_summary
# ============================================================================

# ----------------------------------------------------------------------------
# Library Loading
# AAP mandates tidyverse over base R; cli for user-facing messages
# ----------------------------------------------------------------------------
library(openxlsx)
library(dplyr)
library(stringr)
library(cli)

# ----------------------------------------------------------------------------
# Internal Dependency: xml_output.R
# Provides: wb_create(), create_workbook_styles() [aliased as create_styles()],
#           write_annotated()
# In the SAS original, xml_output.sas was %included at line 33.
# In R, we conditionally source it if the functions are not already available.
# ----------------------------------------------------------------------------
if (!exists("wb_create", mode = "function") ||
    !exists("create_workbook_styles", mode = "function") ||
    !exists("write_annotated", mode = "function")) {
  local({
    # Determine script directory safely (R < 4.4 lacks base %||%)
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

# ----------------------------------------------------------------------------
# Internal Dependency: sl_gs_output.R
# Provides: group_subset_pp(), group_subset_xml_out()
# In the SAS original, sl_gs_output.sas was %included at line 34.
# ----------------------------------------------------------------------------
if (!exists("group_subset_pp", mode = "function") ||
    !exists("group_subset_xml_out", mode = "function")) {
  local({
    # Determine script directory safely (R < 4.4 lacks base %||%)
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
# metadata_summary — Generate Panel Metadata Summary Workbook
# Replaces SAS %metadata_summary macro (lines 39-173 of md_output.sas)
# ============================================================================
#' Generate Panel Metadata Summary Workbook
#'
#' Creates an Excel workbook containing a "Metadata Summary" worksheet with
#' panel configuration information: panel title, NDA/BLA identifier, study ID,
#' analysis run date, panel description narrative, custom dataset information,
#' and grouping/subsetting descriptions. When grouping or subsetting metadata
#' is present, a detailed "Grouping and Subsetting" worksheet is also added.
#'
#' Replaces the SAS \code{\%metadata_summary} macro which generated
#' SpreadsheetML XML and wrote it to disk via \code{DATA _NULL_}.
#'
#' @param md_file       Character. Output file path for the Excel workbook.
#'   Replaces SAS \code{md_file=} macro parameter (line 39).
#' @param panel_title   Character. Title of the analysis panel. Replaces SAS
#'   \code{&panel_title.} global macro variable (line 36, 89, 135). Default
#'   \code{""}.
#' @param ndabla        Character. NDA/BLA identifier. Replaces SAS
#'   \code{&ndabla.} global macro variable (line 97, 136). Default \code{""}.
#' @param studyid       Character. Study identifier. Replaces SAS
#'   \code{&studyid.} global macro variable (line 99, 136). Default \code{""}.
#' @param panel_desc    Character. Panel description text used for row height
#'   calculation (SAS line 113). Default \code{""}.
#' @param panel_message Character. Panel description message to display in the
#'   worksheet. Replaces SAS \code{&panel_message.} macro variable (line 111).
#'   Defaults to \code{panel_desc} if not provided.
#' @param sl_group      Tibble/data.frame or NULL. Script Launcher grouping
#'   metadata passed to \code{group_subset_pp()}. Default \code{NULL}.
#' @param sl_subset     Tibble/data.frame or NULL. Script Launcher subsetting
#'   metadata passed to \code{group_subset_pp()}. Default \code{NULL}.
#' @param sl_datasets   Tibble/data.frame or NULL. Script Launcher datasets
#'   metadata passed to \code{group_subset_pp()}. Default \code{NULL}.
#' @param domain_data   Named list or NULL. Named list of data frames keyed
#'   by domain name for variable label lookup, passed to
#'   \code{group_subset_pp()}. Default \code{NULL}.
#' @param config        List. Additional configuration settings. Currently
#'   reserved for future use. Default \code{list()}.
#'
#' @return The openxlsx workbook object (invisibly). The workbook is also
#'   saved to \code{md_file}.
#'
#' @details
#' The function performs the following steps (mirroring SAS lines 39-173):
#' \enumerate{
#'   \item Creates a workbook with \code{wb_create()} (replaces SAS
#'     \code{\%wb} at line 45).
#'   \item Retrieves the shared style gallery via \code{create_styles()}
#'     (replaces SAS \code{\%styles} at line 46).
#'   \item Adds a "Metadata Summary" worksheet with column widths matching
#'     the SAS layout (lines 63-65: 150pt, 150pt, auto).
#'   \item Calls \code{group_subset_pp()} for grouping/subsetting
#'     preprocessing (replaces SAS \code{\%group_subset_pp} at line 73).
#'   \item Calls \code{group_subset_xml_out()} to add a detailed
#'     "Grouping and Subsetting" worksheet (replaces SAS
#'     \code{\%group_subset_xml_out(delete_im=N)} at line 74).
#'   \item Constructs metadata header rows as an annotation tibble and
#'     writes them via \code{write_annotated()} (replaces SAS DATA step at
#'     lines 77-126 and \code{\%markup} at line 128).
#'   \item Configures page setup with landscape orientation, headers, and
#'     footers (replaces SAS WorksheetOptions XML at lines 132-147).
#'   \item Saves the workbook to \code{md_file} (replaces SAS
#'     \code{DATA _NULL_} file output at lines 165-169).
#' }
#'
#' @export
metadata_summary <- function(md_file,
                             panel_title = "",
                             ndabla = "",
                             studyid = "",
                             panel_desc = "",
                             panel_message = panel_desc,
                             sl_group = NULL,
                             sl_subset = NULL,
                             sl_datasets = NULL,
                             domain_data = NULL,
                             config = list()) {

  # --------------------------------------------------------------------------
  # Input validation
  # --------------------------------------------------------------------------
  if (missing(md_file) || !is.character(md_file) || !nzchar(md_file)) {
    stop("metadata_summary: 'md_file' must be a non-empty character string ",
         "specifying the output file path.", call. = FALSE)
  }

  # Ensure output directory exists
  output_dir <- dirname(md_file)
  if (nzchar(output_dir) && output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Coerce parameters to character, guarding against NULL / NA
  panel_title   <- if (is.null(panel_title) || is.na(panel_title)) "" else as.character(panel_title)
  ndabla        <- if (is.null(ndabla) || is.na(ndabla)) "" else as.character(ndabla)
  studyid       <- if (is.null(studyid) || is.na(studyid)) "" else as.character(studyid)
  panel_desc    <- if (is.null(panel_desc) || is.na(panel_desc)) "" else as.character(panel_desc)
  panel_message <- if (is.null(panel_message) || is.na(panel_message)) panel_desc else as.character(panel_message)

  # --------------------------------------------------------------------------
  # Log start (replaces SAS %put at line 41)
  # --------------------------------------------------------------------------
  cli::cli_alert_info("SCRIPT LAUNCHER METADATA OUTPUT")

  # --------------------------------------------------------------------------
  # Workbook title (replaces SAS %let wbtitle at line 36)
  # --------------------------------------------------------------------------
  wbtitle <- str_c(str_trim(panel_title), " Metadata Summary")

  # --------------------------------------------------------------------------
  # Step 1: Create workbook (replaces SAS %wb at line 45)
  # Uses wb_create() from xml_output.R which sets title and author
  # --------------------------------------------------------------------------
  wb <- wb_create(title = wbtitle)

  # --------------------------------------------------------------------------
  # Step 2: Retrieve shared style gallery (replaces SAS %styles at line 46)
  # Provides Header, Default10, Default10Wrap styles for metadata rows
  # --------------------------------------------------------------------------
  styles <- create_styles()

  # --------------------------------------------------------------------------
  # Step 3: Add "Metadata Summary" worksheet
  # Replaces SAS ws_metadata_start dataset (lines 49-52)
  # --------------------------------------------------------------------------
  sheet_name <- "Metadata Summary"
  openxlsx::addWorksheet(wb, sheet_name)

  # Column widths (SAS lines 63-65: 150pt, 150pt, auto)
  # SAS SpreadsheetML Column ss:Width is in points; openxlsx uses character
  # widths. 150 SAS points ~ 21.4 character widths in openxlsx.
  openxlsx::setColWidths(wb, sheet_name, cols = 1L:3L,
                          widths = c(21.4, 21.4, 8.43))

  # --------------------------------------------------------------------------
  # Step 4: Grouping/subsetting preprocessing
  # Replaces SAS %group_subset_pp at line 73
  # Returns sl_group_desc, sl_subset_desc, sl_custom_ds narratives
  # --------------------------------------------------------------------------
  pp_result <- group_subset_pp(
    sl_group    = sl_group,
    sl_subset   = sl_subset,
    sl_datasets = sl_datasets,
    domain_data = domain_data
  )

  # --------------------------------------------------------------------------
  # Step 5: Add detailed Grouping and Subsetting worksheet
  # Replaces SAS %group_subset_xml_out(delete_im=N) at line 74
  # In SAS, the G&S data was embedded in the same worksheet; in R, a separate
  # richly formatted worksheet is added via group_subset_xml_out().
  # --------------------------------------------------------------------------
  group_subset_xml_out(
    wb                   = wb,
    pp_result            = pp_result,
    ndabla               = ndabla,
    studyid              = studyid,
    delete_intermediate  = FALSE
  )

  # --------------------------------------------------------------------------
  # Step 6: Build metadata header rows as annotations
  # Replaces SAS DATA step ws_metadata_header_data (lines 77-126)
  # and %markup conversion (line 128)
  #
  # Row layout mirrors SAS exactly:
  #   Row 1: blank
  #   Row 2: "{panel_title} Metadata Summary" — Header style
  #   Row 3: blank
  #   Row 4: "NDA/BLA: {ndabla}" — Default10 style
  #   Row 5: "Study: {studyid}" — Default10 style
  #   Row 6: "Analysis run date: {date} {time}" — Default10 style
  #   Row 7: (implicit increment from SAS, blank)
  #   Row 8: blank
  #   [Optional if panel_desc non-empty]:
  #     Row 9: "{panel_message}" — Default10Wrap, merged across 6 cols
  #     Row 10: blank
  #   Row N:   "Custom datasets: {text}" — Default10
  #   Row N+1: "Grouping: {sl_group_desc}" — Default10
  #   Row N+2: "Subsetting: {sl_subset_desc}" — Default10
  # --------------------------------------------------------------------------

  # Build the run date string matching SAS put(date(),e8601da.) || ' ' ||
  # put(time(),timeampm11.) — ISO-8601 date + AM/PM time
  run_date_str <- str_c(
    "Analysis run date: ",
    format(Sys.Date(), "%Y-%m-%d"),
    " ",
    format(Sys.time(), "%I:%M:%S %p")
  )

  # Custom datasets description (SAS line 121)
  custom_ds_text <- dplyr::if_else(
    nzchar(pp_result$sl_custom_ds),
    str_c("Custom datasets: ", pp_result$sl_custom_ds),
    "Custom datasets: No custom datasets"
  )

  # Start building annotation rows using dplyr::tibble and bind_rows
  # Each row becomes one cell annotation for write_annotated()
  row_counter <- 0L

  # Helper to create a single-cell annotation row
  make_annotation_row <- function(row_num, value, style_id = "",
                                  merge_across = NA_integer_,
                                  height = NA_real_) {
    dplyr::tibble(
      row_num      = as.integer(row_num),
      col_num      = 1L,
      var_name     = "Data",
      value        = as.character(value),
      data_type    = "String",
      style_id     = as.character(style_id),
      height       = as.numeric(height),
      merge_across = as.integer(merge_across),
      merge_down   = NA_integer_,
      formula      = NA_character_,
      comment      = NA_character_,
      name         = NA_character_,
      array_range  = NA_character_,
      is_bottom    = FALSE
    )
  }

  # Row 1: blank (SAS line 85-86: Row=1, Data='')
  row_counter <- row_counter + 1L
  ann_rows <- make_annotation_row(row_counter, "")

  # Row 2: Panel title + "Metadata Summary" (SAS line 88-89: Header style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, str_c(panel_title, " Metadata Summary"),
                        style_id = "Header")
  )

  # Row 3: blank (SAS line 91-92)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, "")
  )

  # Row 4: "NDA/BLA: {ndabla}" (SAS line 96-97: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, str_c("NDA/BLA: ", ndabla),
                        style_id = "Default10")
  )

  # Row 5: "Study: {studyid}" (SAS line 98-99: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, str_c("Study: ", studyid),
                        style_id = "Default10")
  )

  # Row 6: Analysis run date (SAS line 100-101: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, run_date_str, style_id = "Default10")
  )

  # Row 7: SAS increments row but outputs nothing (implicit empty row)
  row_counter <- row_counter + 1L

  # Row 8: blank (SAS line 104-105)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, "")
  )

  # Optional: Panel description (SAS lines 107-118)
  # Only included when panel_desc is non-empty (SAS: %if %length(&panel_desc.) > 0)
  if (nzchar(str_trim(panel_desc))) {
    row_counter <- row_counter + 1L
    # Calculate row height based on text length (SAS line 113):
    # Height = ceil(length(trim("&panel_desc."))/150)*12.75
    desc_height <- ceiling(nchar(str_trim(panel_desc)) / 150) * 12.75
    ann_rows <- dplyr::bind_rows(
      ann_rows,
      make_annotation_row(row_counter, panel_message,
                          style_id     = "Default10Wrap",
                          merge_across = 6L,
                          height       = desc_height)
    )

    # Blank row after panel description (SAS lines 116-117)
    row_counter <- row_counter + 1L
    ann_rows <- dplyr::bind_rows(
      ann_rows,
      make_annotation_row(row_counter, "")
    )
  }

  # Custom datasets row (SAS line 120-121: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter, custom_ds_text, style_id = "Default10")
  )

  # Grouping description row (SAS line 122-123: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter,
                        str_c("Grouping: ", pp_result$sl_group_desc),
                        style_id = "Default10")
  )

  # Subsetting description row (SAS line 124-125: Default10 style)
  row_counter <- row_counter + 1L
  ann_rows <- dplyr::bind_rows(
    ann_rows,
    make_annotation_row(row_counter,
                        str_c("Subsetting: ", pp_result$sl_subset_desc),
                        style_id = "Default10")
  )

  # Mark last row as bottom for visual closure
  ann_rows <- ann_rows %>%
    dplyr::mutate(is_bottom = dplyr::if_else(
      row_num == max(row_num), TRUE, FALSE
    ))

  # --------------------------------------------------------------------------
  # Write annotations to the Metadata Summary worksheet
  # Replaces SAS %markup(ws_metadata_header_data, ws_metadata_header) at line 128
  # --------------------------------------------------------------------------
  write_annotated(
    wb          = wb,
    sheet       = sheet_name,
    annotations = ann_rows,
    styles      = styles,
    start_row   = 0L,
    start_col   = 0L
  )

  # --------------------------------------------------------------------------
  # Step 7: Page setup and header/footer
  # Replaces SAS WorksheetOptions XML (lines 132-147)
  #
  # SAS layout: landscape orientation, fit-to-page, 78% scale, 600 DPI
  # SAS header: panel_title on left, NDA/BLA + Study on right
  # SAS footer: "Page X of N" centered
  # --------------------------------------------------------------------------
  openxlsx::pageSetup(
    wb          = wb,
    sheet       = sheet_name,
    orientation = "landscape",
    fitToWidth  = TRUE,
    fitToHeight = FALSE
  )

  # Header: left = panel_title, right = NDA/BLA + Study
  # Footer: center = "Page &P of &N"
  # Replaces SAS Header/Footer XML (lines 135-137)
  header_left  <- panel_title
  header_right <- str_c("NDA/BLA ", ndabla, "\nStudy ", studyid)
  footer_center <- "Page &[Page] of &[Pages]"

  openxlsx::setHeaderFooter(
    wb     = wb,
    sheet  = sheet_name,
    header = c(header_left, NA, header_right),
    footer = c(NA, footer_center, NA)
  )

  # --------------------------------------------------------------------------
  # Step 8: Save workbook to disk
  # Replaces SAS DATA _NULL_ file output (lines 165-169)
  # --------------------------------------------------------------------------
  openxlsx::saveWorkbook(wb, file = md_file, overwrite = TRUE)

  cli::cli_alert_info("Metadata summary saved to {.file {md_file}}")

  # --------------------------------------------------------------------------
  # No cleanup needed in R (SAS line 171 deleted intermediate datasets)
  # --------------------------------------------------------------------------

  invisible(wb)
}


# ============================================================================
# MIGRATION NOTES
# ============================================================================
# ASSUMPTIONS:
#   1. Metadata output matches SAS Excel XML structure — panel title, NDA/BLA,
#      study ID, analysis run date, panel description, and grouping/subsetting
#      narratives are all present in the output workbook.
#   2. Grouping/subsetting narratives from group_subset_pp() (sl_gs_output.R)
#      are available and return the expected list structure with elements
#      sl_group_desc, sl_subset_desc, sl_custom_ds.
#   3. The SAS global macro variables (&panel_title, &ndabla, &studyid,
#      &panel_desc, &panel_message) are mapped to named function arguments
#      with matching defaults.
#   4. The panel_message parameter defaults to panel_desc when not explicitly
#      provided, matching SAS behavior where &panel_message is typically set
#      equal to &panel_desc.
#   5. Column widths are converted from SAS SpreadsheetML points to openxlsx
#      character widths (150pt ~ 21.4 character widths).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   None — this is metadata output only (titles, descriptions, dates).
#   No statistical computations are performed.
#
# NO DIRECT R EQUIVALENT:
#   1. SpreadsheetML XML streaming (SAS DATA _NULL_ + PUT) is replaced by
#      openxlsx in-memory workbook operations.
#   2. SAS %xml_tag_def / %xml_init macros (line 78-79) have no R equivalent;
#      openxlsx handles variable initialization internally.
#   3. SAS DATA _NULL_ file output (lines 165-169) is replaced by
#      openxlsx::saveWorkbook().
#   4. SAS PROC DATASETS cleanup (line 171) is not needed in R; intermediate
#      objects are garbage collected.
#   5. In SAS, grouping/subsetting detail data was embedded within the same
#      "Metadata Summary" worksheet as XML fragments. In R, the detailed
#      grouping/subsetting table is rendered as a separate "Grouping and
#      Subsetting" worksheet via group_subset_xml_out(), which is the
#      idiomatic openxlsx approach for multi-table workbooks.
#   6. SAS WorksheetOptions XML for print settings (Scale=78, FitToPage,
#      HorizontalResolution=600, VerticalResolution=0) are partially mapped
#      to openxlsx::pageSetup(). openxlsx does not support exact DPI or
#      scale percentage controls.
#
# PACKAGE SELECTION RATIONALE:
#   - openxlsx: Selected per AAP for Excel output, replacing SpreadsheetML
#     XML generation. Provides in-memory workbook API with cell-level styling,
#     worksheet management, page setup, and header/footer configuration.
#   - dplyr: Selected per AAP (tidyverse mandate) for tibble construction
#     and row binding when assembling metadata annotation rows.
#   - stringr: Selected per AAP (tidyverse mandate) for string concatenation
#     (str_c) and trimming (str_trim), replacing SAS || and TRIM/LEFT.
#   - cli: Selected for consistent user-facing logging across all migrated
#     utility files (xml_output.R, sl_gs_output.R, err_output.R).
#
# OPEN QUESTIONS:
#   None identified. The metadata output is a straightforward configuration
#   summary with no statistical logic or ambiguous SAS behavior.
# ============================================================================
