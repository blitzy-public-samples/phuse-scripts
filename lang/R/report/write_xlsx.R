# =============================================================================
# write_xlsx.R
# =============================================================================
# Migrated from: lang/SAS/report/sas2xlsx/src/sas2xlsx.sas (669 lines)
# Original SAS macro: %sas2xlsx by Edwin van Stein (Astellas Pharma, 2015)
# Migration: SAS manual OOXML XML generation + zip → openxlsx native workbook
#
# Purpose: Export one or more data frames to an XLSX workbook with configurable
#          headers (labels, names, or both), auto-filters, frozen panes,
#          column widths, and variable exclusion.
#
# Copyright (c) 2015 Edwin van Stein — MIT License (see original SAS source)
# R migration copyright follows the same MIT License terms.
# =============================================================================

# ---------------------------------------------------------------------------
# External package imports
# ---------------------------------------------------------------------------
# openxlsx  >= 4.2.5  : Full OOXML workbook creation (replaces manual XML + zip)
# dplyr     >= 1.1.0  : Variable exclusion via select(-all_of())
# cli       >= 3.6.0  : Verbose logging replacing SAS %PUT statements
# purrr     >= 1.0.0  : Functional iteration over columns
# rlang     >= 1.0.0  : %||% null-coalescing operator for fallback logic

# Local alias for rlang's null-coalescing operator
`%||%` <- rlang::`%||%`

#' Export Data Frames to an XLSX Workbook
#'
#' Migrated from the SAS \code{%sas2xlsx} macro. Exports one or more data
#' frames to an XLSX workbook with configurable headers, auto-filters, frozen
#' panes, clamped column widths, and variable exclusion. Uses the
#' \pkg{openxlsx} package for native OOXML workbook creation, replacing the
#' original SAS manual XML generation and system zip pipeline.
#'
#' @param data A data frame or a named list of data frames to write. Each data
#'   frame becomes one worksheet. \strong{REQUIRED}. Replaces the SAS
#'   \code{inlib} + \code{indata} parameters — the caller passes data directly.
#' @param outfile Output XLSX file name \emph{without} extension.
#'   \strong{REQUIRED}.
#' @param outdir Output directory path. Will be created if it does not exist.
#'   \strong{REQUIRED}. No hardcoded paths.
#' @param sheetname Sheet naming strategy: \code{"MEMLABEL"} uses the data
#'   frame label (haven \code{attr(df, "label")}) falling back to the list
#'   element name or \code{"Sheet\var{i}"}; \code{"MEMNAME"} uses the list
#'   element name or \code{"Sheet\var{i}"}. Sheet names are truncated to 31
#'   characters per the XLSX specification.
#' @param headers Header row style: \code{"BOTH"} writes variable labels on
#'   row 1 and variable names on row 2; \code{"LABEL"} writes only labels;
#'   \code{"NAME"} writes only variable names.
#' @param exclude Character vector of variable names to exclude from output.
#'   Replaces the SAS pipe-delimited exclusion string.
#' @param auto_filter Logical; whether to apply auto-filter to the header row.
#'   Replaces SAS \code{Y}/\code{N} flag.
#' @param freeze Logical; whether to freeze the header pane(s). Replaces SAS
#'   \code{Y}/\code{N} flag.
#' @param minwidth Minimum column width in characters (numeric).
#' @param maxwidth Maximum column width in characters (numeric).
#' @param verbose Logical; whether to print informative messages via
#'   \code{cli}. Replaces SAS \code{Y}/\code{N} flag.
#'
#' @return The full output file path (invisibly).
#'
#' @examples
#' \dontrun{
#' # Single data frame
#' write_xlsx(data = iris, outfile = "iris_report", outdir = tempdir())
#'
#' # Named list of data frames (multiple sheets)
#' write_xlsx(
#'   data = list(Iris = iris, Cars = mtcars),
#'   outfile = "multi_sheet",
#'   outdir = tempdir(),
#'   headers = "LABEL",
#'   freeze = TRUE
#' )
#' }
#'
#' @export
write_xlsx <- function(data        = NULL,
                       outfile     = NULL,
                       outdir      = NULL,
                       sheetname   = "MEMLABEL",
                       headers     = "BOTH",
                       exclude     = NULL,
                       auto_filter = TRUE,
                       freeze      = TRUE,
                       minwidth    = 12,
                       maxwidth    = 40,
                       verbose     = TRUE) {

  # -------------------------------------------------------------------------
  # Phase 1: Record start time (mirrors SAS %LET _starttime = %SYSFUNC(datetime()))
  # -------------------------------------------------------------------------
  func_version <- "1.0 (R migration)"
  start_time   <- Sys.time()

  # -------------------------------------------------------------------------
  # Phase 2: Input validation (mirrors SAS lines 97-145)
  # -------------------------------------------------------------------------


  # Required parameter checks -----------------------------------------------
  if (is.null(data)) {
    cli::cli_abort(c(
      "x" = "{.arg data} must be provided.",
      "i" = "Supply a data frame or a named list of data frames."
    ))
  }
  if (is.null(outfile)) {
    cli::cli_abort(c(
      "x" = "{.arg outfile} must be provided.",
      "i" = "Supply the output XLSX file name without extension."
    ))
  }
  if (is.null(outdir)) {
    cli::cli_abort(c(
      "x" = "{.arg outdir} must be provided.",
      "i" = "Supply the output directory path."
    ))
  }

  # Validate data type -------------------------------------------------------
  if (!is.data.frame(data) && !is.list(data)) {
    cli::cli_abort(c(
      "x" = "{.arg data} must be a data frame or a named list of data frames.",
      "i" = "Received class: {.cls {class(data)}}."
    ))
  }

  # Validate sheetname -------------------------------------------------------
  sheetname_upper <- toupper(sheetname)
  if (!sheetname_upper %in% c("MEMLABEL", "MEMNAME")) {
    cli::cli_abort(c(
      "x" = "{.arg sheetname} must be {.val MEMLABEL} or {.val MEMNAME}.",
      "i" = "Received: {.val {sheetname}}."
    ))
  }

  # Validate headers ----------------------------------------------------------
  headers_upper <- toupper(headers)
  if (!headers_upper %in% c("BOTH", "LABEL", "NAME")) {
    cli::cli_abort(c(
      "x" = "{.arg headers} must be {.val BOTH}, {.val LABEL}, or {.val NAME}.",
      "i" = "Received: {.val {headers}}."
    ))
  }

  # Validate logical flags ----------------------------------------------------
  if (!is.logical(auto_filter) || length(auto_filter) != 1L) {
    cli::cli_abort("{.arg auto_filter} must be a single logical value.")
  }
  if (!is.logical(freeze) || length(freeze) != 1L) {
    cli::cli_abort("{.arg freeze} must be a single logical value.")
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    cli::cli_abort("{.arg verbose} must be a single logical value.")
  }

  # Validate numeric width constraints ----------------------------------------
  if (!is.numeric(minwidth) || length(minwidth) != 1L || minwidth < 1) {
    cli::cli_abort("{.arg minwidth} must be a positive numeric scalar.")
  }
  if (!is.numeric(maxwidth) || length(maxwidth) != 1L || maxwidth < 1) {
    cli::cli_abort("{.arg maxwidth} must be a positive numeric scalar.")
  }
  if (minwidth > maxwidth) {
    cli::cli_abort("{.arg minwidth} ({minwidth}) must not exceed {.arg maxwidth} ({maxwidth}).")
  }

  # Validate exclude ----------------------------------------------------------
  if (!is.null(exclude) && !is.character(exclude)) {
    cli::cli_abort("{.arg exclude} must be a character vector or NULL.")
  }

  # -------------------------------------------------------------------------
  # Data normalisation: single data frame → named list (mirrors SAS indata
  # logic at lines 210-265)
  # -------------------------------------------------------------------------
  if (is.data.frame(data)) {
    # Capture the caller-supplied name for the default sheet label
    data_name <- tryCatch(
      deparse(substitute(data)),
      error = function(e) "Sheet1"
    )
    # Guard against multi-element deparsed names (e.g., long expressions)
    if (length(data_name) != 1L || nchar(data_name) == 0L) {
      data_name <- "Sheet1"
    }
    data_list <- stats::setNames(list(data), data_name)
  } else {
    # Named list of data frames — validate each element
    if (length(data) == 0L) {
      cli::cli_abort(c(
        "x" = "{.arg data} list is empty.",
        "i" = "Provide at least one data frame."
      ))
    }
    # Ensure every element is a data frame
    non_df_idx <- which(!purrr::map_lgl(data, is.data.frame))
    if (length(non_df_idx) > 0L) {
      cli::cli_abort(c(
        "x" = "All elements of {.arg data} must be data frames.",
        "i" = "Non-data-frame element(s) found at position(s): {.val {non_df_idx}}."
      ))
    }
    # If list is unnamed, assign default names
    if (is.null(names(data))) {
      names(data) <- paste0("Sheet", seq_along(data))
    } else {
      # Fill any blank names with defaults
      blank_idx <- which(names(data) == "" | is.na(names(data)))
      if (length(blank_idx) > 0L) {
        names(data)[blank_idx] <- paste0("Sheet", blank_idx)
      }
    }
    data_list <- data
  }

  # Empty data check (mirrors SAS lines 256-264: GALACTUS abort) --------------
  if (length(data_list) == 0L) {
    cli::cli_abort(c(
      "x" = "No data sets to process.",
      "i" = "The normalised data list contains zero elements."
    ))
  }

  # -------------------------------------------------------------------------
  # Output directory creation (mirrors SAS X "mkdir" at line 165)
  # -------------------------------------------------------------------------
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # -------------------------------------------------------------------------
  # Verbose startup logging (mirrors SAS lines 120-127)
  # -------------------------------------------------------------------------
  if (verbose) {
    cli::cli_alert_info(
      "write_xlsx: Version {func_version} started {format(start_time, '%Y-%m-%d %H:%M:%S')}"
    )
    n_sheets <- length(data_list)
    sheet_names_display <- paste(names(data_list), collapse = ", ")
    cli::cli_alert_info(
      "write_xlsx: Parameters — OUTFILE={.val {outfile}}, OUTDIR={.path {outdir}}, SHEETS={n_sheets} ({sheet_names_display})"
    )
    cli::cli_alert_info(
      "write_xlsx: SHEETNAME={.val {sheetname}}, HEADERS={.val {headers}}, FREEZE={.val {freeze}}, AUTO_FILTER={.val {auto_filter}}"
    )
  }

  # =========================================================================
  # Phase 3: Workbook creation — openxlsx pipeline
  # Replaces SAS manual OOXML XML generation (lines 167-321):
  #   _rels/.rels, docProps/app.xml, docProps/core.xml,
  #   xl/_rels/workbook.xml.rels, [Content_Types].xml, xl/workbook.xml
  # =========================================================================
  wb <- openxlsx::createWorkbook()

  # Header style matching SAS bold header formatting --------------------------
  header_style <- openxlsx::createStyle(
    textDecoration = "Bold",
    fgFill         = "#DCE6F1",
    halign         = "center",
    border         = "TopBottomLeftRight",
    borderColour   = "#B2B2B2"
  )

  # =========================================================================
  # Phase 4: Per-sheet processing loop (mirrors SAS loop at lines 352-633)
  # =========================================================================
  sheet_names_vec <- names(data_list)

  for (i in seq_along(data_list)) {

    list_name  <- sheet_names_vec[[i]]
    sheet_data <- data_list[[i]]

    # 4A: Variable exclusion (mirrors SAS lines 338-349) ----------------------
    if (!is.null(exclude) && length(exclude) > 0L) {
      # Only exclude columns that actually exist in the data
      cols_to_drop <- intersect(exclude, names(sheet_data))
      if (length(cols_to_drop) > 0L) {
        sheet_data <- dplyr::select(sheet_data, -dplyr::all_of(cols_to_drop))
      }
    }

    # Guard against zero-column data after exclusion
    if (ncol(sheet_data) == 0L) {
      if (verbose) {
        cli::cli_alert_info(
          "write_xlsx: Skipping sheet {.val {list_name}} — all columns excluded."
        )
      }
      next
    }

    # 4B: Sheet name computation (mirrors SAS lines 303-309) ------------------
    if (sheetname_upper == "MEMLABEL") {
      # Use data frame label → fall back to list element name → default
      df_label <- attr(sheet_data, "label")
      sheet_label <- df_label %||% list_name %||% paste0("Sheet", i)
    } else {
      # MEMNAME: use list element name → default
      sheet_label <- list_name %||% paste0("Sheet", i)
    }

    # Truncate to 31 characters (XLSX specification limit)
    sheet_label <- substr(sheet_label, 1L, 31L)

    # Remove illegal sheet name characters: / \ ? * [ ] :
    # (mirrors SAS COMPRESS function at line 308)
    sheet_label <- stringr::str_replace_all(sheet_label, "[/\\\\?*\\[\\]:]", "")

    # Handle empty sheet name after cleaning
    if (nchar(sheet_label) == 0L) {
      sheet_label <- paste0("Sheet", i)
    }

    # Ensure unique sheet names within the workbook
    existing_sheets <- names(wb)
    if (sheet_label %in% existing_sheets) {
      suffix <- 1L
      candidate <- paste0(substr(sheet_label, 1L, 28L), "_", suffix)
      while (candidate %in% existing_sheets) {
        suffix <- suffix + 1L
        candidate <- paste0(substr(sheet_label, 1L, 28L), "_", suffix)
      }
      sheet_label <- candidate
    }

    # 4C: Add worksheet -------------------------------------------------------
    openxlsx::addWorksheet(wb, sheetName = sheet_label)

    # 4D: Variable metadata extraction (mirrors SAS lines 354-399) ------------
    col_names  <- names(sheet_data)
    n_cols     <- length(col_names)

    # Extract variable labels (haven attr(col, "label") or column name)
    col_labels <- purrr::map_chr(col_names, function(nm) {
      lbl <- attr(sheet_data[[nm]], "label")
      lbl %||% nm
    })

    # 4E: Column width computation (mirrors SAS lines 553-556) ----------------
    # Clamp widths between minwidth and maxwidth, with 1.1x scaling factor
    col_widths <- purrr::map_dbl(seq_len(n_cols), function(j) {
      col_vec <- sheet_data[[j]]

      # Content width: max nchar of formatted values
      if (nrow(sheet_data) > 0L && !all(is.na(col_vec))) {
        content_width <- max(
          nchar(as.character(col_vec[!is.na(col_vec)])),
          na.rm = TRUE
        )
      } else {
        # For empty datasets, use minwidth (mirrors SAS line 445)
        content_width <- 0L
      }

      # Label and name widths
      label_width <- nchar(col_labels[[j]])
      name_width  <- nchar(col_names[[j]])

      # Select the maximum width across content, label, and name
      max_width_val <- max(content_width, label_width, name_width, na.rm = TRUE)

      # Apply SAS formula: round(1.1 * min(max(length, minwidth), maxwidth))
      # Note: bare round() acceptable here — this is column width formatting,
      # not clinical statistical rounding (janitor::round_half_up not required)
      round(1.1 * min(max(max_width_val, minwidth), maxwidth))
    })

    openxlsx::setColWidths(
      wb,
      sheet = sheet_label,
      cols   = seq_len(n_cols),
      widths = col_widths
    )

    # 4F: Header row writing (mirrors SAS lines 452-598) ----------------------
    # The start_row for data depends on which header mode is used
    if (headers_upper == "BOTH") {
      # Row 1: variable labels, Row 2: variable names
      label_matrix <- matrix(col_labels, nrow = 1L)
      name_matrix  <- matrix(col_names,  nrow = 1L)

      openxlsx::writeData(
        wb, sheet_label,
        x = as.data.frame(label_matrix, stringsAsFactors = FALSE),
        startRow = 1L, colNames = FALSE
      )
      openxlsx::writeData(
        wb, sheet_label,
        x = as.data.frame(name_matrix, stringsAsFactors = FALSE),
        startRow = 2L, colNames = FALSE
      )
      openxlsx::addStyle(
        wb, sheet_label,
        style      = header_style,
        rows       = 1L:2L,
        cols       = seq_len(n_cols),
        gridExpand = TRUE
      )
      start_row <- 3L

    } else if (headers_upper == "LABEL") {
      # Row 1: variable labels only
      label_matrix <- matrix(col_labels, nrow = 1L)
      openxlsx::writeData(
        wb, sheet_label,
        x = as.data.frame(label_matrix, stringsAsFactors = FALSE),
        startRow = 1L, colNames = FALSE
      )
      openxlsx::addStyle(
        wb, sheet_label,
        style      = header_style,
        rows       = 1L,
        cols       = seq_len(n_cols),
        gridExpand = TRUE
      )
      start_row <- 2L

    } else {
      # headers_upper == "NAME": Row 1 = variable names only
      name_matrix <- matrix(col_names, nrow = 1L)
      openxlsx::writeData(
        wb, sheet_label,
        x = as.data.frame(name_matrix, stringsAsFactors = FALSE),
        startRow = 1L, colNames = FALSE
      )
      openxlsx::addStyle(
        wb, sheet_label,
        style      = header_style,
        rows       = 1L,
        cols       = seq_len(n_cols),
        gridExpand = TRUE
      )
      start_row <- 2L
    }

    # 4G: Data writing (mirrors SAS lines 601-619) ----------------------------
    # Handle empty datasets gracefully (mirrors SAS lines 401-503)
    if (nrow(sheet_data) == 0L) {
      empty_msg <- data.frame(
        message = "This data set does not contain any observations",
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(
        wb, sheet_label,
        x = empty_msg,
        startRow = start_row,
        colNames = FALSE
      )
      if (verbose) {
        cli::cli_alert_info(
          "write_xlsx: Sheet {.val {sheet_label}} — 0 observations; placeholder row written."
        )
      }
    } else {
      openxlsx::writeData(
        wb, sheet_label,
        x = sheet_data,
        startRow = start_row,
        colNames = FALSE
      )
    }

    # 4H: Auto-filter (mirrors SAS lines 498-500, 624-626) --------------------
    if (auto_filter && nrow(sheet_data) > 0L) {
      # Filter is attached to the "name" row (last header row before data)
      filter_row <- if (headers_upper == "BOTH") 2L else 1L
      openxlsx::addFilter(
        wb, sheet_label,
        rows = filter_row,
        cols = seq_len(n_cols)
      )
    }

    # 4I: Freeze panes (mirrors SAS lines 431-439, 540-549) -------------------
    if (freeze) {
      freeze_row <- if (headers_upper == "BOTH") 3L else 2L
      openxlsx::freezePane(
        wb, sheet_label,
        firstActiveRow = freeze_row,
        firstActiveCol = 1L
      )
    }

    if (verbose) {
      cli::cli_alert_info(
        "write_xlsx: Sheet {.val {sheet_label}} — {nrow(sheet_data)} obs x {n_cols} vars written."
      )
    }

  }
  # =========================================================================
  # Phase 5: Save workbook (mirrors SAS zip/mv at lines 640-648)
  # =========================================================================
  output_path <- file.path(outdir, paste0(outfile, ".xlsx"))

  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)

  # Verbose completion logging (mirrors SAS lines 664-667) --------------------
  if (verbose) {
    # Note: bare round() acceptable here — this is elapsed time display,
    # not clinical statistical rounding (janitor::round_half_up not required)
    elapsed <- round(
      as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      2L
    )
    cli::cli_alert_success(
      "write_xlsx: Workbook saved to {.path {output_path}}. Runtime: {elapsed} seconds."
    )
  }

  # =========================================================================
  # Phase 6: Return value (no cleanup needed — openxlsx handles internally,
  #          unlike SAS's manual temp dir cleanup at lines 654-661)
  # =========================================================================
  invisible(output_path)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS inlib/indata parameters replaced by direct data frame or named
#      list of data frames. Caller must prepare data before calling.
#    - SAS library-based dataset selection (ALL, wildcards, pipe-lists)
#      replaced by R list subsetting. Caller provides the exact data frames.
#    - SAS format-based display widths replaced by nchar() of formatted values.
#    - SAS vvalue() function for formatted display -> format() or as.character()
#    - SAS dataset labels -> haven attr(df, "label")
#    - SAS variable labels -> haven attr(col, "label")
#    - Y/N string flags -> TRUE/FALSE logical arguments
#    - Pipe-delimited exclude list -> character vector
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Column width calculation: SAS uses formatted lengths from SAS formats;
#      R uses nchar(as.character()) which may differ slightly for formatted
#      numeric values
#    - Numeric precision in cells: SAS writes numeric values directly to XML;
#      openxlsx preserves full double precision
#
# NO DIRECT R EQUIVALENT:
#    - SAS sashelp.vtable for dataset discovery -> R caller provides data directly
#    - SAS macro variable context capture (sysmacroname, datetime) -> Sys.time()
#    - SAS COMPRESS=yes option -> not applicable (openxlsx handles compression)
#    - SAS GALACTUS error handler label -> tryCatch() / cli::cli_abort()
#    - SAS system zip command -> openxlsx handles OOXML natively
#    - SAS MISSING=' ' option -> NA handling in R
#
# PACKAGE SELECTION RATIONALE:
#    - openxlsx: Full OOXML workbook creation replacing manual XML generation +
#      zip. Handles styles, filters, freezing, column widths, and multiple sheets.
#      Chosen over writexl (fewer features) and xlsx (Java dependency).
#    - dplyr: Variable exclusion via select(-all_of())
#    - cli: Verbose logging replacing SAS %PUT statements
#    - purrr: Functional iteration over columns for width computation
#    - rlang: %||% null-coalescing operator for fallback logic
#
# OPEN QUESTIONS:
#    - Should haven labels be the primary source for variable labels, or should
#      a separate label mapping be supported?
#    - Confirm behavior for data frames without haven labels (base R data frames)
#    - Verify column width scaling factor (1.1x) matches SAS output appearance
# ============================================================
