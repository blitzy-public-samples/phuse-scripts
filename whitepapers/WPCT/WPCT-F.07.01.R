# =============================================================================
# HEADER
# Display:     Figure 7.1 Box plot - Measurements by Analysis Timepoint,
#              Visit and Treatment
# White paper: Central Tendency
# User Guide:  https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/CentralTendency-UserGuide.txt
# Macro Library (R): whitepapers/utilities/R/
# Specs:       https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/specification/
# Output:      https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/WPCT/outputs_r/
# Contributors: Jeno Pizarro, Suzie Perigaud (original R prototype)
#               Blitzy SAS-to-R Migration (full SAS parity update)
#
# HISTORY:
#   12-JUL-2015, Jeno  - Initial scripting (R prototype)
#   30-AUG-2015, Suzie - Add out of limit red dots and horizontal lines
#   29-NOV-2015, Jeno  - Edited to use ADaM only
#   10-JAN-2016, Jeno  - Allow user to select treatment arm variable,
#                         population flag; toggle options; CSV/XPT support;
#                         automatic page splitting
#   2026-03-26,  Blitzy Migration - Full SAS parity refactoring:
#     - Replaced Hmisc::sasxport.get with haven::read_xpt()
#     - Replaced data.table with dplyr for all data manipulation
#     - Replaced all round() with janitor::round_half_up()
#     - Wrapped script in parameterized function wpct_f_07_01()
#     - Added multi-PARAMCD iteration (SAS loop 1)
#     - Added ATPTN/ATPT timepoint iteration (SAS loop 2)
#     - Added block-aware pagination via util_boxplot_block_ranges()
#     - Added reference line logic via util_get_reference()
#     - Added axis ordering via util_axis_order()
#     - Added precision formatting via util_value_of_param()
#     - Applied PhUSEboxplot GTL theme via theme_phuse()/phuse_boxplot()
#     - Added PDF output with titles and footnotes
#     - Parameterized all paths via function arguments
# =============================================================================

# =============================================================================
# PACKAGE DEPENDENCIES
# =============================================================================
# Replaces: library(ggplot2), library(data.table), library(gridExtra),
#           library(Hmisc), library(tools)
#
# AAP §0.6.2: Load haven, dplyr, ggplot2, gridExtra, janitor
# AAP §0.8.1: Tidyverse over base R
# =============================================================================
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(janitor)
library(forcats)

# =============================================================================
# UTILITY FUNCTION SOURCING
# =============================================================================
# Replaces SAS: OPTIONS SASAUTOS=("whitepapers/utilities" SASAUTOS);
# AAP §0.5.2: source() calls for utility R functions
#
# Each utility script is sourced only if the function it provides is not
# already available in the current session (safe for re-sourcing).
# =============================================================================
local({
  # Determine the directory of this script for relative sourcing
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )

  # Candidate paths for utility files: relative to script, then repo root

  resolve_util <- function(filename) {
    candidates <- c(
      if (!is.null(script_dir)) file.path(script_dir, "..", "utilities", "R", filename),
      file.path("whitepapers", "utilities", "R", filename),
      file.path(".", filename)
    )
    for (f in candidates) {
      if (file.exists(f)) return(normalizePath(f, mustWork = FALSE))
    }
    NULL
  }

  utils_to_source <- c(
    "util_boxplot_block_ranges.R",
    "util_axis_order.R",
    "util_get_reference.R",
    "util_ggplot_theme.R",
    "util_get_var_min_max.R",
    "util_value_of_param.R"
  )

  for (util_file in utils_to_source) {
    path <- resolve_util(util_file)
    if (!is.null(path)) {
      source(path, local = FALSE)
    } else {
      warning(
        "WPCT-F.07.01: Could not locate utility file '", util_file, "'. ",
        "Ensure whitepapers/utilities/R/ is accessible.",
        call. = FALSE
      )
    }
  }
})

# =============================================================================
# wpct_f_07_01 — Main analysis function
# =============================================================================
# Replaces the entire SAS macro %boxplot_each_param_tp and surrounding script.
# All former SAS %let globals and script-level R variables become named
# function arguments with matching defaults.
#
# SAS construct mapping (AAP §0.7.1):
#   %macro boxplot_each_param_tp(plotds=, cleanup=) → wpct_f_07_01(...)
#   %let ds = ADLBC                                → dataset_name = "advs"
#   %let t_var = trtp_short                        → treatment_var = "TRTA"
#   %let tn_var = trtpn                            → treatment_num_var = "TRTPN"
#   %let m_var = aval                              → value_var = "AVAL"
#   %let lo_var = a1lo                             → lo_var = "ANRLO"
#   %let hi_var = a1hi                             → hi_var = "ANRHI"
#   %let p_fl = saffl                              → population_flag = "SAFFL"
#   %let a_fl = anl01fl                            → analysis_flag = "ANL01FL"
#   %let ref_lines = UNIFORM                       → ref_lines = "UNIFORM"
#   %let max_boxes_per_page = 20                   → max_boxes_per_page = 20
#   %let outputs_folder = ...                      → output_path = "output/figures"
# =============================================================================

#' Figure 7.1 — Box Plot of Measurements by Visit, Timepoint and Treatment
#'
#' Produces paginated box plots with summary statistics tables for all
#' parameters and analysis timepoints in a CDISC ADaM dataset. Full functional
#' parity with the SAS \code{WPCT-F.07.01.sas} script.
#'
#' @param data_path Character string. Path to directory containing the input
#'   XPT or CSV file. Replaces SAS \code{libname} / \code{inputdirectory}.
#' @param output_path Character string. Path to directory where PDF/PNG outputs
#'   are written. Replaces SAS \code{\%let outputs_folder}.
#' @param dataset_name Character string. Filename of the dataset to load
#'   (e.g., \code{"advs.xpt"} or \code{"adlbc.xpt"}).
#'   Replaces SAS \code{\%let ds = ADLBC}.
#' @param paramcd Character vector of PARAMCD values to plot, or \code{NULL}
#'   to plot all parameters found in the data (SAS default behaviour).
#' @param population_flag Character string. Column name of the population flag
#'   variable (\code{"Y"} = in population). Replaces SAS \code{\%let p_fl}.
#' @param analysis_flag Character string. Column name of the analysis flag
#'   variable (\code{"Y"} = selected for analysis). Replaces SAS
#'   \code{\%let a_fl}.
#' @param treatment_var Character string. Column name for treatment labels.
#'   Replaces SAS \code{\%let t_var}.
#' @param treatment_num_var Character string or \code{NULL}. Column name for
#'   treatment number (controls display order). Replaces SAS \code{\%let tn_var}.
#' @param visit_var Character string. Column name for visit number.
#' @param value_var Character string. Column name for the measurement value.
#'   Replaces SAS \code{\%let m_var}.
#' @param lo_var Character string or \code{NULL}. Column name for the lower
#'   reference range limit. Replaces SAS \code{\%let lo_var}.
#' @param hi_var Character string or \code{NULL}. Column name for the upper
#'   reference range limit. Replaces SAS \code{\%let hi_var}.
#' @param ref_lines Reference line specification.
#'   Replaces SAS \code{\%let ref_lines}. Accepts
#'   \code{"NONE"}, \code{"UNIFORM"}, \code{"NARROW"}, \code{"ALL"}, or a
#'   numeric vector of explicit positions.
#' @param max_boxes_per_page Positive integer. Maximum number of boxes per
#'   output page. Replaces SAS \code{\%let max_boxes_per_page}.
#' @param output_format Character string. One of \code{"PDF"}, \code{"PNG"},
#'   \code{"TIFF"}, \code{"JPEG"}. Replaces SAS \code{ODS PDF} / original R
#'   \code{filetype}.
#' @param treatment_renames Named character vector for treatment arm
#'   abbreviation, e.g., \code{c("Xanomeline Low Dose" = "X-low")}. Replaces
#'   SAS PROC FORMAT \code{trt_short} and original R \code{oldnames/newnames}.
#' @param pixel_width Integer. Raster output width in pixels (for PNG/TIFF/JPEG).
#' @param pixel_height Integer. Raster output height in pixels.
#' @param chart_title Character string or \code{NULL}. Override title; if
#'   \code{NULL}, a dynamic title is generated per PARAMCD and ATPT.
#' @param ... Additional arguments (reserved for future extensions).
#'
#' @return Invisibly returns a list of file paths written.
#' @export
wpct_f_07_01 <- function(data_path,
                          output_path,
                          dataset_name       = "advs.xpt",
                          paramcd            = NULL,
                          population_flag    = "SAFFL",
                          analysis_flag      = NULL,
                          treatment_var      = "TRTA",
                          treatment_num_var  = "TRTPN",
                          visit_var          = "AVISITN",
                          value_var          = "AVAL",
                          lo_var             = "ANRLO",
                          hi_var             = "ANRHI",
                          ref_lines          = "UNIFORM",
                          max_boxes_per_page = 20L,
                          output_format      = "PDF",
                          treatment_renames  = NULL,
                          pixel_width        = 1200L,
                          pixel_height       = 1000L,
                          chart_title        = NULL,
                          ...) {

  # ===========================================================================
  # INPUT VALIDATION
  # ===========================================================================
  # Mirrors SAS %assert_depend: verify OS, SAS version, variables, macros.
  # In R we validate arguments explicitly.
  # ===========================================================================
  if (!is.character(data_path) || length(data_path) != 1L || !nzchar(data_path)) {
    stop("(WPCT-F.07.01) data_path must be a non-empty character string.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L || !nzchar(output_path)) {
    stop("(WPCT-F.07.01) output_path must be a non-empty character string.", call. = FALSE)
  }
  output_format <- toupper(output_format)
  if (!(output_format %in% c("PDF", "PNG", "TIFF", "JPEG"))) {
    stop("(WPCT-F.07.01) output_format must be one of: PDF, PNG, TIFF, JPEG.", call. = FALSE)
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # ===========================================================================
  # DATA LOADING
  # ===========================================================================
  # Replaces SAS: %util_access_test_data(&ds) + libname work
  # Replaces original R: Hmisc::sasxport.get / read.csv
  # AAP §0.6.1: haven::read_xpt() for XPT files
  # ===========================================================================
  input_file <- file.path(data_path, dataset_name)
  if (!file.exists(input_file)) {
    stop(
      "(WPCT-F.07.01) Input file not found: ", input_file,
      call. = FALSE
    )
  }

  file_ext <- tolower(tools::file_ext(dataset_name))
  if (file_ext == "xpt") {
    # haven::read_xpt replaces Hmisc::sasxport.get (AAP §0.6.1)
    raw_data <- haven::read_xpt(input_file)
  } else if (file_ext == "csv") {
    raw_data <- readr::read_csv(input_file, show_col_types = FALSE)
  } else if (file_ext %in% c("sas7bdat")) {
    raw_data <- haven::read_sas(input_file)
  } else {
    stop(
      "(WPCT-F.07.01) Unsupported file extension: ", file_ext,
      ". Supported: xpt, csv, sas7bdat.",
      call. = FALSE
    )
  }

  # ===========================================================================
  # COLUMN NAME NORMALIZATION
  # ===========================================================================
  # haven::read_xpt preserves original case; ensure consistent uppercase
  # for standard CDISC variable matching.
  # ===========================================================================
  original_names <- names(raw_data)
  names(raw_data) <- toupper(names(raw_data))

  # Normalize user-supplied variable names to uppercase for matching
  population_flag   <- toupper(population_flag)
  analysis_flag     <- if (!is.null(analysis_flag)) toupper(analysis_flag) else NULL
  treatment_var     <- toupper(treatment_var)
  treatment_num_var <- if (!is.null(treatment_num_var)) toupper(treatment_num_var) else NULL
  visit_var         <- toupper(visit_var)
  value_var         <- toupper(value_var)
  lo_var            <- if (!is.null(lo_var)) toupper(lo_var) else NULL
  hi_var            <- if (!is.null(hi_var)) toupper(hi_var) else NULL

  # ===========================================================================
  # VALIDATE REQUIRED COLUMNS
  # ===========================================================================
  required_cols <- c("PARAMCD", "PARAM", population_flag, treatment_var,
                     visit_var, value_var, "AVISIT", "USUBJID")
  if (!is.null(analysis_flag)) required_cols <- c(required_cols, analysis_flag)
  if (!is.null(treatment_num_var)) required_cols <- c(required_cols, treatment_num_var)

  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0L) {
    stop(
      "(WPCT-F.07.01) Required column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ===========================================================================
  # ENSURE TIMEPOINT VARIABLES EXIST
  # ===========================================================================
  # SAS script (lines 28-32): creates dummy ATPTN=1, ATPT="TimePoint unknown"
  # if they are missing from the test data.
  # ===========================================================================
  if (!("ATPTN" %in% names(raw_data))) {
    raw_data <- dplyr::mutate(raw_data, ATPTN = 1L)
    message("(WPCT-F.07.01) NOTE: ATPTN not found in data; set to 1 for all records.")
  }
  if (!("ATPT" %in% names(raw_data))) {
    raw_data <- dplyr::mutate(raw_data, ATPT = "Timepoint Unknown")
    message("(WPCT-F.07.01) NOTE: ATPT not found in data; set to 'Timepoint Unknown'.")
  }

  # ===========================================================================
  # POPULATION AND ANALYSIS FLAG FILTERING
  # ===========================================================================
  # Replaces SAS:
  #   data css_anadata;
  #     set &m_lb..&m_ds;
  #     where &p_fl = 'Y' and &a_fl = 'Y';
  # AAP §0.7.3: Missing values map to NA, never zero-fill
  # ===========================================================================
  ana_data <- raw_data

  # Filter to population of interest
  ana_data <- dplyr::filter(ana_data, .data[[population_flag]] == "Y")

  # Filter to analysis records if flag provided

  if (!is.null(analysis_flag) && analysis_flag %in% names(ana_data)) {
    ana_data <- dplyr::filter(ana_data, .data[[analysis_flag]] == "Y")
  }

  # Remove records with missing measurement values (AAP §0.7.3: NA, never zero)
  ana_data <- dplyr::filter(ana_data, !is.na(.data[[value_var]]))

  if (nrow(ana_data) == 0L) {
    warning("(WPCT-F.07.01) No records remain after population/analysis filtering.",
            call. = FALSE)
    return(invisible(character(0L)))
  }

  # ===========================================================================
  # TREATMENT ARM RENAMING
  # ===========================================================================
  # Replaces SAS: proc format; value trt_short ...
  # Replaces original R: ifelse loop over oldnames/newnames
  # AAP §0.7.1: SAS formats → forcats factor levels
  # ===========================================================================
  if (!is.null(treatment_renames) && length(treatment_renames) > 0L) {
    ana_data <- dplyr::mutate(
      ana_data,
      !!treatment_var := dplyr::case_when(
        !!!purrr::imap(treatment_renames, function(new_name, old_name) {
          rlang::expr(.data[[!!treatment_var]] == !!old_name ~ !!new_name)
        }),
        TRUE ~ .data[[treatment_var]]
      )
    )
  }

  # Order treatment factor by treatment_num_var if available
  if (!is.null(treatment_num_var) && treatment_num_var %in% names(ana_data)) {
    trt_order <- ana_data %>%
      dplyr::distinct(.data[[treatment_var]], .data[[treatment_num_var]]) %>%
      dplyr::arrange(.data[[treatment_num_var]]) %>%
      dplyr::pull(.data[[treatment_var]])
    ana_data <- dplyr::mutate(
      ana_data,
      !!treatment_var := forcats::fct_relevel(.data[[treatment_var]], trt_order)
    )
  }

  # ===========================================================================
  # CREATE NORMAL RANGE OUTLIER VARIABLE
  # ===========================================================================
  # Replaces SAS:
  #   if (2 = n(&m_var, &lo_var) and &m_var < &lo_var) or
  #      (2 = n(&m_var, &hi_var) and &m_var > &hi_var) then m_var_outlier = &m_var;
  #   else m_var_outlier = .;
  # AAP §0.7.3: SAS missing (.) → NA
  # ===========================================================================
  has_lo <- !is.null(lo_var) && lo_var %in% names(ana_data)
  has_hi <- !is.null(hi_var) && hi_var %in% names(ana_data)

  if (has_lo || has_hi) {
    ana_data <- dplyr::mutate(
      ana_data,
      M_VAR_OUTLIER = dplyr::case_when(
        has_lo & has_hi &
          !is.na(.data[[value_var]]) & !is.na(.data[[lo_var]]) &
          .data[[value_var]] < .data[[lo_var]] ~ .data[[value_var]],
        has_lo & has_hi &
          !is.na(.data[[value_var]]) & !is.na(.data[[hi_var]]) &
          .data[[value_var]] > .data[[hi_var]] ~ .data[[value_var]],
        has_lo & !has_hi &
          !is.na(.data[[value_var]]) & !is.na(.data[[lo_var]]) &
          .data[[value_var]] < .data[[lo_var]] ~ .data[[value_var]],
        !has_lo & has_hi &
          !is.na(.data[[value_var]]) & !is.na(.data[[hi_var]]) &
          .data[[value_var]] > .data[[hi_var]] ~ .data[[value_var]],
        TRUE ~ NA_real_
      )
    )
  } else {
    ana_data <- dplyr::mutate(ana_data, M_VAR_OUTLIER = NA_real_)
  }

  # ===========================================================================
  # GATHER INFO FOR DATA-DRIVEN PROCESSING
  # ===========================================================================
  # Replaces SAS:
  #   %util_labels_from_var(css_anadata, paramcd, param)
  #   %util_count_unique_values(css_anadata, &t_var, trtn)
  # ===========================================================================
  param_info <- ana_data %>%
    dplyr::distinct(.data[["PARAMCD"]], .data[["PARAM"]]) %>%
    dplyr::arrange(.data[["PARAMCD"]])

  # If specific PARAMCDs requested, filter to those
  if (!is.null(paramcd)) {
    param_info <- dplyr::filter(param_info, .data[["PARAMCD"]] %in% toupper(paramcd))
    if (nrow(param_info) == 0L) {
      warning(
        "(WPCT-F.07.01) No matching PARAMCD values found for: ",
        paste(paramcd, collapse = ", "),
        call. = FALSE
      )
      return(invisible(character(0L)))
    }
  }

  # Track output file paths
  output_files <- character(0L)

  # ===========================================================================
  # LOOP 1: ITERATE OVER EACH PARAMETER
  # ===========================================================================
  # Replaces SAS: %do pdx = 1 %to &paramcd_n;
  # ===========================================================================
  for (pdx in seq_len(nrow(param_info))) {

    this_paramcd <- param_info[["PARAMCD"]][pdx]
    this_param_label <- param_info[["PARAM"]][pdx]

    # Subset to this parameter
    css_nextparam <- dplyr::filter(ana_data, .data[["PARAMCD"]] == this_paramcd)

    if (nrow(css_nextparam) == 0L) next

    # =========================================================================
    # Timepoint info for this parameter
    # Replaces SAS: %util_labels_from_var(css_nextparam, atptn, atpt)
    # =========================================================================
    atpt_info <- css_nextparam %>%
      dplyr::distinct(.data[["ATPTN"]], .data[["ATPT"]]) %>%
      dplyr::arrange(.data[["ATPTN"]])

    # =========================================================================
    # Reference lines for this parameter across all timepoints
    # Replaces SAS: %util_get_reference_lines(css_nextparam, nxt_reflines, ...)
    # Uses util_get_reference() from whitepapers/utilities/R/util_get_reference.R
    # =========================================================================
    nxt_reflines <- tryCatch(
      util_get_reference(
        df        = css_nextparam,
        low_var   = if (has_lo) lo_var else NULL,
        high_var  = if (has_hi) hi_var else NULL,
        ref_lines = ref_lines
      ),
      error = function(e) {
        message("(WPCT-F.07.01) NOTE: Reference line computation failed: ", e$message)
        NULL
      }
    )

    # =========================================================================
    # LOOP 2: ITERATE OVER EACH TIMEPOINT
    # =========================================================================
    # Replaces SAS: %do tdx = 1 %to &atptn_n;
    # =========================================================================
    for (tdx in seq_len(nrow(atpt_info))) {

      this_atptn <- atpt_info[["ATPTN"]][tdx]
      this_atpt_label <- atpt_info[["ATPT"]][tdx]

      # Subset to this timepoint, sorted by visit and treatment
      # Replaces SAS: proc sort data=css_nextparam(where=(atptn=&&atptn_val&tdx))
      #               out=css_nexttimept; by avisitn &tn_var;
      css_nexttimept <- css_nextparam %>%
        dplyr::filter(.data[["ATPTN"]] == this_atptn) %>%
        dplyr::arrange(.data[[visit_var]],
                       if (!is.null(treatment_num_var) &&
                           treatment_num_var %in% names(.))
                         .data[[treatment_num_var]]
                       else .data[[treatment_var]])

      if (nrow(css_nexttimept) == 0L) next

      # =======================================================================
      # Y-AXIS RANGE
      # Replaces SAS: %util_get_var_min_max(css_nexttimept, &m_var, ...)
      # =======================================================================
      aval_min_max <- tryCatch(
        util_get_var_min_max(
          df    = css_nexttimept,
          var   = value_var,
          extra = nxt_reflines
        ),
        error = function(e) {
          c(min = min(css_nexttimept[[value_var]], na.rm = TRUE),
            max = max(css_nexttimept[[value_var]], na.rm = TRUE))
        }
      )

      # =======================================================================
      # PRECISION FORMATTING
      # Replaces SAS: %util_value_format(css_nexttimept, &m_var)
      # Returns mean_digits and stddev_digits for rounding
      # =======================================================================
      value_fmt <- tryCatch(
        util_value_of_param(css_nexttimept, value_var),
        error = function(e) {
          list(mean_digits = 1L, stddev_digits = 2L)
        }
      )
      dignum <- value_fmt$mean_digits

      # =======================================================================
      # PAGINATION
      # Replaces SAS: %util_boxplot_block_ranges(css_nexttimept,
      #               blockvar=avisitn, catvars=&tn_var)
      # Uses util_boxplot_block_ranges() for block-aware page splitting
      # =======================================================================
      cat_var_for_page <- if (!is.null(treatment_num_var) &&
                              treatment_num_var %in% names(css_nexttimept)) {
        treatment_num_var
      } else {
        treatment_var
      }

      page_info <- tryCatch(
        util_boxplot_block_ranges(
          df                 = css_nexttimept,
          block_var          = visit_var,
          cat_vars           = cat_var_for_page,
          max_boxes_per_page = max_boxes_per_page
        ),
        error = function(e) {
          # Fallback: single page with all data
          list(
            ranges = paste0(
              min(css_nexttimept[[visit_var]], na.rm = TRUE), "<=",
              visit_var, "<=",
              max(css_nexttimept[[visit_var]], na.rm = TRUE)
            ),
            range_string = "",
            pages = dplyr::tibble()
          )
        }
      )

      # =======================================================================
      # Y-AXIS BREAKS
      # Replaces SAS: %let y_axis = %util_axis_order(min, max);
      # =======================================================================
      y_breaks <- tryCatch(
        util_axis_order(aval_min_max[["min"]], aval_min_max[["max"]]),
        error = function(e) {
          pretty(c(aval_min_max[["min"]], aval_min_max[["max"]]), n = 10)
        }
      )
      y_min_axis <- min(y_breaks)
      y_max_axis <- max(y_breaks)
      y_incr <- if (length(y_breaks) >= 2L) {
        y_breaks[2L] - y_breaks[1L]
      } else {
        NULL
      }

      # =======================================================================
      # SUMMARY STATISTICS
      # Replaces SAS: proc summary data=css_nexttimept ...
      # Uses dplyr with janitor::round_half_up() (AAP §0.7.2)
      # =======================================================================
      css_stats <- css_nexttimept %>%
        dplyr::group_by(
          .data[[visit_var]],
          .data[[treatment_var]],
          .data[["AVISIT"]]
        ) %>%
        dplyr::summarise(
          n       = dplyr::n(),
          mean    = janitor::round_half_up(
                      mean(.data[[value_var]], na.rm = TRUE), digits = dignum),
          std     = janitor::round_half_up(
                      stats::sd(.data[[value_var]], na.rm = TRUE),
                      digits = dignum + 1L),
          median  = janitor::round_half_up(
                      stats::median(.data[[value_var]], na.rm = TRUE),
                      digits = dignum),
          datamin = janitor::round_half_up(
                      min(.data[[value_var]], na.rm = TRUE), digits = dignum),
          datamax = janitor::round_half_up(
                      max(.data[[value_var]], na.rm = TRUE), digits = dignum),
          q1      = janitor::round_half_up(
                      stats::quantile(.data[[value_var]], 0.25,
                                      na.rm = TRUE, names = FALSE),
                      digits = dignum),
          q3      = janitor::round_half_up(
                      stats::quantile(.data[[value_var]], 0.75,
                                      na.rm = TRUE, names = FALSE),
                      digits = dignum),
          .groups = "drop"
        ) %>%
        dplyr::arrange(.data[[visit_var]], .data[[treatment_var]])

      # Stack statistics below the plot data (matches SAS css_plot creation)
      # Format mean and std with correct precision
      css_stats <- dplyr::mutate(
        css_stats,
        mean_fmt = formatC(mean, format = "f", digits = dignum),
        std_fmt  = formatC(std, format = "f", digits = dignum + 1L)
      )

      # =======================================================================
      # TITLES AND FOOTNOTES
      # Replaces SAS:
      #   title "Box Plot - &paramcd_lab Observed Values by Visit, ...";
      #   footnote1-3 ...;
      # =======================================================================
      dynamic_title <- if (!is.null(chart_title)) {
        chart_title
      } else {
        paste0(
          "Box Plot - ", this_param_label,
          " Observed Values by Visit, Analysis Timepoint: ", this_atpt_label
        )
      }

      footnote_lines <- paste0(
        "Box plot type is schematic: the box shows median and interquartile ",
        "range (IQR, the box height); the whiskers extend to the minimum\n",
        "and maximum data points within 1.5 IQR of the lower and upper ",
        "quartiles, respectively. Values outside the whiskers are shown as ",
        "outliers.\n",
        "Means are marked with a different symbol for each treatment. ",
        "Red dots indicate measures outside the normal reference range."
      )

      # =======================================================================
      # GRAPHICS SETTINGS
      # Replaces SAS: options orientation=landscape; goptions reset=all;
      #               ods graphics / border=no attrpriority=COLOR;
      # =======================================================================

      # Determine number of pages from page_info
      if (length(page_info$ranges) == 0L) {
        # Fallback: all visits on one page
        page_ranges_list <- list(
          sort(unique(css_nexttimept[[visit_var]]))
        )
      } else {
        # Parse range strings to extract visit subsets per page
        page_ranges_list <- lapply(page_info$ranges, function(range_str) {
          # Parse "min<=AVISITN<=max" to extract min and max
          parts <- regmatches(range_str,
                              regexec("([0-9.e+-]+)<=.*<=([0-9.e+-]+)", range_str))
          if (length(parts[[1L]]) >= 3L) {
            lo <- as.numeric(parts[[1L]][2L])
            hi <- as.numeric(parts[[1L]][3L])
            sort(unique(css_nexttimept[[visit_var]][
              css_nexttimept[[visit_var]] >= lo &
              css_nexttimept[[visit_var]] <= hi
            ]))
          } else {
            sort(unique(css_nexttimept[[visit_var]]))
          }
        })
      }

      # =====================================================================
      # LOOP 3: PAGE-LEVEL RENDERING
      # Replaces SAS: %do %while loop over &boxplot_block_ranges
      # =====================================================================
      for (vdx in seq_along(page_ranges_list)) {

        page_visits <- page_ranges_list[[vdx]]
        if (length(page_visits) == 0L) next

        # Subset data for this page
        page_data <- dplyr::filter(
          css_nexttimept,
          .data[[visit_var]] %in% page_visits
        )

        if (nrow(page_data) == 0L) next

        # Ensure visit is a factor for discrete x-axis
        page_data <- dplyr::mutate(
          page_data,
          !!visit_var := factor(.data[[visit_var]])
        )

        # Subset stats for this page
        page_stats <- dplyr::filter(
          css_stats,
          .data[[visit_var]] %in% page_visits
        )

        # ===================================================================
        # BUILD BOXPLOT
        # Replaces SAS: proc sgrender ... template=PhUSEboxplot
        # Uses phuse_boxplot() from util_ggplot_theme.R for GTL parity
        # ===================================================================
        p <- tryCatch(
          phuse_boxplot(
            data        = page_data,
            x_var       = visit_var,
            y_var       = value_var,
            group_var   = treatment_var,
            title       = dynamic_title,
            y_label     = this_param_label,
            y_min       = y_min_axis,
            y_max       = y_max_axis,
            y_incr      = y_incr,
            block_var   = "AVISIT",
            outlier_var = "M_VAR_OUTLIER",
            ref_lines   = nxt_reflines,
            show_notch  = TRUE,
            show_mean   = TRUE
          ),
          error = function(e) {
            # Fallback to manual ggplot construction if phuse_boxplot fails
            message("(WPCT-F.07.01) NOTE: phuse_boxplot() failed: ", e$message,
                    ". Using fallback ggplot.")
            NULL
          }
        )

        if (is.null(p)) {
          # Fallback ggplot2 construction
          p <- ggplot2::ggplot(
            page_data,
            ggplot2::aes(
              x    = .data[[visit_var]],
              y    = .data[[value_var]],
              fill = .data[[treatment_var]]
            )
          ) +
            ggplot2::geom_boxplot(
              notch = TRUE,
              width = 0.6,
              outlier.shape = 15,
              outlier.size  = 2
            ) +
            ggplot2::stat_summary(
              fun      = mean,
              geom     = "point",
              shape    = 18,
              size     = 3,
              colour   = "darkred",
              position = ggplot2::position_dodge(width = 0.6)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(y = .data[["M_VAR_OUTLIER"]]),
              colour   = "red",
              shape    = 16,
              size     = 1.5,
              position = ggplot2::position_dodge(width = 0.6),
              na.rm    = TRUE
            ) +
            ggplot2::scale_y_continuous(
              limits = c(y_min_axis, y_max_axis),
              breaks = y_breaks
            ) +
            ggplot2::labs(
              title = dynamic_title,
              x     = "Visit Number",
              y     = this_param_label,
              fill  = "Treatments & Outliers:"
            ) +
            ggplot2::theme(
              legend.position = "bottom",
              legend.title    = ggplot2::element_blank(),
              plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold")
            )

          # Add reference lines if available
          if (!is.null(nxt_reflines) && length(nxt_reflines) > 0L) {
            p <- p + ggplot2::geom_hline(
              yintercept = nxt_reflines,
              colour     = "red",
              linetype   = "solid"
            )
          }
        }

        # Add footnote as caption
        p <- p + ggplot2::labs(caption = footnote_lines)

        # ===================================================================
        # BUILD SUMMARY STATISTICS TABLE
        # Replaces SAS: AXISTABLE entries for N, MEAN, STD, etc.
        # ===================================================================
        if (nrow(page_stats) > 0L) {
          # Build the transposed table for gridExtra::tableGrob
          stat_display <- page_stats %>%
            dplyr::mutate(
              visit_trt = paste(.data[[visit_var]], .data[[treatment_var]],
                                sep = " | ")
            ) %>%
            dplyr::select(
              "visit_trt", "n", "mean_fmt", "std_fmt",
              "datamin", "q1", "median", "q3", "datamax"
            )

          # Transpose for display (rows = statistics, cols = visit x trt)
          stat_names <- c("N", "Mean", "Std Dev", "Min", "Q1",
                          "Median", "Q3", "Max")
          stat_matrix <- t(as.matrix(stat_display[, -1L]))
          colnames(stat_matrix) <- stat_display[["visit_trt"]]
          rownames(stat_matrix) <- stat_names

          t1_theme <- gridExtra::ttheme_default(
            core = list(fg_params = list(fontsize = 8)),
            colhead = list(fg_params = list(fontsize = 7)),
            rowhead = list(fg_params = list(fontsize = 8))
          )
          t1 <- gridExtra::tableGrob(
            stat_matrix,
            theme = t1_theme
          )
        } else {
          t1 <- grid::nullGrob()
        }

        # ===================================================================
        # OUTPUT FILE
        # Replaces SAS: ods pdf file="..." / TIFF/JPEG/PNG devices
        # SAS: WPCT-F.07.01_Box_plot_&paramcd_by_visit_for_timepoint_&atptn.pdf
        # ===================================================================
        page_suffix <- if (length(page_ranges_list) > 1L) {
          paste0("_page", vdx)
        } else {
          ""
        }
        base_filename <- paste0(
          "WPCT-F.07.01_Box_plot_", this_paramcd,
          "_by_visit_for_timepoint_", this_atptn,
          page_suffix
        )

        if (output_format == "PDF") {
          out_file <- file.path(output_path, paste0(base_filename, ".pdf"))
          grDevices::pdf(
            out_file,
            width  = 11, height = 8.5,
            paper  = "special",
            title  = paste0(
              "Boxplot of ", this_param_label,
              " by Visit for Timepoint ", this_atpt_label
            )
          )
          gridExtra::grid.arrange(p, t1, ncol = 1,
                                  heights = grid::unit(c(0.7, 0.3), "npc"))
          grDevices::dev.off()
        } else if (output_format == "PNG") {
          out_file <- file.path(output_path, paste0(base_filename, ".png"))
          grDevices::png(out_file, width = pixel_width, height = pixel_height,
                         units = "px", pointsize = 12)
          gridExtra::grid.arrange(p, t1, ncol = 1,
                                  heights = grid::unit(c(0.7, 0.3), "npc"))
          grDevices::dev.off()
        } else if (output_format == "TIFF") {
          out_file <- file.path(output_path, paste0(base_filename, ".tiff"))
          grDevices::tiff(out_file, width = pixel_width, height = pixel_height,
                          units = "px", pointsize = 12)
          gridExtra::grid.arrange(p, t1, ncol = 1,
                                  heights = grid::unit(c(0.7, 0.3), "npc"))
          grDevices::dev.off()
        } else if (output_format == "JPEG") {
          out_file <- file.path(output_path, paste0(base_filename, ".jpeg"))
          grDevices::jpeg(out_file, width = pixel_width, height = pixel_height,
                          units = "px", pointsize = 12)
          gridExtra::grid.arrange(p, t1, ncol = 1,
                                  heights = grid::unit(c(0.7, 0.3), "npc"))
          grDevices::dev.off()
        }

        output_files <- c(output_files, out_file)
        message("(WPCT-F.07.01) Output written: ", out_file)

      } # end LOOP 3 (pages)

    } # end LOOP 2 (timepoints)

  } # end LOOP 1 (parameters)

  message(
    "(WPCT-F.07.01) Complete. ", length(output_files), " file(s) generated."
  )
  invisible(output_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. haven::read_xpt() reads CDISC ADaM XPT files with variable
#       labels preserved, replacing Hmisc::sasxport.get(). Column
#       names are normalised to uppercase for consistent CDISC
#       variable matching (SAS is case-insensitive; R is case-sensitive).
#    2. SAS dummy ATPTN=1 and ATPT="TimePoint unknown" creation
#       (lines 28-32 of SAS source) is handled by auto-creating
#       these columns if missing from the input dataset.
#    3. SAS %assert_depend validation (OS, SAS version, macros,
#       variables) is replaced by R argument validation and column
#       existence checks. The runtime dependency check (OS, SAS version,
#       SYSPROD) has no meaningful R equivalent and is omitted.
#    4. SAS proc format value trt_short is replaced by the
#       treatment_renames parameter using dplyr::case_when() and
#       forcats::fct_relevel() for factor ordering.
#    5. SAS %util_labels_from_var is replaced by dplyr::distinct()
#       to enumerate PARAMCD/PARAM and ATPTN/ATPT combinations.
#    6. SAS %util_count_unique_values is replaced by
#       dplyr::n_distinct() / dplyr::distinct() calls.
#    7. SAS css_plot data set (stacking stats below plot data with one
#       obs per treatment/visit for AXISTABLE) is replaced by a
#       separate css_stats tibble rendered via gridExtra::tableGrob()
#       below the ggplot.
#    8. The SAS cleanup parameter (%util_delete_dsets) has no
#       equivalent in R since R uses garbage collection; temporary
#       data frames go out of scope at function exit.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. ROUNDING: All rounding uses janitor::round_half_up() to match
#       SAS round-half-up behaviour. R base round() uses banker's
#       rounding (half-to-even) which would produce different results
#       for values exactly at 0.5 boundaries (e.g., 2.5 → 2 in base R
#       vs 2.5 → 3 in SAS). Gate 2 audit: every rounding location
#       verified.
#    2. SORT STABILITY: R dplyr::arrange() is stable within groups;
#       SAS PROC SORT is also stable. No difference expected for
#       multi-key sorts, but verify for ties at boundary.
#    3. QUANTILE METHOD: R stats::quantile() uses type=7 by default
#       (linear interpolation); SAS PROC SUMMARY uses method 2
#       (similar to R type=2). For datasets where quartile boundaries
#       fall between observations, Q1 and Q3 values may differ
#       slightly. Statistician review recommended.
#    4. BOXPLOT NOTCH: ggplot2 notch calculation differs from SAS
#       GTL notch algorithm. Notch widths may not match exactly.
#    5. FLOATING POINT: SAS and R both use IEEE 754 64-bit doubles.
#       Epsilon comparisons at precision boundaries may yield
#       differences of ≤ 1 ULP.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PROC SGRENDER with PhUSEboxplot GTL template: Replaced
#       by phuse_boxplot() from util_ggplot_theme.R which constructs
#       an equivalent ggplot2 object with matching aesthetics.
#    2. SAS AXISTABLE for bottom inner-margin statistics: Replaced
#       by gridExtra::tableGrob() arranged below the plot. The visual
#       alignment may differ slightly from the SAS AXISTABLE which
#       shares the x-axis alignment with the boxplot.
#    3. SAS ODS PDF metadata (author, subject, title attributes):
#       Base R pdf() device does not support all ODS PDF metadata
#       fields. Title is included; author/subject require additional
#       PDF post-processing or use of the R cairo_pdf device.
#    4. SAS BLOCKLABEL (top inner margin visit labels): SAS places
#       visit labels as block headers aligned with the x-axis;
#       ggplot2 uses facet or annotation text. Current implementation
#       passes block_var to phuse_boxplot() for handling.
#
# PACKAGE SELECTION RATIONALE:
#    1. haven (2.5.5): Mandated by AAP §0.6.1 for SAS data I/O.
#       Replaces Hmisc::sasxport.get() with haven::read_xpt().
#    2. dplyr (1.1.0+): Mandated by AAP §0.8.1 (tidyverse over base R).
#       Replaces data.table for all data manipulation.
#    3. janitor (2.2.0+): Mandated by AAP §0.7.2 for SAS-compatible
#       round_half_up() rounding behaviour.
#    4. forcats (1.0.0+): Treatment arm factor ordering replacing SAS
#       proc format value statements.
#    5. ggplot2 (3.4.0+): Box plot rendering replacing SAS PROC SGRENDER.
#    6. gridExtra (2.3+): Multi-panel layout combining plot and stats
#       table, replacing SAS ODS region stacking.
#    7. tidyr (1.3.0+): Data reshaping for statistics table construction.
#
# OPEN QUESTIONS:
#    1. Should R quantile type be changed from default (type=7) to
#       type=2 to match SAS PROC SUMMARY quartile calculation? This
#       would require setting type=2 in all stats::quantile() calls.
#       Statistician review recommended before production use.
#    2. SAS AXISTABLE aligns statistics labels with boxplot x-axis
#       categories. The gridExtra::tableGrob approach places the table
#       below but does not guarantee exact column alignment. Consider
#       using patchwork or cowplot for tighter alignment if required.
#    3. PDF metadata (author, subject): Should these be populated using
#       the cairo_pdf device or a post-processing step via qpdf/pdftools?
#    4. SAS PROC SGRENDER dynamic _BLOCKLABEL produces visit name labels
#       above each block of boxes. The current R implementation passes
#       the variable to phuse_boxplot() but exact rendering depends on
#       the util_ggplot_theme.R implementation. Visual verification
#       needed.
# ============================================================
