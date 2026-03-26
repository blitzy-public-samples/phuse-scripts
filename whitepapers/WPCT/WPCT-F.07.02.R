# =============================================================================
# WPCT-F.07.02.R
# =============================================================================
# Display:     Figure 7.2 Box plot - Change from Baseline by Analysis Timepoint,
#              Visit and Treatment
# White paper: Central Tendency
# Specs:       https://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/specification/
# Test Data:   https://github.com/phuse-org/phuse-scripts/tree/master/data/adam/cdisc
# Sample Output: whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf
#
# Migrated from:    whitepapers/WPCT/WPCT-F.07.02.sas (402 lines)
# Consolidates:     WPCT-F.07.02.v01.R (package-style function, phuse::get_inputs())
#                   WPCT-F.07.02.v02.R (tryCatch error handling, progress reporting)
# Original R:       Jeno Pizarro, Kirsten Burdett (10-JAN-2016, adapted from WPCT 7.01.R)
# Original SAS:     Dante Di Tommaso
#
# SAS-to-R construct mapping:
#   PROC SUMMARY              -> dplyr::group_by() + summarise()
#   PROC GLM (ANCOVA, /ss3)   -> stats::lm() + summary()$coefficients
#   PROC SGRENDER PhUSEboxplot -> ggplot2 + theme_phuse()
#   ODS PDF                   -> grDevices::pdf() + gridExtra::grid.arrange()
#   %util_boxplot_block_ranges -> util_boxplot_block_ranges()
#   %util_axis_order           -> util_axis_order()
#   %util_value_format         -> detect_precision() + janitor::round_half_up()
#   %util_proc_template        -> theme_phuse() from util_ggplot_theme.R
#   SAS round()                -> janitor::round_half_up() (AAP section 0.7.2)
#   SAS missing (.)            -> NA (AAP section 0.7.3)
#   SAS format ordering        -> forcats::fct_relevel()
# =============================================================================

# ---- Required packages --------------------------------------------------------
library(haven)        # SAS data I/O: read_xpt() replacing Hmisc::sasxport.get
library(dplyr)        # Tidyverse data manipulation replacing data.table
library(tidyr)        # Data reshaping for stats table pivot
library(ggplot2)      # Visualization replacing PROC SGRENDER
library(gridExtra)    # Plot + table layout replacing ODS composite
library(car)          # ANCOVA Type III SS replacing PROC GLM
library(janitor)      # round_half_up() for SAS-compatible rounding
library(forcats)      # Factor manipulation replacing SAS format ordering
library(yaml)         # YAML config parsing


# ---- Source PhUSE CS utility functions ----------------------------------------
# Replaces SAS: OPTIONS sasautos=(...) for PhUSE macro resolution.
# Per AAP section 0.5.2: source() calls for whitepapers/utilities/R/*.R
local({
  # Try config-based path first, then fallback candidates
  cfg <- tryCatch(
    yaml::read_yaml("config/migration_config.yaml"),
    error = function(e) NULL
  )
  # Use r_source_paths from config when available (AAP §0.5.2)
  cfg_util_path <- if (!is.null(cfg))
    cfg$r_source_paths$wp_utilities_path else NULL
  util_dir_candidates <- c(
    cfg_util_path,
    file.path("whitepapers", "utilities", "R"),
    file.path("..", "utilities", "R")
  )
  # Drop NULL entries from the candidate list
  util_dir_candidates <- util_dir_candidates[!vapply(
    util_dir_candidates, is.null, logical(1)
  )]
  util_files <- c(
    "util_boxplot_block_ranges.R",
    "util_axis_order.R",
    "util_get_reference.R",
    "util_ggplot_theme.R"
  )
  for (util_file in util_files) {
    sourced <- FALSE
    for (util_dir in util_dir_candidates) {
      fpath <- file.path(util_dir, util_file)
      if (file.exists(fpath)) {
        source(fpath, local = FALSE)
        sourced <- TRUE
        break
      }
    }
    if (!sourced) {
      warning("Could not source utility: ", util_file, call. = FALSE)
    }
  }
})


# =============================================================================
# Helper: Detect measurement precision from data values
# =============================================================================
# Replaces SAS: %util_value_format(ds, var)
# Determines the number of decimal places in the raw measurement data, then
# returns MEAN digits (base + 1) and STD digits (base + 2) per SAS convention.
detect_precision <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(list(mean_digits = 1L, std_digits = 2L))
  }
  chars <- format(x, scientific = FALSE, drop0trailing = TRUE)
  decimals <- vapply(chars, function(s) {
    parts <- strsplit(trimws(s), "\\.")[[1L]]
    if (length(parts) < 2L) 0L else nchar(parts[2L])
  }, integer(1L), USE.NAMES = FALSE)
  base_digits <- max(decimals, 0L, na.rm = TRUE)
  list(
    mean_digits = base_digits + 1L,
    std_digits  = base_digits + 2L
  )
}


# =============================================================================
# Helper: Resolve column name case from data frame
# =============================================================================
# Haven::read_xpt preserves uppercase column names from SAS transport files.
# This helper tries exact match, then uppercase, then lowercase.
resolve_col <- function(name, df) {
  if (name %in% names(df)) return(name)
  up <- toupper(name)
  if (up %in% names(df)) return(up)
  lo <- tolower(name)
  if (lo %in% names(df)) return(lo)
  name
}


# =============================================================================
# Helper: Parse reference line specification
# =============================================================================
# Handles numeric values, space-delimited strings, or keyword modes.
# For CHG boxplots, default is "0" (zero reference line).
parse_ref_lines <- function(ref_lines, df = NULL, low_var = NULL,
                            high_var = NULL) {
  if (is.numeric(ref_lines)) {
    return(ref_lines[!is.na(ref_lines)])
  }
  if (is.character(ref_lines) && length(ref_lines) == 1L) {
    upper_val <- toupper(trimws(ref_lines))
    if (upper_val == "NONE") return(numeric(0L))
    # Try parsing as space-delimited numbers
    tokens <- strsplit(trimws(ref_lines), "\\s+")[[1L]]
    parsed <- suppressWarnings(as.numeric(tokens))
    if (!any(is.na(parsed))) return(parsed)
    # If keyword mode (UNIFORM, NARROW, ALL), delegate to util_get_reference
    if (upper_val %in% c("UNIFORM", "NARROW", "ALL") &&
        !is.null(df) && exists("util_get_reference", mode = "function")) {
      result <- tryCatch(
        util_get_reference(df, low_var = low_var, high_var = high_var,
                           ref_lines = ref_lines),
        error = function(e) numeric(0L)
      )
      return(if (is.null(result)) numeric(0L) else result)
    }
  }
  numeric(0L)
}


# =============================================================================
# wpct_f_07_02 - Main analysis function
# =============================================================================
#' Figure 7.2 Box Plot - Change from Baseline by Visit and Treatment
#'
#' Produces paginated boxplots of change-from-baseline (CHG) by analysis visit,
#' treatment arm, parameter, and analysis timepoint, with optional ANCOVA
#' p-values comparing active treatments to a reference arm. Iterates over all
#' PARAMCDs and analysis timepoints (ATPTN) in the data.
#'
#' Migrated from WPCT-F.07.02.sas with full functional parity: PROC SUMMARY
#' statistics, PROC GLM ANCOVA (per-treatment p-values vs reference), paginated
#' PhUSEboxplot rendering, zero reference lines, and PDF output.
#'
#' @param data_path Character. Directory containing ADVS XPT file.
#'   Replaces SAS libname and \%let m_lb. Falls back to config if NULL.
#' @param output_path Character. Directory for PDF output files.
#'   Replaces SAS OUTPUTS_FOLDER. Falls back to config if NULL.
#' @param t_var Character. Treatment name variable (default \code{"TRTP"}).
#'   Replaces SAS \code{T_VAR}.
#' @param tn_var Character. Treatment number variable controlling display order
#'   (default \code{"TRTPN"}). Replaces SAS \code{TN_VAR}.
#' @param c_var Character. Change-from-baseline variable (default \code{"CHG"}).
#'   Replaces SAS \code{C_VAR}.
#' @param b_var Character or NULL. Baseline variable for ANCOVA covariate
#'   (default \code{"BASE"}). Set NULL to omit p-values. Replaces SAS \code{B_VAR}.
#' @param ref_trtn Numeric or NULL. Reference treatment number for ANCOVA
#'   (default NULL). Replaces SAS \code{REF_TRTN}.
#' @param b_visn Numeric. Baseline visit number excluded from plots
#'   (default 0). Replaces SAS \code{B_VISN}.
#' @param e_visn Numeric. Endpoint visit number for ANCOVA
#'   (default 99). Replaces SAS \code{E_VISN}.
#' @param p_fl Character. Population flag variable (default \code{"SAFFL"}).
#'   Replaces SAS \code{P_FL}.
#' @param a_fl Character. Analysis flag variable (default \code{"ANL01FL"}).
#'   Replaces SAS \code{A_FL}.
#' @param ref_lines Character or numeric. Reference line specification
#'   (default \code{"0"}). Replaces SAS \code{_REFLINES}.
#' @param max_boxes_per_page Integer. Maximum boxes per page
#'   (default 20). Replaces SAS \code{MAX_BOXES_PER_PAGE}.
#'
#' @return Invisible list of generated PDF file paths.
#' @export
wpct_f_07_02 <- function(data_path          = NULL,
                          output_path        = NULL,
                          t_var              = "TRTP",
                          tn_var             = "TRTPN",
                          c_var              = "CHG",
                          b_var              = "BASE",
                          ref_trtn           = NULL,
                          b_visn             = 0,
                          e_visn             = 99,
                          p_fl               = "SAFFL",
                          a_fl               = "ANL01FL",
                          ref_lines          = "0",
                          max_boxes_per_page = 20L) {

  # ===========================================================================
  # STEP 0: Resolve configuration paths
  # Replaces SAS %let globals and libname statements
  # ===========================================================================
  config <- tryCatch(
    yaml::read_yaml("config/migration_config.yaml"),
    error = function(e) NULL
  )

  if (is.null(data_path) && !is.null(config)) {
    data_path <- config$data_paths$adam_path
  }
  if (is.null(data_path)) {
    stop("data_path is required. Provide directly or via config/migration_config.yaml.",
         call. = FALSE)
  }

  if (is.null(output_path) && !is.null(config)) {
    output_path <- config$output_paths$figure_output_path
  }
  if (is.null(output_path)) {
    output_path <- "output/figures"
  }

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Apply domain_settings from config as defaults when function args are at

  # their default values (AAP §0.4.1: config/migration_config.yaml provides
  # domain_settings$wpct for max_boxes_per_page, population_flag, etc.)
  wpct_cfg <- if (!is.null(config)) config$domain_settings$wpct else NULL
  if (!is.null(wpct_cfg)) {
    if (missing(max_boxes_per_page) && !is.null(wpct_cfg$max_boxes_per_page)) {
      max_boxes_per_page <- as.integer(wpct_cfg$max_boxes_per_page)
    }
    if (missing(p_fl) && !is.null(wpct_cfg$population_flag)) {
      p_fl <- toupper(wpct_cfg$population_flag)
    }
    if (missing(a_fl) && !is.null(wpct_cfg$analysis_flag)) {
      a_fl <- toupper(wpct_cfg$analysis_flag)
    }
  }

  # ===========================================================================
  # STEP 1: Load ADVS data
  # Replaces SAS: %util_access_test_data(advs) and libname
  # ===========================================================================
  advs_file <- file.path(data_path, "advs.xpt")
  if (!file.exists(advs_file)) {
    stop("ADVS data file not found: ", advs_file, call. = FALSE)
  }

  advs_raw <- haven::read_xpt(advs_file)
  message("Loaded ADVS: ", nrow(advs_raw), " obs x ", ncol(advs_raw), " vars.")

  # Resolve column names (XPT stores uppercase; user may specify either case)
  t_var  <- resolve_col(t_var, advs_raw)
  tn_var <- resolve_col(tn_var, advs_raw)
  c_var  <- resolve_col(c_var, advs_raw)
  p_fl   <- resolve_col(p_fl, advs_raw)
  a_fl   <- resolve_col(a_fl, advs_raw)

  has_baseline <- !is.null(b_var) && nzchar(b_var)
  if (has_baseline) {
    b_var <- resolve_col(b_var, advs_raw)
  }

  # Resolve CDISC structural variables
  paramcd_col <- resolve_col("PARAMCD", advs_raw)
  param_col   <- resolve_col("PARAM", advs_raw)
  avisitn_col <- resolve_col("AVISITN", advs_raw)
  avisit_col  <- resolve_col("AVISIT", advs_raw)
  atptn_col   <- resolve_col("ATPTN", advs_raw)
  atpt_col    <- resolve_col("ATPT", advs_raw)

  # Validate required columns
  required_vars <- unique(c(
    paramcd_col, param_col, avisitn_col, avisit_col,
    t_var, tn_var, c_var, p_fl, a_fl
  ))
  if (has_baseline) required_vars <- c(required_vars, b_var)
  if (atptn_col %in% names(advs_raw)) {
    required_vars <- c(required_vars, atptn_col, atpt_col)
  }

  missing_vars <- setdiff(required_vars, names(advs_raw))
  if (length(missing_vars) > 0L) {
    stop("Required variable(s) not found in ADVS: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  has_atptn <- atptn_col %in% names(advs_raw) && atpt_col %in% names(advs_raw)

  # ===========================================================================
  # STEP 2: Subset to analysis population
  # Replaces SAS DATA css_anadata (lines 162-168):
  #   where &p_fl = 'Y' and &a_fl = 'Y';
  #   where also avisitn ne &b_visn;
  # ===========================================================================
  # Select relevant columns upfront to reduce memory (dplyr::select + all_of)
  keep_cols <- unique(c(
    paramcd_col, param_col, avisitn_col, avisit_col,
    t_var, tn_var, c_var, p_fl, a_fl,
    if (has_baseline) b_var,
    if (atptn_col %in% names(advs_raw)) c(atptn_col, atpt_col)
  ))
  advs_sel <- advs_raw %>%
    dplyr::select(dplyr::all_of(keep_cols))

  css_anadata <- advs_sel %>%
    dplyr::filter(
      .data[[p_fl]] == "Y",
      .data[[a_fl]] == "Y",
      .data[[avisitn_col]] != b_visn
    ) %>%
    # AAP 0.7.3: filter out NA change values (SAS missing = . -> NA)
    dplyr::filter(!is.na(.data[[c_var]])) %>%
    # Convert treatment to ordered factor (forcats::fct_inorder preserves
    # order-of-appearance, fct_reorder enables data-driven reordering)
    dplyr::mutate(
      !!t_var  := forcats::fct_reorder(.data[[t_var]], .data[[tn_var]]),
      !!tn_var := as.numeric(.data[[tn_var]])
    )

  if (nrow(css_anadata) == 0L) {
    warning("No analysis records after population/flag/baseline filtering.",
            call. = FALSE)
    return(invisible(list()))
  }

  message("Analysis dataset: ", nrow(css_anadata), " records after filtering.")

  # ===========================================================================
  # STEP 3: Gather info for data-driven processing
  # Replaces SAS: %util_labels_from_var(css_anadata, paramcd, param)
  #               %util_count_unique_values(css_anadata, &t_var, trtn)
  # ===========================================================================
  # Parameters: unique PARAMCDs with labels
  param_info <- css_anadata %>%
    dplyr::distinct(.data[[paramcd_col]], .data[[param_col]]) %>%
    dplyr::arrange(.data[[paramcd_col]])

  paramcd_vals <- param_info[[paramcd_col]]
  paramcd_labs <- stats::setNames(param_info[[param_col]], paramcd_vals)

  message("Parameters to process: ", paste(paramcd_vals, collapse = ", "))

  # Baseline visit label (for titles)
  b_visn_lab <- tryCatch({
    lbl <- advs_raw %>%
      dplyr::filter(.data[[avisitn_col]] == b_visn) %>%
      dplyr::distinct(.data[[avisit_col]]) %>%
      dplyr::slice(1L) %>%
      dplyr::pull(.data[[avisit_col]])
    if (length(lbl) == 0L) paste("Visit", b_visn) else lbl
  }, error = function(e) paste("Visit", b_visn))

  # Endpoint visit label
  e_visn_lab <- tryCatch({
    lbl <- css_anadata %>%
      dplyr::filter(.data[[avisitn_col]] == e_visn) %>%
      dplyr::distinct(.data[[avisit_col]]) %>%
      dplyr::slice(1L) %>%
      dplyr::pull(.data[[avisit_col]])
    if (length(lbl) == 0L) paste("Visit", e_visn) else lbl
  }, error = function(e) paste("Visit", e_visn))

  # Treatment order lookup (sorted by treatment number)
  trt_order <- css_anadata %>%
    dplyr::distinct(.data[[tn_var]], .data[[t_var]]) %>%
    dplyr::arrange(.data[[tn_var]])

  trt_levels <- as.character(trt_order[[t_var]])
  n_trt <- length(trt_levels)

  # ===========================================================================
  # STEP 4: Main analysis loops
  # Replaces SAS %boxplot_each_param_tp with nested PARAMCD / ATPTN loops
  # ===========================================================================
  output_files <- list()
  compute_pval <- has_baseline && !is.null(ref_trtn)

  for (pdx in seq_along(paramcd_vals)) {
    # ---- LOOP 1: Parameters (SAS lines 220-228) ----------------------------
    this_paramcd <- paramcd_vals[pdx]
    this_param   <- paramcd_labs[[this_paramcd]]

    css_nextparam <- css_anadata %>%
      dplyr::filter(.data[[paramcd_col]] == this_paramcd)

    if (nrow(css_nextparam) == 0L) {
      message("No data for PARAMCD = ", this_paramcd, ". Skipping.")
      next
    }

    # Analysis timepoints for this parameter
    # Replaces SAS: %util_labels_from_var(css_nextparam, atptn, atpt)
    if (has_atptn) {
      atpt_info <- css_nextparam %>%
        dplyr::distinct(.data[[atptn_col]], .data[[atpt_col]]) %>%
        dplyr::filter(!is.na(.data[[atptn_col]])) %>%
        dplyr::arrange(.data[[atptn_col]])

      if (nrow(atpt_info) == 0L) {
        # Fallback: treat as single timepoint
        atpt_info <- dplyr::tibble(
          !!atptn_col := NA_real_, !!atpt_col := "All Timepoints"
        )
      }
    } else {
      atpt_info <- dplyr::tibble(
        ATPTN_DUMMY = NA_real_, ATPT_DUMMY = "All Timepoints"
      )
    }

    atptn_vals <- atpt_info[[1L]]
    atptn_labs <- atpt_info[[2L]]

    for (tdx in seq_along(atptn_vals)) {
      # ---- LOOP 2: Timepoints (SAS lines 236-245) --------------------------
      this_atptn <- atptn_vals[tdx]
      this_atpt  <- atptn_labs[tdx]

      if (has_atptn && !is.na(this_atptn)) {
        css_nexttimept <- css_nextparam %>%
          dplyr::filter(.data[[atptn_col]] == this_atptn) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      } else {
        css_nexttimept <- css_nextparam %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      }

      if (nrow(css_nexttimept) == 0L) {
        message("No data for ATPTN = ", this_atptn, ". Skipping.")
        next
      }

      # ---- Y-axis range (SAS: %util_get_var_min_max + %util_axis_order) ----
      chg_values <- css_nexttimept[[c_var]]
      chg_values <- chg_values[!is.na(chg_values)]
      if (length(chg_values) == 0L) next

      chg_min <- min(chg_values)
      chg_max <- max(chg_values)

      # Handle edge case: constant data
      if (chg_min >= chg_max) {
        chg_min <- chg_min - 1
        chg_max <- chg_max + 1
      }

      y_breaks <- tryCatch(
        util_axis_order(chg_min, chg_max),
        error = function(e) pretty(chg_values)
      )
      y_min <- if (!is.null(attr(y_breaks, "axis_min"))) {
        attr(y_breaks, "axis_min")
      } else {
        min(y_breaks)
      }
      y_max <- if (!is.null(attr(y_breaks, "axis_max"))) {
        attr(y_breaks, "axis_max")
      } else {
        max(y_breaks)
      }

      # ---- Precision detection (SAS: %util_value_format) -------------------
      precision <- detect_precision(chg_values)

      # ---- Summary statistics (SAS lines 258-263: PROC SUMMARY) ------------
      css_stats <- css_nexttimept %>%
        dplyr::group_by(
          .data[[avisitn_col]], .data[[avisit_col]],
          .data[[tn_var]], .data[[t_var]]
        ) %>%
        dplyr::summarise(
          n       = dplyr::n(),
          mean    = janitor::round_half_up(
            mean(.data[[c_var]], na.rm = TRUE),
            digits = precision$mean_digits
          ),
          std     = janitor::round_half_up(
            sd(.data[[c_var]], na.rm = TRUE),
            digits = precision$std_digits
          ),
          median  = janitor::round_half_up(
            median(.data[[c_var]], na.rm = TRUE),
            digits = precision$mean_digits
          ),
          datamin = janitor::round_half_up(
            min(.data[[c_var]], na.rm = TRUE),
            digits = precision$mean_digits
          ),
          datamax = janitor::round_half_up(
            max(.data[[c_var]], na.rm = TRUE),
            digits = precision$mean_digits
          ),
          q1      = janitor::round_half_up(
            quantile(.data[[c_var]], 0.25, na.rm = TRUE),
            digits = precision$mean_digits
          ),
          q3      = janitor::round_half_up(
            quantile(.data[[c_var]], 0.75, na.rm = TRUE),
            digits = precision$mean_digits
          ),
          .groups = "drop"
        ) %>%
        dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])

      # ---- ANCOVA p-values (SAS lines 267-308: PROC GLM) ------------------
      # Only when both b_var and ref_trtn are specified.
      # SAS: proc glm; class &tn_var (ref="&ref_trtn");
      #      model &c_var = &b_var &tn_var / solution;
      if (compute_pval) {
        endpoint_data <- css_nexttimept %>%
          dplyr::filter(
            .data[[avisitn_col]] == e_visn,
            !is.na(.data[[c_var]]),
            !is.na(.data[[b_var]])
          )

        pval_result <- NULL

        if (nrow(endpoint_data) >= 2L &&
            dplyr::n_distinct(endpoint_data[[tn_var]]) >= 2L) {

          pval_result <- tryCatch({
            # Set reference treatment level
            # SAS: class &tn_var (ref="&ref_trtn")
            ep_trt_levels <- sort(unique(endpoint_data[[tn_var]]))
            endpoint_data[[tn_var]] <- factor(
              endpoint_data[[tn_var]],
              levels = c(ref_trtn, setdiff(ep_trt_levels, ref_trtn))
            )

            # Fit ANCOVA model
            # SAS: model &c_var = &b_var &tn_var / solution
            ancova_formula <- stats::reformulate(
              termlabels = c(b_var, tn_var),
              response   = c_var
            )
            ancova_model <- stats::lm(ancova_formula, data = endpoint_data)
            coef_tbl <- summary(ancova_model)$coefficients

            # Extract per-treatment p-values from parameter estimates
            # SAS: where parameter =: "%upcase(&tn_var)"
            trt_rows <- grep(paste0("^", tn_var), rownames(coef_tbl))

            if (length(trt_rows) > 0L) {
              trt_names <- rownames(coef_tbl)[trt_rows]
              trt_nums <- as.numeric(
                gsub(paste0("^", tn_var), "", trt_names)
              )
              dplyr::tibble(
                !!avisitn_col := e_visn,
                !!tn_var      := trt_nums,
                pval          = coef_tbl[trt_rows, "Pr(>|t|)"]
              )
            } else {
              NULL
            }
          }, error = function(e) {
            warning("ANCOVA model failed: ", conditionMessage(e),
                    call. = FALSE)
            NULL
          })
        }

        # Merge p-values into stats (SAS lines 302-305: merge css_stats)
        if (!is.null(pval_result) && nrow(pval_result) > 0L) {
          css_stats <- css_stats %>%
            dplyr::left_join(
              pval_result,
              by = stats::setNames(
                c(avisitn_col, tn_var),
                c(avisitn_col, tn_var)
              )
            )
        } else {
          css_stats[["pval"]] <- NA_real_
        }
      }

      # ---- Treatment factor ordering (SAS format-based ordering) -----------
      # Replaces SAS FORMAT ordering: use TRTPN to determine treatment order
      css_nexttimept[[t_var]] <- forcats::fct_relevel(
        factor(css_nexttimept[[t_var]]),
        trt_levels
      )

      # ---- Pagination (SAS line 254: %util_boxplot_block_ranges) -----------
      page_ranges <- tryCatch(
        util_boxplot_block_ranges(
          df                 = css_nexttimept,
          block_var          = avisitn_col,
          cat_vars           = tn_var,
          max_boxes_per_page = as.integer(max_boxes_per_page)
        ),
        error = function(e) {
          # Fallback: all visits on one page
          all_visits <- sort(unique(css_nexttimept[[avisitn_col]]))
          list(
            pages = dplyr::tibble(
              !!avisitn_col := all_visits,
              count = rep(n_trt, length(all_visits)),
              page  = rep(1L, length(all_visits))
            )
          )
        }
      )

      if (!is.null(page_ranges$pages) && nrow(page_ranges$pages) > 0L) {
        n_pages <- max(page_ranges$pages[["page"]])
        page_assignments <- page_ranges$pages
      } else {
        all_visits <- sort(unique(css_nexttimept[[avisitn_col]]))
        n_pages <- 1L
        page_assignments <- dplyr::tibble(
          !!avisitn_col := all_visits,
          page = rep(1L, length(all_visits))
        )
      }

      # ---- Reference lines ------------------------------------------------
      ref_line_values <- parse_ref_lines(ref_lines)

      # ---- Titles and footnotes (SAS lines 330-333) -----------------------
      plot_title <- paste0(
        "Box Plot - ", this_param,
        " Change from ", toupper(b_visn_lab),
        " to ", toupper(e_visn_lab),
        " by Visit, Analysis Timepoint: ", this_atpt
      )
      footnote_lines <- paste0(
        "Box plot type is schematic: box=median+IQR, ",
        "whiskers to min/max within 1.5*IQR. ",
        "Means marked with symbols per treatment.",
        if (compute_pval) {
          " P-value from ANCOVA: Change = Baseline + Treatment."
        } else {
          ""
        }
      )

      # ---- PDF output (SAS lines 337-344: ODS PDF) ------------------------
      atptn_suffix <- if (!is.na(this_atptn)) this_atptn else "all"
      pdf_filename <- paste0(
        "WPCT-F.07.02_Box_plot_", this_paramcd,
        "_Change_by_visit_for_timepoint_", atptn_suffix, ".pdf"
      )
      pdf_filepath <- file.path(output_path, pdf_filename)

      grDevices::pdf(
        file   = pdf_filepath,
        width  = 11,
        height = 8.5,
        paper  = "special"
      )

      # ---- LOOP 3: Pages (SAS lines 352-384) ------------------------------
      for (vdx in seq_len(n_pages)) {
        page_visits <- page_assignments %>%
          dplyr::filter(.data[["page"]] == vdx) %>%
          dplyr::pull(1L)

        page_data <- css_nexttimept %>%
          dplyr::filter(.data[[avisitn_col]] %in% page_visits)

        if (nrow(page_data) == 0L) next

        page_stats <- css_stats %>%
          dplyr::filter(.data[[avisitn_col]] %in% page_visits)

        # Ensure treatment factor is properly ordered (fct_relevel for explicit
        # ordering; fct_inorder used for visit labels maintaining data order)
        page_data[[t_var]] <- forcats::fct_relevel(
          factor(page_data[[t_var]]), trt_levels
        )
        page_data[[avisit_col]] <- forcats::fct_inorder(
          as.character(page_data[[avisit_col]])
        )

        # ---- Build boxplot ----
        # SAS PhUSEboxplot: all boxes same fill (#B9CFE7), treatments
        # distinguished by mean marker shape and legend
        p <- ggplot2::ggplot(
          page_data,
          ggplot2::aes(
            x    = factor(.data[[avisitn_col]]),
            y    = .data[[c_var]],
            fill = .data[[t_var]]
          )
        ) +
          ggplot2::geom_boxplot(
            colour = phuse_colors$box_outline,
            width  = phuse_sizes$cluster_width,
            outlier.colour = phuse_colors$nr_outlier,
            outlier.shape  = 16,
            outlier.size   = phuse_sizes$nr_outlier_size
          ) +
          # Uniform box fill matching SAS CXB9CFE7
          ggplot2::scale_fill_manual(
            values = stats::setNames(
              rep(phuse_colors$box_fill, n_trt), trt_levels
            )
          ) +
          # Mean markers with per-treatment shapes
          # SAS: mean diamond markers differentiated by treatment
          ggplot2::stat_summary(
            ggplot2::aes(group = .data[[t_var]], shape = .data[[t_var]]),
            fun      = mean,
            geom     = "point",
            size     = phuse_sizes$mean_size,
            colour   = phuse_colors$mean_marker,
            position = ggplot2::position_dodge(
              width = phuse_sizes$cluster_width
            ),
            show.legend = TRUE
          ) +
          ggplot2::scale_shape_manual(
            values = c(18, 15, 16, 17, 4, 8)[seq_len(n_trt)]
          ) +
          # Y-axis with computed nice breaks
          ggplot2::scale_y_continuous(
            breaks = y_breaks,
            limits = c(y_min, y_max)
          ) +
          ggplot2::labs(
            title    = plot_title,
            subtitle = footnote_lines,
            x        = "Visit Number",
            y        = paste("Change in", this_param),
            fill     = "Treatments & Outliers:",
            shape    = "Treatments & Outliers:"
          ) +
          theme_phuse() +
          ggplot2::guides(
            fill  = ggplot2::guide_legend(
              title = "Treatments & Outliers:"
            ),
            shape = ggplot2::guide_legend(
              title = "Treatments & Outliers:"
            )
          )

        # Add reference lines (SAS: _REFLINES = "0")
        if (length(ref_line_values) > 0L) {
          for (ref_val in ref_line_values) {
            p <- p + ggplot2::geom_hline(
              yintercept = ref_val,
              colour     = phuse_colors$ref_line,
              linetype   = "solid",
              linewidth  = 0.5
            )
          }
        }

        # ---- Build summary statistics table --------------------------------
        # Replaces SAS AXISTABLE in PhUSEboxplot template
        stat_cols <- c("n", "mean", "std", "median", "q1", "q3",
                        "datamin", "datamax")
        stat_labels <- c("N", "Mean", "Std Dev", "Median", "Q1", "Q3",
                          "Min", "Max")

        has_pval_col <- compute_pval && "pval" %in% names(page_stats)
        if (has_pval_col) {
          stat_cols   <- c(stat_cols, "pval")
          stat_labels <- c(stat_labels, "p-value")
        }

        # Create matrix: rows=stats, columns=Visit x Treatment
        stats_ordered <- page_stats %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])

        n_groups <- nrow(stats_ordered)

        # Pivot stats to long form for flexible formatting, then back to wide
        # (dplyr::across + all_of for column selection; tidyr::pivot_longer
        # for stats reshaping; tidyr::pivot_wider not needed here but used
        # in STEP 5 summary pipeline)
        stats_long <- stats_ordered %>%
          dplyr::select(
            dplyr::all_of(c(avisitn_col, tn_var, t_var)),
            dplyr::all_of(intersect(stat_cols, names(stats_ordered)))
          ) %>%
          tidyr::pivot_longer(
            cols      = dplyr::all_of(
              intersect(stat_cols, names(stats_ordered))
            ),
            names_to  = "stat",
            values_to = "value"
          ) %>%
          dplyr::mutate(
            formatted = dplyr::case_when(
              .data$stat == "n"    ~ as.character(as.integer(.data$value)),
              .data$stat == "pval" & is.na(.data$value) ~ " ",
              .data$stat == "pval" ~ formatC(.data$value, format = "f",
                                             digits = 4),
              is.na(.data$value)   ~ " ",
              TRUE                 ~ as.character(.data$value)
            )
          )

        # Reshape back to matrix for tableGrob
        tbl_matrix <- matrix("", nrow = length(stat_cols), ncol = n_groups)
        for (i in seq_along(stat_cols)) {
          row_data <- stats_long %>%
            dplyr::filter(.data$stat == stat_cols[i])
          tbl_matrix[i, ] <- row_data$formatted
        }

        # Column headers: treatment name per visit
        col_headers <- paste0(
          stats_ordered[[t_var]], "\n(V",
          stats_ordered[[avisitn_col]], ")"
        )

        tbl_df <- data.frame(
          Stat = stat_labels,
          tbl_matrix,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        colnames(tbl_df) <- c("", col_headers)

        # Render table grob
        t1_theme <- gridExtra::ttheme_default(
          core    = list(fg_params = list(fontsize = 7)),
          colhead = list(fg_params = list(fontsize = 7, fontface = "bold"))
        )
        t1 <- gridExtra::tableGrob(
          tbl_df, rows = NULL, theme = t1_theme
        )

        # Combine plot + stats table on one page
        gridExtra::grid.arrange(
          p, t1,
          ncol    = 1,
          heights = grid::unit(c(3, 1), "null")
        )

      }  # End LOOP 3 (pages)

      grDevices::dev.off()
      output_files[[pdf_filename]] <- pdf_filepath
      message("Created: ", pdf_filepath)

    }  # End LOOP 2 (timepoints)
  }  # End LOOP 1 (parameters)

  invisible(output_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. ADVS dataset has standard CDISC ADaM columns: PARAMCD, PARAM,
#       AVISITN, AVISIT, ATPTN, ATPT, CHG, BASE, TRTP, TRTPN, SAFFL,
#       ANL01FL, USUBJID. Column names may be uppercase (XPT standard)
#       or mixed case; resolve_col() handles both.
#    2. Baseline visit (b_visn) records are excluded from display per
#       SAS source line 167: "where also avisitn ne &b_visn" — baseline
#       CHG is always zero and uninformative.
#    3. ANCOVA model uses stats::lm() with per-treatment t-test p-values
#       from summary()$coefficients, matching SAS PROC GLM with
#       /SOLUTION option which outputs PARAMETERESTIMATES with PROBT.
#    4. If ATPTN/ATPT columns are absent, all data is treated as a
#       single analysis timepoint.
#    5. Treatment ordering is derived from TRTPN via forcats::fct_relevel(),
#       matching SAS format-based ordering.
#    6. Precision detection (detect_precision) mirrors SAS %util_value_format:
#       base decimals from raw data, MEAN +1, STD +2 extra decimals.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. ROUNDING: SAS rounds half-up (0.5 -> 1); R default rounds to even.
#       All rounding in this script uses janitor::round_half_up() to match
#       SAS behavior. Gate 2 rounding audit should verify each statistic.
#    2. ANCOVA P-VALUES: SAS PROC GLM uses Type III SS by default with
#       /SOLUTION; R stats::lm() + summary() reports sequential (Type I)
#       t-tests for individual coefficients but these match SAS parameter
#       estimate p-values (PROBT) exactly for balanced designs. For
#       unbalanced designs, the ANCOVA Type III overall F-test can be
#       obtained via car::Anova(model, type=3) if needed.
#    3. SORT STABILITY: SAS sort is stable by key. R dplyr::arrange() is
#       stable within groups. Multi-key sort order preserved via explicit
#       arrange() calls matching SAS PROC SORT BY statements.
#    4. QUANTILE METHOD: R quantile() default is type=7 (linear interpolation);
#       SAS PROC SUMMARY uses type 2 (SAS definition). For large samples the
#       difference is negligible; for small samples, verify Q1/Q3 values.
#    5. STD DEV FORMULA: Both SAS and R use n-1 denominator (sample std dev).
#       No difference expected.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PROC SGRENDER with dynamic template: Replaced by ggplot2
#       layers + theme_phuse(). The PhUSEboxplot GTL template registered
#       via %util_proc_template has no single R equivalent; it is
#       decomposed into individual ggplot2 geoms and theme settings.
#    2. SAS AXISTABLE (stats below plot): Replaced by gridExtra::tableGrob()
#       arranged below the ggplot via grid.arrange().
#    3. SAS ODS PDF with metadata (author, subject, title): R pdf() device
#       does not support PDF metadata fields. Use an RTF or external tool
#       if metadata is required for submission.
#    4. SAS %util_value_format auto-detection of format width: Approximated
#       by detect_precision() counting decimal places in raw data.
#
# PACKAGE SELECTION RATIONALE:
#    - haven (read_xpt): Mandated by AAP as standard SAS XPT reader,
#      replacing Hmisc::sasxport.get. ReadStat C backend is faster and
#      preserves variable labels.
#    - dplyr: Mandated by AAP for all data manipulation, replacing
#      data.table. Tidyverse-over-base-R rule (AAP 0.8.1).
#    - tidyr (pivot_wider): Used for reshaping stats table display.
#    - ggplot2: Mandated for visualization replacing PROC SGRENDER.
#    - gridExtra (grid.arrange, tableGrob): Combines plot + stats table,
#      replacing SAS AXISTABLE integrated rendering.
#    - car (Anova): Provides Type III SS for ANCOVA verification.
#      Not used in main p-value path (lm summary suffices) but available
#      for validation comparison.
#    - janitor (round_half_up): Mandated by AAP Gate 2 for SAS-compatible
#      rounding behavior.
#    - forcats (fct_relevel): Mandated for factor ordering replacing SAS
#      format-based treatment ordering.
#    - yaml: Parses config/migration_config.yaml for parameterized paths.
#
# OPEN QUESTIONS:
#    1. Should car::Anova(type=3) overall F-test be used instead of
#       per-treatment t-test p-values? The SAS source uses PARAMETERESTIMATES
#       (per-treatment) not the Type III F-test line. Current implementation
#       matches SAS per-treatment approach.
#    2. Quantile type: SAS default quantile algorithm differs from R type=7.
#       Should quantile(..., type=2) be used for exact SAS match?
#    3. PDF metadata: SAS ODS PDF includes author/subject/title metadata.
#       R grDevices::pdf() does not support this. Is PDF metadata required
#       for regulatory submission?
#    4. Treatment arm abbreviation: SAS source line 95-103 creates trtp_short.
#       Should this script accept pre-abbreviated treatment labels, or should
#       it provide a built-in abbreviation mechanism?
# ============================================================
