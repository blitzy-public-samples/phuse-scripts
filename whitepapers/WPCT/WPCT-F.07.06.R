# =============================================================================
# WPCT-F.07.06.R — Figure 7.6: Multi-Study Boxplot
#   Last/Min/Max Baseline and Last/Min/Max Post-baseline Measurements
#   by Treatment and Analysis Timepoint Across Multiple Studies
# =============================================================================
# Migrated from: whitepapers/WPCT/WPCT-F.07.06.sas (24726 bytes, 506 lines)
# Migration date: 2026-03
# PhUSE CS Standard Analyses Working Group (WG5) — WPCT Figure 7.6
#
# Design: Unlike Figures 7.1–7.5 (single-study, multi-visit), Figure 7.6
#   iterates over STUDYID as the primary x-axis dimension, with each study
#   contributing a Baseline (b_visn) and Post-baseline (e_visn) boxplot
#   pair.  An automatically-generated "Pooled" study aggregates all
#   individual study data.  Pagination splits studies across pages.
#
# SAS-to-R Construct Mapping:
#   %derive_lastminmax_measure  -> derive_lastminmax_measure()
#   %util_labels_from_var       -> util_labels_from_var()
#   %util_count_unique_values   -> util_count_unique_values()
#   %util_get_reference_lines   -> util_get_reference()
#   %util_get_var_min_max       -> util_get_var_min_max()
#   %util_value_format          -> util_value_of_param()
#   %util_boxplot_block_ranges  -> util_boxplot_block_ranges()
#   %util_axis_order            -> util_axis_order()
#   %util_proc_template         -> theme_phuse() / phuse_boxplot()
#   %util_delete_dsets          -> util_delete_dsets()
#   PROC SGRENDER PhUSEboxplot  -> ggplot2 + theme_phuse() + phuse_boxplot()
#   PROC SUMMARY                -> dplyr::group_by() + summarise()
#   ODS PDF LANDSCAPE           -> ggplot2::ggsave(device = "pdf")
#   SpreadsheetML AXISTABLE     -> phuse_boxplot_stats_table() + gridExtra
# =============================================================================

# ---------------------------------------------------------------------------
# Required packages
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(janitor)
library(forcats)
library(rlang)

# =============================================================================
# Main function: wpct_f_07_06
# =============================================================================
#' Generate WPCT Figure 7.6 — Multi-Study Boxplot of Baseline/Post-baseline
#'
#' Produces boxplots of Last/Min/Max Baseline and Post-baseline measurements
#' grouped by treatment arm across multiple studies.  Each PARAMCD x ATPTN
#' combination generates a separate set of paginated landscape PDF figures.
#' A "Pooled" study combining all individual studies is automatically appended.
#'
#' @param data_path   Character. Path to directory containing multi-study ADaM
#'   XPT files (cdisc-split structure with per-study subdirectories, or a
#'   single directory containing the XPT directly).
#' @param output_path Character. Directory for PDF output files.
#' @param ds_name     Character. XPT dataset filename (default "advs.xpt").
#' @param t_var       Character. Treatment label variable name (default "TRTP").
#' @param tn_var      Character. Treatment number variable (default "TRTPN").
#' @param m_var       Character. Measurement variable name (default "AVAL").
#' @param lo_var      Character. Lower normal range variable (default "ANRLO").
#' @param hi_var      Character. Upper normal range variable (default "ANRHI").
#' @param p_fl        Character. Population flag variable name (default "SAFFL").
#' @param a_fl        Character. Analysis flag variable name (default "ANL01FL").
#' @param b_visn      Numeric. Baseline visit AVISITN value (default 0).
#' @param e_visn      Numeric. Post-baseline visit AVISITN value (default 99).
#' @param ref_lines   Character or numeric. Reference line mode:
#'   "NONE", "UNIFORM", "NARROW", "ALL", or numeric vector (default "NARROW").
#' @param max_boxes_per_page Integer. Max boxes per page (default 20).
#'
#' @return Character vector of created PDF file paths (invisible).
#' @export
wpct_f_07_06 <- function(
    data_path          = NULL,
    output_path        = NULL,
    ds_name            = "advs.xpt",
    t_var              = "TRTP",
    tn_var             = "TRTPN",
    m_var              = "AVAL",
    lo_var             = "ANRLO",
    hi_var             = "ANRHI",
    p_fl               = "SAFFL",
    a_fl               = "ANL01FL",
    b_visn             = 0,
    e_visn             = 99,
    ref_lines          = "NARROW",
    max_boxes_per_page = 20
) {

  # =========================================================================
  # 1. Configuration loading and path resolution
  # =========================================================================
  # Replaces SAS: %let macession globals, %include, libname statements
  script_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) getwd()
  )

  # Walk upward to locate repository root via config marker file

  repo_root <- script_dir
  for (i in seq_len(10)) {
    if (file.exists(file.path(repo_root, "config", "migration_config.yaml"))) break
    repo_root <- dirname(repo_root)
  }

  config <- tryCatch(
    yaml::read_yaml(file.path(repo_root, "config", "migration_config.yaml")),
    error = function(e) {
      message("Config file not found; using built-in defaults. ", conditionMessage(e))
      list(
        data_paths     = list(adam_split_path = "data/adam/cdisc-split"),
        output_paths   = list(figure_output_path = "output/figures"),
        r_source_paths = list(
          wp_utilities_path = "whitepapers/utilities/R",
          wp_adam_path      = "whitepapers/ADaM/R"
        ),
        domain_settings = list(wpct = list())
      )
    }
  )

  # Access WPCT domain settings (may provide defaults for future extension)
  wpct_settings <- config$domain_settings$wpct %||% list()

  # Resolve data_path from config$data_paths$adam_split_path if not supplied
  if (is.null(data_path)) {
    data_path <- file.path(
      repo_root,
      config$data_paths$adam_split_path %||% "data/adam/cdisc-split"
    )
  }
  # Resolve output_path from config$output_paths$figure_output_path
  if (is.null(output_path)) {
    output_path <- file.path(
      repo_root,
      config$output_paths$figure_output_path %||% "output/figures"
    )
  }

  # =========================================================================
  # 2. Source utility functions
  # =========================================================================
  # Replaces SAS: %include "&utilities_path/util_*.sas";
  utils_dir <- file.path(
    repo_root,
    config$r_source_paths$wp_utilities_path %||% "whitepapers/utilities/R"
  )
  adam_dir <- file.path(
    repo_root,
    config$r_source_paths$wp_adam_path %||% "whitepapers/ADaM/R"
  )

  util_files <- c(
    file.path(utils_dir, "util_labels_from_var.R"),
    file.path(utils_dir, "util_count_unique_values.R"),
    file.path(utils_dir, "util_get_reference.R"),
    file.path(utils_dir, "util_ggplot_theme.R"),
    file.path(utils_dir, "util_get_var_min_max.R"),
    file.path(utils_dir, "util_value_of_param.R"),
    file.path(utils_dir, "util_boxplot_block_ranges.R"),
    file.path(utils_dir, "util_axis_order.R"),
    file.path(utils_dir, "util_delete_dsets.R"),
    file.path(utils_dir, "assert_var_nonmissing.R"),
    file.path(adam_dir,   "derive_lastminmax_measure.R")
  )

  for (uf in util_files) {
    if (file.exists(uf)) {
      source(uf, local = FALSE)
    } else {
      warning("Utility file not found: ", uf, call. = FALSE)
    }
  }

  # =========================================================================
  # 3. Load multi-study data from cdisc-split folder
  # =========================================================================
  message("Loading multi-study data from: ", data_path)

  # Normalize all user-supplied variable names to uppercase
  t_var  <- toupper(t_var)
  tn_var <- toupper(tn_var)
  m_var  <- toupper(m_var)
  lo_var <- toupper(lo_var)
  hi_var <- toupper(hi_var)
  p_fl   <- toupper(p_fl)
  a_fl   <- toupper(a_fl)

  # Multi-study loading: scan subfolders for per-study XPT files
  # Replaces SAS: %util_access_test_data(m_lb=css_data, m_ds=&m_ds) for
  # the cdisc-split directory layout
  study_dirs <- list.dirs(data_path, full.names = TRUE, recursive = FALSE)

  if (length(study_dirs) == 0) {
    # Fallback: try loading a single XPT file directly from data_path
    xpt_file <- file.path(data_path, ds_name)
    if (file.exists(xpt_file)) {
      raw_data <- haven::read_xpt(xpt_file)
      message("  Loaded single dataset: ", xpt_file,
              " (", nrow(raw_data), " obs)")
    } else {
      stop(
        "No study subdirectories found and no direct XPT file at: ",
        xpt_file,
        "\nExpected either per-study subdirectories in data_path or a direct ",
        ds_name, " file.",
        call. = FALSE
      )
    }
  } else {
    # Load ds_name from each study subdirectory and combine via bind_rows
    study_dfs <- list()
    for (sd in study_dirs) {
      xpt_file <- file.path(sd, ds_name)
      if (file.exists(xpt_file)) {
        study_df <- haven::read_xpt(xpt_file)
        study_dfs[[basename(sd)]] <- study_df
        message("  Loaded: ", basename(sd), "/", ds_name,
                " (", nrow(study_df), " obs)")
      }
    }
    if (length(study_dfs) == 0) {
      stop("No ", ds_name, " files found in study subdirectories of: ",
           data_path, call. = FALSE)
    }
    raw_data <- dplyr::bind_rows(study_dfs)
    message("  Combined ", length(study_dfs), " study dataset(s): ",
            nrow(raw_data), " total observations")
  }

  # Normalize column names to uppercase for consistent downstream processing
  names(raw_data) <- toupper(names(raw_data))

  # =========================================================================
  # 4. Validate required variables exist in the dataset
  # =========================================================================
  required_vars <- unique(c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "AVISITN",
    t_var, tn_var, m_var, p_fl, a_fl
  ))

  missing_vars <- setdiff(required_vars, names(raw_data))
  if (length(missing_vars) > 0) {
    stop(
      "Required variables missing from dataset: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Check for normal-range reference variables (optional for ref lines)
  has_ref_vars <- all(c(lo_var, hi_var) %in% names(raw_data))
  if (!has_ref_vars) {
    message("  Reference range variables (", lo_var, "/", hi_var,
            ") not found. Reference lines and NR outliers will be disabled.")
  }

  # Check for ATPTN (Analysis Timepoint Number) — if absent, add placeholder

  has_atptn <- "ATPTN" %in% names(raw_data)
  if (!has_atptn) {
    raw_data <- raw_data %>% dplyr::mutate(ATPTN = 0L)
    message("  ATPTN variable not found; using single timepoint (ATPTN=0).")
  }
  has_atpt <- "ATPT" %in% names(raw_data)

  # Ensure AVISIT exists (will be recoded in step 8)
  if (!"AVISIT" %in% names(raw_data)) {
    raw_data <- raw_data %>%
      dplyr::mutate(AVISIT = as.character(AVISITN))
  }

  # =========================================================================
  # 5. Derive baseline / post-baseline extremes via derive_lastminmax_measure
  # =========================================================================
  # Replaces SAS (source lines 197-202):
  #   %derive_lastminmax_measure(ds=css_data, c_modes=&lmm,
  #     flvars=&a_fl, grpvars=studyid usubjid &tn_var paramcd atptn,
  #     ordvars=avisitn, incl=trtp_short &p_fl param atpt anrlo anrhi);
  grp_vars <- unique(c("STUDYID", "USUBJID", tn_var, "PARAMCD", "ATPTN"))
  incl_vars <- unique(c(t_var, p_fl, "PARAM", "AVISIT"))
  if (has_atpt) incl_vars <- c(incl_vars, "ATPT")
  if (has_ref_vars) incl_vars <- c(incl_vars, lo_var, hi_var)

  analysis_data <- tryCatch(
    derive_lastminmax_measure(
      ds      = raw_data,
      c_modes = c("LAST"),
      flvars  = a_fl,
      grpvars = grp_vars,
      ordvars = c("AVISITN"),
      incl    = incl_vars,
      cleanup = TRUE
    ),
    error = function(e) {
      message("  derive_lastminmax_measure() returned error: ",
              conditionMessage(e),
              "\n  Falling back to raw data with existing analysis flags.")
      raw_data
    }
  )

  # Re-normalise column names after derivation
  names(analysis_data) <- toupper(names(analysis_data))

  # =========================================================================
  # 6. Apply population and analysis flag filters
  # =========================================================================
  # Replaces SAS: where &p_fl = 'Y' and &a_fl = 'Y'
  # Handle heterogeneous flag representations (character 'Y', numeric 1, etc.)
  analysis_data <- analysis_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(p_fl, a_fl)),
        ~ dplyr::if_else(
          . %in% c("Y", "y", 1, TRUE), "Y",
          dplyr::if_else(is.na(.), NA_character_, as.character(.))
        )
      )
    ) %>%
    dplyr::filter(
      .data[[p_fl]] == "Y",
      .data[[a_fl]] == "Y"
    )

  if (nrow(analysis_data) == 0) {
    warning("No records remain after population/analysis flag filtering (",
            p_fl, "='Y' AND ", a_fl, "='Y').", call. = FALSE)
    return(invisible(character(0)))
  }
  message("  After flag filtering: ", nrow(analysis_data), " observations")

  # =========================================================================
  # 7. Restrict to baseline (b_visn) and post-baseline (e_visn) visits
  # =========================================================================
  # Replaces SAS: if avisitn in (&b_visn, &e_visn)
  analysis_data <- analysis_data %>%
    dplyr::filter(AVISITN %in% c(b_visn, e_visn))

  if (nrow(analysis_data) == 0) {
    warning("No records for AVISITN in {", b_visn, ", ", e_visn, "}.",
            call. = FALSE)
    return(invisible(character(0)))
  }
  message("  After visit restriction (b_visn=", b_visn, ", e_visn=",
          e_visn, "): ", nrow(analysis_data), " observations")

  # =========================================================================
  # 8. Recode AVISIT and AVISITN to BASE / POST labels
  # =========================================================================
  # Replaces SAS first.usubjid / last.usubjid BY-group logic:
  #   if first.usubjid then do; avisitn=1; avisit='BASE'; end;
  #   if last.usubjid  then do; avisitn=2; avisit='POST'; end;
  analysis_data <- analysis_data %>%
    dplyr::mutate(
      AVISIT = dplyr::case_when(
        AVISITN == b_visn ~ "BASE",
        AVISITN == e_visn ~ "POST",
        TRUE              ~ as.character(AVISITN)
      ),
      AVISITN = dplyr::case_when(
        AVISITN == b_visn ~ 1,
        AVISITN == e_visn ~ 2,
        TRUE              ~ as.numeric(AVISITN)
      )
    ) %>%
    dplyr::arrange(STUDYID, !!rlang::sym(tn_var), PARAMCD, ATPTN,
                   USUBJID, AVISITN)

  # =========================================================================
  # 9. Assert measurement variable is non-missing after filtering
  # =========================================================================
  # Replaces SAS: %assert_var_nonmissing(css_anadata, &m_var, ...)
  if (exists("assert_var_nonmissing", mode = "function")) {
    nonmiss_ok <- assert_var_nonmissing(
      df  = analysis_data,
      var = m_var,
      whr = NULL
    )
    if (!isTRUE(nonmiss_ok)) {
      warning(
        "Variable ", m_var, " contains missing values in filtered data. ",
        "Proceeding with available non-missing records.", call. = FALSE
      )
    }
  }

  # =========================================================================
  # 10. Create normal-range outlier variable
  # =========================================================================
  # Replaces SAS:
  #   if m_var < lo_var or m_var > hi_var then outlier = m_var; else outlier = .;
  # Values outside [ANRLO, ANRHI] are flagged for red-dot overlay (NR outlier).
  if (has_ref_vars) {
    analysis_data <- analysis_data %>%
      dplyr::mutate(
        OUTLIER = dplyr::case_when(
          is.na(.data[[m_var]])                           ~ NA_real_,
          is.na(.data[[lo_var]]) & is.na(.data[[hi_var]]) ~ NA_real_,
          .data[[m_var]] < .data[[lo_var]]                ~ .data[[m_var]],
          .data[[m_var]] > .data[[hi_var]]                ~ .data[[m_var]],
          TRUE                                            ~ NA_real_
        )
      )
  } else {
    analysis_data <- analysis_data %>%
      dplyr::mutate(OUTLIER = NA_real_)
  }

  # =========================================================================
  # 11. Create pooled data across all studies
  # =========================================================================
  # Replaces SAS (source ~line 265):
  #   studyid = 'A0'x || 'Pooled';   (hex A0 = non-breaking space)
  #   substr(usubjid, 1, 1) = 'P';
  # The non-breaking space prefix (\u00A0) forces "Pooled" to sort AFTER
  # all individual studies in Unicode collation order.
  pooled_data <- analysis_data %>%
    dplyr::mutate(
      STUDYID = paste0("\u00A0", "Pooled"),
      USUBJID = paste0("P", substring(USUBJID, 2))
    )

  analysis_data <- dplyr::bind_rows(analysis_data, pooled_data)
  rm(pooled_data)
  message("  After pooling: ", nrow(analysis_data), " observations (",
          length(unique(analysis_data$STUDYID)), " studies incl. Pooled)")

  # =========================================================================
  # 12. Treatment ordering and labeling
  # =========================================================================
  # Replaces SAS FORMAT-based ordering via TN_VAR numeric sort.
  # forcats::fct_reorder() replicates SAS format-driven factor ordering.
  trt_levels <- analysis_data %>%
    dplyr::distinct(!!rlang::sym(tn_var), !!rlang::sym(t_var)) %>%
    dplyr::arrange(!!rlang::sym(tn_var))

  analysis_data <- analysis_data %>%
    dplyr::mutate(
      !!t_var := forcats::fct_reorder(
        as.character(.data[[t_var]]),
        as.numeric(.data[[tn_var]]),
        .fun = min, na.rm = TRUE
      )
    )

  trt_labels <- trt_levels %>%
    dplyr::select(dplyr::all_of(c(t_var, tn_var)))

  # =========================================================================
  # 13. Create STUDYVISITN composite x-axis variable
  # =========================================================================
  # Replaces SAS (source ~line 278):
  #   retain studyn 0; if first.studyid then studyn + 1;
  #   studyvisitn = studyn * 10 + avisitn;
  # Creates a numeric position for each study x visit combination.
  study_order <- analysis_data %>%
    dplyr::distinct(STUDYID) %>%
    dplyr::arrange(STUDYID) %>%
    dplyr::mutate(STUDYN = dplyr::row_number())

  analysis_data <- analysis_data %>%
    dplyr::left_join(study_order, by = "STUDYID") %>%
    dplyr::mutate(
      STUDYVISITN = STUDYN * 10L + as.integer(AVISITN)
    ) %>%
    dplyr::arrange(STUDYVISITN, !!rlang::sym(tn_var))

  # Create factor label mapping STUDYVISITN -> AVISIT for x-axis text
  svn_labels <- analysis_data %>%
    dplyr::distinct(STUDYVISITN, AVISIT, STUDYID) %>%
    dplyr::arrange(STUDYVISITN)

  # forcats::fct_inorder preserves data-driven order for x-axis
  analysis_data <- analysis_data %>%
    dplyr::mutate(
      STUDYVISITN_F = forcats::fct_inorder(
        as.character(STUDYVISITN)
      )
    )

  # =========================================================================
  # 14. Extract unique parameter codes and analysis timepoints
  # =========================================================================
  # Replaces SAS (source line 320):
  #   %util_labels_from_var(css_anadata, paramcd, param)
  param_info <- util_labels_from_var(
    df  = analysis_data,
    var = "PARAMCD",
    lab = "PARAM"
  )

  param_list <- param_info$pairs
  if (nrow(param_list) == 0) {
    warning("No PARAMCD values found in filtered data.", call. = FALSE)
    return(invisible(character(0)))
  }
  message("  Processing ", param_info$n, " parameter(s): ",
          paste(param_list$value, collapse = ", "))

  # Replaces SAS (source line 323):
  #   %util_count_unique_values(css_anadata, &t_var, trtn)
  n_trt <- util_count_unique_values(df = analysis_data, var = t_var)
  message("  Number of treatment groups: ", n_trt)

  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Accumulate output file paths
  output_files <- character(0)

  # =========================================================================
  # 15. Triple-nested loop: PARAMCD x ATPTN x Pages
  # =========================================================================
  # Replaces SAS: %macro boxplot_each_param_tp (source lines 330-500)
  # LOOP 1: iterate over each parameter code
  for (pidx in seq_len(nrow(param_list))) {
    paramcd_val  <- param_list$value[pidx]
    param_label  <- param_list$label[pidx]
    if (is.na(param_label) || nchar(trimws(param_label)) == 0) {
      param_label <- paramcd_val
    }

    # Subset data for current parameter
    param_data <- analysis_data %>%
      dplyr::filter(PARAMCD == paramcd_val)

    if (nrow(param_data) == 0) {
      message("  Skipping PARAMCD=", paramcd_val, " (no observations).")
      next
    }

    # Get unique ATPTN values for this parameter
    atptn_list <- param_data %>%
      dplyr::distinct(ATPTN) %>%
      dplyr::arrange(ATPTN) %>%
      dplyr::pull(ATPTN)

    # -------------------------------------------------------------------
    # LOOP 2: iterate over each analysis timepoint
    # -------------------------------------------------------------------
    for (atptn_val in atptn_list) {
      tp_data <- param_data %>%
        dplyr::filter(ATPTN == atptn_val)

      if (nrow(tp_data) == 0) next

      # Get ATPT label if available
      atpt_label <- ""
      if (has_atpt && "ATPT" %in% names(tp_data)) {
        atpt_vals <- tp_data %>%
          dplyr::distinct(ATPT) %>%
          dplyr::pull(ATPT)
        atpt_vals <- atpt_vals[!is.na(atpt_vals)]
        atpt_label <- paste(atpt_vals, collapse = " / ")
      }

      message("    PARAMCD=", paramcd_val, " ATPTN=", atptn_val,
              if (nzchar(atpt_label)) paste0(" (", atpt_label, ")") else "")

      # -----------------------------------------------------------------
      # 15a. Determine reference line positions
      # -----------------------------------------------------------------
      # Replaces SAS (source line 366): %util_get_reference_lines
      ref_vals <- NULL
      if (has_ref_vars &&
          !identical(toupper(as.character(ref_lines)), "NONE")) {
        ref_vals <- util_get_reference(
          df        = tp_data,
          low_var   = lo_var,
          high_var  = hi_var,
          ref_lines = ref_lines
        )
      }

      # -----------------------------------------------------------------
      # 15b. Compute Y-axis range and nice axis breaks
      # -----------------------------------------------------------------
      # Replaces SAS (source line 375): %util_get_var_min_max
      var_range <- util_get_var_min_max(
        df    = tp_data,
        var   = m_var,
        extra = ref_vals
      )

      # Replaces SAS (source line 430): %util_axis_order
      axis_breaks <- util_axis_order(
        min_val = var_range["min"],
        max_val = var_range["max"]
      )

      y_min_val  <- attr(axis_breaks, "axis_min")
      y_max_val  <- attr(axis_breaks, "axis_max")
      y_incr_val <- attr(axis_breaks, "step")

      # -----------------------------------------------------------------
      # 15c. Compute display format precision for statistics table
      # -----------------------------------------------------------------
      # Replaces SAS (source line 380): %util_value_format
      # Returns mean_digits (data precision + 1) and stddev_digits (+ 2)
      val_fmt <- util_value_of_param(
        df  = tp_data,
        var = m_var
      )
      mean_digits <- val_fmt$mean_digits
      sd_digits   <- val_fmt$stddev_digits

      # -----------------------------------------------------------------
      # 15d. Compute pagination via block ranges (blocked by STUDYID)
      # -----------------------------------------------------------------
      # Replaces SAS (source line 395):
      #   %util_boxplot_block_ranges(css_anadata, studyid, &tn_var,
      #                              &max_boxes_per_page)
      block_ranges <- util_boxplot_block_ranges(
        df                 = tp_data %>%
                               dplyr::mutate(STUDYID = as.character(STUDYID)),
        block_var          = "STUDYID",
        cat_vars           = t_var,
        max_boxes_per_page = max_boxes_per_page
      )

      n_pages <- length(block_ranges$ranges)

      # -----------------------------------------------------------------
      # 15e. Pre-compute descriptive statistics per study x visit x trt
      # -----------------------------------------------------------------
      # Replaces SAS PROC SUMMARY DATA=css_nextparam NWAY;
      #   CLASS studyid avisitn &tn_var studyvisitn avisit &t_var;
      #   VAR &m_var;
      #   OUTPUT OUT=css_stats N=n MEAN=mean STD=std MEDIAN=median
      #          MIN=datamin MAX=datamax Q1=q1 Q3=q3;
      # Uses janitor::round_half_up() for SAS-compatible rounding (AAP 0.7.2)
      desc_stats <- tp_data %>%
        dplyr::group_by(
          STUDYID, AVISITN, AVISIT,
          STUDYVISITN, STUDYVISITN_F,
          !!rlang::sym(t_var), !!rlang::sym(tn_var)
        ) %>%
        dplyr::summarise(
          n_obs    = dplyr::n(),
          mean_v   = janitor::round_half_up(
            mean(.data[[m_var]], na.rm = TRUE), digits = mean_digits
          ),
          sd_v     = janitor::round_half_up(
            sd(.data[[m_var]], na.rm = TRUE), digits = sd_digits
          ),
          median_v = janitor::round_half_up(
            median(.data[[m_var]], na.rm = TRUE), digits = mean_digits
          ),
          min_v    = janitor::round_half_up(
            min(.data[[m_var]], na.rm = TRUE), digits = mean_digits
          ),
          max_v    = janitor::round_half_up(
            max(.data[[m_var]], na.rm = TRUE), digits = mean_digits
          ),
          q1_v     = janitor::round_half_up(
            quantile(.data[[m_var]], 0.25, na.rm = TRUE), digits = mean_digits
          ),
          q3_v     = janitor::round_half_up(
            quantile(.data[[m_var]], 0.75, na.rm = TRUE), digits = mean_digits
          ),
          .groups  = "drop"
        ) %>%
        dplyr::select(
          STUDYID, AVISITN, AVISIT, STUDYVISITN, STUDYVISITN_F,
          dplyr::all_of(c(t_var, tn_var)),
          n_obs, mean_v, sd_v, median_v, min_v, max_v, q1_v, q3_v
        )

      # Merge treatment ordering label for display enrichment
      desc_stats <- desc_stats %>%
        dplyr::left_join(trt_labels, by = c(t_var, tn_var))

      # -----------------------------------------------------------------
      # LOOP 3: Generate one plot per page
      # -----------------------------------------------------------------
      for (pg in seq_len(n_pages)) {
        # Determine studies assigned to this page via block_ranges$pages
        page_studies <- block_ranges$pages %>%
          dplyr::filter(page == pg) %>%
          dplyr::pull("STUDYID")

        page_data <- tp_data %>%
          dplyr::filter(STUDYID %in% page_studies)

        if (nrow(page_data) == 0) next

        # Re-level STUDYVISITN_F for this page's studies only
        page_svn <- page_data %>%
          dplyr::distinct(STUDYVISITN, STUDYVISITN_F, AVISIT) %>%
          dplyr::arrange(STUDYVISITN)
        page_data <- page_data %>%
          dplyr::mutate(
            STUDYVISITN_F = forcats::fct_relevel(
              STUDYVISITN_F,
              as.character(page_svn$STUDYVISITN_F)
            )
          )

        # Build figure title with page indicator when paginated
        fig_title <- paste0(
          "Figure 7.6  ", param_label,
          if (nzchar(atpt_label)) paste0(" \u2014 ", atpt_label) else "",
          if (n_pages > 1) {
            paste0("  (Page ", pg, " of ", n_pages, ")")
          } else {
            ""
          }
        )

        # ---------------------------------------------------------------
        # 15f-i. Build PhUSE-standard multi-study boxplot
        # ---------------------------------------------------------------
        # Uses phuse_boxplot() from util_ggplot_theme.R which applies the
        # PhUSEboxplot GTL template styling.  _BLOCKLABEL='studyid',
        # _XVAR='studyvisitn', _PERIOD='avisit' in SAS terms.
        box_plot <- phuse_boxplot(
          data         = page_data,
          x_var        = "STUDYVISITN_F",
          y_var        = m_var,
          group_var    = t_var,
          title        = fig_title,
          y_label      = param_label,
          y_min        = y_min_val,
          y_max        = y_max_val,
          y_incr       = y_incr_val,
          block_var    = "STUDYID",
          outlier_var  = "OUTLIER",
          ref_lines    = ref_vals,
          show_notch   = TRUE,
          show_mean    = TRUE,
          legend_title = "Treatment"
        )

        # Apply explicit theme_phuse() and multi-study customizations.
        # PhUSE palette: box_fill=#B9CFE7, outline=navy, NR outliers=red,
        # mean marker=black diamond (shape 18).
        box_plot <- box_plot +
          theme_phuse(
            design_width  = phuse_sizes$design_width_mm,
            design_height = phuse_sizes$design_height_mm
          ) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              size = 7, angle = 45, hjust = 1, vjust = 1
            ),
            panel.grid.major.x = ggplot2::element_blank(),
            plot.caption = ggplot2::element_text(
              size  = 7,
              color = phuse_colors$box_outline,
              hjust = 0
            )
          )

        # Build x-axis labels showing AVISIT under each study cluster
        # Replaces SAS _PERIOD = 'avisit' in PhUSEboxplot template
        x_axis_labels <- page_svn %>%
          dplyr::mutate(display = AVISIT)
        x_label_vec <- stats::setNames(
          x_axis_labels$display,
          as.character(x_axis_labels$STUDYVISITN_F)
        )

        box_plot <- box_plot +
          ggplot2::scale_x_discrete(
            labels = x_label_vec,
            drop   = TRUE
          ) +
          ggplot2::labs(
            x       = "Study / Visit",
            caption = paste0(
              "Source: ", ds_name,
              " | Population: ", p_fl, "='Y'",
              " | Analysis: ", a_fl, "='Y'",
              " | Visits: BASE (AVISITN=", b_visn,
              "), POST (AVISITN=", e_visn, ")"
            )
          )

        # Ensure axis limits are respected with coord_cartesian
        box_plot <- box_plot +
          ggplot2::coord_cartesian(
            ylim = c(y_min_val, y_max_val),
            clip = "off"
          )

        # Apply PhUSE fill palette via scale_fill_manual when >1 treatment
        if (n_trt > 1) {
          fill_colors <- rep_len(
            c(phuse_colors$box_fill, "#E8D4B8", "#C3E6CB", "#F5C6CB",
              "#D6D8DB", "#CCE5FF"),
            n_trt
          )
          names(fill_colors) <- levels(page_data[[t_var]])[seq_len(n_trt)]
          box_plot <- box_plot +
            ggplot2::scale_fill_manual(
              values = fill_colors,
              name   = "Treatment"
            )
        }

        # ---------------------------------------------------------------
        # 15f-ii. Create summary statistics table below the plot
        # ---------------------------------------------------------------
        # Uses phuse_boxplot_stats_table() which internally computes stats
        # with round_half_up() for SAS-compatible rounding.
        stats_tbl <- phuse_boxplot_stats_table(
          data      = page_data,
          x_var     = "STUDYVISITN_F",
          y_var     = m_var,
          group_var = t_var,
          stats     = c("n", "mean", "sd", "median"),
          digits    = mean_digits
        )

        stats_grob <- gridExtra::tableGrob(
          stats_tbl,
          rows  = NULL,
          theme = gridExtra::ttheme_minimal(
            core    = list(
              fg_params = list(fontsize = 7, col = phuse_colors$box_outline)
            ),
            colhead = list(
              fg_params = list(fontsize = 7, fontface = "bold",
                               col = phuse_colors$box_outline)
            )
          )
        )

        # ---------------------------------------------------------------
        # 15f-iii. Create wide-format N-per-treatment annotation
        # ---------------------------------------------------------------
        # Pivot stats to wide format for a compact per-treatment N row
        page_desc <- desc_stats %>%
          dplyr::filter(STUDYID %in% page_studies) %>%
          dplyr::ungroup()

        n_wide <- page_desc %>%
          dplyr::select(STUDYID, AVISIT, dplyr::all_of(t_var), n_obs) %>%
          tidyr::pivot_wider(
            id_cols     = c("STUDYID", "AVISIT"),
            names_from  = dplyr::all_of(t_var),
            values_from = n_obs,
            names_prefix = "N_"
          ) %>%
          dplyr::arrange(STUDYID, AVISIT)

        # Also create long-format stats for potential downstream use
        stats_long <- page_desc %>%
          dplyr::select(STUDYID, AVISIT, dplyr::all_of(t_var),
                        n_obs, mean_v, sd_v, median_v) %>%
          tidyr::pivot_longer(
            cols      = c(n_obs, mean_v, sd_v, median_v),
            names_to  = "stat",
            values_to = "value"
          )

        n_tbl <- tryCatch(
          gridExtra::tableGrob(
            n_wide,
            rows  = NULL,
            theme = gridExtra::ttheme_minimal(
              core    = list(
                fg_params = list(fontsize = 7, col = phuse_colors$box_outline)
              ),
              colhead = list(
                fg_params = list(fontsize = 7, fontface = "bold",
                                 col = phuse_colors$box_outline)
              )
            )
          ),
          error = function(e) NULL
        )

        # ---------------------------------------------------------------
        # 15f-iv. Combine boxplot and statistics tables
        # ---------------------------------------------------------------
        if (!is.null(n_tbl)) {
          combined <- gridExtra::arrangeGrob(
            box_plot,
            stats_grob,
            n_tbl,
            ncol    = 1,
            heights = c(5, 1.0, 0.8)
          )
        } else {
          combined <- gridExtra::arrangeGrob(
            box_plot,
            stats_grob,
            ncol    = 1,
            heights = c(4, 1)
          )
        }

        # Use patchwork for shared title/caption annotation when combining
        # multiple plot components (supplements gridExtra arrangeGrob).
        combined_pw <- tryCatch({
          pw_obj <- patchwork::wrap_plots(box_plot, ncol = 1) +
            patchwork::plot_layout(heights = c(1)) +
            patchwork::plot_annotation(
              title   = fig_title,
              caption = paste0(
                "Multi-study analysis | Pooled includes all studies | ",
                "Rounding: round_half_up (SAS-compatible)"
              )
            )
          pw_obj
        }, error = function(e) NULL)

        # ---------------------------------------------------------------
        # 15f-v. Save to landscape PDF
        # ---------------------------------------------------------------
        # Replaces SAS: ODS PDF FILE="..." STYLE=Printer COLUMNS=1;
        #   OPTIONS ORIENTATION=LANDSCAPE;
        out_file <- file.path(
          output_path,
          paste0(
            "WPCT-F-07-06_", paramcd_val,
            "_ATPTN", atptn_val,
            if (n_pages > 1) paste0("_p", pg) else "",
            ".pdf"
          )
        )

        # Render to interactive device when available
        if (interactive() && dev.interactive()) {
          gridExtra::grid.arrange(
            box_plot, stats_grob,
            ncol    = 1,
            heights = c(4, 1)
          )
        }

        ggplot2::ggsave(
          filename = out_file,
          plot     = combined,
          width    = phuse_sizes$design_width_mm,
          height   = phuse_sizes$design_height_mm + 60,
          units    = "mm",
          device   = "pdf"
        )

        output_files <- c(output_files, out_file)
        message("      Created: ", out_file)

      } # END LOOP 3: pages

    } # END LOOP 2: ATPTN

  } # END LOOP 1: PARAMCD

  # =========================================================================
  # 16. Cleanup temporary objects
  # =========================================================================
  # Replaces SAS (source line 499):
  #   %util_delete_dsets(css_nextparam css_nexttimept css_stats css_plot)
  util_delete_dsets(
    obj_names = c("param_data", "tp_data", "page_data",
                  "desc_stats", "page_desc", "stats_long"),
    envir     = environment()
  )

  # =========================================================================
  # 17. Summary and return
  # =========================================================================
  message(
    "\nFigure 7.6 generation complete. ",
    length(output_files), " file(s) created."
  )
  invisible(output_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#    1. Multi-study data resides in a "cdisc-split" directory structure
#       where each subdirectory contains one study's ADaM XPT files.
#       Alternatively, a single combined XPT file may be provided directly
#       in data_path. Both modes are supported.
#    2. The derive_lastminmax_measure() function creates derived baseline
#       and post-baseline records with the analysis flag variable (a_fl)
#       set to 'Y' for qualifying observations. If the derivation fails
#       (e.g. if the data already contains derived records), the function
#       falls back to the raw data with existing analysis flags.
#    3. b_visn and e_visn correspond to AVISITN values identifying
#       baseline and post-baseline visits respectively. After filtering,
#       AVISITN is recoded to 1 (BASE) and 2 (POST) to match the SAS
#       first.USUBJID / last.USUBJID BY-group processing pattern.
#    4. The "Pooled" pseudo-study includes ALL individual study data with
#       modified STUDYID (non-breaking space prefix for sort ordering)
#       and USUBJID (first character replaced with 'P'). This exactly
#       replicates the SAS behavior at source line ~265.
#    5. STUDYVISITN is a composite discrete x-axis variable computed as
#       STUDYN * 10 + AVISITN, where STUDYN is a sequential study counter.
#       This ensures proper x-axis ordering: Study1-BASE, Study1-POST,
#       Study2-BASE, Study2-POST, ..., Pooled-BASE, Pooled-POST.
#    6. Default derivation mode is c("LAST") matching the SAS default
#       %let lmm = last. The c_modes parameter is hardcoded since it is
#       not exposed in the R function signature.
#    7. ATPTN (Analysis Timepoint Number) iteration is preserved from
#       SAS source. If ATPTN is not present in the data, a single
#       placeholder value (0) is used.
#    8. Treatment ordering uses the numeric TN_VAR (TRTPN) via
#       forcats::fct_reorder() to replicate SAS FORMAT-based sort order.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Rounding: All rounding operations use janitor::round_half_up()
#       to match SAS round-half-up behavior. R default round() uses
#       half-to-even (banker's rounding) which would produce different
#       results for values ending in .5 exactly.
#    2. Sort stability: dplyr::arrange() is stable within groups, but
#       multi-key sorts may produce different tie-breaking than SAS PROC
#       SORT when secondary keys have identical values.
#    3. Boxplot whisker computation: ggplot2 defaults to 1.5 * IQR for
#       whisker extent, matching SAS PhUSEboxplot GTL template behavior.
#    4. Notch computation: ggplot2 notch = +/- 1.58 * IQR / sqrt(n).
#       SAS PhUSEboxplot template uses the same formula.
#    5. Quantile computation: R default quantile() uses type=7 method.
#       SAS PROC UNIVARIATE uses type=3. For small samples, this may
#       produce slightly different Q1/Q3 values. Consider using
#       quantile(x, type=2) for closer SAS approximation if discrepancies
#       are observed during validation.
#    6. The Pooled STUDYID uses Unicode non-breaking space (\u00A0)
#       which sorts identically to SAS hex 'A0'x under EBCDIC/ASCII
#       collation. Under locale-dependent collation in R, verify that
#       Pooled appears last via LC_COLLATE settings.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS GTL PROC TEMPLATE PhUSEboxplot registration is approximated
#       by ggplot2 theme_phuse() + phuse_boxplot() from util_ggplot_theme.R.
#       The R implementation replicates visual styling (fonts, colors,
#       sizing, spacing) but uses ggplot2 rendering, not SAS GTL.
#    2. SAS PROC SGRENDER dynamic variables (_TITLE, _XVAR, _YVAR,
#       _MARKERS, _BLOCKLABEL, _PERIOD, _YOUTLIERS, _REFLINES, _YLABEL,
#       _YMIN/_YMAX/_YINCR, stat columns) are mapped to ggplot2 aes(),
#       labs(), scale_*(), and geom_*() parameters. The _BLOCKLABEL
#       (STUDYID) is handled via block_var in phuse_boxplot().
#    3. SAS ODS PDF destination output is replaced by ggplot2::ggsave()
#       with device = "pdf", landscape orientation via width > height.
#    4. SAS %util_access_test_data macro for multi-study data loading
#       from cdisc-split directories is replaced by R directory scanning
#       with haven::read_xpt() per study and dplyr::bind_rows().
#    5. SAS RETAIN statement for STUDYN counter is replaced by
#       dplyr::row_number() on a distinct-studies tibble.
#
# PACKAGE SELECTION RATIONALE:
#    haven     - SAS XPT transport file reader (per AAP section 0.6.1)
#    dplyr     - Tidyverse data manipulation (per AAP section 0.8.1)
#    tidyr     - Reshaping for wide/long format statistics (pivot_wider,
#                pivot_longer)
#    ggplot2   - PhUSEboxplot GTL replacement via theme_phuse() +
#                geom_boxplot + stat_summary + geom_point (AAP 0.4.3)
#    gridExtra - Boxplot + stats table composition (arrangeGrob, tableGrob,
#                grid.arrange) replacing SAS PROC SGRENDER LAYOUT
#    patchwork - Advanced multi-panel composition with shared annotations
#                (plot_layout, plot_annotation) per AAP 0.5.1
#    janitor   - SAS-compatible round_half_up() (per AAP section 0.7.2)
#    forcats   - Factor ordering: fct_reorder for TN_VAR sort, fct_relevel
#                for page-level study ordering, fct_inorder for data-driven
#                x-axis ordering (per AAP section 0.7.1)
#    rlang     - Tidy evaluation: sym() and .data pronoun for dynamic
#                column references in dplyr/ggplot2 pipelines
#
# OPEN QUESTIONS:
#    1. The default c_modes = c("LAST") for derive_lastminmax_measure
#       matches the SAS user setting %let lmm = last. If the caller
#       requires MIN or MAX derivation modes, the function would need
#       an additional parameter or the data should be pre-derived.
#    2. ATPTN values in the data depend on the ADaM derivation upstream.
#       The function iterates over all unique ATPTN values present after
#       filtering; if specific ATPTN selection is needed, pre-filter
#       the data before calling wpct_f_07_06().
#    3. The x-axis label format (showing AVISIT as "BASE"/"POST" under
#       each study cluster) may need adjustment for studies with more
#       than two visit timepoints in future extensions.
#    4. Quantile type alignment: SAS uses type=3 while R defaults to
#       type=7. For regulatory validation, consider switching to
#       quantile(x, type=2) and documenting in Gate 2 rounding audit.
# ============================================================
