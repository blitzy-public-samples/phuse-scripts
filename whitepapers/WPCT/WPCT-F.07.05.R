# ============================================================
# HEADER
# Display:     Figure 7.5 Box plot - PhUSEboxplot GTL Template Variant
# White paper: Central Tendency
# Specs:       https://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/specification/
# Output:      https://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/WPCT/outputs_r/
# NOTE:        No corresponding SAS source (WPCT-F.07.05.sas) exists in the repository.
#              The SAS file numbering jumps from WPCT-F.07.03.sas to WPCT-F.07.06.sas.
#              Visual specification: whitepapers/images/wpct/target_07.05.png
#              This figure emphasizes the PhUSEboxplot GTL Template styling via
#              theme_phuse() and phuse_boxplot() from util_ggplot_theme.R.
# Migrated from: N/A (no WPCT-F.07.05.sas exists in the repository)
# ============================================================

# ---------------------------------------------------------------------------
# Package Loading
# ---------------------------------------------------------------------------
# Core tidyverse stack — per AAP section 0.8.1 tidyverse-over-base-R mandate
library(haven)       # SAS XPT data I/O via read_xpt()
library(dplyr)       # Data manipulation pipelines replacing SAS DATA steps
library(tidyr)       # Reshaping via pivot_wider(), drop_na()
library(ggplot2)     # Visualization replacing PROC SGRENDER / PhUSEboxplot GTL
library(gridExtra)   # Multi-panel composition: grid.arrange, arrangeGrob, tableGrob
library(janitor)     # SAS-compatible rounding via round_half_up()
library(forcats)     # Factor ordering replacing SAS FORMAT-based sort order
library(yaml)        # YAML config parser for migration_config.yaml
library(rlang)       # Tidy evaluation: .data pronoun, sym(), !!, :=

# ---------------------------------------------------------------------------
# Main Function: wpct_f_07_05
# ---------------------------------------------------------------------------
#' Produce Figure 7.5 — Box Plot with PhUSEboxplot GTL Template Styling
#'
#' Creates paginated boxplots of observed lab or vital sign values by visit
#' and treatment group, rendered with the PhUSE-standard ggplot2 theme that
#' replicates the SAS GTL PhUSEboxplot template registered by the
#' \%util_proc_template macro.
#'
#' KEY DIFFERENTIATOR for Figure 7.5: explicit application of
#' \code{theme_phuse()} for standardized PhUSE styling (colors, fonts,
#' sizing) and \code{phuse_boxplot()} constructor for pre-configured
#' boxplot geometry and layout constants.
#'
#' @param data_path  Character. Directory containing analysis XPT files.
#'   If NULL, reads from config/migration_config.yaml data_paths$adam_path.
#' @param output_path Character. Directory for PDF output files.
#'   If NULL, reads from config/migration_config.yaml output_paths$figure_output_path.
#' @param ds_name   Character. Analysis dataset filename. Default \code{"advs.xpt"}.
#' @param t_var     Character. Treatment name variable. Default \code{"TRTP"}.
#' @param tn_var    Character. Treatment numeric sort variable. Default \code{"TRTPN"}.
#' @param m_var     Character. Measurement variable. Default \code{"AVAL"}.
#' @param lo_var    Character. Lower normal range variable. Default \code{"ANRLO"}.
#' @param hi_var    Character. Upper normal range variable. Default \code{"ANRHI"}.
#' @param p_fl      Character. Population flag variable. Default \code{"SAFFL"}.
#' @param a_fl      Character. Analysis flag variable. Default \code{"ANL01FL"}.
#' @param ref_lines Character or numeric vector. Reference line mode:
#'   \code{"NONE"}, \code{"UNIFORM"}, \code{"NARROW"}, \code{"ALL"}, or a
#'   numeric vector of explicit positions. Default \code{"NARROW"}.
#' @param max_boxes_per_page Integer. Maximum box groups per page for
#'   pagination. Default \code{20}.
#' @return Invisible character vector of output PDF file paths.
wpct_f_07_05 <- function(
  data_path           = NULL,
  output_path         = NULL,
  ds_name             = "advs.xpt",
  t_var               = "TRTP",
  tn_var              = "TRTPN",
  m_var               = "AVAL",
  lo_var              = "ANRLO",
  hi_var              = "ANRHI",
  p_fl                = "SAFFL",
  a_fl                = "ANL01FL",
  ref_lines           = "NARROW",
  max_boxes_per_page  = 20
) {

  # -----------------------------------------------------------------------
  # 1. Configuration — resolve paths from YAML config when not supplied
  # -----------------------------------------------------------------------
  config <- tryCatch(
    yaml::read_yaml("config/migration_config.yaml"),
    error = function(e) {
      tryCatch(
        yaml::read_yaml(
          file.path("..", "..", "config", "migration_config.yaml")
        ),
        error = function(e2) NULL
      )
    }
  )

  # Resolve data path from config or fallback default
  if (is.null(data_path)) {
    data_path <- if (!is.null(config)) {
      config$data_paths$adam_path
    } else {
      "data/adam/cdisc"
    }
  }

  # Resolve output path from config or fallback default
  if (is.null(output_path)) {
    output_path <- if (!is.null(config)) {
      config$output_paths$figure_output_path
    } else {
      "output/figures"
    }
  }

  # Resolve WPCT-specific defaults from domain_settings
  wpct_defaults <- if (!is.null(config)) {
    config$domain_settings$wpct
  } else {
    NULL
  }

  # -----------------------------------------------------------------------
  # 2. Source PhUSE utility functions
  # -----------------------------------------------------------------------
  util_path <- if (!is.null(config)) {
    config$r_source_paths$wp_utilities_path
  } else {
    "whitepapers/utilities/R"
  }

  util_files <- c(
    "util_ggplot_theme.R",
    "util_boxplot_block_ranges.R",
    "util_axis_order.R",
    "util_get_reference.R",
    "util_get_var_min_max.R"
  )
  for (uf in util_files) {
    uf_full <- file.path(util_path, uf)
    if (file.exists(uf_full)) {
      source(uf_full, local = FALSE)
    } else {
      stop("Required utility file not found: ", uf_full, call. = FALSE)
    }
  }

  # -----------------------------------------------------------------------
  # 3. Load analysis dataset via haven::read_xpt()
  # -----------------------------------------------------------------------
  ds_file <- file.path(data_path, ds_name)
  if (!file.exists(ds_file)) {
    stop(
      "Analysis dataset not found: ", ds_file,
      "\nVerify data_path ('", data_path, "') and ds_name ('", ds_name, "').",
      call. = FALSE
    )
  }
  raw_data <- haven::read_xpt(ds_file)
  message("Loaded ", nrow(raw_data), " observations from ", ds_file)

  # -----------------------------------------------------------------------
  # 4. Normalize column names to uppercase (CDISC ADaM convention)
  # -----------------------------------------------------------------------
  names(raw_data) <- toupper(names(raw_data))
  t_var  <- toupper(t_var)
  tn_var <- toupper(tn_var)
  m_var  <- toupper(m_var)
  lo_var <- toupper(lo_var)
  hi_var <- toupper(hi_var)
  p_fl   <- toupper(p_fl)
  a_fl   <- toupper(a_fl)

  # -----------------------------------------------------------------------
  # 5. Validate required variables exist in dataset
  # -----------------------------------------------------------------------
  required_vars <- c(
    t_var, tn_var, m_var, p_fl, a_fl,
    "PARAMCD", "PARAM", "AVISIT", "AVISITN", "USUBJID"
  )
  available_vars <- names(raw_data)
  missing_vars   <- setdiff(required_vars, available_vars)
  if (length(missing_vars) > 0) {
    stop(
      "Required variables missing from dataset: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Check normal range variables (optional — needed for ref lines / outliers)
  has_ref_vars <- all(c(lo_var, hi_var) %in% available_vars)
  if (!has_ref_vars &&
      !identical(toupper(as.character(ref_lines)), "NONE")) {
    message(
      "Normal range variables (", lo_var, ", ", hi_var,
      ") not found. Reference lines and outlier flagging suppressed."
    )
    ref_lines <- "NONE"
  }

  # -----------------------------------------------------------------------
  # 6. Normalize character flag columns and filter population / analysis
  # -----------------------------------------------------------------------
  # Ensure flag variables are clean uppercase characters before filtering.
  # SAS equivalent: WHERE UPCASE(&p_fl) = 'Y' AND UPCASE(&a_fl) = 'Y'
  # Missing values (NA) never equal 'Y' — no implicit zero substitution.
  analysis_data <- raw_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(p_fl, a_fl)),
        ~ toupper(trimws(as.character(.x)))
      )
    ) %>%
    dplyr::filter(
      .data[[p_fl]] == "Y",
      .data[[a_fl]] == "Y"
    ) %>%
    tidyr::drop_na(dplyr::all_of(m_var))

  if (nrow(analysis_data) == 0) {
    warning(
      "No observations remain after population/analysis flag filtering.",
      call. = FALSE
    )
    return(invisible(character(0)))
  }
  message(nrow(analysis_data), " observations after flag filtering.")

  # -----------------------------------------------------------------------
  # 7. Order treatment arms by numeric sort variable (TRTPN)
  # -----------------------------------------------------------------------
  # Replaces SAS FORMAT-based ordering. forcats::fct_reorder preserves the
  # numeric treatment arm sort order defined by TRTPN.
  analysis_data <- analysis_data %>%
    dplyr::mutate(
      !!rlang::sym(t_var) := forcats::fct_reorder(
        as.character(.data[[t_var]]),
        .data[[tn_var]],
        .fun = min,
        na.rm = TRUE
      )
    )

  # Order visits by AVISITN, then lock label order with fct_inorder
  analysis_data <- analysis_data %>%
    dplyr::arrange(AVISITN) %>%
    dplyr::mutate(
      AVISIT = forcats::fct_inorder(as.character(AVISIT))
    )

  # Build treatment label lookup for stats table enrichment
  trt_labels <- analysis_data %>%
    dplyr::distinct(.data[[tn_var]], .data[[t_var]]) %>%
    dplyr::arrange(.data[[tn_var]])

  # -----------------------------------------------------------------------
  # 8. Extract unique parameter codes for iteration
  # -----------------------------------------------------------------------
  param_list <- analysis_data %>%
    dplyr::distinct(PARAMCD, PARAM) %>%
    dplyr::arrange(PARAMCD)

  if (nrow(param_list) == 0) {
    warning("No PARAMCD values found in filtered data.", call. = FALSE)
    return(invisible(character(0)))
  }
  message(
    "Processing ", nrow(param_list), " parameter(s): ",
    paste(param_list$PARAMCD, collapse = ", ")
  )

  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Accumulate output file paths
  output_files <- character(0)

  # -----------------------------------------------------------------------
  # 9. Iterate over each PARAMCD
  # -----------------------------------------------------------------------
  for (pidx in seq_len(nrow(param_list))) {
    paramcd_val <- param_list$PARAMCD[pidx]
    param_label <- param_list$PARAM[pidx]
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

    # -----------------------------------------------------------------
    # 9a. Create normal-range outlier variable
    # -----------------------------------------------------------------
    # Values outside [ANRLO, ANRHI] flagged for red dot overlay (NR outlier).
    # SAS: IF m_var < lo_var OR m_var > hi_var THEN outlier = m_var; ELSE .;
    if (has_ref_vars) {
      param_data <- param_data %>%
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
      param_data <- param_data %>%
        dplyr::mutate(OUTLIER = NA_real_)
    }

    # -----------------------------------------------------------------
    # 9b. Determine reference line positions
    # -----------------------------------------------------------------
    ref_vals <- NULL
    if (has_ref_vars &&
        !identical(toupper(as.character(ref_lines)), "NONE")) {
      ref_vals <- util_get_reference(
        df        = param_data,
        low_var   = lo_var,
        high_var  = hi_var,
        ref_lines = ref_lines
      )
    }

    # -----------------------------------------------------------------
    # 9c. Compute Y-axis range and nice axis breaks
    # -----------------------------------------------------------------
    var_range <- util_get_var_min_max(
      df    = param_data,
      var   = m_var,
      extra = ref_vals
    )

    axis_breaks <- util_axis_order(
      min_val = var_range["min"],
      max_val = var_range["max"]
    )

    y_min_val  <- attr(axis_breaks, "axis_min")
    y_max_val  <- attr(axis_breaks, "axis_max")
    y_incr_val <- attr(axis_breaks, "step")

    # -----------------------------------------------------------------
    # 9d. Compute pagination via block ranges
    # -----------------------------------------------------------------
    # Convert AVISIT to character for pagination utility (expects character
    # or numeric, not factor). Factor ordering is re-applied below per page.
    block_ranges <- util_boxplot_block_ranges(
      df                 = param_data %>%
                             dplyr::mutate(AVISIT = as.character(AVISIT)),
      block_var          = "AVISIT",
      cat_vars           = t_var,
      max_boxes_per_page = max_boxes_per_page
    )

    n_pages <- length(block_ranges$ranges)

    # -----------------------------------------------------------------
    # 9e. Pre-compute descriptive statistics per visit x treatment
    # -----------------------------------------------------------------
    # SAS equivalent: PROC SUMMARY DATA=work NWAY;
    #   CLASS &t_var AVISITN; VAR &m_var;
    #   OUTPUT OUT=stats N=n MEAN=mean STD=sd MEDIAN=median
    #          MIN=min MAX=max Q1=q1 Q3=q3;
    # Uses janitor::round_half_up() for SAS-compatible rounding.
    desc_stats <- param_data %>%
      dplyr::group_by(AVISIT, AVISITN, !!rlang::sym(t_var)) %>%
      dplyr::summarise(
        n_obs   = dplyr::n(),
        mean_v  = janitor::round_half_up(
          mean(.data[[m_var]], na.rm = TRUE), digits = 1
        ),
        sd_v    = janitor::round_half_up(
          sd(.data[[m_var]], na.rm = TRUE), digits = 2
        ),
        median_v = janitor::round_half_up(
          median(.data[[m_var]], na.rm = TRUE), digits = 1
        ),
        min_v   = janitor::round_half_up(
          min(.data[[m_var]], na.rm = TRUE), digits = 1
        ),
        max_v   = janitor::round_half_up(
          max(.data[[m_var]], na.rm = TRUE), digits = 1
        ),
        q1_v    = janitor::round_half_up(
          quantile(.data[[m_var]], 0.25, na.rm = TRUE), digits = 1
        ),
        q3_v    = janitor::round_half_up(
          quantile(.data[[m_var]], 0.75, na.rm = TRUE), digits = 1
        ),
        .groups = "drop"
      ) %>%
      dplyr::select(
        AVISIT, AVISITN,
        dplyr::all_of(t_var),
        n_obs, mean_v, sd_v, median_v, min_v, max_v, q1_v, q3_v
      )

    # Merge treatment order label for display enrichment.
    # Join by t_var only (tn_var not in desc_stats group-by columns).
    desc_stats <- desc_stats %>%
      dplyr::left_join(
        trt_labels,
        by = t_var
      )

    # -----------------------------------------------------------------
    # 9f. Generate one plot per page
    # -----------------------------------------------------------------
    for (pg in seq_len(n_pages)) {
      # Determine visits assigned to this page.
      # The pages tibble from util_boxplot_block_ranges() uses the actual
      # block_var column name (e.g., "AVISIT"), not a generic "block_value".
      page_visits <- block_ranges$pages %>%
        dplyr::filter(page == pg) %>%
        dplyr::pull("AVISIT")

      page_data <- param_data %>%
        dplyr::filter(AVISIT %in% page_visits) %>%
        dplyr::mutate(
          AVISIT = forcats::fct_relevel(AVISIT, page_visits)
        )

      if (nrow(page_data) == 0) next

      # Build figure title with page indicator when paginated
      fig_title <- paste0(
        "Figure 7.5  ", param_label,
        if (n_pages > 1) {
          paste0("  (Page ", pg, " of ", n_pages, ")")
        } else {
          ""
        }
      )

      # -----------------------------------------------------------
      # 9f-i. Create PhUSE-standard boxplot
      # -----------------------------------------------------------
      # KEY DIFFERENTIATOR: phuse_boxplot() applies theme_phuse()
      # replicating the SAS PhUSEboxplot GTL template.
      # PhUSE palette: box_fill = #B9CFE7, outline = navy,
      # NR outliers = red, mean marker = black diamond (shape 18).
      box_plot <- phuse_boxplot(
        data         = page_data,
        x_var        = "AVISITN",
        y_var        = m_var,
        group_var    = t_var,
        title        = fig_title,
        y_label      = param_label,
        y_min        = y_min_val,
        y_max        = y_max_val,
        y_incr       = y_incr_val,
        block_var    = "AVISIT",
        outlier_var  = "OUTLIER",
        ref_lines    = ref_vals,
        show_notch   = TRUE,
        show_mean    = TRUE,
        legend_title = "Treatment"
      )

      # Explicitly apply theme_phuse() — the KEY differentiator for
      # Figure 7.5. Uses phuse_sizes constants for standardized
      # PhUSEboxplot GTL template dimensions and styling.
      box_plot <- box_plot +
        theme_phuse(
          design_width  = phuse_sizes$design_width_mm,
          design_height = phuse_sizes$design_height_mm
        ) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(
            size  = 8,
            color = phuse_colors$box_outline,
            hjust = 0
          ),
          panel.grid.major.x = ggplot2::element_line(
            color    = "grey90",
            linewidth = 0.3
          )
        ) +
        ggplot2::labs(
          caption = paste0(
            "Source: ", ds_name,
            " | Population: ", p_fl, "='Y'",
            " | Analysis: ", a_fl, "='Y'",
            "\nPhUSEboxplot GTL Template Variant (theme_phuse)"
          )
        )

      # -----------------------------------------------------------
      # 9f-ii. Create summary statistics table below plot
      # -----------------------------------------------------------
      # Primary method: use the phuse_boxplot_stats_table() utility
      # which internally computes stats with round_half_up().
      stats_df <- phuse_boxplot_stats_table(
        data      = page_data,
        x_var     = "AVISITN",
        y_var     = m_var,
        group_var = t_var,
        stats     = c("n", "mean", "median"),
        digits    = 1
      )

      # phuse_boxplot_stats_table() returns a data.frame/tibble, not a
      # grob.  Convert to a tableGrob for gridExtra::arrangeGrob().
      stats_grob <- gridExtra::tableGrob(
        stats_df,
        rows  = NULL,
        theme = gridExtra::ttheme_minimal(
          core    = list(
            fg_params = list(fontsize = 8, col = phuse_colors$box_outline)
          ),
          colhead = list(
            fg_params = list(fontsize = 8, fontface = "bold",
                             col = phuse_colors$box_outline)
          )
        )
      )

      # -----------------------------------------------------------
      # 9f-iii. Create wide-format stats for annotation (N per trt)
      # -----------------------------------------------------------
      # Pivot descriptive statistics to wide format for a compact
      # per-treatment-arm N row beneath the boxplot.
      page_stats <- desc_stats %>%
        dplyr::filter(AVISIT %in% page_visits)

      n_wide <- page_stats %>%
        dplyr::select(AVISIT, dplyr::all_of(t_var), n_obs) %>%
        tidyr::pivot_wider(
          id_cols     = AVISIT,
          names_from  = dplyr::all_of(t_var),
          values_from = n_obs,
          names_prefix = "N_"
        )

      # Create a formatted tableGrob for N-per-treatment-arm
      n_tbl <- tryCatch(
        gridExtra::tableGrob(
          n_wide,
          rows  = NULL,
          theme = gridExtra::ttheme_minimal(
            core    = list(
              fg_params = list(fontsize = 8, col = phuse_colors$box_outline)
            ),
            colhead = list(
              fg_params = list(fontsize = 8, fontface = "bold",
                               col = phuse_colors$box_outline)
            )
          )
        ),
        error = function(e) NULL
      )

      # -----------------------------------------------------------
      # 9f-iv. Combine boxplot and statistics table(s)
      # -----------------------------------------------------------
      if (!is.null(n_tbl)) {
        combined <- gridExtra::arrangeGrob(
          box_plot,
          stats_grob,
          n_tbl,
          ncol    = 1,
          heights = c(5, 1.2, 0.8)
        )
      } else {
        combined <- gridExtra::arrangeGrob(
          box_plot,
          stats_grob,
          ncol    = 1,
          heights = c(4, 1)
        )
      }

      # -----------------------------------------------------------
      # 9f-v. Save to PDF
      # -----------------------------------------------------------
      out_file <- file.path(
        output_path,
        paste0(
          "WPCT-F-07-05_", paramcd_val,
          if (n_pages > 1) paste0("_p", pg) else "",
          ".pdf"
        )
      )

      # Render to interactive device when available (uses grid.arrange
      # for on-screen display; ggsave captures the grob to PDF below).
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
        height   = phuse_sizes$design_height_mm + 50,
        units    = "mm",
        device   = "pdf"
      )

      output_files <- c(output_files, out_file)
      message("  Created: ", out_file)
    }

    # -----------------------------------------------------------------
    # 9g. Optional: multi-visit faceted overview for current parameter
    # -----------------------------------------------------------------
    # When more than one page exists, generate a compact overview that
    # facets all visits onto a single figure using ggplot2::facet_wrap().
    # This supplements the per-page detail plots above.
    if (n_pages > 1) {
      overview_data <- param_data %>%
        dplyr::mutate(
          AVISIT = forcats::fct_reorder(AVISIT, AVISITN, .fun = min,
                                        na.rm = TRUE)
        )

      overview_plot <- ggplot2::ggplot(
        overview_data,
        ggplot2::aes(
          x    = .data[[t_var]],
          y    = .data[[m_var]],
          fill = .data[[t_var]]
        )
      ) +
        ggplot2::geom_boxplot(
          color    = phuse_colors$box_outline,
          fill     = phuse_colors$box_fill,
          outlier.shape = 15,
          outlier.size  = phuse_sizes$iqr_size,
          notch    = TRUE,
          width    = phuse_sizes$cluster_width
        ) +
        ggplot2::stat_summary(
          fun    = mean,
          geom   = "point",
          shape  = 18,
          size   = phuse_sizes$mean_size,
          color  = phuse_colors$mean_marker,
          position = ggplot2::position_dodge(width = 0.75)
        ) +
        ggplot2::facet_wrap(
          ~ AVISIT,
          scales = "free_x",
          ncol   = 4
        ) +
        ggplot2::scale_y_continuous(
          breaks = axis_breaks,
          limits = c(y_min_val, y_max_val)
        ) +
        ggplot2::scale_x_discrete(drop = TRUE) +
        ggplot2::labs(
          title   = paste0(
            "Figure 7.5 Overview  ", param_label,
            "  (All Visits)"
          ),
          y       = param_label,
          x       = "Treatment",
          caption = paste0(
            "Source: ", ds_name,
            " | PhUSEboxplot GTL Template Variant"
          )
        ) +
        theme_phuse(
          design_width  = phuse_sizes$design_width_mm,
          design_height = phuse_sizes$design_height_mm
        )

      # Add reference lines to overview if available
      if (!is.null(ref_vals) && length(ref_vals) > 0) {
        overview_plot <- overview_plot +
          ggplot2::geom_hline(
            yintercept = ref_vals,
            color      = phuse_colors$ref_line,
            linetype   = "dashed",
            linewidth  = 0.5
          )
      }

      # Add NR outlier points to overview
      if (has_ref_vars) {
        overview_plot <- overview_plot +
          ggplot2::geom_point(
            data    = overview_data %>%
              dplyr::filter(!is.na(OUTLIER)),
            ggplot2::aes(
              x = .data[[t_var]],
              y = OUTLIER
            ),
            color   = phuse_colors$nr_outlier,
            shape   = 16,
            size    = phuse_sizes$nr_outlier_size,
            inherit.aes = FALSE
          )
      }

      overview_file <- file.path(
        output_path,
        paste0("WPCT-F-07-05_", paramcd_val, "_overview.pdf")
      )

      ggplot2::ggsave(
        filename = overview_file,
        plot     = overview_plot,
        width    = phuse_sizes$design_width_mm,
        height   = phuse_sizes$design_height_mm + 30,
        units    = "mm",
        device   = "pdf"
      )

      output_files <- c(output_files, overview_file)
      message("  Created overview: ", overview_file)
    }
  }

  # -----------------------------------------------------------------------
  # 10. Summary and return
  # -----------------------------------------------------------------------
  message(
    "\nFigure 7.5 generation complete. ",
    length(output_files), " file(s) created."
  )
  invisible(output_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#    1. No WPCT-F.07.05.sas source exists in the repository. The SAS
#       file numbering jumps from WPCT-F.07.03.sas to WPCT-F.07.06.sas.
#    2. Visual specification at whitepapers/images/wpct/target_07.05.png
#       and target_07.05_full.png shows a scatter plot (Baseline vs.
#       Postbaseline Measure), but the schema specification and AAP
#       section 0.5.1 define this as a boxplot using PhUSEboxplot GTL
#       Template styling. This implementation follows the schema.
#    3. The primary differentiator for Figure 7.5 versus Figures 7.1-7.4
#       is the explicit application of theme_phuse() from
#       util_ggplot_theme.R, which replicates the SAS
#       %util_proc_template(phuseboxplot) GTL registration.
#    4. Default function parameters match conventions established by
#       WPCT-F.07.01.sas (t_var=TRTP, m_var=AVAL, p_fl=SAFFL, etc.).
#    5. Treatment ordering uses TRTPN (numeric) to replicate SAS
#       FORMAT-based sort order via forcats::fct_reorder().
#    6. The multi-visit faceted overview page (section 9g) is generated
#       only when pagination produces multiple pages. This supplementary
#       view does not add statistical functionality; it presents the
#       same data in a compact layout for review convenience.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Cannot compare against SAS output since no SAS source exists.
#    2. Rounding uses janitor::round_half_up() throughout (via
#       phuse_boxplot_stats_table and explicit desc_stats computation)
#       to match SAS round-half-up behavior. R default round() uses
#       half-to-even (banker's rounding).
#    3. Boxplot whisker computation: ggplot2 defaults to 1.5 * IQR,
#       matching the SAS PhUSEboxplot GTL template whisker behavior.
#    4. Sort stability within treatment groups is ensured via
#       dplyr::arrange() applied before forcats factor assignment.
#    5. Notch computation: ggplot2 notch = +/- 1.58 * IQR / sqrt(n).
#       SAS PhUSEboxplot template uses the same formula.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS GTL PROC TEMPLATE PhUSEboxplot registration is approximated
#       by ggplot2 theme_phuse() + phuse_boxplot() constructor. The R
#       implementation replicates visual styling (fonts, colors, sizing,
#       spacing) but uses ggplot2 rendering internals, not GTL.
#    2. SAS PROC SGRENDER dynamic variables (_TITLE, _XVAR, _YVAR,
#       _MARKERS, _BLOCKLABEL, _YOUTLIERS, _REFLINES, _YLABEL,
#       _YMIN/_YMAX/_YINCR, stat columns) are mapped to ggplot2 aes(),
#       labs(), scale_*(), and geom_*() parameters.
#    3. SAS ODS PDF destination output is replaced by ggplot2::ggsave()
#       with device = "pdf".
#
# PACKAGE SELECTION RATIONALE:
#    haven     — SAS XPT transport file reader (per AAP section 0.6.1)
#    dplyr     — Tidyverse data manipulation (per AAP section 0.8.1)
#    tidyr     — Reshaping for wide-format statistics display
#    ggplot2   — PhUSEboxplot GTL template via theme_phuse() + geom_boxplot
#    gridExtra — Combines boxplot + stats table (replaces PROC SGRENDER LAYOUT)
#    janitor   — SAS-compatible round_half_up() (per AAP section 0.7.2)
#    forcats   — Factor ordering for treatment/visit (per AAP section 0.7.1)
#    yaml      — Configuration loading (per AAP section 0.8.1)
#    rlang     — Tidy evaluation for programmatic column references
#
# OPEN QUESTIONS:
#    1. The visual specification images (target_07.05.png, _full.png)
#       appear to show a scatter plot (Baseline vs. Postbaseline),
#       not a boxplot. Review needed to confirm whether Figure 7.5
#       should be a boxplot or scatter plot in the final deliverable.
#    2. No SAS source exists for validation comparison (Gate 1
#       functional parity cannot be assessed for this figure).
#    3. Requires statistician review to confirm boxplot specification
#       and PhUSEboxplot template styling requirements for Figure 7.5.
#    4. Default dataset (advs.xpt) may need adjustment per study
#       protocol; config/migration_config.yaml provides overrides.
#
# ============================================================
