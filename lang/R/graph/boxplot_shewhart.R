# ============================================================
# File: lang/R/graph/boxplot_shewhart.R
# Migrated from: lang/SAS/graph/boxplot/src/BoxplotShewhart_Vst.sas
# Purpose: Shewhart boxplot pipeline with summary statistics --
#          visit-level lab measures and change-from-baseline
#          analysis with ANCOVA p-values
# Migration scope: Full SAS-to-R migration per PhUSE WG5 standards
# ============================================================
# Original SAS titles:
#   "Boxchart with Summary Statistics - Laboratory Analysis"
#   "Change from Baseline - Lab Test 1"
# ============================================================

# --- Package Dependencies ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)
library(janitor)
library(tibble)
library(stringr)
library(purrr)
library(grid)
# Note: stats, grDevices are base R and loaded by default

#' Generate Shewhart-style Boxplots with Summary Statistics
#'
#' Produces visit-level laboratory measure boxplots and change-from-baseline
#' boxplots with integrated summary statistics tables, reference lines,
#' outlier annotations, mean symbol markers, and ANCOVA p-values.
#' Migrated from SAS PROC SHEWHART boxchart implementation.
#'
#' @param output_path Character or NULL. Directory for saving PDF output.
#'   NULL (default) returns plots without file output.
#' @param output_format Character. Output format, default "pdf".
#' @param seed Integer. Random seed for data simulation.
#' @param lower_limit Numeric. Lower normal reference limit (default 5.5).
#' @param upper_limit Numeric. Upper normal reference limit (default 9.0).
#' @param measure_label Character. Y-axis label for visit measure plot.
#' @param change_label Character. Y-axis label for change plot.
#' @param title_visit Character. Title for visit boxplot.
#' @param title_change Character. Title for change boxplot.
#' @return Named list (invisible) with visit_with_ref, visit_no_ref, change.
generate_shewhart_boxplots <- function(
    output_path    = NULL,
    output_format  = "pdf",
    seed           = 234111,
    lower_limit    = 5.5,
    upper_limit    = 9.0,
    measure_label  = "xxx Measures (Unit)",
    change_label   = "xxx Change from Baseline (Unit)",
    title_visit    = "Box Plot - xxx Measures Over Time (Weeks Since Randomized)",
    title_change   = "Box Plot - xxx Change from Baseline Over Time (Weeks Since Randomized)"
) {

  # ------------------------------------------------------------------
  # Input Validation
  # ------------------------------------------------------------------
  if (!is.null(output_path)) {
    if (!is.character(output_path) || length(output_path) != 1L) {
      stop("output_path must be a single character string or NULL", call. = FALSE)
    }
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(output_path)) {
        stop("Unable to create output directory: ", output_path, call. = FALSE)
      }
    }
  }
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("seed must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(lower_limit) || !is.numeric(upper_limit)) {
    stop("lower_limit and upper_limit must be numeric", call. = FALSE)
  }
  if (lower_limit >= upper_limit) {
    stop("lower_limit must be less than upper_limit", call. = FALSE)
  }

  # ==================================================================
  # SECTION 1: Data Simulation
  # (SAS DATA step lines 11-34 — CARDS/DO loops with rannor PRNG)
  # SAS rannor(234111) uses a different PRNG than R rnorm(); exact
  # values will differ. CARDS block has 7 lines (wks 0-6); SAS DO
  # loop 0 to 7 attempts 8 reads but 8th hits end-of-cards.
  # ==================================================================
  set.seed(seed)

  # CARDS data: n, mean, std for each week (SAS lines 25-32)
  cards <- tibble::tibble(
    wks       = 0L:6L,
    n         = rep(80L, 7L),
    card_mean = c(7.3, 7.1, 6.7, 7.0, 7.6, 7.3, 6.7),
    card_std  = c(0.55, 0.65, 0.45, 0.50, 0.70, 0.55, 0.55)
  )

  # Build all (wks, trt) combinations using tidyr::expand_grid
  combos <- tidyr::expand_grid(wks = 0L:6L, trt = 0L:1L) %>%
    dplyr::left_join(cards, by = "wks") %>%
    dplyr::mutate(
      multi = wks + 3L * trt * as.integer(
        janitor::round_half_up(wks / 3)
      ),
      n_obs = n - multi
    ) %>%
    dplyr::filter(n_obs > 0L)

  # Generate simulated lab observations per (wks, trt) group
  # SAS: do i=1 to n-multi; id=i+(trt+1)*1000; lab_test_1=mean+std*rannor
  one <- purrr::pmap_dfr(combos, function(wks, trt, n, card_mean,
                                           card_std, multi, n_obs) {
    tibble::tibble(
      id         = seq_len(n_obs) + (trt + 1L) * 1000L,
      wks        = rep(wks, n_obs),
      trt        = rep(trt, n_obs),
      lab_test_1 = card_mean + card_std * stats::rnorm(n_obs)
    )
  })

  # SAS: label lab_test_1 = 'Measure (unit)'
  attr(one$lab_test_1, "label") <- "Measure (unit)"

  # ==================================================================
  # SECTION 2: Baseline Derivation
  # (SAS lines 36-47: PROC SORT by id wks + RETAIN bse)
  # if first.id: bse = lab_test_1 when wks=0, else NA => delete
  # ==================================================================
  new <- one %>%
    dplyr::arrange(id, wks) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      bse = dplyr::if_else(wks[1L] == 0L, lab_test_1[1L], NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(bse))

  # ==================================================================
  # SECTION 3: Cross-Tab Frequencies for Visit Plot
  # (SAS lines 49-61: PROC FREQ trt*wks; ttpoint = 1+wks+trt/3)
  # ttpoint offsets trt=0 and trt=1 so boxes don't overlap
  # ==================================================================
  frq <- new %>%
    dplyr::count(wks, trt, name = "freq") %>%
    dplyr::arrange(wks, trt) %>%
    dplyr::mutate(ttpoint = 1 + wks + trt / 3)

  # ==================================================================
  # SECTION 4: Merge + Summary Statistics
  # (SAS lines 69-82: merge new frq; PROC MEANS 8 stats)
  # ==================================================================
  two <- new %>%
    dplyr::left_join(frq, by = c("wks", "trt"))

  visit_stats <- two %>%
    dplyr::group_by(wks, trt, ttpoint) %>%
    dplyr::summarise(
      n     = dplyr::n(),
      mean  = mean(lab_test_1, na.rm = TRUE),
      std   = stats::sd(lab_test_1, na.rm = TRUE),
      med   = stats::median(lab_test_1, na.rm = TRUE),
      min_v = min(lab_test_1, na.rm = TRUE),
      max_v = max(lab_test_1, na.rm = TRUE),
      q1    = stats::quantile(lab_test_1, 0.25, na.rm = TRUE),
      q3    = stats::quantile(lab_test_1, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # ==================================================================
  # SECTION 5: Block Variables — Formatted Summary Stats
  # (SAS lines 84-131: blockraw + labelraw)
  # janitor::round_half_up at every rounding location per AAP §0.7.2
  # ==================================================================
  blockraw <- visit_stats %>%
    dplyr::arrange(wks, trt) %>%
    dplyr::mutate(
      Treatment = dplyr::if_else(trt == 0L, " A", " B"),
      block1 = Treatment,
      block2 = stringr::str_pad(as.character(n), width = 3, side = "left"),
      block3 = formatC(janitor::round_half_up(mean, 2),
                        format = "f", digits = 2),
      block4 = formatC(janitor::round_half_up(std, 2),
                        format = "f", digits = 2),
      block5 = formatC(janitor::round_half_up(min_v, 2),
                        format = "f", digits = 2),
      block6 = formatC(janitor::round_half_up(q1, 2),
                        format = "f", digits = 2),
      block7 = formatC(janitor::round_half_up(med, 2),
                        format = "f", digits = 2),
      block8 = formatC(janitor::round_half_up(q3, 2),
                        format = "f", digits = 2),
      block9 = formatC(janitor::round_half_up(max_v, 2),
                        format = "f", digits = 2)
    )

  # Header label row (SAS lines 118-131)
  labelraw <- tibble::tibble(
    Treatment = " A", ttpoint = 0,
    block1 = "Treatment", block2 = "N", block3 = "Mean",
    block4 = "Std", block5 = "Min", block6 = "Q1",
    block7 = "Median", block8 = "Q3", block9 = "Max"
  )

  # ==================================================================
  # SECTION 6: Outlier Annotation Data
  # (SAS lines 133-156: annoraw — values outside reference range)
  # SAS: if lab_test_1>9 or .<lab_test_1<5.5 (. < x = not missing)
  # ==================================================================
  annoraw <- two %>%
    dplyr::filter(
      !is.na(lab_test_1) &
        (lab_test_1 > upper_limit | lab_test_1 < lower_limit)
    )

  # ==================================================================
  # SECTION 7: Phase Labels for Visit Plot
  # (SAS lines 158-177: _Phase_ variable, phase boundaries)
  # ==================================================================
  vwiseplot <- two %>%
    dplyr::mutate(
      Treatment   = dplyr::if_else(trt == 0L, " A", " B"),
      phase_label = dplyr::if_else(
        wks == 0L, "Baseline", paste0("Weeks=", wks)
      )
    )

  phase_info <- vwiseplot %>%
    dplyr::group_by(phase_label, wks) %>%
    dplyr::summarise(
      min_tt = min(ttpoint, na.rm = TRUE),
      max_tt = max(ttpoint, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(wks) %>%
    dplyr::mutate(center = (min_tt + max_tt) / 2)

  # Phase boundary x-positions (vertical separators between phases)
  phase_boundaries <- numeric(0)
  if (nrow(phase_info) > 1L) {
    for (k in seq_len(nrow(phase_info) - 1L)) {
      phase_boundaries <- c(
        phase_boundaries,
        (phase_info$max_tt[k] + phase_info$min_tt[k + 1L]) / 2
      )
    }
  }

  # ==================================================================
  # SECTION 8: Helper — Build Summary Statistics Table Grob
  # Creates a gridExtra::tableGrob from formatted block statistics
  # Replaces SAS PROC SHEWHART block variable display below x-axis
  # ==================================================================
  build_summary_grob <- function(block_data, stat_names,
                                  include_pvalue = FALSE) {
    tbl_data <- block_data %>%
      dplyr::select(dplyr::starts_with("block"))

    # Trim to 9 columns (visit) or 10 (change, with P value)
    if (!include_pvalue && ncol(tbl_data) > 9L) {
      tbl_data <- tbl_data[, seq_len(9L)]
    }

    # Transpose: rows = statistics, columns = (group) pairs
    tbl_matrix <- t(as.matrix(tbl_data))
    rownames(tbl_matrix) <- stat_names
    colnames(tbl_matrix) <- NULL
    tbl_df <- as.data.frame(tbl_matrix, stringsAsFactors = FALSE)

    tt <- gridExtra::ttheme_minimal(
      core = list(
        fg_params = list(cex = 0.45, hjust = 0.5, x = 0.5),
        padding   = grid::unit(c(1.5, 1.5), "mm")
      ),
      rowhead = list(
        fg_params = list(cex = 0.45, fontface = "bold",
                         hjust = 1, x = 0.95),
        padding   = grid::unit(c(1.5, 1.5), "mm")
      )
    )

    gridExtra::tableGrob(tbl_df, theme = tt)
  }

  # ==================================================================
  # SECTION 9: Helper — Build Boxplot
  # Constructs a ggplot2 boxplot replicating SAS PROC SHEWHART
  # SAS: boxstyle=schematic, notches, block vars, annotate, symbols
  # ==================================================================
  build_boxplot <- function(plot_data, y_var, y_label, plot_title,
                            footnote_text, show_ref_lines = FALSE,
                            outlier_data = NULL,
                            phase_bounds = NULL,
                            phase_centers = NULL,
                            phase_labels_vec = NULL,
                            x_limits = c(0.5, 8.8),
                            use_facets = FALSE,
                            mean_shapes = c(" A" = 16, " B" = 8),
                            mean_colors = c(" A" = "orange", " B" = "blue"),
                            fill_colors = c(" A" = "white",
                                            " B" = "grey80")) {

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x     = .data[["ttpoint"]],
        y     = .data[[y_var]],
        group = interaction(.data[["wks_or_cate"]], .data[["trt"]]),
        fill  = .data[["Treatment"]]
      )
    ) +
      # SAS boxstyle=schematic is the ggplot2 default (1.5*IQR whiskers)
      ggplot2::geom_boxplot(
        notch    = TRUE,
        width    = 0.25,
        position = ggplot2::position_identity(),
        linewidth = 0.4
      )

    # Reference lines (SAS vREF= lower_limit upper_limit)
    if (show_ref_lines) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = c(lower_limit, upper_limit),
          color = "red", linetype = "dashed", linewidth = 0.5
        ) +
        ggplot2::annotate(
          "text", x = x_limits[1] + 0.3, y = lower_limit,
          label = "Lower Limit", color = "red",
          size = 2.5, vjust = -0.5, hjust = 0
        ) +
        ggplot2::annotate(
          "text", x = x_limits[1] + 0.3, y = upper_limit,
          label = "Upper Limit", color = "red",
          size = 2.5, vjust = -0.5, hjust = 0
        )
    }

    # Outlier annotation (SAS annotate red dots for out-of-range)
    if (!is.null(outlier_data) && nrow(outlier_data) > 0L) {
      p <- p +
        ggplot2::geom_point(
          data = outlier_data,
          ggplot2::aes(
            x = .data[["ttpoint"]], y = .data[[y_var]]
          ),
          color = "red", size = 1.5, shape = 16,
          inherit.aes = FALSE
        )
    }

    # Mean symbols (SAS symbol1 v=circle c=orange; symbol2 v=star c=blue)
    p <- p +
      ggplot2::stat_summary(
        fun = mean, geom = "point",
        ggplot2::aes(
          x     = .data[["ttpoint"]],
          y     = .data[[y_var]],
          shape = .data[["Treatment"]],
          color = .data[["Treatment"]],
          group = interaction(.data[["wks_or_cate"]], .data[["trt"]])
        ),
        size = 3, inherit.aes = FALSE
      )

    # Phase boundary vertical separators (SAS phaseref)
    if (!is.null(phase_bounds) && length(phase_bounds) > 0L) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = phase_bounds,
          linetype = "dotted", color = "grey50", linewidth = 0.3
        )
    }

    # Phase labels at top (SAS phaselegend / readphase)
    # Two rendering modes: facet_grid for panel-based separation, or
    # manual annotate for continuous x-axis with vertical separators
    if (use_facets && "phase_label" %in% names(plot_data)) {
      # facet_grid rendering — each phase becomes a separate panel
      p <- p +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data[["phase_label"]]),
          scales = "free_x", space = "free_x"
        )
    } else if (!is.null(phase_centers) && !is.null(phase_labels_vec)) {
      # Annotate rendering — continuous x-axis with phase labels above
      y_range <- range(plot_data[[y_var]], na.rm = TRUE)
      label_y <- y_range[2] + 0.08 * diff(y_range)
      p <- p +
        ggplot2::annotate(
          "text",
          x = phase_centers, y = label_y,
          label = phase_labels_vec,
          size = 2.5, fontface = "bold"
        )
    }

    # Scales and theme
    p <- p +
      ggplot2::scale_shape_manual(values = mean_shapes) +
      ggplot2::scale_color_manual(values = mean_colors) +
      ggplot2::scale_fill_manual(values = fill_colors) +
      ggplot2::coord_cartesian(xlim = x_limits, clip = "off") +
      ggplot2::ggtitle(plot_title) +
      ggplot2::labs(
        y       = y_label,
        caption = footnote_text
      ) +
      ggplot2::theme(
        axis.title.x       = ggplot2::element_blank(),
        axis.text.x         = ggplot2::element_blank(),
        axis.ticks.x        = ggplot2::element_blank(),
        plot.title           = ggplot2::element_text(
          hjust = 0.5, size = 12
        ),
        plot.caption         = ggplot2::element_text(
          hjust = 0, size = 7
        ),
        legend.position      = "bottom",
        panel.grid.major.x   = ggplot2::element_blank(),
        panel.grid.minor.x   = ggplot2::element_blank(),
        plot.margin          = ggplot2::margin(
          t = 20, r = 10, b = 5, l = 10
        )
      )

    p
  }

  # ==================================================================
  # SECTION 10: Build Visit Boxplots (SAS lines 343-418)
  # Two versions: with and without reference lines
  # ==================================================================
  visit_plot_data <- vwiseplot %>%
    dplyr::mutate(wks_or_cate = as.character(wks))

  outlier_plot_data <- annoraw %>%
    dplyr::mutate(
      Treatment   = dplyr::if_else(trt == 0L, " A", " B"),
      wks_or_cate = as.character(wks)
    )

  # Footnotes: visit with reference lines (SAS lines 346-349)
  fn_visit_ref <- paste0(
    "Box plot type=schematic, the box shows median, interquartile ",
    "range (IQR, edge of the bar), min and max\n",
    "within 1.5 IQR below 25% and above 75% (ends of the whisker). ",
    "Values outside the 1.5 IQR below 25% and\n",
    "above 75% are shown as outliers. Means plotted as different ",
    "symbols by treatments. Red dots indicate\n",
    "out of normal reference range measures."
  )

  # Footnotes: visit without reference lines (SAS lines 386-388)
  fn_visit_noref <- paste0(
    "The box shows median, interquartile range (IQR, edge of the bar)",
    ", min and max.\n",
    "Means plotted as different symbols by treatments. ",
    "Red dots indicate out of normal reference range measures."
  )

  visit_bp_ref <- build_boxplot(
    plot_data       = visit_plot_data,
    y_var           = "lab_test_1",
    y_label         = measure_label,
    plot_title      = title_visit,
    footnote_text   = fn_visit_ref,
    show_ref_lines  = TRUE,
    outlier_data    = outlier_plot_data,
    phase_bounds    = phase_boundaries,
    phase_centers   = phase_info$center,
    phase_labels_vec = phase_info$phase_label,
    x_limits        = c(0.5, 8.8)
  )

  visit_bp_noref <- build_boxplot(
    plot_data       = visit_plot_data,
    y_var           = "lab_test_1",
    y_label         = measure_label,
    plot_title      = title_visit,
    footnote_text   = fn_visit_noref,
    show_ref_lines  = FALSE,
    outlier_data    = outlier_plot_data,
    phase_bounds    = phase_boundaries,
    phase_centers   = phase_info$center,
    phase_labels_vec = phase_info$phase_label,
    x_limits        = c(0.5, 8.8)
  )

  # Summary statistics table grob for visit plots
  visit_stat_names <- c("Treatment", "N", "Mean", "Std", "Min",
                         "Q1", "Median", "Q3", "Max")
  visit_summary_grob <- build_summary_grob(
    blockraw %>% dplyr::arrange(wks, trt),
    stat_names     = visit_stat_names,
    include_pvalue = FALSE
  )

  # Create a footnote grob for visit plots (text beneath summary table)
  visit_footnote_grob <- grid::textGrob(
    label = "Summary statistics displayed per treatment arm below boxplot.",
    gp    = grid::gpar(fontsize = 7, fontface = "italic"),
    x     = grid::unit(0.02, "npc"), just = "left"
  )

  # Combine boxplot + summary table + footnote (SAS block variable display)
  visit_plot_ref_combined <- gridExtra::arrangeGrob(
    visit_bp_ref, visit_summary_grob, visit_footnote_grob,
    nrow = 3, heights = c(3, 1, 0.15)
  )
  visit_plot_noref_combined <- gridExtra::arrangeGrob(
    visit_bp_noref, visit_summary_grob, visit_footnote_grob,
    nrow = 3, heights = c(3, 1, 0.15)
  )

  # ==================================================================
  # SECTION 11: Change-from-Baseline Data Construction
  # (SAS lines 182-235: RETAIN bse, cate, endpoint rows, PROC FREQ)
  # ==================================================================
  new_chg <- one %>%
    dplyr::arrange(id, wks) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      bse  = dplyr::if_else(wks[1L] == 0L, lab_test_1[1L], NA_real_),
      cate = paste0("Wks=", wks)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(bse))

  # Add endpoint rows: last observation per subject (SAS last.id)
  endpoints <- new_chg %>%
    dplyr::group_by(id) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cate = "_Endpoint")

  new_chg <- dplyr::bind_rows(new_chg, endpoints)

  # Filter post-baseline + compute change (SAS: if wks>0; change=lab-bse)
  chg <- new_chg %>%
    dplyr::filter(wks > 0L) %>%
    dplyr::mutate(change = lab_test_1 - bse)

  # Cross-tab frequencies for change x-positions
  # SAS: sort by cate trt; ttpoint = _n_ - trt*0.3
  # Ensure _Endpoint sorts after Wks=* (SAS ASCII: 'W' < '_')
  chgfrq <- chg %>%
    dplyr::distinct(cate, trt) %>%
    dplyr::mutate(
      sort_key = dplyr::if_else(
        cate == "_Endpoint", "ZZZ_Endpoint", cate
      )
    ) %>%
    dplyr::arrange(sort_key, trt) %>%
    dplyr::mutate(ttpoint = dplyr::row_number() - trt * 0.3) %>%
    dplyr::select(cate, trt, ttpoint)

  # Merge change data with ttpoint positions
  chgdata <- chg %>%
    dplyr::left_join(chgfrq, by = c("cate", "trt"))

  # ==================================================================
  # SECTION 12: Change-from-Baseline Summary Statistics
  # (SAS PROC MEANS lines 238-242)
  # ==================================================================
  chgstats <- chgdata %>%
    dplyr::group_by(cate, trt, ttpoint) %>%
    dplyr::summarise(
      n     = dplyr::n(),
      mean  = mean(change, na.rm = TRUE),
      std   = stats::sd(change, na.rm = TRUE),
      med   = stats::median(change, na.rm = TRUE),
      min_v = min(change, na.rm = TRUE),
      max_v = max(change, na.rm = TRUE),
      q1    = stats::quantile(change, 0.25, na.rm = TRUE),
      q3    = stats::quantile(change, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # ==================================================================
  # SECTION 13: ANCOVA Model
  # (SAS PROC MIXED lines 245-261: model change = trt bse)
  # PROC MIXED with no RANDOM statement = OLS with Type III SS
  # => stats::lm() + car::Anova(type="III") per AAP §0.5.1
  # ==================================================================
  endpoint_data <- chgdata %>%
    dplyr::filter(cate == "_Endpoint")

  p_value <- NA_real_
  f_value <- NA_real_

  tryCatch({
    ancova_model <- stats::lm(
      change ~ factor(trt) + bse,
      data = endpoint_data
    )
    ancova_result <- car::Anova(ancova_model, type = "III")
    trt_row <- "factor(trt)"
    if (trt_row %in% rownames(ancova_result)) {
      p_value <- ancova_result[trt_row, "Pr(>F)"]
      f_value <- ancova_result[trt_row, "F value"]
    }
  }, error = function(e) {
    warning(
      "ANCOVA model failed: ", conditionMessage(e),
      "\nP-value will be reported as NA.", call. = FALSE
    )
  })

  # Merge p-value into change statistics (SAS lines 263-272)
  trtpval <- tibble::tibble(
    cate   = "_Endpoint",
    trt    = 1L,
    ProbF  = p_value,
    Fvalue = f_value
  )
  sumstats <- chgstats %>%
    dplyr::left_join(trtpval, by = c("cate", "trt"))

  # ==================================================================
  # SECTION 14: Change Block Variables (SAS lines 276-339)
  # 10 blocks including P value; janitor::round_half_up per §0.7.2
  # SAS: put(blck10, 4.3) => no leading zero on p-value
  # ==================================================================
  blockchg <- sumstats %>%
    dplyr::mutate(
      sort_key = dplyr::if_else(
        cate == "_Endpoint", "ZZZ_Endpoint", cate
      )
    ) %>%
    dplyr::arrange(sort_key, trt) %>%
    dplyr::mutate(
      Treatment = dplyr::if_else(trt == 0L, " A", " B"),
      block1  = Treatment,
      block2  = stringr::str_pad(as.character(n), width = 3, side = "left"),
      block3  = formatC(janitor::round_half_up(mean, 2),
                         format = "f", digits = 2),
      block4  = formatC(janitor::round_half_up(std, 2),
                         format = "f", digits = 2),
      block5  = formatC(janitor::round_half_up(min_v, 2),
                         format = "f", digits = 2),
      block6  = formatC(janitor::round_half_up(q1, 2),
                         format = "f", digits = 2),
      block7  = formatC(janitor::round_half_up(med, 2),
                         format = "f", digits = 2),
      block8  = formatC(janitor::round_half_up(q3, 2),
                         format = "f", digits = 2),
      block9  = formatC(janitor::round_half_up(max_v, 2),
                         format = "f", digits = 2),
      block10 = dplyr::if_else(
        !is.na(ProbF),
        sub("^0", "",
            formatC(janitor::round_half_up(ProbF, 3),
                    format = "f", digits = 3)),
        ""
      )
    )

  # Header label row for change summary table (SAS lines 314-328)
  labelchg <- tibble::tibble(
    Treatment = " A", ttpoint = 0,
    block1 = "Treatment", block2 = "N", block3 = "Mean",
    block4 = "Std", block5 = "Min", block6 = "Q1",
    block7 = "Median", block8 = "Q3", block9 = "Max",
    block10 = "P value"
  )

  # ==================================================================
  # SECTION 15: Change-from-Baseline Boxplot (SAS lines 421-455)
  # No reference lines (SAS vREF commented out); notches enabled
  # ==================================================================
  chg_plot_data <- chgdata %>%
    dplyr::mutate(
      Treatment   = dplyr::if_else(trt == 0L, " A", " B"),
      wks_or_cate = cate,
      phase_label = stringr::str_trim(cate)
    )

  # Phase info for change plot (cate-based grouping)
  chg_phase_info <- chg_plot_data %>%
    dplyr::group_by(phase_label, cate) %>%
    dplyr::summarise(
      min_tt = min(ttpoint, na.rm = TRUE),
      max_tt = max(ttpoint, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sort_key = dplyr::if_else(
        cate == "_Endpoint", "ZZZ_Endpoint", cate
      )
    ) %>%
    dplyr::arrange(sort_key) %>%
    dplyr::mutate(center = (min_tt + max_tt) / 2)

  chg_phase_bounds <- numeric(0)
  if (nrow(chg_phase_info) > 1L) {
    for (k in seq_len(nrow(chg_phase_info) - 1L)) {
      chg_phase_bounds <- c(
        chg_phase_bounds,
        (chg_phase_info$max_tt[k] +
           chg_phase_info$min_tt[k + 1L]) / 2
      )
    }
  }

  # Footnotes for change plot (SAS lines 422-427)
  fn_change <- paste0(
    "wks=visit weeks; end=last postbaseline measure; ",
    "Box plot type=schematic; The box shows median, IQR (edge of the\n",
    "bar), min and max within 1.5 IQR below 25% and above 75% ",
    "(ends of the whisker). Values outside the 1.5 IQR\n",
    "are shown as outliers. Means plotted as different symbols by ",
    "treatments.\n",
    "P value is for the treatment comparison from ANCOVA model ",
    "Change=Baseline+Treatment"
  )

  chg_x_max <- max(chgdata$ttpoint, na.rm = TRUE) + 1.0
  chg_x_limits <- c(0, chg_x_max)

  change_bp <- build_boxplot(
    plot_data        = chg_plot_data,
    y_var            = "change",
    y_label          = change_label,
    plot_title       = title_change,
    footnote_text    = fn_change,
    show_ref_lines   = FALSE,
    outlier_data     = NULL,
    phase_bounds     = chg_phase_bounds,
    phase_centers    = chg_phase_info$center,
    phase_labels_vec = chg_phase_info$phase_label,
    x_limits         = chg_x_limits
  )

  # Change summary table grob (10 columns with P value)
  chg_stat_names <- c("Treatment", "N", "Mean", "Std", "Min",
                       "Q1", "Median", "Q3", "Max", "P value")
  chg_summary_grob <- build_summary_grob(
    blockchg %>%
      dplyr::mutate(
        sort_key = dplyr::if_else(
          cate == "_Endpoint", "ZZZ_Endpoint", cate
        )
      ) %>%
      dplyr::arrange(sort_key, trt),
    stat_names     = chg_stat_names,
    include_pvalue = TRUE
  )

  change_plot_combined <- gridExtra::arrangeGrob(
    change_bp, chg_summary_grob,
    nrow = 2, heights = c(3, 1)
  )

  # ==================================================================
  # SECTION 16: Output Generation
  # (SAS ODS PDF x3 → R pdf() device, landscape 11x8.5)
  # ==================================================================
  if (!is.null(output_path)) {
    pdf_file <- file.path(output_path, "boxplot_shewhart.pdf")
    grDevices::pdf(
      file  = pdf_file,
      width = 11, height = 8.5,
      paper = "special"
    )
    on.exit(grDevices::dev.off(), add = TRUE)

    # Page 1: Visit boxplot WITH reference lines
    gridExtra::grid.arrange(
      visit_bp_ref, visit_summary_grob,
      nrow = 2, heights = c(3, 1)
    )

    # Page 2: Visit boxplot WITHOUT reference lines
    gridExtra::grid.arrange(
      visit_bp_noref, visit_summary_grob,
      nrow = 2, heights = c(3, 1)
    )

    # Page 3: Change-from-baseline boxplot
    gridExtra::grid.arrange(
      change_bp, chg_summary_grob,
      nrow = 2, heights = c(3, 1)
    )

    grDevices::dev.off()
    on.exit(NULL)
    message("PDF output saved to: ", pdf_file)

    # Also save individual PNG files via ggsave for standalone use
    png_ref <- file.path(output_path, "boxplot_visit_ref.png")
    ggplot2::ggsave(
      filename = png_ref, plot = visit_bp_ref,
      width = 11, height = 6.5, dpi = 150
    )

    png_change <- file.path(output_path, "boxplot_change.png")
    ggplot2::ggsave(
      filename = png_change, plot = change_bp,
      width = 11, height = 6.5, dpi = 150
    )
  }

  # ==================================================================
  # SECTION 17: Return Value
  # ==================================================================
  invisible(list(
    visit_with_ref = visit_plot_ref_combined,
    visit_no_ref   = visit_plot_noref_combined,
    change         = change_plot_combined
  ))
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    1. SAS rannor(234111) PRNG differs from R rnorm() with
####       set.seed(234111). Simulated data values will differ between
####       SAS and R; functional parity is validated by structure/logic
####       equivalence, not exact value matching.
####    2. SAS CARDS block has 7 lines (wks 0-6), but DO loop is
####       0 to 7 (8 iterations). The 8th iteration hits end-of-cards.
####       Implemented as wks 0-6 (7 weeks).
####    3. SAS PROC MEANS quantile algorithm (definition 5) may differ
####       from R quantile(type=7). Document any numerical differences
####       in Gate 2 rounding audit.
####    4. PROC MIXED with no RANDOM statement is equivalent to OLS;
####       replaced with stats::lm() + car::Anova(type="III").
####    5. SAS options ps=60 ls=80 nodate and goptions ftext=none
####       htext=1 cell have no direct R equivalent; omitted.
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. Random number generation: SAS rannor(234111) vs R rnorm()
####       with set.seed(234111) produce different sequences. All
####       downstream statistics will differ in value but structural
####       equivalence is preserved.
####    2. Quantile computation: SAS PROC MEANS uses percentile
####       definition 5 (empirical distribution with averaging);
####       R quantile() default is type 7. Use type=2 for closer
####       SAS match if required.
####    3. Rounding: All formatted values use janitor::round_half_up()
####       to match SAS round-half-up behavior per AAP section 0.7.2.
####    4. ANCOVA p-values: Minor floating-point differences possible
####       between PROC MIXED and car::Anova() due to different
####       computational backends.
####    5. SAS round(wks/3) uses round-half-up; R uses
####       janitor::round_half_up(wks/3) for consistency (no practical
####       difference for these values since none are at exactly .5).
#### NO DIRECT R EQUIVALENT:
####    1. PROC SHEWHART boxchart with block variables — replaced with
####       ggplot2 boxplot + gridExtra::tableGrob summary table arranged
####       below plot via gridExtra::arrangeGrob().
####    2. SAS annotate facility (FUNCTION='symbol', XSYS='2') for
####       overlaying red dots — replaced with ggplot2::geom_point()
####       layer with filtered outlier data.
####    3. SAS phase labels (readphase/phaseref/phaselegend) — replaced
####       with ggplot2::geom_vline() separators and annotate() text.
####    4. ODS PDF inline multi-plot output — replaced with
####       grDevices::pdf() device for multi-page landscape output.
####    5. SAS goptions/symbol statements — replaced with ggplot2
####       scale_shape_manual() and scale_color_manual().
####    6. SAS format 4.3 for p-value (no leading zero) — replicated
####       via sub("^0", "", formatC(...)) in R.
#### PACKAGE SELECTION RATIONALE:
####    1. ggplot2: Standard tidyverse visualization replacing PROC
####       SHEWHART/SGPLOT per AAP section 0.8.1.
####    2. dplyr: Data manipulation replacing DATA steps, PROC SORT,
####       MERGE per AAP section 0.8.1 tidyverse-first rule.
####    3. car: Type III ANOVA for ANCOVA replacing PROC MIXED in OLS
####       mode (no RANDOM statement) per AAP section 0.5.1.
####    4. janitor: round_half_up() for SAS-compatible rounding per
####       AAP section 0.7.2.
####    5. gridExtra: Multi-panel layout replacing PROC SHEWHART block
####       variable display with tableGrob + grid.arrange.
####    6. tidyr: expand_grid() for generating (wks, trt) combinations
####       replacing nested SAS DO loops.
####    7. purrr: Functional iteration replacing SAS DO/END loops.
####    8. stringr: String manipulation replacing SAS COMPRESS/STRIP
####       per tidyverse-first rule.
#### OPEN QUESTIONS:
####    1. Should R quantile type match SAS definition 5 exactly?
####       Current implementation uses R default type 7 — switch to
####       type 2 if exact match needed.
####    2. Confirm whether simulated data seed matters for validation
####       (functional structure test vs exact value reproduction).
####    3. SAS PROC MIXED vs R lm(): Verify df method alignment
####       (SAS containment vs R residual).
####    4. Annotation symbol sizes (SAS size=0.5, h=0.75) — visual
####       calibration needed for ggplot2 point sizes.
####    5. SAS boxwidth=2 and boxwidthscale=0 map to ggplot2 width
####       parameter — current value of 0.25 (data units) provides
####       similar visual appearance.
# ============================================================
