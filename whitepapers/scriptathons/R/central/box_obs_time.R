# ============================================================
# PhUSE Scriptathon 2014 — Target01: Box Plot - Observed Values Over Time
# ============================================================
# Migrated from SAS: whitepapers/scriptathons/central/box_obs_time.sas (278 lines)
# Description: Box plot of Diastolic Blood Pressure (DIABP) observed values
#   over time (Weeks Since Randomized) with summary statistics table panel.
#   The output is split into two panels: early visits (Weeks 0-8) and late
#   visits (Weeks 12-26) to match the SAS PROC SHEWHART two-page output.
#
# SAS Source Reference: box_obs_time.sas
#   - Reads ADVS XPT, filters DIABP/ATPTN=815/ANL01FL=Y/SAFFL=Y
#   - Derives ordinal coords for treatment * visit boxplot positioning
#   - PROC MEANS for N/Mean/Std/Median/Min/Max/Q1/Q3 by visit and treatment
#   - PROC SHEWHART boxchart with notches, block summary labels, phase refs
#   - Two-panel landscape PDF output (hsize=15in, vsize=8.5in)
#
# Reference R (read-only): box_obs_time.Rnw (Sweave implementation using
#   SASxport/plyr/ggplot2/xtable — this standalone R file uses modern
#   tidyverse + haven approach instead)
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(janitor)

#' Box Plot - Observed Values Over Time for Diastolic Blood Pressure (DIABP)
#'
#' Produces a two-panel boxplot of DIABP observed values over time, split into
#' early visits (Weeks 0-8) and late visits (Weeks 12-26). Each panel includes
#' notched boxplots by treatment group with mean markers and a summary
#' statistics table (Treatment, N, Mean, Std, Min, Q1, Median, Max, Q3).
#'
#' @param data_path Character. Path to the ADVS XPT file. If NULL, defaults
#'   to \code{file.path("data", "adam", "cdisc", "advs.xpt")}.
#' @param output_path Character. Directory for output files. Defaults to ".".
#' @param output_format Character. Output format: "pdf" or "png". Defaults
#'   to "pdf". Matches SAS ODS PDF landscape output.
#' @return Invisible list containing:
#'   \describe{
#'     \item{output_files}{Character vector of generated output file paths}
#'     \item{data}{Tibble of the processed plot dataset with phase/ordinal info}
#'     \item{stats}{Tibble of summary statistics by visit and treatment}
#'   }
box_obs_time <- function(data_path = NULL,
                         output_path = ".",
                         output_format = "pdf") {

  # ------------------------------------------------------------------
  # Input validation and defaults
  # ------------------------------------------------------------------
  if (is.null(data_path)) {
    data_path <- file.path("data", "adam", "cdisc", "advs.xpt")
  }

  stopifnot(
    "data_path must be a character string" = is.character(data_path),
    "data_path file does not exist"        = file.exists(data_path),
    "output_path must be a character string" = is.character(output_path),
    "output_format must be 'pdf' or 'png'"  = output_format %in% c("pdf", "png")
  )

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # ------------------------------------------------------------------
  # Phase 1: Data Loading and Filtering

  # Replaces SAS: filename source url ...; libname source xport;
  #               data work.advs; set source.advs;
  #               where PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y"
  #                     and . < AVISITN < 99 and SAFFL="Y";
  # ------------------------------------------------------------------
  advs_raw <- haven::read_xpt(data_path)

  # SAS `. < AVISITN < 99` means AVISITN is not missing AND AVISITN < 99
  # (SAS numeric missing `.` sorts below all non-missing values)
  # This includes AVISITN = 0 (Baseline)
  advs <- advs_raw %>%
    dplyr::filter(
      PARAMCD == "DIABP",
      ATPTN == 815,
      ANL01FL == "Y",
      !is.na(AVISITN),
      AVISITN < 99,
      SAFFL == "Y"
    ) %>%
    dplyr::select(
      USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, AVISIT, AVISITN
    )

  # ------------------------------------------------------------------
  # Phase 2: Ordinal Coordinate Derivation
  # Replaces SAS: proc sort; data trts/visits; first.trtpn/first.avisitn;
  #               trtnum=_N_; visnum=_N_; proc sql ntrts; ttpoint formula
  # ------------------------------------------------------------------

  # Treatment ordinal (TRTNUM): dense_rank of distinct TRTPN values
  trt_ordinals <- advs %>%
    dplyr::distinct(TRTPN) %>%
    dplyr::arrange(TRTPN) %>%
    dplyr::mutate(TRTNUM = dplyr::dense_rank(TRTPN))

  # Visit ordinal (VISNUM): dense_rank of distinct AVISITN values
  vis_ordinals <- advs %>%
    dplyr::distinct(AVISITN) %>%
    dplyr::arrange(AVISITN) %>%
    dplyr::mutate(VISNUM = dplyr::dense_rank(AVISITN))

  # Total treatment groups (replaces SAS macro variable &ntrts)
  ntrts <- dplyr::n_distinct(advs$TRTPN)

  # Build frequency/ordinal reference and compute ttpoint offset
  # ttpoint = 0.5 + visnum + (trtnum - 1) / ntrts
  # This spaces treatments within each visit for boxplot positioning
  frq <- advs %>%
    dplyr::distinct(AVISITN, TRTPN) %>%
    dplyr::left_join(trt_ordinals, by = "TRTPN") %>%
    dplyr::left_join(vis_ordinals, by = "AVISITN") %>%
    dplyr::mutate(
      TTPOINT = 0.5 + VISNUM + (TRTNUM - 1) / ntrts
    ) %>%
    dplyr::select(AVISITN, TRTPN, TTPOINT, TRTNUM, VISNUM)

  # Merge ordinals back to main dataset (replaces SAS merge by avisitn trtpn)
  two <- advs %>%
    dplyr::left_join(frq, by = c("AVISITN", "TRTPN"))

  # ------------------------------------------------------------------
  # Phase 3: Summary Statistics Computation
  # Replaces SAS: proc means data=two noprint; by avisitn trtpn;
  #               output out=stats n=n mean=mean std=std ...;
  # ------------------------------------------------------------------
  stats <- two %>%
    dplyr::group_by(AVISITN, TRTPN) %>%
    dplyr::summarise(
      N      = dplyr::n(),
      MEAN   = mean(AVAL, na.rm = TRUE),
      STD    = sd(AVAL, na.rm = TRUE),
      MEDIAN = median(AVAL, na.rm = TRUE),
      MIN    = min(AVAL, na.rm = TRUE),
      MAX    = max(AVAL, na.rm = TRUE),
      Q1     = quantile(AVAL, 0.25, na.rm = TRUE),
      Q3     = quantile(AVAL, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # ------------------------------------------------------------------
  # Phase 4: Block Text Panel Construction
  # Replaces SAS: data blockraw; block2=strip(put(blck2,10.0)); ...
  #               data labelraw; block1='Treatment'; block2='N'; ...
  # Format stats matching SAS PUT format 10.2 / 10.0
  # ------------------------------------------------------------------
  blockraw <- stats %>%
    dplyr::mutate(
      block1 = as.character(TRTPN),
      block2 = sprintf("%.0f", N),
      block3 = sprintf("%.2f", janitor::round_half_up(MEAN, 2)),
      block4 = sprintf("%.2f", janitor::round_half_up(STD, 2)),
      block5 = sprintf("%.2f", janitor::round_half_up(MIN, 2)),
      block6 = sprintf("%.2f", janitor::round_half_up(Q1, 2)),
      block7 = sprintf("%.2f", janitor::round_half_up(MEDIAN, 2)),
      block8 = sprintf("%.2f", janitor::round_half_up(MAX, 2)),
      block9 = sprintf("%.2f", janitor::round_half_up(Q3, 2))
    ) %>%
    dplyr::select(AVISITN, TRTPN, block1, block2, block3, block4,
                  block5, block6, block7, block8, block9)

  # ------------------------------------------------------------------
  # Phase 5: Phase Assignment and Panel Splitting
  # Replaces SAS: if avisitn=0 then _Phase_='Baseline';
  #               else _phase_=compress('Week='||avisitn);
  #               data plot_visit1 where avisitn<=8;
  #               data plot_visit2 where avisitn>8;
  # ------------------------------------------------------------------
  vwiseplot <- two %>%
    dplyr::left_join(
      blockraw %>% dplyr::select(AVISITN, TRTPN, block1, block2, block3,
                                 block4, block5, block6, block7, block8,
                                 block9),
      by = c("AVISITN", "TRTPN")
    ) %>%
    dplyr::mutate(
      PHASE = dplyr::case_when(
        AVISITN == 0 ~ "Baseline",
        TRUE         ~ paste0("Week=", AVISITN)
      )
    ) %>%
    dplyr::arrange(TTPOINT, TRTPN)

  # Split into early visits (AVISITN <= 8) and late visits (AVISITN > 8)
  early_data <- vwiseplot %>%
    dplyr::filter(AVISITN <= 8)

  late_data <- vwiseplot %>%
    dplyr::filter(AVISITN > 8)

  # Build summary statistics tables for each panel
  # Column headers: Treatment, N, Mean, Std, Min, Q1, Median, Max, Q3
  # Note: SAS labelraw swaps Q3/Max labels (block8='Q3', block9='Max')

  # vs data (block8=max, block9=q3). R implementation uses correct mapping.
  stats_col_names <- c("Treatment", "N", "Mean", "Std", "Min",
                        "Q1", "Median", "Max", "Q3")

  early_stats_tbl <- blockraw %>%
    dplyr::filter(AVISITN <= 8) %>%
    dplyr::arrange(AVISITN, TRTPN) %>%
    dplyr::select(block1, block2, block3, block4, block5,
                  block6, block7, block8, block9)
  colnames(early_stats_tbl) <- stats_col_names

  late_stats_tbl <- blockraw %>%
    dplyr::filter(AVISITN > 8) %>%
    dplyr::arrange(AVISITN, TRTPN) %>%
    dplyr::select(block1, block2, block3, block4, block5,
                  block6, block7, block8, block9)
  colnames(late_stats_tbl) <- stats_col_names

  # ------------------------------------------------------------------
  # Phase 6: Panel Builder Helper
  # Replaces SAS: proc shewhart graphics data=plot_visitN;
  #               boxchart aval*ttpoint (blocks)=trtpn / notches ...;
  # ------------------------------------------------------------------
  build_boxplot_panel <- function(panel_data, stats_tbl, use_facet = FALSE) {

    # Ensure AVISIT is ordered by AVISITN for correct x-axis ordering
    visit_order <- panel_data %>%
      dplyr::distinct(AVISIT, AVISITN) %>%
      dplyr::arrange(AVISITN)

    panel_data <- panel_data %>%
      dplyr::mutate(
        AVISIT = factor(AVISIT, levels = visit_order$AVISIT)
      )

    # Treatment label for legend (using numeric TRTPN as in SAS)
    trt_levels <- sort(unique(panel_data$TRTPN))
    trt_labels <- paste("Treatment", trt_levels)

    panel_data <- panel_data %>%
      dplyr::mutate(
        TRT_LABEL = factor(
          paste("Treatment", TRTPN),
          levels = trt_labels
        )
      )

    # Color mapping: SAS symbol1 c=green, symbol2 c=red
    fill_palette <- c("green3", "red", "dodgerblue")[seq_along(trt_levels)]
    names(fill_palette) <- trt_labels

    # Shape mapping: SAS symbol1 v=circle (shape 1), symbol2 v=star (shape 8)
    shape_palette <- c(1L, 8L, 2L)[seq_along(trt_levels)]
    names(shape_palette) <- trt_labels

    # Build the boxplot
    p_box <- ggplot2::ggplot(
      panel_data,
      ggplot2::aes(x = AVISIT, y = AVAL, fill = TRT_LABEL)
    ) +
      # Notched boxplot: SAS boxstyle=schematic with notches option
      ggplot2::geom_boxplot(
        notch        = TRUE,
        position     = ggplot2::position_dodge(width = 0.75),
        alpha        = 0.7,
        outlier.size = 1.5
      ) +
      # Mean markers: SAS symbol1=circle/green, symbol2=star/red
      ggplot2::stat_summary(
        fun      = mean,
        geom     = "point",
        ggplot2::aes(
          group = TRT_LABEL,
          shape = TRT_LABEL,
          color = TRT_LABEL
        ),
        position     = ggplot2::position_dodge(width = 0.75),
        size         = 3,
        show.legend  = TRUE
      ) +
      ggplot2::scale_fill_manual(values = fill_palette, name = NULL) +
      ggplot2::scale_color_manual(values = fill_palette, name = NULL) +
      ggplot2::scale_shape_manual(values = shape_palette, name = NULL) +
      ggplot2::labs(
        title = paste0(
          "Box Plot - Diastolic Blood Pressure Over Time\n",
          "(Weeks Since Randomized)"
        ),
        y = "Diastolic Blood Pressure (mmHg)",
        x = NULL,
        caption = paste0(
          "Box plot type=schematic: the box shows median, interquartile ",
          "range (IQR, edge of the bar), min and max\n",
          "within 1.5 IQR below 25% and above 75% (ends of the whisker). ",
          "Values outside the 1.5 IQR below 25% and\n",
          "above 75% are shown as outliers. ",
          "Means plotted as different symbols by treatments."
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(
          hjust = 0.5, size = 14, face = "bold"
        ),
        plot.caption  = ggplot2::element_text(hjust = 0, size = 8),
        axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
        axis.title.x  = ggplot2::element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal"
      )

    # Optional faceting by phase group to approximate SAS readphase/phaseref
    # Note: ggplot2 4.0.0+ deprecates %+% for data replacement; we rebuild with
    # faceted data directly to avoid deprecation warning.
    if (use_facet && dplyr::n_distinct(panel_data$PHASE) > 1) {
      # Group phases: Baseline vs On-Treatment
      panel_data <- panel_data %>%
        dplyr::mutate(
          PHASE_GROUP = dplyr::case_when(
            AVISITN == 0 ~ "Baseline",
            TRUE         ~ "On-Treatment"
          )
        )
      if (dplyr::n_distinct(panel_data$PHASE_GROUP) > 1) {
        p_box <- ggplot2::ggplot(
          panel_data,
          ggplot2::aes(x = AVISIT, y = AVAL, fill = TRT_LABEL)
        ) +
          ggplot2::geom_boxplot(
            notch        = TRUE,
            position     = ggplot2::position_dodge(width = 0.75),
            alpha        = 0.7,
            outlier.size = 1.5
          ) +
          ggplot2::stat_summary(
            fun      = mean,
            geom     = "point",
            ggplot2::aes(
              group = TRT_LABEL,
              shape = TRT_LABEL,
              color = TRT_LABEL
            ),
            position     = ggplot2::position_dodge(width = 0.75),
            size         = 3,
            show.legend  = TRUE
          ) +
          ggplot2::scale_fill_manual(values = fill_palette, name = NULL) +
          ggplot2::scale_color_manual(values = fill_palette, name = NULL) +
          ggplot2::scale_shape_manual(values = shape_palette, name = NULL) +
          ggplot2::labs(
            title = paste0(
              "Box Plot - Diastolic Blood Pressure Over Time\n",
              "(Weeks Since Randomized)"
            ),
            y = "Diastolic Blood Pressure (mmHg)",
            x = NULL,
            caption = paste0(
              "Box plot type=schematic: the box shows median, interquartile ",
              "range (IQR, edge of the bar), min and max\n",
              "within 1.5 IQR below 25% and above 75% (ends of the whisker). ",
              "Values outside the 1.5 IQR below 25% and\n",
              "above 75% are shown as outliers. ",
              "Means plotted as different symbols by treatments."
            )
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(
              hjust = 0.5, size = 14, face = "bold"
            ),
            plot.caption  = ggplot2::element_text(hjust = 0, size = 8),
            axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
            axis.title.x  = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.direction = "horizontal"
          ) +
          ggplot2::facet_wrap(
            ~ PHASE_GROUP, scales = "free_x", nrow = 1
          )
      }
    }

    # Render summary statistics table as tableGrob
    tbl_grob <- gridExtra::tableGrob(
      stats_tbl,
      rows  = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = 7,
        padding   = grid::unit(c(3, 3), "mm")
      )
    )

    # Combine boxplot + stats table using grid.arrange
    # Suppress intermediate drawing with a temporary null device
    tmp_dev_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file = tmp_dev_file)
    combined <- gridExtra::grid.arrange(
      p_box, tbl_grob,
      ncol    = 1,
      heights = c(3, 1)
    )
    invisible(grDevices::dev.off())
    unlink(tmp_dev_file, force = TRUE)

    combined
  }

  # ------------------------------------------------------------------
  # Phase 7: Output Generation
  # Replaces SAS: ods pdf; goptions hsize=15in vsize=8.5in;
  #               proc shewhart; run; (two panels)
  # ------------------------------------------------------------------
  output_files <- character(0)

  # Panel 1: Early visits (AVISITN <= 8)
  if (nrow(early_data) > 0) {
    panel1 <- build_boxplot_panel(
      early_data, early_stats_tbl, use_facet = TRUE
    )
    panel1_file <- file.path(
      output_path,
      paste0("box_obs_time_panel1.", output_format)
    )
    ggplot2::ggsave(
      panel1_file, panel1,
      width = 15, height = 8.5, units = "in"
    )
    output_files <- c(output_files, panel1_file)
  }

  # Panel 2: Late visits (AVISITN > 8)
  if (nrow(late_data) > 0) {
    panel2 <- build_boxplot_panel(
      late_data, late_stats_tbl, use_facet = FALSE
    )
    panel2_file <- file.path(
      output_path,
      paste0("box_obs_time_panel2.", output_format)
    )
    ggplot2::ggsave(
      panel2_file, panel2,
      width = 15, height = 8.5, units = "in"
    )
    output_files <- c(output_files, panel2_file)
  }

  # ------------------------------------------------------------------
  # Return results invisibly
  # ------------------------------------------------------------------
  invisible(list(
    output_files = output_files,
    data         = vwiseplot,
    stats        = stats
  ))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. ADVS XPT data path is parameterized; default assumes local repo structure
#    2. SAS PROC SHEWHART boxstyle=schematic mapped to ggplot2 geom_boxplot with notch=TRUE
#    3. Block text summary panels rendered via gridExtra::tableGrob beneath boxplot
#    4. SAS symbol1/symbol2 (circle/star) mapped to ggplot2 shape codes 1 and 8
#    5. SAS `. < AVISITN < 99` interpreted as !is.na(AVISITN) & AVISITN < 99 per SAS missing sort order
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Notch width calculation differs between SAS SHEWHART and ggplot2 (1.58*IQR/sqrt(n) in ggplot2)
#    2. Rounding uses janitor::round_half_up() to match SAS round-half-up behavior
#    3. Boxplot whisker extent: SAS schematic uses 1.5*IQR; ggplot2 default is also 1.5*IQR — should match
#
# NO DIRECT R EQUIVALENT:
#    1. PROC SHEWHART boxchart with block labels/positions — approximated with ggplot2 + tableGrob
#    2. SAS _Phase_ variable with readphase/phaseref/phaselegend — approximated with facet_wrap or vertical reference lines + annotations
#    3. SAS goptions hsize/vsize — mapped to ggsave width/height parameters
#
# PACKAGE SELECTION RATIONALE:
#    1. haven: Read SAS XPT transport files (CRAN, tidyverse ecosystem)
#    2. dplyr: Data manipulation replacing DATA steps/PROC SQL (tidyverse core)
#    3. ggplot2: Visualization replacing PROC SHEWHART/SGPLOT (tidyverse core)
#    4. gridExtra: Table rendering below plots (CRAN, widely used)
#    5. janitor: round_half_up() for SAS-compatible rounding (CRAN)
#
# OPEN QUESTIONS:
#    1. Exact notch width formula alignment between SAS SHEWHART and ggplot2 — statistician review recommended
#    2. Phase label placement (SAS phaselegend) — current implementation uses facet_wrap; alternative vertical reference lines may better match SAS output
#    3. Block text positioning precision vs SAS blocklabtype/blockpos parameters
#    4. SAS labelraw has swapped Q3/Max labels (block8='Q3', block9='Max') vs data (block8=max, block9=q3) — R implementation corrects this mapping
# ============================================================
