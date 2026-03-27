# =============================================================================
# mean_time.R
# =============================================================================
# PhUSE Scriptathon 2014 — Mean Time Profile Plot for Diastolic Blood Pressure
#
# Migrated from SAS to R
#   Original SAS code name: target4.sas
#   Original SAS author:    P. Chen
#   Migration:              SAS V9 -> R 4.3+ (tidyverse + ggplot2)
#
# Description:
#   Generates a mean profile plot of Diastolic Blood Pressure (DIABP)
#   over time by treatment group with confidence intervals and an
#   at-risk subject count panel. Reads from a CDISC ADaM ADVS dataset
#   in SAS XPT transport format.
#
# Notes from original author (preserved):
#   1. This is just a test version reference code, may need to be validated
#      before using.
#   2. In many cases, one may wish to use LSMEAN instead of the MEAN; other
#      procedures (e.g., PROC MIXED) could be used to calculate lsmeans.
#      The code for the plot part should only require very minor changes.
#   3. A BANDPLOT statement can be added to draw a shaded "Normal Range" area
#      in the background based on your data and requirements.
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(janitor)

#' Mean Time Profile Plot for Diastolic Blood Pressure (DIABP)
#'
#' Generates a mean profile over time plot with confidence intervals and
#' at-risk subject counts, replicating the SAS PROC SGRENDER DBP_PROFILE
#' template from target4.sas (PhUSE Scriptathon 2014).
#'
#' @param data_path Character string specifying the path to the ADVS XPT file.
#'   If NULL (default), uses \code{file.path("data", "adam", "cdisc", "advs.xpt")}.
#' @param output_path Character string specifying the directory for output files.
#'   Defaults to the current working directory (\code{"."}).
#' @param output_format Character string specifying the output format.
#'   One of \code{"pdf"} (default), \code{"png"}, or \code{"svg"}.
#'
#' @return Invisibly returns a named list with components:
#'   \describe{
#'     \item{plot}{The combined grob object (main plot + at-risk table)}
#'     \item{data}{The wide-format summary dataset used for plotting}
#'     \item{stats}{The long-format summary statistics by treatment and visit}
#'   }
#'
#' @details
#' This function filters the ADVS dataset to PARAMCD="DIABP", ATPTN=815,
#' ANL01FL="Y", non-missing AVISITN < 99, and SAFFL="Y". It computes
#' group-level N, mean, and standard error, then calculates confidence
#' intervals using \code{qt(0.95, n - 1)}. Note: the original SAS code uses
#' TINV(0.95, n-1) which produces approximately a 90\% CI (not 95\%); this
#' is preserved for functional parity with the SAS source.
#'
#' @examples
#' \dontrun{
#'   # Run with default data path
#'   result <- mean_time()
#'
#'   # Run with custom path and PNG output
#'   result <- mean_time(
#'     data_path = "path/to/advs.xpt",
#'     output_path = "output",
#'     output_format = "png"
#'   )
#' }
mean_time <- function(data_path = NULL,
                      output_path = ".",
                      output_format = "pdf") {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (is.null(data_path)) {
    data_path <- file.path("data", "adam", "cdisc", "advs.xpt")
  }

  if (!file.exists(data_path)) {
    stop(
      "ADVS XPT file not found at: ", data_path, "\n",
      "Please provide a valid path via the data_path argument.",
      call. = FALSE
    )
  }

  output_format <- tolower(output_format)
  if (!output_format %in% c("pdf", "png", "svg")) {
    stop(
      "Unsupported output_format: '", output_format, "'. ",
      "Must be one of 'pdf', 'png', or 'svg'.",
      call. = FALSE
    )
  }

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Phase 1: Data Loading
  # ---------------------------------------------------------------------------
  # SAS equivalent: filename source url "..."; libname source xport;
  #                 data advs; set source.advs; ...
  advs_raw <- haven::read_xpt(data_path)

  # ---------------------------------------------------------------------------
  # Phase 2: Data Filtering
  # ---------------------------------------------------------------------------
  # SAS equivalent:
  #   WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y'
  #         and .<avisitn<99 AND SAFFL='Y';
  #   KEEP usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval;
  #
  # SAS ". < avisitn < 99" means: avisitn is not missing AND avisitn < 99
  # NA handling: !is.na() replaces SAS missing check; no implicit zero substitution
  advs_filtered <- advs_raw %>%
    dplyr::filter(
      PARAMCD == "DIABP",
      ATPTN == 815,
      ANL01FL == "Y",
      !is.na(AVISITN),
      AVISITN < 99,
      SAFFL == "Y"
    ) %>%
    dplyr::select(
      USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD,
      AVISIT, AVISITN, ATPTN, SAFFL, AVAL
    )

  # ---------------------------------------------------------------------------
  # Phase 3: Summary Statistics with Confidence Intervals
  # ---------------------------------------------------------------------------
  # SAS equivalent:
  #   PROC MEANS DATA=advs NOPRINT;
  #     BY trtpn avisitn avisit;
  #     VAR aval;
  #     OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm;
  #   RUN;
  #
  #   DATA temp1;
  #     SET temp;
  #     lo = mean - (TINV(0.95, n-1) * stderr);
  #     hi = mean + (TINV(0.95, n-1) * stderr);
  #     drop _TYPE_ _FREQ_ lclm uclm stderr;
  #   RUN;
  #
  # CRITICAL: SAS TINV(0.95, n-1) returns the 95th percentile of the
  # t-distribution with (n-1) degrees of freedom. For a two-sided 95% CI,
  # one would use TINV(0.975, n-1). The original SAS code uses 0.95 which
  # yields approximately a 90% CI. This is preserved exactly for functional
  # parity: R qt(0.95, n-1) == SAS TINV(0.95, n-1).
  #
  # N uses sum(!is.na(AVAL)) to match SAS PROC MEANS N (non-missing count).
  # STDERR uses sd()/sqrt(N) which matches SAS (both use n-1 denominator for sd).
  stats <- advs_filtered %>%
    dplyr::group_by(TRTPN, AVISITN, AVISIT) %>%
    dplyr::summarise(
      n = sum(!is.na(AVAL)),
      mean_val = mean(AVAL, na.rm = TRUE),
      stderr = sd(AVAL, na.rm = TRUE) / sqrt(sum(!is.na(AVAL))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lo = mean_val - (stats::qt(0.95, n - 1) * stderr),
      hi = mean_val + (stats::qt(0.95, n - 1) * stderr)
    )

  # ---------------------------------------------------------------------------
  # Phase 4: Pivot to Wide Format by Treatment Arm
  # ---------------------------------------------------------------------------
  # SAS equivalent:
  #   data a b c; set temp1;
  #     if trtpn=0 then output a;
  #     else if trtpn=54 then output b;
  #     else if trtpn=81 then output c;
  #   run;
  #   data a1; set a; rename n=a_n mean=a_mean lo=a_lcl hi=a_ucl; run;
  #   data b1; set b; rename n=b_n mean=b_mean lo=b_lcl hi=b_ucl; run;
  #   data c1; set c; rename n=c_n mean=c_mean lo=c_lcl hi=c_ucl; run;
  #   data all; merge a1 b1 c1; by avisitn avisit;
  #     rename avisitn=xc avisit=x; drop trtpn;
  #     label a_mean=0 b_mean=54 c_mean=81;
  #   run;
  #
  # Using dplyr filter + rename + left_join (idiomatic tidyverse approach)
  arm_a <- stats %>%
    dplyr::filter(TRTPN == 0) %>%
    dplyr::rename(a_n = n, a_mean = mean_val, a_lcl = lo, a_ucl = hi) %>%
    dplyr::select(AVISITN, AVISIT, a_n, a_mean, a_lcl, a_ucl)

  arm_b <- stats %>%
    dplyr::filter(TRTPN == 54) %>%
    dplyr::rename(b_n = n, b_mean = mean_val, b_lcl = lo, b_ucl = hi) %>%
    dplyr::select(AVISITN, AVISIT, b_n, b_mean, b_lcl, b_ucl)

  arm_c <- stats %>%
    dplyr::filter(TRTPN == 81) %>%
    dplyr::rename(c_n = n, c_mean = mean_val, c_lcl = lo, c_ucl = hi) %>%
    dplyr::select(AVISITN, AVISIT, c_n, c_mean, c_lcl, c_ucl)

  all_data <- arm_a %>%
    dplyr::left_join(arm_b, by = c("AVISITN", "AVISIT")) %>%
    dplyr::left_join(arm_c, by = c("AVISITN", "AVISIT")) %>%
    dplyr::rename(xc = AVISITN, x = AVISIT)

  # ---------------------------------------------------------------------------
  # Phase 5: Prepare Long-Format Plot Data with X-Offsets
  # ---------------------------------------------------------------------------
  # SAS equivalent: STATGRAPH eval() offsets — xc-0.05, xc+0.05, xc+0.15
  # Using long format for idiomatic ggplot2 aesthetic mapping
  arm_labels <- c("Placebo (0)", "Low Dose (54)", "High Dose (81)")

  plot_data <- stats %>%
    dplyr::mutate(
      x_pos = dplyr::case_when(
        TRTPN == 0  ~ AVISITN - 0.05,
        TRTPN == 54 ~ AVISITN + 0.05,
        TRTPN == 81 ~ AVISITN + 0.15
      ),
      arm = factor(
        TRTPN,
        levels = c(0, 54, 81),
        labels = arm_labels
      )
    )

  # ---------------------------------------------------------------------------
  # Phase 5 (cont'd): Define Colors, Line Types, and Shapes
  # ---------------------------------------------------------------------------
  # SAS graphdata2/graphdata3/graphdata4 colors are style-dependent.
  # Using visually distinct, colorblind-friendly colors for the R migration.
  # Marker shapes: circle, triangle-up, square to distinguish arms.
  # Line types: dashed, longdash, dotdash mapping from SAS shortdash,
  #   mediumdash, dash respectively.
  arm_colors <- c(
    "Placebo (0)"     = "#4E79A7",
    "Low Dose (54)"   = "#E15759",
    "High Dose (81)"  = "#59A14F"
  )

  arm_linetypes <- c(
    "Placebo (0)"     = "dashed",
    "Low Dose (54)"   = "longdash",
    "High Dose (81)"  = "dotdash"
  )

  arm_shapes <- c(
    "Placebo (0)"     = 16,
    "Low Dose (54)"   = 17,
    "High Dose (81)"  = 15
  )

  # X-axis tick positions matching SAS tickvaluelist=(0 2 4 6 8 10 12 16 20 24 26)
  x_breaks <- c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26)

  # ---------------------------------------------------------------------------
  # Phase 6: Main Mean Profile Plot with Error Bars
  # ---------------------------------------------------------------------------
  # SAS equivalent: PROC TEMPLATE define statgraph dbp_profile
  #   layout overlay with scatterplot (yerrorlower/yerrorupper),
  #   seriesplot connecting means, axes, legend
  p_main <- ggplot2::ggplot(plot_data) +
    # Error bars for confidence intervals
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = x_pos, ymin = lo, ymax = hi, color = arm
      ),
      width = 0.3,
      linewidth = 0.8
    ) +
    # Points at the mean values
    ggplot2::geom_point(
      ggplot2::aes(
        x = x_pos, y = mean_val, color = arm, shape = arm
      ),
      size = 3
    ) +
    # Lines connecting the means over time
    ggplot2::geom_line(
      ggplot2::aes(
        x = x_pos, y = mean_val, color = arm,
        linetype = arm, group = arm
      ),
      linewidth = 0.8
    ) +
    # Manual scales for color, linetype, and shape
    ggplot2::scale_color_manual(
      name   = "Treatment Group:",
      values = arm_colors
    ) +
    ggplot2::scale_linetype_manual(
      name   = "Treatment Group:",
      values = arm_linetypes
    ) +
    ggplot2::scale_shape_manual(
      name   = "Treatment Group:",
      values = arm_shapes
    ) +
    # X-axis: SAS linearopts tickvaluelist; offsets for arm separation
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = c(-0.5, 27)
    ) +
    # Labels: SAS yaxisopts label, entrytitle
    ggplot2::labs(
      title = paste(
        "Mean of DBP Measures by Treatment:",
        "Profile Over Time (Weeks Since Randomized)"
      ),
      y = "Mean(unit) with 95% CI",
      x = NULL
    ) +
    # Theme: SAS griddisplay=on for y-axis
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey85"),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position     = "bottom",
      legend.box          = "horizontal",
      plot.title          = ggplot2::element_text(
        hjust = 0.5, size = 13, face = "bold"
      ),
      axis.text.x         = ggplot2::element_blank(),
      axis.ticks.x        = ggplot2::element_blank()
    )

  # ---------------------------------------------------------------------------
  # Phase 7: At-Risk Subject Count Block Plot Panel
  # ---------------------------------------------------------------------------
  # SAS equivalent: 3 blockplot rows for c_n (81), b_n (54), a_n (0)
  #   with colored labels and values matching graphdata4/3/2
  #   rowweights = (0.8 0.05 0.05 0.05 0.05) => blockplots ~15% of height
  atrisk_colors <- c("81" = "#59A14F", "54" = "#E15759", "0" = "#4E79A7")

  atrisk_data <- all_data %>%
    dplyr::select(xc, a_n, b_n, c_n) %>%
    tidyr::pivot_longer(
      cols      = c(a_n, b_n, c_n),
      names_to  = "arm",
      values_to = "n_at_risk"
    ) %>%
    dplyr::mutate(
      arm_label = dplyr::case_when(
        arm == "c_n" ~ "81",
        arm == "b_n" ~ "54",
        arm == "a_n" ~ "0"
      ),
      n_display = ifelse(is.na(n_at_risk), "", as.character(as.integer(n_at_risk)))
    )

  p_atrisk <- ggplot2::ggplot(
    atrisk_data,
    ggplot2::aes(
      x     = xc,
      y     = factor(arm_label, levels = c("81", "54", "0")),
      label = n_display,
      color = arm_label
    )
  ) +
    ggplot2::geom_text(size = 3.2, fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = atrisk_colors) +
    ggplot2::scale_x_continuous(
      name   = "Weeks Since Randomized",
      breaks = x_breaks,
      limits = c(-0.5, 27)
    ) +
    ggplot2::labs(y = "At Risk\nSubjects") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 9),
      axis.text.y  = ggplot2::element_text(
        face = "bold", color = c("#59A14F", "#E15759", "#4E79A7")
      ),
      plot.margin  = ggplot2::margin(t = 0, r = 5.5, b = 5.5, l = 5.5)
    )

  # ---------------------------------------------------------------------------
  # Phase 8: Combined Layout
  # ---------------------------------------------------------------------------
  # SAS equivalent: layout lattice rowweights=(0.8 0.05 0.05 0.05 0.05)
  # Main plot 80%, at-risk panel ~20% (3 blockplot rows + sidebar combined)
  combined <- gridExtra::arrangeGrob(
    p_main,
    p_atrisk,
    ncol    = 1,
    heights = c(4, 1)
  )

  # ---------------------------------------------------------------------------
  # Phase 9: Output Generation
  # ---------------------------------------------------------------------------
  # SAS equivalent:
  #   ods html image_dpi=100 file='DBP_Profile.html' path='.';
  #   ods graphics / reset noborder width=600px height=400px ...;
  #   proc sgrender data=all template=dbp_profile;
  #     dynamic title="Mean of DBP Measures by Treatment: ...";
  #   run;
  #
  # Output dimensions: SAS designwidth=17in designheight=14.5in
  output_file <- file.path(
    output_path,
    paste0("mean_time_dbp_profile.", output_format)
  )

  ggplot2::ggsave(
    filename = output_file,
    plot     = combined,
    width    = 17,
    height   = 14.5,
    units    = "in",
    dpi      = 100
  )

  message("Output saved to: ", output_file)

  # Return results invisibly for programmatic use
  invisible(list(
    plot  = combined,
    data  = all_data,
    stats = stats
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. ADVS XPT data path is parameterized; default assumes local repo structure
#       (data/adam/cdisc/advs.xpt). No hardcoded URLs per AAP rule.
#    2. SAS PROC MEANS N/MEAN/STDERR -> dplyr::summarise with
#       sum(!is.na(AVAL))/mean()/sd()/sqrt(n) to match non-missing count.
#    3. SAS TINV(0.95, n-1) -> R qt(0.95, n-1) — both return 95th percentile
#       of the t-distribution with (n-1) degrees of freedom.
#    4. Original SAS code uses TINV(0.95, n-1) which produces approximately a
#       90% CI, NOT a 95% CI. For a true two-sided 95% CI, TINV(0.975, n-1)
#       or qt(0.975, n-1) would be required. This is preserved as-is for
#       functional parity with the SAS source.
#    5. Treatment arms: TRTPN=0 (Placebo), TRTPN=54 (Low Dose),
#       TRTPN=81 (High Dose) — labels inferred from CDISC convention.
#    6. SAS PROC CONTENTS and PROC FREQ diagnostic steps (lines 31-37 of
#       original SAS) are omitted from the R migration as they are exploratory
#       and not part of the production output.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. PROC MEANS STDERR vs R sd()/sqrt(n) — should be identical; both use
#       n-1 denominator for standard deviation calculation.
#    2. qt() vs TINV() — should be identical to machine epsilon (both are
#       inverse CDF of t-distribution).
#    3. Rounding: janitor::round_half_up() is available for any formatting that
#       requires SAS-compatible round-half-up behavior. Currently no explicit
#       rounding of displayed values is performed (matching SAS source which
#       does not explicitly round mean/CI values).
#    4. Mean calculation: R mean(na.rm=TRUE) vs SAS PROC MEANS — identical
#       for non-missing data with the same floating point representation.
#    5. Sort stability: SAS BY-group processing guarantees stable sort;
#       dplyr::group_by() + summarise() does not depend on sort order for
#       aggregation, so results are equivalent.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS STATGRAPH template with layout lattice / blockplot -> approximated
#       with ggplot2 + gridExtra::arrangeGrob(). The lattice rowweights
#       (0.8, 0.05, 0.05, 0.05, 0.05) are approximated with heights = c(4, 1).
#    2. SAS PROC SGRENDER dynamic title -> ggplot2 labs(title = ...).
#    3. SAS sidebar / discretelegend -> ggplot2 theme(legend.position = "bottom")
#       with scale_*_manual() for legend customization.
#    4. SAS graphdata2/graphdata3/graphdata4 color and marker style assignments
#       -> manual ggplot2 scale_color_manual(), scale_shape_manual(), and
#       scale_linetype_manual(). Colors chosen for visual distinction and
#       colorblind accessibility.
#    5. SAS blockplot with per-row color-coded labels and values -> ggplot2
#       geom_text() with color aesthetic in a separate panel.
#
# PACKAGE SELECTION RATIONALE:
#    1. haven: Read SAS XPT transport files (CRAN, tidyverse ecosystem) —
#       replaces SAS filename/libname xport mechanism.
#    2. dplyr: Data manipulation replacing DATA steps, PROC SORT, and
#       PROC MEANS (tidyverse core) — provides filter, select, group_by,
#       summarise, mutate, rename, left_join, case_when.
#    3. tidyr: pivot_longer() replacing SAS arm-splitting DATA steps and
#       pivot_wider() for data reshaping (tidyverse core).
#    4. ggplot2: Visualization replacing PROC TEMPLATE / PROC SGRENDER
#       STATGRAPH (tidyverse core) — provides layered grammar of graphics.
#    5. gridExtra: Multi-panel layout combining main plot + at-risk table
#       (CRAN) — arrangeGrob() replaces SAS lattice layout with rowweights.
#    6. janitor: round_half_up() for SAS-compatible rounding (CRAN) —
#       available for any location where rounding parity is required.
#    7. stats (base R): qt() for t-distribution quantiles replacing SAS
#       TINV() function; mean() and sd() for basic statistics.
#
# OPEN QUESTIONS:
#    1. SAS TINV(0.95, n-1) gives approximately 90% CI — was this intentional
#       or a bug in the original SAS code? Preserved as-is per functional
#       parity mandate. Statistician review recommended.
#    2. Author note suggests LSMEAN may be preferred over MEAN — not
#       implemented per "no added functionality" rule.
#    3. Author note suggests BANDPLOT for normal range — not implemented
#       per "no added functionality" rule.
#    4. Color matching between SAS graphdata2/3/4 and ggplot2 palette —
#       visual review recommended. Colors chosen (#4E79A7, #E15759, #59A14F)
#       are from the Tableau 10 palette for accessibility.
#    5. X-axis offset alignment (xc-0.05, xc+0.05, xc+0.15) matches SAS
#       eval() offsets exactly; may need tuning for ggplot2 rendering at
#       different output resolutions.
#    6. SAS ODS HTML output is replaced with PDF/PNG/SVG; HTML output via
#       plotly/htmlwidgets was not implemented to avoid additional dependencies.
# ============================================================
