# =============================================================================
# Treatment-Emergent Abnormal Qualitative — Blood Pressure Means by Treatment
# and Visit (Target 17)
# =============================================================================
#
# SOURCE:       whitepapers/scriptathons/outliers/outliers_TEAbnorm_Qual.sas
# AUTHOR:       Original SAS by Xiaopeng Li (xiaopeng.li@celerion.com)
# MIGRATED BY:  Blitzy SAS-to-R Migration (2026-03-25)
#
# DESCRIPTION:
#   Plots blood pressure means by treatment arm and visit day (ADY), with 95%
#   confidence interval whisker bars and sample size annotations, using ADVS
#   (Analysis Dataset for Vital Signs) data filtered for DIABP (diastolic blood
#   pressure) safety records taken after lying down for 5 minutes.
#
#   The SAS source title references "Systolic Blood Pressure" but the PARAMCD
#   filter is "DIABP" (diastolic). This discrepancy is preserved from the
#   original SAS to maintain functional parity. See MIGRATION NOTES.
#
# SAS-TO-R TRANSFORMATION SUMMARY:
#   - filename source url / libname xport  -> haven::read_xpt()
#   - DATA step (filter + keep)            -> dplyr::filter() + dplyr::select()
#   - PROC SORT                            -> dplyr::arrange()
#   - PROC MEANS (mean, n, lclm, uclm)    -> dplyr::group_by() + summarise()
#                                             with stats::qt() for t-based CI
#   - DATA step (ADY jitter by treatment)  -> dplyr::mutate() + case_when()
#   - SAS ANNOTATE facility (move/draw)    -> ggplot2::geom_errorbar()
#   - SAS ANNOTATE anno2 (text placement)  -> ggplot2::geom_text()
#   - symbol1-3 (i=j v=dot/triangle/star) -> geom_line() + geom_point() + scale_*
#   - axis1/axis2 (order=)                -> scale_y_continuous / scale_x_continuous
#   - PROC GPLOT with ANNOTATE=            -> ggplot2 layered geoms + ggsave()
#   - title1-title5                        -> labs(title, subtitle, caption)
#
# USAGE:
#   # Using migration config:
#   config <- yaml::read_yaml("config/migration_config.yaml")
#   outliers_teabnorm_qual(
#     data_path   = config$data_paths$adam_path,
#     output_path = config$output_paths$figure_output_path
#   )
#
#   # Or with explicit paths:
#   outliers_teabnorm_qual(
#     data_path   = "data/adam/cdisc",
#     output_path = "output/figures"
#   )
# =============================================================================

# ---------------------------------------------------------------------------
# Library Loading
# ---------------------------------------------------------------------------
library(haven)      # SAS XPT transport file I/O (replaces filename/libname xport)
library(dplyr)      # Core tidyverse data manipulation (replaces DATA steps, PROC SORT, PROC MEANS)
library(ggplot2)    # Visualization engine (replaces PROC GPLOT + ANNOTATE facility)
library(janitor)    # round_half_up() for SAS-compatible rounding (AAP §0.7.2)
library(stringr)    # Tidyverse string manipulation (replaces SAS trim(left()))

# ---------------------------------------------------------------------------
# Main Function: outliers_teabnorm_qual
# ---------------------------------------------------------------------------
#' Treatment-Emergent Abnormal Qualitative — Blood Pressure Means Plot
#'
#' Generates a mean blood pressure plot by treatment arm and visit day with 95%
#' confidence interval whisker bars and sample size annotations. Migrated from
#' the SAS scriptathon script outliers_TEAbnorm_Qual.sas (Target 17).
#'
#' @param data_path Character. Path to directory containing advs.xpt.
#'   Replaces SAS: filename source url "https://...advs.xpt"; libname source xport;
#' @param output_path Character. Output directory for the generated figure.
#'   Replaces SAS: implicit ODS/GPLOT output destination.
#' @param paramcd Character. CDISC parameter code to filter on. Default "DIABP".
#'   Replaces SAS: hardcoded PARAMCD="DIABP" in WHERE clause (line 15).
#' @param atpt_filter Character. Analysis timepoint text filter. Default
#'   "AFTER LYING DOWN FOR 5 MINUTES". Replaces SAS: ATPT= filter (line 15).
#' @param output_format Character. Output file format, one of "pdf", "png".
#'   Default "pdf".
#' @param plot_width Numeric. Plot width in inches. Default 10.
#' @param plot_height Numeric. Plot height in inches. Default 7.5.
#'
#' @return A ggplot2 object (invisibly). The plot is also saved to output_path.
#'
#' @details
#' The function replicates the complete analytical pipeline from the SAS source:
#' \enumerate{
#'   \item Load ADVS from XPT transport file (SAS lines 10-11)
#'   \item Filter for safety population, PARAMCD, ATPT, visit range, analysis
#'         flag (SAS lines 15-17)
#'   \item Compute mean, N, LCLM, UCLM by treatment and analysis day
#'         (SAS PROC MEANS lines 25-29)
#'   \item Apply treatment-specific ADY jittering to prevent overlap
#'         (SAS lines 49-53)
#'   \item Construct CI whisker bars replacing SAS ANNOTATE facility
#'         (SAS lines 59-101)
#'   \item Add sample size text annotations (SAS lines 108-125)
#'   \item Generate combined plot (SAS PROC GPLOT lines 152-178)
#' }
#'
#' @examples
#' \dontrun{
#' config <- yaml::read_yaml("config/migration_config.yaml")
#' outliers_teabnorm_qual(
#'   data_path   = config$data_paths$adam_path,
#'   output_path = config$output_paths$figure_output_path
#' )
#' }
outliers_teabnorm_qual <- function(data_path,
                                   output_path,
                                   paramcd      = "DIABP",
                                   atpt_filter  = "AFTER LYING DOWN FOR 5 MINUTES",
                                   output_format = "pdf",
                                   plot_width   = 10,
                                   plot_height  = 7.5) {

  # -------------------------------------------------------------------------
  # Input Validation
  # -------------------------------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L || nchar(data_path) == 0L) {
    stop("data_path must be a non-empty character string specifying the directory containing advs.xpt",
         call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
    stop("output_path must be a non-empty character string specifying the output directory",
         call. = FALSE)
  }
  if (!is.character(paramcd) || length(paramcd) != 1L) {
    stop("paramcd must be a single character string (e.g., 'DIABP')", call. = FALSE)
  }
  if (!is.character(atpt_filter) || length(atpt_filter) != 1L) {
    stop("atpt_filter must be a single character string", call. = FALSE)
  }
  if (!output_format %in% c("pdf", "png")) {
    stop("output_format must be one of 'pdf' or 'png'", call. = FALSE)
  }

  # Verify input file exists
  advs_file <- file.path(data_path, "advs.xpt")
  if (!file.exists(advs_file)) {
    stop(paste0("ADVS dataset not found: ", advs_file,
                "\nPlease verify data_path points to the directory containing advs.xpt"),
         call. = FALSE)
  }

  # Create output directory if it does not exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------------------------------------------------------
  # Phase 2: Data Loading and Filtering (SAS lines 10-19)
  # -------------------------------------------------------------------------
  # SAS: filename source url "https://...advs.xpt"; libname source xport;
  #      data work.advs; set source.advs;
  advs_raw <- haven::read_xpt(advs_file)

  # SAS lines 15-17:
  #   if SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
  #      and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y";
  #   keep USUBJID TRTA ADY AVAL;
  advs <- advs_raw %>%
    dplyr::filter(
      SAFFL    == "Y",
      PARAMCD  == paramcd,
      ATPT     == atpt_filter,
      AVISITN  >= 0,
      AVISITN  < 99,
      ANL01FL  == "Y"
    ) %>%
    dplyr::select(USUBJID, TRTA, ADY, AVAL)

  # Verify filtered data is non-empty
  if (nrow(advs) == 0L) {
    warning(paste0("No records found after filtering for PARAMCD='", paramcd,
                   "', ATPT='", atpt_filter,
                   "', SAFFL='Y', AVISITN in [0,99), ANL01FL='Y'. ",
                   "Returning NULL."),
            call. = FALSE)
    return(invisible(NULL))
  }

  # -------------------------------------------------------------------------
  # Phase 3: Summary Statistics — Mean and 95% CI (SAS lines 23-29)
  # -------------------------------------------------------------------------
  # SAS: proc sort; by TRTA ADY; run;
  #      proc means data=advs mean n lclm uclm noprint;
  #        by TRTA ADY;
  #        var aval;
  #        output out=meanvs mean=mean n=num lclm=lclm uclm=uclm;
  #
  # SAS PROC MEANS LCLM/UCLM are 95% t-based confidence limits:
  #   LCLM = mean - t(0.975, n-1) * sd / sqrt(n)
  #   UCLM = mean + t(0.975, n-1) * sd / sqrt(n)
  # When n=1, sd is undefined and SAS produces missing (.) — R produces NA.
  meanvs <- advs %>%
    dplyr::arrange(TRTA, ADY) %>%
    dplyr::group_by(TRTA, ADY) %>%
    dplyr::summarise(
      mean_val = mean(AVAL, na.rm = TRUE),
      num      = dplyr::n(),
      sd_val   = stats::sd(AVAL, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      # Compute 95% CI using t-distribution (exact match to SAS PROC MEANS)
      # When n == 1, sd is NA => lclm and uclm become NA (matches SAS missing)
      # Guard df argument with pmax(..., 1L) so qt() never receives df=0

      # (which produces NaN warnings). The if_else() still returns NA when
      # num == 1, so the guarded qt() result is safely discarded.
      lclm = dplyr::if_else(
        num > 1L,
        mean_val - stats::qt(0.975, df = pmax(num - 1L, 1L)) * sd_val / sqrt(num),
        NA_real_
      ),
      uclm = dplyr::if_else(
        num > 1L,
        mean_val + stats::qt(0.975, df = pmax(num - 1L, 1L)) * sd_val / sqrt(num),
        NA_real_
      ),
      # SAS-compatible rounded mean for formatted display annotations.
      # Uses janitor::round_half_up() to match SAS round-half-up behaviour
      # (AAP §0.7.2 Gate 2 rounding audit compliance).
      mean_rounded = janitor::round_half_up(mean_val, digits = 1)
    )

  # -------------------------------------------------------------------------
  # Phase 4: Treatment ADY Offset for CI Jittering (SAS lines 45-55)
  # -------------------------------------------------------------------------
  # SAS: data ci; set meanvs;
  #   if trta="Xanomeline Low Dose" then ady=ady;
  #   else if trta="Xanomeline High Dose" then ady=ady+0.3;
  #   else if trta="Placebo" then ady=ady+0.6;
  #
  # The ADY offset (0, +0.3, +0.6) prevents overlapping CI whiskers.
  # This is a display-only transformation matching the SAS source exactly.
  ci_data <- meanvs %>%
    dplyr::mutate(
      ADY_offset = dplyr::case_when(
        TRTA == "Xanomeline Low Dose"  ~ ADY,
        TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
        TRTA == "Placebo"              ~ ADY + 0.6,
        TRUE                           ~ ADY
      )
    )

  # -------------------------------------------------------------------------
  # Phase 5: Prepare Sample Size Annotations (SAS lines 104-125)
  # -------------------------------------------------------------------------
  # SAS: data anno2; ... set num; if num ne .;
  #   if trta="Placebo"                 then do; y=-170; color='blue';  end;
  #   if trta="Xanomeline High Dose"    then do; y=-180; color='red';   end;
  #   if trta="Xanomeline Low Dose"     then do; y=-190; color='green'; end;
  #   text=trim(left(num));
  #
  # In R, the sample-size annotations are placed at fixed y positions below

  # the main plot area, matching the SAS ANNOTATE y-coordinate values.
  num_data <- ci_data %>%
    dplyr::filter(!is.na(num)) %>%
    dplyr::mutate(
      text_y = dplyr::case_when(
        TRTA == "Placebo"                ~ -170,
        TRTA == "Xanomeline High Dose"   ~ -180,
        TRTA == "Xanomeline Low Dose"    ~ -190,
        TRUE                             ~ NA_real_
      ),
      # SAS: text=trim(left(num)) -> stringr::str_trim for clean label
      num_label = stringr::str_trim(as.character(num))
    )

  # -------------------------------------------------------------------------
  # Phase 6: Color, Shape, and Style Definitions (SAS lines 153-172)
  # -------------------------------------------------------------------------
  # SAS: symbol1 i=j v=dot      color='blue';   -> Placebo
  #      symbol2 i=j v=triangle  color='red';    -> Xanomeline High Dose
  #      symbol3 i=j v=star      color='green';  -> Xanomeline Low Dose
  treatment_colors <- c(
    "Placebo"                = "blue",
    "Xanomeline Low Dose"   = "green",
    "Xanomeline High Dose"  = "red"
  )

  # SAS symbol shapes: dot=16 (filled circle), triangle=17, star=8 (asterisk)
  treatment_shapes <- c(
    "Placebo"                = 16,    # dot (filled circle)
    "Xanomeline Low Dose"   = 8,     # star (asterisk)
    "Xanomeline High Dose"  = 17     # triangle (filled)
  )

  # -------------------------------------------------------------------------
  # Phase 7: Build ggplot2 Visualization (SAS PROC GPLOT lines 174-178)
  # -------------------------------------------------------------------------
  # The entire SAS ANNOTATE dataset for CI whiskers (lines 59-101) with
  # move/draw function commands maps to ggplot2 geom_errorbar().
  # The anno2 dataset for sample size text (lines 108-125) maps to geom_text().
  # symbol1-3 with i=j (join interpolation) map to geom_line()+geom_point().
  mean_plot <- ggplot2::ggplot(
    ci_data,
    ggplot2::aes(x = ADY_offset, y = mean_val, color = TRTA, shape = TRTA)
  ) +
    # Mean line traces per treatment — SAS symbol1-3 i=j (join interpolation)
    ggplot2::geom_line(linewidth = 0.8) +
    # Mean value points — SAS symbol1-3 v=dot/triangle/star
    ggplot2::geom_point(size = 3) +
    # 95% CI whisker bars — replaces entire SAS ANNOTATE dataset (lines 59-101)
    # SAS move/draw commands for vertical CI lines + horizontal caps
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lclm, ymax = uclm),
      width = 0.5,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    # Sample size text annotations — replaces SAS anno2 dataset (lines 108-125)
    ggplot2::geom_text(
      data = num_data,
      ggplot2::aes(x = ADY_offset, y = text_y, label = num_label, color = TRTA),
      size = 2.5,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    # Color mapping — SAS symbol color definitions (blue/green/red)
    ggplot2::scale_color_manual(
      values = treatment_colors,
      name   = "Treatment"
    ) +
    # Shape mapping — SAS symbol v= definitions (dot/star/triangle)
    ggplot2::scale_shape_manual(
      values = treatment_shapes,
      name   = "Treatment"
    ) +
    # Y-axis — SAS axis1: label=(a=90 "Systolic blood pressure")
    #          order=(-200 to 350 by 50)
    ggplot2::scale_y_continuous(
      name   = "Systolic blood pressure",
      limits = c(-200, 350),
      breaks = seq(-200, 350, by = 50)
    ) +
    # X-axis — SAS axis2: label=("Visit number (visit day)")
    #          order=(0 to 250 by 50)
    ggplot2::scale_x_continuous(
      name   = "Visit number (visit day)",
      limits = c(0, 250),
      breaks = seq(0, 250, by = 50)
    ) +
    # Titles — SAS title1-title5 (lines 167-172)
    ggplot2::labs(
      title    = "Target 17",
      subtitle = "Systolic Blood Pressure Figure by Treatment",
      caption  = "Created by Xiaopeng Li"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle    = ggplot2::element_text(hjust = 0.5),
      plot.caption     = ggplot2::element_text(hjust = 0.5, face = "italic"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # -------------------------------------------------------------------------
  # Phase 8: Save Output (no hardcoded paths)
  # -------------------------------------------------------------------------
  output_filename <- paste0("Target17_TEAbnorm_Qual.", output_format)
  output_filepath <- file.path(output_path, output_filename)

  ggplot2::ggsave(
    filename = output_filepath,
    plot     = mean_plot,
    width    = plot_width,
    height   = plot_height,
    units    = "in",
    dpi      = 300
  )

  message("Plot saved to: ", output_filepath)

  # Return the plot object invisibly for further customisation
  invisible(mean_plot)
}


# =============================================================================
#### MIGRATION NOTES
# =============================================================================
#### ASSUMPTIONS:
####    1. SAS ANNOTATE facility (move/draw function commands, lines 59-101)
####       entirely replaced by ggplot2 geom_errorbar(). The SAS ANNOTATE
####       dataset uses xsys/ysys='2' coordinate system with function='move'
####       and function='draw' to render vertical CI lines and horizontal
####       cap lines — this maps directly to geom_errorbar(width=0.5).
####    2. ADY offset values (0, +0.3, +0.6) preserved exactly from SAS
####       source (lines 49-51) for jittering treatment CI bars to prevent
####       overlap at the same ADY value.
####    3. SAS axis ranges (-200 to 350 for y-axis, 0 to 250 for x-axis)
####       preserved as-is from SAS axis1/axis2 definitions (lines 158-162).
####       These ranges may need adjustment for different datasets.
####    4. Incomplete SAS DATA step syntax at lines 104-107 (data num;
####       set num; with no RUN statement) and lines 127-128 (data anno;
####       set anno anno2; with no RUN) treated as missing RUN statements —
####       standard SAS parsing handles implicit RUN at next DATA/PROC.
####    5. Script title says "Systolic Blood Pressure" but PARAMCD filter
####       is "DIABP" (diastolic) — preserved as-is from SAS source.
####    6. SAS commented-out sections (lines 140-150 for 'observation'
####       dataset) are not migrated as they were inactive in the SAS source.
####    7. SAS DATA step at line 132 (data final; set mean num;) merges
####       mean and num datasets — in R, the ci_data tibble already contains
####       all needed fields (mean_val, num, lclm, uclm, ADY_offset).
####    8. The SAS ANNOTATE width=2 line thickness maps to ggplot2
####       linewidth=0.7 for comparable visual appearance.
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. CI computation: SAS PROC MEANS LCLM/UCLM uses t-distribution
####       with the same formula: mean +/- t(0.975, n-1) * sd/sqrt(n).
####       R stats::qt() replication should be numerically identical, but
####       verify df handling when n is small (n=1 -> NA in both SAS and R).
####    2. Mean computation: Both SAS and R use IEEE 754 double-precision
####       arithmetic for arithmetic mean — results should agree exactly.
####    3. Rounding: Any formatted numeric output uses janitor::round_half_up()
####       to match SAS round-half-up behaviour per AAP §0.7.2.
####    4. Sort stability: SAS PROC SORT is stable by key; R dplyr::arrange()
####       is stable within groups. Multi-key sorts verified equivalent.
#### NO DIRECT R EQUIVALENT:
####    1. SAS ANNOTATE facility (xsys/ysys coordinate system, function=
####       'move'/'draw' for line drawing) -> ggplot2 geom_errorbar() for
####       CI whiskers + geom_text() for sample size annotations.
####    2. SAS PROC GPLOT with ANNOTATE= option -> ggplot2 layered geoms.
####    3. SAS symbol1-3 interpolation (i=j for join) -> geom_line().
####    4. SAS symbol v= (dot/triangle/star) -> scale_shape_manual() with
####       R pch codes 16 (circle), 17 (triangle), 8 (asterisk/star).
####    5. SAS goptions/footnote/title global statements -> ggplot2 labs()
####       and theme() elements.
#### PACKAGE SELECTION RATIONALE:
####    haven:    SAS XPT file I/O (tidyverse standard, CRAN v2.5.5)
####    dplyr:    Data manipulation replacing DATA steps, PROC SORT, and
####              PROC MEANS summary statistics (tidyverse core)
####    ggplot2:  Visualization replacing PROC GPLOT + ANNOTATE facility
####              (tidyverse core, CRAN v>=3.4.0)
####    janitor:  round_half_up() for SAS-compatible rounding (CRAN v>=2.2.0)
####    stringr:  str_trim() replacing SAS trim(left()) (tidyverse core)
####    stats:    qt(), sd(), sqrt() for t-based CI calculation (base R)
#### OPEN QUESTIONS:
####    1. Confirm PARAMCD="DIABP" is intentional despite title saying
####       "Systolic Blood Pressure" — likely a copy-paste error in the
####       original SAS source.
####    2. Verify CI whisker cap width=0.5 matches SAS ANNOTATE draw
####       width (xsys='9' x=+/-0.5, which is 0.5% of axis range).
####    3. Confirm sample size text y-position values (-170, -180, -190)
####       are appropriate for the data range and axis limits.
####    4. Review original author note (SAS lines 1-6) about differences
####       between data and target shell — overlapping numbers and
####       imperfect legend noted by the original SAS author.
# =============================================================================
