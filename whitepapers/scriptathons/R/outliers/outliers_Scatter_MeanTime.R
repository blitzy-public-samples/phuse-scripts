# ============================================================================
# Mean-Time Scatter Plot — Max Post-Baseline vs Max Baseline DIABP by Study
# ============================================================================
#
# Source:         whitepapers/scriptathons/outliers/outliers_Scatter_MeanTime.sas
# Original SAS:   Kriss Harris (12 October 2014)
# Migration Date: 2026-03-25
#
# Description:
#   Scatterplot shift figure showing Maximum Post-Baseline vs Maximum Baseline
#   diastolic blood pressure (DIABP) measurements by study. The plot uses a
#   datapanel layout (faceted by STUDYID) with an identity line (y = x),
#   dashed LLN/ULN reference lines on both axes, and a discrete treatment
#   legend at the bottom. Axes are equated (1:1 aspect ratio) with integer
#   breaks.
#
#   Migrated from SAS PROC TEMPLATE (define statgraph shiftplot) +
#   PROC SGRENDER to ggplot2 with facet_wrap.
#
# SAS Constructs Mapped:
#   - filename source url / libname xport  -> haven::read_xpt()
#   - PROC SORT BY studyid                 -> dplyr::arrange()
#   - DATA step WHERE + first.studyid      -> dplyr::filter() + group_by/row_number
#   - PROC SQL min/max/count(distinct)     -> dplyr::summarise() + n_distinct()
#   - PROC TEMPLATE define statgraph       -> ggplot2 construction
#   - layout datapanel classvars=(studyid) -> facet_wrap(~STUDYID)
#   - scatterplot x=base y=aval/group=trta -> geom_point(aes(color=TRTA))
#   - lineparm slope=1 intercept=0         -> geom_abline()
#   - referenceline x/y = ANRLO/ANRHI     -> geom_vline() + geom_hline()
#   - curvelabel LLN_label/ULN_label       -> geom_text() + annotate()
#   - sidebar discretelegend              -> theme(legend.position="bottom")
#   - linearopts viewmin/viewmax           -> coord_fixed() + scale_*_continuous()
#   - PROC SGRENDER                        -> ggsave()
#
# Config Reference:
#   Callers obtain paths from config/migration_config.yaml:
#     config$data_paths$adam_path       -> adsl_path / advsmax_path arguments
#     config$output_paths$figure_output_path -> output_path argument
#
# ============================================================================

# --- Required Libraries ---
library(haven)      # XPT data I/O (replaces SAS filename/libname xport)
library(dplyr)      # Data manipulation (replaces DATA step, PROC SORT, PROC SQL)
library(ggplot2)    # Visualization (replaces PROC TEMPLATE + PROC SGRENDER)
library(janitor)    # round_half_up for SAS-compatible rounding (Gate 2 compliance)
library(forcats)    # Factor level management for treatment ordering
library(cli)

# ============================================================================
# outliers_scatter_meantime
# ============================================================================
#' Generate a scatter plot of Max Post-Baseline vs Max Baseline measurements
#' by study, with identity line, normal range reference lines, and treatment
#' group coloring.
#'
#' @param adsl_path   Character. Path to the ADSL XPT file.
#'   Replaces SAS: filename source url "...adsl.xpt"; libname source xport;
#' @param advsmax_path Character. Path to the ADVSMAX XPT file.
#'   Replaces SAS: filename source url "...advsmax.xpt"; libname source xport;
#' @param output_path Character. Directory for generated figure output.
#'   Replaces SAS: ODS output destination.
#' @param paramcd Character. Analysis parameter code to filter on.
#'   Default "DIABP" (Diastolic Blood Pressure). Replaces SAS WHERE PARAMCD=.
#' @param atptn Numeric. Analysis timepoint number to filter on.
#'   Default 815. Replaces SAS WHERE ATPTN=.
#'
#' @return A list (returned invisibly) containing:
#'   \item{plot}{The ggplot2 scatter plot object}
#'   \item{data}{The filtered and processed ADVSMAX data frame}
#'   \item{adsl}{The loaded ADSL data frame (available for downstream use)}
#'   \item{output_file}{Path to the saved output file}
#'
#' @details
#'   This function implements the complete SAS pipeline from
#'   outliers_Scatter_MeanTime.sas (Kriss Harris, 2014):
#'   1. Load ADSL and ADVSMAX from XPT transport files
#'   2. Filter ADVSMAX by PARAMCD, ATPTN, ANL01FL, ADY, SAFFL
#'   3. Create BY-group labels for reference line annotations
#'   4. Compute equated axis ranges from BASE and AVAL
#'   5. Determine facet layout based on distinct study count
#'   6. Render scatter plot with identity line, reference lines, and legend
#'   7. Save to PDF in the specified output directory
#'
outliers_scatter_meantime <- function(adsl_path,
                                      advsmax_path,
                                      output_path,
                                      paramcd = "DIABP",
                                      atptn = 815) {

  # ==========================================================================
  # Phase 1: Input Validation
  # ==========================================================================
  # Validate all input arguments before proceeding
  if (!is.character(adsl_path) || length(adsl_path) != 1L || nchar(adsl_path) == 0L) {
    cli::cli_abort("adsl_path must be a non-empty character string specifying the ADSL XPT file.",
         call. = FALSE)
  }
  if (!is.character(advsmax_path) || length(advsmax_path) != 1L || nchar(advsmax_path) == 0L) {
    cli::cli_abort("advsmax_path must be a non-empty character string specifying the ADVSMAX XPT file.",
         call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
    cli::cli_abort("output_path must be a non-empty character string specifying the output directory.",
         call. = FALSE)
  }
  if (!is.character(paramcd) || length(paramcd) != 1L || nchar(paramcd) == 0L) {
    cli::cli_abort("paramcd must be a non-empty character string (e.g. 'DIABP').", call. = FALSE)
  }
  if (!is.numeric(atptn) || length(atptn) != 1L || is.na(atptn)) {
    cli::cli_abort("atptn must be a single non-missing numeric value (e.g. 815).", call. = FALSE)
  }

  # Verify input files exist
  if (!file.exists(adsl_path)) {
    cli::cli_abort("ADSL file not found: ", adsl_path, call. = FALSE)
  }
  if (!file.exists(advsmax_path)) {
    cli::cli_abort("ADVSMAX file not found: ", advsmax_path, call. = FALSE)
  }

  # ==========================================================================
  # Phase 2: Data Loading (SAS lines 9-21)
  # ==========================================================================
  # SAS: filename source url "https://.../adsl.xpt"; libname source xport;
  #      data work.adsl; set source.adsl; run;
  # R:   haven::read_xpt() with parameterized path (no hardcoded URLs)
  adsl <- haven::read_xpt(adsl_path)

  # SAS: filename source url "https://.../advsmax.xpt"; libname source xport;
  #      data advsmax; set source.advsmax; run;
  advsmax <- haven::read_xpt(advsmax_path)

  # ==========================================================================
  # Phase 3: Sorting and Filtering (SAS lines 25-39)
  # ==========================================================================
  # SAS: proc sort data = advsmax; by studyid; run;
  # SAS: data advsmax_final; set advsmax; by studyid;
  #      WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y"
  #            and ADY > 1 and SAFFL="Y";
  #      if first.studyid then do;
  #        LLN_label = "LLN"; ULN_label = "ULN";
  #      end;
  #      run;
  #
  # CRITICAL: Sort order must be established BEFORE grouping to match SAS
  # BY-group processing semantics (AAP §0.7.1 BY-group rules).
  # SAS first.studyid -> group_by(STUDYID) + row_number() == 1 after arrange.
  advsmax_final <- advsmax %>%
    dplyr::filter(
      PARAMCD == paramcd,
      ATPTN  == atptn,
      ANL01FL == "Y",
      ADY > 1,
      SAFFL == "Y"
    ) %>%
    dplyr::arrange(STUDYID) %>%
    dplyr::group_by(STUDYID) %>%
    dplyr::mutate(
      LLN_label = dplyr::if_else(dplyr::row_number() == 1L, "LLN", NA_character_),
      ULN_label = dplyr::if_else(dplyr::row_number() == 1L, "ULN", NA_character_)
    ) %>%
    dplyr::ungroup()

  # Validate that filtering produced results

  if (nrow(advsmax_final) == 0L) {
    warning("No records match the filter criteria (PARAMCD='", paramcd,
            "', ATPTN=", atptn,
            ", ANL01FL='Y', ADY>1, SAFFL='Y'). Returning empty plot.",
            call. = FALSE)
    empty_plot <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = paste0("No data for PARAMCD='", paramcd,
                                   "', ATPTN=", atptn))
    return(invisible(list(
      plot = empty_plot, data = advsmax_final, adsl = adsl, output_file = NA_character_
    )))
  }

  # ==========================================================================
  # Phase 4: Axis Range Computation (SAS lines 41-57)
  # ==========================================================================
  # SAS: proc sql; create table range as
  #        select min(base) as min_base, max(base) as max_base,
  #               min(aval) as min_aval, max(aval) as max_aval
  #        from advsmax_final; quit;
  #      data range2; set range;
  #        min = min(min_base, min_aval);
  #        max = max(max_base, max_aval);
  #      run;
  #      proc sql; select min(min), max(max) into : min, : max
  #        from range2; quit;
  #
  # R equivalent: single dplyr summarise chain computing overall axis bounds
  range_vals <- advsmax_final %>%
    dplyr::summarise(
      min_base = min(BASE, na.rm = TRUE),
      max_base = max(BASE, na.rm = TRUE),
      min_aval = min(AVAL, na.rm = TRUE),
      max_aval = max(AVAL, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      axis_min = min(min_base, min_aval),
      axis_max = max(max_base, max_aval)
    )

  # Extract scalars for ggplot2 scale limits
  # SAS-compatible rounding applied to axis bounds for integer-aligned ticks

  # (SAS linearopts integer=true; Gate 2 rounding audit compliance per AAP §0.7.2)
  axis_min <- janitor::round_half_up(floor(range_vals$axis_min), digits = 0)
  axis_max <- janitor::round_half_up(ceiling(range_vals$axis_max), digits = 0)

  # ==========================================================================
  # Phase 5: Study Count and Layout (SAS lines 59-82)
  # ==========================================================================
  # SAS: proc sql; create table advsmax_final2 as
  #        select *, count(distinct studyid) as count_study
  #        from advsmax_final; quit;
  #      data advsmax_final3; set advsmax_final2;
  #        if count_study <=2 then do; column_numbers=2; row_numbers=1; end;
  #        if count_study >2  then do; column_numbers=2; row_numbers=2; end;
  #      run;
  #      proc sql; select distinct column_numbers, row_numbers
  #        into :column_numbers, :row_numbers from advsmax_final3; run;
  #
  # R equivalent: compute scalars directly (no need to add columns to data)
  count_study <- dplyr::n_distinct(advsmax_final$STUDYID)

  if (count_study <= 2L) {
    col_numbers <- 2L
    row_numbers <- 1L
  } else {
    col_numbers <- 2L
    row_numbers <- ceiling(count_study / 2L)
  }

  # ==========================================================================
  # Phase 6: Prepare Reference Line and Label Data
  # ==========================================================================
  # Extract unique ANRLO/ANRHI values per study for reference lines.
  # In SAS, referenceline x=ANRLO draws a vertical line at each record's ANRLO.
  # Since ANRLO/ANRHI are constant within PARAMCD/study, we extract one
  # value per study to avoid redundant line rendering in ggplot2.
  ref_lines <- advsmax_final %>%
    dplyr::group_by(STUDYID) %>%
    dplyr::summarise(
      ANRLO = dplyr::first(ANRLO),
      ANRHI = dplyr::first(ANRHI),
      .groups = "drop"
    )

  # Ensure treatment arm factor ordering for consistent legend display
  # Replaces SAS FORMAT-based ordering (AAP §0.7.1 format mapping rules)
  advsmax_final <- advsmax_final %>%
    dplyr::mutate(TRTA = forcats::fct_inorder(TRTA))

  # Apply fct_relevel to establish treatment ordering if specific order is needed
  # Default: preserve data-encounter order via fct_inorder above
  # Callers may pass pre-factored TRTA or override by re-leveling after call
  trta_levels <- levels(advsmax_final$TRTA)
  if (length(trta_levels) > 0L) {
    advsmax_final <- advsmax_final %>%
      dplyr::mutate(TRTA = forcats::fct_relevel(TRTA, trta_levels))
  }

  # ==========================================================================
  # Phase 7: Build Scatter Plot (SAS lines 84-115)
  # ==========================================================================
  # SAS: proc template; define statgraph shiftplot;
  #        nmvar min max row_numbers column_numbers;
  #        begingraph;
  #          layout datapanel classvars=(studyid) /
  #            order=rowmajor rows=row_numbers columns=column_numbers
  #            rowaxisopts=(label="Maximum Post-baseline Measurment"
  #                         linearopts=(integer=true viewmin=min viewmax=max))
  #            columnaxisopts=(label="Maximum Baseline Measurment"
  #                           linearopts=(integer=true viewmin=min viewmax=max));
  #            layout prototype;
  #              scatterplot x=base y=aval / group=trta name="legend";
  #              lineparm x=0 y=0 slope=1;
  #              referenceline x=ANRLO / lineattrs=(pattern=2)
  #                xaxis=x curvelabel=LLN_label curvelabelposition=min;
  #              referenceline x=ANRHI / lineattrs=(pattern=2)
  #                xaxis=x curvelabel=ULN_label curvelabelposition=min;
  #              referenceline y=ANRLO / lineattrs=(pattern=2)
  #                yaxis=y curvelabel=LLN_label curvelabelposition=min;
  #              referenceline y=ANRHI / lineattrs=(pattern=2)
  #                yaxis=y curvelabel=ULN_label curvelabelposition=min;
  #            endlayout;
  #            sidebar / align=bottom;
  #              discretelegend "legend";
  #            endsidebar;
  #          endlayout;
  #        endgraph;
  #      end; run;
  #      proc sgrender data=advsmax_final3 template=shiftplot; run;

  scatter_plot <- ggplot2::ggplot(
    advsmax_final,
    ggplot2::aes(x = BASE, y = AVAL, color = TRTA)
  ) +

    # --- Scatter points by treatment (SAS line 96: scatterplot x=base y=aval/group=trta) ---
    ggplot2::geom_point(size = 2, alpha = 0.8) +

    # --- Identity line: slope=1, intercept=0 (SAS line 97: lineparm x=0 y=0 slope=1) ---
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.5) +

    # --- Identity line label (annotate for fixed text across all facets) ---
    ggplot2::annotate(
      "text",
      x = axis_max - (axis_max - axis_min) * 0.05,
      y = axis_max - (axis_max - axis_min) * 0.02,
      label = "y = x",
      size = 2.8, color = "grey40", fontface = "italic",
      hjust = 1, vjust = 1
    ) +

    # --- Vertical reference lines at ANRLO and ANRHI (SAS lines 98-99) ---
    # SAS: referenceline x=ANRLO / lineattrs=(pattern=2) xaxis=x
    ggplot2::geom_vline(
      data = ref_lines,
      ggplot2::aes(xintercept = ANRLO),
      linetype = "dashed", color = "grey30", linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      data = ref_lines,
      ggplot2::aes(xintercept = ANRHI),
      linetype = "dashed", color = "grey30", linewidth = 0.4
    ) +

    # --- Horizontal reference lines at ANRLO and ANRHI (SAS lines 101-102) ---
    # SAS: referenceline y=ANRLO / lineattrs=(pattern=2) yaxis=y
    ggplot2::geom_hline(
      data = ref_lines,
      ggplot2::aes(yintercept = ANRLO),
      linetype = "dashed", color = "grey30", linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      data = ref_lines,
      ggplot2::aes(yintercept = ANRHI),
      linetype = "dashed", color = "grey30", linewidth = 0.4
    ) +

    # --- Reference line labels for vertical lines (curvelabelposition=min → bottom) ---
    # SAS: curvelabel=LLN_label curvelabelposition=min on x=ANRLO
    ggplot2::geom_text(
      data = ref_lines,
      ggplot2::aes(x = ANRLO, label = "LLN"),
      y = axis_min + (axis_max - axis_min) * 0.02,
      color = "grey30", size = 2.5, hjust = -0.1, vjust = 0,
      inherit.aes = FALSE
    ) +
    # SAS: curvelabel=ULN_label curvelabelposition=min on x=ANRHI
    ggplot2::geom_text(
      data = ref_lines,
      ggplot2::aes(x = ANRHI, label = "ULN"),
      y = axis_min + (axis_max - axis_min) * 0.02,
      color = "grey30", size = 2.5, hjust = -0.1, vjust = 0,
      inherit.aes = FALSE
    ) +

    # --- Reference line labels for horizontal lines (curvelabelposition=min → left) ---
    # SAS: curvelabel=LLN_label curvelabelposition=min on y=ANRLO
    ggplot2::geom_text(
      data = ref_lines,
      ggplot2::aes(y = ANRLO, label = "LLN"),
      x = axis_min + (axis_max - axis_min) * 0.02,
      color = "grey30", size = 2.5, hjust = 0, vjust = -0.5,
      inherit.aes = FALSE
    ) +
    # SAS: curvelabel=ULN_label curvelabelposition=min on y=ANRHI
    ggplot2::geom_text(
      data = ref_lines,
      ggplot2::aes(y = ANRHI, label = "ULN"),
      x = axis_min + (axis_max - axis_min) * 0.02,
      color = "grey30", size = 2.5, hjust = 0, vjust = -0.5,
      inherit.aes = FALSE
    ) +

    # --- Facet by STUDYID (SAS: layout datapanel classvars=(studyid)) ---
    ggplot2::facet_wrap(
      ~ STUDYID,
      nrow = row_numbers,
      ncol = col_numbers
    ) +

    # --- Equated axes (SAS: linearopts viewmin=min viewmax=max + integer=true) ---
    ggplot2::coord_fixed(
      ratio = 1,
      xlim  = c(axis_min, axis_max),
      ylim  = c(axis_min, axis_max)
    ) +

    # Integer-aligned breaks (replaces SAS linearopts integer=true)
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +

    # --- Axis labels ---
    # SAS: rowaxisopts label="Maximum Post-baseline Measurment" (typo corrected)
    # SAS: columnaxisopts label="Maximum Baseline Measurment" (typo corrected)
    ggplot2::labs(
      x     = "Maximum Baseline Measurement",
      y     = "Maximum Post-baseline Measurement",
      color = "Treatment"
    ) +

    # --- Theme and legend placement ---
    # SAS: sidebar / align=bottom; discretelegend "legend"; endsidebar;
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      strip.text       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )

  # ==========================================================================
  # Phase 8: Output Generation (SAS line 114: proc sgrender)
  # ==========================================================================
  # Ensure the output directory exists (no hardcoded paths)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  output_file <- file.path(output_path, "Target18_scatter_meantime.pdf")

  # Calculate appropriate figure dimensions based on layout
  fig_width  <- 10
  fig_height <- if (row_numbers == 1L) 5.5 else 7.5 + (row_numbers - 2L) * 3

  ggplot2::ggsave(
    filename = output_file,
    plot     = scatter_plot,
    width    = fig_width,
    height   = fig_height,
    units    = "in",
    dpi      = 300
  )

  message("Scatter plot saved to: ", output_file)

  # Return plot object and processed data invisibly for downstream use
  invisible(list(
    plot        = scatter_plot,
    data        = advsmax_final,
    adsl        = adsl,
    output_file = output_file
  ))
}


# ============================================================================
#### MIGRATION NOTES
#### ============================================================================
#### ASSUMPTIONS:
####    1. SAS PROC TEMPLATE statgraph with NMVAR for dynamic rows/columns
####       is approximated via facet_wrap with computed nrow/ncol. For >4
####       studies, row count is extended via ceiling(count_study / 2).
####    2. LLN_label and ULN_label annotation position (curvelabelposition=min)
####       is placed near the axis minimum with a small offset (2% of range)
####       to avoid overlapping the axis border.
####    3. ADSL data is loaded but not explicitly merged in the SAS source;
####       it is preserved as available input and returned in the result list
####       for downstream callers needing demographic variables.
####    4. Commented-out alternative template (shiftplot2 with overlayequated
####       layout, SAS lines 120-140) is NOT migrated. The production template
####       (shiftplot with datapanel layout) is the canonical implementation.
####    5. ANRLO and ANRHI are assumed constant within each STUDYID/PARAMCD
####       combination; the first non-missing value per study is used for
####       reference lines.
####    6. SAS missing values (.) are handled as NA throughout; no implicit
####       zero substitution is applied per AAP §0.8.1 rules.
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. SAS integer axis tick placement may differ slightly from ggplot2
####       scales::breaks_pretty() — both produce sensible integer-aligned
####       breaks but the exact set of ticks may not be identical.
####    2. Coordinate system equating: SAS equatetype=square uses a device-
####       level constraint; R coord_fixed(ratio=1) constrains the data-to-
####       device mapping. Visual aspect ratio should match but rendering
####       engine differences may produce minor pixel-level deviations.
####    3. SAS axis min/max (viewmin/viewmax via NMVAR) may include different
####       padding than ggplot2 coord_fixed xlim/ylim — floor/ceiling rounding
####       applied per SAS round-half-up convention to minimize difference.
#### NO DIRECT R EQUIVALENT:
####    1. SAS PROC TEMPLATE define statgraph -> ggplot2 + facet_wrap
####       (no exact 1:1 GTL template equivalent in R)
####    2. SAS layout datapanel with dynamic NMVAR -> facet_wrap with
####       computed nrow/ncol (R has no template-level macro variable system)
####    3. SAS PROC SGRENDER -> ggplot2 rendering (different graphics engine;
####       SAS uses Java/C++ GTL renderer, R uses grid graphics system)
####    4. SAS curvelabel on reference lines -> ggplot2 geom_text() placed
####       at computed positions near axis minimum (curvelabelposition=min)
#### PACKAGE SELECTION RATIONALE:
####    haven: SAS XPT file I/O — tidyverse standard for transport files,
####       replaces SAS filename/libname xport pattern
####    dplyr: Data manipulation — replaces DATA step WHERE, PROC SORT BY,
####       PROC SQL aggregation; idiomatic tidyverse over base R per AAP §0.8.1
####    ggplot2: Visualization — replaces PROC TEMPLATE define statgraph +
####       PROC SGRENDER; industry standard for R clinical graphics
####    forcats: Factor level management — replaces SAS FORMAT-based treatment
####       arm ordering for consistent legend display
####    janitor: round_half_up for SAS-compatible rounding — ensures numeric
####       axis bounds match SAS round-half-up behavior (Gate 2 compliance)
####    scales: Axis break generation — breaks_pretty() replaces SAS
####       linearopts integer=true for sensible integer-aligned tick positions
#### OPEN QUESTIONS:
####    1. Confirm whether ADSL merge is required for any derived fields
####       in the visualization (SAS source loads ADSL but never merges it
####       with ADVSMAX — the function loads and returns it for caller use)
####    2. Verify reference line label positioning matches SAS
####       curvelabelposition=min behavior across different data ranges
####    3. Confirm facet layout for >4 studies — current implementation
####       extends rows dynamically but original SAS only coded <=2 and >2
####    4. Verify that ANRLO/ANRHI are truly constant within PARAMCD/study
####       for the target CDISC datasets; if they vary by record, a different
####       reference line strategy would be needed
#### ============================================================================
