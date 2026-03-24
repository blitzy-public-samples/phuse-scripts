#' PhUSE ggplot2 Theme and Boxplot Utilities
#'
#' Migrated from: \code{whitepapers/utilities/util_proc_template.sas}
#' Renamed per AAP section 0.4.1: \code{util_proc_template} -> \code{util_ggplot_theme}
#'
#' Provides a ggplot2 theme, colour palette, size constants, and helper
#' functions that replicate the PhUSEboxplot GTL (Graph Template Language)
#' template originally registered via SAS PROC TEMPLATE. The SAS template
#' defined a complete boxplot specification including synchronized
#' layout/legend constants (IQRSIZE, CLUSTERWIDTH, DESIGNWIDTH, DESIGNHEIGHT),
#' box aesthetics, IQR outlier markers, normal-range outlier scatter,
#' reference lines, a discrete legend, top inner-margin block labels, and a
#' bottom inner-margin summary-statistics axis table.
#'
#' In the R migration the global template registration is replaced by:
#' \enumerate{
#'   \item \code{theme_phuse()} — a ggplot2 theme function replicating GTL aesthetics
#'   \item \code{phuse_boxplot()} — a convenience constructor that builds the
#'     complete ggplot matching the PhUSEboxplot template
#'   \item \code{phuse_boxplot_stats_table()} — computes per-group summary
#'     statistics for bottom-margin annotation (SAS AXISTABLE equivalent)
#'   \item \code{phuse_colors} — named colour palette matching SAS CX hex codes
#'   \item \code{phuse_sizes} — named size constants synchronising marker,
#'     box-width, and canvas dimensions
#' }
#'
#' @author Blitzy SAS-to-R Migration
#' @family PhUSE WPCT utilities
#'
#' @name util_ggplot_theme
NULL

# =========================================================================
# Package dependencies
# =========================================================================
# ggplot2 >= 3.4.0 : Core visualisation replacing SAS PROC TEMPLATE / GTL
# dplyr   >= 1.1.0 : Tidyverse data manipulation (AAP mandate)
# rlang   >= 1.1.0 : Tidy evaluation support for dynamic column references
# cli     >= 3.6.0 : User-facing messages replacing SAS %PUT NOTE/ERROR
# janitor >= 2.2.0 : SAS-compatible round_half_up() (AAP Gate 2 compliance)
# =========================================================================

# =========================================================================
# PhUSE Colour Palette — phuse_colors
# =========================================================================
# Maps SAS GTL CX colour codes to R hex / named colours.
# SAS source lines 68–80, 129–136, 151–153, 162.
# =========================================================================

#' PhUSE GTL Colour Palette
#'
#' Named list of colours matching the SAS PhUSEboxplot GTL template.
#'
#' \describe{
#'   \item{box_fill}{\code{"#B9CFE7"} — SAS CXB9CFE7 light-blue box fill}
#'   \item{box_outline}{\code{"navy"} — SAS GraphOutlines(color=navy)}
#'   \item{whisker}{\code{"navy"} — SAS whiskerattrs(color=navy)}
#'   \item{median_line}{\code{"navy"} — SAS medianattrs(color=navy)}
#'   \item{iqr_outlier}{\code{"black"} — SAS CX000000 square markers}
#'   \item{nr_outlier}{\code{"red"} — SAS CXFF0000 circlefilled markers}
#'   \item{ref_line}{\code{"red"} — SAS referenceline lineattrs(color=red)}
#'   \item{mean_marker}{\code{"black"} — SAS default mean diamond marker}
#' }
#' @export
phuse_colors <- list(
  box_fill     = "#B9CFE7",
  box_outline  = "navy",
  whisker      = "navy",
  median_line  = "navy",
  iqr_outlier  = "black",
  nr_outlier   = "red",
  ref_line     = "red",
  mean_marker  = "black"
)

# =========================================================================
# PhUSE Size Constants — phuse_sizes
# =========================================================================
# SAS GTL constants (lines 43–46, 59–60) mapped to ggplot2 scale.
# SAS marker sizes are in GTL units; ggplot2 uses mm-based sizes.
# SAS: iqr_size=6, mean=iqr_size+1=7, nr_outlier=iqr_size-1=5
# ggplot2 scale: proportionally reduced for visual equivalence.
# =========================================================================

#' PhUSE GTL Size Constants
#'
#' Named list of size constants synchronising boxplot, scatter, and legend
#' marker dimensions with the PhUSEboxplot GTL template defaults.
#'
#' \describe{
#'   \item{iqr_size}{IQR outlier marker size (ggplot2 scale; SAS: 6)}
#'   \item{mean_size}{Mean diamond marker size (ggplot2 scale; SAS: 7)}
#'   \item{nr_outlier_size}{Normal-range outlier marker size (ggplot2 scale; SAS: 5)}
#'   \item{cluster_width}{Box / cluster width ratio (SAS: 0.6)}
#'   \item{design_width_mm}{Default graphic width in mm (SAS: 260mm)}
#'   \item{design_height_mm}{Default graphic height in mm (SAS: 170mm)}
#' }
#' @export
phuse_sizes <- list(
  iqr_size        = 2,
  mean_size       = 3,
  nr_outlier_size = 1.5,
  cluster_width   = 0.6,
  design_width_mm = 260,
  design_height_mm = 170
)

# =========================================================================
# theme_phuse()
# =========================================================================
# Replicates the visual aesthetic of the SAS PhUSEboxplot GTL template
# as a ggplot2 theme object.
#
# SAS GTL mapping (lines 59–98):
#   BEGINGRAPH / border=false           → panel.border = element_blank()
#   dataskin=none                        → panel.background = element_blank()
#   walldisplay=none                     → panel.grid = element_blank()
#   pad=(top=20)                         → plot.margin top padding
#   yaxisopts display=standard           → axis.line.y visible
#   xaxisopts display=(line)             → axis.line.x visible
#   discretelegend valign=bottom         → legend.position = "bottom"
#   title='Treatments & Outliers:'       → legend.title text
#   ENTRYTITLE (centred)                 → plot.title hjust = 0.5
# =========================================================================

#' PhUSE ggplot2 Theme
#'
#' Returns a ggplot2 \code{\link[ggplot2]{theme}} object that replicates the
#' PhUSEboxplot GTL template aesthetic. Designed to be added to any ggplot
#' with \code{+ theme_phuse()}.
#'
#' @param design_width Graphic design width in mm (default 260, SAS designwidth).
#'   Stored for reference; does not resize the plot object itself. Use
#'   \code{ggsave(width = ...)} or device dimensions to control output size.
#' @param design_height Graphic design height in mm (default 170, SAS designheight).
#' @param base_size Base font size in pt for theme elements (default 11).
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   theme_phuse()
#'
#' @export
theme_phuse <- function(design_width = 260, design_height = 170, base_size = 11) {

  # --- Input validation ---------------------------------------------------
  if (!is.numeric(design_width) || design_width <= 0) {
    cli::cli_abort(
      "{.arg design_width} must be a positive number, not {.val {design_width}}."
    )
  }
  if (!is.numeric(design_height) || design_height <= 0) {
    cli::cli_abort(
      "{.arg design_height} must be a positive number, not {.val {design_height}}."
    )
  }
  if (!is.numeric(base_size) || base_size <= 0) {
    cli::cli_abort(
      "{.arg base_size} must be a positive number, not {.val {base_size}}."
    )
  }

  # --- Warn about SAS-to-ggplot2 size scaling differences -----------------
  if (design_width != 260 || design_height != 170) {
    cli::cli_warn(c(
      "!" = "Non-default design dimensions ({design_width}x{design_height} mm).",
      "i" = paste0(
        "SAS GTL designwidth/designheight map to ggsave() or device ",
        "dimensions, not to theme internals. Ensure your output device ",
        "is configured accordingly."
      )
    ))
  }

  # --- Build theme --------------------------------------------------------
  # Each line is annotated with the SAS GTL source it replicates.
  phuse_theme <- ggplot2::theme(
    # --- Canvas / Graph border (SAS: border=false, dataskin=none) ---------
    panel.background  = ggplot2::element_blank(),
    panel.border      = ggplot2::element_blank(),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA),

    # --- Grid lines (SAS: walldisplay=none) --------------------------------
    panel.grid.major  = ggplot2::element_blank(),
    panel.grid.minor  = ggplot2::element_blank(),

    # --- Axis lines (SAS: y=standard display, x=display(line)) ------------
    axis.line.y       = ggplot2::element_line(colour = "black", linewidth = 0.5),
    axis.line.x       = ggplot2::element_line(colour = "black", linewidth = 0.5),

    # --- Axis text ---------------------------------------------------------
    axis.text         = ggplot2::element_text(size = base_size, colour = "black"),
    axis.title        = ggplot2::element_text(size = base_size + 1, colour = "black"),

    # --- Axis ticks --------------------------------------------------------
    axis.ticks        = ggplot2::element_line(colour = "black", linewidth = 0.3),

    # --- Legend (SAS: valign=bottom, location=outside, border=false) -------
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.background = ggplot2::element_blank(),
    legend.key        = ggplot2::element_rect(fill = "white", colour = NA),
    legend.title      = ggplot2::element_text(
      size  = base_size,
      face  = "bold"
    ),
    legend.text       = ggplot2::element_text(size = base_size),

    # --- Title (SAS: ENTRYTITLE — centred) ---------------------------------
    plot.title        = ggplot2::element_text(
      hjust = 0.5,
      size  = base_size + 2,
      face  = "bold"
    ),
    plot.subtitle     = ggplot2::element_text(hjust = 0.5, size = base_size),

    # --- Margins (SAS: pad=0, pad(top=20)) ---------------------------------
    # top=20 GTL points ≈ ~7 mm; use generous margin for top, tight elsewhere
    plot.margin       = ggplot2::margin(
      t = 20, r = 5, b = 5, l = 5, unit = "pt"
    ),

    # --- Strip text for facets (timepoint block labels) --------------------
    strip.background  = ggplot2::element_rect(
      fill   = "grey90",
      colour = "black",
      linewidth = 0.3
    ),
    strip.text        = ggplot2::element_text(
      size   = base_size,
      hjust  = 0,
      vjust  = 1
    )
  )

  cli::cli_inform(c(
    "v" = "PhUSE GTL theme applied ({design_width}x{design_height} mm canvas)."
  ))

  phuse_theme
}

# =========================================================================
# phuse_boxplot()
# =========================================================================
# Convenience constructor building a complete ggplot2 boxplot that matches
# the PhUSEboxplot GTL template (SAS lines 48–216).
#
# SAS dynamic variables mapped to function arguments:
#   _TITLE       → title
#   _XVAR        → x_var
#   _YVAR        → y_var
#   _MARKERS     → group_var
#   _YLABEL      → y_label
#   _YMIN/_YMAX/_YINCR → y_min, y_max, y_incr
#   _BLOCKLABEL  → block_var
#   _YOUTLIERS   → outlier_var
#   _REFLINES    → ref_lines
# =========================================================================

#' PhUSE Boxplot Constructor
#'
#' Builds a ggplot2 boxplot object replicating the PhUSEboxplot GTL template.
#' Layers include box-and-whisker with notches, IQR outlier squares, mean
#' diamonds, optional normal-range outlier scatter, optional reference lines,
#' and a bottom legend.
#'
#' @param data A data frame (or tibble) containing the analysis data.
#' @param x_var Character string naming the x-axis (visit/timepoint) column.
#' @param y_var Character string naming the y-axis (measurement) column.
#' @param group_var Character string naming the grouping/treatment column, or
#'   \code{NULL} for ungrouped plots.
#' @param title Plot title (character). Equivalent to SAS \code{_TITLE} dynamic.
#' @param y_label Y-axis label (character). Equivalent to SAS \code{_YLABEL}.
#' @param y_min Numeric minimum for y-axis. Equivalent to SAS \code{_YMIN}.
#' @param y_max Numeric maximum for y-axis. Equivalent to SAS \code{_YMAX}.
#' @param y_incr Numeric increment for y-axis ticks. Equivalent to SAS
#'   \code{_YINCR}.
#' @param block_var Character string naming the block-label column for the top
#'   inner margin (SAS \code{_BLOCKLABEL}), or \code{NULL} to omit.
#' @param outlier_var Character string naming the normal-range outlier column
#'   (SAS \code{_YOUTLIERS}), or \code{NULL} to omit.
#' @param ref_lines Numeric vector of reference-line y-intercepts, or
#'   \code{NULL} to omit. Rendered in red per SAS template.
#' @param show_notch Logical; draw notches on boxes? (SAS default: TRUE)
#' @param show_mean Logical; overlay mean diamond marker? (SAS default: TRUE)
#' @param legend_title Legend title string (default \code{"Treatments & Outliers:"}).
#'
#' @return A \code{ggplot} object ready for printing or further modification.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' phuse_boxplot(
#'   data      = my_adlb,
#'   x_var     = "AVISITN",
#'   y_var     = "AVAL",
#'   group_var = "TRTA",
#'   title     = "Figure 7.1 — Central Tendency",
#'   y_label   = "Diastolic Blood Pressure (mmHg)",
#'   y_min     = 40, y_max = 120, y_incr = 10
#' )
#' }
#'
#' @export
phuse_boxplot <- function(data,
                          x_var,
                          y_var,
                          group_var    = NULL,
                          title        = NULL,
                          y_label      = NULL,
                          y_min        = NULL,
                          y_max        = NULL,
                          y_incr       = NULL,
                          block_var    = NULL,
                          outlier_var  = NULL,
                          ref_lines    = NULL,
                          show_notch   = TRUE,
                          show_mean    = TRUE,
                          legend_title = "Treatments & Outliers:") {

  # --- Input validation (replacing SAS %PUT ERROR) -------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.cls {class(data)}}.")
  }
  if (!is.character(x_var) || length(x_var) != 1L) {
    cli::cli_abort("{.arg x_var} must be a single column name string.")
  }
  if (!is.character(y_var) || length(y_var) != 1L) {
    cli::cli_abort("{.arg y_var} must be a single column name string.")
  }
  if (!(x_var %in% colnames(data))) {
    cli::cli_abort("Column {.val {x_var}} not found in {.arg data}.")
  }
  if (!(y_var %in% colnames(data))) {
    cli::cli_abort("Column {.val {y_var}} not found in {.arg data}.")
  }
  if (!rlang::is_null(group_var)) {
    if (!is.character(group_var) || length(group_var) != 1L) {
      cli::cli_abort("{.arg group_var} must be a single column name string or NULL.")
    }
    if (!(group_var %in% colnames(data))) {
      cli::cli_abort("Column {.val {group_var}} not found in {.arg data}.")
    }
  }
  if (!rlang::is_null(block_var)) {
    if (!is.character(block_var) || length(block_var) != 1L) {
      cli::cli_abort("{.arg block_var} must be a single column name string or NULL.")
    }
    if (!(block_var %in% colnames(data))) {
      cli::cli_abort("Column {.val {block_var}} not found in {.arg data}.")
    }
  }
  if (!rlang::is_null(outlier_var)) {
    if (!is.character(outlier_var) || length(outlier_var) != 1L) {
      cli::cli_abort("{.arg outlier_var} must be a single column name string or NULL.")
    }
    if (!(outlier_var %in% colnames(data))) {
      cli::cli_abort("Column {.val {outlier_var}} not found in {.arg data}.")
    }
  }

  # --- Validate y-axis numeric parameters ----------------------------------
  if (!rlang::is_null(y_min) && !is.numeric(y_min)) {
    cli::cli_abort("{.arg y_min} must be numeric or NULL.")
  }
  if (!rlang::is_null(y_max) && !is.numeric(y_max)) {
    cli::cli_abort("{.arg y_max} must be numeric or NULL.")
  }
  if (!rlang::is_null(y_incr) && (!is.numeric(y_incr) || y_incr <= 0)) {
    cli::cli_abort("{.arg y_incr} must be a positive number or NULL.")
  }
  if (!rlang::is_null(ref_lines) && !is.numeric(ref_lines)) {
    cli::cli_abort("{.arg ref_lines} must be a numeric vector or NULL.")
  }

  # --- Convert x_var to factor for discrete axis if not already ------------
  data <- dplyr::mutate(
    data,
    !!rlang::ensym(x_var) := factor(.data[[x_var]])
  )

  # --- Build base ggplot aesthetic mapping ---------------------------------
  # SAS: boxplot x=_XVAR y=_YVAR / group=_MARKERS
  if (!rlang::is_null(group_var)) {
    base_aes <- ggplot2::aes(
      x    = !!rlang::sym(x_var),
      y    = !!rlang::sym(y_var),
      fill = !!rlang::sym(group_var)
    )
  } else {
    base_aes <- ggplot2::aes(
      x = !!rlang::sym(x_var),
      y = !!rlang::sym(y_var)
    )
  }

  p <- ggplot2::ggplot(data, base_aes)

  # --- Boxplot layer (SAS lines 116–137) -----------------------------------
  # SAS: display=(notches caps mean median fill outliers)
  #      fillattrs=(color=CXB9CFE7) outlineattrs=navy medianattrs=navy
  #      whiskerattrs=navy  meanattrs=(size=iqr_size+1)
  #      outlierattrs=(color=cx000000 symbol=square size=iqr_size)
  #      boxwidth=0.6  capshape=serif  groupdisplay=cluster
  p <- p + ggplot2::geom_boxplot(
    width          = phuse_sizes$cluster_width,
    fill           = phuse_colors$box_fill,
    colour         = phuse_colors$box_outline,
    notch          = show_notch,
    staplewidth    = 0.5,
    outlier.shape  = 15,
    outlier.colour = phuse_colors$iqr_outlier,
    outlier.size   = phuse_sizes$iqr_size,
    linewidth      = 0.4
  )

  # --- Mean overlay (SAS: display=(mean), meanattrs=(size=iqr_size+1)) -----
  # SAS default mean marker is diamond; ggplot2 shape 18 = filled diamond
  if (show_mean) {
    p <- p + ggplot2::stat_summary(
      fun    = mean,
      geom   = "point",
      shape  = 18,
      size   = phuse_sizes$mean_size,
      colour = phuse_colors$mean_marker,
      position = if (!rlang::is_null(group_var)) {
        ggplot2::position_dodge(width = phuse_sizes$cluster_width)
      } else {
        "identity"
      }
    )
  }

  # --- Normal-range outlier scatter (SAS lines 143–157) --------------------
  # SAS: scatterplot x=_XVAR y=_YOUTLIERS / markerattrs=(color=CXFF0000
  #      symbol=circlefilled size=iqr_size-1)  jitter=auto
  if (!rlang::is_null(outlier_var)) {
    outlier_aes <- ggplot2::aes(
      x = !!rlang::sym(x_var),
      y = !!rlang::sym(outlier_var)
    )
    p <- p + ggplot2::geom_point(
      data     = data,
      mapping  = outlier_aes,
      colour   = phuse_colors$nr_outlier,
      shape    = 16,
      size     = phuse_sizes$nr_outlier_size,
      position = if (!rlang::is_null(group_var)) {
        ggplot2::position_dodge(width = phuse_sizes$cluster_width)
      } else {
        ggplot2::position_jitter(width = 0.1)
      },
      na.rm    = TRUE,
      inherit.aes = FALSE
    )
  }

  # --- Reference lines (SAS lines 161–163) ---------------------------------
  # SAS: referenceline y=eval(coln(_REFLINES)) / lineattrs=(color=red)
  if (!rlang::is_null(ref_lines)) {
    p <- p + ggplot2::geom_hline(
      yintercept = ref_lines,
      colour     = phuse_colors$ref_line,
      linetype   = "solid",
      linewidth  = 0.5
    )
  }

  # --- Y-axis scale (SAS lines 86–93) -------------------------------------
  # SAS: linearopts=(viewmin=_YMIN viewmax=_YMAX
  #      tickvaluesequence=(start=_YMIN end=_YMAX increment=_YINCR))
  if (!rlang::is_null(y_min) && !rlang::is_null(y_max)) {
    breaks_vec <- if (!rlang::is_null(y_incr)) {
      seq(y_min, y_max, by = y_incr)
    } else {
      NULL
    }
    p <- p + ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = breaks_vec
    )
  }

  # --- Title (SAS lines 63–65: ENTRYTITLE _TITLE) --------------------------
  if (!rlang::is_null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  # --- Axis labels (SAS: label=_YLABEL) ------------------------------------
  p <- p + ggplot2::labs(
    y    = if (!rlang::is_null(y_label)) y_label else ggplot2::waiver(),
    fill = legend_title
  )

  # --- Apply PhUSE theme ---------------------------------------------------
  p <- p + theme_phuse()

  cli::cli_inform(c(
    "v" = paste0(
      "PhUSE boxplot created: x={.val {x_var}}, y={.val {y_var}}",
      if (!rlang::is_null(group_var)) paste0(", group={.val {group_var}}") else ""
    )
  ))

  p
}

# =========================================================================
# phuse_boxplot_stats_table()
# =========================================================================
# Computes per-group summary statistics matching the SAS bottom inner-margin
# AXISTABLE entries (SAS lines 174–208):
#   _N, _MEAN, _STD, _DATAMIN, _Q1, _MEDIAN, _Q3, _DATAMAX, _PVAL
# Uses SAS-compatible rounding via janitor::round_half_up() per Gate 2.
# =========================================================================

#' PhUSE Boxplot Summary Statistics Table
#'
#' Computes per-group descriptive statistics for the bottom inner-margin
#' annotation of a PhUSE boxplot, replacing the SAS GTL AXISTABLE elements.
#'
#' @param data A data frame (or tibble) containing the analysis data.
#' @param x_var Character string naming the x-axis (visit/timepoint) column.
#' @param y_var Character string naming the y-axis (measurement) column.
#' @param group_var Character string naming the grouping/treatment column, or
#'   \code{NULL} for ungrouped summaries.
#' @param stats Character vector of statistics to compute. Valid values:
#'   \code{"n"}, \code{"mean"}, \code{"sd"}, \code{"min"}, \code{"q1"},
#'   \code{"median"}, \code{"q3"}, \code{"max"}, \code{"pval"}.
#' @param digits Integer; number of decimal places for non-count statistics.
#'   Standard deviation is formatted to \code{digits + 1} to match SAS
#'   convention. Uses \code{janitor::round_half_up()} for SAS-compatible
#'   rounding (AAP Gate 2).
#'
#' @return A tibble with one row per x-group (and optionally per treatment
#'   group) containing the requested summary statistics.
#'
#' @examples
#' \dontrun{
#' stats_tbl <- phuse_boxplot_stats_table(
#'   data      = my_adlb,
#'   x_var     = "AVISITN",
#'   y_var     = "AVAL",
#'   group_var = "TRTA",
#'   digits    = 1
#' )
#' }
#'
#' @export
phuse_boxplot_stats_table <- function(data,
                                      x_var,
                                      y_var,
                                      group_var = NULL,
                                      stats     = c("n", "mean", "sd", "min",
                                                     "q1", "median", "q3",
                                                     "max"),
                                      digits    = 1) {

  # --- Input validation ----------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.cls {class(data)}}.")
  }
  if (!is.character(x_var) || length(x_var) != 1L) {
    cli::cli_abort("{.arg x_var} must be a single column name string.")
  }
  if (!is.character(y_var) || length(y_var) != 1L) {
    cli::cli_abort("{.arg y_var} must be a single column name string.")
  }
  required_cols <- c(x_var, y_var)
  if (!rlang::is_null(group_var)) {
    if (!is.character(group_var) || length(group_var) != 1L) {
      cli::cli_abort("{.arg group_var} must be a single column name string or NULL.")
    }
    required_cols <- c(required_cols, group_var)
  }
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Column(s) {.val {missing_cols}} not found in {.arg data}.")
  }

  valid_stats <- c("n", "mean", "sd", "min", "q1", "median", "q3", "max", "pval")
  invalid_stats <- setdiff(tolower(stats), valid_stats)
  if (length(invalid_stats) > 0L) {
    cli::cli_abort("Invalid statistic(s): {.val {invalid_stats}}. Valid options: {.val {valid_stats}}.")
  }
  stats <- tolower(stats)

  if (!is.numeric(digits) || digits < 0) {
    cli::cli_abort("{.arg digits} must be a non-negative integer.")
  }
  digits <- as.integer(digits)

  # --- Build grouping variables --------------------------------------------
  group_cols <- x_var
  if (!rlang::is_null(group_var)) {
    group_cols <- c(group_cols, group_var)
  }

  # --- Compute summary statistics per group --------------------------------
  # SAS AXISTABLE equivalents: N, Mean, Std Dev, Min, Q1, Median, Q3, Max
  # Rounding uses janitor::round_half_up() for SAS parity (AAP §0.7.2)
  summary_tbl <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n      = if ("n"      %in% stats) dplyr::n()
               else NA_integer_,
      mean   = if ("mean"   %in% stats)
                 janitor::round_half_up(mean(.data[[y_var]], na.rm = TRUE),
                                        digits = digits)
               else NA_real_,
      sd     = if ("sd"     %in% stats)
                 janitor::round_half_up(
                   stats::sd(.data[[y_var]], na.rm = TRUE),
                   digits = digits + 1L
                 )
               else NA_real_,
      min    = if ("min"    %in% stats)
                 janitor::round_half_up(min(.data[[y_var]], na.rm = TRUE),
                                        digits = digits)
               else NA_real_,
      q1     = if ("q1"     %in% stats)
                 janitor::round_half_up(
                   stats::quantile(.data[[y_var]], probs = 0.25,
                                   na.rm = TRUE, names = FALSE),
                   digits = digits
                 )
               else NA_real_,
      median = if ("median" %in% stats)
                 janitor::round_half_up(
                   stats::median(.data[[y_var]], na.rm = TRUE),
                   digits = digits
                 )
               else NA_real_,
      q3     = if ("q3"     %in% stats)
                 janitor::round_half_up(
                   stats::quantile(.data[[y_var]], probs = 0.75,
                                   na.rm = TRUE, names = FALSE),
                   digits = digits
                 )
               else NA_real_,
      max    = if ("max"    %in% stats)
                 janitor::round_half_up(max(.data[[y_var]], na.rm = TRUE),
                                        digits = digits)
               else NA_real_,
      .groups = "drop"
    )

  # --- Remove unrequested statistic columns --------------------------------
  keep_cols <- c(group_cols, intersect(stats, c("n", "mean", "sd", "min",
                                                 "q1", "median", "q3", "max")))
  summary_tbl <- summary_tbl[, keep_cols, drop = FALSE]

  # --- Handle p-value computation if requested (SAS _PVAL) ------------------
  # P-value is computed across treatment groups within each x-group level

  # using a Kruskal-Wallis test (non-parametric) when group_var is present.
  if ("pval" %in% stats && !rlang::is_null(group_var)) {
    pval_tbl <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(x_var))) |>
      dplyr::summarise(
        pval = tryCatch(
          janitor::round_half_up(
            stats::kruskal.test(
              .data[[y_var]] ~ factor(.data[[group_var]])
            )$p.value,
            digits = 4L
          ),
          error = function(e) NA_real_
        ),
        .groups = "drop"
      )
    summary_tbl <- dplyr::mutate(
      summary_tbl,
      pval = pval_tbl$pval[match(.data[[x_var]], pval_tbl[[x_var]])]
    )
  } else if ("pval" %in% stats && rlang::is_null(group_var)) {
    cli::cli_warn(c(
      "!" = "P-value requested but no {.arg group_var} supplied.",
      "i" = "P-value column set to NA. Provide a grouping variable for treatment comparison."
    ))
    summary_tbl <- dplyr::mutate(summary_tbl, pval = NA_real_)
  }

  cli::cli_inform(c(
    "v" = paste0(
      "Summary statistics computed for {.val {y_var}} by ",
      "{.val {paste(group_cols, collapse = ', ')}}."
    )
  ))

  summary_tbl
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Renamed from util_proc_template to util_ggplot_theme per AAP section 0.4.1
#    - SAS GTL template registration replaced by R ggplot2 theme function +
#      helper functions that are called per-plot rather than registered globally
#    - SAS point sizes (6, 7, 5) scaled to ggplot2 proportions (2, 3, 1.5)
#      because GTL units and ggplot2 mm-based sizes differ by roughly 3x
#    - SAS mm dimensions (260x170) preserved as defaults; consumers must pass
#      them to ggsave(width=, height=, units="mm") for correct output sizing
#    - SAS AXISTABLE bottom inner-margin statistics mapped to a standalone
#      summary-statistics tibble function; the SAS template renders these
#      inline whereas ggplot2 requires a separate table grob via gridExtra
#    - Box fill colour CXB9CFE7 mapped to hex "#B9CFE7" (identical value)
#    - SAS capshape=serif has no exact ggplot2 equivalent; staplewidth=0.5
#      provides the closest visual match
#    - Factor conversion of x_var ensures discrete-axis behaviour matching
#      SAS xaxisopts=(type=discrete)
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS SGRENDER box whisker calculation uses IQR * 1.5 fence; ggplot2
#      geom_boxplot uses the same default coef = 1.5 — no difference expected
#    - SAS notch calculation: 1.58 * IQR / sqrt(n); ggplot2 uses the same
#      formula — no difference expected for non-degenerate groups
#    - SAS notch at margins with very small n may produce slightly different
#      visual artefacts than ggplot2 due to rendering engine differences
#    - Summary statistics use janitor::round_half_up() for SAS parity;
#      sd is formatted to digits+1 matching SAS convention
# NO DIRECT R EQUIVALENT:
#    - SAS PROC TEMPLATE global registration → ggplot2 theme function called
#      per plot; the theme is not persisted in a session-level registry
#    - SAS BLOCKPLOT (top inner margin) → ggplot2 facet_grid/facet_wrap or
#      annotation text; no exact inline block-label mechanism exists
#    - SAS AXISTABLE (bottom inner margin) → separate tibble + gridExtra
#      tableGrob below the main plot; ggplot2 does not natively support
#      margin-embedded data tables
#    - SAS GTL conditional elements (IF EXISTS) → R conditional geom layers
#      controlled by NULL checks on function arguments
#    - SAS colorbands=even transparency=0.7 → ggplot2 panel.grid or manual
#      geom_rect strips would be needed for exact replication
# PACKAGE SELECTION RATIONALE:
#    - ggplot2: Mandated replacement for all SAS/GRAPH and GTL visualisation
#      per AAP section 0.7.1
#    - dplyr: Tidyverse data manipulation mandate per AAP section 0.8.1;
#      used in phuse_boxplot_stats_table() for group_by/summarise
#    - rlang: Tidy evaluation for dynamic column references (sym, .data,
#      ensym, is_null) — idiomatic tidyverse per AAP section 0.8.1
#    - cli: User-facing messages (cli_inform, cli_abort, cli_warn) replacing
#      SAS %PUT NOTE/ERROR/WARNING format
#    - janitor: SAS-compatible round_half_up() mandated by AAP section 0.7.2
#      for Gate 2 rounding audit compliance
# OPEN QUESTIONS:
#    - Should the bottom statistics table use a separate tableGrob
#      (gridExtra) or annotation_custom() for inline rendering?
#    - Exact replication of SAS colorbands=even on discrete x-axis may
#      require custom geom_rect shading — acceptable visual deviation?
#    - SAS legend shows group-specific box-fill colours when grouped; the
#      current implementation uses a single fill colour — should group-level
#      colour mapping be added via scale_fill_manual()?
# ============================================================
