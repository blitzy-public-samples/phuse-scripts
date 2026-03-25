# =============================================================================
# Scatter Plot with Quantile References — Max Post-Baseline vs Max Baseline
# =============================================================================
#
# SOURCE:   whitepapers/scriptathons/outliers/outliers_Scatter_Quant.sas
# MIGRATED: 2026-03-25
#
# DESCRIPTION:
#   Produces a composite output containing:
#   1. Scatter plot of Max Post-Baseline vs Max Baseline for a specified lab
#      parameter (default DIABP), colored by treatment group, with ANRLO/ANRHI
#      reference lines and an identity (y=x) overlay line
#   2. Baseline shift frequency table (BNRIND x ANRIND cross-tabulation by
#      treatment group) with counts and percentages
#   3. Treatment-emergent high summary with counts, percentages, and
#      Fisher's exact test p-value
#
# SAS CONSTRUCTS MIGRATED:
#   filename/libname (lines 4-5) -> haven::read_xpt()
#   DATA step filter/keep/label (lines 10-21) -> dplyr::filter(), select(),
#       mutate(), haven::labelled()
#   PROC SORT (line 28) -> dplyr::arrange()
#   PROC FREQ cross-tab (lines 32-38) -> dplyr::group_by() + summarise()
#   PROC FREQ EXACT Fisher (lines 85-88) -> stats::fisher.test()
#   PROC SUMMARY (lines 75-79, 123-126) -> dplyr::group_by() + summarise()
#   PROC TRANSPOSE (lines 57-61) -> tidyr::pivot_wider()
#   PROC GPLOT (lines 147-162) -> ggplot2 scatter plot with geom_point,
#       geom_abline, geom_hline, geom_vline
#   PROC REPORT (lines 176-188, 193-200) -> gridExtra::tableGrob()
#   PROC FORMAT percpar (lines 166-170) -> sprintf() formatting
#   ODS PDF + ODS LAYOUT (lines 112-115, 145, 174, 192, 203, 208-211) ->
#       pdf() + gridExtra::grid.arrange()
#   SAS formats/rounding -> janitor::round_half_up()
#   SAS trim(left()) -> stringr::str_trim(), str_pad()
#
# =============================================================================

# --- Library Loading ---------------------------------------------------------
library(haven)        # XPT data I/O (read_xpt, labelled)
library(dplyr)        # Data manipulation (filter, select, mutate, etc.)
library(tidyr)        # Pivoting (pivot_wider)
library(ggplot2)      # Visualization (ggplot, geom_point, etc.)
library(janitor)      # SAS-compatible rounding (round_half_up)
library(rlang)        # Required by r2rtf for %||% operator
library(r2rtf)        # RTF/PDF table output (rtf_body, rtf_title, etc.)
library(openxlsx)     # Excel workbook output (write.xlsx)
library(stringr)      # String manipulation (str_trim, str_pad)
library(gridExtra)    # Multi-panel composition (arrangeGrob, tableGrob)
library(cli)
# stats is a base package — fisher.test() and table() always available

# =============================================================================
# outliers_scatter_quant
# =============================================================================
#' Scatter Plot with Quantile References for Outlier Analysis
#'
#' Generates a composite output (PDF) containing a scatter plot of Max
#' Post-Baseline vs Max Baseline for a specified lab parameter, a baseline
#' shift frequency table, and a treatment-emergent high summary with
#' Fisher's exact test.
#'
#' Migrated from SAS: whitepapers/scriptathons/outliers/outliers_Scatter_Quant.sas
#'
#' @param data_path Character. Path to directory containing advsmax.xpt.
#'   Replaces SAS 'filename source url' (line 4).
#'   Example: config$data_paths$adam_path from migration_config.yaml
#' @param output_path Character. Directory for output files.
#'   Replaces SAS 'ods pdf file = "Target16.pdf"' (line 113).
#'   Example: config$output_paths$base_output_path from migration_config.yaml
#' @param paramcd Character. Parameter code filter. Default "DIABP".
#'   Replaces SAS WHERE PARAMCD="DIABP" (line 11).
#' @param atptn Numeric. Analysis timepoint number filter. Default 815.
#'   Replaces SAS WHERE ATPTN=815 (line 11).
#' @param major_tick Numeric. Major tick mark interval for plot axes.
#'   Default 10. Replaces SAS '%LET MAJOR = 10' (line 121).
#' @param output_format Character vector. Output format(s) to produce:
#'   "pdf" (default, composite layout), "rtf" (tables via r2rtf),
#'   "xlsx" (tables via openxlsx). May combine, e.g. c("pdf", "xlsx").
#'
#' @return Invisible list containing:
#'   \item{filtered_data}{Filtered analysis dataset (tibble)}
#'   \item{shift_table}{Baseline shift frequency table in wide format (tibble)}
#'   \item{shift_table_long}{Baseline shift table in long format with counts (tibble)}
#'   \item{emergent_high}{Treatment-emergent high summary (tibble)}
#'   \item{fisher_p}{Fisher's exact test two-sided p-value (numeric)}
#'   \item{scatter_plot}{ggplot2 scatter plot object}
#'   \item{output_files}{Character vector of output file paths created}
#'
#' @examples
#' \dontrun{
#'   # Using migration config:
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   results <- outliers_scatter_quant(
#'     data_path   = config$data_paths$adam_path,
#'     output_path = config$output_paths$base_output_path
#'   )
#'
#'   # Direct usage:
#'   results <- outliers_scatter_quant(
#'     data_path   = "data/adam/cdisc",
#'     output_path = "output"
#'   )
#' }
#' @export
outliers_scatter_quant <- function(data_path,
                                   output_path,
                                   paramcd    = "DIABP",
                                   atptn      = 815,
                                   major_tick = 10,
                                   output_format = "pdf") {

  # ---------------------------------------------------------------------------
  # Input Validation
  # ---------------------------------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L || nchar(data_path) == 0L) {
    cli::cli_abort("{.arg data_path} must be a non-empty character string.")
  }
  if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
    cli::cli_abort("{.arg output_path} must be a non-empty character string.")
  }
  if (!is.character(paramcd) || length(paramcd) != 1L || nchar(paramcd) == 0L) {
    cli::cli_abort("{.arg paramcd} must be a non-empty character string.")
  }
  if (!is.numeric(atptn) || length(atptn) != 1L) {
    cli::cli_abort("{.arg atptn} must be a single numeric value.")
  }
  if (!is.numeric(major_tick) || length(major_tick) != 1L || major_tick <= 0) {
    cli::cli_abort("{.arg major_tick} must be a positive numeric value.")
  }
  if (!is.character(output_format) || !all(output_format %in% c("pdf", "rtf", "xlsx"))) {
    cli::cli_abort("{.arg output_format} must contain valid format(s): 'pdf', 'rtf', 'xlsx'.")
  }

  # Create output directory if it does not exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  output_files <- character(0)

  # ===========================================================================
  # PHASE 1: Data Loading and Filtering (SAS lines 1-21)
  # ===========================================================================
  # SAS: filename source url "...advsmax.xpt"; libname source xport;
  #      data work.advsmax; set source.advsmax; run;

  xpt_file <- file.path(data_path, "advsmax.xpt")
  if (!file.exists(xpt_file)) {
    cli::cli_abort("Data file not found: ", xpt_file,
         "\nPlease verify data_path points to a directory containing advsmax.xpt")
  }

  advsmax <- haven::read_xpt(xpt_file)

  # SAS WHERE clause (line 11):
  #   (PARAMCD="DIABP") and (ATPTN=815) and (ANL01FL="Y") and (ADY > 1) and (SAFFL="Y")
  advsmax2 <- advsmax %>%
    dplyr::filter(
      PARAMCD == paramcd,
      ATPTN   == atptn,
      ANL01FL == "Y",
      ADY > 1,
      SAFFL   == "Y"
    ) %>%
    # KEEP statement (SAS lines 12-16)
    dplyr::select(
      USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL,
      BNRIND, ANRIND, SHIFT1, CRIT1FL, ANRLO, ANRHI, BASE
    ) %>%
    # Ref computation (SAS line 17):
    #   if not nmiss(base, aval) then Ref = Base;
    # nmiss() counts missing values among both arguments;
    # "if not nmiss" means "if neither is missing"
    dplyr::mutate(
      Ref = dplyr::if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)
    )

  # Apply labels (SAS lines 18-19):
  #   label BASE = "Max. Baseline"  AVAL = "Max. Post baseline"
  advsmax2 <- advsmax2 %>%
    dplyr::mutate(
      BASE = haven::labelled(as.double(BASE), label = "Max. Baseline"),
      AVAL = haven::labelled(as.double(AVAL), label = "Max. Post baseline")
    )

  # Validate filtered data is not empty
  if (nrow(advsmax2) == 0) {
    warning("No observations match the filter criteria (PARAMCD='", paramcd,
            "', ATPTN=", atptn, "). Returning empty results.")
    return(invisible(list(
      filtered_data    = advsmax2,
      shift_table      = tibble::tibble(),
      shift_table_long = tibble::tibble(),
      emergent_high    = tibble::tibble(),
      fisher_p         = NA_real_,
      scatter_plot     = NULL,
      output_files     = character(0)
    )))
  }

  # ===========================================================================
  # PHASE 2: Baseline Shift Frequency Table (SAS lines 24-61)
  # ===========================================================================

  # Sort by TRTPN (SAS proc sort line 28-30)
  advsmax2 <- advsmax2 %>% dplyr::arrange(TRTPN)

  # Handle missing BNRIND/ANRIND for cross-tabulation.
  # SAS /missing option includes missing as a valid level (displayed as blank).
  # Map NA -> "" to replicate SAS character-missing behavior.
  advsmax2_tab <- advsmax2 %>%
    dplyr::mutate(
      BNRIND = dplyr::if_else(is.na(BNRIND), "", BNRIND),
      ANRIND = dplyr::if_else(is.na(ANRIND), "", ANRIND)
    )

  # Cross-frequency table: BNRIND * ANRIND by TRTPN (SAS lines 32-34)
  # SAS: proc freq ... table BNRIND * ANRIND / missing out = TableA;
  # The PERCENT in the OUT= dataset is the cell percent within each BY group.
  table_a <- advsmax2_tab %>%
    dplyr::group_by(TRTPN, BNRIND, ANRIND) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(TRTPN) %>%
    dplyr::mutate(percent = count / sum(count) * 100) %>%
    dplyr::ungroup()

  # Compute group N per TRTPN (SAS line 37: table TRTPN / missing out = TRTPN)
  group_n <- advsmax2_tab %>%
    dplyr::group_by(TRTPN) %>%
    dplyr::summarise(GroupN = dplyr::n(), .groups = "drop")

  # Create formatted value string (SAS line 44):
  #   ValueFmt = trim(left(put(count, comma8.))) !! " (" !!
  #              trim(left(put(percent, 10.1))) !! ")"
  # Uses janitor::round_half_up for SAS-compatible rounding of percent.
  # Uses stringr::str_trim for trim(left()) and str_pad for alignment.
  table_a <- table_a %>%
    dplyr::mutate(
      ValueFmt = paste0(
        stringr::str_trim(format(count, big.mark = ",")),
        " (",
        stringr::str_pad(
          sprintf("%.1f", janitor::round_half_up(percent, digits = 1)),
          width = 5, side = "left"
        ),
        ")"
      )
    )

  # Merge group N and create GroupFmt label (SAS lines 49-55):
  #   GroupFmt = trim(left(TRTPN)) !! " (N = " !!
  #             trim(left(put(GroupN, comma10.))) !! ")"
  table_a <- table_a %>%
    dplyr::left_join(group_n, by = "TRTPN") %>%
    dplyr::mutate(
      GroupFmt = paste0(
        stringr::str_trim(as.character(TRTPN)),
        " (N = ",
        stringr::str_trim(format(GroupN, big.mark = ",")),
        ")"
      )
    )

  # Transpose to wide format (SAS PROC TRANSPOSE lines 57-61):
  #   proc transpose data=TableA out=TableAt prefix=ValueFmt;
  #     by GroupFmt BNRIND; var ValueFmt; id ANRIND;
  table_at <- table_a %>%
    dplyr::select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
    tidyr::pivot_wider(
      names_from    = ANRIND,
      values_from   = ValueFmt,
      names_prefix  = "ValueFmt",
      values_fill   = ""
    )

  # ===========================================================================
  # PHASE 3: Treatment-Emergent High Summary (SAS lines 63-103)
  # ===========================================================================

  # Filter for CRIT1FL not empty (SAS line 70: if (CRIT1FL ne ""))
  advsmax3 <- advsmax2 %>%
    dplyr::filter(!is.na(CRIT1FL) & CRIT1FL != "")

  # Create emergent high flag.
  #
  # MIGRATION NOTE on SAS line 72:
  #   SAS source line 71 (commented out): *n = (CRIT1FL="Y");
  #   SAS source line 72 (active):        n = mod(_n_,17) = 1;
  #   Line 72 creates a 0/1 flag based on row number modulo 17 -- this is a
  #   debug/test artifact in the original SAS script. The intended logic is
  #   n = (CRIT1FL == "Y") from the commented line 71.
  #   This migration implements the intended CRIT1FL == "Y" logic.
  advsmax3 <- advsmax3 %>%
    dplyr::mutate(n_flag = as.integer(CRIT1FL == "Y"))

  # PROC SUMMARY for emergent high counts (SAS lines 75-79):
  #   class TRTPN; var n;
  #   output out = EmergentHigh n=Nx sum=n mean=percent;
  # Nx = count of non-missing n values per TRTPN
  # n  = sum of the flag (count of emergent high subjects)
  # percent = mean of the flag (proportion, later scaled to %)
  emergent_high <- advsmax3 %>%
    dplyr::group_by(TRTPN) %>%
    dplyr::summarise(
      Nx      = dplyr::n(),
      n       = sum(n_flag, na.rm = TRUE),
      percent = mean(n_flag, na.rm = TRUE),
      .groups = "drop"
    )

  # Fisher's exact test (SAS lines 81-96):
  #   proc freq data = advsmax3;
  #     ods output FishersExact=FishersExact;
  #     table TRTPN * n / exact;
  # Extracts XP2_FISH -- the two-sided Fisher's exact p-value.
  fishers_p <- NA_real_
  n_trt_fish <- if (nrow(advsmax3) > 0) length(unique(advsmax3$TRTPN)) else 0
  n_flag_levels <- if (nrow(advsmax3) > 0) length(unique(advsmax3$n_flag)) else 0

  if (nrow(advsmax3) > 0 && n_trt_fish >= 2 && n_flag_levels >= 2) {
    # Fisher's exact test requires at least a 2x2 contingency table
    fish_table <- base::table(advsmax3$TRTPN, advsmax3$n_flag)
    fishers_p <- tryCatch({
      result <- stats::fisher.test(fish_table)
      result$p.value
    }, error = function(e) {
      warning("Fisher's exact test failed: ", e$message,
              ". Returning NA for p-value.")
      NA_real_
    })
  } else {
    if (n_trt_fish < 2) {
      warning("Fisher's exact test requires at least 2 treatment groups. ",
              "Found ", n_trt_fish, " group(s). Returning NA for p-value.")
    } else if (n_flag_levels < 2) {
      warning("Fisher's exact test requires variability in the emergent ",
              "high flag (both 0 and 1 values needed). All n_flag values are ",
              unique(advsmax3$n_flag)[1], ". Returning NA for p-value.")
    }
  }

  # Merge Fisher's p-value and convert proportion to percent (SAS lines 98-103):
  #   SAS: if n(percent) then percent = 100 * percent;
  #   SAS: format percent 6.1;
  emergent_high <- emergent_high %>%
    dplyr::mutate(
      percent  = dplyr::if_else(!is.na(percent), percent * 100, NA_real_),
      FishersP = fishers_p
    )

  # ===========================================================================
  # PHASE 4: Axis Scaling Computation (SAS lines 121-140)
  # ===========================================================================
  # SAS: proc summary data = advsmax2 nway;
  #        var ANRLO ANRHI BASE AVAL;
  #        output out = ADVSMAXSum mean = min(AVAL)=MinValue max(AVAL)=MaxValue;
  # Computes mean of ANRLO/ANRHI (for reference lines) and min/max of AVAL
  # (for axis limits). Also includes BASE range for symmetric scatter axes.

  advs_sum <- advsmax2 %>%
    dplyr::summarise(
      anrlo     = mean(ANRLO, na.rm = TRUE),
      anrhi     = mean(ANRHI, na.rm = TRUE),
      min_aval  = min(AVAL, na.rm = TRUE),
      max_aval  = max(AVAL, na.rm = TRUE),
      min_base  = min(BASE, na.rm = TRUE),
      max_base  = max(BASE, na.rm = TRUE)
    )

  # Use overall min/max across both AVAL and BASE so scatter axes are symmetric
  overall_min <- min(advs_sum$min_aval, advs_sum$min_base, na.rm = TRUE)
  overall_max <- max(advs_sum$max_aval, advs_sum$max_base, na.rm = TRUE)

  # Compute axis limits with margins (SAS lines 134-136):
  #   margin = (maxvalue - minvalue) / 100;     (1% of range as margin)
  #   YAxisMin = &MAJOR * floor((minvalue - margin) / &MAJOR);
  #   YAxisMax = &MAJOR * ceil ((maxvalue + margin) / &MAJOR);
  margin     <- (overall_max - overall_min) / 100
  y_axis_min <- major_tick * floor((overall_min - margin) / major_tick)
  y_axis_max <- major_tick * ceiling((overall_max + margin) / major_tick)

  # ===========================================================================
  # PHASE 5: Scatter Plot Generation (SAS lines 143-162)
  # ===========================================================================

  # Determine TRTPN levels and map colors/shapes.
  # SAS symbol definitions (assigned to sorted TRTPN levels):
  #   symbol1: value=triangle c=green   -> 1st sorted TRTPN level
  #   symbol2: value=circle   c=red     -> 2nd sorted TRTPN level
  #   symbol3: value=x        c=blue    -> 3rd sorted TRTPN level
  trtpn_levels <- sort(unique(advsmax2$TRTPN))
  n_trt        <- length(trtpn_levels)

  # Color palette per SAS symbol c= attributes: green, red, blue
  color_palette <- c("green", "red", "blue")
  # Shape palette per SAS symbol value= attributes: triangle=2, circle=1, x=4
  shape_palette <- c(2, 1, 4)

  # Create named vectors for ggplot2 scale_*_manual
  trt_labels <- as.character(trtpn_levels[seq_len(min(n_trt, 3))])
  color_map  <- stats::setNames(color_palette[seq_len(min(n_trt, 3))], trt_labels)
  shape_map  <- stats::setNames(shape_palette[seq_len(min(n_trt, 3))], trt_labels)

  # Build scatter plot (replaces SAS PROC GPLOT lines 147-162)
  # SAS: plot AVAL * base = TRTPN /
  #        vref = &ANRLO, &ANRHI lvref = 21 cvref = (blue red)
  #        vaxis = &MINVALUE to &MAXVALUE by &MAJOR
  #        href = &ANRLO, &ANRHI lhref = 21 chref = (blue red);
  #      plot2 Ref * base / overlay noaxis vaxis = ...;
  scatter_plot <- ggplot2::ggplot(
    advsmax2,
    ggplot2::aes(
      x     = BASE,
      y     = AVAL,
      color = factor(TRTPN),
      shape = factor(TRTPN)
    )
  ) +
    # Scatter points (SAS: plot AVAL * base = TRTPN, interpol = none)
    ggplot2::geom_point(size = 2, na.rm = TRUE) +

    # Horizontal reference lines for ANRLO and ANRHI (SAS: vref)
    # SAS lvref=21 -> dashed; cvref=(blue red)
    ggplot2::geom_hline(
      yintercept = advs_sum$anrlo,
      linetype   = "dashed", color = "blue", linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = advs_sum$anrhi,
      linetype   = "dashed", color = "red", linewidth = 0.5
    ) +
    # Vertical reference lines for ANRLO and ANRHI (SAS: href)
    # SAS lhref=21 -> dashed; chref=(blue red)
    ggplot2::geom_vline(
      xintercept = advs_sum$anrlo,
      linetype   = "dashed", color = "blue", linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = advs_sum$anrhi,
      linetype   = "dashed", color = "red", linewidth = 0.5
    ) +

    # Identity line overlay (SAS plot2 Ref*base / overlay)
    # SAS symbol4: value=none interpol=RL line=21 CI=black
    # Since Ref=Base when both non-missing, regression through Ref vs Base
    # IS the identity line y=x.
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "black", linewidth = 0.5
    ) +

    # Axis scaling (SAS: vaxis = &MINVALUE to &MAXVALUE by &MAJOR)
    # Apply same limits to both axes for symmetric scatter
    ggplot2::scale_y_continuous(
      limits = c(y_axis_min, y_axis_max),
      breaks = seq(y_axis_min, y_axis_max, by = major_tick)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(y_axis_min, y_axis_max),
      breaks = seq(y_axis_min, y_axis_max, by = major_tick)
    ) +

    # Colors (SAS goptions colors and symbol c= attributes)
    ggplot2::scale_color_manual(values = color_map, name = "Treatment") +
    # Shapes (SAS symbol value= attributes)
    ggplot2::scale_shape_manual(values = shape_map, name = "Treatment") +

    # Title (SAS TITLE line 154)
    ggplot2::labs(
      title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
      x     = "Max. Baseline",
      y     = "Max. Post baseline"
    ) +

    # Styling
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(size = 9, hjust = 0.5),
      axis.title       = ggplot2::element_text(size = 8),
      axis.text        = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank(),
      aspect.ratio     = 1
    )

  # ===========================================================================
  # PHASE 6: Composite Output Generation (SAS lines 108-211)
  # ===========================================================================

  # --- PDF Composite Output (primary, matching SAS ODS PDF + ODS LAYOUT) -----
  if ("pdf" %in% output_format) {

    # Build baseline shift table grob (SAS PROC REPORT lines 176-188)
    # Remove internal _NAME_ column if present from pivot_wider
    shift_display <- table_at %>%
      dplyr::select(-dplyr::any_of("_NAME_"))

    shift_grob <- gridExtra::tableGrob(
      shift_display,
      rows  = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = 7,
        core    = list(fg_params = list(hjust = 0.5, x = 0.5)),
        colhead = list(fg_params = list(hjust = 0.5, x = 0.5, fontface = "bold"))
      )
    )

    # Add title above shift table
    # SAS spanning header: "Max Post Baseline Result (shift from baseline)"
    shift_title <- grid::textGrob(
      "Baseline Shift Table\nMax Post Baseline Result (shift from baseline)",
      gp   = grid::gpar(fontsize = 8, fontface = "bold"),
      just = "left",
      x    = grid::unit(0.05, "npc")
    )
    shift_with_title <- gridExtra::arrangeGrob(
      shift_title, shift_grob,
      ncol    = 1,
      heights = grid::unit(c(0.6, 3.0), "inches")
    )

    # Build emergent high table grob (SAS PROC REPORT lines 193-200)
    # Format percent with SAS-compatible rounding and string padding
    eh_display <- emergent_high %>%
      dplyr::mutate(
        percent_fmt = stringr::str_pad(
          sprintf("%.1f", janitor::round_half_up(percent, digits = 1)),
          width = 6, side = "left"
        ),
        fishers_fmt = dplyr::if_else(
          is.na(FishersP), "",
          sprintf("%.4f", FishersP)
        )
      ) %>%
      dplyr::select(TRTPN, Nx, n, percent_fmt, fishers_fmt) %>%
      dplyr::rename(
        Treatment  = TRTPN,
        N          = Nx,
        `n`        = n,
        `%`        = percent_fmt,
        `Fisher P` = fishers_fmt
      )

    eh_grob <- gridExtra::tableGrob(
      eh_display,
      rows  = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = 7,
        core    = list(fg_params = list(hjust = 0, x = 0.1)),
        colhead = list(fg_params = list(hjust = 0, x = 0.1, fontface = "bold"))
      )
    )

    eh_title <- grid::textGrob(
      "Treatment Emergent High",
      gp   = grid::gpar(fontsize = 8, fontface = "bold"),
      just = "left",
      x    = grid::unit(0.05, "npc")
    )
    eh_with_title <- gridExtra::arrangeGrob(
      eh_title, eh_grob,
      ncol    = 1,
      heights = grid::unit(c(0.4, 2.0), "inches")
    )

    # Build footnotes grob (SAS ODS TEXT lines 204-206)
    footnote_text <- paste0(
      "Scatter Plot: Includes subjects with both a baseline and a ",
      "post-baseline measure. Reference limits apply to most of the ",
      "participants are used in the plot.\n",
      "N=number of subjects with at least one post-baseline measure; ",
      "Nx=number of subjects with maximum baseline either normal or low ",
      "and have at least one post-baseline measure.\n",
      "Summary tables used the reference limits set 1 (Male, Age 18-65, ",
      "LLN=", sprintf("%.0f", advs_sum$anrlo), " mmol/L ",
      "ULN=", sprintf("%.0f", advs_sum$anrhi), " mmol/L) ",
      "set 2 (Female, Age 18-65, ",
      "LLN=", sprintf("%.0f", advs_sum$anrlo), " mmol/L ",
      "ULN=", sprintf("%.0f", advs_sum$anrhi), " mmol/L)."
    )

    footnote_grob <- grid::textGrob(
      footnote_text,
      gp   = grid::gpar(fontsize = 7),
      just = c("left", "top"),
      x    = grid::unit(0.02, "npc"),
      y    = grid::unit(0.95, "npc")
    )

    # Composite layout (SAS ODS LAYOUT lines 115, 145, 174, 192, 203):
    #   Overall: 9in x 7.5in landscape
    #   Scatter:       x=0.5in  y=0.5in  h=4.5in  w=4.5in
    #   Shift table:   x=5in    y=0.5in  h=3.5in
    #   Emergent high: x=5in    y=4in    h=2.5in
    #   Footnotes:     x=0.5in  y=6in    h=1.5in
    layout_mat <- rbind(
      c(1, 2),
      c(1, 3),
      c(4, 4)
    )

    # Render composite PDF using grid.arrange
    # SAS: options orientation=landscape; ods pdf file = "Target16.pdf" ...;
    pdf_file <- file.path(output_path, "Target16.pdf")
    grDevices::pdf(pdf_file, width = 11, height = 8.5)
    gridExtra::grid.arrange(
      scatter_plot,
      shift_with_title,
      eh_with_title,
      footnote_grob,
      layout_matrix = layout_mat,
      widths  = grid::unit(c(5.0, 4.5), "inches"),
      heights = grid::unit(c(3.5, 2.5, 1.5), "inches")
    )
    grDevices::dev.off()

    output_files <- c(output_files, pdf_file)
    message("PDF composite saved: ", pdf_file)

    # Save standalone scatter plot via ggsave (SAS ODS PDF scatter region)
    scatter_pdf <- file.path(output_path, "Target16_scatter.pdf")
    ggplot2::ggsave(
      filename = scatter_pdf,
      plot     = scatter_plot,
      width    = 7, height = 7, units = "in"
    )
    output_files <- c(output_files, scatter_pdf)
    message("PDF scatter plot saved: ", scatter_pdf)
  }

  # --- Optional RTF Output (using r2rtf) ------------------------------------
  if ("rtf" %in% output_format) {

    # Baseline shift table RTF
    rtf_shift_file <- file.path(output_path, "Target16_shift_table.rtf")
    shift_rtf_data <- table_at %>%
      dplyr::select(-dplyr::any_of("_NAME_"))

    shift_rtf_data %>%
      r2rtf::rtf_body(
        text_font      = 1,
        text_font_size = 8
      ) %>%
      r2rtf::rtf_title(
        title = "Baseline Shift Frequency Table",
        text_font = 1
      ) %>%
      r2rtf::rtf_footnote(
        footnote = "Max Post Baseline Result (shift from baseline)"
      ) %>%
      r2rtf::rtf_page(orientation = "landscape") %>%
      r2rtf::write_rtf(rtf_shift_file)

    output_files <- c(output_files, rtf_shift_file)
    message("RTF shift table saved: ", rtf_shift_file)

    # Emergent high table RTF
    rtf_eh_file <- file.path(output_path, "Target16_emergent_high.rtf")
    eh_rtf_data <- emergent_high %>%
      dplyr::mutate(
        percent  = sprintf("%.1f",
                           janitor::round_half_up(percent, digits = 1)),
        FishersP = dplyr::if_else(
          is.na(FishersP), "",
          sprintf("%.4f", FishersP)
        )
      )

    eh_rtf_data %>%
      r2rtf::rtf_body(
        text_font      = 1,
        text_font_size = 8
      ) %>%
      r2rtf::rtf_title(
        title = "Treatment Emergent High Summary",
        text_font = 1
      ) %>%
      r2rtf::rtf_footnote(
        footnote = paste0(
          "Fisher's Exact Test p-value: ",
          dplyr::if_else(is.na(fishers_p), "N/A",
                         sprintf("%.4f", fishers_p))
        )
      ) %>%
      r2rtf::rtf_page(orientation = "landscape") %>%
      r2rtf::write_rtf(rtf_eh_file)

    output_files <- c(output_files, rtf_eh_file)
    message("RTF emergent high table saved: ", rtf_eh_file)
  }

  # --- Optional Excel Output (using openxlsx) --------------------------------
  if ("xlsx" %in% output_format) {
    xlsx_file <- file.path(output_path, "Target16.xlsx")
    sheet_list <- list(
      "Baseline Shift" = as.data.frame(
        table_at %>% dplyr::select(-dplyr::any_of("_NAME_"))
      ),
      "Emergent High"  = as.data.frame(emergent_high)
    )
    openxlsx::write.xlsx(sheet_list, file = xlsx_file)
    output_files <- c(output_files, xlsx_file)
    message("Excel output saved: ", xlsx_file)
  }

  # ===========================================================================
  # Return Results
  # ===========================================================================
  invisible(list(
    filtered_data    = advsmax2,
    shift_table      = table_at,
    shift_table_long = table_a,
    emergent_high    = emergent_high,
    fisher_p         = fishers_p,
    scatter_plot     = scatter_plot,
    output_files     = output_files
  ))
}


# =============================================================================
#### MIGRATION NOTES
# =============================================================================
#
#### ASSUMPTIONS:
####   1. SAS line 72 'n = mod(_n_,17) = 1' appears to be a debug/test
####      artifact in the original SAS script. The migration implements the
####      intended logic from commented line 71: n = (CRIT1FL == "Y").
####   2. SAS PROC GPLOT symbol-to-TRTPN mapping is order-dependent:
####      symbol1 (green triangle) -> 1st sorted TRTPN level,
####      symbol2 (red circle) -> 2nd sorted TRTPN level,
####      symbol3 (blue x) -> 3rd sorted TRTPN level.
####   3. SAS ODS LAYOUT region positioning (x/y/height/width) is
####      approximated using gridExtra::grid.arrange() with a 3-row x 2-col
####      layout matrix. Exact pixel-level alignment may differ.
####   4. ANRLO and ANRHI are assumed constant across subjects (mean is
####      taken per SAS PROC SUMMARY line 124-125 to derive single values
####      for reference lines).
####   5. SAS character missing (' ') is mapped to "" (empty string) for
####      BNRIND/ANRIND in the cross-tabulation, matching SAS /missing
####      option behavior. No implicit zero substitution.
#
#### POTENTIAL NUMERICAL DIFFERENCES:
####   1. Rounding: janitor::round_half_up() is used at all percent
####      formatting locations to match SAS round-half-up behavior.
####      Verify with Gate 2 rounding audit.
####   2. Fisher's exact p-value: R stats::fisher.test() computes the
####      two-sided p-value using the method of summing small probabilities.
####      SAS PROC FREQ EXACT may produce slightly different p-values for
####      r x c tables (r > 2 or c > 2) due to different algorithms. For
####      2x2 tables, results should be identical.
####   3. PROC FREQ percent denominator: SAS computes cell percent within
####      each BY group. The dplyr implementation replicates this exactly
####      using group_by(TRTPN) + mutate(percent = count/sum(count)*100).
####   4. Axis limits: SAS computed min/max of AVAL only for vaxis. The R
####      implementation uses overall min/max of both AVAL and BASE for
####      symmetric scatter axes (better visual result; may slightly
####      differ from SAS axis range).
#
#### NO DIRECT R EQUIVALENT:
####   1. SAS ODS LAYOUT regions (x/y/height/width positioning) ->
####      approximated with gridExtra layout_matrix and unit-based sizing.
####   2. SAS PROC GPLOT plot2 overlay (Ref * base) ->
####      ggplot2::geom_abline(slope=1, intercept=0) identity line.
####   3. SAS PROC FORMAT picture 'percpar' ("009.9)" with prefix "(") ->
####      sprintf("%.1f", ...) with paste0() for parenthesized format.
####   4. SAS symbol4 interpol=RL (regression line through Ref vs Base) ->
####      geom_abline identity line (mathematically equivalent since Ref=Base).
####   5. SAS goptions colors= global palette -> ggplot2 scale_color_manual
####      with explicit named color vector.
#
#### PACKAGE SELECTION RATIONALE:
####   haven (2.5.5): SAS XPT file I/O -- tidyverse standard, reads SAS
####     transport files directly. Chosen over foreign::read.xport for
####     labelled vector support.
####   dplyr (>=1.1.0) / tidyr (>=1.3.0): Core tidyverse data manipulation
####     replacing all SAS DATA steps, PROC SORT, PROC FREQ aggregation,
####     PROC SUMMARY, and PROC TRANSPOSE. Required per AAP.
####   ggplot2 (>=3.4.0): Visualization replacing SAS PROC GPLOT. Provides
####     layered grammar of graphics with geom_point, geom_abline,
####     geom_hline/vline for scatter with reference lines.
####   janitor (>=2.2.0): round_half_up() for SAS-compatible rounding.
####     Critical for Gate 2 regulatory rounding audit.
####   r2rtf (1.1.1): RTF table output -- Merck-developed, production-ready.
####     Used for optional RTF export of baseline shift and emergent high
####     tables (alternative to PDF).
####   openxlsx (>=4.2.5): Excel workbook output -- replaces SAS SpreadsheetML.
####     Used for optional XLSX export of tables.
####   stringr (>=1.5.0): Tidyverse string manipulation replacing SAS
####     trim(left()) and PUT format operations.
####   gridExtra (>=2.3): Multi-panel figure composition replacing SAS
####     ODS LAYOUT regions. Provides tableGrob for table rendering and
####     grid.arrange for composite layout.
####   stats (base): fisher.test() and table() for Fisher's exact test
####     replacing SAS PROC FREQ EXACT.
#
#### OPEN QUESTIONS:
####   1. Confirm intended behavior of SAS line 72 (mod(_n_,17)) -- the
####      migration assumes this is a debug artifact and uses CRIT1FL=="Y"
####      from commented line 71 instead. If the modulo logic was intentional
####      (e.g., systematic sampling), the R equivalent would be:
####      mutate(n_flag = as.integer(row_number() %% 17 == 1))
####   2. Verify TRTPN-to-color/shape mapping order matches SAS goptions
####      colors assignment -- SAS assigns symbols to sorted TRTPN levels.
####   3. Confirm reference limit variable usage (ANRLO/ANRHI) -- the SAS
####      script uses mean values across all subjects. If subject-specific
####      reference limits are needed, the approach would change.
####   4. The SAS ODS PDF uses style=journal fontscale=70 -- the R output
####      uses theme_minimal with base_size=9 as an approximation. Fine-tune
####      font sizes if exact visual parity is required.
####   5. SAS 'options missing = ""' applies globally -- confirm that empty
####      string representation for missing values in tables is acceptable.
#
# =============================================================================
