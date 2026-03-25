# =============================================================================
# Program Name  : pk_mean_conc.R
# Program Type  : Figure
# Original SAS  : whitepapers/scriptathons/pk/pk_mean_conc.sas
# Author        : Adrienne M Bonwick (SAS original, 17 March 2014)
# R Migration   : Blitzy Platform (2026)
# =============================================================================
# Purpose:
#   R migration of PhUSE Target 13 / Figure 7.7 — Mean PK Concentration-Time
#   Profiles. Produces a two-panel figure (linear + semilogarithmic views)
#   of mean concentration vs time with optional SD error bars, grouped by
#   treatment arm (TRTAN).
#
# PhUSE White Paper Reference:
#   http://www.phusewiki.org/wiki/images/e/ed/PhUSE_CSS_WhitePaper_PK_final_25March2014.pdf
#
# Source SAS File:
#   whitepapers/scriptathons/pk/pk_mean_conc.sas (GCR r135)
#
# Configuration:
#   Callers should load config/migration_config.yaml and construct
#   data_path from config$data_paths$adam_path and output_path from
#   config$output_paths$figure_output_path. Example:
#     config <- yaml::read_yaml("config/migration_config.yaml")
#     pk_mean_conc(
#       data_path   = file.path(config$data_paths$adam_path, "adpc.xpt"),
#       PCTESTCD    = "ANAL1",
#       Meantype    = "A",
#       SD          = "BOTH",
#       output_path = config$output_paths$figure_output_path
#     )
# =============================================================================

# --- Required Packages --------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)
library(janitor)
library(cli)

# ==============================================================================
# pk_mean_conc
# ==============================================================================
#' Generate Mean PK Concentration-Time Profiles (PhUSE Target 13 / Figure 7.7)
#'
#' Reads a CDISC ADaM ADPC XPT dataset, computes arithmetic or geometric mean
#' concentration (+/- SD) by treatment arm and time, and produces a two-panel
#' figure with linear and semilogarithmic views.
#'
#' @param data_path Character. Full path to the ADPC XPT dataset file.
#'   Replaces the SAS hardcoded GitHub URL
#'   (pk_mean_conc.sas line 28: filename source url ...).
#' @param PCTESTCD Character. Analyte test code used to filter ADPC records
#'   (e.g., "ANAL1"). Replaces SAS macro parameter \code{PCTESTCD=}.
#' @param Meantype Character. Type of mean to compute: \code{"A"} for
#'   Arithmetic mean (default), \code{"G"} for Geometric mean.
#'   Replaces SAS macro parameter \code{Meantype=A}.
#' @param SD Character. Controls SD error bars on the linear panel:
#'   \code{"BOTH"} (default) — upper and lower bars,
#'   \code{"UPPER"} — upper bar only,
#'   \code{"N"} — no error bars.
#'   Replaces SAS macro parameter \code{SD=Y}.
#' @param output_path Character or NULL. Directory where the output figure
#'   is saved. If NULL, the figure is not saved to disk. Callers should use
#'   \code{config$output_paths$figure_output_path}.
#'
#' @return A named list (returned invisibly) with components:
#'   \describe{
#'     \item{plot}{The combined patchwork ggplot object.}
#'     \item{mean_data}{A tibble of computed mean/SD data used for plotting.}
#'   }
#'
#' @examples
#' \dontrun{
#' config <- yaml::read_yaml("config/migration_config.yaml")
#' result <- pk_mean_conc(
#'   data_path = file.path(config$data_paths$adam_path, "adpc.xpt"),
#'   PCTESTCD  = "ANAL1",
#'   Meantype  = "A",
#'   SD        = "BOTH",
#'   output_path = config$output_paths$figure_output_path
#' )
#' }
pk_mean_conc <- function(data_path,
                         PCTESTCD,
                         Meantype = "A",
                         SD       = "BOTH",
                         output_path = NULL) {

  # ============================================================================
  # 1. INPUT VALIDATION
  #    Replaces SAS %if/%put ERROR patterns (pk_mean_conc.sas lines 39-44)
  # ============================================================================
  if (is.null(PCTESTCD) || !nzchar(trimws(PCTESTCD))) {
    cli::cli_abort(
      "Argument {.arg PCTESTCD} must be a non-blank analyte test code (e.g., {.val ANAL1})."
    )
  }

  Meantype <- toupper(trimws(Meantype))
  if (!Meantype %in% c("A", "G")) {
    cli::cli_abort(
      "Argument {.arg Meantype} must be {.val A} (Arithmetic) or {.val G} (Geometric). Got {.val {Meantype}}."
    )
  }

  SD <- toupper(trimws(SD))
  if (!SD %in% c("UPPER", "BOTH", "N")) {
    cli::cli_abort(
      "Argument {.arg SD} must be {.val UPPER}, {.val BOTH}, or {.val N}. Got {.val {SD}}."
    )
  }

  if (is.null(data_path) || !nzchar(trimws(data_path))) {
    cli::cli_abort("Argument {.arg data_path} must be a non-blank file path to the ADPC XPT dataset.")
  }

  # ============================================================================
  # 2. DATA ACQUISITION
  #    Replaces SAS filename/libname/DATA step (pk_mean_conc.sas lines 24-33)
  # ============================================================================
  adpc_raw <- haven::read_xpt(data_path)

  # ============================================================================
  # 3. DATA PREPARATION
  #    Replaces SAS DATA step + PROC SORT (pk_mean_conc.sas lines 47-59)
  #
  #    SAS logic:
  #      keep = ANLPRNT TRTAN PCSPEC PCTESTL TRTA DOSREFID PCTESTCD
  #             PCORRESU PCORRES USUBJID PCORRESN EPLTM
  #      where PCTESTCD = "&PCTESTCD"
  #      PCTPT = input(EPLTM, best10.)   -> as.numeric(EPLTM)
  #      ORDERBY = compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN)
  #      BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
  #      sort by TRTAN TRTA PCTPT
  # ============================================================================

  # Resolve variable to avoid R CMD check note about PCTESTCD ambiguity
  pctestcd_val <- PCTESTCD

  adpc <- adpc_raw %>%
    dplyr::select(
      ANLPRNT, TRTAN, PCSPEC, PCTESTL, TRTA, DOSREFID, PCTESTCD,
      PCORRESU, PCORRES, USUBJID, PCORRESN, EPLTM
    ) %>%
    dplyr::filter(PCTESTCD == pctestcd_val) %>%
    dplyr::mutate(
      # input(EPLTM, best10.) -> as.numeric(); non-numeric -> NA (not error)
      PCTPT = suppressWarnings(as.numeric(EPLTM)),
      # compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN) -> concatenate and remove spaces
      ORDERBY = gsub("\\s+", "", paste0(ANLPRNT, PCSPEC, PCTESTL, TRTAN)),
      # compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
      # propcase -> tools::toTitleCase(tolower(...))
      BYVAL6 = trimws(paste0(
        tools::toTitleCase(tolower(pctestcd_val)),
        " Conc (",
        PCORRESU,
        ")"
      ))
    ) %>%
    dplyr::arrange(TRTAN, TRTA, PCTPT)

  # Capture the first BYVAL6 label for y-axis labeling (consistent across rows

  # after the filter on PCTESTCD)
  ylabel <- if (nrow(adpc) > 0L) adpc$BYVAL6[1L] else paste0(pctestcd_val, " Conc")

  # ============================================================================
  # 4. MEAN / SD COMPUTATION
  #    Arithmetic branch: pk_mean_conc.sas lines 61-76
  #    Geometric branch:  pk_mean_conc.sas lines 78-96
  # ============================================================================

  if (Meantype == "A") {
    # --- Arithmetic Mean Branch -----------------------------------------------
    # PROC UNIVARIATE: mean=PCmean STD=PCSD by TRTAN TRTA PCTPT
    mean_data <- adpc %>%
      dplyr::group_by(TRTAN, TRTA, PCTPT) %>%
      dplyr::summarise(
        PCmean = mean(PCORRESN, na.rm = TRUE),
        PCSD   = sd(PCORRESN, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # SAS post-processing (lines 71-74): if PCMEAN=0, set both to missing
      dplyr::mutate(
        PCSD   = dplyr::if_else(PCmean == 0, NA_real_, PCSD),
        PCmean = dplyr::if_else(PCmean == 0, NA_real_, PCmean)
      )

  } else {
    # --- Geometric Mean Branch ------------------------------------------------
    # CRITICAL BEHAVIOR PRESERVATION:
    # The SAS code creates LOGORRES = log(PCORRESN) but PROC UNIVARIATE
    # runs on `var PCORRESN` (the RAW variable, NOT LOGORRES).
    # Then PCMEAN = EXP(mean_of_raw), PCSD = EXP(sd_of_raw).
    # This is the EXACT SAS behavior we must reproduce.
    mean_data <- adpc %>%
      dplyr::group_by(TRTAN, TRTA, PCTPT) %>%
      dplyr::summarise(
        raw_mean = mean(PCORRESN, na.rm = TRUE),
        raw_sd   = sd(PCORRESN, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        PCmean = exp(raw_mean),
        PCSD   = exp(raw_sd)
      ) %>%
      dplyr::select(-raw_mean, -raw_sd)
  }

  # Ensure TRTAN is treated as a factor for consistent legend ordering
  mean_data <- mean_data %>%
    dplyr::mutate(TRTAN = factor(TRTAN))

  # ============================================================================
  # 5. LINEAR VIEW PANEL
  #    Replaces STATGRAPH temp_pksum left cell (pk_mean_conc.sas lines 104-136)
  # ============================================================================

  # Determine label prefix based on mean type
  mean_label <- if (Meantype == "A") "Arithmetic" else "Geometric"

  # Base plot for the linear panel

  p_linear <- ggplot2::ggplot(
    mean_data,
    ggplot2::aes(x = PCTPT, y = PCmean, group = TRTAN, color = TRTAN)
  ) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(color = "black", size = 1.5)

  # --- Conditional SD error bars (matches SAS IF _SD logic) ---
  if (SD == "BOTH") {
    # yerrorlower = PCMEAN - PCSD, yerrorupper = PCMEAN + PCSD
    p_linear <- p_linear +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = PCmean - PCSD, ymax = PCmean + PCSD),
        color = "black",
        width = 0.3
      )
  } else if (SD == "UPPER") {
    # yerrorlower = PCMEAN (no lower bar), yerrorupper = PCMEAN + PCSD
    p_linear <- p_linear +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = PCmean, ymax = PCmean + PCSD),
        color = "black",
        width = 0.3
      )
  }
  # SD == "N": no error bars — nothing added

  # Apply axis labels, scales, and theme for linear panel
  p_linear <- p_linear +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::labs(
      title = "Linear view",
      x     = "Time (h)",
      y     = ylabel
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 9, hjust = 0.5),
      axis.title    = ggplot2::element_text(size = 10),
      axis.text     = ggplot2::element_text(size = 9),
      legend.position = "bottom",
      legend.title  = ggplot2::element_text(size = 9),
      legend.text   = ggplot2::element_text(size = 9)
    )

  # ============================================================================
  # 6. SEMILOGARITHMIC VIEW PANEL
  #    Replaces STATGRAPH temp_pksum right cell (pk_mean_conc.sas lines 137-153)
  #    NOTE: Semilog view does NOT show SD error bars per SAS template
  # ============================================================================

  p_semilog <- ggplot2::ggplot(
    mean_data,
    ggplot2::aes(x = PCTPT, y = PCmean, group = TRTAN, color = TRTAN)
  ) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(color = "black", size = 1.5) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::expand_limits(x = 0) +
    ggplot2::labs(
      title = "Semilogarithmic view",
      x     = "Time (h)",
      y     = ylabel
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 9, hjust = 0.5),
      axis.title    = ggplot2::element_text(size = 10),
      axis.text     = ggplot2::element_text(size = 9),
      legend.position = "bottom",
      legend.title  = ggplot2::element_text(size = 9),
      legend.text   = ggplot2::element_text(size = 9)
    )

  # ============================================================================
  # 7. MULTI-PANEL COMPOSITION
  #    Replaces SAS LAYOUT LATTICE columns=2 rows=2 ROWWEIGHTS=(.9 .1)
  #    and MERGEDLEGEND sidebar/align=bottom (pk_mean_conc.sas lines 103-157)
  # ============================================================================

  # SAS-compatible rounding utility for any formatted display values
  # Uses janitor::round_half_up() per AAP section 0.7.2 to match SAS behavior
  sas_round <- function(x, digits = 0) {
    janitor::round_half_up(x, digits = digits)
  }

  # Compose the two-panel layout using patchwork, matching the SAS

  # LAYOUT LATTICE columns=2 rows=2 ROWWEIGHTS=(.9 .1) with bottom legend.
  # guide_area() explicitly reserves space for the collected legend,
  # matching the SAS sidebar/align=bottom MERGEDLEGEND pattern.
  combined_plot <- (p_linear | p_semilog) /
    patchwork::guide_area() +
    patchwork::plot_layout(
      heights = c(9, 1),
      guides  = "collect"
    ) +
    patchwork::plot_annotation(
      title    = "SPONSOR/PROTOCOL/PRODUCT INFO",
      subtitle = paste0(
        "Figure 14.2-x.x ", mean_label,
        " Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
      ),
      caption  = paste0(
        "Analysis set: PK analysis set\n",
        "PATH DATA/PROGRAM/OUTPUT"
      )
    ) &
    ggplot2::theme(legend.position = "bottom")

  # ============================================================================
  # 8. OUTPUT GENERATION
  #    Replaces SAS ODS GRAPHICS + TAGSETS.RTF (pk_mean_conc.sas lines 162-187)
  #    Figure dimensions: 22.87cm x 12cm matching SAS ods graphics
  # ============================================================================

  if (!is.null(output_path) && nzchar(trimws(output_path))) {
    # Ensure the output directory exists
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    }

    output_file <- file.path(output_path, "target13.png")

    ggplot2::ggsave(
      filename = output_file,
      plot     = combined_plot,
      width    = 22.87,
      height   = 12,
      units    = "cm",
      dpi      = 300
    )
  }

  # Return the plot and mean data invisibly for downstream use/validation
  invisible(list(
    plot      = combined_plot,
    mean_data = mean_data
  ))
}

# ==============================================================================
# Example invocation (commented out — for reference only)
# ==============================================================================
# config <- yaml::read_yaml("config/migration_config.yaml")
# pk_mean_conc(
#   data_path   = file.path(config$data_paths$adam_path, "adpc.xpt"),
#   PCTESTCD    = "ANAL1",
#   Meantype    = "A",
#   SD          = "BOTH",
#   output_path = config$output_paths$figure_output_path
# )

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Geometric mean branch preserves SAS behavior: PROC UNIVARIATE
#      runs on raw PCORRESN (not log-transformed LOGORRES), results
#      then exponentiated via exp(). This is the EXACT SAS code behavior
#      (pk_mean_conc.sas lines 85-94: var PCORRESN; PCMEAN=EXP(LogMEANS);
#      PCSD=EXP(logSD)).
#    - input(EPLTM, best10.) -> as.numeric(EPLTM) assumes EPLTM contains
#      numeric time strings; non-numeric values produce NA (not error).
#    - STATGRAPH 2x2 lattice (rows=2 columns=2 rowweights=0.9/0.1)
#      interpreted as side-by-side linear + semilog panels with a
#      bottom legend area using patchwork guide collection.
#    - SAS SD=Y default is overridden to SD=BOTH following the invocation
#      example on line 190: %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH).
#    - SAS compress() for ORDERBY removes all whitespace; R gsub("\\s+", "")
#      is equivalent.
#    - SAS compbl() for BYVAL6 compresses multiple blanks to one;
#      R trimws() + paste0() achieves equivalent result.
#    - SAS propcase() -> tools::toTitleCase(tolower()) for PCTESTCD label.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS PROC UNIVARIATE vs R mean()/sd() — both use N-1 denominator
#      for STD; minor floating-point differences (< 1e-12) possible due
#      to different accumulator algorithms.
#    - Log-scale axis tick placement may differ between SAS GTL TYPE=LOG
#      BASE=10 and ggplot2 scale_y_log10() auto-break algorithm.
#    - Display formatting precision may differ from SAS PUT formats;
#      janitor::round_half_up() available for SAS-compatible rounding
#      at any formatted display location.
#    - SAS rounding is half-up by default; R default is half-to-even.
#      Use janitor::round_half_up() at any explicit rounding location
#      for regulatory parity.
# NO DIRECT R EQUIVALENT:
#    - SAS STATGRAPH template with dynamic IF conditionals (_SD param)
#      -> ggplot2 with R if/else branching on the SD argument.
#    - SAS MERGEDLEGEND "s1" "s2" with sidebar/align=bottom ->
#      patchwork::plot_layout(guides = "collect") with theme(legend.position
#      = "bottom").
#    - SAS ODS TAGSETS.RTF figure embedding -> ggsave() PNG/PDF output
#      (RTF figure embedding available via r2rtf::rtf_figure() if needed).
#    - SAS filename source url streaming -> haven::read_xpt() with
#      local file path; URL streaming can be achieved via
#      download.file() + read_xpt() if needed.
# PACKAGE SELECTION RATIONALE:
#    - ggplot2: De facto R standard for statistical graphics; replaces
#      SAS PROC SGRENDER + STATGRAPH template system.
#    - patchwork: Multi-panel composition matching SAS LAYOUT LATTICE;
#      chosen over gridExtra for cleaner operator-based syntax and
#      native guide collection support.
#    - haven: XPT I/O for CDISC transport files; tidyverse member;
#      preserves SAS variable labels and tagged NAs.
#    - dplyr: Core data manipulation replacing DATA steps, PROC SORT,
#      and PROC UNIVARIATE BY-group aggregation.
#    - janitor: round_half_up() for SAS-compatible rounding behavior
#      (AAP section 0.7.2) at any formatted numeric display location.
#    - cli: Informative error messages replacing SAS %put ERROR patterns
#      with rich formatting and argument highlighting.
#    - tools: Base R utility for toTitleCase() replacing SAS PROPCASE().
# OPEN QUESTIONS:
#    - Confirm geometric mean PROC UNIVARIATE intent: the SAS code runs
#      PROC UNIVARIATE on raw PCORRESN (not LOGORRES). If the intent was
#      to compute a true geometric mean (exp of mean of logs), the SAS
#      code may contain a bug. The R migration preserves the exact SAS
#      behavior for parity.
#    - Confirm error bar display requirements for semilog panel: the SAS
#      STATGRAPH template shows no SD error bars on the semilog cell
#      (lines 148-150 only have scatterplot + seriesplot, no conditional
#      SD logic). This R migration matches that behavior.
#    - Verify tick mark placement strategy for semilog axis: SAS
#      TYPE=LOG BASE=10 uses specific tick algorithms; ggplot2
#      scale_y_log10() uses breaks_log10() heuristic.
# ============================================================
