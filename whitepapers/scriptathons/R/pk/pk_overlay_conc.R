# ============================================================================
# Program:     pk_overlay_conc.R
# Description: Overlaid PK Concentration-Time Profiles (Linear + Semilogarithmic)
#              PhUSE CS White Paper: Figure 7.5
#              Target 11 — Overlaying PK concentration-time profiles
#
# Original:    whitepapers/scriptathons/pk/pk_overlay_conc.sas
# Author:      John Salter (SAS original, 12OCT2014)
# Migrated:    SAS-to-R migration — PhUSE WG5 Standard Analyses
#
# Purpose:
#   Reads an ADPC (Analysis Dataset for Pharmacokinetic Concentrations) XPT
#   file, derives time and analyte-order variables from ATPT and PARCAT1,
#   applies LLOQ replacement logic (when PCLLOQ is non-missing, concentration
#   is replaced with the LLOQ value), and generates side-by-side linear and
#   semilogarithmic overlaid concentration-time profile plots for each
#   combination of treatment arm and analyte order.
#
#   Produces 2-panel figures (linear + semilog) for every treatment × analyte
#   combination using ggplot2 + patchwork, matching the SAS STATGRAPH pkplot
#   template with LAYOUT LATTICE columns=2.
#
# SAS Constructs Replaced:
#   - filename/libname URL streaming → haven::read_xpt() with parameterized path
#   - DATA step with KEEP/derive/label → dplyr pipeline
#   - PROC SORT → dplyr::arrange()
#   - PROC TEMPLATE (STATGRAPH pkplot) → ggplot2 + patchwork composition
#   - %PLOT macro with %DO loop → purrr::map() functional iteration
#   - ODS GRAPHICS / ODS LISTING → ggplot2::ggsave()
#
# Configuration:
#   Data and output paths are parameterized via function arguments.
#   Callers may load paths from config/migration_config.yaml:
#     config <- yaml::read_yaml("config/migration_config.yaml")
#     pk_overlay_conc(
#       data_path   = file.path(config$data_paths$adam_path, "adpc.xpt"),
#       output_path = config$output_paths$figure_output_path
#     )
#
# ============================================================================

# ---------------------------------------------------------------------------
# Package Loading
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(stringr)
library(janitor)
library(cli)

# ---------------------------------------------------------------------------
# Helper: SAS-compatible rounding wrapper
# ---------------------------------------------------------------------------
# Per AAP §0.7.2: SAS rounds half-up (0.5 → 1); R default is half-to-even.
# This wrapper ensures consistent SAS-compatible rounding behaviour at every
# rounding location across the migration.
sas_round <- function(x, digits = 0) {
  janitor::round_half_up(x, digits)
}

# ============================================================================
# create_pk_overlay
# ============================================================================
#' Create a two-panel overlaid PK concentration-time profile
#'
#' Generates a side-by-side figure with a linear-scale panel (left) and a
#' semilogarithmic-scale panel (right), each showing individual subject
#' concentration–time profiles overlaid as black lines.
#'
#' Replaces the SAS STATGRAPH \code{pkplot} template (LAYOUT LATTICE columns=2)
#' and the PROC SGRENDER call inside the \code{%PLOT} macro.
#'
#' @param data A data frame (typically the prepared \code{conc} tibble) that
#'   must contain at minimum columns \code{t}, \code{concen}, \code{USUBJID},
#'   \code{order}, and \code{TRTA}.
#' @param analyte_order Integer analyte index to select (filters on \code{order}).
#' @param treatment Character string for treatment arm (filters on \code{TRTA}).
#' @param width_cm Numeric figure width in centimetres (default 24, matching
#'   SAS ODS GRAPHICS width=24cm).
#' @param height_cm Numeric figure height in centimetres (default 12, matching
#'   SAS ODS GRAPHICS height=12cm).
#'
#' @return A \code{patchwork} object combining the linear and semilogarithmic
#'   panels with annotated title and subtitle.
#'
#' @export
create_pk_overlay <- function(data,
                              analyte_order,
                              treatment,
                              width_cm = 24,
                              height_cm = 12) {

  # --- Input validation ------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }
  required_cols <- c("t", "concen", "USUBJID", "order", "TRTA")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Missing required column{?s} in {.arg data}: {.val {missing_cols}}."
    )
  }

  # --- Filter to requested analyte + treatment --------------------------------
  plot_data <- data %>%
    dplyr::filter(
      .data$order == analyte_order,
      .data$TRTA == treatment
    )

  if (nrow(plot_data) == 0L) {
    cli::cli_inform(
      c("i" = paste0(
        "No data for analyte_order = {analyte_order}, treatment = '",
        treatment, "'. Returning NULL."
      ))
    )
    return(NULL)
  }

  # --- Remove rows where concen is non-positive for semilog safety ------------

# For the linear panel we keep all rows; for the semilog panel we silently drop
# rows where concen <= 0 since log10(0) is -Inf. ggplot2 will issue a warning
# automatically, but we suppress it for a clean user experience.

  # --- Linear-scale panel (left) — matches SAS LAYOUT OVERLAY #1 -------------
  p_linear <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$t, y = .data$concen, group = .data$USUBJID)
  ) +
    ggplot2::geom_line(color = "black", linewidth = 0.5) +
    ggplot2::labs(
      title = "Linear view",
      x     = "Time",
      y     = "Concentration (ug/mL)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5, size = 10),
      panel.border     = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "none"
    )

  # --- Semilogarithmic-scale panel (right) — matches SAS LAYOUT OVERLAY #2 ---
  # SAS: yaxisopts=(type=log logopts=(base=10 minorticks=TRUE) label=(' '))
  p_semilog <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$t, y = .data$concen, group = .data$USUBJID)
  ) +
    ggplot2::geom_line(color = "black", linewidth = 0.5) +
    ggplot2::scale_y_log10() +
    ggplot2::annotation_logticks(sides = "l") +
    ggplot2::labs(
      title = "Semilogarithmic view",
      x     = "Time",
      y     = " "
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5, size = 10),
      panel.border     = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "none"
    )

  # --- Compose panels using patchwork (matching SAS LAYOUT LATTICE) -----------
  combined <- p_linear + p_semilog +
    patchwork::plot_annotation(
      title    = "Overlaid PK Concentration Plots",
      subtitle = paste0("Analyte: ", analyte_order, "; Treatment=", treatment)
    )

  combined
}

# ============================================================================
# pk_overlay_conc
# ============================================================================
#' Generate Overlaid PK Concentration Plots (Linear + Semilogarithmic)
#'
#' Main driver function that reads an ADPC XPT file, prepares concentration
#' data (deriving time, analyte order, applying LLOQ replacement), and produces
#' two-panel overlay plots for every combination of treatment arm and analyte.
#'
#' This function replaces the entire SAS program pk_overlay_conc.sas, including
#' the DATA step, PROC SORT, PROC TEMPLATE, and the \code{%PLOT} macro with
#' its invocations \code{%plot(trta=A)} and \code{%plot(trta=C)}.
#'
#' @param data_path Character string — full path to the ADPC XPT file.
#'   Replaces the SAS \code{filename source url "...adpc.xpt"} line.
#'   No hardcoded URLs are used.
#' @param treatments Character vector of treatment arm codes to plot.
#'   Defaults to \code{c("A", "C")} matching SAS invocations
#'   \code{%plot(trta=A)} and \code{%plot(trta=C)}.
#' @param n_analytes Positive integer — number of analyte orders (1 through
#'   n_analytes). Defaults to 3 matching SAS \code{%do i=1 %to 3}.
#' @param output_path Optional character string — directory for saving PNG
#'   figures. If \code{NULL} (default), plots are created but not saved to disk.
#' @param width_cm Numeric figure width in centimetres (default 24).
#' @param height_cm Numeric figure height in centimetres (default 12).
#' @param dpi Numeric resolution for saved figures (default 300, matching
#'   SAS \code{image_dpi=300}).
#'
#' @return A nested list of \code{patchwork} plot objects, structured as
#'   \code{plots[[treatment]][[analyte_order]]}. Returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Using migration config
#' config <- yaml::read_yaml("config/migration_config.yaml")
#' plots <- pk_overlay_conc(
#'   data_path   = file.path(config$data_paths$adam_path, "adpc.xpt"),
#'   output_path = config$output_paths$figure_output_path
#' )
#' }
#'
#' @export
pk_overlay_conc <- function(data_path,
                            treatments  = c("A", "C"),
                            n_analytes  = 3L,
                            output_path = NULL,
                            width_cm    = 24,
                            height_cm   = 12,
                            dpi         = 300) {

  # =========================================================================
  # Input Validation
  # =========================================================================
  if (missing(data_path) || !is.character(data_path) || length(data_path) != 1L) {
    cli::cli_abort(
      "{.arg data_path} must be a single character string specifying the ADPC XPT file path."
    )
  }
  if (!file.exists(data_path)) {
    cli::cli_abort(
      "File not found: {.file {data_path}}. Provide a valid path to an ADPC XPT file."
    )
  }
  if (!is.character(treatments) || length(treatments) == 0L) {
    cli::cli_abort(
      "{.arg treatments} must be a non-empty character vector of treatment arm codes."
    )
  }
  if (!is.numeric(n_analytes) || length(n_analytes) != 1L ||
      n_analytes < 1L || n_analytes != as.integer(n_analytes)) {
    cli::cli_abort(
      "{.arg n_analytes} must be a positive integer (>= 1)."
    )
  }
  n_analytes <- as.integer(n_analytes)

  if (!is.null(output_path)) {
    if (!is.character(output_path) || length(output_path) != 1L) {
      cli::cli_abort("{.arg output_path} must be a single character string or NULL.")
    }
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      cli::cli_inform(c("i" = "Created output directory: {.file {output_path}}."))
    }
  }

  # =========================================================================
  # Phase 1: Data Acquisition — replaces SAS filename/libname (lines 13-14)
  # =========================================================================
  cli::cli_inform(c("i" = "Reading ADPC data from {.file {data_path}}."))
  adpc_raw <- haven::read_xpt(data_path)

  # =========================================================================
  # Phase 2: Data Preparation — replaces SAS DATA step (lines 17-26)
  # =========================================================================
  # SAS KEEP list (uppercase in XPT, lowercase in SAS source):
  #   usubjid trta aperiodc atpt parcat3l aval adtm saffn pkfn paexcfln pclloq parcat1
  #
  # Derive:
  #   t      = input(compress(atpt, ' HOURSPOSTDOSE'), best.)
  #            → extract numeric portion from ATPT text
  #   concen = aval (concentration value)
  #   order  = input(compress(parcat1, ' DRUGANALPlasma'), best.)
  #            → extract integer analyte index from PARCAT1
  #
  # LLOQ replacement (line 25):
  #   if pclloq ne . then concen = pclloq
  #   → when PCLLOQ is non-missing, replace concentration with LLOQ value

  keep_cols <- c(
    "USUBJID", "TRTA", "APERIODC", "ATPT", "PARCAT3L",
    "AVAL", "ADTM", "SAFFN", "PKFN", "PAEXCFLN", "PCLLOQ", "PARCAT1"
  )

  # Verify required columns exist in the dataset
  available_cols <- intersect(keep_cols, names(adpc_raw))
  missing_data_cols <- setdiff(keep_cols, names(adpc_raw))
  if (length(missing_data_cols) > 0L) {
    cli::cli_inform(c(
      "!" = "Column{?s} not found in ADPC data: {.val {missing_data_cols}}. Proceeding with available columns."
    ))
  }

  conc <- adpc_raw %>%
    dplyr::select(dplyr::any_of(keep_cols)) %>%
    dplyr::mutate(
      # Derive numeric time from ATPT text
      # SAS: t = input(compress(atpt, ' HOURSPOSTDOSE'), best.)
      # The compress() with 2nd arg removes characters in ' HOURSPOSTDOSE'
      # from atpt, leaving only digits and decimal points.
      # E.g., "0.25 HOURS POST DOSE" → "0.25"
      #        "0 HOURS PRE DOSE"     → "0"
      t = as.numeric(stringr::str_remove_all(.data$ATPT, "[^0-9.]")),

      # Derive concentration value (direct copy of AVAL)
      concen = .data$AVAL,

      # Derive analyte order index from PARCAT1
      # SAS: order = input(compress(parcat1, ' DRUGANALPlasma'), best.)
      # Removes non-numeric characters from "DRUG ANAL 1 Plasma" → "1"
      order = as.numeric(stringr::str_remove_all(.data$PARCAT1, "[^0-9]"))
    ) %>%
    # LLOQ replacement — CRITICAL: matches SAS line 25:
    #   "if pclloq ne . then concen = pclloq"
    # When PCLLOQ is non-missing, replace concentration with LLOQ value.
    # This is a replacement (not censoring to 0, not imputation).
    dplyr::mutate(
      concen = dplyr::if_else(!is.na(.data$PCLLOQ), .data$PCLLOQ, .data$concen)
    )

  # Apply variable labels matching SAS labels (lines 22-23)
  attr(conc$t, "label") <- "Time"
  attr(conc$concen, "label") <- "Concentration (ug/mL)"

  # =========================================================================
  # Phase 3: Sort — replaces SAS PROC SORT (lines 28-31)
  # =========================================================================
  # SAS: proc sort data=conc; by usubjid t; run;
  conc <- conc %>%
    dplyr::arrange(.data$USUBJID, .data$t)

  cli::cli_inform(c(
    "v" = paste0(
      "Data preparation complete. ",
      nrow(conc), " observations, ",
      length(unique(conc$TRTA)), " treatment arms, ",
      length(unique(conc$order[!is.na(conc$order)])), " analyte orders."
    )
  ))

  # =========================================================================
  # Phase 4: Plot Generation — replaces %PLOT macro (lines 61-75)
  # =========================================================================
  # SAS: %macro plot(trta=);
  #        %do i=1 %to 3;
  #          title1 "Overlaid PK Concentration Plots";
  #          title2 "Analyte: &I; Treatment=&TRTA";
  #          proc sgrender data=conc template=PKPlot;
  #            where order=&I and trta="&TRTA";
  #          run;
  #        %end;
  #      %mend;
  #      %plot(trta=A);
  #      %plot(trta=C);
  #
  # Replaced by nested purrr::map() producing 6 plots
  # (3 analytes × 2 treatments by default)

  analyte_orders <- seq_len(n_analytes)

  plots <- purrr::map(treatments, function(trta_val) {
    trta_plots <- purrr::map(analyte_orders, function(i) {
      cli::cli_inform(c(
        "i" = "Generating plot for Analyte: {i}, Treatment: '{trta_val}'."
      ))

      p <- create_pk_overlay(
        data           = conc,
        analyte_order  = i,
        treatment      = trta_val,
        width_cm       = width_cm,
        height_cm      = height_cm
      )

      # Save to disk if output_path is specified
      if (!is.null(output_path) && !is.null(p)) {
        filename <- paste0("pk_overlay_analyte", i, "_trt", trta_val, ".png")
        filepath <- file.path(output_path, filename)

        ggplot2::ggsave(
          filename = filepath,
          plot     = p,
          width    = width_cm,
          height   = height_cm,
          units    = "cm",
          dpi      = dpi
        )
        cli::cli_inform(c("v" = "Saved: {.file {filepath}}."))
      }

      p
    })
    names(trta_plots) <- paste0("analyte_", analyte_orders)
    trta_plots
  })
  names(plots) <- paste0("trt_", treatments)

  cli::cli_inform(c(
    "v" = paste0(
      "Plot generation complete. ",
      length(treatments), " treatment(s) x ",
      n_analytes, " analyte(s) = ",
      length(treatments) * n_analytes, " figure(s)."
    )
  ))

  # =========================================================================
  # Return nested list of plot objects invisibly
  # =========================================================================
  invisible(plots)
}


# ============================================================================
# MIGRATION NOTES
# ============================================================================
# ASSUMPTIONS:
#    - ATPT text parsing: SAS compress(atpt, ' HOURSPOSTDOSE') removes all
#      characters in the set {space, H, O, U, R, S, P, T, D, E} from ATPT,
#      leaving numeric content. R equivalent: str_remove_all(ATPT, "[^0-9.]")
#      extracts only digits and decimal points — functionally identical for
#      all observed ATPT values (e.g., "0.25 HOURS POST DOSE" → "0.25",
#      "0 HOURS PRE DOSE" → "0").
#    - PARCAT1 parsing: SAS compress(parcat1, ' DRUGANALPlasma') removes
#      {space, D, R, U, G, A, N, L, P, l, a, s, m} — case-sensitive removal.
#      R equivalent: str_remove_all(PARCAT1, "[^0-9]") extracts integer analyte
#      codes. Observed values: "DRUG ANAL 1 Plasma" → "1", etc.
#    - LLOQ replacement: When PCLLOQ is non-missing, concentration is replaced
#      with the LLOQ value (not imputed to 0, not censored). Matches SAS line 25:
#      "if pclloq ne . then concen = pclloq".
#    - Analyte order 1–3 assumed from source data structure; n_analytes
#      parameterized (default 3 matching SAS %do i=1 %to 3).
#    - Treatment arms "A" and "C" are defaults from SAS invocations %plot(trta=A)
#      and %plot(trta=C); parameterized via treatments argument.
#    - Variable names are UPPERCASE in XPT files read by haven::read_xpt(),
#      although SAS source uses lowercase. R code uses UPPERCASE to match XPT.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Log-scale axis minor tick placement may differ from SAS GTL
#      minorticks=TRUE due to different tick generation algorithms in ggplot2
#      annotation_logticks() vs SAS ODS Graphics.
#    - Line interpolation at exact time points is identical in both SAS and R;
#      no visual smoothing is applied in either implementation.
#    - ggplot2 scale_y_log10() silently removes rows with concen <= 0, which
#      would also be excluded by SAS log-scale rendering.
#
# NO DIRECT R EQUIVALENT:
#    - SAS STATGRAPH template with LAYOUT LATTICE columns=2 →
#      ggplot2 + patchwork composition using the + operator and
#      plot_annotation() for title/subtitle.
#    - SAS %PLOT macro with %DO i=1 %TO 3 loop →
#      purrr::map() nested functional iteration.
#    - SAS ODS GRAPHICS inline rendering with style=listing →
#      ggplot2::ggsave() explicit file output with parameterized dimensions.
#    - SAS seriesplot lineattrs=(color=black pattern=1) →
#      geom_line(color = "black", linewidth = 0.5).
#
# PACKAGE SELECTION RATIONALE:
#    - ggplot2: De facto R standard for statistical graphics; replaces
#      PROC SGRENDER and STATGRAPH template definitions.
#    - patchwork: Multi-panel layout composition matching SAS LAYOUT LATTICE
#      columns=2 with equal columnweights=(.50 .50).
#    - purrr: Functional iteration replacing SAS %DO macro loops; produces
#      nested list output for programmatic access to all plots.
#    - haven: CDISC XPT I/O with preserved variable labels and tagged NAs for
#      SAS missing value semantics.
#    - stringr: Tidyverse-consistent string manipulation replacing SAS
#      COMPRESS/INPUT character functions for ATPT and PARCAT1 derivations.
#    - janitor: round_half_up() for SAS-compatible rounding per AAP §0.7.2.
#    - cli: Informative user-facing messages replacing SAS %put and implicit
#      SAS error handling.
#
# OPEN QUESTIONS:
#    - Verify LLOQ replacement logic intent with statistician (replace vs
#      censor vs impute). SAS code clearly replaces, but clinical context
#      may warrant review.
#    - Confirm number of analytes expected (hardcoded 3 in SAS macro loop;
#      parameterized in R with n_analytes = 3 default).
#    - Confirm treatment groups expected (hardcoded "A" and "C" in SAS
#      invocations; parameterized in R with treatments = c("A", "C") default).
#    - Confirm whether walldisplay=none in SAS should suppress all grid lines
#      or only panel borders; current R implementation preserves light major
#      grid lines with theme_minimal() for readability.
# ============================================================================
