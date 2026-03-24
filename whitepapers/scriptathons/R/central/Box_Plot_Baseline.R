# ==============================================================================
# Box_Plot_Baseline.R
# ==============================================================================
# PhUSE Scriptathon 2014 — Central Tendency Working Group
# SAS-to-R Migration of Box_Plot_Baseline.sas
#
# Description:
#   Demonstrates a simple regression workflow for coupled vital sign measures
#   (Diastolic Blood Pressure [DIABP] and Pulse [PULSE]) at baseline. The
#   analysis filters ADVS for safety-evaluable subjects at the End of Treatment
#   visit (AVISITN=99), encodes PARAMCD numerically, fits an OLS linear model
#   (AVAL ~ PARAMCD_NUM), and produces two diagnostic plots:
#     1. Scatter plot of AVAL vs PARAMCD_NUM
#     2. Residual diagnostic plot (fitted vs residuals) with LOESS smoother
#
# Original SAS source: whitepapers/scriptathons/central/Box_Plot_Baseline.sas
# Original SAS modification note: 2019-12-23 — path update as data moved
#
# R Migration: Idiomatic tidyverse + ggplot2 implementation
# ==============================================================================

# --- Load required packages ---------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(janitor)

#' Baseline Regression Analysis for Vital Sign Parameters (DIABP and PULSE)
#'
#' Reads an ADVS (Analysis Dataset for Vital Signs) XPT transport file, filters
#' to safety-evaluable subjects at the End of Treatment visit with Diastolic
#' Blood Pressure and Pulse measured after lying down for 5 minutes, fits an
#' OLS linear model of analysis value on numeric parameter code, and produces
#' scatter and residual diagnostic plots.
#'
#' @param data_path Character. Path to the ADVS XPT transport file. If NULL
#'   (default), uses the repository-relative path
#'   \code{file.path("data", "adam", "cdisc", "advs.xpt")}.
#' @param output_path Character. Directory where output plots (PDF) are saved.
#'   Defaults to the current working directory (\code{"."}).
#'
#' @return A named list (returned invisibly) containing:
#'   \describe{
#'     \item{model}{The fitted \code{lm} object from the OLS regression.}
#'     \item{data}{A tibble with the filtered ADVS data augmented by
#'       \code{PARAMCD_NUM}, \code{fitted_value}, and \code{residuals}.}
#'     \item{plots}{A named list with \code{scatter} and \code{residuals}
#'       ggplot objects.}
#'   }
#'
#' @details
#' This function replicates the SAS workflow:
#' \enumerate{
#'   \item DATA step: filter SAFFL='Y', PARAMCD in ('DIABP','PULSE'),
#'         ATPT='AFTER LYING DOWN FOR 5 MINUTES', AVISITN=99
#'   \item DATA step: encode PARAMCD numerically (DIABP=0, PULSE=1)
#'   \item PROC SGPLOT: scatter y=AVAL x=PARAMCD_NUM
#'   \item PROC REG: model AVAL=PARAMCD_NUM; output p=fitted_value r=residuals
#'   \item PROC SGPLOT: scatter + LOESS + refline 0 on residuals
#' }
#'
#' @examples
#' \dontrun{
#' result <- box_plot_baseline()
#' summary(result$model)
#' }
box_plot_baseline <- function(data_path = NULL, output_path = ".") {

  # --------------------------------------------------------------------------
  # Step 0: Resolve data path — no hardcoded URLs

  # --------------------------------------------------------------------------
  if (is.null(data_path)) {
    data_path <- file.path("data", "adam", "cdisc", "advs.xpt")
  }

  if (!file.exists(data_path)) {
    stop(
      "ADVS data file not found at: ", data_path,
      "\nPlease provide a valid path to the ADVS XPT transport file.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Step 1: Read ADVS XPT transport file
  # Replaces SAS: filename source "...advs.xpt"; libname source xport;
  # --------------------------------------------------------------------------
  advs_raw <- haven::read_xpt(data_path)

  # --------------------------------------------------------------------------
  # Step 2: Filter and select variables
  # Replaces SAS DATA step with WHERE clause:
  #   where SAFFL='Y' and PARAMCD in ('DIABP', 'PULSE')
  #     and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99;
  #   keep USUBJID SAFFL PARAMCD TRTPN TRTP AVAL AVISIT AVISITN ATPT;
  #
  # NOTE: SAS WHERE clause comparisons automatically exclude rows where any

  # compared variable is missing. We replicate this with explicit !is.na()
  # checks on the filter columns to avoid including NA matches.
  # --------------------------------------------------------------------------
  advs <- advs_raw %>%
    dplyr::filter(
      !is.na(SAFFL) & SAFFL == "Y",
      !is.na(PARAMCD) & PARAMCD %in% c("DIABP", "PULSE"),
      !is.na(ATPT) & ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
      !is.na(AVISITN) & AVISITN == 99
    ) %>%
    dplyr::select(
      USUBJID, SAFFL, PARAMCD, TRTPN, TRTP, AVAL, AVISIT, AVISITN, ATPT
    )

  # Validate that we have data after filtering
  if (nrow(advs) == 0L) {
    warning(
      "No observations remain after filtering ADVS. ",
      "Check that SAFFL='Y', PARAMCD in ('DIABP','PULSE'), ",
      "ATPT='AFTER LYING DOWN FOR 5 MINUTES', and AVISITN=99 ",
      "are present in the data.",
      call. = FALSE
    )
    return(invisible(list(model = NULL, data = advs, plots = list())))
  }

  # --------------------------------------------------------------------------
  # Step 3: Create numeric PARAMCD encoding
  # Replaces SAS DATA step:
  #   if PARAMCD = 'DIABP' then PARAMCD_NUM=0;
  #   else PARAMCD_NUM=1;
  #
  # Using dplyr::case_when() per tidyverse mandate. Unmatched values map to
  # NA_real_ to preserve SAS missing-value semantics.
  # --------------------------------------------------------------------------
  advs <- advs %>%
    dplyr::mutate(
      PARAMCD_NUM = dplyr::case_when(
        PARAMCD == "DIABP" ~ 0,
        PARAMCD == "PULSE" ~ 1,
        TRUE ~ NA_real_
      )
    )

  # --------------------------------------------------------------------------
  # Step 4: Initial scatter plot — AVAL vs PARAMCD_NUM
  # Replaces SAS:
  #   proc sgplot data=advs1;
  #     scatter y=AVAL x=PARAMCD_NUM;
  #   run;
  # --------------------------------------------------------------------------
  p_scatter <- ggplot2::ggplot(advs, ggplot2::aes(x = PARAMCD_NUM, y = AVAL)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "PARAMCD (0 = DIABP, 1 = PULSE)",
      y = "Analysis Value (AVAL)",
      title = "Scatter Plot: AVAL by PARAMCD (Baseline Regression)"
    ) +
    ggplot2::theme_minimal()

  # Save scatter plot
  scatter_path <- file.path(output_path, "scatter_aval_paramcd.pdf")
  ggplot2::ggsave(
    filename = scatter_path,
    plot = p_scatter,
    device = "pdf",
    width = 8,
    height = 6,
    units = "in"
  )

  # --------------------------------------------------------------------------
  # Step 5: Fit OLS linear regression model
  # Replaces SAS:
  #   proc reg data=advs1;
  #     model AVAL=PARAMCD_NUM;
  #     output out=regout p=fitted_value r=residuals j;
  #   run;
  #
  # stats::lm() uses Ordinary Least Squares — mathematically identical to
  # SAS PROC REG. Both use IEEE 754 double precision arithmetic.
  # --------------------------------------------------------------------------
  reg_model <- stats::lm(AVAL ~ PARAMCD_NUM, data = advs)

  # Print model summary to console (analogous to PROC REG default output)
  cat("\n--- Linear Regression: AVAL ~ PARAMCD_NUM ---\n")
  print(stats::summary.lm(reg_model))

  # Extract fitted values and residuals into the output dataset
  # Replaces SAS OUTPUT statement: p=fitted_value r=residuals
  #
  # NOTE: stats::lm() drops rows with missing AVAL (na.action = na.omit),

  # so fitted() returns fewer values than nrow(advs). We use predict() with
  # newdata to compute fitted values for ALL rows (predict uses only the
  # predictor PARAMCD_NUM, so missing AVAL is irrelevant). Residuals are
  # computed as AVAL - fitted_value; rows with missing AVAL naturally produce
  # NA residuals — matching SAS PROC REG OUTPUT behavior.
  regout <- advs %>%
    dplyr::mutate(
      fitted_value = stats::predict(reg_model, newdata = advs),
      residuals = AVAL - fitted_value
    )

  # --------------------------------------------------------------------------
  # Step 6: Residual diagnostic plot — fitted vs residuals with LOESS
  # Replaces SAS:
  #   proc sgplot data=regout;
  #     scatter x=fitted_value y=residuals;
  #     loess x=fitted_value y=residuals;
  #     refline 0;
  #   run;
  #
  # geom_smooth(method = "loess") replaces SAS LOESS statement.
  # geom_hline(yintercept = 0) replaces SAS refline 0.
  # --------------------------------------------------------------------------
  p_resid <- ggplot2::ggplot(
    regout,
    ggplot2::aes(x = fitted_value, y = residuals)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::labs(
      x = "Fitted Value",
      y = "Residuals",
      title = "Residual Diagnostic Plot (Fitted vs Residuals with LOESS)"
    ) +
    ggplot2::theme_minimal()

  # Save residual diagnostic plot
  resid_path <- file.path(output_path, "residual_diagnostics.pdf")
  ggplot2::ggsave(
    filename = resid_path,
    plot = p_resid,
    device = "pdf",
    width = 8,
    height = 6,
    units = "in"
  )

  # --------------------------------------------------------------------------
  # Step 7: Return results invisibly
  # --------------------------------------------------------------------------
  invisible(list(
    model = reg_model,
    data = regout,
    plots = list(scatter = p_scatter, residuals = p_resid)
  ))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. ADVS XPT data path is parameterized; default assumes local repo structure
#    2. SAS PROC REG with OLS maps directly to stats::lm() — identical mathematical formulation
#    3. SAS PROC SGPLOT scatter -> ggplot2::geom_point
#    4. SAS LOESS statement -> ggplot2::geom_smooth(method = "loess")
#    5. SAS refline 0 -> ggplot2::geom_hline(yintercept = 0)
#    6. PARAMCD numeric encoding: DIABP=0, PULSE=1 preserved exactly from SAS
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. LOESS smoothing span may differ: SAS default span varies; R default span=0.75 — verify visual match
#    2. OLS regression coefficients should be identical (both IEEE 754 double precision)
#    3. Fitted values and residuals should match to machine epsilon
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PROC REG automatic diagnostic panels — R provides summary() + individual diagnostic plots
#    2. SAS ODS automatic graph output — R uses explicit ggsave()
#
# PACKAGE SELECTION RATIONALE:
#    1. haven: Read SAS XPT transport files (CRAN, tidyverse ecosystem)
#    2. dplyr: Data manipulation replacing DATA steps (tidyverse core)
#    3. ggplot2: Visualization replacing PROC SGPLOT (tidyverse core)
#    4. stats::lm: Base R linear model replacing PROC REG (base R — no tidyverse equivalent for linear regression fitting)
#    5. janitor: Available for round_half_up() if any rounding needed
#
# OPEN QUESTIONS:
#    1. LOESS span parameter alignment between SAS and R — visual comparison recommended
#    2. Original SAS author comment indicates uncertainty about outcome/independent variable choice — preserved as-is in migration
#    3. SAS PROC REG 'j' option in OUTPUT statement — this requests jackknife residuals; R equivalent is rstudent() or hatvalues() — not implemented unless needed for parity
# ============================================================
