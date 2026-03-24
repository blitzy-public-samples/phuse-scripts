# =============================================================================
# File:        lang/R/graph/kmplot.R
# Migrated from: lang/SAS/graph/KM/kmplot.sas
# Purpose:     Kaplan-Meier survival plot example — demonstrates non-parametric
#              KM estimation stratified by treatment arm with integrated
#              number-at-risk table, replacing SAS PROC LIFETEST with ODS PDF.
# Author:      Mike Carniello (original SAS); migrated to R per PhUSE WG5
# Migration:   Full SAS-to-R migration per PhUSE CS WG5 standards
# License:     MIT (per repository license)
#
# SAS Source Reference:
#   lang/SAS/graph/KM/kmplot.sas (44 lines)
#   - DATA step: inline two-arm survival dataset (arm, day, censor)
#   - PROC LIFETEST: Kaplan-Meier estimation with at-risk table (0 to 330 by 30)
#   - ODS PDF: output to /bdm/myfolder/mcarniel/kmplot.pdf (hardcoded)
#
# R Migration Strategy:
#   - DATA step → tibble::tibble() + dplyr::bind_rows()
#   - PROC LIFETEST → survival::Surv() + survival::survfit()
#   - ODS graphics → survminer::ggsurvplot() with risk.table=TRUE
#   - ODS PDF → ggplot2::ggsave() with parameterized output_path
#   - Hardcoded paths → function arguments with sensible defaults
# =============================================================================

# --- Package Loading ---------------------------------------------------------
# Per AAP §0.8.1: tidyverse over base R; all visualization via ggplot2-based tools

library(survival)   # Surv(), survfit() — KM estimation replacing PROC LIFETEST
library(survminer)  # ggsurvplot() — KM visualization with risk tables
library(ggplot2)    # theme_minimal(), ggsave() — underlying plot engine & output
library(dplyr)      # bind_rows() — data manipulation (tidyverse-first rule)
library(tibble)     # tibble() — enhanced data frames (tidyverse-first rule)

# =============================================================================
# generate_km_plot
# =============================================================================
#' Generate a Kaplan-Meier Survival Plot
#'
#' Produces a stratified Kaplan-Meier survival curve with integrated number-at-
#' risk table, migrated from the SAS PROC LIFETEST example in kmplot.sas.
#' The inline dataset contains two treatment arms with event/censoring indicators.
#'
#' @param output_path Character string specifying the file path for saving the
#'   plot (e.g., "km_output.pdf"). If \code{NULL} (default), the plot is
#'   displayed interactively but not saved to disk. Replaces the SAS hardcoded
#'   path \code{/bdm/myfolder/mcarniel/kmplot.pdf}. No hardcoded paths per
#'   AAP §0.8.1.
#' @param output_format Character string specifying the output format when
#'   saving. One of \code{"pdf"}, \code{"png"}, \code{"jpeg"}, \code{"tiff"},
#'   or \code{"svg"}. Default is \code{"pdf"} to match SAS ODS PDF destination.
#' @param title Character string for the plot title. Default is
#'   \code{"Example K-M Plot"} matching SAS \code{title1}.
#' @param width Numeric width of the saved plot in inches. Default is 10.
#' @param height Numeric height of the saved plot in inches. Default is 7.
#' @param conf_int Logical; whether to display confidence intervals on the
#'   survival curves. Default is \code{FALSE} to match the SAS source which
#'   does not explicitly request CIs in the PROC LIFETEST output.
#' @param risk_table Logical; whether to display the number-at-risk table.
#'   Default is \code{TRUE}, matching SAS \code{plots=survival(atrisk=...)}.
#' @param break_time_by Numeric; interval for time-axis tick marks and at-risk
#'   table breakpoints. Default is 30 matching SAS \code{atrisk=0 to 330 by 30}.
#' @param xlim Numeric vector of length 2 specifying x-axis limits.
#'   Default is \code{c(0, 360)} to accommodate the full data range including
#'   the maximum observation at day 344.
#'
#' @return Invisibly returns the \code{ggsurvplot} object (a list containing
#'   the survival plot and risk table). This can be further customized or
#'   printed for interactive use.
#'
#' @details
#' \strong{Censoring convention}: In the SAS source, \code{time day*censor(1)}
#' specifies that \code{censor = 1} indicates censoring and \code{censor = 0}
#' indicates the event occurred. The R implementation preserves this semantic
#' by constructing \code{Surv(day, censor == 0)}.
#'
#' \strong{Data note}: SAS source line 14 contains \code{227.230} in the DO
#' loop for Arm 1 events. This is interpreted as two separate values (227 and
#' 230) based on the pattern of all other values being integers. See MIGRATION
#' NOTES at end of file for full discussion.
#'
#' @examples
#' # Display plot interactively (no file output)
#' generate_km_plot()
#'
#' # Save to PDF
#' generate_km_plot(output_path = "kaplan_meier.pdf")
#'
#' # Save to PNG with custom title
#' generate_km_plot(
#'   output_path = "km_plot.png",
#'   output_format = "png",
#'   title = "Overall Survival by Treatment Arm"
#' )
#'
#' @export
generate_km_plot <- function(output_path = NULL,
                             output_format = "pdf",
                             title = "Example K-M Plot",
                             width = 10,
                             height = 7,
                             conf_int = FALSE,
                             risk_table = TRUE,
                             break_time_by = 30,
                             xlim = c(0, 360)) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (!is.null(output_path)) {
    if (!is.character(output_path) || length(output_path) != 1L) {
      stop(
        "output_path must be a single character string or NULL.",
        call. = FALSE
      )
    }
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir) && output_dir != ".") {
      stop(
        paste0(
          "Output directory does not exist: ", output_dir,
          ". Please create it before calling generate_km_plot()."
        ),
        call. = FALSE
      )
    }
  }

  valid_formats <- c("pdf", "png", "jpeg", "tiff", "svg")
  if (!output_format %in% valid_formats) {
    stop(
      paste0(
        "output_format must be one of: ",
        paste(valid_formats, collapse = ", "),
        ". Got: '", output_format, "'."
      ),
      call. = FALSE
    )
  }

  if (!is.character(title) || length(title) != 1L) {
    stop("title must be a single character string.", call. = FALSE)
  }

  if (!is.numeric(width) || width <= 0) {
    stop("width must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(height) || height <= 0) {
    stop("height must be a positive number.", call. = FALSE)
  }

  if (!is.logical(conf_int) || length(conf_int) != 1L) {
    stop("conf_int must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if (!is.logical(risk_table) || length(risk_table) != 1L) {
    stop("risk_table must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if (!is.numeric(break_time_by) || break_time_by <= 0) {
    stop("break_time_by must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(xlim) || length(xlim) != 2L || xlim[1] >= xlim[2]) {
    stop("xlim must be a numeric vector of length 2 with xlim[1] < xlim[2].", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Phase 3: Data Construction
  # ---------------------------------------------------------------------------
  # Reproduce SAS DATA step (lines 10-30 of kmplot.sas)
  # SAS: data one; length arm $8;
  #   censor = 0; arm = "Arm 1";
  #   do day = 143,164,188,188,190,192,206,213,216,220,227.230,
  #            234,246,265,304; output; end;
  #   censor = 1; day = 216; output; day = 244; output;
  #   censor = 0; arm = "Arm 2";
  #   do day = 142,156,163,198,205,232,232,233,233,233,233,239,
  #            240,261,280,280,296,296,323; output; end;
  #   censor = 1; day = 204; output; day = 344; output;
  #
  # NOTE: SAS line 14 has "227.230" — interpreted as two separate integer

  # values (227, 230) based on context. See MIGRATION NOTES for details.
  # ---------------------------------------------------------------------------

  # Arm 1, event observations (censor = 0)
  arm1_events <- tibble::tibble(
    arm    = "Arm 1",
    day    = c(143, 164, 188, 188, 190, 192, 206, 213, 216, 220,
               227, 230, 234, 246, 265, 304),
    censor = 0L
  )

  # Arm 1, censored observations (censor = 1)

  arm1_censored <- tibble::tibble(
    arm    = "Arm 1",
    day    = c(216, 244),
    censor = 1L
  )

  # Arm 2, event observations (censor = 0)
  arm2_events <- tibble::tibble(
    arm    = "Arm 2",
    day    = c(142, 156, 163, 198, 205, 232, 232, 233, 233, 233,
               233, 239, 240, 261, 280, 280, 296, 296, 323),
    censor = 0L
  )

  # Arm 2, censored observations (censor = 1)
  arm2_censored <- tibble::tibble(
    arm    = "Arm 2",
    day    = c(204, 344),
    censor = 1L
  )

  # Combine all subsets using dplyr::bind_rows (tidyverse-first, not base rbind)
  one <- dplyr::bind_rows(
    arm1_events,
    arm1_censored,
    arm2_events,
    arm2_censored
  )

  # ---------------------------------------------------------------------------
  # Phase 4: Survival Analysis — PROC LIFETEST → survival::survfit
  # ---------------------------------------------------------------------------
  # SAS: proc lifetest data=one plots=survival(atrisk=0 to 330 by 30);
  #        time day*censor(1);    /* censor=1 means censored */
  #        srata arm;             /* typo for "strata arm" */
  #
  # In R survival::Surv():
  #   - event indicator: censor == 0 means event occurred (TRUE)
  #   - censor == 1 means censored (FALSE in the event column)
  # This matches SAS semantics where censor(1) designates the censoring value.
  #
  # Ties: SAS PROC LIFETEST uses Efron by default for log-rank tests.
  # For non-parametric KM estimation (survfit), ties are handled inherently
  # by the KM estimator. If a Cox model were fitted, ties = "breslow" would
  # need explicit specification per AAP §0.7.1.
  # ---------------------------------------------------------------------------

  km_fit <- survival::survfit(
    survival::Surv(time = day, event = (censor == 0L)) ~ arm,
    data = one
  )

  # ---------------------------------------------------------------------------
  # Phase 5: Visualization — ODS PDF + PROC LIFETEST plots → ggsurvplot
  # ---------------------------------------------------------------------------
  # SAS: plots=survival(atrisk=0 to 330 by 30)
  #   → risk.table = TRUE, break.time.by = 30
  # SAS: title1 "Example K-M Plot"
  #   → title parameter
  # Per AAP §0.8.1: all visualization via ggplot2/survminer, NOT base R plots.
  # ---------------------------------------------------------------------------

  km_plot <- survminer::ggsurvplot(
    fit          = km_fit,
    data         = one,
    risk.table   = risk_table,
    conf.int     = conf_int,
    break.time.by = break_time_by,
    xlim         = xlim,
    title        = title,
    xlab         = "Day",
    ylab         = "Survival Probability",
    legend.title = "Arm",
    legend.labs  = c("Arm 1", "Arm 2"),
    palette      = c("#E41A1C", "#377EB8"),
    ggtheme      = ggplot2::theme_minimal(),
    risk.table.col = "strata",
    risk.table.height = 0.25,
    risk.table.y.text.col = TRUE,
    risk.table.y.text = TRUE,
    surv.median.line = "none"
  )

  # ---------------------------------------------------------------------------
  # Phase 6: Output Generation — ODS PDF → ggsave with parameterized path
  # ---------------------------------------------------------------------------
  # SAS: ods pdf file = "/bdm/myfolder/mcarniel/kmplot.pdf";
  # R: parameterized output_path argument (no hardcoded paths per §0.8.1)
  # ---------------------------------------------------------------------------

  if (!is.null(output_path)) {
    # Determine the file extension to use
    # If output_path already has an extension, use it; otherwise append format
    path_ext <- tools::file_ext(output_path)
    save_path <- if (nchar(path_ext) == 0L) {
      paste0(output_path, ".", output_format)
    } else {
      output_path
    }

    # For survminer ggsurvplot objects, we use the print method to render
    # the combined plot (survival curve + risk table) before saving.
    # ggsave works with the last printed ggplot; ggsurvplot produces a list
    # that needs to be explicitly printed.
    tryCatch(
      {
        # Open a graphics device matching the requested format
        device_fn <- switch(
          tolower(tools::file_ext(save_path)),
          "pdf"  = grDevices::pdf,
          "png"  = grDevices::png,
          "jpeg" = grDevices::jpeg,
          "jpg"  = grDevices::jpeg,
          "tiff" = grDevices::tiff,
          "svg"  = grDevices::svg,
          grDevices::pdf
        )

        # Device-specific arguments
        if (tolower(tools::file_ext(save_path)) %in% c("png", "jpeg", "jpg", "tiff")) {
          device_fn(
            filename = save_path,
            width    = width,
            height   = height,
            units    = "in",
            res      = 300
          )
        } else {
          device_fn(
            file   = save_path,
            width  = width,
            height = height
          )
        }

        print(km_plot)
        grDevices::dev.off()

        message(
          "Kaplan-Meier plot saved to: ", save_path
        )
      },
      error = function(e) {
        # Ensure the device is closed even on error
        tryCatch(grDevices::dev.off(), error = function(e2) NULL)
        warning(
          "Failed to save plot to '", save_path, "': ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # Return the plot object invisibly for interactive use
  invisible(km_plot)
}

# =============================================================================
#### MIGRATION NOTES
#### =============================================================================
#### ASSUMPTIONS:
####    1. SAS source line 14 `227.230` interpreted as two separate values
####       (227, 230) separated by a period-as-comma typo. In SAS, the DO
####       loop would parse this as a single decimal value 227.23, but all
####       other values in the dataset are integers representing days, making
####       a typo the most likely explanation. If the intent was a single
####       decimal value 227.230, the data construction must be adjusted.
####    2. SAS source line 38 `srata arm` is a typo for `strata arm` —
####       implemented as stratified KM estimation via the ~ arm formula
####       in survival::survfit().
####    3. Censoring convention: censor=1 means censored (per SAS
####       `time day*censor(1)`), censor=0 means event occurred. The R
####       Surv() call uses event = (censor == 0) to preserve this semantic.
####    4. The SAS source does not specify a confidence interval method or
####       request printed CI bands. The R default (conf.int = FALSE) is
####       used, with an option to enable via the conf_int parameter.
#### POTENTIAL NUMERICAL DIFFERENCES:
####    1. KM survival estimates should be identical between SAS PROC
####       LIFETEST and R survival::survfit() for this dataset. Both use
####       the standard product-limit (Kaplan-Meier) estimator.
####    2. Confidence interval method: SAS PROC LIFETEST default is log
####       transform; R survfit default is also "log" transform. If exact
####       CIs are compared, verify both use the same method.
####    3. At-risk table tick marks at 0, 30, 60, ..., 330 may show minor
####       formatting differences (e.g., number alignment, font).
####    4. If `227.230` was intended as a single SAS decimal value (227.23
####       days), the Arm 1 event data would differ by one row vs the R
####       implementation (15 rows vs 16 rows). This would change KM
####       estimates for Arm 1.
#### NO DIRECT R EQUIVALENT:
####    1. ODS PDF inline output — R uses ggsave() or a graphics device
####       (pdf(), png(), etc.); functional parity preserved via the
####       output_path parameter.
####    2. SAS PROC LIFETEST automatic at-risk table integration —
####       survminer::ggsurvplot(risk.table = TRUE) provides an equivalent
####       integrated at-risk display beneath the survival curve.
####    3. SAS ODS LISTING CLOSE / ODS GRAPHICS ON — R does not require
####       explicit graphics device toggling; the survminer plot object
####       renders directly.
#### PACKAGE SELECTION RATIONALE:
####    1. survival (>= 3.5-0): Standard R package for KM estimation,
####       matches SAS PROC LIFETEST functionality (AAP §0.7.1). Provides
####       Surv() and survfit() — the de facto standard for survival
####       analysis in R.
####    2. survminer (>= 0.4.9): ggplot2-based KM visualization with
####       integrated number-at-risk tables, replacing SAS ODS graphics
####       output. Chosen per AAP §0.5.1 specification.
####    3. ggplot2 (>= 3.4.0): Underlying plot engine; theme_minimal()
####       applied per tidyverse-first rule (AAP §0.8.1). Used for
####       ggsave() output.
####    4. dplyr (>= 1.1.0): bind_rows() for combining arm/censor subsets
####       per tidyverse-first rule — must not use base R rbind().
####    5. tibble (>= 3.2.0): tibble() for data construction per
####       tidyverse-first rule — must not use base R data.frame().
#### OPEN QUESTIONS:
####    1. Confirm whether `227.230` in the SAS source is one decimal value
####       or two integer values — requires review of original study data
####       or author (Mike Carniello) clarification.
####    2. Determine if a specific confidence interval method (plain, log,
####       log-log) is required to match SAS output for regulatory parity.
####    3. The SAS source sets xlim via atrisk=0 to 330 by 30, but the
####       maximum observed time is 344 (Arm 2 censored). The R default
####       xlim is set to c(0, 360) to display all data; adjust to
####       c(0, 330) if exact SAS axis range is required.
#### =============================================================================
