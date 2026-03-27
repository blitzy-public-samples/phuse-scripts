# =============================================================================
# util_axis_order.R
# Migrated from: whitepapers/utilities/util_axis_order.sas
# Author:        Dante Di Tommaso (original SAS); migrated to R
# Purpose:       Compute "nice" axis break positions for ggplot2 continuous
#                scales, replicating the SAS %util_axis_order macro algorithm.
# =============================================================================

#' Compute "nice" axis break positions for ggplot2 continuous scales
#'
#' Migrated from the SAS in-line macro \code{\%util_axis_order}
#' (\file{whitepapers/utilities/util_axis_order.sas}).
#'
#' Given the data range \code{[min_val, max_val]} and a desired maximum number
#' of tick intervals, computes evenly-spaced "nice" break positions whose step
#' size is rounded \strong{up} to the nearest increment of the form
#' \eqn{k \times 10^{n}} (where \eqn{k} is a positive integer obtained by
#' ceiling the scientific-notation coefficient). The axis endpoints are then
#' expanded outward so that every data point in the input range is included.
#'
#' @param min_val Numeric scalar. Minimum data value to include in the axis
#'   range. Must be non-missing and strictly less than \code{max_val}.
#' @param max_val Numeric scalar. Maximum data value to include in the axis
#'   range. Must be non-missing and strictly greater than \code{min_val}.
#' @param ticks Positive integer scalar. Maximum number of tick intervals on the
#'   axis (default \code{10}). The actual number of intervals may be fewer due
#'   to step-size rounding. Values less than 1 are silently reset to 10;
#'   non-integer values are truncated toward zero.
#'
#' @return A numeric vector of evenly-spaced break positions from the computed
#'   axis minimum to the computed axis maximum. Suitable for direct use in
#'   \code{ggplot2::scale_y_continuous(breaks = ...)} or
#'   \code{ggplot2::scale_x_continuous(breaks = ...)}.
#'
#'   The returned vector also carries three attributes for programmatic access:
#'   \describe{
#'     \item{\code{axis_min}}{The computed axis lower bound.}
#'     \item{\code{axis_max}}{The computed axis upper bound.}
#'     \item{\code{step}}{The computed "nice" step size.}
#'   }
#'
#' @details
#' The algorithm mirrors the original SAS macro logic (lines 29-64 of the
#' source file):
#' \enumerate{
#'   \item Validate that \code{min_val < max_val}, both numeric and non-missing.
#'   \item Normalise \code{ticks}: reset to 10 if < 1, otherwise truncate to
#'         integer.
#'   \item Compute raw step size: \code{(max_val - min_val) / ticks}.
#'   \item Decompose step into scientific notation:
#'         \eqn{\text{coefficient} \times 10^{\text{exponent}}}.
#'   \item Round the coefficient \strong{up} using \code{ceiling()}.
#'   \item Reconstruct the "nice" step:
#'         \eqn{\lceil\text{coefficient}\rceil \times 10^{\text{exponent}}}.
#'   \item Compute axis minimum: \code{floor(min_val / step) * step}.
#'   \item Compute axis maximum: \code{ceiling(max_val / step) * step}.
#'   \item Generate break sequence: \code{seq(axis_min, axis_max, step)}.
#' }
#'
#' In the original SAS macro this returned the \emph{string}
#' \code{"emin to emax by step"} for use in a \code{SAS/GRAPH AXIS ORDER=}
#' statement. The R version returns a numeric vector for use with ggplot2 scale
#' functions.
#'
#' @section SAS Construct Mapping:
#' \tabular{ll}{
#'   SAS \code{putn(step, e10.)}      \tab R \code{floor(log10(abs(step)))} +
#'                                          coefficient extraction \cr
#'   SAS \code{ceil()}                 \tab R \code{ceiling()} — identical \cr
#'   SAS \code{floor()}               \tab R \code{floor()} — identical \cr
#'   SAS \code{intz()}                \tab R \code{as.integer()} (truncation
#'                                          toward zero) \cr
#'   SAS AXIS ORDER= string           \tab R numeric vector for
#'                                          \code{scale_*_continuous(breaks)}
#' }
#'
#' @examples
#' # Basic usage — compute breaks for data ranging from 4.8 to 23.42
#' breaks <- util_axis_order(4.8, 23.42)
#' print(breaks)
#'
#' # Use with ggplot2
#' # library(ggplot2)
#' # ggplot(df, aes(visit, value)) +
#' #   geom_point() +
#' #   scale_y_continuous(breaks = util_axis_order(min(df$value), max(df$value)))
#'
#' # Customize tick count
#' util_axis_order(0, 100, ticks = 5)
#'
#' # Access computed components via attributes
#' brk <- util_axis_order(3.2, 47.5, ticks = 8)
#' attr(brk, "axis_min")
#' attr(brk, "axis_max")
#' attr(brk, "step")
#'
#' @export
util_axis_order <- function(min_val, max_val, ticks = 10) {


  # ---------------------------------------------------------------------------

  # Input Validation (SAS source lines 32-37)
  # ---------------------------------------------------------------------------
  # SAS: %if %sysevalf(&min >= &max) or %datatyp(&min) = CHAR or ...
  #      %put ERROR: (UTIL_AXIS_ORDER) MIN (&min) and MAX (&max) must be ...

  if (!is.numeric(min_val) || !is.numeric(max_val) ||
      length(min_val) != 1L || length(max_val) != 1L ||
      is.na(min_val) || is.na(max_val) ||
      min_val >= max_val) {
    cli::cli_abort(
      paste0(
        "(UTIL_AXIS_ORDER) MIN ({.val {min_val}}) and MAX ({.val {max_val}}) ",
        "must be ascending, non-missing numeric values."
      )
    )
  }

  # SAS: %else %if %length(&ticks) > 0 and %datatyp(&ticks) = CHAR %then
  #      %put ERROR: (UTIL_AXIS_ORDER) TICKS (&ticks) must be ...
  if (!is.numeric(ticks) || length(ticks) != 1L || is.na(ticks)) {
    cli::cli_abort(
      "(UTIL_AXIS_ORDER) TICKS ({.val {ticks}}) must be a non-missing, positive numeric value."
    )
  }

  # ---------------------------------------------------------------------------
  # Ticks normalisation (SAS source lines 41-42)
  # ---------------------------------------------------------------------------
  # SAS: %if %sysevalf(&ticks < 1) %then %let ticks = 10;
  #      %else %let ticks = %sysfunc(intz(&ticks));
  if (ticks < 1) {
    ticks <- 10L
  } else {
    ticks <- as.integer(ticks)
  }

  # ---------------------------------------------------------------------------
  # Compute raw step size (SAS source lines 45-48)
  # ---------------------------------------------------------------------------
  # SAS: %let diff = %sysevalf(&max - &min);
  #      %let step = %sysevalf(&diff / &ticks);
  diff_range <- max_val - min_val
  step <- diff_range / ticks

  # ---------------------------------------------------------------------------
  # Round UP step to nearest "nice" increment (SAS source lines 51-55)
  # ---------------------------------------------------------------------------
  # SAS algorithm:
  #   1. Convert step to scientific notation via putn(step, e10.)
  #   2. Extract coefficient (before 'E') and exponent (after 'E')
  #   3. Ceil the coefficient
  #   4. Reconstruct: coefficient * 10^exponent
  #
  # R equivalent using log10 decomposition:
  exponent    <- floor(log10(abs(step)))
  coefficient <- step / (10 ^ exponent)
  coefficient <- ceiling(coefficient)
  step        <- coefficient * (10 ^ exponent)

  # ---------------------------------------------------------------------------
  # Compute axis limits (SAS source lines 58-59)
  # ---------------------------------------------------------------------------
  # SAS: %let emin = %sysevalf( %sysfunc(floor(&min/&step)) * &step );
  #      %let emax = %sysevalf( %sysfunc(ceil(&max/&step))  * &step );
  emin <- floor(min_val / step) * step
  emax <- ceiling(max_val / step) * step

  # ---------------------------------------------------------------------------
  # Build result (SAS source line 61)
  # ---------------------------------------------------------------------------
  # SAS returned the inline string "&emin to &emax by &step".
  # R returns a numeric vector suitable for ggplot2 scale_*_continuous(breaks=).
  breaks <- seq(from = emin, to = emax, by = step)


  # Attach axis components as attributes for programmatic access
  attr(breaks, "axis_min") <- emin
  attr(breaks, "axis_max") <- emax
  attr(breaks, "step")     <- step

  breaks
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS in-line string "emin to emax by step" is replaced by an R numeric
#      vector for direct use with ggplot2 scale_y_continuous(breaks = ...) or
#      scale_x_continuous(breaks = ...).
#    - SAS putn(step, e10.) scientific notation decomposition is replicated
#      using R floor(log10(abs(step))) + ceiling(coefficient) to produce
#      identical "nice" step sizes.
#    - SAS ceil() maps to R ceiling() with identical behaviour for finite
#      positive values.
#    - SAS floor() maps to R floor() with identical behaviour.
#    - SAS intz() (truncation toward zero) maps to R as.integer().
#    - SAS %datatyp() CHAR check maps to R is.numeric() negation.
#    - SAS %length() = 0 empty-value check maps to R is.na() + length check.
#    - Axis components (axis_min, axis_max, step) are attached as attributes
#      on the returned numeric vector for callers that need them.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS scientific notation formatting (e10.) rounds the text representation
#      to 10 significant digits before coefficient extraction. R log10/ceiling
#      operates on the full IEEE 754 double. For step values near exact
#      powers of 10 (e.g., step ≈ 10.0000000000001) the SAS formatting may
#      produce a coefficient of exactly 1.0 while R's log10 may yield
#      1.0000000000000x, whose ceiling is still 1. This difference is expected
#      to be negligible (< 1 ULP).
#    - Floating point: SAS and R both use IEEE 754 64-bit doubles. Epsilon
#      comparisons at the boundary of floor/ceiling may differ by ≤ 1 ULP.
#    - seq() accumulation: R's seq(from, to, by) may include or exclude the
#      final endpoint depending on floating-point accumulation; by construction
#      emax is an exact integer multiple of step, so this should be a non-issue.
#
# NO DIRECT R EQUIVALENT:
#    - SAS AXIS ORDER= string syntax ("emin to emax by step") has no direct R
#      equivalent. The R version returns a numeric vector which serves the same
#      purpose when passed to ggplot2::scale_y_continuous(breaks = ...) or
#      ggplot2::scale_x_continuous(breaks = ...).
#
# PACKAGE SELECTION RATIONALE:
#    - Base R math functions (floor, ceiling, log10, seq, as.integer) are the
#      correct tools for pure numeric axis computation. No tidyverse equivalent
#      is needed or appropriate for this calculation.
#    - cli::cli_abort() is used for user-facing validation errors to provide
#      informative, formatted messages consistent with the error style used
#      across the migrated utility library.
#
# OPEN QUESTIONS:
#    - Should the function optionally return a ggplot2 scale object directly
#      (e.g., scale_y_continuous(breaks = ..., limits = ...))? Current design
#      returns a numeric vector which is more composable.
#    - How does this interact with ggplot2's own scales::breaks_extended() or
#      scales::breaks_pretty()? Users may wish to compare outputs.
# ============================================================
