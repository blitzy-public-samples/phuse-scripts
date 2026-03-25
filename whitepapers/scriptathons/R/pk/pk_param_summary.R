# =============================================================================
# pk_param_summary.R
# =============================================================================
# PURPOSE:
#   PK White Paper: Table 7.2.4 — Summary of PK Parameters
#   Produces summary statistics (n, Mean (SD), CV% mean, Geo-mean,
#   CV% geo-mean, Median, [Min; Max]) for PK parameters from ADPP data.
#
# MIGRATED FROM:
#   whitepapers/scriptathons/pk/pk_param_summary.sas (168 lines)
#   PhUSE CS Working Group 5 Standard Analyses
#   Target: http://www.phusewiki.org/wiki/images/e/ed/
#           PhUSE_CSS_WhitePaper_PK_final_25March2014.pdf
#
# OUTPUT:
#   RTF file (Table 14.2-PPTS) — Summary statistics for PK parameters
#   by compound, matrix, analyte and actual treatment
#
# REQUIRED DATA:
#   ADPP (Analysis Dataset for PK Parameters) in XPT format
#
# PACKAGES:
#   haven, dplyr, tidyr, purrr, stringr, Tplyr, r2rtf, janitor, cli
#
# USAGE:
#   config <- yaml::read_yaml("config/migration_config.yaml")
#   pk_param_summary(
#     data_path   = file.path(config$data_paths$adam_path, "adpp.xpt"),
#     output_path = config$output_paths$rtf_output_path
#   )
# =============================================================================

# --- Load required packages ---
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(Tplyr)
library(r2rtf)
library(janitor)
library(cli)

# =============================================================================
# FORMAT MAPPINGS (replacing SAS PROC FORMAT — lines 19-45)
# =============================================================================

# Treatment format: TRTAN numeric code → display label
# Replaces SAS: value TRTAN 1="Period 1 Treatment" 2="..." 3="...";
trtan_labels <- c(
  "1" = "Period 1 Treatment",
  "2" = "Period 2 Treatment",
  "3" = "Period 3 Treatment"
)

# Statistic label format: s_order → display label
# Replaces SAS: value statg 1="n" 2="Mean (SD)" ... 7="[Min; Max]";
stat_labels <- c(
  "1" = "n",
  "2" = "Mean (SD)",
  "3" = "CV% mean",
  "4" = "Geo-mean",
  "5" = "CV% geo-mean",
  "6" = "Median",
  "7" = "[Min; Max]"
)

# PK parameter label format: PPTESTCD → display label
# Replaces SAS: value $pk "AUCINFO"="AUC" "AUCTAU"="AUCtau" ...;
pk_labels <- c(
  AUCINFO = "AUC",
  AUCTAU  = "AUCtau",
  AUCLAST = "AUC(0-tlast)",
  CMAX    = "Cmax",
  CMAXD   = "Cmax/Dose",
  MRTLAST = "MRT",
  TLAST   = "tlast",
  TMAX    = "tmax",
  CLRFO   = "CL/F",
  VZFO    = "VzF",
  LAMZ    = "lambdaZ",
  LAMZHL  = "t1/2",
  CLAST   = "Clast"
)

# =============================================================================
# HELPER: SAS-compatible rounding
# =============================================================================
# Wraps janitor::round_half_up() for consistent SAS round-half-up behavior.
# SAS rounds 0.5 → 1; R's default round() uses banker's rounding (half-to-even).
sas_round <- function(x, digits = 0) {
  janitor::round_half_up(x, digits = digits)
}

# =============================================================================
# HELPER: SAS PUT-format–equivalent formatter
# =============================================================================
# Applies SAS-equivalent PUT format (w.d) to a numeric value.
# Pre-rounds with sas_round() then formats with sprintf().
#
# SAS format semantics:
#   w  = total width (including sign, decimal point, and decimals)
#   d  = decimal places
#   6. → 6 chars, 0 decimals:  sprintf("%6.0f", x)
#   5. → 5 chars, 0 decimals:  sprintf("%5.0f", x)
#   4. → 4 chars, 0 decimals:  sprintf("%4.0f", x)
#   2. → 2 chars, 0 decimals:  sprintf("%2.0f", x)
#   4.1 → 4 chars, 1 decimal:  sprintf("%4.1f", x)
#   4.2 → 4 chars, 2 decimals: sprintf("%4.2f", x)
sas_format <- function(x, width, decimals = 0) {
  if (is.na(x) || !is.finite(x)) {
    return("")
  }
  rounded <- sas_round(x, digits = decimals)
  sprintf(paste0("%", width, ".", decimals, "f"), rounded)
}

# =============================================================================
# HELPER: Classify parameter type from _PARAM label
# =============================================================================
# Matches SAS SELECT/WHEN logic using index(_PARAM, ...) > 0.
# Order matters: AUC first (catches AUCINFO and AUCLAST), then Cmax,
# MRT, t1/2, tmax. This mirrors the SAS SELECT statement evaluation order.
classify_param <- function(param_label) {
  dplyr::case_when(
    grepl("AUC",  param_label, fixed = TRUE) ~ "auc",
    grepl("Cmax", param_label, fixed = TRUE) ~ "cmax",
    grepl("MRT",  param_label, fixed = TRUE) ~ "mrt",
    grepl("t1/2", param_label, fixed = TRUE) ~ "thalf",
    grepl("tmax", param_label, fixed = TRUE) ~ "tmax",
    TRUE ~ "other"
  )
}

# =============================================================================
# HELPER: Generate 7 statistic rows per summary group
# =============================================================================
# Replaces SAS DATA step transposition (lines 65-118) that emits 7 OUTPUT
# rows per observation with format-specific formatting based on parameter type.
#
# Each input row (one per parameter group from PROC UNIVARIATE equivalent)
# produces 7 output rows:
#   s_order=1: n
#   s_order=2: Mean (SD)
#   s_order=3: CV% mean
#   s_order=4: Geo-mean
#   s_order=5: CV% geo-mean
#   s_order=6: Median
#   s_order=7: [Min; Max]
transpose_stats <- function(row) {
  param_type <- classify_param(row[["_PARAM"]])

  n1   <- as.numeric(row[["n1"]])
  mn1  <- as.numeric(row[["mn1"]])
  std1 <- as.numeric(row[["std1"]])
  cv1  <- as.numeric(row[["cv1"]])
  mn2  <- as.numeric(row[["mn2"]])
  std2 <- as.numeric(row[["std2"]])
  med1 <- as.numeric(row[["med1"]])
  min1 <- as.numeric(row[["min1"]])
  max1 <- as.numeric(row[["max1"]])

  # --- s_order = 1: n ---
  # SAS: value = put(n1, 2.)
  val_n <- sas_format(n1, 2, 0)

  # --- s_order = 2: Mean (SD) — format varies by parameter type ---
  # SAS lines 73-79: SELECT/WHEN with parameter-specific PUT formats
  val_mean_sd <- switch(param_type,
    auc   = paste0(sas_format(mn1, 6, 0), " (", sas_format(std1, 5, 0), ")"),
    cmax  = paste0(sas_format(mn1, 5, 0), " (", sas_format(std1, 4, 0), ")"),
    mrt   = paste0(sas_format(mn1, 4, 1), " (", sas_format(std1, 4, 2), ")"),
    thalf = paste0(sas_format(mn1, 4, 2), " (", sas_format(std1, 4, 2), ")"),
    " "
  )

  # --- s_order = 3: CV% mean — blank for tmax ---
  # SAS line 82: if index(_PARAM,'tmax')=0 then value=put(cv1,4.1); else value=' ';
  val_cv_mean <- if (param_type != "tmax") {
    sas_format(cv1, 4, 1)
  } else {
    " "
  }

  # --- s_order = 4: Geo-mean — exp(mn2) with format-specific formatting ---
  # SAS lines 86-92: SELECT/WHEN on parameter type
  geo_mean_val <- if (!is.na(mn2) && is.finite(mn2)) exp(mn2) else NA_real_
  val_geo_mean <- switch(param_type,
    auc   = sas_format(geo_mean_val, 6, 0),
    cmax  = sas_format(geo_mean_val, 5, 0),
    mrt   = sas_format(geo_mean_val, 4, 1),
    thalf = sas_format(geo_mean_val, 4, 2),
    " "
  )

  # --- s_order = 5: CV% geo-mean ---
  # SAS line 95: sqrt(exp(std2**2)-1)*100 — lognormal CV formula; blank for tmax
  if (param_type != "tmax" && !is.na(std2) && is.finite(std2)) {
    cv_geo <- sqrt(exp(std2^2) - 1) * 100
    val_cv_geo <- sas_format(cv_geo, 4, 1)
  } else {
    val_cv_geo <- " "
  }

  # --- s_order = 6: Median — format-specific ---
  # SAS lines 99-106: SELECT/WHEN including tmax at 4.2
  val_median <- switch(param_type,
    auc   = sas_format(med1, 6, 0),
    cmax  = sas_format(med1, 5, 0),
    mrt   = sas_format(med1, 4, 1),
    thalf = sas_format(med1, 4, 2),
    tmax  = sas_format(med1, 4, 2),
    ""
  )

  # --- s_order = 7: [Min; Max] — format-specific brackets ---
  # SAS lines 109-116: SELECT/WHEN with parameter-specific MIN/MAX formatting
  val_minmax <- switch(param_type,
    auc   = paste0("[", sas_format(min1, 6, 0), ";",
                        sas_format(max1, 6, 0), "]"),
    cmax  = paste0("[", sas_format(min1, 4, 0), ";",
                        sas_format(max1, 5, 0), "]"),
    mrt   = paste0("[", sas_format(min1, 4, 2), ";",
                        sas_format(max1, 4, 1), "]"),
    thalf = paste0("[", sas_format(min1, 4, 2), ";",
                        sas_format(max1, 4, 1), "]"),
    tmax  = paste0("[", sas_format(min1, 4, 2), ";",
                        sas_format(max1, 4, 2), "]"),
    ""
  )

  # Build the 7-row tibble for this group
  tibble::tibble(
    ANLPRNT  = rep(as.character(row[["ANLPRNT"]]), 7),
    PPSPECL  = rep(as.character(row[["PPSPECL"]]),  7),
    PPCATL   = rep(as.character(row[["PPCATL"]]),  7),
    TRTAN    = rep(as.numeric(row[["TRTAN"]]),      7),
    PPSDY    = rep(as.numeric(row[["PPSDY"]]),      7),
    `_PARAM` = rep(as.character(row[["_PARAM"]]),   7),
    s_order  = 1L:7L,
    value    = c(val_n, val_mean_sd, val_cv_mean, val_geo_mean,
                 val_cv_geo, val_median, val_minmax)
  )
}

# =============================================================================
# MAIN FUNCTION: pk_param_summary
# =============================================================================
#' Summary of PK Parameters (Table 7.2.4 / Table 14.2-PPTS)
#'
#' Produces a summary statistics table for pharmacokinetic parameters from
#' ADPP data. Generates 7 statistics per parameter per treatment group:
#' n, Mean (SD), CV% mean, Geo-mean, CV% geo-mean, Median, [Min; Max].
#'
#' @param data_path Character. Path to the ADPP XPT file. Required.
#'   Replaces SAS: filename source url "...adpp.xpt"; libname source xport;
#' @param output_path Character or NULL. Directory for RTF output. If NULL,
#'   no RTF file is written. Replaces SAS ODS output destination.
#'   Uses config$output_paths$rtf_output_path from migration_config.yaml.
#' @param analyte Character. Analyte code to filter on after removing
#'   whitespace from PPCATL (default "ANAL2"). Replaces SAS:
#'   compress(PPCATL)='ANAL2'.
#' @param params Character vector. PARAMCD values to include (default
#'   c("AUCINFO","AUCLAST","CMAX","TMAX","MRTLAST","LAMZHL")). Replaces SAS:
#'   paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL').
#'
#' @return Invisibly returns a list with components:
#'   \item{summary_stats}{Tibble of computed summary statistics}
#'   \item{transposed}{Tibble with 7 statistic rows per group}
#'   \item{display_table}{Tibble formatted for display/RTF output}
#'
#' @examples
#' \dontrun{
#' config <- yaml::read_yaml("config/migration_config.yaml")
#' pk_param_summary(
#'   data_path   = file.path(config$data_paths$adam_path, "adpp.xpt"),
#'   output_path = config$output_paths$rtf_output_path
#' )
#' }
pk_param_summary <- function(
  data_path,
  output_path = NULL,
  analyte     = "ANAL2",
  params      = c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL")
) {

  # =========================================================================
  # INPUT VALIDATION
  # Replaces SAS implicit error handling with informative cli messages
  # =========================================================================
  if (missing(data_path) || is.null(data_path) || !nzchar(data_path)) {
    cli::cli_abort(
      "Argument {.arg data_path} must be a non-empty path to an ADPP XPT file."
    )
  }
  if (!file.exists(data_path)) {
    cli::cli_abort("File not found: {.file {data_path}}")
  }
  if (!is.character(analyte) || length(analyte) != 1L || !nzchar(analyte)) {
    cli::cli_abort(
      "Argument {.arg analyte} must be a single non-empty string (e.g., 'ANAL2')."
    )
  }
  if (!is.character(params) || length(params) == 0L) {
    cli::cli_abort(
      "Argument {.arg params} must be a non-empty character vector of PARAMCD values."
    )
  }

  cli::cli_inform("Reading ADPP data from {.file {data_path}}")

  # =========================================================================
  # STEP 1: Data acquisition
  # Replaces SAS lines 1-4:
  #   filename source url "https://...adpp.xpt";
  #   libname source xport;
  # =========================================================================
  adpp_raw <- haven::read_xpt(data_path)

  cli::cli_inform(
    "ADPP data loaded: {nrow(adpp_raw)} obs, {ncol(adpp_raw)} variables."
  )

  # =========================================================================
  # STEP 2: Data preparation pipeline
  # Replaces SAS DATA step (lines 47-52) + PROC SORT (lines 54-56)
  #
  # SAS WHERE clause:
  #   PKFN=1 AND ANL01FN ne 1 AND paramcd IN (...)
  #   AND PPSTRESN ne . AND compress(PPCATL)='ANAL2'
  #
  # SAS derive:
  #   _PARAM = compbl(put(PPTESTCD,$pk.) || ' (' || strip(PPORRESU) || ')')
  #   AVAL_log = log(AVAL)
  # =========================================================================
  adpp <- adpp_raw %>%
    dplyr::filter(
      PKFN == 1,
      # SAS: ANL01FN ne 1 — SAS missing != 1 evaluates TRUE; R NA != 1 is NA
      # MUST include is.na() guard to match SAS missing-value comparison semantics
      ANL01FN != 1 | is.na(ANL01FN),
      PARAMCD %in% params,
      # SAS: PPSTRESN ne . → exclude missing
      !is.na(PPSTRESN),
      # SAS: compress(PPCATL)='ANAL2' → remove all whitespace then compare
      stringr::str_remove_all(PPCATL, " ") == analyte
    ) %>%
    dplyr::mutate(
      # Derive human-readable parameter label from PPTESTCD + units
      # SAS: _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
      `_PARAM` = stringr::str_squish(
        paste0(pk_labels[PPTESTCD], " (", stringr::str_trim(PPORRESU), ")")
      ),
      # Log-transformed analysis value
      # SAS: AVAL_log = log(AVAL)
      # Guard: SAS log(0) → missing (.); R log(0) → -Inf
      # if_else ensures AVAL <= 0 produces NA_real_ matching SAS missing behavior
      AVAL_log = dplyr::if_else(AVAL > 0, log(AVAL), NA_real_)
    ) %>%
    dplyr::arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

  cli::cli_inform(
    "After filtering: {nrow(adpp)} observations for analyte '{analyte}'."
  )

  if (nrow(adpp) == 0L) {
    cli::cli_abort(c(
      "No observations remain after filtering.",
      "i" = paste0(
        "Check that {.arg data_path} contains ADPP data with PKFN=1, ",
        "the specified params, and analyte='", analyte, "'."
      )
    ))
  }

  # =========================================================================
  # STEP 3: Summary statistics (replacing PROC UNIVARIATE — lines 58-63)
  # Computes dual-variable statistics on AVAL (suffix 1) and AVAL_log (suffix 2):
  #   n, mean, std, cv, median, min, max
  # SAS CV = (STD/MEAN)*100; R sd(x)/mean(x)*100 — identical formula
  # SAS N = count of non-missing; R sum(!is.na(x)) — identical
  # =========================================================================
  adpp_sum <- adpp %>%
    dplyr::group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
    dplyr::summarise(
      # --- Raw (AVAL) statistics ---
      n1   = sum(!is.na(AVAL)),
      mn1  = mean(AVAL, na.rm = TRUE),
      std1 = sd(AVAL, na.rm = TRUE),
      cv1  = dplyr::if_else(
        mean(AVAL, na.rm = TRUE) != 0,
        sd(AVAL, na.rm = TRUE) / mean(AVAL, na.rm = TRUE) * 100,
        NA_real_
      ),
      med1 = median(AVAL, na.rm = TRUE),
      min1 = min(AVAL, na.rm = TRUE),
      max1 = max(AVAL, na.rm = TRUE),
      # --- Log-transformed (AVAL_log) statistics ---
      n2   = sum(!is.na(AVAL_log)),
      mn2  = mean(AVAL_log, na.rm = TRUE),
      std2 = sd(AVAL_log, na.rm = TRUE),
      cv2  = dplyr::if_else(
        !is.na(mean(AVAL_log, na.rm = TRUE)) &
          mean(AVAL_log, na.rm = TRUE) != 0,
        sd(AVAL_log, na.rm = TRUE) / mean(AVAL_log, na.rm = TRUE) * 100,
        NA_real_
      ),
      med2 = median(AVAL_log, na.rm = TRUE),
      min2 = min(AVAL_log, na.rm = TRUE),
      max2 = max(AVAL_log, na.rm = TRUE),
      .groups = "drop"
    )

  # Clean up NaN / Inf from empty-group edge cases — replace with NA
  # SAS PROC UNIVARIATE produces missing (.) for empty groups; R produces NaN/Inf
  adpp_sum <- adpp_sum %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ dplyr::if_else(is.finite(.x), .x, NA_real_)
      )
    )

  cli::cli_inform(
    "Summary statistics computed for {nrow(adpp_sum)} parameter groups."
  )

  # =========================================================================
  # STEP 4: Statistic row transposition
  # Replaces SAS DATA step (lines 65-118) — creates 7 rows per group
  # using format-specific formatting based on parameter type
  # Uses purrr::pmap_dfr() replacing SAS OUTPUT statement loops
  # =========================================================================
  adpp_sum_t <- purrr::pmap_dfr(adpp_sum, function(...) {
    row <- list(...)
    transpose_stats(row)
  })

  # Sort transposed result — SAS PROC SORT (lines 120-122)
  adpp_sum_t <- adpp_sum_t %>%
    dplyr::arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

  # =========================================================================
  # STEP 5: Table assembly (replacing PROC REPORT — lines 142-165)
  # =========================================================================

  # Apply format labels for display columns
  adpp_sum_t <- adpp_sum_t %>%
    dplyr::mutate(
      trtan_label = dplyr::if_else(
        as.character(TRTAN) %in% names(trtan_labels),
        trtan_labels[as.character(TRTAN)],
        paste("Treatment", TRTAN)
      ),
      stat_label = stat_labels[as.character(s_order)]
    )

  # Pivot _PARAM values across columns (SAS PROC REPORT ACROSS)
  adpp_wide <- adpp_sum_t %>%
    tidyr::pivot_wider(
      id_cols     = c(ANLPRNT, PPSPECL, PPCATL, TRTAN, trtan_label,
                      PPSDY, s_order, stat_label),
      names_from  = `_PARAM`,
      values_from = value,
      values_fill = ""
    )

  # Insert blank separator rows after each PPSDY group
  # Replaces SAS: break after PPSDY/skip;
  param_cols <- setdiff(
    names(adpp_wide),
    c("ANLPRNT", "PPSPECL", "PPCATL", "TRTAN", "trtan_label",
      "PPSDY", "s_order", "stat_label")
  )

  adpp_report <- adpp_wide %>%
    dplyr::group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY) %>%
    dplyr::group_modify(~ {
      blank_vals <- stats::setNames(
        as.list(rep("", length(param_cols))),
        param_cols
      )
      blank_row <- tibble::tibble(
        trtan_label = "",
        s_order     = NA_integer_,
        stat_label  = ""
      )
      blank_row <- dplyr::bind_cols(blank_row, tibble::as_tibble(blank_vals))
      dplyr::bind_rows(.x, blank_row)
    }) %>%
    dplyr::ungroup()

  # Build display table with human-readable column names
  # SAS PROC REPORT defines: TRTAN format=TRTAN., s_order format=statg.
  display_tbl <- adpp_report %>%
    dplyr::mutate(
      `Actual treatment` = trtan_label,
      `Period day`        = dplyr::if_else(
        is.na(s_order), "", as.character(PPSDY)
      ),
      Statistic           = dplyr::if_else(is.na(stat_label), "", stat_label)
    ) %>%
    dplyr::select(
      `Actual treatment`, `Period day`, Statistic,
      dplyr::all_of(param_cols)
    )

  # =========================================================================
  # STEP 6: RTF output generation
  # Replaces SAS TITLE/FOOTNOTE (lines 124-140) + ODS + PROC REPORT
  # =========================================================================
  if (!is.null(output_path)) {
    output_filename <- "pk_param_summary.rtf"
    output_file     <- file.path(output_path, output_filename)

    # Ensure output directory exists
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }

    # Build compound/matrix/analyte header line
    # SAS COMPUTE BEFORE _PAGE_:
    #   line +1 "Compound:" +1 ANLPRNT $10. +1 ", Matrix:" +1 PPSPECL $10.
    #           +1 ", Analyte:" +1 PPCATL $15.;
    header_text <- paste0(
      "Compound: ", unique(adpp_report$ANLPRNT)[1],
      " , Matrix: ", unique(adpp_report$PPSPECL)[1],
      " , Analyte: ", unique(adpp_report$PPCATL)[1]
    )

    # Generate RTF output matching SAS titles/footnotes exactly
    display_tbl %>%
      r2rtf::rtf_page(
        orientation = "landscape"
      ) %>%
      r2rtf::rtf_title(
        title = c(
          "Study001",
          "Table 14.2-PPTS (Page xxx of xxx)",
          "Summary statistics for PK parameters",
          "by compound, matrix, analyte and actual treatment",
          "Analysis Set: PK analysis set"
        )
      ) %>%
      r2rtf::rtf_footnote(
        footnote = c(
          "CV% = coefficient of variation (%) = sd/mean*100",
          paste0("CV% geo-mean = (sqrt(exp(variance for log ",
                 "transformed data)-1))*100"),
          "Geo-mean: Geometric mean",
          paste0("Geo-mean and CV% geo-mean not presented when the ",
                 "minimum value for a parameter is zero."),
          paste("Data: adpp  Program: pk_param_summary.R  Output:",
                output_filename),
          paste("Fake Data/ Production Run on",
                format(Sys.time(), "%d%b%Y:%H:%M"))
        )
      ) %>%
      r2rtf::rtf_body(
        text_justification = "l"
      ) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(file = output_file)

    cli::cli_inform("RTF output written to {.file {output_file}}")
  }

  # =========================================================================
  # RETURN
  # =========================================================================
  invisible(list(
    summary_stats = adpp_sum,
    transposed    = adpp_sum_t,
    display_table = display_tbl
  ))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - PROC FORMAT value mappings preserved as named vectors/factor levels
#    - ANL01FN ne 1 includes missing values (SAS missing != 1 is TRUE;
#      R: is.na() OR != 1)
#    - compress(PPCATL) comparison removes all whitespace before matching
#      analyte code
#    - log(AVAL) for AVAL=0 produces SAS missing (.); R guard:
#      if_else(AVAL > 0, log(AVAL), NA_real_)
#    - Format-specific decimal precision (6., 5., 4.1, 4.2) mapped to
#      sprintf() format strings with sas_round() pre-rounding
#    - SAS DATA step OUTPUT produces 7 rows per observation; R uses
#      purrr::pmap_dfr() to emit 7 tibble rows per summary group
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS PUT format truncates; R sprintf() rounds — minor display
#      differences possible in boundary cases. Pre-rounding with
#      janitor::round_half_up() mitigates this.
#    - CV% geo-mean formula: sqrt(exp(std2^2) - 1) * 100 — identical
#      in both SAS and R
#    - SAS PROC UNIVARIATE vs R summary stats: both use N-1 denominator
#      for SD; minor floating-point differences possible at ~1e-14 level
#    - Rounding: SAS rounds half-up by default; R sprintf() uses
#      half-to-even — janitor::round_half_up() used for SAS parity
# NO DIRECT R EQUIVALENT:
#    - PROC FORMAT value/character formats -> named vectors + factor levels
#    - PROC REPORT with ACROSS, COMPUTE BEFORE, BREAK AFTER -> manual
#      tibble construction + tidyr::pivot_wider + r2rtf pipeline
#    - SAS DATA step OUTPUT statement (multiple rows per observation) ->
#      purrr::pmap_dfr() generating 7-row tibbles
#    - SAS PROC UNIVARIATE dual-variable output -> dplyr::summarise()
#      with explicit stat pairs for raw and log-transformed variables
# PACKAGE SELECTION RATIONALE:
#    - Tplyr: Loaded for potential clinical summary table construction
#      with traceability (not directly used in this script but available
#      for downstream callers)
#    - r2rtf: RTF output matching SAS PROC REPORT formatting capabilities
#      (rtf_title, rtf_footnote, rtf_body, write_rtf, rtf_page)
#    - haven: ADPP XPT I/O via read_xpt()
#    - dplyr/tidyr: Data manipulation and pivoting replacing DATA step,
#      PROC SORT, PROC SQL, and PROC REPORT ACROSS layout
#    - purrr: pmap_dfr() for row-wise statistic transposition
#    - janitor: round_half_up() for SAS-compatible rounding at every
#      formatting location
#    - stringr: String manipulation replacing SAS COMPRESS/COMPBL/STRIP
#    - cli: User-facing error and info messages replacing SAS %put
# OPEN QUESTIONS:
#    - Confirm PROC REPORT ACROSS column order matches R pivot_wider order
#    - Verify format precision for each PK parameter (mapped from SAS
#      PUT formats in lines 73-117)
#    - Confirm [Min; Max] bracket formatting matches exactly
#    - Verify tmax rows correctly blank out geometric statistics
#    - Confirm SAS COMPUTE BEFORE _PAGE_ header line rendering in RTF
# ============================================================
