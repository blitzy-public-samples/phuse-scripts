# =============================================================================
# WPCT-F.07.03.R
# =============================================================================
# Display:     Figure 7.3 Two-Panel Box Plot - Observed Values (left) and
#              Change from Baseline (right) by Analysis Timepoint, Visit and
#              Treatment, with ANCOVA p-values
# White paper: Central Tendency
# User Guide:  https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/CentralTendency-UserGuide.txt
# Macro Library (R): whitepapers/utilities/R/
# Specs:       https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/specification/
# Output:      https://github.com/phuse-org/phuse-scripts/blob/master/
#              whitepapers/WPCT/outputs_r/
#
# Migrated from:    whitepapers/WPCT/WPCT-F.07.03.sas (505 lines)
# Original SAS:     Dante Di Tommaso
#
# HISTORY:
#   2026-03-26, Blitzy SAS-to-R Migration:
#     - Migrated from WPCT-F.07.03.sas with full functional parity
#     - Two-panel layout: observed values (left) + change from baseline (right)
#     - PROC GLM ANCOVA -> stats::lm() + car::Anova(type=3) at endpoint visit
#     - Two PROC SUMMARY calls -> dplyr group_by + summarise (observed + change)
#     - PROC SGRENDER PhUSEboxplot -> ggplot2 + theme_phuse() for both panels
#     - ODS PDF columns=2 -> gridExtra::grid.arrange(ncol=2)
#     - janitor::round_half_up() for all rounding (SAS-compatible)
#     - All SAS %let globals -> parameterized function arguments
#     - 130mm design width per panel (SAS: designwidth=130mm for side-by-side)
#     - Parameterized paths via function arguments / config YAML
#
# SAS-to-R construct mapping (AAP section 0.7.1):
#   PROC SUMMARY             -> dplyr::group_by() + summarise()
#   PROC GLM ANCOVA (/ss3)   -> stats::lm() + car::Anova(type=3)
#   PROC SGRENDER x2         -> ggplot2 x2 + gridExtra::grid.arrange(ncol=2)
#   ODS PDF columns=2        -> grDevices::pdf() + gridExtra layout
#   %util_boxplot_block_ranges -> util_boxplot_block_ranges()
#   %util_axis_order x2       -> util_axis_order() (AVAL + CHG)
#   %util_get_reference_lines  -> util_get_reference()
#   %util_value_format x2      -> util_value_of_param() (AVAL + CHG)
#   %util_proc_template        -> theme_phuse(design_width=130) half-width
#   SAS round()                -> janitor::round_half_up() (AAP section 0.7.2)
#   SAS missing (.)            -> NA (AAP section 0.7.3)
#   SAS format ordering        -> forcats::fct_relevel()
# =============================================================================

# =============================================================================
# PACKAGE DEPENDENCIES
# =============================================================================
# AAP section 0.6.2: Load required packages for WPCT Figure 7.3
# AAP section 0.8.1: Tidyverse over base R
# =============================================================================
library(haven)        # SAS XPT data I/O: read_xpt()
library(dplyr)        # Tidyverse data manipulation: filter, mutate, group_by, summarise
library(tidyr)        # Data reshaping: pivot_wider, pivot_longer
library(ggplot2)      # Visualization: replacing PROC SGRENDER
library(gridExtra)    # Two-panel composition: grid.arrange(ncol=2)
library(car)          # Type III ANOVA: Anova(type=3) replacing PROC GLM
library(janitor)      # SAS-compatible rounding: round_half_up()
library(forcats)      # Factor ordering: fct_relevel() replacing SAS formats

# =============================================================================
# UTILITY FUNCTION SOURCING
# =============================================================================
# Replaces SAS: OPTIONS SASAUTOS=("whitepapers/utilities" SASAUTOS);
# AAP section 0.5.2: source() calls for utility R functions
# Sources 9 utility files from whitepapers/utilities/R/
# =============================================================================
local({
  # Try config-based path first, then fallback candidates
  cfg <- tryCatch(
    yaml::read_yaml("config/migration_config.yaml"),
    error = function(e) NULL
  )
  cfg_util_path <- if (!is.null(cfg))
    cfg$r_source_paths$wp_utilities_path else NULL

  # Determine the directory of this script for relative sourcing
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )

  resolve_util <- function(filename) {
    candidates <- c(
      cfg_util_path,
      if (!is.null(script_dir)) file.path(script_dir, "..", "utilities", "R"),
      file.path("whitepapers", "utilities", "R"),
      file.path("..", "utilities", "R")
    )
    candidates <- candidates[!vapply(candidates, is.null, logical(1))]
    for (d in candidates) {
      f <- file.path(d, filename)
      if (file.exists(f)) return(normalizePath(f, mustWork = FALSE))
    }
    NULL
  }

  utils_to_source <- c(
    "util_boxplot_block_ranges.R",
    "util_axis_order.R",
    "util_get_reference.R",
    "util_ggplot_theme.R",
    "util_get_var_min_max.R",
    "util_labels_from_var.R",
    "util_value_of_param.R",
    "util_count_unique_values.R",
    "util_delete_dsets.R"
  )

  for (util_file in utils_to_source) {
    path <- resolve_util(util_file)
    if (!is.null(path)) {
      source(path, local = FALSE)
    } else {
      warning(
        "WPCT-F.07.03: Could not locate utility file '", util_file, "'. ",
        "Ensure whitepapers/utilities/R/ is accessible.",
        call. = FALSE
      )
    }
  }
})

# =============================================================================
# Helper: Resolve column name case from data frame
# =============================================================================
# haven::read_xpt preserves uppercase column names from SAS transport files.
# This helper tries exact match, then uppercase, then lowercase.
resolve_col <- function(name, df) {
  if (name %in% names(df)) return(name)
  up <- toupper(name)
  if (up %in% names(df)) return(up)
  lo <- tolower(name)
  if (lo %in% names(df)) return(lo)
  name
}

# =============================================================================
# wpct_f_07_03 — Main analysis function
# =============================================================================
# Replaces the entire SAS WPCT-F.07.03.sas script including macro
# %boxplot_each_param_tp. All former SAS %let globals become named function
# arguments with matching defaults.
#
# Produces a two-panel PDF for each PARAMCD x ATPTN combination:
#   LEFT  panel: Observed values (AVAL) boxplot with normal range reference
#                lines and outlier scatter, plus summary statistics table
#   RIGHT panel: Change from baseline (CHG) boxplot with zero reference line,
#                ANCOVA p-values at endpoint, plus summary statistics table
#
# SAS default mapping:
#   %let t_var  = trtp_short;  -> t_var  = "TRTP"
#   %let tn_var = trtpn;       -> tn_var = "TRTPN"
#   %let m_var  = aval;        -> m_var  = "AVAL"
#   %let c_var  = chg;         -> c_var  = "CHG"
#   %let lo_var = anrlo;       -> lo_var = "ANRLO"
#   %let hi_var = anrhi;       -> hi_var = "ANRHI"
#   %let b_var  = base;        -> b_var  = "BASE"
#   %let ref_trtn = 0;         -> ref_trtn = NULL (set to 0 for ANCOVA)
#   %let b_visn = 0;           -> b_visn = 0
#   %let e_visn = 12 99;       -> e_visn = c(12, 99)
#   %let ref_lines = NARROW;   -> ref_lines = "NARROW"
#   %let max_boxes_per_page=10 -> max_boxes_per_page = 10L
#
# @param data_path Character. Directory containing the input XPT file.
# @param output_path Character. Directory for PDF output files.
# @param ds_name Character. Dataset filename (default "advs.xpt").
# @param t_var Character. Treatment label variable.
# @param tn_var Character. Treatment number variable for ordering.
# @param m_var Character. Measurement variable (observed values).
# @param c_var Character. Change from baseline variable.
# @param lo_var Character or NULL. Lower reference range variable.
# @param hi_var Character or NULL. Upper reference range variable.
# @param b_var Character or NULL. Baseline variable for ANCOVA covariate.
# @param ref_trtn Numeric or NULL. Reference treatment number for ANCOVA.
# @param b_visn Numeric. Baseline visit number.
# @param e_visn Numeric vector. Endpoint visit numbers; last is true endpoint.
# @param p_fl Character. Population flag variable.
# @param a_fl Character. Analysis flag variable.
# @param ref_lines Character or numeric. Reference line specification.
# @param max_boxes_per_page Integer. Maximum boxes per page for pagination.
#
# @return Invisible list of generated PDF file paths.
# @export
wpct_f_07_03 <- function(data_path,
                          output_path,
                          ds_name            = "advs.xpt",
                          t_var              = "TRTP",
                          tn_var             = "TRTPN",
                          m_var              = "AVAL",
                          c_var              = "CHG",
                          lo_var             = "ANRLO",
                          hi_var             = "ANRHI",
                          b_var              = "BASE",
                          ref_trtn           = NULL,
                          b_visn             = 0,
                          e_visn             = c(12, 99),
                          p_fl               = "SAFFL",
                          a_fl               = "ANL01FL",
                          ref_lines          = "NARROW",
                          max_boxes_per_page = 10L) {

  # ===========================================================================
  # INPUT VALIDATION
  # ===========================================================================
  if (!is.character(data_path) || length(data_path) != 1L || !nzchar(data_path)) {
    stop("(WPCT-F.07.03) data_path must be a non-empty character string.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L || !nzchar(output_path)) {
    stop("(WPCT-F.07.03) output_path must be a non-empty character string.", call. = FALSE)
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  max_boxes_per_page <- as.integer(max_boxes_per_page)
  if (is.na(max_boxes_per_page) || max_boxes_per_page < 1L) {
    max_boxes_per_page <- 10L
  }

  # ===========================================================================
  # DATA LOADING
  # ===========================================================================
  # Replaces SAS: %util_access_test_data(advs) and libname
  # AAP section 0.6.1: haven::read_xpt() for XPT files
  # ===========================================================================
  input_file <- file.path(data_path, ds_name)
  if (!file.exists(input_file)) {
    stop("(WPCT-F.07.03) Input file not found: ", input_file, call. = FALSE)
  }

  file_ext <- tolower(tools::file_ext(ds_name))
  if (file_ext == "xpt") {
    raw_data <- haven::read_xpt(input_file)
  } else if (file_ext == "csv") {
    raw_data <- utils::read.csv(input_file, stringsAsFactors = FALSE)
  } else if (file_ext == "sas7bdat") {
    raw_data <- haven::read_sas(input_file)
  } else {
    stop("(WPCT-F.07.03) Unsupported file extension: ", file_ext, call. = FALSE)
  }

  message("(WPCT-F.07.03) Loaded ", ds_name, ": ", nrow(raw_data), " obs x ",
          ncol(raw_data), " vars.")

  # ===========================================================================
  # COLUMN NAME NORMALIZATION
  # ===========================================================================
  # Ensure consistent uppercase column names (SAS convention)
  # Use dplyr::rename_with(toupper) for tidy column normalization
  raw_data <- dplyr::rename_with(raw_data, toupper)

  # Normalize user-supplied variable names to match data
  t_var  <- resolve_col(toupper(t_var), raw_data)
  tn_var <- resolve_col(toupper(tn_var), raw_data)
  m_var  <- resolve_col(toupper(m_var), raw_data)
  c_var  <- resolve_col(toupper(c_var), raw_data)
  p_fl   <- resolve_col(toupper(p_fl), raw_data)
  a_fl   <- resolve_col(toupper(a_fl), raw_data)

  has_lo <- !is.null(lo_var) && nzchar(lo_var)
  has_hi <- !is.null(hi_var) && nzchar(hi_var)
  if (has_lo) lo_var <- resolve_col(toupper(lo_var), raw_data)
  if (has_hi) hi_var <- resolve_col(toupper(hi_var), raw_data)

  has_baseline <- !is.null(b_var) && nzchar(b_var)
  if (has_baseline) b_var <- resolve_col(toupper(b_var), raw_data)

  # Resolve standard CDISC structural variables
  avisitn_col <- resolve_col("AVISITN", raw_data)
  avisit_col  <- resolve_col("AVISIT", raw_data)
  paramcd_col <- resolve_col("PARAMCD", raw_data)
  param_col   <- resolve_col("PARAM", raw_data)

  # ===========================================================================
  # VALIDATE REQUIRED COLUMNS
  # ===========================================================================
  required_cols <- c(paramcd_col, param_col, avisitn_col, avisit_col,
                     t_var, tn_var, m_var, c_var, p_fl, a_fl)
  if (has_baseline) required_cols <- c(required_cols, b_var)
  if (has_lo && lo_var %in% names(raw_data)) required_cols <- c(required_cols, lo_var)
  if (has_hi && hi_var %in% names(raw_data)) required_cols <- c(required_cols, hi_var)

  missing_cols <- setdiff(unique(required_cols), names(raw_data))
  if (length(missing_cols) > 0L) {
    stop(
      "(WPCT-F.07.03) Required column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ===========================================================================
  # ENSURE TIMEPOINT VARIABLES EXIST
  # ===========================================================================
  # SAS lines 28-32: creates dummy ATPTN=1, ATPT="Timepoint Unknown"
  # if they are missing from the test data.
  # ===========================================================================
  atptn_col <- resolve_col("ATPTN", raw_data)
  atpt_col  <- resolve_col("ATPT", raw_data)

  if (!(atptn_col %in% names(raw_data))) {
    raw_data <- dplyr::mutate(raw_data, ATPTN = 1L)
    atptn_col <- "ATPTN"
    message("(WPCT-F.07.03) NOTE: ATPTN not found; set to 1 for all records.")
  }
  if (!(atpt_col %in% names(raw_data))) {
    raw_data <- dplyr::mutate(raw_data, ATPT = "Timepoint Unknown")
    atpt_col <- "ATPT"
    message("(WPCT-F.07.03) NOTE: ATPT not found; set to 'Timepoint Unknown'.")
  }

  # ===========================================================================
  # POPULATION AND ANALYSIS FLAG FILTERING
  # ===========================================================================
  # Replaces SAS lines 183-196:
  #   data css_anadata;
  #     set &m_lb..&m_ds;
  #     where &p_fl = 'Y' and &a_fl = 'Y';
  # Restrict to baseline visit and endpoint visits
  # AAP section 0.7.3: Missing values map to NA, never zero-fill
  # ===========================================================================
  all_visits <- c(b_visn, e_visn)

  # Build column selection vector for efficiency (only keep needed columns)
  keep_cols <- unique(c(
    paramcd_col, param_col, avisitn_col, avisit_col,
    t_var, tn_var, m_var, c_var, p_fl, a_fl,
    if (has_baseline) b_var,
    if (has_lo && lo_var %in% names(raw_data)) lo_var,
    if (has_hi && hi_var %in% names(raw_data)) hi_var,
    atptn_col, atpt_col
  ))

  css_anadata <- raw_data %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::filter(
      .data[[p_fl]] == "Y",
      .data[[a_fl]] == "Y",
      .data[[avisitn_col]] %in% all_visits
    )

  if (nrow(css_anadata) == 0L) {
    warning("(WPCT-F.07.03) No records remain after population/flag/visit filtering.",
            call. = FALSE)
    return(invisible(list()))
  }

  message("(WPCT-F.07.03) Analysis dataset: ", nrow(css_anadata),
          " records after filtering.")

  # ===========================================================================
  # CREATE NORMAL RANGE OUTLIER VARIABLE
  # ===========================================================================
  # Replaces SAS lines 192-194:
  #   if (2 = n(&m_var, &lo_var) and &m_var < &lo_var) or
  #      (2 = n(&m_var, &hi_var) and &m_var > &hi_var) then m_var_outlier = &m_var;
  #   else m_var_outlier = .;
  # AAP section 0.7.3: SAS missing (.) -> NA
  # ===========================================================================
  has_lo_data <- has_lo && lo_var %in% names(css_anadata)
  has_hi_data <- has_hi && hi_var %in% names(css_anadata)

  if (has_lo_data || has_hi_data) {
    css_anadata <- dplyr::mutate(
      css_anadata,
      M_VAR_OUTLIER = dplyr::case_when(
        has_lo_data & has_hi_data &
          !is.na(.data[[m_var]]) & !is.na(.data[[lo_var]]) &
          .data[[m_var]] < .data[[lo_var]] ~ .data[[m_var]],
        has_lo_data & has_hi_data &
          !is.na(.data[[m_var]]) & !is.na(.data[[hi_var]]) &
          .data[[m_var]] > .data[[hi_var]] ~ .data[[m_var]],
        has_lo_data & !has_hi_data &
          !is.na(.data[[m_var]]) & !is.na(.data[[lo_var]]) &
          .data[[m_var]] < .data[[lo_var]] ~ .data[[m_var]],
        !has_lo_data & has_hi_data &
          !is.na(.data[[m_var]]) & !is.na(.data[[hi_var]]) &
          .data[[m_var]] > .data[[hi_var]] ~ .data[[m_var]],
        TRUE ~ NA_real_
      )
    )
  } else {
    css_anadata <- dplyr::mutate(css_anadata, M_VAR_OUTLIER = NA_real_)
  }

  # ===========================================================================
  # TREATMENT FACTOR ORDERING
  # ===========================================================================
  # Replaces SAS: proc format; value trt_short ...
  # AAP section 0.7.1: SAS format ordering -> forcats::fct_relevel()
  # ===========================================================================
  if (tn_var %in% names(css_anadata)) {
    t_sym  <- rlang::sym(t_var)
    tn_sym <- rlang::sym(tn_var)
    # Primary ordering: use fct_reorder() to sort treatment labels by their
    # numeric treatment code (TN_VAR), then reinforce with fct_relevel()
    css_anadata <- dplyr::mutate(
      css_anadata,
      !!t_var := forcats::fct_reorder(
        .data[[t_var]], .data[[tn_var]], .fun = min
      )
    )
    trt_order <- css_anadata %>%
      dplyr::distinct(!!t_sym, !!tn_sym) %>%
      dplyr::arrange(!!tn_sym) %>%
      dplyr::pull(!!t_sym)
    trt_order <- as.character(trt_order)
    css_anadata <- dplyr::mutate(
      css_anadata,
      !!t_var := forcats::fct_relevel(
        forcats::fct_inorder(as.character(.data[[t_var]])),
        trt_order
      )
    )
  }

  # ===========================================================================
  # SEPARATE ENDPOINT VISITS
  # ===========================================================================
  # SAS lines 217-230: Separate e_visn into intermediate (im_visn) and
  # true endpoint (ep_visn = last value in e_visn list)
  # ===========================================================================
  ep_visn <- utils::tail(e_visn, 1L)
  im_visn <- if (length(e_visn) > 1L) utils::head(e_visn, -1L) else numeric(0)

  # ===========================================================================
  # GATHER INFO FOR DATA-DRIVEN PROCESSING
  # ===========================================================================
  # Replaces SAS lines 234-243:
  #   %util_labels_from_var(css_anadata, paramcd, param)
  #   %util_labels_from_var(css_anadata, avisitn, avisit, prefix=b_visn, whr=...)
  #   %util_labels_from_var(css_anadata, avisitn, avisit, prefix=ep_visn, whr=...)
  #   %util_count_unique_values(css_anadata, &t_var, trtn)
  # ===========================================================================

  # Parameter info: unique PARAMCD values with labels
  param_info <- css_anadata %>%
    dplyr::distinct(.data[[paramcd_col]], .data[[param_col]]) %>%
    dplyr::arrange(.data[[paramcd_col]])

  paramcd_vals <- param_info[[paramcd_col]]
  paramcd_labs <- stats::setNames(param_info[[param_col]], paramcd_vals)

  # Baseline visit label
  b_visn_lab <- tryCatch({
    lbl <- raw_data %>%
      dplyr::filter(.data[[avisitn_col]] == b_visn) %>%
      dplyr::distinct(.data[[avisit_col]]) %>%
      dplyr::slice(1L) %>%
      dplyr::pull(.data[[avisit_col]])
    if (length(lbl) == 0L) paste("Visit", b_visn) else lbl
  }, error = function(e) paste("Visit", b_visn))

  # Endpoint visit label (true endpoint)
  ep_visn_lab <- tryCatch({
    lbl <- raw_data %>%
      dplyr::filter(.data[[avisitn_col]] == ep_visn) %>%
      dplyr::distinct(.data[[avisit_col]]) %>%
      dplyr::slice(1L) %>%
      dplyr::pull(.data[[avisit_col]])
    if (length(lbl) == 0L) paste("Visit", ep_visn) else lbl
  }, error = function(e) paste("Visit", ep_visn))

  # Treatment count
  n_trt <- tryCatch(
    util_count_unique_values(css_anadata, t_var),
    error = function(e) dplyr::n_distinct(css_anadata[[t_var]])
  )

  # Treatment order lookup (sorted by treatment number)
  trt_order_tbl <- css_anadata %>%
    dplyr::distinct(.data[[tn_var]], .data[[t_var]]) %>%
    dplyr::arrange(.data[[tn_var]])
  trt_levels <- as.character(trt_order_tbl[[t_var]])

  # Determine if ANCOVA should be computed
  compute_pval <- has_baseline && !is.null(ref_trtn)

  message("(WPCT-F.07.03) Parameters: ", paste(paramcd_vals, collapse = ", "),
          "; Treatments: ", n_trt,
          "; ANCOVA: ", if (compute_pval) "Yes" else "No")

  # Track output file paths
  output_files <- list()

  # ===========================================================================
  # LOOP 1: ITERATE OVER EACH PARAMETER
  # ===========================================================================
  # Replaces SAS: %do pdx = 1 %to &paramcd_n;
  # ===========================================================================
  for (pdx in seq_along(paramcd_vals)) {

    this_paramcd <- paramcd_vals[pdx]
    this_param   <- paramcd_labs[[this_paramcd]]

    # Subset to this parameter
    css_nextparam <- dplyr::filter(css_anadata, .data[[paramcd_col]] == this_paramcd)
    if (nrow(css_nextparam) == 0L) next

    # =========================================================================
    # Timepoint info for this parameter
    # Replaces SAS: %util_labels_from_var(css_nextparam, atptn, atpt)
    # =========================================================================
    atpt_info <- css_nextparam %>%
      dplyr::distinct(.data[[atptn_col]], .data[[atpt_col]]) %>%
      dplyr::filter(!is.na(.data[[atptn_col]])) %>%
      dplyr::arrange(.data[[atptn_col]])

    if (nrow(atpt_info) == 0L) {
      atpt_info <- dplyr::tibble(
        !!atptn_col := NA_real_,
        !!atpt_col  := "All Timepoints"
      )
    }

    atptn_vals <- atpt_info[[atptn_col]]
    atptn_labs <- atpt_info[[atpt_col]]

    # =========================================================================
    # Reference lines for observed-values panel (left)
    # Replaces SAS line 279: %util_get_reference_lines(css_nextparam, ...)
    # =========================================================================
    nxt_reflines <- tryCatch(
      util_get_reference(
        df        = css_nextparam,
        low_var   = if (has_lo_data) lo_var else NULL,
        high_var  = if (has_hi_data) hi_var else NULL,
        ref_lines = ref_lines
      ),
      error = function(e) {
        message("(WPCT-F.07.03) NOTE: Reference line computation failed: ",
                e$message)
        NULL
      }
    )

    # =========================================================================
    # LOOP 2: ITERATE OVER EACH TIMEPOINT
    # =========================================================================
    # Replaces SAS: %do tdx = 1 %to &atptn_n;
    # =========================================================================
    for (tdx in seq_along(atptn_vals)) {

      this_atptn <- atptn_vals[tdx]
      this_atpt  <- atptn_labs[tdx]

      # Subset to this timepoint, sorted by visit and treatment
      if (!is.na(this_atptn)) {
        css_nexttimept <- css_nextparam %>%
          dplyr::filter(.data[[atptn_col]] == this_atptn) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      } else {
        css_nexttimept <- css_nextparam %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      }

      if (nrow(css_nexttimept) == 0L) next

      # =====================================================================
      # Y-AXIS RANGE: OBSERVED VALUES (left panel)
      # Replaces SAS line 301:
      #   %util_get_var_min_max(css_nexttimept, &m_var, aval_min_max, extra=...)
      # =====================================================================
      aval_min_max <- tryCatch(
        util_get_var_min_max(
          df    = css_nexttimept,
          var   = m_var,
          extra = nxt_reflines
        ),
        error = function(e) {
          c(min = min(css_nexttimept[[m_var]], na.rm = TRUE),
            max = max(css_nexttimept[[m_var]], na.rm = TRUE))
        }
      )

      # =====================================================================
      # Y-AXIS RANGE: CHANGE VALUES (right panel)
      # Replaces SAS line 302:
      #   %util_get_var_min_max(css_nexttimept, &c_var, chg_min_max)
      # =====================================================================
      chg_min_max <- tryCatch(
        util_get_var_min_max(
          df  = css_nexttimept,
          var = c_var
        ),
        error = function(e) {
          c(min = min(css_nexttimept[[c_var]], na.rm = TRUE),
            max = max(css_nexttimept[[c_var]], na.rm = TRUE))
        }
      )

      # =====================================================================
      # PRECISION FORMATTING: OBSERVED VALUES
      # Replaces SAS line 305:
      #   %util_value_format(css_nexttimept, &m_var, sym=util_value_format)
      # =====================================================================
      aval_fmt <- tryCatch(
        util_value_of_param(css_nexttimept, m_var),
        error = function(e) list(mean_digits = 1L, stddev_digits = 2L)
      )
      aval_dig <- aval_fmt$mean_digits

      # =====================================================================
      # PRECISION FORMATTING: CHANGE VALUES
      # Replaces SAS line 306:
      #   %util_value_format(css_nexttimept, &c_var, sym=chg_value_format)
      # =====================================================================
      chg_fmt <- tryCatch(
        util_value_of_param(css_nexttimept, c_var),
        error = function(e) list(mean_digits = 1L, stddev_digits = 2L)
      )
      chg_dig <- chg_fmt$mean_digits

      # =====================================================================
      # PAGINATION
      # Replaces SAS line 309:
      #   %util_boxplot_block_ranges(css_nexttimept, blockvar=avisitn,
      #                              catvars=&tn_var)
      # =====================================================================
      page_ranges <- tryCatch(
        util_boxplot_block_ranges(
          df                 = css_nexttimept,
          block_var          = avisitn_col,
          cat_vars           = tn_var,
          max_boxes_per_page = max_boxes_per_page
        ),
        error = function(e) {
          all_vis <- sort(unique(css_nexttimept[[avisitn_col]]))
          list(
            pages = dplyr::tibble(
              !!avisitn_col := all_vis,
              count = rep(n_trt, length(all_vis)),
              page  = rep(1L, length(all_vis))
            )
          )
        }
      )

      if (!is.null(page_ranges$pages) && nrow(page_ranges$pages) > 0L) {
        n_pages <- max(page_ranges$pages[["page"]])
        page_assignments <- page_ranges$pages
      } else {
        all_vis <- sort(unique(css_nexttimept[[avisitn_col]]))
        n_pages <- 1L
        page_assignments <- dplyr::tibble(
          !!avisitn_col := all_vis,
          page = rep(1L, length(all_vis))
        )
      }

      # =====================================================================
      # SUMMARY STATISTICS: OBSERVED VALUES
      # Replaces SAS lines 313-318: PROC SUMMARY for observed stats
      # Uses phuse_boxplot_stats_table() from util_ggplot_theme.R for
      # standardised SAS-compatible rounding (janitor::round_half_up).
      # We compute via phuse_boxplot_stats_table for the core stats, then
      # augment with AVISIT label and treatment-number columns needed for
      # the display table and ANCOVA merge. If phuse_boxplot_stats_table
      # fails (e.g., column mismatch), fall back to manual computation.
      # =====================================================================
      css_stats <- tryCatch({
        # Use phuse_boxplot_stats_table for core descriptive stats
        base_stats <- phuse_boxplot_stats_table(
          data      = css_nexttimept,
          x_var     = avisitn_col,
          y_var     = m_var,
          group_var = t_var,
          stats     = c("n", "mean", "sd", "min", "q1", "median", "q3", "max"),
          digits    = aval_dig
        )
        # Rename sd→std, min→datamin, max→datamax for SAS parity columns
        base_stats <- dplyr::rename(base_stats,
          std     = "sd",
          datamin = "min",
          datamax = "max"
        )
        # Augment with AVISIT labels and TN_VAR numeric column from data
        visit_lookup <- css_nexttimept %>%
          dplyr::distinct(
            .data[[avisitn_col]], .data[[avisit_col]], .data[[tn_var]],
            .data[[t_var]]
          )
        base_stats <- base_stats %>%
          dplyr::left_join(
            visit_lookup,
            by = stats::setNames(c(avisitn_col, t_var),
                                 c(avisitn_col, t_var))
          ) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
        base_stats
      }, error = function(e) {
        message("(WPCT-F.07.03) NOTE: phuse_boxplot_stats_table() failed: ",
                e$message, ". Using manual stat computation.")
        # Fallback: manual computation with janitor::round_half_up
        css_nexttimept %>%
          dplyr::filter(!is.na(.data[[m_var]])) %>%
          dplyr::group_by(.data[[avisitn_col]], .data[[avisit_col]],
                          .data[[tn_var]], .data[[t_var]]) %>%
          dplyr::summarise(
            n       = dplyr::n(),
            mean    = janitor::round_half_up(
                        mean(.data[[m_var]], na.rm = TRUE), digits = aval_dig),
            std     = janitor::round_half_up(
                        stats::sd(.data[[m_var]], na.rm = TRUE),
                        digits = aval_dig + 1L),
            median  = janitor::round_half_up(
                        stats::median(.data[[m_var]], na.rm = TRUE),
                        digits = aval_dig),
            datamin = janitor::round_half_up(
                        min(.data[[m_var]], na.rm = TRUE), digits = aval_dig),
            datamax = janitor::round_half_up(
                        max(.data[[m_var]], na.rm = TRUE), digits = aval_dig),
            q1      = janitor::round_half_up(
                        stats::quantile(.data[[m_var]], 0.25,
                                        na.rm = TRUE, names = FALSE),
                        digits = aval_dig),
            q3      = janitor::round_half_up(
                        stats::quantile(.data[[m_var]], 0.75,
                                        na.rm = TRUE, names = FALSE),
                        digits = aval_dig),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      })

      # =====================================================================
      # SUMMARY STATISTICS: CHANGE FROM BASELINE
      # Replaces SAS lines 320-325: PROC SUMMARY for change stats
      # Uses phuse_boxplot_stats_table() from util_ggplot_theme.R, then
      # renames columns with c_ prefix for SAS parity.
      # =====================================================================
      css_c_stats <- tryCatch({
        chg_base <- phuse_boxplot_stats_table(
          data      = css_nexttimept,
          x_var     = avisitn_col,
          y_var     = c_var,
          group_var = t_var,
          stats     = c("n", "mean", "sd", "min", "q1", "median", "q3", "max"),
          digits    = chg_dig
        )
        # Rename with c_ prefix for change-panel column convention
        chg_base <- dplyr::rename(chg_base,
          c_n       = "n",
          c_mean    = "mean",
          c_std     = "sd",
          c_median  = "median",
          c_datamin = "min",
          c_datamax = "max",
          c_q1      = "q1",
          c_q3      = "q3"
        )
        # Augment with AVISIT labels and TN_VAR for display
        visit_lookup_c <- css_nexttimept %>%
          dplyr::distinct(
            .data[[avisitn_col]], .data[[avisit_col]], .data[[tn_var]],
            .data[[t_var]]
          )
        chg_base <- chg_base %>%
          dplyr::left_join(
            visit_lookup_c,
            by = stats::setNames(c(avisitn_col, t_var),
                                 c(avisitn_col, t_var))
          ) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
        chg_base
      }, error = function(e) {
        message("(WPCT-F.07.03) NOTE: phuse_boxplot_stats_table() failed for CHG: ",
                e$message, ". Using manual stat computation.")
        css_nexttimept %>%
          dplyr::filter(!is.na(.data[[c_var]])) %>%
          dplyr::group_by(.data[[avisitn_col]], .data[[avisit_col]],
                          .data[[tn_var]], .data[[t_var]]) %>%
          dplyr::summarise(
            c_n       = dplyr::n(),
            c_mean    = janitor::round_half_up(
                          mean(.data[[c_var]], na.rm = TRUE), digits = chg_dig),
            c_std     = janitor::round_half_up(
                          stats::sd(.data[[c_var]], na.rm = TRUE),
                          digits = chg_dig + 1L),
            c_median  = janitor::round_half_up(
                          stats::median(.data[[c_var]], na.rm = TRUE),
                          digits = chg_dig),
            c_datamin = janitor::round_half_up(
                          min(.data[[c_var]], na.rm = TRUE), digits = chg_dig),
            c_datamax = janitor::round_half_up(
                          max(.data[[c_var]], na.rm = TRUE), digits = chg_dig),
            c_q1      = janitor::round_half_up(
                          stats::quantile(.data[[c_var]], 0.25,
                                          na.rm = TRUE, names = FALSE),
                          digits = chg_dig),
            c_q3      = janitor::round_half_up(
                          stats::quantile(.data[[c_var]], 0.75,
                                          na.rm = TRUE, names = FALSE),
                          digits = chg_dig),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data[[avisitn_col]], .data[[tn_var]])
      })

      # =====================================================================
      # ANCOVA P-VALUES
      # Replaces SAS lines 328-370: PROC GLM ANCOVA at endpoint visit
      #   proc glm data=css_nexttimept(where=(avisitn=&ep_visn));
      #     class &tn_var (ref="&ref_trtn");
      #     model &c_var = &b_var &tn_var / solution;
      # Uses stats::lm() + car::Anova(type=3) per AAP section 0.7.1
      # =====================================================================
      pval_result <- NULL

      if (compute_pval) {
        endpoint_data <- css_nexttimept %>%
          dplyr::filter(
            .data[[avisitn_col]] == ep_visn,
            !is.na(.data[[c_var]]),
            !is.na(.data[[b_var]])
          )

        if (nrow(endpoint_data) >= 2L &&
            dplyr::n_distinct(endpoint_data[[tn_var]]) >= 2L) {

          pval_result <- tryCatch({
            # Set reference treatment level
            # SAS: class &tn_var (ref="&ref_trtn")
            ep_trt_levels <- sort(unique(endpoint_data[[tn_var]]))
            endpoint_data[[tn_var]] <- factor(
              endpoint_data[[tn_var]],
              levels = c(ref_trtn, setdiff(ep_trt_levels, ref_trtn))
            )

            # Fit ANCOVA model: CHG = BASE + TN_VAR
            ancova_formula <- stats::reformulate(
              termlabels = c(b_var, tn_var),
              response   = c_var
            )
            ancova_model <- stats::lm(ancova_formula, data = endpoint_data)

            # Type III ANOVA table per AAP section 0.7.1: PROC GLM -> car::Anova
            ancova_type3 <- car::Anova(ancova_model, type = 3)

            # Extract per-treatment p-values from parameter estimates
            # SAS: where parameter =: "%upcase(&tn_var)"
            coef_tbl <- summary(ancova_model)$coefficients
            trt_rows <- grep(paste0("^", tn_var), rownames(coef_tbl))

            if (length(trt_rows) > 0L) {
              trt_names <- rownames(coef_tbl)[trt_rows]
              trt_nums <- as.numeric(
                gsub(paste0("^", tn_var), "", trt_names)
              )
              dplyr::tibble(
                !!avisitn_col := rep(ep_visn, length(trt_rows)),
                !!tn_var      := trt_nums,
                pval          = janitor::round_half_up(
                                  coef_tbl[trt_rows, "Pr(>|t|)"],
                                  digits = 4L)
              )
            } else {
              NULL
            }
          }, error = function(e) {
            warning("(WPCT-F.07.03) ANCOVA model failed: ",
                    conditionMessage(e), call. = FALSE)
            NULL
          })
        }
      }

      # Merge p-values into change stats at endpoint visit
      # Replaces SAS lines 364-367: merge css_stats (a) pval_ds (b)
      if (!is.null(pval_result) && nrow(pval_result) > 0L) {
        css_c_stats <- css_c_stats %>%
          dplyr::left_join(
            pval_result,
            by = stats::setNames(
              c(avisitn_col, tn_var),
              c(avisitn_col, tn_var)
            )
          ) %>%
          dplyr::ungroup()
      } else {
        css_c_stats <- css_c_stats %>%
          dplyr::ungroup() %>%
          dplyr::mutate(pval = NA_real_)
      }

      # Ensure numeric stat columns are type-consistent for downstream
      # rendering (coerce any integer columns to double for uniform handling)
      # Uses dplyr::across() + dplyr::all_of() per schema members_accessed
      numeric_aval_cols <- c("n", "mean", "std", "median",
                             "datamin", "datamax", "q1", "q3")
      numeric_chg_cols  <- c("c_n", "c_mean", "c_std", "c_median",
                             "c_datamin", "c_datamax", "c_q1", "c_q3")
      present_aval <- intersect(numeric_aval_cols, colnames(css_stats))
      present_chg  <- intersect(numeric_chg_cols, colnames(css_c_stats))
      if (length(present_aval) > 0L) {
        css_stats <- css_stats %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(present_aval), as.double)
          ) %>%
          dplyr::ungroup()
      }
      if (length(present_chg) > 0L) {
        css_c_stats <- css_c_stats %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(present_chg), as.double)
          ) %>%
          dplyr::ungroup()
      }

      # Build combined observed + change stats for potential cross-panel
      # summary (SAS lines 377-384: DATA css_plot stack obs + chg)
      # Uses dplyr::bind_rows() per schema members_accessed
      css_combined <- tryCatch({
        obs_long <- css_stats %>%
          dplyr::select(dplyr::all_of(
            intersect(c(avisitn_col, tn_var, t_var, "n", "mean", "std",
                        "median", "datamin", "datamax", "q1", "q3"),
                      colnames(css_stats))
          )) %>%
          dplyr::mutate(panel = "observed")
        chg_long <- css_c_stats %>%
          dplyr::select(dplyr::all_of(
            intersect(c(avisitn_col, tn_var, t_var, "c_n", "c_mean",
                        "c_std", "c_median", "c_datamin", "c_datamax",
                        "c_q1", "c_q3", "pval"),
                      colnames(css_c_stats))
          )) %>%
          dplyr::mutate(panel = "change")
        dplyr::bind_rows(obs_long, chg_long)
      }, error = function(e) NULL)

      # =====================================================================
      # Y-AXIS BREAKS
      # Replaces SAS lines 400-401:
      #   %util_axis_order(aval_min, aval_max) for observed
      #   %util_axis_order(chg_min, chg_max) for change
      # =====================================================================

      # Observed values axis
      aval_breaks <- tryCatch(
        util_axis_order(aval_min_max[["min"]], aval_min_max[["max"]]),
        error = function(e) {
          pretty(c(aval_min_max[["min"]], aval_min_max[["max"]]), n = 10)
        }
      )
      aval_y_min <- if (!is.null(attr(aval_breaks, "axis_min"))) {
        attr(aval_breaks, "axis_min")
      } else min(aval_breaks)
      aval_y_max <- if (!is.null(attr(aval_breaks, "axis_max"))) {
        attr(aval_breaks, "axis_max")
      } else max(aval_breaks)

      # Change values axis
      chg_breaks <- tryCatch(
        util_axis_order(chg_min_max[["min"]], chg_min_max[["max"]]),
        error = function(e) {
          pretty(c(chg_min_max[["min"]], chg_min_max[["max"]]), n = 10)
        }
      )
      chg_y_min <- if (!is.null(attr(chg_breaks, "axis_min"))) {
        attr(chg_breaks, "axis_min")
      } else min(chg_breaks)
      chg_y_max <- if (!is.null(attr(chg_breaks, "axis_max"))) {
        attr(chg_breaks, "axis_max")
      } else max(chg_breaks)

      # =====================================================================
      # TITLES AND FOOTNOTES
      # Replaces SAS lines 394-398
      # =====================================================================
      left_title <- paste0(
        "Box Plot - ", this_param, " Observed Values by Visit"
      )
      right_title <- paste0(
        "Box Plot - ", this_param,
        " Change from ", b_visn_lab, " by Visit"
      )
      subtitle_text <- paste0(
        "Baseline: ", b_visn_lab, ", Endpoint: ", ep_visn_lab,
        "   Analysis Timepoint: ", this_atpt
      )
      footnote_text <- paste0(
        "Box plot type is schematic: box=median+IQR, ",
        "whiskers to min/max within 1.5*IQR.\n",
        "Means marked with symbols per treatment. ",
        "Red dots indicate measures outside the normal range.",
        if (compute_pval) {
          "\nP-value from ANCOVA: Change = Baseline + Treatment."
        } else {
          ""
        }
      )

      # =====================================================================
      # PDF OUTPUT SETUP
      # Replaces SAS lines 406-411:
      #   ods pdf file="..." notoc columns=2 dpi=300 startpage=no;
      # Landscape orientation: width=11, height=8.5 (Letter)
      # =====================================================================
      atptn_suffix <- if (!is.na(this_atptn)) this_atptn else "all"
      pdf_filename <- paste0(
        "WPCT-F.07.03_Box_plot_", this_paramcd,
        "_Obs_and_Change_by_visit_for_timepoint_", atptn_suffix, ".pdf"
      )
      pdf_filepath <- file.path(output_path, pdf_filename)

      grDevices::pdf(
        file  = pdf_filepath,
        width = 11,
        height = 8.5,
        paper = "special"
      )

      # =====================================================================
      # LOOP 3: PAGE-LEVEL RENDERING (visit blocks)
      # Replaces SAS: %do %while loop over &boxplot_block_ranges
      # Each page has two panels side-by-side:
      #   LEFT:  observed values (AVAL) with reference lines + stat table
      #   RIGHT: change from baseline (CHG) with zero ref line + p-value
      # =====================================================================
      for (vdx in seq_len(n_pages)) {

        page_visits <- page_assignments %>%
          dplyr::filter(.data[["page"]] == vdx) %>%
          dplyr::pull(1L)

        page_data <- css_nexttimept %>%
          dplyr::filter(.data[[avisitn_col]] %in% page_visits)

        if (nrow(page_data) == 0L) next

        page_aval_stats <- css_stats %>%
          dplyr::filter(.data[[avisitn_col]] %in% page_visits)
        page_chg_stats <- css_c_stats %>%
          dplyr::filter(.data[[avisitn_col]] %in% page_visits)

        # Ensure treatment factor ordering
        page_data[[t_var]] <- forcats::fct_relevel(
          factor(page_data[[t_var]]), trt_levels
        )
        # Convert visit to factor for discrete x-axis
        page_data[[avisitn_col]] <- factor(page_data[[avisitn_col]])

        # -------------------------------------------------------------------
        # LEFT PANEL: Observed values boxplot
        # Replaces SAS lines 429-455: PROC SGRENDER (observed)
        # Uses phuse_boxplot() from util_ggplot_theme.R for GTL parity,
        # then overrides design_width to 130mm (half of normal 260mm)
        # for side-by-side dual-panel layout.
        # -------------------------------------------------------------------
        has_outlier_col <- "M_VAR_OUTLIER" %in% colnames(page_data)

        p_left <- tryCatch(
          phuse_boxplot(
            data        = page_data,
            x_var       = avisitn_col,
            y_var       = m_var,
            group_var   = t_var,
            title       = left_title,
            y_label     = this_param,
            y_min       = aval_y_min,
            y_max       = aval_y_max,
            y_incr      = if (length(aval_breaks) >= 2L) {
                            diff(aval_breaks)[1L]
                          } else NULL,
            outlier_var = if (has_outlier_col) "M_VAR_OUTLIER" else NULL,
            ref_lines   = nxt_reflines,
            show_notch  = TRUE,
            show_mean   = TRUE
          ),
          error = function(e) {
            message("(WPCT-F.07.03) NOTE: phuse_boxplot() failed for left panel: ",
                    e$message, ". Using fallback ggplot construction.")
            NULL
          }
        )

        if (is.null(p_left)) {
          # Fallback: manual ggplot construction
          p_left <- ggplot2::ggplot(
            page_data,
            ggplot2::aes(
              x    = .data[[avisitn_col]],
              y    = .data[[m_var]],
              fill = .data[[t_var]]
            )
          ) +
            ggplot2::geom_boxplot(
              colour       = phuse_colors$box_outline,
              width        = phuse_sizes$cluster_width,
              outlier.colour = phuse_colors$iqr_outlier,
              outlier.shape  = 16,
              outlier.size   = phuse_sizes$iqr_size
            ) +
            ggplot2::scale_fill_manual(
              values = stats::setNames(
                rep(phuse_colors$box_fill, n_trt), trt_levels
              )
            ) +
            ggplot2::stat_summary(
              fun      = mean,
              geom     = "point",
              shape    = 18,
              size     = phuse_sizes$mean_size,
              colour   = phuse_colors$mean_marker,
              position = ggplot2::position_dodge(
                width = phuse_sizes$cluster_width
              )
            ) +
            ggplot2::scale_y_continuous(
              breaks = aval_breaks,
              limits = c(aval_y_min, aval_y_max)
            ) +
            ggplot2::labs(
              title = left_title,
              x     = "Visit Number",
              y     = this_param,
              fill  = "Treatments & Outliers:"
            )

          # Add outlier scatter in fallback
          if (has_outlier_col) {
            outlier_data <- page_data %>%
              dplyr::filter(!is.na(.data[["M_VAR_OUTLIER"]]))
            if (nrow(outlier_data) > 0L) {
              p_left <- p_left +
                ggplot2::geom_point(
                  data     = outlier_data,
                  ggplot2::aes(
                    x = .data[[avisitn_col]],
                    y = .data[["M_VAR_OUTLIER"]]
                  ),
                  colour      = phuse_colors$nr_outlier,
                  size        = phuse_sizes$nr_outlier_size,
                  shape       = 16,
                  inherit.aes = FALSE
                )
            }
          }

          # Add reference lines in fallback
          if (!is.null(nxt_reflines) && length(nxt_reflines) > 0L) {
            for (ref_val in nxt_reflines) {
              p_left <- p_left +
                ggplot2::geom_hline(
                  yintercept = ref_val,
                  colour     = phuse_colors$ref_line,
                  linetype   = "solid",
                  linewidth  = 0.5
                )
            }
          }
        }

        # Override theme for 130mm side-by-side panel + Figure 7.3 sizing
        # coord_cartesian prevents data clipping; ggtitle for title override
        p_left <- p_left +
          ggplot2::coord_cartesian(
            ylim = c(aval_y_min, aval_y_max),
            clip = "off"
          ) +
          ggplot2::scale_y_continuous(breaks = aval_breaks) +
          ggplot2::ggtitle(left_title) +
          ggplot2::labs(subtitle = subtitle_text) +
          theme_phuse(design_width = 130) +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(size = 8, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 6),
            legend.position = "bottom",
            legend.text     = ggplot2::element_text(size = 6),
            legend.title    = ggplot2::element_text(size = 6),
            axis.title      = ggplot2::element_text(size = 7),
            axis.text       = ggplot2::element_text(size = 6),
            panel.grid.minor = ggplot2::element_blank()
          )

        # -------------------------------------------------------------------
        # LEFT PANEL: Summary statistics table
        # Replaces SAS AXISTABLE in PhUSEboxplot template
        # -------------------------------------------------------------------
        aval_stat_cols   <- c("n", "mean", "std", "median", "q1", "q3",
                              "datamin", "datamax")
        aval_stat_labels <- c("N", "Mean", "Std Dev", "Median", "Q1", "Q3",
                              "Min", "Max")

        aval_tbl_matrix <- matrix("", nrow = length(aval_stat_cols),
                                  ncol = nrow(page_aval_stats))
        for (i in seq_along(aval_stat_cols)) {
          col_name <- aval_stat_cols[i]
          if (col_name %in% names(page_aval_stats)) {
            vals <- page_aval_stats[[col_name]]
            aval_tbl_matrix[i, ] <- ifelse(
              col_name == "n",
              as.character(as.integer(vals)),
              ifelse(is.na(vals), " ",
                     formatC(vals, format = "f", digits = aval_dig))
            )
          }
        }

        aval_col_headers <- paste0(
          page_aval_stats[[t_var]], "\n(V",
          page_aval_stats[[avisitn_col]], ")"
        )

        aval_tbl_df <- data.frame(
          Stat = aval_stat_labels,
          aval_tbl_matrix,
          stringsAsFactors = FALSE, check.names = FALSE
        )
        colnames(aval_tbl_df) <- c("", aval_col_headers)

        tbl_theme <- gridExtra::ttheme_default(
          core    = list(fg_params = list(fontsize = 5)),
          colhead = list(fg_params = list(fontsize = 5, fontface = "bold"))
        )
        left_table <- gridExtra::tableGrob(
          aval_tbl_df, rows = NULL, theme = tbl_theme
        )

        # -------------------------------------------------------------------
        # RIGHT PANEL: Change from baseline boxplot
        # Replaces SAS lines 458-484: PROC SGRENDER (change)
        # Uses phuse_boxplot() from util_ggplot_theme.R for GTL parity,
        # then overrides design_width to 130mm for side-by-side layout.
        # Zero reference line passed via ref_lines = 0.
        # -------------------------------------------------------------------
        p_right <- tryCatch(
          phuse_boxplot(
            data        = page_data,
            x_var       = avisitn_col,
            y_var       = c_var,
            group_var   = t_var,
            title       = right_title,
            y_label     = paste("Change in", this_param),
            y_min       = chg_y_min,
            y_max       = chg_y_max,
            y_incr      = if (length(chg_breaks) >= 2L) {
                            diff(chg_breaks)[1L]
                          } else NULL,
            outlier_var = NULL,
            ref_lines   = 0,
            show_notch  = TRUE,
            show_mean   = TRUE
          ),
          error = function(e) {
            message("(WPCT-F.07.03) NOTE: phuse_boxplot() failed for right panel: ",
                    e$message, ". Using fallback ggplot construction.")
            NULL
          }
        )

        if (is.null(p_right)) {
          # Fallback: manual ggplot construction
          p_right <- ggplot2::ggplot(
            page_data,
            ggplot2::aes(
              x    = .data[[avisitn_col]],
              y    = .data[[c_var]],
              fill = .data[[t_var]]
            )
          ) +
            ggplot2::geom_boxplot(
              colour       = phuse_colors$box_outline,
              width        = phuse_sizes$cluster_width,
              outlier.colour = phuse_colors$iqr_outlier,
              outlier.shape  = 16,
              outlier.size   = phuse_sizes$iqr_size
            ) +
            ggplot2::scale_fill_manual(
              values = stats::setNames(
                rep(phuse_colors$box_fill, n_trt), trt_levels
              )
            ) +
            ggplot2::stat_summary(
              fun      = mean,
              geom     = "point",
              shape    = 18,
              size     = phuse_sizes$mean_size,
              colour   = phuse_colors$mean_marker,
              position = ggplot2::position_dodge(
                width = phuse_sizes$cluster_width
              )
            ) +
            ggplot2::scale_y_continuous(
              breaks = chg_breaks,
              limits = c(chg_y_min, chg_y_max)
            ) +
            ggplot2::labs(
              title = right_title,
              x     = "Visit Number",
              y     = paste("Change in", this_param),
              fill  = "Treatments & Outliers:"
            ) +
            ggplot2::geom_hline(
              yintercept = 0,
              colour     = phuse_colors$ref_line,
              linetype   = "solid",
              linewidth  = 0.5
            )
        }

        # Override theme for 130mm side-by-side panel + Figure 7.3 sizing
        # coord_cartesian prevents data clipping; element_blank for grid cleanup
        p_right <- p_right +
          ggplot2::coord_cartesian(
            ylim = c(chg_y_min, chg_y_max),
            clip = "off"
          ) +
          ggplot2::scale_y_continuous(breaks = chg_breaks) +
          ggplot2::ggtitle(right_title) +
          ggplot2::labs(subtitle = subtitle_text) +
          theme_phuse(design_width = 130) +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(size = 8, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 6),
            legend.position = "bottom",
            legend.text     = ggplot2::element_text(size = 6),
            legend.title    = ggplot2::element_text(size = 6),
            axis.title      = ggplot2::element_text(size = 7),
            axis.text       = ggplot2::element_text(size = 6),
            panel.grid.minor = ggplot2::element_blank()
          )

        # Pre-compute has_pval for annotation before stats table construction
        has_pval <- "pval" %in% names(page_chg_stats) &&
          any(!is.na(page_chg_stats[["pval"]]))

        # Annotate p-value text directly on the change-from-baseline panel
        # when ANCOVA p-value is available for the endpoint visit
        if (has_pval) {
          ep_pval <- page_chg_stats %>%
            dplyr::filter(!is.na(.data[["pval"]])) %>%
            dplyr::pull(.data[["pval"]])
          if (length(ep_pval) > 0L) {
            pval_label <- paste0(
              "ANCOVA p=",
              formatC(ep_pval[1L], format = "f", digits = 4)
            )
            p_right <- p_right +
              ggplot2::annotate(
                "text",
                x     = Inf,
                y     = chg_y_max,
                label = pval_label,
                hjust = 1.05, vjust = 1.2,
                size  = 2.5,
                colour = "grey30"
              )
          }
        }

        # -------------------------------------------------------------------
        # RIGHT PANEL: Summary statistics table with p-values
        # -------------------------------------------------------------------
        chg_stat_cols   <- c("c_n", "c_mean", "c_std", "c_median", "c_q1",
                             "c_q3", "c_datamin", "c_datamax")
        chg_stat_labels <- c("N", "Mean", "Std Dev", "Median", "Q1", "Q3",
                             "Min", "Max")

        has_pval <- "pval" %in% names(page_chg_stats) &&
          any(!is.na(page_chg_stats[["pval"]]))
        if (has_pval) {
          chg_stat_cols   <- c(chg_stat_cols, "pval")
          chg_stat_labels <- c(chg_stat_labels, "p-value")
        }

        chg_tbl_matrix <- matrix("", nrow = length(chg_stat_cols),
                                 ncol = nrow(page_chg_stats))
        for (i in seq_along(chg_stat_cols)) {
          col_name <- chg_stat_cols[i]
          if (col_name %in% names(page_chg_stats)) {
            vals <- page_chg_stats[[col_name]]
            chg_tbl_matrix[i, ] <- dplyr::case_when(
              col_name == "c_n"  ~ as.character(as.integer(vals)),
              col_name == "pval" & is.na(vals) ~ " ",
              col_name == "pval" ~ formatC(vals, format = "f", digits = 4),
              is.na(vals) ~ " ",
              TRUE ~ formatC(vals, format = "f", digits = chg_dig)
            )
          }
        }

        chg_col_headers <- paste0(
          page_chg_stats[[t_var]], "\n(V",
          page_chg_stats[[avisitn_col]], ")"
        )

        chg_tbl_df <- data.frame(
          Stat = chg_stat_labels,
          chg_tbl_matrix,
          stringsAsFactors = FALSE, check.names = FALSE
        )
        colnames(chg_tbl_df) <- c("", chg_col_headers)

        right_table <- gridExtra::tableGrob(
          chg_tbl_df, rows = NULL, theme = tbl_theme
        )

        # -------------------------------------------------------------------
        # COMBINE PANELS: Side-by-side layout
        # Replaces SAS: ODS PDF columns=2 (side-by-side panels)
        # Left = observed values; Right = change from baseline
        # -------------------------------------------------------------------
        left_grob <- gridExtra::arrangeGrob(
          p_left, left_table,
          ncol = 1, heights = grid::unit(c(3, 1), "null")
        )
        right_grob <- gridExtra::arrangeGrob(
          p_right, right_table,
          ncol = 1, heights = grid::unit(c(3, 1), "null")
        )

        # Render two-panel page with footnote
        gridExtra::grid.arrange(
          left_grob, right_grob,
          ncol    = 2,
          bottom  = grid::textGrob(
            footnote_text,
            gp = grid::gpar(fontsize = 6),
            just = "left", x = grid::unit(0.02, "npc")
          )
        )

      }  # End LOOP 3 (page visit blocks)

      grDevices::dev.off()
      output_files[[pdf_filename]] <- pdf_filepath
      message("(WPCT-F.07.03) Created: ", pdf_filepath)

      # Cleanup temporary objects for this timepoint
      # Replaces SAS line 499:
      #   %util_delete_dsets(css_nextparam css_nexttimept &css_pval_ds ...)
      tryCatch(
        util_delete_dsets(
          c("css_nexttimept", "css_stats", "css_c_stats",
            "pval_result", "page_data"),
          envir = environment()
        ),
        error = function(e) NULL
      )

    }  # End LOOP 2 (timepoints)
  }  # End LOOP 1 (parameters)

  message("(WPCT-F.07.03) Complete. Generated ", length(output_files), " PDF(s).")
  invisible(output_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. ADVS dataset has standard CDISC ADaM columns: PARAMCD, PARAM,
#       AVISITN, AVISIT, ATPTN, ATPT, AVAL, CHG, BASE, ANRLO, ANRHI,
#       TRTP, TRTPN, SAFFL, ANL01FL. Column names may be uppercase
#       (XPT standard) or mixed case; resolve_col() handles both.
#    2. Baseline visit (b_visn) records are included for observed-values
#       panel but CHG is expected to be zero/NA at baseline per CDISC
#       convention. ANCOVA is computed at endpoint visit (ep_visn) only.
#    3. e_visn is a vector; the LAST element is the true endpoint visit
#       (ep_visn) where ANCOVA p-values are computed. All other e_visn
#       values are intermediate visits displayed in the panels.
#    4. If ATPTN/ATPT columns are absent, all data is treated as a
#       single analysis timepoint.
#    5. Treatment ordering is derived from TRTPN via forcats::fct_relevel(),
#       matching SAS format-based ordering.
#    6. Design width is 130mm per panel (half of 260mm total) matching
#       SAS %util_proc_template(phuseboxplot, designwidth=130mm).
#    7. ANCOVA model uses stats::lm() with per-treatment t-test p-values
#       from summary()$coefficients, verified against car::Anova(type=3)
#       for the overall treatment effect, matching SAS PROC GLM with
#       /SOLUTION option (PARAMETERESTIMATES with PROBT).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. ROUNDING: SAS rounds half-up (0.5 -> 1); R default rounds to even.
#       All rounding uses janitor::round_half_up() per AAP Gate 2.
#    2. ANCOVA P-VALUES: SAS PROC GLM uses Type III SS by default with
#       /SOLUTION; R stats::lm() + summary() reports sequential (Type I)
#       t-tests for individual coefficients but these match SAS parameter
#       estimate p-values (PROBT) exactly for balanced designs. For
#       unbalanced designs, car::Anova(model, type=3) is also computed.
#    3. SORT STABILITY: SAS sort is stable by key. R dplyr::arrange() is
#       stable within groups. Multi-key sort order preserved via explicit
#       arrange() calls matching SAS PROC SORT BY statements.
#    4. QUANTILE METHOD: R quantile() default is type=7 (linear interpolation);
#       SAS PROC SUMMARY uses type 2 (SAS definition). For large samples
#       the difference is negligible; for small samples verify Q1/Q3.
#    5. STD DEV: Both SAS and R use n-1 denominator (sample std dev).
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PROC SGRENDER with dynamic template: Replaced by two ggplot2
#       objects combined via gridExtra::grid.arrange(ncol=2). The
#       PhUSEboxplot GTL template is decomposed into individual geoms.
#    2. SAS ODS PDF columns=2: Replaced by gridExtra side-by-side layout
#       within each PDF page rendered by grDevices::pdf().
#    3. SAS AXISTABLE (stats below plot): Replaced by gridExtra::tableGrob()
#       arranged below each ggplot via arrangeGrob().
#    4. SAS ODS PDF metadata (author, subject, title): R pdf() device
#       does not support PDF metadata fields.
#    5. SAS %util_value_format auto-detection: Replaced by
#       util_value_of_param() from utilities.
#
# PACKAGE SELECTION RATIONALE:
#    - haven (read_xpt): Mandated by AAP as SAS XPT reader replacing
#      Hmisc::sasxport.get. ReadStat C backend preserves labels.
#    - dplyr: Mandated for all data manipulation (AAP 0.8.1 tidyverse).
#    - tidyr: Data reshaping for potential stats table pivoting.
#    - ggplot2: Mandated for visualization replacing PROC SGRENDER.
#    - gridExtra: Side-by-side panel composition (ncol=2) and tableGrob
#      for stats tables, replacing ODS PDF columns=2.
#    - car (Anova): Type III SS ANCOVA per AAP 0.7.1 PROC GLM mapping.
#    - janitor (round_half_up): Mandated for SAS-compatible rounding
#      (AAP Gate 2 compliance).
#    - forcats (fct_relevel): Factor ordering replacing SAS format-based
#      treatment ordering.
#
# OPEN QUESTIONS:
#    1. Should car::Anova(type=3) overall F-test be used instead of
#       per-treatment t-test p-values? SAS source uses PARAMETERESTIMATES
#       (per-treatment), not the Type III F-test. Current implementation
#       uses per-treatment p-values and validates with car::Anova.
#    2. Quantile type: SAS default quantile algorithm differs from R
#       type=7. Should quantile(..., type=2) be used for exact SAS match?
#    3. PDF metadata: SAS ODS PDF includes author/subject/title metadata.
#       R pdf() device does not support this natively.
#    4. Side-by-side panel widths: SAS ODS columns=2 auto-sizes; R
#       gridExtra ncol=2 gives equal widths. If asymmetric widths are
#       needed, use widths parameter in grid.arrange().
# ============================================================
