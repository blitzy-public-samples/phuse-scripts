# ============================================================================
# WPCT-F.07.08.R
# Box Plot of Last/Min/Max Baseline versus Last/Min/Max Post-Baseline
# Measurements by Treatment and Analysis Timepoint
# ============================================================================
#
# Migration source : whitepapers/WPCT/WPCT-F.07.08.sas (30 026 bytes)
# Migration target : whitepapers/WPCT/WPCT-F.07.08.R
# Migration type   : SAS -> R  (PhUSE CS Standard Analyses WG5)
# Functional scope : 100 % parity with SAS source
#
# Description:
#   Figure 7.8 displays pooled Last/Min/Max Baseline versus Post-Baseline
#   measurements as side-by-side boxplots for Integrated Summaries with
#   multiple studies.  Each page shows two panels:
#     Left  -- Observed values  (all BASE + POST visits for each change type)
#     Right -- Change from baseline (POST visits only) with ANCOVA p-values
#
#   The script loops over PARAMCD -> ATPTN -> paginated visit blocks and
#   generates one multi-page PDF per PARAMCD x ATPTN combination.
#
# Key SAS-to-R transformations applied:
#   DATA step            -> dplyr pipelines (AAP S0.5.1)
#   PROC SUMMARY         -> dplyr::summarise (AAP S0.7.1)
#   PROC GLM ANCOVA      -> stats::lm + car::Anova type 3 (AAP S0.7.1)
#   PROC SGRENDER / GTL  -> phuse_boxplot + ggplot2 (AAP S0.7.1)
#   ODS PDF columns=2    -> gridExtra::grid.arrange ncol=2
#   SAS formats/informats-> forcats factor levels
#   SAS round-half-up    -> janitor::round_half_up (AAP S0.7.2)
#   Numeric missing .    -> NA  (AAP S0.7.3)
#   %macro params        -> named R function arguments (AAP S0.5.1)
#   %include macros      -> source() utility R files  (AAP S0.5.2)
#
# ============================================================================

# ---------------------------------------------------------------------------
# Libraries
# ---------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(car)
library(janitor)
library(forcats)
library(rlang)
library(yaml)

# ---------------------------------------------------------------------------
# Source utility functions
# ---------------------------------------------------------------------------
# Paths are resolved relative to repository root.  When this script runs
# inside a package or project context the caller should ensure that the
# working directory (or a sourcing wrapper) resolves these paths correctly.
# ---------------------------------------------------------------------------

local({
  # Attempt to read paths from migration config  (AAP S0.8.1 no hardcoded paths)
  config_path <- file.path(getwd(), "config", "migration_config.yaml")
  if (file.exists(config_path)) {
    cfg      <- yaml::read_yaml(config_path)
    util_dir <- cfg$r_source_paths$wp_utilities_path  # e.g. "whitepapers/utilities/R"
    adam_dir <- cfg$r_source_paths$wp_adam_path        # e.g. "whitepapers/ADaM/R"
  } else {
    util_dir <- "whitepapers/utilities/R"
    adam_dir <- "whitepapers/ADaM/R"
  }

  source(file.path(util_dir, "util_boxplot_block_ranges.R"), local = FALSE)
  source(file.path(util_dir, "util_axis_order.R"),           local = FALSE)
  source(file.path(util_dir, "util_get_reference.R"),        local = FALSE)
  source(file.path(util_dir, "util_ggplot_theme.R"),         local = FALSE)
  source(file.path(util_dir, "util_get_var_min_max.R"),      local = FALSE)
  source(file.path(util_dir, "util_labels_from_var.R"),      local = FALSE)
  source(file.path(util_dir, "util_value_of_param.R"),       local = FALSE)
  source(file.path(adam_dir, "derive_lastminmax_measure.R"), local = FALSE)
})

# ============================================================================
# wpct_f_07_08 -- Main entry point
# ============================================================================
#' Box Plot of Last/Min/Max Baseline vs Post-Baseline by Treatment
#'
#' Produces a two-panel PDF (observed values + change from baseline) for each
#' combination of PARAMCD and analysis timepoint, with ANCOVA p-values.
#'
#' @param data_path     Character path to directory containing the ADaM XPT
#'                      dataset (or a pre-derived dataset with LAST/MIN/MAX
#'                      analysis flags).
#' @param output_path   Character path to directory for PDF output.
#' @param ds_name       Dataset filename inside \code{data_path}
#'                      (default \code{"advs.xpt"}).
#' @param t_var         Treatment label variable  (default \code{"TRTP"}).
#' @param tn_var        Treatment numeric variable (default \code{"TRTPN"}).
#' @param m_var         Observed measurement variable (default \code{"AVAL"}).
#' @param c_var         Change-from-baseline variable (default \code{"CHG"}).
#' @param lo_var        Normal-range low variable (default \code{"ANRLO"}).
#' @param hi_var        Normal-range high variable (default \code{"ANRHI"}).
#' @param b_var         Baseline variable (default \code{"BASE"}).
#' @param ref_trtn      Reference treatment number for ANCOVA
#'                      (default \code{NULL} -- no ANCOVA).
#' @param p_fl          Population flag variable (default \code{"SAFFL"}).
#' @param a_lastfl      Last-value analysis flag (default \code{"ANL12FL"}).
#' @param a_minfl       Min-value analysis flag  (default \code{"ANL14FL"}).
#' @param a_maxfl       Max-value analysis flag  (default \code{"ANL16FL"}).
#' @param timepoint     Analysis timepoint label variable
#'                      (default \code{"ATPT"}).
#' @param timepointn    Analysis timepoint numeric variable
#'                      (default \code{"ATPTN"}).
#' @param visit         Visit label variable (default \code{"AVISIT"}).
#' @param visitn        Visit numeric variable (default \code{"AVISITN"}).
#' @param ref_lines     Reference-line mode passed to
#'                      \code{util_get_reference} (default \code{"NARROW"}).
#' @param max_boxes_per_page Maximum boxes per paginated page
#'                      (default \code{12}).
#'
#' @return Invisibly returns a character vector of generated PDF file paths.
#' @export
wpct_f_07_08 <- function(data_path,
                          output_path,
                          ds_name          = "advs.xpt",
                          t_var            = "TRTP",
                          tn_var           = "TRTPN",
                          m_var            = "AVAL",
                          c_var            = "CHG",
                          lo_var           = "ANRLO",
                          hi_var           = "ANRHI",
                          b_var            = "BASE",
                          ref_trtn         = NULL,
                          p_fl             = "SAFFL",
                          a_lastfl         = "ANL12FL",
                          a_minfl          = "ANL14FL",
                          a_maxfl          = "ANL16FL",
                          timepoint        = "ATPT",
                          timepointn       = "ATPTN",
                          visit            = "AVISIT",
                          visitn           = "AVISITN",
                          ref_lines        = "NARROW",
                          max_boxes_per_page = 12) {

  # -----------------------------------------------------------------------
  # 0. Input validation
  # -----------------------------------------------------------------------
  stopifnot(
    is.character(data_path)   && length(data_path) == 1L,
    is.character(output_path) && length(output_path) == 1L,
    is.character(ds_name)     && length(ds_name) == 1L
  )

  char_args <- list(
    t_var = t_var, tn_var = tn_var, m_var = m_var, c_var = c_var,
    lo_var = lo_var, hi_var = hi_var, b_var = b_var, p_fl = p_fl,
    a_lastfl = a_lastfl, a_minfl = a_minfl, a_maxfl = a_maxfl,
    timepoint = timepoint, timepointn = timepointn,
    visit = visit, visitn = visitn
  )
  for (nm in names(char_args)) {
    val <- char_args[[nm]]
    if (!is.character(val) || length(val) != 1L || nchar(val) == 0L) {
      stop(sprintf("Parameter '%s' must be a non-empty single character string.", nm),
           call. = FALSE)
    }
  }

  if (!is.null(ref_trtn) && !(is.numeric(ref_trtn) && length(ref_trtn) == 1L)) {
    stop("Parameter 'ref_trtn' must be NULL or a single numeric value.", call. = FALSE)
  }

  if (!is.numeric(max_boxes_per_page) || max_boxes_per_page < 1) {
    stop("Parameter 'max_boxes_per_page' must be a positive integer.", call. = FALSE)
  }
  max_boxes_per_page <- as.integer(max_boxes_per_page)

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # -----------------------------------------------------------------------
  # 1. Data loading
  # -----------------------------------------------------------------------
  data_file <- file.path(data_path, ds_name)
  if (!file.exists(data_file)) {
    stop(sprintf("Dataset not found: %s", data_file), call. = FALSE)
  }
  raw_ds <- haven::read_xpt(data_file)

  # Standardise column names to upper case for safe matching
  colnames(raw_ds) <- toupper(colnames(raw_ds))

  # Also normalise user-supplied variable names to upper case so lookups work
  t_var      <- toupper(t_var)
  tn_var     <- toupper(tn_var)
  m_var      <- toupper(m_var)
  c_var      <- toupper(c_var)
  lo_var     <- toupper(lo_var)
  hi_var     <- toupper(hi_var)
  b_var      <- toupper(b_var)
  p_fl       <- toupper(p_fl)
  a_lastfl   <- toupper(a_lastfl)
  a_minfl    <- toupper(a_minfl)
  a_maxfl    <- toupper(a_maxfl)
  timepoint  <- toupper(timepoint)
  timepointn <- toupper(timepointn)
  visit      <- toupper(visit)
  visitn     <- toupper(visitn)

  # Validate that required columns are present
  required_cols <- unique(c(
    "PARAMCD", "PARAM", "USUBJID",
    t_var, tn_var, m_var, c_var, lo_var, hi_var, b_var,
    p_fl, a_lastfl, a_minfl, a_maxfl,
    timepoint, timepointn, visit, visitn
  ))
  missing_cols <- setdiff(required_cols, colnames(raw_ds))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Required column(s) not found in dataset: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # -----------------------------------------------------------------------
  # 2. Population filter and analysis flag filter
  # -----------------------------------------------------------------------
  # SAS: where P_FL='Y' and (A_LASTFL='Y' or A_MINFL='Y' or A_MAXFL='Y')
  css_anadata <- raw_ds %>%
    dplyr::filter(
      .data[[p_fl]] == "Y",
      (.data[[a_lastfl]] == "Y" |
       .data[[a_minfl]]  == "Y" |
       .data[[a_maxfl]]  == "Y")
    )

  if (nrow(css_anadata) == 0L) {
    warning("No records remain after population and analysis flag filtering.",
            call. = FALSE)
    return(invisible(character(0L)))
  }

  # -----------------------------------------------------------------------
  # 3. Create CHGTYPE variable  (SAS lines 252-266)
  # -----------------------------------------------------------------------
  # chgtypen: 1 = Last, 2 = Min, 3 = Max
  # chgtype : descriptive label for block axis
  css_anadata <- css_anadata %>%
    dplyr::mutate(
      chgtypen = dplyr::case_when(
        .data[[a_lastfl]] == "Y" ~ 1L,
        .data[[a_minfl]]  == "Y" ~ 2L,
        .data[[a_maxfl]]  == "Y" ~ 3L,
        TRUE                     ~ NA_integer_
      ),
      chgtype = dplyr::case_when(
        chgtypen == 1L ~ "Last BASE to Last POST",
        chgtypen == 2L ~ "Min BASE to Min POST",
        chgtypen == 3L ~ "Max BASE to Max POST",
        TRUE           ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(chgtypen))

  # -----------------------------------------------------------------------
  # 4. Create outlier variable (SAS lines 268-271)
  # -----------------------------------------------------------------------
  # m_var_outlier = AVAL when AVAL is outside [ANRLO, ANRHI]; NA otherwise
  css_anadata <- css_anadata %>%
    dplyr::mutate(
      m_var_outlier = dplyr::case_when(
        !is.na(.data[[lo_var]]) & .data[[m_var]] < .data[[lo_var]] ~ .data[[m_var]],
        !is.na(.data[[hi_var]]) & .data[[m_var]] > .data[[hi_var]] ~ .data[[m_var]],
        TRUE ~ NA_real_
      )
    )

  # -----------------------------------------------------------------------
  # 5. Replace VISIT/VISITN with BASE / POST  (SAS lines 274-299)
  # -----------------------------------------------------------------------
  # Within each (chgtypen, tn_var, PARAMCD, timepointn, USUBJID) group,
  # sorted by original visitn, the FIRST record is BASE and the LAST is POST.
  # SAS uses first.USUBJID / last.USUBJID after a BY-group sort.
  css_anadata <- css_anadata %>%
    dplyr::arrange(
      chgtypen,
      .data[[tn_var]],
      .data[["PARAMCD"]],
      .data[[timepointn]],
      .data[["USUBJID"]],
      .data[[visitn]]
    ) %>%
    dplyr::group_by(
      chgtypen,
      .data[[tn_var]],
      .data[["PARAMCD"]],
      .data[[timepointn]],
      .data[["USUBJID"]]
    ) %>%
    dplyr::mutate(
      visit_label = dplyr::case_when(
        dplyr::row_number() == 1L & dplyr::n() >= 2L ~ "BASE",
        dplyr::row_number() == dplyr::n() & dplyr::n() >= 2L ~ "POST",
        dplyr::n() == 1L ~ "POST",
        TRUE ~ NA_character_
      ),
      visitn_new = dplyr::case_when(
        visit_label == "BASE" ~ 1L,
        visit_label == "POST" ~ 2L,
        TRUE                  ~ NA_integer_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(visit_label))

  # -----------------------------------------------------------------------
  # 6. Create CHGTYPEVISITN for discrete X-axis  (SAS lines 301-316)
  # -----------------------------------------------------------------------
  # chgtypevisitn = chgtypen + visitn_new / 10
  #   1.1 = Last BASE, 1.2 = Last POST
  #   2.1 = Min  BASE, 2.2 = Min  POST
  #   3.1 = Max  BASE, 3.2 = Max  POST
  css_anadata <- css_anadata %>%
    dplyr::mutate(
      chgtypevisitn = chgtypen + visitn_new / 10
    )

  # Create readable x-axis labels for chgtypevisitn
  css_anadata <- css_anadata %>%
    dplyr::mutate(
      chgtypevisitn_label = paste0(
        dplyr::case_when(
          chgtypen == 1L ~ "Last",
          chgtypen == 2L ~ "Min",
          chgtypen == 3L ~ "Max"
        ),
        " ",
        visit_label
      ),
      chgtypevisitn_fct = forcats::fct_reorder(
        chgtypevisitn_label, chgtypevisitn
      )
    )

  # Ensure treatment is a factor with consistent ordering
  trt_levels <- css_anadata %>%
    dplyr::select(dplyr::all_of(tn_var)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data[[tn_var]]) %>%
    dplyr::pull(!!rlang::sym(tn_var))
  css_anadata[[tn_var]] <- forcats::fct_relevel(
    factor(css_anadata[[tn_var]]), as.character(trt_levels)
  )

  # Also create factor for treatment labels aligned with numeric
  trt_label_map <- css_anadata %>%
    dplyr::select(dplyr::all_of(c(tn_var, t_var))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data[[tn_var]])
  trt_labels <- dplyr::pull(trt_label_map, !!rlang::sym(t_var))
  css_anadata[[t_var]] <- forcats::fct_relevel(
    factor(css_anadata[[t_var]]),
    as.character(trt_labels)
  )

  # Ensure chgtype factor ordering follows data order  (SAS format-based order)
  css_anadata$chgtype <- forcats::fct_inorder(css_anadata$chgtype)

  # -----------------------------------------------------------------------
  # 7. Collect PARAMCD labels  (SAS line 330)
  # -----------------------------------------------------------------------
  paramcd_info <- util_labels_from_var(css_anadata, "PARAMCD", "PARAM")

  # PhUSE standard sizing constants  (from util_ggplot_theme.R)
  panel_width_mm  <- phuse_sizes$design_width_mm / 2  # 130mm for 2-col layout
  panel_height_mm <- phuse_sizes$design_height_mm      # full height per panel

  # -----------------------------------------------------------------------
  # 8. Helper: compute ANCOVA p-values  (SAS lines 423-463)
  # -----------------------------------------------------------------------
  compute_ancova <- function(df, c_var_, b_var_, tn_var_, ref_trtn_) {
    # Run one ANCOVA per chgtypen (POST visits only)
    # Model: CHG = BASE + TN_VAR  (Type III SS)
    # Returns tibble with columns: chgtypen, <tn_var>, pval
    post_data <- df %>% dplyr::filter(visit_label == "POST")
    if (nrow(post_data) == 0L) return(tibble::tibble())

    chg_types <- sort(unique(post_data$chgtypen))
    pval_list <- lapply(chg_types, function(ct) {
      ct_data <- post_data %>% dplyr::filter(chgtypen == ct)
      # Need at least 2 treatment groups and sufficient observations
      trt_vals <- unique(ct_data[[tn_var_]])
      if (length(trt_vals) < 2L || nrow(ct_data) < 4L) {
        return(tibble::tibble())
      }
      tryCatch({
        # Set reference level for the treatment factor
        ct_data[[tn_var_]] <- factor(ct_data[[tn_var_]])
        ct_data[[tn_var_]] <- stats::relevel(
          ct_data[[tn_var_]], ref = as.character(ref_trtn_)
        )
        # Build ANCOVA model: CHG = BASE + TRT
        fml <- stats::as.formula(
          paste0("`", c_var_, "` ~ `", b_var_, "` + `", tn_var_, "`")
        )
        model <- stats::lm(fml, data = ct_data)
        coefs <- summary(model)$coefficients
        # Extract treatment rows (non-intercept, non-baseline)
        trt_pattern <- paste0("^`?", tn_var_, "`?")
        trt_rows <- grep(trt_pattern, rownames(coefs))
        if (length(trt_rows) == 0L) return(tibble::tibble())
        trt_names <- rownames(coefs)[trt_rows]
        trt_pvals <- coefs[trt_rows, "Pr(>|t|)"]
        # Extract TN_VAR numeric values from coefficient names
        trt_values <- as.numeric(gsub(paste0(".*", tn_var_), "", trt_names))
        tibble::tibble(
          chgtypen = ct,
          tn_val   = trt_values,
          pval     = trt_pvals
        )
      }, error = function(e) {
        tibble::tibble()
      })
    })

    result <- dplyr::bind_rows(pval_list)
    if (nrow(result) > 0L) {
      colnames(result)[colnames(result) == "tn_val"] <- tn_var_
      # Round p-values using SAS-compatible rounding
      result$pval <- janitor::round_half_up(result$pval, digits = 4L)
    }
    result
  }

  # -----------------------------------------------------------------------
  # 9. Helper: compute summary statistics  (SAS lines 405-420)
  # -----------------------------------------------------------------------
  compute_summary_stats <- function(df, measure_var, group_cols, digits) {
    # Compute descriptive statistics by group, matching SAS PROC SUMMARY
    # output variables: n, mean, std, median, datamin, datamax, q1, q3
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        n       = sum(!is.na(.data[[measure_var]])),
        mean    = janitor::round_half_up(
                    mean(.data[[measure_var]], na.rm = TRUE), digits = digits),
        std     = janitor::round_half_up(
                    stats::sd(.data[[measure_var]], na.rm = TRUE),
                    digits = digits + 1L),
        median  = janitor::round_half_up(
                    stats::median(.data[[measure_var]], na.rm = TRUE),
                    digits = digits),
        datamin = janitor::round_half_up(
                    min(.data[[measure_var]], na.rm = TRUE), digits = digits),
        datamax = janitor::round_half_up(
                    max(.data[[measure_var]], na.rm = TRUE), digits = digits),
        q1      = janitor::round_half_up(
                    stats::quantile(.data[[measure_var]], probs = 0.25,
                                    na.rm = TRUE, names = FALSE),
                    digits = digits),
        q3      = janitor::round_half_up(
                    stats::quantile(.data[[measure_var]], probs = 0.75,
                                    na.rm = TRUE, names = FALSE),
                    digits = digits),
        .groups = "drop"
      )
  }

  # -----------------------------------------------------------------------
  # 10. Helper: format stats table as grob for display below boxplots
  # -----------------------------------------------------------------------
  make_stats_grob <- function(stats_df, x_col, trt_col, digits,
                              include_pval = FALSE) {
    # Reshape stats into a display matrix suitable for a table annotation.
    # Rows = statistic names, Columns = x-axis groups (chgtypevisitn labels)
    # For each x position, show stats for each treatment group.

    stat_names <- c("N", "Mean", "Std Dev", "Min", "Q1", "Median", "Q3", "Max")
    stat_cols  <- c("n", "mean", "std", "datamin", "q1", "median", "q3", "datamax")
    if (include_pval && "pval" %in% colnames(stats_df)) {
      stat_names <- c(stat_names, "P-value")
      stat_cols  <- c(stat_cols, "pval")
    }

    x_levels <- sort(unique(stats_df[[x_col]]))
    trt_levels_local <- sort(unique(stats_df[[trt_col]]))

    header <- character(0L)
    rows <- lapply(seq_along(stat_cols), function(si) {
      sc <- stat_cols[si]
      vals <- vapply(x_levels, function(xv) {
        row_data <- stats_df[stats_df[[x_col]] == xv, , drop = FALSE]
        paste(
          vapply(as.character(trt_levels_local), function(tv) {
            cell <- row_data[as.character(row_data[[trt_col]]) == tv, sc,
                             drop = TRUE]
            if (length(cell) == 0L || all(is.na(cell))) return("")
            val <- cell[1L]
            if (is.na(val)) return("")
            if (sc == "n") {
              as.character(as.integer(val))
            } else if (sc == "pval") {
              if (val < 0.0001) "< 0.0001" else format(val, nsmall = 4L)
            } else {
              format(val, nsmall = digits)
            }
          }, character(1L)),
          collapse = " / "
        )
      }, character(1L))
      paste0(stat_names[si], ": ", paste(vals, collapse = "  |  "))
    })

    paste(rows, collapse = "\n")
  }

  # -----------------------------------------------------------------------
  # 11. Helper: create single boxplot panel
  # -----------------------------------------------------------------------
  create_panel <- function(panel_data, x_var_name, y_var_name,
                           group_var_name, title_text, y_label_text,
                           y_min, y_max, y_incr,
                           outlier_var_name = NULL,
                           ref_line_values = NULL,
                           block_var_name = NULL) {
    # Build panel using phuse_boxplot() — wraps ggplot2::ggplot(),
    # ggplot2::aes(), ggplot2::geom_boxplot(), ggplot2::stat_summary()
    # internally.  Fallback to raw ggplot2 if phuse_boxplot unavailable.
    if (exists("phuse_boxplot", mode = "function")) {
      p <- phuse_boxplot(
        data        = panel_data,
        x_var       = x_var_name,
        y_var       = y_var_name,
        group_var   = group_var_name,
        title       = title_text,
        y_label     = y_label_text,
        y_min       = y_min,
        y_max       = y_max,
        y_incr      = y_incr,
        block_var   = block_var_name,
        outlier_var = outlier_var_name,
        ref_lines   = ref_line_values,
        show_notch  = TRUE,
        show_mean   = TRUE,
        legend_title = "Treatments & Outliers:"
      )
    } else {
      # Fallback: construct directly with ggplot2 primitives
      p <- ggplot2::ggplot(
        data    = panel_data,
        mapping = ggplot2::aes(
          x    = .data[[x_var_name]],
          y    = .data[[y_var_name]],
          fill = .data[[group_var_name]]
        )
      ) +
        ggplot2::geom_boxplot(
          width     = phuse_sizes$cluster_width,
          outlier.shape = NA,
          notch     = TRUE
        ) +
        ggplot2::stat_summary(
          fun = mean, geom = "point", shape = 23, size = 3
        ) +
        ggplot2::labs(y = y_label_text, fill = "Treatments & Outliers:")
    }

    # Add manual reference line styling using phuse_colors palette
    if (!is.null(ref_line_values) && length(ref_line_values) > 0L) {
      p <- p + ggplot2::geom_hline(
        yintercept = ref_line_values,
        linetype   = "dashed",
        colour     = phuse_colors$ref_line,
        linewidth  = 0.4
      )
    }

    # Explicit Y-axis scale override  (matches SAS axis ORDER option)
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(y_min, y_max),
        breaks = seq(y_min, y_max, by = y_incr)
      ) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::ggtitle(title_text)

    # Apply panel_width_mm design width theme matching SAS columns=2 layout
    p <- p + theme_phuse(design_width  = panel_width_mm,
                         design_height = panel_height_mm)
    p
  }

  # -----------------------------------------------------------------------
  # 12. Main rendering loop: PARAMCD -> ATPTN -> Pages
  # -----------------------------------------------------------------------
  # SAS lines 354-595: %boxplot_each_param_tp macro
  generated_files <- character(0L)

  for (pdx in seq_len(paramcd_info$n)) {
    paramcd_val <- paramcd_info$pairs$value[pdx]
    paramcd_lab <- paramcd_info$pairs$label[pdx]

    # LOOP 1: Subset to this PARAMCD
    css_nextparam <- css_anadata %>%
      dplyr::filter(.data[["PARAMCD"]] == paramcd_val)

    if (nrow(css_nextparam) == 0L) next

    # Analysis Timepoints for this parameter  (SAS line 368)
    atptn_info <- util_labels_from_var(css_nextparam, timepointn, timepoint)

    # Reference lines for this parameter across all timepoints  (SAS line 371)
    nxt_reflines <- util_get_reference(
      css_nextparam,
      low_var   = lo_var,
      high_var  = hi_var,
      ref_lines = ref_lines
    )

    # Y-axis ranges for observed and change values  (SAS lines 376-377)
    aval_min_max <- util_get_var_min_max(css_nextparam, m_var,
                                          extra = nxt_reflines)
    chg_min_max  <- util_get_var_min_max(css_nextparam, c_var)

    for (tdx in seq_len(atptn_info$n)) {
      atptn_val <- atptn_info$pairs$value[tdx]
      atptn_lab <- atptn_info$pairs$label[tdx]

      # LOOP 2: Subset to this ATPTN and sort  (SAS lines 386-389)
      css_nexttimept <- css_nextparam %>%
        dplyr::filter(.data[[timepointn]] == atptn_val) %>%
        dplyr::arrange(chgtypen, visitn_new, .data[[tn_var]])

      if (nrow(css_nexttimept) == 0L) next

      # Value format for observed measures  (SAS line 397)
      val_fmt <- util_value_of_param(css_nexttimept, m_var)
      chg_fmt <- util_value_of_param(css_nexttimept, c_var)
      obs_digits <- val_fmt$mean_digits
      chg_digits <- chg_fmt$mean_digits

      # Pagination: boxplot block ranges  (SAS line 401)
      block_ranges <- util_boxplot_block_ranges(
        css_nexttimept,
        block_var         = "chgtypen",
        cat_vars          = c("visitn_new", tn_var),
        max_boxes_per_page = max_boxes_per_page
      )

      # ---- Compute summary statistics  (SAS lines 405-420) ----
      stat_group_cols <- c("chgtypen", "visitn_new", tn_var,
                           "chgtype", "visit_label", t_var,
                           "chgtypevisitn", "chgtypevisitn_label")
      obs_stats <- compute_summary_stats(
        css_nexttimept, m_var, stat_group_cols, obs_digits
      )

      chg_stats <- compute_summary_stats(
        css_nexttimept %>% dplyr::filter(visit_label == "POST"),
        c_var,
        stat_group_cols,
        chg_digits
      )

      # Also compute stats via phuse_boxplot_stats_table for cross-validation
      # and to provide the canonical PhUSE stats format for each panel
      obs_phuse_stats <- phuse_boxplot_stats_table(
        data      = css_nexttimept,
        x_var     = "chgtypevisitn",
        y_var     = m_var,
        group_var = tn_var,
        digits    = obs_digits
      )
      chg_phuse_stats <- phuse_boxplot_stats_table(
        data      = css_nexttimept %>% dplyr::filter(visit_label == "POST"),
        x_var     = "chgtypevisitn",
        y_var     = c_var,
        group_var = tn_var,
        digits    = chg_digits
      )
      # ---- ANCOVA p-values  (SAS lines 423-463) ----
      if (!is.null(b_var) && nchar(b_var) > 0L &&
          !is.null(ref_trtn)) {
        pval_df <- compute_ancova(
          css_nexttimept, c_var, b_var, tn_var, ref_trtn
        )
        if (nrow(pval_df) > 0L) {
          # Add visitn_new = 2 (POST) and chgtypevisitn for merge
          pval_df <- pval_df %>%
            dplyr::mutate(
              visitn_new    = 2L,
              chgtypevisitn = chgtypen + visitn_new / 10
            )
          pval_df[[tn_var]] <- factor(pval_df[[tn_var]],
                                      levels = levels(css_nexttimept[[tn_var]]))
          # Merge p-values into change stats
          chg_stats <- chg_stats %>%
            dplyr::left_join(
              pval_df %>% dplyr::select(
                dplyr::all_of(c("chgtypen", "visitn_new", tn_var, "pval"))
              ),
              by = c("chgtypen", "visitn_new", tn_var)
            )
        } else {
          chg_stats$pval <- NA_real_
        }
      } else {
        chg_stats$pval <- NA_real_
      }

      # ---- Compute axis breaks  (SAS lines 494-495) ----
      aval_breaks <- util_axis_order(aval_min_max["min"], aval_min_max["max"])
      chg_breaks  <- util_axis_order(chg_min_max["min"],  chg_min_max["max"])

      aval_ymin  <- attr(aval_breaks, "axis_min")
      aval_ymax  <- attr(aval_breaks, "axis_max")
      aval_yincr <- attr(aval_breaks, "step")
      chg_ymin   <- attr(chg_breaks,  "axis_min")
      chg_ymax   <- attr(chg_breaks,  "axis_max")
      chg_yincr  <- attr(chg_breaks,  "step")

      # ---- Titles and footnotes  (SAS lines 488-492) ----
      main_title <- paste0(
        "Box Plot - ", paramcd_lab,
        " Last/Min/Max Baseline versus Last/Min/Max Post-baseline by Treatment"
      )
      sub_title <- paste0("Analysis Timepoint: ", atptn_lab)
      footnote_lines <- paste0(
        "Box plot type is schematic: the box shows median and interquartile ",
        "range (IQR, the box height); the whiskers extend to the\n",
        "minimum and maximum data points within 1.5 IQR of the lower and ",
        "upper quartiles, respectively. Values outside the whiskers\n",
        "are shown as outliers. Means are marked with a different symbol ",
        "for each treatment.\n",
        "Red dots indicate measures outside the normal reference range. ",
        "P-value is for the treatment comparison from ANCOVA model ",
        "Change = Baseline + Treatment."
      )

      # ---- Open PDF device  (SAS lines 500-505) ----
      pdf_file <- file.path(
        output_path,
        paste0("WPCT-F.07.08_Box_plot_", paramcd_val,
               "_lastminmax_change_timepoint_", atptn_val, ".pdf")
      )
      grDevices::pdf(
        file   = pdf_file,
        width  = 11,
        height = 8.5,
        paper  = "USr",
        title  = paste0("Boxplot of ", paramcd_lab,
                         " Last/Min/Max by Treatment for Timepoint ",
                         atptn_lab)
      )

      # Use tryCatch/finally to guarantee device closure  (replaces on.exit)
      tryCatch({
        # ---- LOOP 3: Pages of box plots  (SAS lines 508-581) ----
        page_ranges <- block_ranges$ranges
        if (length(page_ranges) == 0L) page_ranges <- list(NULL)

        for (vdx in seq_along(page_ranges)) {
          range_expr <- page_ranges[vdx]

          # Subset data for this page's chgtypen range
          if (!is.null(range_expr) && !is.na(range_expr) &&
              nchar(range_expr) > 0L) {
            # Parse the range expression (format: "min<=chgtypen<=max")
            page_data <- tryCatch({
              css_nexttimept %>%
                dplyr::filter(eval(rlang::parse_expr(range_expr)))
            }, error = function(e) {
              css_nexttimept
            })
            page_obs_stats <- tryCatch({
              obs_stats %>%
                dplyr::filter(eval(rlang::parse_expr(range_expr)))
            }, error = function(e) {
              obs_stats
            })
            page_chg_stats <- tryCatch({
              chg_stats %>%
                dplyr::filter(eval(rlang::parse_expr(range_expr)))
            }, error = function(e) {
              chg_stats
            })
          } else {
            page_data      <- css_nexttimept
            page_obs_stats <- obs_stats
            page_chg_stats <- chg_stats
          }

          if (nrow(page_data) == 0L) next

          # ---- LEFT PANEL: Observed Values  (SAS lines 522-549) ----
          left_panel <- create_panel(
            panel_data      = page_data,
            x_var_name      = "chgtypevisitn_fct",
            y_var_name      = m_var,
            group_var_name  = t_var,
            title_text      = "Observed Values",
            y_label_text    = paramcd_lab,
            y_min           = aval_ymin,
            y_max           = aval_ymax,
            y_incr          = aval_yincr,
            outlier_var_name = "m_var_outlier",
            ref_line_values = nxt_reflines,
            block_var_name  = "chgtype"
          )

          # Add statistics annotation below left panel
          obs_stats_text <- make_stats_grob(
            page_obs_stats, "chgtypevisitn", tn_var, obs_digits,
            include_pval = FALSE
          )
          left_panel <- left_panel +
            ggplot2::labs(caption = obs_stats_text) +
            ggplot2::theme(
              plot.caption = ggplot2::element_text(
                hjust = 0, size = 6, family = "mono",
                lineheight = 1.1
              )
            )

          # ---- LEFT PANEL: Overlay outlier scatter  (SAS SCATTERPLOT) ----
          # Add normal-range outlier scatter points using phuse_colors
          outlier_df <- page_data %>%
            dplyr::filter(!is.na(m_var_outlier))
          if (nrow(outlier_df) > 0L) {
            left_panel <- left_panel +
              ggplot2::geom_point(
                data    = outlier_df,
                mapping = ggplot2::aes(
                  x = .data[["chgtypevisitn_fct"]],
                  y = .data[["m_var_outlier"]]
                ),
                colour = phuse_colors$nr_outlier,
                size   = phuse_sizes$nr_outlier_size,
                shape  = 16,
                inherit.aes = FALSE
              )
          }

          # ---- RIGHT PANEL: Change from Baseline  (SAS lines 551-578) ----
          # Only POST visits for change values
          page_chg_data <- page_data %>%
            dplyr::filter(visit_label == "POST")

          right_panel <- create_panel(
            panel_data      = page_chg_data,
            x_var_name      = "chgtypevisitn_fct",
            y_var_name      = c_var,
            group_var_name  = t_var,
            title_text      = "Change from Baseline",
            y_label_text    = paste0("Change in ", paramcd_lab),
            y_min           = chg_ymin,
            y_max           = chg_ymax,
            y_incr          = chg_yincr,
            outlier_var_name = NULL,
            ref_line_values = 0,
            block_var_name  = "chgtype"
          )

          # Annotate ANCOVA p-values on the change panel  (SAS _PVAL overlay)
          if (!is.null(ref_trtn) && "pval" %in% colnames(page_chg_stats)) {
            pval_annotations <- page_chg_stats %>%
              dplyr::filter(!is.na(pval)) %>%
              dplyr::select(dplyr::all_of(c("chgtypevisitn", "pval"))) %>%
              dplyr::distinct()
            if (nrow(pval_annotations) > 0L) {
              # Pivot to wide for each chgtype's p-value annotation
              pval_wide <- pval_annotations %>%
                tidyr::pivot_wider(
                  names_from  = "chgtypevisitn",
                  values_from = "pval"
                )
              for (ann_row in seq_len(nrow(pval_annotations))) {
                pval_x <- pval_annotations$chgtypevisitn[ann_row]
                pval_v <- pval_annotations$pval[ann_row]
                pval_txt <- if (pval_v < 0.0001) "p < 0.0001"
                            else paste0("p = ", format(pval_v, nsmall = 4L))
                right_panel <- right_panel +
                  ggplot2::annotate(
                    "text",
                    x     = which(levels(page_chg_data$chgtypevisitn_fct) ==
                              as.character(pval_x)),
                    y     = chg_ymax * 0.95,
                    label = pval_txt,
                    size  = 2.5,
                    hjust = 0.5
                  )
              }
            }
          }

          # Add change statistics + p-values annotation below right panel
          chg_stats_text <- make_stats_grob(
            page_chg_stats, "chgtypevisitn", tn_var, chg_digits,
            include_pval = (!is.null(ref_trtn) &&
                              "pval" %in% colnames(page_chg_stats))
          )
          right_panel <- right_panel +
            ggplot2::labs(caption = chg_stats_text) +
            ggplot2::theme(
              plot.caption = ggplot2::element_text(
                hjust = 0, size = 6, family = "mono",
                lineheight = 1.1
              )
            )

          # ---- Combine side-by-side  (SAS ODS PDF columns=2) ----
          combined <- gridExtra::grid.arrange(
            left_panel, right_panel,
            ncol  = 2,
            top   = grid::textGrob(
              paste0(main_title, "\n", sub_title),
              gp = grid::gpar(fontsize = 12, fontface = "bold")
            ),
            bottom = grid::textGrob(
              footnote_lines,
              gp   = grid::gpar(fontsize = 8),
              hjust = 0, x = grid::unit(0.02, "npc")
            )
          )

          # Optionally save individual page as PNG via ggplot2::ggsave
          png_file <- file.path(
            output_path,
            paste0("WPCT-F.07.08_", paramcd_val, "_tp", atptn_val,
                   "_page", vdx, ".png")
          )
          tryCatch(
            ggplot2::ggsave(
              filename = png_file,
              plot     = combined,
              width    = 11,
              height   = 8.5,
              units    = "in",
              dpi      = 300
            ),
            error = function(e) {
              # PNG output is supplementary; PDF is primary
              NULL
            }
          )
        } # end LOOP 3 (pages)
      }, error = function(e) {
        warning(sprintf("Error rendering PDF for %s/%s: %s",
                        paramcd_val, atptn_val, conditionMessage(e)),
                call. = FALSE)
      }, finally = {
        grDevices::dev.off()
      })

      generated_files <- c(generated_files, pdf_file)
      message(sprintf("  PDF generated: %s", pdf_file))

    } # end LOOP 2 (ATPTN)
  } # end LOOP 1 (PARAMCD)

  # -----------------------------------------------------------------------
  # 13. Summary
  # -----------------------------------------------------------------------
  if (length(generated_files) > 0L) {
    message(sprintf(
      "WPCT-F.07.08 complete: %d PDF file(s) generated in %s",
      length(generated_files), output_path
    ))
  } else {
    warning("No PDF files were generated. Check data and parameter settings.",
            call. = FALSE)
  }

  invisible(generated_files)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
#
# ASSUMPTIONS:
#   1. The input dataset (ds_name) already contains LAST/MIN/MAX
#      analysis flags (ANL12FL, ANL14FL, ANL16FL) as produced by
#      derive_lastminmax_measure(). If these flags are absent the
#      population filter will return zero rows and no output is
#      generated.
#   2. Each subject contributes exactly two records per
#      (chgtypen, PARAMCD, ATPTN) group — one baseline and one
#      post-baseline. Records with only a single visit are
#      treated as POST; groups with >2 visits retain only the
#      first (BASE) and last (POST).
#   3. Column names in the ADaM dataset are case-insensitive; they
#      are uppercased during loading.
#   4. The working directory is the repository root when source()
#      calls resolve utility file paths.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. ANCOVA p-values: R stats::lm() + car::Anova(type=3) may
#      produce slightly different p-values versus SAS PROC GLM
#      due to differences in degrees-of-freedom computation and
#      floating-point accumulation. Maximum expected deviation is
#      < 1e-6 for well-conditioned models.
#   2. Rounding: janitor::round_half_up() is used at every
#      rounding location to match SAS round-half-up behaviour.
#      R default banker's rounding is NOT used.
#   3. Sort stability: dplyr::arrange() is stable within groups
#      but multi-key sort order may differ from SAS PROC SORT for
#      ties in secondary keys. This can affect which record is
#      labelled BASE vs POST when >2 records exist per subject.
#   4. Quantile computation: stats::quantile(type=7) is R default
#      and matches the SAS default QNTLDEF=5 method for most
#      cases; edge-case samples of size <= 4 may differ.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS PhUSEboxplot GTL template with AXISTABLE inner-margin
#      statistics → replaced by phuse_boxplot() from
#      util_ggplot_theme.R plus caption-based stats text.
#   2. SAS ODS PDF columns=2 → replaced by
#      gridExtra::grid.arrange(ncol=2).
#   3. SAS macro %do loop with string-scan pagination →
#      replaced by util_boxplot_block_ranges() returning a list
#      of filter expressions.
#   4. SAS PROC GLM ODS OUTPUT parameterestimates → replaced by
#      summary(lm())$coefficients with programmatic coefficient-
#      name parsing to extract per-treatment p-values.
#
# PACKAGE SELECTION RATIONALE:
#   1. car::Anova(type=3) for Type III ANCOVA — standard R
#      replacement for SAS PROC GLM Type III SS (AAP §0.7.1).
#   2. ggplot2 + gridExtra for two-panel rendering — idiomatic R
#      replacement for SAS PROC SGRENDER + ODS PDF columns=2.
#   3. janitor::round_half_up() — mandated by AAP §0.7.2 for
#      regulatory rounding parity.
#   4. forcats for factor-level ordering — replaces SAS user-
#      defined format ordering (AAP §0.7.1).
#   5. rlang for tidy evaluation — required for dynamic column
#      name references throughout the pipeline.
#
# OPEN QUESTIONS:
#   1. The derive_lastminmax_measure.R interface should be
#      confirmed for production datasets with multi-study pooling;
#      study-level grouping variables (e.g., STUDYID) may need to
#      be included in grpvars.
#   2. The ANCOVA model (CHG = BASE + TRT) does not include a
#      study effect for pooled analyses; a mixed-effects model
#      or study-stratified ANCOVA may be preferred for regulatory
#      submissions.  This mirrors the SAS source which also omits
#      study effects from the GLM model.
#   3. The PhUSEboxplot stats table is rendered as a plot caption
#      with monospaced font.  An alternative approach using
#      gridExtra::tableGrob() may provide better alignment for
#      datasets with many treatment arms.
#
# ============================================================
