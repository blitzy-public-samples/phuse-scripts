# ==============================================================================
# HEADER
# Display:     Figure 7.4 Box plot — Variant with Normal Range Reference Lines
# White paper: Central Tendency
# Specification: WPCT White Paper Figure 7.4
# NOTE:        No corresponding WPCT-F.07.04.sas source exists in the repository.
#              The SAS numbering jumps from 07.03 to 07.06. No WPCT_Fig_7.4
#              specification document was found.
#              This R script was created based on the WPCT boxplot patterns
#              established by WPCT-F.07.01.sas / WPCT-F.07.03.sas and the
#              AAP directive: "Boxplot variant -> ggplot2::geom_boxplot with
#              reference lines" (AAP section 0.5.1).
# Migrated from: N/A (no WPCT-F.07.04.sas exists)
# ==============================================================================

# --- Library imports ----------------------------------------------------------
# Core tidyverse (AAP section 0.8.1: tidyverse over base R)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(janitor)
library(forcats)
library(yaml)
library(rlang)

# ==============================================================================
# resolve_column — case-insensitive column name resolver
# ==============================================================================
# CDISC ADaM datasets loaded from XPT transport files typically use uppercase
# variable names (TRTP, AVAL, AVISITN). This helper resolves user-supplied
# variable names to their actual column-name casing in the data frame, providing
# robustness against mixed-case datasets.
# ==============================================================================
resolve_column <- function(df, var_name) {
  if (var_name %in% names(df)) return(var_name)
  upper_idx <- match(toupper(var_name), toupper(names(df)))
  if (!is.na(upper_idx)) return(names(df)[upper_idx])
  return(NULL)
}

# ==============================================================================
# wpct_f_07_04 — Figure 7.4 Boxplot Variant with Reference Lines
# ==============================================================================
#' Produce WPCT Figure 7.4 — Boxplot with Normal Range Reference Lines
#'
#' Generates paginated boxplot figures showing observed measurement values by
#' treatment arm and analysis visit, with horizontal reference lines for the
#' normal range (ANRLO / ANRHI). This is the distinguishing feature of Figure
#' 7.4 compared to Figure 7.1 (which uses UNIFORM reference lines by default).
#'
#' @param data_path   Character. Path to directory containing XPT datasets.
#'                    Falls back to \code{config$data_paths$adam_path}.
#' @param output_path Character. Path to directory for PDF output.
#'                    Falls back to \code{config$output_paths$figure_output_path}.
#' @param ds_name     Character. XPT dataset filename (default \code{"advs.xpt"}).
#' @param t_var       Character. Treatment variable name (default \code{"TRTP"}).
#' @param tn_var      Character. Treatment numeric ordering variable
#'                    (default \code{"TRTPN"}).
#' @param m_var       Character. Measurement / analysis value variable
#'                    (default \code{"AVAL"}).
#' @param lo_var      Character. Lower normal range limit variable
#'                    (default \code{"ANRLO"}).
#' @param hi_var      Character. Upper normal range limit variable
#'                    (default \code{"ANRHI"}).
#' @param p_fl        Character. Population flag variable
#'                    (default \code{"SAFFL"}; 'Y' = in-population).
#' @param a_fl        Character. Analysis flag variable
#'                    (default \code{"ANL01FL"}; 'Y' = included in analysis).
#' @param ref_lines   Character or numeric vector. Reference line mode:
#'                    \code{"NONE"}, \code{"UNIFORM"}, \code{"NARROW"},
#'                    \code{"ALL"}, or a numeric vector of explicit y-positions.
#'                    Default \code{"NARROW"} — the distinguishing feature of
#'                    Figure 7.4.
#' @param max_boxes_per_page Integer. Maximum number of box-and-whisker groups
#'                    per output page for pagination (default \code{20}).
#'
#' @return Invisible character vector of generated PDF file paths.
#'
#' @examples
#' \dontrun{
#' wpct_f_07_04(
#'   data_path  = "data/adam/cdisc",
#'   output_path = "output/figures",
#'   ds_name    = "advs.xpt",
#'   ref_lines  = "NARROW"
#' )
#' }
#'
#' @export
wpct_f_07_04 <- function(data_path         = NULL,
                          output_path       = NULL,
                          ds_name           = "advs.xpt",
                          t_var             = "TRTP",
                          tn_var            = "TRTPN",
                          m_var             = "AVAL",
                          lo_var            = "ANRLO",
                          hi_var            = "ANRHI",
                          p_fl              = "SAFFL",
                          a_fl              = "ANL01FL",
                          ref_lines         = "NARROW",
                          max_boxes_per_page = 20) {

  # ============================================================================
  # 1. CONFIGURATION LOADING
  # Replaces SAS %let global macro variables and libname statements.
  # All paths are parameterised via config/migration_config.yaml
  # (AAP section 0.8.1: no hardcoded paths).
  # ============================================================================
  config <- tryCatch(
    yaml::read_yaml("config/migration_config.yaml"),
    error = function(e) {
      tryCatch(
        yaml::read_yaml(file.path(
          dirname(dirname(dirname(
            if (interactive()) {
              getwd()
            } else {
              "."
            }
          ))),
          "config", "migration_config.yaml"
        )),
        error = function(e2) list()
      )
    }
  )

  # Resolve paths from config when arguments are NULL
  if (is.null(data_path)) {
    data_path <- config$data_paths$adam_path
    if (is.null(data_path)) data_path <- "data/adam/cdisc"
  }
  if (is.null(output_path)) {
    output_path <- config$output_paths$figure_output_path
    if (is.null(output_path)) output_path <- "output/figures"
  }

  # Resolve utility source path from config
  wp_utils_path <- config$r_source_paths$wp_utilities_path
  if (is.null(wp_utils_path)) wp_utils_path <- "whitepapers/utilities/R"

  # Read WPCT-specific domain settings from config for documentation
  wpct_settings <- config$domain_settings$wpct

  # ============================================================================
  # 2. SOURCE UTILITY FUNCTIONS
  # Replaces SAS %include statements for macro libraries.
  # Each utility is sourced into the local environment to avoid polluting the
  # global namespace.
  # ============================================================================
  source(file.path(wp_utils_path, "util_ggplot_theme.R"), local = TRUE)
  source(file.path(wp_utils_path, "util_boxplot_block_ranges.R"), local = TRUE)
  source(file.path(wp_utils_path, "util_axis_order.R"), local = TRUE)
  source(file.path(wp_utils_path, "util_get_reference.R"), local = TRUE)
  source(file.path(wp_utils_path, "util_get_var_min_max.R"), local = TRUE)

  # ============================================================================
  # 3. INPUT VALIDATION
  # ============================================================================
  if (!is.character(ds_name) || length(ds_name) != 1L) {
    stop("`ds_name` must be a single character string.", call. = FALSE)
  }
  if (!is.character(t_var) || length(t_var) != 1L) {
    stop("`t_var` must be a single character string.", call. = FALSE)
  }
  if (!is.character(tn_var) || length(tn_var) != 1L) {
    stop("`tn_var` must be a single character string.", call. = FALSE)
  }
  if (!is.character(m_var) || length(m_var) != 1L) {
    stop("`m_var` must be a single character string.", call. = FALSE)
  }
  if (!is.character(lo_var) || length(lo_var) != 1L) {
    stop("`lo_var` must be a single character string.", call. = FALSE)
  }
  if (!is.character(hi_var) || length(hi_var) != 1L) {
    stop("`hi_var` must be a single character string.", call. = FALSE)
  }
  if (!is.character(p_fl) || length(p_fl) != 1L) {
    stop("`p_fl` must be a single character string.", call. = FALSE)
  }
  if (!is.character(a_fl) || length(a_fl) != 1L) {
    stop("`a_fl` must be a single character string.", call. = FALSE)
  }
  if (!is.numeric(max_boxes_per_page) || max_boxes_per_page < 1) {
    stop("`max_boxes_per_page` must be a positive integer.", call. = FALSE)
  }
  max_boxes_per_page <- as.integer(max_boxes_per_page)

  # ============================================================================
  # 4. DATA LOADING
  # Replaces SAS: libname adam "&data_path" access=readonly;
  #               data css_anadata; set adam.&ds; ...
  # Uses haven::read_xpt() for SAS transport file I/O (AAP section 0.6.1).
  # ============================================================================
  xpt_path <- file.path(data_path, ds_name)
  if (!file.exists(xpt_path)) {
    stop("Data file not found: ", xpt_path,
         "\nVerify data_path and ds_name arguments.", call. = FALSE)
  }
  data_raw <- haven::read_xpt(xpt_path)

  # ============================================================================
  # 5. COLUMN RESOLUTION AND VALIDATION
  # Resolves user-supplied variable names to actual data column names
  # (case-insensitive) and validates presence of required structural columns.
  # ============================================================================
  required_user_vars <- c(t_var  = t_var,  tn_var = tn_var,
                          m_var  = m_var,  p_fl   = p_fl, a_fl = a_fl)

  structural_vars <- c(PARAMCD = "PARAMCD", PARAM = "PARAM",
                       AVISITN = "AVISITN", AVISIT = "AVISIT")

  all_check_vars <- c(required_user_vars, structural_vars)

  resolved <- vapply(all_check_vars, function(v) {
    rc <- resolve_column(data_raw, v)
    if (is.null(rc)) NA_character_ else rc
  }, character(1))

  missing_vars <- names(resolved)[is.na(resolved)]
  if (length(missing_vars) > 0L) {
    stop("Required column(s) not found in dataset: ",
         paste(all_check_vars[missing_vars], collapse = ", "),
         "\nAvailable columns: ",
         paste(names(data_raw), collapse = ", "),
         call. = FALSE)
  }

  # Resolved column names
  t_col      <- resolved[["t_var"]]
  tn_col     <- resolved[["tn_var"]]
  m_col      <- resolved[["m_var"]]
  p_col      <- resolved[["p_fl"]]
  a_col      <- resolved[["a_fl"]]
  paramcd_col <- resolved[["PARAMCD"]]
  param_col  <- resolved[["PARAM"]]
  visitn_col <- resolved[["AVISITN"]]
  visit_col  <- resolved[["AVISIT"]]

  # Resolve optional reference range columns
  lo_col <- resolve_column(data_raw, lo_var)
  hi_col <- resolve_column(data_raw, hi_var)
  has_lo <- !is.null(lo_col) && any(!is.na(data_raw[[lo_col]]))
  has_hi <- !is.null(hi_col) && any(!is.na(data_raw[[hi_col]]))

  # Resolve optional ATPTN / ATPT columns
  atptn_col <- resolve_column(data_raw, "ATPTN")
  atpt_col  <- resolve_column(data_raw, "ATPT")
  has_atptn <- !is.null(atptn_col)

  # ============================================================================
  # 6. POPULATION AND ANALYSIS FLAG FILTERING
  # Replaces SAS: where upcase(&p_fl) = 'Y' and upcase(&a_fl) = 'Y';
  # toupper() mirrors SAS upcase() for case-insensitive flag matching.
  # ============================================================================
  data_filt <- data_raw %>%
    dplyr::filter(
      toupper(as.character(.data[[p_col]])) == "Y",
      toupper(as.character(.data[[a_col]])) == "Y"
    )

  if (nrow(data_filt) == 0L) {
    warning("No observations after applying population flag (",
            p_fl, " = 'Y') and analysis flag (", a_fl, " = 'Y').",
            call. = FALSE)
    return(invisible(character(0)))
  }

  # Drop rows with missing measurement variable (AAP section 0.7.3: NA, never 0)
  data_filt <- data_filt %>%
    tidyr::drop_na(dplyr::all_of(m_col))

  if (nrow(data_filt) == 0L) {
    warning("No non-missing values in measurement variable '", m_var, "'.",
            call. = FALSE)
    return(invisible(character(0)))
  }

  # ============================================================================
  # 7. OUTLIER VARIABLE DERIVATION
  # Replaces SAS:
  #   &m_var._outlier = .;
  #   if &m_var < &lo_var or &m_var > &hi_var then &m_var._outlier = &m_var;
  # Values outside the normal range are flagged for red dot overlay on the

  # boxplot via geom_point() in phuse_boxplot().
  # Missing . maps to NA (AAP section 0.7.3).
  # ============================================================================
  outlier_var_name <- paste0(m_col, "_outlier")

  if (has_lo && has_hi) {
    data_filt <- data_filt %>%
      dplyr::mutate(
        !!outlier_var_name := dplyr::case_when(
          .data[[m_col]] < .data[[lo_col]] |
            .data[[m_col]] > .data[[hi_col]] ~ .data[[m_col]],
          TRUE ~ NA_real_
        )
      )
  } else if (has_lo) {
    data_filt <- data_filt %>%
      dplyr::mutate(
        !!outlier_var_name := dplyr::case_when(
          .data[[m_col]] < .data[[lo_col]] ~ .data[[m_col]],
          TRUE ~ NA_real_
        )
      )
  } else if (has_hi) {
    data_filt <- data_filt %>%
      dplyr::mutate(
        !!outlier_var_name := dplyr::case_when(
          .data[[m_col]] > .data[[hi_col]] ~ .data[[m_col]],
          TRUE ~ NA_real_
        )
      )
  } else {
    data_filt[[outlier_var_name]] <- NA_real_
  }

  # ============================================================================
  # 8. TREATMENT AND VISIT ORDERING
  # Replaces SAS FORMAT catalog-based ordering and PROC SORT BY statements.
  # Treatment arms ordered by TRTPN numeric variable (forcats::fct_reorder).
  # Placebo/control group moved first when detected (forcats::fct_relevel).
  # Visits ordered by AVISITN (forcats::fct_inorder after arrange).
  # (AAP section 0.7.1: factor ordering preservation requirement)
  # ============================================================================

  # Order treatments by numeric code
  data_filt <- data_filt %>%
    dplyr::mutate(
      !!t_col := forcats::fct_reorder(
        as.character(.data[[t_col]]),
        .data[[tn_col]],
        .fun = min, na.rm = TRUE
      )
    )

  # Move placebo/control group first when present
  trt_levels <- levels(data_filt[[t_col]])
  placebo_idx <- grep("placebo|control|pbo", trt_levels, ignore.case = TRUE)
  if (length(placebo_idx) > 0L) {
    data_filt <- data_filt %>%
      dplyr::mutate(
        !!t_col := forcats::fct_relevel(.data[[t_col]], trt_levels[placebo_idx])
      )
  }

  # Order visits by numeric visit code
  data_filt <- data_filt %>%
    dplyr::arrange(.data[[visitn_col]]) %>%
    dplyr::mutate(
      !!visit_col := forcats::fct_inorder(as.character(.data[[visit_col]]))
    )

  # ============================================================================
  # 9. COMPUTE TREATMENT-LEVEL SAMPLE SIZES
  # Used in plot subtitle and for joining back to the stats table.
  # ============================================================================
  trt_counts <- data_filt %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(t_col))) %>%
    dplyr::summarise(total_n = dplyr::n(), .groups = "drop")

  # ============================================================================
  # 10. SELECT ANALYSIS COLUMNS
  # Keep only variables needed downstream to reduce memory footprint.
  # ============================================================================
  keep_cols <- unique(c(
    t_col, tn_col, m_col, paramcd_col, param_col,
    visitn_col, visit_col, outlier_var_name
  ))
  if (has_lo) keep_cols <- c(keep_cols, lo_col)
  if (has_hi) keep_cols <- c(keep_cols, hi_col)
  if (has_atptn) keep_cols <- c(keep_cols, atptn_col)
  if (!is.null(atpt_col) && atpt_col %in% names(data_filt)) {
    keep_cols <- c(keep_cols, atpt_col)
  }
  keep_cols <- unique(keep_cols)

  data_filt <- data_filt %>%
    dplyr::select(dplyr::all_of(keep_cols))

  # ============================================================================
  # 11. PARAMETER AND TIMEPOINT EXTRACTION
  # Replaces SAS PROC SQL select distinct paramcd / atptn.
  # Outer loop: unique PARAMCDs. Inner loop: unique ATPTNs (if present).
  # ============================================================================
  paramcds <- data_filt %>%
    dplyr::distinct(.data[[paramcd_col]]) %>%
    dplyr::pull(.data[[paramcd_col]])

  if (has_atptn) {
    all_atptns <- data_filt %>%
      tidyr::drop_na(dplyr::all_of(atptn_col)) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c(paramcd_col, atptn_col)))) %>%
      dplyr::arrange(.data[[paramcd_col]], .data[[atptn_col]])
  }

  # Create output directory
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  output_files <- character(0)

  # ============================================================================
  # 12. MAIN RENDERING LOOP — PARAMCD x ATPTN x PAGE
  # Replaces SAS triple-nested %DO loop in %boxplot_each_param_tp macro:
  #   LOOP 1: PARAMCDs
  #   LOOP 2: ATPTNs within each PARAMCD
  #   LOOP 3: Pages within each PARAMCD x ATPTN (via block ranges)
  # ============================================================================
  for (pc in paramcds) {
    param_df <- data_filt %>%
      dplyr::filter(.data[[paramcd_col]] == pc)

    param_label <- param_df %>%
      dplyr::distinct(.data[[param_col]]) %>%
      dplyr::pull(.data[[param_col]]) %>%
      .[1]

    # --- ATPTN sub-loop ---
    if (has_atptn) {
      pc_atptns <- all_atptns %>%
        dplyr::filter(.data[[paramcd_col]] == pc) %>%
        dplyr::pull(.data[[atptn_col]])
      if (length(pc_atptns) == 0L) pc_atptns <- NA_real_
    } else {
      pc_atptns <- NA_real_
    }

    for (atp in pc_atptns) {

      # Filter for this analysis timepoint
      if (has_atptn && !is.na(atp)) {
        tp_df <- param_df %>%
          dplyr::filter(.data[[atptn_col]] == atp)

        atpt_label <- if (!is.null(atpt_col) && atpt_col %in% names(tp_df)) {
          tp_df %>%
            dplyr::distinct(.data[[atpt_col]]) %>%
            dplyr::pull(.data[[atpt_col]]) %>%
            .[1]
        } else {
          as.character(atp)
        }
        title_extra <- paste0(param_label, " — ", atpt_label)
        file_tag    <- paste0(pc, "_atp", atp)
      } else {
        tp_df       <- param_df
        title_extra <- param_label
        file_tag    <- pc
      }

      if (nrow(tp_df) == 0L) next

      # ------------------------------------------------------------------
      # 12a. REFERENCE LINES
      # Replaces SAS: %util_get_reference_lines
      # Mode NARROW: max(ANRLO), min(ANRHI) — giving the tightest band
      # This is the DISTINGUISHING FEATURE of Figure 7.4.
      # ------------------------------------------------------------------
      ref_vals <- util_get_reference(
        df       = tp_df,
        low_var  = if (has_lo) lo_col else NULL,
        high_var = if (has_hi) hi_col else NULL,
        ref_lines = ref_lines
      )

      # ------------------------------------------------------------------
      # 12b. AXIS RANGE
      # Replaces SAS: %util_get_var_min_max + %util_axis_order
      # Range expanded to include reference line positions.
      # ------------------------------------------------------------------
      mm <- util_get_var_min_max(
        df    = tp_df,
        var   = m_col,
        extra = ref_vals
      )

      if (all(is.na(mm))) next

      ax <- util_axis_order(min_val = mm["min"], max_val = mm["max"])

      # ------------------------------------------------------------------
      # 12c. PAGINATION
      # Replaces SAS: %util_boxplot_block_ranges
      # Groups visits into pages with max_boxes_per_page limit, keeping
      # all treatment arms within a visit block on the same page.
      # ------------------------------------------------------------------
      blocks <- util_boxplot_block_ranges(
        df                 = tp_df,
        block_var          = visitn_col,
        cat_vars           = c(visitn_col, t_col),
        max_boxes_per_page = max_boxes_per_page
      )

      n_pages <- max(length(blocks$ranges), 1L)

      # ------------------------------------------------------------------
      # 12d. PAGE LOOP
      # ------------------------------------------------------------------
      for (pg in seq_len(n_pages)) {

        # Filter data for this page's visit blocks
        if (n_pages > 1L && nrow(blocks$pages) > 0L) {
          page_visitns <- blocks$pages %>%
            dplyr::filter(.data[["page"]] == pg) %>%
            dplyr::pull(.data[[visitn_col]])
          pg_df <- tp_df %>%
            dplyr::filter(.data[[visitn_col]] %in% page_visitns)
        } else {
          pg_df <- tp_df
        }

        if (nrow(pg_df) == 0L) next

        # Sort by visit then treatment (matches SAS: proc sort by avisitn trtpn)
        pg_df <- pg_df %>%
          dplyr::arrange(
            !!rlang::sym(visitn_col),
            !!rlang::sym(tn_col)
          )

        # ----------------------------------------------------------------
        # 12e. BUILD BOXPLOT
        # Replaces SAS: PROC SGRENDER template=PhUSEboxplot
        # phuse_boxplot() creates the ggplot2 object with box fill, whiskers,
        # notches, mean markers (diamond), outlier points (red), and
        # reference lines (geom_hline) — all styled per PhUSE theme.
        # ----------------------------------------------------------------
        plot_title <- paste("Figure 7.4", title_extra)
        if (n_pages > 1L) {
          plot_title <- paste0(
            plot_title, " (Page ", pg, " of ", n_pages, ")"
          )
        }

        p <- phuse_boxplot(
          data        = pg_df,
          x_var       = t_col,
          y_var       = m_col,
          block_var   = visit_col,
          outlier_var = outlier_var_name,
          ref_lines   = ref_vals,
          y_min       = attr(ax, "axis_min"),
          y_max       = attr(ax, "axis_max"),
          y_incr      = attr(ax, "step"),
          title       = plot_title,
          y_label     = param_label,
          show_mean   = TRUE,
          show_notch  = TRUE
        )

        # Additional axis and label customisation
        p <- p +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::labs(
            subtitle = if (!is.null(ref_vals) && length(ref_vals) > 0L) {
              paste0(
                "Normal range reference lines (",
                ref_lines, "): ",
                paste(janitor::round_half_up(ref_vals, 2), collapse = ", ")
              )
            } else {
              NULL
            },
            caption = paste0(
              "Population: ", p_fl, " = 'Y'  |  ",
              "Analysis: ", a_fl, " = 'Y'  |  ",
              "Dataset: ", ds_name
            )
          ) +
          ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
              size = 9, hjust = 0.5, face = "italic"
            ),
            plot.caption  = ggplot2::element_text(
              size = 8, hjust = 0
            ),
            axis.line.x   = ggplot2::element_line(
              colour = phuse_colors$box_outline, linewidth = 0.5
            ),
            axis.line.y   = ggplot2::element_line(
              colour = phuse_colors$box_outline, linewidth = 0.5
            )
          )

        # ----------------------------------------------------------------
        # 12f. SUMMARY STATISTICS TABLE
        # Replaces SAS: PROC SUMMARY / AXISTABLE inner-margin rows.
        # Computes N, Mean, Median per visit x treatment, formatted with
        # janitor::round_half_up() for SAS rounding parity (AAP Gate 2).
        # ----------------------------------------------------------------
        stats_tbl <- phuse_boxplot_stats_table(
          data      = pg_df,
          x_var     = visit_col,
          y_var     = m_col,
          group_var = t_col,
          stats     = c("n", "mean", "median"),
          digits    = 1
        )

        # Join overall treatment counts for reference
        stats_tbl <- stats_tbl %>%
          dplyr::left_join(trt_counts, by = t_col)

        # Pivot wider: one row per visit, treatment stats as columns
        # This creates a compact display matching the boxplot x-axis layout
        stats_wide <- stats_tbl %>%
          dplyr::mutate(
            dplyr::across(
              c("mean", "median"),
              ~ format(janitor::round_half_up(.x, 1), nsmall = 1)
            ),
            n = as.character(n)
          ) %>%
          dplyr::select(
            dplyr::all_of(c(visit_col, t_col)),
            N = n, Mean = mean, Median = median
          ) %>%
          tidyr::pivot_wider(
            id_cols     = dplyr::all_of(visit_col),
            names_from  = dplyr::all_of(t_col),
            values_from = c("N", "Mean", "Median"),
            names_glue  = "{.name}_{.value}"
          )

        # Build table grob for composition below the boxplot
        tbl_grob <- gridExtra::tableGrob(
          stats_wide,
          rows  = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size    = 8,
            core    = list(fg_params = list(hjust = 0.5, x = 0.5)),
            colhead = list(fg_params = list(hjust = 0.5, x = 0.5,
                                            fontface = "bold"))
          )
        )

        # ----------------------------------------------------------------
        # 12g. COMPOSE PLOT + TABLE AND SAVE PDF
        # Replaces SAS: ODS PDF FILE="&output_path/figure.pdf";
        # gridExtra::arrangeGrob() stacks the boxplot above the stats table.
        # ggplot2::ggsave() writes the combined figure to PDF.
        # ----------------------------------------------------------------
        combined <- gridExtra::arrangeGrob(
          p, tbl_grob,
          ncol    = 1,
          heights = c(3, 1)
        )

        # Generate unique output filename
        page_tag <- if (n_pages > 1L) paste0("_pg", pg) else ""
        out_file <- file.path(
          output_path,
          paste0("WPCT-F-07-04_", file_tag, page_tag, ".pdf")
        )

        ggplot2::ggsave(
          filename = out_file,
          plot     = combined,
          width    = phuse_sizes$design_width_mm,
          height   = phuse_sizes$design_height_mm + 50,
          units    = "mm",
          device   = "pdf"
        )

        # Interactive preview: display combined figure in the graphics device
        # when running in an interactive R session (e.g., RStudio, R console).
        # In non-interactive mode (batch scripts, CI), this is safely skipped.
        if (interactive()) {
          gridExtra::grid.arrange(
            p, tbl_grob,
            ncol    = 1,
            heights = c(3, 1)
          )
        }

        output_files <- c(output_files, out_file)

      } # end page loop
    } # end ATPTN loop
  } # end PARAMCD loop

  # ============================================================================
  # 13. RETURN
  # ============================================================================
  if (length(output_files) == 0L) {
    warning("No figures were generated. Check data content and filter criteria.",
            call. = FALSE)
  } else {
    message(
      "Figure 7.4 generation complete. ",
      length(output_files), " PDF(s) written to: ", output_path
    )
  }

  invisible(output_files)
}


# ==============================================================================
# MIGRATION NOTES
# ==============================================================================
# ASSUMPTIONS:
#    1. No WPCT-F.07.04.sas source exists in the repository. The SAS numbering
#       sequence jumps from 07.03 to 07.06. No WPCT_Fig_7.4 specification
#       document (WPCT_Fig_7.4_RequirementsSpecification.docx) was found.
#    2. This script was created based on:
#       - The AAP description: "Boxplot variant -> ggplot2::geom_boxplot with
#         reference lines" (AAP section 0.5.1)
#       - Established patterns from WPCT-F.07.01.sas (single boxplot per
#         parameter/timepoint with reference lines)
#       - Established patterns from WPCT-F.07.03.sas (parameterised macro
#         structure, triple-nested loop)
#    3. Figure 7.4 is interpreted as a SINGLE-PANEL boxplot variant (like
#       Figure 7.1) distinguished by the use of NARROW reference lines
#       derived from ANRLO/ANRHI rather than UNIFORM reference lines from
#       analysis range variables (A1LO/A1HI).
#    4. The default dataset is advs.xpt (Analysis Dataset for Vital Signs)
#       rather than adlbc.xpt (Lab Chemistry) used by Figure 7.1, reflecting
#       the use of ANRLO/ANRHI instead of A1LO/A1HI.
#    5. SAS round-half-up behaviour is preserved using janitor::round_half_up()
#       for all displayed statistics (AAP section 0.7.2, Gate 2).
#    6. Missing measurement values (SAS numeric .) map to NA_real_ and are
#       excluded via tidyr::drop_na() before analysis. Missing reference range
#       values result in NA outliers (no red dot overlay). Missing values are
#       never implicitly zero-substituted (AAP section 0.7.3).
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Cannot compare against SAS output since no SAS source exists.
#    2. No specification document WPCT_Fig_7.4_RequirementsSpecification.docx
#       was found for numerical baseline comparison.
#    3. Boxplot whisker/fence calculations use ggplot2's default coef = 1.5
#       (matching SAS IQR * 1.5 fence) — no difference expected.
#    4. Notch calculations: ggplot2 uses 1.58 * IQR / sqrt(n), matching the
#       SAS formula — no difference expected for non-degenerate groups.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PhUSEboxplot GTL template global registration is replaced by
#       phuse_boxplot() + theme_phuse() called per plot.
#    2. SAS AXISTABLE inner-margin statistics are replaced by a separate
#       gridExtra::tableGrob stacked below the plot.
#    3. SAS BLOCKPLOT top inner-margin visit labels are replaced by
#       ggplot2 facet strips or composite x-axis factors.
#
# PACKAGE SELECTION RATIONALE:
#    - ggplot2: Mandated replacement for SAS/GRAPH and GTL (AAP section 0.7.1)
#    - haven: SAS XPT transport file I/O (AAP section 0.6.1)
#    - dplyr: Tidyverse mandate for data manipulation (AAP section 0.8.1)
#    - tidyr: Tidyverse reshaping (pivot_wider for stats display)
#    - forcats: Factor ordering preserving SAS FORMAT sort order
#    - janitor: round_half_up() for SAS rounding parity (AAP section 0.7.2)
#    - gridExtra: Plot + table composition (arrangeGrob, tableGrob)
#    - yaml: Configuration loading replacing SAS %let globals
#    - rlang: Tidy evaluation for dynamic column references
#
# OPEN QUESTIONS:
#    1. Should this Figure 7.4 exist at all given no SAS source or
#       specification document? Requires confirmation from WG5 lead.
#    2. What visually distinguishes Figure 7.4 from Figure 7.1 beyond the
#       reference line mode? Requires review by statistician.
#    3. Is advs.xpt the intended default dataset, or should this use adlbc.xpt
#       like Figure 7.1? Requires subject matter expert review.
#    4. Should the stats table show additional statistics (SD, Q1, Q3, Min, Max)
#       matching the full SAS AXISTABLE, or is the compact N/Mean/Median
#       sufficient? Requires review.
#    5. Are there specific PARAMCD / ATPTN subsetting criteria that should be
#       hardcoded as defaults for this figure? Requires review.
# ==============================================================================
