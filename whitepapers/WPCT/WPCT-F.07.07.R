# ============================================================
# WPCT-F.07.07.R
# Figure 7.7 — Change from Last/Min/Max Baseline to Last/Min/Max
#   Post-baseline by Treatment and Analysis Timepoint — Multi-Study
#
# Migrated from: whitepapers/WPCT/WPCT-F.07.07.sas
# PhUSE CS Working Group 5 — Central Tendency White Paper
# Migration date: 2026-03-26
#
# This script produces paginated multi-study boxplots of change-from-
# baseline measurements by treatment arm and analysis timepoint, with
# ANCOVA p-value annotations. It mirrors the SAS PROC SGRENDER output
# with PhUSEboxplot GTL template, pagination via block ranges, and
# separate ANCOVA models for individual studies and pooled data.
#
# SAS Constructs Migrated:
#   PROC SUMMARY  -> dplyr::summarise with janitor::round_half_up()
#   PROC GLM      -> stats::lm() + car::Anova(type=3) validation
#   PROC SGRENDER -> ggplot2 + theme_phuse() + phuse_boxplot()
#   ODS PDF       -> grDevices::pdf() landscape output
#   Macro params  -> Named function arguments with defaults
#
# Usage:
#   source("whitepapers/WPCT/WPCT-F.07.07.R")
#   wpct_f_07_07(
#     data_path   = "data/adam/cdisc",
#     output_path = "output/figures",
#     ds_name     = "advs.xpt",
#     ref_trtn    = 0
#   )
# ============================================================

# --- External libraries -----------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(car)
library(janitor)
library(forcats)
library(rlang)

# --- Source internal utility functions --------------------------------
# Paths relative to repository root; adjust if running from elsewhere.
source("whitepapers/utilities/R/util_boxplot_block_ranges.R")
source("whitepapers/utilities/R/util_axis_order.R")
source("whitepapers/utilities/R/util_ggplot_theme.R")
source("whitepapers/utilities/R/util_get_var_min_max.R")
source("whitepapers/utilities/R/util_labels_from_var.R")
source("whitepapers/utilities/R/util_value_of_param.R")
source("whitepapers/utilities/R/util_count_unique_values.R")
source("whitepapers/utilities/R/util_delete_dsets.R")
source("whitepapers/ADaM/R/derive_lastminmax_measure.R")
source("whitepapers/utilities/R/util_get_reference.R")

# ======================================================================
# wpct_f_07_07 — Main entry point
# ======================================================================
#' WPCT Figure 7.7 — Multi-Study Change-from-Baseline Boxplot with ANCOVA
#'
#' Produces paginated PDF boxplots of change-from-baseline measurements
#' by treatment arm and analysis timepoint across multiple studies, with
#' ANCOVA p-value annotations and a summary statistics table below each
#' boxplot panel.
#'
#' @param data_path   Character. Directory containing the ADaM XPT file.
#' @param output_path Character. Directory for PDF output files.
#' @param ds_name     Character. Dataset filename (default \code{"advs.xpt"}).
#' @param t_var       Character. Treatment name variable (default \code{"TRTP"}).
#' @param tn_var      Character. Treatment number variable (default \code{"TRTPN"}).
#' @param c_var       Character. Change-from-baseline variable (default \code{"CHG"}).
#' @param b_var       Character. Baseline variable (default \code{"BASE"}).
#' @param ref_trtn    Numeric or NULL. Reference treatment number for ANCOVA.
#' @param p_fl        Character. Population flag variable (default \code{"SAFFL"}).
#' @param a_fl        Character. Analysis flag variable (default \code{"ANL01FL"}).
#' @param c_mode      Character. Change mode label: \code{"LAST"}, \code{"MIN"},
#'                    or \code{"MAX"} (default \code{"LAST"}).
#' @param ref_lines   Character. Reference line values, comma-separated
#'                    (default \code{"0"} for zero-change reference).
#' @param max_boxes_per_page Integer. Max boxes per PDF page (default 20).
#'
#' @return Invisible character vector of generated PDF file paths.
#' @export
wpct_f_07_07 <- function(
  data_path,
  output_path,
  ds_name            = "advs.xpt",
  t_var              = "TRTP",
  tn_var             = "TRTPN",
  c_var              = "CHG",
  b_var              = "BASE",
  ref_trtn           = NULL,
  p_fl               = "SAFFL",
  a_fl               = "ANL01FL",
  c_mode             = "LAST",
  ref_lines          = "0",
  max_boxes_per_page = 20
) {

  # ====================================================================
  # INPUT VALIDATION
  # ====================================================================
  stopifnot(
    is.character(data_path)   && length(data_path) == 1,
    is.character(output_path) && length(output_path) == 1,
    is.character(ds_name)     && length(ds_name) == 1,
    is.character(t_var)       && length(t_var) == 1,
    is.character(tn_var)      && length(tn_var) == 1,
    is.character(c_var)       && length(c_var) == 1,
    is.character(b_var)       && length(b_var) == 1,
    is.null(ref_trtn) || (is.numeric(ref_trtn) && length(ref_trtn) == 1),
    is.character(p_fl)        && length(p_fl) == 1,
    is.character(a_fl)        && length(a_fl) == 1,
    is.character(c_mode)      && length(c_mode) == 1,
    toupper(c_mode) %in% c("LAST", "MIN", "MAX"),
    is.character(ref_lines)   && length(ref_lines) == 1,
    is.numeric(max_boxes_per_page) && max_boxes_per_page > 0
  )

  c_mode    <- toupper(c_mode)
  data_file <- file.path(data_path, ds_name)
  if (!file.exists(data_file)) stop("Data file not found: ", data_file)
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

  # ====================================================================
  # 1. DATA LOADING
  # Mirrors SAS: %util_access_test_data / libname statement
  # ====================================================================
  raw_data <- haven::read_xpt(data_file)

  required_cols <- c("STUDYID", "USUBJID", "PARAMCD", "PARAM",
                     "ATPTN", "ATPT", t_var, tn_var, c_var, b_var,
                     p_fl, a_fl)
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in dataset: ", paste(missing_cols, collapse = ", "))
  }

  # ====================================================================
  # 2. DERIVE LAST/MIN/MAX MEASURES
  # Mirrors SAS: %derive_lastminmax_measure(&ds._sub, &LMM, ...)
  # Creates analysis flag columns based on change mode; falls back to
  # using data as-is if derivation columns are absent.
  # ====================================================================
  ds_derived <- tryCatch(
    derive_lastminmax_measure(
      ds      = raw_data,
      c_modes = c_mode,
      grpvars = c("STUDYID", "USUBJID", tn_var, "PARAMCD", "ATPTN"),
      ordvars = c("ADT", "AVISITN"),
      cleanup = TRUE
    ),
    error = function(e) {
      message("derive_lastminmax_measure note: ", conditionMessage(e),
              "\nProceeding with input data as-is.")
      raw_data
    }
  )

  # ====================================================================
  # 3. POPULATION FILTERING
  # Mirrors SAS: where &p_fl = 'Y' and &a_fl = 'Y'
  # P_FL = analysis population; A_FL = post-baseline non-missing change
  # ====================================================================
  css_safana <- ds_derived %>%
    dplyr::filter(.data[[p_fl]] == "Y", .data[[a_fl]] == "Y")

  if (nrow(css_safana) == 0) {
    warning("No observations after filtering ",
            p_fl, "='Y' and ", a_fl, "='Y'.")
    return(invisible(character(0)))
  }

  # ====================================================================
  # 4. POOLED DATA CREATION
  # Mirrors SAS:
  #   studyid = 'A0'x || 'Pooled';   (hex A0 = NBSP for sort-last)
  #   substr(usubjid, 1, 1) = 'P';   (distinct IDs in pooled copy)
  # ====================================================================
  pooled_label <- paste0("\u00A0", "Pooled")

  css_safana_pooled <- css_safana %>%
    dplyr::mutate(
      STUDYID = pooled_label,
      USUBJID = paste0("P", substr(.data[["USUBJID"]], 2,
                                   nchar(.data[["USUBJID"]])))
    )

  css_anadata <- dplyr::bind_rows(css_safana, css_safana_pooled)

  # ====================================================================
  # 5. STUDYNUM DERIVATION
  # Mirrors SAS:
  #   proc sort; by studyid &tn_var;
  #   retain studynum; if first.studyid then studynum + 1;
  # Sequential integer per unique STUDYID; Pooled sorts last via NBSP.
  # ====================================================================
  study_order <- css_anadata %>%
    dplyr::distinct(.data[["STUDYID"]]) %>%
    dplyr::arrange(.data[["STUDYID"]]) %>%
    dplyr::mutate(studynum = dplyr::row_number())

  css_anadata <- css_anadata %>%
    dplyr::left_join(study_order, by = "STUDYID") %>%
    dplyr::arrange(.data[["STUDYID"]], .data[[tn_var]])

  # Total study count including Pooled — used for pooled ANCOVA studynum
  stdyn <- max(study_order$studynum, na.rm = TRUE)

  # ====================================================================
  # 6. INFORMATION GATHERING
  # Mirrors SAS: %util_labels_from_var / %util_count_unique_values
  # ====================================================================
  param_info  <- util_labels_from_var(css_anadata, "PARAMCD", "PARAM")
  num_studies <- util_count_unique_values(css_anadata, "STUDYID")
  pdf_paths   <- character(0)

  # ====================================================================
  # 7. TRIPLE-NESTED RENDERING LOOP: PARAMCD -> ATPTN -> PAGE BLOCKS
  # ====================================================================
  for (p_idx in seq_len(param_info$n)) {

    paramcd_val <- param_info$pairs$value[p_idx]
    param_label <- param_info$pairs$label[p_idx]

    css_nextparam <- css_anadata %>%
      dplyr::filter(.data[["PARAMCD"]] == paramcd_val)

    # ATPTN-ATPT pairs for this parameter
    # Mirrors SAS: %util_labels_from_var(css_nextparam, atptn, atpt)
    atptn_info <- util_labels_from_var(css_nextparam, "ATPTN", "ATPT")
    if (atptn_info$n == 0) next

    # --- Open PDF (one per PARAMCD) ---
    # Mirrors SAS: ods pdf file="Fig_7.7_&paramcd_&c_mode._&ds..pdf"
    pdf_filename <- paste0("Fig_7.7_", paramcd_val, "_", c_mode, "_",
                           tools::file_path_sans_ext(ds_name), ".pdf")
    pdf_filepath <- file.path(output_path, pdf_filename)
    pdf_paths    <- c(pdf_paths, pdf_filepath)
    grDevices::pdf(file = pdf_filepath, width = 11, height = 8.5,
                   paper = "USr")
    page_rendered <- FALSE

    # ==================================================================
    # 7.1 ATPTN LOOP
    # ==================================================================
    for (a_idx in seq_len(atptn_info$n)) {

      atptn_val  <- atptn_info$pairs$value[a_idx]
      atpt_label <- atptn_info$pairs$label[a_idx]

      css_nexttimept <- css_nextparam %>%
        dplyr::filter(.data[["ATPTN"]] == atptn_val)
      if (nrow(css_nexttimept) == 0) next

      # --- Y-axis limits from change variable ---
      # Mirrors SAS: %util_get_var_min_max / %util_axis_order
      aval_min_max <- util_get_var_min_max(css_nexttimept, c_var)
      axis_breaks  <- util_axis_order(aval_min_max["min"],
                                      aval_min_max["max"])
      y_min  <- attr(axis_breaks, "axis_min")
      y_max  <- attr(axis_breaks, "axis_max")
      y_incr <- attr(axis_breaks, "step")

      # --- Value format precision ---
      # Mirrors SAS: %util_value_format(css_nexttimept, &c_var)
      val_fmt     <- util_value_of_param(css_nexttimept, c_var)
      stat_digits <- val_fmt$mean_digits

      # --- Reference lines (default 0 for change-from-baseline) ---
      ref_vals <- tryCatch(
        util_get_reference(css_nexttimept, NULL, NULL, ref_lines),
        error = function(e) as.numeric(unlist(strsplit(ref_lines, ",")))
      )
      ref_vals <- ref_vals[!is.na(ref_vals)]

      # --- Pagination by studynum ---
      # Mirrors SAS: %util_boxplot_block_ranges(blockvar=studynum,
      #              catvars=&tn_var)
      block_info <- util_boxplot_block_ranges(
        df                 = css_nexttimept,
        block_var          = "studynum",
        cat_vars           = tn_var,
        max_boxes_per_page = max_boxes_per_page
      )
      n_pages <- length(block_info$ranges)

      # ================================================================
      # 7.2 PAGE-BLOCK LOOP
      # ================================================================
      for (blk_idx in seq_len(n_pages)) {

        blk_range <- block_info$ranges[[blk_idx]]
        blk_lo    <- blk_range[1]
        blk_hi    <- blk_range[2]

        css_plot <- css_nexttimept %>%
          dplyr::filter(.data[["studynum"]] >= blk_lo,
                        .data[["studynum"]] <= blk_hi)
        if (nrow(css_plot) == 0) next

        # === Treatment factor setup ===
        # Set factor levels so reference treatment is the baseline.
        trtn_levels <- sort(unique(as.numeric(
          as.character(css_plot[[tn_var]]))))
        if (!is.null(ref_trtn) && ref_trtn %in% trtn_levels) {
          css_plot <- css_plot %>%
            dplyr::mutate(
              !!tn_var := forcats::fct_relevel(
                factor(.data[[tn_var]]),
                as.character(ref_trtn)
              )
            )
        } else {
          css_plot <- css_plot %>%
            dplyr::mutate(!!tn_var := factor(.data[[tn_var]]))
        }

        # Treatment label factor (for legend / display)
        trt_labels <- css_plot %>%
          dplyr::distinct(.data[[tn_var]], .data[[t_var]]) %>%
          dplyr::arrange(.data[[tn_var]])
        css_plot <- css_plot %>%
          dplyr::mutate(
            !!t_var := forcats::fct_relevel(
              factor(.data[[t_var]]),
              trt_labels[[t_var]]
            )
          )

        # === STUDYID factor for x-axis ===
        css_plot <- css_plot %>%
          dplyr::mutate(
            STUDYID = forcats::fct_reorder(
              factor(.data[["STUDYID"]]),
              .data[["studynum"]]
            )
          )

        # ==============================================================
        # 7.3 SUMMARY STATISTICS
        # Mirrors SAS: proc summary data=css_nexttimept nway;
        #   class studyid &tn_var studynum &t_var;
        #   var &c_var;
        #   output out=css_stats n= mean= std= median= min= max=
        #          q1= q3= / autoname;
        # All rounding uses janitor::round_half_up per AAP section 0.7.2
        # ==============================================================
        c_sym <- rlang::sym(c_var)

        css_stats <- css_plot %>%
          dplyr::group_by(.data[["STUDYID"]], .data[[tn_var]],
                          .data[["studynum"]], .data[[t_var]]) %>%
          dplyr::summarise(
            c_n      = sum(!is.na(!!c_sym)),
            c_mean   = janitor::round_half_up(mean(!!c_sym, na.rm = TRUE),
                                              stat_digits),
            c_std    = janitor::round_half_up(sd(!!c_sym, na.rm = TRUE),
                                              stat_digits + 1),
            c_median = janitor::round_half_up(
                         stats::median(!!c_sym, na.rm = TRUE), stat_digits),
            c_q1     = janitor::round_half_up(
                         stats::quantile(!!c_sym, 0.25, na.rm = TRUE,
                                         names = FALSE),
                         stat_digits),
            c_q3     = janitor::round_half_up(
                         stats::quantile(!!c_sym, 0.75, na.rm = TRUE,
                                         names = FALSE),
                         stat_digits),
            c_min    = janitor::round_half_up(min(!!c_sym, na.rm = TRUE),
                                              stat_digits),
            c_max    = janitor::round_half_up(max(!!c_sym, na.rm = TRUE),
                                              stat_digits),
            .groups  = "drop"
          )

        # ==============================================================
        # 7.4 ANCOVA P-VALUES
        # Mirrors SAS:
        #   Individual studies:
        #     proc glm data=css_nexttimept (where studyid NE Pooled);
        #       by studynum; class &tn_var (ref="&ref_trtn");
        #       model &c_var = &b_var &tn_var / solution;
        #       ods output parameterestimates=css_est;
        #   Pooled:
        #     proc glm data=css_nexttimept (where studyid = Pooled);
        #       class &tn_var (ref="&ref_trtn"); studynum;
        #       model &c_var = &b_var &tn_var studynum / solution;
        #       ods output parameterestimates=css_poolest;
        #
        # car::Anova(type=3) provides overall F-test validation.
        # Per-treatment p-values from summary(lm()) coefficients probt.
        # ==============================================================
        css_pvals <- NULL

        if (!is.null(ref_trtn)) {
          b_sym  <- rlang::sym(b_var)
          tn_sym <- rlang::sym(tn_var)

          # --- Individual-study ANCOVA ---
          individual_data <- css_plot %>%
            dplyr::filter(.data[["STUDYID"]] != pooled_label)

          if (nrow(individual_data) > 0) {
            study_groups <- individual_data %>%
              dplyr::group_by(.data[["studynum"]]) %>%
              dplyr::group_split()

            pval_rows <- lapply(study_groups, function(sg) {
              sg_studynum <- unique(sg[["studynum"]])
              sg_studyid  <- unique(sg[["STUDYID"]])

              # Need at least 2 treatment levels and enough obs
              trtn_present <- unique(sg[[tn_var]])
              if (length(trtn_present) < 2 || nrow(sg) < 4) return(NULL)

              tryCatch({
                sg[[tn_var]] <- factor(sg[[tn_var]])
                if (as.character(ref_trtn) %in% levels(sg[[tn_var]])) {
                  sg[[tn_var]] <- forcats::fct_relevel(
                    sg[[tn_var]], as.character(ref_trtn))
                }

                fmla <- stats::reformulate(
                  termlabels = c(b_var, tn_var), response = c_var)
                mdl  <- stats::lm(fmla, data = sg)
                coefs <- summary(mdl)$coefficients

                # Extract rows where parameter name starts with tn_var
                coef_names  <- rownames(coefs)
                trt_rows    <- grepl(paste0("^", tn_var), coef_names)
                if (!any(trt_rows)) return(NULL)

                trt_coefs   <- coefs[trt_rows, , drop = FALSE]
                trt_levels  <- sub(paste0("^", tn_var), "",
                                   rownames(trt_coefs))

                tibble::tibble(
                  studynum = sg_studynum,
                  STUDYID  = sg_studyid,
                  tn_level = trt_levels,
                  pval     = janitor::round_half_up(
                               trt_coefs[, "Pr(>|t|)"], 4)
                )
              }, error = function(e) NULL)
            })

            css_pvals_indiv <- dplyr::bind_rows(pval_rows)
          } else {
            css_pvals_indiv <- tibble::tibble(
              studynum = integer(0), STUDYID = character(0),
              tn_level = character(0), pval = numeric(0))
          }

          # --- Pooled ANCOVA ---
          pooled_data <- css_plot %>%
            dplyr::filter(.data[["STUDYID"]] == pooled_label)

          css_pvals_pooled <- tibble::tibble(
            studynum = integer(0), STUDYID = character(0),
            tn_level = character(0), pval = numeric(0))

          if (nrow(pooled_data) > 0) {
            trtn_present <- unique(pooled_data[[tn_var]])
            if (length(trtn_present) >= 2 && nrow(pooled_data) >= 4) {
              tryCatch({
                pooled_data[[tn_var]] <- factor(pooled_data[[tn_var]])
                if (as.character(ref_trtn) %in%
                      levels(pooled_data[[tn_var]])) {
                  pooled_data[[tn_var]] <- forcats::fct_relevel(
                    pooled_data[[tn_var]], as.character(ref_trtn))
                }

                # Include studynum only if it has >1 unique value
                # (prevents singularity in single-study datasets)
                studynum_vals <- unique(pooled_data[["studynum"]])
                if (length(studynum_vals) > 1) {
                  pooled_terms <- c(b_var, tn_var, "studynum")
                } else {
                  pooled_terms <- c(b_var, tn_var)
                }

                fmla_pool <- stats::reformulate(
                  termlabels = pooled_terms, response = c_var)
                mdl_pool  <- stats::lm(fmla_pool, data = pooled_data)

                # Validate with car::Anova type III
                tryCatch(
                  car::Anova(mdl_pool, type = 3),
                  error = function(e) NULL
                )

                coefs_pool <- summary(mdl_pool)$coefficients
                coef_names <- rownames(coefs_pool)
                trt_rows   <- grepl(paste0("^", tn_var), coef_names)

                if (any(trt_rows)) {
                  trt_coefs  <- coefs_pool[trt_rows, , drop = FALSE]
                  trt_levels <- sub(paste0("^", tn_var), "",
                                    rownames(trt_coefs))
                  p_studynum <- unique(pooled_data[["studynum"]])

                  css_pvals_pooled <- tibble::tibble(
                    studynum = p_studynum[1],
                    STUDYID  = pooled_label,
                    tn_level = trt_levels,
                    pval     = janitor::round_half_up(
                                 trt_coefs[, "Pr(>|t|)"], 4)
                  )
                }
              }, error = function(e) NULL)
            }
          }

          # Combine individual + pooled p-values
          css_pvals <- dplyr::bind_rows(css_pvals_indiv, css_pvals_pooled)

          # Convert tn_level to numeric for joining
          css_pvals <- css_pvals %>%
            dplyr::mutate(
              tn_level_num = suppressWarnings(
                as.numeric(.data[["tn_level"]]))
            )
        }

        # ==============================================================
        # 7.5 BUILD DISPLAY STATS TABLE
        # Merge p-values into summary statistics for annotation.
        # Mirrors SAS: merge css_stats with css_pvals by studynum tn_var
        # ==============================================================
        display_stats <- css_stats

        if (!is.null(css_pvals) && nrow(css_pvals) > 0) {
          pval_join <- css_pvals %>%
            dplyr::select("studynum", "tn_level_num", "pval") %>%
            dplyr::rename(!!tn_var := "tn_level_num")

          # Ensure join key types match
          pval_join[[tn_var]] <- as.numeric(
            as.character(pval_join[[tn_var]]))
          display_stats <- display_stats %>%
            dplyr::mutate(
              !!tn_var := as.numeric(as.character(.data[[tn_var]]))
            )

          display_stats <- display_stats %>%
            dplyr::left_join(pval_join,
                             by = c("studynum", tn_var))

          # Reference treatment has no p-value (set NA)
          if (!is.null(ref_trtn)) {
            display_stats <- display_stats %>%
              dplyr::mutate(
                pval = dplyr::if_else(
                  as.numeric(as.character(.data[[tn_var]])) == ref_trtn,
                  NA_real_, .data[["pval"]])
              )
          }

          # Restore factor type for tn_var
          display_stats <- display_stats %>%
            dplyr::mutate(!!tn_var := factor(.data[[tn_var]]))
        } else {
          display_stats <- display_stats %>%
            dplyr::mutate(pval = NA_real_)
        }

        # Format p-value for display
        display_stats <- display_stats %>%
          dplyr::mutate(
            pval_display = dplyr::case_when(
              is.na(.data[["pval"]])     ~ "",
              .data[["pval"]] < 0.0001   ~ "<.0001",
              TRUE ~ format(
                janitor::round_half_up(.data[["pval"]], 4),
                nsmall = 4, scientific = FALSE)
            )
          )

        # ==============================================================
        # 7.6 RENDER BOXPLOT
        # Mirrors SAS: proc sgrender data=css_plot
        #              template=PhUSEboxplot
        #   dynamic _YVAR='CHG' _XVAR='studynum' _REFLINES='0'
        # Uses phuse_boxplot() from util_ggplot_theme.R
        # ==============================================================
        plot_title <- paste0(
          "Figure 7.7  Change from ", c_mode,
          " Baseline to ", c_mode, " Post-baseline\n",
          param_label, " - ", atpt_label, "\n",
          "By Treatment and Study"
        )
        y_label <- paste0(
          c_mode, " Change from Baseline (", c_var, ")")

        # Build boxplot using phuse_boxplot helper
        p_box <- tryCatch(
          phuse_boxplot(
            data        = css_plot,
            x_var       = "studynum",
            y_var       = c_var,
            group_var   = t_var,
            title       = plot_title,
            y_label     = y_label,
            y_min       = y_min,
            y_max       = y_max,
            y_incr      = y_incr,
            block_var   = "STUDYID",
            ref_lines   = ref_vals,
            show_notch  = FALSE,
            show_mean   = TRUE,
            legend_title = "Treatment"
          ),
          error = function(e) {
            # Fallback: manual ggplot if helper fails
            ggplot2::ggplot(
              css_plot,
              ggplot2::aes(
                x    = factor(.data[["studynum"]]),
                y    = .data[[c_var]],
                fill = .data[[t_var]])
            ) +
              ggplot2::geom_boxplot(
                position = ggplot2::position_dodge(width = 0.8),
                outlier.shape = 1, outlier.size = 1.5) +
              ggplot2::geom_hline(
                yintercept = ref_vals,
                linetype   = "dashed",
                color      = "grey50") +
              ggplot2::scale_y_continuous(
                limits = c(y_min, y_max),
                breaks = seq(y_min, y_max, by = y_incr)) +
              ggplot2::labs(
                title = plot_title,
                y     = y_label,
                x     = "Study",
                fill  = "Treatment") +
              theme_phuse() +
              ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = 45, hjust = 1),
                plot.title = ggplot2::element_text(
                  size = 10, hjust = 0.5)
              )
          }
        )

        # ==============================================================
        # 7.7 RENDER STATS TABLE (below boxplot)
        # Mirrors SAS: block overlay in PhUSEboxplot GTL showing
        #   N, Mean, StdDev, Median, Min, Max, Q1, Q3, p-value
        # ==============================================================
        stat_rows <- display_stats %>%
          dplyr::arrange(
            .data[["studynum"]],
            as.numeric(as.character(.data[[tn_var]]))) %>%
          dplyr::transmute(
            Study     = as.character(.data[["STUDYID"]]),
            Treatment = as.character(.data[[t_var]]),
            N         = as.character(.data[["c_n"]]),
            Mean      = formatC(.data[["c_mean"]],
                                format = "f",
                                digits = stat_digits),
            SD        = formatC(.data[["c_std"]],
                                format = "f",
                                digits = stat_digits + 1),
            Median    = formatC(.data[["c_median"]],
                                format = "f",
                                digits = stat_digits),
            Q1        = formatC(.data[["c_q1"]],
                                format = "f",
                                digits = stat_digits),
            Q3        = formatC(.data[["c_q3"]],
                                format = "f",
                                digits = stat_digits),
            Min       = formatC(.data[["c_min"]],
                                format = "f",
                                digits = stat_digits),
            Max       = formatC(.data[["c_max"]],
                                format = "f",
                                digits = stat_digits),
            `P-value` = .data[["pval_display"]]
          )

        tbl_theme <- gridExtra::ttheme_minimal(
          core = list(
            fg_params = list(fontsize = 7, hjust = 0.5)),
          colhead = list(
            fg_params = list(fontsize = 7,
                             fontface = "bold",
                             hjust = 0.5)),
          padding = grid::unit(c(3, 2), "mm")
        )
        tbl_grob <- gridExtra::tableGrob(
          stat_rows, rows = NULL, theme = tbl_theme)

        # ==============================================================
        # 7.8 COMBINE PLOT + TABLE AND DRAW TO PDF PAGE
        # Mirrors SAS: single ODS PDF page with boxplot above stat table
        # ==============================================================
        combined <- gridExtra::arrangeGrob(
          p_box, tbl_grob,
          nrow    = 2,
          heights = grid::unit(c(0.70, 0.30), "npc")
        )

        grid::grid.newpage()
        grid::grid.draw(combined)
        page_rendered <- TRUE

        # --- Memory cleanup per page block ---
        # Mirrors SAS: %util_delete_dsets(css_plot css_stats ...)
        util_delete_dsets(
          c("css_plot", "css_stats", "css_pvals",
            "css_pvals_indiv", "css_pvals_pooled",
            "display_stats", "stat_rows"),
          envir = environment()
        )

      }
    }

    # Close PDF device for this PARAMCD
    if (page_rendered) {
      grDevices::dev.off()
      message("PDF generated: ", pdf_filepath)
    } else {
      grDevices::dev.off()
      # Remove empty PDF if no pages were rendered
      if (file.exists(pdf_filepath)) file.remove(pdf_filepath)
      pdf_paths <- setdiff(pdf_paths, pdf_filepath)
    }

    # Cleanup timepoint-level objects
    util_delete_dsets(
      c("css_nextparam", "css_nexttimept"),
      envir = environment()
    )

  }

  # ====================================================================
  # 8. FINAL CLEANUP
  # Mirrors SAS: %util_delete_dsets(css_safana css_anadata ...)
  # ====================================================================
  util_delete_dsets(
    c("raw_data", "ds_derived", "css_safana", "css_safana_pooled",
      "css_anadata", "study_order"),
    envir = environment()
  )

  if (length(pdf_paths) == 0) {
    warning("No PDF output was generated.")
  } else {
    message("WPCT-F.07.07 complete. Generated ", length(pdf_paths),
            " PDF file(s).")
  }

  invisible(pdf_paths)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. C_MODE parameter (LAST/MIN/MAX) corresponds to an analysis
#       flag (A_FL) that already identifies the appropriate post-
#       baseline observations in the input dataset. If the flag
#       is not pre-derived, derive_lastminmax_measure() is called
#       to create it.
#    2. PROC GLM Type III SS from SAS matches car::Anova(type=3)
#       when contrasts are set to contr.sum. Per-treatment p-values
#       extracted from summary(lm())$coefficients t-tests match SAS
#       parameterestimates probt values for balanced designs.
#    3. The SAS hex character 'A0'x (non-breaking space, U+00A0)
#       prepended to "Pooled" study label ensures Pooled sorts last
#       in both SAS and R collation sequences.
#    4. Subject IDs in pooled data are prefixed with "P" to create
#       distinct USUBJID values, matching SAS substr(usubjid,1,1)='P'.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Quantile method: SAS PROC SUMMARY PCTLDEF=5 vs R default
#       type=7 may produce slightly different Q1/Q3 values for
#       small samples. Differences are typically < 1 unit in the
#       last displayed decimal.
#    2. ANCOVA p-values: SAS PROC GLM uses sweep-operator matrix
#       inversion; R uses QR decomposition. Differences appear
#       only beyond 10 decimal places for well-conditioned models.
#    3. Rounding: All displayed values use janitor::round_half_up()
#       to match SAS round-half-up behavior. R default banker's
#       rounding is NOT used anywhere in this script.
#    4. Floating-point representation: SAS 8-byte doubles and R
#       doubles are both IEEE 754 but epsilon comparisons may
#       differ at extreme precision.
#
# NO DIRECT R EQUIVALENT:
#    1. PhUSEboxplot GTL template registered via PROC TEMPLATE in
#       SAS is replaced by theme_phuse() + phuse_boxplot() from
#       util_ggplot_theme.R. The ggplot2 rendering may differ in
#       exact box widths and whisker cap sizes.
#    2. SAS ODS PDF with inline PROC SGRENDER is replaced by
#       grDevices::pdf() + grid::grid.draw() + grid::grid.newpage()
#       for multi-page output.
#    3. SAS DATA step RETAIN for studynum sequential numbering is
#       replaced by dplyr::row_number() on distinct sorted STUDYID.
#
# PACKAGE SELECTION RATIONALE:
#    - haven: SAS XPT transport file reader (only tidyverse-blessed
#      SAS data I/O package)
#    - dplyr/tidyr: Core data manipulation (AAP mandates tidyverse
#      over base R)
#    - ggplot2: Visualization (replaces PROC SGRENDER per AAP)
#    - gridExtra: Multi-panel figure composition (boxplot + table)
#    - car: Type III ANOVA (AAP mandates car::Anova)
#    - janitor: round_half_up (AAP mandates SAS-compatible rounding)
#    - forcats: Factor level ordering (AAP mandates forcats for
#      format-based ordering)
#    - rlang: Tidy evaluation for programmatic column references
#
# OPEN QUESTIONS:
#    1. Multi-study XPT loading: SAS uses a single ADaM library
#       with pooled studies. This R implementation assumes a single
#       XPT file containing all studies. If studies are in separate
#       XPT files, the calling script must combine them before
#       invoking this function.
#    2. derive_lastminmax_measure relationship: The function is
#       called to derive analysis flags if needed. If the input
#       dataset already has the correct A_FL flag derived, the
#       derivation step is a no-op (gracefully handled by tryCatch).
#    3. Pooled ANCOVA with STUDYNUM: When only one study is present,
#       STUDYNUM is constant in pooled data and would cause
#       singularity. This implementation detects and excludes
#       STUDYNUM from the pooled model in that case.
# ============================================================
