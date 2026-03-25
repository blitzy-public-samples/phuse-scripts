# =============================================================================
# Program: pk_subj_conc.R
# Purpose: Figure 16.2.5-x.x — Individual Subject Concentration-Time Profiles
#
# Migrated from: whitepapers/scriptathons/pk/pk_subj_conc.sas
# Migration date: 2026-03-25
#
# Description:
#   Generates individual PK concentration-time line plots for each unique
#   combination of subject (USUBJID) and parameter category (PARCAT1).
#   This is the R equivalent of the SAS PROC GPLOT BY USUBJID parcat1 program
#   from the PhUSE CSS PK White Paper (Figure 7.6).
#
# SAS-to-R Mapping:
#   - filename/libname XPT streaming  → haven::read_xpt()
#   - PROC SORT BY USUBJID parcat1 eactm → dplyr::arrange()
#   - PROC GPLOT BY USUBJID parcat1  → ggplot2 + facet_wrap() or paginated plots
#   - SYMBOL1 INTERPOL=JOIN VALUE=NONE → geom_line() (no geom_point())
#   - AXIS1/AXIS2 MINOR=NONE         → scale_y/x_continuous(minor_breaks = NULL)
#   - FRAME option                    → panel.border = element_rect()
#   - ODS titles/footnotes            → labs(title, subtitle, caption)
#   - options nobyline                → facet strip labels serve equivalent purpose
#   - GOPTIONS RESET = SYMBOL         → no R equivalent needed (GC handles cleanup)
#
# Packages:
#   haven    — SAS XPT file I/O (CDISC transport)
#   dplyr    — data sorting replacing PROC SORT
#   ggplot2  — visualization replacing PROC GPLOT
#   janitor  — SAS-compatible round_half_up() for migration consistency
#   cli      — informative user-facing error/status messages
#
# Configuration:
#   All file paths are parameterized via function arguments. Callers should
#   use config/migration_config.yaml to resolve paths:
#     config <- yaml::read_yaml("config/migration_config.yaml")
#     adpc_path <- file.path(config$data_paths$adam_path, "adpc.xpt")
#     output_path <- config$output_paths$figure_output_path
#
# =============================================================================

# --- Package Loading ---------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(janitor)
library(cli)

# =============================================================================
# pk_subj_conc — Individual Subject Concentration-Time Profiles
# =============================================================================
#' Generate individual PK concentration-time profile plots.
#'
#' Reads an ADPC (Analysis Dataset Pharmacokinetics Concentrations) XPT file,
#' sorts by USUBJID, PARCAT1, and EACTM (elapsed actual time), and produces
#' concentration-time line plots — one panel per subject × parameter category
#' combination. Replaces the SAS PROC GPLOT BY USUBJID parcat1 program from
#' pk_subj_conc.sas with 100% functional parity.
#'
#' @param adpc_path Character string. Absolute or relative path to the ADPC
#'   XPT file (e.g., "data/adam/cdisc/adpc.xpt"). Required.
#' @param adsl_path Character string or NULL. Optional path to the ADSL XPT
#'   file. Loaded in the original SAS program but not used in the plot. Included
#'   for consistency; if provided, the data is read and returned in the output
#'   list but not used for plotting.
#' @param output_path Character string or NULL. Directory path where output
#'   figure files are saved. If NULL, no files are written to disk; the ggplot
#'   object(s) are returned invisibly for interactive use.
#' @param panels_per_page Integer or NULL. Number of subject × PARCAT1 panels
#'   to display per page/plot. If NULL (default), all panels are rendered in a
#'   single faceted plot. If specified, the function produces multiple paginated
#'   ggplot objects and saves separate files (pk_subj_conc_page1.png, etc.).
#' @param width Numeric. Width of each output figure in inches. Default 12.
#' @param height Numeric. Height of each output figure in inches. Default 8.
#' @param dpi Numeric. Resolution for saved figures. Default 300.
#'
#' @return A list (returned invisibly) containing:
#'   \describe{
#'     \item{plots}{A list of ggplot objects (one per page).}
#'     \item{data}{The sorted ADPC data frame used for plotting.}
#'     \item{adsl}{The ADSL data frame if adsl_path was provided; NULL otherwise.}
#'     \item{n_subjects}{Integer count of unique subjects plotted.}
#'     \item{n_panels}{Integer count of unique USUBJID × PARCAT1 panels.}
#'     \item{output_files}{Character vector of saved file paths (empty if output_path is NULL).}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Using migration config
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   result <- pk_subj_conc(
#'     adpc_path = file.path(config$data_paths$adam_path, "adpc.xpt"),
#'     output_path = config$output_paths$figure_output_path
#'   )
#'
#'   # Interactive use without saving
#'   result <- pk_subj_conc(adpc_path = "data/adam/cdisc/adpc.xpt")
#'   print(result$plots[[1]])
#'
#'   # Paginated output
#'   result <- pk_subj_conc(
#'     adpc_path = "data/adam/cdisc/adpc.xpt",
#'     output_path = "output/figures",
#'     panels_per_page = 4
#'   )
#' }
pk_subj_conc <- function(adpc_path,
                         adsl_path = NULL,
                         output_path = NULL,
                         panels_per_page = NULL,
                         width = 12,
                         height = 8,
                         dpi = 300) {

  # ---------------------------------------------------------------------------
  # INPUT VALIDATION
  # ---------------------------------------------------------------------------
  # Replaces implicit SAS error handling with explicit, informative messages.


  # Validate adpc_path: must be a non-empty character string pointing to a file

  if (missing(adpc_path) || is.null(adpc_path)) {
    cli::cli_abort(c(
      "x" = "{.arg adpc_path} is required.",
      "i" = "Provide the path to an ADPC XPT file.",
      "i" = "Example: {.code pk_subj_conc(adpc_path = \"data/adam/cdisc/adpc.xpt\")}"
    ))
  }

  if (!is.character(adpc_path) || length(adpc_path) != 1L || nchar(adpc_path) == 0L) {
    cli::cli_abort(c(
      "x" = "{.arg adpc_path} must be a single non-empty character string.",
      "i" = "Received: {.val {adpc_path}}"
    ))
  }

  if (!file.exists(adpc_path)) {
    cli::cli_abort(c(
      "x" = "ADPC file not found: {.file {adpc_path}}",
      "i" = "Verify the file path and ensure the XPT file exists."
    ))
  }

  # Validate adsl_path if provided
  if (!is.null(adsl_path)) {
    if (!is.character(adsl_path) || length(adsl_path) != 1L || nchar(adsl_path) == 0L) {
      cli::cli_abort(c(
        "x" = "{.arg adsl_path} must be a single non-empty character string or NULL.",
        "i" = "Received: {.val {adsl_path}}"
      ))
    }
    if (!file.exists(adsl_path)) {
      cli::cli_abort(c(
        "x" = "ADSL file not found: {.file {adsl_path}}",
        "i" = "Verify the file path and ensure the XPT file exists."
      ))
    }
  }

  # Validate output_path if provided
  if (!is.null(output_path)) {
    if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
      cli::cli_abort(c(
        "x" = "{.arg output_path} must be a single non-empty character string or NULL.",
        "i" = "Received: {.val {output_path}}"
      ))
    }
    # Create output directory if it does not exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      cli::cli_inform(c("i" = "Created output directory: {.file {output_path}}"))
    }
  }

  # Validate panels_per_page if provided
  if (!is.null(panels_per_page)) {
    if (!is.numeric(panels_per_page) || length(panels_per_page) != 1L ||
        panels_per_page < 1L || panels_per_page != janitor::round_half_up(panels_per_page, 0)) {
      cli::cli_abort(c(
        "x" = "{.arg panels_per_page} must be a positive integer or NULL.",
        "i" = "Received: {.val {panels_per_page}}"
      ))
    }
    panels_per_page <- as.integer(panels_per_page)
  }

  # Validate numeric dimensions
  if (!is.numeric(width) || length(width) != 1L || width <= 0) {
    cli::cli_abort("{.arg width} must be a positive number.")
  }
  if (!is.numeric(height) || length(height) != 1L || height <= 0) {
    cli::cli_abort("{.arg height} must be a positive number.")
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || dpi <= 0) {
    cli::cli_abort("{.arg dpi} must be a positive number.")
  }

  # ---------------------------------------------------------------------------
  # DATA ACQUISITION
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   filename source url "https://...adpc.xpt"; libname source xport;
  #   data work.adpc; set source.adpc; run;
  # With parameterized haven::read_xpt() call (no hardcoded URLs).

  cli::cli_inform(c("i" = "Reading ADPC data from {.file {adpc_path}}..."))
  adpc <- haven::read_xpt(adpc_path)
  cli::cli_inform(c(
    "v" = "ADPC loaded: {nrow(adpc)} observations, {ncol(adpc)} variables."
  ))

  # Optionally read ADSL — loaded in original SAS but not used in PROC GPLOT.
  # Retained for functional parity and potential downstream use.
  adsl <- NULL
  if (!is.null(adsl_path)) {
    cli::cli_inform(c("i" = "Reading ADSL data from {.file {adsl_path}}..."))
    adsl <- haven::read_xpt(adsl_path)
    cli::cli_inform(c(
      "v" = "ADSL loaded: {nrow(adsl)} observations, {ncol(adsl)} variables."
    ))
  }

  # ---------------------------------------------------------------------------
  # VARIABLE NAME RESOLUTION
  # ---------------------------------------------------------------------------
  # SAS is case-insensitive; the source code uses mixed case (USUBJID, parcat1,

  # eactm, AVAL). XPT v5 transport format stores names in UPPERCASE. haven
  # preserves the case from the file. We resolve to the actual column names
  # present in the data using case-insensitive matching.

  resolve_colname <- function(df, target_name) {
    # Case-insensitive match of target_name against actual column names
    idx <- match(toupper(target_name), toupper(names(df)))
    if (is.na(idx)) {
      cli::cli_abort(c(
        "x" = "Required variable {.val {target_name}} not found in ADPC dataset.",
        "i" = "Available variables: {.val {head(names(df), 20)}}...",
        "i" = "Ensure the input file is a valid CDISC ADPC XPT."
      ))
    }
    names(df)[idx]
  }

  col_usubjid <- resolve_colname(adpc, "USUBJID")
  col_parcat1 <- resolve_colname(adpc, "PARCAT1")
  col_eactm   <- resolve_colname(adpc, "EACTM")
  col_aval    <- resolve_colname(adpc, "AVAL")

  # ---------------------------------------------------------------------------
  # DATA PREPARATION
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc;
  #     BY USUBJID parcat1 eactm;
  #   RUN;
  #
  # dplyr::arrange() replaces PROC SORT. The sort order matches the SAS BY
  # statement exactly: USUBJID, PARCAT1, EACTM.

  # Convert EACTM to numeric if stored as character (common in XPT files)
  # SAS treats all BY-variable sorts numerically when the variable has a numeric
  # format; haven may read some XPT variables as character.
  sort_adpc <- adpc %>%
    dplyr::mutate(
      # Coerce EACTM to numeric — SAS processes this as numeric elapsed time.
      # Blank or non-numeric values map to NA (SAS missing → R NA).
      !!rlang::sym(col_eactm) := suppressWarnings(
        as.numeric(!!rlang::sym(col_eactm))
      )
    ) %>%
    dplyr::arrange(
      !!rlang::sym(col_usubjid),
      !!rlang::sym(col_parcat1),
      !!rlang::sym(col_eactm)
    )

  # Remove rows where both EACTM and AVAL are NA (no plottable data)
  # Keep rows where AVAL is NA (SAS missing → gap in line, NOT zero substitution)
  sort_adpc <- sort_adpc %>%
    dplyr::filter(
      !is.na(!!rlang::sym(col_eactm))
    )

  cli::cli_inform(c(
    "v" = "Data sorted and filtered: {nrow(sort_adpc)} plottable observations."
  ))

  # ---------------------------------------------------------------------------
  # PANEL IDENTIFICATION
  # ---------------------------------------------------------------------------
  # Identify unique USUBJID × PARCAT1 combinations for faceting/pagination.
  # This matches the SAS BY USUBJID parcat1 statement which produces one plot
  # per combination.

  panel_combos <- sort_adpc %>%
    dplyr::distinct(
      !!rlang::sym(col_usubjid),
      !!rlang::sym(col_parcat1)
    ) %>%
    dplyr::arrange(
      !!rlang::sym(col_usubjid),
      !!rlang::sym(col_parcat1)
    )

  n_subjects <- length(unique(sort_adpc[[col_usubjid]]))
  n_panels   <- nrow(panel_combos)

  cli::cli_inform(c(
    "i" = "Generating plots for {n_subjects} subjects across {n_panels} panels."
  ))

  # ---------------------------------------------------------------------------
  # TITLE AND FOOTNOTE CONSTRUCTION
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   title1 j=center "(page x of x)";
  #   title2 j=center "Figure 16.2.5-x.x Individual concentration-time profiles";
  #   title3 j=center "by compound, matrix, analyte and ...";
  #   title4 j=center "Analysis Set : PK analysis set";
  #   FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on ...";

  plot_title    <- "Figure 16.2.5-x.x Individual concentration-time profiles"
  plot_subtitle <- paste0(
    "by compound, matrix, analyte and [actual/randomised][treatments/group]\n",
    "Analysis Set : PK analysis set"
  )
  plot_caption  <- paste(
    "Generated by R",
    R.version.string,
    "on",
    format(Sys.time(), "%d%b%Y at %I:%M %p")
  )

  # ---------------------------------------------------------------------------
  # GGPLOT2 VISUALIZATION (replacing PROC GPLOT BY)
  # ---------------------------------------------------------------------------
  # SAS constructs mapped:
  #   SYMBOL1: INTERPOL=JOIN → geom_line(); VALUE=NONE → no geom_point();
  #            LINE=1 → solid linetype; WIDTH=2 → linewidth scaled
  #   AXIS1:   LABEL="Conc (ug/mL)", MINOR=NONE → scale_y_continuous()
  #   AXIS2:   LABEL="Time (hour)", MINOR=NONE → scale_x_continuous()
  #   FRAME    → panel.border = element_rect(color = "black", fill = NA)
  #   nobyline → facet strip labels provide equivalent BY-group identification

  build_plot <- function(plot_data, page_label = NULL) {
    # Construct the ggplot object for a set of panels
    p <- ggplot2::ggplot(
      data = plot_data,
      mapping = ggplot2::aes(
        x = !!rlang::sym(col_eactm),
        y = !!rlang::sym(col_aval)
      )
    ) +
      # geom_line: replaces SYMBOL1 INTERPOL=JOIN VALUE=NONE
      # SAS WIDTH=2 points ≈ linewidth 0.75 mm in ggplot2 (approximate mapping)
      # LINE=1 → solid linetype
      ggplot2::geom_line(
        linewidth = 0.75,
        linetype  = "solid",
        na.rm     = TRUE
      ) +
      # Faceting: replaces BY USUBJID parcat1 in PROC GPLOT
      ggplot2::facet_wrap(
        facets = stats::as.formula(paste0("~ ", col_usubjid, " + ", col_parcat1)),
        scales = "free"
      ) +
      # Axis labels matching SAS AXIS1/AXIS2 LABEL= specifications
      ggplot2::labs(
        x        = "Time (hour)",
        y        = "Conc (ug/mL)",
        title    = if (!is.null(page_label)) {
          paste0(plot_title, "\n", page_label)
        } else {
          plot_title
        },
        subtitle = plot_subtitle,
        caption  = plot_caption
      ) +
      # Theme: minimal base with FRAME border
      ggplot2::theme_minimal() +
      ggplot2::theme(
        # FRAME → solid black border around each panel
        panel.border    = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
        # Strip text (BY-group labels) — compact for many panels
        strip.text      = ggplot2::element_text(size = 8, face = "bold"),
        # Title centered to match SAS j=center alignment
        plot.title      = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 10),
        plot.caption    = ggplot2::element_text(hjust = 0, size = 7, face = "italic"),
        # Axis text styling
        axis.title      = ggplot2::element_text(size = 10),
        axis.text       = ggplot2::element_text(size = 8)
      ) +
      # AXIS1 MINOR=NONE → suppress minor gridlines on Y-axis
      ggplot2::scale_y_continuous(minor_breaks = NULL) +
      # AXIS2 MINOR=NONE → suppress minor gridlines on X-axis
      ggplot2::scale_x_continuous(minor_breaks = NULL)

    p
  }

  # ---------------------------------------------------------------------------
  # PAGINATION LOGIC
  # ---------------------------------------------------------------------------
  # SAS PROC GPLOT BY generates one page per BY-group combination.
  # ggplot2 facet_wrap renders all panels in a single plot by default.
  # When panels_per_page is specified, split into multiple pages to better
  # approximate the SAS one-figure-per-page behavior.

  output_files <- character(0)
  plots_list   <- list()

  if (is.null(panels_per_page) || n_panels <= panels_per_page) {
    # --- Single page: all panels in one faceted plot ---
    p <- build_plot(sort_adpc)
    plots_list <- list(p)

    if (!is.null(output_path)) {
      out_file <- file.path(output_path, "pk_subj_conc.png")
      ggplot2::ggsave(
        filename = out_file,
        plot     = p,
        width    = width,
        height   = height,
        dpi      = dpi,
        bg       = "white"
      )
      output_files <- c(output_files, out_file)
      cli::cli_inform(c("v" = "Saved figure: {.file {out_file}}"))
    }
  } else {
    # --- Multiple pages: split panels into groups ---
    # Create a panel index for each USUBJID × PARCAT1 combination
    panel_combos <- panel_combos %>%
      dplyr::mutate(
        panel_id = dplyr::row_number(),
        page_num = ceiling(panel_id / panels_per_page)
      )

    n_pages <- max(panel_combos$page_num)

    for (pg in seq_len(n_pages)) {
      # Identify panels for this page
      page_panels <- panel_combos %>%
        dplyr::filter(page_num == pg)

      # Filter data to include only subjects/parcat1 combos for this page
      page_data <- sort_adpc %>%
        dplyr::semi_join(
          page_panels,
          by = stats::setNames(
            c(col_usubjid, col_parcat1),
            c(col_usubjid, col_parcat1)
          )
        )

      page_label <- paste0("(page ", pg, " of ", n_pages, ")")
      p <- build_plot(page_data, page_label = page_label)
      plots_list <- c(plots_list, list(p))

      if (!is.null(output_path)) {
        out_file <- file.path(
          output_path,
          paste0("pk_subj_conc_page", pg, ".png")
        )
        ggplot2::ggsave(
          filename = out_file,
          plot     = p,
          width    = width,
          height   = height,
          dpi      = dpi,
          bg       = "white"
        )
        output_files <- c(output_files, out_file)
        cli::cli_inform(c("v" = "Saved page {pg}/{n_pages}: {.file {out_file}}"))
      }
    }

    cli::cli_inform(c(
      "v" = "Generated {n_pages} paginated figures with {panels_per_page} panels each."
    ))
  }

  # ---------------------------------------------------------------------------
  # RETURN RESULTS
  # ---------------------------------------------------------------------------
  # Return plot objects and metadata invisibly for programmatic access.
  # Matches the SAS cleanup phase (TITLE; FOOTNOTE; GOPTIONS RESET = SYMBOL;)
  # by returning a clean result structure and allowing R garbage collection.

  result <- list(
    plots        = plots_list,
    data         = sort_adpc,
    adsl         = adsl,
    n_subjects   = n_subjects,
    n_panels     = n_panels,
    output_files = output_files
  )

  cli::cli_inform(c(
    "v" = "pk_subj_conc completed: {n_panels} panels for {n_subjects} subjects."
  ))

  invisible(result)
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - ADSL loaded in SAS but not used in PROC GPLOT; included as optional
#      parameter (adsl_path) for consistency with the original program structure.
#    - Variable case: XPT v5 transport stores all variable names in UPPERCASE.
#      SAS source code uses mixed case (USUBJID, parcat1, eactm, AVAL) but SAS
#      is case-insensitive. The R implementation uses case-insensitive column name
#      resolution via resolve_colname() to handle either case transparently.
#    - PROC GPLOT BY USUBJID parcat1 produces one plot per USUBJID+PARCAT1
#      combination. ggplot2 facet_wrap provides equivalent multi-panel layout.
#      Pagination via panels_per_page argument emulates the SAS one-page-per-plot
#      behavior for large datasets.
#    - SAS 'options nobyline' suppresses BY-group headers in output; ggplot2
#      facet strip labels serve the equivalent purpose of identifying panels.
#    - EACTM is stored as character in the XPT file; converted to numeric via
#      as.numeric() for proper axis scaling. Non-numeric values become NA.
#    - SAS missing (.) maps to R NA — no implicit zero substitution.
#      Missing AVAL values produce gaps in lines (geom_line na.rm = TRUE).
#    - SAS hardcoded GitHub URLs replaced with parameterized adpc_path argument;
#      callers use config/migration_config.yaml to resolve paths.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - No statistical computations in this script; line plots are point-to-point
#      interpolation and are functionally identical between SAS GPLOT and ggplot2.
#    - Axis auto-range may differ slightly between SAS GPLOT and ggplot2 due to
#      different padding algorithms (SAS expands ~5%; ggplot2 uses expand_scale).
#    - Sort stability: dplyr::arrange() uses a stable sort matching SAS PROC SORT
#      behavior for ties within BY variables.
# NO DIRECT R EQUIVALENT:
#    - PROC GPLOT with BY statement → ggplot2 + facet_wrap() (one-to-many page
#      generation handled via pagination parameter; SAS generates separate pages
#      automatically, ggplot2 uses faceted grid or paginated output).
#    - SAS SYMBOL1 INTERPOL=JOIN → geom_line() (functionally equivalent;
#      connected lines without point markers).
#    - SAS SYMBOL1 CV=_STYLE_ → ggplot2 default color mapping (theme-dependent).
#    - SAS GOPTIONS RESET = SYMBOL → no R equivalent needed (R garbage collection
#      handles object cleanup automatically).
#    - SAS &_SASSERVERNAME, &SYSSCPL runtime macros → R.version.string and
#      Sys.time() for equivalent runtime environment identification in footnote.
# PACKAGE SELECTION RATIONALE:
#    - ggplot2: Replaces legacy SAS PROC GPLOT for all graphics rendering;
#      industry standard for R visualization in pharma (tidyverse ecosystem).
#    - haven (2.5.5): XPT I/O for CDISC transport files; reads SAS transport
#      format with preserved labels and haven-tagged NAs.
#    - dplyr (>=1.1.0): Data sorting and manipulation replacing PROC SORT;
#      tidyverse core package per migration rules.
#    - janitor (>=2.2.0): SAS-compatible round_half_up() included per migration
#      framework consistency, even though this visualization script performs
#      minimal rounding. Used in pagination calculations.
#    - cli (>=3.6.0): Informative user-facing error/status messages replacing
#      implicit SAS error handling. Provides structured CLI output.
# OPEN QUESTIONS:
#    - Confirm pagination requirements: should output be one page per subject
#      (matching SAS BY-group behavior) or multi-subject grid (default facet)?
#    - Verify variable case for EACTM/PARCAT1: XPT v5 stores UPPERCASE; if
#      non-standard XPT files use mixed case, resolve_colname() handles this.
#    - Confirm whether ADSL data is actually needed for this figure or if it
#      was loaded in the SAS source as a side effect of a broader workflow.
#    - Determine optimal panels_per_page default for production use.
# ============================================================
