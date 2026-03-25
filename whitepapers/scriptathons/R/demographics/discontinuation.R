# =============================================================================
# PROGRAM:     discontinuation.R
# DESCRIPTION: Listing 7.1 — Subjects who Discontinue (Target 32)
#              Generates a grouped listing of subjects who discontinued,
#              organized by treatment arm, showing discontinuation reason code
#              and textual reason description.
#
# SOURCE SAS:  whitepapers/scriptathons/demographics/discontinuation.sas
#              (London 2014 PhUSE Script-athon, Target 32)
#
# MIGRATION:   SAS-to-R migration per PhUSE CS WG5 standard.
#              - SAS %STDREP macro → R parameterized function
#              - SAS PROC SQL / PROC REPORT → dplyr pipeline + r2rtf output
#              - SAS filename URL / libname xport → haven::read_xpt()
#              - SAS options missing='' → NA displayed as blank in RTF
#              - SAS format/formchar → not needed in R
#
# DATASETS:    Input:  ADSL (ADaM Subject-Level Analysis Dataset)
#              Output: RTF listing grouped by treatment arm
#
# VARIABLES:   USUBJID  — Unique Subject Identifier
#              TRTAN    — Treatment Arm (Numeric) — grouping variable
#              DSREASCD — Standardized Disposition Reason Code
#              DSTERM   — Reported Disposition Term (free text)
#              ITTFL    — Intent-to-Treat Population Flag (for filtering)
#
# REPLACES:    SAS filename source url "https://...adsl.xpt";
#              SAS libname source xport;
#              SAS %macro stdrep (lines 46-109)
#              SAS proc sql (lines 72-83)
#              SAS proc report (lines 91-105)
#
# NOTE:        The original SAS source has a syntax error on line 107
#              (%endmac: should be %mend stdrep;). The R version corrects
#              this by implementing the intended report logic as a clean
#              parameterized function.
#
# CONFIG:      Paths are parameterized via function arguments. Callers
#              can load config/migration_config.yaml to obtain:
#                config$data_paths$adam_path   → data_path argument
#                config$output_paths$rtf_output_path → output_path argument
#
# =============================================================================

# -----------------------------------------------------------------------------
# Library Loading
# -----------------------------------------------------------------------------
library(haven)
library(dplyr)
library(rlang)
library(r2rtf)
library(stringr)
library(janitor)
library(cli)

# =============================================================================
# generate_discontinuation_listing
# =============================================================================
#' Generate Discontinuation Listing (Target 32)
#'
#' Produces "Listing 7.1 — Subjects who Discontinue" showing subjects grouped
#' by treatment arm with discontinuation reason code and textual description.
#' This is the R migration of the SAS \code{%STDREP} macro from
#' \code{discontinuation.sas} (London 2014 PhUSE Script-athon, Target 32).
#'
#' The function reads ADSL transport data, optionally filters to the
#' intent-to-treat (ITT) population, selects the required listing variables,
#' sorts by treatment arm and subject, and generates an RTF listing with
#' treatment group headers using the r2rtf package.
#'
#' @param data_path Character string. Path to the directory containing
#'   \code{adsl.xpt}. Replaces SAS \code{filename source url ...} and
#'   \code{libname source xport} (SAS lines 27-28). No trailing slash required.
#' @param output_path Character string. Full file path for the output RTF file.
#'   Defaults to \code{"discontinuation_listing.rtf"} in the current working
#'   directory. Replaces SAS ODS RTF destination.
#' @param pop_flag Character string. Set to \code{"Y"} to filter to the ITT
#'   population (\code{ITTFL == "Y"}); any other value includes all subjects.
#'   Replaces SAS \code{\%let _it = Y;} (line 55).
#' @param pop_type Character string. Population label used in the listing
#'   title (e.g., \code{"Randomized"} or \code{"Enrolled"}). Replaces SAS
#'   \code{\%let _pp = Randomized;} (lines 56-58).
#'
#' @return Invisible character string containing the output file path.
#'
#' @details
#' \strong{SAS-to-R Mapping:}
#' \describe{
#'   \item{SAS \code{\%let _dd=adsl;}}{Replaced by \code{data_path} parameter}
#'   \item{SAS \code{\%let _it = Y;}}{Replaced by \code{pop_flag} parameter}
#'   \item{SAS \code{\%let _pp = Randomized;}}{Replaced by \code{pop_type} parameter}
#'   \item{SAS \code{proc sql; create table _rp ...}}{Replaced by
#'     \code{dplyr::select()} and \code{dplyr::arrange()}}
#'   \item{SAS \code{proc report ... by TRTAN}}{Replaced by
#'     \code{r2rtf::rtf_body()} with \code{group_by}}
#'   \item{SAS \code{compute before TRTAN}}{Replaced by treatment label
#'     column used as \code{subline_by} in r2rtf}
#'   \item{SAS \code{options missing=''}}{Replaced by explicit
#'     \code{NA} to empty string conversion before RTF rendering}
#' }
#'
#' @examples
#' \dontrun{
#' # Using config/migration_config.yaml paths:
#' config <- yaml::read_yaml("config/migration_config.yaml")
#' generate_discontinuation_listing(
#'   data_path    = config$data_paths$adam_path,
#'   output_path  = file.path(config$output_paths$rtf_output_path,
#'                            "discontinuation_listing.rtf"),
#'   pop_flag     = "Y",
#'   pop_type     = "Randomized"
#' )
#'
#' # Direct usage with explicit paths:
#' generate_discontinuation_listing(
#'   data_path   = "data/adam/cdisc",
#'   output_path = "output/rtf/discontinuation_listing.rtf"
#' )
#' }
#'
#' @export
generate_discontinuation_listing <- function(data_path,
                                              output_path = "discontinuation_listing.rtf",
                                              pop_flag = "Y",
                                              pop_type = "Randomized") {

  # ---------------------------------------------------------------------------
  # Input Validation — Type checks first, filesystem checks second
  # ---------------------------------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L || nchar(data_path) == 0L) {
    cli::cli_abort("'data_path' must be a non-empty single character string.", call. = FALSE)
  }

  if (!is.character(output_path) || length(output_path) != 1L || nchar(output_path) == 0L) {
    cli::cli_abort("'output_path' must be a non-empty single character string.", call. = FALSE)
  }

  if (!is.character(pop_flag) || length(pop_flag) != 1L) {
    cli::cli_abort("'pop_flag' must be a single character string ('Y' or other).", call. = FALSE)
  }

  if (!is.character(pop_type) || length(pop_type) != 1L || nchar(pop_type) == 0L) {
    cli::cli_abort("'pop_type' must be a non-empty single character string.", call. = FALSE)
  }

  # Filesystem validation — after all type checks pass
  adsl_file <- file.path(data_path, "adsl.xpt")
  if (!file.exists(adsl_file)) {
    cli::cli_abort(
      paste0("ADSL transport file not found at expected location: ", adsl_file,
             "\nEnsure data_path points to a directory containing adsl.xpt."),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Phase 1: Data Reading
  # ---------------------------------------------------------------------------
  # Replaces SAS lines 27-28:
  #   filename source url "https://raw.githubusercontent.com/.../adsl.xpt";
  #   libname source xport;
  adsl <- haven::read_xpt(adsl_file)

  # Validate that all required variables are present in the ADSL dataset
  required_vars <- c("USUBJID", "TRTAN", "DSREASCD", "DSTERM")
  missing_vars <- setdiff(required_vars, names(adsl))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      paste0("Required variable(s) not found in ADSL: ",
             paste(missing_vars, collapse = ", "),
             "\nAvailable variables: ",
             paste(head(names(adsl), 20L), collapse = ", "),
             if (length(names(adsl)) > 20L) ", ..." else ""),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Phase 2: Population Selection
  # ---------------------------------------------------------------------------
  # Replaces SAS lines 55-58 and 80:
  #   %let _it = Y;
  #   %if "%upcase(&_it)"="Y" %then %do; where ITTFL='Y'; %end;
  #   %if ... %then %let _pp = Randomized; %else %let _pp = Enrolled;
  if (toupper(pop_flag) == "Y") {
    if (!"ITTFL" %in% names(adsl)) {
      cli::cli_abort(
        "Population flag 'ITTFL' not found in ADSL but pop_flag='Y' requires it. ",
        "Set pop_flag to a value other than 'Y' to skip ITT filtering, ",
        "or ensure ITTFL is present in the ADSL dataset.",
        call. = FALSE
      )
    }
    adsl <- dplyr::filter(adsl, .data$ITTFL == "Y")
  }

  # ---------------------------------------------------------------------------
  # Phase 3: Data Preparation (replacing SAS PROC SQL, lines 72-83)
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   proc sql;
  #     create table _rp as
  #     select USUBJID, TRTAN, DSREASCD, DSTERM, ITTFL
  #     from source.adsl
  #     [where ITTFL='Y'];
  #   quit;
  report_data <- adsl %>%
    dplyr::select(USUBJID, TRTAN, DSREASCD, DSTERM) %>%
    dplyr::arrange(TRTAN, USUBJID)

  # Apply SAS-compatible rounding to TRTAN to ensure consistent integer display.
  # Uses janitor::round_half_up() per AAP §0.7.2 mandate for SAS numeric

  # equivalence — SAS rounds half-up, R rounds half-to-even by default.
  report_data <- report_data %>%
    dplyr::mutate(
      TRTAN = janitor::round_half_up(as.numeric(TRTAN), digits = 0)
    )

  # Check for empty dataset after filtering
  if (nrow(report_data) == 0L) {
    warning(
      "No records remain after population filtering (pop_flag='",
      pop_flag, "'). The RTF listing will be generated with no data rows.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Phase 4: Title Construction (replacing SAS lines 64-66)
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   %let _tt1 = %str(Listing 7.1  &_pp Subjects who Discontinue
  #                     due to Physician Decision,);
  #   %let _tt2 = %str(Withdrawal by Subject, Withdrawal by
  #                     Parent/Guardian, or Other);
  #   %let _tt3 = %str(Study: London 2014 Script-athon);
  tt1 <- stringr::str_squish(
    paste0("Listing 7.1  ", pop_type,
           " Subjects who Discontinue due to Physician Decision,")
  )
  tt2 <- "Withdrawal by Subject, Withdrawal by Parent/Guardian, or Other"
  tt3 <- stringr::str_trim("Study: London 2014 Script-athon")

  # ---------------------------------------------------------------------------
  # Phase 5: Construct Display Data for RTF
  # ---------------------------------------------------------------------------
  # Replaces SAS PROC REPORT COMPUTE BEFORE TRTAN:
  #   compute before TRTAN;
  #     put 'Treatment: #TRTAN#';
  #     put line;
  #   endcomp;
  #
  # Create a treatment group label column for r2rtf subline_by grouping.
  # The treatment label is constructed dynamically from the numeric TRTAN value.
  display_data <- report_data %>%
    dplyr::mutate(
      treatment_label = dplyr::if_else(
        is.na(TRTAN),
        "Treatment: Unknown",
        stringr::str_squish(paste0("Treatment: ", as.character(as.integer(TRTAN))))
      )
    ) %>%
    dplyr::select(treatment_label, USUBJID, DSREASCD, DSTERM)

  # Replace NA with empty string for RTF display
  # Replaces SAS: options missing='';
  # SAS character missing (' ') → displayed as blank in listing.
  # R NA → empty string for display purposes only; data retains NA semantics.
  display_data <- display_data %>%
    dplyr::mutate(
      USUBJID  = dplyr::if_else(is.na(USUBJID),  "", as.character(USUBJID)),
      DSREASCD = dplyr::if_else(is.na(DSREASCD), "", as.character(DSREASCD)),
      DSTERM   = dplyr::if_else(is.na(DSTERM),   "", as.character(DSTERM))
    )

  # ---------------------------------------------------------------------------
  # Phase 6: RTF Output Generation (replacing SAS PROC REPORT, lines 91-105)
  # ---------------------------------------------------------------------------
  # Replaces SAS:
  #   proc report data=_rp nowd headline headskip missing
  #              split='?' spacing=2 list;
  #     column TRTAN USUBJID DSREASCD DSTERM;
  #     by TRTAN;
  #     define TRTAN   /order order=internal noprint;
  #     define USUBJID /display width=15 'Subject?ID';
  #     define DSREASCD/display width=30 'Reason for?Discontinuation';
  #     define DSTERM  /display width=30 'Textual Reason';
  #     compute before TRTAN;
  #       put 'Treatment: #TRTAN#';
  #       put line;
  #     endcomp;
  #   run;
  #
  # Column widths proportional to SAS character widths:
  #   USUBJID=15, DSREASCD=30, DSTERM=30  →  ratio 2:4:4
  # col_header_widths applies to the 3 visible columns in rtf_colheader()
  # col_body_widths includes the group_by column (treatment_label) + 3 display cols
  col_header_widths <- c(2, 4, 4)
  col_body_widths   <- c(3, 2, 4, 4)

  # Ensure the output directory exists before writing

  output_dir <- dirname(output_path)
  if (nchar(output_dir) > 0L && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Build and write the RTF listing using r2rtf pipeline
  display_data %>%
    rtf_page(orientation = "portrait") %>%
    rtf_title(
      title = c(tt1, tt2, tt3)
    ) %>%
    rtf_colheader(
      colheader = "Subject ID | Reason for Discontinuation | Textual Reason",
      col_rel_width = col_header_widths
    ) %>%
    rtf_body(
      col_rel_width = col_body_widths,
      group_by = c("treatment_label")
    ) %>%
    rtf_encode() %>%
    write_rtf(file = output_path)

  message("Discontinuation listing (Target 32) written to: ", output_path)

  # Return the output path invisibly for programmatic use
  invisible(output_path)
}


# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    1. ADSL transport file (adsl.xpt) is located at data_path/adsl.xpt
#    2. ITT population filter is applied when pop_flag="Y" using ITTFL variable
#    3. DSREASCD and DSTERM variables are present in the ADSL dataset
#    4. TRTAN (Treatment Arm Numeric) is used for grouping — the SAS source
#       uses TRTAN exclusively (no TRTA character treatment variable)
#    5. The SAS source URL (GitHub raw content) is replaced by a local
#       file path parameter per AAP requirement (no hardcoded paths)
#    6. Population type label ("Randomized" vs "Enrolled") is provided
#       by the caller rather than derived from pop_flag, giving more
#       flexibility than the original SAS logic
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. None expected — this is a subject listing with no statistical
#       calculations. All data values are displayed as-is from ADSL.
#    2. TRTAN rounding uses janitor::round_half_up() for SAS equivalence,
#       though TRTAN is typically already integer-valued in ADaM datasets.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS %STDREP macro with COMPUTE BEFORE TRTAN / put 'Treatment: #TRTAN#'
#       → Replaced by r2rtf group_by parameter in rtf_body(), which creates
#       treatment group header rows spanning all display columns.
#    2. SAS options formchar='|____|+|___+=|_/\<>*'
#       → Not needed in R; this controls SAS ODS listing box-drawing characters.
#    3. SAS options missing=''
#       → Explicit NA-to-empty-string conversion using dplyr::if_else() before
#       RTF rendering. Data integrity preserved (NAs retained in processing).
#    4. SAS %endmac: (line 107) — syntax error in source corrected in R version.
#       The intended statement was %mend stdrep; which is the standard SAS
#       macro termination. The R function uses standard R function scoping.
#    5. SAS PROC REPORT split='?' option (line break on '?' in column headers)
#       → r2rtf handles column header line breaks via the pipe-delimited
#       colheader string format.
#
# PACKAGE SELECTION RATIONALE:
#    1. haven (2.5.5)  — CDISC XPT transport file I/O; read_xpt() preserves
#       SAS labels and format metadata. Selected over SASxport (legacy).
#    2. dplyr (>=1.1.0) — Data manipulation (filter, select, arrange, mutate,
#       if_else); replaces SAS DATA step, PROC SQL, and PROC SORT. Preferred
#       over base R per AAP rule: "tidyverse over base R."
#    3. rlang (>=1.1.0) — Provides the %||% operator required internally by
#       r2rtf 1.3.0 on R < 4.4.0. Must be loaded before r2rtf. Base R only
#       added %||% in 4.4.0; loading rlang ensures cross-version compatibility.
#    4. r2rtf (1.3.0)  — RTF listing output; replaces SAS ODS RTF + PROC REPORT.
#       Supports group_by for treatment arm headers, col_rel_width for column
#       sizing, and rtf_title for dynamic titles. Pipeline requires rtf_encode()
#       step before write_rtf() to convert attributes to RTF markup. Selected
#       per AAP target stack.
#    5. stringr (>=1.5.0) — String manipulation (str_trim, str_squish); replaces
#       SAS %str() macro quoting and TRIM(LEFT()) functions. Preferred over
#       base R trimws() per AAP tidyverse rule.
#    6. janitor (>=2.2.0) — SAS-compatible rounding via round_half_up(); ensures
#       numeric formatting matches SAS default round-half-up behavior per
#       AAP §0.7.2. Included for regulatory-grade numeric equivalence even
#       though this listing has minimal calculations.
#
# OPEN QUESTIONS:
#    1. Confirm listing sort order: SAS annotations specify "Sort by STUDYID
#       and USUBJID" (line 13), but the PROC REPORT sorts by TRTAN then
#       USUBJID. The R implementation follows the PROC REPORT behavior
#       (sort by TRTAN, USUBJID). If STUDYID sort is required, add STUDYID
#       to the arrange() call.
#    2. Verify treatment label source: The SAS source uses TRTAN (numeric)
#       for both sorting and group headers, displaying "Treatment: <number>".
#       If the intended display uses TRTA (character treatment name) instead,
#       modify the treatment_label construction to use TRTA. This would
#       require adding TRTA to the select() call.
#    3. The SAS source filters for specific discontinuation reasons in the
#       titles (Physician Decision, Withdrawal by Subject, Withdrawal by
#       Parent/Guardian, Other) but does NOT filter the data for these
#       reasons — the listing shows ALL subjects. Confirm whether the
#       listing should be filtered to only these reason categories.
# =============================================================================
