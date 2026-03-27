# =============================================================================
# PROGRAM:     medication_listing.R
# DESCRIPTION: Target 36 — Listing of medications (Safety Population)
# MIGRATED FROM: whitepapers/scriptathons/demographics/medication_listing.sas
# ORIGINAL AUTHOR: Adrienne M Bonwick
# ORIGINAL DATE:   12th October 2014 (PhUSE scriptathon)
# MIGRATION DATE:  2026
# EXTERNAL FILES:  ADCM.xpt (CDISC ADaM Concomitant Medications)
#
# FUNCTIONAL OVERVIEW:
#   Reads ADCM transport data, filters to safety population (SAFFL='Y'),
#   sorts by TRTA/STUDYID/USUBJID/CMSTDTC/CMENDTC/CMDECOD, derives
#   StartStop, DoseUnit, and Ongoing fields, then writes an RTF listing
#   grouped by treatment arm (TRTA) with landscape orientation.
#
# SAS-TO-R MAPPING:
#   SAS filename/libname xport  -> haven::read_xpt()
#   SAS %let POPN/POPNLABEL     -> pop_var/pop_label function parameters
#   SAS PROC SORT + WHERE       -> dplyr::filter() + arrange()
#   SAS DATA step (ADCM2)       -> dplyr::mutate()
#   SAS compbl()                -> stringr::str_squish()
#   SAS ODS RTF + PROC REPORT   -> r2rtf pipeline (rtf_body + rtf_colheader
#                                  + rtf_title + rtf_page + write_rtf)
#
# CONFIGURATION:
#   Callers should load config/migration_config.yaml for centralized paths:
#     config <- yaml::read_yaml("config/migration_config.yaml")
#     generate_medication_listing(
#       data_path   = config$data_paths$adam_path,
#       output_path = file.path(config$output_paths$rtf_output_path,
#                               "Target36.rtf")
#     )
# =============================================================================

# ---------------------------------------------------------------------------
# Library Loading
# ---------------------------------------------------------------------------
library(haven)    # SAS transport I/O: read_xpt() replaces SAS libname xport
library(dplyr)    # Data manipulation: filter, arrange, mutate, select, if_else
library(stringr)  # String functions: str_squish() replaces SAS compbl()
library(rlang)    # Tidy evaluation: sym(), %||% (required by r2rtf internals)
library(r2rtf)    # RTF output: replaces SAS ODS RTF + PROC REPORT
library(janitor)  # SAS-compatible rounding: round_half_up()
library(cli)

# ---------------------------------------------------------------------------
# generate_medication_listing
# ---------------------------------------------------------------------------
#' Generate Medication Listing (Safety Population)
#'
#' Produces a landscape RTF listing of concomitant medications for the
#' specified analysis population, grouped by treatment arm. Migrated from
#' Target 36 SAS script (medication_listing.sas by Adrienne M Bonwick).
#'
#' @param data_path  Character. Path to directory containing adcm.xpt.
#'   Replaces SAS: \code{filename source url "..."; libname source xport;}
#' @param output_path Character. Path for the output RTF file.
#'   Replaces SAS: \code{ods rtf file="Target36.rtf"}
#'   Default: \code{"Target36.rtf"}
#' @param pop_var Character. Name of the population flag variable to filter on.
#'   Replaces SAS: \code{\%let POPN=SAFFL;}
#'   Default: \code{"SAFFL"}
#' @param pop_label Character. Population label used in listing title.
#'   Replaces SAS: \code{\%let POPNLABEL=Safety;}
#'   Default: \code{"Safety"}
#'
#' @return Invisible character path to the generated RTF file.
#'
#' @examples
#' \dontrun{
#'   # Direct usage:
#'   generate_medication_listing(
#'     data_path   = "data/adam/cdisc",
#'     output_path = "output/rtf/Target36.rtf"
#'   )
#'
#'   # Using migration_config.yaml:
#'   config <- yaml::read_yaml("config/migration_config.yaml")
#'   generate_medication_listing(
#'     data_path   = config$data_paths$adam_path,
#'     output_path = file.path(config$output_paths$rtf_output_path,
#'                             "Target36.rtf")
#'   )
#' }
#'
#' @export
generate_medication_listing <- function(data_path,
                                        output_path = "Target36.rtf",
                                        pop_var     = "SAFFL",
                                        pop_label   = "Safety") {

  # =========================================================================
  # Input Validation
  # =========================================================================
  if (!is.character(data_path) || length(data_path) != 1L || is.na(data_path)) {
    cli::cli_abort("'data_path' must be a single non-NA character string.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L ||
      is.na(output_path)) {
    cli::cli_abort("'output_path' must be a single non-NA character string.",
         call. = FALSE)
  }
  if (!is.character(pop_var) || length(pop_var) != 1L || is.na(pop_var)) {
    cli::cli_abort("'pop_var' must be a single non-NA character string.", call. = FALSE)
  }
  if (!is.character(pop_label) || length(pop_label) != 1L ||
      is.na(pop_label)) {
    cli::cli_abort("'pop_label' must be a single non-NA character string.",
         call. = FALSE)
  }

  adcm_file <- file.path(data_path, "adcm.xpt")
  if (!file.exists(adcm_file)) {
    cli::cli_abort(
      paste0("ADCM transport file not found: ", adcm_file),
      call. = FALSE
    )
  }

  # =========================================================================
  # Phase 1 — Data Reading (SAS lines 16-17)
  #   Replaces:
  #     filename source url "https://...adcm.xpt";
  #     libname source xport;
  # =========================================================================
  adcm_raw <- haven::read_xpt(adcm_file)

  # Verify population flag variable exists
  if (!pop_var %in% names(adcm_raw)) {
    cli::cli_abort(
      paste0(
        "Population variable '", pop_var,
        "' not found in ADCM dataset. Available columns: ",
        paste(names(adcm_raw), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # =========================================================================
  # Phase 2 — Population Filtering and Sorting (SAS lines 24-28)
  #   Replaces:
  #     proc sort out=work.adcm data=source.ADCM;
  #       by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD;
  #       WHERE &POPN='Y';
  #     run;
  # =========================================================================
  adcm <- adcm_raw %>%
    dplyr::filter(!!dplyr::sym(pop_var) == "Y") %>%
    dplyr::arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

  if (nrow(adcm) == 0L) {
    warning(
      paste0(
        "No records found after filtering ", pop_var, " = 'Y'. ",
        "An empty RTF listing will be generated."
      ),
      call. = FALSE
    )

    # r2rtf cannot render a zero-row data frame. Create a minimal
    # placeholder row indicating no data, then generate the RTF.
    empty_row <- data.frame(
      Treatment = "",
      USUBJID   = "No records found.",
      CMTRT     = "", CMDECOD = "", CMCLAS = "",
      DoseUnit  = "", CMINDC  = "", StartStop = "",
      ADURN     = "", Ongoing = "",
      stringsAsFactors = FALSE
    )

    col_header_text <- paste(
      "USUBJID", "CMTRT", "CMDECOD", "CMCLAS",
      "Dose (unit)", "CMINDC", "Start Date / Stop Date",
      "Dur (Days)", "Ongoing",
      sep = " | "
    )
    col_widths <- c(11, 10, 10, 10, 6, 10, 10, 6, 7)

    output_dir <- dirname(output_path)
    if (nchar(output_dir) > 0L && output_dir != ".") {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }

    empty_row %>%
      rtf_page(orientation = "landscape") %>%
      rtf_title(
        title = c(
          paste0(pop_label, " Population"),
          "Study Period",
          "No matching records"
        ),
        text_justification = "l"
      ) %>%
      rtf_colheader(
        colheader = col_header_text,
        col_rel_width = col_widths,
        text_justification = "l"
      ) %>%
      rtf_body(
        col_rel_width = col_widths,
        text_justification = "l"
      ) %>%
      write_rtf(file = output_path)

    return(invisible(output_path))
  }

  # =========================================================================
  # Phase 3 — Derive New Variables (SAS DATA step lines 30-42)
  #   Replaces:
  #     Data ADCM2; set ADCM;
  #       length StartStop $25 Doseunit $50;
  #       Period ='Study Period';
  #       if CMSTDTC='' then CMSTDTC='NK';
  #       if CMENDTC='' then CMENDTC='NK';
  #       StartStop = compbl(CMSTDTC||'/'||CMENDTC);
  #       If cmunit ne . then DoseUnit = compbl(CMDOSE||'('||cmunit||')');
  #         else DoseUnit = compbl(CMDOSE);
  #       if AENRF='Ongoing' then Ongoing='Y'; else Ongoing='N';
  #     run;
  # =========================================================================

  # Identify dose-unit column: CDISC ADaM standard is CMDOSU; original SAS

  # code references 'cmunit' (SAS is case-insensitive). Prefer CMDOSU.
  dose_unit_var <- if ("CMDOSU" %in% names(adcm)) {
    "CMDOSU"
  } else if ("CMUNIT" %in% names(adcm)) {
    "CMUNIT"
  } else {
    NA_character_
  }

  # Core derivations --------------------------------------------------------
  adcm2 <- adcm %>%
    dplyr::mutate(
      # Period assignment (SAS line 33)
      Period = "Study Period",

      # Handle missing start dates (SAS line 34):
      #   if CMSTDTC='' then CMSTDTC='NK';
      CMSTDTC = dplyr::if_else(
        is.na(CMSTDTC) | CMSTDTC == "",
        "NK",
        as.character(CMSTDTC)
      ),

      # Handle missing end dates (SAS line 35):
      #   if CMENDTC='' then CMENDTC='NK';
      CMENDTC = dplyr::if_else(
        is.na(CMENDTC) | CMENDTC == "",
        "NK",
        as.character(CMENDTC)
      ),

      # StartStop concatenation (SAS line 36):
      #   StartStop = compbl(CMSTDTC||'/'||CMENDTC);
      StartStop = stringr::str_squish(paste0(CMSTDTC, "/", CMENDTC)),

      # Ongoing derivation (SAS lines 40-41):
      #   if AENRF='Ongoing' then Ongoing='Y'; else Ongoing='N';
      # Note: CDISC data may use uppercase 'ONGOING'. We compare
      # case-insensitively for robustness while preserving SAS logic.
      Ongoing = dplyr::if_else(
        toupper(AENRF) == "ONGOING",
        "Y",
        "N"
      )
    )

  # DoseUnit derivation (SAS lines 37-38) ----------------------------------
  # Handled separately due to conditional column reference.
  #   SAS: If cmunit ne . then DoseUnit = compbl(CMDOSE||'('||cmunit||')')
  #        else DoseUnit = compbl(CMDOSE);
  if (!is.na(dose_unit_var)) {
    adcm2 <- adcm2 %>%
      dplyr::mutate(
        DoseUnit = dplyr::if_else(
          !is.na(!!dplyr::sym(dose_unit_var)) &
            as.character(!!dplyr::sym(dose_unit_var)) != "",
          stringr::str_squish(
            paste0(CMDOSE, "(", !!dplyr::sym(dose_unit_var), ")")
          ),
          stringr::str_squish(as.character(CMDOSE))
        )
      )
  } else {
    # No dose unit column available — use CMDOSE only
    adcm2 <- adcm2 %>%
      dplyr::mutate(
        DoseUnit = stringr::str_squish(as.character(CMDOSE))
      )
  }

  # =========================================================================
  # Phase 4 — Prepare Output Dataset (SAS PROC REPORT lines 48-64)
  #   Select display columns, convert for RTF rendering.
  #   SAS COLUMNS: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit
  #                CMINDC StartStop ADURN Ongoing
  #   SAS DEFINE TRTA / noprint  =>  used for page grouping only
  # =========================================================================

  # Create treatment grouping label for r2rtf page_by
  # Replaces SAS: title3 JUSTIFY=L "Treatment: #byval1";
  output_data <- adcm2 %>%
    dplyr::arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD) %>%
    dplyr::mutate(
      # Rename TRTA -> Treatment for display in page_by header
      Treatment = paste0("Treatment: ", TRTA),
      # Convert ADURN to character; NA -> blank (not zero)
      ADURN = dplyr::if_else(is.na(ADURN), "", as.character(ADURN))
    ) %>%
    dplyr::select(
      Treatment, USUBJID, CMTRT, CMDECOD, CMCLAS,
      DoseUnit, CMINDC, StartStop, ADURN, Ongoing
    )

  # Replace remaining NAs with empty strings for clean RTF display
  # (SAS missing values render as blank in PROC REPORT listings)
  output_data <- output_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(as.character(.)), "", as.character(.))
      )
    )

  # =========================================================================
  # Phase 5 — RTF Output Generation (SAS lines 45-66)
  #   Replaces:
  #     ods rtf file="Target36.rtf" style=styles.journal;
  #     options nobyline;
  #     Proc Report data=ADCM2 headline;
  #       by TRTA;
  #       Columns TRTA USUBJID CMTRT cmdecod cmclas DoseUnit
  #               CMINDC StartStop ADURN Ongoing;
  #       define TRTA / noprint;
  #       ...
  #       title  JUSTIFY=L "&POPNLABEL Population";
  #       title2 JUSTIFY=L "Study Period";
  #       title3 JUSTIFY=L "Treatment: #byval1";
  #     run; quit;
  #     ods rtf close;
  # =========================================================================

  # Column headers matching SAS DEFINE statements (9 display columns)
  col_header_text <- paste(
    "USUBJID", "CMTRT", "CMDECOD", "CMCLAS",
    "Dose (unit)", "CMINDC", "Start Date / Stop Date",
    "Dur (Days)", "Ongoing",
    sep = " | "
  )

  # Column relative widths proportional to SAS character widths
  # SAS widths: USUBJID=11, CMTRT=10, CMDECOD=10, CMCLAS=10,
  #             DoseUnit=6, CMINDC=10, StartStop=10, ADURN=6, Ongoing=7
  col_widths <- c(11, 10, 10, 10, 6, 10, 10, 6, 7)

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (nchar(output_dir) > 0L && output_dir != ".") {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Generate RTF listing
  # - rtf_page: landscape orientation (SAS: options orientation=landscape)
  # - rtf_title: population and period titles (SAS: title/title2)
  # - rtf_colheader: column headers matching SAS DEFINE labels
  # - rtf_body: data body with page_by for treatment grouping
  #   (SAS: by TRTA + title3 "Treatment: #byval1")
  # - write_rtf: file output (SAS: ods rtf close)
  output_data %>%
    rtf_page(orientation = "landscape") %>%
    rtf_title(
      title = c(
        paste0(pop_label, " Population"),
        "Study Period"
      ),
      text_justification = "l"
    ) %>%
    rtf_colheader(
      colheader = col_header_text,
      col_rel_width = col_widths,
      text_justification = "l"
    ) %>%
    rtf_body(
      col_rel_width = col_widths,
      page_by = "Treatment",
      text_justification = "l"
    ) %>%
    write_rtf(file = output_path)

  # =========================================================================
  # Return
  # =========================================================================
  invisible(output_path)
}


# =============================================================================
#
# MIGRATION NOTES
#
# =============================================================================
#
# ASSUMPTIONS:
#   1. ADCM transport file (adcm.xpt) resides at data_path/adcm.xpt.
#   2. SAFFL (or user-specified pop_var) population flag is present.
#   3. AENRF variable contains ongoing-treatment status ('ONGOING' or
#      'Ongoing'); case-insensitive comparison used for robustness.
#   4. Variable names follow CDISC ADaM ADCM standard: CMSTDTC, CMENDTC,
#      CMDECOD, CMCLAS, CMDOSE, CMDOSU (or CMUNIT), CMINDC, ADURN, AENRF,
#      CMTRT, TRTA, STUDYID, USUBJID.
#   5. The original SAS code references 'cmunit' for dose unit (SAS is
#      case-insensitive). CDISC standard uses CMDOSU. This R version
#      checks for CMDOSU first, then CMUNIT, to handle both conventions.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. None expected — this is a listing with no statistical calculations.
#   2. ADURN (duration in days) is carried through unchanged from the source
#      dataset with no rounding applied.
#   3. janitor::round_half_up() is loaded per AAP mandate for all migrated
#      scripts but is not directly invoked in this listing script.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS compbl() -> stringr::str_squish(): both remove excess internal
#      whitespace and trim leading/trailing spaces.
#   2. SAS ODS RTF style=styles.journal -> r2rtf default styling: r2rtf
#      produces clean RTF output; journal style approximated by default fonts.
#   3. SAS PROC REPORT 'flow' option -> r2rtf text wrapping: r2rtf wraps
#      text based on column widths specified via col_rel_width.
#   4. SAS title3 JUSTIFY=L "Treatment: #byval1" (dynamic by-group title) ->
#      r2rtf page_by parameter with "Treatment: {value}" formatted column.
#      r2rtf inserts the page_by value as a group header row on each page.
#   5. SAS 'options nodate nonumber' -> r2rtf does not include date/page
#      numbers by default; no additional configuration required.
#
# PACKAGE SELECTION RATIONALE:
#   - haven: ADCM XPT I/O (read_xpt replaces SAS filename/libname xport)
#   - dplyr: DATA step sorting/filtering/derivation (tidyverse per AAP)
#   - stringr: compbl() equivalent via str_squish() (tidyverse per AAP)
#   - r2rtf: ODS RTF listing output (pharmaverse stack per AAP)
#   - janitor: SAS-compatible rounding (loaded per AAP mandate; not actively
#     used in this listing script but available if needed)
#
# OPEN QUESTIONS:
#   1. Verify CMUNIT vs CMDOSU variable name in CDISC ADCM — the original
#      SAS code uses 'cmunit' but the CDISC pilot data uses CMDOSU. This R
#      version handles both via dynamic column detection.
#   2. Confirm "NK" (Not Known) convention for missing dates vs displaying
#      blank — the original SAS logic replaces blank dates with "NK".
#   3. Verify whether PROC REPORT 'flow' wrapping behavior needs explicit
#      r2rtf column width tuning for long medication names.
#   4. The SAS code checks AENRF == 'Ongoing' (mixed case) but the CDISC
#      pilot data contains 'ONGOING' (uppercase). This R version uses
#      case-insensitive comparison (toupper) for robustness.
#
# =============================================================================
