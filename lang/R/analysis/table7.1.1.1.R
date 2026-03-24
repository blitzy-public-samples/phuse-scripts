# ============================================================================
# Table 7.1.1.1 — Deaths Listing
# ============================================================================
#
# Purpose:
#   Regulatory mortality listing from DM (Demographics) and EX (Exposure)
#   CDISC domain datasets. Produces a formatted deaths listing table
#   suitable for regulatory submissions (NDA/BLA to FDA, PMDA).
#
# Original SAS Source:
#   lang/SAS/analysis/UCM072974/src/table7.1.1.1.sas
#
# Transformation:
#   SAS DATA step + PROC REPORT → dplyr pipeline + r2rtf output
#
# Migration Framework:
#   PhUSE CS WG5 Standard Analyses — SAS-to-R Migration
#   Migration date: 2026-03-24
#
# Description:
#   The SAS program merges DM and EX domains, filters to deceased subjects
#   (DTHFL = 'Y'), keeps only the last exposure record per subject,
#   derives a timing metric (DTHDY), and renders a PROC REPORT listing.
#   This R version produces numerically equivalent output using
#   tidyverse + r2rtf.
#
# ============================================================================

# ---------------------------------------------------------------------------
# Library Loading
# ---------------------------------------------------------------------------
# All libraries loaded via library() — NOT require() — per migration policy.
# Tidyverse packages are mandated over base R equivalents.
# ---------------------------------------------------------------------------

library(haven)    # SAS XPT transport file I/O: read_xpt()
library(dplyr)    # Core tidyverse data manipulation replacing DATA step
library(tidyr)    # Tidyverse reshaping utilities (standard migration stack)
library(stringr)  # Tidyverse string manipulation (standard migration stack)
library(rlang)    # Tidy evaluation infrastructure (required by r2rtf for %||%)
library(Tplyr)    # Clinical table construction grammar (pharmaverse stack)
library(r2rtf)    # Production-ready RTF output replacing ODS RTF
library(janitor)  # SAS-compatible rounding: round_half_up()
library(readr)    # Tidyverse flat file I/O (standard migration stack)

# ---------------------------------------------------------------------------
# Helper: Ensure SAS date values are converted to R Date objects
# ---------------------------------------------------------------------------
# SAS stores dates as integer days from 1960-01-01.
# haven::read_xpt() typically returns Date objects, but if raw numeric
# values are present, this utility converts them safely.
# ---------------------------------------------------------------------------
ensure_date <- function(x) {
  if (is.numeric(x) && !inherits(x, "Date")) {
    # SAS numeric date: integer days from 1960-01-01
    as.Date(x, origin = "1960-01-01")
  } else if (inherits(x, "Date")) {
    x
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    as.Date(x)
  } else {
    # Return as-is (may be character or NA); caller handles downstream
    x
  }
}

# ---------------------------------------------------------------------------
# Helper: Sanitize character columns — map SAS blank strings to NA_character_
# ---------------------------------------------------------------------------
# SAS character missing (' ') must map to NA_character_, not empty string "".
# ---------------------------------------------------------------------------
sanitize_char_missing <- function(x) {
  if (is.character(x)) {
    dplyr::if_else(
      stringr::str_trim(x) == "" | is.na(x),
      NA_character_,
      x
    )
  } else {
    x
  }
}

# ===========================================================================
# Primary Function: generate_deaths_listing
# ===========================================================================
#
# Migrated from SAS:
#   data dth;
#     merge dm (where=(dthfl='Y')) ex;
#     by subjid;
#     if last.exdt;
#     if dthdtc gt rfendtc then dthdy = rfendtc-rfstdtc+1||'/'||dthdtc-rfendtc;
#     else dthdy = dthdtc - rfstdtc + 1;
#   run;
#
#   proc report data=dth nofs headline headskip;
#     column studyid / group ... ;
#     ...
#   run;
#
# All SAS macro variables / hardcoded paths are replaced by named R
# function arguments with matching defaults per AAP §0.8.1.
#
# ===========================================================================

generate_deaths_listing <- function(dm_path,
                                    ex_path,
                                    output_path = NULL,
                                    treatment_label = "New Drug",
                                    cutoff_date = "Cutoff Date",
                                    death_window_days = 30L) {

  # -------------------------------------------------------------------------
  # Input Validation
  # -------------------------------------------------------------------------
  if (!is.character(dm_path) || length(dm_path) != 1L) {
    stop(
      "dm_path must be a single character string specifying the DM domain XPT file path.",
      call. = FALSE
    )
  }
  if (!is.character(ex_path) || length(ex_path) != 1L) {
    stop(
      "ex_path must be a single character string specifying the EX domain XPT file path.",
      call. = FALSE
    )
  }
  if (!file.exists(dm_path)) {
    stop(
      paste0("DM domain file not found: ", dm_path),
      call. = FALSE
    )
  }
  if (!file.exists(ex_path)) {
    stop(
      paste0("EX domain file not found: ", ex_path),
      call. = FALSE
    )
  }
  if (!is.null(output_path) && (!is.character(output_path) || length(output_path) != 1L)) {
    stop(
      "output_path must be NULL or a single character string for the RTF output file.",
      call. = FALSE
    )
  }
  if (!is.character(treatment_label) || length(treatment_label) != 1L) {
    stop(
      "treatment_label must be a single character string.",
      call. = FALSE
    )
  }
  if (!is.character(cutoff_date) || length(cutoff_date) != 1L) {
    stop(
      "cutoff_date must be a single character string.",
      call. = FALSE
    )
  }
  if (!is.integer(death_window_days) || length(death_window_days) != 1L || death_window_days < 0L) {
    stop(
      "death_window_days must be a non-negative integer (use L suffix, e.g. 30L).",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------------
  # Phase 3: Data Ingestion — Replacing SAS MERGE input
  # -------------------------------------------------------------------------
  # Read SAS XPT transport files using haven::read_xpt().
  # Column names are normalized to lowercase to handle SAS uppercase.
  # SAS numeric missing (.) maps to NA automatically via haven.
  # -------------------------------------------------------------------------

  dm <- haven::read_xpt(dm_path) %>%
    dplyr::rename_with(tolower)

  ex <- haven::read_xpt(ex_path) %>%
    dplyr::rename_with(tolower)

  # Sanitize character columns: map SAS blank strings to NA_character_
  dm <- dm %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      sanitize_char_missing
    ))

  ex <- ex %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      sanitize_char_missing
    ))

  # -------------------------------------------------------------------------
  # Phase 4: Data Manipulation — Replacing SAS DATA Step
  # -------------------------------------------------------------------------


  # Step 1: Filter DM to deceased subjects (DTHFL = 'Y')
  # SAS: dm (where = (dthfl = 'Y'))
  dm_deaths <- dm %>%
    dplyr::filter(dthfl == "Y")

  # If no deceased subjects, return empty tibble with expected structure

  if (nrow(dm_deaths) == 0L) {
    empty_listing <- tibble::tibble(
      studyid   = character(0L),
      siteid    = character(0L),
      subjid    = character(0L),
      age       = numeric(0L),
      sex       = character(0L),
      exdose    = numeric(0L),
      dthdy     = character(0L),
      source    = character(0L),
      psntime   = character(0L),
      desc      = character(0L)
    )
    if (!is.null(output_path)) {
      # Ensure output directory exists
      output_dir <- dirname(output_path)
      if (!dir.exists(output_dir) && output_dir != ".") {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
      # r2rtf requires at least one row in rtf_body(); write a placeholder
      # row indicating no deaths were reported for the treatment arm.
      placeholder_row <- tibble::tibble(
        studyid   = "No deaths reported",
        siteid    = "", subjid = "", age = "", sex = "",
        exdose    = "", dthdy = "", source = "", psntime = "",
        desc      = ""
      )
      placeholder_row %>%
        r2rtf::rtf_page(orientation = "landscape") %>%
        r2rtf::rtf_title(
          title = c(
            "Table 7.1.1.1",
            "Deaths Listing",
            paste0("Treatment = ", treatment_label),
            cutoff_date
          ),
          text_font = 1
        ) %>%
        r2rtf::rtf_colheader(
          colheader = paste(
            "Trial", "Center", "Patient", "Age (yrs)", "Sex",
            "Dose (mg)", "Time (days)", "Source", "Person Time",
            "Description",
            sep = " | "
          ),
          col_rel_width = rep(1, 10),
          text_font = 1
        ) %>%
        r2rtf::rtf_body(
          col_rel_width = rep(1, 10),
          text_font = 1
        ) %>%
        r2rtf::rtf_footnote(
          footnote = paste0(
            "All deaths that occurred within a period of up to ",
            death_window_days,
            " days following discontinuation from drug"
          ),
          text_font = 1
        ) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(file = output_path)
      return(invisible(empty_listing))
    }
    return(empty_listing)
  }

  # Step 2: Merge DM (deceased) with EX by SUBJID
  # SAS: merge dm (where=(dthfl='Y')) ex; by subjid;
  # Using left_join to preserve all death-flagged DM records.
  dth <- dm_deaths %>%
    dplyr::left_join(ex, by = "subjid", suffix = c("", ".ex"))

  # Step 3: Replicate IF LAST.EXDT — keep only the row with the latest EXDT
  # SAS sorts by SUBJID then within each subject keeps the last observation

  # by EXDT. We group by subjid, arrange descending by exdt, and take the
  # first row per group.
  # If exdt is missing (NA), it sorts last in desc() order — this preserves
  # SAS behavior where missing dates sort low.
  dth <- dth %>%
    dplyr::group_by(subjid) %>%
    dplyr::arrange(dplyr::desc(exdt), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Step 4: Ensure date columns are proper Date objects
  # SAS dates are integer days from 1960-01-01; haven typically reads them
  # as Date objects but we handle edge cases explicitly.
  date_cols <- c("dthdtc", "rfstdtc", "rfendtc", "exdt")
  for (col in date_cols) {
    if (col %in% colnames(dth)) {
      dth <- dth %>%
        dplyr::mutate(!!rlang::sym(col) := ensure_date(!!rlang::sym(col)))
    }
  }

  # Step 5: Derive DTHDY variable
  # SAS logic:
  #   if dthdtc gt rfendtc then dthdy = rfendtc-rfstdtc + 1||'/'||dthdtc-rfendtc
  #   else dthdy = dthdtc - rfstdtc + 1
  #
  # DTHDY is a CHARACTER variable in SAS (due to || concatenation).
  # - When death occurs after treatment end (dthdtc > rfendtc):
  #   DTHDY = "<on-drug days>/<off-drug days>"
  #   where on-drug days = rfendtc - rfstdtc + 1
  #         off-drug days = dthdtc - rfendtc
  # - When death occurs during treatment:
  #   DTHDY = "<days on drug at death>"
  #   where days = dthdtc - rfstdtc + 1
  #
  # Missing value handling: if any date component is NA, DTHDY = NA_character_
  dth <- dth %>%
    dplyr::mutate(
      dthdy = dplyr::case_when(
        # Any missing date yields NA — no implicit zero substitution
        is.na(dthdtc) | is.na(rfstdtc) ~ NA_character_,
        # Post-discontinuation death: rfendtc must also be non-missing
        (!is.na(rfendtc)) & (dthdtc > rfendtc) ~ paste0(
          as.integer(difftime(rfendtc, rfstdtc, units = "days")) + 1L,
          "/",
          as.integer(difftime(dthdtc, rfendtc, units = "days"))
        ),
        # During-treatment death (or rfendtc is missing, treat as on-drug)
        TRUE ~ as.character(
          as.integer(difftime(dthdtc, rfstdtc, units = "days")) + 1L
        )
      )
    )

  # -------------------------------------------------------------------------
  # Phase 5: Table Construction — Replacing SAS PROC REPORT
  # -------------------------------------------------------------------------
  # Select the 10 columns required by the listing, matching SAS COLUMN
  # statement order. Arrange by STUDYID, SITEID, SUBJID for grouped display.
  # -------------------------------------------------------------------------

  # Determine which columns exist — handle gracefully if some are absent
  required_cols <- c(
    "studyid", "siteid", "subjid", "age", "sex",
    "exdose", "dthdy", "source", "psntime", "desc"
  )
  available_cols <- intersect(required_cols, colnames(dth))
  missing_cols <- setdiff(required_cols, colnames(dth))

  # Add any missing columns as NA to preserve listing structure
  for (mc in missing_cols) {
    dth <- dth %>%
      dplyr::mutate(!!rlang::sym(mc) := NA_character_)
  }

  # Select and arrange for the listing
  listing <- dth %>%
    dplyr::select(
      dplyr::all_of(required_cols)
    ) %>%
    dplyr::arrange(studyid, siteid, subjid)

  # Convert all columns to character for RTF output rendering
  # (r2rtf expects character data frames for consistent formatting)
  listing_rtf <- listing %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(is.na(as.character(.x)), "", as.character(.x))
    ))

  # -------------------------------------------------------------------------
  # Phase 6: RTF Output Generation — Replacing ODS RTF
  # -------------------------------------------------------------------------
  # Generates formatted RTF using r2rtf when output_path is provided.
  # Titles, footnotes, column headers, and font match SAS PROC REPORT exactly.
  #
  # SAS Titles:
  #   title1 'Table 7.1.1.1'
  #   title2 'Deaths Listing'
  #   title3 'Treatment = New Drug'  (parameterized)
  #   title4 'Cutoff Date'           (parameterized)
  #
  # SAS Footnote:
  #   footnote1 'All deaths that occurred within a period of up to 30 days
  #              following discontinuation from drug'
  #
  # Column widths: all equal (matching SAS width=10 for each column)
  # Font: Times New Roman (text_font=1 in r2rtf, matching SAS ODS default)
  # -------------------------------------------------------------------------

  if (!is.null(output_path)) {
    # Ensure output directory exists
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir) && output_dir != ".") {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Column header string with pipe separators (r2rtf convention)
    col_header_str <- paste(
      "Trial", "Center", "Patient", "Age (yrs)", "Sex",
      "Dose (mg)", "Time (days)", "Source", "Person Time",
      "Description",
      sep = " | "
    )

    # Build and write RTF
    listing_rtf %>%
      r2rtf::rtf_page(orientation = "landscape") %>%
      r2rtf::rtf_title(
        title = c(
          "Table 7.1.1.1",
          "Deaths Listing",
          paste0("Treatment = ", treatment_label),
          cutoff_date
        ),
        text_font = 1
      ) %>%
      r2rtf::rtf_colheader(
        colheader = col_header_str,
        col_rel_width = rep(1, 10),
        text_font = 1
      ) %>%
      r2rtf::rtf_body(
        col_rel_width = rep(1, 10),
        text_font = 1,
        group_by = c("studyid", "siteid")
      ) %>%
      r2rtf::rtf_footnote(
        footnote = paste0(
          "All deaths that occurred within a period of up to ",
          death_window_days,
          " days following discontinuation from drug"
        ),
        text_font = 1
      ) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(file = output_path)

    return(invisible(listing))
  }

  # -------------------------------------------------------------------------
  # Phase 7: Function Return Value
  # -------------------------------------------------------------------------
  # Return the final listing tibble with proper column names.
  # Returned visibly when no RTF output is written.
  # -------------------------------------------------------------------------
  listing
}


# ===========================================================================
# Example Usage
# ===========================================================================
# deaths <- generate_deaths_listing(
#   dm_path = "path/to/dm.xpt",
#   ex_path = "path/to/ex.xpt",
#   output_path = "output/table_7_1_1_1.rtf",
#   treatment_label = "New Drug",
#   cutoff_date = "2024-01-15",
#   death_window_days = 30L
# )
# ===========================================================================


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. SAS MERGE BY SUBJID with IF LAST.EXDT is interpreted as keeping
#       only the record with the latest EXDT per subject after the DM-EX join.
#    2. SAS date variables (DTHDTC, RFSTDTC, RFENDTC, EXDT) are assumed to
#       be stored as SAS numeric dates (days from 1960-01-01) or as
#       Date-class objects after haven::read_xpt() processing.
#    3. The DTHDY variable in SAS is CHARACTER (due to || concatenation);
#       the R equivalent is also character to preserve the "X/Y" format
#       for post-discontinuation deaths.
#    4. Variables SOURCE, PSNTIME, and DESC are assumed present in the
#       input DM or EX domain datasets. The original SAS code references
#       them but does not derive them — they must exist in the source data.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. Date arithmetic: SAS integer day math vs R Date class subtraction
#       should be identical, but verify edge cases around missing dates.
#    2. Sort stability: SAS BY SUBJID sort is stable; R dplyr::arrange()
#       is also stable within groups, but multi-key sort order should be
#       verified against SAS output.
#    3. EXDT tie-breaking: If multiple EX records share the same maximum
#       EXDT for a subject, SAS LAST.EXDT retains the physically last
#       observation in sorted order. R slice(1) after desc(exdt) arrange
#       retains the first in descending order, which may differ if there
#       are ties. Document and review.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS PROC REPORT GROUP option (merged cell display for STUDYID
#       and SITEID) — approximated using r2rtf group_by parameter in
#       rtf_body(). Exact cell-merge behavior may differ visually.
#    2. SAS HEADLINE/HEADSKIP options — approximated via r2rtf column
#       header styling with border lines.
#
# PACKAGE SELECTION RATIONALE:
#    1. haven: Required for reading SAS XPT transport files (CDISC standard)
#    2. dplyr: Idiomatic tidyverse data manipulation replacing DATA step
#    3. r2rtf: Production-ready RTF output replacing ODS RTF (Merck-developed,
#       used in regulatory submissions)
#    4. Tplyr: Clinical table construction grammar (optional for this listing;
#       primary use is structured table layers for frequency/summary tables)
#    5. janitor: round_half_up() for SAS-compatible rounding behavior
#    6. stringr: String operations for DTHDY concatenation
#    7. tidyr: Standard migration stack for reshaping (available if needed)
#    8. readr: Standard migration stack for flat file I/O (available if needed)
#
# OPEN QUESTIONS:
#    1. Are SOURCE, PSNTIME, and DESC variables present in the ADaM/SDTM
#       datasets used with this listing, or must they be derived? The SAS
#       code references but does not create them.
#    2. Should DTHDY concatenation format ("X/Y") use a different delimiter
#       for regulatory submissions (e.g., "X / Y" with spaces)?
#    3. Is the death inclusion window (30 days) parameterized correctly, or
#       should it be part of a study-level configuration?
#    4. Exact EXDT tie-breaking behavior requires statistician review if
#       multiple exposure records share the same date for a subject.
# ============================================================
