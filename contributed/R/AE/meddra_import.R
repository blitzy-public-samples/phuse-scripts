# =============================================================================
# Program:     meddra_import.R
# Description: Import MedDRA hierarchy ASC text files (mdhier.asc, pt.asc,
#              llt.asc), build a complete SOC-to-LLT hierarchy via joins,
#              uppercase terms for AE matching, and optionally save output
#              as SAS7BDAT datasets.
# Source:      Migrated from contributed/AE/meddra_import.sas (204 lines)
# SAS Macros:  %import (lines 18-160), %meddra (lines 166-204)
# R Functions: import_meddra_files(), meddra_import()
# =============================================================================

# --- Required Libraries -------------------------------------------------------
library(readr)
library(dplyr)
library(haven)
library(stringr)
library(cli)

# ==============================================================================
# import_meddra_files
# ==============================================================================
#' Import MedDRA Hierarchy ASC Files and Build SOC-to-LLT Mapping
#'
#' Reads three dollar-delimited MedDRA ASC text files (mdhier.asc, pt.asc,
#' llt.asc), joins PT with LLT to create a preferred-to-lower-level-term
#' mapping, then joins with the main hierarchy to produce a full SOC-to-LLT
#' dataset. Optionally writes output as SAS7BDAT datasets for interoperability.
#'
#' Migrated from SAS \code{%import} macro (lines 18-160 of
#' contributed/AE/meddra_import.sas).
#'
#' @param path Character. Directory path containing the MedDRA ASC files
#'   (mdhier.asc, pt.asc, llt.asc). No trailing separator required.
#' @param ver  Character. MedDRA version string (e.g., "22.0"). Used to label
#'   rows and name output files. Dots are converted to underscores for file
#'   naming (SAS \code{%sysfunc(translate(&ver.,'_','.'))}).
#' @param outpath Character or NULL. If non-NULL, directory path where SAS7BDAT
#'   output files will be written. If NULL, no files are saved.
#'
#' @return A named list with two tibbles:
#'   \describe{
#'     \item{mdhier}{SOC/HLGT/HLT/PT hierarchy sorted by aebodsys, aedecod.}
#'     \item{mdhier_llt}{Full SOC-to-LLT hierarchy sorted by soc_name,
#'       hlgt_name, hlt_name, pt_name, llt_name.}
#'   }
#'
#' @details
#' Column specifications for each ASC file mirror the SAS INPUT statements:
#' \itemize{
#'   \item mdhier.asc — 12 columns: pt_code (num), hlt_code (num),
#'     hlgt_code (num), soc_code (num), pt_name (chr), hlt_name (chr),
#'     hlgt_name (chr), soc_name (chr), soc_abbrev (chr), null_field (chr),
#'     pt_soc_code (num), primary_soc_fg (chr).
#'   \item pt.asc — 11 columns: pt_code (num), pt_name (chr),
#'     null_field (chr), pt_soc_code (num), pt_whoart_code (chr),
#'     pt_harts_code (num), pt_costart_sym (chr), pt_icd9_code (chr),
#'     pt_icd9cm_code (chr), pt_icd10_code (chr), pt_jart_code (chr).
#'   \item llt.asc — 11 columns: llt_code (num), llt_name (chr),
#'     pt_code (num), llt_whoart_code (chr), llt_harts_code (num),
#'     llt_costart_sym (chr), llt_icd9_code (chr), llt_icd9cm_code (chr),
#'     llt_icd10_code (chr), llt_currency (chr), llt_jart_code (chr).
#' }
import_meddra_files <- function(path, ver, outpath = NULL) {

  # Derive version string with underscores for file naming

#   SAS: %let dsver = %sysfunc(translate(&ver.,'_','.'));
  dsver <- stringr::str_replace_all(ver, "\\.", "_")

  # Trim version string (SAS: left("&ver."))
  ver_trimmed <- stringr::str_trim(ver)

  # ---------------------------------------------------------------------------
  # 1. Import mdhier.asc — SOC/HLGT/HLT/PT hierarchy (SAS lines 25-53)
  # ---------------------------------------------------------------------------
  # SAS: infile "&path.\mdhier.asc" dsd dlm='$' missover lrecl=32767;
  # Column types explicitly specified to match SAS INPUT statement:
  #   Numeric: pt_code, hlt_code, hlgt_code, soc_code, pt_soc_code
  #   Character: pt_name, hlt_name, hlgt_name, soc_name, soc_abbrev,
  #              null_field, primary_soc_fg
  mdhier_col_types <- readr::cols(
    X1  = readr::col_double(),     # pt_code
    X2  = readr::col_double(),     # hlt_code
    X3  = readr::col_double(),     # hlgt_code
    X4  = readr::col_double(),     # soc_code
    X5  = readr::col_character(),  # pt_name
    X6  = readr::col_character(),  # hlt_name
    X7  = readr::col_character(),  # hlgt_name
    X8  = readr::col_character(),  # soc_name
    X9  = readr::col_character(),  # soc_abbrev
    X10 = readr::col_character(),  # null_field
    X11 = readr::col_double(),     # pt_soc_code
    X12 = readr::col_character()   # primary_soc_fg
  )

  mdhier_raw <- readr::read_delim(
    file.path(path, "mdhier.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = mdhier_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE
  )

  # Assign column names matching SAS INPUT statement (lines 34-45)
  colnames(mdhier_raw) <- c(
    "pt_code", "hlt_code", "hlgt_code", "soc_code",
    "pt_name", "hlt_name", "hlgt_name", "soc_name",
    "soc_abbrev", "null_field", "pt_soc_code", "primary_soc_fg"
  )

  # Add derived columns (SAS lines 47-50)
  # ver = left("&ver.") — trimmed version label

  # aebodsys = upcase(soc_name) — uppercased SOC for AE matching
  # aedecod = upcase(pt_name) — uppercased PT for AE matching
  mdhier <- mdhier_raw %>%
    dplyr::mutate(
      ver      = ver_trimmed,
      aebodsys = stringr::str_to_upper(soc_name),
      aedecod  = stringr::str_to_upper(pt_name)
    ) %>%
    # SAS line 53: proc sort data=mdhier_&dsver.; by aebodsys aedecod; run;
    dplyr::arrange(aebodsys, aedecod)

  # ---------------------------------------------------------------------------
  # 2. Import pt.asc — Preferred Terms with cross-reference codes (SAS 56-81)
  # ---------------------------------------------------------------------------
  # SAS: infile "&path.\pt.asc" dsd dlm='$' missover lrecl=32767;
  pt_col_types <- readr::cols(
    X1  = readr::col_double(),     # pt_code
    X2  = readr::col_character(),  # pt_name
    X3  = readr::col_character(),  # null_field
    X4  = readr::col_double(),     # pt_soc_code
    X5  = readr::col_character(),  # pt_whoart_code
    X6  = readr::col_double(),     # pt_harts_code
    X7  = readr::col_character(),  # pt_costart_sym
    X8  = readr::col_character(),  # pt_icd9_code
    X9  = readr::col_character(),  # pt_icd9cm_code
    X10 = readr::col_character(),  # pt_icd10_code
    X11 = readr::col_character()   # pt_jart_code
  )

  pt_raw <- readr::read_delim(
    file.path(path, "pt.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = pt_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE
  )

  # Assign column names matching SAS INPUT statement (lines 67-77)
  colnames(pt_raw) <- c(
    "pt_code", "pt_name", "null_field", "pt_soc_code",
    "pt_whoart_code", "pt_harts_code", "pt_costart_sym",
    "pt_icd9_code", "pt_icd9cm_code", "pt_icd10_code", "pt_jart_code"
  )

  # Add ver column (SAS lines 79-80)
  pt <- pt_raw %>%
    dplyr::mutate(ver = ver_trimmed)

  # ---------------------------------------------------------------------------
  # 3. Import llt.asc — Lower Level Terms (SAS lines 84-109)
  # ---------------------------------------------------------------------------
  # SAS: infile "&path.\llt.asc" dsd dlm='$' missover lrecl=32767;
  llt_col_types <- readr::cols(
    X1  = readr::col_double(),     # llt_code
    X2  = readr::col_character(),  # llt_name
    X3  = readr::col_double(),     # pt_code
    X4  = readr::col_character(),  # llt_whoart_code
    X5  = readr::col_double(),     # llt_harts_code
    X6  = readr::col_character(),  # llt_costart_sym
    X7  = readr::col_character(),  # llt_icd9_code
    X8  = readr::col_character(),  # llt_icd9cm_code
    X9  = readr::col_character(),  # llt_icd10_code
    X10 = readr::col_character(),  # llt_currency
    X11 = readr::col_character()   # llt_jart_code
  )

  llt_raw <- readr::read_delim(
    file.path(path, "llt.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = llt_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE
  )

  # Assign column names matching SAS INPUT statement (lines 95-105)
  colnames(llt_raw) <- c(
    "llt_code", "llt_name", "pt_code",
    "llt_whoart_code", "llt_harts_code", "llt_costart_sym",
    "llt_icd9_code", "llt_icd9cm_code", "llt_icd10_code",
    "llt_currency", "llt_jart_code"
  )

  # Add ver column (SAS lines 107-108)
  llt <- llt_raw %>%
    dplyr::mutate(ver = ver_trimmed)

  # ---------------------------------------------------------------------------
  # 4. Combine PT and LLT — inner join on pt_code (SAS lines 112-120)
  # ---------------------------------------------------------------------------
  # SAS PROC SQL:
  #   select a.ver, a.pt_name, b.llt_name, a.pt_code, b.llt_code
  #   from pt a, llt b
  #   where a.pt_code = b.pt_code
  #   order by llt_name;
  #
  # Use suffix to disambiguate overlapping column names from the two tables.
  # Select ver and pt_name from the PT table (alias 'a' in SAS).
  # Overlapping non-key columns between pt and llt: "ver" only.
  # "pt_name" is unique to pt (no suffix); "llt_name" unique to llt.
  pt_llt <- dplyr::inner_join(
    pt, llt,
    by = "pt_code",
    suffix = c(".pt", ".llt")
  ) %>%
    dplyr::select(
      ver = ver.pt,
      pt_name,
      llt_name,
      pt_code,
      llt_code
    ) %>%
    dplyr::arrange(llt_name)

  # ---------------------------------------------------------------------------
  # 5. Create full SOC-to-LLT hierarchy (SAS lines 122-146)
  # ---------------------------------------------------------------------------
  # SAS PROC SQL:
  #   select b.llt_name, a.pt_name, a.hlt_name, a.hlgt_name, a.soc_name,
  #          a.soc_abbrev, a.null_field, a.primary_soc_fg, b.llt_code,
  #          a.pt_code, a.hlt_code, a.hlgt_code, a.soc_code,
  #          a.pt_soc_code, a.ver, a.aebodsys, a.aedecod
  #   from mdhier a, pt_llt b
  #   where a.pt_code = b.pt_code
  #   order by soc_name, hlgt_name, hlt_name, pt_name, llt_name;
  #
  # Inner join mdhier with pt_llt on pt_code. llt_name and llt_code come

  # from pt_llt; all other columns from mdhier.
  mdhier_llt <- dplyr::inner_join(
    mdhier, pt_llt,
    by = "pt_code",
    suffix = c(".mdhier", ".ptllt")
  ) %>%
    dplyr::select(
      llt_name,
      pt_name    = pt_name.mdhier,
      hlt_name,
      hlgt_name,
      soc_name,
      soc_abbrev,
      null_field,
      primary_soc_fg,
      llt_code,
      pt_code,
      hlt_code,
      hlgt_code,
      soc_code,
      pt_soc_code,
      ver        = ver.mdhier,
      aebodsys,
      aedecod
    ) %>%
    # SAS line 145: order by soc_name, hlgt_name, hlt_name, pt_name, llt_name
    dplyr::arrange(soc_name, hlgt_name, hlt_name, pt_name, llt_name)

  # ---------------------------------------------------------------------------
  # 6. Optionally save output as SAS7BDAT (SAS lines 148-155)
  # ---------------------------------------------------------------------------
  # SAS: data out.mdhier_&dsver.; set mdhier_&dsver.; run;
  #      data out.mdhier_llt_&dsver.; set mdhier_llt_&dsver.; run;
  if (!is.null(outpath)) {
    haven::write_sas(
      mdhier,
      file.path(outpath, paste0("mdhier_", dsver, ".sas7bdat"))
    )
    haven::write_sas(
      mdhier_llt,
      file.path(outpath, paste0("mdhier_llt_", dsver, ".sas7bdat"))
    )
    cli::cli_alert_info(
      "Output datasets written to {.path {outpath}}: mdhier_{dsver}.sas7bdat, mdhier_llt_{dsver}.sas7bdat"
    )
  }

  # SAS line 158: proc datasets delete pt llt — not needed in R;
  #   pt and llt are function-local and will be garbage collected.

  # Return both hierarchy datasets as a named list
  list(
    mdhier     = mdhier,
    mdhier_llt = mdhier_llt
  )
}


# ==============================================================================
# meddra_import
# ==============================================================================
#' Import MedDRA Hierarchy — Main Entry Point
#'
#' Validates that required MedDRA ASC files exist and that the output directory
#' (if specified) is valid, then delegates to \code{import_meddra_files()} for
#' the actual import and join logic.
#'
#' Migrated from SAS \code{%meddra} macro (lines 166-204 of
#' contributed/AE/meddra_import.sas).
#'
#' @param path    Character. Directory path containing the MedDRA ASC files
#'   (mdhier.asc, pt.asc, llt.asc).
#' @param ver     Character. MedDRA version string (e.g., "22.0").
#' @param outpath Character or NULL. Directory path for SAS7BDAT output files.
#'   Set to NULL (default) to skip file output.
#'
#' @return A named list with two tibbles: \code{mdhier} and \code{mdhier_llt}.
#'   See \code{import_meddra_files()} for details.
#'
#' @examples
#' \dontrun{
#' # Import MedDRA v22.0 from a local directory
#' result <- meddra_import(
#'   path    = "/data/meddra/MedAscii",
#'   ver     = "22.0",
#'   outpath = "/output/sas_datasets"
#' )
#' # Access the hierarchy data frames
#' head(result$mdhier)
#' head(result$mdhier_llt)
#' }
meddra_import <- function(path, ver, outpath = NULL) {

  # SAS line 169: %put IMPORTING MEDDRA VERSION &ver.;
  cli::cli_alert_info("IMPORTING MEDDRA VERSION {ver}")

  # ---------------------------------------------------------------------------
  # File existence checks (SAS lines 172-185)
  # ---------------------------------------------------------------------------
  # SAS:
  #   mdhier_exist = fileexist("&path.\mdhier.asc");
  #   pt_exist     = fileexist("&path.\pt.asc");
  #   llt_exist    = fileexist("&path.\llt.asc");
  #   if mdhier_exist and pt_exist and llt_exist then file_exist = 1;
  required_files <- c("mdhier.asc", "pt.asc", "llt.asc")
  file_paths     <- file.path(path, required_files)
  files_exist    <- file.exists(file_paths)

  if (!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    cli::cli_abort(
      c(
        "THE REQUIRED MEDDRA FILES DO NOT EXIST IN THE SPECIFIED DIRECTORY: {.path {path}}",
        "x" = "Missing file(s): {.file {missing_files}}"
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Output path validation (SAS lines 187-196)
  # ---------------------------------------------------------------------------
  # SAS: out_exist = ifn(libref('out')=0,1,0);
  #      if not out_exist → error
  if (!is.null(outpath)) {
    if (!dir.exists(outpath)) {
      cli::cli_abort(
        "{.path {toupper(outpath)}} DOES NOT EXIST"
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Delegate to import_meddra_files (SAS line 198: %import;)
  # ---------------------------------------------------------------------------
  result <- tryCatch(
    import_meddra_files(path = path, ver = ver, outpath = outpath),
    error = function(e) {
      cli::cli_abort(
        c(
          "Error during MedDRA file import",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  result
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - MedDRA ASC files are $-delimited with no header row (DSD format)
####    - Column order in ASC files matches SAS infile INPUT specifications exactly
####    - File encoding is ASCII/UTF-8 compatible with readr defaults
####    - SAS character variable lengths ($100, $5, $1) are not enforced in R
####      (R character vectors are unbounded; no truncation applied)
####    - SAS MISSOVER option maps to readr's default NA handling for short rows
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- this is data import/join only, no computation
#### NO DIRECT R EQUIVALENT:
####    - SAS infile DSD DLM='$' MISSOVER LRECL=32767 -> readr::read_delim(delim = "$")
####    - SAS libname/libref validation -> dir.exists()
####    - SAS FILEEXIST function -> file.exists()
####    - SAS libname out "&outpath." -> haven::write_sas() for SAS dataset output
####    - SAS proc sort -> dplyr::arrange()
####    - SAS proc sql inner join -> dplyr::inner_join()
####    - SAS options mprint mlogic -> no R equivalent (diagnostic tracing not needed)
####    - SAS %goto exit error flow -> tryCatch + cli::cli_abort
#### PACKAGE SELECTION RATIONALE:
####    - readr: Efficient delimited file reading with type inference; replaces SAS infile
####    - haven: SAS dataset output compatibility (write_sas) for interop with SAS consumers
####    - dplyr: Joins and sorting replacing PROC SQL and PROC SORT
####    - cli: User-facing messages replacing SAS %put statements
####    - stringr: String manipulation for version translation and text case conversion
#### OPEN QUESTIONS:
####    - Confirm MedDRA ASC file encoding (ASCII vs UTF-8) matches readr defaults
####    - Verify column order in ASC files is identical across MedDRA versions
####    - Whether haven::write_sas() output is byte-compatible with SAS-produced .sas7bdat
#### ============================================================
