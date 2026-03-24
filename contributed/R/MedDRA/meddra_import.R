# ==============================================================================
# meddra_import.R - MedDRA Hierarchy ASC File Import Utility
# ==============================================================================
#
# DESCRIPTION:
#   Imports MedDRA hierarchy text files in ASC format ($-delimited), builds
#   the SOC-to-LLT hierarchy table, and optionally saves output as SAS7BDAT.
#   This is a standalone import module with no internal dependencies.
#
# SOURCE:
#   Migrated from: contributed/MedDRA/meddra_import.sas (204 lines)
#
# EXPORTS:
#   import_meddra_files() - Core import function that reads ASC files and
#                           builds hierarchy datasets
#   meddra_import()       - Main entry point with validation and error handling
#
# USAGE:
#   source("contributed/R/MedDRA/meddra_import.R")
#   result <- meddra_import(
#     path    = "/path/to/meddra/asc/files",
#     ver     = "22.0",
#     outpath = "/path/to/output"
#   )
#   # result$mdhier     - SOC/HLGT/HLT/PT hierarchy
#   # result$mdhier_llt - Full SOC-to-LLT hierarchy
#   # result$pt         - Preferred Terms with cross-reference codes
#   # result$llt        - Lowest Level Terms with cross-reference codes
#
# ==============================================================================

# --- Library Dependencies ---
library(readr)
library(dplyr)
library(haven)
library(stringr)
library(cli)

# ==============================================================================
# import_meddra_files()
# ==============================================================================
# Core import function replacing SAS %import macro (lines 18-160).
# Reads MedDRA ASC hierarchy files (mdhier.asc, pt.asc, llt.asc), constructs
# SOC-to-LLT hierarchy, and optionally writes output to SAS7BDAT format.
#
# SAS construct mapping:
#   - infile DSD DLM='$' MISSOVER LRECL=32767 -> readr::read_delim(delim = "$")
#   - DATA step column assignment -> dplyr::mutate()
#   - PROC SORT -> dplyr::arrange()
#   - PROC SQL inner joins -> dplyr::inner_join()
#   - DATA out.dataset; SET dataset; RUN; -> haven::write_sas()
#   - PROC DATASETS DELETE -> not needed (R garbage collection)
#
# @param path     Character. Directory containing MedDRA ASC files
#                 (mdhier.asc, pt.asc, llt.asc). No default - must be specified.
# @param ver      Character. MedDRA version string (e.g., "22.0").
# @param outpath  Character or NULL. Output directory for SAS7BDAT files.
#                 If NULL, datasets are returned but not written to disk.
#
# @return A named list with components:
#   \item{mdhier}{tibble - SOC/HLGT/HLT/PT hierarchy with ver/aebodsys/aedecod}
#   \item{mdhier_llt}{tibble - Full SOC-to-LLT hierarchy}
#   \item{pt}{tibble - Preferred Terms with cross-reference codes}
#   \item{llt}{tibble - Lowest Level Terms with cross-reference codes}
# ==============================================================================
import_meddra_files <- function(path, ver, outpath = NULL) {

  # --------------------------------------------------------------------------
  # Derive dataset version suffix: dots -> underscores
  # SAS line 11: %let dsver = %sysfunc(translate(&ver.,'_','.'));
  # --------------------------------------------------------------------------
  dsver <- stringr::str_replace_all(ver, "\\.", "_")

  # Trim version string for embedding in datasets
  # SAS lines 48, 80, 108: ver = left("&ver.");
  ver_trimmed <- stringr::str_trim(ver)

  cli::cli_alert_info("READING IN TEXT FILES AND CREATING R DATASETS")

  # ==========================================================================
  # 1. Import mdhier.asc - SOC/HLGT/HLT/PT hierarchy (SAS lines 25-53)
  # ==========================================================================
  # SAS: infile "&path.\mdhier.asc" dsd dlm='$' missover lrecl=32767;
  # 12 columns with SAS types:
  #   pt_code (numeric), hlt_code (numeric), hlgt_code (numeric),
  #   soc_code (numeric), pt_name ($100), hlt_name ($100),
  #   hlgt_name ($100), soc_name ($100), soc_abbrev ($5),
  #   null_field ($1), pt_soc_code (numeric), primary_soc_fg ($1)
  mdhier_col_names <- c(
    "pt_code", "hlt_code", "hlgt_code", "soc_code",
    "pt_name", "hlt_name", "hlgt_name", "soc_name",
    "soc_abbrev", "null_field", "pt_soc_code", "primary_soc_fg"
  )

  mdhier_col_types <- readr::cols(
    X1  = readr::col_double(),    # pt_code (numeric)
    X2  = readr::col_double(),    # hlt_code (numeric)
    X3  = readr::col_double(),    # hlgt_code (numeric)
    X4  = readr::col_double(),    # soc_code (numeric)
    X5  = readr::col_character(), # pt_name ($100)
    X6  = readr::col_character(), # hlt_name ($100)
    X7  = readr::col_character(), # hlgt_name ($100)
    X8  = readr::col_character(), # soc_name ($100)
    X9  = readr::col_character(), # soc_abbrev ($5)
    X10 = readr::col_character(), # null_field ($1)
    X11 = readr::col_double(),    # pt_soc_code (numeric)
    X12 = readr::col_character()  # primary_soc_fg ($1)
  )

  mdhier_raw <- readr::read_delim(
    file           = file.path(path, "mdhier.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = mdhier_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE,
    na             = c("", "NA")
  )

  # Assign column names matching SAS variable names
  colnames(mdhier_raw) <- mdhier_col_names

  # Add derived columns (SAS lines 47-50) and sort (SAS line 53)
  # SAS: ver = left("&ver."); aebodsys = upcase(soc_name); aedecod = upcase(pt_name);
  # SAS: proc sort data=mdhier; by aebodsys aedecod; run;
  mdhier <- mdhier_raw %>%
    dplyr::mutate(
      ver      = ver_trimmed,
      aebodsys = stringr::str_to_upper(soc_name),
      aedecod  = stringr::str_to_upper(pt_name)
    ) %>%
    dplyr::arrange(aebodsys, aedecod)

  cli::cli_alert_info("Imported mdhier.asc: {nrow(mdhier)} rows")

  # ==========================================================================
  # 2. Import pt.asc - Preferred Term level (SAS lines 56-81)
  # ==========================================================================
  # SAS: infile "&path.\pt.asc" dsd dlm='$' missover lrecl=32767;
  # 11 columns with SAS types:
  #   pt_code (numeric), pt_name ($100), null_field ($1),
  #   pt_soc_code (numeric), pt_whoart_code ($7), pt_harts_code (numeric),
  #   pt_costart_sym ($21), pt_icd9_code ($8), pt_icd9cm_code ($8),
  #   pt_icd10_code ($8), pt_jart_code ($6)
  pt_col_names <- c(
    "pt_code", "pt_name", "null_field", "pt_soc_code",
    "pt_whoart_code", "pt_harts_code", "pt_costart_sym",
    "pt_icd9_code", "pt_icd9cm_code", "pt_icd10_code", "pt_jart_code"
  )

  pt_col_types <- readr::cols(
    X1  = readr::col_double(),    # pt_code (numeric)
    X2  = readr::col_character(), # pt_name ($100)
    X3  = readr::col_character(), # null_field ($1)
    X4  = readr::col_double(),    # pt_soc_code (numeric)
    X5  = readr::col_character(), # pt_whoart_code ($7)
    X6  = readr::col_double(),    # pt_harts_code (numeric)
    X7  = readr::col_character(), # pt_costart_sym ($21)
    X8  = readr::col_character(), # pt_icd9_code ($8)
    X9  = readr::col_character(), # pt_icd9cm_code ($8)
    X10 = readr::col_character(), # pt_icd10_code ($8)
    X11 = readr::col_character()  # pt_jart_code ($6)
  )

  pt_raw <- readr::read_delim(
    file           = file.path(path, "pt.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = pt_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE,
    na             = c("", "NA")
  )

  colnames(pt_raw) <- pt_col_names

  # Add version column (SAS line 80: ver = left("&ver.");)
  pt <- pt_raw %>%
    dplyr::mutate(ver = ver_trimmed)

  cli::cli_alert_info("Imported pt.asc: {nrow(pt)} rows")

  # ==========================================================================
  # 3. Import llt.asc - Lowest Level Terms (SAS lines 84-109)
  # ==========================================================================
  # SAS: infile "&path.\llt.asc" dsd dlm='$' missover lrecl=32767;
  # 11 columns with SAS types:
  #   llt_code (numeric), llt_name ($100), pt_code (numeric),
  #   llt_whoart_code ($7), llt_harts_code (numeric),
  #   llt_costart_sym ($21), llt_icd9_code ($8), llt_icd9cm_code ($8),
  #   llt_icd10_code ($8), llt_currency ($1), llt_jart_code ($6)
  llt_col_names <- c(
    "llt_code", "llt_name", "pt_code", "llt_whoart_code", "llt_harts_code",
    "llt_costart_sym", "llt_icd9_code", "llt_icd9cm_code",
    "llt_icd10_code", "llt_currency", "llt_jart_code"
  )

  llt_col_types <- readr::cols(
    X1  = readr::col_double(),    # llt_code (numeric)
    X2  = readr::col_character(), # llt_name ($100)
    X3  = readr::col_double(),    # pt_code (numeric)
    X4  = readr::col_character(), # llt_whoart_code ($7)
    X5  = readr::col_double(),    # llt_harts_code (numeric)
    X6  = readr::col_character(), # llt_costart_sym ($21)
    X7  = readr::col_character(), # llt_icd9_code ($8)
    X8  = readr::col_character(), # llt_icd9cm_code ($8)
    X9  = readr::col_character(), # llt_icd10_code ($8)
    X10 = readr::col_character(), # llt_currency ($1)
    X11 = readr::col_character()  # llt_jart_code ($6)
  )

  llt_raw <- readr::read_delim(
    file           = file.path(path, "llt.asc"),
    delim          = "$",
    col_names      = FALSE,
    col_types      = llt_col_types,
    show_col_types = FALSE,
    trim_ws        = TRUE,
    na             = c("", "NA")
  )

  colnames(llt_raw) <- llt_col_names

  # Add version column (SAS line 108: ver = left("&ver.");)
  llt <- llt_raw %>%
    dplyr::mutate(ver = ver_trimmed)

  cli::cli_alert_info("Imported llt.asc: {nrow(llt)} rows")

  # ==========================================================================
  # 4. Combine PT and LLT (SAS lines 112-120)
  # ==========================================================================
  # SAS PROC SQL:
  #   create table pt_llt as
  #   select a.ver, a.pt_name, b.llt_name, a.pt_code, b.llt_code
  #   from pt a, llt b
  #   where a.pt_code = b.pt_code
  #   order by llt_name;
  #
  # Pre-select only needed columns from each table to avoid name collisions.
  # PT -> LLT is one-to-many (each PT maps to multiple LLTs).
  pt_for_join <- pt %>%
    dplyr::select(ver, pt_name, pt_code)

  llt_for_join <- llt %>%
    dplyr::select(llt_name, pt_code, llt_code)

  pt_llt <- dplyr::inner_join(
    pt_for_join, llt_for_join,
    by = "pt_code",
    relationship = "one-to-many"
  ) %>%
    dplyr::select(ver, pt_name, llt_name, pt_code, llt_code) %>%
    dplyr::arrange(llt_name)

  cli::cli_alert_info("Combined PT-LLT mapping: {nrow(pt_llt)} rows")

  # ==========================================================================
  # 5. Create full MedDRA hierarchy: SOC to LLT (SAS lines 122-146)
  # ==========================================================================
  # SAS PROC SQL:
  #   create table mdhier_llt as
  #   select b.llt_name, a.pt_name, a.hlt_name, a.hlgt_name, a.soc_name,
  #          a.soc_abbrev, a.null_field, a.primary_soc_fg,
  #          b.llt_code, a.pt_code, a.hlt_code, a.hlgt_code, a.soc_code,
  #          a.pt_soc_code, a.ver, a.aebodsys, a.aedecod
  #   from mdhier a, pt_llt b
  #   where a.pt_code = b.pt_code
  #   order by soc_name, hlgt_name, hlt_name, pt_name, llt_name;
  #
  # Pre-select only needed columns from pt_llt to avoid name collisions.
  # mdhier -> pt_llt is many-to-many (one PT may appear under multiple SOCs,
  # and one PT may map to multiple LLTs).
  pt_llt_for_hier <- pt_llt %>%
    dplyr::select(llt_name, llt_code, pt_code)

  mdhier_llt <- dplyr::inner_join(
    mdhier, pt_llt_for_hier,
    by = "pt_code",
    relationship = "many-to-many"
  ) %>%
    dplyr::select(
      llt_name, pt_name, hlt_name, hlgt_name, soc_name,
      soc_abbrev, null_field, primary_soc_fg,
      llt_code, pt_code, hlt_code, hlgt_code, soc_code,
      pt_soc_code, ver, aebodsys, aedecod
    ) %>%
    dplyr::arrange(soc_name, hlgt_name, hlt_name, pt_name, llt_name)

  cli::cli_alert_info("Created full SOC-to-LLT hierarchy: {nrow(mdhier_llt)} rows")

  # ==========================================================================
  # 6. Save output to SAS7BDAT if outpath specified (SAS lines 148-155)
  # ==========================================================================
  # SAS: data out.mdhier_&dsver.; set mdhier_&dsver.; run;
  # SAS: data out.mdhier_llt_&dsver.; set mdhier_llt_&dsver.; run;
  if (!is.null(outpath)) {
    mdhier_outfile <- file.path(outpath, paste0("mdhier_", dsver, ".sas7bdat"))
    haven::write_sas(mdhier, mdhier_outfile)
    cli::cli_alert_info("Saved mdhier to: {mdhier_outfile}")

    mdhier_llt_outfile <- file.path(
      outpath, paste0("mdhier_llt_", dsver, ".sas7bdat")
    )
    haven::write_sas(mdhier_llt, mdhier_llt_outfile)
    cli::cli_alert_info("Saved mdhier_llt to: {mdhier_llt_outfile}")
  }

  # SAS line 158: proc datasets library=work nolist nodetails;
  #               delete pt_&dsver. llt_&dsver.; quit;
  # Not needed in R - temporary objects are garbage collected automatically.

  # Return all datasets as a named list for direct use in R workflows
  list(
    mdhier     = mdhier,
    mdhier_llt = mdhier_llt,
    pt         = pt,
    llt        = llt
  )
}


# ==============================================================================
# meddra_import()
# ==============================================================================
# Main entry point replacing SAS %meddra wrapper macro (lines 166-204).
# Validates input paths and file existence, then delegates to
# import_meddra_files() for the actual import work.
#
# SAS construct mapping:
#   - %put -> cli::cli_alert_info()
#   - FILEEXIST() -> file.exists()
#   - libref('out') check -> dir.exists()
#   - %let err_msg = ... ; %goto exit; -> cli::cli_abort()
#   - %import; -> import_meddra_files()
#   - %exit: %if %symexist(err_msg) -> tryCatch() error handling
#
# @param path     Character. Directory containing MedDRA ASC files.
# @param ver      Character. MedDRA version string (e.g., "22.0").
# @param outpath  Character or NULL. Output directory for SAS7BDAT files.
#                 If NULL, datasets are returned but not written to disk.
#
# @return A named list with mdhier, mdhier_llt, pt, llt tibbles.
#         See import_meddra_files() for detailed component descriptions.
# ==============================================================================
meddra_import <- function(path, ver, outpath = NULL) {

  # Log version being imported (SAS line 169: %put IMPORTING MEDDRA VERSION &ver.;)
  cli::cli_alert_info("IMPORTING MEDDRA VERSION {ver}")

  # --------------------------------------------------------------------------
  # File existence checks (SAS lines 172-185)
  # --------------------------------------------------------------------------
  # SAS: data _null_;
  #        mdhier_exist = fileexist("&path.\mdhier.asc");
  #        pt_exist     = fileexist("&path.\pt.asc");
  #        llt_exist    = fileexist("&path.\llt.asc");
  #        if mdhier_exist and pt_exist and llt_exist then file_exist = 1;
  #        else file_exist = 0;
  #        call symputx('file_exist', file_exist, 'g');
  #      run;
  #      %if not &file_exist. %then %do;
  #        %let err_msg = THE REQUIRED MEDDRA FILES DO NOT EXIST ...;
  #        %goto exit;
  #      %end;
  required_files <- c("mdhier.asc", "pt.asc", "llt.asc")
  file_paths <- file.path(path, required_files)
  files_exist <- file.exists(file_paths)

  if (!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    cli::cli_abort(c(
      paste0(
        "THE REQUIRED MEDDRA FILES DO NOT EXIST IN THE SPECIFIED ",
        "DIRECTORY: {path}"
      ),
      "x" = "Missing file(s): {paste(missing_files, collapse = ', ')}"
    ))
  }

  # --------------------------------------------------------------------------
  # Output path validation (SAS lines 187-196)
  # --------------------------------------------------------------------------
  # SAS: data _null_;
  #        out_exist = ifn(libref('out')=0, 1, 0);
  #        call symputx('out_exist', out_exist, 'g');
  #      run;
  #      %if not &out_exist. %then %do;
  #        %let err_msg = %upcase(&outpath.) DOES NOT EXIST;
  #        %goto exit;
  #      %end;
  if (!is.null(outpath)) {
    if (!dir.exists(outpath)) {
      cli::cli_abort(c(
        "{stringr::str_to_upper(outpath)} DOES NOT EXIST",
        "i" = "Please create the output directory before running the import."
      ))
    }
  }

  # --------------------------------------------------------------------------
  # Execute import (SAS line 198: %import;)
  # --------------------------------------------------------------------------
  # SAS line 200: %exit: %if %symexist(err_msg) %then %put ERROR: &err_msg.;
  # Wrap in tryCatch for comprehensive error handling matching the
  # SAS %goto exit + %symexist(err_msg) error propagation pattern
  result <- tryCatch(
    {
      import_meddra_files(path = path, ver = ver, outpath = outpath)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Error during MedDRA import for version {ver}",
        "x" = conditionMessage(e)
      ))
    }
  )

  cli::cli_alert_info("MedDRA version {ver} import completed successfully")

  result
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    1. MedDRA ASC files are $-delimited with no header row
####       (SAS DSD format); column order matches the SAS infile
####       specifications exactly:
####       - mdhier.asc: 12 columns (pt_code, hlt_code, hlgt_code,
####         soc_code, pt_name, hlt_name, hlgt_name, soc_name,
####         soc_abbrev, null_field, pt_soc_code, primary_soc_fg)
####       - pt.asc: 11 columns (pt_code, pt_name, null_field,
####         pt_soc_code, pt_whoart_code, pt_harts_code,
####         pt_costart_sym, pt_icd9_code, pt_icd9cm_code,
####         pt_icd10_code, pt_jart_code)
####       - llt.asc: 11 columns (llt_code, llt_name, pt_code,
####         llt_whoart_code, llt_harts_code, llt_costart_sym,
####         llt_icd9_code, llt_icd9cm_code, llt_icd10_code,
####         llt_currency, llt_jart_code)
####    2. File encoding is ASCII or UTF-8, compatible with
####       readr's default locale
####    3. SAS options mprint mlogic (line 13) are macro debugging
####       options with no R equivalent; R uses standard debug tools
####    4. MedDRA codes (pt_code, hlt_code, etc.) are numeric
####       integers stored as doubles for SAS compatibility
####    5. PT-to-LLT mapping is one-to-many (one PT has many LLTs);
####       mdhier-to-pt_llt is many-to-many (one PT can appear
####       under multiple SOCs via primary/secondary pathways)
####
#### POTENTIAL NUMERICAL DIFFERENCES:
####    None - this is a data import utility only. No statistical
####    computations or rounding operations are performed.
####
#### NO DIRECT R EQUIVALENT:
####    1. SAS infile DSD DLM='$' MISSOVER LRECL=32767
####       -> readr::read_delim(delim = "$") handles missing values
####       naturally as NA and has no line length restriction
####    2. SAS libname/libref('out') validation
####       -> dir.exists() checks output directory accessibility
####    3. SAS FILEEXIST() function
####       -> file.exists() provides equivalent path validation
####    4. SAS proc datasets library=work nolist nodetails; delete;
####       -> Not needed in R; temporary objects are garbage collected
####    5. SAS character length declarations ($100, $5, $1)
####       -> R character vectors are unbounded; no truncation occurs
####    6. SAS left() function for string left-trimming
####       -> stringr::str_trim() trims both sides (more thorough)
####    7. SAS %sysfunc(translate(&ver.,'_','.'))
####       -> stringr::str_replace_all(ver, "\\.", "_")
####
#### PACKAGE SELECTION RATIONALE:
####    1. readr (>=2.1.0) - Efficient delimited file reading with
####       explicit column type specification via col_types; handles
####       $-delimited ASC files with automatic NA handling replacing
####       SAS MISSOVER behavior. Chosen over utils::read.delim for
####       consistency with tidyverse ecosystem (AAP mandate).
####    2. haven (>=2.5.0) - SAS7BDAT output compatibility for
####       downstream SAS consumers using write_sas(); part of
####       tidyverse I/O ecosystem mandated by AAP.
####    3. dplyr (>=1.1.0) - Idiomatic tidyverse joins (inner_join)
####       replacing SAS PROC SQL, column selection (select), sorting
####       (arrange), and derived column creation (mutate). AAP
####       mandates tidyverse over base R equivalents.
####    4. stringr (>=1.5.0) - Tidyverse string manipulation
####       (str_replace_all, str_to_upper, str_trim) replacing SAS
####       translate(), upcase(), and left() functions. AAP mandates
####       tidyverse over base R gsub/toupper/trimws.
####    5. cli (>=3.6.0) - User-facing formatted messages
####       (cli_alert_info, cli_abort) replacing SAS %put and
####       %let err_msg + %goto exit pattern for structured error
####       handling with informative context.
####
#### OPEN QUESTIONS:
####    1. Confirm MedDRA ASC file encoding (ASCII vs UTF-8 vs
####       Latin-1) matches readr default assumption (UTF-8);
####       may need locale = locale(encoding = "latin1") for
####       older MedDRA distributions
####    2. Verify column order in ASC files across all MedDRA
####       versions (v15+ through v27+) matches the SAS infile
####       specification used here
####    3. SAS DSD option implies quoted fields may contain the
####       delimiter - verify if MedDRA ASC files use quoting
####       (readr handles quoted fields by default)
#### ============================================================
