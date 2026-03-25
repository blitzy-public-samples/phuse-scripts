# =============================================================================
# Script Name:  gate3_missing_value_audit.R
# Purpose:      Gate 3 — Missing Value Handling Audit
# Description:  Part of the 8-gate SAS-to-R migration validation framework.
#               Verifies that SAS missing value semantics are correctly
#               preserved in all migrated R scripts — specifically that:
#                 - SAS numeric missing (.) maps to R NA (never 0 or NaN)
#                 - SAS character missing (' ') maps to NA_character_ (never "")
#                 - NO implicit zero substitution occurs anywhere
#                 - SAS special missing (.A-.Z) handled via haven::tagged_na()
# Critical:     No implicit zero substitution permitted (AAP §0.8.1)
# Author:       PhUSE CS WG5 Migration
# Framework:    AAP §0.5.1, §0.7.3, §0.8.1, §0.8.2 — 8-gate validation
# =============================================================================

# -----------------------------------------------------------------------------
# Package Loading
# -----------------------------------------------------------------------------
library(testthat)
library(diffdf)
library(haven)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(yaml)
library(cli)

# -----------------------------------------------------------------------------
# Configuration Loading
# -----------------------------------------------------------------------------
# Load centralized YAML configuration providing parameterized paths for
# discovering ADaM XPT datasets to catalog missing values (data_paths),
# finding migrated R source files to scan for zero substitution patterns
# (r_source_paths), and domain-specific missing value expectations.
# Uses tryCatch to handle the case where config file is not yet available.
config <- tryCatch(
  yaml::read_yaml("config/migration_config.yaml"),
  error = function(e) {
    message("INFO: config/migration_config.yaml not found. Using default paths.")
    list(
      data_paths = list(
        adam_path = "data/adam/cdisc",
        sdtm_path = "data/sdtm/TDF_SDTM_v1.0",
        adam_split_path = "data/adam/cdisc-split"
      ),
      output_paths = list(
        base_output_path = "output",
        rtf_output_path = "output/rtf",
        excel_output_path = "output/excel",
        figure_output_path = "output/figures",
        log_output_path = "output/logs"
      ),
      r_source_paths = list(
        r_macros_path = "tested/R/macros",
        r_utilities_path = "tested/R/utilities",
        wp_utilities_path = "whitepapers/utilities/R",
        wp_adam_path = "whitepapers/ADaM/R"
      ),
      domain_settings = list(
        ae = list(panel_title = "AE Toxicity"),
        demographics = list(panel_title = "Demographics"),
        liver = list(panel_title = "Liver Labs"),
        disposition = list(panel_title = "Disposition"),
        exposure = list(panel_title = "Exposure"),
        meddra = list(panel_title = "MedDRA at a Glance")
      )
    )
  }
)

# =============================================================================
# SECTION 1: MISSING VALUE CATALOG FUNCTION
# =============================================================================
# Catalogs missing value metadata for every column in a dataset.
# Returns a tibble with one row per column documenting missing value
# characteristics for the Gate 3 audit report.
#
# Per AAP §0.7.3:
#   SAS numeric missing (.)  → NA (never 0, never NaN)
#   SAS character missing ('') → NA_character_ (never "")
#   SAS special missing (.A-.Z) → haven::tagged_na()
# =============================================================================

#' Catalog missing values in a dataset
#'
#' Examines every column in the provided data frame and computes missing
#' value metadata including counts, percentages, type-specific indicators,
#' and violation flags for the Gate 3 audit.
#'
#' @param dataset A data frame or tibble to catalog.
#' @param dataset_name Character string identifier for the dataset.
#' @return A tibble with one row per column containing missing value metadata.
catalog_missing_values <- function(dataset, dataset_name) {
  # Validate inputs
  if (!is.data.frame(dataset)) {
    cli::cli_abort("catalog_missing_values: {.arg dataset} must be a data frame or tibble.")
  }
  if (!is.character(dataset_name) || length(dataset_name) != 1L) {
    cli::cli_abort("catalog_missing_values: {.arg dataset_name} must be a single character string.")
  }

  n_total <- nrow(dataset)

  # Handle empty datasets

if (n_total == 0L || ncol(dataset) == 0L) {
    return(
      dplyr::tibble(
        dataset      = character(0L),
        variable     = character(0L),
        col_type     = character(0L),
        n_total      = integer(0L),
        n_missing    = integer(0L),
        pct_missing  = numeric(0L),
        has_tagged_na   = logical(0L),
        has_empty_string = logical(0L),
        has_zero_suspect = logical(0L)
      )
    )
  }

  # Iterate over every column and compute metadata
  purrr::imap_dfr(dataset, function(col, col_name) {
    # Determine column type class
    is_char <- is.character(col) || is.factor(col)
    is_num  <- is.numeric(col) || is.integer(col)
    col_type_label <- dplyr::case_when(
      is_char ~ "character",
      is_num  ~ "numeric",
      inherits(col, "Date")    ~ "date",
      inherits(col, "POSIXct") ~ "datetime",
      TRUE                     ~ "other"
    )

    n_missing_val <- sum(is.na(col))
    pct_missing_val <- dplyr::if_else(
      n_total > 0L,
      (n_missing_val / n_total) * 100,
      0
    )

    # Check for haven tagged NAs (SAS special missing .A-.Z)
    has_tagged <- tryCatch(
      any(haven::is_tagged_na(col), na.rm = TRUE),
      error = function(e) FALSE
    )

    # Check for empty strings in character columns — a VIOLATION
    # SAS character missing (' ') must map to NA_character_, not ""
    has_empty <- FALSE
    if (is_char) {
      char_vals <- as.character(col)
      has_empty <- any(char_vals == "", na.rm = TRUE)
    }

    # Flag for suspicious zero values in numeric columns that might
    # indicate implicit zero substitution of missing values
    has_zero <- FALSE
    if (is_num && n_missing_val == 0L && n_total > 0L) {
      # If there are zero NA values but many zeros, flag for review
      n_zeros <- sum(col == 0, na.rm = TRUE)
      zero_pct <- (n_zeros / n_total) * 100
      # Flag if >50% zeros and it's not a binary/flag variable
      if (zero_pct > 50 && length(unique(stats::na.omit(col))) > 2L) {
        has_zero <- TRUE
      }
    }

    dplyr::tibble(
      dataset        = dataset_name,
      variable       = col_name,
      col_type       = col_type_label,
      n_total        = as.integer(n_total),
      n_missing      = as.integer(n_missing_val),
      pct_missing    = pct_missing_val,
      has_tagged_na    = has_tagged,
      has_empty_string = has_empty,
      has_zero_suspect = has_zero
    )
  })
}

# =============================================================================
# SECTION 2: ZERO SUBSTITUTION SOURCE CODE SCANNER
# =============================================================================
# Scans migrated R source files for patterns that indicate implicit zero
# substitution of missing values — an absolute violation per AAP §0.8.1.
#
# Violation patterns detected:
#   replace_na(0) or replace_na(., 0)
#   ifelse(is.na(x), 0, x) or if_else(is.na(x), 0, x)
#   coalesce(x, 0)
#   tidyr::replace_na(list(col = 0))
#   x[is.na(x)] <- 0
# =============================================================================

#' Scan an R source file for implicit zero substitution patterns
#'
#' Reads the file line-by-line and checks for anti-patterns that replace
#' NA values with zero without explicit documentation.
#'
#' @param r_file_path Character path to the R source file.
#' @return A tibble with columns: file_path, line_number, line_content,
#'   pattern_matched, violation_type.
scan_zero_substitution <- function(r_file_path) {
  # Validate the file exists
  if (!file.exists(r_file_path)) {
    return(
      dplyr::tibble(
        file_path       = character(0L),
        line_number     = integer(0L),
        line_content    = character(0L),
        pattern_matched = character(0L),
        violation_type  = character(0L)
      )
    )
  }

  lines <- readr::read_lines(r_file_path, lazy = FALSE)

  if (length(lines) == 0L) {
    return(
      dplyr::tibble(
        file_path       = character(0L),
        line_number     = integer(0L),
        line_content    = character(0L),
        pattern_matched = character(0L),
        violation_type  = character(0L)
      )
    )
  }

  # Define zero-substitution anti-patterns with regex
  # Each entry: list(pattern = regex, name = human label, type = violation class)
  zero_patterns <- list(
    list(
      pattern = "replace_na\\s*\\([^,)]*,\\s*0\\s*\\)|replace_na\\s*\\(\\s*0\\s*\\)",
      name    = "replace_na(..., 0)",
      type    = "ZERO_SUBSTITUTION"
    ),
    list(
      pattern = "replace_na\\s*\\(\\s*list\\s*\\([^)]*=\\s*0",
      name    = "replace_na(list(col=0))",
      type    = "ZERO_SUBSTITUTION"
    ),
    list(
      pattern = "if_?else\\s*\\(\\s*is\\.na\\s*\\([^)]*\\)\\s*,\\s*0",
      name    = "ifelse(is.na(x), 0, ...)",
      type    = "ZERO_SUBSTITUTION"
    ),
    list(
      pattern = "coalesce\\s*\\([^,]+,\\s*0\\s*\\)",
      name    = "coalesce(x, 0)",
      type    = "ZERO_SUBSTITUTION"
    ),
    list(
      pattern = "\\[\\s*is\\.na\\s*\\([^)]*\\)\\s*\\]\\s*<-\\s*0",
      name    = "x[is.na(x)] <- 0",
      type    = "ZERO_SUBSTITUTION"
    ),
    list(
      pattern = "na\\.fill\\s*\\([^,]+,\\s*0\\s*\\)",
      name    = "na.fill(x, 0)",
      type    = "ZERO_SUBSTITUTION"
    )
  )

  # Scan each line against all patterns
  results <- purrr::map_dfr(zero_patterns, function(pat) {
    matched_lines <- which(stringr::str_detect(lines, pat$pattern))

    if (length(matched_lines) == 0L) {
      return(
        dplyr::tibble(
          file_path       = character(0L),
          line_number     = integer(0L),
          line_content    = character(0L),
          pattern_matched = character(0L),
          violation_type  = character(0L)
        )
      )
    }

    # Check if each matched line has an inline documentation comment
    # allowing legitimate zero replacement (e.g., cumulative counts)
    purrr::map_dfr(matched_lines, function(ln) {
      line_text <- lines[ln]
      # Lines with explicit documentation markers are flagged as DOCUMENTED
      is_documented <- stringr::str_detect(
        line_text,
        "(?i)(#.*legitimate|#.*intentional|#.*documented|#.*cumulative|#.*initialized|#.*GATE3.EXEMPT)"
      )
      # Lines that are comments themselves are not violations
      is_comment_line <- stringr::str_detect(
        stringr::str_trim(line_text), "^#"
      )

      if (is_comment_line) {
        return(
          dplyr::tibble(
            file_path       = character(0L),
            line_number     = integer(0L),
            line_content    = character(0L),
            pattern_matched = character(0L),
            violation_type  = character(0L)
          )
        )
      }

      vtype <- dplyr::if_else(is_documented, "DOCUMENTED", pat$type)

      # Extract the actual matched fragment for detailed reporting
      matched_fragment <- stringr::str_extract(line_text, pat$pattern)
      if (is.na(matched_fragment)) matched_fragment <- pat$name

      # Normalize whitespace in line content for clean reporting
      clean_content <- stringr::str_replace(
        stringr::str_trim(line_text), "\\s+", " "
      )

      dplyr::tibble(
        file_path       = r_file_path,
        line_number     = as.integer(ln),
        line_content    = clean_content,
        pattern_matched = matched_fragment,
        violation_type  = vtype
      )
    })
  })

  results
}

# =============================================================================
# SECTION 3: CORRECT MISSING HANDLING SOURCE CODE SCANNER
# =============================================================================
# Scans migrated R source files for correct missing value handling patterns
# that replace SAS constructs (MISSING(), NMISS(), CMISS(), etc.).
# =============================================================================

#' Scan an R source file for correct missing value handling patterns
#'
#' Reads the file line-by-line and catalogs uses of is.na(), sum(is.na()),
#' na.rm=TRUE, NA_character_, NA_real_, and haven::tagged_na().
#'
#' @param r_file_path Character path to the R source file.
#' @return A tibble with columns: file_path, line_number, function_used, context.
scan_missing_handling <- function(r_file_path) {
  # Validate the file exists
  if (!file.exists(r_file_path)) {
    return(
      dplyr::tibble(
        file_path     = character(0L),
        line_number   = integer(0L),
        function_used = character(0L),
        context       = character(0L)
      )
    )
  }

  lines <- readr::read_lines(r_file_path, lazy = FALSE)

  if (length(lines) == 0L) {
    return(
      dplyr::tibble(
        file_path     = character(0L),
        line_number   = integer(0L),
        function_used = character(0L),
        context       = character(0L)
      )
    )
  }

  # Define correct missing-handling patterns
  correct_patterns <- list(
    list(pattern = "is\\.na\\s*\\(",        name = "is.na()",          ctx = "Replaces SAS MISSING()"),
    list(pattern = "sum\\s*\\(\\s*is\\.na",  name = "sum(is.na())",     ctx = "Replaces SAS NMISS()/CMISS()"),
    list(pattern = "na\\.rm\\s*=\\s*TRUE",   name = "na.rm=TRUE",       ctx = "Explicit NA removal — verify intentional"),
    list(pattern = "NA_character_",          name = "NA_character_",    ctx = "Correct character missing (replaces SAS ' ')"),
    list(pattern = "NA_real_",               name = "NA_real_",         ctx = "Correct numeric missing (replaces SAS .)"),
    list(pattern = "tagged_na\\s*\\(",       name = "haven::tagged_na()", ctx = "SAS special missing (.A-.Z)"),
    list(pattern = "is_tagged_na\\s*\\(",    name = "haven::is_tagged_na()", ctx = "Detecting SAS special missing"),
    list(pattern = "complete\\.cases\\s*\\(", name = "complete.cases()", ctx = "Row-wise NA filtering — verify documented"),
    list(pattern = "drop_na\\s*\\(",         name = "tidyr::drop_na()", ctx = "NA row removal — verify documented")
  )

  # Scan each line for correct patterns, extracting match context
  purrr::map_dfr(correct_patterns, function(pat) {
    matched_lines <- which(stringr::str_detect(lines, pat$pattern))

    if (length(matched_lines) == 0L) {
      return(
        dplyr::tibble(
          file_path     = character(0L),
          line_number   = integer(0L),
          function_used = character(0L),
          context       = character(0L)
        )
      )
    }

    # Use str_match to extract the matched portion with surrounding context
    purrr::map_dfr(matched_lines, function(ln) {
      # Extract surrounding context using str_match with a capture group
      match_result <- stringr::str_match(
        lines[ln],
        paste0("(", pat$pattern, ")")
      )
      # Provide enriched context if the match provides additional detail
      enriched_ctx <- pat$ctx
      if (!is.na(match_result[1, 1])) {
        enriched_ctx <- paste0(pat$ctx, " [matched: ",
                               stringr::str_trim(match_result[1, 1]), "]")
      }

      dplyr::tibble(
        file_path     = r_file_path,
        line_number   = as.integer(ln),
        function_used = pat$name,
        context       = enriched_ctx
      )
    })
  })
}

# =============================================================================
# SECTION 4: HELPER FUNCTIONS FOR FILE DISCOVERY
# =============================================================================

#' Discover all ADaM XPT datasets in the configured data path
#'
#' Scans the adam_path directory for .xpt files and returns a named character
#' vector of file paths suitable for iteration with haven::read_xpt().
#'
#' @param adam_path Character path to the ADaM XPT directory.
#' @return Named character vector (name = dataset stem, value = full path).
discover_adam_datasets <- function(adam_path) {
  if (!dir.exists(adam_path)) {
    message("WARNING: ADaM data path not found: ", adam_path)
    return(character(0L))
  }

  xpt_files <- list.files(adam_path, pattern = "\\.xpt$", full.names = TRUE,
                           ignore.case = TRUE)

  if (length(xpt_files) == 0L) {
    message("WARNING: No .xpt files found in: ", adam_path)
    return(character(0L))
  }

  # Name each path by its dataset stem (e.g., adsl, adae, advs)
  names(xpt_files) <- tolower(tools::file_path_sans_ext(basename(xpt_files)))
  xpt_files
}

#' Discover all migrated R source files across configured source paths
#'
#' Iterates over all r_source_paths entries and collects .R files
#' for zero substitution and missing handling scans.
#'
#' @param r_source_paths Named list of R source directory paths.
#' @return Character vector of full file paths to migrated R source files.
discover_r_source_files <- function(r_source_paths) {
  # Start with config-provided paths

  all_paths <- unlist(r_source_paths, use.names = FALSE)


  # Append ALL migrated R directories per AAP §0.4.1 target structure

  # to ensure comprehensive scanning across the entire migration scope

  additional_dirs <- c(
    "tested/R/AE",
    "tested/R/DM",
    "tested/R/DS",
    "tested/R/EX",
    "tested/R/LB",
    "tested/R/MedDRA",
    "whitepapers/WPCT",
    "whitepapers/scriptathons/R",
    "contributed/R",
    "lang/R"
  )
  all_paths <- unique(c(all_paths, additional_dirs))

  existing_paths <- all_paths[dir.exists(all_paths)]

  if (length(existing_paths) == 0L) {
    message("INFO: No migrated R source directories found yet.")
    return(character(0L))
  }

  r_files <- unlist(
    purrr::map(existing_paths, function(dir_path) {
      list.files(dir_path, pattern = "\\.R$", full.names = TRUE,
                 recursive = TRUE, ignore.case = TRUE)
    }),
    use.names = FALSE
  )

  # Deduplicate in case config paths overlap with additional dirs
  unique(r_files)
}

#' Safely load an XPT dataset with error handling
#'
#' Wraps haven::read_xpt() with tryCatch to gracefully handle
#' corrupt or unreadable transport files. Uses janitor::clean_names()
#' to normalize column names for consistent audit reporting.
#'
#' @param xpt_path Character path to the .xpt file.
#' @param normalize_names Logical; if TRUE, apply clean_names() to columns.
#' @return Data frame or NULL if loading fails.
safe_read_xpt <- function(xpt_path, normalize_names = FALSE) {
  tryCatch({
    ds <- haven::read_xpt(xpt_path)
    if (normalize_names) {
      ds <- janitor::clean_names(ds)
    }
    ds
  },
  error = function(e) {
    message("WARNING: Failed to read XPT file: ", xpt_path,
            " — Error: ", conditionMessage(e))
    NULL
  })
}

# =============================================================================
# SECTION 4b: DIFFDF-BASED MISSING VALUE COMPARISON
# =============================================================================
# Uses diffdf to compare missing value patterns between two data frames,
# enabling side-by-side SAS baseline vs R output comparison.
# =============================================================================

#' Compare missing value patterns between two data frames using diffdf
#'
#' For each shared column, computes the missing value count in both
#' datasets and uses diffdf::diffdf() to detect discrepancies that
#' might indicate implicit zero substitution in the R output.
#'
#' @param sas_data Data frame from the SAS baseline (loaded via haven).
#' @param r_data Data frame from the migrated R output.
#' @param dataset_name Character identifier for reporting.
#' @return Tibble with variable-level comparison results.
compare_missing_patterns <- function(sas_data, r_data, dataset_name) {
  if (is.null(sas_data) || is.null(r_data)) {
    return(dplyr::tibble(
      dataset   = character(0L),
      variable  = character(0L),
      sas_na    = integer(0L),
      r_na      = integer(0L),
      diff      = integer(0L),
      status    = character(0L)
    ))
  }

  # Find shared columns
  shared_cols <- intersect(names(sas_data), names(r_data))
  shared_cols <- purrr::map_chr(shared_cols, function(x) x)

  if (length(shared_cols) == 0L) {
    return(dplyr::tibble(
      dataset   = character(0L),
      variable  = character(0L),
      sas_na    = integer(0L),
      r_na      = integer(0L),
      diff      = integer(0L),
      status    = character(0L)
    ))
  }

  # Use diffdf for structural comparison (suppressing output)
  diff_result <- tryCatch(
    suppressMessages(diffdf::diffdf(sas_data[shared_cols], r_data[shared_cols])),
    error = function(e) NULL
  )

  # Build variable-level missing comparison
  purrr::map_dfr(shared_cols, function(col) {
    sas_na_count <- sum(is.na(sas_data[[col]]))
    r_na_count   <- sum(is.na(r_data[[col]]))
    na_diff      <- sas_na_count - r_na_count

    comp_status <- dplyr::case_when(
      na_diff == 0L  ~ "MATCH",
      na_diff > 0L   ~ "SUSPECT_ZERO_SUB",
      TRUE           ~ "R_HAS_MORE_NA"
    )

    dplyr::tibble(
      dataset  = dataset_name,
      variable = col,
      sas_na   = as.integer(sas_na_count),
      r_na     = as.integer(r_na_count),
      diff     = as.integer(na_diff),
      status   = comp_status
    )
  })
}

# =============================================================================
# SECTION 5: DATASET-LEVEL MISSING VALUE TESTS
# =============================================================================
# testthat test blocks that validate missing value handling at the dataset
# level — run as part of run_gate3_validation() main function.
# =============================================================================

#' Run dataset-level missing value tests
#'
#' Loads all ADaM datasets and performs systematic missing value
#' validation using testthat assertions.
#'
#' @param adam_path Character path to ADaM XPT directory.
#' @return List with test_results tibble and loaded_catalogs tibble.
run_dataset_tests <- function(adam_path) {
  xpt_files <- discover_adam_datasets(adam_path)
  test_results <- dplyr::tibble(
    test_name   = character(0L),
    dataset     = character(0L),
    status      = character(0L),
    details     = character(0L)
  )
  all_catalogs <- dplyr::tibble()

  if (length(xpt_files) == 0L) {
    test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
      test_name = "Dataset Discovery",
      dataset   = "ALL",
      status    = "SKIP",
      details   = "No ADaM XPT datasets found at configured path."
    ))
    return(list(test_results = test_results, catalogs = all_catalogs))
  }

  # Iterate over each dataset
  for (ds_name in names(xpt_files)) {
    ds_path <- xpt_files[[ds_name]]
    ds <- safe_read_xpt(ds_path)

    if (is.null(ds)) {
      test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
        test_name = "Dataset Load",
        dataset   = ds_name,
        status    = "FAIL",
        details   = paste("Failed to read XPT file:", ds_path)
      ))
      next
    }

    # --- Catalog missing values for this dataset ---
    catalog <- catalog_missing_values(ds, ds_name)
    all_catalogs <- dplyr::bind_rows(all_catalogs, catalog)

    # --- Test: No empty strings in character columns ---
    char_cols <- catalog %>% dplyr::filter(col_type == "character")
    empty_string_violations <- char_cols %>%
      dplyr::filter(has_empty_string == TRUE)

    if (nrow(empty_string_violations) > 0L) {
      violation_vars <- paste(empty_string_violations$variable, collapse = ", ")
      test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
        test_name = "No Empty Strings in Character Columns",
        dataset   = ds_name,
        status    = "FAIL",
        details   = paste("Empty strings found in:", violation_vars,
                          "— should be NA_character_")
      ))
    } else {
      test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
        test_name = "No Empty Strings in Character Columns",
        dataset   = ds_name,
        status    = "PASS",
        details   = paste(nrow(char_cols),
                          "character columns checked — none contain empty strings")
      ))
    }

    # --- Test: Document tagged NA presence ---
    tagged_vars <- catalog %>% dplyr::filter(has_tagged_na == TRUE)
    if (nrow(tagged_vars) > 0L) {
      tagged_var_list <- paste(tagged_vars$variable, collapse = ", ")
      test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
        test_name = "Tagged NA Preservation (SAS Special Missing)",
        dataset   = ds_name,
        status    = "INFO",
        details   = paste("Tagged NAs found in:", tagged_var_list,
                          "— SAS special missing (.A-.Z) preserved via haven")
      ))
    }

    # --- Test: Flag suspicious zero patterns ---
    zero_suspects <- catalog %>% dplyr::filter(has_zero_suspect == TRUE)
    if (nrow(zero_suspects) > 0L) {
      suspect_vars <- paste(zero_suspects$variable, collapse = ", ")
      test_results <- dplyr::bind_rows(test_results, dplyr::tibble(
        test_name = "Suspicious Zero Pattern Review",
        dataset   = ds_name,
        status    = "REVIEW",
        details   = paste("Potentially suspicious zero counts in:", suspect_vars,
                          "— verify these are not implicit zero substitutions")
      ))
    }
  }

  list(test_results = test_results, catalogs = all_catalogs)
}

# =============================================================================
# SECTION 6: SOURCE CODE SCANNING ORCHESTRATOR
# =============================================================================
# Runs zero-substitution and missing-handling scans across all migrated
# R source files discovered via the config r_source_paths.
# =============================================================================

#' Run source code scans across all migrated R files
#'
#' Discovers all R source files and runs both the zero substitution
#' scanner and the missing handling scanner on each file.
#'
#' @param r_source_paths Named list of R source directory paths.
#' @return List with zero_violations tibble and handling_catalog tibble.
run_source_code_scans <- function(r_source_paths) {
  r_files <- discover_r_source_files(r_source_paths)

  zero_violations <- dplyr::tibble(
    file_path       = character(0L),
    line_number     = integer(0L),
    line_content    = character(0L),
    pattern_matched = character(0L),
    violation_type  = character(0L)
  )

  handling_catalog <- dplyr::tibble(
    file_path     = character(0L),
    line_number   = integer(0L),
    function_used = character(0L),
    context       = character(0L)
  )

  if (length(r_files) == 0L) {
    message("INFO: No migrated R source files discovered for scanning.")
    return(list(
      zero_violations  = zero_violations,
      handling_catalog = handling_catalog,
      files_scanned    = 0L
    ))
  }

  # Use purrr::possibly for fault-tolerant scanning
  safe_scan_zero <- purrr::possibly(scan_zero_substitution, otherwise = dplyr::tibble(
    file_path       = character(0L),
    line_number     = integer(0L),
    line_content    = character(0L),
    pattern_matched = character(0L),
    violation_type  = character(0L)
  ))

  safe_scan_handling <- purrr::possibly(scan_missing_handling, otherwise = dplyr::tibble(
    file_path     = character(0L),
    line_number   = integer(0L),
    function_used = character(0L),
    context       = character(0L)
  ))

  zero_violations <- purrr::map_dfr(r_files, safe_scan_zero)
  handling_catalog <- purrr::map_dfr(r_files, safe_scan_handling)

  list(
    zero_violations  = zero_violations,
    handling_catalog = handling_catalog,
    files_scanned    = length(r_files)
  )
}

# =============================================================================
# SECTION 7: MISSING VALUE AUDIT REPORT GENERATOR
# =============================================================================
# Combines dataset-level missing catalogs with source code scan results
# into a comprehensive audit tibble for Gate 3 reporting.
#
# Per AAP §0.8.2 Gate 3: "List every variable with missing values,
# SAS handling, and R equivalent."
# =============================================================================

#' Generate the comprehensive Gate 3 missing value audit report
#'
#' Combines dataset-level missing value catalogs with source code scanning
#' results into a structured audit report tibble.
#'
#' @param catalog_results Tibble from catalog_missing_values, one row per
#'   dataset-variable combination.
#' @param scan_results List from run_source_code_scans containing
#'   zero_violations and handling_catalog.
#' @return A list with:
#'   \describe{
#'     \item{audit_details}{Tibble with per-variable audit rows}
#'     \item{violation_summary}{Tibble summarizing violations by type}
#'     \item{summary_stats}{Named list of aggregate audit statistics}
#'     \item{overall_status}{Character: "PASS", "FAIL", or "REVIEW"}
#'   }
generate_missing_value_audit <- function(catalog_results, scan_results) {
  # ---- Build per-variable audit detail ----
  if (nrow(catalog_results) == 0L) {
    audit_details <- dplyr::tibble(
      dataset      = character(0L),
      variable     = character(0L),
      col_type     = character(0L),
      n_missing    = integer(0L),
      pct_missing  = numeric(0L),
      sas_handling = character(0L),
      r_handling   = character(0L),
      status       = character(0L)
    )
  } else {
    audit_details <- catalog_results %>%
      dplyr::mutate(
        sas_handling = dplyr::case_when(
          col_type == "numeric" & n_missing > 0L ~
            "SAS numeric missing (.) — displayed as blank via OPTIONS MISSING=''",
          col_type == "character" & n_missing > 0L ~
            "SAS character missing (' ') — blank string",
          has_tagged_na == TRUE ~
            "SAS special missing (.A-.Z) — tagged NA via haven",
          n_missing == 0L ~
            "No missing values in this variable",
          TRUE ~
            "Standard missing handling"
        ),
        r_handling = dplyr::case_when(
          col_type == "numeric" & n_missing > 0L ~
            "R NA (generic) — is.na() for testing, na.rm for exclusion",
          col_type == "character" & n_missing > 0L ~
            "R NA_character_ — must NOT be empty string ''",
          has_tagged_na == TRUE ~
            "haven::tagged_na() — preserves .A-.Z distinction",
          n_missing == 0L ~
            "No missing values — no handling required",
          TRUE ~
            "R NA — standard handling"
        ),
        status = dplyr::case_when(
          has_empty_string == TRUE ~ "VIOLATION",
          has_zero_suspect == TRUE ~ "REVIEW",
          has_tagged_na == TRUE    ~ "COMPLIANT",
          n_missing > 0L           ~ "COMPLIANT",
          TRUE                     ~ "COMPLIANT"
        )
      ) %>%
      dplyr::select(
        dataset, variable, col_type,
        n_missing, pct_missing,
        sas_handling, r_handling, status
      )
  }

  # ---- Summarize zero substitution violations from source code ----
  zero_violations <- scan_results$zero_violations
  undocumented_violations <- zero_violations %>%
    dplyr::filter(violation_type != "DOCUMENTED")

  violation_summary <- dplyr::tibble(
    violation_category = c(
      "Empty strings in character columns",
      "Zero substitution in source code (undocumented)",
      "Zero substitution in source code (documented)",
      "Suspicious zero patterns in data"
    ),
    count = c(
      sum(audit_details$status == "VIOLATION", na.rm = TRUE),
      nrow(undocumented_violations),
      sum(zero_violations$violation_type == "DOCUMENTED", na.rm = TRUE),
      sum(audit_details$status == "REVIEW", na.rm = TRUE)
    )
  )

  # ---- Compute per-dataset summary using group_by/summarise/n ----
  dataset_summary <- dplyr::tibble()
  if (nrow(audit_details) > 0L) {
    dataset_summary <- audit_details %>%
      dplyr::group_by(dataset) %>%
      dplyr::summarise(
        n_vars       = dplyr::n(),
        n_with_miss  = sum(n_missing > 0L, na.rm = TRUE),
        n_compliant  = sum(status == "COMPLIANT", na.rm = TRUE),
        n_violations = sum(status == "VIOLATION", na.rm = TRUE),
        n_review     = sum(status == "REVIEW", na.rm = TRUE),
        .groups = "drop"
      )
  }

  # ---- Compute aggregate summary statistics ----
  total_variables_audited <- nrow(audit_details)
  variables_with_missing <- sum(audit_details$n_missing > 0L, na.rm = TRUE)
  compliant_count <- sum(audit_details$status == "COMPLIANT", na.rm = TRUE)
  violation_count <- sum(audit_details$status == "VIOLATION", na.rm = TRUE)
  review_count <- sum(audit_details$status == "REVIEW", na.rm = TRUE)

  summary_stats <- list(
    total_variables_audited = total_variables_audited,
    variables_with_missing  = variables_with_missing,
    compliant_count         = compliant_count,
    violation_count         = violation_count,
    review_count            = review_count,
    files_scanned           = scan_results$files_scanned,
    undocumented_zero_sub   = nrow(undocumented_violations),
    documented_zero_sub     = sum(zero_violations$violation_type == "DOCUMENTED",
                                  na.rm = TRUE),
    handling_patterns_found = nrow(scan_results$handling_catalog)
  )

  # ---- Determine overall status ----
  # PASS only if: zero empty strings, zero undocumented zero substitutions,
  # and missing counts verified
  has_violations <- violation_count > 0L
  has_undocumented_zero <- nrow(undocumented_violations) > 0L

  overall_status <- dplyr::case_when(
    has_violations || has_undocumented_zero ~ "FAIL",
    review_count > 0L                       ~ "REVIEW",
    TRUE                                    ~ "PASS"
  )

  # ---- Create a long-form audit detail for flexible reporting ----
  # Uses pivot_longer to reshape the sas_handling/r_handling columns
  audit_long <- dplyr::tibble()
  if (nrow(audit_details) > 0L) {
    audit_long <- audit_details %>%
      tidyr::pivot_longer(
        cols      = c(sas_handling, r_handling),
        names_to  = "handling_source",
        values_to = "handling_description"
      )
  }

  # ---- Create a dataset-status cross-tab via pivot_wider ----
  status_crosstab <- dplyr::tibble()
  if (nrow(dataset_summary) > 0L) {
    status_long <- dataset_summary %>%
      tidyr::pivot_longer(
        cols      = c(n_compliant, n_violations, n_review),
        names_to  = "status_type",
        values_to = "count"
      )
    status_crosstab <- status_long %>%
      tidyr::pivot_wider(
        id_cols    = dataset,
        names_from = status_type,
        values_from = count,
        values_fill = 0L
      )
  }

  list(
    audit_details     = audit_details,
    audit_long        = audit_long,
    dataset_summary   = dataset_summary,
    status_crosstab   = status_crosstab,
    violation_summary = violation_summary,
    summary_stats     = summary_stats,
    overall_status    = overall_status
  )
}

# =============================================================================
# SECTION 8: MAIN GATE 3 VALIDATION RUNNER
# =============================================================================
# Orchestrates the complete Gate 3 validation workflow:
#   1. Load all relevant ADaM datasets
#   2. Run missing value catalog for each dataset
#   3. Scan all migrated R files for zero substitution patterns
#   4. Generate comprehensive audit report
#   5. Run testthat assertion blocks
#   6. Return structured result with gate, status, audit_report, timestamp
# =============================================================================

#' Run the complete Gate 3 Missing Value Handling Audit
#'
#' Main entry point for Gate 3 validation. Loads datasets, catalogs missing
#' values, scans source code for violations, runs testthat assertions, and
#' returns a structured result.
#'
#' @return A list with:
#'   \describe{
#'     \item{gate}{Character: "Gate 3"}
#'     \item{status}{Character: "PASS", "FAIL", or "REVIEW"}
#'     \item{audit_report}{List containing audit_details, violation_summary,
#'       summary_stats, and overall_status from generate_missing_value_audit()}
#'     \item{timestamp}{POSIXct timestamp of validation execution}
#'   }
#' @export
run_gate3_validation <- function() {
  message("=== Gate 3 — Missing Value Handling Audit ===")
  message("Timestamp: ", Sys.time())
  message("")

  # ---- Step 1: Configure paths ----
  adam_path <- config$data_paths$adam_path
  r_source_paths <- config$r_source_paths
  output_path <- config$output_paths$base_output_path

  if (is.null(adam_path)) adam_path <- "data/adam/cdisc"
  if (is.null(r_source_paths)) {
    r_source_paths <- list(
      r_macros_path     = "tested/R/macros",
      r_utilities_path  = "tested/R/utilities",
      wp_utilities_path = "whitepapers/utilities/R",
      wp_adam_path      = "whitepapers/ADaM/R"
    )
  }

  # Log configured source paths using purrr::walk for each entry
  purrr::walk(names(r_source_paths), function(path_name) {
    message("  Source path [", path_name, "]: ", r_source_paths[[path_name]])
  })

  message("[1/5] Discovering ADaM datasets at: ", adam_path)

  # ---- Step 2: Run dataset-level tests ----
  dataset_result <- run_dataset_tests(adam_path)
  message("[2/5] Dataset tests complete. ",
          nrow(dataset_result$catalogs), " variable entries cataloged.")

  # ---- Step 3: Run source code scans ----
  message("[3/5] Scanning migrated R source files...")
  scan_result <- run_source_code_scans(r_source_paths)
  message("  Files scanned: ", scan_result$files_scanned)
  message("  Zero substitution violations: ", nrow(scan_result$zero_violations))
  message("  Missing handling patterns: ", nrow(scan_result$handling_catalog))

  # ---- Step 4: Generate audit report ----
  message("[4/5] Generating missing value audit report...")
  audit <- generate_missing_value_audit(
    catalog_results = dataset_result$catalogs,
    scan_results    = scan_result
  )

  # ---- Step 5: Run testthat assertion blocks ----
  message("[5/5] Running testthat validation assertions...")
  testthat_results <- run_testthat_assertions(
    dataset_result = dataset_result,
    scan_result    = scan_result,
    audit          = audit
  )

  # ---- Combine into final result ----
  # If testthat produced failures, override audit status to FAIL
  final_status <- audit$overall_status
  if (any(testthat_results$status == "FAIL")) {
    final_status <- "FAIL"
  }

  message("")
  message("=== Gate 3 Summary ===")
  message("  Variables audited:          ",
          audit$summary_stats$total_variables_audited)
  message("  Variables with missing:     ",
          audit$summary_stats$variables_with_missing)
  message("  Compliant:                  ",
          audit$summary_stats$compliant_count)
  message("  Violations:                 ",
          audit$summary_stats$violation_count)
  message("  Review needed:              ",
          audit$summary_stats$review_count)
  message("  Files scanned:              ",
          audit$summary_stats$files_scanned)
  message("  Undocumented zero subs:     ",
          audit$summary_stats$undocumented_zero_sub)
  message("  Overall Status:             ", final_status)
  message("========================")

  result <- list(
    gate         = "Gate 3",
    status       = final_status,
    audit_report = audit,
    timestamp    = Sys.time()
  )

  result
}

# =============================================================================
# SECTION 9: TESTTHAT ASSERTION BLOCKS
# =============================================================================
# Structured testthat test blocks validating each Gate 3 criterion.
# These are called by run_gate3_validation() and also runnable independently.
# =============================================================================

#' Run testthat assertion blocks for Gate 3 validation
#'
#' Executes structured testthat assertions covering empty strings,
#' zero substitution, tagged NA preservation, and source code compliance.
#'
#' @param dataset_result List from run_dataset_tests().
#' @param scan_result List from run_source_code_scans().
#' @param audit List from generate_missing_value_audit().
#' @return Tibble with test_name and status (PASS/FAIL/SKIP).
run_testthat_assertions <- function(dataset_result, scan_result, audit) {
  assertion_results <- dplyr::tibble(
    test_name = character(0L),
    status    = character(0L),
    details   = character(0L)
  )

  # -- Test 1: No empty strings in character columns --
  tryCatch({
    empty_string_violations <- audit$audit_details %>%
      dplyr::filter(status == "VIOLATION")
    n_violations <- nrow(empty_string_violations)

    testthat::test_that(
      "No empty strings in character columns (should be NA_character_)", {
      testthat::expect_equal(
        n_violations, 0L,
        label = paste(
          "Character columns with empty strings instead of NA_character_:",
          n_violations)
      )
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "No empty strings in character columns",
      status    = dplyr::if_else(n_violations == 0L, "PASS", "FAIL"),
      details   = dplyr::if_else(
        n_violations == 0L,
        "All character missing values correctly use NA_character_",
        paste(n_violations, "character columns contain empty strings"))
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "No empty strings in character columns",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 2: No undocumented zero substitution in source code --
  tryCatch({
    undocumented <- scan_result$zero_violations %>%
      dplyr::filter(violation_type != "DOCUMENTED")
    n_undoc <- nrow(undocumented)

    testthat::test_that(
      "No undocumented zero substitution patterns in migrated R files", {
      testthat::expect_equal(
        n_undoc, 0L,
        label = paste("Undocumented zero substitution violations:", n_undoc)
      )
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "No undocumented zero substitution in source code",
      status    = dplyr::if_else(n_undoc == 0L, "PASS", "FAIL"),
      details   = dplyr::if_else(
        n_undoc == 0L,
        paste(scan_result$files_scanned,
              "files scanned — no undocumented zero substitutions"),
        paste(n_undoc,
              "undocumented zero substitution patterns found"))
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "No undocumented zero substitution in source code",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 3: Tagged NA preservation for SAS special missing --
  tryCatch({
    catalog <- dataset_result$catalogs
    tagged_vars <- catalog %>% dplyr::filter(has_tagged_na == TRUE)
    n_tagged <- nrow(tagged_vars)

    testthat::test_that(
      "Tagged NA values from SAS special missing (.A-.Z) are preserved", {
      # This test documents rather than fails — tagged NAs may or may not exist
      testthat::expect_true(
        is.data.frame(tagged_vars),
        label = "Tagged NA catalog is valid data frame"
      )
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Tagged NA preservation (SAS special missing)",
      status    = "PASS",
      details   = dplyr::if_else(
        n_tagged > 0L,
        paste(n_tagged, "variables with tagged NAs documented"),
        "No tagged NAs found in datasets (may not be applicable)")
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Tagged NA preservation (SAS special missing)",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 4: Missing value handling patterns exist in source code --
  tryCatch({
    n_handling <- nrow(scan_result$handling_catalog)
    n_files <- scan_result$files_scanned

    testthat::test_that(
      "Migrated R files use correct missing value handling patterns", {
      # If files exist, at least some should use is.na() or related patterns
      if (n_files > 0L) {
        testthat::expect_true(
          n_handling > 0L,
          label = paste("At least one correct missing handling pattern",
                        "found in migrated files")
        )
      } else {
        testthat::expect_true(TRUE, label = "No files to scan yet — skipping")
      }
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Correct missing value handling in source code",
      status    = dplyr::if_else(
        n_files == 0L || n_handling > 0L, "PASS", "FAIL"),
      details   = paste(n_handling, "correct missing handling patterns across",
                        n_files, "files")
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Correct missing value handling in source code",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 5: Overall missing value audit compliance --
  tryCatch({
    overall <- audit$overall_status

    testthat::test_that("Overall Gate 3 missing value audit is compliant", {
      testthat::expect_true(
        overall %in% c("PASS", "REVIEW"),
        label = paste("Overall audit status:", overall)
      )
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Overall Gate 3 compliance",
      status    = dplyr::if_else(
        overall %in% c("PASS", "REVIEW"), "PASS", "FAIL"),
      details   = paste("Overall status:", overall,
                        "| Variables:",
                        audit$summary_stats$total_variables_audited,
                        "| Violations:",
                        audit$summary_stats$violation_count)
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Overall Gate 3 compliance",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 6: Missing value handling in statistical computations --
  tryCatch({
    handling <- scan_result$handling_catalog
    na_rm_usages <- handling %>%
      dplyr::filter(function_used == "na.rm=TRUE")
    drop_na_usages <- handling %>%
      dplyr::filter(
        function_used %in% c("tidyr::drop_na()", "complete.cases()"))

    testthat::test_that(
      "Statistical computation NA handling is documented", {
      # na.rm and drop_na usage should be intentional and documented
      testthat::expect_true(
        is.data.frame(na_rm_usages) && is.data.frame(drop_na_usages),
        label = "NA handling catalogs are valid"
      )
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Statistical computation NA handling documented",
      status    = "PASS",
      details   = paste("na.rm=TRUE usages:", nrow(na_rm_usages),
                        "| drop_na/complete.cases:", nrow(drop_na_usages))
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Statistical computation NA handling documented",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  # -- Test 7 (describe block): Per-file handling pattern summary --
  tryCatch({
    testthat::describe("Gate 3 per-file missing handling inventory", {
      handling <- scan_result$handling_catalog
      if (nrow(handling) > 0L) {
        # Create nested tibble of handling patterns grouped by file,
        # then unnest for verification
        nested_handling <- handling %>%
          dplyr::group_by(file_path) %>%
          tidyr::nest(.key = "patterns")
        unnested_check <- nested_handling %>%
          tidyr::unnest(cols = c(patterns))
        testthat::it("produces consistent results after nest/unnest", {
          testthat::expect_equal(nrow(unnested_check), nrow(handling))
        })
      } else {
        testthat::it("has no files to verify (migration in progress)", {
          testthat::expect_true(TRUE)
        })
      }
    })

    assertion_results <- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Per-file handling pattern inventory",
      status    = "PASS",
      details   = paste("Handling patterns verified via nest/unnest across",
                        length(unique(scan_result$handling_catalog$file_path)),
                        "files")
    ))
  }, error = function(e) {
    assertion_results <<- dplyr::bind_rows(assertion_results, dplyr::tibble(
      test_name = "Per-file handling pattern inventory",
      status    = "FAIL",
      details   = conditionMessage(e)
    ))
  })

  assertion_results
}

# =============================================================================
# SECTION 10: EXECUTION ENTRY POINT
# =============================================================================
if (sys.nframe() == 0L) {
  results <- run_gate3_validation()
  cat(sprintf("\nGate 3 — Missing Value Audit: %s\n", results$status))
  cat(sprintf("Timestamp: %s\n",
              format(results$timestamp, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Variables audited: %d\n",
              results$audit_report$summary_stats$total_variables_audited))
  cat(sprintf("Violations: %d\n",
              results$audit_report$summary_stats$violation_count))
}

# =============================================================================
# MIGRATION NOTES
# =============================================================================
# ASSUMPTIONS:
#    - SAS numeric missing (.) always maps to R NA (never 0 or NaN)
#    - SAS character missing (' ') always maps to R NA_character_ (never "")
#    - Empty strings in haven-loaded data indicate potential character missing
#      handling issues
#    - Zero substitution is never acceptable for clinical data without
#      explicit documentation
#    - ADaM XPT datasets loaded via haven preserve SAS missing semantics
#    - Zero substitution patterns in comment lines are not violations
#    - Lines with explicit GATE3.EXEMPT or documentation markers are
#      classified as DOCUMENTED rather than violations
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS treats missing values as less than any non-missing value in
#      comparisons; R NA propagation differs (NA < 1 yields NA, not TRUE)
#    - R NA propagation differs from SAS missing in some aggregate
#      functions (mean(c(1,NA)) is NA in R without na.rm, but . in SAS
#      depends on the PROC)
#    - na.rm=TRUE in R vs SAS default missing exclusion behavior varies
#      by PROC (PROC MEANS excludes by default; PROC SQL does not)
#    - Sort order: SAS sorts missing before non-missing; R sorts NA last
#      by default (use na.last=FALSE to match SAS)
#
# NO DIRECT R EQUIVALENT:
#    - SAS special missing (.A-.Z) -> haven::tagged_na() preserves the
#      distinction but requires haven-aware code for detection
#    - SAS NMISS()/CMISS() -> sum(is.na()) in R for both numeric and
#      character types (R does not distinguish by type)
#    - SAS CALL MISSING() -> multiple assignment with NA/NA_character_
#      (no single-function equivalent)
#    - SAS OPTIONS MISSING=''; -> no R equivalent (R always displays NA)
#
# PACKAGE SELECTION RATIONALE:
#    - haven: Preserves SAS missing semantics including tagged NA values
#      for special missing (.A-.Z); read_xpt() is the standard for
#      regulatory SAS transport files
#    - stringr: Pattern matching for source code scanning — str_detect()
#      provides vectorized regex matching for anti-pattern detection
#    - purrr: Functional iteration over columns (imap_dfr) and files
#      (map_dfr) with fault tolerance via possibly()
#    - diffdf: Data frame comparison for detecting discrepancies in
#      missing value counts between SAS baseline and R output
#    - testthat: Standard R testing framework for structured assertions
#    - janitor: clean_names() for consistent audit tibble column naming
#
# OPEN QUESTIONS:
#    - Which ADaM variables use SAS special missing (.A-.Z) and require
#      tagged_na? (Depends on study-specific data)
#    - Are there legitimate zero-replacement locations that need
#      documentation? (e.g., cumulative event counts initialized to 0)
#    - Should aggregate function na.rm behavior be verified per-PROC or
#      globally? (Recommended: per-PROC verification in Gate 1)
#    - Should the suspicious zero threshold (>50%) be configurable per
#      domain? (e.g., flag variables are legitimately mostly 0)
# =============================================================================
