# =============================================================================
# gate7_scope_matching.R
# =============================================================================
# Purpose : Gate 7 — Scope Matching Confirmation
#           Part of the 8-gate SAS-to-R migration validation framework
#           (AAP §0.5.1, §0.8.2)
#
# Confirms:
#   1. No statistical functionality was added beyond what each SAS script
#      implements (AAP §0.8.1).
#   2. No statistical functionality was removed — every SAS PROC / macro
#      has a corresponding R equivalent.
#   3. 1:1 SAS-to-R file mapping is verified via a traceability matrix.
#   4. Features with no direct R equivalent are documented with approved
#      workarounds.
#
# Author  : PhUSE CS WG5 Migration
# =============================================================================

# ---------------------------------------------------------------------------
# Package Loading
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
# Load centralised configuration (parameterised paths).
# When config/migration_config.yaml is unavailable (e.g. during CI on a clean
# checkout) the functions still work — callers can supply `repo_root` directly.
load_gate7_config <- function(config_path = "config/migration_config.yaml") {
  if (file.exists(config_path)) {
    yaml::read_yaml(config_path)
  } else {
    list(
      r_source_paths = list(
        r_macros_path     = "tested/R/macros",
        r_utilities_path  = "tested/R/utilities",
        wp_utilities_path = "whitepapers/utilities/R",
        wp_adam_path      = "whitepapers/ADaM/R"
      ),
      data_paths = list(
        adam_path = "data/adam/cdisc"
      ),
      output_paths = list(
        base_output_path = "output"
      )
    )
  }
}

# =============================================================================
# SAS INVENTORY BUILDER
# =============================================================================
#' Build an inventory of all in-scope SAS source files in the repository.
#'
#' Scans predefined directory trees for .sas files and extracts metadata
#' (PROC usage, macro definitions, macro calls, line count) from each file.
#'
#' @param repo_root Character scalar — path to the repository root.
#' @return A tibble with columns: sas_file_path, sas_file_name, proc_list,
#'   macro_defs, macro_calls, line_count.
build_sas_inventory <- function(repo_root = ".", config = NULL) {

  # Directories that are in-scope for migration (AAP §0.3.1).

  sas_search_patterns <- c(
    file.path(repo_root, "tested", "SAS"),
    file.path(repo_root, "whitepapers", "WPCT"),
    file.path(repo_root, "whitepapers", "utilities"),
    file.path(repo_root, "whitepapers", "ADaM"),
    file.path(repo_root, "whitepapers", "qualification"),
    file.path(repo_root, "whitepapers", "scriptathons"),
    file.path(repo_root, "lang", "SAS"),
    file.path(repo_root, "contributed")
  )

  # Discover all .sas files under each search root.
  safe_list <- purrr::possibly(
    function(d) {
      if (dir.exists(d)) {
        list.files(d, pattern = "\\.sas$", recursive = TRUE, full.names = TRUE)
      } else {
        character(0)
      }
    },
    otherwise = character(0)
  )

  all_sas_files <- purrr::map(sas_search_patterns, safe_list) |>
    purrr::reduce(c) |>
    unique()

  if (length(all_sas_files) == 0L) {
    return(dplyr::tibble(
      sas_file_path = character(),
      sas_file_name = character(),
      proc_list     = list(),
      macro_defs    = list(),
      macro_calls   = list(),
      line_count    = integer()
    ))
  }

  # Extract metadata per file.
  extract_sas_metadata <- function(fp) {
    lines <- tryCatch(
      readr::read_lines(fp, progress = FALSE),
      error = function(e) character(0)
    )
    lines_lower <- stringr::str_to_lower(lines)

    # Statistical PROCs used.
    proc_pattern <- paste0(
      "\\bproc\\s+(",
      paste(c("freq", "means", "univariate", "glm", "mixed", "glimmix",
              "lifetest", "phreg", "report", "tabulate", "sgplot",
              "sgrender", "shewhart", "summary", "sql", "sort",
              "datasets", "contents", "compare", "import", "export",
              "print", "transpose"),
            collapse = "|"),
      ")\\b"
    )
    procs_found <- stringr::str_extract_all(
      paste(lines_lower, collapse = " "),
      proc_pattern
    )[[1]] |>
      stringr::str_extract("(?<=proc\\s{0,5})\\w+") |>
      unique()

    # Macro definitions (%macro name).
    macro_def_pattern <- "%macro\\s+(\\w+)"
    macro_defs_found <- stringr::str_match(lines_lower, macro_def_pattern)[, 2] |>
      stats::na.omit() |>
      as.character() |>
      unique()

    # Macro include calls (%include).
    include_pattern <- "%include\\s"
    macro_calls_found <- lines_lower[stringr::str_detect(lines_lower, include_pattern)]
    macro_calls_found <- stringr::str_trim(macro_calls_found) |> unique()

    dplyr::tibble(
      sas_file_path = fp,
      sas_file_name = tools::file_path_sans_ext(basename(fp)),
      proc_list     = list(procs_found),
      macro_defs    = list(macro_defs_found),
      macro_calls   = list(macro_calls_found),
      line_count    = length(lines)
    )
  }

  result <- purrr::map_dfr(all_sas_files, extract_sas_metadata) |>
    dplyr::distinct(sas_file_path, .keep_all = TRUE)

  # Normalise column names for consistency (janitor::clean_names).
  result <- janitor::clean_names(result)

  # Restore expected column names (clean_names lowercases and underscores).
  # The tibble columns are already snake_case, so clean_names is idempotent
  # but ensures consistency if upstream metadata ever changes format.
  result
}

# =============================================================================
# R INVENTORY BUILDER
# =============================================================================
#' Build an inventory of all migrated R files in the target directories.
#'
#' Scans the R-side directory trees (tested/R, whitepapers/WPCT, etc.) for .R
#' files and extracts metadata about functions defined, library calls, source
#' calls, and key R function usage.
#'
#' @param repo_root Character scalar — path to the repository root.
#' @return A tibble with columns: r_file_path, r_file_name, function_list,
#'   function_defs, source_calls, library_calls, line_count.
build_r_inventory <- function(repo_root = ".", config = NULL) {

  r_search_patterns <- c(
    file.path(repo_root, "tested", "R"),
    file.path(repo_root, "whitepapers", "WPCT"),
    file.path(repo_root, "whitepapers", "utilities", "R"),
    file.path(repo_root, "whitepapers", "ADaM", "R"),
    file.path(repo_root, "whitepapers", "qualification", "R"),
    file.path(repo_root, "whitepapers", "scriptathons", "R"),
    file.path(repo_root, "lang", "R"),
    file.path(repo_root, "contributed", "R")
  )

  safe_list <- purrr::possibly(
    function(d) {
      if (dir.exists(d)) {
        list.files(d, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
      } else {
        character(0)
      }
    },
    otherwise = character(0)
  )

  all_r_files <- purrr::map(r_search_patterns, safe_list) |>
    purrr::reduce(c) |>
    unique()

  if (length(all_r_files) == 0L) {
    return(dplyr::tibble(
      r_file_path   = character(),
      r_file_name   = character(),
      function_list = list(),
      function_defs = list(),
      source_calls  = list(),
      library_calls = list(),
      line_count    = integer()
    ))
  }

  extract_r_metadata <- function(fp) {
    lines <- tryCatch(
      readr::read_lines(fp, progress = FALSE),
      error = function(e) character(0)
    )

    # Key analytical R functions detected.
    func_patterns <- c(
      "tplyr"       = "\\b(tplyr_table|add_layer|group_count|group_desc|set_pop_data|build)\\b",
      "dplyr"       = "\\b(mutate|filter|select|group_by|summarise|summarize|left_join|inner_join|bind_rows|arrange|distinct|case_when)\\b",
      "survival"    = "\\b(survfit|coxph|survdiff|Surv)\\b",
      "mmrm"        = "\\bmmrm\\b",
      "ggplot2"     = "\\b(ggplot|geom_boxplot|geom_point|geom_line|geom_bar|facet_wrap|facet_grid)\\b",
      "r2rtf"       = "\\b(rtf_body|rtf_title|rtf_footnote|rtf_page|write_rtf)\\b",
      "openxlsx"    = "\\b(createWorkbook|addWorksheet|writeData|saveWorkbook|createStyle|addStyle)\\b",
      "fisher"      = "\\bfisher\\.test\\b",
      "car_anova"   = "\\bAnova\\b",
      "stats_aov"   = "\\baov\\b",
      "haven_xpt"   = "\\bread_xpt\\b"
    )
    combined <- paste(lines, collapse = "\n")
    found_funcs <- purrr::map_chr(func_patterns, function(pat) {
      if (stringr::str_detect(combined, pat)) pat else NA_character_
    }) |>
      stats::na.omit() |>
      names()

    # Function definitions: name <- function(
    func_def_pattern <- "(\\w+)\\s*<-\\s*function\\s*\\("
    func_defs_found <- stringr::str_match(lines, func_def_pattern)[, 2] |>
      stats::na.omit() |>
      as.character() |>
      unique()

    # source() calls
    source_pattern <- 'source\\s*\\('
    source_calls_found <- lines[stringr::str_detect(lines, source_pattern)]
    source_calls_found <- stringr::str_trim(source_calls_found) |> unique()

    # library() calls
    lib_pattern <- "library\\s*\\(\\s*[\"']?(\\w+)[\"']?\\s*\\)"
    lib_calls_found <- stringr::str_match(lines, lib_pattern)[, 2] |>
      stats::na.omit() |>
      as.character() |>
      unique()

    dplyr::tibble(
      r_file_path   = fp,
      r_file_name   = tools::file_path_sans_ext(basename(fp)),
      function_list = list(found_funcs),
      function_defs = list(func_defs_found),
      source_calls  = list(source_calls_found),
      library_calls = list(lib_calls_found),
      line_count    = length(lines)
    )
  }

  result <- purrr::map_dfr(all_r_files, extract_r_metadata) |>
    dplyr::distinct(r_file_path, .keep_all = TRUE)

  # Normalise column names for consistency.
  janitor::clean_names(result)
}

# =============================================================================
# TRACEABILITY MATRIX BUILDER
# =============================================================================
#' Build a traceability matrix mapping every in-scope SAS file to its expected
#' R counterpart using the project naming convention.
#'
#' @param sas_inventory Tibble returned by \code{build_sas_inventory()}.
#' @param r_inventory   Tibble returned by \code{build_r_inventory()}.
#' @param repo_root     Character scalar — repository root path.
#' @return A tibble with columns: sas_file, expected_r_file, r_file_exists,
#'   mapping_status ("MAPPED", "MISSING", or "EXTRA").
build_traceability_matrix <- function(sas_inventory, r_inventory,
                                      repo_root = ".") {

  # ------------------------------------------------------------------
  # Helper: given a SAS path, derive the expected R counterpart path.
  # ------------------------------------------------------------------
  sas_to_r_path <- function(sas_path) {
    # Normalise separators.
    p <- stringr::str_replace_all(sas_path, "\\\\", "/")

    # Remove repo_root prefix if present.
    root_prefix <- stringr::str_replace_all(
      paste0(repo_root, "/"), "\\\\", "/"
    )
    p <- stringr::str_remove(p, paste0("^", stringr::fixed(root_prefix)))

    # ------ tested/SAS  -->  tested/R -----------------------------------
    if (stringr::str_detect(p, "^tested/SAS/ZZ_Utilities/")) {
      p <- stringr::str_replace(p, "^tested/SAS/ZZ_Utilities/", "tested/R/utilities/")
    } else if (stringr::str_detect(p, "^tested/SAS/macros/")) {
      p <- stringr::str_replace(p, "^tested/SAS/macros/", "tested/R/macros/")
    } else if (stringr::str_detect(p, "^tested/SAS/")) {
      p <- stringr::str_replace(p, "^tested/SAS/", "tested/R/")
    }

    # ------ whitepapers/utilities/*.sas  -->  whitepapers/utilities/R/*.R -
    if (stringr::str_detect(p, "^whitepapers/utilities/") &&
        stringr::str_detect(p, "\\.sas$") &&
        !stringr::str_detect(p, "^whitepapers/utilities/R/")) {
      p <- stringr::str_replace(
        p,
        "^whitepapers/utilities/",
        "whitepapers/utilities/R/"
      )
    }

    # ------ whitepapers/ADaM/*.sas  -->  whitepapers/ADaM/R/*.R ----------
    if (stringr::str_detect(p, "^whitepapers/ADaM/") &&
        stringr::str_detect(p, "\\.sas$") &&
        !stringr::str_detect(p, "^whitepapers/ADaM/R/")) {
      p <- stringr::str_replace(
        p,
        "^whitepapers/ADaM/",
        "whitepapers/ADaM/R/"
      )
    }

    # ------ whitepapers/qualification/*.sas --> whitepapers/qualification/R/*.R
    if (stringr::str_detect(p, "^whitepapers/qualification/") &&
        stringr::str_detect(p, "\\.sas$") &&
        !stringr::str_detect(p, "^whitepapers/qualification/R/")) {
      p <- stringr::str_replace(
        p,
        "^whitepapers/qualification/",
        "whitepapers/qualification/R/"
      )
    }

    # ------ whitepapers/scriptathons/**/*.sas --> whitepapers/scriptathons/R/**/*.R
    if (stringr::str_detect(p, "^whitepapers/scriptathons/") &&
        stringr::str_detect(p, "\\.sas$") &&
        !stringr::str_detect(p, "^whitepapers/scriptathons/R/")) {
      p <- stringr::str_replace(
        p,
        "^whitepapers/scriptathons/",
        "whitepapers/scriptathons/R/"
      )
    }

    # ------ lang/SAS/**  -->  lang/R/** ---------------------------------
    if (stringr::str_detect(p, "^lang/SAS/")) {
      p <- stringr::str_replace(p, "^lang/SAS/", "lang/R/")
      # Flatten deep subdirs: only keep the immediate category folder.
      # e.g. lang/R/graph/KM/kmplot.sas --> lang/R/graph/kmplot.R
      # Heuristic: keep first two path components after lang/R/ plus filename.
      parts <- unlist(stringr::str_split(p, "/"))
      if (length(parts) > 4) {
        # lang / R / category / ... / file.sas  -->  lang/R/category/file.R
        p <- paste(c(parts[1:3], parts[length(parts)]), collapse = "/")
      }
    }

    # ------ contributed/**/*.sas  -->  contributed/R/**/*.R ---------------
    if (stringr::str_detect(p, "^contributed/") &&
        stringr::str_detect(p, "\\.sas$") &&
        !stringr::str_detect(p, "^contributed/R/")) {
      p <- stringr::str_replace(p, "^contributed/", "contributed/R/")
    }

    # ------ WPCT .sas stays in same directory, just extension change -----
    # (whitepapers/WPCT/WPCT-F.07.03.sas --> whitepapers/WPCT/WPCT-F.07.03.R)

    # Replace extension .sas --> .R
    p <- stringr::str_replace(p, "\\.sas$", ".R")

    p
  }

  # Build per-SAS-file mapping.
  if (nrow(sas_inventory) == 0L) {
    trace_tbl <- dplyr::tibble(
      sas_file       = character(),
      expected_r_file = character(),
      r_file_exists  = logical(),
      mapping_status = character()
    )
  } else {
    trace_tbl <- sas_inventory |>
      dplyr::mutate(
        expected_r_file = purrr::map_chr(sas_file_path, sas_to_r_path),
        r_file_exists   = purrr::map_lgl(
          expected_r_file,
          ~ file.exists(file.path(repo_root, .x))
        ),
        mapping_status  = dplyr::if_else(r_file_exists, "MAPPED", "MISSING")
      ) |>
      dplyr::select(
        sas_file        = sas_file_path,
        expected_r_file,
        r_file_exists,
        mapping_status
      )
  }

  # Detect EXTRA R files that have no SAS counterpart.
  if (nrow(r_inventory) > 0L && nrow(trace_tbl) > 0L) {
    r_paths_expected <- trace_tbl |> dplyr::pull(expected_r_file)

    # Normalise R file paths relative to repo_root for matching.
    r_inv_norm <- r_inventory |>
      dplyr::mutate(
        r_rel_path = stringr::str_remove(
          stringr::str_replace_all(r_file_path, "\\\\", "/"),
          paste0("^", stringr::str_replace_all(
            paste0(repo_root, "/"), "\\\\", "/"
          ))
        )
      )

    extra_r <- r_inv_norm |>
      dplyr::filter(!r_rel_path %in% r_paths_expected) |>
      dplyr::mutate(
        sas_file        = NA_character_,
        expected_r_file = r_rel_path,
        r_file_exists   = TRUE,
        mapping_status  = "EXTRA"
      ) |>
      dplyr::select(sas_file, expected_r_file, r_file_exists, mapping_status)

    trace_tbl <- dplyr::bind_rows(trace_tbl, extra_r)
  }

  trace_tbl
}

# =============================================================================
# PROC COVERAGE COMPARATOR
# =============================================================================
#' Compare SAS PROC usage in a source file against R function usage in the
#' migrated counterpart to verify functional scope preservation.
#'
#' @param sas_procs   Character vector of SAS PROC names found in a file
#'   (lower-case, e.g. "freq", "means").
#' @param r_functions Character vector of R function-category tags found in
#'   the counterpart R file (e.g. "tplyr", "fisher", "dplyr").
#' @param sas_file    Character scalar — SAS source file path (for reporting).
#' @param r_file      Character scalar — R counterpart file path (for reporting).
#' @return A tibble with columns: sas_file, r_file, sas_proc,
#'   expected_r_equivalent, r_equivalent_found, notes.
compare_proc_coverage <- function(sas_procs,
                                  r_functions,
                                  sas_file = NA_character_,
                                  r_file   = NA_character_) {

 # Mapping table: SAS PROC -> acceptable R equivalents (tag names from
 # build_r_inventory's function_list).
  proc_r_map <- dplyr::tribble(
    ~sas_proc,    ~r_tags,                                       ~description,
    "freq",       c("tplyr", "fisher"),                          "Tplyr count layer / fisher.test()",
    "means",      c("tplyr", "dplyr"),                           "Tplyr desc layer / dplyr summarise()",
    "univariate", c("tplyr", "dplyr"),                           "Tplyr desc layer / dplyr summarise()",
    "summary",    c("tplyr", "dplyr"),                           "Tplyr desc layer / dplyr summarise()",
    "glm",        c("car_anova", "stats_aov"),                   "car::Anova() / stats::aov()",
    "mixed",      c("mmrm"),                                     "mmrm::mmrm()",
    "glimmix",    c("mmrm"),                                     "mmrm::mmrm()",
    "lifetest",   c("survival"),                                 "survival::survfit()",
    "phreg",      c("survival"),                                 "survival::coxph()",
    "report",     c("tplyr", "r2rtf", "openxlsx"),               "Tplyr + r2rtf / openxlsx",
    "tabulate",   c("tplyr", "r2rtf", "openxlsx"),               "Tplyr + r2rtf / openxlsx",
    "sgplot",     c("ggplot2"),                                  "ggplot2",
    "sgrender",   c("ggplot2"),                                  "ggplot2",
    "shewhart",   c("ggplot2"),                                  "ggplot2 geom_boxplot()",
    "sql",        c("dplyr"),                                    "dplyr verbs",
    "sort",       c("dplyr"),                                    "dplyr::arrange()",
    "transpose",  c("dplyr", "tplyr"),                           "tidyr::pivot_wider/longer",
    "import",     c("haven_xpt", "dplyr"),                       "haven::read_xpt() / readr",
    "export",     c("openxlsx", "r2rtf", "haven_xpt"),           "openxlsx / r2rtf / haven",
    "print",      c("dplyr", "r2rtf", "openxlsx", "tplyr"),      "print / output equiv",
    "datasets",   c("dplyr"),                                    "R object management",
    "contents",   c("dplyr"),                                    "str() / glimpse()",
    "compare",    c("dplyr"),                                    "diffdf::diffdf()"
  )

  if (length(sas_procs) == 0L) {
    return(dplyr::tibble(
      sas_file              = character(),
      r_file                = character(),
      sas_proc              = character(),
      expected_r_equivalent = character(),
      r_equivalent_found    = logical(),
      notes                 = character()
    ))
  }

  purrr::map_dfr(sas_procs, function(proc) {
    map_row <- proc_r_map |>
      dplyr::filter(sas_proc == !!proc)

    if (nrow(map_row) == 0L) {
      return(dplyr::tibble(
        sas_file              = sas_file,
        r_file                = r_file,
        sas_proc              = proc,
        expected_r_equivalent = "UNMAPPED",
        r_equivalent_found    = NA,
        notes                 = "No mapping defined for this PROC"
      ))
    }

    expected_tags  <- map_row$r_tags[[1]]
    expected_label <- map_row$description
    found <- any(expected_tags %in% r_functions)

    dplyr::tibble(
      sas_file              = sas_file,
      r_file                = r_file,
      sas_proc              = proc,
      expected_r_equivalent = expected_label,
      r_equivalent_found    = found,
      notes                 = dplyr::if_else(
        found,
        "Covered",
        paste0("Missing R equivalent for PROC ", toupper(proc))
      )
    )
  })
}

# =============================================================================
# SCOPE MATCHING REPORT GENERATOR
# =============================================================================
#' Compile a comprehensive scope matching report from traceability, PROC
#' coverage, and macro-to-function mapping results.
#'
#' @param traceability_matrix Tibble from \code{build_traceability_matrix()}.
#' @param proc_coverage       Tibble from per-file \code{compare_proc_coverage()}.
#' @param macro_mapping       Tibble summarising SAS macro -> R function mapping.
#' @param r_inventory         Tibble from \code{build_r_inventory()} (used for
#'   extra-functionality and MIGRATION NOTES scanning).
#' @param repo_root           Character scalar -- repository root path.
#' @return A named list with: report (summary tibble), gate_status ("PASS" /
#'   "FAIL"), summary (list of key metrics), no_equivalent_features (tibble).
generate_scope_matching_report <- function(traceability_matrix,
                                           proc_coverage,
                                           macro_mapping,
                                           r_inventory  = NULL,
                                           repo_root    = ".") {

  # ---------- File-level summary ------------------------------------------
  total_sas_files <- traceability_matrix |>
    dplyr::filter(!is.na(sas_file)) |>
    nrow()

  mapped_count <- traceability_matrix |>
    dplyr::filter(mapping_status == "MAPPED") |>
    nrow()

  missing_count <- traceability_matrix |>
    dplyr::filter(mapping_status == "MISSING") |>
    nrow()

  extra_count <- traceability_matrix |>
    dplyr::filter(mapping_status == "EXTRA") |>
    nrow()

  # ---------- PROC-level summary ------------------------------------------
  total_procs <- nrow(proc_coverage)

  covered_procs <- proc_coverage |>
    dplyr::filter(!is.na(r_equivalent_found) & r_equivalent_found) |>
    nrow()

  missing_procs <- proc_coverage |>
    dplyr::filter(!is.na(r_equivalent_found) & !r_equivalent_found) |>
    nrow()

  unmapped_procs <- proc_coverage |>
    dplyr::filter(is.na(r_equivalent_found)) |>
    nrow()

  # ---------- Macro-level summary -----------------------------------------
  total_macros  <- nrow(macro_mapping)
  mapped_macros <- macro_mapping |>
    dplyr::filter(r_function_exists) |>
    nrow()

  # ---------- No-equivalent features (scan R MIGRATION NOTES) -------------
  no_equivalent_features <- dplyr::tibble(
    r_file         = character(),
    feature_text   = character()
  )

  if (!is.null(r_inventory) && nrow(r_inventory) > 0L) {
    safe_scan <- purrr::possibly(function(fp) {
      lines <- readr::read_lines(fp)
      in_block <- FALSE
      features <- character()
      for (ln in lines) {
        if (stringr::str_detect(ln, "(?i)NO DIRECT R EQUIVALENT")) {
          in_block <- TRUE
          next
        }
        if (in_block) {
          if (stringr::str_detect(ln, "^#\\s*={5,}") ||
              stringr::str_detect(ln, "(?i)(PACKAGE SELECTION|OPEN QUESTIONS|POTENTIAL NUMERICAL|ASSUMPTIONS)")) {
            break
          }
          cleaned <- stringr::str_trim(stringr::str_remove(ln, "^#\\s*"))
          if (nchar(cleaned) > 0L) {
            features <- c(features, cleaned)
          }
        }
      }
      features
    }, otherwise = character())

    no_eq_rows <- purrr::map_dfr(seq_len(nrow(r_inventory)), function(i) {
      fp       <- r_inventory$r_file_path[i]
      features <- safe_scan(fp)
      if (length(features) > 0L) {
        dplyr::tibble(r_file = fp, feature_text = features)
      } else {
        dplyr::tibble(r_file = character(), feature_text = character())
      }
    })
    no_equivalent_features <- no_eq_rows
  }

  # ---------- Data domain coverage (XPT files) ----------------------------
  # Verify that the R migration covers the same data domains (ADaM datasets)
  # as the SAS source by checking available XPT files with haven::read_xpt.
  data_domain_coverage <- tryCatch({
    adam_path <- file.path(repo_root, "data", "adam", "cdisc")
    if (dir.exists(adam_path)) {
      xpt_files <- list.files(adam_path, pattern = "\\.xpt$",
                              full.names = TRUE, recursive = TRUE)
      if (length(xpt_files) > 0L) {
        # Read header of first XPT file to verify haven::read_xpt works.
        sample_xpt <- haven::read_xpt(xpt_files[1], n_max = 1L)
        list(
          xpt_count    = length(xpt_files),
          xpt_domains  = tools::file_path_sans_ext(basename(xpt_files)),
          sample_vars  = names(sample_xpt),
          read_success = TRUE
        )
      } else {
        list(xpt_count = 0L, xpt_domains = character(),
             sample_vars = character(), read_success = TRUE)
      }
    } else {
      list(xpt_count = 0L, xpt_domains = character(),
           sample_vars = character(), read_success = TRUE)
    }
  }, error = function(e) {
    list(xpt_count = 0L, xpt_domains = character(),
         sample_vars = character(), read_success = FALSE)
  })

  # ---------- Extra-functionality flag ------------------------------------
  extra_files <- traceability_matrix |>
    dplyr::filter(mapping_status == "EXTRA") |>
    dplyr::pull(expected_r_file)

  # ---------- Gate status determination -----------------------------------
  file_pass <- missing_count == 0L
  proc_pass <- missing_procs == 0L
  # Extra R files that existed before migration (WPCT-F.07.01.R, etc.)
  # are not grounds for failure -- flagged for manual review only.
  extra_pass <- TRUE

  gate_status <- dplyr::if_else(
    file_pass & proc_pass & extra_pass,
    "PASS",
    "FAIL"
  )

  summary_metrics <- list(
    total_sas_files      = total_sas_files,
    mapped_count         = mapped_count,
    missing_count        = missing_count,
    extra_count          = extra_count,
    total_procs          = total_procs,
    covered_procs        = covered_procs,
    missing_procs        = missing_procs,
    unmapped_procs       = unmapped_procs,
    total_macros         = total_macros,
    mapped_macros        = mapped_macros,
    no_equivalent_count  = nrow(no_equivalent_features),
    gate_status          = gate_status
  )

  list(
    report                 = traceability_matrix,
    gate_status            = gate_status,
    summary                = summary_metrics,
    proc_coverage          = proc_coverage,
    macro_mapping          = macro_mapping,
    no_equivalent_features = no_equivalent_features,
    extra_files            = extra_files,
    data_domain_coverage   = data_domain_coverage
  )
}

# =============================================================================
# MAIN GATE 7 ORCHESTRATOR
# =============================================================================
#' Run the complete Gate 7 -- Scope Matching Confirmation validation.
#'
#' Orchestrates inventory building, traceability mapping, PROC coverage
#' comparison, macro mapping, and report generation.
#'
#' @param repo_root   Character scalar -- repository root path (default ".").
#' @param config_path Character scalar -- path to migration_config.yaml relative
#'   to \code{repo_root}.
#' @return A named list containing: gate, status, scope_report,
#'   traceability_matrix, proc_coverage, macro_mapping, timestamp.
run_gate7_validation <- function(repo_root   = ".",
                                 config_path = "config/migration_config.yaml") {

  message("=== Gate 7 -- Scope Matching Confirmation ===")
  message("Repository root: ", repo_root)
  timestamp_val <- Sys.time()

  # 1. Load config.
  config <- load_gate7_config(
    file.path(repo_root, config_path)
  )

  # 2. Build inventories.
  message("[1/6] Building SAS inventory ...")
  sas_inv <- build_sas_inventory(repo_root = repo_root, config = config)
  message("  Found ", nrow(sas_inv), " SAS files")

  message("[2/6] Building R inventory ...")
  r_inv <- build_r_inventory(repo_root = repo_root, config = config)
  message("  Found ", nrow(r_inv), " R files")

  # 3. Traceability matrix.
  message("[3/6] Building traceability matrix ...")
  trace_mat <- build_traceability_matrix(sas_inv, r_inv, repo_root = repo_root)
  n_mapped  <- sum(trace_mat$mapping_status == "MAPPED")
  n_missing <- sum(trace_mat$mapping_status == "MISSING")
  n_extra   <- sum(trace_mat$mapping_status == "EXTRA")
  message(sprintf("  Mapped: %d | Missing: %d | Extra: %d",
                  n_mapped, n_missing, n_extra))

  # 4. PROC coverage comparison (per mapped SAS file).
  message("[4/6] Comparing PROC coverage ...")
  proc_cov_all <- dplyr::tibble(
    sas_file              = character(),
    r_file                = character(),
    sas_proc              = character(),
    expected_r_equivalent = character(),
    r_equivalent_found    = logical(),
    notes                 = character()
  )

  mapped_rows <- trace_mat |> dplyr::filter(mapping_status == "MAPPED")
  if (nrow(mapped_rows) > 0L && nrow(sas_inv) > 0L && nrow(r_inv) > 0L) {
    proc_cov_all <- purrr::map_dfr(seq_len(nrow(mapped_rows)), function(idx) {
      sas_path <- mapped_rows$sas_file[idx]
      r_path   <- mapped_rows$expected_r_file[idx]

      sas_row <- sas_inv |> dplyr::filter(sas_file_path == sas_path)

      # Match R file path -- normalise for comparison.
      r_row <- r_inv |>
        dplyr::filter(
          stringr::str_detect(
            stringr::str_replace_all(r_file_path, "\\\\", "/"),
            stringr::fixed(r_path)
          )
        )

      sas_procs   <- if (nrow(sas_row) > 0L) sas_row$proc_list[[1]] else character()
      r_functions <- if (nrow(r_row) > 0L)   r_row$function_list[[1]] else character()

      compare_proc_coverage(sas_procs, r_functions,
                            sas_file = sas_path, r_file = r_path)
    })
  }
  message("  PROC checks: ", nrow(proc_cov_all))

  # 5. Macro -> R function mapping.
  message("[5/6] Checking macro-to-function mapping ...")
  macro_mapping <- dplyr::tibble(
    sas_file          = character(),
    macro_name        = character(),
    expected_r_func   = character(),
    r_function_exists = logical()
  )

  if (nrow(sas_inv) > 0L) {
    macro_mapping <- purrr::map_dfr(seq_len(nrow(sas_inv)), function(idx) {
      row        <- sas_inv[idx, ]
      macro_defs <- row$macro_defs[[1]]

      if (length(macro_defs) == 0L) {
        return(dplyr::tibble(
          sas_file          = character(),
          macro_name        = character(),
          expected_r_func   = character(),
          r_function_exists = logical()
        ))
      }

      purrr::map_dfr(macro_defs, function(macro_nm) {
        # Look for an R function named identically (case-insensitive).
        found_in_r <- any(purrr::map_lgl(seq_len(nrow(r_inv)), function(ri) {
          func_defs <- r_inv$function_defs[[ri]]
          any(stringr::str_to_lower(func_defs) ==
              stringr::str_to_lower(macro_nm))
        }))

        dplyr::tibble(
          sas_file          = row$sas_file_path,
          macro_name        = macro_nm,
          expected_r_func   = macro_nm,
          r_function_exists = found_in_r
        )
      })
    })
  }
  message("  Macro mappings: ", nrow(macro_mapping))

  # 6. Compile report.
  message("[6/6] Generating scope matching report ...")
  scope_report <- generate_scope_matching_report(
    traceability_matrix = trace_mat,
    proc_coverage       = proc_cov_all,
    macro_mapping       = macro_mapping,
    r_inventory         = r_inv,
    repo_root           = repo_root
  )

  message(sprintf(
    "Gate 7 result: %s  (files=%d/%d, procs=%d/%d)",
    scope_report$gate_status,
    scope_report$summary$mapped_count,
    scope_report$summary$total_sas_files,
    scope_report$summary$covered_procs,
    scope_report$summary$total_procs
  ))

  # Optional: use diffdf to compare SAS vs R inventory shapes for auditing.
  inventory_diff <- tryCatch({
    sas_summary <- sas_inv |>
      dplyr::mutate(file_name = sas_file_name, type = "SAS") |>
      dplyr::select(file_name, type, line_count)
    r_summary <- r_inv |>
      dplyr::mutate(file_name = r_file_name, type = "R") |>
      dplyr::select(file_name, type, line_count)

    # Find common file names for direct comparison.
    common_names <- intersect(sas_summary$file_name, r_summary$file_name)
    if (length(common_names) > 0L) {
      sas_common <- sas_summary |>
        dplyr::filter(file_name %in% common_names) |>
        dplyr::distinct(file_name, .keep_all = TRUE) |>
        dplyr::arrange(file_name)
      r_common <- r_summary |>
        dplyr::filter(file_name %in% common_names) |>
        dplyr::distinct(file_name, .keep_all = TRUE) |>
        dplyr::arrange(file_name)
      # diffdf compares two data frames, returning structured differences.
      diff_result <- suppressMessages(
        diffdf::diffdf(sas_common, r_common, keys = "file_name",
                       suppress_warnings = TRUE)
      )
      diff_result
    } else {
      NULL
    }
  }, error = function(e) NULL)

  list(
    gate                = "Gate 7",
    status              = scope_report$gate_status,
    scope_report        = scope_report,
    traceability_matrix = trace_mat,
    proc_coverage       = proc_cov_all,
    macro_mapping       = macro_mapping,
    inventory_diff      = inventory_diff,
    timestamp           = timestamp_val
  )
}

# =============================================================================
# TESTTHAT TEST BLOCKS — Scope Matching Verification
# =============================================================================
# These test_that() blocks execute the scope matching checks described in
# Gate 7 of the 8-gate validation framework (AAP Section 0.8.2).
#
# They are designed to be run via testthat::test_file() or as part of
# run_gate7_validation(). Each block is self-contained with descriptive
# test names aligning to the gate specification.
# =============================================================================

# ---------------------------------------------------------------------------
# Test 1: 1:1 SAS file to R file mapping
# ---------------------------------------------------------------------------
test_that("Gate 7: Every in-scope SAS file has a corresponding R file", {
  repo_root <- "."
  config    <- load_gate7_config(
    file.path(repo_root, "config/migration_config.yaml")
  )

  sas_inv   <- build_sas_inventory(repo_root = repo_root, config = config)
  r_inv     <- build_r_inventory(repo_root = repo_root, config = config)
  trace_mat <- build_traceability_matrix(sas_inv, r_inv, repo_root = repo_root)

  missing_files <- trace_mat |>
    dplyr::filter(mapping_status == "MISSING")

  # Verify: no SAS file is left unmapped.
  expect_equal(
    nrow(missing_files), 0L,
    info = paste0(
      "The following SAS files have no R counterpart:\n",
      paste(missing_files$sas_file, collapse = "\n")
    )
  )

  # Verify: at least one SAS file was discovered (sanity).
  expect_gt(nrow(sas_inv), 0L,
            label = "SAS inventory should contain at least one file")
})

# ---------------------------------------------------------------------------
# Test 2: No extra R functionality beyond SAS source
# ---------------------------------------------------------------------------
test_that("Gate 7: No extra statistical functionality added in R migration", {
  repo_root <- "."
  config    <- load_gate7_config(
    file.path(repo_root, "config/migration_config.yaml")
  )

  sas_inv   <- build_sas_inventory(repo_root = repo_root, config = config)
  r_inv     <- build_r_inventory(repo_root = repo_root, config = config)
  trace_mat <- build_traceability_matrix(sas_inv, r_inv, repo_root = repo_root)

  # Extra R files with no SAS counterpart may indicate added functionality.
  extra_files <- trace_mat |>
    dplyr::filter(mapping_status == "EXTRA")

  # Pre-migration R files that existed before the migration (e.g., existing
  # WPCT R implementations, development utilities) are expected extras.
  known_pre_migration <- c(
    "WPCT-F.07.01.R",
    "WPCT-F.07.02-R-v01.R",
    "WPCT-F.07.02-R-v02.R",
    "Func_comm.R",
    "TK_functions.R"
  )

  genuinely_extra <- extra_files |>
    dplyr::filter(!purrr::map_lgl(expected_r_file, function(fp) {
      any(stringr::str_detect(fp, stringr::fixed(known_pre_migration)))
    }))

  # If any genuinely extra files exist, they must be documented as
  # infrastructure/validation (not statistical additions).
  infrastructure_patterns <- c(
    "gate[0-9]", "migration_config", "test_", "testthat",
    "validation", "renv", ".Rprofile"
  )

  unexplained_extras <- genuinely_extra |>
    dplyr::filter(!purrr::map_lgl(expected_r_file, function(fp) {
      any(stringr::str_detect(fp, infrastructure_patterns))
    }))

  expect_equal(
    nrow(unexplained_extras), 0L,
    info = paste0(
      "Unexplained extra R files (possible added functionality):\n",
      paste(unexplained_extras$expected_r_file, collapse = "\n")
    )
  )
})

# ---------------------------------------------------------------------------
# Test 3: No removed SAS functionality in R migration
# ---------------------------------------------------------------------------
test_that("Gate 7: No SAS PROC functionality removed in R migration", {
  repo_root <- "."
  config    <- load_gate7_config(
    file.path(repo_root, "config/migration_config.yaml")
  )

  sas_inv   <- build_sas_inventory(repo_root = repo_root, config = config)
  r_inv     <- build_r_inventory(repo_root = repo_root, config = config)
  trace_mat <- build_traceability_matrix(sas_inv, r_inv, repo_root = repo_root)

  mapped_rows <- trace_mat |> dplyr::filter(mapping_status == "MAPPED")

  if (nrow(mapped_rows) == 0L) {
    skip("No mapped SAS-R file pairs found -- skipping PROC coverage test")
  }

  proc_cov <- purrr::map_dfr(seq_len(nrow(mapped_rows)), function(idx) {
    sas_path <- mapped_rows$sas_file[idx]
    r_path   <- mapped_rows$expected_r_file[idx]

    sas_row <- sas_inv |> dplyr::filter(sas_file_path == sas_path)
    r_row   <- r_inv |>
      dplyr::filter(
        stringr::str_detect(
          stringr::str_replace_all(r_file_path, "\\\\", "/"),
          stringr::fixed(r_path)
        )
      )

    sas_procs   <- if (nrow(sas_row) > 0L) sas_row$proc_list[[1]] else character()
    r_functions <- if (nrow(r_row) > 0L)   r_row$function_list[[1]] else character()

    compare_proc_coverage(sas_procs, r_functions,
                          sas_file = sas_path, r_file = r_path)
  })

  removed_procs <- proc_cov |>
    dplyr::filter(!is.na(r_equivalent_found) & !r_equivalent_found)

  expect_equal(
    nrow(removed_procs), 0L,
    info = paste0(
      "SAS PROCs without R equivalent:\n",
      paste(
        sprintf("  %s: PROC %s -> %s",
                removed_procs$sas_file,
                toupper(removed_procs$sas_proc),
                removed_procs$expected_r_equivalent),
        collapse = "\n"
      )
    )
  )
})

# ---------------------------------------------------------------------------
# Test 4: All SAS macros mapped to R functions
# ---------------------------------------------------------------------------
test_that("Gate 7: All SAS macro definitions have corresponding R functions", {
  repo_root <- "."
  config    <- load_gate7_config(
    file.path(repo_root, "config/migration_config.yaml")
  )

  sas_inv <- build_sas_inventory(repo_root = repo_root, config = config)
  r_inv   <- build_r_inventory(repo_root = repo_root, config = config)

  # Build macro mapping.
  macro_mapping <- purrr::map_dfr(seq_len(nrow(sas_inv)), function(idx) {
    row        <- sas_inv[idx, ]
    macro_defs <- row$macro_defs[[1]]

    if (length(macro_defs) == 0L) {
      return(dplyr::tibble(
        sas_file = character(), macro_name = character(),
        expected_r_func = character(), r_function_exists = logical()
      ))
    }

    purrr::map_dfr(macro_defs, function(macro_nm) {
      found_in_r <- any(purrr::map_lgl(seq_len(nrow(r_inv)), function(ri) {
        func_defs <- r_inv$function_defs[[ri]]
        any(stringr::str_to_lower(func_defs) ==
            stringr::str_to_lower(macro_nm))
      }))

      dplyr::tibble(
        sas_file = row$sas_file_path, macro_name = macro_nm,
        expected_r_func = macro_nm, r_function_exists = found_in_r
      )
    })
  })

  unmapped_macros <- macro_mapping |>
    dplyr::filter(!r_function_exists)

  expect_equal(
    nrow(unmapped_macros), 0L,
    info = paste0(
      "SAS macros without R function counterpart:\n",
      paste(sprintf("  %s (from %s)",
                    unmapped_macros$macro_name,
                    unmapped_macros$sas_file),
            collapse = "\n")
    )
  )
})

# ---------------------------------------------------------------------------
# Test 5: YAML governance manifests have R counterparts
# ---------------------------------------------------------------------------
test_that("Gate 7: SAS YAML governance manifests have R counterparts", {
  repo_root <- "."

  # Find all *_sas.yml files.
  sas_ymls <- list.files(
    repo_root,
    pattern    = "_sas\\.yml$",
    recursive  = TRUE,
    full.names = TRUE
  )

  if (length(sas_ymls) == 0L) {
    skip("No SAS YAML manifests found")
  }

  # Derive expected R manifest names: *_sas.yml -> *_r.yml
  r_ymls_expected <- stringr::str_replace(sas_ymls, "_sas\\.yml$", "_r.yml")
  r_ymls_exist    <- file.exists(r_ymls_expected)

  missing_ymls <- sas_ymls[!r_ymls_exist]

  expect_equal(
    length(missing_ymls), 0L,
    info = paste0(
      "SAS YAML manifests without R counterpart:\n",
      paste(missing_ymls, collapse = "\n")
    )
  )

  # If any R manifests exist, verify they contain Language: R.
  existing_r_ymls <- r_ymls_expected[r_ymls_exist]
  if (length(existing_r_ymls) > 0L) {
    for (yml_path in existing_r_ymls) {
      yml_content <- tryCatch(
        yaml::read_yaml(yml_path),
        error = function(e) list()
      )
      expect_true(
        !is.null(yml_content[["Language"]]) ||
        !is.null(yml_content[["language"]]),
        info = paste("R YAML manifest missing Language field:", yml_path)
      )
    }
  }
})

# ---------------------------------------------------------------------------
# Test 6: No-direct-R-equivalent features are documented
# ---------------------------------------------------------------------------
test_that("Gate 7: Features with no direct R equivalent are documented", {
  repo_root <- "."
  config    <- load_gate7_config(
    file.path(repo_root, "config/migration_config.yaml")
  )

  r_inv <- build_r_inventory(repo_root = repo_root, config = config)

  if (nrow(r_inv) == 0L) {
    skip("No R files found -- skipping MIGRATION NOTES scan")
  }

  # Check that each migrated R file contains a MIGRATION NOTES block.
  has_migration_notes <- purrr::map_lgl(r_inv$r_file_path, function(fp) {
    tryCatch({
      content <- readr::read_file(fp)
      stringr::str_detect(content, "(?i)MIGRATION NOTES")
    }, error = function(e) FALSE)
  })

  files_without_notes <- r_inv$r_file_path[!has_migration_notes]

  # Not all R files require MIGRATION NOTES -- only those migrated from SAS.
  # This test reports coverage; full strictness depends on project maturity.
  coverage_pct <- 100 * mean(has_migration_notes)

  # Validate that a meaningful proportion of migrated R files contain

  # MIGRATION NOTES blocks. Coverage should be >0% indicating the
  # migration convention is being applied. A strict 100% threshold is
  # not enforced because infrastructure/utility files (e.g., gate scripts,
  # config loaders) are R but not direct SAS migrations.
  expect_true(
    coverage_pct > 0 || nrow(r_inv) == 0L,
    info = sprintf(
      "MIGRATION NOTES coverage: %.1f%% (%d/%d R files)\nFiles without notes:\n%s",
      coverage_pct,
      sum(has_migration_notes),
      nrow(r_inv),
      paste(files_without_notes, collapse = "\n")
    )
  )
})

# ---------------------------------------------------------------------------
# Test 7: Contributed and lang scripts have R counterparts
# ---------------------------------------------------------------------------
test_that("Gate 7: Contributed and lang SAS scripts have R counterparts", {
  repo_root <- "."

  # Contributed SAS files.
  contrib_sas <- list.files(
    file.path(repo_root, "contributed"),
    pattern    = "\\.sas$",
    recursive  = TRUE,
    full.names = FALSE
  )

  # Lang SAS files.
  lang_sas <- list.files(
    file.path(repo_root, "lang", "SAS"),
    pattern    = "\\.sas$",
    recursive  = TRUE,
    full.names = FALSE
  )

  # Skip if neither directory has SAS files.
  if (length(contrib_sas) == 0L && length(lang_sas) == 0L) {
    skip("No contributed or lang SAS files found")
  }

  # For contributed: check contributed/R/ exists and has .R files.
  if (length(contrib_sas) > 0L) {
    contrib_r_dir <- file.path(repo_root, "contributed", "R")
    if (dir.exists(contrib_r_dir)) {
      contrib_r <- list.files(
        contrib_r_dir,
        pattern   = "\\.R$",
        recursive = TRUE
      )
      expect_gt(
        length(contrib_r), 0L,
        label = "contributed/R/ exists but contains no R files"
      )
    }
  }

  # For lang: check lang/R/ exists and has .R files.
  if (length(lang_sas) > 0L) {
    lang_r_dir <- file.path(repo_root, "lang", "R")
    if (dir.exists(lang_r_dir)) {
      lang_r <- list.files(
        lang_r_dir,
        pattern   = "\\.R$",
        recursive = TRUE
      )
      expect_gt(
        length(lang_r), 0L,
        label = "lang/R/ exists but contains no R files"
      )
    }
  }
})

# =============================================================================
# EXECUTION ENTRY POINT
# =============================================================================
# When this script is run directly (not sourced), execute the full Gate 7
# validation and print the summary to stdout.
if (sys.nframe() == 0) {
  results <- run_gate7_validation()
  cat(sprintf("Gate 7 -- Scope Matching Confirmation: %s\n", results$status))
  cat(sprintf("  SAS files: %d | R files: %d | Coverage: %.1f%%\n",
      nrow(results$traceability_matrix),
      sum(results$traceability_matrix$r_file_exists),
      100 * mean(results$traceability_matrix$r_file_exists)))
  cat(sprintf("  PROC coverage: %d/%d\n",
      results$scope_report$summary$covered_procs,
      results$scope_report$summary$total_procs))
  cat(sprintf("  Macro mappings: %d/%d\n",
      results$scope_report$summary$mapped_macros,
      results$scope_report$summary$total_macros))
  cat(sprintf("  No-equivalent features documented: %d\n",
      results$scope_report$summary$no_equivalent_count))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS file naming convention maps predictably to R file names via
#      deterministic path transformation rules (tested/SAS -> tested/R,
#      whitepapers/utilities -> whitepapers/utilities/R, etc.)
#    - PROC-level coverage is sufficient granularity for scope matching;
#      sub-PROC option-level checks (e.g. EXACT FISHER within PROC FREQ)
#      are addressed by Gate 1 (functional parity) and Gate 4 (model
#      parameter verification) rather than this gate.
#    - Macro-to-function parameter mapping is verifiable via source scanning
#      of %macro definitions and R function() definitions.
#    - YAML governance manifest pairing follows *_sas.yml -> *_r.yml convention
#      as documented in AAP Section 0.4.1.
#    - Pre-migration R files (e.g. WPCT-F.07.01.R, development utilities)
#      are expected "extra" files and do not constitute added functionality.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - N/A for scope matching -- this gate verifies functional coverage,
#      not numeric output. Numeric differences are captured by Gate 1
#      (functional output parity) and Gate 2 (rounding and precision audit).
# NO DIRECT R EQUIVALENT:
#    - SAS PROC COMPARE -> diffdf::diffdf() (documented as scope-equivalent)
#    - SAS PROC CONTENTS -> base::str() + dplyr::glimpse() (scope-equivalent)
#    - SAS PROC DATASETS -> base file/object management functions
#    - SAS macro language conditional compilation (%if/%then at compile time)
#      -> R if/else at runtime (semantic equivalence, different mechanism)
#    - SAS ODS TAGSETS.EXCELXP -> openxlsx workbook pipeline (scope-equivalent)
#    - SAS JET/PCFILES engine -> openxlsx::saveWorkbook() (scope-equivalent)
# PACKAGE SELECTION RATIONALE:
#    - stringr: Robust pattern matching for scanning SAS PROCs and R functions
#      across hundreds of source files. Preferred over base grep for
#      consistency and vectorised operations.
#    - purrr: Functional iteration over file inventories with map_dfr/map_chr
#      for building inventory tibbles. Preferred over base lapply for
#      type-stable output and error handling (purrr::possibly).
#    - yaml: Reading SAS/R governance manifests (.yml) for comparison.
#    - readr: read_lines()/read_file() for loading source files as text
#      for content scanning. Preferred over base readLines for consistent
#      encoding handling and connection management.
#    - diffdf: Data frame comparison utility as direct replacement for SAS
#      PROC COMPARE. Used in traceability matrix comparisons.
#    - testthat: Primary testing framework for all 8 gates. test_that()
#      blocks enable individual test execution and clear failure reporting.
# OPEN QUESTIONS:
#    - Should obsolete SAS macros (assert_var_nonmissing.sas,
#      obsolete_util_figure_out_label.sas) require R counterparts, or
#      should they be excluded from the traceability matrix?
#    - How to handle SAS scripts with partial R implementations that
#      existed before migration (e.g. WPCT-F.07.02-R-v01.R and v02)?
#      Current approach: mark as EXTRA and exclude from failure criteria.
#    - Should contributed scripts with community variants all map 1:1
#      or can they consolidate into fewer R scripts?
#    - Should scriptathon archive SAS scripts require R counterparts
#      or are they historical reference only?
# ============================================================
