###############################################################################
#         PROGRAM NAME: Grouping and Subsetting Output (R Migration)          #
#                                                                             #
#          DESCRIPTION: Create output for grouping and subsetting metadata    #
#                       Contains three exported functions:                     #
#                          group_subset_pp -- Preprocessing to create          #
#                                             output tibbles                  #
#                          group_subset_xls_out -- Excel workbook output       #
#                                                 (template style)            #
#                          group_subset_xml_out -- Excel workbook output       #
#                                                 (XML-style rich layout)     #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: March 4, 2011                                         #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/MedDRA/ZZ_Utilities/sl_gs_output.sas       #
#     - Migration target: Idiomatic R using openxlsx for Excel output         #
#     - SAS %group_subset_pp macro -> R group_subset_pp() function            #
#     - SAS %group_subset_xls_out macro -> R group_subset_xls_out() function  #
#     - SAS %group_subset_xml_out macro -> R group_subset_xml_out() function  #
#     - SpreadsheetML XML streaming -> openxlsx in-memory workbook API        #
#     - SAS global macro variables -> R function return list elements          #
#     - SAS PROC SQL -> dplyr verbs                                           #
#     - SAS DATA step BY-group processing -> dplyr group_by/mutate/lag        #
#     - SAS %put -> cli messages                                              #
#     - SAS hash lookup -> dplyr left_join                                    #
#     - SAS PCFILES/JET engine -> openxlsx saveWorkbook                       #
#     - SAS %annotate/%markup -> annotate_data()/write_annotated()            #
#     - SAS %gs_rows merge calc -> compute_merge_spans() helper               #
#                                                                             #
#  EXTERNAL FILES USED: xml_output.R -- shared Excel output utilities         #
#                       (provides create_workbook_styles, annotate_data,       #
#                        write_annotated)                                      #
#                                                                             #
#  PARAMETERS REQUIRED: sl_group  -- grouping tibble (or NULL)                #
#                       sl_subset -- subsetting tibble (or NULL)              #
#                       sl_datasets -- datasets tibble (or NULL)              #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by MedDRA analysis scripts.   #
#                       In SAS, it was included via:                           #
#                         %include "&utilpath.\sl_gs_output.sas";             #
#                       xml_output.R must be sourced before this file          #
#                       (or sourced automatically if not already loaded).      #
#                                                                             #
#            REVISIONS:                                                        #
#              2026-03-25  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(openxlsx)
library(dplyr)
library(stringr)
library(cli)

# --------------------------------------------------------------------------- #
# Internal Dependency: xml_output.R
# Provides: create_workbook_styles(), annotate_data(), write_annotated()
# In the SAS original, xml_output.sas was %included before sl_gs_output.sas.
# In R, we conditionally source it if the functions are not already available.
# --------------------------------------------------------------------------- #
if (!exists("create_workbook_styles", mode = "function") ||
    !exists("annotate_data", mode = "function") ||
    !exists("write_annotated", mode = "function")) {
  local({
    # Determine script directory safely (R < 4.4 lacks base %||%)
    script_dir <- tryCatch(dirname(sys.frame(1L)$ofile), error = function(e) ".")
    if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."
    candidates <- c(
      file.path(script_dir, "xml_output.R"),
      "contributed/R/MedDRA/xml_output.R",
      file.path(".", "xml_output.R")
    )
    for (cpath in candidates) {
      if (file.exists(cpath)) {
        source(cpath, local = FALSE)
        break
      }
    }
  })
}
# Alias for schema compatibility: create_styles -> create_workbook_styles
if (exists("create_workbook_styles", mode = "function") &&
    !exists("create_styles", mode = "function")) {
  create_styles <- create_workbook_styles
}

# =========================================================================== #
# HELPER FUNCTIONS (internal, not exported)
# =========================================================================== #

# --------------------------------------------------------------------------- #
# get_var_label — Look up a variable label from domain data
# Replaces SAS open()/varnum()/varlabel() pattern
#
# @param domain_name Character. Name of the domain dataset (e.g. "DM", "LB").
# @param var_name    Character. Name of the variable to look up.
# @param domain_data Named list of data frames keyed by domain name.
# @return Character label string, or NA_character_ if not found.
# --------------------------------------------------------------------------- #
get_var_label <- function(domain_name, var_name, domain_data) {
  if (is.null(domain_data)) return(NA_character_)
  if (is.na(domain_name) || !nzchar(domain_name)) return(NA_character_)
  if (!domain_name %in% names(domain_data)) return(NA_character_)
  ds <- domain_data[[domain_name]]
  if (!var_name %in% names(ds)) return(NA_character_)
  lbl <- attr(ds[[var_name]], "label")
  if (is.null(lbl) || !nzchar(lbl)) return(NA_character_)
  as.character(lbl)
}


# --------------------------------------------------------------------------- #
# build_operator_desc — Build the operator description string
# Replaces SAS DO loop + SELECT logic (lines 406-417 of sl_gs_output.sas)
#
# For condition_count=3, condition_no=1, inner_op='AND':
#   Returns "Conditions 1.1 AND 1.2 AND 1.3"
# For condition_count=2, condition_no=2, inner_op='OR':
#   Returns "Condition 2.1 OR 2.2"
#
# @param condition_no   Integer. The condition group number.
# @param condition_count Integer. Number of sub-conditions.
# @param inner_op       Character. The inner operator ('AND' or 'OR').
# @return Character string describing the operator relationship.
# --------------------------------------------------------------------------- #
build_operator_desc <- function(condition_no, condition_count, inner_op) {
  if (is.na(inner_op) || !nzchar(str_trim(inner_op))) return("N/A")
  # Validate operator is a recognized type using str_detect
  if (!str_detect(str_to_lower(inner_op), "^(and|or)$")) return("N/A")

  parts <- vapply(seq_len(condition_count), function(i) {
    num_str <- str_c(as.character(condition_no), ".", as.character(i))
    sep <- if (i < condition_count) {
      str_c(" ", toupper(inner_op))
    } else {
      ""
    }
    str_c(num_str, sep)
  }, character(1L))

  body <- str_c(parts, collapse = " ")
  prefix <- if (str_to_lower(inner_op) == "or") "Condition " else "Conditions "
  str_squish(str_c(prefix, body))
}


# --------------------------------------------------------------------------- #
# compute_merge_spans — Compute hierarchical BY-group merge spans
# Replaces SAS %gs_rows macro (lines 635-674 of sl_gs_output.sas)
#
# For each variable in by_vars, computes the number of additional rows
# to merge down from the first row of each group. Respects hierarchical
# nesting: a change in an outer variable resets all inner variables.
#
# @param data    Data frame. Must be sorted by by_vars already.
# @param by_vars Character vector. Hierarchical BY variables (outermost first).
# @return Tibble with column 'row' and '<var>_n' for each by_var.
#   At group starts: <var>_n = number of additional rows to merge.
#   At continuation rows: <var>_n = NA_integer_.
# --------------------------------------------------------------------------- #
compute_merge_spans <- function(data, by_vars) {
  n <- nrow(data)
  if (n == 0L) return(dplyr::tibble(row = integer()))

  result <- dplyr::tibble(row = seq_len(n))
  outer_change <- rep(FALSE, n)
  outer_change[1L] <- TRUE

  for (j in seq_along(by_vars)) {
    var <- by_vars[j]
    var_n_col <- str_c(var, "_n")

    vals <- as.character(data[[var]])
    val_changed <- c(TRUE, vals[-1L] != vals[-n])
    group_start_flag <- val_changed | outer_change

    group_ids <- cumsum(group_start_flag)
    result[[var_n_col]] <- NA_integer_

    runs <- rle(group_ids)
    end_pos <- cumsum(runs$lengths)
    start_pos <- c(1L, end_pos[-length(end_pos)] + 1L)

    for (g in seq_along(runs$lengths)) {
      merge_count <- as.integer(runs$lengths[g] - 1L)
      result[[var_n_col]][start_pos[g]] <- merge_count
    }

    outer_change <- group_start_flag
  }

  result
}


# =========================================================================== #
# group_subset_pp() — Grouping and Subsetting Preprocessing
# Replaces SAS %group_subset_pp macro (lines 36-465 of sl_gs_output.sas)
# =========================================================================== #
#' Grouping and Subsetting Preprocessing
#'
#' Preprocess Script Launcher grouping and subsetting metadata into
#' formatted tibbles suitable for output. Replaces the SAS
#' \code{\%group_subset_pp} macro.
#'
#' @param sl_group   Tibble/data.frame. Script Launcher grouping metadata
#'   with columns: group_name, domain, partition, var_name, var_value,
#'   dsvg_grp_name. May be NULL or zero-row if no grouping is used.
#' @param sl_subset  Tibble/data.frame. Script Launcher subsetting metadata
#'   with columns: name, domain, partition, var_name, var_value,
#'   outer_operator, inner_operator. May be NULL or zero-row.
#' @param sl_datasets Tibble/data.frame. Script Launcher datasets metadata
#'   with columns: datatype, default, partition_variable.
#' @param domain_data Named list. Optional named list of data frames keyed
#'   by domain name for variable label lookup (replaces SAS OPEN/VARNUM/
#'   VARLABEL). Defaults to NULL.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{sl_group_desc}{Character. Human-readable grouping description.}
#'     \item{sl_subset_desc}{Character. Human-readable subsetting description.}
#'     \item{sl_subset_operator}{Character. Operator narrative.}
#'     \item{sl_gs_desc}{Character. Combined single-line description.}
#'     \item{sl_custom_ds}{Character. Comma-separated custom dataset names.}
#'     \item{sl_out_group}{Tibble. Formatted group detail table.}
#'     \item{sl_out_subset}{Tibble. Formatted subset detail table.}
#'     \item{sl_raw_group}{Tibble. Sorted raw group data (for merge calcs).}
#'     \item{sl_raw_subset}{Tibble. Sorted raw subset data (for merge calcs).}
#'   }
#'
#' @export
group_subset_pp <- function(sl_group = NULL,
                            sl_subset = NULL,
                            sl_datasets = NULL,
                            domain_data = NULL) {

  cli::cli_h2("SL GROUPING/SUBSETTING PREPROCESSING")

  # ---------------------------------------------------------------------- #
  # Observation counts (SAS lines 40-52)
  # ---------------------------------------------------------------------- #
  sl_group_nobs <- if (!is.null(sl_group) && is.data.frame(sl_group)) {
    nrow(sl_group)
  } else {
    0L
  }
  sl_subset_nobs <- if (!is.null(sl_subset) && is.data.frame(sl_subset)) {
    nrow(sl_subset)
  } else {
    0L
  }

  # ---------------------------------------------------------------------- #
  # Grouping description (SAS lines 58-92)
  # Uses stringr for Oxford comma logic replacing SAS TRANWRD
  # ---------------------------------------------------------------------- #
  if (sl_group_nobs > 0L) {
    group_names <- sl_group %>%
      dplyr::distinct(group_name) %>%
      dplyr::arrange(group_name) %>%
      dplyr::pull(group_name)

    group_count <- length(group_names)
    sl_group_desc <- str_c(group_names, collapse = ", ")

    if (group_count == 2L) {
      # Two groups: replace ", " with " and " (SAS TRANWRD)
      sl_group_desc <- str_replace(sl_group_desc, ", ", " and ")
    } else if (group_count > 2L) {
      # Three+ groups: replace last comma with ", and"
      # Find position of last comma and insert " and" after it
      last_comma_pos <- max(str_locate_all(sl_group_desc, ",")[[1L]][, 1L])
      sl_group_desc <- str_c(
        substr(sl_group_desc, 1L, last_comma_pos),
        " and",
        substr(sl_group_desc, last_comma_pos + 1L, nchar(sl_group_desc))
      )
    }
    sl_group_desc <- str_c("Grouped by ", sl_group_desc)
  } else {
    sl_group_desc <- "No grouping"
  }

  # ---------------------------------------------------------------------- #
  # Subsetting description (SAS lines 96-139)
  # ---------------------------------------------------------------------- #
  if (sl_subset_nobs > 0L) {
    # Extract outer operator (lowcase) — SAS PROC SQL SELECT DISTINCT
    sl_subset_outer <- sl_subset %>%
      dplyr::distinct(outer_operator) %>%
      dplyr::pull(outer_operator) %>%
      str_to_lower() %>%
      unique()
    sl_subset_outer_val <- sl_subset_outer[1L]

    # Extract distinct subset names
    subset_names <- sl_subset %>%
      dplyr::distinct(name) %>%
      dplyr::pull(name)
    sl_subset_count <- length(subset_names)

    # Build description: names joined by outer operator
    sl_subset_desc <- str_c(
      "Subset by ",
      str_c(subset_names, collapse = str_c(" ", sl_subset_outer_val, " "))
    )

    # Create operator narrative (SAS lines 119-133)
    if (sl_subset_count > 1L) {
      sl_subset_operator <- dplyr::case_when(
        sl_subset_outer_val == "and" ~ str_c(
          "ALL of the following rules must be true ",
          "for a subject to be included in the analysis."),
        sl_subset_outer_val == "or" ~ str_c(
          "ANY of the following rules must be true ",
          "for a subject to be included in the analysis."),
        TRUE ~ ""
      )
    } else {
      sl_subset_operator <- ""
    }
  } else {
    sl_subset_desc <- "No subsetting"
    sl_subset_operator <- "N/A"
  }

  cli::cli_alert_info(sl_group_desc)
  cli::cli_alert_info(sl_subset_desc)

  # ---------------------------------------------------------------------- #
  # Combined single-line description (SAS lines 146-157)
  # ---------------------------------------------------------------------- #
  if (sl_group_nobs > 0L && sl_subset_nobs > 0L) {
    sl_gs_desc <- str_c(sl_group_desc, "; ", sl_subset_desc)
  } else if (sl_group_nobs > 0L) {
    sl_gs_desc <- sl_group_desc
  } else if (sl_subset_nobs > 0L) {
    sl_gs_desc <- sl_subset_desc
  } else {
    sl_gs_desc <- ""
  }

  cli::cli_alert_info(sl_gs_desc)

  # ---------------------------------------------------------------------- #
  # NA-safe not-equal helper — SAS treats two missing BY-values as equal
  # Defined at function scope so both group and subset sections can use it
  # ---------------------------------------------------------------------- #
  na_safe_ne <- function(a, b) {
    dplyr::if_else(is.na(a) & is.na(b), FALSE,
                   dplyr::if_else(is.na(a) | is.na(b), TRUE, a != b))
  }

  # ---------------------------------------------------------------------- #
  # Group detail table (SAS lines 160-277)
  # ---------------------------------------------------------------------- #
  cli::cli_alert_info("GROUP DETAIL")

  if (sl_group_nobs > 0L) {
    # Sort by group_name, partition, var_name, dsvg_grp_name, var_value
    # (SAS PROC SORT line 170)
    sorted_group <- sl_group %>%
      dplyr::arrange(group_name, partition, var_name, dsvg_grp_name, var_value)

    # Look up variable labels and build descriptions
    sorted_group <- sorted_group %>%
      dplyr::mutate(
        varlabel = vapply(seq_len(dplyr::n()), function(idx) {
          get_var_label(domain[idx], var_name[idx], domain_data)
        }, character(1L)),
        var_desc = dplyr::if_else(
          !is.na(varlabel) & nzchar(varlabel),
          str_c(str_trim(varlabel), " (", str_trim(var_name), ")"),
          str_trim(var_name)
        ),
        partition_desc = dplyr::if_else(
          !is.na(partition) & nzchar(as.character(partition)),
          as.character(partition),
          "N/A"
        ),
        placeholder = NA_character_
      )

    # Keep raw sorted data for merge span computation in xml_out
    sl_raw_group <- sorted_group %>%
      dplyr::select(group_name, domain, partition_desc, var_desc,
                     placeholder, var_value, dsvg_grp_name)

    # Format for display: blank repeated values (SAS lines 254-266)
    # Detect BY-group first occurrences (hierarchical)
    sl_out_group <- sorted_group %>%
      dplyr::mutate(
        is_first_gn = dplyr::row_number() == 1L |
          na_safe_ne(group_name, dplyr::lag(group_name)),
        is_first_part = is_first_gn |
          na_safe_ne(partition, dplyr::lag(partition)),
        is_first_vn = is_first_part |
          na_safe_ne(var_name, dplyr::lag(var_name)),
        is_first_dsvg = is_first_vn |
          na_safe_ne(dsvg_grp_name, dplyr::lag(dsvg_grp_name)),
        group_name = dplyr::if_else(is_first_gn, group_name, ""),
        domain = dplyr::if_else(is_first_gn, domain, ""),
        partition_desc = dplyr::if_else(is_first_part, partition_desc, ""),
        var_desc = dplyr::if_else(is_first_vn, var_desc, ""),
        dsvg_grp_name = dplyr::if_else(is_first_dsvg, dsvg_grp_name, "")
      ) %>%
      dplyr::select(group_name, domain, partition_desc, var_desc,
                     placeholder, var_value, dsvg_grp_name)
  } else {
    sl_out_group <- dplyr::tibble(
      group_name = character(), domain = character(),
      partition_desc = character(), var_desc = character(),
      placeholder = character(), var_value = character(),
      dsvg_grp_name = character()
    )
    sl_raw_group <- sl_out_group
  }

  # ---------------------------------------------------------------------- #
  # Subset detail table (SAS lines 280-446)
  # ---------------------------------------------------------------------- #
  cli::cli_alert_info("SUBSET DETAIL")

  if (sl_subset_nobs > 0L) {
    # Sort and rename (SAS line 286, rename name=subset_name)
    sorted_subset <- sl_subset %>%
      dplyr::rename(subset_name = name) %>%
      dplyr::arrange(subset_name, domain, partition, var_name, var_value)

    # Look up variable labels
    sorted_subset <- sorted_subset %>%
      dplyr::mutate(
        varlabel = vapply(seq_len(dplyr::n()), function(idx) {
          get_var_label(domain[idx], var_name[idx], domain_data)
        }, character(1L)),
        var_desc = dplyr::if_else(
          !is.na(varlabel) & nzchar(varlabel),
          str_c(str_trim(varlabel), " (", str_trim(var_name), ")"),
          as.character(var_name)
        ),
        partition_desc = dplyr::if_else(
          !is.na(partition) & nzchar(as.character(partition)),
          as.character(partition),
          "N/A"
        )
      )

    # Condition numbering (SAS lines 334-342)
    # Detect BY-group first occurrences (hierarchical within subset)
    # Uses na_safe_ne() for NA-safe comparisons (partition may be NA)
    sorted_subset <- sorted_subset %>%
      dplyr::mutate(
        is_first_subset = dplyr::row_number() == 1L |
          na_safe_ne(subset_name, dplyr::lag(subset_name)),
        is_first_domain = is_first_subset |
          na_safe_ne(domain, dplyr::lag(domain)),
        is_first_partition = is_first_domain |
          na_safe_ne(partition, dplyr::lag(partition)),
        is_first_var_name = is_first_partition |
          na_safe_ne(var_name, dplyr::lag(var_name)),
        condition_no = cumsum(is_first_subset)
      ) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(
        condition_sub_no = cumsum(is_first_var_name)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        condition = str_c(as.character(condition_no), ".",
                          as.character(condition_sub_no))
      )

    # Condition count per subset (SAS lines 298-305)
    # Uses summarise() explicitly for group-level aggregation
    sl_subset_condition <- sorted_subset %>%
      dplyr::distinct(subset_name, partition, var_name) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::summarise(condition_count = dplyr::n(), .groups = "drop")

    # Build operator descriptions (SAS lines 393-418)
    sorted_subset <- sorted_subset %>%
      dplyr::left_join(sl_subset_condition, by = "subset_name") %>%
      dplyr::mutate(
        operator = vapply(seq_len(dplyr::n()), function(idx) {
          if (!is_first_subset[idx]) return("")
          io <- inner_operator[idx]
          if (is.na(io) || !nzchar(str_trim(io))) return("N/A")
          build_operator_desc(condition_no[idx], condition_count[idx], io)
        }, character(1L))
      )

    # Keep raw sorted data (for merge span computation)
    sl_raw_subset <- sorted_subset %>%
      dplyr::select(subset_name, domain, partition_desc, var_desc,
                     condition, var_value, operator)

    # Format for display: blank repeated values (SAS lines 422-434)
    sl_out_subset <- sorted_subset %>%
      dplyr::mutate(
        subset_name = dplyr::if_else(is_first_subset, subset_name, ""),
        domain = dplyr::if_else(is_first_domain, domain, ""),
        partition_desc = dplyr::if_else(is_first_partition, partition_desc, ""),
        var_desc = dplyr::if_else(is_first_var_name, var_desc, ""),
        condition = dplyr::if_else(is_first_var_name, condition, "")
      ) %>%
      dplyr::select(subset_name, domain, partition_desc, var_desc,
                     condition, var_value, operator)
  } else {
    sl_out_subset <- dplyr::tibble(
      subset_name = character(), domain = character(),
      partition_desc = character(), var_desc = character(),
      condition = character(), var_value = character(),
      operator = character()
    )
    sl_raw_subset <- sl_out_subset
  }

  # ---------------------------------------------------------------------- #
  # Custom datasets (SAS lines 451-464)
  # ---------------------------------------------------------------------- #
  cli::cli_alert_info("CUSTOM DATASETS")
  sl_custom_ds <- ""
  if (!is.null(sl_datasets) && is.data.frame(sl_datasets) &&
      nrow(sl_datasets) > 0L) {
    custom <- sl_datasets %>%
      dplyr::filter(default == "N") %>%
      dplyr::pull(datatype)
    if (length(custom) > 0L) {
      sl_custom_ds <- str_c(custom, collapse = ", ")
    }
  }

  # ---------------------------------------------------------------------- #
  # Return all results as a named list
  # Replaces SAS global macro variables (call symputx)
  # ---------------------------------------------------------------------- #
  list(
    sl_group_desc      = sl_group_desc,
    sl_subset_desc     = sl_subset_desc,
    sl_subset_operator = sl_subset_operator,
    sl_gs_desc         = sl_gs_desc,
    sl_custom_ds       = sl_custom_ds,
    sl_out_group       = sl_out_group,
    sl_out_subset      = sl_out_subset,
    sl_raw_group       = sl_raw_group,
    sl_raw_subset      = sl_raw_subset
  )
}


# =========================================================================== #
# group_subset_xls_out() — Excel Workbook Output (Template Style)
# Replaces SAS %group_subset_xls_out macro (lines 471-559 of sl_gs_output.sas)
# =========================================================================== #
#' Excel Workbook Output (Template Style)
#'
#' Write grouping/subsetting detail tables and summary info to an Excel
#' workbook using the template-style layout (direct data write).
#' Replaces SAS PCFILES/JET engine-based Excel writes.
#'
#' @param gs_file   Character. Output file path for the Excel workbook.
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla    Character. NDA/BLA identifier. Default \code{""}.
#' @param studyid   Character. Study identifier. Default \code{""}.
#'
#' @return Invisible NULL. Workbook is written to \code{gs_file}.
#' @export
group_subset_xls_out <- function(gs_file,
                                 pp_result,
                                 ndabla = "",
                                 studyid = "") {

  cli::cli_alert_info("GROUPING/SUBSETTING EXCEL TEMPLATE OUTPUT")

  # Row counts (SAS lines 475-481)
  # Use dplyr::count for explicit row counting
  sl_group_row_count <- if (nrow(pp_result$sl_out_group) > 0L) {
    pp_result$sl_out_group %>% dplyr::count() %>% dplyr::pull(n)
  } else {
    0L
  }
  sl_subset_row_count <- if (nrow(pp_result$sl_out_subset) > 0L) {
    pp_result$sl_out_subset %>% dplyr::count() %>% dplyr::pull(n)
  } else {
    0L
  }

  # Build info dataset (SAS lines 484-519)
  note_val <- dplyr::case_when(
    sl_group_row_count == 0L & sl_subset_row_count > 0L ~
      "No grouping was used.",
    sl_group_row_count > 0L & sl_subset_row_count == 0L ~
      "No subsetting was used.",
    sl_group_row_count == 0L & sl_subset_row_count == 0L ~
      "Neither grouping nor subsetting were used.",
    TRUE ~ ""
  )

  # Construct info dataset using bind_rows for structured row composition
  sl_out_group_subset_info <- dplyr::bind_rows(
    dplyr::tibble(val_desc = "Grouped by", val = pp_result$sl_group_desc),
    dplyr::tibble(val_desc = "Subset by", val = pp_result$sl_subset_desc),
    dplyr::tibble(val_desc = "Group row count",
                  val = as.character(sl_group_row_count)),
    dplyr::tibble(val_desc = "Subset row count",
                  val = as.character(sl_subset_row_count)),
    dplyr::tibble(val_desc = "Subset operator",
                  val = pp_result$sl_subset_operator),
    dplyr::tibble(val_desc = "Note", val = note_val),
    dplyr::tibble(val_desc = "GS description", val = pp_result$sl_gs_desc)
  )

  # Create workbook and write sheets (SAS lines 524-557)
  wb <- openxlsx::createWorkbook()

  # Sheet 1: Group Detail (SAS lines 545-547)
  openxlsx::addWorksheet(wb, "group_detail")
  if (sl_group_row_count > 0L) {
    # Rename columns to match SAS labels (SAS lines 268-274)
    group_out <- pp_result$sl_out_group
    names(group_out) <- c("Grouping Rule Name", "Domain",
                          "For Observations Where...", "Variable",
                          "", "Original Variable Value",
                          "Grouped Variable Value")
    openxlsx::writeData(wb, "group_detail", group_out, startRow = 1L)
  }

  # Sheet 2: Subset Detail (SAS lines 549-551)
  openxlsx::addWorksheet(wb, "subset_detail")
  if (sl_subset_row_count > 0L) {
    subset_out <- pp_result$sl_out_subset
    names(subset_out) <- c("Subset Rule Name", "Domain",
                           "For Observations Where...", "Variable",
                           "Condition Number", "Variable Value",
                           "Which Conditions Apply?")
    openxlsx::writeData(wb, "subset_detail", subset_out, startRow = 1L)
  }

  # Sheet 3: Group Subset Info (SAS lines 553-555)
  openxlsx::addWorksheet(wb, "group_subset_info")
  openxlsx::writeData(wb, "group_subset_info", sl_out_group_subset_info,
                       startRow = 1L)

  # Save workbook (SAS libname xls clear)
  openxlsx::saveWorkbook(wb, gs_file, overwrite = TRUE)
  cli::cli_alert_info("Workbook saved to {.file {gs_file}}")
  invisible(NULL)
}


# =========================================================================== #
# group_subset_xml_out() — Excel Workbook Output (XML-Style Rich Layout)
# Replaces SAS %group_subset_xml_out macro (lines 565-1132 of sl_gs_output.sas)
# =========================================================================== #
#' Excel Workbook Output (XML-Style Rich Layout)
#'
#' Add a richly formatted "Grouping and Subsetting" worksheet to an existing
#' openxlsx workbook. This replicates the SAS SpreadsheetML XML output with
#' styled headers, explanatory text, column headers, data tables with merged
#' cells, and print-ready page setup.
#'
#' Uses \code{create_styles()} for the shared style gallery,
#' \code{annotate_data()} for cell-level annotation, and
#' \code{write_annotated()} for structured output — all from xml_output.R.
#'
#' @param wb        An openxlsx workbook object (created externally).
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla    Character. NDA/BLA identifier. Default \code{""}.
#' @param studyid   Character. Study identifier. Default \code{""}.
#' @param delete_intermediate Logical. Whether to clean up intermediate data.
#'   Replaces SAS \code{delete_im=Y} parameter. Default \code{TRUE}.
#'
#' @return The modified workbook object (invisibly).
#' @export
group_subset_xml_out <- function(wb,
                                 pp_result,
                                 ndabla = "",
                                 studyid = "",
                                 delete_intermediate = TRUE) {

  cli::cli_h2("SL GROUPING/SUBSETTING EXCEL XML OUTPUT")

  ws_name <- "Grouping and Subsetting"
  openxlsx::addWorksheet(wb, ws_name)

  # Get the shared style gallery from xml_output.R
  styles <- create_styles()

  # Column widths (SAS lines 586-594: 162, 48, 162, 162, 50.5, 162, 162, auto)
  # Converted from SAS points to openxlsx character widths
  openxlsx::setColWidths(wb, ws_name, cols = 1L:8L,
                          widths = c(23, 7, 23, 23, 7.2, 23, 23, 8.43))

  # -------------------------------------------------------------------- #
  # HEADER SECTION (SAS lines 603-628)
  # Title, NDA/BLA, Study, Run date
  # -------------------------------------------------------------------- #
  current_row <- 1L

  # Blank row
  current_row <- current_row + 1L

  # Title: "Grouping and Subsetting Summary"
  openxlsx::writeData(wb, ws_name, "Grouping and Subsetting Summary",
                       startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, ws_name, styles[["Header"]],
                      rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Blank row
  current_row <- current_row + 1L

  # NDA/BLA
  openxlsx::writeData(wb, ws_name, str_c("NDA/BLA: ", ndabla),
                       startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, ws_name, styles[["Default8"]],
                      rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Study
  openxlsx::writeData(wb, ws_name, str_c("Study: ", studyid),
                       startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, ws_name, styles[["Default8"]],
                      rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # Analysis run date (SAS line 627)
  run_date_str <- str_c("Analysis run date: ",
                         format(Sys.Date(), "%Y-%m-%d"), " ",
                         format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, ws_name, run_date_str,
                       startRow = current_row, startCol = 1L, colNames = FALSE)
  openxlsx::addStyle(wb, ws_name, styles[["Default8"]],
                      rows = current_row, cols = 1L)
  current_row <- current_row + 1L

  # -------------------------------------------------------------------- #
  # Determine content mode (SAS lines 632, 858, 1062)
  # -------------------------------------------------------------------- #
  has_group <- nrow(pp_result$sl_out_group) > 0L
  has_subset <- nrow(pp_result$sl_out_subset) > 0L

  if (has_group || has_subset) {

    # ================================================================== #
    # GROUPING SECTION (SAS lines 676-853)
    # ================================================================== #
    if (has_group) {
      cli::cli_alert_info("GROUPING")

      # Two blank rows + description (SAS lines 691-703)
      current_row <- current_row + 2L

      openxlsx::writeData(wb, ws_name, pp_result$sl_group_desc,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                          rows = current_row, cols = 1L)
      current_row <- current_row + 1L

      # Blank row
      current_row <- current_row + 1L

      # Explanatory paragraph 1 (SAS lines 706-713, MergeAcross=6, Height=50)
      group_text_1 <- str_c(
        "The table below shows the rules used for grouping values together. ",
        "Each grouping applies to a single domain and variable. The values ",
        "of that variable can be put into more than one group. For example, ",
        "if four arms from the planned arm (ARM) variable in domain DM are ",
        "put into two groups -- one with the control arm and the other with ",
        "the three other arms -- this table would show four values in the ",
        "Original Value column mapped onto two values in the Grouped Value ",
        "column."
      )
      openxlsx::writeData(wb, ws_name, group_text_1,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["Default10Wrap"]],
                          rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, ws_name, cols = 1L:7L, rows = current_row)
      current_row <- current_row + 1L

      # Explanatory paragraph 2 (SAS lines 715-719, Height=12.75)
      group_text_2 <- str_c(
        "If a grouping for the LB or VS domain has a non-empty cell in the ",
        "Test column, that grouping is applied only to lab or vital sign ",
        "tests of the kind stated in the Test column. "
      )
      openxlsx::writeData(wb, ws_name, group_text_2,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["Default10Wrap"]],
                          rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, ws_name, cols = 1L:7L, rows = current_row)
      current_row <- current_row + 1L

      # Blank row
      current_row <- current_row + 1L

      # Column headers (SAS lines 728-744)
      group_headers <- c("Grouping Name", "Domain", "Test",
                         "Variable", "", "Original Value", "Grouped Value")
      for (ci in seq_along(group_headers)) {
        openxlsx::writeData(wb, ws_name, group_headers[ci],
                             startRow = current_row, startCol = ci,
                             colNames = FALSE)
        openxlsx::addStyle(wb, ws_name, styles[["ColumnOutline"]],
                            rows = current_row, cols = ci)
      }
      # Merge "Variable" header across cols 4-5 (MergeAcross=1)
      openxlsx::mergeCells(wb, ws_name, cols = 4L:5L, rows = current_row)
      current_row <- current_row + 1L

      # Data table with merge spans (SAS lines 750-819)
      data_start_row <- current_row
      raw_grp <- pp_result$sl_raw_group

      # Compute merge spans from raw data
      merge_info <- compute_merge_spans(
        raw_grp,
        c("group_name", "partition_desc", "var_desc", "dsvg_grp_name")
      )

      # Annotate the formatted data using annotate_data()
      ann <- annotate_data(pp_result$sl_out_group, style_id = "GS_BTLRB")

      if (nrow(ann) > 0L) {
        # Join merge info with annotation by row_num
        ann <- ann %>%
          dplyr::left_join(
            merge_info %>%
              dplyr::select(row, group_name_n, partition_desc_n,
                             var_desc_n, dsvg_grp_name_n),
            by = c("row_num" = "row")
          )

        # Apply cell-level merge and style logic (SAS lines 766-816)
        ann <- ann %>%
          dplyr::mutate(
            # Delete placeholder column cells
            keep = !(var_name == "placeholder"),

            # var_desc needs both horizontal (cols D:E) AND vertical merge.
            # openxlsx cannot handle overlapping linear merges — both
            # merge_across and merge_down on the same cell cause a conflict.
            # Solution: var_desc merges are applied as rectangular merges
            # AFTER write_annotated; we store the spans but set NA here.
            merge_across = NA_integer_,

            # Set MergeDown for non-var_desc columns only.
            # var_desc vertical merges are handled post-write.
            merge_down = dplyr::case_when(
              var_name %in% c("group_name", "domain",
                              "partition_desc") & !is.na(group_name_n) ~
                group_name_n,
              var_name == "dsvg_grp_name" & !is.na(dsvg_grp_name_n) ~
                dsvg_grp_name_n,
              TRUE ~ NA_integer_
            ),

            # Delete continuation rows for merged columns
            keep = dplyr::case_when(
              !keep ~ FALSE,
              var_name %in% c("group_name", "domain", "partition_desc",
                              "var_desc") & is.na(group_name_n) ~ FALSE,
              var_name == "dsvg_grp_name" & is.na(dsvg_grp_name_n) ~ FALSE,
              TRUE ~ TRUE
            ),

            # Style assignment (SAS lines 810-815)
            style_id = dplyr::case_when(
              var_name != "var_value" ~ "GS_BTLRB",
              var_name == "var_value" & !is.na(dsvg_grp_name_n) &
                is_bottom ~ "GS_BTLRB",
              var_name == "var_value" & !is.na(dsvg_grp_name_n) ~ "GS_BTLR",
              var_name == "var_value" & is.na(dsvg_grp_name_n) &
                is_bottom ~ "GS_BLRB",
              var_name == "var_value" ~ "GS_BLR",
              TRUE ~ style_id
            )
          )

        # Extract var_desc rectangular merge regions BEFORE filtering
        # These need cols 4:5 merged as a rectangle (merge_across + merge_down)
        var_desc_rect_merges <- ann %>%
          dplyr::filter(var_name == "var_desc", keep, !is.na(group_name_n)) %>%
          dplyr::select(row_num, span = group_name_n)

        # Now filter and clean up annotations
        ann <- ann %>%
          dplyr::filter(keep) %>%
          dplyr::select(-keep, -group_name_n, -partition_desc_n,
                         -var_desc_n, -dsvg_grp_name_n)

        # Write annotated data using write_annotated()
        current_row <- write_annotated(
          wb, ws_name, ann, styles,
          start_row = data_start_row - 1L, start_col = 0L
        )

        # Apply rectangular merges for var_desc (cols D:E) post-write
        # This replaces the conflicting merge_across + merge_down combination
        row_offset <- data_start_row - 1L
        for (mi in seq_len(nrow(var_desc_rect_merges))) {
          vd_row <- var_desc_rect_merges$row_num[mi] + row_offset
          vd_span <- var_desc_rect_merges$span[mi]
          if (!is.na(vd_span) && vd_span > 0L) {
            # Rectangular merge: cols 4:5, rows start to start+span
            openxlsx::mergeCells(wb, ws_name,
                                  cols = 4L:5L,
                                  rows = vd_row:(vd_row + vd_span))
          } else {
            # Single-row horizontal merge only
            openxlsx::mergeCells(wb, ws_name,
                                  cols = 4L:5L, rows = vd_row)
          }
        }
      }
    } else {
      # No grouping (SAS lines 828-853)
      cli::cli_alert_info("NO GROUPING")
      current_row <- current_row + 2L
      openxlsx::writeData(wb, ws_name, pp_result$sl_group_desc,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                          rows = current_row, cols = 1L)
      current_row <- current_row + 1L
    }

    # ================================================================== #
    # SUBSETTING SECTION (SAS lines 855-1059)
    # ================================================================== #
    if (has_subset) {
      cli::cli_alert_info("SUBSETTING")

      # Two blank rows + description (SAS lines 869-878)
      current_row <- current_row + 2L

      openxlsx::writeData(wb, ws_name, pp_result$sl_subset_desc,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                          rows = current_row, cols = 1L)
      current_row <- current_row + 1L

      # Blank row
      current_row <- current_row + 1L

      # Explanatory paragraph 1 (SAS lines 885-892, Height=50)
      subset_text_1 <- str_c(
        "The table below shows the rules for subsetting subjects and those ",
        "subjects' observations in other domains. For a subject to be ",
        "included in the subset and used in analysis, they must have at ",
        "least one value in the Value column for the associated variable ",
        "in the Variable column. For example, if the Value column has ",
        "values 5 through 10 for domain LB and variable LBSTRESN, a ",
        "subject must have at least one lab test in LB with LBSTRESN ",
        "from 5 and 10."
      )
      openxlsx::writeData(wb, ws_name, subset_text_1,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["Default10Wrap"]],
                          rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, ws_name, cols = 1L:7L, rows = current_row)
      current_row <- current_row + 1L

      # Explanatory paragraph 2 (SAS lines 896-902, Height=50)
      subset_text_2 <- str_c(
        "Each subset can be made up of several conditions and these are ",
        "numbered in the Condition No. column. The 'Which Conditions ",
        "Apply? (AND vs OR)' column states whether for a given subset, ",
        "all its conditions must be true for a subject to be included ",
        "in the subset, or any one of them being true will suffice. If ",
        "all must be true, this column will list out all the subset's ",
        "conditions separated by an 'AND'. If only one must be true, ",
        "the subset's conditions will be separated by an 'OR'."
      )
      openxlsx::writeData(wb, ws_name, subset_text_2,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["Default10Wrap"]],
                          rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, ws_name, cols = 1L:7L, rows = current_row)
      current_row <- current_row + 1L

      # Explanatory paragraph 3 (SAS lines 904-911, Height=50)
      subset_text_3 <- str_c(
        "If a subset using the LB or VS domain has a non-empty cell in ",
        "the Test column, only lab or vital sign tests of the kind stated ",
        "in the Test column are used to determine whether a subject should ",
        "be included in the subset. For example, if the domain is LB and ",
        "lab test is ALBUMIN and variable LBSTRESN must have values from ",
        "5 to 10, only subjects with albumin lab test results from 5 to ",
        "10 are included in the subset and used in subsequent analysis."
      )
      openxlsx::writeData(wb, ws_name, subset_text_3,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["Default10Wrap"]],
                          rows = current_row, cols = 1L)
      openxlsx::mergeCells(wb, ws_name, cols = 1L:7L, rows = current_row)
      current_row <- current_row + 1L

      # Operator narrative (SAS line 915)
      if (nzchar(pp_result$sl_subset_operator) &&
          pp_result$sl_subset_operator != "N/A") {
        openxlsx::writeData(wb, ws_name, pp_result$sl_subset_operator,
                             startRow = current_row, startCol = 1L,
                             colNames = FALSE)
        openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                            rows = current_row, cols = 1L)
        current_row <- current_row + 1L
      }

      # Blank row
      current_row <- current_row + 1L

      # Column headers (SAS lines 925-941)
      subset_headers <- c("Subset Name", "Domain", "Test", "Variable",
                          "Condition No.", "Value",
                          "Which Conditions Apply?\n(AND vs OR)")
      for (ci in seq_along(subset_headers)) {
        openxlsx::writeData(wb, ws_name, subset_headers[ci],
                             startRow = current_row, startCol = ci,
                             colNames = FALSE)
        openxlsx::addStyle(wb, ws_name, styles[["ColumnOutline"]],
                            rows = current_row, cols = ci)
      }
      current_row <- current_row + 1L

      # Data table with merge spans (SAS lines 946-1031)
      data_start_row <- current_row
      raw_sub <- pp_result$sl_raw_subset

      # Compute merge spans from raw data
      merge_info_sub <- compute_merge_spans(
        raw_sub,
        c("subset_name", "domain", "partition_desc", "var_desc")
      )

      # Annotate the formatted data
      ann_sub <- annotate_data(pp_result$sl_out_subset,
                               style_id = "GS_BTLRB")

      if (nrow(ann_sub) > 0L) {
        # Join merge info with annotation
        ann_sub <- ann_sub %>%
          dplyr::left_join(
            merge_info_sub %>%
              dplyr::select(row, subset_name_n, domain_n,
                             partition_desc_n, var_desc_n),
            by = c("row_num" = "row")
          )

        # Apply cell-level merge and style logic (SAS lines 991-1022)
        ann_sub <- ann_sub %>%
          dplyr::mutate(
            # MergeDown based on variable (SAS lines 991-1009)
            merge_down = dplyr::case_when(
              var_name %in% c("subset_name", "operator") &
                !is.na(subset_name_n) ~ subset_name_n,
              var_name == "domain" & !is.na(domain_n) ~ domain_n,
              var_name == "partition_desc" & !is.na(partition_desc_n) ~
                partition_desc_n,
              var_name %in% c("var_desc", "condition") &
                !is.na(var_desc_n) ~ var_desc_n,
              TRUE ~ NA_integer_
            ),

            # Delete continuation rows for merged columns
            keep = dplyr::case_when(
              var_name %in% c("subset_name", "operator") &
                is.na(subset_name_n) ~ FALSE,
              var_name == "domain" & is.na(domain_n) ~ FALSE,
              var_name == "partition_desc" & is.na(partition_desc_n) ~ FALSE,
              var_name %in% c("var_desc", "condition") &
                is.na(var_desc_n) ~ FALSE,
              TRUE ~ TRUE
            ),

            # Style assignment (SAS lines 1015-1021)
            style_id = dplyr::case_when(
              var_name == "condition" ~ "GSC_BTLRB",
              var_name != "var_value" ~ "GS_BTLRB",
              var_name == "var_value" & !is.na(var_desc_n) &
                is_bottom ~ "GS_BTLRB",
              var_name == "var_value" & !is.na(var_desc_n) ~ "GS_BTLR",
              var_name == "var_value" & is.na(var_desc_n) &
                is_bottom ~ "GS_BLRB",
              var_name == "var_value" ~ "GS_BLR",
              TRUE ~ style_id
            )
          ) %>%
          dplyr::filter(keep) %>%
          dplyr::select(-keep, -subset_name_n, -domain_n,
                         -partition_desc_n, -var_desc_n)

        # Write annotated data
        current_row <- write_annotated(
          wb, ws_name, ann_sub, styles,
          start_row = data_start_row - 1L, start_col = 0L
        )
      }
    } else {
      # No subsetting (SAS lines 1034-1058)
      cli::cli_alert_info("NO SUBSETTING")
      current_row <- current_row + 2L
      openxlsx::writeData(wb, ws_name, pp_result$sl_subset_desc,
                           startRow = current_row, startCol = 1L,
                           colNames = FALSE)
      openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                          rows = current_row, cols = 1L)
      current_row <- current_row + 1L
    }

  } else {
    # Neither grouping nor subsetting (SAS lines 1062-1092)
    cli::cli_alert_info("NO GROUPING OR SUBSETTING")
    current_row <- current_row + 2L
    openxlsx::writeData(
      wb, ws_name,
      "Neither grouping nor subsetting were used",
      startRow = current_row, startCol = 1L, colNames = FALSE
    )
    openxlsx::addStyle(wb, ws_name, styles[["SubHeader"]],
                        rows = current_row, cols = 1L)
    current_row <- current_row + 1L
  }

  # -------------------------------------------------------------------- #
  # Page setup (SAS lines 1094-1112)
  # Landscape orientation, header/footer, fit to page
  # -------------------------------------------------------------------- #
  openxlsx::pageSetup(wb, ws_name,
                       orientation = "landscape",
                       fitToWidth = TRUE,
                       fitToHeight = FALSE)

  openxlsx::setHeaderFooter(
    wb, ws_name,
    header = c(
      str_c("Grouping and Subsetting Summary"),
      NA_character_,
      str_c("NDA/BLA ", ndabla, "\nStudy ", studyid)
    ),
    footer = c(NA_character_, "Page &P of &N", NA_character_)
  )

  invisible(wb)
}


# =========================================================================== #
# MIGRATION NOTES
# =========================================================================== #
# ============================================================
#### MIGRATION NOTES
#### ============================================================
#
#### ASSUMPTIONS:
####   1. SAS SL_GROUP, SL_SUBSET, SL_DATASETS datasets are passed as
####      tibbles/data.frames with the expected column names. In SAS,
####      these were available in the WORK library; in R they are explicit
####      function parameters.
####   2. Variable labels are accessible via attr(df$var, "label") on
####      haven-imported datasets. The optional domain_data parameter
####      provides these datasets keyed by domain name (e.g. "DM", "LB").
####   3. SAS global macro variables (&sl_group_desc., &sl_subset_desc.,
####      &sl_subset_operator., &sl_gs_desc., &sl_custom_ds.) are returned
####      as elements of the pp_result list instead of being set globally.
####   4. The placeholder column in sl_out_group preserves the SAS RETAIN
####      variable ordering required for proper column-position alignment
####      in the XML-style output.
####   5. The %gs_rows merge-span macro is replaced by compute_merge_spans()
####      which uses run-length encoding on hierarchically grouped data.
####   6. SAS hash lookups (DECLARE HASH) for sl_subset_condition and
####      partition variables are replaced by dplyr::left_join().
#
#### POTENTIAL NUMERICAL DIFFERENCES:
####   None — this module produces narrative descriptions and metadata
####   output only. No statistical computations are performed.
####   Row ordering may differ from SAS due to locale-aware sort (R
####   uses locale collation; SAS uses byte-order by default). Use
####   Sys.setlocale("LC_COLLATE", "C") for byte-order sort if needed.
#
#### NO DIRECT R EQUIVALENT:
####   1. SAS OPEN()/VARNUM()/VARLABEL() functions for runtime dataset
####      introspection -> haven label attributes via attr(df$var, "label")
####      accessed through the get_var_label() helper function.
####   2. SAS PCFILES/JET engine for direct Excel writes ->
####      openxlsx::createWorkbook() + saveWorkbook() pipeline.
####   3. SAS SpreadsheetML XML streaming (DATA _NULL_ + PUT) ->
####      openxlsx in-memory workbook API with annotate_data() +
####      write_annotated() from xml_output.R.
####   4. SAS %markup/%annotate macros for XML cell construction ->
####      annotate_data() creates a tibble of cell metadata, then
####      write_annotated() applies styles and merges via openxlsx API.
####   5. SAS call symputx() for setting global macro variables ->
####      function return list elements.
####   6. SAS DATA step RETAIN for BY-group first/last detection ->
####      dplyr::lag() comparisons for hierarchical group boundaries.
####   7. SAS SLEEP(1) for PCFILES engine delay -> not needed in R
####      since openxlsx operates entirely in memory.
#
#### PACKAGE SELECTION RATIONALE:
####   - openxlsx: Replaces both SpreadsheetML XML generation and
####     PCFILES/JET engine. Provides full cell-level formatting,
####     merging, page setup, and header/footer support without
####     requiring Java (unlike xlsx package).
####   - dplyr: Core tidyverse data manipulation replacing PROC SQL
####     and DATA step BY-group processing. AAP mandates tidyverse
####     over base R.
####   - stringr: Tidyverse string manipulation for Oxford comma logic
####     (str_replace replacing SAS TRANWRD), description building
####     (str_c replacing || concatenation), and operator cleanup
####     (str_squish replacing SAS COMPBL).
####   - cli: User-facing messages replacing SAS %put statements.
####     Provides structured, colored console output.
#
#### OPEN QUESTIONS:
####   1. Verify variable label extraction works correctly with
####      haven-imported datasets. The attr(df$var, "label") approach
####      depends on labels being preserved during data import.
####   2. Exact column width mapping from SAS SpreadsheetML points to
####      openxlsx character widths is approximate (162pt -> 23 chars).
####   3. Merged cell rendering in older Excel versions may differ
####      slightly from SAS SpreadsheetML output.
####   4. The SAS source used PROC DATASETS to clean intermediate
####      datasets; in R, garbage collection handles this automatically
####      but the delete_intermediate parameter is preserved for
####      explicit control if needed.
####   5. SAS COMPBL behavior for multiple consecutive spaces is
####      replicated by stringr::str_squish() which also trims
####      leading/trailing whitespace — verify this matches for all
####      edge cases in operator descriptions.
#
#### ============================================================
