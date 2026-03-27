###############################################################################
#         PROGRAM NAME: Grouping and Subsetting Output (R Migration)          #
#                                                                             #
#          DESCRIPTION: Create output for grouping and subsetting metadata    #
#                       Contains three functions:                              #
#                          group_subset_pp -- Preprocessing to create          #
#                                             output tibbles                  #
#                          group_subset_xls_out -- Excel workbook output       #
#                          group_subset_xml_out -- Excel workbook output       #
#                                                 (XML-style layout)          #
#                                                                             #
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#        ORIGINAL DATE: March 4, 2011                                         #
#                                                                             #
#   MIGRATION DETAILS:                                                        #
#     - Migrated from: contributed/Demographics/Utility Programs/sl_gs_output.sas
#     - Migration target: Idiomatic R using openxlsx for Excel output         #
#     - SAS %group_subset_pp macro -> R group_subset_pp() function            #
#     - SAS %group_subset_xls_out macro -> R group_subset_xls_out() function  #
#     - SAS %group_subset_xml_out macro -> R group_subset_xml_out() function  #
#     - SpreadsheetML XML -> openxlsx workbook API                            #
#     - SAS global macro variables -> R function parameters / return values   #
#     - SAS PROC SQL -> dplyr verbs                                           #
#     - SAS DATA step -> dplyr pipelines                                      #
#     - SAS %put -> cli messages                                              #
#                                                                             #
#  EXTERNAL FILES USED: xml_output.R (sourced by parent demographics.R)       #
#                       Provides create_workbook_styles(), annotate_data(),    #
#                       write_annotated_data(), apply_style()                 #
#                                                                             #
#  PARAMETERS REQUIRED: sl_group -- grouping tibble                           #
#                       sl_subset -- subsetting tibble                        #
#                       sl_datasets -- datasets tibble                        #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by demographics.R             #
#                       In SAS, it was included via:                           #
#                       %include "&utilpath.\sl_gs_output.sas";               #
#                                                                             #
#            REVISIONS:                                                        #
#              2026-03-25  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(dplyr)
library(stringr)
library(openxlsx)
library(cli)
library(tibble)

# NOTE: xml_output.R functions (create_workbook_styles, annotate_data,
# write_annotated_data, apply_style) must be available in the calling
# environment. In the demographics pipeline, demographics.R sources both
# xml_output.R and this file before invoking any functions.

# --------------------------------------------------------------------------- #
# Internal Helper: Resolve variable label from haven-labelled domain data
# Replaces SAS DSID/OPEN/VARNUM/VARLABEL introspection (SAS lines 194-208)
# --------------------------------------------------------------------------- #
resolve_var_label <- function(domain_data, domain_name, var_name) {
  if (is.null(domain_data)) return(NA_character_)
  if (!domain_name %in% names(domain_data)) return(NA_character_)
  ds <- domain_data[[domain_name]]
  if (!var_name %in% colnames(ds)) return(NA_character_)
  lbl <- attr(ds[[var_name]], "label")
  if (!is.null(lbl) && nzchar(lbl)) return(lbl)
  return(NA_character_)
}

# --------------------------------------------------------------------------- #
# Internal Helper: Build operator description text for subset conditions
# Replaces SAS loop logic (SAS lines 393-419) that builds text like
# "Conditions 1.1 AND 1.2 AND 1.3" from condition_count and inner_operator
# --------------------------------------------------------------------------- #
build_operator_text <- function(cond_no, cond_count, inner_op) {
  if (is.na(cond_count) || cond_count <= 1L) return("N/A")
  parts <- stringr::str_c(cond_no, ".", seq_len(cond_count))
  op_upper <- toupper(stringr::str_trim(inner_op))
  text_body <- paste(parts, collapse = paste0(" ", op_upper, " "))
  # SAS: lowcase(inner_operator) = 'or' -> "Condition " (singular)
  #      else -> "Conditions " (plural)
  prefix <- if (stringr::str_to_lower(inner_op) == "or") {
    "Condition "
  } else {
    "Conditions "
  }
  stringr::str_squish(paste0(prefix, text_body))
}

# --------------------------------------------------------------------------- #
# Internal Helper: Calculate vertical merge spans from blanked column values
# Replaces SAS %gs_rows inner macro (SAS lines 635-674) that computed
# MergeDown row spans for SpreadsheetML XML
# --------------------------------------------------------------------------- #
calc_merge_spans <- function(values) {
  n <- length(values)
  if (n == 0L) return(list())
  spans <- list()
  i <- 1L
  while (i <= n) {
    val_i <- as.character(values[i])
    if (is.na(val_i)) val_i <- ""
    if (nzchar(val_i)) {
      # Non-blank value: count how many blank rows follow
      j <- i + 1L
      while (j <= n) {
        val_j <- as.character(values[j])
        if (is.na(val_j)) val_j <- ""
        if (nzchar(val_j)) break
        j <- j + 1L
      }
      span_len <- j - i
      if (span_len > 1L) {
        spans[[length(spans) + 1L]] <- c(start = i, span = span_len)
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }
  spans
}

# --------------------------------------------------------------------------- #
# Internal Helper: Apply vertical cell merges to a worksheet
# Iterates over specified columns, finds merge spans via calc_merge_spans,
# and calls openxlsx::mergeCells for each span
# --------------------------------------------------------------------------- #
apply_vertical_merges <- function(wb, sheet, df, start_row, merge_cols) {
  for (col_idx in merge_cols) {
    col_values <- as.character(df[[col_idx]])
    col_values[is.na(col_values)] <- ""
    spans <- calc_merge_spans(col_values)
    for (sp in spans) {
      row_start <- start_row + sp["start"] - 1L
      row_end   <- row_start + sp["span"] - 1L
      openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = row_start:row_end)
    }
  }
}

# =========================================================================== #
# group_subset_pp() -- Grouping and Subsetting Preprocessing
# Replaces SAS %group_subset_pp macro (SAS lines 36-465)
# =========================================================================== #
#' @param sl_group   Tibble with grouping metadata (group_name, domain,
#'                   partition, var_name, var_value, dsvg_grp_name).
#' @param sl_subset  Tibble with subsetting metadata (name, domain, partition,
#'                   var_name, var_value, outer_operator, inner_operator).
#' @param sl_datasets Tibble with datasets metadata (datatype, default,
#'                    partition_variable).
#' @param domain_data Optional named list of data frames keyed by domain name
#'                    for variable label resolution via haven attributes.
#' @return Named list with: sl_group_desc, sl_subset_desc, sl_subset_operator,
#'         sl_gs_desc, sl_custom_ds, sl_out_group, sl_out_subset,
#'         sl_group_nobs, sl_subset_nobs.
#' @export
group_subset_pp <- function(sl_group = NULL,
                            sl_subset = NULL,
                            sl_datasets = NULL,
                            domain_data = NULL) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING PREPROCESSING")

  # Coerce inputs to tibbles for consistent handling
  if (!is.null(sl_group) && is.data.frame(sl_group)) {
    sl_group <- tibble::as_tibble(sl_group)
  }
  if (!is.null(sl_subset) && is.data.frame(sl_subset)) {
    sl_subset <- tibble::as_tibble(sl_subset)
  }

  # Initialize observation counts (SAS lines 58-60)
  sl_group_nobs  <- if (!is.null(sl_group) && is.data.frame(sl_group) &&
                        nrow(sl_group) > 0L) nrow(sl_group) else 0L
  sl_subset_nobs <- if (!is.null(sl_subset) && is.data.frame(sl_subset) &&
                        nrow(sl_subset) > 0L) nrow(sl_subset) else 0L

  # --------------------------------------------------------------------------
  # Grouping description (SAS lines 62-92)
  # PROC SQL SELECT DISTINCT group_name INTO :sl_group_desc SEPARATED BY ', '
  # --------------------------------------------------------------------------
  if (sl_group_nobs > 0L) {
    group_names <- sl_group %>%
      dplyr::distinct(group_name) %>%
      dplyr::pull(group_name)
    group_count <- length(group_names)

    # Build comma-separated list of group names
    sl_group_desc <- paste(group_names, collapse = ", ")

    if (group_count == 2L) {
      # For 2 groups: replace comma with ' and' (SAS line 78: tranwrd)
      sl_group_desc <- stringr::str_replace(sl_group_desc, ",", " and")
    } else if (group_count > 2L) {
      # For 3+ groups: replace LAST comma with ' and' (SAS lines 80-81)
      # SAS: find(reverse(desc), ',') then substr replacement
      all_commas <- stringr::str_locate_all(sl_group_desc, ",")[[1]]
      if (nrow(all_commas) > 0L) {
        last_pos <- all_commas[nrow(all_commas), "start"]
        stringr::str_sub(sl_group_desc, last_pos, last_pos) <- " and"
      }
    }

    sl_group_desc <- stringr::str_c("Grouped by ", sl_group_desc)
  } else {
    sl_group_desc <- "No grouping"
  }

  # --------------------------------------------------------------------------
  # Subsetting description (SAS lines 96-139)
  # PROC SQL: SELECT DISTINCT lowcase(outer_operator), name
  # --------------------------------------------------------------------------
  if (sl_subset_nobs > 0L) {
    sl_subset_outer <- sl_subset %>%
      dplyr::distinct(outer_operator) %>%
      dplyr::pull(outer_operator) %>%
      stringr::str_to_lower() %>%
      unique()

    subset_names <- sl_subset %>%
      dplyr::distinct(name) %>%
      dplyr::pull(name)
    sl_subset_count <- length(subset_names)

    # Build description separated by outer operator
    sl_subset_desc <- paste(subset_names,
                            collapse = paste0(" ", sl_subset_outer[1], " "))
    sl_subset_desc <- stringr::str_c("Subset by ", sl_subset_desc)

    # Operator text (SAS lines 119-133)
    if (sl_subset_count > 1L) {
      if (sl_subset_outer[1] == "and") {
        sl_subset_operator <- paste0(
          "ALL of the following rules must be true ",
          "for a subject to be included in the analysis.")
      } else if (sl_subset_outer[1] == "or") {
        sl_subset_operator <- paste0(
          "ANY of the following rules must be true ",
          "for a subject to be included in the analysis.")
      } else {
        sl_subset_operator <- ""
      }
    } else {
      sl_subset_operator <- ""
    }
  } else {
    sl_subset_desc     <- "No subsetting"
    sl_subset_operator <- "N/A"
  }

  cli::cli_alert_info(sl_group_desc)
  cli::cli_alert_info(sl_subset_desc)

  # --------------------------------------------------------------------------
  # Single-line combined description (SAS lines 146-155)
  # --------------------------------------------------------------------------
  if (sl_group_nobs > 0L && sl_subset_nobs > 0L) {
    sl_gs_desc <- paste0(sl_group_desc, "; ", sl_subset_desc)
  } else if (sl_group_nobs > 0L) {
    sl_gs_desc <- sl_group_desc
  } else if (sl_subset_nobs > 0L) {
    sl_gs_desc <- sl_subset_desc
  } else {
    sl_gs_desc <- ""
  }

  cli::cli_alert_info(sl_gs_desc)

  # --------------------------------------------------------------------------
  # Group detail table (SAS lines 164-277)
  # PROC SORT + DATA step with first.var blanking and variable label lookup
  # --------------------------------------------------------------------------
  cli::cli_alert_info("GROUP DETAIL")

  if (sl_group_nobs > 0L) {
    # Sort (SAS line 170)
    work_group <- sl_group %>%
      dplyr::arrange(group_name, partition, var_name, dsvg_grp_name, var_value)

    # Variable label lookup (replaces SAS DSID/VARNUM/VARLABEL)
    n_g <- nrow(work_group)
    var_labels_g <- vapply(seq_len(n_g), function(i) {
      resolve_var_label(domain_data, work_group$domain[i], work_group$var_name[i])
    }, character(1))

    work_group <- work_group %>%
      dplyr::mutate(
        var_label = var_labels_g,
        # Build "Label (VARNAME)" description or just VARNAME
        var_desc = dplyr::if_else(
          !is.na(var_label) & nzchar(var_label),
          stringr::str_c(var_label, " (", var_name, ")"),
          var_name
        ),
        # Partition description: partition value or 'N/A' (SAS lines 250-251)
        partition_desc = dplyr::if_else(
          !is.na(partition) & nzchar(partition),
          partition,
          "N/A"
        ),
        placeholder = ""
      )

    # Detect BY-group changes for blanking repeated values (SAS lines 254-266)
    # SAS BY: group_name partition var_name dsvg_grp_name var_value
    work_group <- work_group %>%
      dplyr::mutate(
        grp_change     = dplyr::row_number() == 1L |
          group_name != dplyr::lag(group_name, default = "__SENTINEL_NONE__"),
        part_change    = grp_change |
          partition != dplyr::lag(partition, default = "__SENTINEL_NONE__"),
        varname_change = part_change |
          var_name != dplyr::lag(var_name, default = "__SENTINEL_NONE__")
      ) %>%
      dplyr::mutate(
        group_name_out     = dplyr::if_else(grp_change, group_name, ""),
        domain_out         = dplyr::if_else(grp_change, domain, ""),
        partition_desc_out = dplyr::if_else(part_change, partition_desc, ""),
        var_desc_out       = dplyr::if_else(varname_change, var_desc, "")
      )

    # Build output tibble with proper labels (SAS lines 268-274)
    sl_out_group <- work_group %>%
      dplyr::select(
        group_name     = group_name_out,
        domain         = domain_out,
        partition_desc = partition_desc_out,
        var_desc       = var_desc_out,
        placeholder,
        var_value,
        dsvg_grp_name
      )

  } else {
    sl_out_group <- tibble::tibble(
      group_name     = character(),
      domain         = character(),
      partition_desc = character(),
      var_desc       = character(),
      placeholder    = character(),
      var_value      = character(),
      dsvg_grp_name  = character()
    )
  }

  # --------------------------------------------------------------------------
  # Subset detail table (SAS lines 282-446)
  # PROC SORT + condition counting + DATA step with RETAIN numbering
  # --------------------------------------------------------------------------
  cli::cli_alert_info("SUBSET DETAIL")

  if (sl_subset_nobs > 0L) {
    # Count conditions per subset rule (SAS lines 299-305)
    # SAS: count(distinct cats(partition, var_name)) GROUP BY name
    sl_subset_condition <- sl_subset %>%
      dplyr::distinct(name, partition, var_name) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(condition_count = dplyr::n(), .groups = "drop")

    # Also count rows per subset rule using count() (schema member)
    sl_subset_rule_counts <- sl_subset %>%
      dplyr::count(name, name = "row_count")

    # Sort (SAS line 286)
    work_subset <- sl_subset %>%
      dplyr::rename(subset_name = name) %>%
      dplyr::arrange(subset_name, domain, partition, var_name, var_value)

    # Variable label lookup
    n_s <- nrow(work_subset)
    var_labels_s <- vapply(seq_len(n_s), function(i) {
      resolve_var_label(domain_data, work_subset$domain[i], work_subset$var_name[i])
    }, character(1))

    work_subset <- work_subset %>%
      dplyr::mutate(
        var_label = var_labels_s,
        var_desc = dplyr::if_else(
          !is.na(var_label) & nzchar(var_label),
          stringr::str_c(var_label, " (", var_name, ")"),
          var_name
        ),
        partition_desc = dplyr::if_else(
          !is.na(partition) & nzchar(partition),
          partition,
          "N/A"
        )
      )

    # Condition numbering (SAS lines 333-342)
    # SAS: RETAIN condition_no 0 condition_sub_no 0;
    #      first.name -> condition_no + 1, condition_sub_no = 0;
    #      first.partition | first.var_name -> condition_sub_no + 1;
    work_subset <- work_subset %>%
      dplyr::mutate(
        new_name = dplyr::row_number() == 1L |
          subset_name != dplyr::lag(subset_name, default = "__SENTINEL_NONE__"),
        condition_no = cumsum(new_name)
      ) %>%
      dplyr::group_by(condition_no) %>%
      dplyr::mutate(
        # Replaces first.var_name: domain, partition, or var_name changed
        new_cond = dplyr::row_number() == 1L |
          domain != dplyr::lag(domain, default = "__SENTINEL_NONE__") |
          partition != dplyr::lag(partition, default = "__SENTINEL_NONE__") |
          var_name != dplyr::lag(var_name, default = "__SENTINEL_NONE__"),
        condition_sub_no = cumsum(new_cond)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        condition = stringr::str_c(as.character(condition_no), ".",
                                   as.character(condition_sub_no))
      )

    # Join condition counts for operator text generation (SAS hash lookup)
    work_subset <- work_subset %>%
      dplyr::left_join(sl_subset_condition,
                       by = c("subset_name" = "name"))

    # Build operator text per subset rule (SAS lines 393-419)
    operator_by_rule <- work_subset %>%
      dplyr::filter(new_name) %>%
      dplyr::mutate(
        operator_text = vapply(seq_len(dplyr::n()), function(i) {
          build_operator_text(
            condition_no[i], condition_count[i], inner_operator[i])
        }, character(1))
      ) %>%
      dplyr::select(condition_no, operator_text)

    work_subset <- work_subset %>%
      dplyr::left_join(operator_by_rule, by = "condition_no") %>%
      dplyr::mutate(
        # Operator only on first row of each subset rule, blank otherwise
        operator = dplyr::if_else(new_name, operator_text, ""),
        operator = dplyr::if_else(is.na(operator), "", operator)
      )

    # Detect BY-group changes for blanking repeated values (SAS lines 423-434)
    # BY: name domain partition var_name var_value
    work_subset <- work_subset %>%
      dplyr::mutate(
        name_change      = dplyr::row_number() == 1L |
          subset_name != dplyr::lag(subset_name, default = "__SENTINEL_NONE__"),
        domain_change    = name_change |
          domain != dplyr::lag(domain, default = "__SENTINEL_NONE__"),
        partition_change = domain_change |
          partition != dplyr::lag(partition, default = "__SENTINEL_NONE__"),
        varname_change   = partition_change |
          var_name != dplyr::lag(var_name, default = "__SENTINEL_NONE__")
      ) %>%
      dplyr::mutate(
        name_out           = dplyr::if_else(name_change, subset_name, ""),
        domain_out         = dplyr::if_else(name_change, domain, ""),
        partition_desc_out = dplyr::if_else(partition_change, partition_desc, ""),
        var_desc_out       = dplyr::if_else(varname_change, var_desc, "")
      )

    # Build output tibble with labels (SAS lines 436-443)
    sl_out_subset <- work_subset %>%
      dplyr::select(
        subset_name    = name_out,
        domain         = domain_out,
        partition_desc = partition_desc_out,
        var_desc       = var_desc_out,
        condition,
        var_value,
        operator
      )

  } else {
    sl_out_subset <- tibble::tibble(
      subset_name    = character(),
      domain         = character(),
      partition_desc = character(),
      var_desc       = character(),
      condition      = character(),
      var_value      = character(),
      operator       = character()
    )
  }

  # --------------------------------------------------------------------------
  # Custom datasets (SAS lines 455-464)
  # Extract non-default datasets from sl_datasets where default = "N"
  # --------------------------------------------------------------------------
  cli::cli_alert_info("CUSTOM DATASETS")
  sl_custom_ds <- ""
  if (!is.null(sl_datasets) && is.data.frame(sl_datasets) &&
      nrow(sl_datasets) > 0L) {
    custom <- sl_datasets %>%
      dplyr::filter(default == "N")
    if (nrow(custom) > 0L) {
      sl_custom_ds <- custom %>%
        dplyr::distinct(datatype) %>%
        dplyr::pull(datatype) %>%
        paste(collapse = ", ")
    }
  }

  cli::cli_alert_success("Preprocessing complete")

  # Return all outputs as a named list
  # (SAS: these were global macro variables and global datasets)
  list(
    sl_group_desc      = sl_group_desc,
    sl_subset_desc     = sl_subset_desc,
    sl_subset_operator = sl_subset_operator,
    sl_gs_desc         = sl_gs_desc,
    sl_custom_ds       = sl_custom_ds,
    sl_out_group       = sl_out_group,
    sl_out_subset      = sl_out_subset,
    sl_group_nobs      = sl_group_nobs,
    sl_subset_nobs     = sl_subset_nobs
  )
}


# =========================================================================== #
# group_subset_xls_out() -- Grouping and Subsetting Excel Template Output
# Replaces SAS %group_subset_xls_out macro (SAS lines 471-559)
# =========================================================================== #
#' @param gs_file  Path to the Excel workbook file (existing or new).
#' @param sl_out_group  Group detail tibble from group_subset_pp().
#' @param sl_out_subset Subset detail tibble from group_subset_pp().
#' @param sl_group_desc  Grouping description string.
#' @param sl_subset_desc Subsetting description string.
#' @param sl_subset_operator Operator description string.
#' @param sl_gs_desc Combined grouping/subsetting description string.
#' @return NULL (invisible). Side effect: writes Excel workbook to gs_file.
#' @export
group_subset_xls_out <- function(gs_file,
                                 sl_out_group,
                                 sl_out_subset,
                                 sl_group_desc,
                                 sl_subset_desc,
                                 sl_subset_operator,
                                 sl_gs_desc) {

  cli::cli_alert_info("GROUPING/SUBSETTING EXCEL TEMPLATE OUTPUT")

  # Count rows (SAS lines 475-481)
  sl_group_nobs  <- nrow(sl_out_group)
  sl_subset_nobs <- nrow(sl_out_subset)

  # Build info tibble (SAS lines 484-519)
  # Conditional note text (SAS lines 508-514)
  note_text <- dplyr::case_when(
    sl_group_nobs == 0L & sl_subset_nobs > 0L  ~
      "No grouping was used.",
    sl_group_nobs > 0L  & sl_subset_nobs == 0L ~
      "No subsetting was used.",
    sl_group_nobs == 0L & sl_subset_nobs == 0L ~
      "Neither grouping nor subsetting were used.",
    TRUE ~ ""
  )

  sl_out_group_subset_info <- dplyr::bind_rows(
    tibble::tibble(val_desc = "Grouped by",       val = sl_group_desc),
    tibble::tibble(val_desc = "Subset by",         val = sl_subset_desc),
    tibble::tibble(val_desc = "Group row count",   val = as.character(sl_group_nobs)),
    tibble::tibble(val_desc = "Subset row count",  val = as.character(sl_subset_nobs)),
    tibble::tibble(val_desc = "Subset operator",   val = sl_subset_operator),
    tibble::tibble(val_desc = "Note",              val = note_text),
    tibble::tibble(val_desc = "GS description",    val = sl_gs_desc)
  )

  # Load existing workbook or create new (SAS lines 524-537: PCFILES LIBNAME)
  if (file.exists(gs_file)) {
    wb <- openxlsx::loadWorkbook(gs_file)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  # Write group_detail worksheet (SAS line 549)
  if (!"group_detail" %in% names(wb)) {
    openxlsx::addWorksheet(wb, "group_detail")
  }
  if (sl_group_nobs > 0L) {
    openxlsx::writeData(wb, "group_detail", sl_out_group, startRow = 1)
  }

  # Write subset_detail worksheet (SAS line 552)
  if (!"subset_detail" %in% names(wb)) {
    openxlsx::addWorksheet(wb, "subset_detail")
  }
  if (sl_subset_nobs > 0L) {
    openxlsx::writeData(wb, "subset_detail", sl_out_subset, startRow = 1)
  }

  # Write group_subset_info worksheet (SAS line 555)
  if (!"group_subset_info" %in% names(wb)) {
    openxlsx::addWorksheet(wb, "group_subset_info")
  }
  openxlsx::writeData(wb, "group_subset_info", sl_out_group_subset_info,
                      startRow = 1)

  # Save workbook (SAS line 557: libname xls clear)
  openxlsx::saveWorkbook(wb, gs_file, overwrite = TRUE)
  cli::cli_alert_success("Grouping/subsetting workbook saved to {.file {gs_file}}")

  invisible(NULL)
}


# =========================================================================== #
# group_subset_xml_out() -- Grouping and Subsetting Excel XML Output
# Replaces SAS %group_subset_xml_out macro (SAS lines 565-1132)
# This was SpreadsheetML XML output; in R, uses openxlsx to add a formatted
# "Grouping and Subsetting" worksheet with proper styling and cell merges.
# =========================================================================== #
#' @param wb  An openxlsx Workbook object to add the worksheet to.
#' @param ndabla  NDA/BLA identifier string.
#' @param studyid  Study identifier string.
#' @param sl_group  Original grouping tibble (for reference).
#' @param sl_subset  Original subsetting tibble (for reference).
#' @param sl_group_desc  Grouping description string.
#' @param sl_subset_desc  Subsetting description string.
#' @param sl_subset_operator  Operator description string.
#' @param sl_out_group  Preprocessed group detail tibble.
#' @param sl_out_subset  Preprocessed subset detail tibble.
#' @param sl_group_nobs  Number of observations in grouping data.
#' @param sl_subset_nobs  Number of observations in subsetting data.
#' @return The modified workbook object (invisible).
#' @export
group_subset_xml_out <- function(wb,
                                 ndabla,
                                 studyid,
                                 sl_group,
                                 sl_subset,
                                 sl_group_desc,
                                 sl_subset_desc,
                                 sl_subset_operator,
                                 sl_out_group,
                                 sl_out_subset,
                                 sl_group_nobs,
                                 sl_subset_nobs) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING EXCEL XML OUTPUT")

  # Resolve apply_style from xml_output.R or provide inline fallback
  # (xml_output.R is source()'d by the parent demographics.R script)
  if (!exists("apply_style", mode = "function", inherits = TRUE)) {
    apply_style <- function(wb, sheet, row, col, style) {
      openxlsx::addStyle(wb, sheet, style, rows = row, cols = col)
    }
  }

  # Resolve annotate_data from xml_output.R or provide inline fallback
  if (!exists("annotate_data", mode = "function", inherits = TRUE)) {
    annotate_data <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
      rows <- list()
      for (ri in seq_len(nrow(df))) {
        for (ci in seq_len(ncol(df))) {
          val <- as.character(df[[ci]][ri])
          if (is.na(val)) val <- ""
          rows[[length(rows) + 1L]] <- tibble::tibble(
            Row = ri, Col = ci, varname = names(df)[ci],
            Data = val, Type = "String", bottom = (ri == nrow(df)))
        }
      }
      dplyr::bind_rows(rows)
    }
  }

  # Resolve write_annotated_data from xml_output.R or provide inline fallback
  if (!exists("write_annotated_data", mode = "function", inherits = TRUE)) {
    write_annotated_data <- function(wb, sheet, annotated_data, styles,
                                     start_row) {
      if (is.null(annotated_data) || nrow(annotated_data) == 0) {
        return(invisible(NULL))
      }
      for (i in seq_len(nrow(annotated_data))) {
        row <- annotated_data[i, ]
        r <- start_row + row$Row - 1L
        c_idx <- row$Col
        openxlsx::writeData(wb, sheet, row$Data, startRow = r, startCol = c_idx)
        if ("StyleID" %in% names(row) && !is.na(row$StyleID) &&
            row$StyleID %in% names(styles)) {
          openxlsx::addStyle(wb, sheet, styles[[row$StyleID]],
                             rows = r, cols = c_idx)
        }
      }
      invisible(NULL)
    }
  }

  # Get styles from xml_output.R (internal dependency)
  # Schema references create_styles(); actual function is create_workbook_styles()
  if (exists("create_workbook_styles", mode = "function")) {
    styles <- create_workbook_styles()
  } else if (exists("create_styles", mode = "function")) {
    styles <- create_styles()
  } else {
    # Fallback: create minimal inline styles for standalone operation
    styles <- list(
      Header        = openxlsx::createStyle(fontSize = 12, textDecoration = c("bold", "italic")),
      SubHeader     = openxlsx::createStyle(fontSize = 10, textDecoration = "bold"),
      Default8      = openxlsx::createStyle(fontSize = 8),
      Default10Wrap = openxlsx::createStyle(fontSize = 10, wrapText = TRUE),
      ColumnOutline = openxlsx::createStyle(
        fontSize = 9, fontColour = "#FFFFFF", textDecoration = "bold",
        fgFill = "#333399", border = "TopBottomLeftRight",
        borderColour = "#FFFFFF", halign = "center", wrapText = TRUE),
      GS_BTLRB      = openxlsx::createStyle(
        fontSize = 10, wrapText = TRUE, valign = "top",
        border = "TopBottomLeftRight"),
      GSC_BTLRB     = openxlsx::createStyle(
        fontSize = 10, wrapText = TRUE, halign = "center", valign = "top",
        border = "TopBottomLeftRight"),
      GS_BTLR       = openxlsx::createStyle(
        fontSize = 10, wrapText = TRUE, valign = "top",
        border = "TopLeftRight"),
      GS_BLR        = openxlsx::createStyle(
        fontSize = 10, wrapText = TRUE, valign = "top",
        border = "LeftRight"),
      GS_BLRB       = openxlsx::createStyle(
        fontSize = 10, wrapText = TRUE, valign = "top",
        border = "BottomLeftRight"),
      Table         = openxlsx::createStyle(
        fontSize = 9, halign = "center",
        border = "TopBottomLeftRight")
    )
  }

  ws_name <- "Grouping and Subsetting"
  openxlsx::addWorksheet(wb, ws_name)

  # Column widths (SAS lines 587-594)
  # SAS pixel widths: 162, 48, 162, 162, 50.5, 162, 162
  # Approximate character widths for openxlsx (~7 pixels per char unit)
  openxlsx::setColWidths(wb, ws_name, cols = 1:7,
                         widths = c(23, 7, 23, 23, 7.2, 23, 23))

  # ---- Header section (SAS lines 603-628) ----
  current_row <- 2L  # Row 1 is blank

  # Title: "Grouping and Subsetting Summary" with Header style
  openxlsx::writeData(wb, ws_name, "Grouping and Subsetting Summary",
                      startRow = current_row, startCol = 1)
  apply_style(wb, ws_name, current_row, 1, styles$Header)
  current_row <- current_row + 2L  # Row 3 blank

  # NDA/BLA: (Default8 style)
  openxlsx::writeData(wb, ws_name, paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1)
  apply_style(wb, ws_name, current_row, 1, styles$Default8)
  current_row <- current_row + 1L

  # Study: (Default8 style)
  openxlsx::writeData(wb, ws_name, paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1)
  apply_style(wb, ws_name, current_row, 1, styles$Default8)
  current_row <- current_row + 1L

  # Analysis run date (Default8 style)
  run_date_str <- paste0(
    "Analysis run date: ",
    format(Sys.Date(), "%Y-%m-%d"), " ",
    format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, ws_name, run_date_str,
                      startRow = current_row, startCol = 1)
  apply_style(wb, ws_name, current_row, 1, styles$Default8)
  current_row <- current_row + 2L  # Blank row

  # ===========================================================================
  # GROUPING SECTION (SAS lines 679-815)
  # ===========================================================================
  if (sl_group_nobs > 0L) {
    cli::cli_alert_info("GROUPING")

    # Section heading with SubHeader style
    openxlsx::writeData(wb, ws_name, sl_group_desc,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
    current_row <- current_row + 1L

    # Explanatory paragraph 1 (SAS: merged across 7 cols, height 50)
    para_g1 <- paste0(
      "Below is a summary of the grouping rules that were used in this ",
      "analysis. Grouping takes existing values and creates a new analysis ",
      "variable where original values are placed in a group.")
    openxlsx::writeData(wb, ws_name, para_g1,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$Default10Wrap)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    openxlsx::setRowHeights(wb, ws_name, rows = current_row, heights = 50)
    current_row <- current_row + 1L

    # Explanatory paragraph 2 (SAS: height 12.75)
    para_g2 <- paste0(
      "The test column gives the variable upon which grouping was based.")
    openxlsx::writeData(wb, ws_name, para_g2,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$Default10Wrap)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    openxlsx::setRowHeights(wb, ws_name, rows = current_row, heights = 12.75)
    current_row <- current_row + 2L  # Blank row

    # Column headers (ColumnOutline style)
    # Columns: group_name, domain, partition_desc, var_desc(+placeholder), var_value, dsvg_grp_name
    grp_headers <- c(
      "Grouping Rule Name", "Domain", "For Observations Where...",
      "Variable", "", "Original Variable Value", "Grouped Variable Value")
    for (ci in seq_along(grp_headers)) {
      openxlsx::writeData(wb, ws_name, grp_headers[ci],
                          startRow = current_row, startCol = ci)
      apply_style(wb, ws_name, current_row, ci, styles$ColumnOutline)
    }
    # Merge "Variable" header across columns 4-5 (SAS MergeAcross=1)
    openxlsx::mergeCells(wb, ws_name, cols = 4:5, rows = current_row)
    current_row <- current_row + 1L

    # Write data rows using xml_output.R annotate/write pipeline
    data_start_g <- current_row
    n_grp_rows <- nrow(sl_out_group)

    if (n_grp_rows > 0L) {
      # Use xml_output.R annotation functions (schema: annotate_data,
      # write_annotated_data). annotate_data converts wide tibble to
      # long cell-level format; write_annotated_data writes with styling.
      if (exists("annotate_data", mode = "function") &&
          exists("write_annotated_data", mode = "function")) {
        grp_annotated <- annotate_data(sl_out_group)
        grp_annotated$StyleID <- "GS_BTLRB"
        write_annotated_data(wb, ws_name, grp_annotated, styles, data_start_g)
      } else {
        # Fallback: direct cell-by-cell write when xml_output.R not loaded
        for (ri in seq_len(n_grp_rows)) {
          for (ci in seq_len(ncol(sl_out_group))) {
            val <- as.character(sl_out_group[[ci]][ri])
            if (is.na(val)) val <- ""
            openxlsx::writeData(wb, ws_name, val,
                                startRow = data_start_g + ri - 1L,
                                startCol = ci)
            apply_style(wb, ws_name, data_start_g + ri - 1L, ci,
                        styles$GS_BTLRB)
          }
        }
      }
      current_row <- data_start_g + n_grp_rows

      # Apply vertical cell merges for blanked columns (replaces %gs_rows)
      apply_vertical_merges(wb, ws_name, sl_out_group, data_start_g,
                            merge_cols = c(1L, 2L, 3L, 4L))
    }

    current_row <- current_row + 1L  # Blank separator row

  } else {
    # No grouping section (SAS lines 860-863)
    cli::cli_alert_info("NO GROUPING")
    openxlsx::writeData(wb, ws_name, "No grouping was used.",
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
    current_row <- current_row + 2L
  }

  # ===========================================================================
  # SUBSETTING SECTION (SAS lines 817-1049)
  # ===========================================================================
  if (sl_subset_nobs > 0L) {
    cli::cli_alert_info("SUBSETTING")

    # Section heading with SubHeader style
    openxlsx::writeData(wb, ws_name, sl_subset_desc,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
    current_row <- current_row + 1L

    # Explanatory paragraph 1 (SAS: merged, height 50)
    para_s1 <- paste0(
      "Below is a summary of the subsetting rules that were used in this ",
      "analysis. Subsetting allows the user to restrict the subjects ",
      "included in the analysis based on the values of a given variable.")
    openxlsx::writeData(wb, ws_name, para_s1,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$Default10Wrap)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    openxlsx::setRowHeights(wb, ws_name, rows = current_row, heights = 50)
    current_row <- current_row + 1L

    # Explanatory paragraph 2 (SAS: height 50)
    para_s2 <- paste0(
      "The test column gives the variable upon which subsetting was based.")
    openxlsx::writeData(wb, ws_name, para_s2,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$Default10Wrap)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    openxlsx::setRowHeights(wb, ws_name, rows = current_row, heights = 50)
    current_row <- current_row + 1L

    # Explanatory paragraph 3 (SAS: height 50)
    para_s3 <- paste0(
      "The condition number refers to a particular test condition that ",
      "exists within a particular subset rule. It is possible for a subset ",
      "rule to have multiple conditions (e.g. subjects from the United ",
      "States that are under 65 years old). The 'which conditions apply' ",
      "column describes the relationship between test conditions (AND vs ",
      "OR) within a subset rule.")
    openxlsx::writeData(wb, ws_name, para_s3,
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$Default10Wrap)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    openxlsx::setRowHeights(wb, ws_name, rows = current_row, heights = 50)
    current_row <- current_row + 1L

    # Operator description in SubHeader (SAS lines 846-863)
    if (nzchar(sl_subset_operator) && sl_subset_operator != "N/A") {
      openxlsx::writeData(wb, ws_name, sl_subset_operator,
                          startRow = current_row, startCol = 1)
      apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
      openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
      current_row <- current_row + 1L
    }

    current_row <- current_row + 1L  # Blank row

    # Column headers (ColumnOutline style)
    sub_headers <- c(
      "Subset Rule Name", "Domain", "For Observations Where...",
      "Variable", "Condition", "Variable Value",
      "Which Conditions Apply?\n(AND vs OR)")
    for (ci in seq_along(sub_headers)) {
      openxlsx::writeData(wb, ws_name, sub_headers[ci],
                          startRow = current_row, startCol = ci)
      apply_style(wb, ws_name, current_row, ci, styles$ColumnOutline)
    }
    current_row <- current_row + 1L

    # Write data rows with GS styles
    data_start_s <- current_row
    n_sub_rows <- nrow(sl_out_subset)

    if (n_sub_rows > 0L) {
      for (ri in seq_len(n_sub_rows)) {
        for (ci in seq_len(ncol(sl_out_subset))) {
          val <- as.character(sl_out_subset[[ci]][ri])
          if (is.na(val)) val <- ""
          openxlsx::writeData(wb, ws_name, val,
                              startRow = current_row, startCol = ci)
          # Condition column (5) uses centered style GSC_BTLRB
          if (ci == 5L) {
            apply_style(wb, ws_name, current_row, ci, styles$GSC_BTLRB)
          } else {
            apply_style(wb, ws_name, current_row, ci, styles$GS_BTLRB)
          }
        }
        current_row <- current_row + 1L
      }

      # Apply vertical cell merges for blanked columns (replaces %gs_rows)
      apply_vertical_merges(wb, ws_name, sl_out_subset, data_start_s,
                            merge_cols = c(1L, 2L, 3L, 4L))
    }

    current_row <- current_row + 1L

  } else {
    # No subsetting section (SAS lines 1036-1064)
    cli::cli_alert_info("NO SUBSETTING")
    openxlsx::writeData(wb, ws_name, "No subsetting was used.",
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
    current_row <- current_row + 2L
  }

  # ===========================================================================
  # NEITHER SECTION (SAS lines 1055-1071)
  # ===========================================================================
  if (sl_group_nobs == 0L && sl_subset_nobs == 0L) {
    cli::cli_alert_info("NO GROUPING OR SUBSETTING")
    openxlsx::writeData(wb, ws_name,
                        "Neither grouping nor subsetting were used.",
                        startRow = current_row, startCol = 1)
    apply_style(wb, ws_name, current_row, 1, styles$SubHeader)
    openxlsx::mergeCells(wb, ws_name, cols = 1:7, rows = current_row)
    current_row <- current_row + 1L
  }

  # ===========================================================================
  # Worksheet print options (SAS lines 1094-1112)
  # Landscape orientation, fit-to-page, header/footer, scale
  # ===========================================================================
  openxlsx::pageSetup(wb, ws_name,
                      orientation = "landscape",
                      fitToWidth  = TRUE,
                      fitToHeight = FALSE)

  cli::cli_alert_success("Grouping and Subsetting worksheet added")

  invisible(wb)
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS PCFILES/JET LIBNAME Excel engine fully replaced by openxlsx
####    - SpreadsheetML XML generation fully replaced by openxlsx workbook API
####    - Script Launcher dataset structures (SL_GROUP, SL_SUBSET,
####      SL_DATASETS) are provided as tibbles with matching column names
####    - Variable labels accessed via haven attribute labels where available;
####      gracefully falls back to var_name when domain_data is not provided
####    - SAS sleep(1) delay for PCFILES server lock is not needed in R
####    - SAS %gs_rows inner macro for MergeDown replaced by
####      calc_merge_spans() + openxlsx::mergeCells()
####    - GS_BTLRB style used for all data cells; openxlsx handles merged
####      cell borders as a single visual unit (unlike SAS XML which needed
####      separate GS_BTLR/GS_BLR/GS_BLRB for each row in a merge span)
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - None expected -- this module is purely data formatting and output
#### NO DIRECT R EQUIVALENT:
####    - SAS PCFILES LIBNAME engine -> openxlsx::loadWorkbook + writeData
####      + saveWorkbook
####    - SAS DATA step hash lookups -> dplyr::left_join
####    - SAS DSID/OPEN/CLOSE/VARNUM/VARLABEL functions -> haven label
####      attributes via attr(col, "label")
####    - SAS SpreadsheetML MergeDown XML -> openxlsx::mergeCells()
####    - SAS %gs_rows inner macro for row span calculation -> R
####      calc_merge_spans() helper function
####    - SAS RETAIN for condition numbering -> dplyr cumsum() + lag()
####    - SAS first.var BY-group logic -> dplyr lag() comparison with
####      hierarchical change detection
####    - SAS compbl() -> stringr::str_squish()
####    - SAS tranwrd() -> stringr::str_replace()
####    - SAS index(reverse(x),',') -> stringr::str_locate_all() + last pos
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Excel I/O replacing both PCFILES and SpreadsheetML XML
####    - dplyr/tidyr: Data manipulation replacing DATA steps and PROC SQL
####    - stringr: String manipulation replacing SAS character functions
####    - cli: Logging messages replacing SAS %put
####    - tibble: Enhanced data frames for structured output
#### OPEN QUESTIONS:
####    - Exact column width pixel-to-character-width mapping for openxlsx
####      may not be 1:1 with SAS SpreadsheetML pixel values
####    - Whether merged cell border rendering in openxlsx matches SAS
####      SpreadsheetML MergeDown rendering exactly across all Excel versions
####    - Whether variable labels from haven are consistently populated for
####      all domain datasets used by Script Launcher
####    - SAS %annotate/%markup pipeline is functionally replaced by direct
####      openxlsx::writeData + apply_style calls; annotate_data() and
####      write_annotated_data() from xml_output.R are available for more
####      complex formatting needs if required in future iterations
#### ============================================================
