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
#     - Migrated from: contributed/MedDRA/ZZ_Utilities/sl_gs_output.sas       #
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
#  EXTERNAL FILES USED: None (self-contained utility; openxlsx replaces       #
#                       xml_output.sas dependency)                            #
#                                                                             #
#  PARAMETERS REQUIRED: sl_group -- grouping tibble                           #
#                       sl_subset -- subsetting tibble                        #
#                       sl_datasets -- datasets tibble                        #
#                                                                             #
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5                         #
#                                                                             #
#                NOTES: This file is source()'d by MedDRA analysis scripts    #
#                       In SAS, it was included via:                           #
#                       %include "&utilpath.\sl_gs_output.sas";               #
#                                                                             #
#            REVISIONS:                                                        #
#              2026-xx-xx  Blitzy  Migrated from SAS to R                     #
#                                                                             #
###############################################################################

# --------------------------------------------------------------------------- #
# Library Loading
# --------------------------------------------------------------------------- #
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(cli)

# --------------------------------------------------------------------------- #
# group_subset_pp() — Grouping and Subsetting Preprocessing
# --------------------------------------------------------------------------- #
#' Grouping and Subsetting Preprocessing
#'
#' Preprocess Script Launcher grouping and subsetting metadata into
#' formatted tibbles suitable for output. Replaces the SAS
#' \%group_subset_pp macro.
#'
#' @param sl_group Tibble. Script Launcher grouping metadata with columns:
#'   group_name, domain, partition, var_name, var_value, dsvg_grp_name.
#'   May be NULL or zero-row if no grouping is used.
#' @param sl_subset Tibble. Script Launcher subsetting metadata with columns:
#'   name, domain, partition, var_name, var_value, outer_operator, inner_operator.
#'   May be NULL or zero-row if no subsetting is used.
#' @param sl_datasets Tibble. Script Launcher datasets metadata with columns:
#'   datatype, default, partition_variable.
#' @param domain_data Named list. Optional named list of data frames keyed
#'   by domain name for variable label lookup. Defaults to NULL.
#'
#' @return A named list with elements: sl_group_desc, sl_subset_desc,
#'   sl_subset_operator, sl_gs_desc, sl_custom_ds, sl_out_group, sl_out_subset.
#'
#' @export
group_subset_pp <- function(sl_group = NULL,
                            sl_subset = NULL,
                            sl_datasets = NULL,
                            domain_data = NULL) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING PREPROCESSING")

  sl_group_nobs <- if (!is.null(sl_group)) nrow(sl_group) else 0L
  sl_subset_nobs <- if (!is.null(sl_subset)) nrow(sl_subset) else 0L

  # --------------------------------------------------------------------------
  # Grouping description
  # --------------------------------------------------------------------------
  if (sl_group_nobs > 0) {
    group_names <- sl_group %>%
      dplyr::distinct(group_name) %>%
      dplyr::pull(group_name)

    group_count <- length(group_names)
    sl_group_desc <- paste(group_names, collapse = ", ")

    if (group_count == 2) {
      sl_group_desc <- gsub(", ", " and ", sl_group_desc, fixed = TRUE)
    } else if (group_count > 2) {
      last_comma <- regexpr(",([^,]*)$", sl_group_desc)
      if (last_comma > 0) {
        sl_group_desc <- paste0(
          substr(sl_group_desc, 1, last_comma),
          " and",
          substr(sl_group_desc, last_comma + 1, nchar(sl_group_desc))
        )
      }
    }
    sl_group_desc <- paste0("Grouped by ", sl_group_desc)
  } else {
    sl_group_desc <- "No grouping"
  }

  # --------------------------------------------------------------------------
  # Subsetting description
  # --------------------------------------------------------------------------
  if (sl_subset_nobs > 0) {
    sl_subset_outer <- sl_subset %>%
      dplyr::distinct(outer_operator) %>%
      dplyr::pull(outer_operator) %>%
      tolower() %>%
      unique()

    subset_names <- sl_subset %>%
      dplyr::distinct(name) %>%
      dplyr::pull(name)

    sl_subset_count <- length(subset_names)
    sl_subset_desc_text <- paste(subset_names,
                                 collapse = paste0(" ", sl_subset_outer[1], " "))
    sl_subset_desc <- paste0("Subset by ", sl_subset_desc_text)

    if (sl_subset_count > 1) {
      sl_subset_operator <- dplyr::case_when(
        sl_subset_outer[1] == "and" ~ paste0(
          "ALL of the following rules must be true ",
          "for a subject to be included in the analysis."),
        sl_subset_outer[1] == "or" ~ paste0(
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

  # --------------------------------------------------------------------------
  # Combined single-line description
  # --------------------------------------------------------------------------
  if (sl_group_nobs > 0 && sl_subset_nobs > 0) {
    sl_gs_desc <- paste0(sl_group_desc, "; ", sl_subset_desc)
  } else if (sl_group_nobs > 0) {
    sl_gs_desc <- sl_group_desc
  } else if (sl_subset_nobs > 0) {
    sl_gs_desc <- sl_subset_desc
  } else {
    sl_gs_desc <- ""
  }

  # --------------------------------------------------------------------------
  # Group detail table
  # --------------------------------------------------------------------------
  cli::cli_alert_info("GROUP DETAIL")

  if (sl_group_nobs > 0) {
    sl_out_group <- sl_group %>%
      dplyr::arrange(group_name, partition, var_name, dsvg_grp_name, var_value) %>%
      dplyr::group_by(group_name) %>%
      dplyr::mutate(
        var_label = purrr::map2_chr(domain, var_name, function(dom, vn) {
          if (!is.null(domain_data) && dom %in% names(domain_data)) {
            lbl <- attr(domain_data[[dom]][[vn]], "label")
            if (!is.null(lbl) && nchar(lbl) > 0) return(lbl)
          }
          return(NA_character_)
        }),
        var_desc = dplyr::if_else(!is.na(var_label),
                                  paste0(var_label, " (", var_name, ")"),
                                  var_name),
        partition_desc = dplyr::if_else(!is.na(partition) & partition != "",
                                        partition, "N/A")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        group_name_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            group_name != dplyr::lag(group_name, default = ""),
          group_name, ""),
        domain_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            group_name != dplyr::lag(group_name, default = ""),
          domain, "")
      ) %>%
      dplyr::select(group_name = group_name_fmt, domain = domain_fmt,
                     partition_desc, var_desc, var_value, dsvg_grp_name)
  } else {
    sl_out_group <- tibble::tibble(
      group_name = character(), domain = character(),
      partition_desc = character(), var_desc = character(),
      var_value = character(), dsvg_grp_name = character()
    )
  }

  # --------------------------------------------------------------------------
  # Subset detail table
  # --------------------------------------------------------------------------
  cli::cli_alert_info("SUBSET DETAIL")

  if (sl_subset_nobs > 0) {
    sl_subset_condition <- sl_subset %>%
      dplyr::distinct(name, partition, var_name) %>%
      dplyr::count(name, name = "condition_count")

    sl_out_subset <- sl_subset %>%
      dplyr::rename(subset_name = name) %>%
      dplyr::arrange(subset_name, domain, partition, var_name, var_value) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(
        var_label = purrr::map2_chr(domain, var_name, function(dom, vn) {
          if (!is.null(domain_data) && dom %in% names(domain_data)) {
            lbl <- attr(domain_data[[dom]][[vn]], "label")
            if (!is.null(lbl) && nchar(lbl) > 0) return(lbl)
          }
          return(NA_character_)
        }),
        var_desc = dplyr::if_else(!is.na(var_label),
                                  paste0(var_label, " (", var_name, ")"),
                                  var_name),
        partition_desc = dplyr::if_else(!is.na(partition) & partition != "",
                                        partition, "N/A")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        condition_no = cumsum(subset_name != dplyr::lag(subset_name, default = "")),
        condition_sub_no = dplyr::row_number()
      ) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(condition_sub_no = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(condition = paste0(condition_no, ".", condition_sub_no)) %>%
      dplyr::left_join(sl_subset_condition, by = c("subset_name" = "name")) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(
        operator = dplyr::if_else(
          dplyr::row_number() == 1 & !is.na(inner_operator) & inner_operator != "",
          {
            conds <- paste(purrr::map_chr(seq_len(condition_count[1]), function(i) {
              sep <- if (i < condition_count[1]) paste0(" ", toupper(inner_operator[1])) else ""
              paste0(condition_no[1], ".", i, sep)
            }), collapse = " ")
            prefix <- if (tolower(inner_operator[1]) == "or") "Condition " else "Conditions "
            trimws(paste0(prefix, conds))
          },
          "N/A"
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        subset_name_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            subset_name != dplyr::lag(subset_name, default = ""),
          subset_name, ""),
        domain_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            domain != dplyr::lag(domain, default = ""),
          domain, "")
      ) %>%
      dplyr::select(subset_name = subset_name_fmt, domain = domain_fmt,
                     partition_desc, var_desc, condition, var_value, operator)
  } else {
    sl_out_subset <- tibble::tibble(
      subset_name = character(), domain = character(),
      partition_desc = character(), var_desc = character(),
      condition = character(), var_value = character(),
      operator = character()
    )
  }

  # --------------------------------------------------------------------------
  # Custom datasets
  # --------------------------------------------------------------------------
  cli::cli_alert_info("CUSTOM DATASETS")
  sl_custom_ds <- ""
  if (!is.null(sl_datasets) && nrow(sl_datasets) > 0) {
    custom <- sl_datasets %>%
      dplyr::filter(default == "N") %>%
      dplyr::pull(datatype)
    if (length(custom) > 0) sl_custom_ds <- paste(custom, collapse = ", ")
  }

  list(
    sl_group_desc = sl_group_desc,
    sl_subset_desc = sl_subset_desc,
    sl_subset_operator = sl_subset_operator,
    sl_gs_desc = sl_gs_desc,
    sl_custom_ds = sl_custom_ds,
    sl_out_group = sl_out_group,
    sl_out_subset = sl_out_subset
  )
}


# --------------------------------------------------------------------------- #
# group_subset_xls_out() — Excel Workbook Output (Template Style)
# --------------------------------------------------------------------------- #
#' @inheritParams group_subset_pp
#' @param gs_file Character. Output file path for the Excel workbook.
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla Character. NDA/BLA identifier.
#' @param studyid Character. Study identifier.
#' @return Invisible NULL. Workbook written to gs_file.
#' @export
group_subset_xls_out <- function(gs_file, pp_result,
                                 ndabla = "", studyid = "") {

  cli::cli_alert_info("GROUPING/SUBSETTING EXCEL TEMPLATE OUTPUT")

  wb <- openxlsx::createWorkbook()
  col_header_style <- openxlsx::createStyle(
    fontSize = 8, textDecoration = "bold",
    border = "TopBottom", borderStyle = "thin"
  )
  default_style <- openxlsx::createStyle(fontSize = 8)

  openxlsx::addWorksheet(wb, "Group Detail")
  if (nrow(pp_result$sl_out_group) > 0) {
    openxlsx::writeData(wb, "Group Detail", pp_result$sl_out_group,
                        startRow = 1, headerStyle = col_header_style)
  }

  openxlsx::addWorksheet(wb, "Subset Detail")
  if (nrow(pp_result$sl_out_subset) > 0) {
    openxlsx::writeData(wb, "Subset Detail", pp_result$sl_out_subset,
                        startRow = 1, headerStyle = col_header_style)
  }

  openxlsx::addWorksheet(wb, "Group Subset Info")
  info_df <- tibble::tibble(
    val_desc = c("Grouped by", "Subset by", "Group row count",
                 "Subset row count", "Subset operator", "Note",
                 "GS description"),
    val = c(pp_result$sl_group_desc, pp_result$sl_subset_desc,
            as.character(nrow(pp_result$sl_out_group)),
            as.character(nrow(pp_result$sl_out_subset)),
            pp_result$sl_subset_operator,
            dplyr::case_when(
              nrow(pp_result$sl_out_group) == 0 & nrow(pp_result$sl_out_subset) > 0 ~
                "No grouping was used.",
              nrow(pp_result$sl_out_group) > 0 & nrow(pp_result$sl_out_subset) == 0 ~
                "No subsetting was used.",
              nrow(pp_result$sl_out_group) == 0 & nrow(pp_result$sl_out_subset) == 0 ~
                "Neither grouping nor subsetting were used.",
              TRUE ~ ""),
            pp_result$sl_gs_desc)
  )
  openxlsx::writeData(wb, "Group Subset Info", info_df,
                      startRow = 1, headerStyle = col_header_style)

  openxlsx::saveWorkbook(wb, gs_file, overwrite = TRUE)
  cli::cli_alert_success("Grouping/subsetting workbook saved to {.file {gs_file}}")
  invisible(NULL)
}


# --------------------------------------------------------------------------- #
# group_subset_xml_out() — Excel Workbook Output (XML-Style Layout)
# --------------------------------------------------------------------------- #
#' @param wb An openxlsx workbook object.
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla Character. NDA/BLA identifier.
#' @param studyid Character. Study identifier.
#' @param delete_intermediate Logical. Whether to remove intermediate data.
#' @return The modified workbook object (invisibly).
#' @export
group_subset_xml_out <- function(wb, pp_result,
                                 ndabla = "", studyid = "",
                                 delete_intermediate = TRUE) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING EXCEL XML OUTPUT")

  ws_name <- "Grouping and Subsetting"
  openxlsx::addWorksheet(wb, ws_name)

  header_style <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold")
  subheader_style <- openxlsx::createStyle(fontSize = 10, textDecoration = "bold")
  default8_style <- openxlsx::createStyle(fontSize = 8)
  col_header_style <- openxlsx::createStyle(
    fontSize = 8, textDecoration = "bold",
    border = "TopBottom", borderStyle = "thin"
  )

  openxlsx::setColWidths(wb, ws_name, cols = 1:8,
                         widths = c(23, 7, 23, 23, 7.2, 23, 23, 8.43))

  current_row <- 2
  openxlsx::writeData(wb, ws_name, "Grouping and Subsetting Summary",
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, header_style, rows = current_row, cols = 1)
  current_row <- current_row + 2

  openxlsx::writeData(wb, ws_name, paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style, rows = current_row, cols = 1)
  current_row <- current_row + 1

  openxlsx::writeData(wb, ws_name, paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style, rows = current_row, cols = 1)
  current_row <- current_row + 1

  run_date <- paste0("Analysis run date: ", format(Sys.Date(), "%Y-%m-%d"),
                     " ", format(Sys.time(), "%I:%M:%S %p"))
  openxlsx::writeData(wb, ws_name, run_date,
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style, rows = current_row, cols = 1)
  current_row <- current_row + 2

  if (nrow(pp_result$sl_out_group) > 0) {
    openxlsx::writeData(wb, ws_name, pp_result$sl_group_desc,
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, ws_name, subheader_style, rows = current_row, cols = 1)
    current_row <- current_row + 2

    group_headers <- c("Grouping Rule Name", "Domain", "For Observations Where...",
                       "Variable", "Original Variable Value", "Grouped Variable Value")
    for (ci in seq_along(group_headers)) {
      openxlsx::writeData(wb, ws_name, group_headers[ci],
                          startRow = current_row, startCol = ci)
      openxlsx::addStyle(wb, ws_name, col_header_style,
                         rows = current_row, cols = ci)
    }
    current_row <- current_row + 1
    openxlsx::writeData(wb, ws_name, pp_result$sl_out_group,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    current_row <- current_row + nrow(pp_result$sl_out_group) + 1
  }

  if (nrow(pp_result$sl_out_subset) > 0) {
    current_row <- current_row + 1
    openxlsx::writeData(wb, ws_name, pp_result$sl_subset_desc,
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, ws_name, subheader_style, rows = current_row, cols = 1)
    current_row <- current_row + 1

    if (nchar(pp_result$sl_subset_operator) > 0 &&
        pp_result$sl_subset_operator != "N/A") {
      openxlsx::writeData(wb, ws_name, pp_result$sl_subset_operator,
                          startRow = current_row, startCol = 1)
      current_row <- current_row + 1
    }
    current_row <- current_row + 1

    subset_headers <- c("Subset Rule Name", "Domain", "For Observations Where...",
                        "Variable", "Condition", "Variable Value",
                        "Which Conditions Apply?")
    for (ci in seq_along(subset_headers)) {
      openxlsx::writeData(wb, ws_name, subset_headers[ci],
                          startRow = current_row, startCol = ci)
      openxlsx::addStyle(wb, ws_name, col_header_style,
                         rows = current_row, cols = ci)
    }
    current_row <- current_row + 1
    openxlsx::writeData(wb, ws_name, pp_result$sl_out_subset,
                        startRow = current_row, startCol = 1, colNames = FALSE)
    current_row <- current_row + nrow(pp_result$sl_out_subset) + 1
  }

  if (nrow(pp_result$sl_out_group) == 0 && nrow(pp_result$sl_out_subset) == 0) {
    openxlsx::writeData(wb, ws_name,
                        "Neither grouping nor subsetting were used.",
                        startRow = current_row, startCol = 1)
  }

  openxlsx::pageSetup(wb, ws_name, orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)
  invisible(wb)
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS sl_group, sl_subset, sl_datasets are passed as tibbles
####    - Domain data for variable label lookup is optional
####    - SAS global macro variables are returned as a named list
####    - SAS hash lookup for partition variables replaced by dplyr
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Row ordering may differ due to locale-aware sort
#### NO DIRECT R EQUIVALENT:
####    - SAS PCFILES/JET engine -> openxlsx::saveWorkbook()
####    - SAS SpreadsheetML XML -> openxlsx cell writes
####    - SAS %markup/%annotate macros -> direct openxlsx formatting
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Replaces SpreadsheetML XML and PCFILES engine
####    - dplyr/purrr/stringr: Tidyverse data manipulation
####    - cli: User-facing messages replacing SAS %put
#### OPEN QUESTIONS:
####    - Exact column width mapping from SAS pixel widths
####    - Merged cell formatting parity with SAS
#### ============================================================
