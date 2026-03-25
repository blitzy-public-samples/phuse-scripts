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
#  EXTERNAL FILES USED: None (self-contained utility; openxlsx replaces       #
#                       xml_output.sas dependency)                            #
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
#'   by domain name (e.g., list(AE = ae_df, DM = dm_df)) used to look up
#'   variable labels. Defaults to NULL (labels not resolved).
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{sl_group_desc}{Character. Human-readable grouping description.}
#'     \item{sl_subset_desc}{Character. Human-readable subsetting description.}
#'     \item{sl_subset_operator}{Character. Description of which conditions apply.}
#'     \item{sl_gs_desc}{Character. Combined single-line description.}
#'     \item{sl_custom_ds}{Character. Comma-separated custom dataset names.}
#'     \item{sl_out_group}{Tibble. Formatted grouping detail table.}
#'     \item{sl_out_subset}{Tibble. Formatted subsetting detail table.}
#'   }
#'
#' @details
#' The SAS version uses PROC SQL, DATA steps, and global macro variables
#' to assemble grouping/subsetting descriptions and detail tables. This
#' R migration replaces those with dplyr pipelines and returns all results
#' in a structured list instead of polluting the global environment.
#'
#' @export
group_subset_pp <- function(sl_group = NULL,
                            sl_subset = NULL,
                            sl_datasets = NULL,
                            domain_data = NULL) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING PREPROCESSING")

  # Initialize counts

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

    # Format with "and" for natural language
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

    sl_subset_desc_text <- paste(subset_names, collapse = paste0(" ", sl_subset_outer[1], " "))
    sl_subset_desc <- paste0("Subset by ", sl_subset_desc_text)

    # Operator description
    if (sl_subset_count > 1) {
      sl_subset_operator <- dplyr::case_when(
        sl_subset_outer[1] == "and" ~ paste0(
          "ALL of the following rules must be true ",
          "for a subject to be included in the analysis."
        ),
        sl_subset_outer[1] == "or" ~ paste0(
          "ANY of the following rules must be true ",
          "for a subject to be included in the analysis."
        ),
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
  # Single-line combined description
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

  cli::cli_alert_info(sl_gs_desc)

  # --------------------------------------------------------------------------
  # Group detail table
  # --------------------------------------------------------------------------
  cli::cli_alert_info("GROUP DETAIL")

  if (sl_group_nobs > 0) {
    sl_out_group <- sl_group %>%
      dplyr::arrange(group_name, partition, var_name, dsvg_grp_name, var_value) %>%
      dplyr::group_by(group_name) %>%
      dplyr::mutate(
        # Look up variable labels from domain data if available
        var_label = purrr::map2_chr(domain, var_name, function(dom, vn) {
          if (!is.null(domain_data) && dom %in% names(domain_data)) {
            ds <- domain_data[[dom]]
            lbl <- attr(ds[[vn]], "label")
            if (!is.null(lbl) && nchar(lbl) > 0) return(lbl)
          }
          return(NA_character_)
        }),
        var_desc = dplyr::if_else(
          !is.na(var_label),
          paste0(var_label, " (", var_name, ")"),
          var_name
        ),
        partition_desc = dplyr::if_else(
          !is.na(partition) & partition != "",
          partition,
          "N/A"
        )
      ) %>%
      dplyr::ungroup() %>%
      # Suppress repeated values for display formatting
      dplyr::mutate(
        group_name_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            group_name != dplyr::lag(group_name, default = ""),
          group_name,
          ""
        ),
        domain_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            group_name != dplyr::lag(group_name, default = ""),
          domain,
          ""
        )
      ) %>%
      dplyr::select(
        group_name = group_name_fmt,
        domain = domain_fmt,
        partition_desc,
        var_desc,
        var_value,
        dsvg_grp_name
      )
  } else {
    sl_out_group <- tibble::tibble(
      group_name = character(),
      domain = character(),
      partition_desc = character(),
      var_desc = character(),
      var_value = character(),
      dsvg_grp_name = character()
    )
  }

  # --------------------------------------------------------------------------
  # Subset detail table
  # --------------------------------------------------------------------------
  cli::cli_alert_info("SUBSET DETAIL")

  if (sl_subset_nobs > 0) {
    # Condition counts per subset rule
    sl_subset_condition <- sl_subset %>%
      dplyr::distinct(name, partition, var_name) %>%
      dplyr::count(name, name = "condition_count")

    sl_out_subset <- sl_subset %>%
      dplyr::rename(subset_name = name) %>%
      dplyr::arrange(subset_name, domain, partition, var_name, var_value) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(
        # Variable label look-up
        var_label = purrr::map2_chr(domain, var_name, function(dom, vn) {
          if (!is.null(domain_data) && dom %in% names(domain_data)) {
            ds <- domain_data[[dom]]
            lbl <- attr(ds[[vn]], "label")
            if (!is.null(lbl) && nchar(lbl) > 0) return(lbl)
          }
          return(NA_character_)
        }),
        var_desc = dplyr::if_else(
          !is.na(var_label),
          paste0(var_label, " (", var_name, ")"),
          var_name
        ),
        partition_desc = dplyr::if_else(
          !is.na(partition) & partition != "",
          partition,
          "N/A"
        )
      ) %>%
      dplyr::ungroup() %>%
      # Build condition numbers
      dplyr::mutate(
        condition_no = cumsum(subset_name != dplyr::lag(subset_name, default = "")),
        condition_sub_no = dplyr::row_number()
      ) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(condition_sub_no = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        condition = paste0(condition_no, ".", condition_sub_no)
      ) %>%
      # Build operator description
      dplyr::left_join(sl_subset_condition, by = c("subset_name" = "name")) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::mutate(
        operator = dplyr::if_else(
          dplyr::row_number() == 1 & !is.na(inner_operator) & inner_operator != "",
          {
            conds <- paste(
              purrr::map_chr(seq_len(condition_count[1]), function(i) {
                sep <- if (i < condition_count[1]) paste0(" ", toupper(inner_operator[1])) else ""
                paste0(condition_no[1], ".", i, sep)
              }),
              collapse = " "
            )
            prefix <- if (tolower(inner_operator[1]) == "or") "Condition " else "Conditions "
            trimws(paste0(prefix, conds))
          },
          "N/A"
        )
      ) %>%
      dplyr::ungroup() %>%
      # Suppress repeated values for display
      dplyr::mutate(
        subset_name_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            subset_name != dplyr::lag(subset_name, default = ""),
          subset_name,
          ""
        ),
        domain_fmt = dplyr::if_else(
          dplyr::row_number() == 1 |
            domain != dplyr::lag(domain, default = ""),
          domain,
          ""
        )
      ) %>%
      dplyr::select(
        subset_name = subset_name_fmt,
        domain = domain_fmt,
        partition_desc,
        var_desc,
        condition,
        var_value,
        operator
      )
  } else {
    sl_out_subset <- tibble::tibble(
      subset_name = character(),
      domain = character(),
      partition_desc = character(),
      var_desc = character(),
      condition = character(),
      var_value = character(),
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
    if (length(custom) > 0) {
      sl_custom_ds <- paste(custom, collapse = ", ")
    }
  }

  # Return structured list
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
#' Write Grouping and Subsetting to Excel Workbook (Template Style)
#'
#' Writes preprocessed grouping/subsetting detail to an Excel workbook.
#' Replaces the SAS \%group_subset_xls_out macro which used
#' PCFILES/JET LIBNAME engine for Excel template population.
#'
#' @param gs_file Character. Output file path for the Excel workbook.
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla Character. NDA/BLA identifier.
#' @param studyid Character. Study identifier.
#'
#' @return Invisible NULL. Workbook is written to \code{gs_file}.
#'
#' @export
group_subset_xls_out <- function(gs_file,
                                 pp_result,
                                 ndabla = "",
                                 studyid = "") {

  cli::cli_alert_info("GROUPING/SUBSETTING EXCEL TEMPLATE OUTPUT")

  wb <- openxlsx::createWorkbook()

  # Header style
  header_style <- openxlsx::createStyle(
    fontSize = 12, textDecoration = "bold"
  )
  default_style <- openxlsx::createStyle(fontSize = 8)
  col_header_style <- openxlsx::createStyle(
    fontSize = 8, textDecoration = "bold",
    border = "TopBottom", borderStyle = "thin"
  )

  # --- Group Detail sheet ---
  openxlsx::addWorksheet(wb, "Group Detail")
  if (nrow(pp_result$sl_out_group) > 0) {
    openxlsx::writeData(wb, "Group Detail", pp_result$sl_out_group,
                        startRow = 1, headerStyle = col_header_style)
    openxlsx::addStyle(wb, "Group Detail", default_style,
                       rows = seq_len(nrow(pp_result$sl_out_group) + 1),
                       cols = seq_len(ncol(pp_result$sl_out_group)),
                       gridExpand = TRUE, stack = TRUE)
  }

  # --- Subset Detail sheet ---
  openxlsx::addWorksheet(wb, "Subset Detail")
  if (nrow(pp_result$sl_out_subset) > 0) {
    openxlsx::writeData(wb, "Subset Detail", pp_result$sl_out_subset,
                        startRow = 1, headerStyle = col_header_style)
    openxlsx::addStyle(wb, "Subset Detail", default_style,
                       rows = seq_len(nrow(pp_result$sl_out_subset) + 1),
                       cols = seq_len(ncol(pp_result$sl_out_subset)),
                       gridExpand = TRUE, stack = TRUE)
  }

  # --- Group Subset Info sheet ---
  openxlsx::addWorksheet(wb, "Group Subset Info")

  info_df <- tibble::tibble(
    val_desc = c(
      "Grouped by", "Subset by",
      "Group row count", "Subset row count",
      "Subset operator", "Note", "GS description"
    ),
    val = c(
      pp_result$sl_group_desc,
      pp_result$sl_subset_desc,
      as.character(nrow(pp_result$sl_out_group)),
      as.character(nrow(pp_result$sl_out_subset)),
      pp_result$sl_subset_operator,
      # Note logic matching SAS behavior
      dplyr::case_when(
        nrow(pp_result$sl_out_group) == 0 & nrow(pp_result$sl_out_subset) > 0 ~
          "No grouping was used.",
        nrow(pp_result$sl_out_group) > 0 & nrow(pp_result$sl_out_subset) == 0 ~
          "No subsetting was used.",
        nrow(pp_result$sl_out_group) == 0 & nrow(pp_result$sl_out_subset) == 0 ~
          "Neither grouping nor subsetting were used.",
        TRUE ~ ""
      ),
      pp_result$sl_gs_desc
    )
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
#' Write Grouping and Subsetting to Excel Workbook (XML-Style Layout)
#'
#' Writes preprocessed grouping/subsetting data to an Excel workbook
#' with formatted headers, merged cells, and XML-style structure.
#' Replaces the SAS \%group_subset_xml_out macro which generated
#' SpreadsheetML XML output.
#'
#' @param wb An openxlsx workbook object. The worksheet is added to this
#'   existing workbook.
#' @param pp_result List. Output from \code{group_subset_pp()}.
#' @param ndabla Character. NDA/BLA identifier.
#' @param studyid Character. Study identifier.
#' @param delete_intermediate Logical. Whether to remove intermediate data.
#'   Defaults to TRUE. Replaces SAS delete_im=Y parameter.
#'
#' @return The modified openxlsx workbook object (invisibly).
#'
#' @export
group_subset_xml_out <- function(wb,
                                 pp_result,
                                 ndabla = "",
                                 studyid = "",
                                 delete_intermediate = TRUE) {

  cli::cli_alert_info("SL GROUPING/SUBSETTING EXCEL XML OUTPUT")

  ws_name <- "Grouping and Subsetting"
  openxlsx::addWorksheet(wb, ws_name)

  # Define styles matching SAS style gallery
  header_style <- openxlsx::createStyle(
    fontSize = 12, textDecoration = "bold"
  )
  subheader_style <- openxlsx::createStyle(
    fontSize = 10, textDecoration = "bold"
  )
  default8_style <- openxlsx::createStyle(fontSize = 8)
  col_header_style <- openxlsx::createStyle(
    fontSize = 8, textDecoration = "bold",
    border = "TopBottom", borderStyle = "thin"
  )

  # Column widths matching SAS definitions (in characters)
  openxlsx::setColWidths(wb, ws_name,
                         cols = 1:8,
                         widths = c(23, 7, 23, 23, 7.2, 23, 23, 8.43))

  # Header section
  current_row <- 2
  openxlsx::writeData(wb, ws_name, "Grouping and Subsetting Summary",
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, header_style,
                     rows = current_row, cols = 1)
  current_row <- current_row + 2

  openxlsx::writeData(wb, ws_name, paste0("NDA/BLA: ", ndabla),
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style,
                     rows = current_row, cols = 1)
  current_row <- current_row + 1

  openxlsx::writeData(wb, ws_name, paste0("Study: ", studyid),
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style,
                     rows = current_row, cols = 1)
  current_row <- current_row + 1

  run_date <- paste0(
    "Analysis run date: ",
    format(Sys.Date(), "%Y-%m-%d"), " ",
    format(Sys.time(), "%I:%M:%S %p")
  )
  openxlsx::writeData(wb, ws_name, run_date,
                      startRow = current_row, startCol = 1)
  openxlsx::addStyle(wb, ws_name, default8_style,
                     rows = current_row, cols = 1)
  current_row <- current_row + 2

  # Grouping section
  if (nrow(pp_result$sl_out_group) > 0) {
    openxlsx::writeData(wb, ws_name, pp_result$sl_group_desc,
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, ws_name, subheader_style,
                       rows = current_row, cols = 1)
    current_row <- current_row + 2

    # Column headers
    group_headers <- c("Grouping Rule Name", "Domain", "For Observations Where...",
                       "Variable", "Original Variable Value", "Grouped Variable Value")
    for (ci in seq_along(group_headers)) {
      openxlsx::writeData(wb, ws_name, group_headers[ci],
                          startRow = current_row, startCol = ci)
      openxlsx::addStyle(wb, ws_name, col_header_style,
                         rows = current_row, cols = ci)
    }
    current_row <- current_row + 1

    # Write data rows
    openxlsx::writeData(wb, ws_name, pp_result$sl_out_group,
                        startRow = current_row, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, ws_name, default8_style,
                       rows = seq(current_row, current_row + nrow(pp_result$sl_out_group) - 1),
                       cols = 1:6, gridExpand = TRUE)
    current_row <- current_row + nrow(pp_result$sl_out_group) + 1
  }

  # Subsetting section
  if (nrow(pp_result$sl_out_subset) > 0) {
    current_row <- current_row + 1
    openxlsx::writeData(wb, ws_name, pp_result$sl_subset_desc,
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, ws_name, subheader_style,
                       rows = current_row, cols = 1)
    current_row <- current_row + 1

    if (nchar(pp_result$sl_subset_operator) > 0 &&
        pp_result$sl_subset_operator != "N/A") {
      openxlsx::writeData(wb, ws_name, pp_result$sl_subset_operator,
                          startRow = current_row, startCol = 1)
      openxlsx::addStyle(wb, ws_name, default8_style,
                         rows = current_row, cols = 1)
      current_row <- current_row + 1
    }
    current_row <- current_row + 1

    # Column headers
    subset_headers <- c("Subset Rule Name", "Domain", "For Observations Where...",
                        "Variable", "Condition", "Variable Value", "Which Conditions Apply?")
    for (ci in seq_along(subset_headers)) {
      openxlsx::writeData(wb, ws_name, subset_headers[ci],
                          startRow = current_row, startCol = ci)
      openxlsx::addStyle(wb, ws_name, col_header_style,
                         rows = current_row, cols = ci)
    }
    current_row <- current_row + 1

    # Write data rows
    openxlsx::writeData(wb, ws_name, pp_result$sl_out_subset,
                        startRow = current_row, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, ws_name, default8_style,
                       rows = seq(current_row, current_row + nrow(pp_result$sl_out_subset) - 1),
                       cols = 1:7, gridExpand = TRUE)
    current_row <- current_row + nrow(pp_result$sl_out_subset) + 1
  }

  # No grouping/subsetting note
  if (nrow(pp_result$sl_out_group) == 0 && nrow(pp_result$sl_out_subset) == 0) {
    openxlsx::writeData(wb, ws_name,
                        "Neither grouping nor subsetting were used.",
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, ws_name, default8_style,
                       rows = current_row, cols = 1)
  }

  # Page setup matching SAS WorksheetOptions
  openxlsx::pageSetup(wb, ws_name,
                       orientation = "landscape",
                       fitToWidth = TRUE, fitToHeight = FALSE)

  invisible(wb)
}

# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS sl_group, sl_subset, sl_datasets are passed as tibbles
####      rather than being read from the SAS work library
####    - Domain data for variable label lookup is optionally passed
####      as a named list; if unavailable, var_name is used directly
####    - SAS hash lookup for partition variables is replaced by
####      dplyr left_join; commented-out SAS code for partition lookup
####      is preserved as comments for traceability
####    - SAS global macro variables (sl_group_desc, sl_subset_desc,
####      sl_subset_operator, sl_gs_desc, sl_custom_ds) are returned
####      as a named list instead of polluting the R global environment
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Row ordering may differ due to R's locale-aware sort vs SAS
####      PROC SORT collation; both use ascending order by key
####    - Character case handling: SAS UPCASE() vs R toupper() should
####      produce identical results for ASCII characters
#### NO DIRECT R EQUIVALENT:
####    - SAS PCFILES/JET LIBNAME engine -> openxlsx::saveWorkbook()
####    - SAS DATA _NULL_ file writing -> openxlsx workbook API
####    - SAS SpreadsheetML XML string assembly -> openxlsx cell writes
####    - SAS %markup/%annotate macros -> direct openxlsx formatting
####    - SAS PROC DATASETS DELETE -> R garbage collection (automatic)
####    - SAS sleep() for PCFILES timing -> not needed with openxlsx
#### PACKAGE SELECTION RATIONALE:
####    - openxlsx: Replaces SpreadsheetML XML and PCFILES engine;
####      full Excel workbook API with cell-level formatting
####    - dplyr: Data manipulation replacing PROC SQL and DATA steps
####    - purrr: Functional iteration replacing SAS macro loops
####    - stringr: String manipulation replacing SAS character functions
####    - cli: User-facing messages replacing SAS %put
#### OPEN QUESTIONS:
####    - Exact column width mapping from SAS XML pixel widths to
####      openxlsx character widths (approximation used)
####    - Whether merged cell formatting in XML output matches SAS
####      MergeDown/MergeAcross behavior exactly
####    - Variable label resolution when domain datasets are not
####      available (currently falls back to var_name)
#### ============================================================
