# ==============================================================================
#         PROGRAM NAME: Grouping and Subsetting Output (R Migration)
#
#          DESCRIPTION: Create output for grouping and subsetting metadata.
#                       Contains four functions:
#                         group_subset_pp     -- Preprocessing to create
#                                                output tibbles
#                         group_subset_xls_out -- XLS template output
#                         group_subset_xml_out -- Excel XML output
#                         gs_rows              -- Helper for merge span calc
#
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#        ORIGINAL DATE: March 4, 2011
#
#   MIGRATION DETAILS:
#     - Source: contributed/AE/ZZ_Utilities/sl_gs_output.sas (1132 lines)
#     - SAS %group_subset_pp   -> R group_subset_pp()
#     - SAS %group_subset_xls_out -> R group_subset_xls_out()
#     - SAS %group_subset_xml_out -> R group_subset_xml_out()
#     - SAS %gs_rows (inner macro) -> R gs_rows()
#     - SpreadsheetML XML -> openxlsx workbook API
#     - SAS PCFILES/JET -> openxlsx::saveWorkbook()
#     - SAS global macro variables -> R return list
#     - SAS PROC SQL / DATA step -> dplyr pipelines
#     - SAS %put -> cli messages
#
#            MADE WITH: R >= 4.3.0, openxlsx >= 4.2.5
#            REVISIONS: 2026 — Blitzy — Migrated from SAS to R
# ==============================================================================

# --- Required Libraries -------------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)
library(haven)
library(cli)

# --- Source Dependency ---------------------------------------------------------
# xml_output.R provides create_styles() and write_annotated_data()
# The caller must source xml_output.R before sourcing this file, e.g.:
#   source(file.path(util_path, "xml_output.R"))
#   source(file.path(util_path, "sl_gs_output.R"))


# ==============================================================================
# gs_rows — Calculate merge spans for openxlsx mergeCells
# ------------------------------------------------------------------------------
# Replaces the SAS inner %gs_rows macro (SAS lines 635-674).
# For a sorted data frame and a list of BY-group variables, computes the
# starting row and span count for each contiguous group of each variable.
# Uses SAS BY-group semantics: first.var triggers when var changes OR any
# higher-level variable changes.
#
# @param data      A data frame sorted by the variables in varlist.
# @param varlist   Character vector of column names defining the BY hierarchy.
# @return A tibble with columns: row, and <var>_n for each variable in varlist.
#         <var>_n is NA for rows inside a merge group, and the merge-down count
#         (0 = no merge, N = merge N additional rows) for the first row of
#         each group.
# ==============================================================================
gs_rows <- function(data, varlist) {
  cli::cli_inform("Computing merge spans for variables: {paste(varlist, collapse = ', ')}")

  n_rows <- nrow(data)


  # Edge case: empty data
  if (n_rows == 0L) {
    result <- dplyr::tibble(row = integer())
    for (var in varlist) {
      result[[paste0(var, "_n")]] <- numeric()
    }
    return(result)
  }

  # For each variable, compute merge span info in long format, then pivot_wider
  long_list <- vector("list", length(varlist))

  for (j in seq_along(varlist)) {
    var <- varlist[j]

    # --- Detect SAS-style first.var and last.var ---
    # first.var = TRUE when this variable (or any higher-level variable) changes
    is_first <- rep(FALSE, n_rows)
    is_first[1L] <- TRUE
    for (k in seq_len(j)) {
      v <- as.character(data[[varlist[k]]])
      v[is.na(v)] <- "\x01NA_SENTINEL\x01"
      change <- c(TRUE, v[-1L] != v[-n_rows])
      is_first <- is_first | change
    }

    # last.var = TRUE when the NEXT row would trigger first.var, or at last row
    is_last <- rep(FALSE, n_rows)
    is_last[n_rows] <- TRUE
    for (k in seq_len(j)) {
      v <- as.character(data[[varlist[k]]])
      v[is.na(v)] <- "\x01NA_SENTINEL\x01"
      change <- c(v[-n_rows] != v[-1L], TRUE)
      is_last <- is_last | change
    }

    # --- Compute n values (SAS lines 653-662) ---
    # At first.var: start new group, n = 0
    # Non-first: n increments
    # At last.var: record n at the starting row
    var_n <- rep(NA_real_, n_rows)
    start_row <- 1L
    count <- 0L

    for (i in seq_len(n_rows)) {
      if (is_first[i]) {
        start_row <- i
        count <- 0L
      } else {
        count <- count + 1L
      }
      if (is_last[i]) {
        var_n[start_row] <- as.numeric(count)
      }
    }

    # Collect non-NA entries in long format for pivot_wider
    non_na_idx <- which(!is.na(var_n))
    if (length(non_na_idx) > 0L) {
      long_list[[j]] <- dplyr::tibble(
        row = non_na_idx,
        col_name = paste0(var, "_n"),
        n_value = var_n[non_na_idx]
      )
    }
  }

  # Combine all long results and pivot to wide
  long_combined <- dplyr::bind_rows(long_list)

  if (nrow(long_combined) > 0L) {
    wide_df <- long_combined %>%
      tidyr::pivot_wider(names_from = col_name, values_from = n_value)
  } else {
    wide_df <- dplyr::tibble(row = integer())
    for (var in varlist) {
      wide_df[[paste0(var, "_n")]] <- numeric()
    }
  }

  # Join with full row sequence to include NA rows
  all_rows <- dplyr::tibble(row = seq_len(n_rows))
  result <- all_rows %>%
    dplyr::left_join(wide_df, by = "row")

  # Ensure all expected columns exist
  for (var in varlist) {
    col_name <- paste0(var, "_n")
    if (!col_name %in% names(result)) {
      result[[col_name]] <- NA_real_
    }
  }

  result
}


# ==============================================================================
# group_subset_pp — Grouping and Subsetting Preprocessing
# ------------------------------------------------------------------------------
# Replaces SAS %group_subset_pp macro (SAS lines 36-465).
# Preprocesses Script Launcher grouping/subsetting metadata into formatted
# tibbles and human-readable descriptions.
#
# @param sl_group    Tibble or NULL. Grouping metadata with columns:
#                    group_name, domain, partition, var_name, var_value,
#                    dsvg_grp_name.
# @param sl_subset   Tibble or NULL. Subsetting metadata with columns:
#                    name, domain, partition, var_name, var_value,
#                    outer_operator, inner_operator.
# @param sl_datasets Tibble or NULL. Datasets metadata with columns:
#                    datatype, default, partition_variable.
#
# @return Named list with:
#   sl_group_desc, sl_subset_desc, sl_subset_operator, sl_gs_desc,
#   sl_custom_ds, sl_out_group, sl_out_subset,
#   sl_group_nobs, sl_subset_nobs,
#   sl_group_sorted, sl_subset_sorted
# ==============================================================================
group_subset_pp <- function(sl_group = NULL,
                            sl_subset = NULL,
                            sl_datasets = NULL) {

  cli::cli_inform("SL GROUPING/SUBSETTING PREPROCESSING")

  # --- Row counts (SAS lines 40-52) ---
  sl_group_nobs <- if (!is.null(sl_group) && is.data.frame(sl_group) &&
                        nrow(sl_group) > 0L) nrow(sl_group) else 0L
  sl_subset_nobs <- if (!is.null(sl_subset) && is.data.frame(sl_subset) &&
                         nrow(sl_subset) > 0L) nrow(sl_subset) else 0L

  # ==========================================================================
  # Grouping description (SAS lines 58-92)
  # ==========================================================================
  if (sl_group_nobs > 0L) {
    group_names <- sl_group %>%
      dplyr::distinct(group_name) %>%
      dplyr::pull(group_name)

    group_count <- dplyr::n_distinct(sl_group$group_name)
    sl_group_desc <- paste(group_names, collapse = ", ")

    # Oxford comma handling (SAS lines 72-87)
    if (group_count == 2L) {
      # Replace single ", " with " and " (SAS line 78)
      sl_group_desc <- sub(", ", " and ", sl_group_desc, fixed = TRUE)
    } else if (group_count > 2L) {
      # Insert " and" before the last comma (SAS lines 79-81)
      pos <- regexpr(",([^,]*)$", sl_group_desc)
      if (pos > 0L) {
        sl_group_desc <- paste0(
          substr(sl_group_desc, 1L, pos),
          " and",
          substr(sl_group_desc, pos + 1L, nchar(sl_group_desc))
        )
      }
    }

    sl_group_desc <- paste0("Grouped by ", sl_group_desc)
  } else {
    sl_group_desc <- "No grouping"
  }

  # ==========================================================================
  # Subsetting description (SAS lines 95-139)
  # ==========================================================================
  if (sl_subset_nobs > 0L) {
    # Extract distinct outer operators (SAS lines 101-102)
    sl_subset_outer_vals <- sl_subset %>%
      dplyr::distinct(outer_operator) %>%
      dplyr::pull(outer_operator) %>%
      tolower() %>%
      unique()

    # Error check: more than 1 distinct outer_operator (SAS lines 104-105)
    if (length(sl_subset_outer_vals) > 1L) {
      cli::cli_warn(
        "Multiple distinct outer_operator values found: {paste(sl_subset_outer_vals, collapse = ', ')}"
      )
    }
    sl_subset_outer <- sl_subset_outer_vals[1L]

    # Distinct subset names joined by operator (SAS lines 107-108)
    subset_names <- sl_subset %>%
      dplyr::distinct(name) %>%
      dplyr::pull(name)

    sl_subset_count <- dplyr::n_distinct(sl_subset$name)
    sl_subset_desc <- paste0(
      "Subset by ",
      paste(subset_names, collapse = paste0(" ", sl_subset_outer, " "))
    )

    # Operator description (SAS lines 119-133)
    if (sl_subset_count > 1L) {
      sl_subset_operator <- switch(
        sl_subset_outer,
        "and" = paste0("ALL of the following rules must be true ",
                       "for a subject to be included in the analysis."),
        "or"  = paste0("ANY of the following rules must be true ",
                       "for a subject to be included in the analysis."),
        ""
      )
      if (is.null(sl_subset_operator)) sl_subset_operator <- ""
    } else {
      sl_subset_operator <- ""
    }
  } else {
    sl_subset_desc <- "No subsetting"
    sl_subset_operator <- "N/A"
  }

  cli::cli_inform("{sl_group_desc}")
  cli::cli_inform("{sl_subset_desc}")

  # ==========================================================================
  # Single-line combined description (SAS lines 145-157)
  # ==========================================================================
  if (sl_group_nobs > 0L && sl_subset_nobs > 0L) {
    sl_gs_desc <- paste0(sl_group_desc, "; ", sl_subset_desc)
  } else if (sl_group_nobs > 0L) {
    sl_gs_desc <- sl_group_desc
  } else if (sl_subset_nobs > 0L) {
    sl_gs_desc <- sl_subset_desc
  } else {
    sl_gs_desc <- ""
  }

  cli::cli_inform("{sl_gs_desc}")

  # ==========================================================================
  # Group detail table (SAS lines 160-277)
  # ==========================================================================
  cli::cli_inform("GROUP DETAIL")

  if (sl_group_nobs > 0L) {
    # Sort (SAS line 170)
    sorted_grp <- sl_group %>%
      dplyr::arrange(group_name, partition, var_name, dsvg_grp_name, var_value)

    n_g <- nrow(sorted_grp)

    # Build var_desc: attempt haven label lookup, fallback to var_name
    # SAS lines 194-208: dsid = open(domain); varlabel(dsid, varnum)
    sorted_grp <- sorted_grp %>%
      dplyr::mutate(
        var_desc = var_name,
        partition_desc = dplyr::if_else(
          !is.na(partition) & partition != "",
          partition,
          "N/A"
        ),
        placeholder = NA_character_
      )

    # Store original sorted version for xml_out gs_rows
    sl_group_sorted <- sorted_grp

    # --- Display formatting: blank first occurrence duplicates (SAS 255-266) ---
    # Use dplyr::lag() to detect when a grouping key changes from one row to
    # the next (replaces SAS BY-group first. variable logic).
    gn_v <- as.character(dplyr::coalesce(sorted_grp$group_name, ""))
    pt_v <- as.character(dplyr::coalesce(sorted_grp$partition, ""))
    vn_v <- as.character(dplyr::coalesce(sorted_grp$var_name, ""))
    dg_v <- as.character(dplyr::coalesce(sorted_grp$dsvg_grp_name, ""))

    first_gn <- gn_v != dplyr::lag(gn_v, default = "")
    first_pt <- first_gn | (pt_v != dplyr::lag(pt_v, default = ""))
    first_vn <- first_pt | (vn_v != dplyr::lag(vn_v, default = ""))
    first_dg <- first_vn | (dg_v != dplyr::lag(dg_v, default = ""))

    sl_out_group <- sorted_grp %>%
      dplyr::mutate(
        group_name    = dplyr::if_else(first_gn, group_name, ""),
        domain        = dplyr::if_else(first_gn, domain, ""),
        partition_desc = dplyr::if_else(first_pt, partition_desc, ""),
        var_desc      = dplyr::if_else(first_vn, var_desc, ""),
        dsvg_grp_name = dplyr::if_else(first_dg, dsvg_grp_name, "")
      ) %>%
      dplyr::select(group_name, domain, partition_desc, placeholder,
                    var_desc, var_value, dsvg_grp_name)

  } else {
    sl_out_group <- dplyr::tibble(
      group_name = character(), domain = character(),
      partition_desc = character(), placeholder = character(),
      var_desc = character(), var_value = character(),
      dsvg_grp_name = character()
    )
    sl_group_sorted <- sl_out_group
  }

  # ==========================================================================
  # Subset detail table (SAS lines 280-448)
  # ==========================================================================
  cli::cli_inform("SUBSET DETAIL")

  if (sl_subset_nobs > 0L) {
    # Sort and add subset_name alias for 'name' (SAS line 286)
    sorted_sub <- sl_subset %>%
      dplyr::mutate(subset_name = name) %>%
      dplyr::arrange(subset_name, domain, partition, var_name, var_value)

    n_s <- nrow(sorted_sub)

    # --- Hierarchical first-occurrence flags ---
    sn_v <- as.character(dplyr::coalesce(sorted_sub$subset_name, ""))
    dm_v <- as.character(dplyr::coalesce(sorted_sub$domain, ""))
    pt_v <- as.character(dplyr::coalesce(sorted_sub$partition, ""))
    vn_v <- as.character(dplyr::coalesce(sorted_sub$var_name, ""))

    first_sn <- c(TRUE, sn_v[-1L] != sn_v[-n_s])
    first_dm <- first_sn | c(TRUE, dm_v[-1L] != dm_v[-n_s])
    first_pt <- first_dm | c(TRUE, pt_v[-1L] != pt_v[-n_s])
    first_vn <- first_pt | c(TRUE, vn_v[-1L] != vn_v[-n_s])

    # --- Condition numbering (SAS lines 334-342) ---
    # condition_no: retained counter incrementing at each new subset_name
    condition_no <- cumsum(first_sn)

    # condition_sub_no: resets at each new subset, increments at new var key
    condition_sub_no <- integer(n_s)
    current_sub <- 0L
    for (i in seq_len(n_s)) {
      if (first_sn[i]) current_sub <- 0L
      if (first_vn[i]) current_sub <- current_sub + 1L
      condition_sub_no[i] <- current_sub
    }

    # Build var_desc, partition_desc, condition label (SAS lines 316-386)
    sorted_sub <- sorted_sub %>%
      dplyr::mutate(
        condition_no      = condition_no,
        condition_sub_no  = condition_sub_no,
        condition         = paste0(condition_no, ".", condition_sub_no),
        var_desc          = var_name,
        partition_desc    = dplyr::if_else(
          !is.na(partition) & partition != "",
          partition,
          "N/A"
        )
      )

    # --- Condition counts per subset (SAS lines 299-305) ---
    cond_counts <- sorted_sub %>%
      dplyr::distinct(subset_name, condition) %>%
      dplyr::group_by(subset_name) %>%
      dplyr::summarise(condition_count = dplyr::n(), .groups = "drop")

    # --- Operator description per subset (SAS lines 389-418) ---
    build_operator <- function(inner_op, ccount, cond_no) {
      if (is.na(inner_op) || nchar(trimws(inner_op)) == 0L) return("N/A")
      inner_up <- toupper(trimws(inner_op))
      parts <- paste0(cond_no, ".", seq_len(ccount))
      op_str <- paste(parts, collapse = paste0(" ", inner_up, " "))
      prefix <- if (ccount > 1L) "Conditions " else "Condition "
      trimws(paste0(prefix, op_str))
    }

    first_row_info <- sorted_sub %>%
      dplyr::group_by(subset_name) %>%
      dplyr::filter(dplyr::row_number() == 1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(subset_name, inner_operator, condition_no) %>%
      dplyr::left_join(cond_counts, by = "subset_name")

    operators_df <- first_row_info %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        operator = build_operator(inner_operator, condition_count, condition_no)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(subset_name, operator)

    sorted_sub <- sorted_sub %>%
      dplyr::left_join(operators_df, by = "subset_name")

    # Store original sorted for xml_out gs_rows calculations
    sl_subset_sorted <- sorted_sub

    # --- Display formatting: blank first-occurrence duplicates (SAS 422-434) ---
    sl_out_subset <- sorted_sub %>%
      dplyr::mutate(
        subset_name    = dplyr::if_else(first_sn, subset_name, ""),
        domain         = dplyr::if_else(first_dm, domain, ""),
        partition_desc = dplyr::if_else(first_pt, partition_desc, ""),
        var_desc       = dplyr::if_else(first_vn, var_desc, ""),
        condition      = dplyr::if_else(first_vn, condition, ""),
        operator       = dplyr::if_else(first_sn, operator, "")
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
    sl_subset_sorted <- sl_out_subset
  }

  # ==========================================================================
  # Custom datasets (SAS lines 451-464)
  # ==========================================================================
  cli::cli_inform("CUSTOM DATASETS")
  sl_custom_ds <- ""
  if (!is.null(sl_datasets) && is.data.frame(sl_datasets) &&
      nrow(sl_datasets) > 0L) {
    custom_rows <- sl_datasets %>%
      dplyr::filter(default == "N") %>%
      dplyr::pull(datatype)
    if (length(custom_rows) > 0L) {
      sl_custom_ds <- paste(custom_rows, collapse = ", ")
    }
  }

  # ==========================================================================
  # Return structured result list
  # ==========================================================================
  list(
    sl_group_desc      = sl_group_desc,
    sl_subset_desc     = sl_subset_desc,
    sl_subset_operator = sl_subset_operator,
    sl_gs_desc         = sl_gs_desc,
    sl_custom_ds       = sl_custom_ds,
    sl_out_group       = sl_out_group,
    sl_out_subset      = sl_out_subset,
    sl_group_nobs      = sl_group_nobs,
    sl_subset_nobs     = sl_subset_nobs,
    sl_group_sorted    = sl_group_sorted,
    sl_subset_sorted   = sl_subset_sorted
  )
}


# ==============================================================================
# group_subset_xls_out — Excel XLS Template Output
# ------------------------------------------------------------------------------
# Replaces SAS %group_subset_xls_out macro (SAS lines 471-559).
# Creates a standalone Excel workbook with 3 worksheets:
#   group_detail, subset_detail, group_subset_info
#
# @param gs_file   Character. File path for the output Excel workbook.
# @param pp_result Named list returned by group_subset_pp().
# @return Invisible NULL. Workbook is saved to gs_file.
# ==============================================================================
group_subset_xls_out <- function(gs_file, pp_result) {
  cli::cli_inform("GROUPING/SUBSETTING EXCEL TEMPLATE OUTPUT")

  # Row counts (SAS lines 475-481)
  sl_group_row_count  <- nrow(pp_result$sl_out_group)
  sl_subset_row_count <- nrow(pp_result$sl_out_subset)

  # --- Build group_subset_info table (SAS lines 484-519) ---
  # Use dplyr::case_when() for multi-condition note text derivation
  note_text <- dplyr::case_when(
    sl_group_row_count == 0L & sl_subset_row_count > 0L  ~
      "No grouping was used.",
    sl_group_row_count > 0L  & sl_subset_row_count == 0L ~
      "No subsetting was used.",
    sl_group_row_count == 0L & sl_subset_row_count == 0L ~
      "Neither grouping nor subsetting were used.",
    TRUE ~ ""
  )

  sl_out_group_subset_info <- dplyr::tibble(
    val_desc = c("Grouped by", "Subset by", "Group row count",
                 "Subset row count", "Subset operator", "Note",
                 "GS description"),
    val = c(pp_result$sl_group_desc,
            pp_result$sl_subset_desc,
            as.character(sl_group_row_count),
            as.character(sl_subset_row_count),
            pp_result$sl_subset_operator,
            note_text,
            pp_result$sl_gs_desc)
  )

  # --- Write to Excel via openxlsx (SAS lines 524-558) ---
  wb <- openxlsx::createWorkbook()

  # group_detail sheet
  openxlsx::addWorksheet(wb, "group_detail")
  if (sl_group_row_count > 0L) {
    openxlsx::writeData(wb, "group_detail", pp_result$sl_out_group)
  }

  # subset_detail sheet
  openxlsx::addWorksheet(wb, "subset_detail")
  if (sl_subset_row_count > 0L) {
    openxlsx::writeData(wb, "subset_detail", pp_result$sl_out_subset)
  }

  # group_subset_info sheet
  openxlsx::addWorksheet(wb, "group_subset_info")
  openxlsx::writeData(wb, "group_subset_info", sl_out_group_subset_info)

  openxlsx::saveWorkbook(wb, gs_file, overwrite = TRUE)
  cli::cli_inform("Workbook saved to {.file {gs_file}}")

  invisible(NULL)
}


# ==============================================================================
# group_subset_xml_out — Formatted Excel XML Worksheet Output
# ------------------------------------------------------------------------------
# Replaces SAS %group_subset_xml_out macro (SAS lines 565-1132).
# Adds a "Grouping and Subsetting" worksheet to an existing openxlsx workbook
# with formatted sections, merged cells, and professional styles.
#
# @param wb        An openxlsx Workbook object.
# @param pp_result Named list returned by group_subset_pp().
# @param ndabla    Character. NDA/BLA identifier.
# @param studyid   Character. Study identifier.
# @param styles    Named list of openxlsx Style objects from create_styles().
# @return The modified workbook object (invisibly).
# ==============================================================================
group_subset_xml_out <- function(wb, pp_result, ndabla, studyid, styles) {
  cli::cli_inform("SL GROUPING/SUBSETTING EXCEL XML OUTPUT")

  sheet_name <- "Grouping and Subsetting"
  openxlsx::addWorksheet(wb, sheet_name)

  # --- Column widths (SAS lines 587-594) ---
  # SAS widths in points: 162, 48, 162, 162, 50.5, 162, 162, auto(default)
  # openxlsx uses character widths; approximate conversion: points / 7
  col_widths <- c(162 / 7, 48 / 7, 162 / 7, 162 / 7,
                  50.5 / 7, 162 / 7, 162 / 7, 8.43)
  openxlsx::setColWidths(wb, sheet_name, cols = 1:8, widths = col_widths)

  current_row <- 1L

  # ==========================================================================
  # Local helper: build annotated text block for write_annotated_data
  # ==========================================================================
  make_text_block <- function(rows_spec) {
    n <- length(rows_spec)
    result_list <- vector("list", n)
    for (i in seq_len(n)) {
      r <- rows_spec[[i]]
      result_list[[i]] <- dplyr::tibble(
        Row         = i,
        Data        = if (!is.null(r$Data)) r$Data else "",
        Type        = "String",
        varname     = NA_character_,
        bottom      = if (i == n) 1L else 0L,
        Height      = if (!is.null(r$Height)) as.numeric(r$Height) else NA_real_,
        Index       = NA_real_,
        MergeAcross = if (!is.null(r$MergeAcross)) as.numeric(r$MergeAcross) else NA_real_,
        MergeDown   = NA_real_,
        StyleID     = if (!is.null(r$StyleID)) r$StyleID else NA_character_,
        Formula     = NA_character_,
        Comment     = NA_character_,
        Name        = NA_character_,
        ArrayRange  = NA_character_
      )
    }
    dplyr::bind_rows(result_list)
  }

  # ==========================================================================
  # Header section (SAS lines 602-630)
  # ==========================================================================
  run_date_str <- paste0("Analysis run date: ",
                         format(Sys.Date(), "%Y-%m-%d"), " ",
                         format(Sys.time(), "%I:%M:%S %p"))

  header_block <- make_text_block(list(
    list(Data = ""),
    list(Data = "Grouping and Subsetting Summary", StyleID = "Header"),
    list(Data = ""),
    list(Data = paste0("NDA/BLA: ", ndabla), StyleID = "Default8"),
    list(Data = paste0("Study: ", studyid), StyleID = "Default8"),
    list(Data = run_date_str, StyleID = "Default8")
  ))
  current_row <- write_annotated_data(wb, sheet_name, header_block,
                                       styles, current_row)

  sl_group_nobs  <- pp_result$sl_group_nobs
  sl_subset_nobs <- pp_result$sl_subset_nobs

  # ==========================================================================
  # Main content: grouping + subsetting, or neither
  # ==========================================================================
  if (sl_group_nobs > 0L || sl_subset_nobs > 0L) {

    # ========================================================================
    # GROUPING section (SAS lines 676-853)
    # ========================================================================
    if (sl_group_nobs > 0L) {
      cli::cli_inform("GROUPING")

      # --- Section text (SAS lines 684-723) ---
      grp_text <- make_text_block(list(
        list(Data = ""),
        list(Data = ""),
        list(Data = pp_result$sl_group_desc, StyleID = "SubHeader"),
        list(Data = ""),
        list(Data = paste0(
          "The table below shows the rules used for grouping values together. ",
          "Each grouping applies to a single domain and variable. The values ",
          "of that variable can be put into more than one group. For example, ",
          "if four arms from the planned arm (ARM) variable in domain DM are ",
          "put into two groups -- one with the control arm and the other with ",
          "the three other arms -- this table would show four values in the ",
          "Original Value column mapped onto two values in the Grouped Value ",
          "column."),
          StyleID = "Default10Wrap", MergeAcross = 6, Height = 50),
        list(Data = paste0(
          "If a grouping for the LB or VS domain has a non-empty cell in the ",
          "Test column, that grouping is applied only to lab or vital sign ",
          "tests of the kind stated in the Test column. "),
          StyleID = "Default10Wrap", MergeAcross = 6, Height = 12.75),
        list(Data = "")
      ))
      current_row <- write_annotated_data(wb, sheet_name, grp_text,
                                           styles, current_row)

      # --- Column headers (SAS lines 728-746) ---
      grp_hdrs  <- c("Grouping Name", "Domain", "Test",
                      "Variable", "Original Value", "Grouped Value")
      grp_cols  <- c(1L, 2L, 3L, 4L, 6L, 7L)
      openxlsx::setRowHeights(wb, sheet_name, rows = current_row, heights = 30)
      for (hi in seq_along(grp_hdrs)) {
        openxlsx::writeData(wb, sheet_name, grp_hdrs[hi],
                            startRow = current_row, startCol = grp_cols[hi])
        openxlsx::addStyle(wb, sheet_name, styles[["ColumnOutline"]],
                           rows = current_row, cols = grp_cols[hi],
                           stack = TRUE)
      }
      # "Variable" merges cols 4-5
      openxlsx::mergeCells(wb, sheet_name, cols = 4:5, rows = current_row)
      openxlsx::addStyle(wb, sheet_name, styles[["ColumnOutline"]],
                         rows = current_row, cols = 5L, stack = TRUE)
      current_row <- current_row + 1L

      # --- Data table with merged cells (SAS lines 750-826) ---
      grp_sorted <- pp_result$sl_group_sorted
      merge_g <- gs_rows(grp_sorted,
                         c("group_name", "partition", "var_name",
                           "dsvg_grp_name"))
      n_g <- nrow(grp_sorted)
      data_start <- current_row

      for (i in seq_len(n_g)) {
        r <- data_start + i - 1L
        rd <- grp_sorted[i, ]
        mi <- merge_g[i, ]
        is_bottom <- (i == n_g)

        gn_n  <- mi$group_name_n
        dg_n  <- mi$dsvg_grp_name_n

        # --- Cells merged on group_name span: cols 1,2,3,4-5 (SAS 801-808) ---
        if (!is.na(gn_n)) {
          md <- as.integer(gn_n)

          # Col 1: group_name
          openxlsx::writeData(wb, sheet_name, rd$group_name,
                              startRow = r, startCol = 1L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 1L, stack = TRUE)
          if (md > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 1L,
                                 rows = r:(r + md))
          }

          # Col 2: domain
          domain_val <- if (!is.na(rd$domain)) rd$domain else ""
          openxlsx::writeData(wb, sheet_name, domain_val,
                              startRow = r, startCol = 2L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 2L, stack = TRUE)
          if (md > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 2L,
                                 rows = r:(r + md))
          }

          # Col 3: partition_desc
          part_val <- if (!is.na(rd$partition) && rd$partition != "") {
            rd$partition
          } else {
            "N/A"
          }
          openxlsx::writeData(wb, sheet_name, part_val,
                              startRow = r, startCol = 3L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 3L, stack = TRUE)
          if (md > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 3L,
                                 rows = r:(r + md))
          }

          # Cols 4-5: var_desc (MergeAcross to span 2 columns)
          var_desc_val <- if (!is.na(rd$var_name)) rd$var_name else ""
          openxlsx::writeData(wb, sheet_name, var_desc_val,
                              startRow = r, startCol = 4L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 4L, stack = TRUE)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 5L, stack = TRUE)
          if (md > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 4:5,
                                 rows = r:(r + md))
          } else {
            openxlsx::mergeCells(wb, sheet_name, cols = 4:5, rows = r)
          }
        }

        # --- Col 6: var_value (no merge, specific styles) (SAS 810-815) ---
        vv_style <- if (!is.na(dg_n)) {
          if (is_bottom) "GS_BTLRB" else "GS_BTLR"
        } else {
          if (is_bottom) "GS_BLRB" else "GS_BLR"
        }
        vv_val <- if (!is.na(rd$var_value)) rd$var_value else ""
        openxlsx::writeData(wb, sheet_name, vv_val,
                            startRow = r, startCol = 6L)
        openxlsx::addStyle(wb, sheet_name, styles[[vv_style]],
                           rows = r, cols = 6L, stack = TRUE)

        # --- Col 7: dsvg_grp_name (merge on dsvg_grp_name_n) (SAS 805-808) ---
        if (!is.na(dg_n)) {
          md_dg <- as.integer(dg_n)
          dg_val <- if (!is.na(rd$dsvg_grp_name)) rd$dsvg_grp_name else ""
          openxlsx::writeData(wb, sheet_name, dg_val,
                              startRow = r, startCol = 7L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 7L, stack = TRUE)
          if (md_dg > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 7L,
                                 rows = r:(r + md_dg))
          }
        }
      }
      current_row <- data_start + n_g

    } else {
      # --- No grouping (SAS lines 828-853) ---
      cli::cli_inform("NO GROUPING")
      no_grp_block <- make_text_block(list(
        list(Data = ""),
        list(Data = ""),
        list(Data = pp_result$sl_group_desc, StyleID = "SubHeader")
      ))
      current_row <- write_annotated_data(wb, sheet_name, no_grp_block,
                                           styles, current_row)
    }

    # ========================================================================
    # SUBSETTING section (SAS lines 855-1058)
    # ========================================================================
    if (sl_subset_nobs > 0L) {
      cli::cli_inform("SUBSETTING")

      # --- Section text (SAS lines 862-920) ---
      sub_text <- make_text_block(list(
        list(Data = ""),
        list(Data = ""),
        list(Data = pp_result$sl_subset_desc, StyleID = "SubHeader"),
        list(Data = ""),
        list(Data = paste0(
          "The table below shows the rules for subsetting subjects and those ",
          "subjects' observations in other domains. For a subject to be ",
          "included in the subset and used in analysis, they must have at ",
          "least one value in the Value column for the associated variable ",
          "in the Variable column. For example, if the Value column has ",
          "values 5 through 10 for domain LB and variable LBSTRESN, a ",
          "subject must have at least one lab test in LB with LBSTRESN ",
          "from 5 and 10."),
          StyleID = "Default10Wrap", MergeAcross = 6, Height = 50),
        list(Data = paste0(
          "Each subset can be made up of several conditions and these are ",
          "numbered in the Condition No. column. The 'Which Conditions ",
          "Apply? (AND vs OR)' column states whether for a given subset, ",
          "all its conditions must be true for a subject to be included in ",
          "the subset, or any one of them being true will suffice. If all ",
          "must be true, this column will list out all the subset's ",
          "conditions separated by an 'AND'. If only one must be true, the ",
          "subset's conditions will be separated by an 'OR'.'"),
          StyleID = "Default10Wrap", MergeAcross = 6, Height = 50),
        list(Data = paste0(
          "If a subset using the LB or VS domain has a non-empty cell in ",
          "the Test column, only lab or vital sign tests of the kind ",
          "stated in the Test column are used to determine whether a ",
          "subject should be included in the subset. For example, if the ",
          "domain is LB and lab test is ALBUMIN and variable LBSTRESN ",
          "must have values from 5 to 10, only subjects with albumin lab ",
          "test results from 5 to 10 are included in the subset and used ",
          "in subsequent analysis."),
          StyleID = "Default10Wrap", MergeAcross = 6, Height = 50),
        list(Data = pp_result$sl_subset_operator,
             StyleID = "SubHeader", Height = 12.75),
        list(Data = "", Height = 12.75)
      ))
      current_row <- write_annotated_data(wb, sheet_name, sub_text,
                                           styles, current_row)

      # --- Column headers (SAS lines 925-943) ---
      sub_hdrs <- c("Subset Name", "Domain", "Test", "Variable",
                     "Condition No.", "Value",
                     "Which Conditions Apply?\n(AND vs OR)")
      openxlsx::setRowHeights(wb, sheet_name, rows = current_row,
                              heights = 30)
      for (hi in seq_along(sub_hdrs)) {
        openxlsx::writeData(wb, sheet_name, sub_hdrs[hi],
                            startRow = current_row, startCol = hi)
        openxlsx::addStyle(wb, sheet_name, styles[["ColumnOutline"]],
                           rows = current_row, cols = hi, stack = TRUE)
      }
      current_row <- current_row + 1L

      # --- Data table with merged cells (SAS lines 946-1032) ---
      sub_sorted <- pp_result$sl_subset_sorted
      merge_s <- gs_rows(sub_sorted,
                         c("subset_name", "domain", "partition",
                           "var_name"))
      n_s <- nrow(sub_sorted)
      data_start_s <- current_row

      for (i in seq_len(n_s)) {
        r <- data_start_s + i - 1L
        rd <- sub_sorted[i, ]
        mi <- merge_s[i, ]
        is_bottom <- (i == n_s)

        sn_n  <- mi$subset_name_n
        dm_n  <- mi$domain_n
        pt_n  <- mi$partition_n
        vn_n  <- mi$var_name_n

        # --- Col 1: subset_name (merge on subset_name_n) (SAS 992-994) ---
        if (!is.na(sn_n)) {
          md_sn <- as.integer(sn_n)
          sn_val <- if (!is.na(rd$subset_name)) rd$subset_name else ""
          openxlsx::writeData(wb, sheet_name, sn_val,
                              startRow = r, startCol = 1L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 1L, stack = TRUE)
          if (md_sn > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 1L,
                                 rows = r:(r + md_sn))
          }
        }

        # --- Col 2: domain (merge on domain_n) (SAS 996-998) ---
        if (!is.na(dm_n)) {
          md_dm <- as.integer(dm_n)
          dm_val <- if (!is.na(rd$domain)) rd$domain else ""
          openxlsx::writeData(wb, sheet_name, dm_val,
                              startRow = r, startCol = 2L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 2L, stack = TRUE)
          if (md_dm > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 2L,
                                 rows = r:(r + md_dm))
          }
        }

        # --- Col 3: partition_desc (merge on partition_n) (SAS 1000-1002) ---
        if (!is.na(pt_n)) {
          md_pt <- as.integer(pt_n)
          pt_val <- if (!is.na(rd$partition_desc)) rd$partition_desc else "N/A"
          openxlsx::writeData(wb, sheet_name, pt_val,
                              startRow = r, startCol = 3L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 3L, stack = TRUE)
          if (md_pt > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 3L,
                                 rows = r:(r + md_pt))
          }
        }

        # --- Col 4: var_desc (merge on var_name_n) (SAS 1004-1006) ---
        if (!is.na(vn_n)) {
          md_vn <- as.integer(vn_n)
          vd_val <- if (!is.na(rd$var_desc)) rd$var_desc else ""
          openxlsx::writeData(wb, sheet_name, vd_val,
                              startRow = r, startCol = 4L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 4L, stack = TRUE)
          if (md_vn > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 4L,
                                 rows = r:(r + md_vn))
          }
        }

        # --- Col 5: condition (merge on var_name_n) (SAS 1004-1006) ---
        if (!is.na(vn_n)) {
          md_vn2 <- as.integer(vn_n)
          cond_val <- if (!is.na(rd$condition)) rd$condition else ""
          openxlsx::writeData(wb, sheet_name, cond_val,
                              startRow = r, startCol = 5L)
          # condition uses GSC_BTLRB style (SAS line 1015)
          openxlsx::addStyle(wb, sheet_name, styles[["GSC_BTLRB"]],
                             rows = r, cols = 5L, stack = TRUE)
          if (md_vn2 > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 5L,
                                 rows = r:(r + md_vn2))
          }
        }

        # --- Col 6: var_value (no merge, specific styles) (SAS 1017-1021) ---
        vv_style_s <- if (!is.na(vn_n)) {
          if (is_bottom) "GS_BTLRB" else "GS_BTLR"
        } else {
          if (is_bottom) "GS_BLRB" else "GS_BLR"
        }
        vv_val_s <- if (!is.na(rd$var_value)) rd$var_value else ""
        openxlsx::writeData(wb, sheet_name, vv_val_s,
                            startRow = r, startCol = 6L)
        openxlsx::addStyle(wb, sheet_name, styles[[vv_style_s]],
                           rows = r, cols = 6L, stack = TRUE)

        # --- Col 7: operator (merge on subset_name_n) (SAS 992-994) ---
        if (!is.na(sn_n)) {
          md_op <- as.integer(sn_n)
          op_val <- if (!is.na(rd$operator)) rd$operator else ""
          openxlsx::writeData(wb, sheet_name, op_val,
                              startRow = r, startCol = 7L)
          openxlsx::addStyle(wb, sheet_name, styles[["GS_BTLRB"]],
                             rows = r, cols = 7L, stack = TRUE)
          if (md_op > 0L) {
            openxlsx::mergeCells(wb, sheet_name, cols = 7L,
                                 rows = r:(r + md_op))
          }
        }
      }
      current_row <- data_start_s + n_s

    } else {
      # --- No subsetting (SAS lines 1034-1058) ---
      cli::cli_inform("NO SUBSETTING")
      no_sub_block <- make_text_block(list(
        list(Data = ""),
        list(Data = ""),
        list(Data = pp_result$sl_subset_desc, StyleID = "SubHeader")
      ))
      current_row <- write_annotated_data(wb, sheet_name, no_sub_block,
                                           styles, current_row)
    }

  } else {
    # ========================================================================
    # Neither grouping nor subsetting (SAS lines 1062-1092)
    # ========================================================================
    cli::cli_inform("NO GROUPING OR SUBSETTING")

    neither_block <- make_text_block(list(
      list(Data = ""),
      list(Data = ""),
      list(Data = "Neither grouping nor subsetting were used",
           StyleID = "SubHeader")
    ))
    current_row <- write_annotated_data(wb, sheet_name, neither_block,
                                         styles, current_row)
  }

  # ==========================================================================
  # Page setup (SAS lines 1094-1112)
  # ==========================================================================
  cli::cli_inform("COMBINE AND OUTPUT")

  openxlsx::pageSetup(wb, sheet_name, orientation = "landscape",
                       scale = 78, fitToWidth = TRUE, fitToHeight = FALSE)

  header_left  <- "Grouping and Subsetting Summary"
  header_right <- paste0("NDA/BLA ", ndabla, "\nStudy ", studyid)
  footer_text  <- "Page &[Page] of &[Pages]"

  openxlsx::setHeaderFooter(wb, sheet_name,
    header = c(header_left, NA, header_right),
    footer = c(NA, footer_text, NA)
  )

  invisible(wb)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. SAS PCFILES/JET engine for direct Excel writes is replaced by
#      openxlsx::createWorkbook()/addWorksheet()/writeData()/saveWorkbook().
#   2. SpreadsheetML XML <MergeAcross> and <MergeDown> are replaced by
#      openxlsx::mergeCells() using row and column range parameters.
#   3. Variable labels in SAS were accessed via OPEN/VARNUM/VARLABEL on the
#      actual analysis dataset; in R, labels would be accessed via haven
#      attribute attr(col, "label"). Since the actual analysis datasets are
#      not passed to group_subset_pp(), var_desc falls back to var_name.
#      To get full labels, pass haven-loaded data frames and enhance
#      var_desc construction with attr() lookups.
#   4. SAS %gs_rows inner macro is extracted as a standalone R helper
#      function gs_rows() that computes merge spans via dplyr grouping
#      and first-occurrence detection.
#   5. SAS hash object for merge span lookup is replaced by a merge-info
#      data frame returned by gs_rows() that is indexed by row position.
#   6. The SAS %markup and %annotate macros are replaced by
#      write_annotated_data() and annotate_data() from xml_output.R for
#      text blocks, and by direct openxlsx API calls for data tables
#      requiring complex merged-cell layouts.
#   7. SAS column widths (in points: 162, 48, 162, 162, 50.5, 162, 162)
#      are converted to openxlsx character-width units by dividing by ~7.
#   8. SAS run_location macro variable logic for PCFILES vs XML output
#      is not needed; R always uses openxlsx for Excel output.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   None expected. This module generates metadata and formatting output
#   only; no statistical computations are performed.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS hash objects for merge-span lookup -> R data frame with
#      gs_rows() helper computing first_row / span columns per variable.
#   2. SAS %gs_rows inner macro (nested within %group_subset_xml_out) ->
#      Standalone R function gs_rows() with equivalent semantics.
#   3. SAS PROC TRANSPOSE for merge formatting -> dplyr/tidyr operations
#      within gs_rows() to compute hierarchical span information.
#   4. SAS PCFILES/JET engine and SpreadsheetML XML string generation ->
#      openxlsx API calls (createWorkbook, writeData, addStyle, mergeCells).
#   5. SAS OPEN()/VARNUM()/VARLABEL() for dataset variable labels ->
#      haven::read_xpt() preserves labels as column attributes; accessed
#      via attr(df$col, "label").
#
# PACKAGE SELECTION RATIONALE:
#   - openxlsx (>=4.2.5): Full Excel workbook manipulation replacing both
#     SpreadsheetML XML generation and SAS PCFILES/JET engine. Supports
#     cell-level formatting, merged cells, page setup, header/footer.
#   - dplyr (>=1.1.0): Core data manipulation for sorting, joining,
#     grouping, filtering, and mutating. Replaces SAS DATA steps,
#     PROC SQL, and PROC SORT throughout all 4 functions.
#   - tidyr (>=1.3.0): Data reshaping for pivot operations in gs_rows()
#     helper when computing merge span specifications.
#   - haven (>=2.5.0): SAS variable label preservation via labelled
#     vectors. When datasets are loaded with haven::read_xpt(), column
#     labels are accessible as attributes for var_desc construction.
#   - cli (>=3.6.0): User-facing diagnostic messages replacing SAS %PUT
#     statements for progress logging throughout the module.
#
# OPEN QUESTIONS:
#   1. Confirm openxlsx mergeCells behavior matches SAS MergeDown
#      precisely for edge cases (single-row merges, boundary merges).
#   2. Verify column width conversion accuracy (SAS points / 7 to
#      openxlsx character-width units) across different display contexts.
#   3. Should var_desc include haven variable labels when the actual
#      analysis datasets are available? If so, group_subset_pp() would
#      need an additional parameter for the loaded datasets.
#   4. Confirm that openxlsx setHeaderFooter() placeholders &[Page] and
#      &[Pages] render correctly as dynamic page numbers in all Excel
#      versions.
# ============================================================
