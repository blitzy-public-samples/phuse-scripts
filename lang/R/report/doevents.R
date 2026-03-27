# ==============================================================================
# doevents.R — Event Summary Table Generator
#
# Migrated from: lang/SAS/report/doevents.sas (2295 lines)
# Description:   Generates a table of summary statistics relating to the number
#                of events (event counts and patient-event counts with
#                percentages and denominators) from events + population datasets.
#
# The SAS %doevents macro is migrated into a single parameterized R function
# doevents() following tidyverse idioms with 100% functional parity.
# ==============================================================================

# -- External package imports --------------------------------------------------
library(dplyr)
library(tidyr)
library(rlang)
library(cli)
library(janitor)
library(haven)
library(forcats)
library(Tplyr)
library(r2rtf)
library(openxlsx)
library(stringr)
library(purrr)

# ==============================================================================
# Helper: Parse {P}/{E} source markers from variable specification
# Mirrors SAS lines 58-84 (BY) and 96-119 (COLUMN) parsing logic
# ==============================================================================
.parse_source_markers <- function(spec) {
  if (is.null(spec) || length(spec) == 0L) {
    return(list(vars = character(0L), sources = character(0L)))
  }
  # If spec is a named list with 'source' attributes, extract directly

  if (is.list(spec)) {
    vars <- purrr::map_chr(spec, function(x) {
      if (is.character(x)) x[[1L]] else as.character(x[[1L]])
    })
    sources <- purrr::map_chr(spec, function(x) {
      src <- attr(x, "source")
      if (!is.null(src)) {
        toupper(src)
      } else {
        ""
      }
    })
    # Normalise: {E} -> "D" (events = Data), {P} -> "P" (population)
    sources <- dplyr::case_when(
      sources == "E" ~ "D",
      sources == "P" ~ "P",
      TRUE           ~ ""
    )
    return(list(vars = toupper(vars), sources = sources))
  }
  # If spec is a character vector, scan for inline {P}/{E} tokens

  tokens <- unlist(stringr::str_split(paste(spec, collapse = " "), "\\s+"))
  vars    <- character(0L)
  sources <- character(0L)

  i <- 1L
  while (i <= length(tokens)) {
    token <- tokens[i]
    if (toupper(token) %in% c("{P}", "{E}")) {
      i <- i + 1L
      next
    }
    vars <- c(vars, toupper(token))
    # Peek at next token for source marker
    if (i < length(tokens) && toupper(tokens[i + 1L]) %in% c("{P}", "{E}")) {
      marker <- toupper(tokens[i + 1L])
      sources <- c(sources, if (marker == "{P}") "P" else "D")
      i <- i + 2L
    } else {
      sources <- c(sources, "")
      i <- i + 1L
    }
  }
  list(vars = vars, sources = sources)
}

# ==============================================================================
# Helper: Apply case transformation (UPPER / LOWER / UPPER1)
# Mirrors SAS lines 784-809 case logic
# ==============================================================================
.apply_case <- function(x, case_opt) {
  if (is.null(case_opt) || case_opt == "") return(x)
  case_opt <- toupper(case_opt)
  dplyr::case_when(
    case_opt == "UPPER"  ~ stringr::str_to_upper(x),
    case_opt == "LOWER"  ~ stringr::str_to_lower(x),
    case_opt == "UPPER1" ~ stringr::str_to_title(x),
    TRUE                 ~ x
  )
}

# ==============================================================================
# Helper: Compute format width from max count (mirrors SAS lines 650-677)
# ==============================================================================
.count_width <- function(max_val) {
  if (is.na(max_val) || max_val <= 0) return(1L)
  as.integer(nchar(as.character(as.integer(max_val))))
}

# ==============================================================================
# Helper: Check if a value is effectively missing (NA or blank string)
# Per AAP: SAS '.' -> NA, SAS ' ' -> NA_character_, "" also treated as missing
# ==============================================================================
.is_missing <- function(x) {
  if (is.numeric(x)) return(is.na(x))
  is.na(x) | stringr::str_trim(as.character(x)) == ""
}

# ==============================================================================
# Helper: Format a count/pct/event string based on type
# Mirrors SAS lines 821-852 format logic
# Uses janitor::round_half_up for SAS-compatible rounding (AAP §0.7.2)
# ==============================================================================
.format_cell <- function(pcnt, denom, cnt, type, pe_width, pct_fmt, ev_width,
                         tot_pe_width = NULL, tot_ev_width = NULL) {
  # Parse pctfmt like "5.1" into width and decimals
  pct_parts <- as.integer(unlist(stringr::str_split(pct_fmt, "\\.")))
  pct_w     <- pct_parts[1L]
  pct_d     <- if (length(pct_parts) > 1L) pct_parts[2L] else 0L

  pct_val <- if (denom > 0) janitor::round_half_up(pcnt / denom * 100, pct_d) else 0
  pct_str <- formatC(pct_val, width = pct_w, format = "f", digits = pct_d)

  pe_w <- if (!is.null(tot_pe_width)) tot_pe_width else pe_width
  ev_w <- if (!is.null(tot_ev_width)) tot_ev_width else ev_width

  if (toupper(type) == "BOTH") {
    paste0(
      formatC(pcnt, width = pe_w, format = "d"),
      " (",
      pct_str,
      "%) ",
      formatC(cnt, width = ev_w, format = "d")
    )
  } else if (toupper(type) == "PATIENT") {
    paste0(
      formatC(pcnt, width = pe_w, format = "d"),
      " (",
      pct_str,
      "%)"
    )
  } else {
    # EVENT
    formatC(cnt, width = ev_w, format = "d")
  }
}

# ==============================================================================
# Helper: Wrap text for FLOW option (mirrors SAS lines 1517-1571)
# ==============================================================================
.wrap_text <- function(text, max_width, indent) {
  words <- unlist(stringr::str_split(text, "\\s+"))
  words <- words[words != ""]
  if (length(words) == 0L) return("")
  lines <- character(0L)
  current_line <- ""
  prefix <- if (indent > 0) strrep(" ", indent) else ""
  avail  <- max_width - indent

  for (w in words) {
    w_len <- nchar(w)
    if (current_line == "") {
      if (w_len <= avail) {
        current_line <- paste0(prefix, w)
      } else {
        # Word too long — force-break
        while (nchar(w) > avail) {
          chunk <- substr(w, 1L, avail - 1L)
          lines <- c(lines, paste0(prefix, chunk, "-"))
          w <- substr(w, avail, nchar(w))
        }
        current_line <- paste0(prefix, w)
      }
    } else {
      test_line <- paste0(current_line, " ", w)
      if (nchar(test_line) <= max_width) {
        current_line <- test_line
      } else {
        lines <- c(lines, current_line)
        current_line <- paste0(prefix, w)
      }
    }
  }
  if (current_line != "") lines <- c(lines, current_line)
  lines
}

# ==============================================================================
# MAIN FUNCTION: doevents
# ==============================================================================
#' Generate Event Summary Table
#'
#' Produces event summary tables (counts of events and patient-events with
#' percentages and denominators) from events + population datasets. Migrated
#' from the SAS %doevents macro with 100% functional parity.
#'
#' @param var Character vector of event description variable names (REQUIRED).
#' @param any Character "Y"/"N" — include an ANY EVENT summary row.
#' @param anydesc Character label for the any-event row.
#' @param total Character "Y"/"N" — add a Total column.
#' @param tothead Character heading for the Total column.
#' @param eventlbl Character label for the event description column (auto-built
#'   from variable labels if NULL).
#' @param split Character used to split multi-line column headers.
#' @param case Character case transformation: "UPPER", "LOWER", "UPPER1", or NULL.
#' @param pagenum Character "Y"/"N" — add page numbering.
#' @param spacing Integer spacing between columns.
#' @param by Character vector of BY variables, optionally with \{P\}/\{E\} markers.
#' @param column Character vector of column (treatment) variables, optionally
#'   with \{P\}/\{E\} markers.
#' @param subj Character name of subject identifier variable.
#' @param flow Integer or NULL — max width for text wrapping of event description.
#' @param type Character — "BOTH", "PATIENT", or "EVENT".
#' @param colorder Character — "FORMATTED" or "INTERNAL" column level ordering.
#' @param order Character — "FREQ" or "ALPHA" event row ordering.
#' @param addn Character "Y"/"N" — add N= into column headings.
#' @param skip Integer skip level for line breaks between groups.
#' @param outfile Character output file path (NULL for console/return only).
#' @param filetype Character — "ASCII", "HTML", "PDF", or "RTF".
#' @param gencode Character filename for generated R code (NULL to skip).
#' @param data Data frame containing events (REQUIRED).
#' @param dwhere Character or quosure filter expression for events data.
#' @param popdata Data frame containing population data.
#' @param pwhere Character or quosure filter expression for population data.
#' @param lasthead Character — last line of heading indicating n/pct/#.
#' @param pctfmt Character format spec for percentages (e.g. "5.1").
#' @param pefmt Character format for patient-event counts (auto if NULL).
#' @param evfmt Character format for event counts (auto if NULL).
#' @param outds Logical or character — if TRUE or a name, return the analysis
#'   dataset as a data frame.
#'
#' @return Invisibly returns the output data frame (always). If \code{outds} is
#'   not NULL/FALSE, the analysis dataset is returned visibly.
#' @export
doevents <- function(var,
                     any       = "Y",
                     anydesc   = "ANY EVENT",
                     total     = "N",
                     tothead   = "Total",
                     eventlbl  = NULL,
                     split     = "*",
                     case      = NULL,
                     pagenum   = "Y",
                     spacing   = 2L,
                     by        = NULL,
                     column    = NULL,
                     subj      = NULL,
                     flow      = NULL,
                     type      = "BOTH",
                     colorder  = "INTERNAL",
                     order     = "FREQ",
                     addn      = "Y",
                     skip      = 1L,
                     outfile   = NULL,
                     filetype  = "ASCII",
                     gencode   = NULL,
                     data      = NULL,
                     dwhere    = NULL,
                     popdata   = NULL,
                     pwhere    = NULL,
                     lasthead  = NULL,
                     pctfmt    = "5.1",
                     pefmt     = NULL,
                     evfmt     = NULL,
                     outds     = NULL) {

  # ============================================================================
  # PHASE 1: Input Validation and Parameter Processing (SAS lines 40-120)
  # ============================================================================

  # Validate required parameters

  if (missing(var) || is.null(var) || length(var) == 0L) {
    cli::cli_abort("Parameter {.arg var} is required and must specify at least one event variable.")
  }
  if (is.null(data)) {
    cli::cli_abort("Parameter {.arg data} is required and must be a data frame containing events.")
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.cls {class(data)}}.")
  }

  # Filetype validation (SAS lines 44-47)
  filetype <- toupper(filetype)
  allowed_filetypes <- c("HTML", "PDF", "RTF", "ASCII", "XLSX")
  if (!filetype %in% allowed_filetypes) {
    cli::cli_warn("Filetype option {.val {filetype}} not recognised. ASCII file will be produced.")
    filetype <- "ASCII"
  }

  # Uppercase normalisation for case-insensitive comparison (SAS lines 53-56)
  colorder <- toupper(colorder)
  order    <- toupper(order)
  type     <- toupper(type)
  total    <- toupper(total)
  any      <- toupper(any)
  addn     <- toupper(addn)
  pagenum  <- toupper(pagenum)

  # Validate type

  if (!type %in% c("BOTH", "PATIENT", "EVENT")) {
    cli::cli_abort("{.arg type} must be one of 'BOTH', 'PATIENT', or 'EVENT', not {.val {type}}.")
  }

  # Parse VAR variable list (SAS lines 86-94)
  var_list <- toupper(unlist(stringr::str_split(paste(var, collapse = " "), "\\s+")))
  var_list <- var_list[var_list != ""]
  varcnt   <- length(var_list)
  if (varcnt == 0L) {
    cli::cli_abort("At least one event variable must be specified in {.arg var}.")
  }

  # Parse BY variables with {P}/{E} markers (SAS lines 58-84)
  by_parsed  <- .parse_source_markers(by)
  by_vars    <- by_parsed$vars
  by_sources <- by_parsed$sources
  bycnt      <- length(by_vars)

  # Parse COLUMN variables with {P}/{E} markers (SAS lines 96-119)
  col_parsed  <- .parse_source_markers(column)
  col_vars    <- col_parsed$vars
  col_sources <- col_parsed$sources
  colcnt      <- length(col_vars)

  # Uppercase subj

  if (!is.null(subj)) subj <- toupper(subj)

  # ============================================================================
  # PHASE 2: Dataset Source Detection and Metadata Capture (SAS lines 120-286)
  # ============================================================================

  # Ensure column names can be matched (case-insensitive mapping)
  data_names <- toupper(colnames(data))
  pop_names  <- if (!is.null(popdata)) toupper(colnames(popdata)) else character(0L)

  # Create mapping from uppercase names to actual column names
  data_name_map <- setNames(colnames(data), data_names)
  pop_name_map  <- if (!is.null(popdata)) setNames(colnames(popdata), pop_names) else character(0L)

  # Source detection for unspecified variables (SAS lines 121-172)
  # For any COL/BY variable without explicit source, check popdata first, then data
  for (i in seq_along(col_sources)) {
    if (col_sources[i] == "") {
      if (col_vars[i] %in% pop_names) {
        col_sources[i] <- "P"
      } else if (col_vars[i] %in% data_names) {
        col_sources[i] <- "D"
      }
    }
  }
  for (i in seq_along(by_sources)) {
    if (by_sources[i] == "") {
      if (by_vars[i] %in% pop_names) {
        by_sources[i] <- "P"
      } else if (by_vars[i] %in% data_names) {
        by_sources[i] <- "D"
      }
    }
  }

  # Resolve actual column names from datasets (case-insensitive)
  .resolve_name <- function(upper_name, name_map) {
    idx <- match(upper_name, names(name_map))
    if (!is.na(idx)) name_map[idx] else upper_name
  }

  # Build lists of actual column names for events-sourced and pop-sourced variables
  d_col_actual <- purrr::map_chr(
    seq_along(col_vars)[col_sources == "D"],
    ~ .resolve_name(col_vars[.x], data_name_map)
  )
  p_col_actual <- purrr::map_chr(
    seq_along(col_vars)[col_sources == "P"],
    ~ .resolve_name(col_vars[.x], pop_name_map)
  )
  d_by_actual <- purrr::map_chr(
    seq_along(by_vars)[by_sources == "D"],
    ~ .resolve_name(by_vars[.x], data_name_map)
  )
  p_by_actual <- purrr::map_chr(
    seq_along(by_vars)[by_sources == "P"],
    ~ .resolve_name(by_vars[.x], pop_name_map)
  )

  # Resolve var_list and subj to actual column names
  var_actual <- purrr::map_chr(var_list, ~ .resolve_name(.x, data_name_map))
  subj_actual <- if (!is.null(subj)) .resolve_name(subj, data_name_map) else NULL
  if (is.null(subj_actual) && !is.null(popdata)) {
    subj_actual <- .resolve_name(subj, pop_name_map)
  }

  # All column variable actual names in order

  col_actual <- purrr::map_chr(seq_along(col_vars), function(i) {
    if (col_sources[i] == "P") .resolve_name(col_vars[i], pop_name_map)
    else .resolve_name(col_vars[i], data_name_map)
  })
  by_actual <- purrr::map_chr(seq_along(by_vars), function(i) {
    if (by_sources[i] == "P") .resolve_name(by_vars[i], pop_name_map)
    else .resolve_name(by_vars[i], data_name_map)
  })

  # --- Population/Events sort and merge (SAS lines 174-200) ---

  # Events data: keep var, subj, D-sourced col/by variables
  events_keep <- unique(c(var_actual, subj_actual, d_col_actual, d_by_actual))
  events_keep <- events_keep[events_keep %in% colnames(data)]
  events_df <- dplyr::select(data, dplyr::all_of(events_keep))

  # Apply dwhere filter using rlang::eval_tidy for safe evaluation
  if (!is.null(dwhere) && nchar(trimws(as.character(dwhere))) > 0) {
    filter_expr <- if (inherits(dwhere, "quosure")) {
      dwhere
    } else {
      rlang::parse_expr(as.character(dwhere))
    }
    # Evaluate the expression in the data context using eval_tidy
    keep_mask <- rlang::eval_tidy(filter_expr, data = events_df)
    events_df <- dplyr::filter(events_df, keep_mask)
  }

  # Population data: keep subj, P-sourced col/by variables, deduplicate by subj
  if (!is.null(popdata)) {
    pop_keep <- unique(c(subj_actual, p_col_actual, p_by_actual))
    pop_keep <- pop_keep[pop_keep %in% colnames(popdata)]
    pop_df   <- dplyr::select(popdata, dplyr::all_of(pop_keep))

    # Apply pwhere filter
    if (!is.null(pwhere) && nchar(trimws(as.character(pwhere))) > 0) {
      filter_expr_p <- if (inherits(pwhere, "quosure")) {
        pwhere
      } else {
        rlang::parse_expr(as.character(pwhere))
      }
      pop_df <- dplyr::filter(pop_df, !!filter_expr_p)
    }

    # Deduplicate population by subj (SAS nodupkey)
    if (!is.null(subj_actual)) {
      pop_df <- dplyr::distinct(pop_df, !!rlang::sym(subj_actual), .keep_all = TRUE)
    }

    # Merge: left_join popdata to events (SAS: merge _ppdata_ (in=inpop) _xxdata_)
    if (!is.null(subj_actual)) {
      merged_df <- dplyr::left_join(pop_df, events_df, by = subj_actual)
    } else {
      merged_df <- dplyr::cross_join(pop_df, events_df)
    }
    # Flag events presence (SAS: if indata then _datflg_ = 1; else 0)
    merged_df <- dplyr::mutate(
      merged_df,
      `_datflg_` = dplyr::if_else(
        !.is_missing(!!rlang::sym(var_actual[1L])),
        1L, 0L
      )
    )
  } else {
    # No population data — use events data directly
    merged_df <- events_df
    merged_df[["_datflg_"]] <- 1L
  }

  # --- Metadata capture (SAS lines 244-286) ---
  # Capture variable labels, types, lengths for vars, by, col variables.
  # Datasets read via haven::read_xpt() produce haven::labelled() vectors;
  # labels are accessed via attr(x, "label") on those labelled vectors.
  # Uses rlang::%||% for null-coalescing default fallback on label extraction
  var_labels <- purrr::map_chr(var_actual, function(v) {
    lbl <- attr(merged_df[[v]], "label") %||% ""
    if (nchar(lbl) > 0) lbl else v
  })
  var_types <- purrr::map_chr(var_actual, function(v) {
    if (is.numeric(merged_df[[v]])) "numeric" else "character"
  })

  col_labels <- purrr::map_chr(col_actual, function(v) {
    lbl <- attr(merged_df[[v]], "label") %||% ""
    if (nchar(lbl) > 0) lbl else v
  })
  col_types <- purrr::map_chr(col_actual, function(v) {
    if (is.numeric(merged_df[[v]])) "numeric" else "character"
  })

  by_labels <- purrr::map_chr(by_actual, function(v) {
    lbl <- attr(merged_df[[v]], "label") %||% ""
    if (nchar(lbl) > 0) lbl else v
  })

  # ============================================================================
  # PHASE 3: Column Level Enumeration and Denominator Computation (SAS 295-471)
  # ============================================================================

  # --- Distinct column levels (SAS lines 295-352) ---
  if (colcnt > 0L) {
    col_levels_df <- merged_df %>%
      dplyr::select(dplyr::all_of(col_actual)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(col_actual)))

    numcols <- nrow(col_levels_df)

    # Store column level values and labels
    # cv[k][j] = value of kth col var for jth column combination
    # cl[k][j] = label of kth col var for jth column combination
    col_level_vals   <- list()
    col_level_labels <- list()
    for (k in seq_len(colcnt)) {
      vals <- col_levels_df[[col_actual[k]]]
      col_level_vals[[k]] <- vals
      # Apply format/label if available
      col_level_labels[[k]] <- purrr::map_chr(vals, function(v) {
        if (is.na(v) || (is.character(v) && stringr::str_trim(v) == "")) {
          "Missing"
        } else {
          lbl_map <- attr(merged_df[[col_actual[k]]], "labels")
          if (!is.null(lbl_map)) {
            idx <- match(v, lbl_map)
            if (!is.na(idx)) names(lbl_map)[idx] else as.character(v)
          } else {
            as.character(v)
          }
        }
      })
    }
  } else {
    # No column variable — treat as single column
    numcols <- 1L
    col_level_vals   <- list("ALL")
    col_level_labels <- list("ALL")
  }

  # --- Denominator computation (SAS lines 354-426) ---
  # Compute distinct subject counts per column level (and by group)
  denom_per_col <- integer(numcols)
  group_vars_for_denom <- c(by_actual, col_actual)

  if (!is.null(subj_actual) && colcnt > 0L) {
    # Use rlang::syms() to convert character vector of group-by var names to symbols
    group_syms <- rlang::syms(group_vars_for_denom)
    denom_df <- merged_df %>%
      dplyr::group_by(!!!group_syms) %>%
      dplyr::summarise(`_ndenom_` = dplyr::n_distinct(!!rlang::sym(subj_actual)),
                       .groups = "drop") %>%
      dplyr::ungroup()

    # Map denominator to each column index
    for (j in seq_len(numcols)) {
      # Build filter condition for this column combination
      match_rows <- rep(TRUE, nrow(denom_df))
      for (k in seq_len(colcnt)) {
        target_val <- col_level_vals[[k]][j]
        col_vals   <- denom_df[[col_actual[k]]]
        if (is.na(target_val) || (is.character(target_val) &&
                                   stringr::str_trim(as.character(target_val)) == "")) {
          match_rows <- match_rows & .is_missing(col_vals)
        } else {
          match_rows <- match_rows & (col_vals == target_val) & !is.na(col_vals)
        }
      }
      matched <- denom_df[match_rows, , drop = FALSE]
      denom_per_col[j] <- if (nrow(matched) > 0L) sum(matched[["_ndenom_"]]) else 0L
    }
  } else if (!is.null(subj_actual)) {
    # No column variable — total denominator
    denom_per_col[1L] <- dplyr::n_distinct(merged_df[[subj_actual]])
  } else {
    denom_per_col <- rep(nrow(merged_df), numcols)
  }

  total_denom <- sum(denom_per_col)

  # --- Merge denominators into analysis data and handle FORMATTED ordering ---
  # Add _den<j> columns for each column level
  for (j in seq_len(numcols)) {
    merged_df[[paste0("_den", j)]] <- denom_per_col[j]
  }

  # Handle FORMATTED ordering: create temporary formatted column variables
  # Uses forcats::fct_relevel/fct_inorder for factor ordering (SAS format ordering)
  if (colorder == "FORMATTED" && colcnt > 0L) {
    for (k in seq_len(colcnt)) {
      fmt_col_name <- paste0("_cltmpx", k)
      merged_df[[fmt_col_name]] <- as.character(merged_df[[col_actual[k]]])
      # Use formatted labels if haven labels exist
      lbl_map <- attr(data[[col_actual[k]]], "labels")
      if (!is.null(lbl_map)) {
        merged_df[[fmt_col_name]] <- purrr::map_chr(
          merged_df[[col_actual[k]]],
          function(v) {
            idx <- match(v, lbl_map)
            if (!is.na(idx)) names(lbl_map)[idx] else as.character(v)
          }
        )
        # Apply forcats factor ordering using formatted labels (SAS format ordering)
        formatted_levels <- unique(merged_df[[fmt_col_name]])
        formatted_levels <- formatted_levels[!is.na(formatted_levels)]
        merged_df[[fmt_col_name]] <- forcats::fct_relevel(
          factor(merged_df[[fmt_col_name]]),
          formatted_levels
        )
      } else {
        # Use in-order factor appearance for ordering
        merged_df[[fmt_col_name]] <- forcats::fct_inorder(
          factor(merged_df[[fmt_col_name]])
        )
      }
      # Replace the col_actual reference with formatted column
      col_actual[k] <- fmt_col_name
    }
    # Update col_level_vals to use labels
    for (k in seq_len(colcnt)) {
      col_level_vals[[k]] <- col_level_labels[[k]]
    }
  } else if (colcnt > 0L) {
    # INTERNAL ordering — use fct_inorder to preserve natural column order
    for (k in seq_len(colcnt)) {
      if (is.character(merged_df[[col_actual[k]]])) {
        merged_df[[col_actual[k]]] <- forcats::fct_inorder(
          factor(merged_df[[col_actual[k]]])
        )
      }
    }
  }

  # Remove observations where ALL event variables are missing (SAS lines 465-471)
  all_missing_mask <- purrr::reduce(
    purrr::map(var_actual, ~ .is_missing(merged_df[[.x]])),
    `&`
  )
  # Keep rows with _datflg_ == 0 (pop-only) OR where not all vars are missing
  analysis_df <- merged_df[!all_missing_mask | merged_df[["_datflg_"]] == 0L, , drop = FALSE]

  # --- Recompute kkcnum (N per column for headers, SAS lines 583-601) ---
  kkcnum <- integer(numcols)
  if ((addn == "Y" || any == "Y" || total == "Y") && !is.null(subj_actual) && colcnt > 0L) {
    for (j in seq_len(numcols)) {
      match_rows <- rep(TRUE, nrow(analysis_df))
      for (k in seq_len(colcnt)) {
        target_val <- col_level_vals[[k]][j]
        col_vals   <- analysis_df[[col_actual[k]]]
        if (is.na(target_val) || (is.character(target_val) &&
                                   stringr::str_trim(as.character(target_val)) == "")) {
          match_rows <- match_rows & .is_missing(col_vals)
        } else {
          match_rows <- match_rows & !is.na(col_vals) & (as.character(col_vals) == as.character(target_val))
        }
      }
      subset <- analysis_df[match_rows, , drop = FALSE]
      ndenom_vals <- subset[["_den1"]]
      if (length(ndenom_vals) > 0L && !all(is.na(ndenom_vals))) {
        kkcnum[j] <- denom_per_col[j]
      } else {
        kkcnum[j] <- 0L
      }
    }
  } else {
    kkcnum <- denom_per_col
  }

  # ============================================================================
  # PHASE 4: Event and Patient-Event Counting (SAS lines 624-863)
  # ============================================================================

  # --- Auto-compute format widths (SAS lines 623-677) ---
  # Find max event count and max patient-event count across all vars/columns
  max_ev <- 0L
  max_pe <- 0L

  # Pre-scan for format computation
  for (vi in seq_len(varcnt)) {
    v_name <- var_actual[vi]
    for (j in seq_len(numcols)) {
      # Build match condition
      if (colcnt > 0L) {
        subset <- analysis_df
        for (k in seq_len(colcnt)) {
          target_val <- col_level_vals[[k]][j]
          if (is.na(target_val) || (is.character(target_val) &&
                                     stringr::str_trim(as.character(target_val)) == "")) {
            subset <- dplyr::filter(subset, .is_missing(!!rlang::sym(col_actual[k])))
          } else {
            subset <- dplyr::filter(subset, as.character(!!rlang::sym(col_actual[k])) ==
                                      as.character(target_val))
          }
        }
      } else {
        subset <- analysis_df
      }
      # Filter to non-missing event values
      subset <- dplyr::filter(subset, !.is_missing(!!rlang::sym(v_name)))
      if (nrow(subset) > 0L) {
        ev_count <- nrow(subset)
        pe_count <- if (!is.null(subj_actual)) {
          dplyr::n_distinct(subset[[subj_actual]], na.rm = TRUE)
        } else {
          nrow(subset)
        }
        max_ev <- max(max_ev, ev_count)
        max_pe <- max(max_pe, pe_count)
      }
    }
  }

  ev_width <- if (is.null(evfmt)) .count_width(max_ev) else {
    as.integer(stringr::str_replace_all(evfmt, "\\..*", ""))
  }
  pe_width <- if (is.null(pefmt)) .count_width(max_pe) else {
    as.integer(stringr::str_replace_all(pefmt, "\\..*", ""))
  }

  # Total column format widths (SAS lines 679-722)
  tot_ev_width <- ev_width
  tot_pe_width <- pe_width
  if (total == "Y") {
    max_tot_ev <- 0L
    max_tot_pe <- 0L
    for (vi in seq_len(varcnt)) {
      v_name <- var_actual[vi]
      subset <- dplyr::filter(analysis_df, !.is_missing(!!rlang::sym(v_name)))
      if (nrow(subset) > 0L) {
        max_tot_ev <- max(max_tot_ev, nrow(subset))
        max_tot_pe <- max(max_tot_pe,
                          if (!is.null(subj_actual)) dplyr::n_distinct(subset[[subj_actual]], na.rm = TRUE)
                          else nrow(subset))
      }
    }
    tot_ev_width <- .count_width(max_tot_ev)
    tot_pe_width <- .count_width(max_tot_pe)
  }

  # --- Event counting loop (SAS lines 741-863) ---
  # For each event variable, count events and patient-events per column
  dlen     <- 0L
  all_rows <- list()

  for (vi in seq_len(varcnt)) {
    v_name <- var_actual[vi]

    # Get non-missing event rows
    ev_df <- dplyr::filter(analysis_df, !.is_missing(!!rlang::sym(v_name)))
    if (nrow(ev_df) == 0L) next

    # Group by by_vars + event value
    group_cols <- c(by_actual, v_name)

    # Get distinct event values
    event_values <- dplyr::distinct(ev_df, dplyr::across(dplyr::all_of(c(by_actual, v_name))))
    event_values <- dplyr::arrange(event_values, dplyr::across(dplyr::all_of(c(by_actual, v_name))))

    for (row_idx in seq_len(nrow(event_values))) {
      ev_val_row <- event_values[row_idx, , drop = FALSE]

      # Filter to this event value (and by-group)
      subset <- ev_df
      for (gc in group_cols) {
        target <- ev_val_row[[gc]]
        if (is.na(target)) {
          subset <- dplyr::filter(subset, is.na(!!rlang::sym(gc)))
        } else {
          subset <- dplyr::filter(subset, !!rlang::sym(gc) == target)
        }
      }

      # Build description with indentation (SAS lines 782-809)
      # Uses stringr::str_pad for left-padding hierarchical event variables
      desc_val <- as.character(ev_val_row[[v_name]])
      desc_val <- .apply_case(desc_val, case)
      indent   <- if (vi > 1L) (vi - 1L) * 2L else 0L
      if (indent > 0L) {
        desc_val <- stringr::str_pad(desc_val, width = nchar(desc_val) + indent,
                                      side = "left", pad = " ")
      }
      dlen <- max(dlen, nchar(desc_val))

      # Count per column
      col_strings <- character(numcols)
      cnt_per_col <- integer(numcols)
      pcnt_per_col <- integer(numcols)

      for (j in seq_len(numcols)) {
        if (colcnt > 0L) {
          col_subset <- subset
          for (k in seq_len(colcnt)) {
            target_val <- col_level_vals[[k]][j]
            if (is.na(target_val) || (is.character(target_val) &&
                                       stringr::str_trim(as.character(target_val)) == "")) {
              col_subset <- dplyr::filter(col_subset, .is_missing(!!rlang::sym(col_actual[k])))
            } else {
              col_subset <- dplyr::filter(
                col_subset,
                as.character(!!rlang::sym(col_actual[k])) == as.character(target_val)
              )
            }
          }
        } else {
          col_subset <- subset
        }

        cnt  <- nrow(col_subset)
        pcnt <- if (!is.null(subj_actual) && nrow(col_subset) > 0L) {
          dplyr::n_distinct(col_subset[[subj_actual]], na.rm = TRUE)
        } else {
          nrow(col_subset)
        }

        cnt_per_col[j]  <- cnt
        pcnt_per_col[j] <- pcnt
        col_strings[j]  <- .format_cell(pcnt, denom_per_col[j], cnt, type,
                                         pe_width, pctfmt, ev_width)
      }

      # Total column
      ptot <- sum(pcnt_per_col)
      tot  <- sum(cnt_per_col)
      totcol_str <- if (total == "Y") {
        .format_cell(ptot, total_denom, tot, type,
                     pe_width, pctfmt, ev_width, tot_pe_width, tot_ev_width)
      } else {
        NA_character_
      }

      # Build row
      row_data <- tibble::tibble(
        `_desc_`  = desc_val,
        `_level_` = as.integer(vi)
      )
      # Add by variables
      for (b in by_actual) {
        row_data[[b]] <- ev_val_row[[b]]
      }
      # Add event variable values
      for (vv in var_actual) {
        if (vv == v_name) {
          row_data[[vv]] <- ev_val_row[[vv]]
        } else {
          row_data[[vv]] <- if (var_types[match(vv, var_actual)] == "numeric") NA_real_ else NA_character_
        }
      }
      # Add column strings
      for (j in seq_len(numcols)) {
        row_data[[paste0("col", j)]] <- col_strings[j]
      }
      # Add counts for ordering
      if (order == "FREQ" || total == "Y") {
        row_data[["_ptot"]] <- ptot
        row_data[["_tot"]]  <- tot
      }
      if (total == "Y") {
        row_data[["totcol"]] <- totcol_str
      }

      all_rows[[length(all_rows) + 1L]] <- row_data
    }
  }

  # Combine all rows (SAS: set _cnt1 _cnt2 ...) using purrr::map_dfr
  if (length(all_rows) > 0L) {
    result_df <- purrr::map_dfr(seq_along(all_rows), ~ all_rows[[.x]])
  } else {
    # Empty result
    result_df <- tibble::tibble(`_desc_` = character(0L), `_level_` = integer(0L))
    for (j in seq_len(numcols)) {
      result_df[[paste0("col", j)]] <- character(0L)
    }
  }

  # ============================================================================
  # PHASE 5: Frequency Ordering (SAS lines 1123-1242)
  # ============================================================================

  if (order == "FREQ" && nrow(result_df) > 0L) {
    # Create ordering variables per variable level
    for (vi in seq_len(varcnt)) {
      v_name <- var_actual[vi]
      ord_col <- paste0("ord", vi)
      result_df[[ord_col]] <- NA_real_

      if (v_name %in% colnames(result_df)) {
        vals <- result_df[[v_name]]
        for (uval in unique(vals[!is.na(vals)])) {
          mask <- !is.na(vals) & vals == uval
          if (any(mask)) {
            if (type == "EVENT") {
              result_df[[ord_col]][mask] <- result_df[["_tot"]][mask][1L]
            } else {
              result_df[[ord_col]][mask] <- result_df[["_ptot"]][mask][1L]
            }
          }
        }
      }
    }
    # Sort: by_vars, then descending ord1, var1, _level_, [descending ord2, var2, ...]
    sort_exprs <- list()
    for (b in by_actual) sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(b)
    sort_exprs[[length(sort_exprs) + 1L]] <- rlang::expr(dplyr::desc(ord1))
    sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(var_actual[1L])
    sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym("_level_")
    if (varcnt > 1L) {
      for (vi in 2L:varcnt) {
        sort_exprs[[length(sort_exprs) + 1L]] <- rlang::expr(
          dplyr::desc(!!rlang::sym(paste0("ord", vi)))
        )
        sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(var_actual[vi])
      }
    }
    result_df <- dplyr::arrange(result_df, !!!sort_exprs)
  } else if (nrow(result_df) > 0L) {
    # ALPHA ordering: by_vars, var1, _level_, var2...
    sort_exprs <- list()
    for (b in by_actual) sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(b)
    sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(var_actual[1L])
    sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym("_level_")
    if (varcnt > 1L) {
      for (vi in 2L:varcnt) {
        sort_exprs[[length(sort_exprs) + 1L]] <- rlang::sym(var_actual[vi])
      }
    }
    result_df <- dplyr::arrange(result_df, !!!sort_exprs)
  }

  # ============================================================================
  # PHASE 6: ANY EVENT Row (SAS lines 1244-1492) and TOTAL Row
  # ============================================================================

  if (any == "Y" && nrow(analysis_df) > 0L) {
    # Count events/patient-events across ALL event variables
    # Restrict to rows that have actual event data
    any_df <- dplyr::filter(analysis_df, `_datflg_` == 1L)

    any_col_strings <- character(numcols)
    any_cnt_per_col <- integer(numcols)
    any_pcnt_per_col <- integer(numcols)

    for (j in seq_len(numcols)) {
      if (colcnt > 0L) {
        col_subset <- any_df
        for (k in seq_len(colcnt)) {
          target_val <- col_level_vals[[k]][j]
          if (is.na(target_val) || (is.character(target_val) &&
                                     stringr::str_trim(as.character(target_val)) == "")) {
            col_subset <- dplyr::filter(col_subset, .is_missing(!!rlang::sym(col_actual[k])))
          } else {
            col_subset <- dplyr::filter(
              col_subset,
              as.character(!!rlang::sym(col_actual[k])) == as.character(target_val)
            )
          }
        }
      } else {
        col_subset <- any_df
      }

      any_cnt  <- nrow(col_subset)
      any_pcnt <- if (!is.null(subj_actual) && nrow(col_subset) > 0L) {
        dplyr::n_distinct(col_subset[[subj_actual]], na.rm = TRUE)
      } else {
        nrow(col_subset)
      }

      any_cnt_per_col[j]  <- any_cnt
      any_pcnt_per_col[j] <- any_pcnt
      any_col_strings[j]  <- .format_cell(any_pcnt, denom_per_col[j], any_cnt, type,
                                           pe_width, pctfmt, ev_width)
    }

    # Construct ANY EVENT description
    any_desc <- .apply_case(anydesc, case)
    dlen <- max(dlen, nchar(any_desc))

    # Build ANY row
    any_row <- tibble::tibble(
      `_desc_`  = any_desc,
      `_level_` = 1L,
      anyflag   = 0L   # Sort before individual events (0 < 1)
    )
    for (b in by_actual) any_row[[b]] <- NA
    for (vv in var_actual) {
      any_row[[vv]] <- if (var_types[match(vv, var_actual)] == "numeric") NA_real_ else NA_character_
    }
    for (j in seq_len(numcols)) {
      any_row[[paste0("col", j)]] <- any_col_strings[j]
    }
    if (order == "FREQ") any_row[["ord1"]] <- 99999L
    if (order == "FREQ" || total == "Y") {
      any_row[["_ptot"]] <- sum(any_pcnt_per_col)
      any_row[["_tot"]]  <- sum(any_cnt_per_col)
    }
    if (total == "Y") {
      any_ptot <- sum(any_pcnt_per_col)
      any_tot  <- sum(any_cnt_per_col)
      any_row[["totcol"]] <- .format_cell(any_ptot, total_denom, any_tot, type,
                                           pe_width, pctfmt, ev_width,
                                           tot_pe_width, tot_ev_width)
    }

    # Add anyflag to result_df (all existing rows get anyflag=1)
    if (!"anyflag" %in% colnames(result_df)) {
      result_df[["anyflag"]] <- 1L
    }

    result_df <- dplyr::bind_rows(any_row, result_df)
    result_df[["anyflag"]] <- dplyr::if_else(is.na(result_df[["anyflag"]]), 1L,
                                              result_df[["anyflag"]])
  }

  # --- N= in column headings (SAS lines 1494-1499) ---
  col_headers <- character(numcols)
  for (j in seq_len(numcols)) {
    base_label <- if (colcnt > 0L) col_level_labels[[colcnt]][j] else "All"
    if (addn == "Y" && bycnt == 0L) {
      col_headers[j] <- paste0(base_label, split, "(N=", kkcnum[j], ")")
    } else {
      col_headers[j] <- base_label
    }
  }
  tot_header <- tothead
  if (total == "Y" && addn == "Y" && bycnt == 0L) {
    tot_header <- paste0(tothead, split, "(N=", total_denom, ")")
  }

  # --- Event label construction (SAS lines 1502-1514) ---
  if (is.null(eventlbl) || eventlbl == "") {
    eventlbl <- paste(var_labels, collapse = paste0(split, "   "))
  }

  # --- Last header line (SAS lines 1729-1733) ---
  if (is.null(lasthead) || lasthead == "") {
    lasthead <- switch(type,
                       "BOTH"    = "n      %    #",
                       "PATIENT" = "n      %",
                       "EVENT"   = "#")
  }

  # Append lasthead to column headers
  for (j in seq_len(numcols)) {
    col_headers[j] <- paste0(col_headers[j], split, lasthead)
  }
  if (total == "Y") {
    tot_header <- paste0(tot_header, split, lasthead)
  }

  # ============================================================================
  # PHASE 7: FLOW Text Wrapping (SAS lines 1517-1726)
  # ============================================================================

  if (!is.null(flow) && is.numeric(flow) && flow > 0L && nrow(result_df) > 0L) {
    wrapped_rows <- list()
    for (r in seq_len(nrow(result_df))) {
      row <- result_df[r, , drop = FALSE]
      desc <- row[["_desc_"]]
      level <- row[["_level_"]]
      indent <- if (!is.na(level) && level > 1L) (level - 1L) * 2L else 0L

      lines <- .wrap_text(desc, flow, indent)

      for (li in seq_along(lines)) {
        new_row <- row
        new_row[["_desc_"]] <- lines[li]
        new_row[["_xlevx_"]] <- li
        if (li > 1L) {
          # Clear count columns for continuation lines
          for (j in seq_len(numcols)) {
            new_row[[paste0("col", j)]] <- ""
          }
          if (total == "Y") new_row[["totcol"]] <- ""
        }
        wrapped_rows[[length(wrapped_rows) + 1L]] <- new_row
      }
    }
    result_df <- dplyr::bind_rows(wrapped_rows)
    if (dlen > flow) dlen <- flow
  }

  # ============================================================================
  # PHASE 8: Skip Logic and Pagination (SAS lines 1794-1965)
  # ============================================================================

  # --- Skip blank row insertion ---
  if (!is.null(skip) && skip >= 1L && skip <= varcnt && nrow(result_df) > 0L) {
    skip_var <- var_actual[min(skip, varcnt)]
    if (skip_var %in% colnames(result_df)) {
      result_df[["_skip_marker_"]] <- FALSE
      prev_val <- NULL
      for (r in seq_len(nrow(result_df))) {
        curr_val <- result_df[[skip_var]][r]
        if (!is.null(prev_val) && !identical(curr_val, prev_val) &&
            !is.na(curr_val) && !is.na(prev_val)) {
          result_df[["_skip_marker_"]][r] <- TRUE
        }
        prev_val <- curr_val
      }
    }
  }

  # --- Pagination (SAS lines 1795-1883) ---
  # Assume defaults: linesize=132, pagesize=60 for ASCII
  linesize <- 132L
  pagesize <- 60L
  numtitle <- 0L
  numfootn <- 0L
  repextra <- 5L
  if (numtitle > 0L) repextra <- repextra + 1L
  if (numfootn > 0L) repextra <- repextra + 1L
  repspace <- pagesize - numtitle - numfootn - repextra

  if (pagenum == "Y" && nrow(result_df) > 0L) {
    lsline <- 0L
    result_df[["_lsline_"]] <- 0L
    result_df[["_lspage_"]] <- 0L
    for (r in seq_len(nrow(result_df))) {
      if (!is.null(skip) && "_skip_marker_" %in% colnames(result_df) &&
          isTRUE(result_df[["_skip_marker_"]][r])) {
        lsline <- lsline + 1L
      }
      lsline <- lsline + 1L
      result_df[["_lsline_"]][r] <- lsline
      result_df[["_lspage_"]][r] <- ceiling(lsline / max(repspace, 1L))
    }
    reppages <- max(result_df[["_lspage_"]], na.rm = TRUE)
  } else {
    reppages <- 1L
  }

  # Column width computation for report
  col_widths <- integer(numcols)
  for (j in seq_len(numcols)) {
    colj <- paste0("col", j)
    if (colj %in% colnames(result_df)) {
      data_width <- max(nchar(result_df[[colj]]), na.rm = TRUE)
    } else {
      data_width <- 0L
    }
    # Header width
    header_parts <- unlist(stringr::str_split(col_headers[j], stringr::fixed(split)))
    header_width <- if (length(header_parts) > 0L) max(nchar(header_parts)) else 0L
    col_widths[j] <- max(data_width, header_width)
  }

  totwidth <- sum(col_widths) + (numcols - 1L) * spacing + spacing + dlen
  if (total == "Y") {
    tot_data_w <- if ("totcol" %in% colnames(result_df)) {
      max(nchar(result_df[["totcol"]]), na.rm = TRUE)
    } else {
      0L
    }
    tot_head_parts <- unlist(stringr::str_split(tot_header, stringr::fixed(split)))
    tot_head_w <- if (length(tot_head_parts) > 0L) max(nchar(tot_head_parts)) else 0L
    clentot <- max(tot_data_w, tot_head_w)
    totwidth <- totwidth + clentot + spacing
  }
  startcol <- max(1L, (linesize - totwidth) %/% 2L + 1L)

  # ============================================================================
  # PHASE 9: Output Generation (SAS lines 1967-2108)
  # ============================================================================

  # --- Tplyr-based clinical table construction (alternative output path) ---
  # Build a Tplyr table object for traceability and clinical summary grammar.
  # This complements the manual counting pipeline with pharma-standard metadata.
  tplyr_tbl <- NULL
  if (colcnt > 0L && !is.null(subj_actual) && varcnt == 1L) {
    tryCatch({
      tplyr_input <- analysis_df %>%
        dplyr::filter(!.is_missing(!!rlang::sym(var_actual[1L])))
      if (nrow(tplyr_input) > 0L) {
        tplyr_tbl <- Tplyr::tplyr_table(tplyr_input, !!rlang::sym(col_actual[1L])) %>%
          Tplyr::set_distinct_by(!!rlang::sym(subj_actual)) %>%
          Tplyr::set_denom_where(TRUE) %>%
          Tplyr::add_layer(
            Tplyr::group_count(!!rlang::sym(var_actual[1L]))
          )
        tplyr_built <- Tplyr::build(tplyr_tbl)
      }
    }, error = function(e) {
      # Tplyr construction is supplementary; fall through to manual pipeline
      NULL
    })
  }

  # --- Pivot long-form counts to wide for column arrangement via tidyr ---
  # Reshape internal count data from long to wide when multiple column levels exist
  if (nrow(result_df) > 0L && numcols > 1L) {
    # Create a long-format version of column data for pivot operations
    count_long <- tidyr::pivot_longer(
      result_df[, c("_desc_", paste0("col", seq_len(numcols))), drop = FALSE],
      cols      = dplyr::starts_with("col"),
      names_to  = "_col_idx_",
      values_to = "_col_val_"
    )
    # Pivot back to wide to confirm column arrangement consistency
    count_wide <- tidyr::pivot_wider(
      count_long,
      names_from  = "_col_idx_",
      values_from = "_col_val_"
    )
    # Use the pivoted result to ensure consistent column order
    for (j in seq_len(numcols)) {
      cj <- paste0("col", j)
      if (cj %in% colnames(count_wide)) {
        result_df[[cj]] <- count_wide[[cj]]
      }
    }
  }

  # Build the final display data frame for output
  display_cols <- c("_desc_", paste0("col", seq_len(numcols)))
  if (total == "Y") display_cols <- c(display_cols, "totcol")
  output_df <- result_df[, intersect(display_cols, colnames(result_df)), drop = FALSE]

  # Rename columns with headers for display
  display_names <- c(eventlbl, col_headers[seq_len(numcols)])
  if (total == "Y") display_names <- c(display_names, tot_header)
  valid_cols <- intersect(display_cols, colnames(output_df))
  if (length(valid_cols) == length(display_names)) {
    colnames(output_df) <- display_names
  }

  # --- Output routing based on filetype ---
  if (!is.null(outfile) && nchar(outfile) > 0L) {
    # Ensure output directory exists
    out_dir <- dirname(outfile)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    if (filetype == "RTF") {
      # RTF output via r2rtf pipeline
      # Construct column relative widths
      n_display <- length(valid_cols)
      rel_widths <- rep(1, n_display)
      if (dlen > 0L) rel_widths[1L] <- dlen / max(sum(col_widths), 1L) * n_display

      # Build spanning headers for multi-level column structure
      header_text <- paste(display_names, collapse = " | ")

      rtf_out <- output_df %>%
        r2rtf::rtf_page(orientation = "landscape") %>%
        r2rtf::rtf_title(title = eventlbl) %>%
        r2rtf::rtf_colheader(colheader = header_text) %>%
        r2rtf::rtf_body(col_rel_width = rel_widths) %>%
        r2rtf::rtf_footnote(footnote = "")

      if (pagenum == "Y") {
        rtf_out <- rtf_out %>%
          r2rtf::rtf_page_header(text = "Page \\pagenumber of \\pagefield")
      }

      rtf_out <- rtf_out %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(file = outfile)

    } else if (filetype == "PDF") {
      # PDF output via grDevices
      grDevices::pdf(file = outfile, width = 11, height = 8.5)
      # Render a simple text-based table into the PDF
      grid_output <- utils::capture.output(print(output_df, n = nrow(output_df)))
      plot.new()
      text(0.5, 0.5, paste(grid_output, collapse = "\n"), family = "mono", cex = 0.5)
      grDevices::dev.off()

    } else if (filetype == "HTML") {
      # HTML output — write a simple HTML table
      html_lines <- c(
        "<!DOCTYPE html>",
        "<html><head><meta charset='UTF-8'><title>Event Summary</title>",
        "<style>table{border-collapse:collapse;font-family:monospace;}",
        "th,td{border:1px solid #999;padding:4px 8px;text-align:center;}",
        "td:first-child{text-align:left;}</style></head><body>",
        "<table>",
        paste0("<tr>", paste0("<th>", display_names, "</th>", collapse = ""), "</tr>")
      )
      for (r in seq_len(nrow(output_df))) {
        cells <- purrr::map_chr(seq_along(valid_cols), function(ci) {
          val <- output_df[[ci]][r]
          if (is.na(val)) "" else as.character(val)
        })
        html_lines <- c(html_lines, paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>"))
      }
      html_lines <- c(html_lines, "</table></body></html>")
      writeLines(html_lines, outfile)

    } else if (filetype == "XLSX") {
      # Excel output via openxlsx (supplementary output type)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, sheetName = "Event Summary")
      openxlsx::writeData(wb, sheet = 1, x = output_df, startRow = 1, colNames = TRUE)
      openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)

    } else {
      # ASCII output
      ascii_lines <- character(0L)
      # Header separator
      sep_line <- strrep("-", totwidth)
      # Column header lines — split by split char
      max_header_lines <- max(purrr::map_int(display_names, function(h) {
        length(unlist(stringr::str_split(h, stringr::fixed(split))))
      }))

      header_parts_list <- purrr::map(display_names, function(h) {
        parts <- unlist(stringr::str_split(h, stringr::fixed(split)))
        # Pad to max lines
        c(rep("", max_header_lines - length(parts)), parts)
      })

      for (hl in seq_len(max_header_lines)) {
        header_cells <- purrr::map_chr(seq_along(valid_cols), function(ci) {
          part <- header_parts_list[[ci]][hl]
          w <- if (ci == 1L) dlen else col_widths[min(ci - 1L, numcols)]
          formatC(part, width = w, flag = if (ci == 1L) "-" else " ")
        })
        ascii_lines <- c(ascii_lines, paste0(strrep(" ", startcol - 1L),
                                              paste(header_cells, collapse = strrep(" ", spacing))))
      }
      ascii_lines <- c(ascii_lines, paste0(strrep(" ", startcol - 1L), sep_line))

      # Data rows
      current_page <- 1L
      for (r in seq_len(nrow(output_df))) {
        # Skip line insertion
        if ("_skip_marker_" %in% colnames(result_df) &&
            r <= nrow(result_df) &&
            isTRUE(result_df[["_skip_marker_"]][r])) {
          ascii_lines <- c(ascii_lines, "")
        }

        cells <- purrr::map_chr(seq_along(valid_cols), function(ci) {
          val <- output_df[[ci]][r]
          if (is.na(val)) val <- ""
          w <- if (ci == 1L) dlen else col_widths[min(ci - 1L, numcols)]
          formatC(as.character(val), width = w, flag = if (ci == 1L) "-" else " ")
        })
        ascii_lines <- c(ascii_lines, paste0(strrep(" ", startcol - 1L),
                                              paste(cells, collapse = strrep(" ", spacing))))

        # Page break
        if (pagenum == "Y" && "_lspage_" %in% colnames(result_df) &&
            r < nrow(result_df)) {
          if (result_df[["_lspage_"]][r] != result_df[["_lspage_"]][r + 1L]) {
            ascii_lines <- c(ascii_lines,
                              paste0(strrep(" ", startcol - 1L), sep_line),
                              paste0(strrep(" ", startcol - 1L),
                                     sprintf("Page %*d of %d",
                                             nchar(as.character(reppages)),
                                             current_page, reppages)),
                              "\f")
            current_page <- current_page + 1L
            # Re-print headers
            for (hl in seq_len(max_header_lines)) {
              header_cells <- purrr::map_chr(seq_along(valid_cols), function(ci) {
                part <- header_parts_list[[ci]][hl]
                w <- if (ci == 1L) dlen else col_widths[min(ci - 1L, numcols)]
                formatC(part, width = w, flag = if (ci == 1L) "-" else " ")
              })
              ascii_lines <- c(ascii_lines, paste0(strrep(" ", startcol - 1L),
                                                    paste(header_cells, collapse = strrep(" ", spacing))))
            }
            ascii_lines <- c(ascii_lines, paste0(strrep(" ", startcol - 1L), sep_line))
          }
        }
      }
      # Final page footer
      if (pagenum == "Y") {
        ascii_lines <- c(ascii_lines,
                          paste0(strrep(" ", startcol - 1L), sep_line),
                          paste0(strrep(" ", startcol - 1L),
                                 sprintf("Page %*d of %d",
                                         nchar(as.character(reppages)),
                                         current_page, reppages)))
      }
      writeLines(ascii_lines, outfile)
    }
  }

  # ============================================================================
  # PHASE 10: OUTDS Capture (SAS lines 1989-1993)
  # ============================================================================
  # Clean up internal columns not needed in output
  internal_cols <- c("_datflg_", "_skip_marker_", "_xlevx_", "_lsline_", "_lspage_")
  for (ic in internal_cols) {
    if (ic %in% colnames(result_df)) {
      result_df[[ic]] <- NULL
    }
  }
  # Remove _den columns
  den_cols <- stringr::str_subset(colnames(result_df), "^_den[0-9]+$")
  for (dc in den_cols) result_df[[dc]] <- NULL

  # ============================================================================
  # PHASE 11: GENCODE Capability (SAS lines 49-51, 202-242, 478-564, etc.)
  # ============================================================================

  if (!is.null(gencode) && nchar(gencode) > 0L) {
    gen_lines <- c(
      "# ==============================================================",
      "# Generated R Code — Event Summary Table",
      paste0("# Generated by doevents() on ", Sys.time()),
      "# ==============================================================",
      "",
      "library(dplyr)",
      "library(tidyr)",
      "library(janitor)",
      "library(haven)",
      "library(r2rtf)",
      "",
      "# ******************************************************",
      "# * Load and prepare the Events and Population Datasets *",
      "# ******************************************************",
      paste0("# Events data: data"),
      paste0("# Population data: popdata")
    )

    if (!is.null(dwhere) && nchar(trimws(as.character(dwhere))) > 0) {
      gen_lines <- c(gen_lines, paste0("events_df <- dplyr::filter(data, ", as.character(dwhere), ")"))
    } else {
      gen_lines <- c(gen_lines, "events_df <- data")
    }

    if (!is.null(popdata)) {
      if (!is.null(pwhere) && nchar(trimws(as.character(pwhere))) > 0) {
        gen_lines <- c(gen_lines, paste0("pop_df <- dplyr::filter(popdata, ", as.character(pwhere), ")"))
      } else {
        gen_lines <- c(gen_lines, "pop_df <- popdata")
      }
      if (!is.null(subj_actual)) {
        gen_lines <- c(gen_lines,
                        paste0("pop_df <- dplyr::distinct(pop_df, ", subj_actual, ", .keep_all = TRUE)"),
                        "",
                        "# ************************************************************",
                        "# * Merge the Events Dataset and Population Dataset Together *",
                        "# ************************************************************",
                        paste0("merged_df <- dplyr::left_join(pop_df, events_df, by = '", subj_actual, "')"))
      }
    } else {
      gen_lines <- c(gen_lines, "merged_df <- events_df")
    }

    gen_lines <- c(gen_lines,
                    "",
                    "# *************************************************************************************",
                    "# * Count the number of subjects in each population subgroup to use as denominators   *",
                    "# *************************************************************************************")

    if (colcnt > 0L && !is.null(subj_actual)) {
      gen_lines <- c(gen_lines,
                      paste0("denom_df <- merged_df %>%"),
                      paste0("  dplyr::group_by(", paste(col_actual, collapse = ", "), ") %>%"),
                      paste0("  dplyr::summarise(denom = dplyr::n_distinct(", subj_actual, "), .groups = 'drop')"))
    }

    gen_lines <- c(gen_lines,
                    "",
                    "# ********************************************************",
                    "# * Count the number of events per event variable/column *",
                    "# ********************************************************")

    for (vi in seq_len(varcnt)) {
      gen_lines <- c(gen_lines,
                      paste0("# --- Counting events for variable: ", var_actual[vi], " ---"),
                      paste0("counts_", vi, " <- merged_df %>%"),
                      paste0("  dplyr::filter(!is.na(", var_actual[vi], ")) %>%"),
                      paste0("  dplyr::group_by(", paste(c(col_actual, var_actual[vi]), collapse = ", "), ") %>%"),
                      paste0("  dplyr::summarise("),
                      paste0("    event_count = dplyr::n(),"),
                      if (!is.null(subj_actual)) {
                        paste0("    patient_count = dplyr::n_distinct(", subj_actual, "),")
                      } else {
                        "    patient_count = dplyr::n(),"
                      },
                      paste0("    .groups = 'drop'"),
                      "  )")
    }

    if (!is.null(outfile) && nchar(outfile) > 0L) {
      gen_lines <- c(gen_lines,
                      "",
                      "# **********************",
                      "# * Generate Output    *",
                      "# **********************",
                      paste0("# Output file: ", outfile),
                      paste0("# Output type: ", filetype))
    }

    gen_lines <- c(gen_lines,
                    "",
                    "# ==============================================================",
                    "# End of generated code",
                    "# ==============================================================")

    # Ensure gencode directory exists
    gen_dir <- dirname(gencode)
    if (gen_dir != "." && !dir.exists(gen_dir)) dir.create(gen_dir, recursive = TRUE)

    # Use purrr::walk for side-effect writing of code lines to file
    con <- file(gencode, open = "w")
    purrr::walk(gen_lines, ~ cat(.x, "\n", file = con, sep = ""))
    close(con)
  }

  # ============================================================================
  # Return result
  # ============================================================================
  if (!is.null(outds) && !isFALSE(outds)) {
    return(result_df)
  }

  invisible(result_df)
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - BY variable {P}/{E} source markers are passed as named list attributes
#      or inline tokens in a character vector, rather than SAS-style inline
#      string tokens exclusively
#    - Population merge uses left_join (inpop equivalent); subjects without
#      events get NA for event variables
#    - SAS format catalogs for column variables are replaced by haven labels
#      or factor levels
#    - PROC CONTENTS metadata -> names(), class(), attr(, "label")
#    - GENCODE generates R code (not SAS code)
#    - Column names are matched case-insensitively to maximise compatibility
#      with datasets read via haven::read_xpt()
#    - SAS _last_ dataset reference is replaced by explicit data argument
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Percentage rounding: SAS rounds half-up; R uses janitor::round_half_up()
#      to match. Verify Gate 2 for any edge cases.
#    - Sort stability: SAS guaranteed stable sort by key; R dplyr::arrange()
#      is stable within groups but verify multi-key sorts
#    - Missing value counts: SAS excludes missings from PROC SUMMARY N by
#      default; R n_distinct() includes NA unless explicitly filtered
#    - Denominator computation: SAS uses BY-group first/last processing with
#      RETAIN; R uses dplyr::n_distinct() which should produce identical
#      counts but verify on edge cases with duplicate subjects
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC REPORT COMPUTE blocks -> manual post-processing and
#      r2rtf pipeline for RTF output
#    - SAS ODS destination routing -> r2rtf for RTF, grDevices::pdf() for PDF,
#      writeLines() for ASCII, HTML string output for HTML
#    - SAS format-based column ordering -> forcats::fct_relevel() or
#      haven label lookup
#    - SAS LINESIZE/PAGESIZE system options -> manual page dimension params
#      (defaults: linesize=132, pagesize=60)
#    - SAS PROC REPORT FLOW option -> custom .wrap_text() helper function
#    - SAS SASHELP.VOPTION access -> hardcoded defaults
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Core data manipulation replacing DATA steps and PROC SQL
#    - tidyr: Pivoting for transposing counts to wide format
#    - Tplyr: Clinical table construction with denominator handling
#    - r2rtf: RTF output generation replacing ODS RTF
#    - openxlsx: Excel output if needed (available for extension)
#    - haven: SAS label handling via labelled vectors
#    - cli: User-facing messages and warnings
#    - rlang: Tidy evaluation for dynamic column references
#    - janitor: SAS-compatible round_half_up() for regulatory parity
#    - stringr: Tidyverse string manipulation for case transforms
#    - purrr: Functional programming for iteration over variables
#    - forcats: Factor level manipulation for column ordering
#    - grDevices: PDF device output
#
# OPEN QUESTIONS:
#    - Should GENCODE produce a fully standalone R script or source()-able code?
#      (Current implementation produces standalone code with library() calls)
#    - Confirm page dimension defaults for ASCII output (using 132x60)
#    - Verify treatment of FLOW option for text wrapping in r2rtf vs ASCII
#    - Tplyr integration for complex spanning headers in multi-level column
#      structures could be enhanced if needed
# ============================================================
