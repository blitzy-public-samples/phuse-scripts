#' Boxplot Block Range Computation for Paginated Boxplots
#'
#' Pagination utility that calculates x-axis CATEGORY ranges for each boxplot
#' page. Keeps all categories within each block together (e.g., all treatments
#' within a visit on the same page) to prevent splitting blocks across pages.
#'
#' Migrated from SAS macro: \code{\%util_boxplot_block_ranges(ds, blockvar=, catvars=, sym=)}
#' Source: whitepapers/utilities/util_boxplot_block_ranges.sas (133 lines)
#'
#' @param df A data frame containing measurement data and block/category
#'   variables. Replaces the SAS \code{DS} positional parameter.
#' @param block_var Single character string naming the block variable (e.g.,
#'   \code{"AVISITN"} for visit number). All categories within each block are
#'   kept together on a single page. Replaces the SAS \code{BLOCKVAR} keyword.
#' @param cat_vars Character vector of category variable names used to identify
#'   and count distinct boxes within each block (e.g., \code{c("TRTPN")} for
#'   treatment number). Replaces the SAS \code{CATVARS} keyword (space-delimited
#'   in SAS).
#' @param max_boxes_per_page Positive integer specifying the maximum number of
#'   boxes allowed per page. Replaces the SAS global symbol
#'   \code{MAX_BOXES_PER_PAGE}, parameterized as a function argument per AAP
#'   section 0.8.1.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{\code{ranges}}{Character vector of range expressions, one per page.
#'       For numeric block variables: \code{"min<=block_var<=max"}.
#'       For character block variables: \code{'"min"<=block_var<="max"'}.}
#'     \item{\code{range_string}}{Single pipe-delimited string joining all
#'       ranges, matching the SAS global symbol value format.}
#'     \item{\code{pages}}{Tibble showing page assignments with columns for the
#'       block variable value, category count, and assigned page number.}
#'   }
#'
#' @examples
#' # Numeric block variable (visit number) with 3 treatments per visit
#' plot_data <- data.frame(
#'   AVISITN = rep(c(0, 4, 8, 12, 16, 20, 24), each = 3),
#'   TRTPN   = rep(1:3, times = 7),
#'   AVAL    = rnorm(21)
#' )
#' result <- util_boxplot_block_ranges(
#'   df                 = plot_data,
#'   block_var          = "AVISITN",
#'   cat_vars           = "TRTPN",
#'   max_boxes_per_page = 12
#' )
#' result$range_string
#' # "0<=AVISITN<=12|16<=AVISITN<=24"
#'
#' @export
util_boxplot_block_ranges <- function(df,
                                      block_var,
                                      cat_vars,
                                      max_boxes_per_page) {


  # ===================================================================

  # Input validation
  # ===================================================================

  if (!is.data.frame(df)) {
    cli::cli_abort(
      "{.arg df} must be a data frame, not {.cls {class(df)}}."
    )
  }

  if (!rlang::is_character(block_var) || length(block_var) != 1L ||
      nchar(block_var) == 0L) {
    cli::cli_abort(
      "{.arg block_var} must be a single non-empty character string."
    )
  }

  if (!rlang::is_character(cat_vars) || length(cat_vars) == 0L ||
      any(nchar(cat_vars) == 0L)) {
    cli::cli_abort(
      "{.arg cat_vars} must be a non-empty character vector of variable names."
    )
  }

  if (!rlang::is_scalar_integerish(max_boxes_per_page) ||
      max_boxes_per_page < 1L) {
    cli::cli_abort(
      "{.arg max_boxes_per_page} must be a positive integer, not {.val {max_boxes_per_page}}."
    )
  }

  # Validate that all required columns exist in df
  all_vars    <- c(block_var, cat_vars)
  missing_vars <- setdiff(all_vars, names(df))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      "Variable{?s} {.var {missing_vars}} not found in {.arg df}."
    )
  }

  # ===================================================================
  # Handle empty data frame edge case
  # ===================================================================

  if (nrow(df) == 0L) {
    cli::cli_inform(
      c("i" = "No observations in data frame. Returning empty ranges.")
    )
    return(
      list(
        ranges       = character(0L),
        range_string = "",
        pages        = dplyr::tibble()
      )
    )
  }

  # ===================================================================
  # Handle missing values in block_var (AAP section 0.7.3)
  #
  # SAS PROC FREQ with /MISSING includes missing values in the count;

  # however, range strings for NA blocks are meaningless for downstream
  # subsetting, so we warn and exclude them.
  # ===================================================================

  n_na_block <- sum(is.na(df[[block_var]]))
  if (n_na_block > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          n_na_block,
          " observation(s) with missing {.var {block_var}} excluded ",
          "from range computation."
        ),
        "i" = "Missing values map to {.val NA} per AAP section 0.7.3."
      )
    )
    df <- dplyr::filter(df, !is.na(.data[[block_var]]))
  }

  # Guard: all rows may have been NA
  if (nrow(df) == 0L) {
    cli::cli_warn(
      "All observations had missing {.var {block_var}}. Returning empty ranges."
    )
    return(
      list(
        ranges       = character(0L),
        range_string = "",
        pages        = dplyr::tibble()
      )
    )
  }

  # ===================================================================
  # Step 1: Get distinct block x category combinations
  # Replaces SAS PROC SORT NODUPKEY (source lines 46-49)
  # ===================================================================

  cats <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(block_var, cat_vars))))

  # ===================================================================
  # Step 2: Count categories within each block
  # Replaces SAS PROC FREQ (source lines 51-53)
  # ===================================================================

  block_counts <- cats %>%
    dplyr::count(dplyr::across(dplyr::all_of(block_var)), name = "count") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(block_var)))

  # ===================================================================
  # Step 3: Page assignment using purrr::accumulate()
  # Replaces SAS DATA step with RETAIN (source lines 55-78)
  #
  # SAS semantics preserved:
  #   - pagecount starts at 0, page starts at 1
  #   - For each block (first.&blockvar in SAS):
  #       * Warn if block count alone > max_boxes_per_page
  #       * If pagecount + count > max_boxes_per_page -> new page
  #       * Else accumulate count on current page
  # ===================================================================

  counts_vec <- block_counts[["count"]]


  # Emit warnings for oversized blocks (matches SAS lines 63-64)
  oversized_idx <- which(counts_vec > max_boxes_per_page)
  if (length(oversized_idx) > 0L) {
    for (oi in oversized_idx) {
      block_val_display <- block_counts[[block_var]][oi]
      cli::cli_warn(
        c(
          "!" = paste0(
            "MAX_BOXES_PER_PAGE (", max_boxes_per_page,
            ") is too small for this blocking: ",
            block_var, "=", block_val_display,
            ", count=", counts_vec[oi]
          )
        )
      )
    }
  }

  # Use purrr::accumulate() to carry forward state across blocks.
  # State is an integer vector c(pagecount, page).
  # This is the idiomatic tidyverse replacement for the SAS RETAIN
  # variable carryforward pattern (AAP section 0.7.1).
  state_list <- purrr::accumulate(
    .x = counts_vec,
    .f = function(state, cnt) {
      pagecount <- state[1L]
      page      <- state[2L]
      if (pagecount + cnt > max_boxes_per_page) {
        # This block starts a new page; reset pagecount to this block's count
        c(cnt, page + 1L)
      } else {
        # This block fits on the current page; accumulate
        c(pagecount + cnt, page)
      }
    },
    .init = c(0L, 1L)
  )

  # Remove the initial state element and extract page assignments
  page_numbers <- purrr::map_int(state_list[-1L], ~ .x[2L])
  block_counts[["page"]] <- page_numbers

  # ===================================================================
  # Step 4: Determine block variable type
  # Replaces SAS VTYPE/VLENGTH (source lines 74-77)
  # ===================================================================

  is_block_numeric <- is.numeric(df[[block_var]])

  # ===================================================================
  # Step 5: Build range strings per page
  # Replaces SAS DATA step range construction (source lines 91-112)
  # and PROC SQL concatenation (source lines 117-121)
  #
  # Range format:
  #   Numeric:   "min<=block_var<=max"
  #   Character: '"min"<=block_var<="max"'
  # ===================================================================

  page_ranges <- block_counts %>%
    dplyr::group_by(.data[["page"]]) %>%
    dplyr::summarise(
      min_val = min(.data[[block_var]], na.rm = TRUE),
      max_val = max(.data[[block_var]], na.rm = TRUE),
      .groups = "drop"
    )

  ranges <- purrr::map_chr(seq_len(nrow(page_ranges)), function(i) {
    min_v <- page_ranges[["min_val"]][i]
    max_v <- page_ranges[["max_val"]][i]

    if (is_block_numeric) {
      # SAS equivalent: strip(put(&blockvar, 8.-L))
      # Use format() with scientific=FALSE and trimws for clean numeric strings
      paste0(
        trimws(format(min_v, scientific = FALSE)),
        "<=", block_var, "<=",
        trimws(format(max_v, scientific = FALSE))
      )
    } else {
      # SAS equivalent: quote(strip(&blockvar))
      # Wrap character values in double quotes for range expressions
      paste0(
        '"', trimws(as.character(min_v)), '"',
        "<=", block_var, "<=",
        '"', trimws(as.character(max_v)), '"'
      )
    }
  })

  # Join page ranges with pipe delimiter (matches SAS SEPARATED BY '|')
  range_string <- paste(ranges, collapse = "|")

  # ===================================================================
  # Step 6: Inform user of computed ranges
  # Replaces SAS lines 125-126 %PUT NOTE messages
  # ===================================================================

  cli::cli_inform(c(
    "i" = paste0(
      "Default block ranges for each plot, limiting to ",
      max_boxes_per_page, " boxes max per page."
    ),
    "i" = paste0("BOXPLOT_BLOCK_RANGES set to: ", range_string)
  ))

  # ===================================================================
  # Return result as a named list (replaces SAS global symbol assignment)
  # ===================================================================

  list(
    ranges       = ranges,
    range_string = range_string,
    pages        = block_counts
  )
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - max_boxes_per_page is passed as function argument instead of
#      SAS global symbol MAX_BOXES_PER_PAGE
#    - Block variable type (numeric/character) detected via
#      is.numeric() instead of SAS VTYPE
#    - Range string format preserved for downstream consumers
#    - SAS sym= parameter (output symbol name) is not needed because
#      R returns the result directly instead of setting a global symbol
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Sort order of blocks: R arrange() vs SAS PROC SORT — both
#      stable for single-key sorts; identical results expected
#    - Category counting: dplyr count() vs SAS PROC FREQ COUNT —
#      identical semantics for non-missing data
#    - Numeric formatting: SAS put(x, 8.-L) truncates to integer
#      representation; R preserves decimal precision. This is
#      more correct for fractional block values.
# NO DIRECT R EQUIVALENT:
#    - SAS global macro variable assignment (%GLOBAL &sym) replaced
#      by R function return value (list with $range_string)
#    - SAS PROC DATASETS DELETE (%util_delete_dsets) replaced by
#      R garbage collection (no explicit cleanup needed)
#    - SAS missing option setting (OPTIONS MISSING='.') not needed
#      in R as NA displays as NA by default
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Idiomatic tidyverse replacement for DATA step, PROC
#      FREQ, PROC SORT, and PROC SQL operations (AAP section 0.8.1)
#    - rlang: Tidy evaluation with dynamic column names via .data
#      pronoun; input type validation via is_character() and
#      is_scalar_integerish()
#    - cli: User-facing messages matching SAS %PUT NOTE/WARNING format
#    - purrr: accumulate() for SAS RETAIN state carryforward pattern;
#      map_chr() for functional range string construction
# OPEN QUESTIONS:
#    - Should downstream WPCT scripts consume the pipe-delimited
#      string ($range_string) or the character vector ($ranges)?
#    - Is the range string format used in dplyr::filter() expressions
#      or ggplot2 facet_wrap? Downstream consumers may need to parse
#      the range expressions differently in R vs SAS contexts.
# ============================================================
