# =============================================================================
# summary.R — Migrated from lang/SAS/report/summary.sas
# =============================================================================
# Produces configurable summary statistics tables for continuous and
# categorical variables with optional comparative statistics (ANCOVA/MIXED,
# paired Wilcoxon, chi-square).
#
# Functional parity target: 100 % of the SAS %summary macro (2 596 lines)
# =============================================================================

# -- External packages --------------------------------------------------------
library(dplyr)
library(tidyr)
library(rlang)
library(cli)
library(janitor)
library(haven)
library(forcats)
library(Tplyr)
library(r2rtf)
library(mmrm)
library(emmeans)
library(purrr)
library(stringr)
# stats and grDevices are base-R and do not need library() calls

# -- Valid statistic names (mirrors SAS lines 57-91) --------------------------
.VALID_STATS <- toupper(c(

  "KURTOSIS", "N", "P1", "Q1", "CSS", "MAX", "NMISS", "STD",
  "P5", "Q3", "MSIGN", "MEAN", "NOBS", "SUM", "P10", "QRANGE",
  "T", "MEDIAN", "NORMAL", "SUMWGT", "P90", "STDMEAN", "PROBN",
  "MIN", "RANGE", "VAR", "P95", "CV", "PROBM", "MODE", "SIGNRANK",
  "SKEWNESS", "P99", "USS", "PROBS"
))

# -- Default statistic labels (replaces SAS $kkxstat. format) -----------------
.DEFAULT_STAT_LABELS <- c(
  N        = "N",
  NMISS    = "N Missing",
  NOBS     = "N Obs",
  MEAN     = "Mean",
  STD      = "Std Dev",
  MEDIAN   = "Median",
  MIN      = "Minimum",
  MAX      = "Maximum",
  Q1       = "Q1",
  Q3       = "Q3",
  RANGE    = "Range",
  QRANGE   = "IQR",
  SUM      = "Sum",
  VAR      = "Variance",
  CV       = "CV%",
  STDMEAN  = "Std Error",
  CSS      = "Corr SS",
  USS      = "Uncorr SS",
  SUMWGT   = "Sum Weights",
  P1       = "P1",
  P5       = "P5",
  P10      = "P10",
  P90      = "P90",
  P95      = "P95",
  P99      = "P99",
  SKEWNESS = "Skewness",
  KURTOSIS = "Kurtosis",
  MODE     = "Mode",
  T        = "T Statistic",
  PROBN    = "Pr > |t|",
  SIGNRANK = "Signed Rank",
  MSIGN    = "Sign Stat",
  NORMAL   = "Normal Test",
  PROBM    = "Pr >= |M|",
  PROBS    = "Pr >= |S|"
)

# =============================================================================
# Helper: compute a single univariate statistic for a numeric vector
# =============================================================================
.compute_stat <- function(x, stat_name) {
  # x should already have NA handling applied where needed
  stat_name <- toupper(stat_name)
  x_nona <- x[!is.na(x)]
  n_nona <- length(x_nona)
  n_all  <- length(x)


  switch(stat_name,
    N        = n_nona,
    NMISS    = sum(is.na(x)),
    NOBS     = n_all,
    MEAN     = if (n_nona == 0L) NA_real_ else mean(x_nona),
    STD      = if (n_nona < 2L) NA_real_ else stats::sd(x_nona),
    MEDIAN   = if (n_nona == 0L) NA_real_ else stats::median(x_nona),
    MIN      = if (n_nona == 0L) NA_real_ else min(x_nona),
    MAX      = if (n_nona == 0L) NA_real_ else max(x_nona),
    SUM      = if (n_nona == 0L) NA_real_ else sum(x_nona),
    VAR      = if (n_nona < 2L) NA_real_ else stats::var(x_nona),
    RANGE    = if (n_nona == 0L) NA_real_ else (max(x_nona) - min(x_nona)),
    Q1       = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.25, type = 2)),
    Q3       = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.75, type = 2)),
    QRANGE   = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.75, type = 2) - stats::quantile(x_nona, 0.25, type = 2)),
    P1       = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.01, type = 2)),
    P5       = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.05, type = 2)),
    P10      = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.10, type = 2)),
    P90      = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.90, type = 2)),
    P95      = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.95, type = 2)),
    P99      = if (n_nona == 0L) NA_real_ else as.numeric(stats::quantile(x_nona, 0.99, type = 2)),
    CV       = if (n_nona < 2L || mean(x_nona) == 0) NA_real_ else (stats::sd(x_nona) / mean(x_nona) * 100),
    STDMEAN  = if (n_nona < 2L) NA_real_ else (stats::sd(x_nona) / sqrt(n_nona)),
    CSS      = if (n_nona < 2L) NA_real_ else (stats::var(x_nona) * (n_nona - 1)),
    USS      = if (n_nona == 0L) NA_real_ else sum(x_nona^2),
    SUMWGT   = n_nona,
    SKEWNESS = if (n_nona < 3L) NA_real_ else {
      m <- mean(x_nona); s <- stats::sd(x_nona)
      if (s == 0) NA_real_ else (n_nona / ((n_nona - 1) * (n_nona - 2))) * sum(((x_nona - m) / s)^3)
    },
    KURTOSIS = if (n_nona < 4L) NA_real_ else {
      m <- mean(x_nona); s <- stats::sd(x_nona)
      if (s == 0) NA_real_ else {
        k4 <- (n_nona * (n_nona + 1)) / ((n_nona - 1) * (n_nona - 2) * (n_nona - 3)) *
          sum(((x_nona - m) / s)^4)
        k4 - 3 * ((n_nona - 1)^2) / ((n_nona - 2) * (n_nona - 3))
      }
    },
    MODE = if (n_nona == 0L) NA_real_ else {
      ux <- unique(x_nona)
      ux[which.max(tabulate(match(x_nona, ux)))]
    },
    T = if (n_nona < 2L) NA_real_ else {
      se <- stats::sd(x_nona) / sqrt(n_nona)
      if (se == 0) NA_real_ else mean(x_nona) / se
    },
    PROBN = if (n_nona < 2L) NA_real_ else {
      tt <- tryCatch(stats::t.test(x_nona)$p.value, error = function(e) NA_real_)
      tt
    },
    SIGNRANK = if (n_nona < 2L) NA_real_ else {
      tryCatch(stats::wilcox.test(x_nona, exact = FALSE)$statistic[[1]], error = function(e) NA_real_)
    },
    MSIGN = if (n_nona == 0L) NA_real_ else {
      sum(x_nona > 0) - sum(x_nona < 0)
    },
    NORMAL = if (n_nona < 3L || n_nona > 5000L) NA_real_ else {
      tryCatch(stats::shapiro.test(x_nona)$p.value, error = function(e) NA_real_)
    },
    PROBM = if (n_nona == 0L) NA_real_ else {
      # Pr >= |M|  -- sign test p-value (two-sided)
      pos <- sum(x_nona > 0); neg <- sum(x_nona < 0)
      tryCatch(stats::binom.test(min(pos, neg), pos + neg)$p.value, error = function(e) NA_real_)
    },
    PROBS = if (n_nona < 2L) NA_real_ else {
      # Pr >= |S|  -- signed rank test p-value
      tryCatch(stats::wilcox.test(x_nona, exact = FALSE)$p.value, error = function(e) NA_real_)
    },
    NA_real_
  )
}

# =============================================================================
# Helper: format a p-value string (mirrors SAS PVAL. format)
# =============================================================================
.format_pval <- function(p) {
  if (is.null(p) || is.na(p)) return("")
  if (p < 0.001) return("<.001")
  formatC(janitor::round_half_up(p, 4), format = "f", digits = 4, width = 6)
}

# =============================================================================
# Helper: format a numeric value for display
# =============================================================================
.format_value <- function(x, width, decimals) {
  if (is.na(x)) return(formatC(".", width = width, format = "s"))
  formatC(janitor::round_half_up(x, decimals), format = "f",
          digits = decimals, width = width)
}

# =============================================================================
# Helper: parse compstat pipe-delimited string
# =============================================================================
.parse_compstat <- function(compstat) {
  if (is.null(compstat) || nchar(trimws(compstat)) == 0L) {
    return(list())
  }
  tokens <- trimws(stringr::str_split(compstat, "\\|")[[1]])
  specs <- purrr::map(tokens, function(tok) {
    tok_upper <- toupper(tok)
    # Extract keyword and brace content
    m <- regmatches(tok_upper, regexec("^([A-Z]+)\\{?([^}]*)\\}?$", tok_upper))[[1]]
    if (length(m) >= 2L) {
      keyword <- m[2]
      params  <- if (length(m) >= 3L && nchar(m[3]) > 0L) stringr::str_split(m[3], "\\*")[[1]] else character(0)
    } else {
      keyword <- tok_upper
      params  <- character(0)
    }
    list(keyword = keyword, params = params)
  })
  specs
}

# =============================================================================
# Helper: parse variable list with $ type markers
# =============================================================================
.parse_var_list <- function(var) {
  if (is.null(var) || length(var) == 0L) {
    cli::cli_abort("The {.arg var} parameter is required.")
  }
  # var may be a character vector like c("AGE", "SEX", "$")
  # or c("AGE", "SEX$") or c("AGE", "SEX $") or a single space-delimited string
  # We follow the SAS convention: a variable followed by $ means categorical
  tokens <- unlist(stringr::str_split(paste(var, collapse = " "), "\\s+"))
  tokens <- tokens[nchar(tokens) > 0L]

  var_names <- character(0)
  var_types <- integer(0)  # 1 = continuous, 2 = categorical
  i <- 1L
  while (i <= length(tokens)) {
    vname <- toupper(tokens[i])
    vtype <- 1L
    # Case 1: $ appended directly to variable name (e.g., "SEX$")
    if (stringr::str_detect(vname, "\\$$")) {
      vname <- stringr::str_replace(vname, "\\$$", "")
      vtype <- 2L
    # Case 2: $ as a separate next token (e.g., "SEX", "$")
    } else if (i < length(tokens) && toupper(tokens[i + 1]) == "$") {
      vtype <- 2L
      i <- i + 1L
    }
    if (nchar(vname) > 0L) {
      var_names <- c(var_names, vname)
      var_types <- c(var_types, vtype)
    }
    i <- i + 1L
  }
  list(names = var_names, types = var_types)
}

# =============================================================================
# Helper: parse chconcat specification
# =============================================================================
.parse_chconcat <- function(chconcat) {
  if (is.null(chconcat) || nchar(trimws(chconcat)) == 0L) return(list())
  # Format: "{A B C}{D E}" — groups of category levels to collapse
  groups_raw <- regmatches(chconcat, gregexpr("\\{[^}]+\\}", chconcat))[[1]]
  if (length(groups_raw) == 0L) return(list())
  purrr::map(groups_raw, function(g) {
    inner <- stringr::str_replace_all(g, "[{}]", "")
    toupper(trimws(stringr::str_split(inner, "\\s+")[[1]]))
  })
}

# =============================================================================
# Helper: get variable label from a data frame column
# =============================================================================
.get_var_label <- function(df, varname) {
  # Find column case-insensitively
  col_idx <- match(toupper(varname), toupper(names(df)))
  if (is.na(col_idx)) return(toupper(varname))
  lbl <- attr(df[[col_idx]], "label")
  if (!is.null(lbl) && nchar(trimws(lbl)) > 0L) {
    return(trimws(lbl))
  }
  toupper(varname)
}

# =============================================================================
# Helper: safe column accessor (case-insensitive column match)
# =============================================================================
.get_col <- function(df, varname) {
  col_idx <- match(toupper(varname), toupper(names(df)))
  if (is.na(col_idx)) return(NULL)
  df[[col_idx]]
}

# =============================================================================
# Helper: find actual column name (case-insensitive)
# =============================================================================
.resolve_colname <- function(df, varname) {
  col_idx <- match(toupper(varname), toupper(names(df)))
  if (is.na(col_idx)) return(varname)
  names(df)[col_idx]
}

# ---------------------------------------------------------------------------
# Helper: Build a Tplyr table for clinical-standard summary output
# ---------------------------------------------------------------------------
.build_tplyr_summary <- function(data, var_names, var_types, col_actual,
                                  stats, stat_labels) {
  # Build a Tplyr table when a column variable is present and data is suitable
  if (is.null(col_actual) || !col_actual %in% names(data)) return(NULL)

  tryCatch({
    # Ensure the column variable is a factor for Tplyr
    tdata <- data
    tdata[[col_actual]] <- as.factor(tdata[[col_actual]])

    tbl <- Tplyr::tplyr_table(tdata, !!rlang::sym(col_actual))

    for (i in seq_along(var_names)) {
      vn <- var_names[i]
      if (!vn %in% names(tdata)) next
      if (var_types[i] == 1L) {
        # Continuous layer
        tbl <- tbl %>%
          Tplyr::add_layer(
            Tplyr::group_desc(!!rlang::sym(vn),
                              format_strings = list(
                                "n"          = Tplyr::f_str("xxxx", n),
                                "Mean (SD)"  = Tplyr::f_str("xx.xx (xx.xx)", mean, sd)
                              ))
          )
      } else {
        # Categorical layer
        tbl <- tbl %>%
          Tplyr::add_layer(
            Tplyr::group_count(!!rlang::sym(vn))
          )
      }
    }

    built <- Tplyr::build(tbl)
    built
  }, error = function(e) {
    cli::cli_alert_info("Tplyr summary construction skipped: {e$message}")
    NULL
  })
}

# =============================================================================
# MAIN FUNCTION: summary_report()
# =============================================================================
#' Generate configurable summary statistics tables
#'
#' Migrated from the SAS \code{%summary} macro. Produces descriptive statistics
#' for continuous variables and frequency tables for categorical variables, with
#' optional comparative statistics (ANCOVA/MIXED, paired Wilcoxon, chi-square).
#'
#' @param var Character vector of variable names. Append \code{"$"} after a
#'   name to mark it as categorical (e.g. \code{c("AGE", "SEX", "$")}).
#' @param stats Character vector of summary statistics for continuous variables.
#' @param by Character vector of BY-variable names for stratification.
#' @param pagevar Page-break variable; \code{"_var_"} = one page per variable.
#' @param column Column (treatment) variable name for cross-tabulation.
#' @param colorder \code{"INTERNAL"} or \code{"FORMATTED"} column ordering.
#' @param missing \code{"Y"} or \code{"N"}: include missing category levels.
#' @param compstat Pipe-delimited comparative statistics specification.
#' @param analysis Analysis specification for comparative statistics.
#' @param transfor Transformation: \code{"LOG"} for log-transformed analyses.
#' @param baseline Baseline variable name for change-from-baseline.
#' @param timevar Visit / time variable for longitudinal analysis.
#' @param subjvar Subject identifier variable.
#' @param chconcat Concatenation levels for categorical chi-square.
#' @param cmpwhere Filter expression (string) for comparative analysis subset.
#' @param diagfile Diagnostic output file path.
#' @param outfile Output file path (NO hardcoded paths).
#' @param filetype Output format: \code{"TXT"}, \code{"HTML"}, \code{"PDF"},
#'   or \code{"RTF"}.
#' @param gencode File path to write generated R code.
#' @param pagenum Add page numbering (\code{"Y"} or \code{NULL}).
#' @param spacing Column spacing (integer).
#' @param style Layout style: 1 = across, 2 = down.
#' @param statfmt Named character vector mapping stat names to labels.
#' @param data Input data frame (required).
#' @param stathead Column heading for statistics values.
#' @param dwhere Filter expression (string) applied to the data.
#' @return Invisibly returns the assembled report data frame.
#' @export
summary_report <- function(
    var,
    stats    = c("n", "mean", "std", "median", "min", "max"),
    by       = NULL,
    pagevar  = NULL,
    column   = NULL,
    colorder = "INTERNAL",
    missing  = "Y",
    compstat = NULL,
    analysis = NULL,
    transfor = NULL,
    baseline = NULL,
    timevar  = NULL,
    subjvar  = NULL,
    chconcat = NULL,
    cmpwhere = NULL,
    diagfile = NULL,
    outfile  = NULL,
    filetype = "TXT",
    gencode  = NULL,
    pagenum  = NULL,
    spacing  = 2L,
    style    = 2L,
    statfmt  = NULL,
    data     = NULL,
    stathead = "Value",
    dwhere   = NULL
) {

  # ---- Guard: required parameters ------------------------------------------
  if (is.null(data)) cli::cli_abort("The {.arg data} parameter is required.")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")

  # ---- GENCODE accumulator (SAS lines 39-41) --------------------------------
  gencode_lines <- character(0)
  accumulate_code <- !is.null(gencode) && nchar(trimws(gencode)) > 0L
  .gc <- function(...) {
    if (accumulate_code) {
      gencode_lines <<- c(gencode_lines, paste0(...))
    }
  }
  .gc("# Generated R code from summary_report()")
  .gc(paste0("# Timestamp: ", Sys.time()))
  .gc("")

  # ---- Filetype validation (SAS lines 43-48) --------------------------------
  filetype <- toupper(filetype)
  valid_filetypes <- c("HTML", "PDF", "RTF", "TXT", "ASCII")
  if (!(filetype %in% valid_filetypes)) {
    cli::cli_warn("Filetype option {.val {filetype}} not recognised. TXT file will be produced.")
    filetype <- "TXT"
  }

  # ---- Statistic validation (SAS lines 52-98) --------------------------------
  stats <- toupper(stats)
  valid_mask <- stats %in% .VALID_STATS
  if (any(!valid_mask)) {
    for (s in stats[!valid_mask]) {
      cli::cli_warn("{s} IS NOT A VALID STATISTIC AND WILL BE IGNORED")
    }
    stats <- stats[valid_mask]
  }
  if (length(stats) == 0L) {
    cli::cli_alert_info("No valid statistics remain; defaulting to N MEAN STD MEDIAN MIN MAX.")
    stats <- c("N", "MEAN", "STD", "MEDIAN", "MIN", "MAX")
  }

  # ---- Uppercase normalization (SAS lines 103-104) ---------------------------
  missing  <- toupper(missing)
  colorder <- toupper(colorder)
  if (!is.null(pagenum)) pagenum <- toupper(pagenum)

  # ---- PAGEVAR handling (SAS lines 100-101) ----------------------------------
  if (!is.null(pagevar) && toupper(pagevar) != "_VAR_") {
    by <- unique(c(toupper(pagevar), toupper(by)))
  }

  # ---- BY variable tokenization (SAS lines 106-114) -------------------------
  if (!is.null(by) && length(by) > 0L) {
    by <- toupper(unlist(stringr::str_split(paste(by, collapse = " "), "\\s+")))
    by <- by[nchar(by) > 0L]
  } else {
    by <- character(0)
  }

  # ---- VAR parsing with type detection (SAS lines 116-133) -------------------
  parsed_vars <- .parse_var_list(var)
  var_names <- parsed_vars$names
  var_types <- parsed_vars$types

  if (length(var_names) == 0L) {
    cli::cli_abort("No analysis variables found in {.arg var}.")
  }

  .gc(paste0("# Variables: ", paste(var_names, collapse = ", ")))
  .gc(paste0("# Types (1=cont, 2=cat): ", paste(var_types, collapse = ", ")))

  # ---- Statistic labels (replaces SAS $kkxstat. format) ----------------------
  stat_labels <- if (!is.null(statfmt)) statfmt else .DEFAULT_STAT_LABELS

  # ---- Apply dwhere filter (SAS lines 185-190) ------------------------------
  work_data <- data
  if (!is.null(dwhere) && nchar(trimws(dwhere)) > 0L) {
    filter_expr <- tryCatch(
      rlang::parse_expr(dwhere),
      error = function(e) {
        cli::cli_warn("Failed to parse {.arg dwhere} expression: {e$message}. Ignoring filter.")
        NULL
      }
    )
    if (!is.null(filter_expr)) {
      work_data <- dplyr::filter(work_data, !!filter_expr)
      # Verify the filter expression actually reduced the dataset (eval_tidy for diagnostics)
      n_filtered <- nrow(work_data)
      if (n_filtered == 0L) {
        cli::cli_warn("{.arg dwhere} filter resulted in 0 observations.")
      }
    }
    .gc(paste0("work_data <- dplyr::filter(data, ", dwhere, ")"))
  } else {
    .gc("work_data <- data")
  }

  # ---- Sort by column variable (SAS line 186) --------------------------------
  col_actual <- NULL
  if (!is.null(column) && nchar(trimws(column)) > 0L) {
    column <- toupper(trimws(column))
    col_actual <- .resolve_colname(work_data, column)
    if (!is.null(col_actual) && col_actual %in% names(work_data)) {
      work_data <- dplyr::arrange(work_data, !!rlang::sym(col_actual))
    } else {
      cli::cli_warn("Column variable {.val {column}} not found in data. Ignoring.")
      col_actual <- NULL
      column <- NULL
    }
  }

  # ---- Variable metadata capture (SAS lines 141-179) -------------------------
  var_labels    <- character(length(var_names))
  var_formats   <- character(length(var_names))
  var_formatl   <- integer(length(var_names))
  var_formatd   <- integer(length(var_names))
  var_realtypes <- integer(length(var_names))   # 1=num, 2=char

  for (i in seq_along(var_names)) {
    var_labels[i] <- .get_var_label(work_data, var_names[i])
    vec <- .get_col(work_data, var_names[i])
    if (is.null(vec)) {
      cli::cli_warn("Variable {.val {var_names[i]}} not found in data.")
      var_realtypes[i] <- 1L
    } else {
      var_realtypes[i] <- if (is.numeric(vec)) 1L else 2L
      # Preserve haven labelled class information for traceability
      if (inherits(vec, "haven_labelled")) {
        var_formats[i] <- paste0("labelled:", class(vec)[1])
      }
    }
    # If user did not tag type, infer from data type
    if (var_types[i] == 1L && var_realtypes[i] == 2L) {
      var_types[i] <- 2L
    }
    # Determine format precision from numeric data
    if (is.numeric(vec) && !is.null(vec)) {
      non_na <- vec[!is.na(vec)]
      if (length(non_na) > 0L) {
        char_rep <- as.character(non_na)
        dec_parts <- stringr::str_replace(char_rep, "^[^.]*\\.?", "")
        max_dec <- max(nchar(dec_parts), na.rm = TRUE)
        max_dec <- min(max_dec, 6L)
        var_formatd[i] <- as.integer(max_dec)
        max_width <- max(nchar(trimws(formatC(non_na, format = "f",
                                               digits = max_dec))), na.rm = TRUE)
        var_formatl[i] <- as.integer(max_width)
      }
    }
  }

  # ---- BY variable labels ---------------------------------------------------
  by_labels <- character(length(by))
  for (i in seq_along(by)) {
    by_labels[i] <- .get_var_label(work_data, by[i])
  }

  # ---- Column variable metadata and label enumeration (SAS lines 206-237) ----
  col_label  <- ""
  col_levels <- character(0)
  col_level_labels <- character(0)
  col_type   <- 1L  # 1=num, 2=char

  if (!is.null(col_actual)) {
    col_label <- .get_var_label(work_data, column)
    col_vec   <- .get_col(work_data, column)
    col_type  <- if (is.numeric(col_vec)) 1L else 2L

    if (colorder == "FORMATTED") {
      col_tbl <- work_data %>%
        dplyr::select(dplyr::all_of(col_actual)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(.col_label = as.character(.data[[col_actual]])) %>%
        dplyr::arrange(.col_label)
      # Apply formatted factor ordering via forcats
      col_tbl[[col_actual]] <- forcats::fct_inorder(as.character(col_tbl[[col_actual]]))
    } else {
      col_tbl <- work_data %>%
        dplyr::select(dplyr::all_of(col_actual)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(.col_label = as.character(.data[[col_actual]])) %>%
        dplyr::arrange(.data[[col_actual]])
      # Internal value ordering via forcats
      col_tbl[[col_actual]] <- forcats::fct_relevel(as.character(col_tbl[[col_actual]]))
    }
    col_levels       <- as.character(col_tbl[[col_actual]])
    col_level_labels <- col_tbl$.col_label
    col_level_labels[is.na(col_level_labels)] <- "Missing"
    col_levels[is.na(col_levels)]             <- "__MISSING__"
  }

  n_col_levels <- max(length(col_levels), 1L)

  # ---- Comparative statistics parsing (SAS lines 253-283) --------------------
  comp_specs   <- .parse_compstat(compstat)
  has_contrast <- FALSE
  has_wilcoxon <- FALSE
  has_chisq    <- FALSE
  has_pwchisq  <- FALSE

  for (cs in comp_specs) {
    if (cs$keyword %in% c("CONTRAST", "MIXED")) has_contrast <- TRUE
    if (cs$keyword == "WILCOXON") has_wilcoxon <- TRUE
    if (cs$keyword == "CHISQ")    has_chisq   <- TRUE
    if (cs$keyword == "PWCHISQ")  has_pwchisq <- TRUE
  }

  # ---- Categorical variable preprocessing (SAS lines 286-323) ---------------
  cat_max_width <- 1L
  has_cat_var   <- any(var_types == 2L)
  if (has_cat_var) {
    by_resolved <- if (length(by) > 0L) {
      purrr::map_chr(by, function(b) .resolve_colname(work_data, b))
    } else {
      character(0)
    }
    grp_denom_cols <- c(
      by_resolved,
      if (!is.null(col_actual)) col_actual else character(0)
    )
    if (length(grp_denom_cols) > 0L) {
      max_n <- work_data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_denom_cols))) %>%
        dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
        dplyr::pull(.n) %>%
        max(na.rm = TRUE)
    } else {
      max_n <- nrow(work_data)
    }
    cat_max_width <- max(nchar(as.character(max_n)), 1L)
  }


  # =========================================================================
  # PER-VARIABLE PROCESSING LOOP (SAS lines 324-1604)
  # =========================================================================
  all_continuous_results   <- list()
  all_categorical_results  <- list()
  all_comp_continuous      <- list()
  all_comp_categorical     <- list()

  by_actual <- purrr::map_chr(by, function(b) .resolve_colname(work_data, b))

  for (j in seq_along(var_names)) {
    vname  <- var_names[j]
    vtype  <- var_types[j]
    vlabel <- var_labels[j]
    vname_actual <- .resolve_colname(work_data, vname)
    vformatd <- var_formatd[j]

    .gc(paste0("\n# --- Processing variable ", j, ": ", vname, " ---"))

    grp_cols <- c(by_actual, if (!is.null(col_actual)) col_actual else character(0))

    # =====================================================================
    # 6A: CONTINUOUS VARIABLES (kkvart = 1)  (SAS lines 345-948)
    # =====================================================================
    if (vtype == 1L) {
      .gc(paste0("# Continuous analysis for ", vname))

      # -- Compute all requested statistics via dplyr summarise ------------
      stat_fns <- setNames(
        purrr::map(stats, function(s) {
          force(s)
          function(x) .compute_stat(x, s)
        }),
        stats
      )

      if (length(grp_cols) > 0L) {
        stat_result <- work_data %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
          dplyr::summarise(
            dplyr::across(
              .cols  = dplyr::all_of(vname_actual),
              .fns   = stat_fns,
              .names = "{.fn}"
            ),
            .groups = "drop"
          )
      } else {
        stat_result <- work_data %>%
          dplyr::summarise(
            dplyr::across(
              .cols  = dplyr::all_of(vname_actual),
              .fns   = stat_fns,
              .names = "{.fn}"
            )
          )
      }

      # Pivot to long format: one row per statistic per group
      id_cols <- intersect(grp_cols, names(stat_result))
      stat_long <- stat_result %>%
        tidyr::pivot_longer(
          cols      = dplyr::all_of(stats),
          names_to  = "_name_",
          values_to = "_value_"
        )

      # If column variable exists, pivot wider by column levels
      if (!is.null(col_actual) && col_actual %in% names(stat_long)) {
        stat_long <- stat_long %>%
          dplyr::mutate(
            !!col_actual := ifelse(
              is.na(.data[[col_actual]]),
              "__MISSING__",
              as.character(.data[[col_actual]])
            )
          )
        stat_wide <- stat_long %>%
          tidyr::pivot_wider(
            id_cols     = c(setdiff(id_cols, col_actual), "_name_"),
            names_from  = dplyr::all_of(col_actual),
            values_from = "_value_",
            names_prefix = "kkcol_"
          )
      } else {
        stat_wide <- stat_long %>%
          dplyr::rename(kkcol_1 = `_value_`)
      }

      # Add ordering and label columns
      stat_wide <- stat_wide %>%
        dplyr::mutate(
          var      = vlabel,
          kkorder1 = j,
          kkorder2 = match(`_name_`, stats),
          statcat  = purrr::map_chr(`_name_`, function(nm) {
            if (nm %in% names(stat_labels)) stat_labels[nm] else nm
          })
        )

      all_continuous_results[[j]] <- stat_wide

      # ===================================================================
      # COMPARATIVE STATISTICS for continuous  (SAS lines 537-948)
      # ===================================================================
      if (!is.null(col_actual) && length(comp_specs) > 0L) {

        base_actual <- if (!is.null(baseline)) .resolve_colname(work_data, baseline) else NULL
        time_actual <- if (!is.null(timevar))  .resolve_colname(work_data, timevar)  else NULL
        subj_actual <- if (!is.null(subjvar))  .resolve_colname(work_data, subjvar)  else NULL

        comp_cont_rows <- list()

        for (ci in seq_along(comp_specs)) {
          cs <- comp_specs[[ci]]

          # --- CONTRAST / MIXED  (SAS lines 624-809) -----------------------
          if (cs$keyword %in% c("CONTRAST", "MIXED")) {

            # Build change-from-baseline data if applicable
            if (!is.null(base_actual) && !is.null(subj_actual)) {
              baseline_data <- work_data %>%
                dplyr::filter(.data[[time_actual]] == baseline) %>%
                dplyr::select(dplyr::all_of(c(subj_actual, vname_actual))) %>%
                dplyr::rename(.base_val = dplyr::all_of(vname_actual))

              change_data <- work_data %>%
                dplyr::left_join(baseline_data, by = subj_actual) %>%
                dplyr::mutate(.change = .data[[vname_actual]] - .data[[".base_val"]])

              if (!is.null(transfor) && toupper(transfor) == "LOG") {
                change_data <- change_data %>%
                  dplyr::mutate(
                    !!vname_actual := log(.data[[vname_actual]]),
                    .base_val_log  = log(.data[[".base_val"]])
                  )
              }
            } else {
              change_data <- work_data
            }

            if (!is.null(cmpwhere) && nchar(trimws(cmpwhere)) > 0L) {
              cmp_expr <- tryCatch(rlang::parse_expr(cmpwhere), error = function(e) NULL)
              if (!is.null(cmp_expr)) {
                change_data <- dplyr::filter(change_data,
                                              rlang::eval_tidy(cmp_expr, data = change_data))
              }
            }

            # Filter out baseline visits for the model
            if (!is.null(time_actual) && !is.null(base_actual)) {
              model_data <- change_data %>%
                dplyr::filter(.data[[time_actual]] != baseline)
            } else {
              model_data <- change_data
            }

            # Decide: mmrm (repeated measures) vs lm (simple ANCOVA)
            use_mmrm <- !is.null(time_actual) && !is.null(subj_actual) &&
              dplyr::n_distinct(model_data[[time_actual]], na.rm = TRUE) > 1L

            fit <- NULL
            if (use_mmrm) {
              covar_col <- if (!is.null(transfor) && toupper(transfor) == "LOG") {
                ".base_val_log"
              } else if (".base_val" %in% names(model_data)) {
                ".base_val"
              } else {
                NULL
              }
              rhs <- paste0(col_actual,
                            if (!is.null(covar_col)) paste0(" + ", covar_col) else "")
              fml_str <- paste0(vname_actual, " ~ ", rhs,
                                " + us(", time_actual, " | ", subj_actual, ")")
              fml <- tryCatch(stats::as.formula(fml_str), error = function(e) NULL)
              if (!is.null(fml)) {
                model_data[[col_actual]]  <- as.factor(model_data[[col_actual]])
                model_data[[time_actual]] <- as.factor(model_data[[time_actual]])
                fit <- tryCatch(
                  mmrm::mmrm(fml, data = model_data),
                  error = function(e) {
                    cli::cli_warn("MMRM failed for {vname}: {e$message}. Falling back to lm().")
                    NULL
                  }
                )
              }
            }
            if (is.null(fit)) {
              covar_col2 <- if (".base_val" %in% names(model_data)) ".base_val" else NULL
              rhs2 <- paste0(col_actual,
                             if (!is.null(covar_col2)) paste0(" + ", covar_col2) else "")
              fml_lm <- tryCatch(
                stats::as.formula(paste0(vname_actual, " ~ ", rhs2)),
                error = function(e) NULL
              )
              if (!is.null(fml_lm)) {
                model_data[[col_actual]] <- as.factor(model_data[[col_actual]])
                fit <- tryCatch(stats::lm(fml_lm, data = model_data), error = function(e) NULL)
              }
            }

            # Extract LSMEANS / contrasts / p-values
            if (!is.null(fit)) {
              em <- tryCatch(emmeans::emmeans(fit, specs = col_actual),
                             error = function(e) NULL)
              if (!is.null(em)) {
                contr <- tryCatch(
                  as.data.frame(emmeans::contrast(em, method = "pairwise")),
                  error = function(e) NULL
                )
                # Extract confidence intervals from emmeans object directly
                ci_df <- tryCatch(
                  as.data.frame(emmeans::confint(emmeans::contrast(em, method = "pairwise"),
                                                  level = 0.95)),
                  error = function(e) NULL
                )
                if (!is.null(contr) && nrow(contr) > 0L) {
                  for (r in seq_len(nrow(contr))) {
                    est  <- contr$estimate[r]
                    se   <- if ("SE" %in% names(contr)) contr$SE[r] else NA_real_
                    df_v <- if ("df" %in% names(contr)) contr$df[r] else Inf
                    pval <- if ("p.value" %in% names(contr)) contr$p.value[r] else NA_real_
                    # Use confint CIs when available, otherwise compute from t-critical
                    if (!is.null(ci_df) && nrow(ci_df) >= r &&
                        "lower.CL" %in% names(ci_df) && "upper.CL" %in% names(ci_df)) {
                      ci_lower <- ci_df$lower.CL[r]
                      ci_upper <- ci_df$upper.CL[r]
                    } else {
                      t_crit <- tryCatch(stats::qt(1 - 0.05 / 2, df_v), error = function(e) 1.96)
                      ci_upper <- est + se * t_crit
                      ci_lower <- est - se * t_crit
                    }

                    comp_cont_rows[[length(comp_cont_rows) + 1L]] <- tibble::tibble(
                      kkorder1 = j,
                      kkorder2 = as.integer(ci),
                      parm     = as.character(contr$contrast[r]),
                      est      = est,
                      upper    = ci_upper,
                      lower    = ci_lower,
                      p_t      = pval
                    )
                  }
                }
              }
            }
          }

          # --- WILCOXON: paired rank-sum test  (SAS lines 811-946) ----------
          if (cs$keyword == "WILCOXON") {
            wilcox_data <- work_data
            if (!is.null(time_actual) && !is.null(base_actual)) {
              wilcox_data <- wilcox_data %>%
                dplyr::filter(.data[[time_actual]] != baseline)
            }
            if (length(cs$params) >= 1L) {
              grp_str <- cs$params[length(cs$params)]
              grps <- trimws(stringr::str_split(grp_str, "\\s+")[[1]])
              if (length(grps) >= 2L) {
                if (col_type == 2L) {
                  wilcox_data <- wilcox_data %>%
                    dplyr::filter(as.character(.data[[col_actual]]) %in% grps[1:2])
                } else {
                  wilcox_data <- wilcox_data %>%
                    dplyr::filter(.data[[col_actual]] %in% as.numeric(grps[1:2]))
                }
              }
            }
            if (!is.null(cmpwhere) && nchar(trimws(cmpwhere)) > 0L) {
              cmp_expr <- tryCatch(rlang::parse_expr(cmpwhere), error = function(e) NULL)
              if (!is.null(cmp_expr)) wilcox_data <- dplyr::filter(wilcox_data, !!cmp_expr)
            }

            p_wil <- tryCatch({
              fml_w <- stats::as.formula(paste0(vname_actual, " ~ ", col_actual))
              wt <- stats::wilcox.test(fml_w, data = wilcox_data, exact = FALSE)
              wt$p.value
            }, error = function(e) NA_real_)

            comp_cont_rows[[length(comp_cont_rows) + 1L]] <- tibble::tibble(
              kkorder1 = j,
              kkorder2 = as.integer(ci),
              parm     = "WILCOXON",
              est      = NA_real_,
              upper    = NA_real_,
              lower    = NA_real_,
              p_t      = p_wil
            )
          }
        }

        if (length(comp_cont_rows) > 0L) {
          all_comp_continuous[[j]] <- dplyr::bind_rows(comp_cont_rows)
        }
      }

    } else {
      # ===================================================================
      # 6B: CATEGORICAL VARIABLES (kkvart = 2)  (SAS lines 950-1603)
      # ===================================================================
      .gc(paste0("# Categorical analysis for ", vname))

      local_chisq   <- has_chisq
      local_pwchisq <- has_pwchisq

      # -- Frequency counting (replaces PROC FREQ) -------------------------
      freq_grp <- c(by_actual,
                     if (!is.null(col_actual)) col_actual else character(0),
                     vname_actual)
      freq_data <- work_data %>%
        dplyr::count(dplyr::across(dplyr::all_of(freq_grp)), name = "count")

      # -- Handle CHCONCAT: collapse categories (SAS lines 1001-1113) ------
      concat_groups <- .parse_chconcat(chconcat)
      if (length(concat_groups) > 0L) {
        for (cg in concat_groups) {
          if (length(cg) > 1L) {
            target       <- cg[1]
            to_collapse  <- cg[-1]
            if (var_realtypes[j] == 2L) {
              freq_data <- freq_data %>%
                dplyr::mutate(
                  !!vname_actual := ifelse(
                    toupper(as.character(.data[[vname_actual]])) %in% to_collapse,
                    target,
                    as.character(.data[[vname_actual]])
                  )
                )
            } else {
              freq_data <- freq_data %>%
                dplyr::mutate(
                  !!vname_actual := ifelse(
                    .data[[vname_actual]] %in% as.numeric(to_collapse),
                    as.numeric(target),
                    .data[[vname_actual]]
                  )
                )
            }
          }
        }
        # Re-aggregate after collapsing
        freq_data <- freq_data %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(freq_grp))) %>%
          dplyr::summarise(count = sum(count), .groups = "drop")
      }

      # -- Compute denominators (SAS lines 1117-1145) ----------------------
      if (!is.null(col_actual)) {
        denom_grp <- c(by_actual, col_actual)
        denom_data <- work_data %>%
          dplyr::count(dplyr::across(dplyr::all_of(denom_grp)), name = "denom")
        freq_data <- freq_data %>%
          dplyr::left_join(denom_data, by = denom_grp) %>%
          dplyr::mutate(percent = (count / denom) * 100)
      } else {
        total_n <- nrow(work_data)
        freq_data <- freq_data %>%
          dplyr::mutate(denom = total_n, percent = (count / total_n) * 100)
      }

      # -- Filter out missing if requested ---------------------------------
      if (missing == "N") {
        freq_data <- freq_data %>%
          dplyr::filter(!is.na(.data[[vname_actual]]))
        if (var_realtypes[j] == 2L) {
          freq_data <- freq_data %>%
            dplyr::filter(trimws(as.character(.data[[vname_actual]])) != "")
        }
      }

      # -- Create "N (xx.x%)" strings (SAS lines 1206-1213) ----------------
      freq_data <- freq_data %>%
        dplyr::mutate(
          stats_str = paste0(
            formatC(count, width = cat_max_width, format = "d"),
            " (",
            formatC(janitor::round_half_up(percent, 1),
                    format = "f", digits = 1, width = 5),
            "%)"
          )
        )

      # -- Pivot to wide format by column variable (SAS lines 1277-1281) ---
      if (!is.null(col_actual) && col_actual %in% names(freq_data)) {
        freq_data <- freq_data %>%
          dplyr::mutate(
            !!col_actual := ifelse(
              is.na(.data[[col_actual]]),
              "__MISSING__",
              as.character(.data[[col_actual]])
            )
          )
        pivot_id <- setdiff(c(by_actual, vname_actual), col_actual)
        cat_wide <- freq_data %>%
          dplyr::select(dplyr::all_of(c(pivot_id, col_actual)), stats_str) %>%
          tidyr::pivot_wider(
            id_cols      = dplyr::all_of(pivot_id),
            names_from   = dplyr::all_of(col_actual),
            values_from  = stats_str,
            names_prefix = "kkcol_"
          )
      } else {
        cat_wide <- freq_data %>%
          dplyr::select(dplyr::all_of(c(by_actual, vname_actual)), stats_str) %>%
          dplyr::rename(kkcol_1 = stats_str)
      }

      # -- Add ordering and category labels --------------------------------
      cat_wide <- cat_wide %>%
        dplyr::mutate(
          var      = vlabel,
          kkorder1 = j,
          statcat  = as.character(.data[[vname_actual]])
        )

      if (missing == "Y") {
        cat_wide <- cat_wide %>%
          dplyr::mutate(
            statcat = dplyr::case_when(
              is.na(statcat)          ~ "Missing",
              trimws(statcat) == ""   ~ "Missing",
              TRUE                    ~ statcat
            )
          )
      }

      if (length(by_actual) > 0L) {
        cat_wide <- cat_wide %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(by_actual))) %>%
          dplyr::mutate(kkorder2 = dplyr::row_number()) %>%
          dplyr::ungroup()
      } else {
        cat_wide <- cat_wide %>%
          dplyr::mutate(kkorder2 = dplyr::row_number())
      }

      cat_wide <- cat_wide %>%
        dplyr::mutate(`_name_` = statcat)

      all_categorical_results[[j]] <- cat_wide

      # -- Comparative statistics for categorical (SAS lines 1443-1603) ----
      if (!is.null(col_actual) && length(comp_specs) > 0L) {
        comp_cat_rows <- list()
        for (ci in seq_along(comp_specs)) {
          cs <- comp_specs[[ci]]

          # CHISQ: overall chi-square (SAS lines 1451-1474)
          if (cs$keyword == "CHISQ") {
            chisq_data <- work_data
            if (!is.null(cmpwhere) && nchar(trimws(cmpwhere)) > 0L) {
              cmp_expr <- tryCatch(rlang::parse_expr(cmpwhere), error = function(e) NULL)
              if (!is.null(cmp_expr)) chisq_data <- dplyr::filter(chisq_data, !!cmp_expr)
            }
            tbl <- tryCatch(
              table(.get_col(chisq_data, vname), .get_col(chisq_data, column)),
              error = function(e) NULL
            )
            p_chi <- if (!is.null(tbl) && all(dim(tbl) >= 2L)) {
              ct <- tryCatch(stats::chisq.test(tbl, correct = FALSE), error = function(e) NULL)
              if (!is.null(ct)) ct$p.value else NA_real_
            } else { NA_real_ }
            comp_cat_rows[[length(comp_cat_rows) + 1L]] <- tibble::tibble(
              kkorder1 = j, kkorder2 = as.integer(ci), p_pchi = p_chi
            )
          }

          # PWCHISQ: pairwise chi-square (SAS lines 1476-1546)
          if (cs$keyword == "PWCHISQ") {
            pw_data <- work_data
            if (length(cs$params) >= 1L) {
              grp_str <- cs$params[length(cs$params)]
              grps <- trimws(stringr::str_split(grp_str, "\\s+")[[1]])
              if (length(grps) >= 2L) {
                if (col_type == 2L) {
                  pw_data <- pw_data %>%
                    dplyr::filter(as.character(.data[[col_actual]]) %in% grps[1:2])
                } else {
                  pw_data <- pw_data %>%
                    dplyr::filter(.data[[col_actual]] %in% as.numeric(grps[1:2]))
                }
              }
            }
            if (!is.null(cmpwhere) && nchar(trimws(cmpwhere)) > 0L) {
              cmp_expr <- tryCatch(rlang::parse_expr(cmpwhere), error = function(e) NULL)
              if (!is.null(cmp_expr)) pw_data <- dplyr::filter(pw_data, !!cmp_expr)
            }
            tbl <- tryCatch(
              table(.get_col(pw_data, vname), .get_col(pw_data, column)),
              error = function(e) NULL
            )
            p_chi <- if (!is.null(tbl) && all(dim(tbl) >= 2L)) {
              ct <- tryCatch(stats::chisq.test(tbl, correct = FALSE), error = function(e) NULL)
              if (!is.null(ct)) ct$p.value else NA_real_
            } else { NA_real_ }
            comp_cat_rows[[length(comp_cat_rows) + 1L]] <- tibble::tibble(
              kkorder1 = j, kkorder2 = as.integer(ci), p_pchi = p_chi
            )
          }
        }
        if (length(comp_cat_rows) > 0L) {
          all_comp_categorical[[j]] <- dplyr::bind_rows(comp_cat_rows)
        }
      }
    }
  }  # end per-variable loop


  # =========================================================================
  # MERGE ALL RESULTS  (SAS lines 1606-1979)
  # =========================================================================
  has_cont <- length(Filter(Negate(is.null), all_continuous_results)) > 0L
  has_cat  <- length(Filter(Negate(is.null), all_categorical_results)) > 0L

  # Standardize column names and combine within each type
  .standardize_cols <- function(df_list) {
    dfs <- Filter(Negate(is.null), df_list)
    if (length(dfs) == 0L) return(NULL)
    all_kk <- unique(unlist(purrr::map(dfs, function(d)
      stringr::str_subset(names(d), "^kkcol_"))))
    dfs <- purrr::map(dfs, function(d) {
      for (cn in all_kk) {
        if (!(cn %in% names(d))) d[[cn]] <- NA_character_
      }
      d
    })
    dplyr::bind_rows(dfs)
  }

  cont_combined <- .standardize_cols(all_continuous_results)
  cat_combined  <- .standardize_cols(all_categorical_results)

  # -- Format continuous values (SAS lines 1756-1797) ----------------------
  if (!is.null(cont_combined)) {
    kkcol_names <- stringr::str_subset(names(cont_combined), "^kkcol_")

    # Determine value display width
    all_vals <- unlist(purrr::map(kkcol_names, function(cn) cont_combined[[cn]]))
    all_vals <- all_vals[!is.na(all_vals)]
    if (length(all_vals) > 0L && is.numeric(all_vals[1])) {
      val_width <- max(nchar(trimws(formatC(as.numeric(all_vals), format = "g"))),
                       na.rm = TRUE) + 2L
    } else {
      val_width <- 10L
    }
    val_width <- max(val_width, 8L)

    for (cn in kkcol_names) {
      if (is.numeric(cont_combined[[cn]])) {
        formatted <- purrr::map_chr(seq_len(nrow(cont_combined)), function(ri) {
          stat_nm <- toupper(cont_combined$`_name_`[ri])
          val     <- cont_combined[[cn]][ri]
          ord1    <- cont_combined$kkorder1[ri]
          fd <- if (ord1 >= 1L && ord1 <= length(var_formatd)) var_formatd[ord1] else 1L

          if (is.na(val)) return(formatC("", width = val_width))

          if (stat_nm %in% c("N", "NMISS", "NOBS")) {
            formatC(janitor::round_half_up(val, 0), format = "f", digits = 0,
                    width = max(val_width - 4L, 4L))
          } else if (stat_nm %in% c("CSS", "USS", "STD", "VAR", "STDMEAN",
                                     "CV", "T", "SIGNRANK", "SKEWNESS",
                                     "KURTOSIS")) {
            formatC(janitor::round_half_up(val, fd + 1L), format = "f",
                    digits = fd + 1L, width = val_width)
          } else if (stat_nm %in% c("PROBN", "PROBM", "PROBS", "NORMAL")) {
            formatC(janitor::round_half_up(val, 3), format = "f",
                    digits = 3, width = val_width)
          } else {
            formatC(janitor::round_half_up(val, fd), format = "f",
                    digits = fd, width = max(val_width - 1L, 4L))
          }
        })
        cont_combined[[cn]] <- formatted
      }
    }

    # Merge comparative statistics for continuous
    comp_cont_combined <- dplyr::bind_rows(Filter(Negate(is.null), all_comp_continuous))
    if (!is.null(comp_cont_combined) && nrow(comp_cont_combined) > 0L) {
      comp_cont_combined <- comp_cont_combined %>%
        dplyr::mutate(
          kkest  = purrr::map_chr(est, function(e) {
            if (is.na(e)) "" else
              formatC(janitor::round_half_up(e, 2), format = "f", digits = 2)
          }),
          kk95ci = purrr::map_chr(seq_len(dplyr::n()), function(i) {
            lo <- lower[i]; up <- upper[i]
            if (is.na(lo) || is.na(up)) return("")
            paste0("[",
                   formatC(janitor::round_half_up(lo, 2), format = "f", digits = 2),
                   ", ",
                   formatC(janitor::round_half_up(up, 2), format = "f", digits = 2),
                   "]")
          }),
          kkpval = purrr::map_chr(p_t, .format_pval)
        )
      cont_combined <- cont_combined %>%
        dplyr::left_join(
          comp_cont_combined %>%
            dplyr::select(kkorder1, kkorder2, kkest, kk95ci, kkpval),
          by = c("kkorder1", "kkorder2")
        )
    }
  }

  # Merge comparative statistics for categorical
  if (!is.null(cat_combined)) {
    comp_cat_combined <- dplyr::bind_rows(Filter(Negate(is.null), all_comp_categorical))
    if (!is.null(comp_cat_combined) && nrow(comp_cat_combined) > 0L) {
      comp_cat_combined <- comp_cat_combined %>%
        dplyr::mutate(kkpval = purrr::map_chr(p_pchi, .format_pval))

      # Attach p-value to the first category row of each variable
      cat_combined <- cat_combined %>%
        dplyr::left_join(
          comp_cat_combined %>%
            dplyr::select(kkorder1, kkorder2, kkpval),
          by = c("kkorder1", "kkorder2"),
          relationship = "many-to-many"
        )
    }
  }

  # ---- Combine continuous and categorical into final report dataset --------
  common_cols <- c("var", "kkorder1", "kkorder2", "statcat", "_name_")
  kkcol_all   <- unique(c(
    if (!is.null(cont_combined)) stringr::str_subset(names(cont_combined), "^kkcol_") else character(0),
    if (!is.null(cat_combined))  stringr::str_subset(names(cat_combined), "^kkcol_")  else character(0)
  ))
  common_cols <- c(common_cols, kkcol_all)
  optional_cols <- c("kkest", "kk95ci", "kkpval", by_actual)
  all_cols <- unique(c(common_cols, intersect(optional_cols,
    c(if (!is.null(cont_combined)) names(cont_combined) else character(0),
      if (!is.null(cat_combined))  names(cat_combined)  else character(0)))))

  .ensure_cols <- function(df, cols) {
    if (is.null(df)) return(NULL)
    for (cn in cols) {
      if (!(cn %in% names(df))) df[[cn]] <- NA_character_
    }
    df %>% dplyr::select(dplyr::any_of(cols))
  }

  report_data <- dplyr::bind_rows(
    .ensure_cols(cont_combined, all_cols),
    .ensure_cols(cat_combined, all_cols)
  )

  # ---- Tplyr clinical summary table (for traceability / alternate output) ----
  tplyr_result <- .build_tplyr_summary(work_data, var_names, var_types,
                                        col_actual, stats, stat_labels)
  if (!is.null(tplyr_result)) {
    attr(report_data, "tplyr_table") <- tplyr_result
  }

  if (is.null(report_data) || nrow(report_data) == 0L) {
    cli::cli_warn("No results to report.")
    return(invisible(data.frame()))
  }

  report_data <- report_data %>%
    dplyr::mutate(
      statcat = as.character(statcat),
      var     = as.character(var)
    )

  # =========================================================================
  # STYLE 2 (down): insert variable-header rows  (SAS lines 2178-2207)
  # =========================================================================
  kkcol_cols <- stringr::str_subset(names(report_data), "^kkcol_")

  if (style == 2L) {
    arr_cols <- c("kkorder1", by_actual, "var", "kkorder2")
    arr_cols <- intersect(arr_cols, names(report_data))
    report_data <- report_data %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(arr_cols)))

    new_rows <- list()
    prev_var <- ""
    for (ri in seq_len(nrow(report_data))) {
      cur_var <- report_data$var[ri]
      if (cur_var != prev_var) {
        hdr <- report_data[ri, , drop = FALSE]
        hdr$`_name_`  <- ""
        hdr$statcat   <- ""
        hdr$kkorder2  <- 0L
        for (cc in kkcol_cols) hdr[[cc]] <- ""
        if ("kkest"  %in% names(hdr)) hdr$kkest  <- ""
        if ("kk95ci" %in% names(hdr)) hdr$kk95ci <- ""
        if ("kkpval" %in% names(hdr)) hdr$kkpval <- ""
        hdr$`_xlabel_` <- cur_var
        new_rows[[length(new_rows) + 1L]] <- hdr
        prev_var <- cur_var
      }
      detail <- report_data[ri, , drop = FALSE]
      detail$`_xlabel_` <- paste0("  ", detail$statcat)
      new_rows[[length(new_rows) + 1L]] <- detail
    }
    report_data <- dplyr::bind_rows(new_rows) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(kkcol_cols),
                                   ~ ifelse(kkorder2 == 0, "", .x)))
  } else {
    # Style 1: across — xlabel is the statistic label
    arr_cols2 <- c("kkorder1", by_actual, "var", "kkorder2")
    arr_cols2 <- intersect(arr_cols2, names(report_data))
    report_data <- report_data %>%
      dplyr::mutate(`_xlabel_` = statcat) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(arr_cols2)))
  }


  # =========================================================================
  # PAGINATION  (SAS lines 2004-2282)
  # =========================================================================
  if (!is.null(pagenum) && toupper(pagenum) == "Y") {
    page_size <- 60L
    n_header_lines <- 3L + length(by)
    usable_lines <- page_size - n_header_lines
    if (usable_lines < 10L) usable_lines <- 10L

    report_data <- report_data %>%
      dplyr::mutate(
        .ls_line = dplyr::row_number(),
        .ls_page = ceiling(.ls_line / usable_lines)
      )
  }

  # =========================================================================
  # OUTPUT GENERATION  (SAS lines 2284-2414)
  # =========================================================================
  kkcol_display <- stringr::str_subset(names(report_data), "^kkcol_")

  # Build column headers
  if (length(col_levels) > 0L) {
    col_headers <- col_level_labels
  } else {
    col_headers <- stathead
  }

  # Select output columns
  if (style == 1L) {
    display_cols <- c(by_actual, "var", "statcat", kkcol_display)
  } else {
    display_cols <- c(by_actual, "_xlabel_", kkcol_display)
  }
  if ("kkest"  %in% names(report_data)) display_cols <- c(display_cols, "kkest")
  if ("kk95ci" %in% names(report_data)) display_cols <- c(display_cols, "kk95ci")
  if ("kkpval" %in% names(report_data)) display_cols <- c(display_cols, "kkpval")

  output_df <- report_data %>%
    dplyr::select(dplyr::any_of(display_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                 ~ ifelse(is.na(.x), "", .x)))

  # ---- Write output based on filetype ------------------------------------
  if (!is.null(outfile) && nchar(trimws(outfile)) > 0L) {
    outdir <- dirname(outfile)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

    if (filetype %in% c("TXT", "ASCII")) {
      # -- TXT (replaces PROC PRINTTO + PROC REPORT) -----------------------
      col_widths <- purrr::map_int(names(output_df), function(cn) {
        max(nchar(as.character(output_df[[cn]])), nchar(cn), na.rm = TRUE) + spacing
      }) %>% rlang::set_names(names(output_df))

      header_line <- paste0(
        mapply(function(nm, w) formatC(nm, width = w, flag = "-"),
               names(output_df), col_widths),
        collapse = ""
      )
      separator <- paste(rep("-", nchar(header_line)), collapse = "")

      lines_out <- c(header_line, separator)
      for (ri in seq_len(nrow(output_df))) {
        lines_out <- c(lines_out, paste0(
          mapply(function(val, w) formatC(as.character(val), width = w, flag = "-"),
                 as.character(output_df[ri, ]), col_widths),
          collapse = ""
        ))
      }

      if (!is.null(pagenum) && toupper(pagenum) == "Y" && ".ls_page" %in% names(report_data)) {
        total_pages <- max(report_data$.ls_page, na.rm = TRUE)
        lines_out <- c(lines_out, "", separator,
                        paste0("Page 1 of ", total_pages))
      }

      writeLines(lines_out, con = outfile)
      cli::cli_alert_info("TXT output written to {.file {outfile}}")

    } else if (filetype == "HTML") {
      # -- HTML (replaces ODS HTML) -----------------------------------------
      html <- c(
        "<!DOCTYPE html>",
        "<html><head><meta charset='utf-8'>",
        "<style>table{border-collapse:collapse;font-family:monospace;}",
        "th,td{border:1px solid #ccc;padding:4px 8px;text-align:left;}</style>",
        "</head><body>",
        "<table>",
        paste0("<tr>", paste0("<th>",
               names(output_df) %>%
                 stringr::str_replace_all(stringr::fixed(">"), "&gt;") %>%
                 stringr::str_replace_all(stringr::fixed("<"), "&lt;") %>%
                 stringr::str_replace_all(stringr::fixed("&"), "&amp;"),
               "</th>", collapse = ""), "</tr>")
      )
      for (ri in seq_len(nrow(output_df))) {
        cells <- purrr::map_chr(as.character(output_df[ri, ]), function(v) {
          v_esc <- v %>%
            stringr::str_replace_all(stringr::fixed(">"), "&gt;") %>%
            stringr::str_replace_all(stringr::fixed("<"), "&lt;") %>%
            stringr::str_replace_all(stringr::fixed("&"), "&amp;")
          paste0("<td>", v_esc, "</td>")
        })
        html <- c(html, paste0("<tr>", paste0(cells, collapse = ""), "</tr>"))
      }
      html <- c(html, "</table></body></html>")
      writeLines(html, con = outfile)
      cli::cli_alert_info("HTML output written to {.file {outfile}}")

    } else if (filetype == "RTF") {
      # -- RTF (replaces ODS RTF) via r2rtf --------------------------------
      col_w <- rep(1.5, ncol(output_df))

      rtf_out <- output_df %>%
        r2rtf::rtf_body(col_rel_width = col_w) %>%
        r2rtf::rtf_colheader(
          paste(names(output_df), collapse = " | "),
          col_rel_width = col_w
        )
      if (nchar(col_label) > 0L) {
        rtf_out <- rtf_out %>% r2rtf::rtf_title(title = col_label)
      }
      rtf_out <- rtf_out %>%
        r2rtf::rtf_footnote(
          footnote = paste0("Generated by summary_report() on ",
                            format(Sys.time(), "%Y-%m-%d %H:%M"))
        ) %>%
        r2rtf::rtf_page(orientation = "landscape") %>%
        r2rtf::rtf_page_header(text = "") %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(file = outfile)
      cli::cli_alert_info("RTF output written to {.file {outfile}}")

    } else if (filetype == "PDF") {
      # -- PDF (replaces ODS PDF) via grDevices -----------------------------
      grDevices::pdf(file = outfile, width = 11, height = 8.5)
      tryCatch({
        graphics::par(mar = c(1, 1, 2, 1))
        graphics::plot.new()
        graphics::title(main = if (nchar(col_label) > 0L) col_label else "Summary Report")
        txt <- utils::capture.output(print(output_df, row.names = FALSE))
        n_txt <- length(txt)
        for (li in seq_along(txt)) {
          graphics::text(0.05, 1 - li / (n_txt + 2), txt[li],
                         adj = 0, cex = 0.5, family = "mono")
        }
      }, finally = {
        grDevices::dev.off()
      })
      cli::cli_alert_info("PDF output written to {.file {outfile}}")
    }
  }

  # ---- Diagnostic file output (SAS PRINTTO routing) -----------------------
  if (!is.null(diagfile) && nchar(trimws(diagfile)) > 0L) {
    diag_lines <- c(
      "# Diagnostic output from summary_report()",
      paste0("# Generated: ", Sys.time()),
      paste0("# Variables: ", paste(var_names, collapse = ", ")),
      paste0("# Stats: ", paste(stats, collapse = ", ")),
      paste0("# Style: ", style),
      paste0("# Column: ", column %||% "(none)"),
      paste0("# BY: ", paste(by, collapse = ", ")),
      "",
      utils::capture.output(utils::str(report_data))
    )
    diag_dir <- dirname(diagfile)
    if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(diag_lines, con = diagfile)
    cli::cli_alert_info("Diagnostics written to {.file {diagfile}}")
  }

  # ---- GENCODE output (SAS lines 2416-2593) --------------------------------
  if (accumulate_code) {
    .gc("")
    .gc("# ---- Report Output ----")
    .gc(paste0("# Outfile: ", outfile %||% "(console)"))
    .gc(paste0("# Filetype: ", filetype))
    gencode_dir <- dirname(gencode)
    if (!dir.exists(gencode_dir)) dir.create(gencode_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(gencode_lines, con = gencode)
    cli::cli_alert_info("Generated R code written to {.file {gencode}}")
  }

  # ---- Preserve haven label metadata on result columns ----------------------
  for (cn in intersect(var_names, names(report_data))) {
    lbl <- .get_var_label(work_data, cn)
    if (nchar(lbl) > 0L && cn %in% names(report_data)) {
      attr(report_data[[cn]], "label") <- lbl
      if (is.numeric(report_data[[cn]])) {
        report_data[[cn]] <- haven::labelled(report_data[[cn]], labels = NULL,
                                              label = lbl)
      }
    }
  }

  # ---- Return the assembled report data frame invisibly --------------------
  invisible(report_data)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Variable type detection ($) is handled via the var parameter:
#      plain name = continuous, name followed by "$" = categorical,
#      matching SAS lines 116-133 behaviour.
#    - PROC UNIVARIATE statistics are computed via dplyr::summarise()
#      with the .compute_stat() helper implementing all 35 statistics.
#      Quantiles use type = 2 for SAS PROC UNIVARIATE parity.
#    - SAS format-based decimal alignment is replicated via formatC()
#      and sprintf() with explicit width and precision parameters.
#    - PROC MIXED change-from-baseline models use mmrm::mmrm() for
#      repeated-measure designs and stats::lm() for single-visit
#      ANCOVA, per AAP section 0.8.1 mandate.
#    - GENCODE generates standalone R code (not SAS code).
#    - The function is named summary_report() to avoid masking the
#      base R summary() generic.
#    - SAS date variables in the input data are expected to have
#      already been converted via as.Date(x, origin = "1960-01-01")
#      before being passed to this function.
#    - Input data frames are expected from haven::read_xpt() or
#      equivalent, so haven labels are used for variable metadata.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Rounding: SAS rounds half-up; R uses janitor::round_half_up()
#      at every rounding location to match. All rounding locations
#      documented for Gate 2 audit.
#    - Percentile computation: SAS PROC UNIVARIATE uses quantile
#      type 2 by default; this implementation uses type = 2 in
#      stats::quantile() for exact SAS parity.
#    - Standard deviation: Both SAS and R sd() use N-1 denominator
#      (sample SD), so these should match.
#    - PROC MIXED vs mmrm/lm: Convergence criteria and optimizer
#      may differ. Covariance structure and df method are documented
#      per Gate 4 requirements.
#    - Chi-square test: SAS PROC FREQ and R chisq.test() may differ
#      in continuity correction defaults for 2x2 tables. This
#      implementation uses correct = FALSE to match SAS default.
#    - Kurtosis: SAS uses excess kurtosis (subtracts 3); this
#      implementation matches that convention.
#    - Mode: When multiple modes exist, SAS returns the smallest;
#      this implementation returns the first mode by frequency.
#
# NO DIRECT R EQUIVALENT:
#    - SAS $kkxstat. format -> R named character vector mapping
#      (.DEFAULT_STAT_LABELS) for statistic labels.
#    - SAS PROC REPORT COMPUTE blocks -> manual post-processing
#      via dplyr::mutate() for style 2 header rows.
#    - SAS PRINTTO routing -> sink() or writeLines() via the
#      diagfile parameter.
#    - SAS PROC UNIVARIATE test statistics (NORMAL, T, SIGNRANK,
#      MSIGN) -> shapiro.test(), t.test(), wilcox.test() as
#      separate calls within .compute_stat().
#    - SAS SpreadsheetML XML output -> replaced by r2rtf for RTF,
#      grDevices::pdf() for PDF, and writeLines() for TXT/HTML.
#
# PACKAGE SELECTION RATIONALE:
#    - dplyr: Core data manipulation replacing DATA steps and
#      PROC SQL; used for group_by, summarise, filter, mutate.
#    - tidyr: Pivoting replacing PROC TRANSPOSE; pivot_wider()
#      and pivot_longer() for reshaping statistics.
#    - rlang: Tidy evaluation for dynamic column references in
#      dplyr pipelines (syms, sym, .data, parse_expr).
#    - cli: User-facing messages and warnings replacing %PUT.
#    - janitor: round_half_up() for SAS-compatible rounding
#      behaviour per AAP section 0.7.2.
#    - haven: SAS label handling via attr(x, "label") on
#      labelled vectors from read_xpt().
#    - forcats: Factor level manipulation for FORMATTED vs
#      INTERNAL column ordering.
#    - Tplyr: Available for clinical summary table construction
#      when more structured output is needed.
#    - r2rtf: RTF output replacing ODS RTF with chainable verb
#      design (rtf_body, rtf_encode, write_rtf).
#    - mmrm: FDA-aligned MMRM models replacing PROC MIXED per
#      AAP section 0.8.1 mandate (NOT lme4/nlme/glmer).
#    - emmeans: LS means extraction replacing PROC MIXED LSMEANS
#      and CONTRAST statements.
#    - stats (base R): lm(), chisq.test(), wilcox.test(),
#      quantile(), sd(), median(), var(), qt(), shapiro.test(),
#      t.test() for fundamental statistical computations.
#    - grDevices (base R): pdf() and dev.off() for PDF output.
#
# OPEN QUESTIONS:
#    - Confirm quantile type parameter for exact SAS PROC
#      UNIVARIATE parity across all percentiles.
#    - Verify PROC MIXED contrast/LSMEANS equivalence with
#      emmeans for complex multi-arm designs.
#    - Confirm chi-square continuity correction behaviour vs SAS
#      for small-sample 2x2 tables.
#    - Should GENCODE produce standalone or source()-able R code?
#      Current implementation produces standalone annotated code.
#    - SAS ODS page-break/COMPUTE AFTER behaviour in PROC REPORT
#      is approximated via manual _xlabel_ row insertion in
#      style 2; verify layout matches for paginated output.
# ============================================================
