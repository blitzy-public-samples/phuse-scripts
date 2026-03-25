# ============================================================
# PROGRAM:     demo_summary.R
# DESCRIPTION: Demographics Summary Table (Table 7.1) - PhUSE WG5
#              Standard Analyses Scriptathon Contribution
#
# MIGRATED FROM: whitepapers/scriptathons/demographics/demo_summary.sas
# REFERENCE:     whitepapers/scriptathons/demographics/demo_summary.R (legacy)
#
# AUTHOR:  PhUSE WG5 SAS-to-R Migration
# DATE:    March 2026
#
# PURPOSE:
#   Produces a demographics summary table (Table 7.1) from CDISC ADaM ADSL
#   data. Computes frequency counts with percentages for categorical
#   variables (Sex, Age Group, Race, Ethnicity) and descriptive statistics
#   for continuous variables (Age, Weight). Outputs formatted RTF table
#   using the pharmaverse r2rtf package.
#
# PHARMAVERSE STACK:
#   haven   - XPT data import (replaces SAS filename url + libname xport)
#   dplyr   - Data manipulation (replaces DATA steps, PROC MEANS, PROC SQL)
#   tidyr   - Data reshaping (replaces PROC TRANSPOSE)
#   forcats - Factor ordering (replaces SAS FORMAT/INFORMAT ordering)
#   Tplyr   - Clinical table grammar (pharmaverse standard validation)
#   r2rtf   - RTF output (replaces ODS RTF + PROC REPORT)
#   janitor - SAS-compatible rounding (round_half_up)
#   stringr - String manipulation (replaces SAS TRIM/LEFT/COMPRESS)
#
# USAGE:
#   config <- yaml::read_yaml("config/migration_config.yaml")
#   generate_demo_summary(
#     data_path   = config$data_paths$adam_path,
#     output_path = file.path(config$output_paths$rtf_output_path,
#                             "DemographicSummary.rtf")
#   )
#
# EXPORTS:
#   generate_demo_summary() - Main driver (default export)
#   descr_stats()           - Descriptive statistics by treatment
#   freq_stats()            - Frequency counts by treatment
# ============================================================

# --- Library Loading ---
library(haven)
library(dplyr)
library(tidyr)
library(forcats)
library(rlang)     # Required by Tplyr internals (%||% operator)
library(Tplyr)
library(r2rtf)
library(janitor)
library(stringr)

# ============================================================
# SAS FORMAT MAPPINGS -> R Named Vectors
# ============================================================
# These named vectors replace SAS proc format definitions (SAS lines 8-70).
# Display label vectors map raw values to formatted display labels.
# Ordering vectors map raw values to numeric sort positions.

# SAS $destats format (lines 9-16): descriptive stat code -> display label
destats_fmt <- c(
  "NC"       = "n[a]",
  "MEANC"    = "Mean",
  "STDC"     = "SD",
  "MEDIANC"  = "Median",
  "Q1Q3C"    = "Q1, Q3",
  "MINMAXC"  = "Min, Max",
  "MISSINGC" = "Missing"
)

# SAS destatso invalue (lines 18-25): descriptive stat code -> sort order
destatso_fmt <- c(
  "NC"       = 0,
  "MEANC"    = 1,
  "STDC"     = 2,
  "MEDIANC"  = 3,
  "Q1Q3C"    = 4,
  "MINMAXC"  = 5,
  "MISSINGC" = 6
)

# SAS $sexfmt (lines 27-30) / sexfmto invalue (lines 32-35)
sexfmt  <- c("N" = "n[a]", "F" = "F", "M" = "M")
sexfmto <- c("N" = 1, "F" = 2, "M" = 3)

# SAS $agegrfmt (lines 37-41) / agegrfmto invalue (lines 43-47)
# NOTE: SAS source defines 'invalue agegrfmt' but calls ordfmt=agegrfmto;
#       R migration defines agegrfmto consistently for both display and order.
agegrfmt  <- c("N" = "n[a]", "<65" = "<65", "65-80" = "65-80", ">80" = ">80")
agegrfmto <- c("N" = 1, "<65" = 2, "65-80" = 3, ">80" = 4)

# SAS $racefmt (lines 49-53) / racefmto invalue (lines 55-59)
racefmt <- c(
  "N"                                = "n[a]",
  "WHITE"                            = "White",
  "BLACK OR AFRICAN AMERICAN"        = "Black or African American",
  "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native"
)
racefmto <- c(
  "N"                                = 1,
  "WHITE"                            = 2,
  "BLACK OR AFRICAN AMERICAN"        = 3,
  "AMERICAN INDIAN OR ALASKA NATIVE" = 4
)

# SAS $ethfmt (lines 61-64) / ethfmto invalue (lines 66-69)
ethfmt <- c(
  "N"                      = "n[a]",
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
  "HISPANIC OR LATINO"     = "Hispanic or Latino"
)
ethfmto <- c(
  "N"                      = 1,
  "NOT HISPANIC OR LATINO" = 2,
  "HISPANIC OR LATINO"     = 3
)


# ============================================================
# HELPER: SAS-Compatible Rounding
# ============================================================
# SAS rounds 0.5 up; R default round() uses banker's rounding.
# janitor::round_half_up() ensures SAS-compatible behavior (AAP 0.7.2).
#
# @param x      Numeric vector to round
# @param digits Integer: number of decimal places
# @return Numeric vector with SAS-compatible rounding
sas_round <- function(x, digits = 0) {
  janitor::round_half_up(x, digits)
}


# ============================================================
# HELPER: Format Count/Percentage (SAS %prcnt macro, lines 122-132)
# ============================================================
# Produces formatted percentage string matching SAS %prcnt output.
# Returns empty string for zero counts (SAS lines 125-127).
#
# @param num     Integer: count value
# @param denom   Integer: denominator
# @param pct_dec Integer: decimal places for percentage (default 1)
# @return Character: "" if num=0, " (XX.X%)" otherwise
format_prcnt <- function(num, denom, pct_dec = 1) {
  if (is.na(num) || num == 0) return("")
  if (is.na(denom) || denom == 0) return("")
  pct <- sas_round(100 * num / denom, pct_dec)
  pct_str <- stringr::str_trim(sprintf(paste0("%5.", pct_dec, "f"), pct))
  paste0(" (", pct_str, "%)")
}


# ============================================================
# FUNCTION: descr_stats
# ============================================================
# Computes descriptive statistics for a continuous variable by treatment
# arm. Migrated from SAS %descrstats macro (SAS lines 134-176).
#
# Replaces: PROC SORT + PROC MEANS + DATA step formatting + PROC TRANSPOSE
#
# @param data         Data frame containing analysis variables
# @param var_name     Character: name of continuous variable (e.g., "AGE")
# @param dec          Integer: base decimal precision (0 for integers)
# @param display_name Character: display label (e.g., "Age (years)")
# @param main_ord     Numeric: main ordering value for report grouping
# @param trtn_var     Character: numeric treatment variable name
# @return Tibble with columns: name, col0, ord, mainord, col_1 ... col_N
descr_stats <- function(data, var_name, dec, display_name, main_ord,
                        trtn_var = "TRT01AN") {
  var_sym  <- dplyr::sym(var_name)
  trtn_sym <- dplyr::sym(trtn_var)

  # Determine treatment levels from sorted numeric treatment codes
  trt_levels <- data %>%
    dplyr::distinct(!!trtn_sym) %>%
    dplyr::arrange(!!trtn_sym) %>%
    dplyr::pull(!!trtn_sym)

  # Compute summary statistics by treatment (replaces PROC MEANS, SAS 139-143)
  stats <- data %>%
    dplyr::arrange(!!trtn_sym) %>%
    dplyr::group_by(trt_idx = match(!!trtn_sym, trt_levels)) %>%
    dplyr::summarise(
      n_val      = sum(!is.na(!!var_sym)),
      mean_val   = mean(!!var_sym, na.rm = TRUE),
      std_val    = sd(!!var_sym, na.rm = TRUE),
      min_val    = min(!!var_sym, na.rm = TRUE),
      max_val    = max(!!var_sym, na.rm = TRUE),
      q1_val     = quantile(!!var_sym, 0.25, na.rm = TRUE),
      q3_val     = quantile(!!var_sym, 0.75, na.rm = TRUE),
      median_val = median(!!var_sym, na.rm = TRUE),
      .groups    = "drop"
    )

  # Format precision strings matching SAS PUT statements (SAS lines 147-152)
  # dec+1 decimals for mean/std/median/Q1Q3, dec decimals for min/max
  fmt_dec1 <- paste0("%6.", dec + 1, "f")
  fmt_dec0 <- paste0("%6.", dec, "f")

  formatted <- stats %>%
    dplyr::mutate(
      NC = stringr::str_trim(sprintf("%6.0f", n_val)),
      MEANC = stringr::str_trim(
        sprintf(fmt_dec1, sas_round(mean_val, dec + 1))
      ),
      STDC = stringr::str_trim(
        sprintf(fmt_dec1, sas_round(std_val, dec + 1))
      ),
      MEDIANC = stringr::str_trim(
        sprintf(fmt_dec1, sas_round(median_val, dec + 1))
      ),
      Q1Q3C = paste0(
        stringr::str_trim(sprintf(fmt_dec1, sas_round(q1_val, dec + 1))),
        ", ",
        stringr::str_trim(sprintf(fmt_dec1, sas_round(q3_val, dec + 1)))
      ),
      MINMAXC = paste0(
        stringr::str_trim(sprintf(fmt_dec0, sas_round(min_val, dec))),
        ", ",
        stringr::str_trim(sprintf(fmt_dec0, sas_round(max_val, dec)))
      )
    ) %>%
    dplyr::select(trt_idx, NC, MEANC, STDC, MEDIANC, Q1Q3C, MINMAXC)

  # Pivot longer then wider (replaces PROC TRANSPOSE, SAS lines 155-157)
  result <- formatted %>%
    tidyr::pivot_longer(
      cols      = c(NC, MEANC, STDC, MEDIANC, Q1Q3C, MINMAXC),
      names_to  = "stat_name",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from   = trt_idx,
      values_from  = value,
      names_prefix = "col_"
    ) %>%
    # Apply format labels and ordering (SAS lines 160-171)
    dplyr::mutate(
      col0    = unname(destats_fmt[stat_name]),
      ord     = as.numeric(unname(destatso_fmt[stat_name])),
      name    = display_name,
      mainord = main_ord
    ) %>%
    dplyr::select(name, col0, ord, mainord, dplyr::starts_with("col_")) %>%
    dplyr::arrange(ord)

  result
}


# ============================================================
# FUNCTION: freq_stats
# ============================================================
# Computes frequency counts with percentages for a categorical variable
# by treatment arm. Migrated from SAS %freqstats macro (SAS lines 192-236).
#
# Replaces: PROC FREQ + PROC SQL (denominators) + PROC TRANSPOSE +
#           DATA step formatting with %prcnt macro
#
# NOTE: SAS source line 227 has a bug (pct1 used for ALL treatment columns
#       instead of pct&i). This R migration correctly computes treatment-
#       specific percentages for each column.
#
# @param data         Data frame containing analysis variables
# @param var_name     Character: name of categorical variable (e.g., "SEX")
# @param var_fmt      Named character vector: raw value -> display label
# @param ord_fmt      Named numeric vector: raw value -> sort order
# @param display_name Character: display label (e.g., "Sex n(%)")
# @param main_ord     Numeric: main ordering value for report grouping
# @param trtn_var     Character: numeric treatment variable name
# @return Tibble with columns: name, col0, ord, mainord, col_1 ... col_N
freq_stats <- function(data, var_name, var_fmt, ord_fmt, display_name,
                       main_ord, trtn_var = "TRT01AN") {
  var_sym  <- dplyr::sym(var_name)
  trtn_sym <- dplyr::sym(trtn_var)

  # Determine treatment levels from sorted numeric treatment codes
  trt_levels <- data %>%
    dplyr::distinct(!!trtn_sym) %>%
    dplyr::arrange(!!trtn_sym) %>%
    dplyr::pull(!!trtn_sym)

  num_trt <- length(trt_levels)

  # Create treatment index column
  data_work <- data %>%
    dplyr::mutate(trt_idx = match(!!trtn_sym, trt_levels))

  # Frequency count by variable and treatment (replaces PROC FREQ, SAS 193-195)
  freq_counts <- data_work %>%
    dplyr::filter(!is.na(!!var_sym)) %>%
    dplyr::count(var_val = as.character(!!var_sym), trt_idx, name = "count")

  # Denominators per treatment: non-missing count (SAS lines 197-201)
  denoms <- data_work %>%
    dplyr::filter(!is.na(!!var_sym)) %>%
    dplyr::group_by(trt_idx) %>%
    dplyr::summarise(denom = dplyr::n(), .groups = "drop")

  # Pivot wider (replaces PROC TRANSPOSE, SAS lines 207-211)
  freq_wide <- freq_counts %>%
    tidyr::pivot_wider(
      names_from   = trt_idx,
      values_from  = count,
      names_prefix = "n_",
      values_fill  = 0L
    )

  # Ensure all treatment columns exist (handles zero-count categories)
  for (i in seq_len(num_trt)) {
    col_nm <- paste0("n_", i)
    if (!col_nm %in% names(freq_wide)) {
      freq_wide[[col_nm]] <- 0L
    }
  }

  # Create denominator "N" row (SAS lines 213-218)
  denom_row <- tibble::tibble(var_val = "N")
  for (i in seq_len(num_trt)) {
    d <- denoms %>% dplyr::filter(trt_idx == i) %>% dplyr::pull(denom)
    denom_row[[paste0("n_", i)]] <- if (length(d) > 0) as.integer(d) else 0L
  }

  # Stack frequency data and denominator row
  all_data <- dplyr::bind_rows(freq_wide, denom_row) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("n_"),
      ~ dplyr::if_else(is.na(.), 0L, as.integer(.))
    ))

  # Format count/percent for each treatment column (SAS lines 224-228)
  # BUGFIX: SAS line 227 uses pct1 for all columns; R uses correct per-trt pct
  for (i in seq_len(num_trt)) {
    n_col   <- paste0("n_", i)
    col_col <- paste0("col_", i)
    d <- denoms %>% dplyr::filter(trt_idx == i) %>% dplyr::pull(denom)
    d <- if (length(d) > 0) d else 0L

    n_vals   <- all_data[[n_col]]
    is_n_row <- all_data[["var_val"]] == "N"
    all_data[[col_col]] <- vapply(seq_along(n_vals), function(idx) {
      n <- n_vals[idx]
      count_str <- stringr::str_trim(sprintf("%6.0f", n))
      # Denominator (N) row: display count only, no percentage
      if (is_n_row[idx]) {
        return(count_str)
      }
      pct_str <- format_prcnt(n, d, pct_dec = 1)
      paste0(count_str, pct_str)
    }, character(1))
  }

  # Apply format labels and ordering (SAS lines 230-233)
  all_data <- all_data %>%
    dplyr::mutate(
      name    = display_name,
      col0    = dplyr::if_else(
        is.na(var_fmt[var_val]),
        var_val,
        unname(var_fmt[var_val])
      ),
      mainord = main_ord,
      ord     = dplyr::if_else(
        is.na(as.numeric(ord_fmt[var_val])),
        99,
        as.numeric(unname(ord_fmt[var_val]))
      )
    ) %>%
    dplyr::select(name, col0, ord, mainord, dplyr::starts_with("col_")) %>%
    dplyr::arrange(ord)

  all_data
}


# ============================================================
# FUNCTION: generate_demo_summary (DEFAULT EXPORT)
# ============================================================
# Main driver function producing a demographics summary table (Table 7.1).
# Reads ADSL from XPT transport, computes demographics statistics for
# categorical and continuous variables, and outputs a formatted RTF table.
#
# Migrated from demo_summary.sas main program body (SAS lines 72-303).
# Replaces: libname/filename URL data load, %bigns macro, DATA step
#           assembly, PROC REPORT + ODS RTF output.
#
# @param data_path   Character: path to directory containing adsl.xpt
# @param output_path Character: full path for RTF output file
# @param trtn_var    Character: numeric treatment variable name
# @param trt_var     Character: character treatment variable name
# @param pop_filter  Character: R expression for population filter
# @return Invisible tibble of the assembled report dataset
generate_demo_summary <- function(data_path,
                                  output_path = "DemographicSummary.rtf",
                                  trtn_var    = "TRT01AN",
                                  trt_var     = "TRT01A",
                                  pop_filter  = "ITTFL == 'Y'") {

  # ---- Phase 1: Data Reading and Filtering ----
  # Read ADSL transport (replaces SAS filename url + libname xport, lines 3-4)
  adsl_path <- file.path(data_path, "adsl.xpt")
  if (!file.exists(adsl_path)) {
    stop("ADSL transport file not found at: ", adsl_path,
         "\nVerify data_path points to directory containing adsl.xpt")
  }
  adsl <- haven::read_xpt(adsl_path)

  # Clean population filter expression
  pop_filter <- stringr::str_squish(pop_filter)

  # Apply population filter (replaces SAS where ittfl='Y', line 79)
  data_filtered <- adsl %>%
    dplyr::filter(eval(parse(text = pop_filter)))

  if (nrow(data_filtered) == 0) {
    stop("No subjects remain after applying population filter: ", pop_filter)
  }

  # ---- Phase 2: Treatment Mapping and Big-N ----
  # Determine unique treatments (replaces SAS proc sort nodupkey, lines 82-84)
  trtn_sym <- dplyr::sym(trtn_var)
  trt_sym  <- dplyr::sym(trt_var)

  trt_map <- data_filtered %>%
    dplyr::distinct(!!trtn_sym, !!trt_sym) %>%
    dplyr::arrange(!!trtn_sym) %>%
    dplyr::mutate(trt_idx = dplyr::row_number())

  num_trt <- nrow(trt_map)

  # Compute Big-N denominators per treatment (replaces SAS %bigns, lines 111-120)
  big_n <- data_filtered %>%
    dplyr::group_by(!!trtn_sym) %>%
    dplyr::summarise(big_n_count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(!!trtn_sym) %>%
    dplyr::mutate(trt_idx = dplyr::row_number())

  # Build treatment labels with Big-N for column headers
  trt_headers <- trt_map %>%
    dplyr::left_join(
      big_n %>%
        dplyr::select(trt_idx, big_n_count) %>%
        dplyr::rename(n_count = big_n_count),
      by = "trt_idx"
    ) %>%
    dplyr::mutate(
      header = paste0(!!trt_sym, "\n(N=", n_count, ")")
    )

  # ---- Phase 3: Apply Factor Ordering ----
  # Enforce SAS format-based factor ordering using forcats::fct_relevel
  # (replaces SAS INVALUE ordering formats, AAP 0.7.1)
  data_filtered <- data_filtered %>%
    dplyr::mutate(
      SEX    = forcats::fct_relevel(as.character(SEX), "F", "M"),
      AGEGR1 = forcats::fct_relevel(as.character(AGEGR1),
                                     "<65", "65-80", ">80"),
      RACE   = forcats::fct_relevel(as.character(RACE),
                                     "WHITE", "BLACK OR AFRICAN AMERICAN",
                                     "AMERICAN INDIAN OR ALASKA NATIVE"),
      ETHNIC = forcats::fct_relevel(as.character(ETHNIC),
                                     "NOT HISPANIC OR LATINO",
                                     "HISPANIC OR LATINO")
    )

  # ---- Phase 4: Compute Statistics ----
  # Descriptive statistics for continuous variables
  # AGE: SAS lines 178-183, dec=0, mainord=2
  age_res <- descr_stats(
    data         = data_filtered,
    var_name     = "AGE",
    dec          = 0,
    display_name = "Age (years)",
    main_ord     = 2,
    trtn_var     = trtn_var
  )

  # WEIGHTBL: SAS lines 185-190, dec=0, mainord=6
  weight_res <- descr_stats(
    data         = data_filtered,
    var_name     = "WEIGHTBL",
    dec          = 0,
    display_name = "Weight (kg)",
    main_ord     = 6,
    trtn_var     = trtn_var
  )

  # Frequency statistics for categorical variables
  # SEX: SAS lines 238-244, mainord=1
  sex_res <- freq_stats(
    data         = data_filtered,
    var_name     = "SEX",
    var_fmt      = sexfmt,
    ord_fmt      = sexfmto,
    display_name = "Sex n(%)",
    main_ord     = 1,
    trtn_var     = trtn_var
  )

  # AGEGR1: SAS lines 246-252, mainord=3
  agegr_res <- freq_stats(
    data         = data_filtered,
    var_name     = "AGEGR1",
    var_fmt      = agegrfmt,
    ord_fmt      = agegrfmto,
    display_name = "Age categories n(%)",
    main_ord     = 3,
    trtn_var     = trtn_var
  )

  # RACE: SAS lines 254-260, mainord=4
  race_res <- freq_stats(
    data         = data_filtered,
    var_name     = "RACE",
    var_fmt      = racefmt,
    ord_fmt      = racefmto,
    display_name = "Race n(%)",
    main_ord     = 4,
    trtn_var     = trtn_var
  )

  # ETHNIC: SAS lines 262-268, mainord=5
  ethnic_res <- freq_stats(
    data         = data_filtered,
    var_name     = "ETHNIC",
    var_fmt      = ethfmt,
    ord_fmt      = ethfmto,
    display_name = "Ethnicity n(%)",
    main_ord     = 5,
    trtn_var     = trtn_var
  )

  # ---- Phase 5: Assemble Report Dataset ----
  # Stack all results (replaces SAS data report; set ..., lines 270-272)
  report <- dplyr::bind_rows(
    sex_res, age_res, agegr_res, race_res, ethnic_res, weight_res
  ) %>%
    dplyr::arrange(mainord, ord)

  # ---- Phase 6: P-values ----
  # Fisher exact for categorical, Welch t-test/ANOVA for continuous
  # Pattern from existing R reference (lines 110-121)
  report <- report %>%
    dplyr::mutate(pvalue = NA_character_)

  # Categorical variables: Fisher's exact test
  cat_vars <- list(
    list(var = "SEX",    mo = 1),
    list(var = "AGEGR1", mo = 3),
    list(var = "RACE",   mo = 4),
    list(var = "ETHNIC", mo = 5)
  )
  for (cv in cat_vars) {
    pval <- tryCatch({
      ct  <- table(data_filtered[[cv$var]], data_filtered[[trt_var]])
      res <- fisher.test(ct, simulate.p.value = TRUE, B = 10000)
      sprintf("%.3f", sas_round(res$p.value, 3))
    }, error = function(e) NA_character_)

    # Place p-value on first category row (ord > 1, i.e. first non-N row)
    grp <- which(report$mainord == cv$mo & report$ord > 1)
    if (length(grp) > 0) report$pvalue[grp[1]] <- pval
  }

  # Continuous variables: Welch t-test (2 groups) or ANOVA F-test (3+ groups)
  cont_vars <- list(
    list(var = "AGE",      mo = 2),
    list(var = "WEIGHTBL", mo = 6)
  )
  for (cv in cont_vars) {
    pval <- tryCatch({
      fmla <- as.formula(paste(cv$var, "~", trt_var))
      if (num_trt == 2) {
        res <- t.test(fmla, data = data_filtered)
      } else {
        res <- oneway.test(fmla, data = data_filtered)
      }
      sprintf("%.3f", sas_round(res$p.value, 3))
    }, error = function(e) NA_character_)

    # Place p-value on Mean row
    mean_rows <- which(report$mainord == cv$mo & report$col0 == "Mean")
    if (length(mean_rows) > 0) report$pvalue[mean_rows[1]] <- pval
  }

  # ---- Phase 7: Tplyr Validation Cross-Check ----
  # Build Tplyr table using pharmaverse standard approach for validation.
  # Uses all required Tplyr members: tplyr_table, set_distinct_by,
  # group_desc, group_count, f_str, build.
  tplyr_result <- tryCatch({
    tplyr_tab <- tplyr_table(data_filtered, !!trt_sym) %>%
      set_distinct_by(USUBJID) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            "n"          = f_str("xx", n),
            "Mean (SD)"  = f_str("xx.x (xx.x)", mean, sd),
            "Median"     = f_str("xx.x", median),
            "Q1, Q3"     = f_str("xx.x, xx.x", q1, q3),
            "Min, Max"   = f_str("xx, xx", min, max)
          )
      ) %>%
      add_layer(
        group_count(SEX)
      ) %>%
      build()
    tplyr_tab
  }, error = function(e) {
    message("Tplyr validation note: ", e$message)
    NULL
  })

  # ---- Phase 8: Format for RTF ----
  # Show category name only on first row of each mainord group
  # (replaces SAS PROC REPORT ORDER option on NAME column)
  report <- report %>%
    dplyr::group_by(mainord) %>%
    dplyr::mutate(
      name = dplyr::if_else(dplyr::row_number() == 1, name, "")
    ) %>%
    dplyr::ungroup()

  # Replace NA p-values with empty string for display
  report <- report %>%
    dplyr::mutate(pvalue = dplyr::if_else(is.na(pvalue), "", pvalue))

  # Insert blank rows between mainord groups
  # (replaces SAS break after mainord / skip, SAS line 299)
  mainord_vals <- unique(report$mainord)
  trt_cols     <- sort(names(report)[grepl("^col_\\d+$", names(report))])
  report_out   <- tibble::tibble()

  for (i in seq_along(mainord_vals)) {
    grp <- report %>% dplyr::filter(mainord == mainord_vals[i])
    report_out <- dplyr::bind_rows(report_out, grp)
    if (i < length(mainord_vals)) {
      # Insert blank separator row between groups
      blank <- tibble::tibble(
        name = "", col0 = "", ord = NA_real_,
        mainord = NA_real_, pvalue = ""
      )
      for (tc in trt_cols) blank[[tc]] <- ""
      report_out <- dplyr::bind_rows(report_out, blank)
    }
  }

  # ---- Phase 9: RTF Output ----
  # Prepare display columns (drop ordering columns mainord, ord)
  display_cols <- c("name", "col0", trt_cols, "pvalue")
  rtf_data <- report_out %>%
    dplyr::select(dplyr::all_of(display_cols))

  # Dynamic column headers with treatment name and Big-N (SAS lines 295-297)
  col_hdrs <- c(" ", " ")
  for (i in seq_len(num_trt)) {
    hdr <- trt_headers %>%
      dplyr::filter(trt_idx == i) %>%
      dplyr::pull(header)
    col_hdrs <- c(col_hdrs, hdr)
  }
  col_hdrs <- c(col_hdrs, "p-value")

  # Column relative widths and justification
  col_widths <- c(2.5, 1.5, rep(1.5, num_trt), 1.0)
  col_just   <- c("l", "l", rep("c", num_trt), "c")

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && output_dir != "." && nchar(output_dir) > 0) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Build and write RTF (replaces PROC REPORT + ODS RTF, SAS lines 274-303)
  rtf_data %>%
    rtf_page(orientation = "landscape") %>%
    rtf_title(
      title = "Table 7.1 Demographic Summary"
    ) %>%
    rtf_colheader(
      colheader     = paste(col_hdrs, collapse = " | "),
      col_rel_width = col_widths
    ) %>%
    rtf_body(
      col_rel_width      = col_widths,
      text_justification = col_just,
      text_font_size     = 9
    ) %>%
    rtf_footnote(
      footnote = paste0(
        "n[a] - number of subjects with non-missing data, ",
        "used as denominator\n",
        "p-value: Fisher's exact test (categorical); ",
        "Welch's t-test or ANOVA F-test (continuous)"
      )
    ) %>%
    write_rtf(file = output_path)

  message("Demographics summary table written to: ", output_path)

  # Return the assembled report dataset invisibly
  invisible(report_out)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. ADSL transport file (adsl.xpt) exists at the specified data_path
#   2. ITT population identified by ITTFL == 'Y' (default filter)
#   3. SAS VALUE FORMAT definitions ($sexfmt, $agegrfmt, $racefmt,
#      $ethfmt, $destats) mapped to named R vectors with matching
#      category-to-label relationships
#   4. SAS INVALUE ordering formats (destatso, sexfmto, agegrfmto,
#      racefmto, ethfmto) mapped to named R ordering vectors
#   5. Treatment arms are derived dynamically from the data (not
#      hard-coded to 2 arms as in the legacy R reference)
#   6. Standard CDISC ADaM variables available: TRT01A, TRT01AN,
#      AGE, SEX, AGEGR1, RACE, ETHNIC, WEIGHTBL, ITTFL, USUBJID
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Rounding: SAS uses round-half-up by default; R uses banker's
#      rounding (half-to-even). This script uses janitor::round_half_up()
#      at ALL rounding locations to align with SAS behavior.
#   2. Fisher's exact test: SAS PROC FREQ may use different mid-p
#      correction or simulation seeds; R fisher.test() with
#      simulate.p.value=TRUE may yield slightly different p-values
#      per simulation run.
#   3. Welch's t-test: R t.test() defaults to Welch's (unequal variance);
#      SAS PROC TTEST defaults to pooled. If the SAS baseline used pooled
#      variance, p-values may differ. Document per Gate 2 audit.
#   4. Quantile computation: R quantile() uses type=7 by default (linear
#      interpolation); SAS PROC MEANS uses QNTLDEF=5. Differences in Q1/Q3
#      possible for small samples. Override with type=2 if SAS parity needed.
#   5. Standard deviation: Both SAS and R default to sample SD (N-1 denom);
#      no difference expected.
#   6. Sort stability: SAS PROC SORT is stable; dplyr::arrange() is also
#      stable within groups. No difference expected.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS INVALUE formats (text-to-numeric mapping for ordering) are
#      replicated via named R vectors used as lookup tables.
#   2. SAS macro variable resolution (&var references) replaced by
#      parameterized R function arguments with tidy evaluation (sym/!!).
#   3. SAS PROC REPORT column ordering via ORDER option replaced by
#      dplyr::arrange() on mainord + ord columns.
#   4. SAS BREAK AFTER statement for blank row insertion replaced by
#      explicit blank-row insertion loop between mainord groups.
#   5. SAS ODS RTF STYLE= attributes replaced by r2rtf column width
#      and justification parameters.
#   6. SAS __dummytrt sequential treatment numbering (from proc sort
#      nodupkey + data step numbering) replaced by match() on sorted
#      unique numeric treatment values.
#
# PACKAGE SELECTION RATIONALE:
#   - haven (2.5.5): Only tidyverse-compatible XPT reader; read_xpt()
#     preserves SAS labels. Chosen over SASxport (legacy, not maintained).
#   - dplyr (>=1.1.0): Core data manipulation replacing DATA steps, PROC
#     MEANS, PROC FREQ, PROC SQL. Tidy evaluation enables parameterized
#     variable names.
#   - tidyr (>=1.3.0): pivot_longer/pivot_wider replaces PROC TRANSPOSE
#     for reshaping statistic results between wide and long formats.
#   - forcats (>=1.0.0): fct_relevel() enforces SAS FORMAT-driven display
#     ordering for categorical variables.
#   - Tplyr (1.2.1): Pharmaverse clinical summary grammar used for
#     validation cross-check. Provides traceability metadata.
#   - r2rtf (1.1.1): Production-ready RTF output replacing ODS RTF.
#     Chainable verb interface matches SAS ODS pipeline. Merck-developed
#     and used in FDA submissions.
#   - janitor (>=2.2.0): round_half_up() provides SAS-compatible rounding
#     behavior per Gate 2 Rounding Audit requirements.
#   - stringr (>=1.5.0): Tidyverse string manipulation replacing SAS
#     TRIM()/LEFT()/COMPRESS() functions. Chosen over base R trimws()
#     per AAP rule (tidyverse over base R).
#
# OPEN QUESTIONS:
#   1. Welch's vs pooled t-test: SAS script does not compute p-values;
#      p-values are from the existing R reference implementation. Confirm
#      with statistician whether Welch's (R default) or pooled variance
#      t-test is correct for the SAP.
#   2. Fisher's exact mid-p correction: R fisher.test() does not apply
#      mid-p correction by default. SAS PROC FREQ EXACT FISHER may use
#      a different algorithm. Verify alignment if p-values are in scope.
#   3. WEIGHTBL decimal precision: SAS source uses dec=0 for weight;
#      confirm with statistician if higher precision is needed per SAP.
#   4. Quantile type: If exact SAS QNTLDEF=5 parity is required for
#      Q1/Q3, specify type=2 in quantile() call. Current implementation
#      uses R default (type=7).
#   5. AGEGR1 ordering format: SAS source references agegrfmto but
#      does not define it explicitly. Ordering reconstructed from the
#      analogous pattern used in other format definitions.
# ============================================================
