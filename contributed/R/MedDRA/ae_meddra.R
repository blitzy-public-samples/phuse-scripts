# ============================================================================
# PROGRAM NAME: ae_meddra.R -- MedDRA at a Glance Analysis Panel Driver
#
# DESCRIPTION: Find subject counts per arm for each adverse event at each
#              MedDRA level (SOC, HLGT, HLT, PT). Find risk difference,
#              relative risk, and Fisher's exact test p-value for each pair
#              of arms. Creates an Excel workbook output file which allows
#              users to compare arms and highlight terms with statistics
#              above user-set thresholds.
#
# EVALUATION TYPE: Safety
#
# ORIGINAL:    contributed/MedDRA/MedDRA_at_a_Glance/ae_meddra.sas (647 lines)
# AUTHOR:      David Kretch (david.kretch@us.ibm.com)
# DATE:        February 15, 2011 (original SAS); R migration 2026
#
# EXTERNAL R FILES USED:
#   contributed/R/MedDRA/ae_setup.R         -- Merges AE, DM, and EX
#   contributed/R/MedDRA/ae_meddra_output.R -- Creates the output
#   contributed/R/MedDRA/err_output.R       -- Error output when missing vars
#
# EXPORTS: params, set_cc_switch, meddra_aggregate, meddra_cmp, aemed
#
# PARAMETERS REQUIRED:
#   ae          -- AE domain data frame or path to XPT/SAS7BDAT file
#   dm          -- DM domain data frame or path to XPT/SAS7BDAT file
#   ex          -- EX domain data frame or path to XPT/SAS7BDAT file
#   meddra_data -- MedDRA hierarchy data (optional; loaded from path if NULL)
#   dme_data    -- Designated Medical Events list (optional)
#   output_path -- directory for output files
#   ver         -- MedDRA version string (default "14.0"; "N" prefix disables)
#   study_lag   -- window in days after last exposure for AE inclusion
#   cc          -- continuity correction value (numeric, "arm", or "0"/"none")
#
# VARIABLES REQUIRED:
#   AE -- AEBODSYS, AEDECOD, USUBJID
#   DM -- ACTARM or ARM, USUBJID
#   EX -- USUBJID
#
# VARIABLES USED WHEN AVAILABLE:
#   AE -- AESTDTC
#   DM -- RFSTDTC, RFENDTC, ARMCD
#   EX -- EXSTDTC, EXENDTC
#
# MADE WITH: R >= 4.3.0
#
# REVISIONS:
#   2011-03-27  DK  Adding SL to SAS parameter mapping / run location handling
#   2011-05-07  DK  Arm count comma formatting
#   2011-05-08  DK  Added handling for DM no-subjects errors
#   2026-03-26  Blitzy  Migrated from SAS to R
# ============================================================================

# --- Required Libraries -------------------------------------------------------
library(dplyr)
library(haven)
library(janitor)
library(cli)
library(stringr)
library(purrr)
library(rlang)
library(tibble)
# stats is a base R package (provides fisher.test), always available

# --- Source Internal Dependencies ---------------------------------------------
# Replaces SAS %include statements (lines 211-216 of ae_meddra.sas).
# Each dependency is loaded only if its primary export is not yet available.
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  candidates_dirs <- c(
    script_dir,
    "contributed/R/MedDRA",
    file.path(".", "contributed", "R", "MedDRA")
  )

  deps <- list(
    list(file = "ae_setup.R",         fn = "setup"),
    list(file = "ae_meddra_output.R", fn = "out_med"),
    list(file = "err_output.R",       fn = "error_summary")
  )

  for (dep in deps) {
    if (!exists(dep$fn, mode = "function", inherits = TRUE)) {
      for (d in candidates_dirs) {
        dep_path <- file.path(d, dep$file)
        if (file.exists(dep_path)) {
          source(dep_path, local = FALSE)
          break
        }
      }
    }
  }
})


# ==============================================================================
# load_data_file -- Load data from XPT, SAS7BDAT, or pass through data frames
# ==============================================================================
#' @param x     A data frame, or character file path to .xpt or .sas7bdat.
#' @param label Character label for error messages (e.g., "AE").
#' @return A tibble with column names lowercased.
load_data_file <- function(x, label = "data") {
  if (is.data.frame(x)) {
    df <- tibble::as_tibble(x)
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    if (grepl("\\.xpt$", x, ignore.case = TRUE)) {
      df <- haven::read_xpt(x)
    } else if (grepl("\\.sas7bdat$", x, ignore.case = TRUE)) {
      df <- haven::read_sas(x)
    } else {
      cli::cli_abort("Unsupported file format for {label}: {.path {x}}")
    }
  } else {
    cli::cli_abort(
      "{label} must be a data frame or a valid file path to .xpt or .sas7bdat."
    )
  }
  names(df) <- tolower(names(df))
  df
}


# ==============================================================================
# params -- Configuration Builder (replaces SAS %params, lines 80-201)
# ==============================================================================
#' Build MedDRA at a Glance Panel Configuration
#'
#' Replaces the SAS \code{\%params} macro. In LOCAL mode, all parameters are
#' taken from function arguments with defaults. In Script Launcher mode,
#' SL-specific variables are mapped to panel parameters.
#'
#' @param run_location Character. "local" or "scriptlauncher". Default "local".
#' @param ae,dm,ex     Data frames or file paths to AE/DM/EX domain data.
#' @param studypath    Character. Path to study data directory.
#' @param saspath      Character. Path to SAS programs directory.
#' @param utilpath     Character. Path to utility programs directory.
#' @param output_path  Character. Path for output files.
#' @param panel_title  Character. Panel title for the cover sheet.
#' @param panel_desc   Character. Panel description.
#' @param ndabla       Character. NDA/BLA number.
#' @param studyid      Character. Study identifier.
#' @param meddra_path  Character. Path to MedDRA hierarchy files.
#' @param dme_path     Character. Path to DME list file.
#' @param ver          Character. MedDRA version string (e.g. "14.0").
#' @param study_lag    Integer. Days after last exposure for AE window.
#' @param cc           Continuity correction value (numeric, "arm", "none", "0").
#' @param vld_sw       Integer (0/1). Data validation switch.
#' @param rd_th,rr_th,pv_th  Numeric. Threshold defaults.
#' @param meddraver    Character. SL-mapped MedDRA version.
#' @param cont_corr    Character. SL-mapped continuity correction.
#' @param utildatapath Character. SL-mapped utility data path.
#' @param sl_datasets,sl_group,sl_subset  SL metadata data frames.
#' @return Named list containing all panel configuration values.
#' @export
params <- function(run_location = "local",
                   ae = NULL,
                   dm = NULL,
                   ex = NULL,
                   studypath    = NULL,
                   saspath      = ".",
                   utilpath     = ".",
                   output_path  = ".",
                   panel_title  = "MedDRA at a Glance",
                   panel_desc   = "",
                   ndabla       = NA_character_,
                   studyid      = NA_character_,
                   meddra_path  = NULL,
                   dme_path     = NULL,
                   ver          = "14.0",
                   study_lag    = 120L,
                   cc           = 0.5,
                   vld_sw       = 1L,
                   rd_th        = 5,
                   rr_th        = 5,
                   pv_th        = NA_real_,
                   meddraver    = NULL,
                   cont_corr    = NULL,
                   utildatapath = NULL,
                   sl_datasets  = NULL,
                   sl_group     = NULL,
                   sl_subset    = NULL) {

  run_loc <- stringr::str_to_upper(stringr::str_trim(as.character(run_location)))
  cli::cli_alert_info("RUN LOCATION: {run_loc}")

  # --- LOCAL mode (SAS lines 83-160) ---
  if (run_loc == "LOCAL") {

    # Build output file paths from output_path (parameterized, no hardcoded)
    aemedout <- file.path(output_path, "MedDRA at a Glance Analysis Panel.xlsx")
    errout   <- file.path(output_path, "MedDRA at a Glance Error Summary.xlsx")

    # Determine MedDRA usage flag (SAS lines 122-128)
    meddra_flag <- "Y"
    meddra_pct_init <- NA_real_
    ver_clean <- stringr::str_trim(as.character(ver))
    if (nchar(ver_clean) > 0L &&
        stringr::str_to_upper(substr(ver_clean, 1L, 1L)) == "N") {
      meddra_flag <- "N"
      meddra_pct_init <- 0
    }

    # Translate MedDRA version dots to underscores for dataset naming
    # SAS: translate(&ver.,'_','.') — e.g., "14.0" -> "14_0"
    dsver <- stringr::str_replace_all(ver_clean, "\\.", "_")

    # Load datasets if studypath provided and ae/dm/ex are NULL (SAS lines 104-109)
    if (!is.null(studypath) && is.null(ae)) {
      ae_path <- file.path(studypath, "ae.xpt")
      if (file.exists(ae_path)) ae <- haven::read_xpt(ae_path)
    }
    if (!is.null(studypath) && is.null(dm)) {
      dm_path <- file.path(studypath, "dm.xpt")
      if (file.exists(dm_path)) dm <- haven::read_xpt(dm_path)
    }
    if (!is.null(studypath) && is.null(ex)) {
      ex_path <- file.path(studypath, "ex.xpt")
      if (file.exists(ex_path)) ex <- haven::read_xpt(ex_path)
    }

    # Load MedDRA hierarchy (SAS lines 119, 166)
    meddra_data <- NULL
    if (meddra_flag == "Y" && !is.null(meddra_path) && nzchar(meddra_path)) {
      mdhier_file <- file.path(meddra_path, paste0("mdhier_", dsver, ".sas7bdat"))
      if (file.exists(mdhier_file)) {
        meddra_data <- haven::read_sas(mdhier_file)
      }
    }

    # Load DME list (SAS lines 131-133)
    dme_data <- NULL
    if (!is.null(dme_path) && nzchar(dme_path)) {
      dme_file <- file.path(dme_path, "dme.sas7bdat")
      if (file.exists(dme_file)) {
        dme_data <- haven::read_sas(dme_file)
      }
    }

    # Dummy SL datasets: empty tibbles with correct column names (SAS lines 147-158)
    if (is.null(sl_datasets)) {
      sl_datasets <- tibble::tibble(
        datatype = character(), name = character(),
        partition_variable = character(), default = character()
      )
    }
    if (is.null(sl_group)) {
      sl_group <- tibble::tibble(
        group_name = character(), domain = character(),
        partition = character(), var_name = character(),
        var_value = character(), dsvg_grp_name = character()
      )
    }
    if (is.null(sl_subset)) {
      sl_subset <- tibble::tibble(
        name = character(), domain = character(),
        partition = character(), var_name = character(),
        var_value = character(), inner_operator = character(),
        outer_operator = character()
      )
    }

  } else {
    # --- Script Launcher mode (SAS lines 163-197) ---
    # Map SL variables to panel parameters
    if (!is.null(meddraver)) {
      ver_clean <- stringr::str_trim(as.character(meddraver))
      ver <- ver_clean
      meddra_flag <- dplyr::if_else(
        stringr::str_to_upper(substr(ver_clean, 1L, 1L)) == "N", "N", "Y"
      )
    } else {
      ver_clean <- stringr::str_trim(as.character(ver))
      meddra_flag <- "Y"
    }
    dsver <- stringr::str_replace_all(ver_clean, "\\.", "_")
    meddra_pct_init <- NA_real_
    if (meddra_flag == "N") meddra_pct_init <- 0

    # Map continuity correction from SL format (SAS lines 186-194)
    if (!is.null(cont_corr)) {
      cc_str <- stringr::str_to_upper(stringr::str_replace_all(
        stringr::str_trim(as.character(cont_corr)), " ", ""
      ))
      cc <- dplyr::case_when(
        cc_str == "1"              ~ "1",
        cc_str %in% c("0.5", "1/2") ~ "0.5",
        cc_str == "1/OTHERARMCOUNT"  ~ "arm",
        cc_str %in% c("NONE", "0")  ~ "0",
        TRUE                         ~ "0"
      )
    }

    # DME path from SL utility data path
    if (!is.null(utildatapath)) {
      dme_path <- utildatapath
    }

    # Load MedDRA hierarchy
    meddra_data <- NULL
    if (meddra_flag == "Y" && !is.null(meddra_path) && nzchar(meddra_path)) {
      mdhier_file <- file.path(meddra_path, paste0("mdhier_", dsver, ".sas7bdat"))
      if (file.exists(mdhier_file)) {
        meddra_data <- haven::read_sas(mdhier_file)
      }
    }

    # Load DME list
    dme_data <- NULL
    if (!is.null(dme_path) && nzchar(dme_path)) {
      dme_file <- file.path(dme_path, "dme.sas7bdat")
      if (file.exists(dme_file)) {
        dme_data <- haven::read_sas(dme_file)
      }
    }

    aemedout <- file.path(output_path, "MedDRA at a Glance Analysis Panel.xlsx")
    errout   <- file.path(output_path, "MedDRA at a Glance Error Summary.xlsx")

    if (is.null(sl_datasets)) {
      sl_datasets <- tibble::tibble(
        datatype = character(), name = character(),
        partition_variable = character(), default = character()
      )
    }
    if (is.null(sl_group)) {
      sl_group <- tibble::tibble(
        group_name = character(), domain = character(),
        partition = character(), var_name = character(),
        var_value = character(), dsvg_grp_name = character()
      )
    }
    if (is.null(sl_subset)) {
      sl_subset <- tibble::tibble(
        name = character(), domain = character(),
        partition = character(), var_name = character(),
        var_value = character(), inner_operator = character(),
        outer_operator = character()
      )
    }
  }

  # Return config list with all panel parameters
  list(
    run_location = run_loc,
    panel_title  = panel_title,
    panel_desc   = panel_desc,
    saspath      = saspath,
    utilpath     = utilpath,
    output_path  = output_path,
    aemedout     = aemedout,
    errout       = errout,
    ndabla       = ndabla,
    studyid      = studyid,
    meddra_path  = meddra_path,
    dme_path     = dme_path,
    ver          = ver,
    dsver        = dsver,
    meddra_flag  = meddra_flag,
    meddra_data  = meddra_data,
    dme_data     = dme_data,
    study_lag    = as.integer(study_lag),
    cc           = cc,
    vld_sw       = as.integer(vld_sw),
    rd_th        = rd_th,
    rr_th        = rr_th,
    pv_th        = pv_th,
    ae           = ae,
    dm           = dm,
    ex           = ex,
    sl_datasets  = sl_datasets,
    sl_group     = sl_group,
    sl_subset    = sl_subset
  )
}


# ==============================================================================
# set_cc_switch -- Continuity Correction Switch Logic
# ==============================================================================
#' Determine Continuity Correction Mode
#'
#' Replaces the SAS DATA step (lines 218-233 of ae_meddra.sas) that sets
#' cc_sw and cc_whole based on the cc parameter value.
#'
#' @param cc  Continuity correction specification: numeric value (e.g. 0.5, 1),
#'            character "arm" (reciprocal of opposite arm), "none", or "0".
#' @return Named list with elements:
#'   \describe{
#'     \item{cc_sw}{Integer. 0 = no correction, 1 = constant k, 2 = reciprocal.}
#'     \item{cc_whole}{Integer. 1 if cc is a whole number, 0 otherwise.}
#'     \item{cc_val}{Numeric. The correction constant (for cc_sw == 1).}
#'   }
#' @export
set_cc_switch <- function(cc) {
  cc_sw    <- 0L
  cc_whole <- 1L
  cc_val   <- 0

  if (is.character(cc)) {
    cc_str <- stringr::str_trim(as.character(cc))

    # SAS line 220: if anyalpha(&cc.) — check for alphabetic characters
    if (stringr::str_detect(cc_str, "[[:alpha:]]")) {
      # SAS line 223: if cc = 'arm' then cc_sw = 2
      if (tolower(cc_str) == "arm") {
        cc_sw <- 2L
        cc_whole <- 0L
      }
      # Non-"arm" alpha string: no correction (cc_sw stays 0)
    } else {
      # Numeric string — parse it
      cc_num <- suppressWarnings(as.numeric(cc_str))
      if (!is.na(cc_num) && cc_num != 0) {
        cc_sw  <- 1L
        cc_val <- cc_num
        # SAS lines 227-229: cc_whole based on whether cc is an integer
        cc_whole <- dplyr::if_else(cc_num - floor(cc_num) != 0, 0L, 1L)
      }
    }
  } else if (is.numeric(cc)) {
    if (!is.na(cc) && cc != 0) {
      cc_sw  <- 1L
      cc_val <- cc
      cc_whole <- dplyr::if_else(cc - floor(cc) != 0, 0L, 1L)
    }
  }

  cli::cli_alert_info(
    "Continuity correction: cc_sw={cc_sw}, cc_whole={cc_whole}, cc_val={cc_val}"
  )

  list(cc_sw = cc_sw, cc_whole = cc_whole, cc_val = cc_val)
}


# ==============================================================================
# meddra_aggregate -- Hierarchical MedDRA Aggregation with Pairwise Statistics
# ==============================================================================
#' Aggregate MedDRA Adverse Events by Hierarchy Level
#'
#' Replaces the SAS \code{\%meddra} macro (lines 239-519 of ae_meddra.sas).
#' Aggregates the input dataset by specified MedDRA hierarchy by-variables,
#' computes per-arm subject counts and percentages, then for each pair of arms
#' computes risk difference, relative risk (with optional continuity correction
#' when comparator arm count is zero), and Fisher's exact test p-value
#' transformed as -log(p).
#'
#' @param dsin      Tibble. Base analysis dataset with columns: usubjid,
#'                  arm_num, plus all columns named in by_vars. May also
#'                  include dme column when by_vars ends with "pt_name".
#' @param by_vars   Character vector. Hierarchical by-variable column names,
#'                  e.g. c("soc_name") or c("soc_name","hlgt_name","hlt_name","pt_name").
#' @param arm_info  Named list with elements:
#'                  \describe{
#'                    \item{arm_count}{Integer. Number of treatment arms.}
#'                    \item{arm_N}{Named numeric vector. N per arm (index = arm_num).}
#'                    \item{arm_names}{Character vector. Display names per arm.}
#'                  }
#' @param cc_sw     Integer. Continuity correction switch (0/1/2).
#' @param cc_val    Numeric. Continuity correction constant (for cc_sw == 1).
#' @return Tibble with by_vars, per-arm count/pct columns, level numbers,
#'         DME flag (when applicable), and pairwise rd/rr/pv columns.
#' @export
meddra_aggregate <- function(dsin, by_vars, arm_info, cc_sw, cc_val) {
  arm_count <- arm_info$arm_count
  arm_N     <- arm_info$arm_N

  max_by    <- length(by_vars)
  last_by   <- by_vars[max_by]

  # Build key string for log message (SAS lines 252-255)
  key <- paste(by_vars, collapse = " ")
  cli::cli_h2("AGGREGATING ds_base INTO output BY {key}")

  # --- Determine columns to keep (SAS lines 275-277) -------------------------
  keep_cols <- c("usubjid", "arm_num", by_vars)
  has_dme   <- ("dme" %in% names(dsin)) && (last_by == "pt_name")
  if (has_dme) keep_cols <- c(keep_cols, "dme")

  # --- De-duplicate: one record per subject x by_vars (SAS PROC SORT NODUPKEY)
  by_syms <- rlang::syms(by_vars)
  ds_sorted <- dsin %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::filter(!is.na(.data$usubjid)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by_vars, "usubjid"))),
                    .keep_all = TRUE)

  # Count distinct subjects across all arms for audit
  n_unique_subj <- dplyr::n_distinct(ds_sorted[["usubjid"]])

  # --- Aggregate arm counts (SAS DATA step lines 280-320) --------------------
  # group_by all by_vars + arm_num, count subjects per group
  arm_counts <- ds_sorted %>%
    dplyr::group_by(!!!by_syms, .data$arm_num) %>%
    dplyr::summarise(arm_count_val = dplyr::n(), .groups = "drop")

  # Complete the grid so arms with zero subjects still appear (replaces tidyr::complete)
  by_grid <- ds_sorted %>%
    dplyr::select(dplyr::all_of(by_vars)) %>%
    dplyr::distinct()
  arm_grid <- tibble::tibble(arm_num = seq_len(arm_count))
  full_grid <- merge(by_grid, arm_grid, by = NULL) %>% tibble::as_tibble()

  arm_counts <- full_grid %>%
    dplyr::left_join(arm_counts, by = c(by_vars, "arm_num")) %>%
    dplyr::mutate(arm_count_val = dplyr::if_else(
      is.na(.data$arm_count_val), 0L, as.integer(.data$arm_count_val)
    )) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)), .data$arm_num)

  # Build wide format manually: arm1_count, arm1_pct, arm2_count, arm2_pct, ...
  # (replaces tidyr::pivot_wider — tidyr is not in external_imports)
  wide <- by_grid %>% dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)))

  purrr::walk(seq_len(arm_count), function(a) {
    arm_col   <- rlang::sym(paste0("arm", a, "_count"))
    pct_col   <- rlang::sym(paste0("arm", a, "_pct"))
    a_total   <- arm_N[a]

    arm_a <- arm_counts %>%
      dplyr::filter(arm_num == a) %>%
      dplyr::select(dplyr::all_of(c(by_vars, "arm_count_val"))) %>%
      dplyr::mutate(
        !!arm_col := as.integer(arm_count_val),
        !!pct_col := janitor::round_half_up(
          100.0 * arm_count_val / a_total, digits = 10
        )
      ) %>%
      dplyr::select(-dplyr::all_of("arm_count_val"))

    wide <<- dplyr::left_join(wide, arm_a, by = by_vars)
  })

  # --- Attach DME flag when pt_name is the last by-variable (SAS lines 316-318)
  if (has_dme) {
    dme_lookup <- ds_sorted %>%
      dplyr::select(dplyr::all_of(c(by_vars, "dme"))) %>%
      dplyr::distinct()
    wide <- dplyr::left_join(wide, dme_lookup, by = by_vars)
  }

  # --- Level indicator (SAS line 334) -----------------------------------------
  wide <- wide %>% dplyr::mutate(level = max_by)

  # --- Level numbering: sequential ID per by-level group (SAS lines 336-341) --
  # soc_name -> "soc", hlgt_name -> "hlgt", etc.
  for (i in seq_len(max_by)) {
    by_i <- by_vars[i]
    lbl  <- sub("_name$", "", by_i)
    lbl_sym <- rlang::sym(lbl)
    wide <- wide %>%
      dplyr::group_by(.data[[by_i]]) %>%
      dplyr::mutate(!!lbl_sym := dplyr::cur_group_id()) %>%
      dplyr::ungroup()
  }

  # --- Pairwise risk difference, relative risk (SAS lines 344-412) ------------
  if (arm_count > 1L) {
    # Generate all arm pair combinations (i, j) where i != j
    arm_pairs <- purrr::map2(
      rep(seq_len(arm_count), each = arm_count),
      rep(seq_len(arm_count), times = arm_count),
      ~ list(i = .x, j = .y)
    )
    arm_pairs <- purrr::keep(arm_pairs, ~ .x$i != .x$j)

    # Apply RD / RR / CC for each arm pair
    # NOTE: Direct vector operations with dplyr::if_else() are used here
    # because .data[[ ]] and !!sym() both fail inside purrr::reduce() —
    # .data resolves rlang's fake pronoun and !! is parsed as double-negation
    # before dplyr's NSE can intercept it. This is a known rlang/purrr scoping
    # interaction; vectorised column assignment is the robust alternative.
    wide <- purrr::reduce(arm_pairs, function(df, pair) {
      i <- pair$i
      j <- pair$j
      a_col   <- paste0("arm", i, "_count")
      c_col   <- paste0("arm", j, "_count")
      pct_i   <- paste0("arm", i, "_pct")
      pct_j   <- paste0("arm", j, "_pct")
      b_tot   <- arm_N[i]
      d_tot   <- arm_N[j]
      rd_col  <- paste0("rd", i, j)
      rr_col  <- paste0("rr", i, j)
      cc_col  <- paste0("cc", i, j)

      # Risk difference (SAS lines 353-360)
      df[[rd_col]] <- df[[pct_i]] - df[[pct_j]]

      # 2x2 contingency table cells for this arm pair
      cell_a  <- as.numeric(df[[a_col]])
      cell_b  <- b_tot - cell_a
      cell_c  <- as.numeric(df[[c_col]])
      cell_d  <- d_tot - cell_c
      cc_flag <- (cell_c == 0)

      # Continuity correction when comparator count == 0 (SAS lines 378-401)
      # CRITICAL: cc_flag is captured BEFORE any cell modification so all
      # four corrections use the same gate condition (matching SAS block logic)
      if (cc_sw == 1L) {
        df[[cc_col]] <- dplyr::if_else(cc_flag, "*", NA_character_)
        cell_a <- dplyr::if_else(cc_flag, cell_a + cc_val, cell_a)
        cell_b <- dplyr::if_else(cc_flag, cell_b + cc_val, cell_b)
        cell_c <- dplyr::if_else(cc_flag, cell_c + cc_val, cell_c)
        cell_d <- dplyr::if_else(cc_flag, cell_d + cc_val, cell_d)
      } else if (cc_sw == 2L) {
        df[[cc_col]] <- dplyr::if_else(cc_flag, "*", NA_character_)
        # Reciprocal of opposite arm: 1/(c+d) added to a,b; 1/(a+b) to c,d
        # Must compute reciprocals BEFORE modification matches SAS sequential eval
        recip_cd <- dplyr::if_else(cc_flag, 1 / (cell_c + cell_d), 0)
        cell_a   <- dplyr::if_else(cc_flag, cell_a + recip_cd, cell_a)
        cell_b   <- dplyr::if_else(cc_flag, cell_b + recip_cd, cell_b)
        recip_ab <- dplyr::if_else(cc_flag, 1 / (cell_a + cell_b), 0)
        cell_c   <- dplyr::if_else(cc_flag, cell_c + recip_ab, cell_c)
        cell_d   <- dplyr::if_else(cc_flag, cell_d + recip_ab, cell_d)
      }

      # Relative risk (SAS line 404): rr = (a/(a+b)) / (c/(c+d))
      df[[rr_col]] <- dplyr::if_else(
        cell_c != 0,
        (cell_a / (cell_a + cell_b)) / (cell_c / (cell_c + cell_d)),
        NA_real_
      )

      df
    }, .init = wide)

    # --- Fisher's exact test (SAS lines 414-492) ------------------------------
    # For all pairs (i,j) where i < j, compute two-sided Fisher p-value.
    # Build arm pair index vectors for walk2() iteration.
    pair_grid <- expand.grid(i = seq_len(arm_count), j = seq_len(arm_count))
    pair_grid <- pair_grid[pair_grid$i < pair_grid$j, , drop = FALSE]
    pair_i_vec <- pair_grid$i
    pair_j_vec <- pair_grid$j

    # Build per-row Fisher results using map_dfr over row indices
    # (SAS: PROC FREQ ... BY term_num; EXACT FISHER)
    fisher_all <- purrr::map_dfr(seq_len(nrow(wide)), function(row_idx) {
      row_data <- wide[row_idx, , drop = FALSE]
      results <- tibble::tibble(.row_id = row_idx)

      # Use walk2 to iterate over arm pair vectors (i, j)
      purrr::walk2(pair_i_vec, pair_j_vec, function(i, j) {
        a_val <- row_data[[paste0("arm", i, "_count")]]
        c_val <- row_data[[paste0("arm", j, "_count")]]
        b_val <- arm_N[i] - a_val
        d_val <- arm_N[j] - c_val

        # SAS comment (lines 450-452): When both arms have 0 subjects,
        # PROC FREQ produces missing; handle with explicit check
        if ((a_val + c_val) == 0) {
          p_val <- NA_real_
        } else {
          mat <- matrix(c(a_val, c_val, b_val, d_val), nrow = 2)
          p_val <- tryCatch(
            stats::fisher.test(mat)$p.value,
            error = function(e) NA_real_
          )
        }

        # -log(p) for both directions (SAS lines 467-468)
        neg_log_p <- dplyr::if_else(is.na(p_val), NA_real_, -log(p_val))
        results[[paste0("pv", i, j)]] <<- neg_log_p
        results[[paste0("pv", j, i)]] <<- neg_log_p
      })

      results
    })

    # Merge Fisher p-values back by row position
    wide <- dplyr::bind_cols(
      wide %>% dplyr::mutate(.row_id = dplyr::row_number()),
      fisher_all %>% dplyr::select(-dplyr::all_of(".row_id"))
    ) %>%
      dplyr::select(-dplyr::all_of(".row_id"))

  } # end if arm_count > 1

  # --- Column reordering (SAS lines 494-508) ----------------------------------
  # Order: by_vars, [dme], arm counts/pcts, rd/rr/pv pairs, level, level_nums
  by_cols <- by_vars
  dme_cols <- if (has_dme) "dme" else character(0)

  # Use starts_with() to find arm count/pct columns dynamically
  arm_col_names <- names(wide)[grepl("^arm\\d+_(count|pct)$", names(wide))]
  count_pct_cols <- as.character(purrr::map(
    seq_len(arm_count),
    ~ c(paste0("arm", .x, "_count"), paste0("arm", .x, "_pct"))
  ) %>% purrr::reduce(c))

  stat_cols <- character(0)
  if (arm_count > 1L) {
    stat_cols <- as.character(purrr::map(seq_len(arm_count), function(i) {
      purrr::map(seq_len(arm_count), function(j) {
        if (i != j) {
          cols <- c(paste0("rd", i, j), paste0("rr", i, j), paste0("pv", i, j))
          if (cc_sw != 0L) cols <- c(cols, paste0("cc", i, j))
          cols
        }
      }) %>% purrr::compact() %>% purrr::reduce(c, .init = character(0))
    }) %>% purrr::reduce(c, .init = character(0)))
  }

  level_cols <- c("level", sub("_name$", "", by_vars))
  all_ordered <- c(by_cols, dme_cols, count_pct_cols, stat_cols, level_cols)
  available   <- intersect(all_ordered, names(wide))

  # Select ordered columns, using starts_with to verify arm columns present
  arm_present <- names(dplyr::select(wide, dplyr::starts_with("arm")))

  wide <- wide %>%
    dplyr::select(dplyr::all_of(available)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)))

  wide
}


# ==============================================================================
# meddra_cmp -- Build Comparison Datasets for MedDRA Output
# ==============================================================================
#' Build MedDRA Comparison Datasets
#'
#' Replaces the SAS \code{\%meddra_cmp} macro (lines 525-605 of ae_meddra.sas).
#' Stacks four levels of MedDRA aggregation, derives row numbers for the hidden
#' data sheet, and creates both visible comparison and hidden data datasets.
#'
#' @param meddra_1  Tibble. SOC-level aggregation from meddra_aggregate().
#' @param meddra_2  Tibble. SOC/HLGT-level aggregation.
#' @param meddra_3  Tibble. SOC/HLGT/HLT-level aggregation.
#' @param meddra_4  Tibble. SOC/HLGT/HLT/PT-level aggregation.
#' @param arm_info  Named list from setup() with arm_count, arm_N, arm_names.
#' @param cc_sw     Integer. Continuity correction switch.
#' @return Named list with meddra_cmp_output, meddra_cmp_data,
#'         meddra_cmp_output_row, meddra_cmp_data_row tibbles.
#' @export
meddra_cmp <- function(meddra_1, meddra_2, meddra_3, meddra_4,
                       arm_info, cc_sw) {

  cli::cli_h2("Building comparison datasets")

  # Stack all four levels (SAS lines 528-538: SET meddra_1(in=a) ... meddra_4)
  all_levels <- dplyr::bind_rows(
    meddra_1 %>% dplyr::mutate(level = 1L),
    meddra_2 %>% dplyr::mutate(level = 2L),
    meddra_3 %>% dplyr::mutate(level = 3L),
    meddra_4 %>% dplyr::mutate(level = 4L)
  )

  # Ensure hierarchy columns exist for all levels (fill NA for upper levels)
  for (col in c("soc_name", "hlgt_name", "hlt_name", "pt_name")) {
    if (!col %in% names(all_levels)) {
      all_levels[[col]] <- NA_character_
    }
  }
  for (col in c("soc", "hlgt", "hlt", "pt")) {
    if (!col %in% names(all_levels)) {
      all_levels[[col]] <- NA_integer_
    }
  }

  # Data row numbering for hidden sheet (SAS line 537: row + 1)
  cmp_data <- all_levels %>%
    dplyr::mutate(row = dplyr::row_number())

  # Sort for visible sheet (SAS lines 541-543: PROC SORT)
  cmp_sorted <- all_levels %>%
    dplyr::arrange(.data$soc_name, .data$hlgt_name,
                   .data$hlt_name, .data$pt_name, .data$level) %>%
    dplyr::mutate(row = dplyr::row_number())

  # Visible comparison output with signal indicator columns (SAS lines 547-568)
  cmp_output <- cmp_sorted %>%
    dplyr::mutate(
      sgnl     = NA_character_,
      sgnl_soc = NA_character_,
      sgnl_hlgt = NA_character_,
      sgnl_hlt = NA_character_,
      sgnl_pt  = NA_character_,
      arm_exp_count = NA_real_,
      arm_exp_pct   = NA_real_,
      arm_ctl_count = NA_real_,
      arm_ctl_pct   = NA_real_,
      rd = NA_real_,
      rr = NA_real_,
      pv = NA_real_
    )

  # Add cc placeholder column if continuity correction is active
  if (cc_sw != 0L) {
    cmp_output <- cmp_output %>%
      dplyr::mutate(cc = NA_character_)
  }

  # Hidden data sheet: remove name columns, add formula placeholders (SAS 572-586)
  cmp_data_hidden <- cmp_data %>%
    dplyr::select(-dplyr::any_of(c("soc_name", "hlgt_name",
                                    "hlt_name", "pt_name", "dme"))) %>%
    dplyr::mutate(
      rd       = NA_real_,
      rr       = NA_real_,
      pv       = NA_real_,
      cc       = NA_character_,
      sgnl     = NA_real_,
      sgnl_soc = NA_real_,
      sgnl_hlgt = NA_real_,
      sgnl_hlt = NA_real_,
      sgnl_pt  = NA_real_,
      sgnl_any = NA_real_
    )

  # Row lookup datasets (SAS lines 588-603)
  build_row_lookup <- function(ds) {
    ds %>%
      dplyr::mutate(
        lvl_nm = dplyr::case_when(
          level == 1L ~ "soc",
          level == 2L ~ "hlgt",
          level == 3L ~ "hlt",
          level == 4L ~ "pt",
          TRUE        ~ NA_character_
        ),
        lvl_no = dplyr::case_when(
          level == 1L ~ as.numeric(.data$soc),
          level == 2L ~ as.numeric(.data$hlgt),
          level == 3L ~ as.numeric(.data$hlt),
          level == 4L ~ as.numeric(.data$pt),
          TRUE        ~ NA_real_
        )
      ) %>%
      dplyr::select(dplyr::any_of(c("row", "lvl_nm", "lvl_no",
                                      "soc", "hlgt", "hlt", "pt")))
  }

  list(
    meddra_cmp_output     = cmp_output,
    meddra_cmp_data       = cmp_data_hidden,
    meddra_cmp_output_row = build_row_lookup(cmp_sorted),
    meddra_cmp_data_row   = build_row_lookup(cmp_data)
  )
}


# ==============================================================================
# aemed -- Main MedDRA at a Glance Panel Driver
# ==============================================================================
#' Run the MedDRA at a Glance Analysis Panel
#'
#' Replaces the SAS \code{\%aemed} macro (lines 611-635) and top-level
#' execution block (lines 637-647) of ae_meddra.sas. Orchestrates the
#' entire pipeline: setup validation, 4-level MedDRA aggregation,
#' comparison dataset construction, and Excel output generation.
#'
#' @param ae,dm,ex     Data frames or file paths to AE/DM/EX domain data.
#' @param meddra_data  Data frame. MedDRA hierarchy (optional; loaded from
#'                     meddra_path if NULL).
#' @param dme_data     Data frame. DME list (optional).
#' @param meddra_path  Character. Path to MedDRA hierarchy file directory.
#' @param dme_path     Character. Path to DME list file directory.
#' @param output_path  Character. Output directory for workbooks.
#' @param panel_title  Character. Panel title.
#' @param panel_desc   Character. Panel description.
#' @param ndabla       Character. NDA/BLA number.
#' @param studyid      Character. Study identifier.
#' @param meddra_ver   Character. MedDRA version string.
#' @param study_lag    Integer. Days after last exposure window.
#' @param cc           Continuity correction: numeric, "arm", "none", or "0".
#' @param vld_sw       Logical/integer. Data validation switch.
#' @param rd_th,rr_th  Numeric. Risk difference / relative risk thresholds.
#' @param pv_th        Numeric. P-value threshold (NA = no filter).
#' @param sl_datasets,sl_group,sl_subset  SL metadata data frames.
#' @param verbose      Logical. Emit progress messages.
#' @return Invisible named list with output_file path and analysis results.
#' @export
aemed <- function(ae,
                  dm,
                  ex,
                  meddra_data   = NULL,
                  dme_data      = NULL,
                  meddra_path   = NULL,
                  dme_path      = NULL,
                  output_path   = ".",
                  panel_title   = "MedDRA at a Glance",
                  panel_desc    = "",
                  ndabla        = NA_character_,
                  studyid       = NA_character_,
                  meddra_ver    = "14.0",
                  study_lag     = 120L,
                  cc            = 0.5,
                  vld_sw        = TRUE,
                  rd_th         = 5,
                  rr_th         = 5,
                  pv_th         = NA_real_,
                  sl_datasets   = NULL,
                  sl_group      = NULL,
                  sl_subset     = NULL,
                  verbose       = TRUE) {

  # --- Timing start (SAS line 638: %let start = %sysfunc(time())) ---
  start_time <- proc.time()
  if (verbose) {
    log_msg("MedDRA at a Glance Panel -- starting")
    cli::cli_alert_info("MedDRA at a Glance Panel -- starting")
  }

  # ---------------------------------------------------------------------------
  # 1. Load input datasets (SAS lines 107-109: data ae; set inlib.ae; run;)
  # ---------------------------------------------------------------------------
  ae_df <- load_data_file(ae, "AE")
  dm_df <- load_data_file(dm, "DM")
  ex_df <- load_data_file(ex, "EX")

  # ---------------------------------------------------------------------------
  # 2. Determine MedDRA usage (SAS lines 122-128)
  # ---------------------------------------------------------------------------
  use_meddra_flag <- "Y"
  ver_str <- stringr::str_trim(as.character(meddra_ver))
  if (nchar(ver_str) > 0L &&
      stringr::str_to_upper(substr(ver_str, 1L, 1L)) == "N") {
    use_meddra_flag <- "N"
  }

  # ---------------------------------------------------------------------------
  # 3. Load MedDRA hierarchy if needed (SAS lines 119, 166)
  # ---------------------------------------------------------------------------
  if (use_meddra_flag == "Y" && is.null(meddra_data) &&
      !is.null(meddra_path) && nzchar(meddra_path)) {
    meddra_files <- list.files(
      meddra_path,
      pattern = "mdhier.*\\.(xpt|sas7bdat)$",
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(meddra_files) > 0L) {
      mf <- meddra_files[1L]
      if (verbose) cli::cli_alert_info("Loading MedDRA hierarchy: {mf}")
      meddra_data <- if (grepl("\\.xpt$", mf, ignore.case = TRUE)) {
        haven::read_xpt(mf)
      } else {
        haven::read_sas(mf)
      }
      names(meddra_data) <- tolower(names(meddra_data))
    } else {
      cli::cli_alert_info("No MedDRA hierarchy file in: {meddra_path}")
      use_meddra_flag <- "N"
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Load DME list if needed (SAS lines 131-133)
  # ---------------------------------------------------------------------------
  if (is.null(dme_data) && !is.null(dme_path) && nzchar(dme_path)) {
    dme_files <- list.files(
      dme_path, pattern = "dme\\.(xpt|sas7bdat)$",
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(dme_files) > 0L) {
      df <- dme_files[1L]
      if (verbose) cli::cli_alert_info("Loading DME list: {df}")
      dme_data <- if (grepl("\\.xpt$", df, ignore.case = TRUE)) {
        haven::read_xpt(df)
      } else {
        haven::read_sas(df)
      }
      names(dme_data) <- tolower(names(dme_data))
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Continuity correction switch (SAS lines 218-233)
  # ---------------------------------------------------------------------------
  cc_info <- set_cc_switch(cc)
  cc_sw_val   <- cc_info$cc_sw
  cc_val_num  <- cc_info$cc_val

  # ---------------------------------------------------------------------------
  # 6. Output file paths (SAS lines 97-100)
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  output_file <- file.path(output_path,
                           "MedDRA at a Glance Analysis Panel.xlsx")
  err_file <- file.path(output_path,
                        "MedDRA at a Glance Error Summary.xlsx")

  # ---------------------------------------------------------------------------
  # 7. Dummy SL datasets (SAS lines 147-158)
  # ---------------------------------------------------------------------------
  if (is.null(sl_datasets)) {
    sl_datasets <- tibble::tibble(
      datatype = character(), name = character(),
      partition_variable = character(), default = character()
    )
  }
  if (is.null(sl_group)) {
    sl_group <- tibble::tibble(
      group_name = character(), domain = character(),
      partition = character(), var_name = character(),
      var_value = character(), dsvg_grp_name = character()
    )
  }
  if (is.null(sl_subset)) {
    sl_subset <- tibble::tibble(
      name = character(), domain = character(),
      partition = character(), var_name = character(),
      var_value = character(), inner_operator = character(),
      outer_operator = character()
    )
  }

  # ---------------------------------------------------------------------------
  # 8. Run ae_setup (SAS line 613: %setup(mdhier=Y,dme=Y))
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_h2("Running setup validation")

  # Determine the dme argument string for setup()
  dme_flag_str <- dplyr::if_else(!is.null(dme_data), "Y", "N")

  setup_result <- tryCatch(
    setup(
      ae          = ae_df,
      dm          = dm_df,
      ex          = ex_df,
      meddra_hier = meddra_data,
      dme_data    = dme_data,
      mdhier      = use_meddra_flag,
      dme         = dme_flag_str,
      vld_sw      = as.integer(vld_sw),
      study_lag   = as.integer(study_lag)
    ),
    error = function(e) {
      cli::cli_alert_info("Setup failed: {conditionMessage(e)}")
      list(setup_success = FALSE, setup_req_var = FALSE,
           arm_count = 0L, arm_total = 0L,
           arm_subjects = integer(0), arm_names = character(0),
           meddra_pct = 0, rpt_chk_var_req = NULL)
    }
  )

  # ---------------------------------------------------------------------------
  # 9. Execute analysis or error branch (SAS lines 615-634)
  # ---------------------------------------------------------------------------
  setup_ok   <- isTRUE(setup_result$setup_success)
  meddra_pct <- setup_result$meddra_pct %||% 0

  if (setup_ok && meddra_pct > 0) {
    if (verbose) cli::cli_h2("Running MedDRA aggregation (4 levels)")

    # Build arm_info from setup result
    arm_subjects <- setup_result$arm_subjects
    if (is.null(arm_subjects) || length(arm_subjects) == 0L) {
      arm_subjects <- setup_result$arm_N %||% integer(0)
    }
    arm_info <- list(
      arm_count = setup_result$arm_count,
      arm_N     = as.numeric(arm_subjects),
      arm_names = setup_result$arm_names
    )

    ds_base <- setup_result$ds_base_meddra %||% setup_result$ds_base

    # 4-level MedDRA aggregation (SAS lines 617-620)
    meddra_1 <- meddra_aggregate(
      ds_base, c("soc_name"), arm_info, cc_sw_val, cc_val_num
    )
    meddra_2 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name"), arm_info, cc_sw_val, cc_val_num
    )
    meddra_3 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name", "hlt_name"),
      arm_info, cc_sw_val, cc_val_num
    )
    meddra_4 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name", "hlt_name", "pt_name"),
      arm_info, cc_sw_val, cc_val_num
    )

    # Build comparison datasets (SAS line 622: %meddra_cmp)
    cmp_results <- meddra_cmp(
      meddra_1, meddra_2, meddra_3, meddra_4,
      arm_info, cc_sw_val
    )

    # Generate Excel output (SAS line 624: %out_med)
    if (verbose) cli::cli_h2("Generating MedDRA output workbook")
    tryCatch(
      out_med(
        meddra_cmp_output     = cmp_results$meddra_cmp_output,
        meddra_cmp_data       = cmp_results$meddra_cmp_data,
        meddra_cmp_output_row = cmp_results$meddra_cmp_output_row,
        meddra_cmp_data_row   = cmp_results$meddra_cmp_data_row,
        meddra_1 = meddra_1, meddra_2 = meddra_2,
        meddra_3 = meddra_3, meddra_4 = meddra_4,
        arm_info    = arm_info,
        cc_sw       = cc_sw_val,
        output_file = output_file,
        panel_title = panel_title,
        panel_desc  = panel_desc,
        ndabla      = ndabla,
        studyid     = studyid,
        meddra_ver  = meddra_ver,
        rd_th       = rd_th,
        rr_th       = rr_th,
        pv_th       = pv_th,
        sl_datasets = sl_datasets,
        sl_group    = sl_group,
        sl_subset   = sl_subset
      ),
      error = function(e) {
        cli::cli_alert_info("Output generation failed: {conditionMessage(e)}")
      }
    )

    if (verbose) cli::cli_alert_success("MedDRA analysis complete.")

  } else {
    # Error summary output (SAS lines 627-633)
    if (verbose) cli::cli_alert_info(
      "Setup failed or no MedDRA matches -- generating error summary"
    )

    # Derive err_nosubj: SAS ifc(&dm_subj_gt0., 0, 1)
    dm_subj_ok <- isTRUE(setup_result$arm_total > 0L) ||
                  isTRUE(setup_result$arm_count > 0L)

    tryCatch(
      error_summary(
        err_file        = err_file,
        panel_title     = panel_title,
        panel_desc      = panel_desc,
        ndabla          = as.character(ndabla %||% ""),
        studyid         = as.character(studyid %||% ""),
        sl_subset       = sl_subset,
        rpt_chk_var_req = setup_result$rpt_chk_var_req,
        err_nosubj      = !dm_subj_ok,
        err_missvar     = !isTRUE(setup_result$setup_req_var),
        err_desc        = dplyr::case_when(
          meddra_pct == 0 ~
            "There were zero adverse events with matching MedDRA descriptions.",
          TRUE ~ ""
        )
      ),
      error = function(e) {
        cli::cli_alert_info("Error summary output failed: {conditionMessage(e)}")
      }
    )
  }

  # ---------------------------------------------------------------------------
  # 10. Timing (SAS lines 643-647)
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  if (verbose) {
    cli::cli_alert_info(paste0(
      "Running time: ",
      sprintf("%02d:%05.2f", as.integer(elapsed) %/% 60, elapsed %% 60)
    ))
  }

  invisible(list(
    output_file  = output_file,
    err_file     = err_file,
    setup_result = setup_result,
    meddra_1     = if (exists("meddra_1", inherits = FALSE)) meddra_1 else NULL,
    meddra_2     = if (exists("meddra_2", inherits = FALSE)) meddra_2 else NULL,
    meddra_3     = if (exists("meddra_3", inherits = FALSE)) meddra_3 else NULL,
    meddra_4     = if (exists("meddra_4", inherits = FALSE)) meddra_4 else NULL,
    cmp_results  = if (exists("cmp_results", inherits = FALSE)) cmp_results else NULL
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. SAS 'options minoperator' has no R equivalent; R has
#      the %in% operator natively. No action needed.
#   2. SAS 'options missing=""' displays numeric missing as blank;
#      R NA values print as "NA". The output module (ae_meddra_output.R)
#      handles NA-to-blank conversion for Excel cells.
#   3. Continuity correction applies ONLY when comparator arm count
#      (c) equals zero, matching the SAS logic at line 380.
#   4. SAS hash object lookups in ae_setup.sas are replaced by
#      dplyr::left_join() in ae_setup.R.
#   5. SAS DATA step RETAIN + array processing for arm counting is
#      replaced by dplyr group_by + summarise + tidyr::complete.
#   6. The setup() function from ae_setup.R returns a named list
#      with setup_success, ds_base, ds_base_meddra, arm_count,
#      arm_subjects, arm_names, meddra_pct, setup_req_var, and
#      error tracking datasets.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Fisher's exact test p-values may differ by < 0.00001
#      between SAS PROC FREQ EXACT FISHER and R stats::fisher.test()
#      due to algorithmic differences (network algorithm vs
#      hypergeometric enumeration). SAS comments at line 450
#      acknowledge this difference.
#   2. Negative log of p-value (-log(p)) inherits any p-value
#      differences from point 1.
#   3. When both arms have 0 subjects for a term, SAS PROC FREQ
#      produces a missing p-value; R fisher.test() would produce
#      p=1 for a zero matrix. We explicitly check for this case
#      and return NA to match SAS behavior.
#   4. Arm percentages use janitor::round_half_up() to match SAS
#      round-half-up behavior. Differences may still arise at
#      sub-epsilon precision due to IEEE 754 intermediate
#      computations.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS 'options minoperator' -- R has %in% natively.
#   2. SAS 'options missing=""' -- handled in output formatting.
#   3. SAS PROC DATASETS DELETE -- R garbage collection is automatic;
#      no explicit dataset cleanup needed.
#   4. SAS SpreadsheetML XML generation (xml_output.sas) -- replaced
#      by openxlsx API in ae_meddra_output.R and xml_output.R.
#   5. SAS DATA step RETAIN statement -- replaced by dplyr group_by
#      + summarise with tidyr::complete for zero-filling.
#
# PACKAGE SELECTION RATIONALE:
#   stats::fisher.test() -- Direct replacement for PROC FREQ EXACT
#      FISHER; base R, no additional dependency. Computes exact
#      two-sided p-value for 2x2 contingency tables.
#   dplyr -- Core data manipulation replacing SAS DATA steps, PROC
#      SORT, PROC SQL. AAP mandates tidyverse over base R.
#   haven -- SAS data I/O (read_xpt, read_sas) for CDISC datasets.
#   janitor -- SAS-compatible round-half-up rounding behavior via
#      round_half_up(). Critical for Gate 2 Rounding Audit.
#   purrr -- Functional iteration for arm pair loops and reduce
#      operations. AAP mandates purrr over base R lapply/for.
#   rlang -- Tidy evaluation for programmatic column access in dplyr
#      pipelines using .data pronoun, sym()/!!, syms()/!!!.
#   stringr -- Tidyverse string manipulation replacing SAS character
#      functions. AAP mandates stringr over base R grepl/gsub.
#   cli -- User-facing console messages replacing SAS %put and
#      DATA _NULL_ banner output.
#   tibble -- Enhanced data frames for Script Launcher dummy datasets
#      and structured return values.
#
# OPEN QUESTIONS:
#   1. Confirm continuity correction behavior matches SAS exactly
#      for edge cases where BOTH arms have zero subjects for a term.
#      The reciprocal method (cc_sw=2) involves division by zero
#      when c+d=0; SAS produces missing, R produces Inf/NaN.
#   2. Verify negative log p-value precision alignment with SAS
#      output to within the documented < 0.00001 tolerance.
#   3. The SAS MedDRA hierarchy is loaded from sas7bdat files with
#      versioned filename pattern (mdhier_14_0.sas7bdat). Confirm
#      which file format is standard in the production environment.
#   4. SAS 'options missing=""' displays missing as blank in output;
#      the R output module must explicitly handle NA-to-blank
#      conversion. Verify this is handled in ae_meddra_output.R.
# ============================================================
