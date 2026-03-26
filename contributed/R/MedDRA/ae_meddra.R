# ============================================================================
# PROGRAM NAME: ae_meddra.R — MedDRA at a Glance Analysis Panel Driver
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
#   contributed/R/MedDRA/xml_output.R       -- openxlsx formatting macros
#   contributed/R/MedDRA/data_checks.R      -- Generic variable checks
#   contributed/R/MedDRA/sl_gs_output.R     -- Script Launcher settings output
#   contributed/R/MedDRA/err_output.R       -- Error output when missing vars
#
# PARAMETERS REQUIRED:
#   ae          -- AE domain data frame or path to XPT/SAS7BDAT file
#   dm          -- DM domain data frame or path to XPT/SAS7BDAT file
#   ex          -- EX domain data frame or path to XPT/SAS7BDAT file
#   meddra_data -- MedDRA hierarchy data frame (optional; loaded from
#                  meddra_path if NULL)
#   dme_data    -- Designated Medical Events list data frame (optional)
#   output_path -- directory for output files
#   meddra_ver  -- MedDRA version string (default "14.0"; "N/A" disables)
#   study_lag   -- window in days after last exposure for AE inclusion
#   cc          -- continuity correction value (numeric, "arm", or "none")
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
# ============================================================================

# --- Required Libraries -------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(janitor)
library(openxlsx)
library(cli)

# --- Source Internal Dependencies ---------------------------------------------
# Replaces SAS %include statements (lines 211-216 of ae_meddra.sas).
# Dependencies are loaded with guarded sourcing: only load if their primary
# exported function is not yet available in the session.
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  # Map each dependency to its primary export function and filename
  deps <- list(
    list(file = "ae_setup.R",         fn = "setup"),
    list(file = "ae_meddra_output.R", fn = "out_med"),
    list(file = "xml_output.R",       fn = "create_styles"),
    list(file = "data_checks.R",      fn = "chk_var"),
    list(file = "sl_gs_output.R",     fn = "group_subset_pp"),
    list(file = "err_output.R",       fn = "error_summary")
  )

  candidates_dirs <- c(
    script_dir,
    "contributed/R/MedDRA",
    file.path(".", "contributed", "R", "MedDRA")
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
# load_data_file — Load data from XPT, SAS7BDAT, or pass through data frames
# ==============================================================================
# Helper that handles the polymorphic data input pattern used across the
# contributed panel drivers. Accepts a data frame (returned as-is) or a
# character file path (loaded via haven).
#
# @param x   A data frame, or character file path to .xpt or .sas7bdat.
# @param label Character label for error messages (e.g., "AE").
# @return A data frame / tibble with column names lowercased.
# ==============================================================================
load_data_file <- function(x, label = "data") {
  if (is.data.frame(x)) {
    df <- x
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
  # Lowercase column names for consistent handling
  names(df) <- tolower(names(df))
  df
}


# ==============================================================================
# meddra_aggregate — Hierarchical MedDRA Aggregation with Pairwise Statistics
# ==============================================================================
# Replaces SAS %meddra macro (lines 239-519 of ae_meddra.sas).
# Aggregates the input dataset by the specified MedDRA hierarchy by-variables,
# computes per-arm subject counts and percentages, then for each pair of arms
# computes risk difference, relative risk (with continuity correction when
# c == 0), and Fisher's exact test p-value (-log transformed).
#
# @param dsin      Data frame: the base analysis dataset (ds_base or
#                  ds_base_meddra) with columns: usubjid, arm_num, plus the
#                  by_vars columns. Also uses dme column when by_vars include
#                  pt_name.
# @param by_vars   Character vector of hierarchical by-variable column names,
#                  e.g., c("soc_name") or c("soc_name","hlgt_name","hlt_name","pt_name").
# @param arm_info  Named list from setup() with elements:
#                    arm_count, arm_N (named vector of N per arm), arm_names.
# @param cc_sw     Integer 0/1/2 — continuity correction switch.
# @param cc_val    Numeric — continuity correction constant (for cc_sw == 1).
# @return Tibble with by_vars, per-arm counts/percentages, level numbers,
#         DME flag (when applicable), and pairwise rd/rr/pv columns.
# ==============================================================================
meddra_aggregate <- function(dsin, by_vars, arm_info, cc_sw, cc_val) {
  arm_count <- arm_info$arm_count
  arm_N     <- arm_info$arm_N

  max_by    <- length(by_vars)
  last_by   <- by_vars[max_by]

  # --- De-duplicate: one record per subject × by_vars (SAS lines 274-278) -----
  ds_sorted <- dsin %>%
    dplyr::select(dplyr::all_of(c("usubjid", "arm_num", by_vars,
                                   if ("dme" %in% names(dsin) &&
                                       last_by == "pt_name") "dme"))) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by_vars, "usubjid"))),
                    .keep_all = TRUE)

  # --- Aggregate arm counts (SAS lines 280-320) ------------------------------
  arm_counts <- ds_sorted %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars)), arm_num) %>%
    dplyr::summarise(arm_count_val = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(tidyr::nesting(!!!rlang::syms(by_vars)),
                    arm_num = seq_len(arm_count),
                    fill = list(arm_count_val = 0L)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)), arm_num)

  # Pivot to wide: arm1_count, arm1_pct, arm2_count, arm2_pct, ...
  wide <- arm_counts %>%
    dplyr::mutate(
      arm_pct = 100.0 * arm_count_val /
        purrr::map_dbl(arm_num, ~ arm_N[.x])
    ) %>%
    tidyr::pivot_wider(
      names_from  = arm_num,
      values_from = c(arm_count_val, arm_pct),
      names_glue  = "arm{arm_num}_{.value}"
    )

  # Rename pivoted columns to arm1_count, arm1_pct, ...
  rename_map <- setNames(
    c(paste0("arm", seq_len(arm_count), "_arm_count_val"),
      paste0("arm", seq_len(arm_count), "_arm_pct")),
    c(paste0("arm", seq_len(arm_count), "_count"),
      paste0("arm", seq_len(arm_count), "_pct"))
  )
  wide <- dplyr::rename(wide, dplyr::all_of(rename_map))

  # --- Add DME flag when pt_name is the last by-variable (SAS lines 316-318) --

  if (last_by == "pt_name" && "dme" %in% names(ds_sorted)) {
    dme_lookup <- ds_sorted %>%
      dplyr::select(dplyr::all_of(by_vars), dme) %>%
      dplyr::distinct()
    wide <- dplyr::left_join(wide, dme_lookup, by = by_vars)
  }

  # --- Level numbering (SAS lines 336-342) ------------------------------------
  wide <- wide %>%
    dplyr::mutate(level = max_by)

  # Assign sequential numbers per by-level group
  for (i in seq_len(max_by)) {
    by_i <- by_vars[i]
    lbl  <- sub("_name$", "", by_i)
    wide <- wide %>%
      dplyr::group_by(.data[[by_i]]) %>%
      dplyr::mutate(!!lbl := dplyr::cur_group_id()) %>%
      dplyr::ungroup()
  }

  # --- Pairwise risk difference, relative risk, Fisher's exact (SAS 344-491) --
  if (arm_count > 1L) {
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i != j) {
          a_col <- paste0("arm", i, "_count")
          b_col <- paste0("arm", j, "_count")
          rd_col <- paste0("rd", i, j)
          rr_col <- paste0("rr", i, j)
          cc_col <- paste0("cc", i, j)

          wide <- wide %>%
            dplyr::mutate(
              !!rd_col := .data[[paste0("arm", i, "_pct")]] -
                          .data[[paste0("arm", j, "_pct")]],
              # Set up 2x2 cells
              .a = .data[[a_col]],
              .b = arm_N[i] - .data[[a_col]],
              .c = .data[[b_col]],
              .d = arm_N[j] - .data[[b_col]]
            )

          # Continuity correction when c == 0 (SAS lines 378-401)
          if (cc_sw != 0L) {
            if (cc_sw == 1L) {
              wide <- wide %>%
                dplyr::mutate(
                  !!cc_col := dplyr::if_else(.c == 0L, "*", NA_character_),
                  .a = dplyr::if_else(.c == 0L, .a + cc_val, .a),
                  .b = dplyr::if_else(.c == 0L, .b + cc_val, .b),
                  .c = dplyr::if_else(.c == 0L, .c + cc_val, .c),
                  .d = dplyr::if_else(.c == 0L, .d + cc_val, .d)
                )
            } else if (cc_sw == 2L) {
              wide <- wide %>%
                dplyr::mutate(
                  !!cc_col := dplyr::if_else(.c == 0L, "*", NA_character_),
                  .a = dplyr::if_else(.c == 0L, .a + 1 / (.c + .d), .a),
                  .b = dplyr::if_else(.c == 0L, .b + 1 / (.c + .d), .b),
                  .c = dplyr::if_else(.c == 0L, .c + 1 / (.a + .b), .c),
                  .d = dplyr::if_else(.c == 0L, .d + 1 / (.a + .b), .d)
                )
            }
          }

          # Relative risk (SAS line 404)
          wide <- wide %>%
            dplyr::mutate(
              !!rr_col := dplyr::if_else(
                .c != 0,
                (.a / (.a + .b)) / (.c / (.c + .d)),
                NA_real_
              )
            )

          # Clean up temp columns
          wide <- dplyr::select(wide, -dplyr::all_of(c(".a", ".b", ".c", ".d")))
        }
      }
    }

    # --- Fisher's exact test (SAS lines 416-492) ----------------------------
    # Build arm-pair n×2 tables and run fisher.test
    for (i in seq_len(arm_count)) {
      for (j in seq_len(arm_count)) {
        if (i < j) {
          pv_ij <- paste0("pv", i, j)
          pv_ji <- paste0("pv", j, i)

          fisher_pvals <- wide %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              .pv = {
                a_val <- .data[[paste0("arm", i, "_count")]]
                c_val <- .data[[paste0("arm", j, "_count")]]
                b_val <- arm_N[i] - a_val
                d_val <- arm_N[j] - c_val
                mat <- matrix(c(a_val, c_val, b_val, d_val), nrow = 2)
                tryCatch(
                  stats::fisher.test(mat)$p.value,
                  error = function(e) NA_real_
                )
              }
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              !!pv_ij := -log(.pv),
              !!pv_ji := -log(.pv)
            ) %>%
            dplyr::select(dplyr::all_of(c(pv_ij, pv_ji)))

          wide <- dplyr::bind_cols(
            dplyr::select(wide, -dplyr::any_of(c(pv_ij, pv_ji))),
            fisher_pvals
          )
        }
      }
    }
  }

  wide
}


# ==============================================================================
# meddra_cmp — Build Comparison Datasets for MedDRA Output
# ==============================================================================
# Replaces SAS %meddra_cmp macro (lines 525-605 of ae_meddra.sas).
# Stacks the four levels of MedDRA aggregation (meddra_1..meddra_4),
# derives row numbers for hidden data sheet referencing, and creates
# both a visible comparison output dataset and a hidden data sheet dataset.
#
# @param meddra_1  Tibble from meddra_aggregate at SOC level.
# @param meddra_2  Tibble from meddra_aggregate at SOC/HLGT level.
# @param meddra_3  Tibble from meddra_aggregate at SOC/HLGT/HLT level.
# @param meddra_4  Tibble from meddra_aggregate at SOC/HLGT/HLT/PT level.
# @param arm_info  Named list with arm_count, arm_N, arm_names.
# @param cc_sw     Integer — continuity correction switch.
# @return Named list with meddra_cmp_output, meddra_cmp_data,
#         meddra_cmp_output_row, meddra_cmp_data_row.
# ==============================================================================
meddra_cmp <- function(meddra_1, meddra_2, meddra_3, meddra_4,
                       arm_info, cc_sw) {

  # Stack all four levels with level indicator (SAS lines 528-538)
  all_levels <- dplyr::bind_rows(
    meddra_1 %>% dplyr::mutate(level = 1L),
    meddra_2 %>% dplyr::mutate(level = 2L),
    meddra_3 %>% dplyr::mutate(level = 3L),
    meddra_4 %>% dplyr::mutate(level = 4L)
  )

  # Ensure hierarchy columns exist in all rows (fill NA for upper levels)
  for (col in c("soc_name", "hlgt_name", "hlt_name", "pt_name")) {
    if (!col %in% names(all_levels)) {
      all_levels[[col]] <- NA_character_
    }
  }

  # Data row numbering for hidden sheet (SAS line 537)
  cmp_data <- all_levels %>%
    dplyr::mutate(row = dplyr::row_number())

  # Sort for visible sheet (SAS lines 541-543)
  cmp_output <- all_levels %>%
    dplyr::arrange(soc_name, hlgt_name, hlt_name, pt_name, level) %>%
    dplyr::mutate(row = dplyr::row_number())

  # Build data row lookup (SAS lines 590-603)
  build_row_lookup <- function(ds) {
    ds %>%
      dplyr::mutate(
        lvl_nm = dplyr::case_when(
          level == 1L ~ "soc",
          level == 2L ~ "hlgt",
          level == 3L ~ "hlt",
          level == 4L ~ "pt",
          TRUE        ~ NA_character_
        )
      ) %>%
      dplyr::mutate(
        lvl_no = dplyr::case_when(
          level == 1L ~ soc,
          level == 2L ~ hlgt,
          level == 3L ~ hlt,
          level == 4L ~ pt,
          TRUE        ~ NA_integer_
        )
      ) %>%
      dplyr::select(dplyr::any_of(c("row", "lvl_nm", "lvl_no",
                                      "soc", "hlgt", "hlt", "pt")))
  }

  list(
    meddra_cmp_output     = cmp_output,
    meddra_cmp_data       = cmp_data,
    meddra_cmp_output_row = build_row_lookup(cmp_output),
    meddra_cmp_data_row   = build_row_lookup(cmp_data)
  )
}


# ==============================================================================
# ae_meddra_panel — Main MedDRA at a Glance Panel Driver
# ==============================================================================
# Replaces SAS %params (lines 80-199), %aemed (lines 611-635), and the
# top-level execution block (lines 637-647) from ae_meddra.sas.
#
# Workflow:
#   1. Load AE/DM/EX data (from data frames or XPT file paths)
#   2. Optionally load MedDRA hierarchy and DME datasets
#   3. Normalize continuity correction parameters
#   4. Run ae_setup validation (setup function from ae_setup.R)
#   5. Run 4-level MedDRA aggregation (SOC, SOC/HLGT, SOC/HLGT/HLT,
#      SOC/HLGT/HLT/PT)
#   6. Build comparison datasets
#   7. Generate Excel output workbook
#
# @param ae            Data frame or path to AE domain file.
# @param dm            Data frame or path to DM domain file.
# @param ex            Data frame or path to EX domain file.
# @param meddra_data   Data frame of MedDRA hierarchy (optional; loaded from
#                      meddra_path if NULL).
# @param dme_data      Data frame of DME list (optional).
# @param meddra_path   Character path to directory with MedDRA hierarchy files.
# @param dme_path      Character path to directory with DME file.
# @param output_path   Character path to output directory for workbooks.
# @param panel_title   Character panel title for the cover sheet.
# @param panel_desc    Character panel description.
# @param ndabla        Character NDA/BLA number.
# @param studyid       Character study identifier.
# @param meddra_ver    Character MedDRA version string.
# @param study_lag     Integer window in days after last exposure.
# @param cc            Continuity correction: numeric, "arm", "none", or "0".
# @param vld_sw        Logical — perform data validation on AEs.
# @param rd_th         Numeric risk difference threshold (default 5).
# @param rr_th         Numeric relative risk threshold (default 5).
# @param pv_th         Numeric p-value threshold (default NA — no filter).
# @param sl_datasets   Data frame for Script Launcher datasets metadata.
# @param sl_group      Data frame for Script Launcher grouping metadata.
# @param sl_subset     Data frame for Script Launcher subsetting metadata.
# @param verbose       Logical — emit progress messages.
# @return Invisible named list with output_file path and analysis results.
# ==============================================================================
ae_meddra_panel <- function(
    ae,
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
    verbose       = TRUE
) {
  start_time <- proc.time()

  # ---------------------------------------------------------------------------
  # 1. Load input datasets (SAS lines 107-109)
  # ---------------------------------------------------------------------------
  ae_df <- load_data_file(ae, "AE")
  dm_df <- load_data_file(dm, "DM")
  ex_df <- load_data_file(ex, "EX")

  # ---------------------------------------------------------------------------
  # 2. Determine MedDRA usage flag (SAS lines 122-128)
  # ---------------------------------------------------------------------------
  use_meddra <- TRUE
  if (is.character(meddra_ver) &&
      toupper(substr(trimws(meddra_ver), 1, 1)) == "N") {
    use_meddra <- FALSE
  }

  # ---------------------------------------------------------------------------
  # 3. Load MedDRA hierarchy if needed (SAS lines 119, 166)
  # ---------------------------------------------------------------------------
  if (use_meddra && is.null(meddra_data) &&
      !is.null(meddra_path) && nzchar(meddra_path)) {
    meddra_files <- list.files(
      meddra_path,
      pattern = "mdhier.*\\.(xpt|sas7bdat)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(meddra_files) > 0L) {
      mf <- meddra_files[1L]
      if (verbose) cli::cli_inform(c("i" = "Loading MedDRA hierarchy: {mf}"))
      meddra_data <- if (grepl("\\.xpt$", mf, ignore.case = TRUE)) {
        haven::read_xpt(mf)
      } else {
        haven::read_sas(mf)
      }
      names(meddra_data) <- tolower(names(meddra_data))
    } else {
      cli::cli_warn("No MedDRA hierarchy file found in: {.path {meddra_path}}")
      use_meddra <- FALSE
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Load DME list if needed (SAS lines 131-133)
  # ---------------------------------------------------------------------------
  if (is.null(dme_data) && !is.null(dme_path) && nzchar(dme_path)) {
    dme_files <- list.files(
      dme_path,
      pattern = "dme\\.(xpt|sas7bdat)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(dme_files) > 0L) {
      df <- dme_files[1L]
      if (verbose) cli::cli_inform(c("i" = "Loading DME list: {df}"))
      dme_data <- if (grepl("\\.xpt$", df, ignore.case = TRUE)) {
        haven::read_xpt(df)
      } else {
        haven::read_sas(df)
      }
      names(dme_data) <- tolower(names(dme_data))
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Normalize continuity correction (SAS lines 219-233)
  # ---------------------------------------------------------------------------
  cc_sw <- 0L
  cc_whole <- 1L
  cc_val_num <- 0

  if (is.character(cc)) {
    cc_clean <- tolower(trimws(cc))
    if (cc_clean == "arm") {
      cc_sw <- 2L; cc_whole <- 0L
    } else if (cc_clean %in% c("none", "0")) {
      cc_sw <- 0L
    } else {
      cc_val_num <- suppressWarnings(as.numeric(cc_clean))
      if (!is.na(cc_val_num) && cc_val_num != 0) {
        cc_sw <- 1L
        cc_whole <- as.integer(cc_val_num == floor(cc_val_num))
      }
    }
  } else if (is.numeric(cc)) {
    if (cc != 0) {
      cc_sw <- 1L
      cc_val_num <- cc
      cc_whole <- as.integer(cc == floor(cc))
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Set up output paths (SAS lines 97-100)
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  output_file <- file.path(output_path,
                           "MedDRA at a Glance Analysis Panel.xlsx")
  err_file <- file.path(output_path,
                        "MedDRA at a Glance Error Summary.xlsx")

  # ---------------------------------------------------------------------------
  # 7. Run ae_setup validation (SAS line 613: %setup(mdhier=Y, dme=Y))
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_h2("Running setup validation")

  setup_result <- tryCatch(
    setup(
      ae        = ae_df,
      dm        = dm_df,
      ex        = ex_df,
      mdhier    = use_meddra,
      dme       = !is.null(dme_data),
      study_lag = study_lag,
      vld_sw    = vld_sw,
      meddra_data = meddra_data,
      dme_data    = dme_data
    ),
    error = function(e) {
      cli::cli_warn("Setup failed: {conditionMessage(e)}")
      list(setup_success = FALSE, dm_subj_gt0 = FALSE,
           setup_req_var = FALSE, meddra_pct = 0)
    }
  )

  # ---------------------------------------------------------------------------
  # 8. Run analysis if setup succeeded and MedDRA match > 0% (SAS lines 615-634)
  # ---------------------------------------------------------------------------
  setup_ok <- isTRUE(setup_result$setup_success)
  meddra_pct <- setup_result$meddra_pct %||% 0

  if (setup_ok && meddra_pct > 0) {
    if (verbose) cli::cli_h2("Running MedDRA aggregation (4 levels)")

    ds_base <- setup_result$ds_base_meddra %||% setup_result$ds_base
    arm_info <- list(
      arm_count = setup_result$arm_count,
      arm_N     = setup_result$arm_N,
      arm_names = setup_result$arm_names
    )

    # 4-level MedDRA aggregation (SAS lines 617-620)
    meddra_1 <- meddra_aggregate(
      ds_base, c("soc_name"), arm_info, cc_sw, cc_val_num
    )
    meddra_2 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name"), arm_info, cc_sw, cc_val_num
    )
    meddra_3 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name", "hlt_name"),
      arm_info, cc_sw, cc_val_num
    )
    meddra_4 <- meddra_aggregate(
      ds_base, c("soc_name", "hlgt_name", "hlt_name", "pt_name"),
      arm_info, cc_sw, cc_val_num
    )

    # Build comparison datasets (SAS line 622)
    cmp_results <- meddra_cmp(
      meddra_1, meddra_2, meddra_3, meddra_4,
      arm_info, cc_sw
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
        arm_info   = arm_info,
        cc_sw      = cc_sw,
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
        cli::cli_warn("Output generation failed: {conditionMessage(e)}")
      }
    )

    if (verbose) cli::cli_alert_success("MedDRA analysis complete.")
  } else {
    # Error summary output (SAS lines 627-633)
    if (verbose) cli::cli_alert_warning("Setup validation failed or no MedDRA matches.")
    tryCatch(
      error_summary(
        err_file  = err_file,
        err_nosubj = !isTRUE(setup_result$dm_subj_gt0),
        err_missvar = !isTRUE(setup_result$setup_req_var),
        err_desc = if (meddra_pct == 0) {
          "There were zero adverse events with matching MedDRA descriptions."
        } else {
          NA_character_
        }
      ),
      error = function(e) {
        cli::cli_warn("Error summary output failed: {conditionMessage(e)}")
      }
    )
  }

  # ---------------------------------------------------------------------------
  # 9. Timing (SAS lines 638-647)
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  if (verbose) {
    cli::cli_inform(c("i" = paste0(
      "Running time: ",
      sprintf("%02d:%05.2f", as.integer(elapsed) %/% 60, elapsed %% 60)
    )))
  }

  invisible(list(
    output_file   = output_file,
    err_file      = err_file,
    setup_result  = setup_result,
    meddra_1      = if (exists("meddra_1")) meddra_1 else NULL,
    meddra_2      = if (exists("meddra_2")) meddra_2 else NULL,
    meddra_3      = if (exists("meddra_3")) meddra_3 else NULL,
    meddra_4      = if (exists("meddra_4")) meddra_4 else NULL,
    cmp_results   = if (exists("cmp_results")) cmp_results else NULL
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. The setup() function from ae_setup.R returns a named list
#      with elements: setup_success, ds_base, ds_base_meddra,
#      arm_count, arm_N, arm_names, meddra_pct, dm_subj_gt0,
#      setup_req_var. This mirrors the tested/R equivalent.
#   2. Fisher's exact test uses R's stats::fisher.test() which
#      computes the exact 2-sided p-value via the hypergeometric
#      distribution.
#   3. Continuity correction is applied only when c (control arm
#      count) equals 0, matching the SAS logic at line 380.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. Fisher's exact p-values: SAS PROC FREQ vs R fisher.test()
#      may differ by ~1e-5 for large sparse tables due to
#      algorithmic differences (network algorithm vs. enumeration).
#   2. -log(p) values inherit the above p-value differences.
#   3. Arm percentage calculations use R double precision; SAS
#      also uses IEEE 754 doubles but intermediate rounding in
#      the PDV may cause sub-epsilon differences.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS SpreadsheetML XML is replaced by openxlsx API (handled
#      in ae_meddra_output.R and xml_output.R).
#   2. SAS PROC DATASETS DELETE is unnecessary in R (garbage
#      collected automatically).
#
# PACKAGE SELECTION RATIONALE:
#   haven — SAS data I/O (read_xpt, read_sas)
#   dplyr — Data manipulation replacing SAS DATA steps and PROC SQL
#   tidyr — Pivoting and completing sparse arm-count grids
#   purrr — Functional iteration for arm N lookups
#   forcats — Factor level management for MedDRA hierarchy ordering
#   janitor — SAS-compatible rounding via round_half_up()
#   openxlsx — Excel output replacing SpreadsheetML XML
#   cli — User-facing informative messages
#
# OPEN QUESTIONS:
#   1. The SAS MedDRA hierarchy is loaded from sas7bdat files with
#      a versioned filename pattern (mdhier_14_0.sas7bdat). The R
#      code supports both XPT and SAS7BDAT; confirm which format
#      is standard in the production environment.
#   2. The SAS code uses options missing='' which displays missing
#      values as blank; R NA values will display as "NA" in output
#      unless explicitly converted. The output module handles this.
# ============================================================
