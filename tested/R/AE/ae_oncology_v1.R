# ==============================================================================
# PROGRAM: ae_oncology_v1.R
# DESCRIPTION: AE Toxicity (Oncology) Analysis Panel (R Migration)
#   - Find subject counts by toxicity grade for each AE
#   - Find preferred term analysis by grade per arm
#   - Two-arm comparison: RD, RR, OR, CI, p-value with continuity correction
#   - Optional MedDRA hierarchy integration (SOC/PT)
#   - Creates output workbook with 3 analyses
# MIGRATED FROM: tested/SAS/AE/ae_oncology_v1.sas (390 lines)
# EVALUATION TYPE: Safety - Oncology
# AUTHORS: David Kretch, Andreas Anastassopoulos (original SAS)
# DATE: February 7, 2011 (original); R migration 2026
# ==============================================================================

# --- Library loading ----------------------------------------------------------
# Core tidyverse ecosystem — AAP mandates tidyverse over base R
library(haven)      # SAS data I/O: read_xpt() for CDISC ADaM/SDTM datasets
library(dplyr)      # Data manipulation: replacing SAS DATA steps and PROC SQL
library(tidyr)      # Reshaping: pivoting, completing analysis result structures
library(stringr)    # String manipulation: MedDRA term processing, arm labels
library(cli)        # User-facing diagnostic messages replacing SAS %PUT
library(janitor)    # SAS-compatible rounding via round_half_up()
library(openxlsx)   # Excel output replacing SAS SpreadsheetML XML generation

# --- Source internal dependencies ---------------------------------------------
# All paths are relative to repository root; sourced for environment
# availability of the functions defined in each module.
# NOTE: In production use, callers should ensure the working directory
# is set to the repository root, or adjust paths via a config object.

# Determine the directory of this script for relative sourcing
.ae_onc_v1_script_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE),
  error = function(e) "."
)
.ae_onc_v1_repo_root <- tryCatch(
  normalizePath(file.path(.ae_onc_v1_script_dir, "..", "..", ".."),
                mustWork = FALSE),
  error = function(e) "."
)

# Source dependency modules using repo-root-relative paths
# These correspond to SAS %include statements (lines 283-289 of ae_oncology_v1.sas)
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "utilities", "ae_setup.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "macros",
                 "ae_oncology_aggregate.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "macros",
                 "ae_oncology_output.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "utilities",
                 "data_checks.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "utilities",
                 "err_output.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "utilities",
                 "sl_gs_output.R"))
source(file.path(.ae_onc_v1_repo_root, "tested", "R", "utilities",
                 "xml_output.R"))


# ==============================================================================
# ae_oncology_v1
# ==============================================================================
# Main oncology AE toxicity analysis panel driver.
#
# Replaces SAS %params macro (lines 80-268) for parameter setup and
# %onc macro (lines 310-378) for the analysis workflow.
#
# Workflow:
#   1. Load AE/DM/EX data (from data frames or XPT file paths)
#   2. Optionally load MedDRA hierarchy data
#   3. Normalize continuity correction parameters
#   4. Run setup validation (ae_setup.R: setup_validation)
#   5. Validate MedDRA match percentage (80% threshold)
#   6. Resolve experimental and control arm designations
#   7. Run Analysis 1: Toxicity Grade Summary (total aggregate)
#   8. Run Analysis 2: Preferred Term by Grade (SOC/PT aggregate)
#   9. Run Analysis 3: Two-Arm Comparison with RD/RR/OR (conditional)
#  10. Generate Excel output workbook
#
# Parameters:
#   ae            - AE domain data frame or path to XPT file
#   dm            - DM domain data frame or path to XPT file
#   ex            - EX domain data frame or path to XPT file
#   panel_title   - Character: panel title for output headers (SAS line ~100)
#   panel_desc    - Character: panel description (SAS line ~101)
#   outpath       - Character: output directory path (SAS line ~107)
#   oncaeout      - Character: output workbook filename (SAS line ~108)
#   errout        - Character: error summary filename (SAS line ~110)
#   ndabla        - Character: NDA/BLA number (SAS line ~123)
#   studyid       - Character: study identifier (SAS line ~124)
#   meddra_path   - Character or NULL: path to MedDRA hierarchy data (SAS ~131)
#   meddra_ver    - Character: MedDRA version; "N" disables MedDRA (SAS ~135)
#   study_lag     - Numeric: days after last exposure (default 30, SAS ~140)
#   toxgr_grp5_sw - Logical: grade 5 grouping switch (SAS ~143)
#   exp_arm       - Integer or character: experimental arm designator (SAS ~153)
#   ctl_arm       - Integer or character: control arm designator (SAS ~154)
#   cmptrm        - Character vector: comparison terms (SAS ~157)
#   cmpgr         - Character: comparison grade group (SAS ~160)
#   cmpsort       - Character: sort metric for comparison (SAS ~163)
#   cc            - Numeric or "arm": continuity correction (SAS ~176)
#   toxgr_min     - Integer: minimum toxicity grade (SAS ~227)
#   toxgr_max     - Integer: maximum toxicity grade (SAS ~228)
#   ae_rate_ci_sw - Logical: AE rate CI switch (SAS ~229)
#   vld_sw        - Logical: validation switch (SAS ~272)
#
# Returns:
#   Invisible named list with:
#     success       - Logical: overall success flag
#     pt_1          - Analysis 1 output (total aggregate)
#     pt_2          - Analysis 2 output (SOC/PT formatted)
#     pt_3          - Analysis 3 output (comparison formatted, or NULL)
#     meddra_active - Logical: whether MedDRA was used
#     setup         - Full setup_validation return list
# ==============================================================================
ae_oncology_v1 <- function(
  # Data inputs
  ae,
  dm,
  ex,

  # Panel metadata — SAS lines ~100-101

  panel_title = "AE Toxicity",
  panel_desc = "",

  # Path configuration — SAS lines ~107-110
  outpath = ".",

  oncaeout = "AE Toxicity Analysis.xlsx",
  errout = "AE Toxicity Error Summary.xlsx",

  # Study identifiers — SAS lines ~123-124
  ndabla = "",
  studyid = "",

  # MedDRA configuration — SAS lines ~131-136
  meddra_path = NULL,
  meddra_ver = "14.1",

  # Analysis parameters — SAS lines ~140-176
  study_lag = 30,
  toxgr_grp5_sw = TRUE,
  exp_arm = 1L,
  ctl_arm = 2L,
  cmptrm = c("soc_name", "pt_name"),
  cmpgr = "all",
  cmpsort = "rr",
  cc = 0.5,

  # Derived parameters — SAS lines ~227-229
  toxgr_min = 1L,
  toxgr_max = 5L,
  ae_rate_ci_sw = TRUE,

  # Validation switch — SAS line ~272
  vld_sw = TRUE
) {

  # Record start time for performance logging (SAS lines 385-390)
  start_time <- proc.time()

  # ============================================================================
  # STEP 1: DATA LOADING (SAS lines 112-117, 127)
  # ============================================================================
  # Accept either pre-loaded data frames or file paths to XPT files.
  # When paths are provided, use haven::read_xpt() for SAS transport files.
  cli::cli_inform("Starting AE Oncology Toxicity Panel analysis.")

  if (is.character(ae) && length(ae) == 1L) {
    if (!file.exists(ae)) {
      cli::cli_abort("AE data file not found: {.path {ae}}")
    }
    ae <- haven::read_xpt(ae)
  }
  if (is.character(dm) && length(dm) == 1L) {
    if (!file.exists(dm)) {
      cli::cli_abort("DM data file not found: {.path {dm}}")
    }
    dm <- haven::read_xpt(dm)
  }
  if (is.character(ex) && length(ex) == 1L) {
    if (!file.exists(ex)) {
      cli::cli_abort("EX data file not found: {.path {ex}}")
    }
    ex <- haven::read_xpt(ex)
  }

  # Validate inputs are data frames

  if (!is.data.frame(ae)) cli::cli_abort("{.arg ae} must be a data frame or XPT path.")
  if (!is.data.frame(dm)) cli::cli_abort("{.arg dm} must be a data frame or XPT path.")
  if (!is.data.frame(ex)) cli::cli_abort("{.arg ex} must be a data frame or XPT path.")

  # Normalize column names to lowercase (SAS is case-insensitive)
  ae <- ae %>% dplyr::rename_with(tolower)
  dm <- dm %>% dplyr::rename_with(tolower)
  ex <- ex %>% dplyr::rename_with(tolower)

  # ============================================================================
  # STEP 2: MEDDRA HIERARCHY LOADING (SAS lines ~131-136)
  # ============================================================================
  # SAS: libname mdhier "&meddrapath." access=readonly;
  # Load MedDRA hierarchy data from the specified path if provided.
  meddra_data <- NULL
  meddra_active <- FALSE

  if (!is.null(meddra_path) && nzchar(meddra_path)) {
    # Search for MedDRA hierarchy file in common formats
    meddra_xpt <- file.path(meddra_path, "mdhier.xpt")
    meddra_sas <- file.path(meddra_path, "mdhier.sas7bdat")

    if (file.exists(meddra_xpt)) {
      meddra_data <- haven::read_xpt(meddra_xpt) %>%
        dplyr::rename_with(tolower)
      meddra_active <- TRUE
      cli::cli_inform("Loaded MedDRA hierarchy from {.path {meddra_xpt}}.")
    } else if (file.exists(meddra_sas)) {
      meddra_data <- haven::read_sas(meddra_sas) %>%
        dplyr::rename_with(tolower)
      meddra_active <- TRUE
      cli::cli_inform("Loaded MedDRA hierarchy from {.path {meddra_sas}}.")
    } else {
      cli::cli_warn(c(
        "!" = "MedDRA hierarchy file not found in {.path {meddra_path}}.",
        "i" = "Proceeding without MedDRA integration."
      ))
    }
  }

  # ============================================================================
  # STEP 3: MEDDRA VERSION CHECK (SAS lines 312-313)
  # ============================================================================
  # SAS: %if %upcase(%substr(&ver.,1,1)) = N %then %do;
  #        %setup; %global meddra; %let meddra = N;
  # If meddra_ver starts with "N", disable MedDRA entirely.
  if (!is.null(meddra_ver) && nzchar(meddra_ver)) {
    if (toupper(substr(trimws(meddra_ver), 1, 1)) == "N") {
      meddra_active <- FALSE
      meddra_data <- NULL
      cli::cli_inform("MedDRA disabled by version parameter ('{meddra_ver}').")
    }
  }

  # ============================================================================
  # STEP 4: CONTINUITY CORRECTION NORMALIZATION (SAS lines 291-307)
  # ============================================================================
  # Three modes:
  #   cc = "arm"       -> cc_sw=2, cc_whole=0 (reciprocal of opposite arm N)
  #   cc = non-zero    -> cc_sw=1, cc_whole depends on integer check
  #   cc = 0 or absent -> cc_sw=0, cc_whole=0 (no correction)
  # Default for oncology panel: cc = 0.5 (differs from severity panel's 0)
  cc_sw <- 0L
  cc_whole <- 0L
  cc_value <- 0

  if (is.character(cc)) {
    if (tolower(trimws(cc)) == "arm") {
      cc_sw <- 2L
      cc_whole <- 0L
      cc_value <- NA_real_
    } else {
      cc_num <- suppressWarnings(as.numeric(cc))
      if (!is.na(cc_num) && cc_num != 0) {
        cc_sw <- 1L
        cc_whole <- as.integer(cc_num == floor(cc_num))
        cc_value <- cc_num
      }
    }
  } else if (is.numeric(cc)) {
    if (!is.na(cc) && cc != 0) {
      cc_sw <- 1L
      cc_whole <- as.integer(cc == floor(cc))
      cc_value <- cc
    }
  }

  # Build human-readable CC description for output workbook
  cc_desc <- if (cc_sw == 0L) {
    "None"
  } else if (cc_sw == 2L) {
    "Reciprocal of opposite arm N"
  } else {
    as.character(cc_value)
  }

  # ============================================================================
  # STEP 5: SETUP VALIDATION (SAS lines 283, 312-316)
  # ============================================================================
  # Call setup_validation which merges AE/DM/EX domains, validates required
  # and optional variables, builds the safety population, assigns arms,
  # computes arm counts, and optionally processes MedDRA lookup.
  # SAS: %if %upcase(%substr(&ver.,1,1)) = N %then %setup;
  #      %else %setup(mdhier=Y);
  cli::cli_inform("Running setup validation...")

  setup_result <- setup_validation(
    ae          = ae,
    dm          = dm,
    ex          = ex,
    mdhier      = meddra_active,
    meddra_data = meddra_data,
    vld_sw      = vld_sw,
    study_lag   = study_lag,
    toxgr_min   = toxgr_min,
    toxgr_max   = toxgr_max
  )

  # ============================================================================
  # STEP 6: SETUP FAILURE PATH (SAS lines 367-375)
  # ============================================================================
  # If setup failed (no subjects in DM or missing required variables),
  # produce error summary workbook and return early.
  if (!setup_result$setup_success) {
    cli::cli_warn("Setup validation failed. Generating error summary.")

    error_summary(
      err_file        = file.path(outpath, errout),
      panel_title     = panel_title,
      panel_desc      = panel_desc,
      ndabla          = ndabla,
      studyid         = studyid,
      err_nosubj      = !setup_result$dm_subj_gt0,
      err_missvar     = !setup_result$setup_req_var,
      rpt_chk_var_req = setup_result$rpt_chk_var_req
    )

    elapsed <- (proc.time() - start_time)[["elapsed"]]
    cli::cli_inform("Oncology AE panel completed (error path) in {round(elapsed, 1)}s.")

    return(invisible(list(
      success       = FALSE,
      pt_1          = NULL,
      pt_2          = NULL,
      pt_3          = NULL,
      meddra_active = meddra_active,
      setup         = setup_result
    )))
  }

  # ============================================================================
  # STEP 7: POST-SETUP MEDDRA VALIDATION (SAS line 319)
  # ============================================================================
  # SAS: %if &meddra_pct. < 80 %then %let meddra = N;
  # If MedDRA was requested but the match percentage is below 80%,
  # disable MedDRA and fall back to standard AE terms.
  meddra_pct <- setup_result$meddra_pct

  if (meddra_active) {
    if (is.na(meddra_pct) || meddra_pct < 80) {
      cli::cli_warn(c(
        "!" = "MedDRA match percentage ({if (is.na(meddra_pct)) 'NA' else paste0(round(meddra_pct, 1), '%')}) below 80% threshold.",
        "i" = "Disabling MedDRA; falling back to AEBODSYS/AEDECOD."
      ))
      meddra_active <- FALSE
    } else {
      cli::cli_inform("MedDRA match: {round(meddra_pct, 1)}% (threshold: 80%).")
    }
  }

  # Character flag for output workbook (SAS: &meddra = Y/N)
  meddra_flag <- if (meddra_active) "Y" else "N"

  # ============================================================================
  # STEP 8: ARM RESOLUTION (SAS lines 322-339)
  # ============================================================================
  # Extract arm metadata from setup result
  arm_count <- setup_result$arm_count
  arm_names <- setup_result$arm_names
  arm_display_names <- setup_result$arm_display_names

  # Derive per-arm safety population subject counts from all_dm_ex
  # SAS: arm_N_1, arm_N_2, ..., arm_N_{arm_count} global macro variables
  arm_subjcnt <- setup_result$all_dm_ex %>%
    dplyr::group_by(.data$arm_num) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data$usubjid), .groups = "drop") %>%
    dplyr::arrange(.data$arm_num) %>%
    dplyr::pull(.data$n)

  # Ensure arm_subjcnt has exactly arm_count elements
  if (length(arm_subjcnt) < arm_count) {
    arm_subjcnt <- c(arm_subjcnt,
                     rep(0L, arm_count - length(arm_subjcnt)))
  }

  # Resolve experimental and control arm designators

  # SAS lines 322-339: if not LOCAL mode, resolve from arm labels
  # In R, accept integer indices directly OR character arm names
  if (is.character(exp_arm)) {
    exp_idx <- which(tolower(arm_names) == tolower(trimws(exp_arm)))
    if (length(exp_idx) == 0L) {
      # Partial match fallback (SAS: =: operator for prefix matching)
      exp_idx <- which(startsWith(tolower(arm_names),
                                  tolower(trimws(exp_arm))))
    }
    exp_arm <- if (length(exp_idx) > 0L) exp_idx[1L] else 1L
  }
  if (is.character(ctl_arm)) {
    ctl_idx <- which(tolower(arm_names) == tolower(trimws(ctl_arm)))
    if (length(ctl_idx) == 0L) {
      ctl_idx <- which(startsWith(tolower(arm_names),
                                  tolower(trimws(ctl_arm))))
    }
    ctl_arm <- if (length(ctl_idx) > 0L) ctl_idx[1L] else min(2L, arm_count)
  }

  # Ensure arm designators are integers within valid range
  exp_arm <- as.integer(exp_arm)
  ctl_arm <- as.integer(ctl_arm)
  if (is.na(exp_arm) || exp_arm < 1L || exp_arm > arm_count) exp_arm <- 1L
  if (is.na(ctl_arm) || ctl_arm < 1L || ctl_arm > arm_count) {
    ctl_arm <- min(2L, arm_count)
  }

  cli::cli_inform(paste0(
    "Arms resolved: experimental = ", arm_names[exp_arm],
    " (", exp_arm, "), control = ", arm_names[ctl_arm],
    " (", ctl_arm, ")."
  ))

  # ============================================================================
  # STEP 9: GRADE-5 GROUPING ADJUSTMENT (SAS line 345-346)
  # ============================================================================
  # SAS: %if &toxgr_max. = 4 %then %let toxgr_grp5_sw = 0;
  # Cannot group grade 5 with 3 and 4 if 4 is the upper bound.
  if (toxgr_max == 4L) {
    toxgr_grp5_sw <- FALSE
  }

  # Detect whether AETOXGR variable exists in the AE dataset
  # (needed for onc_out_workbook ae_aetoxgr parameter)
  ae_aetoxgr <- "aetoxgr" %in% colnames(setup_result$ds_base)

  # ============================================================================
  # STEP 10: ANALYSIS 1 — TOXICITY GRADE SUMMARY (SAS line 349)
  # ============================================================================
  # SAS: %aggregate(ds_base, pt_1, total);
  # Aggregate all AEs by toxicity grade across all arms, using a constant

  # "total" column as the single BY variable.
  cli::cli_inform("Running Analysis 1: Toxicity Grade Summary (total aggregate).")

  # Add constant "total" BY variable (SAS passes "total" as by1)
  ds_pt1 <- setup_result$ds_base %>%
    dplyr::mutate(total = "Total")

  pt_1_result <- onc_aggregate(
    ds            = ds_pt1,
    by_vars       = c("total"),
    arm_count     = arm_count,
    arm_names     = arm_display_names,
    arm_subjcnt   = arm_subjcnt,
    toxgr_min     = toxgr_min,
    toxgr_max     = toxgr_max,
    toxgr_grp5_sw = toxgr_grp5_sw,
    report        = TRUE,
    output        = TRUE
  )

  # Initialize accumulated report tibbles
  rpt_key <- pt_1_result$rpt_key_row
  rpt_missing <- pt_1_result$rpt_missing_row

  # ============================================================================
  # STEP 11: ANALYSIS 2 — PREFERRED TERM ANALYSIS BY GRADE (SAS lines 352-353)
  # ============================================================================
  # SAS: %aggregate(ds_base, pt_2, aebodsys, aedecod);
  #      %fmt_output(pt_2_output);
  cli::cli_inform("Running Analysis 2: Preferred Term Analysis by Grade (SOC/PT).")

  pt_2_result <- onc_aggregate(
    ds            = setup_result$ds_base,
    by_vars       = c("aebodsys", "aedecod"),
    arm_count     = arm_count,
    arm_names     = arm_display_names,
    arm_subjcnt   = arm_subjcnt,
    toxgr_min     = toxgr_min,
    toxgr_max     = toxgr_max,
    toxgr_grp5_sw = toxgr_grp5_sw,
    report        = TRUE,
    output        = TRUE
  )

  # Accumulate report metadata
  rpt_key <- dplyr::bind_rows(rpt_key, pt_2_result$rpt_key_row)
  rpt_missing <- dplyr::bind_rows(rpt_missing, pt_2_result$rpt_missing_row)

  # Format output for display (SAS: %fmt_output(pt_2_output))
  pt_2_formatted <- onc_fmt_output(
    data         = pt_2_result$output,
    by_vars      = c("aebodsys", "aedecod"),
    rpt_key_tbl  = rpt_key,
    ds_name      = "pt_2",
    sort_sw      = FALSE,
    sortvar      = NULL,
    sortgrp_sw   = FALSE,
    cc_sw        = cc_sw,
    cc_ind_tbl   = NULL
  )

  # ============================================================================
  # STEP 12: ANALYSIS 3 — TWO-ARM COMPARISON (SAS lines 356-364)
  # ============================================================================
  # SAS: %if &arm_count. > 1 %then %do;
  #        %if &meddra. = Y %then %compare(ds_base_meddra, pt_3, &cmptrm.);
  #        %else %do; %let cmptrm = aebodsys,aedecod;
  #                   %compare(ds_base, pt_3, &cmptrm.); %end;
  #        %fmt_output(pt_3_output, sort_sw=yes, sortvar=&cmpsort.,
  #                    sortgrp_sw=yes);
  #      %end;
  pt_3_result <- NULL
  pt_3_formatted <- NULL

  if (arm_count > 1L) {
    cli::cli_inform("Running Analysis 3: Two-Arm Comparison.")

    # When MedDRA matching was successful, ds_base already contains
    # MedDRA columns (soc_name, hlgt_name, hlt_name, pt_name) from
    # setup_validation(mdhier=TRUE). Use ds_base directly.
    # When MedDRA is disabled, fall back to aebodsys/aedecod.
    ds_compare <- setup_result$ds_base
    cmptrm_use <- cmptrm

    if (!meddra_active) {
      # SAS: %let cmptrm = aebodsys,aedecod; (line 362)
      cmptrm_use <- c("aebodsys", "aedecod")
      cli::cli_inform("MedDRA inactive; comparison uses aebodsys/aedecod.")
    }

    # SAS: %compare(ds, pt_3, &cmptrm.)
    pt_3_result <- onc_compare(
      ds            = ds_compare,
      by_vars       = cmptrm_use,
      arm_count     = arm_count,
      arm_names     = arm_display_names,
      arm_subjcnt   = arm_subjcnt,
      toxgr_min     = toxgr_min,
      toxgr_max     = toxgr_max,
      cmpgr         = cmpgr,
      ctl           = ctl_arm,
      exp           = exp_arm,
      cc_sw         = cc_sw,
      cc_whole      = as.logical(cc_whole),
      cc_value      = cc_value,
      ae_rate_ci_sw = ae_rate_ci_sw,
      report        = TRUE
    )

    # Accumulate report metadata
    rpt_key <- dplyr::bind_rows(rpt_key, pt_3_result$rpt_key_row)
    rpt_missing <- dplyr::bind_rows(rpt_missing, pt_3_result$rpt_missing_row)

    # SAS: %fmt_output(pt_3_output, sort_sw=yes, sortvar=&cmpsort.,
    #                  sortgrp_sw=yes)
    pt_3_formatted <- onc_fmt_output(
      data         = pt_3_result$compare_output,
      by_vars      = cmptrm_use,
      rpt_key_tbl  = rpt_key,
      ds_name      = "pt_3",
      sort_sw      = TRUE,
      sortvar      = cmpsort,
      sortgrp_sw   = TRUE,
      sortdir      = "desc",
      cc_sw        = cc_sw,
      cc_ind_tbl   = pt_3_result$compare_cc_ind
    )
  } else {
    cli::cli_inform("Only 1 arm detected; skipping two-arm comparison.")
  }

  # ============================================================================
  # STEP 13: EXCEL OUTPUT GENERATION (SAS line 366: %out_onc)
  # ============================================================================
  # Generate the complete AE Toxicity Analysis Excel workbook with cover page,
  # toxicity grade summary (pt_1), preferred-term analysis (pt_2), two-arm
  # comparison with RD/RR/OR (pt_3), data-check summary, and grouping/
  # subsetting worksheets.
  cli::cli_inform("Generating Excel output workbook.")

  # Determine validation error flag for the workbook
  vld_err <- !setup_result$setup_success

  onc_out_workbook(
    output_file     = file.path(outpath, oncaeout),
    pt_1_output     = pt_1_result$output,
    pt_2_data       = pt_2_formatted,
    pt_3_data       = pt_3_formatted,
    rpt_dm          = setup_result$rpt_dm,
    rpt_err         = setup_result$rpt_err,
    rpt_err_term    = setup_result$rpt_err_term,
    rpt_missing     = rpt_missing,
    rpt_meddra      = setup_result$rpt_meddra,
    rpt_meddra_term = setup_result$rpt_meddra_term,
    rpt_key         = rpt_key,
    ndabla          = ndabla,
    studyid         = studyid,
    arm_count       = arm_count,
    arm_names       = arm_display_names,
    toxgr_max       = toxgr_max,
    toxgr_grp5_sw   = toxgr_grp5_sw,
    cmpgr           = cmpgr,
    meddra          = meddra_flag,
    meddra_pct      = meddra_pct,
    cc_sw           = cc_sw,
    cc_desc         = cc_desc,
    study_lag       = study_lag,
    vld_sw          = vld_sw,
    vld_err         = vld_err,
    ae_aetoxgr      = ae_aetoxgr,
    ae_rate_ci_sw   = ae_rate_ci_sw
  )

  # ============================================================================
  # STEP 14: TIMING AND RETURN VALUE (SAS lines 385-390)
  # ============================================================================
  elapsed <- (proc.time() - start_time)[["elapsed"]]
  elapsed_fmt <- sprintf("%02d:%02d",
                         as.integer(elapsed) %/% 60L,
                         as.integer(elapsed) %% 60L)
  cli::cli_inform("Oncology AE panel completed in {elapsed_fmt} ({round(elapsed, 1)}s).")

  invisible(list(
    success       = TRUE,
    pt_1          = pt_1_result,
    pt_2          = pt_2_formatted,
    pt_3          = pt_3_formatted,
    meddra_active = meddra_active,
    setup         = setup_result
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS %params + %onc macros consolidated into single ae_oncology_v1()
#      function with 24 named parameters matching all SAS macro variables
#    - MedDRA hierarchy loaded from XPT or SAS7BDAT file in meddra_path dir
#    - MedDRA match threshold of 80% preserved from SAS (line 319)
#    - Experimental arm = 1, Control arm = 2 defaults match SAS
#    - Toxicity grade variable: aetoxgr (integer 1-5 range)
#    - study_lag = 30 default (oncology-specific; severity panel uses 120)
#    - cc = 0.5 default (oncology-specific; severity panel uses 0)
#    - setup_validation(mdhier=TRUE) enriches ds_base with MedDRA columns
#      in-place, so no separate ds_base_meddra object is needed
#    - arm_subjcnt derived from all_dm_ex via n_distinct(usubjid) per arm_num
#    - SAS LOCAL/Script Launcher detection not applicable in R; arm resolution
#      supports both integer indices and character arm name matching
#    - Report tibbles (rpt_key, rpt_missing) accumulated via bind_rows()
#      across all three analyses before passing to output workbook
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - Fisher's exact test: R fisher.test() uses exact conditional inference;
#      SAS PROC FREQ EXACT FISHER uses same algorithm but implementation
#      details (workspace size, algorithm variant) may cause tie-breaking
#      differences at extreme p-values
#    - Continuity correction: 0.5 added to all four 2x2 cells when any cell
#      is zero, matching SAS behavior; verify edge cases with both arms at 0
#    - Risk difference CI: R prop.test() Wald CI vs SAS RISKDIFF; Newcombe
#      vs Wald method selection may differ at small sample sizes
#    - Relative risk CI: epitools::riskratio() log-based CI vs SAS RELRISK
#    - Odds ratio CI: Fisher exact CI used when no CC or whole-number CC;
#      log-based CI from epitools when fractional CC applied
#    - Rounding: janitor::round_half_up() used throughout for SAS-compatible
#      round-half-up behavior; R default banker's rounding not used
#    - Sort stability: R dplyr::arrange() is stable within groups; multi-key
#      sorts verified to match SAS PROC SORT behavior
#
# NO DIRECT R EQUIVALENT:
#    - SAS libname mdhier (MedDRA access) -> haven::read_xpt() + manual
#      hierarchy merge in setup_validation
#    - SAS %aggregate/%compare/%fmt_output macro chain -> R function
#      composition via onc_aggregate()/onc_compare()/onc_fmt_output()
#    - SAS format-based grade labeling -> R factor levels with explicit
#      label assignment via forcats or direct factor()
#    - SAS LOCAL/Script Launcher detection -> not needed in R; all
#      parameters passed explicitly to the function
#    - SAS call symputx() for global state -> R named list return values
#    - SAS SpreadsheetML XML generation -> openxlsx workbook pipeline
#
# PACKAGE SELECTION RATIONALE:
#    - haven (>=2.5.5): SAS data I/O; read_xpt() for CDISC transport files
#    - dplyr (>=1.1.0): all data manipulation replacing DATA steps
#    - tidyr (>=1.3.0): reshaping for aggregate result structures
#    - stringr (>=1.5.0): string operations for arm label matching
#    - cli (>=3.6.0): diagnostic messages replacing SAS %PUT
#    - janitor (>=2.2.0): round_half_up() for SAS-compatible rounding
#    - openxlsx (>=4.2.5): Excel output replacing SpreadsheetML XML
#    - No survival/mmrm needed for this panel (no time-to-event analysis)
#
# OPEN QUESTIONS:
#    - MedDRA hierarchy data format: XPT vs SAS7BDAT depends on site setup;
#      both formats supported with priority given to XPT
#    - MedDRA column names: soc_name, hlgt_name, hlt_name, pt_name expected;
#      if alternate naming (AESOC, AEDECOD), setup_validation handles mapping
#    - Continuity correction interaction with Fisher's exact: SAS applies CC
#      only to RR/OR computation, not to Fisher's test itself; R mirrors this
#    - Sort stability for comparison metrics: dplyr::arrange() verified stable;
#      ties in sort metric resolved by original term ordering
#    - PCFILES/JET engine Excel writes -> openxlsx equivalent verified for
#      formatting fidelity; cell-level styles match SAS XML output gallery
# ============================================================
