# ==============================================================================
#         PROGRAM NAME: ae_oncology_v1.R — AE Toxicity (Oncology) Panel Driver
#
#          DESCRIPTION: Adverse Events Oncology/Toxicity Analysis Panel.
#                       Summarises adverse events per arm by toxicity grade.
#                       If more than one arm, compares experimental arms
#                       against a control arm with p-values, risk differences,
#                       relative risks, and odds ratios.
#                       Creates a multi-sheet Excel (.xlsx) workbook.
#
#      EVALUATION TYPE: Safety
#
#      ORIGINAL SOURCE: contributed/AE/AE_Toxicity/ae_oncology.sas (389 lines)
#      ORIGINAL AUTHOR: David Kretch (david.kretch@us.ibm.com)
#                DATE:  May 2011
#
#  EXTERNAL R FILES USED:
#    AE_Toxicity/ae_oncology_aggregate.R -- onc_aggregate(), onc_compare(),
#                                            fmt_output()
#    AE_Toxicity/ae_oncology_output.R    -- out_onc()
#    ZZ_Utilities/ae_setup.R             -- setup()
#    ZZ_Utilities/data_checks.R          -- chk_var(), chk_cmp()
#    ZZ_Utilities/err_output.R           -- error_summary()
#    ZZ_Utilities/sl_gs_output.R         -- group_subset_pp()
#    ZZ_Utilities/xml_output.R           -- create_styles()
#
#  VARIABLES REQUIRED:
#    AE -- AEDECOD, AETOXGR, USUBJID
#    DM -- ACTARM or ARM, USUBJID
#    EX -- USUBJID
#
#  VARIABLES USED WHEN AVAILABLE:
#    AE -- AEBODSYS, AESTDTC
#    DM -- RFSTDTC, RFENDTC, ARMCD
#    EX -- EXSTDTC, EXENDTC
#
#            MADE WITH: R >= 4.3.0
# ==============================================================================

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
# Replaces SAS %include statements (SAS lines 220-226).
local({
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."

  tox_deps <- list(
    list(file = "ae_oncology_aggregate.R", fn = "onc_aggregate"),
    list(file = "ae_oncology_output.R",    fn = "out_onc")
  )
  util_deps <- list(
    list(file = "ae_setup.R",     fn = "setup"),
    list(file = "data_checks.R",  fn = "chk_var"),
    list(file = "err_output.R",   fn = "error_summary"),
    list(file = "sl_gs_output.R", fn = "group_subset_pp"),
    list(file = "xml_output.R",   fn = "create_styles")
  )

  tox_dirs <- c(
    script_dir,
    "contributed/R/AE/AE_Toxicity",
    file.path(".", "contributed", "R", "AE", "AE_Toxicity")
  )
  util_dirs <- c(
    file.path(script_dir, "..", "ZZ_Utilities"),
    "contributed/R/AE/ZZ_Utilities",
    file.path(".", "contributed", "R", "AE", "ZZ_Utilities")
  )

  source_dep <- function(dep, search_dirs) {
    if (!exists(dep$fn, mode = "function", inherits = TRUE)) {
      for (d in search_dirs) {
        dep_path <- file.path(d, dep$file)
        if (file.exists(dep_path)) {
          source(dep_path, local = FALSE)
          break
        }
      }
    }
  }

  for (dep in util_deps) source_dep(dep, util_dirs)
  for (dep in tox_deps)  source_dep(dep, tox_dirs)
})


# ==============================================================================
# load_data_file — Load data from XPT, SAS7BDAT, or pass through data frames
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
  names(df) <- tolower(names(df))
  df
}


# ==============================================================================
# ae_oncology_panel — Main AE Oncology Panel Orchestrator
# ==============================================================================
# Replaces SAS %params (lines 83-213), %onc macro (lines 228-381), and
# %onc call (line 389) from contributed/AE/AE_Toxicity/ae_oncology.sas.
#
# Workflow:
#   1. Load AE/DM/EX data
#   2. Normalize continuity correction and grade range parameters
#   3. Run ae_setup validation via setup() with optional MedDRA
#   4. Determine experimental/control arms
#   5. Analysis 1 — Toxicity grade summary via onc_aggregate()
#      a. By AEDECOD (preferred term)
#      b. By AEBODSYS (system organ class) if AEBODSYS present
#   6. Analysis 2 — Comparison via onc_compare() if arm_count > 1
#      a. Overall grade comparison
#      b. Per-grade comparison for each toxicity grade
#   7. Format output via fmt_output()
#   8. Generate Excel output via out_onc()
#   9. Error summary if setup fails
#
# @param ae              Data frame or path to AE domain.
# @param dm              Data frame or path to DM domain.
# @param ex              Data frame or path to EX domain.
# @param meddra_data     Data frame of MedDRA hierarchy (optional).
# @param dme_data        Data frame of DME list (optional).
# @param meddra_path     Character path to MedDRA hierarchy files directory.
# @param output_path     Character path to output directory.
# @param panel_title     Character panel title (default "Oncology AE").
# @param panel_desc      Character panel description.
# @param ndabla          Character NDA/BLA number.
# @param studyid         Character study identifier.
# @param study_lag       Integer window in days after last exposure.
# @param cc              Continuity correction: numeric, "arm", "none", or "0".
# @param ae_rate_ci_sw   Integer 0/1 — show AE rate confidence intervals.
# @param exp             Integer — experimental arm number (default 1).
# @param ctl             Integer — control arm number (default 2).
# @param toxgr_min       Integer — minimum toxicity grade (default 1).
# @param toxgr_max       Integer — maximum toxicity grade (default 5).
# @param toxgr_grp5_sw   Integer 0/1 — include grade 5 in grouped grades (3-5).
# @param sortvar         Character — sort variable for output.
# @param sortgrp_sw      Integer 0/1 — sort within groups.
# @param vld_sw          Integer or logical — perform data validation.
# @param sl_datasets     Data frame for Script Launcher datasets metadata.
# @param sl_group        Data frame for Script Launcher grouping metadata.
# @param sl_subset       Data frame for Script Launcher subsetting metadata.
# @param verbose         Logical — emit progress messages.
# @return Invisible named list with output file path and analysis results.
# ==============================================================================
ae_oncology_panel <- function(
    ae,
    dm,
    ex,
    meddra_data    = NULL,
    dme_data       = NULL,
    meddra_path    = NULL,
    output_path    = ".",
    panel_title    = "Oncology AE",
    panel_desc     = "",
    ndabla         = NA_character_,
    studyid        = NA_character_,
    study_lag      = 120L,
    cc             = 0,
    ae_rate_ci_sw  = 1L,
    exp            = 1L,
    ctl            = 2L,
    toxgr_min      = 1L,
    toxgr_max      = 5L,
    toxgr_grp5_sw  = 1L,
    sortvar        = NA_character_,
    sortgrp_sw     = 0L,
    vld_sw         = TRUE,
    sl_datasets    = NULL,
    sl_group       = NULL,
    sl_subset      = NULL,
    verbose        = TRUE
) {
  start_time <- proc.time()

  # ---------------------------------------------------------------------------
  # 1. Load input datasets (SAS lines 118-121)
  # ---------------------------------------------------------------------------
  ae_df <- load_data_file(ae, "AE")
  dm_df <- load_data_file(dm, "DM")
  ex_df <- load_data_file(ex, "EX")

  # ---------------------------------------------------------------------------
  # 2. Normalize continuity correction (SAS lines 228-249)
  # ---------------------------------------------------------------------------
  cc_sw    <- 0L
  cc_whole <- 1L
  cc_val   <- 0

  if (is.character(cc)) {
    cc_clean <- tolower(trimws(cc))
    if (cc_clean == "arm") {
      cc_sw <- 2L; cc_whole <- 0L
    } else if (cc_clean %in% c("none", "0")) {
      cc_sw <- 0L
    } else {
      cc_val <- suppressWarnings(as.numeric(cc_clean))
      if (!is.na(cc_val) && cc_val != 0) {
        cc_sw <- 1L
        cc_whole <- as.integer(cc_val == floor(cc_val))
      }
    }
  } else if (is.numeric(cc)) {
    if (cc != 0) {
      cc_sw <- 1L
      cc_val <- cc
      cc_whole <- as.integer(cc == floor(cc))
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Set up output paths (SAS lines 104-111)
  # ---------------------------------------------------------------------------
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }
  onc_out <- file.path(output_path, "Oncology AE Analysis Panel.xlsx")
  errout  <- file.path(output_path, "Oncology AE Error Summary.xlsx")

  # ---------------------------------------------------------------------------
  # 4. Determine MedDRA usage (SAS lines 133-140)
  # ---------------------------------------------------------------------------
  use_meddra <- !is.null(meddra_data)
  if (!use_meddra && !is.null(meddra_path) && nzchar(meddra_path)) {
    meddra_files <- list.files(meddra_path,
                               pattern = "mdhier.*\\.(xpt|sas7bdat)$",
                               full.names = TRUE, ignore.case = TRUE)
    if (length(meddra_files) > 0L) {
      mf <- meddra_files[1L]
      if (verbose) cli::cli_inform(c("i" = "Loading MedDRA hierarchy: {mf}"))
      meddra_data <- if (grepl("\\.xpt$", mf, ignore.case = TRUE)) {
        haven::read_xpt(mf)
      } else {
        haven::read_sas(mf)
      }
      names(meddra_data) <- tolower(names(meddra_data))
      use_meddra <- TRUE
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Build configuration object
  # ---------------------------------------------------------------------------
  config <- list(
    panel_title   = panel_title,
    panel_desc    = panel_desc,
    ndabla        = ndabla,
    studyid       = studyid,
    study_lag     = as.integer(study_lag),
    cc            = cc,
    cc_sw         = cc_sw,
    cc_whole      = cc_whole,
    cc_val        = cc_val,
    ae_rate_ci_sw = as.integer(ae_rate_ci_sw),
    exp           = as.integer(exp),
    ctl           = as.integer(ctl),
    toxgr_min     = as.integer(toxgr_min),
    toxgr_max     = as.integer(toxgr_max),
    toxgr_grp5_sw = as.integer(toxgr_grp5_sw),
    sortvar       = sortvar,
    sortgrp_sw    = as.integer(sortgrp_sw),
    vld_sw        = as.integer(vld_sw),
    onc_out       = onc_out,
    errout        = errout,
    output_path   = output_path,
    sl_datasets   = sl_datasets,
    sl_group      = sl_group,
    sl_subset      = sl_subset
  )

  # ---------------------------------------------------------------------------
  # 6. Run AE setup (SAS line 253: %setup)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_h2("Running AE Oncology setup validation")

  setup_result <- tryCatch(
    setup(
      dm          = dm_df,
      ae          = ae_df,
      ex          = ex_df,
      mdhier      = if (use_meddra) "Y" else "N",
      meddra_data = meddra_data,
      dme_data    = dme_data,
      study_lag   = study_lag,
      vld_sw      = config$vld_sw,
      toxgr_min   = toxgr_min,
      toxgr_max   = toxgr_max,
      sl_subset   = sl_subset
    ),
    error = function(e) {
      cli::cli_warn("Setup failed: {conditionMessage(e)}")
      list(setup_success = FALSE, dm_subj_gt0 = FALSE,
           setup_req_var = FALSE)
    }
  )

  # Enrich config with setup results
  config$arm_count <- setup_result$arm_count %||% 0L
  config$arm_N     <- setup_result$arm_N     %||% integer(0)
  config$arm_names <- setup_result$arm_names %||% character(0)
  config$ae_aebodsys <- setup_result$ae_aebodsys %||% FALSE
  config$rpt_chk_var_req <- setup_result$rpt_chk_var_req %||% NULL

  # Fix ctl if it exceeds arm count (SAS line 269)
  if (config$arm_count > 0L && config$ctl > config$arm_count) {
    config$ctl <- config$arm_count
  }

  # Results collector
  results <- list()

  # ---------------------------------------------------------------------------
  # 7. Run analyses if setup succeeded (SAS lines 256-375)
  # ---------------------------------------------------------------------------
  setup_ok <- isTRUE(setup_result$setup_success)

  if (setup_ok) {
    ds_base <- setup_result$ds_base

    # Build arm_subjcnt named vector
    arm_subjcnt <- config$arm_N
    names(arm_subjcnt) <- seq_along(arm_subjcnt)

    # --- Analysis 1: Toxicity grade summary per preferred term (SAS lines 271-280)
    if (verbose) cli::cli_h3("Analysis 1 — Aggregate by Preferred Term")
    results$pt_1 <- tryCatch(
      onc_aggregate(
        dsin        = ds_base,
        dsout_name  = "pt_1",
        by_vars     = "aedecod",
        arm_count   = config$arm_count,
        arm_subjcnt = arm_subjcnt,
        arm_names   = config$arm_names,
        toxgr_min   = config$toxgr_min,
        toxgr_max   = config$toxgr_max,
        toxgr_grp5_sw = config$toxgr_grp5_sw,
        meddra      = if (use_meddra) "Y" else "N"
      ),
      error = function(e) {
        cli::cli_warn("Analysis 1 (PT aggregate) failed: {conditionMessage(e)}")
        NULL
      }
    )

    # Analysis 1b: Aggregate by SOC if AEBODSYS is available (SAS lines 282-290)
    if (isTRUE(config$ae_aebodsys)) {
      if (verbose) cli::cli_h3("Analysis 1b — Aggregate by SOC")
      results$pt_1_soc <- tryCatch(
        onc_aggregate(
          dsin        = ds_base,
          dsout_name  = "pt_1_soc",
          by_vars     = c("aebodsys", "aedecod"),
          arm_count   = config$arm_count,
          arm_subjcnt = arm_subjcnt,
          arm_names   = config$arm_names,
          toxgr_min   = config$toxgr_min,
          toxgr_max   = config$toxgr_max,
          toxgr_grp5_sw = config$toxgr_grp5_sw,
          meddra      = if (use_meddra) "Y" else "N"
        ),
        error = function(e) {
          cli::cli_warn("Analysis 1b (SOC aggregate) failed: {conditionMessage(e)}")
          NULL
        }
      )
    }

    # --- Analysis 2: Build report key for formatted output --------------------
    rpt_key <- tibble::tibble(
      ds      = c("pt_1", "pt_2", "pt_3"),
      key     = c("aedecod", "aedecod", "aedecod"),
      keyvar_cnt = c(1L, 1L, 1L)
    )
    rpt_missing <- NULL

    # --- Analysis 2: Comparison (SAS lines 295-360) --------------------------
    if (config$arm_count > 1L) {
      if (verbose) cli::cli_h3("Analysis 2 — Two-arm comparison (all grades)")
      # Compare "all" grades combined (SAS line 305)
      results$pt_2 <- tryCatch(
        onc_compare(
          dsin        = ds_base,
          dsout_name  = "pt_2_all",
          by_vars     = "aedecod",
          exp         = config$exp,
          ctl         = config$ctl,
          arm_count   = config$arm_count,
          arm_subjcnt = arm_subjcnt,
          arm_names   = config$arm_names,
          cmpgr       = "all",
          cc          = config$cc_val,
          cc_sw       = config$cc_sw,
          cc_whole    = config$cc_whole,
          ae_rate_ci_sw = config$ae_rate_ci_sw,
          toxgr_grp5_sw = config$toxgr_grp5_sw,
          meddra      = if (use_meddra) "Y" else "N",
          all_arm     = results$pt_1
        ),
        error = function(e) {
          cli::cli_warn("Comparison (all) failed: {conditionMessage(e)}")
          NULL
        }
      )

      # Compare each individual grade (SAS lines 311-317)
      results$pt_2_grades <- list()
      for (gr in seq(config$toxgr_min, config$toxgr_max)) {
        gr_str <- as.character(gr)
        if (verbose) cli::cli_h3("Analysis 2 — Comparison grade {gr}")
        results$pt_2_grades[[gr_str]] <- tryCatch(
          onc_compare(
            dsin        = ds_base,
            dsout_name  = paste0("pt_2_", gr_str),
            by_vars     = "aedecod",
            exp         = config$exp,
            ctl         = config$ctl,
            arm_count   = config$arm_count,
            arm_subjcnt = arm_subjcnt,
            arm_names   = config$arm_names,
            cmpgr       = gr_str,
            cc          = config$cc_val,
            cc_sw       = config$cc_sw,
            cc_whole    = config$cc_whole,
            ae_rate_ci_sw = config$ae_rate_ci_sw,
            toxgr_grp5_sw = config$toxgr_grp5_sw,
            meddra      = if (use_meddra) "Y" else "N",
            all_arm     = results$pt_1
          ),
          error = function(e) {
            cli::cli_warn("Comparison grade {gr} failed: {conditionMessage(e)}")
            NULL
          }
        )
      }

      # Compare grouped grades (3-4-5 or 3-4) (SAS lines 320-330)
      grp_str <- if (config$toxgr_grp5_sw == 1L) "345" else "34"
      if (verbose) cli::cli_h3("Analysis 2 — Comparison grouped grades {grp_str}")
      results$pt_2_grp <- tryCatch(
        onc_compare(
          dsin        = ds_base,
          dsout_name  = paste0("pt_2_", grp_str),
          by_vars     = "aedecod",
          exp         = config$exp,
          ctl         = config$ctl,
          arm_count   = config$arm_count,
          arm_subjcnt = arm_subjcnt,
          arm_names   = config$arm_names,
          cmpgr       = grp_str,
          cc          = config$cc_val,
          cc_sw       = config$cc_sw,
          cc_whole    = config$cc_whole,
          ae_rate_ci_sw = config$ae_rate_ci_sw,
          toxgr_grp5_sw = config$toxgr_grp5_sw,
          meddra      = if (use_meddra) "Y" else "N",
          all_arm     = results$pt_1
        ),
        error = function(e) {
          cli::cli_warn("Comparison grouped grades failed: {conditionMessage(e)}")
          NULL
        }
      )

      # Compare missing grade (SAS lines 333-343)
      if (verbose) cli::cli_h3("Analysis 2 — Comparison missing grade")
      results$pt_2_missing <- tryCatch(
        onc_compare(
          dsin        = ds_base,
          dsout_name  = "pt_2_missing",
          by_vars     = "aedecod",
          exp         = config$exp,
          ctl         = config$ctl,
          arm_count   = config$arm_count,
          arm_subjcnt = arm_subjcnt,
          arm_names   = config$arm_names,
          cmpgr       = "missing",
          cc          = config$cc_val,
          cc_sw       = config$cc_sw,
          cc_whole    = config$cc_whole,
          ae_rate_ci_sw = config$ae_rate_ci_sw,
          toxgr_grp5_sw = config$toxgr_grp5_sw,
          meddra      = if (use_meddra) "Y" else "N",
          all_arm     = results$pt_1
        ),
        error = function(e) {
          cli::cli_warn("Comparison missing grades failed: {conditionMessage(e)}")
          NULL
        }
      )

      # --- Format output for comparison worksheets (SAS lines 345-363) --------
      results$pt_2_output_fmt <- list()
      results$pt_3_output_fmt <- list()

      # Combine all pt_2 comparison results into a single list for formatting
      all_comparisons <- c(
        list(all = results$pt_2),
        results$pt_2_grades,
        list(grp = results$pt_2_grp, missing = results$pt_2_missing)
      )

      for (nm in names(all_comparisons)) {
        cmp <- all_comparisons[[nm]]
        if (!is.null(cmp) && is.data.frame(cmp$output %||% cmp)) {
          ds_data <- if (is.list(cmp) && !is.data.frame(cmp)) cmp$output else cmp
          formatted <- tryCatch(
            fmt_output(
              ds       = ds_data,
              ds_name  = paste0("pt_2_", nm),
              rpt_key  = rpt_key,
              cc_sw    = config$cc_sw,
              cc_ind_ds = if (is.list(cmp)) cmp$cc_ind else NULL,
              sort_sw   = !is.na(config$sortvar) && nzchar(config$sortvar),
              sortvar   = config$sortvar,
              sortgrp_sw = config$sortgrp_sw == 1L,
              sortdir   = "desc"
            ),
            error = function(e) {
              cli::cli_warn("Format output for {nm} failed: {conditionMessage(e)}")
              NULL
            }
          )
          results$pt_2_output_fmt[[nm]] <- formatted
        }
      }
    }

    # --- Generate Excel output (SAS line 370: %out_onc) -----------------------
    if (verbose) cli::cli_h3("Generating Oncology AE Excel output")
    tryCatch({
      # Assemble output arguments
      pt_1_out <- if (!is.null(results$pt_1)) {
        if (is.list(results$pt_1) && !is.data.frame(results$pt_1)) {
          results$pt_1$output %||% results$pt_1
        } else {
          results$pt_1
        }
      } else {
        tibble::tibble()
      }

      pt_2_out <- if (!is.null(results$pt_2)) {
        if (is.list(results$pt_2) && !is.data.frame(results$pt_2)) {
          results$pt_2$output %||% results$pt_2
        } else {
          results$pt_2
        }
      } else {
        tibble::tibble()
      }

      pt_2_out_fmt <- if (!is.null(results$pt_2_output_fmt$all)) {
        results$pt_2_output_fmt$all$fmt %||% pt_2_out
      } else {
        pt_2_out
      }

      # DM validation data from setup
      dm_val <- setup_result$dm_validation_data %||% tibble::tibble()

      out_onc(
        params_list      = config,
        pt_1_output      = pt_1_out,
        pt_2_output      = pt_2_out,
        pt_2_output_fmt  = pt_2_out_fmt,
        rpt_key          = rpt_key,
        rpt_missing      = rpt_missing,
        dm_validation_data = dm_val,
        ae_matching_data = setup_result$ae_matching_data
      )
    }, error = function(e) {
      cli::cli_warn("Oncology output generation failed: {conditionMessage(e)}")
    })

    if (verbose) cli::cli_alert_success("AE Oncology analysis complete.")

  } else {
    # --- Setup failed: generate error summary (SAS lines 376-383) -------------
    if (verbose) cli::cli_alert_warning("Setup validation failed.")
    if (exists("error_summary", mode = "function")) {
      tryCatch(
        error_summary(
          err_file    = errout,
          panel_title = panel_title,
          ndabla      = ndabla,
          studyid     = studyid,
          err_nosubj  = !isTRUE(setup_result$dm_subj_gt0),
          err_missvar = !isTRUE(setup_result$setup_req_var),
          rpt_chk_var_req = setup_result$rpt_chk_var_req
        ),
        error = function(e) {
          cli::cli_warn("Error summary output failed: {conditionMessage(e)}")
        }
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 8. Timing
  # ---------------------------------------------------------------------------
  elapsed <- (proc.time() - start_time)["elapsed"]
  if (verbose) {
    cli::cli_inform(c("i" = paste0(
      "Running time: ",
      sprintf("%02d:%05.2f", as.integer(elapsed) %/% 60, elapsed %% 60)
    )))
  }

  invisible(list(
    output_file  = onc_out,
    err_file     = errout,
    config       = config,
    setup_result = setup_result,
    results      = results
  ))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#   1. setup() from ae_setup.R supports the mdhier, toxgr_min,
#      toxgr_max parameters for oncology-specific MedDRA and
#      grade range handling.
#   2. onc_aggregate() returns a named list with $output (tibble)
#      and additional metadata, or a tibble directly.
#   3. onc_compare() returns a named list with $output, $cc_ind,
#      or a tibble directly.
#   4. fmt_output() returns a named list with $fmt (formatted
#      tibble), $fmt_ind (indicator tibble), $fmt_cc_ind.
#   5. out_onc() writes the final Excel workbook to config$onc_out.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#   1. SAS PROC FREQ p-values for chi-square and Fisher's exact
#      may differ by ~1e-5 vs R's chisq.test()/fisher.test().
#   2. Confidence interval computation for AE rate CI: SAS uses
#      exact binomial (Clopper-Pearson) via PROC FREQ; R uses
#      binom.test() which is also Clopper-Pearson. Results should
#      match within floating-point precision.
#   3. Sort order stability: Both SAS PROC SORT and R dplyr::arrange()
#      use stable sorts, so tie-breaking should be identical when
#      sort keys match.
#
# NO DIRECT R EQUIVALENT:
#   1. SAS DATA step with HASH object lookups for SOC matching
#      → replaced by dplyr::left_join().
#   2. SAS ODS TRACE / ODS SELECT for output routing → not needed;
#      openxlsx writes directly to workbook.
#   3. SAS PROC DATASETS for intermediate cleanup → not needed;
#      R garbage collects.
#
# PACKAGE SELECTION RATIONALE:
#   haven   — SAS data I/O (read_xpt, read_sas)
#   dplyr   — Data manipulation replacing SAS DATA steps
#   tidyr   — Pivoting and reshaping
#   purrr   — Functional iteration
#   forcats — Factor ordering for toxicity grades
#   janitor — SAS-compatible rounding via round_half_up()
#   openxlsx — Excel output replacing SpreadsheetML XML
#   cli     — User-facing informative messages
#
# OPEN QUESTIONS:
#   1. When MedDRA hierarchy data is available, the SAS code joins
#      on AEBODSYS to the SOC level. Confirm that the meddra_data
#      provided always contains the soc_name column.
#   2. The exp/ctl arm numbering defaults to 1/2 but the SAS code
#      also supports determining arms from ARMCD sorting. Confirm
#      the default behavior is acceptable.
# ============================================================
