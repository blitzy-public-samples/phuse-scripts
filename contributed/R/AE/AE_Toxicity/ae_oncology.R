###############################################################################
#         PROGRAM NAME: AE Toxicity Panel                                     #
#                                                                             #
#          DESCRIPTION: Find subject counts per arm for each adverse event    #
#                       broken down by toxicity grade.                         #
#                       Compare the AEs between two arms and find             #
#                       risk difference, relative risk, odds ratio,           #
#                       95% confidence intervals, and p-value.                #
#                                                                             #
#      EVALUATION TYPE: Safety                                                #
#                                                                             #
#               AUTHOR: David Kretch (david.kretch@us.ibm.com)               #
#                                                                             #
#                 DATE: February 7, 2011                                      #
#                                                                             #
#      MIGRATED FROM:  contributed/AE/AE_Toxicity/ae_oncology.sas (390 lines)#
#                                                                             #
#  EXTERNAL FILES USED: ae_setup.R -- Merges AE, DM, and EX                  #
#                       ae_oncology_aggregate.R -- Does the analysis          #
#                       ae_oncology_output.R -- Creates the output            #
#                       xml_output.R -- openxlsx formatting functions         #
#                       data_checks.R -- Generic variable checks              #
#                       sl_gs_output.R -- Script Launcher settings output     #
#                       err_output.R -- Error output when missing vars        #
#                       mdhier_x_y.sas7bdat -- MedDRA hierarchy ver. X.Y     #
#                                                                             #
#  PARAMETERS REQUIRED: meddrapath -- location of MedDRA hierarchy datasets   #
#                       saspath -- location of the external R programs        #
#                       utilpath -- location of the external R programs       #
#                                                                             #
#                       study_lag -- window in days after last exposure       #
#                                    where AEs should be kept in analysis     #
#                                                                             #
#                       toxgr_min -- minimum valid toxicity grade             #
#                       toxgr_max -- maximum valid toxicity grade             #
#                       toxgr_grp5_sw -- group toxgrade 5 with 3&4           #
#                                                                             #
#                       ver -- MedDRA version                                 #
#                       exp -- comparison treatment arm number                #
#                       ctl -- comparison control arm number                  #
#                       cmptrm -- MedDRA levels to compare                    #
#                       cmpgr -- toxicity grades to compare                   #
#                       cmpsort -- variable to sort formatted output by       #
#                       cc -- continuity correction value                     #
#                                                                             #
#           LOCAL ONLY: studypath -- location of the drug study datasets      #
#                       outpath -- location of the output                     #
#                                                                             #
#   VARIABLES REQUIRED: AE -- AEBODSYS                                        #
#                             AEDECOD                                         #
#                             USUBJID                                         #
#                       DM -- ACTARM or ARM                                   #
#                             USUBJID                                         #
#                       EX -- USUBJID                                         #
#                                                                             #
#       VARIABLES USED: AE -- AESTDTC                                         #
#       WHEN AVAILABLE        AETOXGR                                         #
#                       DM -- RFSTDTC                                         #
#                             RFENDTC                                         #
#                             ARMCD                                           #
#                       EX -- EXSTDTC                                         #
#                             EXENDTC                                         #
#                                                                             #
#            MADE WITH: R >= 4.3.0                                            #
#                                                                             #
#                NOTES: Idiomatic R migration using tidyverse ecosystem.       #
#                       SAS global macro scope replaced by named-list returns. #
#                                                                             #
###############################################################################

# REVISIONS
# 2011-03-26  DK  Script Launcher parameter mapping
#                 handling for MedDRA version 'N/A'
# 2011-05-08  DK  Added handling for errors in case DM has no subjects
# 2011-06-02  DK  Merged the counts from part 3 onto the cartesian product
#                 of arm number and AE term number to avoid issues from arms
#                 with no AEs
#                 Added check for MedDRA matching percentage < 80
# 2026-03-26  Blitzy  Migrated from SAS to R

# =============================================================================
# Library Loading
# =============================================================================
library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(stringr)
library(janitor)
library(cli)
library(openxlsx)


# =============================================================================
# params — Parameter Setup Function
# =============================================================================
# Migrated from SAS %params macro (SAS lines 88-268).
# Handles both LOCAL mode (standalone execution with user-specified paths)
# and Script Launcher mode (panel option values mapped from SL interface).
#
# SAS global macro variables are replaced by a returned named list.
# SAS %let / call symputx → R named list elements.
# SAS libname / DATA step → haven::read_xpt() / haven::read_sas().
# SAS %symdel / proc datasets kill → not needed (R function scoping).
#
# @param run_location  Character: "LOCAL" for standalone, anything else for
#                      Script Launcher mode.
# @param panel_title   Character: panel display title.
# @param panel_desc    Character: optional panel description.
# @param saspath       Character: path to the AE_Toxicity R programs directory.
# @param utilpath      Character: path to the ZZ_Utilities R programs directory.
# @param outpath       Character: path to the output directory.
# @param studypath     Character: path to the directory containing XPT/SAS
#                      datasets (LOCAL mode only).
# @param ndabla        Character: NDA/BLA identifier.
# @param studyid       Character: study identifier.
# @param meddrapath    Character: path to MedDRA hierarchy datasets.
# @param ver           Character: MedDRA version string (or "N/A" to skip).
# @param study_lag     Numeric: window in days after last exposure.
# @param toxgr_grp5_sw Integer: 1 = group grades 3/4/5; 0 = group 3/4 only.
# @param exp           Integer: treatment arm number for comparison.
# @param ctl           Integer: control arm number for comparison.
# @param cmptrm        Character: comma-separated MedDRA levels for comparison
#                      (e.g., "soc_name,pt_name").
# @param cmpgr         Character: toxicity grades for comparison
#                      (valid: "all", "5", "34", "345").
# @param cmpsort       Character: sort variable for formatted output
#                      (valid: "rd", "rr", "ort", "p_value").
# @param cc            Character: continuity correction value
#                      ("0.5", "1", "0", "arm").
# @param meddraver     Character: MedDRA version from Script Launcher.
# @param tox_grade     Character: toxicity grade grouping from SL.
# @param meddracomp    Character: MedDRA comparison terms from SL (e.g.,
#                      "soc/pt").
# @param compmet       Character: comparison metric from SL.
# @param sortby        Character: sort-by variable description from SL.
# @param cont_corr     Character: continuity correction description from SL.
# @param treat_arm     Character: treatment arm name from SL.
# @param cont_arm      Character: control arm name from SL.
#
# @return Named list containing all resolved parameter values, datasets
#         (in LOCAL mode), and Script Launcher metadata tibbles.
# =============================================================================
params <- function(run_location  = "LOCAL",
                   panel_title   = "AE Toxicity",
                   panel_desc    = "",
                   saspath       = NULL,
                   utilpath      = NULL,
                   outpath       = NULL,
                   studypath     = NULL,
                   ndabla        = "",
                   studyid       = "",
                   meddrapath    = NULL,
                   ver           = "",
                   study_lag     = 30,
                   toxgr_grp5_sw = 1L,
                   exp           = 1L,
                   ctl           = 2L,
                   cmptrm        = "soc_name,pt_name",
                   cmpgr         = "all",
                   cmpsort       = "rr",
                   cc            = "0.5",
                   meddraver     = NULL,
                   tox_grade     = NULL,
                   meddracomp    = NULL,
                   compmet       = NULL,
                   sortby        = NULL,
                   cont_corr     = NULL,
                   treat_arm     = NULL,
                   cont_arm      = NULL) {

  # Detect run location (SAS lines 85-86: %symexist + %let run_location)
  run_location <- toupper(as.character(run_location))
  cli::cli_alert_info("RUN LOCATION: {run_location}")

  # Initialize results container
  result <- list()

  # ---------------------------------------------------------------------------
  # LOCAL mode (SAS lines 91-193)
  # ---------------------------------------------------------------------------
  if (run_location == "LOCAL") {

    # Panel identity (SAS lines 97-99)
    result$panel_title <- panel_title
    result$panel_desc  <- panel_desc

    # Paths (SAS lines 101-108)
    result$saspath  <- saspath
    result$utilpath <- utilpath
    result$outpath  <- outpath

    # Construct output file paths (SAS lines 107-108)
    # .xlsx extension for openxlsx (replacing .xls for SAS SpreadsheetML)
    result$oncaeout <- file.path(outpath, "AE Toxicity Analysis.xlsx")
    result$errout   <- file.path(outpath, "AE Toxicity Error Summary.xlsx")

    # Load datasets via haven (SAS lines 112-117)
    result$ae <- haven::read_xpt(file.path(studypath, "ae.xpt"))
    result$dm <- haven::read_xpt(file.path(studypath, "dm.xpt"))
    result$ex <- haven::read_xpt(file.path(studypath, "ex.xpt"))

    # NDA/BLA and study number (SAS lines 120-122)
    result$ndabla  <- ndabla
    result$studyid <- studyid

    # MedDRA hierarchy path (SAS lines 124-127)
    result$meddrapath <- meddrapath

    # MedDRA version handling (SAS lines 130-136)
    result$ver <- ver
    if (nzchar(ver) && toupper(stringr::str_sub(ver, 1, 1)) == "N") {
      result$meddra     <- "N"
      result$meddra_pct <- 0
    } else {
      result$meddra     <- "Y"
      result$meddra_pct <- NA_real_
    }

    # Study lag (SAS lines 138-141)
    result$study_lag <- as.numeric(study_lag)

    # Toxicity grade parameters (SAS lines 143-149)
    result$toxgr_grp5_sw <- as.integer(toxgr_grp5_sw)

    # Comparison parameters (SAS lines 151-178)
    result$exp     <- as.integer(exp)
    result$ctl     <- as.integer(ctl)
    result$cmptrm  <- cmptrm
    result$cmpgr   <- cmpgr
    result$cmpsort <- cmpsort
    result$cc      <- as.character(cc)

    # Dummy Script Launcher metadata tibbles (SAS lines 181-192)
    result$sl_datasets <- dplyr::tibble(
      datatype           = character(0),
      name               = character(0),
      partition_variable = character(0),
      default            = character(0)
    )
    result$sl_group <- dplyr::tibble(
      group_name    = character(0),
      domain        = character(0),
      partition     = character(0),
      var_name      = character(0),
      var_value     = character(0),
      dsvg_grp_name = character(0)
    )
    result$sl_subset <- dplyr::tibble(
      name           = character(0),
      domain         = character(0),
      partition      = character(0),
      var_name       = character(0),
      var_value      = character(0),
      inner_operator = character(0),
      outer_operator = character(0)
    )

    # Script Launcher arm parameters (not used in LOCAL, but set for consistency)
    result$treat_arm <- treat_arm
    result$cont_arm  <- cont_arm

  # ---------------------------------------------------------------------------
  # Script Launcher mode (SAS lines 197-266)
  # ---------------------------------------------------------------------------
  } else {

    # Panel identity (passed by Script Launcher framework)
    result$panel_title <- panel_title
    result$panel_desc  <- panel_desc
    result$saspath     <- saspath
    result$utilpath    <- utilpath
    result$outpath     <- outpath
    result$meddrapath  <- meddrapath
    result$ndabla      <- ndabla
    result$studyid     <- studyid

    # Output file paths (constructed by SL framework or passed in)
    result$oncaeout <- file.path(outpath, "AE Toxicity Analysis.xlsx")
    result$errout   <- file.path(outpath, "AE Toxicity Error Summary.xlsx")

    # MedDRA version (SAS lines 205-210)
    sl_meddraver <- if (!is.null(meddraver)) stringr::str_trim(meddraver) else ""
    result$ver <- sl_meddraver
    if (nzchar(sl_meddraver) &&
        toupper(stringr::str_sub(sl_meddraver, 1, 1)) == "N") {
      result$meddra     <- "N"
      result$meddra_pct <- 0
    } else {
      result$meddra     <- "Y"
      result$meddra_pct <- NA_real_
    }

    # Study lag (SAS lines 212-215)
    sl_study_lag <- if (!is.null(study_lag)) {
      stringr::str_trim(as.character(study_lag))
    } else {
      "30"
    }
    result$study_lag <- as.numeric(sl_study_lag)

    # Toxicity grade grouping (SAS lines 217-223)
    sl_tox_grade <- if (!is.null(tox_grade)) {
      stringr::str_trim(tox_grade)
    } else {
      ""
    }
    result$toxgr_grp5_sw <- dplyr::case_when(
      sl_tox_grade == "All Grades, Grades 3 and 4, Grade 5" ~ 0L,
      sl_tox_grade == "All Grades, Grades 3 and above"      ~ 1L,
      TRUE                                                   ~ 0L
    )

    # Comparison terms (SAS lines 225-230)
    sl_meddracomp <- if (!is.null(meddracomp)) {
      tolower(stringr::str_trim(meddracomp))
    } else {
      "soc/pt"
    }
    cmptrm_parts <- strsplit(sl_meddracomp, "/")[[1]]
    cmptrm_1 <- paste0(stringr::str_trim(cmptrm_parts[1]), "_name")
    cmptrm_2 <- if (length(cmptrm_parts) >= 2) {
      paste0(stringr::str_trim(cmptrm_parts[2]), "_name")
    } else {
      "pt_name"
    }
    result$cmptrm <- paste0(cmptrm_1, ",", cmptrm_2)

    # Toxicity grades for two-term comparison (SAS lines 232-241)
    sl_compmet <- if (!is.null(compmet)) {
      stringr::str_trim(compmet)
    } else {
      "All Grades"
    }
    result$cmpgr <- dplyr::case_when(
      sl_compmet == "All Grades"          ~ "all",
      sl_compmet == "Grades 3 and 4"      ~ "34",
      sl_compmet == "Grades 3, 4, and 5"  ~ "345",
      sl_compmet == "Grade 5"             ~ "5",
      TRUE                                ~ "all"
    )

    # Sort-by variable (SAS lines 243-252)
    sl_sortby <- if (!is.null(sortby)) {
      stringr::str_trim(sortby)
    } else {
      "Risk Difference"
    }
    result$cmpsort <- dplyr::case_when(
      sl_sortby == "Risk Difference" ~ "rd",
      sl_sortby == "Relative Risk"   ~ "rr",
      sl_sortby == "Odds Ratio"      ~ "ort",
      sl_sortby == "P-Value"         ~ "p_value",
      TRUE                           ~ "rd"
    )

    # Continuity correction (SAS lines 254-263)
    sl_cont_corr <- if (!is.null(cont_corr)) {
      stringr::str_to_upper(gsub("\\s+", "", cont_corr))
    } else {
      "0.5"
    }
    result$cc <- dplyr::case_when(
      sl_cont_corr == "1"              ~ "1",
      sl_cont_corr %in% c("0.5", "1/2") ~ "0.5",
      sl_cont_corr == "1/OTHERARMCOUNT"  ~ "arm",
      sl_cont_corr %in% c("NONE", "0")  ~ "0",
      TRUE                               ~ "0"
    )

    # Comparison arm numbers (SAS defaults; overridden later in onc())
    result$exp <- as.integer(exp)
    result$ctl <- as.integer(ctl)

    # Script Launcher arm names for arm detection in onc()
    result$treat_arm <- treat_arm
    result$cont_arm  <- cont_arm

    # Script Launcher metadata tibbles (passed by SL framework)
    result$sl_datasets <- if (is.data.frame(result$sl_datasets)) {
      result$sl_datasets
    } else {
      dplyr::tibble(
        datatype           = character(0),
        name               = character(0),
        partition_variable = character(0),
        default            = character(0)
      )
    }
    result$sl_group <- if (is.data.frame(result$sl_group)) {
      result$sl_group
    } else {
      dplyr::tibble(
        group_name    = character(0),
        domain        = character(0),
        partition     = character(0),
        var_name      = character(0),
        var_value     = character(0),
        dsvg_grp_name = character(0)
      )
    }
    result$sl_subset <- if (is.data.frame(result$sl_subset)) {
      result$sl_subset
    } else {
      dplyr::tibble(
        name           = character(0),
        domain         = character(0),
        partition      = character(0),
        var_name       = character(0),
        var_value      = character(0),
        inner_operator = character(0),
        outer_operator = character(0)
      )
    }
  }

  # Store run location for downstream use

  result$run_location <- run_location

  result
}


# =============================================================================
# parse_continuity_correction — Continuity Correction Parsing
# =============================================================================
# Migrated from SAS DATA _NULL_ block (SAS lines 291-307).
# Determines whether a continuity correction should be applied and whether
# the correction value is a whole number.
#
# SAS logic:
#   - If cc contains alphabetical characters and equals "arm" → cc_sw = 2
#   - Else if cc is non-missing and non-zero → cc_sw = 1
#   - Else → cc_sw = 0
#   - If cc_sw = 2 → cc_whole = 0
#   - Else if cc has fractional part → cc_whole = 0
#   - Else → cc_whole = 1
#
# @param cc Character string: continuity correction value.
# @return Named list with elements:
#   \item{cc_sw}{Integer: 0 = no correction, 1 = constant correction,
#                2 = reciprocal of opposite arm ("arm" mode).}
#   \item{cc_whole}{Integer: 1 = cc is a whole number, 0 = fractional or arm.}
# =============================================================================
parse_continuity_correction <- function(cc) {

  # Defensive input handling
  if (is.null(cc) || length(cc) == 0L) {
    return(list(cc_sw = 0L, cc_whole = 1L))
  }
  cc <- as.character(cc)

  # Determine if cc contains alphabetical characters (SAS: anyalpha())
  has_alpha <- stringr::str_detect(cc, "[A-Za-z]")

  # Determine cc_sw (SAS lines 297-299)
  if (has_alpha && tolower(cc) == "arm") {
    cc_sw <- 2L
  } else {
    cc_numeric <- suppressWarnings(as.numeric(cc))
    if (!is.na(cc_numeric) && cc_numeric != 0) {
      cc_sw <- 1L
    } else {
      cc_sw <- 0L
    }
  }

  # Determine cc_whole (SAS lines 301-303)
  if (cc_sw == 2L) {
    cc_whole <- 0L
  } else {
    cc_numeric <- suppressWarnings(as.numeric(cc))
    if (!is.na(cc_numeric) && (cc_numeric - floor(cc_numeric)) != 0) {
      cc_whole <- 0L
    } else {
      cc_whole <- 1L
    }
  }

  list(cc_sw = cc_sw, cc_whole = cc_whole)
}


# =============================================================================
# onc — Main AE Toxicity Panel Orchestrator
# =============================================================================
# Migrated from SAS %onc macro (SAS lines 310-378) plus runtime timing
# (SAS lines 380-388) and global setup (SAS lines 272-289).
#
# This function orchestrates the complete AE toxicity analysis workflow:
#   1. Source dependent utility and analysis modules
#   2. Set global configuration parameters (validation switch, toxicity bounds)
#   3. Call setup() to validate and merge DM/AE/EX datasets
#   4. Detect treatment/control arms in Script Launcher mode
#   5. Perform toxicity grade summary aggregation
#   6. Perform preferred term analysis by toxicity grade
#   7. Perform two-term comparison analysis (if >1 arm and comparison possible)
#   8. Format outputs for Excel worksheets
#   9. Generate complete Excel workbook via out_onc()
#  10. Handle error conditions via error_summary()
#
# SAS %include directives → source() calls with parameterized paths.
# SAS global %let variables → local R variables within the function scope.
# SAS DATA _NULL_ arm detection → dplyr pipeline on arm_summary.
# SAS timing block → Sys.time() with cli output.
#
# @param params_list    Named list: output from params() function containing
#                       all resolved configuration values.
# @param ae             Data frame: AE domain dataset (CDISC SDTM).
# @param dm             Data frame: DM domain dataset (CDISC SDTM).
# @param ex             Data frame: EX domain dataset (CDISC SDTM).
# @param meddra_data    Data frame or NULL: MedDRA hierarchy dataset.
# @param saspath        Character: path to the AE_Toxicity R programs directory.
#                       Overrides params_list$saspath if provided.
# @param utilpath       Character: path to the ZZ_Utilities R programs directory.
#                       Overrides params_list$utilpath if provided.
#
# @return Named list with:
#   \item{success}{Logical: TRUE if analysis completed, FALSE on error.}
#   \item{output_file}{Character: path to the generated workbook (or error
#                      workbook).}
#   \item{params_list}{Named list: final resolved parameters.}
# =============================================================================
onc <- function(params_list,
                ae          = NULL,
                dm          = NULL,
                ex          = NULL,
                meddra_data = NULL,
                saspath     = NULL,
                utilpath    = NULL) {

  # Record start time (SAS line 380)
  start_time <- Sys.time()

  # -----------------------------------------------------------------------
  # Resolve paths and source dependencies (SAS lines 272-289)
  # -----------------------------------------------------------------------
  saspath_resolved  <- if (!is.null(saspath)) saspath else params_list$saspath
  utilpath_resolved <- if (!is.null(utilpath)) utilpath else params_list$utilpath

  # Source utility modules (SAS %include directives, lines 283-289)
  # These are sourced into the calling environment so their functions are
  # available to this orchestrator and to each other.
  source(file.path(utilpath_resolved, "data_checks.R"),   local = FALSE)
  source(file.path(utilpath_resolved, "xml_output.R"),    local = FALSE)
  source(file.path(utilpath_resolved, "sl_gs_output.R"),  local = FALSE)
  source(file.path(utilpath_resolved, "err_output.R"),    local = FALSE)
  source(file.path(utilpath_resolved, "ae_setup.R"),      local = FALSE)
  source(file.path(saspath_resolved,  "ae_oncology_aggregate.R"), local = FALSE)
  source(file.path(saspath_resolved,  "ae_oncology_output.R"),    local = FALSE)

  # -----------------------------------------------------------------------
  # Global configuration setup (SAS lines 272-281)
  # -----------------------------------------------------------------------
  vld_sw        <- 1L                        # data validation switch (SAS line 273)
  toxgr_min     <- 1L                        # min toxicity grade (SAS line 277)
  toxgr_max     <- 5L                        # max toxicity grade (SAS line 278)
  ae_rate_ci_sw <- 1L                        # AE rate confidence limits (SAS line 281)

  # -----------------------------------------------------------------------
  # Resolve input datasets (from params_list if not passed directly)
  # -----------------------------------------------------------------------
  if (is.null(ae)) ae <- params_list$ae
  if (is.null(dm)) dm <- params_list$dm
  if (is.null(ex)) ex <- params_list$ex

  # Extract key parameters from params_list
  ver            <- params_list$ver
  meddra         <- params_list$meddra
  meddra_pct     <- params_list$meddra_pct
  toxgr_grp5_sw  <- params_list$toxgr_grp5_sw
  exp_arm        <- params_list$exp
  ctl_arm        <- params_list$ctl
  cmptrm         <- params_list$cmptrm
  cmpgr          <- params_list$cmpgr
  cmpsort        <- params_list$cmpsort
  cc             <- params_list$cc
  run_location   <- params_list$run_location
  study_lag      <- params_list$study_lag

  # Parse continuity correction (SAS lines 291-307)
  cc_result <- parse_continuity_correction(cc)
  cc_sw     <- cc_result$cc_sw
  cc_whole  <- cc_result$cc_whole

  # -----------------------------------------------------------------------
  # Setup call (SAS lines 312-316)
  # -----------------------------------------------------------------------
  use_mdhier <- if (nzchar(ver) &&
                    toupper(stringr::str_sub(ver, 1, 1)) == "N") "N" else "Y"

  if (use_mdhier == "N") {
    # No MedDRA (SAS lines 312-314: %setup; %let meddra = N;)
    setup_result <- setup(
      dm        = dm,
      ae        = ae,
      ex        = ex,
      mdhier    = "N",
      vld_sw    = vld_sw,
      study_lag = study_lag,
      toxgr_min = toxgr_min,
      toxgr_max = toxgr_max,
      sl_subset = params_list$sl_subset
    )
    meddra <- "N"
  } else {
    # With MedDRA (SAS line 316: %setup(mdhier=Y);)
    setup_result <- setup(
      dm          = dm,
      ae          = ae,
      ex          = ex,
      mdhier      = "Y",
      meddra_data = meddra_data,
      ver         = ver,
      vld_sw      = vld_sw,
      study_lag   = study_lag,
      toxgr_min   = toxgr_min,
      toxgr_max   = toxgr_max,
      sl_subset   = params_list$sl_subset
    )
  }

  # Extract setup results
  setup_success  <- setup_result$setup_success
  ds_base        <- setup_result$ds_base
  arm_count      <- setup_result$arm_count
  arm_names      <- setup_result$arm_names
  arm_counts     <- setup_result$arm_counts
  rpt_chk_var    <- setup_result$rpt_chk_var
  rpt_chk_var_req <- setup_result$rpt_chk_var_req
  meddra_pct_val <- setup_result$meddra_pct

  # -----------------------------------------------------------------------
  # MedDRA matching percentage check (SAS line 319)
  # -----------------------------------------------------------------------
  if (!is.null(meddra_pct_val) && !is.na(meddra_pct_val) &&
      meddra_pct_val < 80) {
    meddra <- "N"
  }

  # -----------------------------------------------------------------------
  # Treatment/control arm detection — Script Launcher mode (SAS lines 323-339)
  # -----------------------------------------------------------------------
  if (run_location != "LOCAL" && setup_success) {
    treat_arm_name <- params_list$treat_arm
    cont_arm_name  <- params_list$cont_arm

    # Build arm lookup tibble from setup results (SAS: all_arm dataset)
    # arm_names is a named vector: c(arm_name_1 = "Name1", ...)
    all_arm <- dplyr::tibble(
      arm_num = seq_len(arm_count),
      arm     = unname(arm_names[paste0("arm_name_", seq_len(arm_count))])
    )

    # Find exp arm number by matching arm name (SAS lines 327-328)
    found_exp <- NA_integer_
    if (!is.null(treat_arm_name) && nzchar(treat_arm_name)) {
      match_exp <- all_arm %>%
        dplyr::filter(
          toupper(trimws(gsub("\\s+", " ", .data$arm))) ==
            toupper(trimws(gsub("\\s+", " ", treat_arm_name)))
          |
          startsWith(
            toupper(trimws(gsub("\\s+", " ", .data$arm))),
            toupper(trimws(gsub("\\s+", " ", treat_arm_name)))
          )
        ) %>%
        dplyr::pull(arm_num)
      if (length(match_exp) > 0) found_exp <- match_exp[1]
    }

    # Find ctl arm number by matching arm name (SAS lines 329-330)
    found_ctl <- NA_integer_
    if (!is.null(cont_arm_name) && nzchar(cont_arm_name)) {
      match_ctl <- all_arm %>%
        dplyr::filter(
          toupper(trimws(gsub("\\s+", " ", .data$arm))) ==
            toupper(trimws(gsub("\\s+", " ", cont_arm_name)))
          |
          startsWith(
            toupper(trimws(gsub("\\s+", " ", .data$arm))),
            toupper(trimws(gsub("\\s+", " ", cont_arm_name)))
          )
        ) %>%
        dplyr::pull(arm_num)
      if (length(match_ctl) > 0) found_ctl <- match_ctl[1]
    }

    # Apply defaults if not found (SAS lines 332-334)
    exp_arm <- dplyr::if_else(is.na(found_exp), 1L, as.integer(found_exp))
    ctl_arm <- dplyr::if_else(is.na(found_ctl),
                              as.integer(min(2L, arm_count)),
                              as.integer(found_ctl))
  }

  # -----------------------------------------------------------------------
  # Main analysis block (SAS lines 342-370)
  # -----------------------------------------------------------------------
  if (setup_success) {

    # Ensure toxgr_grp5_sw is valid when toxgr_max is 4 (SAS line 346)
    if (toxgr_max == 4L) {
      toxgr_grp5_sw <- 0L
    }

    # Build arm subject count vector from setup results
    arm_subjcnt <- unname(arm_counts[paste0("arm_", seq_len(arm_count))])
    arm_names_vec <- unname(arm_names[paste0("arm_name_", seq_len(arm_count))])

    # Build all_arm tibble for compare function
    all_arm_tbl <- dplyr::tibble(
      arm_num = seq_len(arm_count),
      arm     = arm_names_vec,
      arm_n   = arm_subjcnt
    )

    # --- Toxicity Grade Summary (SAS line 349) ---
    # %aggregate(ds_base,pt_1,total)
    pt_1_result <- onc_aggregate(
      dsin          = ds_base,
      dsout_name    = "pt_1",
      by_vars       = "total",
      arm_count     = arm_count,
      arm_subjcnt   = arm_subjcnt,
      arm_names     = arm_names_vec,
      toxgr_min     = toxgr_min,
      toxgr_max     = toxgr_max,
      toxgr_grp5_sw = toxgr_grp5_sw,
      meddra        = meddra,
      report        = TRUE,
      output        = TRUE
    )

    # --- Preferred Term Analysis by Toxicity Grade (SAS lines 352-353) ---
    # %aggregate(ds_base,pt_2,aebodsys,aedecod)
    pt_2_result <- onc_aggregate(
      dsin          = ds_base,
      dsout_name    = "pt_2",
      by_vars       = c("aebodsys", "aedecod"),
      arm_count     = arm_count,
      arm_subjcnt   = arm_subjcnt,
      arm_names     = arm_names_vec,
      toxgr_min     = toxgr_min,
      toxgr_max     = toxgr_max,
      toxgr_grp5_sw = toxgr_grp5_sw,
      meddra        = meddra,
      report        = TRUE,
      output        = TRUE
    )

    # Collect report metadata
    rpt_key     <- dplyr::bind_rows(pt_1_result$rpt_key, pt_2_result$rpt_key)
    rpt_missing <- dplyr::bind_rows(pt_1_result$rpt_missing,
                                     pt_2_result$rpt_missing)

    # %fmt_output(pt_2_output)
    pt_2_fmt_result <- fmt_output(
      ds      = pt_2_result$output,
      ds_name = "pt_2",
      rpt_key = rpt_key,
      cc_sw   = cc_sw
    )

    # --- Two-Term Analysis (SAS lines 355-366) ---
    pt_3_result     <- NULL
    pt_3_fmt_result <- NULL
    pt_3_cc_ind     <- NULL

    if (arm_count > 1L) {

      # Determine dataset and comparison terms based on MedDRA availability
      if (meddra == "Y") {
        # Use MedDRA-enriched dataset (SAS line 359)
        compare_ds    <- ds_base
        compare_terms <- cmptrm
      } else {
        # Fall back to provided terms (SAS lines 362-363)
        compare_terms <- "aebodsys,aedecod"
        compare_ds    <- ds_base
      }

      # Parse comparison term vector
      compare_by_vars <- stringr::str_trim(strsplit(compare_terms, ",")[[1]])

      # %compare(ds,pt_3,cmptrm) (SAS line 359 or 363)
      pt_3_result <- onc_compare(
        dsin          = compare_ds,
        dsout_name    = "pt_3",
        by_vars       = compare_by_vars,
        exp           = exp_arm,
        ctl           = ctl_arm,
        arm_count     = arm_count,
        arm_subjcnt   = arm_subjcnt,
        arm_names     = arm_names_vec,
        cmpgr         = cmpgr,
        cc            = as.numeric(dplyr::if_else(cc_sw == 0L, "0", cc)),
        cc_sw         = cc_sw,
        cc_whole      = cc_whole,
        ae_rate_ci_sw = ae_rate_ci_sw,
        toxgr_grp5_sw = toxgr_grp5_sw,
        meddra        = meddra,
        report        = TRUE,
        all_arm       = all_arm_tbl
      )

      # Update rpt_key and rpt_missing with pt_3 metadata
      if (!is.null(pt_3_result$rpt_key)) {
        rpt_key <- dplyr::bind_rows(rpt_key, pt_3_result$rpt_key)
      }
      if (!is.null(pt_3_result$rpt_missing)) {
        rpt_missing <- dplyr::bind_rows(rpt_missing, pt_3_result$rpt_missing)
      }

      # Extract cc_ind data for formatting
      pt_3_cc_ind <- pt_3_result$output_cc_ind

      # %fmt_output(pt_3_output,sort_sw=yes,sortvar=&cmpsort.,sortgrp_sw=yes)
      pt_3_fmt_result <- fmt_output(
        ds         = pt_3_result$output,
        ds_name    = "pt_3",
        rpt_key    = rpt_key,
        cc_sw      = cc_sw,
        cc_ind_ds  = pt_3_cc_ind,
        sort_sw    = TRUE,
        sortvar    = cmpsort,
        sortgrp_sw = TRUE
      )
    }

    # --- Build comprehensive params_list for output function ---
    output_params <- params_list
    output_params$arm_count       <- arm_count
    output_params$arm_names       <- arm_names
    output_params$arm_counts      <- arm_counts
    output_params$toxgr_min       <- toxgr_min
    output_params$toxgr_max       <- toxgr_max
    output_params$toxgr_grp5_sw   <- toxgr_grp5_sw
    output_params$meddra          <- meddra
    output_params$cc_sw           <- cc_sw
    output_params$cc_whole        <- cc_whole
    output_params$ae_rate_ci_sw   <- ae_rate_ci_sw
    output_params$vld_sw          <- vld_sw
    output_params$exp             <- exp_arm
    output_params$ctl             <- ctl_arm
    output_params$cmptrm          <- cmptrm
    output_params$cmpgr           <- cmpgr
    output_params$cmpsort         <- cmpsort
    output_params$sl_subset_outer <- NULL

    # --- Generate workbook (SAS line 368: %out_onc) ---
    output_file <- out_onc(
      params_list         = output_params,
      pt_1_output         = pt_1_result$output,
      pt_2_output         = pt_2_result$output,
      pt_2_output_fmt     = pt_2_fmt_result$fmt,
      pt_3_output         = if (!is.null(pt_3_result)) pt_3_result$output else NULL,
      pt_3_output_fmt     = if (!is.null(pt_3_fmt_result)) pt_3_fmt_result$fmt else NULL,
      pt_3_output_cc_ind  = if (!is.null(pt_3_fmt_result)) pt_3_fmt_result$fmt_cc_ind else NULL,
      rpt_key             = rpt_key,
      rpt_missing         = rpt_missing,
      dm_validation_data  = setup_result$rpt_dm,
      ae_matching_data    = setup_result$rpt_meddra
    )

    # Record end time and report duration (SAS lines 384-388)
    end_time <- Sys.time()
    elapsed  <- end_time - start_time
    cli::cli_alert_info("RUNNING TIME: {format(elapsed)}")

    return(list(
      success     = TRUE,
      output_file = output_file,
      params_list = output_params
    ))

  # -----------------------------------------------------------------------
  # Error handling block (SAS lines 371-376)
  # -----------------------------------------------------------------------
  } else {

    # Determine error flags (SAS lines 373-374)
    # dm_subj_gt0 is TRUE if DM had subjects; invert for err_nosubj
    dm_subj_gt0   <- !is.null(setup_result$arm_count) &&
                     setup_result$arm_count > 0L
    setup_req_var <- !is.null(setup_result$rpt_chk_var_req) &&
                     all(setup_result$rpt_chk_var_req$ind == 1L)

    err_result <- error_summary(
      err_file        = params_list$errout,
      panel_title     = params_list$panel_title,
      ndabla          = params_list$ndabla,
      studyid         = params_list$studyid,
      err_nosubj      = !dm_subj_gt0,
      err_missvar     = !setup_req_var,
      err_seterr      = TRUE,
      panel_desc      = params_list$panel_desc,
      sl_subset       = params_list$sl_subset,
      rpt_chk_var_req = setup_result$rpt_chk_var_req
    )

    # Record end time and report duration
    end_time <- Sys.time()
    elapsed  <- end_time - start_time
    cli::cli_alert_info("RUNNING TIME: {format(elapsed)}")

    return(list(
      success     = FALSE,
      output_file = params_list$errout,
      params_list = params_list,
      error       = err_result
    ))
  }
}


# ============================================================
#### MIGRATION NOTES
#### ============================================================
#### ASSUMPTIONS:
####    - SAS `options missing='';` means missing values display as blank;
####      R NA handles this natively with formatting options
####    - SAS `options minoperator;` enables the IN operator in macro language;
####      R has native %in% operator — no equivalent needed
####    - Run location detection (SAS %symexist) → R function argument with
####      default "LOCAL"
####    - SAS global macro variable scope → R named list returned from params()
####    - SAS libname/dataset references → haven::read_xpt()/haven::read_sas()
####      with parameterized paths
####    - Script Launcher metadata tables → tibble placeholders with identical
####      column structure
####    - SAS `=:` operator (starts-with comparison in arm matching) is
####      replicated via startsWith() in R
####    - SAS compbl(trim(...)) for multi-space normalization is replicated via
####      gsub("\\s+", " ", trimws(...))
#### POTENTIAL NUMERICAL DIFFERENCES:
####    - Timing precision: SAS uses time() (seconds since midnight) vs R
####      Sys.time() (POSIXct) — format differences only, no functional impact
####    - MedDRA matching percentage threshold (80%) applied identically
#### NO DIRECT R EQUIVALENT:
####    - SAS %symdel (delete macro variables) → not needed; R uses function
####      scoping
####    - SAS proc datasets kill → not needed; R environments are managed
####      automatically
####    - SAS %symexist → R: exists() or argument-based detection
####    - SAS %include → source()
####    - SAS call symputx → R: direct variable assignment within function scope
####    - SAS libname → haven::read_xpt()/haven::read_sas()
#### PACKAGE SELECTION RATIONALE:
####    - haven: SAS data I/O (read_xpt, read_sas) for CDISC XPT transport
####      files
####    - dplyr: Data manipulation replacing DATA steps (mutate, filter,
####      group_by, case_when, if_else)
####    - tidyr: Data reshaping loaded for downstream module availability
####    - readr: Flat file I/O loaded for downstream module availability
####    - stringr: String manipulation (str_detect, str_trim, str_to_upper,
####      str_sub) replacing SAS character functions
####    - janitor: round_half_up() for SAS-compatible rounding where needed
####    - cli: User-facing messages replacing SAS %put statements
####    - openxlsx: Excel output replacing SpreadsheetML XML generation
#### OPEN QUESTIONS:
####    - Confirm dataset format (XPT vs SAS7BDAT) for input data
####    - Verify Script Launcher parameter mapping is complete for all panel
####      options
####    - Whether MedDRA hierarchy datasets are in XPT or SAS7BDAT format
#### ============================================================
