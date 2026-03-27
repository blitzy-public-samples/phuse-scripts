#' Assert Complete Reference Dataset
#'
#' Validates that a reference dataset (e.g., ADSL) contains key information for
#' all observations that appear in related measurement datasets (e.g., ADAE,
#' ADVS, ADLBC). This is the R migration of the SAS macro
#' \code{\%assert_complete_refds(dsets, keys)} from
#' \code{whitepapers/utilities/assert_complete_refds.sas}.
#'
#' @param dsets A named list of data frames where the \strong{first element} is
#'   the reference dataset and all subsequent elements are measurement datasets.
#'   A minimum of 2 data frames is required.
#'   \itemize{
#'     \item Example: \code{list(adsl = adsl_df, adae = adae_df, advs = advs_df)}
#'     \item If unnamed, default names \code{"dataset_1"}, \code{"dataset_2"}, etc.
#'       are assigned automatically.
#'   }
#' @param keys Character vector of key column names used to link observations
#'   across the reference and measurement datasets.
#'   \itemize{
#'     \item Example: \code{c("STUDYID", "USUBJID")}
#'     \item All keys must exist in every dataset provided.
#'   }
#'
#' @return Invisible logical: \code{TRUE} (PASS) if the reference dataset
#'   contains all key combinations present in every measurement dataset, or
#'   \code{FALSE} (FAIL) if any measurement dataset contains key combinations
#'   missing from the reference. On FAIL, the return value carries an attribute
#'   \code{"fail_crds"} — a tibble listing the missing key combinations along
#'   with the source measurement dataset name.
#'
#' @details
#' The function mirrors the SAS macro behaviour:
#' \enumerate{
#'   \item Validates all datasets exist via \code{assert_dset_exist()}.
#'   \item Extracts distinct key combinations from each dataset
#'         (replaces SAS \code{PROC SORT NODUPKEY}).
#'   \item For each measurement dataset, performs an anti-join against the
#'         reference keys to find observations missing from the reference
#'         (replaces SAS \code{DATA step MERGE} with \code{IN=} flags).
#'   \item Collects all failures into a single tibble (\code{fail_crds}).
#'   \item Returns \code{TRUE}/\code{FALSE} analogous to SAS global
#'         \code{CONTINUE = 1/0}.
#' }
#'
#' @section SAS Equivalence:
#' \describe{
#'   \item{SAS}{%assert_complete_refds(ADSL ADAE ADVS, STUDYID USUBJID)}
#'   \item{R}{assert_complete_refds(list(adsl=adsl, adae=adae, advs=advs),
#'            c("STUDYID", "USUBJID"))}
#' }
#'
#' @examples
#' # Create example data
#' adsl <- data.frame(STUDYID = "S1", USUBJID = c("01", "02", "03"))
#' adae <- data.frame(STUDYID = "S1", USUBJID = c("01", "02"), AETERM = c("H", "N"))
#' advs <- data.frame(STUDYID = "S1", USUBJID = c("01", "02"), AVAL = c(120, 130))
#'
#' # All measurement subjects in reference — PASS
#' result <- assert_complete_refds(
#'   dsets = list(adsl = adsl, adae = adae, advs = advs),
#'   keys  = c("STUDYID", "USUBJID")
#' )
#' # result is TRUE
#'
#' # Measurement dataset has subject not in reference — FAIL
#' adae_extra <- data.frame(STUDYID = "S1", USUBJID = c("01", "99"), AETERM = c("H", "X"))
#' result <- assert_complete_refds(
#'   dsets = list(adsl = adsl, adae = adae_extra),
#'   keys  = c("STUDYID", "USUBJID")
#' )
#' # result is FALSE; attr(result, "fail_crds") contains the missing keys
#'
#' @export
assert_complete_refds <- function(dsets, keys) {

  # ===========================================================================
  # Phase 1: Input validation

  # Mirrors SAS macro parameter checks before looping over datasets.
  # ===========================================================================

  # Validate dsets is a proper list of data frames (not a single data frame).

  # Note: data frames are technically lists in R (rlang::is_list returns TRUE),
  # so we must check for data frames first before the is_list guard.
  if (is.data.frame(dsets)) {
    rlang::abort(
      paste0(
        "(ASSERT_COMPLETE_REFDS) `dsets` must be a list of data frames, ",
        "not a single data frame. Wrap multiple data frames in list()."
      ),
      class = "assert_complete_refds_input_error"
    )
  }

  if (!rlang::is_list(dsets)) {
    rlang::abort(
      paste0(
        "(ASSERT_COMPLETE_REFDS) `dsets` must be a list of data frames, not ",
        class(dsets)[[1L]], "."
      ),
      class = "assert_complete_refds_input_error"
    )
  }

  # Validate keys is a character vector
  if (!rlang::is_character(keys) || length(keys) == 0L) {
    rlang::abort(
      "(ASSERT_COMPLETE_REFDS) `keys` must be a non-empty character vector of column names.",
      class = "assert_complete_refds_input_error"
    )
  }

  # Ensure minimum of 2 datasets (SAS line 6: "minimum of 2 data sets")
  if (length(dsets) < 2L) {
    cli::cli_abort(
      paste0(
        "(ASSERT_COMPLETE_REFDS) Result is {.strong FAIL}. ",
        "At least 2 datasets required (1 reference + 1 measurement), but ",
        "{length(dsets)} provided."
      )
    )
  }

  # Assign default names if the list is unnamed (for informative error messages)
  if (is.null(names(dsets))) {
    names(dsets) <- paste0("dataset_", seq_along(dsets))
  } else {
    # Fill in any blank names
    empty_names <- !nzchar(names(dsets))
    if (any(empty_names)) {
      names(dsets)[empty_names] <- paste0("dataset_", which(empty_names))
    }
  }

  # Validate every element is a data frame
  non_df <- vapply(dsets, function(x) !is.data.frame(x), logical(1L))
  if (any(non_df)) {
    bad_names <- names(dsets)[non_df]
    cli::cli_abort(
      paste0(
        "(ASSERT_COMPLETE_REFDS) Result is {.strong FAIL}. ",
        "The following elements of `dsets` are not data frames: ",
        paste(toupper(bad_names), collapse = ", "), "."
      )
    )
  }

  # ===========================================================================
  # Phase 2: Dataset existence validation
  # Mirrors SAS lines 45-57: %assert_dset_exist(&nxt) for each dataset.
  # ===========================================================================
  ok <- TRUE

  for (ds_name in names(dsets)) {
    # Call assert_dset_exist with each data frame directly.
    # The dependency function returns TRUE for a data frame passed directly.
    ds_exists <- assert_dset_exist(dsets[[ds_name]])

    if (!isTRUE(ds_exists)) {
      ok <- FALSE
      # SAS line 55: ERROR for non-existent dataset
      cli::cli_warn(
        paste0(
          "(ASSERT_COMPLETE_REFDS) Result is FAIL. ",
          "Invalid, non-existent dataset {.val {toupper(ds_name)}}."
        )
      )
    }
  }

  # If any dataset failed existence check, return FALSE early (SAS: OK = 0
  # means the MERGE block is skipped entirely — lines 59-79)
  if (!ok) {
    result <- FALSE
    attr(result, "fail_crds") <- NULL
    return(invisible(result))
  }

  # ===========================================================================
  # Phase 3: Key column validation
  # SAS line 27: "Macro checks that data sets exist, but does not check that
  # keys exist on each dset." We go beyond SAS by also validating keys exist,
  # providing a better user experience.
  # ===========================================================================
  for (ds_name in names(dsets)) {
    missing_keys <- setdiff(keys, names(dsets[[ds_name]]))
    if (length(missing_keys) > 0L) {
      cli::cli_abort(
        paste0(
          "(ASSERT_COMPLETE_REFDS) Result is {.strong FAIL}. ",
          "Dataset {.val {toupper(ds_name)}} does not contain required key column(s): ",
          paste(toupper(missing_keys), collapse = ", "), "."
        )
      )
    }
  }

  # ===========================================================================
  # Phase 4: Extract distinct key combinations from each dataset
  # Replaces SAS PROC SORT NODUPKEY (lines 49-51):
  #   proc sort data = &nxt (keep=&keys) out=crds_dset&idx nodupkey;
  #     by &keys;
  #   run;
  # ===========================================================================
  key_sets <- purrr::map(dsets, function(.df) {
    dplyr::distinct(.df, dplyr::across(dplyr::all_of(keys)))
  })

  # ===========================================================================
  # Phase 5: Reference completeness check
  # Replaces SAS DATA step MERGE with IN= flags (lines 59-76).
  # For each measurement dataset: find key combinations NOT in the reference.
  # SAS logic: keeps only rows where NOT in_ds1 (reference), logs ERROR for
  # each, and collects them into FAIL_CRDS.
  # ===========================================================================
  ref_name <- names(dsets)[[1L]]
  ref_keys <- key_sets[[1L]]

  # Named list of measurement datasets (all except the first)
  measurement_names <- names(key_sets)[-1L]

  # Use purrr::imap to iterate over measurement datasets with their names
  failure_list <- purrr::imap(key_sets[measurement_names], function(meas_keys, ds_name) {
    # Anti-join: find keys in the measurement dataset that are NOT in
    # the reference dataset (replaces SAS MERGE with IN= flag check)
    missing_obs <- dplyr::anti_join(meas_keys, ref_keys, by = keys)

    if (nrow(missing_obs) > 0L) {
      # Add source dataset identifier column (replaces SAS found_ds{idx} flags)
      dplyr::mutate(missing_obs, source_dataset = ds_name)
    } else {
      NULL
    }
  })

  # Combine all failure records across measurement datasets into a single tibble
  # Replaces the SAS FAIL_CRDS output dataset
  fail_crds <- dplyr::bind_rows(failure_list)

  # ===========================================================================
  # Phase 6: Reporting and return value
  # Mirrors SAS lines 70-78 and 87-88 (CONTINUE flag).
  # ===========================================================================
  if (nrow(fail_crds) == 0L) {
    # All measurement subjects found in reference — PASS
    # SAS line 78: NOTE: (ASSERT_COMPLETE_REFDS) Result is PASS. <REF> includes all subjects.
    cli::cli_inform(
      paste0(
        "(ASSERT_COMPLETE_REFDS) Result is PASS. ",
        toupper(ref_name),
        " includes all subjects."
      )
    )

    result <- TRUE
    return(invisible(result))
  }

  # --------------------------------------------------------------------------
  # FAIL path: some measurement datasets contain keys missing from reference
  # SAS lines 70-74: ERROR for each missing observation + FAIL_CRDS dataset
  # --------------------------------------------------------------------------

  # Log a warning for each missing key combination
  # (SAS uses PUT "ERROR:" inside the DATA step for each observation)
  for (row_idx in seq_len(nrow(fail_crds))) {
    row_data <- fail_crds[row_idx, , drop = FALSE]
    # Build a readable key-value representation for the log message
    key_vals <- vapply(keys, function(k) {
      paste0(toupper(k), "=", as.character(row_data[[k]]))
    }, character(1L))
    key_str <- paste(key_vals, collapse = " ")
    source_ds <- row_data[["source_dataset"]]

    cli::cli_warn(
      paste0(
        "(ASSERT_COMPLETE_REFDS) Result is FAIL. ",
        "Obs missing from reference dataset {.val {toupper(ref_name)}}: ",
        key_str,
        " (found in {.val {toupper(source_ds)}})."
      )
    )
  }

  # Return FALSE with fail_crds attached as attribute
  # Mirrors SAS: CONTINUE = 0 and WORK.FAIL_CRDS dataset
  result <- FALSE
  attr(result, "fail_crds") <- fail_crds
  return(invisible(result))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS global CONTINUE flag (1/0) mapped to R logical return
#      value (TRUE/FALSE). Callers check the return value instead
#      of reading a global variable.
#    - SAS space-delimited dataset names (%scan) mapped to an R
#      named list of data frames. The first element is always the
#      reference dataset (e.g., ADSL); remaining elements are
#      measurement datasets.
#    - SAS space-delimited KEYS parameter mapped to an R character
#      vector, e.g., c("STUDYID", "USUBJID").
#    - SAS PROC SORT NODUPKEY mapped to dplyr::distinct(across(
#      all_of(keys))) for extracting unique key combinations.
#    - SAS DATA step MERGE with IN= flags for key comparison mapped
#      to dplyr::anti_join() — finds keys in measurement datasets
#      that are NOT present in the reference dataset.
#    - SAS FAIL_CRDS output dataset mapped to an R tibble attached
#      as the "fail_crds" attribute on the return value.
#    - Key column validation was added beyond original SAS behavior
#      (SAS line 27 notes "does not check that keys exist on each
#      dset") to provide earlier, more informative error messages.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a validation utility comparing key values
#      (character/numeric equality). No rounding or arithmetic is
#      performed.
# NO DIRECT R EQUIVALENT:
#    - SAS %GLOBAL CONTINUE: R uses a function return value
#      (TRUE/FALSE). Callers must check the result rather than
#      reading a global macro variable.
#    - SAS PROC DATASETS DELETE: R relies on garbage collection.
#      Temporary objects created inside the function are
#      automatically reclaimed when the function exits.
#    - SAS DATA step MERGE (full outer join with IN= flags):
#      Replaced by dplyr::anti_join() which directly identifies
#      keys missing from the reference, achieving the same logical
#      outcome without an explicit full outer merge.
# PACKAGE SELECTION RATIONALE:
#    - dplyr (>=1.1.0): anti_join() for finding missing keys
#      (replaces DATA step MERGE with IN= flags); distinct() with
#      across(all_of()) for unique key extraction (replaces PROC
#      SORT NODUPKEY); bind_rows() for collecting failures; mutate()
#      for adding dataset-source identifiers.
#    - purrr (>=1.0.0): map() for iterating over dataset list to
#      extract distinct keys (replaces SAS %DO loop); imap() for
#      named iteration during reference completeness check.
#    - cli (>=3.6.0): Informative user-facing messages matching SAS
#      %PUT NOTE/ERROR format. cli_inform() for PASS, cli_abort()
#      for programming errors, cli_warn() for data quality warnings.
#    - rlang (>=1.1.0): is_list() and is_character() for input type
#      validation; abort() for signaling programming errors with
#      structured condition classes.
# OPEN QUESTIONS:
#    - Should the function stop execution on failure (cli_abort) or
#      return FALSE for the caller to handle? Current implementation
#      returns FALSE, matching SAS behavior where CONTINUE=0 does
#      not halt the SAS session.
#    - Should fail_crds be returned as a standalone tibble output or
#      as an attribute? Current implementation uses an attribute to
#      keep the return type simple (logical).
#    - The SAS macro does NOT validate that key columns exist in each
#      dataset (noted on SAS line 27). This R version adds that
#      check for safety. If strict SAS parity is required, the key
#      validation block can be removed.
# ============================================================
