# =============================================================================
# derive_lastminmax_measure.R
# =============================================================================
# Purpose:
#   ADaM-compliant derivation utility for computing baseline and post-baseline
#   measures across LAST, MIN, and MAX change modes. Returns a vertical (long)
#   dataset with derived baseline/post-baseline rows for subjects who have BOTH
#   a non-missing baseline and post-baseline measure.
#
# Migration Source:
#   whitepapers/ADaM/derive_lastminmax_measure.sas (213 lines)
#   Original SAS macro: %derive_lastminmax_measure
#
# Version: 1.0.0 (R migration)
# License: MIT (per PhUSE CS WG5 repository)
#
# Required Packages: dplyr (>=1.1.0), rlang (>=1.1.0), tibble (>=3.2.0),
#                    cli (>=3.6.0)
# =============================================================================

#' Derive Baseline and Post-Baseline Measures for LAST/MIN/MAX Change Modes
#'
#' @title Derive Last/Min/Max Baseline and Post-Baseline Measures
#'
#' @description
#' Migrated from SAS macro \code{\%derive_lastminmax_measure}. Computes baseline
#' and post-baseline analysis records across one or more change modes (LAST, MIN,
#' MAX). For each mode and each grouping combination, the function selects a
#' single baseline observation (ADT <= TRTSDT) and a single post-baseline
#' observation (ADT > TRTSDT) according to the specified extremum rule. It then
#' derives BASE, CHG, AVISIT, AVISITN, and a flag variable for each mode.
#'
#' Design decision (from SAS source): Returns vertical data, which works well
#' with GTL PhUSEboxplot template. Baseline and Post-baseline values remain in
#' AVAL in the output dataset.
#'
#' @param ds A data.frame or tibble containing ADaM data with non-missing
#'   baseline and post-baseline measures. Must contain columns: TRTSDT
#'   (treatment start date), ADT (analysis date), AVAL (analysis value). Columns
#'   referenced in \code{grpvars}, \code{ordvars}, and \code{incl} must also be
#'   present. Date columns (TRTSDT, ADT) should be R Date objects. If they are
#'   numeric SAS date values, convert with \code{as.Date(x, origin = "1960-01-01")}
#'   before calling this function. REQUIRED.
#' @param c_modes Character vector of change modes. Valid values: "LAST", "MIN",
#'   "MAX" (case-insensitive; auto-uppercased internally). Invalid values default
#'   to "LAST" with a warning. REQUIRED. Example: \code{c("LAST", "MIN", "MAX")}.
#' @param dsout_name Optional character string. Not used for dataset naming (R
#'   returns a tibble). Kept only for backward-compatible labeling/metadata.
#'   Equivalent to SAS \code{dsout=} parameter. Default \code{NULL}.
#' @param flvars Optional character vector of flag variable names, one per
#'   element of \code{c_modes}. If \code{NULL} or shorter than \code{c_modes},
#'   missing names are auto-generated as \code{paste0("ANL", mode, "FL")}
#'   (matching SAS line 130 behavior). Default \code{NULL}.
#' @param grpvars Character vector of grouping variable names (column names on
#'   \code{ds}). The function returns 2 records per group (baseline and
#'   post-baseline) for each change mode. REQUIRED. Example:
#'   \code{c("STUDYID", "USUBJID", "TRTPN", "PARAMCD", "ATPTN")}.
#' @param ordvars Optional character vector of ordering variable names (column
#'   names on \code{ds}). Controls which observation among duplicates is kept
#'   when ties occur in the extremum selection. Default \code{NULL}.
#' @param incl Optional character vector of additional variable names from
#'   \code{ds} to carry through unmodified to the output. Default \code{NULL}.
#' @param cleanup Logical. In SAS, controls deletion of intermediate work
#'   datasets. In R, this is effectively a no-op because temporary objects are
#'   automatically garbage collected. Included for API parity with the SAS macro.
#'   Default \code{TRUE}.
#'
#' @return A tibble with columns: all \code{grpvars}, all \code{ordvars} (if
#'   specified), all \code{incl} variables (if specified), AVAL, ADT, AVISIT,
#'   AVISITN, BASE, CHG, and one flag column per change mode. For each mode and
#'   each group: one baseline row (AVISIT = "Baseline (<MODE>)", CHG = 0,
#'   BASE = AVAL) and one post-baseline row (AVISIT = "Post-baseline (<MODE>)",
#'   CHG = AVAL - BASE). AVISITN values start at 9911/9912 for the first mode
#'   and increment by 10 for each subsequent mode.
#'
#' @examples
#' \dontrun{
#' # Minimal example with LAST change mode
#' library(dplyr)
#' advs <- tibble::tibble(
#'   STUDYID  = rep("STUDY1", 6),
#'   USUBJID  = rep(c("SUBJ-01", "SUBJ-02"), each = 3),
#'   TRTPN    = rep(1L, 6),
#'   PARAMCD  = rep("SYSBP", 6),
#'   ATPTN    = rep(1L, 6),
#'   TRTSDT   = as.Date(rep("2020-01-15", 6)),
#'   ADT      = as.Date(c("2020-01-10", "2020-01-14", "2020-02-15",
#'                         "2020-01-12", "2020-01-13", "2020-03-01")),
#'   AVAL     = c(120, 125, 130, 118, 122, 135),
#'   AVISIT   = c("Screening", "Baseline", "Week 4",
#'                "Screening", "Baseline", "Week 8")
#' )
#'
#' result <- derive_lastminmax_measure(
#'   ds      = advs,
#'   c_modes = c("LAST", "MIN"),
#'   grpvars = c("STUDYID", "USUBJID", "TRTPN", "PARAMCD", "ATPTN")
#' )
#' }
#'
#' @export
derive_lastminmax_measure <- function(ds,
                                      c_modes,
                                      dsout_name = NULL,
                                      flvars     = NULL,
                                      grpvars    = NULL,
                                      ordvars    = NULL,
                                      incl       = NULL,
                                      cleanup    = TRUE) {

  # ===========================================================================
  # Input Validation
  # ===========================================================================


  # --- Validate ds is a data.frame or tibble ---
  if (!is.data.frame(ds)) {
    cli::cli_abort(c(
      "x" = "{.arg ds} must be a data.frame or tibble.",
      "i" = "You supplied an object of class {.cls {class(ds)}}."
    ))
  }

  # --- Validate c_modes is a non-empty character vector ---
  if (missing(c_modes) || is.null(c_modes) || !is.character(c_modes) ||
      length(c_modes) == 0L) {
    cli::cli_abort(c(
      "x" = "{.arg c_modes} must be a non-empty character vector.",
      "i" = "Valid values are {.val LAST}, {.val MIN}, and {.val MAX}."
    ))
  }

  # --- Validate grpvars is a non-empty character vector ---
  if (is.null(grpvars) || !is.character(grpvars) || length(grpvars) == 0L) {
    cli::cli_abort(c(
      "x" = "{.arg grpvars} must be a non-empty character vector of column names.",
      "i" = "Example: {.code c('STUDYID', 'USUBJID', 'TRTPN', 'PARAMCD', 'ATPTN')}"
    ))
  }

  # --- Auto-uppercase c_modes (SAS line 81) ---
  c_modes <- toupper(c_modes)

  # --- Validate required columns exist in ds ---
  required_cols <- c("TRTSDT", "ADT", "AVAL")
  all_needed_cols <- unique(c(required_cols, grpvars))
  if (!is.null(ordvars) && length(ordvars) > 0L) {
    all_needed_cols <- unique(c(all_needed_cols, ordvars))
  }
  if (!is.null(incl) && length(incl) > 0L) {
    all_needed_cols <- unique(c(all_needed_cols, incl))
  }

  missing_cols <- setdiff(all_needed_cols, colnames(ds))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "x" = "Required column{?s} missing from {.arg ds}: {.val {missing_cols}}.",
      "i" = "Columns present: {.val {colnames(ds)}}."
    ))
  }

  # --- Handle flvars: pad or auto-generate as needed (SAS line 130 logic) ---
  if (is.null(flvars)) {
    flvars <- paste0("ANL", c_modes, "FL")
  } else {
    if (length(flvars) < length(c_modes)) {
      # Pad with auto-generated names for missing entries
      n_missing <- length(c_modes) - length(flvars)
      auto_names <- paste0("ANL", c_modes[(length(flvars) + 1L):length(c_modes)], "FL")
      flvars <- c(flvars, auto_names)
    }
    # Replace empty strings with auto-generated names
    empty_mask <- is.na(flvars) | flvars == ""
    if (any(empty_mask)) {
      flvars[empty_mask] <- paste0("ANL", c_modes[empty_mask], "FL")
    }
  }

  # ===========================================================================
  # Baseline / Post-Baseline Split (SAS lines 89-102)
  # ===========================================================================

  # Build column selection vector for select() calls
  # Always include AVAL, ADT + grpvars; conditionally add ordvars and incl
  select_cols <- unique(c("AVAL", "ADT", grpvars))
  if (!is.null(ordvars) && length(ordvars) > 0L) {
    select_cols <- unique(c(select_cols, ordvars))
  }
  if (!is.null(incl) && length(incl) > 0L) {
    select_cols <- unique(c(select_cols, incl))
  }

  # Build arrange column vector: grpvars + ordvars
  arrange_cols <- grpvars
  if (!is.null(ordvars) && length(ordvars) > 0L) {
    arrange_cols <- c(grpvars, ordvars)
  }

  # Filter rows: remove where TRTSDT, ADT, or AVAL is NA
  # SAS line 92: if n(adt, trtsdt) < 2 or missing(aval) then delete
  # CRITICAL: Missing values map to NA, NEVER zero (AAP §0.7.3)
  ds_filtered <- ds %>%
    dplyr::filter(!is.na(.data$TRTSDT), !is.na(.data$ADT), !is.na(.data$AVAL))

  # Split into baseline (ADT <= TRTSDT) and post-baseline (ADT > TRTSDT)
  # SAS lines 94-95
  lmm_base <- ds_filtered %>%
    dplyr::filter(.data$ADT <= .data$TRTSDT)

  lmm_post <- ds_filtered %>%
    dplyr::filter(.data$ADT > .data$TRTSDT)

  # Initialize empty output tibble
  # SAS lines 104-110: creates empty DSOUT shell
  dsout <- tibble::tibble()

  # ===========================================================================
  # Change Mode Loop (SAS lines 112-209)
  # ===========================================================================

  for (idx in seq_along(c_modes)) {

    # --- Extract current mode and flag variable name ---
    nxtcm  <- c_modes[idx]
    nxtflv <- flvars[idx]

    # --- Mode -> function / variable mapping (SAS lines 119-128) ---
    if (nxtcm == "MIN") {
      lmm_func <- min
      lmm_var  <- "AVAL"
    } else if (nxtcm == "MAX") {
      lmm_func <- max
      lmm_var  <- "AVAL"
    } else {
      # LAST or unknown mode
      if (nxtcm != "LAST") {
        warning(
          "(DERIVE_LASTMINMAX_MEASURES) Invalid C_MODES value: ", nxtcm,
          ". Defaulting to LAST (rather than MIN or MAX).",
          call. = FALSE
        )
        nxtcm <- "LAST"
        # Also update flvars if it was auto-generated from the invalid mode
        if (grepl("^ANL.*FL$", nxtflv) && !grepl("LAST", nxtflv)) {
          nxtflv <- paste0("ANL", nxtcm, "FL")
        }
      }
      lmm_func <- max
      lmm_var  <- "ADT"
    }

    # --- Auto-generate flag name if empty (SAS line 130) ---
    if (is.na(nxtflv) || nxtflv == "") {
      nxtflv <- paste0("ANL", nxtcm, "FL")
    }

    # =========================================================================
    # Phase 4a: Extremum Selection (SAS lines 132-154)
    # PROC SQL HAVING + PROC SORT NODUPKEY -> group_by + filter + slice(1)
    # =========================================================================

    # --- Guard: skip this mode if either baseline or post-baseline is empty ---
    if (nrow(lmm_base) == 0L || nrow(lmm_post) == 0L) {
      next
    }

    # --- Dynamic column symbol for tidy evaluation ---
    lmm_var_sym <- rlang::sym(lmm_var)

    # --- Baseline extremum ---
    # SAS lines 133-139: PROC SQL with HAVING func(var) = var
    # SAS lines 150-151: PROC SORT NODUPKEY BY grpvars
    lmm_base_anl <- lmm_base %>%
      dplyr::select(dplyr::all_of(select_cols)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grpvars))) %>%
      dplyr::filter(!!lmm_var_sym == lmm_func(!!lmm_var_sym, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols))) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup()

    # --- Post-baseline extremum ---
    # SAS lines 141-146: PROC SQL (same pattern for post-baseline)
    # SAS lines 152-153: PROC SORT NODUPKEY BY grpvars
    lmm_post_anl <- lmm_post %>%
      dplyr::select(dplyr::all_of(select_cols)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grpvars))) %>%
      dplyr::filter(!!lmm_var_sym == lmm_func(!!lmm_var_sym, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols))) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup()

    # =========================================================================
    # Phase 4b: Merge and Derive Baseline Records (SAS lines 156-175)
    # Only keep subjects with BOTH baseline and post-baseline values
    # =========================================================================

    # Identify baseline groups that have a matching post-baseline record
    # SAS: merge lmm_base_anl (in=in_base) lmm_post_anl (in=in_post keep=grpvars)
    base_with_post <- dplyr::semi_join(
      lmm_base_anl, lmm_post_anl, by = grpvars
    )
    base_without_post <- dplyr::anti_join(
      lmm_base_anl, lmm_post_anl, by = grpvars
    )

    # SAS line 167: WARNING for omitted baseline-only records
    if (nrow(base_without_post) > 0L) {
      warning(
        "(DERIVE_LASTMINMAX_MEASURES) Omitting ", nrow(base_without_post),
        " obs without a post-baseline measure",
        call. = FALSE
      )
    }

    # Derive baseline columns (SAS lines 171-174)
    lmm_base_anl <- base_with_post %>%
      dplyr::mutate(
        AVISIT  = paste0("Baseline (", nxtcm, ")"),
        AVISITN = 9900 + 10 * idx + 1,
        BASE    = .data$AVAL,
        CHG     = 0
      )

    # =========================================================================
    # Phase 4c: Merge and Derive Post-Baseline Records (SAS lines 177-194)
    # Only keep post-baseline records with matching baseline
    # =========================================================================

    # Prepare baseline key for joining BASE to post-baseline records
    # SAS: merge lmm_post_anl (in=in_post) lmm_base_anl (in=in_base keep=grpvars base)
    base_for_join <- lmm_base_anl %>%
      dplyr::select(dplyr::all_of(c(grpvars, "BASE")))

    # Identify post-baseline records without a matching baseline (for warning)
    post_without_base <- dplyr::anti_join(
      lmm_post_anl, lmm_base_anl, by = grpvars
    )

    # SAS line 187: WARNING for omitted post-only records
    if (nrow(post_without_base) > 0L) {
      warning(
        "(DERIVE_LASTMINMAX_MEASURES) Omitting ", nrow(post_without_base),
        " obs without a baseline measure",
        call. = FALSE
      )
    }

    # Join BASE onto post-baseline records and derive columns (SAS lines 191-193)
    lmm_post_anl <- lmm_post_anl %>%
      dplyr::inner_join(base_for_join, by = grpvars) %>%
      dplyr::mutate(
        AVISIT  = paste0("Post-baseline (", nxtcm, ")"),
        AVISITN = 9900 + 10 * idx + 2,
        CHG     = .data$AVAL - .data$BASE
      )

    # =========================================================================
    # Phase 4d: Append to Output and Add Flag (SAS lines 196-208)
    # =========================================================================

    # Add flag variable (SAS lines 202-205): attrib &nxtflv length=$1; &nxtflv = 'Y'
    lmm_base_anl[[nxtflv]] <- "Y"
    lmm_post_anl[[nxtflv]] <- "Y"

    # Append baseline and post-baseline records to output
    # dplyr::bind_rows handles column mismatch by filling with NA,
    # which is correct behavior for flag columns from other modes
    dsout <- dplyr::bind_rows(dsout, lmm_base_anl, lmm_post_anl)

    # Set label attribute on the flag variable for ADaM compliance
    # SAS: attrib &nxtflv label="Analysis Record Flag, Change derived from
    #       &nxtcm Baseline to &nxtcm value in this timepoint"
    attr(dsout[[nxtflv]], "label") <- paste0(
      "Analysis Record Flag, Change derived from ", nxtcm,
      " Baseline to ", nxtcm, " value in this timepoint"
    )

  } # end for loop over c_modes

  # ===========================================================================
  # Return Value
  # ===========================================================================

  # cleanup parameter preserved for API parity with SAS macro.
  # In R, temporary objects are automatically garbage collected.
  # The SAS macro calls %util_delete_dsets to remove WORK datasets.
  # This is a no-op in the R functional paradigm.

  return(dsout)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    1. Input `ds` is a data.frame/tibble with properly typed columns
#       (Date columns as R Date objects, not SAS numeric dates).
#       If dates are SAS numeric origins, convert before calling:
#       as.Date(x, origin = "1960-01-01")
#    2. Column names are case-sensitive in R (use uppercase: TRTSDT,
#       ADT, AVAL, AVISIT, AVISITN). SAS is case-insensitive.
#    3. SAS dataset name parameter `ds` replaced with actual
#       data.frame object. R passes objects by value, not by name.
#    4. `dsout` parameter renamed to `dsout_name` and serves only as
#       metadata label. R function returns the result instead of
#       writing to a named SAS dataset in WORK library.
#    5. `cleanup` parameter is a no-op in R (automatic garbage
#       collection replaces SAS %util_delete_dsets).
#    6. SAS `c_modes` is a space-delimited string; R `c_modes` is a
#       character vector — idiomatic for R.
#    7. SAS `grpvars`, `ordvars`, `incl` are space-delimited strings;
#       R equivalents are character vectors.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    1. SAS PROC SQL HAVING clause with ties: SAS may return multiple
#       rows, then PROC SORT NODUPKEY keeps the first by sort order.
#       R slice(1) after arrange() produces equivalent behavior, but
#       verify sort stability with production data.
#    2. Floating-point comparison in HAVING clause: SAS compares
#       8-byte doubles; R also uses doubles but epsilon differences
#       may cause different tie-breaking in edge cases with very
#       close AVAL or ADT values.
#    3. SAS sort is guaranteed stable by key; R dplyr::arrange() is
#       stable within groups but multi-key sort stability should be
#       verified for production datasets.
#
# NO DIRECT R EQUIVALENT:
#    1. SAS `vlength(avisit)` for runtime string length detection — R
#       strings are dynamically sized, so the AVISIT length guard
#       (SAS lines 97-102) is unnecessary. Documented but not
#       replicated.
#    2. SAS `%util_delete_dsets` — R garbage collector handles
#       temporary objects automatically.
#    3. SAS `call symput` for runtime macro variable creation — not
#       needed in R functional paradigm.
#    4. SAS two-level dataset names (e.g., WORK.ADVS) — R operates
#       on in-memory objects directly.
#
# PACKAGE SELECTION RATIONALE:
#    1. dplyr: Core tidyverse for data manipulation (AAP mandates
#       tidyverse over base R). Replaces DATA steps, PROC SQL,
#       PROC SORT via filter/mutate/group_by/arrange/slice/join.
#    2. rlang: For tidy evaluation (!! operator, sym()) needed for
#       dynamic column references when switching between AVAL and
#       ADT based on change mode.
#    3. cli: For informative, structured error messages during input
#       validation via cli_abort().
#    4. tibble: For initializing the empty output tibble (dsout)
#       ensuring proper tibble return type.
#    5. admiral: Considered for derive_var_extreme_flag() which could
#       replace manual extremum selection. Current implementation
#       preserves exact SAS logic fidelity. A future refactor could
#       adopt admiral patterns for reduced maintenance burden.
#
# OPEN QUESTIONS:
#    1. Should the function accept SAS numeric dates and auto-convert
#       via as.Date(x, origin = "1960-01-01"), or require pre-converted
#       Date objects? Current design requires Date objects.
#    2. When multiple rows tie for extremum within a group, SAS keeps
#       the first by PROC SORT order — verify this matches production
#       expectations with real CDISC datasets.
#    3. Should flag variable labels use haven::labelled() for
#       round-trip XPT compatibility, or is base attr() sufficient?
#    4. SAS PROC SQL HAVING semantics: the HAVING clause filters
#       after GROUP BY but without a proper aggregate — SAS treats
#       this as a non-standard extension. The dplyr equivalent
#       (group_by + filter) is semantically identical.
# ============================================================
