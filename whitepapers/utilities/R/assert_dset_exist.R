#' Assert Dataset Existence
#'
#' Checks whether one or more datasets (data frames or files) exist. Returns
#' TRUE (PASS) only when ALL specified datasets are accessible. This is the R
#' migration of the SAS macro \code{%assert_dset_exist(ds)} from
#' \code{whitepapers/utilities/assert_dset_exist.sas}.
#'
#' @param ds One of the following:
#'   \itemize{
#'     \item A data frame object — always returns TRUE (it exists by being passed).
#'     \item A character string naming an in-memory data frame or a file path.
#'     \item A character vector of names/paths — ALL must exist to return TRUE.
#'   }
#' @param envir Environment in which to look up string names. Defaults to the
#'   calling environment (\code{parent.frame()}).
#'
#' @return Logical \code{TRUE} if ALL datasets exist (PASS), \code{FALSE}
#'   otherwise (FAIL).
#'
#' @details
#' The function mirrors the SAS macro behaviour where every dataset in a
#' space-delimited list must exist for the assertion to pass. In R the input
#' is a character vector instead of a space-delimited string.
#'
#' Detection strategy for each element of \code{ds}:
#' \enumerate{
#'   \item If the element looks like a file path (contains \code{/} or
#'     \code{\\}, or ends with a recognised data-file extension such as
#'     \code{.xpt}, \code{.sas7bdat}, \code{.csv}, \code{.rds}, \code{.rda},
#'     \code{.parquet}), \code{file.exists()} is used.
#'   \item Otherwise the element is treated as the name of an object in
#'     \code{envir}. The object must both exist \emph{and} be a data frame
#'     (or tibble).
#' }
#'
#' @examples
#' # In-memory data frame passed directly
#' assert_dset_exist(mtcars) # TRUE
#'
#' # Character name of an existing data frame
#' my_df <- data.frame(x = 1:3)
#' assert_dset_exist("my_df") # TRUE
#'
#' # Multiple names — all must exist
#' assert_dset_exist(c("my_df", "mtcars")) # TRUE
#'
#' # File path
#' assert_dset_exist("path/to/data.xpt") # depends on file existence
#'
#' @export
assert_dset_exist <- function(ds, envir = parent.frame()) {

  # -------------------------------------------------------------------

  # Guard: NULL or empty input  (SAS line 25: %if %length(&ds) = 0)
  # -------------------------------------------------------------------

  if (is.null(ds)) {
    cli::cli_abort(
      "{.fn ASSERT_DSET_EXIST} Result is {.strong FAIL}. Please specify a dataset name."
    )
  }

  # If a data frame (or tibble) is passed directly, it exists by

  # definition — return TRUE immediately.
  if (is.data.frame(ds)) {
    ds_label <- deparse(substitute(ds))
    cli::cli_inform(
      paste0(
        "(ASSERT_DSET_EXIST) Result is PASS. Dataset '",
        toupper(ds_label),
        "' is accessible."
      )
    )
    return(invisible(TRUE))
  }

  # At this point ds must be a character vector of names/paths.
  if (!is.character(ds)) {
    cli::cli_abort(
      paste0(
        "{.fn ASSERT_DSET_EXIST} Result is {.strong FAIL}. ",
        "Expected a data frame, character name, or character vector of names."
      )
    )
  }

  # Remove any empty-string elements and check that something remains.
  ds <- ds[nzchar(ds)]
  if (length(ds) == 0L) {
    cli::cli_abort(
      "{.fn ASSERT_DSET_EXIST} Result is {.strong FAIL}. Please specify a dataset name."
    )
  }

  # -------------------------------------------------------------------

  # Recognised data-file extensions used to distinguish file paths from

  # in-memory object names.
  # -------------------------------------------------------------------
  file_extensions <- c(
    ".xpt", ".sas7bdat", ".csv", ".rds", ".rda",
    ".rdata", ".parquet", ".feather", ".xlsx", ".xls",
    ".tsv", ".sav", ".dta", ".por"
  )

  #' Helper: determine whether a string looks like a file path.
  is_file_path <- function(name) {
    if (grepl("[/\\\\]", name)) {
      return(TRUE)
    }
    ext <- tolower(tools::file_ext(name))
    if (nzchar(ext) && paste0(".", ext) %in% file_extensions) {
      return(TRUE)
    }
    return(FALSE)
  }

  # -------------------------------------------------------------------

  # Loop through each element — mirrors SAS %do %while loop (lines 28-42)
  # -------------------------------------------------------------------
  ok <- TRUE # Will be set FALSE on first failure (SAS: OK starts 0,
              # becomes 1 on first success, resets to 0 on any failure)

  for (i in seq_along(ds)) {
    name <- ds[[i]]

    if (is_file_path(name)) {
      # ---- File-path check ----
      found <- file.exists(name)
    } else {
      # ---- In-memory object check ----
      found <- exists(name, envir = envir, inherits = FALSE) &&
        is.data.frame(get(name, envir = envir))
    }

    if (found) {
      # SAS line 34: %put NOTE: (ASSERT_DSET_EXIST) Result is PASS ...
      cli::cli_inform(
        paste0(
          "(ASSERT_DSET_EXIST) Result is PASS. Dataset '",
          toupper(name),
          "' is accessible."
        )
      )
    } else {
      # SAS line 38: %put ERROR: (ASSERT_DSET_EXIST) Result is FAIL ...
      ok <- FALSE
      cli::cli_warn(
        paste0(
          "(ASSERT_DSET_EXIST) Result is FAIL. Dataset '",
          toupper(name),
          "' is NOT accessible. Try another data set."
        )
      )
    }
  }

  return(invisible(ok))
}

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS dataset existence (%sysfunc(exist)) mapped to R exists() for
#      in-memory objects and file.exists() for on-disk files.
#    - SAS WORK.name and LIBNAME.name mapped to R in-memory data frame
#      names or file paths.
#    - Function accepts data frames directly (always TRUE), string names,
#      or file paths.
#    - ALL datasets must exist to return TRUE (matching SAS behaviour where
#      OK resets to 0 on any failure).
#    - envir parameter defaults to parent.frame() to check the calling
#      environment, analogous to SAS checking the WORK library.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a validation utility with no numerical computation.
# NO DIRECT R EQUIVALENT:
#    - SAS %sysfunc(exist()) checks SAS data libraries; R uses exists() +
#      file.exists() as a dual-check strategy.
#    - SAS two-level names (libname.memname) have no R analogue; R file
#      paths or in-memory object names are used instead.
# PACKAGE SELECTION RATIONALE:
#    - cli (>=3.6.0): Provides informative user-facing messages matching
#      the SAS %PUT NOTE/ERROR format for PASS/FAIL assertion logging via
#      cli_inform(), cli_warn(), and cli_abort().
#    - Base R exists()/file.exists()/is.data.frame(): Direct object and
#      file existence checks — no additional packages required.
# OPEN QUESTIONS:
#    - Should this function also validate that the object is a data frame
#      (not just that it exists)? Current implementation requires
#      is.data.frame() for in-memory checks, which is stricter than the
#      SAS %sysfunc(exist()) equivalent.
#    - Should it support haven::read_xpt() auto-loading of XPT files that
#      exist on disk but are not yet loaded into memory?
# ============================================================
