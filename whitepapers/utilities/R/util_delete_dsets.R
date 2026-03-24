#' Delete Data Frame Objects from an Environment
#'
#' Migrated from SAS macro \code{\%util_delete_dsets(dsets)} in
#' \code{whitepapers/utilities/util_delete_dsets.sas}.
#'
#' This is a PhUSE CS clean-up utility that removes data frame objects
#' from a specified R environment. It supports removing individual named
#' objects or all data frames at once using the \code{_ALL_} keyword,
#' mirroring the SAS PROC DATASETS DELETE behaviour against the WORK library.
#'
#' @param obj_names Character vector of R object names to remove.
#'   \itemize{
#'     \item If \code{NULL} or a single string \code{"_ALL_"}, all data frame
#'           objects in \code{envir} are removed (analogous to SAS
#'           \code{\%util_delete_dsets(_ALL_)}).
#'     \item Otherwise, each element is treated as the name of an object to
#'           remove. Names are matched case-sensitively.
#'   }
#' @param envir The environment from which objects should be removed.
#'   Defaults to \code{parent.frame()} (the calling environment), which
#'   mirrors the SAS WORK library context.
#'
#' @return Invisibly returns a character vector of the names of objects
#'   that were actually removed. Objects that were not found are reported
#'   via \code{cli::cli_inform()} but do not cause an error, matching the
#'   SAS NOTE behaviour of PROC DATASETS DELETE.
#'
#' @details
#' \strong{SAS-to-R mapping}
#' \tabular{ll}{
#'   SAS construct \tab R equivalent \cr
#'   \code{PROC SQL ... dictionary.members ... WORK} \tab \code{ls(envir) + sapply(is.data.frame)} \cr
#'   \code{PROC DATASETS LIBRARY=WORK DELETE} \tab \code{rm(list = ..., envir = envir)} \cr
#'   SAS NOTE for not-found datasets \tab \code{cli::cli_inform()} for each missing name \cr
#'   Freeing WORK space \tab \code{gc()} after removal \cr
#' }
#'
#' @examples
#' \dontrun{
#'   # Remove specific objects from the current environment
#'   my_data_1 <- data.frame(x = 1)
#'   my_data_2 <- data.frame(y = 2)
#'   util_delete_dsets(c("my_data_1", "my_data_2"))
#'
#'   # Remove all data frames from the current environment
#'   util_delete_dsets("_ALL_")
#'
#'   # Remove from a specific environment
#'   e <- new.env(parent = emptyenv())
#'   e$tmp_df <- data.frame(a = 1:3)
#'   util_delete_dsets("_ALL_", envir = e)
#' }
#'
#' @seealso \code{\link[base]{rm}}, \code{\link[base]{gc}}
#' @author Migrated by Blitzy from SAS original by Dante Di Tommaso
#' @export
util_delete_dsets <- function(obj_names = NULL, envir = parent.frame()) {

  # ------------------------------------------------------------------
  # Input validation

  # ------------------------------------------------------------------
  if (!is.environment(envir)) {
    cli::cli_inform(
      paste0(
        "NOTE: UTIL_DELETE_DSETS: ",
        "The supplied 'envir' is not an environment. No action taken."
      )
    )
    return(invisible(character(0L)))
  }

  # Normalise obj_names: accept a single space-delimited string as well as

  # a character vector, matching the SAS behaviour where DSETS is a
  # space-delimited token list.
  if (!is.null(obj_names) && is.character(obj_names) && length(obj_names) == 1L) {
    # A single element that is NOT the _ALL_ keyword may contain multiple
    # space-separated names (SAS convention).
    if (!identical(toupper(trimws(obj_names)), "_ALL_")) {
      obj_names <- strsplit(trimws(obj_names), "\\s+")[[1L]]
    }
  }

  # ------------------------------------------------------------------
  # Determine removal mode: _ALL_ vs specific names
  # ------------------------------------------------------------------
  remove_all <- is.null(obj_names) ||
    (is.character(obj_names) && length(obj_names) == 1L &&
     identical(toupper(trimws(obj_names)), "_ALL_"))

  removed <- character(0L)


  if (remove_all) {
    # ----------------------------------------------------------------
    # _ALL_ mode: remove every data frame object in the environment
    # Mirrors SAS: PROC SQL select unique(memname) from
    #   dictionary.members where memtype='DATA' and libname='WORK'
    # ----------------------------------------------------------------
    all_objs <- ls(envir = envir)

    if (length(all_objs) == 0L) {
      cli::cli_inform(
        "NOTE: UTIL_DELETE_DSETS: No objects found in the target environment."
      )
      gc(verbose = FALSE)
      return(invisible(character(0L)))
    }

    # Identify which objects are data frames (including tibbles).
    # We use inherits() which is safe for subclasses (tbl_df, etc.).
    is_df <- vapply(all_objs, function(nm) {
      obj <- tryCatch(get(nm, envir = envir), error = function(e) NULL)
      if (is.null(obj)) return(FALSE)
      inherits(obj, "data.frame")
    }, logical(1L))

    df_objs <- all_objs[is_df]

    if (length(df_objs) == 0L) {
      cli::cli_inform(
        "NOTE: UTIL_DELETE_DSETS: No data frame objects found in the target environment."
      )
      gc(verbose = FALSE)
      return(invisible(character(0L)))
    }

    # Remove all identified data frames
    rm(list = df_objs, envir = envir)
    removed <- df_objs

    cli::cli_inform(
      paste0(
        "NOTE: UTIL_DELETE_DSETS: Removed ",
        length(removed),
        " data frame object(s): ",
        paste(removed, collapse = ", "),
        "."
      )
    )

  } else {
    # ----------------------------------------------------------------
    # Specific-names mode: attempt to remove each named object
    # Mirrors SAS: PROC DATASETS LIBRARY=WORK DELETE &uds_list;
    # SAS logs a NOTE for each dataset not found.
    # ----------------------------------------------------------------

    # Validate obj_names is character
    if (!is.character(obj_names)) {
      cli::cli_inform(
        paste0(
          "NOTE: UTIL_DELETE_DSETS: ",
          "'obj_names' must be a character vector of object names. No action taken."
        )
      )
      gc(verbose = FALSE)
      return(invisible(character(0L)))
    }

    # Remove empty strings and trim whitespace
    obj_names <- trimws(obj_names)
    obj_names <- obj_names[nchar(obj_names) > 0L]

    if (length(obj_names) == 0L) {
      cli::cli_inform(
        "NOTE: UTIL_DELETE_DSETS: No valid object names provided. No action taken."
      )
      gc(verbose = FALSE)
      return(invisible(character(0L)))
    }

    found_names <- character(0L)
    not_found_names <- character(0L)

    for (nm in obj_names) {
      if (exists(nm, envir = envir, inherits = FALSE)) {
        found_names <- c(found_names, nm)
      } else {
        not_found_names <- c(not_found_names, nm)
        # Match SAS NOTE format:
        # "NOTE: The file WORK.JUNK (memtype=DATA) was not found,
        #  but appears on a DELETE statement."
        cli::cli_inform(
          paste0(
            "NOTE: The object '",
            nm,
            "' was not found in the target environment, ",
            "but appears on a delete request."
          )
        )
      }
    }

    if (length(found_names) > 0L) {
      rm(list = found_names, envir = envir)
      removed <- found_names

      cli::cli_inform(
        paste0(
          "NOTE: UTIL_DELETE_DSETS: Removed ",
          length(removed),
          " object(s): ",
          paste(removed, collapse = ", "),
          "."
        )
      )
    } else {
      cli::cli_inform(
        "NOTE: UTIL_DELETE_DSETS: None of the specified objects were found."
      )
    }
  }

  # ------------------------------------------------------------------
  # Garbage collection — analogous to SAS freeing WORK library space
  # ------------------------------------------------------------------
  gc(verbose = FALSE)

  # ------------------------------------------------------------------
  # Return removed names invisibly (SAS macro has no return value;
  # R idiomatic to return useful information)
  # ------------------------------------------------------------------
  invisible(removed)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS WORK library maps to the R calling environment (parent.frame())
#    - SAS _ALL_ keyword maps to removing all data.frame objects in the
#      target environment (including tibbles via inherits("data.frame"))
#    - gc() is called after rm() to reclaim memory, analogous to SAS
#      freeing WORK library space after PROC DATASETS DELETE
#    - A single space-delimited string of names is split into individual
#      names, matching SAS macro token list behaviour
#    - In specific-names mode, ALL objects with the given names are removed
#      regardless of type (matching SAS PROC DATASETS DELETE which removes
#      the named member without type filtering). In _ALL_ mode, only
#      data frames are removed (matching SAS dictionary.members WHERE
#      memtype='DATA').
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None. This is a cleanup utility with no numerical computation.
#
# NO DIRECT R EQUIVALENT:
#    - SAS PROC DATASETS LIBRARY=WORK DELETE with NOTE logging
#      -> R rm() + exists() check + cli::cli_inform()
#    - SAS dictionary.members for WORK listing
#      -> R ls(envir) + vapply(inherits, "data.frame")
#    - SAS PROC SQL INTO :uds_list SEPARATED BY ' '
#      -> R character vector from ls()
#
# PACKAGE SELECTION RATIONALE:
#    - Base R rm()/gc()/ls()/exists() are the correct tools for object
#      cleanup — no tidyverse wrapper needed for environment operations
#    - cli: For informative user-facing messages matching the SAS log
#      NOTE format produced by PROC DATASETS DELETE
#
# OPEN QUESTIONS:
#    - Should envir default to .GlobalEnv instead of parent.frame()?
#      Current choice mirrors SAS WORK = calling context.
#    - Should this support removing non-data-frame objects (lists, vectors)
#      in _ALL_ mode? Current implementation filters to data frames only
#      for _ALL_, matching SAS memtype=DATA filter.
#    - SAS comments (lines 13-15) describe special handling when a dataset
#      literally named "_ALL_" exists in WORK, but the SAS code does not
#      implement this distinction. The R migration follows the SAS code
#      behaviour (not the aspirational comment).
# ============================================================
