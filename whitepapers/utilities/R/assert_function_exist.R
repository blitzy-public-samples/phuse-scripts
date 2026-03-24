#' Assert Function Exist
#'
#' Checks whether a named R function is available in the current R environment
#' or search path, analogous to the SAS \code{\%assert_macro_exist} macro that
#' searches AUTOCALL paths for a compiled macro.
#'
#' Migrated from: \code{whitepapers/utilities/assert_macro_exist.sas}
#' Renamed per AAP §0.4.1: \code{assert_macro_exist} → \code{assert_function_exist}
#'
#' @description
#' In SAS, \code{\%assert_macro_exist(sym)} resolves the SASAUTOS paths via
#' \code{\%util_resolve_sasautos}, iterates through each path, and checks whether
#' \code{path/sym.sas} exists via \code{\%sysfunc(fileexist())}. In R, function
#' resolution is handled natively by the search path and namespace system, so the
#' equivalent operation is \code{exists(fn_name, mode = "function")} combined with
#' \code{find()} for location reporting and \code{getAnywhere()} for extended
#' namespace search.
#'
#' @param fn_name Character string. The name of the R function to check for
#'   existence. Replaces SAS \code{SYM} positional parameter (a macro name).
#' @param envir Environment in which to begin the search. Defaults to
#'   \code{parent.frame()} so that the caller's environment is checked first,
#'   followed by the entire R search path. This mirrors the SAS AUTOCALL path
#'   resolution order (local scope first, then library paths).
#'
#' @return Logical \code{TRUE} (PASS — function found) or \code{FALSE}
#'   (FAIL — function not found). Replaces SAS inline return of 1 or 0.
#'
#' @details
#' The function performs a three-tier search:
#' \enumerate{
#'   \item \strong{Primary search}: \code{exists(fn_name, mode = "function", envir = envir)}
#'         checks the specified environment and its parent chain (equivalent to the
#'         SAS AUTOCALL path loop).
#'   \item \strong{Location identification}: If found, \code{find(fn_name)} identifies
#'         the package or environment where the function is located (equivalent to
#'         SAS logging the full path of the found \code{.sas} file).
#'   \item \strong{Extended namespace search}: If the primary search fails,
#'         \code{getAnywhere(fn_name)} is used to check installed but not-yet-loaded
#'         package namespaces (extending beyond the SAS AUTOCALL path concept).
#' }
#'
#' @examples
#' # Check for a base R function
#' assert_function_exist("mean")
#' # TRUE — found in package:base
#'
#' # Check for a non-existent function
#' assert_function_exist("nonexistent_xyz_function")
#' # FALSE — not found
#'
#' # Check for a function defined in the calling environment
#' my_custom_fn <- function(x) x + 1
#' assert_function_exist("my_custom_fn")
#' # TRUE — found in calling environment
#'
#' @export
assert_function_exist <- function(fn_name, envir = parent.frame()) {

  # ── Input validation ──────────────────────────────────────────────────────

# Mirrors SAS lines 39, 64: empty SYM produces ERROR and returns 0
  if (is.null(fn_name) || !is.character(fn_name) || length(fn_name) != 1L ||
      is.na(fn_name) || nchar(trimws(fn_name)) == 0L) {
    cli::cli_abort(
      c("x" = paste0(
        "ASSERT_FUNCTION_EXIST: FAIL, please provide a ",
        "non-missing function name."
      )),
      call = NULL
    )
  }

  fn_name <- trimws(fn_name)

  # ── Primary search ────────────────────────────────────────────────────────
  # Mirrors SAS lines 48-58: loop through AUTOCALL paths checking fileexist()
  # In R the search path is traversed automatically by exists()
  found <- exists(fn_name, mode = "function", envir = envir)

  if (found) {
    # ── Location identification ──────────────────────────────────────────────
    # Mirrors SAS line 54: log PASS with full path of found macro
    location <- tryCatch(
      {
        locs <- find(fn_name, mode = "function")
        if (length(locs) > 0L) {
          paste(locs, collapse = ", ")
        } else {
          "calling environment"
        }
      },
      error = function(e) "calling environment"
    )

    cli::cli_inform(
      c("v" = paste0(
        "ASSERT_FUNCTION_EXIST: PASS, found function '",
        toupper(fn_name), "' in ", location, "."
      ))
    )
    return(TRUE)
  }

  # ── Extended namespace search ─────────────────────────────────────────────
  # Extends SAS AUTOCALL concept: check installed (but not loaded) packages
  # This is analogous to searching all available AUTOCALL libraries, not just

  # those currently on the SASAUTOS path
  anywhere_result <- tryCatch(
    {
      ga <- getAnywhere(fn_name)
      # getAnywhere returns an object; check if any matches are functions
      if (length(ga$objs) > 0L) {
        fn_indices <- purrr::map_lgl(ga$objs, is.function)
        if (any(fn_indices)) {
          # Identify which namespace(s) contain the function
          ns_names <- ga$where[fn_indices]
          list(found = TRUE, locations = ns_names)
        } else {
          list(found = FALSE, locations = character(0L))
        }
      } else {
        list(found = FALSE, locations = character(0L))
      }
    },
    error = function(e) list(found = FALSE, locations = character(0L))
  )

  if (anywhere_result$found) {
    ns_info <- paste(anywhere_result$locations, collapse = ", ")
    cli::cli_inform(
      c("v" = paste0(
        "ASSERT_FUNCTION_EXIST: PASS, found function '",
        toupper(fn_name), "' in ", ns_info,
        " (not on current search path)."
      ))
    )
    return(TRUE)
  }

  # ── Not found ─────────────────────────────────────────────────────────────
  # Mirrors SAS lines 60-62: log FAIL when macro not found in any AUTOCALL path
  cli::cli_warn(
    c("!" = paste0(
      "ASSERT_FUNCTION_EXIST: FAIL, unable to find function '",
      toupper(fn_name), "'."
    ))
  )
  return(FALSE)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Renamed from assert_macro_exist to assert_function_exist per AAP §0.4.1
#    - SAS macro AUTOCALL path search → R exists(mode="function") + find()
#    - SAS file-based search (path/sym.sas) → R environment-based search
#    - SAS directory delimiter (\ vs /) not needed in R (R handles paths natively)
#    - SAS %util_resolve_sasautos dependency removed; R manages search paths
#      automatically via the search() path and namespace system
#    - envir defaults to parent.frame() to mirror SAS searching from the
#      caller's scope outward through the AUTOCALL path hierarchy
#    - Extended namespace search via getAnywhere() goes beyond the SAS model
#      by finding functions in installed-but-not-loaded packages, analogous to
#      scanning ALL available AUTOCALL libraries (not just those on SASAUTOS)
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a validation utility producing only PASS/FAIL status
#
# NO DIRECT R EQUIVALENT:
#    - SAS SASAUTOS path resolution → R search() path and loaded namespaces
#    - SAS %sysfunc(fileexist()) for .sas files → R exists() for functions
#      in memory / on the search path
#    - SAS %util_resolve_sasautos → not needed (R manages search paths
#      automatically; there is no separate macro compilation step)
#    - SAS OS-specific directory delimiter logic → not needed (R's file.path()
#      and search path system are OS-agnostic)
#
# PACKAGE SELECTION RATIONALE:
#    - cli (>=3.6.0): Informative user-facing messages matching SAS %PUT
#      NOTE/ERROR/WARNING format for PASS/FAIL assertion logging.
#      cli::cli_inform() replaces SAS %PUT NOTE (PASS messages),
#      cli::cli_warn() replaces SAS %PUT ERROR for non-fatal FAIL messages,
#      cli::cli_abort() replaces SAS %PUT ERROR for fatal input validation.
#    - Base R exists(mode="function"), find(), getAnywhere(): Direct
#      equivalents of checking function availability across R's search path,
#      replacing SAS file-system-based AUTOCALL resolution.
#
# OPEN QUESTIONS:
#    - Should this check loadable packages (not yet loaded) via
#      requireNamespace()? Currently handled by getAnywhere() extended search.
#    - Should envir be limited to .GlobalEnv or search the entire R search
#      path? Current implementation searches from envir upward through the
#      full parent chain, matching SAS AUTOCALL traversal behaviour.
# ============================================================
