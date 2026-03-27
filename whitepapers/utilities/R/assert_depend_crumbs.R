# ============================================================
# assert_depend_crumbs.R
# Migration of: whitepapers/utilities/assert_depend.sas
# ============================================================
# Purpose : Validates runtime dependencies — R version, package
#           availability, variable existence on datasets, function
#           availability, and object existence in the calling
#           environment. Returns TRUE (PASS) or FALSE (FAIL).
# Author  : Dante Di Tommaso (original SAS macro)
# Ack.    : Inspired by FUTS system from Thotwave
#           http://thotwave.com/resources/futs-framework-unit-testing-sas/
# Migrated: SAS 9.4 -> R 4.3+ (pharmaverse / tidyverse stack)
# Renamed : assert_depend -> assert_depend_crumbs per AAP §0.4.1
# ============================================================

# --- External dependency checks --------------------------------
# Verify cli and rlang are available before defining the function.
# These are required CRAN packages per the migration stack.
if (!requireNamespace("cli", quietly = TRUE)) {
  stop(
    "Package 'cli' (>= 3.6.0) is required for assert_depend_crumbs. ",
    "Install with: install.packages('cli')",
    call. = FALSE
  )
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop(
    "Package 'rlang' (>= 1.1.0) is required for assert_depend_crumbs. ",
    "Install with: install.packages('rlang')",
    call. = FALSE
  )
}

# --- Internal dependency sourcing --------------------------------
# Source the three internal dependency functions if not already
# available in the current session. Mirrors SAS %include for
# %assert_dset_exist, %assert_var_exist, and %assert_macro_exist.
local({
  needed <- c(
    "assert_dset_exist"    = "assert_dset_exist.R",
    "assert_var_exist"     = "assert_var_exist.R",
    "assert_function_exist" = "assert_function_exist.R"
  )

  # Try to determine script directory for relative sourcing
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) NULL
  )

  for (fn_name in names(needed)) {
    if (!exists(fn_name, mode = "function", inherits = TRUE)) {
      candidates <- c(
        if (!is.null(script_dir)) file.path(script_dir, needed[[fn_name]]),
        file.path("whitepapers", "utilities", "R", needed[[fn_name]]),
        file.path(".", needed[[fn_name]])
      )
      for (f in candidates) {
        if (file.exists(f)) {
          source(f, local = FALSE)
          break
        }
      }
    }
  }
})

#' Assert Dependency Crumbs — Runtime Dependency Validation
#'
#' Validates that all runtime dependencies are satisfied before a program
#' proceeds. Checks R version, installed packages, variable existence on
#' datasets, function availability, and object existence in the calling
#' environment. This is the R migration of the SAS macro
#' \code{\%assert_depend(OS=, SASV=, SYSPROD=, vars=, macros=, symbols=)}
#' from \code{whitepapers/utilities/assert_depend.sas}.
#'
#' Renamed from \code{assert_depend} to \code{assert_depend_crumbs} per
#' AAP §0.4.1.
#'
#' @param r_version Character string specifying the minimum required R
#'   version, e.g. \code{"4.3.0"}. Replaces SAS \code{SASV=} parameter.
#'   If \code{NULL} (default), the R version check is skipped.
#' @param packages Character vector of required package names. Replaces
#'   SAS \code{SYSPROD=} parameter (which was TO DO in the SAS source).
#'   If \code{NULL} (default), the package check is skipped.
#' @param vars Named list of variable checks. Each name is a data frame
#'   name (resolved in the calling environment), and each value is a
#'   character vector of required column names. Replaces SAS
#'   \code{vars=} comma-delimited \code{"ds : var1 var2"} syntax.
#'   Example: \code{list("adsl" = c("USUBJID", "SAFFL"))}.
#'   If \code{NULL} (default), the variable check is skipped.
#' @param functions Character vector of required function names.
#'   Replaces SAS \code{macros=} parameter, checking function
#'   availability on the R search path. Uses
#'   \code{\link{assert_function_exist}} internally.
#'   If \code{NULL} (default), the function check is skipped.
#' @param objects Character vector of required object names in the
#'   calling environment. Replaces SAS \code{symbols=} parameter,
#'   checking macro variable existence via \code{\%symexist()}.
#'   If \code{NULL} (default), the object check is skipped.
#' @param os Character vector of acceptable operating system names
#'   (matched against \code{Sys.info()[["sysname"]]}). Replaces SAS
#'   \code{OS=} parameter. OS mismatch produces a WARNING only (not
#'   FAIL), matching SAS behaviour. If \code{NULL} (default), the OS
#'   check is skipped.
#'
#' @return Logical \code{TRUE} (PASS — all dependencies met) or
#'   \code{FALSE} (FAIL — one or more dependencies not met). The result
#'   is returned invisibly.
#'
#' @details
#' The function mirrors the SAS macro's behaviour of collecting ALL
#' failures before returning a final PASS/FAIL status. Individual check
#' failures are logged via \code{cli::cli_warn()} and do not halt
#' execution, allowing all dependency issues to be surfaced in a single
#' run.
#'
#' The SAS macro's six dependency categories map to R as follows:
#' \describe{
#'   \item{OS (SAS \code{&SYSSCP})}{R \code{Sys.info()[["sysname"]]};
#'     WARNING only, not FAIL — R is cross-platform}
#'   \item{SASV (SAS \code{&SYSVLONG})}{R version via
#'     \code{R.version} + \code{utils::compareVersion()}}
#'   \item{SYSPROD}{R \code{rlang::is_installed()} for package checks}
#'   \item{vars}{R \code{assert_dset_exist()} + \code{assert_var_exist()}}
#'   \item{macros}{R \code{assert_function_exist()} for function checks}
#'   \item{symbols}{R \code{exists()} in the calling environment}
#' }
#'
#' @section SAS Lineage:
#' \describe{
#'   \item{Source}{whitepapers/utilities/assert_depend.sas (162 lines)}
#'   \item{Author}{Dante Di Tommaso}
#'   \item{Acknowledgement}{Inspired by FUTS system from Thotwave}
#' }
#'
#' @examples
#' # Check R version and required packages
#' assert_depend_crumbs(r_version = "4.3.0", packages = c("dplyr", "haven"))
#'
#' # Check variables exist on a data frame
#' my_df <- data.frame(USUBJID = 1:3, SAFFL = "Y")
#' assert_depend_crumbs(vars = list("my_df" = c("USUBJID", "SAFFL")))
#'
#' # Check function availability
#' assert_depend_crumbs(functions = c("mean", "sd"))
#'
#' # Check objects exist in the calling environment
#' my_setting <- "PRODUCTION"
#' assert_depend_crumbs(objects = c("my_setting"))
#'
#' # Combined check (all dependency types)
#' assert_depend_crumbs(
#'   r_version = "4.3.0",
#'   packages  = c("dplyr"),
#'   vars      = list("my_df" = c("USUBJID")),
#'   functions = c("mean"),
#'   objects   = c("my_setting")
#' )
#'
#' @export
assert_depend_crumbs <- function(r_version = NULL,
                                  packages  = NULL,
                                  vars      = NULL,
                                  functions = NULL,
                                  objects   = NULL,
                                  os        = NULL) {

  # ==================================================================
  # Capture the calling environment once for all downstream checks.
  # rlang::caller_env() is used instead of parent.frame() per the
  # AAP §0.8.1 tidyverse-over-base-R mandate for environment
  # introspection.
  # ==================================================================
  calling_env <- rlang::caller_env()

  # ------------------------------------------------------------------
  # Input validation — guard against invalid parameter types.
  # Fatal errors use cli::cli_abort() (matching SAS ERROR + STOP).
  # ------------------------------------------------------------------
  if (!is.null(r_version) && (!is.character(r_version) ||
      length(r_version) != 1L || is.na(r_version))) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) r_version must be a single ",
        "character string like \"4.3.0\", not {.cls {class(r_version)}}."
      )),
      call = NULL
    )
  }

  if (!is.null(packages) && !is.character(packages)) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) packages must be a character vector ",
        "of package names, not {.cls {class(packages)}}."
      )),
      call = NULL
    )
  }

  if (!is.null(vars) && (!is.list(vars) || is.null(names(vars)))) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) vars must be a named list where ",
        "names are data frame names and values are character vectors ",
        "of variable names. Example: list(\"adsl\" = c(\"USUBJID\"))."
      )),
      call = NULL
    )
  }

  if (!is.null(functions) && !is.character(functions)) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) functions must be a character vector ",
        "of function names, not {.cls {class(functions)}}."
      )),
      call = NULL
    )
  }

  if (!is.null(objects) && !is.character(objects)) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) objects must be a character vector ",
        "of object names, not {.cls {class(objects)}}."
      )),
      call = NULL
    )
  }

  if (!is.null(os) && !is.character(os)) {
    cli::cli_abort(
      c("x" = paste0(
        "(ASSERT_DEPEND_CRUMBS) os must be a character vector of OS ",
        "names (e.g., c(\"Linux\", \"Windows\")), not ",
        "{.cls {class(os)}}."
      )),
      call = NULL
    )
  }

  # ==================================================================
  # Master OK flag — mirrors SAS %let OK = 1 (line 59)
  # Set to FALSE on any MANDATORY failure; collected across all checks.
  # ==================================================================
  ok <- TRUE

  # ==================================================================
  # STEP 1 — OS Compatibility Check
  # Mirrors SAS lines 61-66: &SYSSCP against OS list.
  # IMPORTANCE: WARNING only — not a FAIL condition. R is cross-
  # platform, so OS mismatch is informational, matching the original
  # SAS %PUT WARNING behaviour.
  # ==================================================================
  if (!is.null(os) && length(os) > 0L) {
    current_os <- Sys.info()[["sysname"]]
    if (!current_os %in% os) {
      cli::cli_warn(
        paste0(
          "(ASSERT_DEPEND_CRUMBS) Program requires OS like (",
          paste(os, collapse = ", "),
          "), but this system is ", current_os,
          ". Let us see what happens."
        )
      )
      # NOTE: OS mismatch is WARNING only — does NOT set ok <- FALSE,
      # matching SAS behaviour where OK is not modified on OS mismatch.
    }
  }

  # ==================================================================
  # STEP 2 — R Version Check
  # Mirrors SAS lines 68-95: SYSVLONG version parsing.
  # Uses utils::compareVersion() for robust semver comparison,
  # replacing the SAS manual major.minor.maintenance parsing.
  # IMPORTANCE: MANDATORY — sets ok <- FALSE if version too low.
  # ==================================================================
  if (!is.null(r_version) && nzchar(r_version)) {

    # Validate r_version contains at least major.minor (like "4.3")
    if (!grepl("\\.", r_version)) {
      cli::cli_warn(
        paste0(
          "(ASSERT_DEPEND_CRUMBS) Specify at least a major and minor ",
          "R version like 4.3. Version \"", r_version,
          "\" is not sufficient."
        )
      )
      ok <- FALSE
    } else {
      # Build the current R version string (e.g., "4.3.3")
      current_version <- paste(R.version$major, R.version$minor, sep = ".")

      if (utils::compareVersion(current_version, r_version) < 0L) {
        ok <- FALSE
        cli::cli_warn(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) Program requires R >= ",
            r_version, ", but current R is ", current_version, "."
          )
        )
      }
    }
  }

  # ==================================================================
  # STEP 3 — Package Availability Check
  # Mirrors SAS lines 97-100: SYSPROD check (was TO DO in SAS source).
  # Now fully implemented using rlang::is_installed() for tidyverse-
  # idiomatic package availability verification.
  # IMPORTANCE: MANDATORY — sets ok <- FALSE if package missing.
  # ==================================================================
  if (!is.null(packages) && length(packages) > 0L) {
    for (pkg in packages) {
      if (!nzchar(pkg)) next
      if (!rlang::is_installed(pkg)) {
        ok <- FALSE
        cli::cli_warn(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) Package '",
            toupper(pkg),
            "' is required but not installed."
          )
        )
      }
    }
  }

  # ==================================================================
  # STEP 4 — Variable Existence on Datasets
  # Mirrors SAS lines 102-128: comma-delimited "ds : var1 var2" loop.
  # R equivalent uses a named list where names are data frame names
  # (resolved in the calling environment) and values are character
  # vectors of required column names.
  # Delegates to assert_dset_exist() (SAS line 109) and
  # assert_var_exist() (SAS line 114) with the caller's environment.
  # IMPORTANCE: MANDATORY — sets ok <- FALSE if any check fails.
  # ==================================================================
  if (!is.null(vars) && length(vars) > 0L) {
    for (df_name in names(vars)) {
      if (!nzchar(df_name)) next

      # Gate: dataset must exist (mirrors SAS line 109)
      dset_ok <- tryCatch(
        assert_dset_exist(df_name, envir = calling_env),
        error = function(e) FALSE
      )

      if (isTRUE(dset_ok)) {
        # Dataset exists — check each required variable
        var_names <- vars[[df_name]]
        if (is.character(var_names) && length(var_names) > 0L) {
          for (var_name in var_names) {
            if (!nzchar(var_name)) next

            # Mirrors SAS line 114: %assert_var_exist(&dnxt, &vnxt)
            var_ok <- tryCatch(
              assert_var_exist(df_name, var_name, envir = calling_env),
              error = function(e) FALSE
            )

            if (!isTRUE(var_ok)) {
              ok <- FALSE
              # assert_var_exist already logged the detailed FAIL
              # message; add the ASSERT_DEPEND_CRUMBS context.
              cli::cli_warn(
                paste0(
                  "(ASSERT_DEPEND_CRUMBS) Data set ",
                  toupper(df_name),
                  " does not contain required variable ",
                  toupper(var_name), "."
                )
              )
            }
          }
        }
      } else {
        # Dataset not available (mirrors SAS lines 122-124)
        ok <- FALSE
        cli::cli_warn(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) Data set ",
            toupper(df_name),
            " is not available."
          )
        )
      }
    }
  }

  # ==================================================================
  # STEP 5 — Function Availability Check
  # Mirrors SAS lines 130-139: space-delimited macro names checked via
  # %assert_macro_exist. In R, uses assert_function_exist() which
  # searches the R search path and installed namespaces.
  # IMPORTANCE: MANDATORY — sets ok <- FALSE if function not found.
  # ==================================================================
  if (!is.null(functions) && length(functions) > 0L) {
    for (fn_name in functions) {
      if (!nzchar(fn_name)) next

      # Mirrors SAS line 134: %assert_macro_exist(&nxt)
      fn_ok <- tryCatch(
        assert_function_exist(fn_name, envir = calling_env),
        error = function(e) FALSE
      )

      if (!isTRUE(fn_ok)) {
        ok <- FALSE
        # assert_function_exist already logged its own FAIL message;
        # add the ASSERT_DEPEND_CRUMBS context.
        cli::cli_warn(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) Function ",
            toupper(fn_name),
            " is required but not found in the search path."
          )
        )
      }
    }
  }

  # ==================================================================
  # STEP 6 — Object (Symbol) Existence Check
  # Mirrors SAS lines 141-151: %symexist(&nxt) for macro variable
  # existence. In R, checks whether objects exist in the calling
  # environment using exists() with the captured rlang::caller_env().
  # IMPORTANCE: MANDATORY — sets ok <- FALSE if object not found.
  # ==================================================================
  if (!is.null(objects) && length(objects) > 0L) {
    for (obj_name in objects) {
      if (!nzchar(obj_name)) next

      if (exists(obj_name, envir = calling_env, inherits = FALSE)) {
        # Mirrors SAS line 149: %put NOTE: PASS, found mac var ...
        # Attempt to display the object value for diagnostic clarity.
        obj_val <- tryCatch(
          {
            val <- get(obj_name, envir = calling_env)
            if (is.atomic(val) && length(val) <= 5L) {
              paste(as.character(val), collapse = " ")
            } else if (is.data.frame(val)) {
              paste0("<data.frame [", nrow(val), " x ", ncol(val), "]>")
            } else {
              paste0("<", class(val)[1L], ">")
            }
          },
          error = function(e) "<unable to display>"
        )
        cli::cli_inform(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) PASS, found object '",
            toupper(obj_name), "' with value \"", obj_val, "\"."
          )
        )
      } else {
        # Mirrors SAS lines 145-148: ERROR for missing symbol
        ok <- FALSE
        cli::cli_warn(
          paste0(
            "(ASSERT_DEPEND_CRUMBS) Symbol (object) ",
            toupper(obj_name),
            " is required but not found."
          )
        )
      }
    }
  }

  # ==================================================================
  # STEP 7 — Write Final Result to Log
  # Mirrors SAS lines 153-161: overall PASS or FAIL message, then
  # return the OK flag.
  # ==================================================================
  if (ok) {
    cli::cli_inform("(ASSERT_DEPEND_CRUMBS) Result is PASS.")
  } else {
    cli::cli_warn(
      paste0(
        "(ASSERT_DEPEND_CRUMBS) Result is FAIL. ",
        "Dependencies for this program not met. Expect problems."
      )
    )
  }

  return(invisible(ok))
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - Renamed from assert_depend to assert_depend_crumbs per AAP §0.4.1
#    - SAS OS check (SYSSCP) -> R Sys.info()["sysname"] check (optional,
#      since R is cross-platform). OS mismatch produces WARNING only, not
#      FAIL — matching SAS behaviour where OK is not modified.
#    - SAS version check (SYSVLONG) -> R version check via R.version +
#      utils::compareVersion(). SAS parsed major.minor.maintenance
#      manually; R uses compareVersion() which handles semver natively.
#    - SAS SYSPROD (TO DO in original SAS) -> R rlang::is_installed() for
#      package availability. This was not implemented in the SAS source
#      (line 99: "TO DO using SYSPROD()") but is now fully implemented
#      in R.
#    - SAS %symexist -> R exists() for object availability in the calling
#      environment, captured via rlang::caller_env().
#    - SAS %assert_macro_exist -> R assert_function_exist() for function
#      availability on the R search path, renamed per AAP §0.4.1.
#    - SAS vars= comma-delimited "ds : var1 var2" -> R named list of
#      variable checks: list("df_name" = c("var1", "var2")).
#    - The function collects ALL failures before returning, matching the
#      SAS macro's behaviour of logging all errors before returning OK.
#    - rlang::caller_env() is used instead of parent.frame() per the
#      AAP §0.8.1 tidyverse-over-base-R mandate.
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None — this is a validation utility with no numerical computation.
#
# NO DIRECT R EQUIVALENT:
#    - SAS &SYSSCP automatic symbol -> R Sys.info()["sysname"]
#    - SAS &SYSVLONG version parsing (major.minor.maintenance with M/D
#      separators) -> R R.version + utils::compareVersion() (much simpler)
#    - SAS SASHELP.VMACRO for macro existence -> R exists() with
#      mode="function" + search path resolution
#    - SAS AUTOCALL path resolution -> R search path / namespace
#      resolution via exists() and getAnywhere()
#    - SAS %GLOBAL/%LOCAL scoping -> R lexical function scoping (no
#      manual global variable management needed)
#    - SAS in-line return via &OK -> R function return value
#
# PACKAGE SELECTION RATIONALE:
#    - cli (>=3.6.0): Informative user-facing messages matching SAS
#      %PUT NOTE/ERROR/WARNING format for PASS/FAIL assertion logging.
#      cli_inform() replaces SAS %PUT NOTE, cli_warn() replaces SAS
#      %PUT ERROR for non-fatal failures, cli_abort() replaces SAS
#      %PUT ERROR for fatal input validation errors.
#    - rlang (>=1.1.0): Tidyverse-idiomatic environment introspection
#      via caller_env() for robust calling-environment capture, and
#      is_installed() for package availability checks. Used per AAP
#      §0.8.1 tidyverse-over-base-R mandate.
#    - utils (base R): compareVersion() for robust R version comparison,
#      replacing SAS manual SYSVLONG parsing logic.
#
# OPEN QUESTIONS:
#    - Should the function stop on first failure or collect all failures?
#      Current: collects all (matching SAS behaviour).
#    - Should envir parameter be exposed for checking non-parent
#      environments? Current: uses rlang::caller_env() internally.
#    - Should the function return a detailed report (list of check
#      results) in addition to the logical TRUE/FALSE?
# ============================================================
