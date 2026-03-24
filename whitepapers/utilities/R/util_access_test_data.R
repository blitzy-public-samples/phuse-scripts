#' Access PhUSE CS Test Data from XPT Transport Files
#'
#' Migrated from: whitepapers/utilities/util_access_test_data.sas
#'
#' Accesses PhUSE CS test data from XPT containers, either from a GitHub
#' remote URL or a local folder override. Reads the XPT transport file
#' and returns the specified dataset as a tibble.
#'
#' See explanation in the PhUSE Wiki of this data access pattern:
#'   \url{http://www.phusewiki.org/wiki/index.php?title=WG5_Code_to_Retrieve_CSS/PhUSE_Test_Data}
#'
#' See the PhUSE Repository in GitHub for available PhUSE CS test data sets:
#'   \url{https://github.com/phuse-org/phuse-scripts/tree/master/data/adam/cdisc}
#'
#' @param ds Character string. Name of the PhUSE CS test dataset to retrieve.
#'   Required positional parameter. One-level name (e.g., \code{"adsl"},
#'   \code{"advs"}, \code{"adae"}).
#' @param xport Character string or \code{NULL}. Name of the XPT archive file
#'   if different from \code{ds}. If \code{NULL} or empty string, defaults to
#'   \code{ds}. Example: \code{"advs_container"} when the dataset is stored in
#'   a multi-member transport file.
#' @param folder Character string. Name of the \code{data/adam} subfolder in the
#'   PhUSE GitHub repository from which to retrieve data. Default is
#'   \code{"cdisc"} (single-study data). Case-sensitive, as it appears in the
#'   GitHub URL. Example: \code{"cdisc-split"}.
#' @param local Character string or \code{NULL}. Path to a local folder
#'   containing the PhUSE CS test data sets. When provided, overrides remote
#'   GitHub access. Example: \code{"/data/adam/cdisc"} or
#'   \code{"C:/CSS/phuse-scripts/data/adam/cdisc"}.
#'
#' @return A tibble containing the loaded dataset. Column labels from the XPT
#'   file are preserved as haven label attributes.
#'
#' @details
#' This utility function isolates access to PhUSE CS test data XPT archives.
#' Any future change to the data access interface can be implemented in one
#' place (this function) without requiring changes to other PhUSE CS template
#' programs.
#'
#' When \code{local} is provided, the function reads from the local file system
#' using \code{file.path()} to construct the full path (OS-appropriate
#' separators are handled automatically — no need for the SAS last-character
#' separator check).
#'
#' When \code{local} is \code{NULL} (the default), the function downloads the
#' XPT file directly from the PhUSE GitHub repository via URL.
#'
#' R's \code{haven::read_xpt()} handles both local file paths and URLs
#' transparently, replacing the SAS \code{FILENAME + LIBNAME XPORT} pattern
#' with a single function call.
#'
#' @examples
#' \dontrun{
#' # Remote access (default) -- download ADSL from GitHub
#' adsl <- util_access_test_data("adsl")
#'
#' # Remote access with alternate folder
#' adsl_split <- util_access_test_data("adsl", folder = "cdisc-split")
#'
#' # Remote access with different XPT container name
#' advs <- util_access_test_data("advs", xport = "advs_container")
#'
#' # Local access override
#' adsl <- util_access_test_data("adsl", local = "/data/adam/cdisc")
#' }
#'
#' @export
util_access_test_data <- function(ds, xport = NULL, folder = "cdisc", local = NULL) {

  # --------------------------------------------------------------------------
  # Input Validation
  # --------------------------------------------------------------------------


  # ds is required and must be a non-empty character string
  if (missing(ds) || is.null(ds) || !is.character(ds) ||
      length(ds) != 1L || nchar(trimws(ds)) == 0L) {
    cli::cli_abort(c(
      "UTIL_ACCESS_TEST_DATA: {.arg ds} must be a non-empty character string specifying the dataset name.",
      "i" = "Example: {.code util_access_test_data(\"adsl\")}"
    ))
  }

  # Normalise ds (trim whitespace for safety)
  ds <- trimws(ds)

  # folder must be a non-empty character string
  if (!is.character(folder) || length(folder) != 1L ||
      nchar(trimws(folder)) == 0L) {
    cli::cli_abort(c(
      "UTIL_ACCESS_TEST_DATA: {.arg folder} must be a non-empty character string.",
      "i" = "Default is {.val cdisc}. See {.url https://github.com/phuse-org/phuse-scripts/tree/master/data/adam}"
    ))
  }
  folder <- trimws(folder)

  # --------------------------------------------------------------------------
  # Default xport to ds if not specified

  # Mirrors SAS line 45: %if %length(&xport) = 0 %then %let xport = &ds;
  # --------------------------------------------------------------------------

  if (is.null(xport) || !is.character(xport) ||
      length(xport) != 1L || nchar(trimws(xport)) == 0L) {
    xport <- ds
  } else {
    xport <- trimws(xport)
  }

  # --------------------------------------------------------------------------
  # Construct XPT file path — local override or remote GitHub URL
  # --------------------------------------------------------------------------

  if (!is.null(local) && is.character(local) && length(local) == 1L &&
      nchar(trimws(local)) > 0L) {

    # --- Local path override ---
    # R file.path() handles OS-specific separators automatically.
    # No need for SAS lastchar separator check (SAS lines 48-54).
    local <- trimws(local)
    xpt_path <- file.path(local, paste0(xport, ".xpt"))

    # Validate that the local file exists before attempting to read
    if (!file.exists(xpt_path)) {
      cli::cli_abort(c(
        "UTIL_ACCESS_TEST_DATA: Local XPT file not found.",
        "x" = "Path: {.file {xpt_path}}",
        "i" = "Please verify that the local path and file name are correct."
      ))
    }

    cli::cli_inform(c(
      "i" = "UTIL_ACCESS_TEST_DATA: Reading from local path: {.file {xpt_path}}"
    ))

  } else {

    # --- Remote GitHub URL ---
    # Mirrors SAS lines 58-60:
    #   filename source url "https://github.com/.../data/adam/&folder/&xport..xpt";
    xpt_path <- paste0(
      "https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/",
      folder, "/", xport, ".xpt"
    )

    cli::cli_inform(c(
      "i" = "UTIL_ACCESS_TEST_DATA: Reading from remote URL: {.url {xpt_path}}"
    ))
  }

  # --------------------------------------------------------------------------
  # Read XPT transport file
  # Replaces:
  #   SAS line 62:  libname source xport access=READONLY;
  #   SAS lines 64-66:  data work.&ds; set source.&ds; run;
  # --------------------------------------------------------------------------

  result <- tryCatch(
    {
      haven::read_xpt(xpt_path)
    },
    error = function(e) {
      # Mirrors SAS lines 68-71:
      #   %put ERROR: (UTIL_ACCESS_TEST_DATA) Please confirm that data set
      #   %upcase(&DS) exists in transport file %upcase(&XPORT).;
      cli::cli_abort(c(
        "UTIL_ACCESS_TEST_DATA: Failed to read XPT transport file.",
        "x" = paste0(
          "Please confirm that data set {.val {toupper(ds)}} exists in ",
          "transport file {.val {toupper(xport)}}."
        ),
        "i" = "XPT path: {.file {xpt_path}}",
        "!" = "Underlying error: {e$message}"
      ))
    }
  )

  # --------------------------------------------------------------------------
  # Validate result
  # --------------------------------------------------------------------------

  if (is.null(result) || !is.data.frame(result)) {
    cli::cli_abort(c(
      "UTIL_ACCESS_TEST_DATA: Reading XPT file returned an unexpected result.",
      "x" = "Expected a data frame, got {.cls {class(result)}}."
    ))
  }

  if (nrow(result) == 0L) {
    cli::cli_inform(c(
      "!" = paste0(
        "UTIL_ACCESS_TEST_DATA: Dataset {.val {toupper(ds)}} loaded ",
        "with 0 observations."
      )
    ))
  } else {
    cli::cli_inform(c(
      "v" = paste0(
        "UTIL_ACCESS_TEST_DATA: Successfully loaded {.val {toupper(ds)}} ",
        "({nrow(result)} obs, {ncol(result)} vars)."
      )
    ))
  }

  # Return the tibble — replaces SAS side-effect of creating WORK.&DS
  result
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS FILENAME + LIBNAME XPORT -> R haven::read_xpt() (much simpler).
#    - SAS WORK.&DS side-effect -> R returned tibble (functional, no side
#      effects). Callers assign the return value instead of referencing a
#      WORK dataset.
#    - SAS URL FILENAME -> R haven::read_xpt() supports URLs directly;
#      no intermediate FILENAME statement required.
#    - R file.path() handles OS-specific path separators automatically,
#      making the SAS lastchar separator check (lines 48-54) unnecessary.
#    - The SAS FILENAME/LIBNAME CLEAR statements (lines 73-74) have no R
#      equivalent because haven::read_xpt() does not create persistent
#      resource handles.
# POTENTIAL NUMERICAL DIFFERENCES:
#    - None -- this is a data access utility, not a computational one.
#      All data values are read as-is from the XPT file.
# NO DIRECT R EQUIVALENT:
#    - SAS LIBNAME XPORT with FILENAME -> haven::read_xpt() (direct
#      equivalent that combines both SAS statements into one R call).
#    - SAS multi-member transport files -> haven reads all variables from
#      the XPT file. The ds vs xport distinction is retained for
#      documentation but in practice R reads the single member.
#    - SAS %util_delete_dsets(&ds) on error (line 70) -> not needed in R
#      because no work dataset is created on failure (the function simply
#      raises an error via cli::cli_abort).
# PACKAGE SELECTION RATIONALE:
#    - haven (>=2.5.0): Mandated SAS data I/O package per AAP section 0.6.1.
#      Provides read_xpt() for SAS transport files with label preservation.
#    - cli (>=3.6.0): Informative, structured error messages per AAP
#      section 0.6.1. Provides cli_abort() for errors and cli_inform() for
#      informational messages with rich formatting.
# OPEN QUESTIONS:
#    - Should this function cache downloaded data for repeated access?
#      Consider memoise or local temp-file caching for large datasets.
#    - Should URL access use httr2 or curl for better HTTP error handling,
#      retry logic, and proxy support?
#    - SAS XPORT containers can theoretically hold multiple members;
#      haven::read_xpt() reads the entire file. If ds != xport in the SAS
#      sense (selecting a specific member from a multi-member container),
#      additional handling may be needed (rare in CDISC practice).
# ============================================================
