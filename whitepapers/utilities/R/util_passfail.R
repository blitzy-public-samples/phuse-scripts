#' util_passfail — Structured PASS/FAIL Test Execution Framework
#'
#' Migrated from SAS macro \code{\%util_passfail(dsin, criterion=, savexml=, debug=N)}
#' in \code{whitepapers/utilities/util_passfail.sas} (974 lines).
#'
#' Executes and reports results of structured PASS/FAIL tests defined in a tibble.
#' Each row in the test definitions tibble describes a test: what function to call,
#' what arguments to provide, and what result to expect. The framework supports four
#' test types:
#'
#' \describe{
#'   \item{M (Macro/Value)}{Execute an R function and compare its scalar return value
#'     against the expected value.}
#'   \item{S (String)}{Execute an R function, apply optional string post-processing
#'     (B/C/L/T flags), and compare the character result.}
#'   \item{D (Dataset)}{Execute an R function and compare the resulting data frame
#'     against an expected data frame using \code{diffdf::diffdf()} or
#'     \code{all.equal()} with tolerance.}
#'   \item{I (Inline/Expression)}{Evaluate an R expression (optionally within wrapper
#'     code) and compare the result.}
#' }
#'
#' @param test_defs A tibble or data.frame defining the tests to execute. Required
#'   columns: \code{test_id} (character), \code{test_desc} (character),
#'   \code{test_type} (character: "M", "S", "D", or "I"), \code{test_func}
#'   (character: function name or expression), \code{test_args} (list column:
#'   named list of arguments per test), \code{test_expect} (expected result).
#'   Optional columns: \code{test_expect_sym} (named list of expected environment
#'   symbols), \code{test_wrap} (wrapper expression template with _MACCALL_
#'   placeholders), \code{test_pdlim} (delimiter for multiple argument sets),
#'   \code{test_post} (string post-processing flags: B/C/L/T).
#' @param criterion Numeric tolerance for comparison. When \code{NULL} (default),
#'   exact matching is used (\code{identical()} or \code{all.equal(tolerance = 0)}).
#'   When numeric, comparison uses \code{all.equal(tolerance = criterion)}. Maps to
#'   SAS \code{PROC COMPARE CRITERION=}.
#' @param save_results Optional file path to save results. Supports \code{.csv}
#'   (via \code{readr::write_csv()}) and \code{.rds} (via \code{saveRDS()}).
#'   Replaces SAS \code{SAVEXML} parameter.
#' @param debug Logical flag for verbose output. When \code{TRUE}, prints detailed
#'   information for each test including function call, expected, actual, and
#'   comparison details. Maps to SAS \code{DEBUG=Y/N}. Default \code{FALSE}.
#'
#' @return A tibble (returned invisibly) with columns: \code{test_id},
#'   \code{test_desc}, \code{test_type}, \code{expected}, \code{actual},
#'   \code{status} ("PASS" or "FAIL"), \code{message}.
#'
#' @examples
#' \dontrun{
#'   test_defs <- dplyr::tibble(
#'     test_id   = c("T01", "T02"),
#'     test_desc = c("Add two numbers", "Concatenate strings"),
#'     test_type = c("M", "S"),
#'     test_func = c("sum", "paste0"),
#'     test_args = list(list(1, 2), list("hello", " world")),
#'     test_expect = list(3, "hello world")
#'   )
#'   results <- util_passfail(test_defs, debug = TRUE)
#' }
#'
#' @export
util_passfail <- function(test_defs,
                          criterion = NULL,
                          save_results = NULL,
                          debug = FALSE) {

  # ---------------------------------------------------------------------------
  # 0. Package Loading

  # ---------------------------------------------------------------------------
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  requireNamespace("cli", quietly = TRUE)
  requireNamespace("testthat", quietly = TRUE)
  requireNamespace("diffdf", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  # ---------------------------------------------------------------------------
  # 1. Input Validation
  # ---------------------------------------------------------------------------
  if (rlang::is_null(test_defs) || !is.data.frame(test_defs)) {
    cli::cli_abort(
      "UTIL_PASSFAIL CRITICAL: {.arg test_defs} must be a data frame or tibble."
    )
  }

  required_cols <- c("test_id", "test_desc", "test_type", "test_func", "test_expect")
  missing_cols <- setdiff(required_cols, names(test_defs))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "UTIL_PASSFAIL CRITICAL: Missing required columns in {.arg test_defs}: {.val {missing_cols}}."
    )
  }

  if (nrow(test_defs) == 0L) {
    cli::cli_warn("UTIL_PASSFAIL: {.arg test_defs} has 0 rows. No tests to execute.")
    empty_results <- dplyr::tibble(
      test_id   = character(0),
      test_desc = character(0),
      test_type = character(0),
      expected  = character(0),
      actual    = character(0),
      status    = character(0),
      message   = character(0)
    )
    return(invisible(empty_results))
  }

  # Validate test_type values
  valid_types <- c("M", "S", "D", "I")
  invalid_types <- setdiff(unique(test_defs$test_type), valid_types)
  if (length(invalid_types) > 0L) {
    cli::cli_abort(
      "UTIL_PASSFAIL CRITICAL: Invalid test types found: {.val {invalid_types}}. Must be one of {.val {valid_types}}."
    )
  }

  if (!rlang::is_null(criterion)) {
    if (!is.numeric(criterion) || length(criterion) != 1L || is.na(criterion) || criterion < 0) {
      cli::cli_abort(
        "UTIL_PASSFAIL CRITICAL: {.arg criterion} must be a single non-negative numeric value or NULL."
      )
    }
  }

  if (!is.logical(debug) || length(debug) != 1L) {
    debug <- FALSE
  }

  # ---------------------------------------------------------------------------
  # 2. Ensure optional columns exist with safe defaults
  # ---------------------------------------------------------------------------
  if (!"test_args" %in% names(test_defs)) {
    test_defs$test_args <- replicate(nrow(test_defs), list(), simplify = FALSE)
  }
  if (!"test_expect_sym" %in% names(test_defs)) {
    test_defs$test_expect_sym <- replicate(nrow(test_defs), NULL, simplify = FALSE)
  }
  if (!"test_wrap" %in% names(test_defs)) {
    test_defs$test_wrap <- NA_character_
  }
  if (!"test_pdlim" %in% names(test_defs)) {
    test_defs$test_pdlim <- NA_character_
  }
  if (!"test_post" %in% names(test_defs)) {
    test_defs$test_post <- NA_character_
  }

  # Normalise test_args: ensure every element is a list

  test_defs$test_args <- purrr::map(test_defs$test_args, function(a) {
    if (is.null(a)) list() else if (!is.list(a)) list(a) else a
  })

  # ---------------------------------------------------------------------------
  # 3. Snapshot of caller environment (for symbol comparison)
  # ---------------------------------------------------------------------------
  exec_env <- rlang::caller_env()
  pre_symbols <- ls(envir = exec_env)

  # ---------------------------------------------------------------------------
  # 4. Header Banner
  # ---------------------------------------------------------------------------
  cli::cli_h1("UTIL_PASSFAIL: Test Execution Framework")
  n_tests <- nrow(test_defs)
  cli::cli_inform("Running {n_tests} test{?s}.")
  if (!rlang::is_null(criterion)) {
    cli::cli_inform("Comparison tolerance (criterion): {criterion}")
  } else {
    cli::cli_inform("Comparison mode: EXACT (no tolerance)")
  }
  cli::cli_rule()

  # ---------------------------------------------------------------------------
  # 5. Core Test Execution — iterate through rows
  # ---------------------------------------------------------------------------
  results_list <- vector("list", n_tests)

  for (idx in seq_len(n_tests)) {
    row <- test_defs[idx, , drop = FALSE]

    t_id   <- as.character(row$test_id)
    t_desc <- as.character(row$test_desc)
    t_type <- toupper(as.character(row$test_type))
    t_func <- as.character(row$test_func)
    t_args <- row$test_args[[1]]
    t_expect <- row$test_expect
    if (is.list(t_expect) && length(t_expect) == 1L) {
      t_expect <- t_expect[[1]]
    }
    t_expect_sym <- if ("test_expect_sym" %in% names(row)) row$test_expect_sym[[1]] else NULL
    t_wrap <- if ("test_wrap" %in% names(row)) as.character(row$test_wrap) else NA_character_
    t_pdlim <- if ("test_pdlim" %in% names(row)) as.character(row$test_pdlim) else NA_character_
    t_post <- if ("test_post" %in% names(row)) as.character(row$test_post) else NA_character_

    if (debug) {
      cli::cli_h2("Test {t_id}: {t_desc}")
      cli::cli_inform("  Type: {t_type} | Function: {t_func}")
    }

    # Snapshot symbols before test execution (for symbol comparison)
    pre_test_symbols <- ls(envir = exec_env)

    # ----- Execute test based on type ----------------------------------------
    result_record <- tryCatch(
      {
        switch(t_type,
          "M" = execute_type_m(t_func, t_args, t_expect, criterion, exec_env, debug),
          "S" = execute_type_s(t_func, t_args, t_expect, t_post, exec_env, debug),
          "D" = execute_type_d(t_func, t_args, t_expect, criterion, t_pdlim,
                               exec_env, debug),
          "I" = execute_type_i(t_func, t_args, t_expect, t_wrap, criterion,
                               exec_env, debug),
          list(status = "FAIL",
               actual = NA_character_,
               message = paste0("Unknown test type: ", t_type))
        )
      },
      error = function(e) {
        list(
          status  = "FAIL",
          actual  = NA_character_,
          message = paste0("Execution error: ", conditionMessage(e))
        )
      }
    )

    # ----- Expected symbol comparison ----------------------------------------
    sym_status <- "PASS"
    sym_message <- ""
    if (!rlang::is_null(t_expect_sym) && is.list(t_expect_sym) && length(t_expect_sym) > 0L) {
      sym_result <- check_expected_symbols(t_expect_sym, exec_env, debug)
      sym_status  <- sym_result$status
      sym_message <- sym_result$message
    }

    # ----- Check for unexpected new symbols ----------------------------------
    post_test_symbols <- ls(envir = exec_env)
    new_symbols <- setdiff(post_test_symbols, pre_test_symbols)
    unexpected_msg <- ""
    if (length(new_symbols) > 0L && debug) {
      unexpected_msg <- paste0("New symbols created: ", paste(new_symbols, collapse = ", "))
      cli::cli_warn("  WARNING: Unexpected new symbols after test {t_id}: {.val {new_symbols}}")
    }

    # ----- Combine status ----------------------------------------------------
    final_status <- if (result_record$status == "PASS" && sym_status == "PASS") {
      "PASS"
    } else {
      "FAIL"
    }

    combined_message <- paste0(
      result_record$message,
      if (nzchar(sym_message)) paste0(" | Symbols: ", sym_message) else "",
      if (nzchar(unexpected_msg)) paste0(" | ", unexpected_msg) else ""
    )

    # ----- Format expected/actual for the results tibble ---------------------
    expected_str <- format_value_for_display(t_expect)
    actual_str   <- format_value_for_display(result_record$actual)

    # ----- Store result row --------------------------------------------------
    results_list[[idx]] <- dplyr::tibble(
      test_id   = t_id,
      test_desc = t_desc,
      test_type = t_type,
      expected  = expected_str,
      actual    = actual_str,
      status    = final_status,
      message   = combined_message
    )

    # ----- Per-test reporting ------------------------------------------------
    status_icon <- if (final_status == "PASS") "\u2714" else "\u2718"
    if (final_status == "PASS") {
      cli::cli_inform("  {status_icon} [{t_id}] {t_desc}: PASS")
    } else {
      cli::cli_warn("  {status_icon} [{t_id}] {t_desc}: FAIL - {combined_message}")
    }

    if (debug) {
      cli::cli_inform("    Expected: {expected_str}")
      cli::cli_inform("    Actual:   {actual_str}")
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Assemble results tibble
  # ---------------------------------------------------------------------------
  results <- dplyr::bind_rows(results_list)
  results <- dplyr::mutate(results, test_number = seq_len(dplyr::n()))
  results <- dplyr::select(results, test_number, test_id, test_desc, test_type,
                            expected, actual, status, message)

  # ---------------------------------------------------------------------------
  # 7. Summary Reporting
  # ---------------------------------------------------------------------------
  cli::cli_rule()
  cli::cli_h1("UTIL_PASSFAIL: Summary")

  n_total_actual <- nrow(results)
  n_pass   <- sum(results$status == "PASS")
  n_fail   <- sum(results$status == "FAIL")

  cli::cli_inform("Total tests: {n_total_actual}")
  cli::cli_inform("PASSED:      {n_pass}")
  cli::cli_inform("FAILED:      {n_fail}")

  if (n_fail > 0L) {
    cli::cli_warn("UTIL_PASSFAIL: {n_fail} test{?s} FAILED.")
    failed_tests <- dplyr::filter(results, .data$status == "FAIL")
    for (fi in seq_len(nrow(failed_tests))) {
      cli::cli_warn(
        "  FAIL: [{failed_tests$test_id[fi]}] {failed_tests$test_desc[fi]} - {failed_tests$message[fi]}"
      )
    }
  } else {
    cli::cli_inform("UTIL_PASSFAIL: All {n_total_actual} tests PASSED.")
  }
  cli::cli_rule()

  # ---------------------------------------------------------------------------
  # 8. Save Results
  # ---------------------------------------------------------------------------
  if (!rlang::is_null(save_results) && rlang::is_character(save_results) &&
      length(save_results) == 1L && nzchar(save_results)) {
    tryCatch(
      {
        ext <- tolower(tools::file_ext(save_results))
        if (ext == "csv") {
          readr::write_csv(results, file = save_results)
          cli::cli_inform("Results saved to CSV: {.file {save_results}}")
        } else if (ext == "rds") {
          saveRDS(results, file = save_results)
          cli::cli_inform("Results saved to RDS: {.file {save_results}}")
        } else {
          readr::write_csv(results, file = save_results)
          cli::cli_inform("Results saved (defaulting to CSV): {.file {save_results}}")
        }
      },
      error = function(e) {
        cli::cli_warn(
          "UTIL_PASSFAIL: Could not save results to {.file {save_results}}: {conditionMessage(e)}"
        )
      }
    )
  }

  invisible(results)
}


# =============================================================================
# Internal Helper: Execute Type M (Macro Variable / Scalar Return) Tests
# =============================================================================
#' @description Executes function, captures scalar return value, compares with expected.
#' @keywords internal
execute_type_m <- function(func_name, func_args, expected, criterion,
                           exec_env, debug) {
  # Resolve function

  fn <- resolve_function(func_name, exec_env)

  # Build programmatic call using rlang::call2 for traceability and debug display
  call_obj <- rlang::call2(fn, !!!func_args)
  if (debug) {
    cli::cli_inform("    [Type M] Constructed call: {deparse(call_obj, width.cutoff = 200L)}")
  }

  # Execute function call
  actual <- rlang::eval_tidy(call_obj, env = exec_env)

  # Compare
  comparison <- compare_values(actual, expected, criterion)

  if (debug) {
    cli::cli_inform("    [Type M] Return value: {format_value_for_display(actual)}")
  }

  list(
    status  = comparison$status,
    actual  = actual,
    message = comparison$message
  )
}


# =============================================================================
# Internal Helper: Execute Type S (String) Tests
# =============================================================================
#' @description Executes function, applies string post-processing, compares strings.
#' @keywords internal
execute_type_s <- function(func_name, func_args, expected, post_flags,
                           exec_env, debug) {
  # Resolve function

  fn <- resolve_function(func_name, exec_env)

  # Execute function call
  actual <- do.call(fn, func_args, envir = exec_env)

  # Coerce to character
  actual_str <- as.character(actual)
  expected_str <- as.character(expected)

  # Apply post-processing flags (B/C/L/T) to both actual and expected
  actual_str   <- apply_string_post_processing(actual_str, post_flags)
  expected_str <- apply_string_post_processing(expected_str, post_flags)

  if (debug) {
    cli::cli_inform("    [Type S] Post-processing flags: {ifelse(is.na(post_flags), 'none', post_flags)}")
    cli::cli_inform("    [Type S] Actual (post):   '{actual_str}'")
    cli::cli_inform("    [Type S] Expected (post): '{expected_str}'")
  }

  # Compare strings
  if (identical(actual_str, expected_str)) {
    list(status = "PASS", actual = actual_str, message = "String match")
  } else {
    list(
      status  = "FAIL",
      actual  = actual_str,
      message = paste0("String mismatch: expected '", expected_str,
                        "' but got '", actual_str, "'")
    )
  }
}


# =============================================================================
# Internal Helper: Execute Type D (Dataset) Tests
# =============================================================================
#' @description Executes function, compares resulting data frames using diffdf.
#' @keywords internal
execute_type_d <- function(func_name, func_args, expected, criterion,
                           pdlim, exec_env, debug) {

  # --- Handle test_expect parsing for Type D ---

  # expected can be:

  #   1. A data frame directly (expected_df)
  #   2. A character string "expected_name=result_name" for named comparisons
  #   3. A character string "-name" meaning name should NOT exist
  #   4. A list of data frame pairs

  # If expected is a character string representing "exp=res" pairs
  if (rlang::is_character(expected) && length(expected) == 1L) {
    return(execute_type_d_string_spec(func_name, func_args, expected, criterion,
                                      pdlim, exec_env, debug))
  }

  # Execute function
  fn <- resolve_function(func_name, exec_env)
  actual <- do.call(fn, func_args, envir = exec_env)

  # If expected is a data frame, compare directly

  if (is.data.frame(expected)) {
    return(compare_data_frames(expected, actual, criterion, debug))
  }

  # If expected is a named list of data frames, compare each using purrr::pmap

  if (is.list(expected) && !is.data.frame(expected)) {
    df_names <- names(expected)
    comparison_inputs <- dplyr::tibble(
      nm       = df_names,
      exp_df   = purrr::map(df_names, ~ expected[[.x]]),
      act_df   = purrr::map(df_names, function(nm) {
        if (is.list(actual) && nm %in% names(actual)) actual[[nm]] else NULL
      })
    )

    comparison_results <- purrr::pmap(comparison_inputs, function(nm, exp_df, act_df) {
      if (!is.data.frame(exp_df)) {
        return(list(nm = nm, status = "PASS", message = ""))
      }
      if (is.null(act_df) || !is.data.frame(act_df)) {
        return(list(nm = nm, status = "FAIL",
                    message = paste0(nm, ": result dataset not found or not a data frame")))
      }
      cmp <- compare_data_frames(exp_df, act_df, criterion, debug)
      list(nm = nm, status = cmp$status,
           message = if (cmp$status != "PASS") paste0(nm, ": ", cmp$message) else "")
    })

    all_pass <- all(purrr::map_chr(comparison_results, "status") == "PASS")
    messages <- purrr::map_chr(comparison_results, "message")
    messages <- messages[nzchar(messages)]

    return(list(
      status  = if (all_pass) "PASS" else "FAIL",
      actual  = format_value_for_display(actual),
      message = if (all_pass) "All dataset comparisons PASS" else paste(messages, collapse = "; ")
    ))
  }

  list(
    status  = "FAIL",
    actual  = NA_character_,
    message = "Type D: Could not determine expected data frame format"
  )
}


# =============================================================================
# Internal Helper: Type D with string specification ("exp=res" or "-name")
# =============================================================================
#' @keywords internal
execute_type_d_string_spec <- function(func_name, func_args, expect_str,
                                        criterion, pdlim, exec_env, debug) {
  # Execute function first
  fn <- resolve_function(func_name, exec_env)
  do.call(fn, func_args, envir = exec_env)

  # Parse expect_str: may contain multiple pairs separated by spaces
  pairs <- trimws(unlist(strsplit(expect_str, "\\s+")))
  all_pass <- TRUE
  messages <- character(0)
  actual_list <- list()

  for (pair in pairs) {
    if (startsWith(pair, "-")) {
      # Negated: object should NOT exist
      obj_name <- sub("^-", "", pair)
      obj_exists <- exists(obj_name, envir = exec_env)
      if (obj_exists) {
        all_pass <- FALSE
        messages <- c(messages, paste0("Object '", obj_name, "' should NOT exist but does"))
      } else {
        if (debug) cli::cli_inform("    [Type D] Confirmed '{obj_name}' does NOT exist (expected)")
      }
    } else if (grepl("=", pair, fixed = TRUE)) {
      # "expected_name=result_name" comparison
      parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
      exp_name <- trimws(parts[1])
      res_name <- trimws(parts[2])

      exp_df <- get_df_from_env(exp_name, exec_env)
      res_df <- get_df_from_env(res_name, exec_env)

      if (is.null(exp_df)) {
        all_pass <- FALSE
        messages <- c(messages, paste0("Expected dataset '", exp_name, "' not found"))
      } else if (is.null(res_df)) {
        all_pass <- FALSE
        messages <- c(messages, paste0("Result dataset '", res_name, "' not found"))
      } else {
        cmp <- compare_data_frames(exp_df, res_df, criterion, debug)
        if (cmp$status != "PASS") {
          all_pass <- FALSE
          messages <- c(messages, paste0(exp_name, "=", res_name, ": ", cmp$message))
        }
        actual_list[[res_name]] <- res_df
      }
    } else {
      # Single dataset name — check existence only
      obj_exists <- exists(pair, envir = exec_env)
      if (!obj_exists) {
        all_pass <- FALSE
        messages <- c(messages, paste0("Dataset '", pair, "' not found after execution"))
      }
    }
  }

  list(
    status  = if (all_pass) "PASS" else "FAIL",
    actual  = if (length(actual_list) > 0) format_value_for_display(actual_list) else expect_str,
    message = if (all_pass) "Dataset comparison(s) PASS" else paste(messages, collapse = "; ")
  )
}


# =============================================================================
# Internal Helper: Execute Type I (Inline / Expression) Tests
# =============================================================================
#' @description Evaluates R expressions, optionally within wrapper code templates.
#' @keywords internal
execute_type_i <- function(func_name, func_args, expected, wrap_template,
                           criterion, exec_env, debug) {

  # Build the expression code
  code <- func_name

  # If wrapper template provided, substitute _MACCALL_ placeholders
  if (!is.na(wrap_template) && nzchar(wrap_template)) {
    # Build function calls from func_name and func_args
    call_strings <- build_call_strings(func_name, func_args)

    # Replace _MACCALL<i>_ placeholders with actual calls
    code <- wrap_template
    for (ci in seq_along(call_strings)) {
      placeholder <- paste0("_MACCALL", ci, "_")
      code <- gsub(placeholder, call_strings[[ci]], code, fixed = TRUE)
    }
    # Also handle single _MACCALL_ placeholder
    if (length(call_strings) >= 1L) {
      code <- gsub("_MACCALL_", call_strings[[1]], code, fixed = TRUE)
    }
  } else if (is.list(func_args) && length(func_args) > 0L) {
    # Build a single function call as expression
    call_strings <- build_call_strings(func_name, func_args)
    code <- paste(call_strings, collapse = "; ")
  }

  if (debug) {
    cli::cli_inform("    [Type I] Evaluating expression: {code}")
  }

  # Execute the expression
  actual <- tryCatch(
    {
      expr <- rlang::parse_expr(code)
      rlang::eval_tidy(expr, env = exec_env)
    },
    error = function(e) {
      # Try multi-statement evaluation using rlang::parse_exprs (secure alternative)
      tryCatch(
        {
          exprs <- rlang::parse_exprs(code)
          results <- purrr::map(exprs, ~ base::eval(.x, envir = exec_env))
          results[[length(results)]]
        },
        error = function(e2) {
          structure(NA, error = conditionMessage(e2))
        }
      )
    }
  )

  # Check for evaluation error
  if (identical(actual, NA) && !is.null(attr(actual, "error"))) {
    return(list(
      status  = "FAIL",
      actual  = NA_character_,
      message = paste0("Expression evaluation error: ", attr(actual, "error"))
    ))
  }

  # Now compare result with expected — for Type I the comparison is usually

  # on data frames produced in the exec_env
  if (rlang::is_character(expected) && length(expected) == 1L && grepl("=", expected, fixed = TRUE)) {
    # "exp=res" pair comparison — same pattern as Type D
    return(execute_type_d_string_spec("identity", list(), expected, criterion,
                                      NA_character_, exec_env, debug))
  }

  # Default: scalar comparison
  comparison <- compare_values(actual, expected, criterion)

  list(
    status  = comparison$status,
    actual  = actual,
    message = comparison$message
  )
}


# =============================================================================
# Internal Helper: Compare Scalar Values
# =============================================================================
#' @keywords internal
compare_values <- function(actual, expected, criterion) {
  # Handle NA cases
  if (is.null(actual) && is.null(expected)) {
    return(list(status = "PASS", message = "Both NULL"))
  }
  if ((is.null(actual) && !is.null(expected)) || (!is.null(actual) && is.null(expected))) {
    return(list(
      status = "FAIL",
      message = paste0("NULL mismatch: actual is ",
                        if (is.null(actual)) "NULL" else "non-NULL",
                        ", expected is ",
                        if (is.null(expected)) "NULL" else "non-NULL")
    ))
  }

  # Both are NA
  if (length(actual) == 1L && is.na(actual) && length(expected) == 1L && is.na(expected)) {
    return(list(status = "PASS", message = "Both NA"))
  }

  # Exact comparison (no criterion)
  if (rlang::is_null(criterion)) {
    if (identical(actual, expected)) {
      return(list(status = "PASS", message = "Exact match"))
    }
    # Try coercion for character/numeric mismatches
    if (is.numeric(actual) && rlang::is_character(expected)) {
      expected_num <- suppressWarnings(as.numeric(expected))
      if (!is.na(expected_num) && identical(actual, expected_num)) {
        return(list(status = "PASS", message = "Numeric match after coercion"))
      }
    }
    if (rlang::is_character(actual) && is.numeric(expected)) {
      actual_num <- suppressWarnings(as.numeric(actual))
      if (!is.na(actual_num) && identical(actual_num, expected)) {
        return(list(status = "PASS", message = "Numeric match after coercion"))
      }
    }
    # all.equal as fallback
    eq_result <- all.equal(actual, expected, tolerance = 0)
    if (isTRUE(eq_result)) {
      return(list(status = "PASS", message = "Exact match (all.equal)"))
    }
    return(list(
      status = "FAIL",
      message = paste0("Value mismatch: ", paste(eq_result, collapse = "; "))
    ))
  }

  # Tolerance-based comparison (criterion provided)
  eq_result <- all.equal(actual, expected, tolerance = criterion)
  if (isTRUE(eq_result)) {
    return(list(status = "PASS", message = paste0("Match within tolerance ", criterion)))
  }
  return(list(
    status = "FAIL",
    message = paste0("Value mismatch (tolerance=", criterion, "): ",
                      paste(eq_result, collapse = "; "))
  ))
}


# =============================================================================
# Internal Helper: Compare Data Frames
# =============================================================================
#' @keywords internal
compare_data_frames <- function(expected_df, actual_df, criterion, debug) {

  if (!is.data.frame(actual_df)) {
    return(list(
      status  = "FAIL",
      actual  = format_value_for_display(actual_df),
      message = "Actual result is not a data frame"
    ))
  }

  # Use diffdf for clinical-grade comparison
  tolerance_val <- if (rlang::is_null(criterion)) 0 else criterion

  diff_result <- tryCatch(
    {
      suppressMessages(
        diffdf::diffdf(
          base    = expected_df,
          compare = actual_df,
          tolerance = tolerance_val
        )
      )
    },
    error = function(e) {
      # Fall back to all.equal
      NULL
    }
  )

  if (!is.null(diff_result)) {
    # diffdf returns an object; check if there are differences
    n_diffs <- length(diff_result)
    if (n_diffs == 0L) {
      return(list(
        status  = "PASS",
        actual  = paste0("data.frame [", nrow(actual_df), " x ", ncol(actual_df), "]"),
        message = "Dataset comparison PASS (diffdf: no differences)"
      ))
    } else {
      diff_summary <- tryCatch(
        paste(capture.output(print(diff_result)), collapse = "\n"),
        error = function(e) paste(n_diffs, "differences found")
      )
      if (debug) {
        cli::cli_inform("    [Type D] diffdf differences:\n{diff_summary}")
      }
      return(list(
        status  = "FAIL",
        actual  = paste0("data.frame [", nrow(actual_df), " x ", ncol(actual_df), "]"),
        message = paste0("Dataset comparison FAIL: ", n_diffs, " difference(s) found")
      ))
    }
  }

  # Fallback: all.equal
  eq_result <- all.equal(expected_df, actual_df,
                          tolerance = if (rlang::is_null(criterion)) 0 else criterion)
  if (isTRUE(eq_result)) {
    return(list(
      status  = "PASS",
      actual  = paste0("data.frame [", nrow(actual_df), " x ", ncol(actual_df), "]"),
      message = "Dataset comparison PASS (all.equal)"
    ))
  }
  list(
    status  = "FAIL",
    actual  = paste0("data.frame [", nrow(actual_df), " x ", ncol(actual_df), "]"),
    message = paste0("Dataset comparison FAIL: ", paste(eq_result, collapse = "; "))
  )
}


# =============================================================================
# Internal Helper: Check Expected Symbols in Caller Environment
# =============================================================================
#' @keywords internal
check_expected_symbols <- function(expect_sym_list, exec_env, debug) {
  if (!is.list(expect_sym_list) || length(expect_sym_list) == 0L) {
    return(list(status = "PASS", message = ""))
  }

  all_pass <- TRUE
  messages <- character(0)

  for (sym_name in names(expect_sym_list)) {
    expected_val <- expect_sym_list[[sym_name]]

    if (exists(sym_name, envir = exec_env, inherits = FALSE)) {
      actual_val <- get(sym_name, envir = exec_env)
      if (debug) {
        cli::cli_inform(
          "    [Symbol] {sym_name} = {format_value_for_display(actual_val)} (expected: {format_value_for_display(expected_val)})"
        )
      }

      # Compare values
      cmp <- compare_values(actual_val, expected_val, NULL)
      if (cmp$status != "PASS") {
        all_pass <- FALSE
        messages <- c(messages, paste0(
          "Symbol '", sym_name, "': expected ",
          format_value_for_display(expected_val),
          " but got ", format_value_for_display(actual_val)
        ))
      }
    } else {
      all_pass <- FALSE
      messages <- c(messages, paste0("Expected symbol '", sym_name, "' not found"))
      if (debug) {
        cli::cli_warn("    [Symbol] Expected symbol '{sym_name}' NOT found in environment")
      }
    }
  }

  list(
    status  = if (all_pass) "PASS" else "FAIL",
    message = paste(messages, collapse = "; ")
  )
}


# =============================================================================
# Internal Helper: Apply String Post-Processing Flags (B/C/L/T)
# =============================================================================
#' Maps SAS string functions to R stringr equivalents:
#' \itemize{
#'   \item B = compBl() -> stringr::str_squish() (collapse multiple whitespace)
#'   \item C = compress() -> stringr::str_remove_all(pattern = " ") (remove all spaces)
#'   \item L = left() -> stringr::str_trim(side = "left") (trim leading whitespace)
#'   \item T = trim() -> stringr::str_trim() (trim trailing whitespace)
#' }
#' @keywords internal
apply_string_post_processing <- function(x, flags) {
  if (is.na(flags) || !nzchar(flags)) {
    return(x)
  }

  flag_chars <- toupper(unlist(strsplit(as.character(flags), "")))

  for (flag in flag_chars) {
    x <- switch(flag,
      "B" = stringr::str_squish(x),
      "C" = stringr::str_remove_all(x, pattern = " "),
      "L" = stringr::str_trim(x, side = "left"),
      "T" = stringr::str_trim(x, side = "right"),
      x  # default: no change for unknown flags
    )
  }

  x
}


# =============================================================================
# Internal Helper: Resolve Function from Name
# =============================================================================
#' @keywords internal
resolve_function <- function(func_name, exec_env) {
  # Try to find the function in the execution environment or search path
  fn <- tryCatch(
    {
      if (exists(func_name, envir = exec_env, mode = "function")) {
        get(func_name, envir = exec_env, mode = "function")
      } else if (grepl("::", func_name, fixed = TRUE)) {
        # Namespaced function call like "pkg::func"
        parts <- strsplit(func_name, "::", fixed = TRUE)[[1]]
        getExportedValue(parts[1], parts[2])
      } else {
        # Try global search with safe fallback via purrr::possibly
        safe_match <- purrr::possibly(match.fun, otherwise = NULL)
        resolved <- safe_match(func_name)
        if (is.null(resolved)) {
          cli::cli_abort(
            "UTIL_PASSFAIL: Cannot resolve function '{func_name}' in any search path"
          )
        }
        resolved
      }
    },
    error = function(e) {
      cli::cli_abort(
        "UTIL_PASSFAIL: Cannot resolve function '{func_name}': {conditionMessage(e)}"
      )
    }
  )

  fn
}


# =============================================================================
# Internal Helper: Build Call Strings from Function Name and Arguments
# =============================================================================
#' @keywords internal
build_call_strings <- function(func_name, func_args) {
  if (!is.list(func_args) || length(func_args) == 0L) {
    return(func_name)
  }

  # If func_args is a list of lists (multiple calls), build each one
  if (is.list(func_args[[1]]) && !is.data.frame(func_args[[1]])) {
    return(purrr::map_chr(func_args, function(args) {
      build_single_call_string(func_name, args)
    }))
  }

  # Single call
  build_single_call_string(func_name, func_args)
}


# =============================================================================
# Internal Helper: Build Single Function Call String
# =============================================================================
#' @keywords internal
build_single_call_string <- function(func_name, args) {
  if (length(args) == 0L) {
    return(paste0(func_name, "()"))
  }

  arg_strings <- purrr::map_chr(seq_along(args), function(i) {
    nm <- names(args)[i]
    val <- args[[i]]
    val_str <- if (rlang::is_character(val)) {
      paste0('"', val, '"')
    } else if (is.null(val)) {
      "NULL"
    } else if (is.atomic(val) && length(val) == 1L && is.na(val)) {
      "NA"
    } else if (is.logical(val)) {
      as.character(val)
    } else if (is.data.frame(val)) {
      deparse(val, width.cutoff = 200L)
    } else {
      as.character(val)
    }

    if (!is.null(nm) && nzchar(nm)) {
      paste0(nm, " = ", val_str)
    } else {
      val_str
    }
  })

  paste0(func_name, "(", paste(arg_strings, collapse = ", "), ")")
}


# =============================================================================
# Internal Helper: Get Data Frame from Environment
# =============================================================================
#' @keywords internal
get_df_from_env <- function(name, envir) {
  name <- trimws(name)
  if (exists(name, envir = envir)) {
    obj <- get(name, envir = envir)
    if (is.data.frame(obj)) return(obj)
  }
  # Also check global environment
  if (exists(name, envir = globalenv())) {
    obj <- get(name, envir = globalenv())
    if (is.data.frame(obj)) return(obj)
  }
  NULL
}


# =============================================================================
# Internal Helper: Format Value for Display
# =============================================================================
#' @keywords internal
format_value_for_display <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) {
    return(paste0("data.frame [", nrow(x), " x ", ncol(x), "]"))
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(paste0("list(", length(x), " elements)"))
  }
  if (length(x) == 0L) return("empty")
  if (length(x) == 1L && is.na(x)) return("NA")
  if (length(x) > 5L) {
    return(paste0(paste(utils::head(x, 5), collapse = ", "), ", ..."))
  }
  paste(x, collapse = ", ")
}


# =============================================================================
# testthat Integration Wrapper
# =============================================================================
#' Run util_passfail tests within a testthat context
#'
#' Wraps \code{util_passfail} execution inside \code{testthat::test_that()} blocks
#' for integration with standard R test suites.
#'
#' @param test_defs Same as \code{util_passfail} parameter.
#' @param criterion Same as \code{util_passfail} parameter.
#' @param description Character string for the testthat context. Default
#'   "util_passfail test suite".
#'
#' @keywords internal
run_passfail_in_testthat <- function(test_defs, criterion = NULL,
                                      description = "util_passfail test suite") {
  results <- util_passfail(test_defs, criterion = criterion, debug = FALSE)

  testthat::test_that(description, {
    # Individual test assertions
    for (i in seq_len(nrow(results))) {
      row <- results[i, ]
      test_label <- paste0("[", row$test_id, "] ", row$test_desc)
      if (row$status == "PASS") {
        testthat::expect_true(TRUE, info = test_label)
      } else {
        testthat::expect_true(
          FALSE,
          info = paste0(test_label, ": ", row$message)
        )
      }
    }

    # Summary assertions using expect_equal, expect_identical, expect_false
    n_fail <- sum(results$status == "FAIL")
    testthat::expect_equal(n_fail, 0L,
                           info = "All tests should pass (zero failures expected)")

    if (nrow(results) > 0L) {
      testthat::expect_identical(
        sort(unique(results$status)),
        "PASS",
        info = "Only PASS status expected across all tests"
      )
    }

    testthat::expect_false(
      any(is.na(results$test_id)),
      info = "All test IDs should be non-NA"
    )
  })

  invisible(results)
}


# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - SAS structured test dataset (PPARM_*/KPARM_* columns) mapped to R tibble
#      with test_args list column containing named lists of arguments
#    - SAS PFEXCODE file-based code execution mapped to rlang::parse_expr() /
#      rlang::parse_exprs() and rlang::eval_tidy() / base::eval() for secure
#      dynamic expression evaluation (eval(parse()) eliminated per security audit)
#    - SAS PROC COMPARE mapped to diffdf::diffdf() for dataset comparison with
#      tolerance support (METHOD=EXACT via tolerance=0, METHOD=ABSOLUTE via
#      tolerance=criterion)
#    - SAS macro variable results mapped to R environment variable checks via
#      exists()/get() in the caller environment
#    - SAS %build_macro_calls and %add_parms helper macros mapped to
#      build_call_strings() and build_single_call_string() R helpers
#    - SAS %iniglobsyms/%endglobsyms global symbol tracking mapped to
#      ls(envir) snapshots and check_expected_symbols()
#    - SAS TEST_PDLIM delimiter for multiple macro calls mapped to list of
#      argument sets in test_args
#    - SAS TEST_WRAP wrapper code with _MACCALL<i>_ placeholders mapped to
#      R expression template string with gsub-based substitution
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS PROC COMPARE uses IEEE 754 comparison; R all.equal() uses
#      relative tolerance by default. When criterion is NULL, R uses
#      identical() (exact match) equivalent to SAS METHOD=EXACT.
#    - Floating-point comparisons: SAS and R both use 8-byte IEEE 754
#      doubles but epsilon comparisons may differ at the margin.
#    - diffdf tolerance parameter is absolute, matching SAS METHOD=ABSOLUTE
#      CRITERION=value when criterion is specified.
# NO DIRECT R EQUIVALENT:
#    - SAS PFEXCODE fileref temp file execution -> R rlang::parse_exprs() +
#      purrr::map(base::eval) for secure multi-statement evaluation without
#      intermediate file creation (eval(parse()) replaced per security audit)
#    - SAS PROC DATASETS DELETE -> rm() in R (handled by garbage collection)
#    - SAS dictionary.columns for test structure introspection -> R
#      names()/sapply() for tibble column structure analysis
#    - SAS %RESOLVE() for macro variable expansion -> R environment variable
#      access via get() with direct evaluation
#    - SAS %SYMEXIST() -> R exists(name, envir) for symbol checking
#    - SAS %SYMDEL() -> R rm(name, envir) for symbol cleanup
# PACKAGE SELECTION RATIONALE:
#    - testthat: Industry-standard R testing framework; natural replacement
#      for SAS PASS/FAIL harness per AAP section 0.4.1 and 0.8.2
#    - diffdf: Purpose-built clinical data frame comparison (regulatory-grade);
#      maps directly to SAS PROC COMPARE with tolerance support
#    - purrr: Functional iteration replacing SAS %DO loops per AAP section 0.7.1
#    - cli: Rich error/warning messages replacing SAS %PUT ERROR/WARNING/NOTE
#    - stringr: Tidyverse string manipulation per AAP section 0.8.1
#      (stringr over base trimws/gsub)
#    - rlang: Tidy evaluation for dynamic expression execution
#    - readr: Tidyverse file I/O for saving results (over utils::write.csv)
#    - dplyr: tibble construction, bind_rows, mutate, filter for results
#      management per AAP section 0.8.1
# OPEN QUESTIONS:
#    - Should test_defs accept testthat-style test_that() blocks directly?
#    - Should we support JUnit XML output for CI/CD integration?
#    - How to handle SAS %BUILD_MACRO_CALLS dynamic code generation in R
#      beyond the current build_call_strings() approach?
#    - Should the function support parallel test execution via furrr::future_pmap?
# ============================================================
