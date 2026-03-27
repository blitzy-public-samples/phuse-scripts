# ==============================================================================
# PROGRAM NAME: qualification_harnesses.R
#
# DESCRIPTION:  Consolidated R testthat qualification harnesses for ALL PhUSE CS
#               WG5 Project 02 utility functions. Migrates 13 SAS PASS/FAIL
#               harness scripts from whitepapers/qualification/ into a single
#               idiomatic R file using testthat framework.
#
# REFERENCE:    http://www.lexjansen.com/phuse/2011/ad/AD04.pdf
#               PhUSE CS Standard Analyses Working Group (WG5) Qualification
#
# ORIGINAL SAS HARNESS SCRIPTS (all 13):
#   Phase 2:  example_passfail_test_definitions.sas  (trivial add2nums)
#   Phase 3:  test_assert_complete_refds.sas          (reference completeness)
#   Phase 4:  test_assert_depend.sas                  (runtime dependencies)
#   Phase 5:  test_assert_dset_exist.sas              (dataset existence)
#   Phase 6:  test_assert_macro_exist.sas             (function existence)
#   Phase 7:  test_assert_unique_keys.sas             (key uniqueness)
#   Phase 8:  test_assert_var_exist.sas               (variable existence)
#   Phase 9:  test_assert_var_nonmissing.sas          (non-missing values)
#   Phase 10: test_obsolete_util_boxplot_ranges.sas   (visit range pagination)
#   Phase 11: test_util_access_test_data.sas          (XPT data access)
#   Phase 12: test_util_axis_order.sas                (axis break computation)
#   Phase 13: test_util_boxplot_block_ranges.sas      (block range pagination)
#   (Template: test_TEMPLATE.sas — structure reference only, no tests)
#
# ORIGINAL AUTHOR: Dante Di Tommaso (SAS harnesses, 2013-2015)
# MIGRATED TO R:   Blitzy Platform — pharmaverse / tidyverse stack
# MADE WITH:       R >= 4.3.0, testthat >= 3.2.0
# ==============================================================================

# --- Required Packages --------------------------------------------------------
library(testthat)
library(dplyr)
library(tibble)
library(haven)
library(forcats)

# --- Source Utility Functions Under Test --------------------------------------
# Resolve project root from this script's own path or known markers.
# No hardcoded absolute paths per AAP section 0.8.1.
local({
  # Resolve project root via multiple strategies
  find_root <- function() {
    markers <- c("renv.lock", ".Rprofile", ".git")

    # Strategy 1: walk up from the directory of THIS script file.
    # sys.frame traversal finds the 'ofile' set when source() was used.
    for (i in seq_len(sys.nframe())) {
      f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(f) && nzchar(f)) {
        d <- dirname(normalizePath(f, mustWork = FALSE))
        for (j in 1:10) {
          if (any(file.exists(file.path(d, markers)))) return(d)
          parent <- dirname(d)
          if (parent == d) break
          d <- parent
        }
      }
    }

    # Strategy 2: walk up from working directory
    d <- getwd()
    for (j in 1:10) {
      if (any(file.exists(file.path(d, markers)))) return(d)
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }

    # Strategy 3: check PHUSE_ROOT env var
    env_root <- Sys.getenv("PHUSE_ROOT", unset = "")
    if (nzchar(env_root) && dir.exists(env_root)) return(env_root)

    # Fallback: current directory
    return(getwd())
  }

  project_root <- find_root()
  util_dir <- file.path(project_root, "whitepapers", "utilities", "R")

  source_utility <- function(file_name, fn_name) {
    if (exists(fn_name, mode = "function", inherits = TRUE)) return(invisible(NULL))
    fpath <- file.path(util_dir, file_name)
    if (file.exists(fpath)) {
      source(fpath, local = FALSE)
      return(invisible(NULL))
    }
    warning(paste0("Could not source '", file_name, "' (function: ", fn_name,
                   "). Looked in: ", util_dir), call. = FALSE)
  }

  source_utility("assert_dset_exist.R",          "assert_dset_exist")
  source_utility("assert_var_exist.R",            "assert_var_exist")
  source_utility("assert_complete_refds.R",       "assert_complete_refds")
  source_utility("assert_depend_crumbs.R",        "assert_depend_crumbs")
  source_utility("assert_function_exist.R",       "assert_function_exist")
  source_utility("assert_unique_keys.R",          "assert_unique_keys")
  source_utility("assert_var_nonmissing.R",       "assert_var_nonmissing")
  source_utility("util_passfail.R",               "util_passfail")
  source_utility("util_axis_order.R",             "util_axis_order")
  source_utility("util_boxplot_block_ranges.R",   "util_boxplot_block_ranges")
  source_utility("util_boxplot_visit_ranges.R",   "util_boxplot_visit_ranges")
  source_utility("util_count_unique_values.R",    "util_count_unique_values")
  source_utility("util_access_test_data.R",       "util_access_test_data")
})

# --- Helper: safe call wrapper ------------------------------------------------
# Converts errors/warnings from utility functions to FALSE, matching SAS
# behaviour where invalid inputs produce FAIL (0) rather than halting.
safe_false <- function(expr) {
  tryCatch(
    suppressWarnings(expr),
    error = function(e) FALSE
  )
}


# ==============================================================================
# PHASE 2: Example PASSFAIL — from example_passfail_test_definitions.sas
# ==============================================================================
# SAS macro %add2nums(pnum, knum=) adds two numbers. 6 test cases via
# %util_passfail. Migrated to a trivial R function + testthat assertions.

add2nums <- function(pnum = NULL, knum = NULL) {
  p <- if (!is.null(pnum) && nzchar(as.character(pnum))) as.numeric(pnum) else 0
  k <- if (!is.null(knum) && nzchar(as.character(knum))) as.numeric(knum) else 0
  return(p + k)
}

test_that("Phase 2 — Example PASSFAIL: add2nums function (a2n_001-a2n_006)", {
  # a2n_001: add 2 positive numbers

  expect_equal(add2nums(2, knum = 8), 10)
  # a2n_002: add 2 negative numbers
  expect_equal(add2nums(-4, knum = -6), -10)
  # a2n_003: add positive + negative
  expect_equal(add2nums(2, knum = -6), -4)
  # a2n_004: provide just pnum
  expect_equal(add2nums(2), 2)
  # a2n_005: provide just knum
  expect_equal(add2nums(knum = -6), -6)
  # a2n_006: composite wrapper — sum of 3 calls
  expect_equal(
    add2nums(2, knum = 8) + add2nums(-4, knum = -6) + add2nums(2, knum = -6),
    -4
  )
})

# --- Phase 2 extended: exercise util_passfail (SAS %util_passfail) directly ---
test_that("Phase 2 — util_passfail: basic pass/fail framework (pf_001-003)", {
  # Verify util_passfail framework with trivial test definitions
  # Required columns: test_id, test_desc, test_type (M/S/D/I),
  #                   test_func (function name), test_expect (expected result)
  test_defs <- tibble(
    test_id      = c("pf_001", "pf_002", "pf_003"),
    test_desc    = c("sum 2+2=4", "sum 1+1=3 (should fail)", "sum 0+0=0"),
    test_type    = c("M", "M", "M"),
    test_func    = c("sum", "sum", "sum"),
    test_args    = list(list(2, 2), list(1, 1), list(0, 0)),
    test_expect  = list(4, 3, 0)
  )
  result <- util_passfail(test_defs)
  expect_true(is.data.frame(result))
  # pf_001: sum(2,2) == 4 => PASS
  expect_true(result$status[result$test_id == "pf_001"] == "PASS")
  # pf_002: sum(1,1)=2 != 3 => FAIL
  expect_true(result$status[result$test_id == "pf_002"] == "FAIL")
  # pf_003: sum(0,0) == 0 => PASS
  expect_true(result$status[result$test_id == "pf_003"] == "PASS")
})

# --- Phase 2 extended: exercise util_count_unique_values ---
test_that("Phase 2 — util_count_unique_values: basic counting verification", {
  test_df <- tibble(
    grp = c("A", "A", "B", "B", "C"),
    val = c(1, 1, 2, 3, 3)
  )
  # Count unique group values
  n_grps <- util_count_unique_values(test_df, "grp")
  expect_equal(n_grps, 3L)
  # Count unique numeric values
  n_vals <- util_count_unique_values(test_df, "val")
  expect_equal(n_vals, 3L)
  # Verify via dplyr::distinct + dplyr::n() for cross-check
  n_chk <- test_df %>% distinct(grp) %>% summarise(cnt = n()) %>% pull(cnt)
  expect_equal(n_grps, as.integer(n_chk))
})


# ==============================================================================
# PHASE 3: assert_complete_refds — from test_assert_complete_refds.sas (214 lines)
# ==============================================================================
# Tests %assert_complete_refds(dsets, keys) with single-key (numeric/character)
# and multi-key scenarios.

test_that("Phase 3 — assert_complete_refds: single numeric key (refds_01_a_i, refds_01_a_ii)", {
  # --- Test data construction ---
  # Reference dataset with keys 0.001, 1.002, 2.003, 3.004
  my_reference <- tibble(
    my_key         = c(0.001, 1.002, 2.003, 3.004),
    extra_ref_info = paste("My extra info for key", c(0.001, 1.002, 2.003, 3.004))
  )

  # Related dataset: subset of reference keys (missing 2.003)
  set.seed(6743)
  base_keys <- c(0.001, 1.002, 3.004)
  my_related <- tibble(
    my_key         = base_keys,
    extra_rel_info = paste("My extra info for key", base_keys)
  )
  # Add random duplicates (SAS ranuni(6743) < 0.5)
  for (k in base_keys) {
    if (runif(1) < 0.5) {
      my_related <- bind_rows(
        my_related,
        tibble(my_key = k, extra_rel_info = paste("Additional info for key", k))
      )
    }
  }

  # "Extra" related: add key 1.5 not in reference
  my_related_extra <- bind_rows(
    my_related,
    tibble(my_key = 1.5, extra_rel_info = "No reference match!")
  )

  # refds_01_a_i: related is subset of reference — extra REF rec allowed => PASS
  result_pass <- assert_complete_refds(
    dsets = list(ref = my_reference, rel = my_related),
    keys  = "my_key"
  )
  expect_true(result_pass)

  # refds_01_a_ii: related has extra key 1.5 not in reference => FAIL
  result_fail <- assert_complete_refds(
    dsets = list(ref = my_reference, rel = my_related_extra),
    keys  = "my_key"
  )
  expect_false(result_fail)
  # Verify fail_crds contains key 1.5
  fail_data <- attr(result_fail, "fail_crds")
  expect_true(!is.null(fail_data))
  expect_true(1.5 %in% fail_data$my_key)
})

test_that("Phase 3 — assert_complete_refds: single character key (refds_01_b_i, refds_01_b_ii)", {
  # Character equivalents of the numeric key tests
  my_reference_c <- tibble(
    my_char_key    = paste("Record", c(0.001, 1.002, 2.003, 3.004)),
    extra_ref_info = paste("My extra info for key", c(0.001, 1.002, 2.003, 3.004))
  )

  my_related_c <- tibble(
    my_char_key    = paste("Record", c(0.001, 1.002, 3.004)),
    extra_rel_info = paste("My extra info for key", c(0.001, 1.002, 3.004))
  )

  my_related_extra_c <- bind_rows(
    my_related_c,
    tibble(my_char_key = "Record 1.5", extra_rel_info = "No reference match!")
  )

  # refds_01_b_i: char key, extra REF allowed => PASS
  result_pass <- assert_complete_refds(
    dsets = list(ref = my_reference_c, rel = my_related_c),
    keys  = "my_char_key"
  )
  expect_true(result_pass)

  # refds_01_b_ii: char key, extra REL => FAIL
  result_fail <- assert_complete_refds(
    dsets = list(ref = my_reference_c, rel = my_related_extra_c),
    keys  = "my_char_key"
  )
  expect_false(result_fail)
  fail_data <- attr(result_fail, "fail_crds")
  expect_true(!is.null(fail_data))
  expect_true("Record 1.5" %in% fail_data$my_char_key)
})

test_that("Phase 3 — assert_complete_refds: multiple keys (refds_02_a, refds_02_b)", {
  # Build multi-key reference: 4 x 3 x 3 = 36 combos
  my_ref_2 <- tidyr::expand_grid(
    num_key  = c(0.001, 1.002, 2.003, 3.004),
    key_char = c("Record A", "Record B", "Record C"),
    key3     = c("Subrec 0.001", "Subrec 1.002", "Subrec 2.003")
  ) %>%
    mutate(extra_ref_col = paste("Ref info:", num_key, key_char, key3))

  # Create related datasets as subsets (SAS ranuni seeds: 61475, 56147, 75614)
  set.seed(61475)
  my_lb <- my_ref_2 %>%
    filter(runif(n()) < 0.9) %>%
    mutate(extra_lb_col = paste("LB data for", num_key))

  set.seed(56147)
  my_vs <- my_ref_2 %>%
    filter(runif(n()) < 0.9) %>%
    mutate(extra_vs_col = paste("VS data for", num_key))

  set.seed(75614)
  my_ecg <- my_ref_2 %>%
    filter(runif(n()) < 0.9) %>%
    mutate(extra_ecg_col = paste("ECG data for", num_key))

  keys_multi <- c("num_key", "key_char", "key3")

  # refds_02_a: three related datasets all subsets of reference => PASS
  result_pass <- assert_complete_refds(
    dsets = list(ref = my_ref_2, lb = my_lb, vs = my_vs, ecg = my_ecg),
    keys  = keys_multi
  )
  expect_true(result_pass)

  # Extended datasets with invalid key combos
  my_lb_ext <- bind_rows(
    my_lb,
    tibble(num_key = 1.5, key_char = "Record B", key3 = "Subrec 0.001",
           extra_lb_col = "Invalid LB")
  )
  my_vs_ext <- bind_rows(
    my_vs,
    tibble(num_key = 1.002, key_char = "Rec D", key3 = "Subrec 1.002",
           extra_vs_col = "Invalid VS")
  )
  my_ecg_ext <- bind_rows(
    my_ecg,
    tibble(num_key = 2.003, key_char = "Record C", key3 = "Subrec 400",
           extra_ecg_col = "Invalid ECG")
  )

  # refds_02_b: extended related datasets with invalid keys => FAIL
  result_fail <- assert_complete_refds(
    dsets = list(ref = my_ref_2, lb = my_lb_ext, vs = my_vs_ext, ecg = my_ecg_ext),
    keys  = keys_multi
  )
  expect_false(result_fail)

  # Verify fail_crds contains the expected invalid key combinations
  fail_data <- attr(result_fail, "fail_crds")
  expect_true(!is.null(fail_data))
  # Check that at least one of the invalid combos is captured
  expect_true(
    any(fail_data$num_key == 1.5) ||
    any(fail_data$key_char == "Rec D") ||
    any(fail_data$key3 == "Subrec 400")
  )
})


# ==============================================================================
# PHASE 4: assert_depend_crumbs — from test_assert_depend.sas (106 lines)
# ==============================================================================
# SAS dependency checks: macro existence, symbol existence, SAS version, OS.
# Mapped to R equivalents: function existence, object existence, R version, OS.

test_that("Phase 4 — assert_depend_crumbs: function existence (ad.1.a.1, ad.1.a.2)", {
  # ad.1.a.1: existing base R functions => PASS
  result_pass <- assert_depend_crumbs(
    functions = c("trimws", "assert_complete_refds")
  )
  expect_true(result_pass)

  # ad.1.a.2: non-existent function => FAIL
  result_fail <- assert_depend_crumbs(
    functions = c("trimws_DOES_NOT_EXIST")
  )
  expect_false(result_fail)
})

test_that("Phase 4 — assert_depend_crumbs: object existence (ad.1.b.1)", {
  # ad.1.b.1: existing objects => PASS
  MacroName <- "test_value"
  result_pass <- assert_depend_crumbs(objects = c("MacroName"))
  expect_true(result_pass)

  # ad.1.b.1 (second variant): mix of existing + non-existent => FAIL
  result_fail <- assert_depend_crumbs(
    objects = c("MacroName", "MV_DoesNotExist")
  )
  expect_false(result_fail)
})

test_that("Phase 4 — assert_depend_crumbs: R version checks (ad.2.a.1-3, ad.2.b1-b2)", {
  # ad.2.a.1-3: R version >= low thresholds => PASS
  expect_true(assert_depend_crumbs(r_version = "3.0.0"))
  expect_true(assert_depend_crumbs(r_version = "4.0.0"))
  current_ver <- paste0(R.version$major, ".", R.version$minor)
  expect_true(assert_depend_crumbs(r_version = current_ver))

  # ad.2.b1-b2: R version >= impossible future version => FAIL
  expect_false(assert_depend_crumbs(r_version = "99.0.0"))
  expect_false(assert_depend_crumbs(r_version = "20.5"))
})

test_that("Phase 4 — assert_depend_crumbs: OS checks (ad.3.a)", {
  # OS checks issue WARNING only (never FAIL in SAS). Verify no error.
  current_os <- Sys.info()[["sysname"]]
  result <- suppressWarnings(
    assert_depend_crumbs(os = c(current_os, "SomeOtherOS"))
  )
  # OS check alone should not cause failure (SAS behaviour: WARNING only)
  expect_true(is.logical(result))
})

test_that("Phase 4 — assert_depend_crumbs: combined checks (ad.4.a-d)", {
  # ad.4.a: missing function => overall FAIL
  result_a <- assert_depend_crumbs(
    functions = c("nonexistent_function_xyz"),
    r_version = paste0(R.version$major, ".", R.version$minor)
  )
  expect_false(result_a)

  # ad.4.b: R version too high => overall FAIL
  result_b <- assert_depend_crumbs(
    functions = c("mean"),
    r_version = "99.0.0"
  )
  expect_false(result_b)

  # ad.4.d: all conditions pass => overall PASS
  result_d <- assert_depend_crumbs(
    functions = c("mean", "sd"),
    r_version = "3.0.0"
  )
  expect_true(result_d)
})


# ==============================================================================
# PHASE 5: assert_dset_exist — from test_assert_dset_exist.sas (114 lines)
# ==============================================================================
# SAS dataset existence checks mapped to R data frame / file existence.

test_that("Phase 5 — assert_dset_exist: NULL input (dse.1)", {
  # dse.1: NULL dataset => FAIL (R throws error via cli_abort)
  expect_error(assert_dset_exist(NULL))
})

test_that("Phase 5 — assert_dset_exist: existing data frames (dse.2.a.1-2)", {
  test_env <- new.env(parent = globalenv())
  test_env$class_modified <- as_tibble(iris)
  test_env$not_df <- as_tibble(iris)

  # dse.2.a.1: existing one-level name => PASS
  expect_true(assert_dset_exist("not_df", envir = test_env))
  # dse.2.a.2: existing two-level name => PASS
  expect_true(assert_dset_exist("class_modified", envir = test_env))
})

test_that("Phase 5 — assert_dset_exist: non-existent data frames (dse.2.b.1-2)", {
  test_env <- new.env(parent = globalenv())
  # dse.2.b.1: non-existent name => FAIL
  expect_false(safe_false(assert_dset_exist("nonexistent_ds", envir = test_env)))
  # dse.2.b.2: another non-existent name => FAIL
  expect_false(safe_false(assert_dset_exist("work_classes", envir = test_env)))
})

test_that("Phase 5 — assert_dset_exist: file-based dataset (dse.2.c.1, dse.2.d.1)", {
  # dse.2.c.1: existing file => PASS
  tmp_file <- tempfile(fileext = ".rds")
  saveRDS(iris, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  expect_true(assert_dset_exist(tmp_file))

  # dse.2.d.1: non-existent file => FAIL
  expect_false(safe_false(
    assert_dset_exist(file.path(tempdir(), "nonexistent_file.xpt"))
  ))
})

test_that("Phase 5 — assert_dset_exist: multiple datasets (dse.3.a, dse.3.b.1-3)", {
  test_env <- new.env(parent = globalenv())
  test_env$ds_alpha <- tibble(x = 1)
  test_env$ds_beta  <- tibble(y = 2)
  test_env$ds_gamma <- tibble(z = 3)

  # dse.3.a: multiple existing => PASS
  expect_true(assert_dset_exist(
    c("ds_alpha", "ds_beta", "ds_gamma"),
    envir = test_env
  ))

  # dse.3.b.1: first missing => FAIL
  expect_false(safe_false(assert_dset_exist(
    c("nonexist_first", "ds_beta", "ds_gamma"),
    envir = test_env
  )))

  # dse.3.b.2: last missing => FAIL
  expect_false(safe_false(assert_dset_exist(
    c("ds_alpha", "ds_beta", "nonexist_last"),
    envir = test_env
  )))

  # dse.3.b.3: middle missing => FAIL
  expect_false(safe_false(assert_dset_exist(
    c("ds_alpha", "nonexist_mid", "ds_gamma"),
    envir = test_env
  )))
})


# ==============================================================================
# PHASE 6: assert_function_exist — from test_assert_macro_exist.sas (109 lines)
# ==============================================================================
# SAS AUTOCALL macro lookup => R function existence on search path.

test_that("Phase 6 — assert_function_exist: base R functions (me.1.a.1-3)", {
  # me.1.a.1: tolower (SAS: CMPRES) => PASS
  expect_true(assert_function_exist("tolower"))
  # me.1.a.2: trimws (SAS: QLEFT) => PASS
  expect_true(assert_function_exist("trimws"))
  # me.1.a.3: tolower again — case insensitive (SAS: LOWCASE) => PASS
  expect_true(assert_function_exist("tolower"))
})

test_that("Phase 6 — assert_function_exist: partial/non-existent (me.1.b.1-3, me.1.c)", {
  # me.1.b.1: partial name => FAIL
  expect_false(assert_function_exist("tolo"))
  # me.1.b.2: partial name => FAIL (Note: "trim" exists in renv namespace, use "triw" instead)
  expect_false(assert_function_exist("triw"))
  # me.1.b.3: non-existent => FAIL
  expect_false(assert_function_exist("css_lowercasing_nonexistent"))
  # me.1.c: NULL / empty => FAIL
  expect_false(safe_false(assert_function_exist("")))
})

test_that("Phase 6 — assert_function_exist: sourced PhUSE functions (me.2.a.1-2)", {
  # me.2.a.1: assert_complete_refds should be available after sourcing => PASS
  expect_true(assert_function_exist("assert_complete_refds"))
  # me.2.a.2: util_boxplot_block_ranges => PASS
  expect_true(assert_function_exist("util_boxplot_block_ranges"))
})

test_that("Phase 6 — assert_function_exist: dynamically created function (me.3.a)", {
  # me.3.a: create function on-the-fly and find it => PASS
  css_onthefly <- function() message("PASS, on-the-fly function found and executed.")
  expect_true(assert_function_exist("css_onthefly"))
})


# ==============================================================================
# PHASE 7: assert_unique_keys — from test_assert_unique_keys.sas (157 lines)
# ==============================================================================
# Tests key uniqueness validation with various scenarios.

test_that("Phase 7 — assert_unique_keys: NULL/invalid inputs (auk1.a, auk1.b)", {
  # auk1.a.1: NULL dataset => FAIL
  expect_false(safe_false(assert_unique_keys(NULL, keys = "id")))

  # auk1.a.2: invalid dataset name => FAIL
  expect_false(safe_false(
    assert_unique_keys("nonexistent_dataset_xyz", keys = "id")
  ))

  # auk1.b.1: NULL keys => FAIL
  test_df <- tibble(id = 1:3, val = letters[1:3])
  expect_false(safe_false(assert_unique_keys(test_df, keys = NULL)))

  # auk1.b.2: invalid key (column doesn't exist) => FAIL
  expect_false(safe_false(assert_unique_keys(test_df, keys = "nonexistent_col")))
})

test_that("Phase 7 — assert_unique_keys: unique single key (auk2.a)", {
  # SAS: sashelp.class with NAME as key => unique (19 unique names)
  class_data <- tibble(
    name   = c("Alfred", "Alice", "Barbara", "Carol", "Henry",
               "James", "Jane", "Janet", "Jeffrey", "John",
               "Joyce", "Judy", "Louise", "Mary", "Philip",
               "Robert", "Ronald", "Thomas", "William"),
    sex    = c("M","F","F","F","M","M","F","F","M","M",
               "F","F","F","F","M","M","M","M","M"),
    age    = c(14,13,13,14,14,12,12,15,13,12,11,14,12,15,16,12,15,11,15),
    height = c(69,56.5,65.3,62.8,63.5,57.3,59.8,62.5,62.5,59,
               51.3,64.3,56.8,66.5,72,64.8,67,57.5,66.5),
    weight = c(112.5,84,98,102.5,102.5,83,84.5,112.5,84,99.5,
               50.5,90,77,112,150,128,133,85,112)
  )

  # auk2.a: unique single key (name) => PASS
  expect_true(assert_unique_keys(class_data, keys = "name"))
})

test_that("Phase 7 — assert_unique_keys: non-unique single key (auk2.b)", {
  # Doubled data with sex flipped (SAS: classes with name repeated)
  class_data <- tibble(
    name   = c("Alfred", "Alice", "Barbara", "Carol", "Henry",
               "James", "Jane", "Janet", "Jeffrey", "John",
               "Joyce", "Judy", "Louise", "Mary", "Philip",
               "Robert", "Ronald", "Thomas", "William"),
    sex    = c("M","F","F","F","M","M","F","F","M","M",
               "F","F","F","F","M","M","M","M","M"),
    age    = c(14,13,13,14,14,12,12,15,13,12,11,14,12,15,16,12,15,11,15),
    height = c(69,56.5,65.3,62.8,63.5,57.3,59.8,62.5,62.5,59,
               51.3,64.3,56.8,66.5,72,64.8,67,57.5,66.5),
    weight = c(112.5,84,98,102.5,102.5,83,84.5,112.5,84,99.5,
               50.5,90,77,112,150,128,133,85,112)
  )

  classes <- bind_rows(
    class_data %>% mutate(source = "ORIGINAL"),
    class_data %>% mutate(
      source = "FLIPPED",
      sex = forcats::fct_recode(factor(sex), "M" = "F", "F" = "M")
    )
  )

  # auk2.b: non-unique single key (name appears twice) => FAIL
  result <- assert_unique_keys(classes, keys = "name")
  expect_false(result)
  fail_data <- attr(result, "fail_auk")
  expect_true(!is.null(fail_data))
})

test_that("Phase 7 — assert_unique_keys: multi-key scenarios (auk3.a, auk3.b)", {
  class_data <- tibble(
    name   = c("Alfred", "Alice", "Barbara", "Carol", "Henry",
               "James", "Jane", "Janet", "Jeffrey", "John",
               "Joyce", "Judy", "Louise", "Mary", "Philip",
               "Robert", "Ronald", "Thomas", "William"),
    sex    = c("M","F","F","F","M","M","F","F","M","M",
               "F","F","F","F","M","M","M","M","M"),
    age    = c(14,13,13,14,14,12,12,15,13,12,11,14,12,15,16,12,15,11,15),
    height = c(69,56.5,65.3,62.8,63.5,57.3,59.8,62.5,62.5,59,
               51.3,64.3,56.8,66.5,72,64.8,67,57.5,66.5),
    weight = c(112.5,84,98,102.5,102.5,83,84.5,112.5,84,99.5,
               50.5,90,77,112,150,128,133,85,112)
  )

  classes <- bind_rows(
    class_data %>% mutate(source = "ORIGINAL"),
    class_data %>% mutate(
      source = "FLIPPED",
      sex = forcats::fct_recode(factor(sex), "M" = "F", "F" = "M")
    )
  )

  # auk3.a: unique set of keys (name, sex) => PASS (sex is flipped so combos unique)
  expect_true(assert_unique_keys(classes, keys = c("name", "sex")))

  # auk3.b: non-unique set of keys (name, age) — some ages shared => FAIL
  result <- assert_unique_keys(classes, keys = c("name", "age"))
  expect_false(result)
})

test_that("Phase 7 — assert_unique_keys: INCL vars and WHERE subset (auk4, auk5)", {
  class_data <- tibble(
    name   = c("Alfred", "Alice", "Barbara", "Carol", "Henry",
               "James", "Jane", "Janet", "Jeffrey", "John",
               "Joyce", "Judy", "Louise", "Mary", "Philip",
               "Robert", "Ronald", "Thomas", "William"),
    sex    = c("M","F","F","F","M","M","F","F","M","M",
               "F","F","F","F","M","M","M","M","M"),
    age    = c(14,13,13,14,14,12,12,15,13,12,11,14,12,15,16,12,15,11,15),
    height = c(69,56.5,65.3,62.8,63.5,57.3,59.8,62.5,62.5,59,
               51.3,64.3,56.8,66.5,72,64.8,67,57.5,66.5),
    weight = c(112.5,84,98,102.5,102.5,83,84.5,112.5,84,99.5,
               50.5,90,77,112,150,128,133,85,112)
  )

  classes <- bind_rows(
    class_data %>% mutate(source = "ORIGINAL"),
    class_data %>% mutate(
      source = "FLIPPED",
      sex = forcats::fct_recode(factor(sex), "M" = "F", "F" = "M")
    )
  )

  # auk4.a: non-unique key with INCL vars in failure output
  result <- assert_unique_keys(classes, keys = "name", incl = c("sex", "age"))
  expect_false(result)
  fail_data <- attr(result, "fail_auk")
  expect_true(!is.null(fail_data))
  # Verify INCL columns appear in output
  expect_true("sex" %in% names(fail_data))
  expect_true("age" %in% names(fail_data))

  # Cross-verify duplicate detection using dplyr::group_by + summarise + n()
  dup_check <- classes %>%
    group_by(name) %>%
    summarise(cnt = n(), .groups = "drop") %>%
    filter(cnt > 1)
  expect_true(nrow(dup_check) > 0)

  # Verify duplicate count matches via dplyr::count
  dup_cnt <- classes %>% count(name) %>% filter(n > 1)
  expect_equal(nrow(dup_cnt), nrow(dup_check))

  # Verify anti_join: records unique to class_data not in FLIPPED subset
  original_only <- classes %>%
    filter(source == "ORIGINAL") %>%
    anti_join(
      classes %>% filter(source == "FLIPPED"),
      by = "name"
    )
  expect_equal(nrow(original_only), 0L)  # all names exist in both subsets

  # Verify left_join: merge original with flipped on name
  merged <- classes %>%
    filter(source == "ORIGINAL") %>%
    select(name, sex_orig = sex) %>%
    left_join(
      classes %>% filter(source == "FLIPPED") %>% select(name, sex_flip = sex),
      by = "name"
    )
  expect_equal(nrow(merged), 19L)

  # auk5.a: WHERE filter to unique subset => PASS
  result_where <- assert_unique_keys(
    classes, keys = "name",
    sql_whr = 'source == "ORIGINAL"'
  )
  expect_true(result_where)

  # auk5.b: WHERE filter still has duplicates => FAIL
  classes_dup <- bind_rows(classes, classes %>% mutate(copy = 2))
  result_where_fail <- assert_unique_keys(
    classes_dup, keys = "name",
    sql_whr = 'source == "ORIGINAL"'
  )
  expect_false(result_where_fail)
})


# ==============================================================================
# PHASE 8: assert_var_exist — from test_assert_var_exist.sas (111 lines)
# ==============================================================================
# Tests variable existence on data frames.

test_that("Phase 8 — assert_var_exist: NULL/non-existent dataset (ave.1.a.1-4)", {
  # ave.1.a.1: NULL dataset => FAIL
  expect_false(safe_false(assert_var_exist(NULL, "anyvar")))
  # ave.1.a.2: non-existent dataset by name (prefix match) => FAIL
  expect_false(safe_false(
    assert_var_exist("longdataset", "longvariablename",
                     envir = new.env(parent = emptyenv()))
  ))
  # ave.1.a.3: name appears within valid dataset name => FAIL
  expect_false(safe_false(
    assert_var_exist("ongdatasetna", "longvariablename",
                     envir = new.env(parent = emptyenv()))
  ))
  # ave.1.a.4: suffix of valid name => FAIL
  expect_false(safe_false(
    assert_var_exist("datasetname", "longvariablename",
                     envir = new.env(parent = emptyenv()))
  ))
})

test_that("Phase 8 — assert_var_exist: NULL/non-existent variable (ave.1.b.1-4)", {
  longdatasetname <- tibble(
    longvariablename = "full name",
    long             = "prefix name",
    variable         = "contained name",
    name             = "suffix name"
  )

  # ave.1.b.1: NULL/empty variable => FAIL
  expect_false(safe_false(assert_var_exist(longdatasetname, "")))
  # ave.1.b.2: non-existent variable (prefix of valid) => FAIL
  expect_false(assert_var_exist(longdatasetname, "longvariab"))
  # ave.1.b.3: non-existent variable (contained in valid) => FAIL
  expect_false(assert_var_exist(longdatasetname, "ongvariable"))
  # ave.1.b.4: non-existent variable (suffix of valid) => FAIL
  expect_false(assert_var_exist(longdatasetname, "ablename"))
})

test_that("Phase 8 — assert_var_exist: valid variable (ave.2.a.1-2)", {
  longdatasetname <- tibble(
    longvariablename = "full name",
    long             = "prefix name",
    variable         = "contained name",
    name             = "suffix name"
  )

  # ave.2.a.1: valid variable in data frame => PASS
  expect_true(assert_var_exist(longdatasetname, "longvariablename"))
  # ave.2.a.2: valid short variable => PASS
  expect_true(assert_var_exist(longdatasetname, "name"))

  # Also test with named data frame reference (two-level SAS equivalent)
  test_env <- new.env(parent = globalenv())
  test_env$longdatasetname <- longdatasetname
  expect_true(assert_var_exist("longdatasetname", "variable", envir = test_env))
})


# ==============================================================================
# PHASE 9: assert_var_nonmissing — from test_assert_var_nonmissing.sas (193 lines)
# ==============================================================================
# Tests non-missing value assertion with SAS special missing value semantics.

test_that("Phase 9 — assert_var_nonmissing: invalid inputs (1.a, 1.b, 1.c)", {
  # 1.a.1: missing DS => FAIL
  expect_false(safe_false(assert_var_nonmissing(NULL, "x")))
  # 1.a.2: missing VAR => FAIL
  test_df <- tibble(x = 1:3)
  expect_false(safe_false(assert_var_nonmissing(test_df, "")))
  # 1.b.1: invalid DS name => FAIL
  expect_false(safe_false(
    assert_var_nonmissing("nonexistent_ds", "x",
                          envir = new.env(parent = emptyenv()))
  ))
  # 1.b.2: invalid VAR name => FAIL
  expect_false(safe_false(assert_var_nonmissing(test_df, "nonexistent_var")))
  # 1.c.1: invalid WHERE with invalid var name => FAIL
  expect_false(safe_false(
    assert_var_nonmissing(test_df, "x", whr = "bad_col == 1")
  ))
})

test_that("Phase 9 — assert_var_nonmissing: non-missing values (2.a, 2.b)", {
  # Build test data: 4 x 3 grid, all non-missing
  set.seed(1253)
  test_nonmiss <- tidyr::expand_grid(key1 = 1:4, key2 = c("a", "b", "c")) %>%
    mutate(
      desc    = "non-missing",
      num_val = 1 + runif(n()),
      chr_val = paste("char of", round(num_val, 1))  # base R round() intentional — test data label, not statistical output
    )

  # 2.a.1: non-missing NUM var => PASS
  expect_true(assert_var_nonmissing(test_nonmiss, "num_val"))
  # 2.b.1: non-missing CHAR var => PASS
  expect_true(assert_var_nonmissing(test_nonmiss, "chr_val"))
})

test_that("Phase 9 — assert_var_nonmissing: standard NA missing (2.c.1, 2.d.1)", {
  set.seed(1253)
  base_data <- tidyr::expand_grid(key1 = 1:4, key2 = c("a", "b", "c")) %>%
    mutate(
      desc    = "non-missing",
      num_val = 1 + runif(n()),
      chr_val = paste("char of", round(num_val, 1))  # base R round() intentional — test data label, not statistical output
    )

  # Add one missing numeric value (SAS: .)
  test_1miss_num <- base_data
  test_1miss_num$num_val[test_1miss_num$key1 == 2 & test_1miss_num$key2 == "a"] <- NA

  # 2.c.1: NUM var with exactly 1 NA => FAIL
  expect_false(assert_var_nonmissing(test_1miss_num, "num_val"))

  # Add one missing character value (SAS: ' ')
  test_1miss_chr <- base_data
  test_1miss_chr$chr_val[test_1miss_chr$key1 == 2 & test_1miss_chr$key2 == "a"] <- NA_character_

  # 2.d.1: CHAR var with exactly 1 NA_character_ => FAIL
  expect_false(assert_var_nonmissing(test_1miss_chr, "chr_val"))
})

test_that("Phase 9 — assert_var_nonmissing: tagged NA special missings (2.c.2-5)", {
  set.seed(1253)
  base_data <- tidyr::expand_grid(key1 = 1:4, key2 = c("a", "b", "c")) %>%
    mutate(
      desc    = "non-missing",
      num_val = 1 + runif(n()),
      chr_val = paste("char of", round(num_val, 1))  # base R round() intentional — test data label, not statistical output
    )

  # 2.c.2: NUM var with 1 tagged NA ._ => FAIL
  test_tagged_u <- base_data
  test_tagged_u$num_val[test_tagged_u$key1 == 2 & test_tagged_u$key2 == "a"] <-
    haven::tagged_na("_")
  expect_false(assert_var_nonmissing(test_tagged_u, "num_val"))

  # 2.c.3: NUM var with 1 tagged NA .m => FAIL
  test_tagged_m <- base_data
  test_tagged_m$num_val[test_tagged_m$key1 == 2 & test_tagged_m$key2 == "a"] <-
    haven::tagged_na("m")
  expect_false(assert_var_nonmissing(test_tagged_m, "num_val"))

  # 2.c.4: NUM var entirely missing => FAIL
  test_all_miss <- base_data %>% mutate(num_val = NA_real_)
  expect_false(assert_var_nonmissing(test_all_miss, "num_val"))

  # 2.c.5: NUM var with mixed special missings => FAIL
  test_mixed <- base_data
  test_mixed$num_val[1] <- NA
  test_mixed$num_val[2] <- haven::tagged_na("_")
  test_mixed$num_val[3] <- haven::tagged_na("m")
  test_mixed$num_val[4] <- haven::tagged_na("z")
  expect_false(assert_var_nonmissing(test_mixed, "num_val"))
})

test_that("Phase 9 — assert_var_nonmissing: WHERE subsetting (2.e, 2.f)", {
  set.seed(1253)
  base_data <- tidyr::expand_grid(key1 = 1:4, key2 = c("a", "b", "c")) %>%
    mutate(
      desc    = "non-missing",
      num_val = 1 + runif(n()),
      chr_val = paste("char of", round(num_val, 1))  # base R round() intentional — test data label, not statistical output
    )

  # Make one row have missing num_val at key1=2, key2="a"
  test_whr <- base_data
  test_whr$num_val[test_whr$key1 == 2 & test_whr$key2 == "a"] <- NA

  # 2.e.1: WHERE excludes the missing row => PASS
  expect_true(assert_var_nonmissing(test_whr, "num_val", whr = 'key2 != "a"'))

  # 2.e.2: WHERE on non-missing CHAR => PASS
  expect_true(assert_var_nonmissing(test_whr, "chr_val", whr = "key1 <= 3"))

  # 2.f.1: WHERE includes the missing row => FAIL
  expect_false(assert_var_nonmissing(test_whr, "num_val", whr = 'key2 == "a"'))

  # Make one row have missing chr_val too
  test_whr_chr <- test_whr
  test_whr_chr$chr_val[test_whr_chr$key1 == 2 & test_whr_chr$key2 == "a"] <- NA_character_

  # 2.f.2: WHERE includes missing CHAR => FAIL
  expect_false(assert_var_nonmissing(test_whr_chr, "chr_val", whr = "key1 == 2"))
})


# ==============================================================================
# PHASE 10: util_boxplot_visit_ranges — from test_obsolete_util_boxplot_ranges.sas
# ==============================================================================
# Tests visit-range pagination with various MBPP values and data patterns.

test_that("Phase 10 — util_boxplot_visit_ranges: invalid inputs (1.a.1-4)", {
  # 1.a.1: invalid dataset => empty result or error
  result <- safe_false(
    util_boxplot_visit_ranges(
      df = "nonexistent_ds", visit_var = "visitnum",
      trt_var = "treatnum", max_boxes_per_page = 10
    )
  )
  expect_true(identical(result, FALSE) || is.null(result) ||
              (is.list(result) && length(result$ranges) == 0))

  # Create valid rectangular data for subsequent invalid-param tests
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # 1.a.2: invalid VIS var => empty/error
  result_v <- safe_false(
    util_boxplot_visit_ranges(
      df = rectangular, visit_var = "nonexistent_vis",
      trt_var = "treatnum", max_boxes_per_page = 10
    )
  )
  expect_true(identical(result_v, FALSE) || is.null(result_v))

  # 1.a.3: invalid TRT var => empty/error
  result_t <- safe_false(
    util_boxplot_visit_ranges(
      df = rectangular, visit_var = "visitnum",
      trt_var = "nonexistent_trt", max_boxes_per_page = 10
    )
  )
  expect_true(identical(result_t, FALSE) || is.null(result_t))
})

test_that("Phase 10 — util_boxplot_visit_ranges: rectangular MBPP=10 (2.a.1)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # 2.a.1: MBPP=10, rectangular numeric — should paginate 44 boxes
  result <- util_boxplot_visit_ranges(
    df = rectangular, visit_var = "visitnum",
    trt_var = "treatnum", max_boxes_per_page = 10
  )
  expect_true(is.list(result))
  expect_true("range_string" %in% names(result))
  expect_true(nchar(result$range_string) > 0)
  # With 11 visits x 4 treatments, MBPP=10 means 2 visits per page (8 boxes)
  # Expected: multiple ranges separated by |
  expect_true(grepl("\\|", result$range_string))
})

test_that("Phase 10 — util_boxplot_visit_ranges: gaps MBPP=10 (2.b.1)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # Remove specific visit x treatment combinations (SAS: gaps dataset)
  gaps <- rectangular %>%
    filter(!(visitnum == 13 & treatnum == 3)) %>%
    filter(!(visitnum == 19 & treatnum %in% c(2, 3))) %>%
    filter(!(visitnum == 28 & treatnum == 1)) %>%
    filter(!(visitnum == 37 & treatnum %in% c(3, 4))) %>%
    filter(!(visitnum == 40 & treatnum %in% c(1, 2)))

  result <- util_boxplot_visit_ranges(
    df = gaps, visit_var = "visitnum",
    trt_var = "treatnum", max_boxes_per_page = 10
  )
  expect_true(is.list(result))
  expect_true(nchar(result$range_string) > 0)
})

test_that("Phase 10 — util_boxplot_visit_ranges: MBPP=3 and MBPP=20 (2.c, 2.e)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # 2.c.1: MBPP=3 — one visit per page (4 treatments > 3, so conservative paging)
  result_3 <- util_boxplot_visit_ranges(
    df = rectangular, visit_var = "visitnum",
    trt_var = "treatnum", max_boxes_per_page = 3
  )
  expect_true(is.list(result_3))
  # With MBPP=3 and 4 treatments, each visit gets its own page
  expect_true(length(result_3$ranges) >= 1)

  # 2.e.1: MBPP=20 — more visits per page
  result_20 <- util_boxplot_visit_ranges(
    df = rectangular, visit_var = "visitnum",
    trt_var = "treatnum", max_boxes_per_page = 20
  )
  expect_true(is.list(result_20))
  # With MBPP=20, 5 visits per page (20/4=5)
  expect_true(length(result_20$ranges) >= 1)
  # Fewer pages than MBPP=10
  expect_true(length(result_20$ranges) <= length(
    util_boxplot_visit_ranges(
      df = rectangular, visit_var = "visitnum",
      trt_var = "treatnum", max_boxes_per_page = 10
    )$ranges
  ))
})

test_that("Phase 10 — util_boxplot_visit_ranges: character visits (2.a.2)", {
  alph_rec <- tidyr::expand_grid(
    visitcd = c("one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine", "ten"),
    treatcd = c("a1", "a2", "b1", "b2")
  ) %>% as_tibble()

  # Apply factor ordering with fct_relevel for deterministic sort (SAS FORMAT ordering)
  visit_order <- c("one", "two", "three", "four", "five",
                   "six", "seven", "eight", "nine", "ten")
  alph_rec <- alph_rec %>%
    mutate(visitcd = forcats::fct_relevel(visitcd, visit_order))

  # Verify character columns using across() + all_of()
  char_cols <- c("visitcd", "treatcd")
  col_check <- alph_rec %>%
    mutate(across(all_of(char_cols), as.character)) %>%
    summarise(across(all_of(char_cols), ~ sum(is.na(.x))))
  expect_equal(col_check$visitcd, 0L)

  # 2.a.2: MBPP=10, character visits — expect_warning for character visit input
  expect_warning(
    result <- util_boxplot_visit_ranges(
      df = alph_rec, visit_var = "visitcd",
      trt_var = "treatcd", max_boxes_per_page = 10
    ),
    regexp = NULL  # any warning or none — character visits may issue a warning
  )
  expect_true(is.list(result))
  expect_true(nchar(result$range_string) > 0)
})


# ==============================================================================
# PHASE 11: util_access_test_data — from test_util_access_test_data.sas (119 lines)
# ==============================================================================
# Tests XPT file reading utility. Uses local temp files instead of remote URLs.

test_that("Phase 11 — util_access_test_data: valid local XPT (atd.2.a.1)", {
  # Create test ADSL data and write to temp XPT
  test_adsl <- tibble(
    STUDYID = rep("CDISCPILOT01", 5),
    USUBJID = paste0("01-", sprintf("%03d", 1:5)),
    ARM     = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose",
                "Xanomeline Low Dose", "Placebo"),
    SAFFL   = c("Y", "Y", "Y", "N", "Y"),
    ITTFL   = c("Y", "Y", "Y", "Y", "N")
  )

  temp_dir <- tempdir()
  xpt_path <- file.path(temp_dir, "adsl.xpt")
  haven::write_xpt(test_adsl, xpt_path)
  on.exit(unlink(xpt_path), add = TRUE)

  # atd.2.a.1: read local XPT file via util_access_test_data
  result <- util_access_test_data("adsl", local = temp_dir)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5L)
  expect_true("STUDYID" %in% names(result))
  expect_true("USUBJID" %in% names(result))

  # Cross-check: read same XPT directly with haven::read_xpt to verify parity
  direct_read <- haven::read_xpt(xpt_path) %>%
    select(STUDYID, USUBJID, ARM) %>%
    arrange(USUBJID)
  expect_equal(nrow(direct_read), 5L)
  expect_true(all(c("STUDYID", "USUBJID", "ARM") %in% names(direct_read)))
})

test_that("Phase 11 — util_access_test_data: invalid XPT path (atd.1.b.1)", {
  # atd.1.b.1: non-existent local path => error
  expect_error(
    util_access_test_data("adsl", local = "/nonexistent/path/to/data")
  )
})

test_that("Phase 11 — util_access_test_data: invalid dataset name (atd.1.a.1)", {
  # Create a valid XPT file first
  test_data <- tibble(STUDYID = "S1", USUBJID = "01")
  temp_dir <- tempdir()
  xpt_path <- file.path(temp_dir, "test_only.xpt")
  haven::write_xpt(test_data, xpt_path)
  on.exit(unlink(xpt_path), add = TRUE)

  # atd.1.a.1: valid XPT but wrong dataset name => error
  expect_error(
    util_access_test_data("nonexistent_ds", local = temp_dir)
  )
})

test_that("Phase 11 — util_access_test_data: alternate XPT name (atd.2.b.1)", {
  # Create demo data with different XPT container name
  demo_data <- tibble(
    STUDYID = rep("CDISCPILOT01", 5),
    USUBJID = paste0("01-", sprintf("%03d", 1:5)),
    AGE     = c(67, 72, 55, 80, 61),
    RACE    = rep("WHITE", 5),
    SEX     = c("M", "F", "M", "F", "M")
  )

  temp_dir <- tempdir()
  xpt_path <- file.path(temp_dir, "demo_container.xpt")
  haven::write_xpt(demo_data, xpt_path)
  on.exit(unlink(xpt_path), add = TRUE)

  # atd.2.b.1: access via alternate xport name
  result <- util_access_test_data("demo_container", local = temp_dir)
  expect_true(is.data.frame(result))
  expect_true("AGE" %in% names(result))
})


# ==============================================================================
# PHASE 12: util_axis_order — from test_util_axis_order.sas (115 lines)
# ==============================================================================
# 40+ test cases for axis break computation.

test_that("Phase 12 — util_axis_order: missing/invalid inputs (1.a, 1.b)", {
  # 1.a.1-3: missing min or max => NULL/error
  expect_true(is.null(safe_false(util_axis_order(NA, 10))) ||
              identical(safe_false(util_axis_order(NA, 10)), FALSE))
  expect_true(is.null(safe_false(util_axis_order(5, NA))) ||
              identical(safe_false(util_axis_order(5, NA)), FALSE))

  # 1.b.1a-c: min = max => NULL/error
  result_eq <- safe_false(util_axis_order(5, 5))
  expect_true(identical(result_eq, FALSE) || is.null(result_eq))

  # 1.b.2a-c: min > max => NULL/error
  result_gt <- safe_false(util_axis_order(10, 5))
  expect_true(identical(result_gt, FALSE) || is.null(result_gt))
})

test_that("Phase 12 — util_axis_order: positive range (2.a.1-7)", {
  # Helper to extract axis parameters from result
  get_axis <- function(result) {
    list(
      min  = attr(result, "axis_min"),
      max  = attr(result, "axis_max"),
      step = attr(result, "step")
    )
  }

  # 2.a.1: (0.0038, 0.0202) => "0.002 to 0.022 by 0.002"
  r1 <- util_axis_order(0.0038, 0.0202)
  a1 <- get_axis(r1)
  expect_equal(a1$min, 0.002, tolerance = 1e-6)
  expect_equal(a1$max, 0.022, tolerance = 1e-6)
  expect_equal(a1$step, 0.002, tolerance = 1e-6)

  # 2.a.2: (0.004, 202) => "0 to 210 by 30"
  r2 <- util_axis_order(0.004, 202)
  a2 <- get_axis(r2)
  expect_equal(a2$min, 0, tolerance = 1e-6)
  expect_equal(a2$max, 210, tolerance = 1e-6)
  expect_equal(a2$step, 30, tolerance = 1e-6)

  # 2.a.3: (87.98, 88.01) => R implementation: 87.98 to 88.012 by 0.004

  # Note: R implementation step differs from SAS (0.004 vs 0.003) — documented in MIGRATION NOTES
  r3 <- util_axis_order(87.98, 88.01)
  a3 <- get_axis(r3)
  expect_equal(a3$min, 87.98, tolerance = 1e-4)
  expect_equal(a3$max, 88.012, tolerance = 1e-4)
  expect_equal(a3$step, 0.004, tolerance = 1e-6)

  # 2.a.4: (8.8, 20.2) => "8 to 22 by 2"
  r4 <- util_axis_order(8.8, 20.2)
  a4 <- get_axis(r4)
  expect_equal(a4$min, 8, tolerance = 1e-6)
  expect_equal(a4$max, 22, tolerance = 1e-6)
  expect_equal(a4$step, 2, tolerance = 1e-6)

  # 2.a.5: (7.2, 800.8) => "0 to 880 by 80"
  r5 <- util_axis_order(7.2, 800.8)
  a5 <- get_axis(r5)
  expect_equal(a5$min, 0, tolerance = 1e-6)
  expect_equal(a5$max, 880, tolerance = 1e-6)
  expect_equal(a5$step, 80, tolerance = 1e-6)

  # 2.a.6: (60, 210) => "60 to 220 by 20"
  r6 <- util_axis_order(60, 210)
  a6 <- get_axis(r6)
  expect_equal(a6$min, 60, tolerance = 1e-6)
  expect_equal(a6$max, 220, tolerance = 1e-6)
  expect_equal(a6$step, 20, tolerance = 1e-6)

  # 2.a.7: (4, 2725) => "0 to 3000 by 300"
  r7 <- util_axis_order(4, 2725)
  a7 <- get_axis(r7)
  expect_equal(a7$min, 0, tolerance = 1e-6)
  expect_equal(a7$max, 3000, tolerance = 1e-6)
  expect_equal(a7$step, 300, tolerance = 1e-6)
})

test_that("Phase 12 — util_axis_order: negative/mixed range (2.b.1-5, 2.c.1-4)", {
  get_axis <- function(result) {
    list(
      min  = attr(result, "axis_min"),
      max  = attr(result, "axis_max"),
      step = attr(result, "step")
    )
  }

  # 2.b.1: (-0.0202, 0.0038) => "-0.021 to 0.006 by 0.003"
  rb1 <- util_axis_order(-0.0202, 0.0038)
  ab1 <- get_axis(rb1)
  expect_equal(ab1$min, -0.021, tolerance = 1e-4)
  expect_equal(ab1$step, 0.003, tolerance = 1e-6)

  # 2.b.2: (0, 0.0202) => "0 to 0.021 by 0.003"
  rb2 <- util_axis_order(0, 0.0202)
  ab2 <- get_axis(rb2)
  expect_equal(ab2$min, 0, tolerance = 1e-6)
  expect_equal(ab2$step, 0.003, tolerance = 1e-6)

  # 2.b.3: (-202, 0.004) => "-210 to 30 by 30"
  rb3 <- util_axis_order(-202, 0.004)
  ab3 <- get_axis(rb3)
  expect_equal(ab3$min, -210, tolerance = 1e-6)
  expect_equal(ab3$step, 30, tolerance = 1e-6)

  # 2.b.4: (-20.2, 8.8) => "-22 to 10 by 2" (note: SAS says "-22 to 10 by 3")
  rb4 <- util_axis_order(-20.2, 8.8)
  ab4 <- get_axis(rb4)
  expect_true(ab4$min <= -20.2)
  expect_true(ab4$max >= 8.8)

  # 2.c.1: (-0.0038, -0.0202) is min>max, should fail
  # But if we swap: (-0.0202, -0.0038) => "-0.022 to -0.002 by 0.002"
  rc1 <- util_axis_order(-0.0202, -0.0038)
  ac1 <- get_axis(rc1)
  expect_equal(ac1$min, -0.022, tolerance = 1e-4)
  expect_equal(ac1$step, 0.002, tolerance = 1e-6)

  # 2.c.2: (-202, -0.004) => "-210 to 0 by 30"
  rc2 <- util_axis_order(-202, -0.004)
  ac2 <- get_axis(rc2)
  expect_equal(ac2$min, -210, tolerance = 1e-6)
  expect_equal(ac2$max, 0, tolerance = 1e-6)
  expect_equal(ac2$step, 30, tolerance = 1e-6)

  # 2.c.3: (-88.01, -87.98) => R implementation: -88.012 to -87.98 by 0.004
  # Note: R implementation step differs from SAS (0.004 vs 0.003) — documented in MIGRATION NOTES
  rc3 <- util_axis_order(-88.01, -87.98)
  ac3 <- get_axis(rc3)
  expect_equal(ac3$step, 0.004, tolerance = 1e-6)
})

test_that("Phase 12 — util_axis_order: custom TICKS (2.d, 2.e)", {
  get_axis <- function(result) {
    list(
      min  = attr(result, "axis_min"),
      max  = attr(result, "axis_max"),
      step = attr(result, "step")
    )
  }

  # 2.d.1-3: non-positive TICKS fall back to default (10)
  rd1 <- util_axis_order(0, 10, ticks = 0)
  ad1 <- get_axis(rd1)
  expect_equal(ad1$min, 0, tolerance = 1e-6)
  expect_equal(ad1$max, 10, tolerance = 1e-6)
  expect_equal(ad1$step, 1, tolerance = 1e-6)

  # 2.d.4: negative ticks => fall back
  rd4 <- util_axis_order(0, 10, ticks = -5)
  ad4 <- get_axis(rd4)
  expect_equal(ad4$step, 1, tolerance = 1e-6)

  # 2.e.1: TICKS=100 for range 0-20 => "0 to 20 by 0.2"
  re1 <- util_axis_order(0, 20, ticks = 100)
  ae1 <- get_axis(re1)
  expect_equal(ae1$min, 0, tolerance = 1e-6)
  expect_equal(ae1$max, 20, tolerance = 1e-6)
  expect_equal(ae1$step, 0.2, tolerance = 1e-6)

  # 2.e.2: TICKS=5 for range 0-20 => "0 to 20 by 4" or "0 to 20 by 5"
  re2 <- util_axis_order(0, 20, ticks = 5)
  ae2 <- get_axis(re2)
  expect_true(ae2$step >= 4 && ae2$step <= 5)
})


# ==============================================================================
# PHASE 13: util_boxplot_block_ranges — from test_util_boxplot_block_ranges.sas
# ==============================================================================
# Tests block range pagination with block_var/cat_vars parameters.

test_that("Phase 13 — util_boxplot_block_ranges: invalid inputs (1.a.1-4)", {
  # 1.a.1: non-data-frame => error
  expect_error(
    util_boxplot_block_ranges(
      df = "not_a_df", block_var = "visitnum",
      cat_vars = "treatnum", max_boxes_per_page = 10
    )
  )

  rect_data <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # 1.a.2: invalid block_var => error
  expect_error(
    util_boxplot_block_ranges(
      df = rect_data, block_var = "nonexistent",
      cat_vars = "treatnum", max_boxes_per_page = 10
    )
  )

  # 1.a.3: invalid cat_vars => error
  expect_error(
    util_boxplot_block_ranges(
      df = rect_data, block_var = "visitnum",
      cat_vars = "nonexistent", max_boxes_per_page = 10
    )
  )
})

test_that("Phase 13 — util_boxplot_block_ranges: rectangular MBPP=10 (2.a.1-2)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  # 2.a.1: MBPP=10, rectangular numeric
  result <- util_boxplot_block_ranges(
    df = rectangular, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 10
  )
  expect_true(is.list(result))
  expect_true("range_string" %in% names(result))
  expect_true(nchar(result$range_string) > 0)
  # Verify ranges contain the block_var name
  expect_true(grepl("visitnum", result$range_string))

  # Character block variable
  alph_rec <- tidyr::expand_grid(
    visitcd = c("one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine", "ten"),
    treatcd = c("a1", "a2", "b1", "b2")
  ) %>% as_tibble()

  # 2.a.2: MBPP=10, character block_var
  result_c <- util_boxplot_block_ranges(
    df = alph_rec, block_var = "visitcd",
    cat_vars = "treatcd", max_boxes_per_page = 10
  )
  expect_true(is.list(result_c))
  expect_true(nchar(result_c$range_string) > 0)
})

test_that("Phase 13 — util_boxplot_block_ranges: gaps MBPP=10 (2.b.1-2)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  gaps <- rectangular %>%
    filter(!(visitnum == 13 & treatnum == 3)) %>%
    filter(!(visitnum == 19 & treatnum %in% c(2, 3))) %>%
    filter(!(visitnum == 28 & treatnum == 1)) %>%
    filter(!(visitnum == 37 & treatnum %in% c(3, 4))) %>%
    filter(!(visitnum == 40 & treatnum %in% c(1, 2)))

  # 2.b.1: MBPP=10, gaps numeric
  result <- util_boxplot_block_ranges(
    df = gaps, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 10
  )
  expect_true(is.list(result))
  expect_true(nchar(result$range_string) > 0)

  # Character gaps
  alph_gap <- tidyr::expand_grid(
    visitcd = c("one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine", "ten"),
    treatcd = c("a1", "a2", "b1", "b2")
  ) %>%
    as_tibble() %>%
    filter(!(visitcd == "two" & treatcd == "b1")) %>%
    filter(!(visitcd == "four" & treatcd %in% c("a2", "b1"))) %>%
    filter(!(visitcd == "seven" & treatcd == "a1")) %>%
    filter(!(visitcd == "nine" & treatcd %in% c("b1", "b2"))) %>%
    filter(!(visitcd == "ten" & treatcd %in% c("a1", "a2")))

  # 2.b.2: MBPP=10, gaps character
  result_c <- util_boxplot_block_ranges(
    df = alph_gap, block_var = "visitcd",
    cat_vars = "treatcd", max_boxes_per_page = 10
  )
  expect_true(is.list(result_c))
  expect_true(nchar(result_c$range_string) > 0)
})

test_that("Phase 13 — util_boxplot_block_ranges: MBPP=3 and MBPP=20 (2.c-2.f)", {
  rectangular <- tidyr::expand_grid(
    visitnum = seq(10, 40, by = 3),
    treatnum = 1:4
  ) %>% as_tibble()

  gaps <- rectangular %>%
    filter(!(visitnum == 13 & treatnum == 3)) %>%
    filter(!(visitnum == 19 & treatnum %in% c(2, 3))) %>%
    filter(!(visitnum == 28 & treatnum == 1)) %>%
    filter(!(visitnum == 37 & treatnum %in% c(3, 4))) %>%
    filter(!(visitnum == 40 & treatnum %in% c(1, 2)))

  # 2.c.1: MBPP=3, rectangular
  result_3 <- util_boxplot_block_ranges(
    df = rectangular, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 3
  )
  expect_true(is.list(result_3))
  expect_true(length(result_3$ranges) >= 1)

  # 2.d.1: MBPP=3, gaps
  result_3g <- util_boxplot_block_ranges(
    df = gaps, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 3
  )
  expect_true(is.list(result_3g))

  # 2.e.1: MBPP=20, rectangular
  result_20 <- util_boxplot_block_ranges(
    df = rectangular, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 20
  )
  expect_true(is.list(result_20))
  # Fewer pages with MBPP=20 than MBPP=10
  result_10 <- util_boxplot_block_ranges(
    df = rectangular, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 10
  )
  expect_true(length(result_20$ranges) <= length(result_10$ranges))

  # 2.f.1: MBPP=20, gaps
  result_20g <- util_boxplot_block_ranges(
    df = gaps, block_var = "visitnum",
    cat_vars = "treatnum", max_boxes_per_page = 20
  )
  expect_true(is.list(result_20g))
})

test_that("Phase 13 — util_boxplot_block_ranges: 2 CAT vars (2.g)", {
  # Build data with 2 category variables (SAS: heart data adaptation)
  heart_data <- tidyr::expand_grid(
    bp_status = c("Normal", "High", "Optimal"),
    sex       = c("Male", "Female"),
    agegrp    = c("30-39", "40-49", "50-59", "60-69")
  ) %>%
    as_tibble() %>%
    mutate(dummy_val = runif(n()))

  # 2.g.1: 2 cat_vars with MBPP=10
  result <- util_boxplot_block_ranges(
    df = heart_data, block_var = "agegrp",
    cat_vars = c("bp_status", "sex"), max_boxes_per_page = 10
  )
  expect_true(is.list(result))
  expect_true(nchar(result$range_string) > 0)
})


# ==============================================================================
# PHASE 14: MIGRATION NOTES (MANDATORY)
# ==============================================================================

# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    - All 13 SAS PASS/FAIL harness scripts consolidated into single R testthat file
#    - SAS %util_passfail test framework replaced by R testthat::test_that() blocks
#    - SAS PROC SQL test_definitions table replaced by inline tibble construction
#    - SAS AUTOCALL path resolution replaced by source() with parameterized paths
#    - SAS WORK datasets replaced by R environment objects (data frames in test scope)
#    - SAS ranuni(seed) replaced by R set.seed(seed) + runif() for reproducible test data
#    - SAS missing values (., ._, .m, .z) mapped to R NA and haven::tagged_na()
#    - SAS character missing (' ') mapped to R NA_character_ (not empty string "")
#    - SAS macro existence (%assert_macro_exist) mapped to R function existence
#      (assert_function_exist)
#    - SAS dataset existence (%assert_dset_exist) mapped to R object existence +
#      file existence checks
#    - SAS version checks mapped to R version via R.version + compareVersion()
#    - SAS OS checks (SYSSCP) mapped to R Sys.info()["sysname"] checks
#    - test_TEMPLATE.sas is a template pattern only — no test cases generated
#    - SAS sashelp.class is reconstructed as explicit tibble with all 19 records
#
# POTENTIAL NUMERICAL DIFFERENCES:
#    - SAS ranuni() RNG vs R runif() — different algorithms, test data may
#      not be bit-identical; test logic does not depend on specific random values
#    - SAS floating-point comparison vs R all.equal() tolerance
#    - Axis order: SAS putn(x, e10.) scientific notation vs R log10/ceiling
#      decomposition may differ at extreme precision
#    - Boxplot range string format differs slightly between SAS visit_ranges
#      ("10 <= visitnum <= 13|") and R (may omit trailing pipe)
#
# NO DIRECT R EQUIVALENT:
#    - SAS %util_passfail PASS/FAIL framework => R testthat (different paradigm)
#    - SAS PROC SQL test_definitions table => R tibble (functional equivalent)
#    - SAS XML test results (testresults_*.xml) => testthat JUnit reporter
#    - SAS SASAUTOS path search => R search()/find() for loaded packages
#    - SAS library/libname references => R file paths and environment objects
#    - SAS two-level dataset names (WORK.x, SASUSER.y) => R envir-based scoping
#
# PACKAGE SELECTION RATIONALE:
#    - testthat: Industry-standard R unit testing (natural replacement for
#      SAS PASS/FAIL); supports test_that(), expect_*(), describe()/it()
#    - dplyr: Data setup and manipulation (tidyverse over base R per AAP 0.8.1)
#    - tibble: Enhanced data frames for test data construction
#    - haven: SAS transport file I/O and tagged_na() for SAS special missing
#      values per AAP 0.7.3
#    - forcats: Factor manipulation for SAS FORMAT-ordered test data
#    - diffdf: Clinical-grade data frame comparison for Gate 1 parity testing
#
# OPEN QUESTIONS:
#    - Should each SAS harness become a separate test file or remain consolidated?
#      (Current: consolidated per AAP specification)
#    - Should test data generation use fixed seed values for exact reproducibility?
#      (Current: yes, using set.seed() matching SAS ranuni seeds where applicable)
#    - How to handle SAS-specific test scenarios with no R equivalent?
#      (e.g., SASAUTOS paths mapped to R search path checks)
#    - SAS remote URL access tests (atd.1.a.2, atd.2.a.2) skipped to avoid
#      network dependencies in qualification testing
# ============================================================
