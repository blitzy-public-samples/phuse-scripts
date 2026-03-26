### Project Programming Guidelines

[Programming Guidelines](http://www.phusewiki.org/wiki/index.php?title=WG5_P02_Programming_Guidelines) for this project are in our PhUSE Wiki.

As detailed in the guidelines, we base these on [PhUSE Good Programming Practices](http://www.phusewiki.org/wiki/index.php?title=Good_Programming_Practice_Guidance).

---

## R Programming Guidelines

The following guidelines govern all R code in this repository, including scripts migrated from SAS and new R implementations. These standards ensure idiomatic, production-ready, regulatory-compliant R code aligned with the pharmaverse ecosystem.

### 1. Tidyverse Coding Standards

**Tidyverse over base R**: Do NOT use base R equivalents when a tidyverse function exists and is appropriate.

- Use the pipe operator (`%>%` from magrittr or `|>` native R 4.1+) for readable, chainable data transformations
- **dplyr** for data manipulation: `mutate()`, `filter()`, `select()`, `arrange()`, `group_by()`, `summarise()`, `left_join()`, `inner_join()`, `bind_rows()`
- **tidyr** for reshaping: `pivot_longer()`, `pivot_wider()`, `unnest()`, `nest()`
- **purrr** for iteration: `map()`, `walk()`, `accumulate()` — replaces SAS array processing and macro loops
- **stringr** for string manipulation: `str_detect()`, `str_replace()`, `str_extract()`, `str_c()` — replaces SAS character functions
- **lubridate** for date handling: interval arithmetic, date parsing, and SAS date epoch conversion (see [Date Arithmetic Rules](#4-date-arithmetic-rules))
- **forcats** for factor manipulation: `fct_relevel()`, `fct_inorder()`, `fct_recode()` — replaces SAS format-based ordering
- **readr** for flat-file I/O: `read_csv()`, `read_delim()`, `write_csv()`

Example — idiomatic dplyr pipeline (replacing SAS DATA step):

```r
# SAS equivalent:
#   data ae_summary;
#     set adae;
#     where saffl = 'Y';
#     by trtan aebodsys aedecod;
#   run;

ae_summary <- adae %>%
  filter(SAFFL == "Y") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD) %>%
  group_by(TRTAN, AEBODSYS, AEDECOD)
```

### 2. Package Usage Conventions

The following packages are the **mandatory choices** for their respective domains. Do not substitute alternatives unless explicitly justified and documented.

| Domain | Package | Purpose | Replaces (SAS) |
|--------|---------|---------|-----------------|
| Data I/O | `haven` | `read_xpt()`, `read_sas()`, `write_xpt()` for SAS datasets | LIBNAME, PROC IMPORT (XPT) |
| Data I/O | `readr` | `read_csv()`, `read_delim()` for flat files | PROC IMPORT (CSV) |
| Clinical tables | `Tplyr` | Frequency/summary tables with denominators and traceability | PROC FREQ, PROC MEANS, PROC REPORT |
| RTF/PDF output | `r2rtf` | Production-quality RTF/PDF document generation | ODS RTF, ODS PDF |
| Excel output | `openxlsx` | Workbook generation with styles and formatting | SpreadsheetML XML, PCFILES/JET engine |
| ADaM derivations | `admiral` | CDISC-compliant analysis dataset creation | Custom ADaM derivation macros |
| Visualization | `ggplot2` | All graphics and figures | PROC SGPLOT, PROC SGRENDER, PROC SHEWHART |
| Plot composition | `patchwork` / `gridExtra` | Multi-panel figure layouts | GTL LAYOUT statements |
| Mixed models (MMRM) | `mmrm` | FDA-aligned MMRM with Satterthwaite/Kenward-Roger df | PROC MIXED, PROC GLIMMIX |
| LS means | `emmeans` | Estimated marginal means for MMRM and GLM models | LSMEANS / ESTIMATE / CONTRAST statements |
| Survival analysis | `survival` | `Surv()`, `survfit()`, `coxph()`, `survdiff()` | PROC LIFETEST, PROC PHREG |
| Survival plots | `survminer` | `ggsurvplot()` for Kaplan-Meier curves | PROC SGPLOT (KM overlays) |
| ANOVA | `car` | `Anova()` for Type II/III sums of squares | PROC GLM |
| Statistical tests | base `stats` | `fisher.test()`, `t.test()`, `wilcox.test()`, `prop.test()` | PROC FREQ (EXACT FISHER), PROC TTEST |
| Rounding | `janitor` | `round_half_up()` for SAS-compatible rounding | SAS ROUND() function |
| Table rendering | `gt` | Complex nested table layouts (alternative to Tplyr for display) | PROC REPORT / PROC TABULATE |
| Assertions | `cli` | `cli_abort()`, `cli_warn()` for informative error messages | %PUT ERROR / %PUT WARNING |
| Config parsing | `yaml` | `read_yaml()` for configuration files | %LET / %INCLUDE config |
| Data comparison | `diffdf` | SAS-vs-R output comparison for validation | PROC COMPARE |
| Testing | `testthat` | Unit testing framework | Qualification harness scripts |
| Reproducibility | `renv` | Package version lockfile management | N/A (new for R) |

**Prohibited package substitutions:**

- Do **NOT** use `lme4`, `nlme`, or `glmer` for models where `mmrm` applies
- Do **NOT** use base R `merge()` when `dplyr::left_join()` (or other dplyr join) is appropriate
- Do **NOT** use base R `aggregate()` when `dplyr::summarise()` is appropriate
- Do **NOT** use base R `round()` for clinical outputs — use `janitor::round_half_up()` instead

### 3. Missing Value Handling Rules

Missing value handling is **critical for regulatory compliance**. SAS and R handle missing values differently, and every migrated script must follow these rules precisely.

| SAS Construct | R Equivalent | Notes |
|---------------|-------------|-------|
| Numeric missing (`.`) | `NA` | NEVER `0`, NEVER `NaN` |
| Character missing (`' '`) | `NA_character_` | NOT empty string `""` |
| Special missing (`.A` – `.Z`) | `haven::tagged_na('A')` – `haven::tagged_na('Z')` | Only when distinction is required |
| `MISSING(x)` function | `is.na(x)` | Use consistently for all missing checks |
| `NMISS(x)` | `sum(is.na(x))` | Count of missing numeric values |
| `CMISS(x)` | `sum(is.na(x))` | Count of missing character values |
| `COALESCE(a, b)` | `dplyr::coalesce(a, b)` | First non-missing value |

**Non-negotiable rules:**

- Missing values are **NEVER** assumed to be zero — no implicit zero substitution
- When reading SAS datasets via `haven::read_xpt()` or `haven::read_sas()`, SAS missing values are automatically mapped to `NA`; verify this mapping for every critical variable
- When comparing values that may be `NA`, always guard with `is.na()` checks or use `dplyr::if_else()` (which is strict about `NA` types) rather than base `ifelse()`
- Document every variable with missing values in the Gate 3 Missing Value Audit

### 4. Date Arithmetic Rules

SAS stores dates as integer days from **January 1, 1960**. R stores dates as integer days from **January 1, 1970**. All date conversions must account for this epoch difference.

| SAS Operation | R Equivalent | Example |
|---------------|-------------|---------|
| SAS numeric date → R Date | `as.Date(sas_value, origin = "1960-01-01")` | `as.Date(21185, origin = "1960-01-01")` → `"2018-01-01"` |
| SAS numeric datetime → R POSIXct | `as.POSIXct(sas_value, origin = "1960-01-01")` | Note: SAS datetime is in **seconds**, not days |
| `INTCK('MONTH', d1, d2)` | `lubridate::interval(d1, d2) %/% months(1)` | Use lubridate interval division for month counts |
| `INTCK('DAY', d1, d2)` | `as.integer(d2 - d1)` | Simple day difference |
| `INTCK('YEAR', d1, d2)` | `lubridate::interval(d1, d2) %/% years(1)` | Full-year count |
| `INTNX('MONTH', d, n)` | `d %m+% months(n)` | lubridate month addition with rollback |
| `DATEPART(dt)` | `as.Date(dt)` | Extract date from datetime |
| `TIMEPART(dt)` | `format(dt, "%H:%M:%S")` | Extract time from datetime |

**Rules:**

- All date arithmetic in migrated scripts **must** be verified against SAS output
- Be explicit about the `origin` parameter — never rely on R's default epoch
- Use `lubridate` for interval and period arithmetic; distinguish between **durations** (exact seconds) and **periods** (calendar units)
- CDISC date character variables (e.g., `RFSTDTC`, `ASTDTC`) should be parsed with `lubridate::ymd()` or `as.Date(x, format = "%Y-%m-%d")`

### 5. Rounding Behavior

SAS uses **round-half-up** (0.5 rounds to 1). R uses **round-half-to-even** (banker's rounding) by default. This difference is **critical for regulatory outputs** where numeric values must match SAS exactly.

**Mandatory rule:** Use `janitor::round_half_up()` at every rounding location in migrated R scripts.

```r
# WRONG — R default banker's rounding:
round(2.5, 0)        # Returns 2 (half-to-even)
round(3.5, 0)        # Returns 4 (half-to-even)

# CORRECT — SAS-compatible rounding:
janitor::round_half_up(2.5, digits = 0)  # Returns 3 (half-up)
janitor::round_half_up(3.5, digits = 0)  # Returns 4 (half-up)
```

A convenience wrapper may be defined project-wide:

```r
sas_round <- function(x, digits = 0) {
  janitor::round_half_up(x, digits = digits)
}
```

Every location where rounding occurs **must** be documented in the **Gate 2 Rounding and Precision Audit**.

### 6. SAS-to-R Construct Mapping Reference

The following table provides the canonical mapping between SAS constructs and their R equivalents. All migrated scripts must follow these mappings.

| SAS Construct | R Equivalent | Preservation Requirement |
|---------------|-------------|--------------------------|
| DATA step (merge, set, array, retain) | dplyr pipelines, purrr maps | Logic semantics identical, not syntax |
| PROC SQL | dplyr verbs or dbplyr | Join type, filter order, aggregation behavior preserved |
| SAS macros (`%macro name(p=d)`) | Parameterized R functions (`name <- function(p = d)`) | All parameters → named args with matching defaults |
| `%let` / `%global` / `%local` | Function scoping / environment variables | Use function scope by default; environment for global only when necessary |
| `%if` / `%do` loops | `if` / `for` / `purrr::map()` | Control flow semantics preserved |
| RETAIN statement | `purrr::accumulate()` or `dplyr::lag()` | State carryforward semantics preserved |
| BY-group processing | `group_by() + arrange()` | Sort order **must** be established before grouping |
| PROC MIXED / PROC GLIMMIX | `mmrm::mmrm()` or `glmmTMB` | Covariance structure, df method, optimizer specified explicitly |
| PROC LIFETEST / PROC PHREG | `survival::survfit()`, `survival::coxph()` | Ties method, stratification, test statistics preserved |
| PROC FREQ | `Tplyr` count layer or `table()` | Denominator logic, ordering, missing handling preserved |
| PROC FREQ EXACT FISHER | `fisher.test()` | Continuity correction must be explicitly coded if required |
| PROC MEANS / PROC UNIVARIATE | Tplyr desc layer or `dplyr::summarise()` | Statistic set, format precision, N vs N_obs preserved |
| PROC REPORT / PROC TABULATE | Tplyr, rtables, or gt | Layout structure preserved, not just data |
| PROC SGPLOT / PROC SGRENDER | `ggplot2` | Visual output structure and labeling preserved |
| PROC SHEWHART | `ggplot2::geom_boxplot()` + custom theme | Control chart semantics preserved |
| ODS RTF / ODS PDF | `r2rtf` | Page orientation, font, column widths, titles/footnotes mapped |
| SpreadsheetML XML / PCFILES | `openxlsx` | Workbook structure, styles, multi-sheet layout preserved |
| SAS formats / informats | `haven` labels, `factor()` levels | All user-defined formats mapped; factor ordering preserved |
| SAS date math (days from 1960-01-01) | `as.Date(x, origin = "1960-01-01")` | All date arithmetic verified |
| Numeric missing (`.`) | `NA` | No implicit zero substitution |
| Character missing (`' '`) | `NA_character_` | Blank vs missing distinction preserved |
| PUT / INPUT functions | `format()`, `as.numeric()`, `as.character()` | Conversion semantics verified, no silent truncation |
| LIBNAME | `haven::read_xpt()` / `haven::read_sas()` with config paths | No hardcoded paths |
| `%INCLUDE` | `source()` | Relative paths via config object |
| FILE STATUS codes | `tryCatch()` + condition handling | Every failure mode mapped |

### 7. MIGRATION NOTES Format

Every migrated R script **MUST** include a `MIGRATION NOTES` block at the end of the file. This block provides traceability, documents assumptions, and flags items requiring review.

**Required format:**

```r
# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    [List assumptions where SAS behavior was ambiguous]
# POTENTIAL NUMERICAL DIFFERENCES:
#    [List locations where R and SAS may produce different results]
# NO DIRECT R EQUIVALENT:
#    [List SAS functionality with no direct R equivalent and approved workaround]
# PACKAGE SELECTION RATIONALE:
#    [List packages selected and why]
# OPEN QUESTIONS:
#    [List questions requiring statistician review]
# ============================================================
```

**Rules:**

- This block is **mandatory** — every migrated `.R` file must include it
- Each section must be populated with specific, actionable content (not left as placeholder text)
- ASSUMPTIONS must list every instance where SAS documentation was ambiguous and a judgment call was made
- POTENTIAL NUMERICAL DIFFERENCES must list locations where R and SAS may produce different numeric results, including rounding, degrees-of-freedom calculations, sort stability, and default options
- NO DIRECT R EQUIVALENT must list any SAS functionality that has no one-to-one R counterpart and describe the approved workaround
- PACKAGE SELECTION RATIONALE must justify any package choice where multiple alternatives existed
- OPEN QUESTIONS must list unresolved items requiring statistician or programmer review before submission use

### 8. Naming Conventions for R Files

#### 8.1 File Naming

- Migrated R files use the **same base name** as the SAS source with a `.R` extension
  - Example: `ae_v1.sas` → `ae_v1.R`
  - Example: `demographics_v1.sas` → `demographics_v1.R`
  - Example: `WPCT-F.07.03.sas` → `WPCT-F.07.03.R`

- Utility functions may be **renamed for R idiom** where the SAS name references SAS-specific concepts:
  - `assert_macro_exist.sas` → `assert_function_exist.R`
  - `util_value_of_macro.sas` → `util_value_of_param.R`
  - `util_proc_template.sas` → `util_ggplot_theme.R`

#### 8.2 Function Naming

- SAS macros become **parameterized R functions**: `%macro name(param=default)` → `name <- function(param = default)`
- All macro parameters become **named function arguments** with matching defaults
- Use `snake_case` for function names and arguments (consistent with tidyverse style)
- Internal helper functions should be prefixed with a dot (`.helper_name`) or kept unexported

#### 8.3 Directory Structure

Migrated R files reside in a parallel `R/` directory structure adjacent to the SAS originals:

```
tested/SAS/AE/ae_v1.sas        →  tested/R/AE/ae_v1.R
tested/SAS/macros/ae_output.sas →  tested/R/macros/ae_output.R
tested/SAS/ZZ_Utilities/data_checks.sas → tested/R/utilities/data_checks.R
whitepapers/WPCT/WPCT-F.07.03.sas      → whitepapers/WPCT/WPCT-F.07.03.R
whitepapers/utilities/util_passfail.sas → whitepapers/utilities/R/util_passfail.R
lang/SAS/graph/KM/kmplot.sas           → lang/R/graph/kmplot.R
```

### 9. Configuration and Path Management

**No hardcoded file paths** are permitted in any R script.

All paths must be parameterized via function arguments or the centralized configuration file `config/migration_config.yaml`.

```r
# Load configuration
config <- yaml::read_yaml("config/migration_config.yaml")

# Access ADaM data
adsl <- haven::read_xpt(file.path(config$data_paths$adam_path, "adsl.xpt"))
adae <- haven::read_xpt(file.path(config$data_paths$adam_path, "adae.xpt"))

# Access reference data
exdosfrq <- readr::read_csv(config$reference_data$exposure_exdosfrq)

# Output paths
output_rtf <- file.path(config$output_paths$rtf_output_path, "ae_summary.rtf")
output_xlsx <- file.path(config$output_paths$excel_output_path, "ae_summary.xlsx")
```

**Configuration transformation from SAS:**

| SAS Pattern | R Equivalent |
|-------------|-------------|
| `%let data_path = /study/data;` | `config$data_paths$adam_path` (from YAML) |
| `libname adam "&data_path" access=readonly;` | `haven::read_xpt(file.path(config$data_paths$adam_path, "adsl.xpt"))` |
| `%include "&macros_path/ae_aggregate.sas";` | `source(file.path(config$r_source_paths$r_macros_path, "ae_aggregate.R"))` |
| `ods rtf file="&output_path/report.rtf";` | `r2rtf::write_rtf(tbl, file = file.path(config$output_paths$rtf_output_path, "report.rtf"))` |

### 10. Reproducibility

All R scripts in this repository require a reproducible package environment managed by `renv`.

- **Package lockfile**: All package versions are pinned in `renv.lock` at the repository root
- **Environment restore**: Users run `renv::restore()` to install exact package versions
- **R version target**: R >= 4.3.0
- **Bootstrap**: `.Rprofile` at the repository root bootstraps renv via `source("renv/activate.R")`
- **Adding packages**: When a new package is needed, install it, then run `renv::snapshot()` to update `renv.lock`

**Setup for new contributors:**

```r
# 1. Install R >= 4.3.0
# 2. Clone the repository
# 3. Open R in the repository root (renv activates automatically via .Rprofile)
# 4. Restore the package environment:
renv::restore()
# 5. Verify packages load:
library(dplyr)
library(haven)
library(Tplyr)
```

### 11. Validation Requirements

All migrated R scripts must satisfy **100% functional parity** with the corresponding SAS output — zero behavioral regressions. No statistical functionality may be added or removed beyond what the SAS script implements.

#### 11.1 Eight-Gate Validation Framework

Every migrated script must pass all eight validation gates:

| Gate | Name | Requirement |
|------|------|-------------|
| 1 | Functional Output Parity | Side-by-side comparison of SAS vs R output for every statistic, count, and formatted value in the TLF |
| 2 | Rounding and Precision Audit | Document every rounding location; use `janitor::round_half_up()` to align or justify deviation; zero undocumented differences |
| 3 | Missing Value Audit | List every variable with missing values, SAS handling, and R equivalent |
| 4 | Model Parameter Verification | For MMRM, logistic, survival: document covariance structure, df method, optimizer, convergence criteria |
| 5 | TLF Layout Verification | Confirm title lines, footnote lines, column headers, spanning headers, stub indentation match SAS ODS |
| 6 | Package Reproducibility | All packages pinned in `renv.lock`; clean `renv::restore()` on fresh R produces identical environment |
| 7 | Scope Matching | Confirm no statistical functionality added or removed vs SAS source; document features with no direct R equivalent |
| 8 | Migration Sign-Off Checklist | All above gates confirmed; traceability matrix complete (100% of SAS steps mapped to R equivalents) |

#### 11.2 Validation Scripts

Automated validation scripts are located in `tests/validation/`:

- `gate1_functional_parity.R` — Output parity comparison using `diffdf`
- `gate2_rounding_audit.R` — Rounding difference detection
- `gate3_missing_value_audit.R` — Missing value handling verification
- `gate4_model_parameters.R` — Model covariance, df method, optimizer verification
- `gate5_tlf_layout.R` — TLF title/footnote/header comparison
- `gate7_scope_matching.R` — Scope matching confirmation

#### 11.3 Unit Testing

- All migrated functions must have corresponding unit tests in `tests/testthat/`
- Tests use the `testthat` framework (version >= 3.2.0)
- Test files follow the naming convention `test_<module_name>.R`
- Run all tests: `testthat::test_dir("tests/testthat")`
