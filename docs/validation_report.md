# PhUSE WG5 SAS-to-R Migration — Validation Report

| Field | Value |
|-------|-------|
| **Document Version** | 1.0 |
| **Date** | [YYYY-MM-DD] |
| **Author** | PhUSE CS Working Group 5 (WG5) — Standard Analyses |
| **Purpose** | Documents the validation gate results for the SAS (9.4) → R (4.3+) migration of the `phuse-scripts` repository |
| **Scope** | All migrated SAS scripts across `tested/`, `whitepapers/`, `lang/`, `contributed/` domains |
| **Classification** | Validation Deliverable — 8-Gate Migration Framework |

---

## Introduction

This document presents the validation results for the migration of the PhUSE CS Working Group 5 (WG5) Standard Analyses repository (`phuse-scripts`) from SAS 9.4 to R 4.3+. The migration produces idiomatic R equivalents of all SAS programs — each SAS construct is understood statistically and operationally, then implemented as the correct R equivalent. This is not a line-by-line transliteration of SAS syntax.

Validation follows an **8-gate framework** ensuring:

1. **Functional parity** — numeric equivalence between SAS and R outputs
2. **Rounding consistency** — SAS round-half-up behavior preserved via `janitor::round_half_up()`
3. **Missing value integrity** — no implicit zero substitution; SAS `.` → `NA`, SAS `' '` → `NA_character_`
4. **Model parameter accuracy** — covariance structures, df methods, ties methods explicitly documented
5. **TLF layout fidelity** — titles, footnotes, column headers, indentation match SAS ODS output
6. **Package reproducibility** — all packages pinned in `renv.lock` with deterministic restore
7. **Scope matching** — 1:1 SAS-to-R file coverage with no added or removed functionality
8. **Final sign-off** — all gates confirmed, traceability matrix complete

All validation scripts reside in `tests/validation/` and use `testthat` (>= 3.2.0) + `diffdf` (>= 1.0.4) for automated checks. **No SAS runtime is required** for any validation gate — all verification runs against a local R environment with R-generated outputs compared to saved SAS baselines.

---

## Validation Gate Summary

| Gate | Name | Status | Automated Script | Key Metric |
|------|------|--------|-----------------|------------|
| 1 | Functional Output Parity | [PASS/FAIL] | `tests/validation/gate1_functional_parity.R` | 100% numeric equivalence |
| 2 | Rounding and Precision Audit | [PASS/FAIL] | `tests/validation/gate2_rounding_audit.R` | 0 undocumented rounding differences |
| 3 | Missing Value Audit | [PASS/FAIL] | `tests/validation/gate3_missing_value_audit.R` | 0 implicit zero substitutions |
| 4 | Model Parameter Verification | [PASS/FAIL] | `tests/validation/gate4_model_parameters.R` | All model specs explicitly documented |
| 5 | TLF Layout Verification | [PASS/FAIL] | `tests/validation/gate5_tlf_layout.R` | All layout elements matched |
| 6 | Package Reproducibility | [PASS/FAIL] | `renv.lock` + `renv::restore()` | Clean restore on fresh R installation |
| 7 | Scope Matching | [PASS/FAIL] | `tests/validation/gate7_scope_matching.R` | 1:1 SAS-to-R file coverage |
| 8 | Migration Sign-Off Checklist | [PASS/FAIL] | Manual review | All 7 gates confirmed |

> **Note:** Status fields are placeholders (`[PASS/FAIL]`) to be populated after each validation run is executed against production-representative CDISC datasets.

---

## Gate 1 — Functional Output Parity

### Objective

Side-by-side comparison of SAS vs R output for every statistic, count, and formatted value in every TLF (Table, Listing, Figure) produced by the migrated R scripts. Every SAS script must produce output that is numerically equivalent to the documented SAS baseline.

### Automated Script

`tests/validation/gate1_functional_parity.R`

### Methodology

- Uses `diffdf::diffdf()` for structured data frame comparison with configurable tolerance
- Loads SAS baseline outputs via `haven::read_xpt()` from configured data paths
- Compares **numeric columns** with a default tolerance of 1e-10 (configurable per domain)
- Compares **character columns** with exact match after trailing whitespace trimming (SAS pads character variables to declared length)
- Covers **all 6 tested domain panels** (AE, DM, DS, EX, LB, MedDRA), **WPCT Figures 7.1–7.8**, and **lang/contributed** scripts
- For each domain, the following comparison workflow executes:
  1. Load SAS baseline XPT data via `haven::read_xpt()`
  2. Execute the migrated R script to generate R output
  3. Align column names and types between SAS and R outputs
  4. Run `diffdf::diffdf()` with the configured tolerance
  5. Catalog any differences with column name, row index, SAS value, R value, and absolute difference
  6. Aggregate domain-level pass/fail results

### SAS-to-R Mapping Context

The following SAS construct transformations are verified for functional parity by this gate:

| SAS Construct | R Equivalent | Parity Check |
|--------------|-------------|--------------|
| `PROC FREQ` | Tplyr count layer | Denominator logic, frequency counts, percentages |
| `PROC FREQ EXACT FISHER` | `fisher.test()` | P-values with continuity correction |
| `PROC MEANS` / `PROC UNIVARIATE` | Tplyr desc layer / `dplyr::summarise()` | N, MEAN, STD, MEDIAN, Q1, Q3, MIN, MAX |
| `PROC LIFETEST` | `survival::survfit()` | Kaplan-Meier survival estimates |
| `PROC PHREG` | `survival::coxph(ties = "breslow")` | Hazard ratios, confidence intervals |
| `PROC GLM` | `car::Anova()` | ANCOVA F-statistics, p-values |
| `PROC REPORT` / `PROC TABULATE` | Tplyr + `r2rtf` | Formatted table values |
| `DATA step` merges | `dplyr::left_join()` / `inner_join()` | Merged row counts and values |

### Domain Coverage Matrix

| Domain | SAS Source | R Target | Comparisons |
|--------|-----------|----------|-------------|
| AE Severity | `tested/SAS/AE/ae_v1.sas`, `ae_v1upd.sas` | `tested/R/AE/ae_v1.R`, `ae_v1upd.R` | Frequencies, percentages, Fisher's exact p-values |
| AE Oncology | `tested/SAS/AE/ae_oncology_v1.sas`, `ae_oncology_v1upd.sas` | `tested/R/AE/ae_oncology_v1.R`, `ae_oncology_v1upd.R` | Oncology-specific aggregations, comparisons |
| Demographics | `tested/SAS/DM/demographics_v1.sas` | `tested/R/DM/demographics_v1.R` | Age/race distributions, descriptive statistics |
| Disposition | `tested/SAS/DS/disposition_v2.sas` | `tested/R/DS/disposition_v2.R` | Counts by arm, time-to-event, KM estimates |
| Exposure | `tested/SAS/EX/exposure_v1.sas` | `tested/R/EX/exposure_v1.R` | Retention curves, dose distributions, descriptive stats |
| Liver Labs | `tested/SAS/LB/liver_v2.sas` | `tested/R/LB/liver_v2.R` | ALT/AST/ALP/BILI, ULN multiples, DILI metrics |
| MedDRA | `tested/SAS/MedDRA/ae_meddra_w_flag_generation_v1.sas` | `tested/R/MedDRA/ae_meddra_w_flag_generation_v1.R` | SOC/HLGT/HLT/PT hierarchy, RD, RR, Fisher's exact |
| WPCT Figures | `whitepapers/WPCT/WPCT-F.07.01.sas` – `WPCT-F.07.08.sas` | `whitepapers/WPCT/WPCT-F.07.01.R` – `WPCT-F.07.08.R` | Boxplot statistics, ANCOVA p-values |

### Pass Criteria

- **ALL** domain panels achieve 100% numeric parity within the configured tolerance
- Zero character column mismatches after whitespace normalization
- Zero missing value count discrepancies between SAS and R outputs
- All `testthat::test_that()` blocks pass

### Results

[To be populated after validation run]

### Deviations

Known acceptable deviations that do not constitute failures:

- **Floating-point epsilon**: Differences at the 1e-15 level due to IEEE 754 representation are acceptable if within the configured tolerance
- **Sort order of tied rows**: SAS guarantees stable sort by key; R `dplyr::arrange()` is stable within groups but multi-key tie-breaking may differ — row reordering without value changes is acceptable
- **Trailing whitespace**: SAS pads character variables to declared length; R trims — this is handled by whitespace normalization

### Open Questions

- Exact tolerance thresholds per domain require statistician review (e.g., survival p-values may need wider tolerance than frequency counts)
- SAS baseline XPT file availability needs confirmation for all domain panels
- Whether formatted output comparison (e.g., `"12.3 (45.6%)"` strings) requires pixel-exact or value-exact matching

---

## Gate 2 — Rounding and Precision Audit

### Objective

Document every rounding location across all migrated R scripts; verify that `janitor::round_half_up()` is used to align with SAS round-half-up behavior, or document justified deviations; achieve zero undocumented rounding differences.

### Automated Script

`tests/validation/gate2_rounding_audit.R`

### Methodology

- **Source code scan**: Reads all migrated R files and scans for rounding function calls using `stringr::str_detect()` and `readr::read_lines()`
- **Classification**: Each rounding location is classified as:
  - `COMPLIANT` — Uses `janitor::round_half_up()` or `round_half_up()` (correct SAS-compatible behavior)
  - `JUSTIFIED` — Uses bare `round()` with documented justification in an inline comment (e.g., integer rounding where half-values cannot occur)
  - `VIOLATION` — Uses bare `round()` without any documented justification
- **Boundary value tests**: Verifies SAS-compatible behavior at critical divergence points:
  - `round_half_up(0.5, 0)` returns `1` (R default `round(0.5)` returns `0`)
  - `round_half_up(2.5, 0)` returns `3` (R default `round(2.5)` returns `2`)
  - `round_half_up(0.05, 1)` returns `0.1` (R default may return `0.0`)
  - `round_half_up(-0.5, 0)` returns `-1` (away from zero, matching SAS)
- **Implicit rounding detection**: Flags `sprintf()`, `formatC()`, and `format()` calls that perform implicit rounding during formatting

### SAS-to-R Rounding Context

SAS `ROUND()` implements **round-half-up** (also known as "arithmetic rounding"): when the digit to be dropped is exactly 5, it rounds away from zero. R's default `round()` implements **round-half-to-even** (also known as "banker's rounding"): when the digit is exactly 5, it rounds to the nearest even number.

This difference is **critical for regulatory submissions** where SAS and R outputs must match exactly. The `janitor::round_half_up()` function replicates SAS behavior.

**Example divergence points:**

| Value | SAS `ROUND()` | R `round()` | `round_half_up()` | Match? |
|-------|--------------|-------------|-------------------|--------|
| 0.5 | 1 | 0 | 1 | SAS ↔ round_half_up |
| 1.5 | 2 | 2 | 2 | All match |
| 2.5 | 3 | 2 | 3 | SAS ↔ round_half_up |
| -0.5 | -1 | 0 | -1 | SAS ↔ round_half_up |

### Pass Criteria

- Zero `VIOLATION`-classified rounding locations across all migrated R scripts
- All boundary value tests pass
- All `sprintf()`/`formatC()` implicit rounding locations documented

### Results

[To be populated after validation run]

### Deviations

- Locations where bare `round()` is justified (e.g., integer rounding where half-value cases provably cannot occur) are documented as `JUSTIFIED`
- `ceiling()`, `floor()`, and `trunc()` calls are cataloged for documentation but are not classified as rounding violations (they have deterministic behavior matching SAS equivalents)

### Open Questions

- Tolerance for floating-point boundary cases (e.g., `0.15` stored as `0.14999999...` in IEEE 754) requires statistician input
- Whether `sprintf("%.*f", ...)` implicit rounding should be classified as `VIOLATION` or `REVIEW` status

---

## Gate 3 — Missing Value Audit

### Objective

Catalog every variable with missing values across all ADaM datasets, document the SAS handling and the corresponding R equivalent, and verify that no implicit zero substitution occurs anywhere in the migrated R code.

### Automated Script

`tests/validation/gate3_missing_value_audit.R`

### Methodology

- **Dataset catalog**: Loads all ADaM datasets via `haven::read_xpt()` and catalogs missing values per variable:
  - `n_total` — total observations
  - `n_missing` — count of `NA` values via `sum(is.na(col))`
  - `pct_missing` — percentage missing
  - `col_type` — numeric or character
  - `has_tagged_na` — whether `haven::tagged_na()` values are present (SAS special missing `.A`–`.Z`)
  - `has_empty_string` — whether empty strings `""` exist in character columns (should be `NA_character_`)
- **Source code scan for zero-substitution anti-patterns**:
  - `replace_na(0)` or `replace_na(., 0)` — **VIOLATION**
  - `ifelse(is.na(x), 0, x)` or `if_else(is.na(x), 0, x)` — **VIOLATION**
  - `coalesce(x, 0)` — **VIOLATION** for clinical data unless documented
  - `tidyr::replace_na(list(col = 0))` — **VIOLATION**
  - `x[is.na(x)] <- 0` — **VIOLATION**
- **Missing handling verification**: Scans for correct patterns (`is.na()`, `sum(is.na())`, `NA_character_`, `haven::tagged_na()`)

### SAS-to-R Missing Value Mapping

| SAS Construct | R Equivalent | Rule |
|--------------|-------------|------|
| Numeric missing (`.`) | `NA` | Never `0`, never `NaN` |
| Character missing (`' '`) | `NA_character_` | Never empty string `""` |
| Special missing (`.A` – `.Z`) | `haven::tagged_na("A")` – `haven::tagged_na("Z")` | Preserve distinction if required by analysis |
| `MISSING(x)` | `is.na(x)` | Direct equivalent |
| `NMISS(x)` | `sum(is.na(x))` | For numeric vectors |
| `CMISS(x)` | `sum(is.na(x))` | For character vectors (same in R) |
| `CALL MISSING(a, b, c)` | `a <- NA; b <- NA; c <- NA` | Multiple assignment |

### Pass Criteria

- Zero empty strings (`""`) in character columns across all ADaM datasets loaded via `haven::read_xpt()`
- Zero undocumented zero-substitution patterns in migrated R source code
- Missing value counts match between SAS baselines and R outputs for every variable
- All `testthat::test_that()` blocks pass

### Results

[To be populated after validation run]

### Deviations

- Legitimate zero-replacement locations (e.g., cumulative count initialization at zero, denominator defaults) are documented with inline comments and classified as `JUSTIFIED`
- `na.rm = TRUE` usage in aggregate functions is cataloged and verified to match SAS PROC behavior (SAS excludes missing by default in most PROCs)

### Open Questions

- Which specific ADaM variables in this study use SAS special missing (`.A`–`.Z`) and therefore require `haven::tagged_na()` preservation?
- Are there legitimate zero-replacement locations that need documentation as exceptions?
- Should aggregate function `na.rm` behavior be verified per-PROC or globally?

---

## Gate 4 — Model Parameter Verification

### Objective

For all inferential statistical models (MMRM, survival, ANCOVA, logistic, Fisher's exact), document the covariance structure, degrees-of-freedom method, optimizer, convergence criteria, and ties methods to confirm they match SAS specifications.

### Automated Script

`tests/validation/gate4_model_parameters.R`

### Methodology

- **Source code scan** across all migrated R files for model function calls:
  - `mmrm::mmrm()` — MMRM model specifications
  - `survival::coxph()` — Cox proportional hazards
  - `survival::survfit()` — Kaplan-Meier survival estimates
  - `survival::survdiff()` — Log-rank/Wilcoxon test statistics
  - `car::Anova()` — Type II/III ANCOVA tests
  - `fisher.test()` — Fisher's exact test
  - `emmeans::emmeans()` — Estimated marginal means (LS means)
- **Prohibited package scan** — automatic FAIL if any of the following appear:
  - `library(lme4)` or `lme4::` — **PROHIBITED** for MMRM
  - `library(nlme)` or `nlme::` — **PROHIBITED** for MMRM
  - `glmer(` or `lmer(` — **PROHIBITED** function calls
- **Parameter extraction** for each model call:
  - MMRM: covariance structure (`us`, `ar1`, `toep`, `cs`, `ante`), df method (Satterthwaite/Kenward-Roger), optimizer
  - Survival: ties method (must be `"breslow"` to match SAS default), strata terms
  - ANCOVA: SS type (II or III), formula specification
  - Fisher's: continuity correction flag, confidence level

### SAS-to-R Model Mapping

| SAS Construct | R Equivalent | Critical Parameters |
|--------------|-------------|-------------------|
| `PROC MIXED` / `PROC GLIMMIX` | `mmrm::mmrm()` | Covariance structure (`us`/`ar1`/`toep`/`cs`/`ante`), df method (Satterthwaite/Kenward-Roger) |
| `PROC LIFETEST` | `survival::survfit()` | Ties method (Breslow is SAS default) |
| `PROC PHREG` | `survival::coxph(ties = "breslow")` | Ties method must be explicitly set to `"breslow"` |
| `PROC GLM` (Type III SS) | `car::Anova(type = "III")` | SS type must match SAS specification |
| `PROC FREQ` / `EXACT FISHER` | `fisher.test()` | Continuity correction per MedDRA requirements |
| `LSMEANS` / `ESTIMATE` / `CONTRAST` | `emmeans::emmeans()` | Backend model must be `mmrm` (not `lm`/`glm` for MMRM contexts) |

### Files Subject to Model Parameter Verification

| R File | Model Types | SAS Counterpart |
|--------|------------|----------------|
| `tested/R/DS/disposition_v2.R` | Survival (time-to-event) | `PROC LIFETEST` in `disposition_v2.sas` |
| `tested/R/EX/exposure_v1.R` | Survival (retention curves) | `PROC LIFETEST` in `exposure_v1.sas` |
| `tested/R/MedDRA/ae_meddra_w_flag_generation_v1.R` | Fisher's exact | `PROC FREQ EXACT FISHER` in MedDRA SAS |
| `whitepapers/WPCT/WPCT-F.07.03.R` – `WPCT-F.07.08.R` | ANCOVA | `PROC GLM` in WPCT SAS scripts |
| `lang/R/graph/kmplot.R` | Survival (KM + Cox) | `PROC LIFETEST` / `PROC PHREG` in `kmplot.sas` |
| `lang/R/graph/boxplot_shewhart.R` | ANCOVA | `PROC GLM` in `BoxplotShewhart_Vst.sas` |

### Pass Criteria

- Zero prohibited package usage (`lme4`, `nlme`, `glmer`) across all migrated R files
- All MMRM models use `mmrm` package with explicit covariance structure and df method
- All `coxph()` calls specify `ties = "breslow"` explicitly
- All `car::Anova()` calls specify the correct SS type (II or III) matching SAS PROC GLM
- Fisher's exact continuity correction matches SAS MedDRA source specification
- All `testthat::test_that()` blocks pass

### Results

[To be populated after validation run]

### Deviations

- MMRM convergence criteria may differ slightly between SAS PROC MIXED and the `mmrm` package due to different optimizer implementations (SAS uses Newton-Raphson by default; `mmrm` uses L-BFGS-B via TMB) — documented as acceptable if model results converge within tolerance
- Fisher's exact mid-p correction default differs between SAS (some contexts use mid-p) and R (`fisher.test()` does not use mid-p by default) — continuity correction from SAS source is applied explicitly

### Open Questions

- SAS PROC MIXED optimizer (Newton-Raphson) vs `mmrm` default optimizer alignment — do convergence paths differ?
- Whether Kenward-Roger or Satterthwaite df method is specified in the SAP for each model
- Exact MMRM convergence criteria matching between SAS and `mmrm` package needs verification against SAP

---

## Gate 5 — TLF Layout Verification

### Objective

Confirm that title lines, footnote lines, column headers, spanning headers, and stub indentation in R-generated TLF outputs match the corresponding SAS ODS output for every domain panel.

### Automated Script

`tests/validation/gate5_tlf_layout.R`

### Methodology

- **RTF layout extraction**: Reads R-generated RTF files (from `r2rtf`) and SAS ODS RTF baselines, parsing RTF control words to extract:
  - Title lines (header section text)
  - Footnote lines (footer section text)
  - Column headers (header row cell content)
  - Spanning headers (merged header cells)
  - Page orientation (`\landscape` keyword)
  - Font family and size (`\fonttbl`, `\fs` keywords)
  - Column widths (`\cellx` values)
- **Excel layout extraction**: Reads R-generated Excel workbooks (from `openxlsx`) and SAS SpreadsheetML baselines:
  - Header rows and column headers
  - Title text in merged cells
  - Footnote text in merged cells
  - Column widths and cell styles
- **Figure annotation extraction**: For WPCT figures (ggplot2 outputs):
  - Plot titles, subtitles, captions
  - Axis labels, legend labels
  - Reference line annotations
- **Match classification**: Each layout element is classified as:
  - `MATCH` — exact match between SAS and R
  - `CLOSE_MATCH` — whitespace-only differences (acceptable with documentation)
  - `MISMATCH` — substantive difference requiring investigation

### SAS-to-R Layout Mapping

| SAS Output Component | R Equivalent | Comparison Focus |
|---------------------|-------------|-----------------|
| ODS RTF page orientation | `r2rtf::rtf_page(orientation = "landscape")` | Portrait vs landscape |
| ODS RTF font | `r2rtf` `text_font = 1` (Times New Roman) | Font family match |
| ODS RTF column widths | `r2rtf` `col_rel_width` parameter | Proportional width ratios |
| ODS RTF titles | `r2rtf::rtf_title()` | Title text content |
| ODS RTF footnotes | `r2rtf::rtf_footnote()` | Footnote text content |
| ODS RTF page numbers | `r2rtf::rtf_page_header()` with `\pagenumber` | Page numbering presence |
| SpreadsheetML style gallery | `openxlsx::createStyle()` | Font, border, fill definitions |
| SpreadsheetML worksheets | `openxlsx::addWorksheet()` | Worksheet names, count |
| PROC SGRENDER plot annotations | `ggplot2` `labs()`, `annotate()` | Title, axis labels, annotations |

### Pass Criteria

- All required layout elements achieve `MATCH` or `CLOSE_MATCH` status
- `CLOSE_MATCH` items are documented with justification
- Zero `MISMATCH` items without documented resolution
- Column width proportions match within 5% tolerance (not pixel-exact)
- Font families match by name (Times New Roman)

### Results

[To be populated after validation run]

### Deviations

- **Whitespace differences**: Flagged as `CLOSE_MATCH` — SAS ODS may insert different whitespace padding than `r2rtf`
- **Column width matching**: Proportional match is verified (not absolute pixel values), as RTF rendering varies by viewer
- **Page break positions**: May differ between SAS ODS and `r2rtf` due to content flow differences — acceptable if content is complete

### Open Questions

- Spanning header comparison scope: should exact column span count be verified, or text content only?
- SAS ODS-specific style attributes (e.g., `style(header)={}` blocks) with no direct R equivalent — how to document?
- Should figure annotation comparison include coordinate positions or text content only?

---

## Gate 6 — Package Reproducibility

### Objective

All R packages used by the migration are pinned in `renv.lock` with exact versions. A clean `renv::restore()` on a fresh R 4.3+ installation must produce an identical environment.

### Automated Script

N/A — Verified via manual `renv::restore()` execution on a fresh R 4.3+ installation.

### Methodology

1. Verify `renv.lock` exists at the repository root and contains all required packages
2. Verify `.Rprofile` contains `source("renv/activate.R")` for automatic renv bootstrap
3. Execute `renv::restore(prompt = FALSE)` on a clean R environment
4. Confirm all packages install at their pinned versions without errors
5. Verify loaded package versions match `renv.lock` specifications

### Required Packages (Verified in `renv.lock`)

**Core Tidyverse:**

| Package | Pinned Version | Purpose |
|---------|---------------|---------|
| dplyr | 1.2.0 | Data manipulation (replaces DATA steps, PROC SQL) |
| tidyr | 1.3.2 | Data tidying (pivot, nest, reshape) |
| purrr | 1.2.1 | Functional programming (replaces SAS arrays, macro loops) |
| stringr | 1.6.0 | String manipulation (replaces SAS character functions) |
| lubridate | 1.9.5 | Date/time handling (replaces SAS date math) |
| forcats | 1.0.1 | Factor manipulation (replaces SAS format ordering) |
| haven | 2.5.5 | SAS data I/O (read_xpt, read_sas, write_xpt) |
| readr | 2.2.0 | Flat file I/O (CSV, delimited) |
| tibble | 3.3.1 | Enhanced data frames |
| ggplot2 | 4.0.2 | Visualization (replaces PROC SGPLOT/SGRENDER) |

**Clinical/Pharmaverse:**

| Package | Pinned Version | Purpose |
|---------|---------------|---------|
| admiral | 1.4.1 | CDISC ADaM derivations |
| Tplyr | 1.3.2 | Clinical summary tables with traceability |
| r2rtf | 1.3.0 | RTF/PDF output (replaces ODS RTF/PDF) |
| mmrm | 0.3.17 | FDA-aligned MMRM (replaces PROC MIXED) |
| survival | 3.8-6 | Survival analysis (replaces PROC LIFETEST/PHREG) |
| survminer | 0.5.2 | Survival visualization (KM plots) |

**Analysis and Output:**

| Package | Pinned Version | Purpose |
|---------|---------------|---------|
| car | 3.1-5 | ANOVA Type II/III (replaces PROC GLM) |
| emmeans | 2.0.2 | LS means (replaces LSMEANS statement) |
| janitor | 2.2.1 | SAS-compatible rounding (`round_half_up()`) |
| openxlsx | 4.2.8.1 | Excel output (replaces SpreadsheetML/PCFILES) |
| gt | 1.3.0 | Complex table rendering |
| gridExtra | 2.3 | Multi-panel figure layouts |
| patchwork | 1.3.2 | Advanced plot composition |

**Testing and Validation:**

| Package | Pinned Version | Purpose |
|---------|---------------|---------|
| testthat | 3.3.2 | Unit testing framework |
| diffdf | 1.1.2 | Data frame comparison (replaces PROC COMPARE) |
| withr | 3.0.2 | Temporary state management |

**Infrastructure:**

| Package | Pinned Version | Purpose |
|---------|---------------|---------|
| renv | (project-level) | Package reproducibility |
| cli | (see renv.lock) | User-facing messages |
| yaml | 2.3.12 | Configuration parsing |

### Pass Criteria

- `renv.lock` exists and contains all packages listed above
- Clean `renv::restore()` succeeds with zero errors on a fresh R 4.3+ installation
- `.Rprofile` contains `source("renv/activate.R")` bootstrap
- All loaded package versions match pinned versions exactly

### Results

[To be populated after validation run]

### Deviations

- None expected. If a pinned package version becomes unavailable from CRAN (e.g., archived), document the version change and pin the nearest available version.

### Open Questions

- None.

---

## Gate 7 — Scope Matching

### Objective

Confirm no statistical functionality was added or removed compared to the SAS source. Every SAS program has a corresponding R equivalent; every SAS PROC call has an R function counterpart; every SAS macro parameter maps to an R function argument.

### Automated Script

`tests/validation/gate7_scope_matching.R`

### Methodology

- **SAS source inventory**: Discovers all in-scope SAS files (`.sas`) across:
  - `tested/SAS/**/*.sas` — Domain panels, macros, utilities
  - `whitepapers/WPCT/*.sas` — WPCT figure scripts
  - `whitepapers/utilities/*.sas` — Utility macros
  - `whitepapers/ADaM/*.sas` — ADaM derivation macros
  - `lang/SAS/**/*.sas` — Language-specific scripts
  - `contributed/**/*.sas` — Contributed scripts
  - `whitepapers/qualification/**/*.sas` — Qualification harnesses
  - `whitepapers/scriptathons/**/*.sas` — Scriptathon archives
- **R target inventory**: Discovers all migrated R files (`.R`) across parallel directories:
  - `tested/R/**/*.R`, `whitepapers/WPCT/*.R`, `whitepapers/utilities/R/*.R`, `whitepapers/ADaM/R/*.R`, `lang/R/**/*.R`, `contributed/R/**/*.R`, `whitepapers/qualification/R/*.R`, `whitepapers/scriptathons/R/**/*.R`
- **Traceability matrix construction**: Maps each SAS file to its expected R counterpart using the naming convention defined in the migration plan
- **PROC-level coverage**: For each SAS PROC call, verifies a corresponding R function exists in the R counterpart
- **Macro-to-function mapping**: For each SAS `%macro name(param=default)`, verifies an R function `name <- function(param = default)` exists with matching parameters
- **Extra functionality scan**: Detects any R functionality not present in the SAS source (zero additions permitted)
- **YAML manifest pairing**: Verifies each `*_sas.yml` governance manifest has a corresponding `*_r.yml`

### Scope Reference

See `docs/migration_traceability.md` for the complete file-level SAS-to-R traceability matrix.

### Coverage Categories

| Category | SAS Scope | R Scope | Verification |
|----------|----------|---------|-------------|
| Tested Domain Panels | 9 SAS drivers | 9 R scripts | 1:1 file mapping |
| Shared Macros | 10 SAS macros | 10 R functions | 1:1 function mapping |
| Framework Utilities | 6 SAS utilities | 6 R utilities | 1:1 mapping |
| WPCT Figures | 8 SAS scripts | 8 R scripts | 1:1 mapping (2 UPDATE, 6 CREATE) |
| Utility Macros | 16+ SAS macros | 16+ R functions | 1:1 mapping |
| ADaM Derivation | 1 SAS macro | 1 R function | 1:1 mapping |
| Lang/SAS Scripts | 6 SAS scripts | 6 R scripts | 1:1 mapping |
| Contributed Scripts | Multiple SAS | Multiple R | 1:1 mapping |
| Qualification | 14+ SAS harnesses | 1 R consolidated | Consolidated into testthat |
| Scriptathon Archives | Multiple SAS | Multiple R | 1:1 mapping |

### Pass Criteria

- 100% SAS file coverage — every in-scope SAS file has a corresponding R file
- 100% PROC coverage — every SAS PROC call has an R function equivalent
- Zero added functionality — no R functions that don't correspond to SAS source
- Zero removed functionality — no SAS PROCs without R equivalents
- All macro parameters mapped to function arguments
- All `testthat::test_that()` blocks pass

### Results

[To be populated after validation run]

### Deviations

- **Obsolete SAS macros** (`assert_var_nonmissing.sas`, `obsolete_util_figure_out_label.sas`, `util_figure_out_label_v2.sas`) — documented as intentionally excluded from R migration if functionality is superseded
- **Qualification harness consolidation** — 14+ SAS qualification scripts consolidated into a single `qualification_harnesses.R` using `testthat` — scope is preserved, structure is simplified
- **SAS constructs with no direct R equivalent**: Documented in MIGRATION NOTES blocks of each affected R script with approved workarounds:
  - SAS PROC COMPARE → `diffdf::diffdf()` (scope-equivalent)
  - SAS PROC CONTENTS → `base::str()` + `dplyr::glimpse()` (scope-equivalent)
  - SAS PROC DATASETS → base file/object management functions
  - SAS macro conditional compilation → R `if`/`else` at runtime

### Open Questions

- Should obsolete SAS macros (`assert_var_nonmissing.sas`, `obsolete_util_*`) require R counterparts, or are they intentionally deprecated?
- Contributed scripts with community variants — should all map 1:1 or can they consolidate?
- How to handle SAS scripts with partial R implementations that existed before migration?

---

## Gate 8 — Migration Sign-Off Checklist

### Objective

All preceding gates (1–7) are confirmed as passing; the traceability matrix is 100% complete with every SAS step mapped to its R equivalent; MIGRATION NOTES blocks are present in every migrated R script.

### Verification Method

Manual review of Gates 1–7 results by designated reviewers.

### Sign-Off Checklist

| Item | Status | Reviewer | Date |
|------|--------|----------|------|
| Gate 1 — Functional Output Parity | [ ] CONFIRMED | [Name] | [Date] |
| Gate 2 — Rounding and Precision Audit | [ ] CONFIRMED | [Name] | [Date] |
| Gate 3 — Missing Value Audit | [ ] CONFIRMED | [Name] | [Date] |
| Gate 4 — Model Parameter Verification | [ ] CONFIRMED | [Name] | [Date] |
| Gate 5 — TLF Layout Verification | [ ] CONFIRMED | [Name] | [Date] |
| Gate 6 — Package Reproducibility | [ ] CONFIRMED | [Name] | [Date] |
| Gate 7 — Scope Matching | [ ] CONFIRMED | [Name] | [Date] |
| Traceability Matrix 100% Complete | [ ] CONFIRMED | [Name] | [Date] |
| MIGRATION NOTES Blocks Present | [ ] CONFIRMED | [Name] | [Date] |
| No SAS Runtime Required | [ ] CONFIRMED | [Name] | [Date] |

### Traceability Matrix Verification

- Complete traceability matrix: see `docs/migration_traceability.md`
- Every in-scope SAS file mapped to its R target
- Every SAS construct mapped to its R equivalent
- Version headers referenced by filename — SAS source code not reproduced

### MIGRATION NOTES Block Verification

Every migrated R script must contain a MIGRATION NOTES block at the end of the file with the following sections:

```
# ============================================================
# MIGRATION NOTES
# ============================================================
# ASSUMPTIONS:
#    [List assumptions made where SAS behavior was ambiguous]
# POTENTIAL NUMERICAL DIFFERENCES:
#    [List locations where R and SAS may produce different results]
# NO DIRECT R EQUIVALENT:
#    [List SAS functionality with no direct R equivalent and approved workaround]
# PACKAGE SELECTION RATIONALE:
#    [List packages selected and why when multiple options existed]
# OPEN QUESTIONS:
#    [List questions requiring statistician or programmer review]
# ============================================================
```

### Results

[To be populated after all gates pass]

### Final Approvers

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Migration Lead | [Name] | | [Date] |
| Statistical Reviewer | [Name] | | [Date] |
| QA Reviewer | [Name] | | [Date] |

---

## Appendix A: Running the Validation Suite

### Prerequisites

- R 4.3+ installed
- `renv` restored: `Rscript -e 'source("renv/activate.R"); renv::restore(prompt = FALSE)'`
- ADaM datasets placed in the configured data paths (see `config/migration_config.yaml`)
- All migrated R scripts executed to produce output files

### Execution Commands

```bash
# Step 1: Ensure renv is restored
Rscript -e 'source("renv/activate.R"); renv::restore(prompt = FALSE)'

# Step 2: Run individual validation gates
Rscript tests/validation/gate1_functional_parity.R
Rscript tests/validation/gate2_rounding_audit.R
Rscript tests/validation/gate3_missing_value_audit.R
Rscript tests/validation/gate4_model_parameters.R
Rscript tests/validation/gate5_tlf_layout.R
Rscript tests/validation/gate7_scope_matching.R

# Step 3: Review results
# Each gate script prints its status (PASS/FAIL) to stdout
# Detailed results are available in the returned result objects
```

### Configuration

All paths are configured via `config/migration_config.yaml`. Key configuration sections:

- `data_paths.adam_path` — Location of ADaM XPT baseline datasets
- `output_paths` — Location of R-generated output files
- `r_source_paths` — Directories containing migrated R source files
- `domain_settings` — Domain-specific tolerance thresholds and TLF specifications

### Notes

- Gate 6 (Package Reproducibility) does not have a dedicated R script — it is verified via `renv::restore()` execution
- Gate 8 (Migration Sign-Off) is a manual review process referencing Gates 1–7 results
- All gate scripts can be run independently in any order
- No SAS runtime is required for any gate

---

## Appendix B: Package Versions

The authoritative source for all package versions is `renv.lock` at the repository root. The table below summarizes the key packages pinned for this migration.

| Category | Package | Version | CRAN | Purpose |
|----------|---------|---------|------|---------|
| Core | dplyr | 1.2.0 | Yes | Data manipulation |
| Core | tidyr | 1.3.2 | Yes | Data tidying |
| Core | purrr | 1.2.1 | Yes | Functional programming |
| Core | stringr | 1.6.0 | Yes | String manipulation |
| Core | lubridate | 1.9.5 | Yes | Date/time handling |
| Core | forcats | 1.0.1 | Yes | Factor manipulation |
| Core | haven | 2.5.5 | Yes | SAS data I/O |
| Core | readr | 2.2.0 | Yes | Flat file I/O |
| Core | tibble | 3.3.1 | Yes | Data frames |
| Core | ggplot2 | 4.0.2 | Yes | Visualization |
| Clinical | admiral | 1.4.1 | Yes | ADaM derivations |
| Clinical | Tplyr | 1.3.2 | Yes | Clinical tables |
| Clinical | r2rtf | 1.3.0 | Yes | RTF output |
| Clinical | mmrm | 0.3.17 | Yes | MMRM models |
| Clinical | survival | 3.8-6 | Yes | Survival analysis |
| Clinical | survminer | 0.5.2 | Yes | Survival plots |
| Analysis | car | 3.1-5 | Yes | ANOVA Type II/III |
| Analysis | emmeans | 2.0.2 | Yes | LS means |
| Analysis | janitor | 2.2.1 | Yes | Rounding (round_half_up) |
| Output | openxlsx | 4.2.8.1 | Yes | Excel output |
| Output | gt | 1.3.0 | Yes | Table rendering |
| Output | gridExtra | 2.3 | Yes | Plot arrangement |
| Output | patchwork | 1.3.2 | Yes | Plot composition |
| Testing | testthat | 3.3.2 | Yes | Unit testing |
| Testing | diffdf | 1.1.2 | Yes | Data comparison |
| Testing | withr | 3.0.2 | Yes | Temporary state |
| Infra | yaml | 2.3.12 | Yes | Configuration |
| Infra | cli | (see lock) | Yes | Messages |
| Infra | renv | (project) | Yes | Reproducibility |

Total packages in `renv.lock`: **184** (including all transitive dependencies).

---

## Appendix C: Related Documents

| Document | Path | Purpose |
|----------|------|---------|
| Migration Traceability Matrix | `docs/migration_traceability.md` | Full SAS-to-R file mapping (Gate 8 deliverable) |
| Repository README | `README.md` | Repository overview, R setup instructions |
| R Programming Guidelines | `whitepapers/ProgrammingGuidelines.md` | R coding standards for migrated scripts |
| Migration Configuration | `config/migration_config.yaml` | Parameterized path configuration |
| Package Lockfile | `renv.lock` | Authoritative package version list (Gate 6) |
| R Environment Bootstrap | `.Rprofile` | renv activation script |

---

## Appendix D: SAS Qualification Framework Reference

The SAS qualification harnesses in `whitepapers/qualification/` provided the conceptual foundation for the R validation framework. The SAS approach used `%util_passfail` macro with test definition datasets created via `PROC SQL`, executing macro calls and comparing results against expected values.

The R migration replaces this framework with:

| SAS Component | R Equivalent |
|--------------|-------------|
| `%util_passfail` macro | `testthat::test_that()` + `expect_*()` assertions |
| `PROC SQL` test definition tables | R test fixtures (data frames defined in test scripts) |
| SAS `PASS`/`FAIL` string comparison | `testthat::expect_equal()` with tolerance |
| XML test result output | `testthat` JUnit XML reporter or console output |
| Test TEMPLATE (`test_TEMPLATE.sas`) | `testthat` test file template pattern |

This conversion maintains the same quality assurance rigor while using R-native testing conventions.

---

## Change Log

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | [YYYY-MM-DD] | PhUSE CS WG5 | Initial validation report structure |

---

*This document is a validation deliverable of the PhUSE CS Working Group 5 SAS-to-R migration project. All status fields marked `[PASS/FAIL]` are placeholders to be populated after validation execution. No SAS source code is reproduced in this document.*
