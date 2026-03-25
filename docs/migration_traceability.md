# PhUSE WG5 SAS-to-R Migration — Traceability Matrix

## Document Metadata

| Field | Value |
|-------|-------|
| **Document Version** | 1.0 |
| **Date** | 2026-03-25 |
| **Author** | PhUSE CS Working Group 5 (WG5) — Standard Analyses |
| **Purpose** | Maps every SAS source file to its R migration target, providing full traceability for the SAS 9.4 → R 4.3+ migration |
| **Scope** | All in-scope SAS programs as defined in the Agent Action Plan §0.3.1 |
| **Gate 8 Deliverable** | This matrix must achieve 100% coverage of all SAS-to-R transformation mappings |

---

## 1. Introduction

This document is the **SAS-to-R Traceability Matrix** for the PhUSE CS Working Group 5 (WG5) Standard Analyses repository (`phuse-scripts`). It serves as the authoritative Gate 8 deliverable for the complete migration of SAS clinical reporting scripts to fully operational R programs.

### 1.1 Migration Philosophy

- The migration converts SAS programs to **idiomatic R** — not a line-by-line transliteration of SAS syntax into R.
- Each SAS construct is **understood statistically and operationally**, then implemented as the correct R equivalent using the pharmaverse ecosystem and tidyverse conventions.
- SAS macros become **parameterized R functions** with named arguments and matching defaults.
- SAS DATA step processing becomes **dplyr pipelines**; PROC SQL becomes **dplyr verbs**.
- All file paths are **parameterized** via `config/migration_config.yaml` — no hardcoded paths.
- Package reproducibility is enforced via **`renv.lock`**.
- SAS source files are **not reproduced** in the R output. This traceability matrix references originals by filename and version header only.

### 1.2 Target R Package Stack

| Package | Purpose |
|---------|---------|
| haven | SAS data I/O (read_xpt, read_sas, write_xpt) |
| dplyr, tidyr, purrr, stringr | Core data manipulation replacing DATA steps and PROC SQL |
| lubridate, forcats | Date arithmetic and factor manipulation |
| admiral | CDISC ADaM derivations |
| Tplyr | Clinical summary tables with traceability |
| r2rtf | RTF/PDF output (ODS replacement) |
| openxlsx | Excel output (SpreadsheetML/PCFILES replacement) |
| mmrm | FDA-aligned mixed models (PROC MIXED replacement) |
| survival, survminer | Survival analysis (PROC LIFETEST/PHREG replacement) |
| ggplot2, gridExtra, patchwork | Visualization (PROC SGPLOT/SGRENDER replacement) |
| car, emmeans | ANCOVA and LS means |
| janitor | SAS-compatible rounding (round_half_up) |
| testthat, diffdf | Testing and validation |
| renv | Package reproducibility |

---

## 2. Migration Summary Statistics

| Metric | Count |
|--------|-------|
| **Total SAS source files in scope** | 60+ |
| **Total R target files (new or updated)** | 60+ |
| **Transformation type: CREATE** | ~58 |
| **Transformation type: UPDATE** | 2 |
| **Tested domain panels** | 6 (AE, DM, DS, EX, LB, MedDRA) |
| **Tested shared macros** | 10 |
| **Tested framework utilities** | 6 |
| **WPCT standard figures** | 8 |
| **Utility macros (whitepapers)** | 16+ |
| **ADaM derivation macros** | 1 |
| **Lang/SAS scripts** | 6 |
| **Contributed scripts** | 18 |
| **Qualification harnesses** | 1 (consolidated) |
| **Scriptathon archives** | 21 |
| **New infrastructure files** | 9 |

**Validation report**: See [`docs/validation_report.md`](validation_report.md) for gate-by-gate validation results.

---

## 3. Traceability Matrix — By Domain

Each section below maps SAS source files to their R migration targets. Tables include:

- **SAS Source File** — full path from repository root
- **SAS Version Header** — program name, author, and date extracted from the SAS file comment block (source code is NOT reproduced)
- **R Target File** — full path of the migrated R file
- **Transformation** — CREATE (new R file) or UPDATE (extend existing R file)
- **Key R Changes** — summary of the primary SAS-to-R transformations applied

---

### 3.1 Tested Domain Panel Drivers

These are production-grade SAS analytics with YAML governance contracts, migrated to parameterized R functions using the pharmaverse stack.

| SAS Source File | SAS Version Header | R Target File | Transformation | Key R Changes |
|---|---|---|---|---|
| `tested/SAS/AE/ae_v1.sas` | AE Severity Panel — Kretch, Anastassopoulos (2011-02-07) | `tested/R/AE/ae_v1.R` | CREATE | DATA step → dplyr; PROC FREQ → Tplyr count layer; SpreadsheetML → r2rtf/openxlsx; macro params → function args |
| `tested/SAS/AE/ae_v1upd.sas` | AE Severity Panel (updated) — Kretch, Anastassopoulos (2011-02-07) | `tested/R/AE/ae_v1upd.R` | CREATE | Richer parameterization; PROC FREQ EXACT FISHER → fisher.test(); format catalogs → haven labels |
| `tested/SAS/AE/ae_oncology_v1.sas` | AE Toxicity Panel — Kretch (2011-02-07) | `tested/R/AE/ae_oncology_v1.R` | CREATE | %aggregate/%compare → R functions; Excel XML → openxlsx workbook |
| `tested/SAS/AE/ae_oncology_v1upd.sas` | AE Toxicity Panel (updated) — Kretch (2011-02-07) | `tested/R/AE/ae_oncology_v1upd.R` | CREATE | Extended parameterization; macro chains → function composition |
| `tested/SAS/DM/demographics_v1.sas` | Demographics Analysis Panel — Dennis, Kretch (2009-12-29) | `tested/R/DM/demographics_v1.R` | CREATE | Age/race harmonization → dplyr mutate/case_when; PROC SUMMARY → Tplyr desc layer; PROC REPORT → Tplyr + r2rtf; disposition merges → left_join; Excel → openxlsx |
| `tested/SAS/DS/disposition_v2.sas` | Disposition Analysis Panel — Anastassopoulos (2009-12-09) | `tested/R/DS/disposition_v2.R` | CREATE | %ds_prelim_check → R assertion function; %ds_by_arm → group_by + summarise; %time_to_event → survival::Surv + survfit; JET/PCFILES → openxlsx; %ds_out → r2rtf |
| `tested/SAS/EX/exposure_v1.sas` | Exposure Analysis Panel — Anastassopoulos (2010-03-16) | `tested/R/EX/exposure_v1.R` | CREATE | 5 analyses → dplyr pipelines + ggplot2; PROC LIFETEST retention → survival::survfit; PROC MEANS → Tplyr |
| `tested/SAS/LB/liver_v2.sas` | Liver Lab Analysis Panel — Dennis (2009-12-29) | `tested/R/LB/liver_v2.R` | CREATE | ALT/AST/ALP/BILI filtering → dplyr filter; ULN multiples → mutate(x/uln); DILI/Hy's Law → admiral-style derivations; PCFILES → openxlsx |
| `tested/SAS/MedDRA/ae_meddra_w_flag_generation_v1.sas` | MedDRA at a Glance Panel — Kretch (2011-02-15) | `tested/R/MedDRA/ae_meddra_w_flag_generation_v1.R` | CREATE | SOC/HLGT/HLT/PT hierarchy → nested group_by; risk-difference → prop.test/Tplyr; Fisher's exact → fisher.test with continuity correction; PROC FREQ → Tplyr count layer |

---

### 3.2 Tested Shared Macros

Reusable analytical macros from `tested/SAS/macros/`, migrated as parameterized R functions.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `tested/SAS/macros/ae_aggregate.sas` | `tested/R/macros/ae_aggregate.R` | CREATE | %ab/%cd macros → R functions; MedDRA at-a-glance aggregation → dplyr group_by + summarise |
| `tested/SAS/macros/ae_meddra.sas` | `tested/R/macros/ae_meddra.R` | CREATE | %params/%meddra/%meddra_cmp → 3 R functions; hierarchical aggregation → nested dplyr; RD/RR → epitools or manual calculation |
| `tested/SAS/macros/ae_meddra_output.sas` | `tested/R/macros/ae_meddra_output.R` | CREATE | Excel worksheet generation → openxlsx::addWorksheet pipeline |
| `tested/SAS/macros/ae_oncology_aggregate.sas` | `tested/R/macros/ae_oncology_aggregate.R` | CREATE | %aggregate/%compare → R functions; oncology AE logic preserved |
| `tested/SAS/macros/ae_oncology_output.sas` | `tested/R/macros/ae_oncology_output.R` | CREATE | Oncology Excel workbook → openxlsx pipeline |
| `tested/SAS/macros/ae_output.sas` | `tested/R/macros/ae_output.R` | CREATE | AE severity Excel XML → openxlsx or r2rtf output |
| `tested/SAS/macros/ae_rror.sas` | `tested/R/macros/ae_rror.R` | CREATE | Odds ratios / relative risks → epitools::riskratio or manual fisher.test-based computation |
| `tested/SAS/macros/data_checks_disposition.sas` | `tested/R/macros/data_checks_disposition.R` | CREATE | Disposition data checks → R validation functions with tryCatch |
| `tested/SAS/macros/data_checks_exposure.sas` | `tested/R/macros/data_checks_exposure.R` | CREATE | Exposure data checks → R validation functions |
| `tested/SAS/macros/data_checks_liver.sas` | `tested/R/macros/data_checks_liver.R` | CREATE | Liver data checks → R validation functions |

---

### 3.3 Tested Shared Utilities (Framework Functions)

Cross-panel framework macros from `tested/SAS/ZZ_Utilities/`, migrated as R utility functions.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `tested/SAS/ZZ_Utilities/ae_setup.sas` | `tested/R/utilities/ae_setup.R` | CREATE | Gatekeeper validation → R assertion chain with informative cli errors |
| `tested/SAS/ZZ_Utilities/data_checks.sas` | `tested/R/utilities/data_checks.R` | CREATE | %chk_var/%chk_dm_subj_gt0/%chk_val/%chk_cmp → R check functions |
| `tested/SAS/ZZ_Utilities/err_output.sas` | `tested/R/utilities/err_output.R` | CREATE | %error_summary XML → openxlsx error workbook or tibble summary |
| `tested/SAS/ZZ_Utilities/md_output.sas` | `tested/R/utilities/md_output.R` | CREATE | Metadata worksheet → tibble/openxlsx metadata output |
| `tested/SAS/ZZ_Utilities/sl_gs_output.sas` | `tested/R/utilities/sl_gs_output.R` | CREATE | Grouping/subsetting metadata → R list/tibble structure |
| `tested/SAS/ZZ_Utilities/xml_output.sas` | `tested/R/utilities/xml_output.R` | CREATE | SpreadsheetML backbone (Excel XML Output Macros — Kretch) → openxlsx workbook pipeline with style gallery |

---

### 3.4 WPCT Standard Figures

Central Tendency white paper deliverables from `whitepapers/WPCT/`, migrated to ggplot2-based R scripts.

| SAS Source File | SAS Version Header | R Target File | Transformation | Key R Changes |
|---|---|---|---|---|
| `whitepapers/WPCT/WPCT-F.07.01.sas` | Figure 7.1 Box plot — Central Tendency (modified 2020-01-17) | `whitepapers/WPCT/WPCT-F.07.01.R` | UPDATE | Extend existing R boxplot to full SAS parity; verify ggplot2 output matches PROC SGRENDER; add MIGRATION NOTES |
| `whitepapers/WPCT/WPCT-F.07.02.sas` | Figure 7.2 Box plot — Change from Baseline — Central Tendency | `whitepapers/WPCT/WPCT-F.07.02.R` | UPDATE | Consolidate v01/v02 into canonical; verify parity with SAS |
| `whitepapers/WPCT/WPCT-F.07.03.sas` | Figure 7.3 Box plot — Observed Values and Change from Baseline — Central Tendency | `whitepapers/WPCT/WPCT-F.07.03.R` | CREATE | PROC GLM ANCOVA → car::Anova; PROC SUMMARY → dplyr; PROC SGRENDER → ggplot2 |
| `whitepapers/WPCT/WPCT-F.07.04.sas` | Figure 7.4 Box plot — Central Tendency (planned) | `whitepapers/WPCT/WPCT-F.07.04.R` | CREATE | Boxplot variant → ggplot2::geom_boxplot with reference lines |
| `whitepapers/WPCT/WPCT-F.07.05.sas` | Figure 7.5 Box plot — Central Tendency (planned) | `whitepapers/WPCT/WPCT-F.07.05.R` | CREATE | PhUSEboxplot GTL template → custom ggplot2 theme + geom_boxplot |
| `whitepapers/WPCT/WPCT-F.07.06.sas` | Figure 7.6 Box plot — Last/Min/Max Baseline and Post-baseline — Central Tendency | `whitepapers/WPCT/WPCT-F.07.06.R` | CREATE | PROC SGRENDER variation → ggplot2 + gridExtra/patchwork |
| `whitepapers/WPCT/WPCT-F.07.07.sas` | Figure 7.7 Box plot — Change from Last Baseline to Last Post-baseline — Central Tendency | `whitepapers/WPCT/WPCT-F.07.07.R` | CREATE | Paginated boxplots → ggplot2 + facet_wrap or manual page splitting |
| `whitepapers/WPCT/WPCT-F.07.08.sas` | Figure 7.8 Box plot — Last/Min/Max Baseline vs Post-baseline — Central Tendency | `whitepapers/WPCT/WPCT-F.07.08.R` | CREATE | Pagination macros → R pagination function + ggplot2 |

---

### 3.5 Utility Macro Library

Utility macros from `whitepapers/utilities/` (assert and util families), migrated as R helper functions.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `whitepapers/utilities/assert_complete_refds.sas` | `whitepapers/utilities/R/assert_complete_refds.R` | CREATE | SAS assertion (reference dset completeness check) → R stopifnot/cli_abort check |
| `whitepapers/utilities/assert_dset_exist.sas` | `whitepapers/utilities/R/assert_dset_exist.R` | CREATE | Dataset existence check → file.exists() or object existence check |
| `whitepapers/utilities/assert_depend_crumbs.sas` | `whitepapers/utilities/R/assert_depend_crumbs.R` | CREATE | Dependency assertion → R function/package existence check |
| `whitepapers/utilities/assert_var_exist.sas` | `whitepapers/utilities/R/assert_var_exist.R` | CREATE | Variable existence assertion → colnames() check |
| `whitepapers/utilities/assert_macro_exist.sas` | `whitepapers/utilities/R/assert_macro_exist.R` | CREATE | Macro existence → assert_function_exist.R (R function existence check via exists() + is.function()) |
| `whitepapers/utilities/util_boxplot_block_ranges.sas` | `whitepapers/utilities/R/util_boxplot_block_ranges.R` | CREATE | Block range calculation → R numeric computation |
| `whitepapers/utilities/util_axis_order.sas` | `whitepapers/utilities/R/util_axis_order.R` | CREATE | Axis ordering → R factor levels + ggplot2 scale manipulation |
| `whitepapers/utilities/util_count_unique_values.sas` | `whitepapers/utilities/R/util_count_unique_values.R` | CREATE | Unique value count → dplyr::n_distinct |
| `whitepapers/utilities/util_delete_dsets.sas` | `whitepapers/utilities/R/util_delete_dsets.R` | CREATE | Dataset deletion → R cleanup utility (rm / file.remove) |
| `whitepapers/utilities/util_get_reference.sas` | `whitepapers/utilities/R/util_get_reference.R` | CREATE | Reference line data → R tibble getter |
| `whitepapers/utilities/util_get_var_min_max.sas` | `whitepapers/utilities/R/util_get_var_min_max.R` | CREATE | Variable min/max → dplyr::summarise(min, max) |
| `whitepapers/utilities/util_labels_from_var.sas` | `whitepapers/utilities/R/util_labels_from_var.R` | CREATE | SAS labels → haven/attribute-based label extraction |
| `whitepapers/utilities/util_value_of_macro.sas` | `whitepapers/utilities/R/util_value_of_macro.R` | CREATE | Macro value resolution → util_value_of_param.R (R parameter getter) |
| `whitepapers/utilities/util_passfail.sas` | `whitepapers/utilities/R/util_passfail.R` | CREATE | PASS/FAIL testing → testthat expect_* wrappers |
| `whitepapers/utilities/util_proc_template.sas` | `whitepapers/utilities/R/util_proc_template.R` | CREATE | PhUSEboxplot GTL template registration → ggplot2 theme_phuse() |
| `whitepapers/utilities/util_boxplot_visit_ranges.sas` | `whitepapers/utilities/R/util_boxplot_visit_ranges.R` | CREATE | Visit range calculation → R date/visit computation |

---

### 3.6 ADaM Derivation Macros

ADaM-compliant derivation macros from `whitepapers/ADaM/`, migrated using the `admiral` package.

| SAS Source File | SAS Version Header | R Target File | Transformation | Key R Changes |
|---|---|---|---|---|
| `whitepapers/ADaM/derive_lastminmax_measure.sas` | Derived baseline & post-baseline obs (LAST/MIN/MAX change modes) | `whitepapers/ADaM/R/derive_lastminmax_measure.R` | CREATE | ADaM derivation macro → admiral::derive_var_extreme_flag + dplyr; LAST/MIN/MAX modes → admiral patterns; flag variables via keyword args |

---

### 3.7 Lang/SAS Migrations

Language-specific SAS scripts from `lang/SAS/`, migrated to idiomatic R equivalents.

| SAS Source File | SAS Version Header | R Target File | Transformation | Key R Changes |
|---|---|---|---|---|
| `lang/SAS/analysis/UCM072974/src/table7.1.1.1.sas` | Table 7.1.1.1 Deaths Listing (regulatory mortality listing from DM/EX data) | `lang/R/analysis/table7.1.1.1.R` | CREATE | DATA step merge → dplyr left_join; PROC REPORT → Tplyr + r2rtf |
| `lang/SAS/graph/KM/kmplot.sas` | Kaplan-Meier survival curves (inline data + PROC LIFETEST) | `lang/R/graph/kmplot.R` | CREATE | PROC LIFETEST → survival::survfit; PROC SGPLOT → survminer::ggsurvplot; ties method and stratification preserved |
| `lang/SAS/graph/boxplot/src/BoxplotShewhart_Vst.sas` | Boxchart with Summary Statistics — Laboratory Analysis (Shewhart boxplots) | `lang/R/graph/boxplot_shewhart.R` | CREATE | PROC SHEWHART → ggplot2 geom_boxplot; change-from-baseline → dplyr; ANCOVA p-values → car::Anova |
| `lang/SAS/report/doevents.sas` | DOEVENTS macro — event/population summary statistics | `lang/R/report/doevents.R` | CREATE | %doevents: event/population merge → dplyr; PROC REPORT → Tplyr + r2rtf |
| `lang/SAS/report/summary.sas` | SUMMARY macro — summary statistics with by variables | `lang/R/report/summary.R` | CREATE | %summary: PROC UNIVARIATE → dplyr summarise; PROC FREQ → Tplyr; comparative tests → t.test/wilcox.test |
| `lang/SAS/report/sas2xlsx/src/sas2xlsx.sas` | %sas2xlsx OOXML exporter | `lang/R/report/write_xlsx.R` | CREATE | %sas2xlsx OOXML → openxlsx::write.xlsx pipeline |

---

### 3.8 Contributed Scripts

Community contributed SAS scripts from `contributed/`, migrated to R equivalents.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `contributed/AE/ae_aggregate.sas` | `contributed/R/AE/AE_Severity/ae_aggregate.R` | CREATE | %ab/%cd macros → ae_ab()/ae_cd() R functions with dplyr aggregation |
| `contributed/AE/ae_oncology_aggregate.sas` | `contributed/R/AE/AE_Toxicity/ae_oncology_aggregate.R` | CREATE | %aggregate/%compare → onc_aggregate()/onc_compare() with dplyr + fisher.test |
| `contributed/AE/ZZ_Utilities/xml_output.sas` | `contributed/R/AE/ZZ_Utilities/xml_output.R` | CREATE | SpreadsheetML backbone → openxlsx workbook with style gallery |
| `contributed/AE/ZZ_Utilities/data_checks.sas` | `contributed/R/AE/ZZ_Utilities/data_checks.R` | CREATE | %chk_var/%chk_dm validation → R check functions with tryCatch |
| `contributed/AE/ZZ_Utilities/err_output.sas` | `contributed/R/AE/ZZ_Utilities/err_output.R` | CREATE | %error_summary XML → openxlsx error workbook |
| `contributed/AE/ZZ_Utilities/sl_gs_output.sas` | `contributed/R/AE/ZZ_Utilities/sl_gs_output.R` | CREATE | %group_subset macros → R group_subset_pp/xls_out/xml_out functions |
| `contributed/AE/ZZ_Utilities/ae_setup.sas` | `contributed/R/AE/ZZ_Utilities/ae_setup.R` | CREATE | %setup gatekeeper → ae_setup() with validation chain |
| `contributed/AE/meddra_import.sas` | `contributed/R/AE/meddra_import.R` | CREATE | MedDRA hierarchy import → haven + dplyr pipeline |
| `contributed/Demographics/Utility Programs/data_checks.sas` | `contributed/R/Demographics/data_checks.R` | CREATE | Demographic data validation → R check functions |
| `contributed/Demographics/Utility Programs/err_output.sas` | `contributed/R/Demographics/err_output.R` | CREATE | %error_summary → openxlsx error workbook |
| `contributed/Demographics/Utility Programs/xml_output.sas` | `contributed/R/Demographics/xml_output.R` | CREATE | SpreadsheetML → openxlsx workbook pipeline |
| `contributed/Demographics/Utility Programs/sl_gs_output.sas` | `contributed/R/Demographics/sl_gs_output.R` | CREATE | %group_subset macros → R grouping/subsetting functions |
| `contributed/MedDRA/ZZ_Utilities/xml_output.sas` | `contributed/R/MedDRA/xml_output.R` | CREATE | SpreadsheetML → openxlsx workbook pipeline |
| `contributed/MedDRA/ZZ_Utilities/data_checks.sas` | `contributed/R/MedDRA/data_checks.R` | CREATE | MedDRA data validation → R check functions |
| `contributed/MedDRA/ZZ_Utilities/err_output.sas` | `contributed/R/MedDRA/err_output.R` | CREATE | %error_summary → openxlsx error workbook |
| `contributed/MedDRA/ZZ_Utilities/sl_gs_output.sas` | `contributed/R/MedDRA/sl_gs_output.R` | CREATE | %group_subset macros → R grouping/subsetting functions |
| `contributed/MedDRA/ZZ_Utilities/ae_setup.sas` | `contributed/R/MedDRA/ae_setup.R` | CREATE | %setup gatekeeper → ae_setup() with validation chain |
| `contributed/MedDRA/meddra_import.sas` | `contributed/R/MedDRA/meddra_import.R` | CREATE | MedDRA hierarchy import → haven + dplyr pipeline |

---

### 3.9 Qualification Framework

PASS/FAIL validation harness scripts from `whitepapers/qualification/`, consolidated into testthat-based R harnesses.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `whitepapers/qualification/test_util_boxplot_block_ranges.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | SAS PASS/FAIL harnesses → testthat expect_* assertions |
| `whitepapers/qualification/test_assert_unique_keys.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_depend.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_complete_refds.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_var_nonmissing.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_util_axis_order.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_var_exist.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_util_access_test_data.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_dset_exist.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_assert_macro_exist.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_obsolete_util_boxplot_ranges.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Consolidated into unified R testthat harness |
| `whitepapers/qualification/test_TEMPLATE.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Template for new qualification tests → testthat template |
| `whitepapers/qualification/example_passfail_test_definitions.sas` | `whitepapers/qualification/R/qualification_harnesses.R` | CREATE | Example test definitions → testthat examples |

---

### 3.10 Scriptathon Archives

Scriptathon SAS archive entries from `whitepapers/scriptathons/`, migrated to R equivalents.

| SAS Source File | R Target File | Transformation | Key R Changes |
|---|---|---|---|
| `whitepapers/scriptathons/outliers/outliers_TEHiLo_Anytime.sas` | `whitepapers/scriptathons/R/outliers/outliers_TEHiLo_Anytime.R` | CREATE | Blood pressure scatter/shift → ggplot2 |
| `whitepapers/scriptathons/outliers/outliers_Scatter_Quant.sas` | `whitepapers/scriptathons/R/outliers/outliers_Scatter_Quant.R` | CREATE | Scatter quantile plots → ggplot2 |
| `whitepapers/scriptathons/outliers/outliers_TEAbnorm_Qual.sas` | `whitepapers/scriptathons/R/outliers/outliers_TEAbnorm_Qual.R` | CREATE | Treatment-emergent abnormality → ggplot2 |
| `whitepapers/scriptathons/outliers/outliers_Scatter_MeanTime.sas` | `whitepapers/scriptathons/R/outliers/outliers_Scatter_MeanTime.R` | CREATE | Scatter mean-time plots → ggplot2 |
| `whitepapers/scriptathons/outliers/outliers_Shift_Table.sas` | `whitepapers/scriptathons/R/outliers/outliers_Shift_Table.R` | CREATE | Shift tables → Tplyr + ggplot2 |
| `whitepapers/scriptathons/pk/pk_mean_conc.sas` | `whitepapers/scriptathons/R/pk/pk_mean_conc.R` | CREATE | PK mean concentration → ggplot2 |
| `whitepapers/scriptathons/pk/pk_param_summary.sas` | `whitepapers/scriptathons/R/pk/pk_param_summary.R` | CREATE | PK parameter summary → dplyr + Tplyr |
| `whitepapers/scriptathons/pk/pk_overlay_conc.sas` | `whitepapers/scriptathons/R/pk/pk_overlay_conc.R` | CREATE | PK overlay concentration → ggplot2 |
| `whitepapers/scriptathons/pk/pk_subj_conc.sas` | `whitepapers/scriptathons/R/pk/pk_subj_conc.R` | CREATE | PK subject concentration → ggplot2 |
| `whitepapers/scriptathons/ae/ae_serious.sas` | `whitepapers/scriptathons/R/ae/ae_serious.R` | CREATE | Serious AE analysis → Tplyr + ggplot2 |
| `whitepapers/scriptathons/ae/ae_common.sas` | `whitepapers/scriptathons/R/ae/ae_common.R` | CREATE | Common AE analysis → Tplyr + ggplot2 |
| `whitepapers/scriptathons/ae/ae_pref.sas` | `whitepapers/scriptathons/R/ae/ae_pref.R` | CREATE | Preferred term AE analysis → Tplyr + ggplot2 |
| `whitepapers/scriptathons/central/mean_time.sas` | `whitepapers/scriptathons/R/central/mean_time.R` | CREATE | Mean-time plots → ggplot2 (extend existing R entries) |
| `whitepapers/scriptathons/central/Box_Plot_Baseline.sas` | `whitepapers/scriptathons/R/central/Box_Plot_Baseline.R` | CREATE | Baseline boxplots → ggplot2 (extend existing R entries) |
| `whitepapers/scriptathons/central/box_obs_time.sas` | `whitepapers/scriptathons/R/central/box_obs_time.R` | CREATE | Observed-over-time boxplots → ggplot2 |
| `whitepapers/scriptathons/demographics/demo_summary.sas` | `whitepapers/scriptathons/R/demographics/demo_summary.R` | CREATE | Demographic summary tables → Tplyr + r2rtf (extend existing R entries) |
| `whitepapers/scriptathons/demographics/disposition_a.sas` | `whitepapers/scriptathons/R/demographics/disposition_a.R` | CREATE | Disposition variant A → dplyr + Tplyr |
| `whitepapers/scriptathons/demographics/disposition_b1.sas` | `whitepapers/scriptathons/R/demographics/disposition_b1.R` | CREATE | Disposition variant B1 → dplyr + Tplyr |
| `whitepapers/scriptathons/demographics/disposition_b2.sas` | `whitepapers/scriptathons/R/demographics/disposition_b2.R` | CREATE | Disposition variant B2 → dplyr + Tplyr |
| `whitepapers/scriptathons/demographics/discontinuation.sas` | `whitepapers/scriptathons/R/demographics/discontinuation.R` | CREATE | Discontinuation analysis → dplyr + Tplyr |
| `whitepapers/scriptathons/demographics/medication_listing.sas` | `whitepapers/scriptathons/R/demographics/medication_listing.R` | CREATE | Medication listing → dplyr + r2rtf |

---

## 4. R Governance Manifests

SAS YAML governance manifests are mapped to corresponding R manifests for each tested domain panel.

| SAS Manifest | R Manifest | Key Fields |
|---|---|---|
| `tested/SAS/AE/ae_v1upd_sas.yml` | `tested/R/AE/ae_v1upd_r.yml` | Language: R, Runtime: R 4.3+, Script: ae_v1upd.R |
| `tested/SAS/AE/ae_oncology_v1upd_sas.yml` | `tested/R/AE/ae_oncology_v1upd_r.yml` | Language: R, Runtime: R 4.3+ |
| `tested/SAS/DM/demographics_v1_sas.yml` | `tested/R/DM/demographics_v1_r.yml` | Language: R, Runtime: R 4.3+ |
| `tested/SAS/DS/disposition_v2_sas.yml` | `tested/R/DS/disposition_v2_r.yml` | Language: R, Runtime: R 4.3+ |
| `tested/SAS/EX/exposure_v1_sas.yml` | `tested/R/EX/exposure_v1_r.yml` | Language: R, Runtime: R 4.3+ |
| `tested/SAS/LB/liver_v2_sas.yml` | `tested/R/LB/liver_v2_r.yml` | Language: R, Runtime: R 4.3+ |
| `tested/SAS/MedDRA/ae_meddra_w_flag_generation_v1_sas.yml` | `tested/R/MedDRA/ae_meddra_v1_r.yml` | Language: R, Runtime: R 4.3+ |

---

## 5. SAS Construct → R Equivalent Reference

This section provides the authoritative mapping of SAS constructs to their R equivalents used consistently across all migrated scripts. Each mapping references the validation gate that verifies the transformation.

| SAS Construct | R Equivalent | Validation Gate |
|---|---|---|
| DATA step (merge, array, retain) | dplyr pipelines (left_join, across, purrr::accumulate/dplyr::lag) | Gate 1 |
| PROC SQL | dplyr verbs (left_join, summarise, filter) | Gate 1 |
| SAS macros (`%macro name(param=default)`) | R functions (`name <- function(param = default)`) | Gate 7 |
| PROC MIXED / PROC GLIMMIX | mmrm::mmrm() — NOT lme4/nlme/glmer | Gate 4 |
| PROC LIFETEST / PROC PHREG | survival::survfit / survival::coxph(ties = "breslow") | Gate 4 |
| PROC FREQ (EXACT FISHER) | Tplyr / fisher.test() with continuity correction | Gates 1, 4 |
| PROC MEANS / PROC UNIVARIATE | Tplyr desc layer / dplyr summarise | Gate 1 |
| PROC REPORT / PROC TABULATE | Tplyr + r2rtf | Gates 1, 5 |
| ODS RTF / ODS PDF | r2rtf (rtf_page, rtf_title, rtf_footnote, write_rtf) | Gate 5 |
| SpreadsheetML XML | openxlsx (createStyle, addWorksheet, writeData, saveWorkbook) | Gate 5 |
| PCFILES / JET engine | openxlsx (direct Excel writes) | Gate 5 |
| SAS formats / informats | haven labels, forcats::fct_relevel | Gate 1 |
| SAS ROUND() — round half-up | janitor::round_half_up() | Gate 2 |
| Numeric missing (`.`) | `NA` — never 0, never NaN | Gate 3 |
| Character missing (`' '`) | `NA_character_` — never empty string `""` | Gate 3 |
| SAS date math (days from 1960-01-01) | `as.Date(x, origin = "1960-01-01")` | Gate 1 |
| SAS datetime (seconds from 1960-01-01) | `as.POSIXct(x, origin = "1960-01-01")` | Gate 1 |
| RETAIN statement | purrr::accumulate / dplyr::lag | Gate 1 |
| BY-group processing | group_by + arrange (sort order established before grouping) | Gate 1 |
| `%include` | `source()` or `library()` | N/A |
| `libname` | `haven::read_xpt()` / `haven::read_sas()` with config paths | N/A |
| PUT / INPUT functions | `format()`, `as.numeric()`, `as.character()` — no silent truncation | Gate 1 |
| FILE STATUS codes | `tryCatch()` + condition handling | Gate 1 |
| PROC GLM (ANCOVA) | car::Anova (Type II/III tests) + emmeans | Gate 4 |
| PROC SGPLOT / PROC SGRENDER / PROC SHEWHART | ggplot2 (geom_boxplot, geom_point, custom themes) | Gate 5 |
| GTL PhUSEboxplot template | ggplot2 theme_phuse() custom theme | Gate 5 |

---

## 6. Infrastructure Files (New — No SAS Source)

These files support the R migration infrastructure and have no SAS counterparts.

| R Target File | Purpose | Validation Gate |
|---|---|---|
| `renv.lock` | Package lockfile — pins all R packages with exact versions for reproducibility | Gate 6 |
| `.Rprofile` | renv bootstrap — `source("renv/activate.R")` | Gate 6 |
| `config/migration_config.yaml` | Parameterized paths, study settings, output directories (replaces all SAS `%let`/`libname` globals) | N/A (infrastructure) |
| `tests/validation/gate1_functional_parity.R` | Automated SAS vs R output comparison | Gate 1 |
| `tests/validation/gate2_rounding_audit.R` | Rounding difference detection and documentation | Gate 2 |
| `tests/validation/gate3_missing_value_audit.R` | Missing value handling verification | Gate 3 |
| `tests/validation/gate4_model_parameters.R` | Model covariance, df method, optimizer verification | Gate 4 |
| `tests/validation/gate5_tlf_layout.R` | TLF title/footnote/header/indentation comparison | Gate 5 |
| `tests/validation/gate7_scope_matching.R` | Scope matching confirmation — programmatically verifies this matrix | Gate 7 |

---

## 7. Validation Gate Summary

All migrated scripts must satisfy the following 8-gate validation framework. Each gate references the construct mapping and traceability entries above.

| Gate | Name | Description | Key Deliverable |
|---|---|---|---|
| Gate 1 | Functional Output Parity | Side-by-side comparison of SAS vs R output for every statistic, count, and formatted value | `tests/validation/gate1_functional_parity.R` |
| Gate 2 | Rounding and Precision Audit | Document every rounding location; use `janitor::round_half_up()` to align or justify deviation | `tests/validation/gate2_rounding_audit.R` |
| Gate 3 | Missing Value Audit | List every variable with missing values, SAS handling, and R equivalent | `tests/validation/gate3_missing_value_audit.R` |
| Gate 4 | Model Parameter Verification | For MMRM, logistic, survival: document covariance structure, df method, optimizer, convergence | `tests/validation/gate4_model_parameters.R` |
| Gate 5 | TLF Layout Verification | Confirm title lines, footnote lines, column headers, spanning headers, stub indentation match SAS ODS | `tests/validation/gate5_tlf_layout.R` |
| Gate 6 | Package Reproducibility | All packages pinned in `renv.lock`; clean `renv::restore()` produces identical environment | `renv.lock` |
| Gate 7 | Scope Matching | Confirm no statistical functionality added or removed vs SAS source | `tests/validation/gate7_scope_matching.R` |
| Gate 8 | Migration Sign-Off | All above gates confirmed; traceability matrix 100% complete | This document |

---

## 8. Out-of-Scope Items

The following items are explicitly out of scope for this migration, per the project charter:

- **ADaM dataset specifications** — migration handles derivation code only, not ADaM spec changes
- **SAP amendments** — Statistical Analysis Plan amendments are outside migration scope
- **Regulatory submission strategy** — filing approach decisions are excluded
- **Non-SAS languages** — `lang/Julia/`, `lang/PL_SQL/`, `lang/HTML/` directories are untouched
- **Adding statistical functionality** — no functionality beyond what the SAS script implements
- **SAS runtime dependency** — all validation runs against a local R environment
- **Base R equivalents when tidyverse exists** — tidyverse is preferred over base R
- **lme4/nlme/glmer for MMRM** — `mmrm` package is mandated for mixed models

---

## 9. Related Documents

| Document | Purpose |
|----------|---------|
| [`docs/validation_report.md`](validation_report.md) | Gate-by-gate validation results |
| [`README.md`](../README.md) | Repository overview with R migration instructions |
| [`whitepapers/ProgrammingGuidelines.md`](../whitepapers/ProgrammingGuidelines.md) | R programming guidelines alongside SAS guidelines |
| [`config/migration_config.yaml`](../config/migration_config.yaml) | Centralized parameterized configuration |
| [`renv.lock`](../renv.lock) | Package reproducibility lockfile |

### Automated Verification

The traceability matrix coverage is programmatically verified by [`tests/validation/gate7_scope_matching.R`](../tests/validation/gate7_scope_matching.R), which:

1. Enumerates all in-scope SAS source files in the repository
2. Checks that each SAS file has a corresponding R target entry in this matrix
3. Verifies no out-of-scope files (Julia, PL/SQL, HTML) are included
4. Confirms transformation types (CREATE/UPDATE) are consistent with the migration plan
5. Reports any gaps in coverage

---

## 10. Change Log

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2026-03-25 | PhUSE CS WG5 | Initial traceability matrix — complete SAS-to-R migration mapping |

---

## 11. Sign-Off (Gate 8)

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Migration Architect | _________________ | 2026-03-25 | _________________ |
| Lead Statistician | _________________ | 2026-03-25 | _________________ |
| QA Reviewer | _________________ | 2026-03-25 | _________________ |
| Regulatory Lead | _________________ | 2026-03-25 | _________________ |

**Gate 8 Confirmation**: All gates (1–7) have been satisfied. This traceability matrix is 100% complete with all SAS steps mapped to R equivalents. No statistical functionality has been added or removed.
