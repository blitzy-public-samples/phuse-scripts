[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/phuse-org/phuse-scripts/blob/master/LICENSE.md) 
[Search](https://github.com/search/advanced)


## PhUSE CS Standard Analyses Working Group

#### Links to PhUSE Wiki Site

* PhUSE CS Standard Analyses Working Group (SAWG)<br/>
  [details available in the PhUSE Wiki](http://www.phusewiki.org/wiki/index.php?title=Standard_Scripts)
* SAWG's [Analysis and Display White Papers project](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_08) - Defining Standard Analyses
* SAWG's [Repository Governance & Infrastructure project](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_03) - Setting up our GitHub repository
* SAWG's [Repository content and delivery project](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_02) - Implementing Standard Analyses
* SAWG's [Script discovery and acquisition project](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_07)
* SAWG's [Communication, Promotion and Education project](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_07)

### Participating and Contributing

[**Our GitHub Wiki**](http://github.com/phuse-org/phuse-scripts/wiki/Current-Activities) contains latest information for active efforts.

We maintain Task Lists (to-do lists) for both:

  * [Working Group 5, overall](http://github.com/phuse-org/phuse-scripts/blob/master/TODO.md)
  * The [WPCT Standard Script Package](http://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/WPCT/TODO.md)

### Git and GitHub help

* [Git e-book](http://www.git-scm.com/book/en/v2) - note the helpful [references, such as the Visual Git Cheat Sheet](http://www.git-scm.com/docs).
* [GitHub for beginners](http://sixrevisions.com/resources/git-tutorials-beginners/)
* [Learn: Code Academy quick video tutorial](http://www.youtube.com/watch?v=0fKg7e37bQE)
* [Git Workflows - "Centralized" is a good starting point](http://www.atlassian.com/git/tutorials/comparing-workflows/centralized-workflow)
* [GitHub Workflow - Working with Forks](http://guides.github.com/activities/forking/)

### Various desktop clients for Git:
* [GitHub Clients](http://help.github.com/articles/set-up-git/)
* [Using Git in R/RStudio](http://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN)

---

## R Migration (SAS → R)

### Overview

This repository includes R implementations of SAS clinical reporting scripts, being migrated for use in regulated pharmaceutical environments. The foundation layer (infrastructure, core utilities, macros, and standalone scripts) has been migrated, with remaining domain panels and WPCT figures to follow in subsequent milestones. The R migration targets **100% functional parity** with the original SAS programs — every statistic, count, and formatted value produced by SAS is reproduced by the corresponding R script.

SAS source files are preserved in their original locations (e.g., `tested/SAS/`) alongside the new R equivalents (e.g., `tested/R/`). This side-by-side structure ensures full traceability between SAS originals and R implementations. A comprehensive traceability matrix is available in [`docs/migration_traceability.md`](docs/migration_traceability.md).

### R Setup Instructions

**Prerequisites:**

* [R](https://cran.r-project.org/) >= 4.3.0
* [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

**Restoring the reproducible environment:**

After cloning the repository, restore all pinned R packages using [`renv`](https://rstudio.github.io/renv/):

```r
# Clone the repository, then from the project root:
install.packages("renv")
renv::restore()
```

This reads `renv.lock` and installs all pinned packages at their exact versions, ensuring a fully reproducible analysis environment.

> **Note:** The `.Rprofile` file at the project root auto-bootstraps `renv` on R startup, so the package library is activated automatically when you open the project in RStudio or start R from the project directory.

Study-level paths, output directories, and other parameterized settings are configured in [`config/migration_config.yaml`](config/migration_config.yaml). Update this file to point to your local CDISC dataset locations before running any analysis scripts.

### Repository Structure (R)

The migrated R files are organized in a parallel directory structure alongside the original SAS programs:

| Directory | Description |
|-----------|-------------|
| `tested/R/` | Migrated domain panel R scripts (AE, DM, DS, EX, LB, MedDRA) |
| `tested/R/macros/` | Parameterized R functions replacing SAS analytical macros |
| `tested/R/utilities/` | Cross-panel framework utility functions |
| `whitepapers/WPCT/*.R` | Central Tendency figures in R (ggplot2-based) |
| `whitepapers/utilities/R/` | Migrated utility function library (assert and util families) |
| `whitepapers/ADaM/R/` | ADaM derivation functions using admiral |
| `lang/R/` | Language-specific R scripts (analysis, graph, report) |
| `contributed/R/` | Community-contributed R scripts |
| `tests/` | testthat unit tests and validation gate scripts |
| `config/` | Study-level YAML configuration |
| `docs/` | Migration traceability and validation documentation |

### Key R Packages

The migration leverages the [pharmaverse](https://pharmaverse.org/) ecosystem and other industry-standard R packages:

| Package | Purpose |
|---------|---------|
| **haven** | SAS data I/O — reads SAS transport (XPT) and SAS7BDAT files |
| **dplyr / tidyr / purrr** | Core data manipulation — replaces DATA steps, PROC SQL, and macro loops |
| **admiral** | CDISC ADaM derivation functions |
| **Tplyr** | Clinical summary tables with traceability — replaces PROC FREQ, PROC MEANS, PROC REPORT |
| **r2rtf** | RTF/PDF output generation — replaces ODS RTF/PDF destinations |
| **mmrm** | FDA-aligned mixed models for repeated measures (MMRM) |
| **survival / survminer** | Survival analysis and Kaplan-Meier visualization |
| **ggplot2** | Statistical visualization — replaces PROC SGPLOT/SGRENDER |
| **openxlsx** | Excel workbook generation — replaces SpreadsheetML XML and PCFILES engine |
| **janitor** | SAS-compatible rounding via `round_half_up()` |
| **car / emmeans** | ANOVA (Type II/III) and estimated marginal means for ANCOVA models |
| **testthat / diffdf** | Unit testing framework and data frame comparison for validation |

All package versions are pinned in `renv.lock` for reproducibility.

### Validation Framework

Every migrated R script is validated through an **8-gate validation framework** to ensure regulatory-grade quality:

| Gate | Description |
|------|-------------|
| **Gate 1** | **Functional Output Parity** — Side-by-side comparison of SAS vs R output for every TLF |
| **Gate 2** | **Rounding and Precision Audit** — Documents every rounding location; uses `round_half_up()` |
| **Gate 3** | **Missing Value Audit** — Verifies SAS missing (`.`) maps to `NA`, not zero |
| **Gate 4** | **Model Parameter Verification** — Confirms covariance structure, df method, optimizer for mixed models |
| **Gate 5** | **TLF Layout Verification** — Matches titles, footnotes, column headers, and indentation |
| **Gate 6** | **Package Reproducibility** — Clean `renv::restore()` reproduces the identical environment |
| **Gate 7** | **Scope Matching** — Confirms no statistical functionality added or removed vs SAS source |
| **Gate 8** | **Migration Sign-Off Checklist** — All gates confirmed; traceability matrix complete |

Automated validation gate scripts are located in [`tests/validation/`](tests/validation/). Detailed gate results are documented in [`docs/validation_report.md`](docs/validation_report.md), and the complete SAS-to-R traceability matrix is in [`docs/migration_traceability.md`](docs/migration_traceability.md).
