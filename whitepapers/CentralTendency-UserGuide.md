### PhUSE CS Central Tendency Standard Analyses
##### User & Contributor Guide
##### Version 0.1

PhUSE CS Central Tendency analyses and displays are described in detail in the [White Paper on Measures of Central Tendency (WPCT)](http://www.phusewiki.org/wiki/images/4/48/CSS_WhitePaper_CentralTendency_v1.0.pdf), which PhUSE have [published in the CS Deliverables Catalog](http://www.phuse.eu/CSS-deliverables.aspx)

#### Scope

Analyses and their SAS and R implementations are based on ADaM-compliant data sets.

Out of scope:
* ADaM conformance checks
* Analyses of tabulated data, SDTM

#### Participating & Contributing: Getting started

We maintain to-do lists in project directories:
* [Working Group 5 to-do list](http://github.com/phuse-org/phuse-scripts)
* [WPCT to-do list](http://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/WPCT/TODO.md)

Review the [Script Basics and Programming Conventions](#wpct-package-scripts--conventions), below.

##### SAS and R versions

* Standard SAS scripts use functionality in **SAS 9.4 M02** or later, utilizing ODS and GTL.
   See our [visual guide to the WPCT PhUSEboxplot GTL template](http://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/documentation/GTL_PhUSEboxplot_1_annotated.png).
   (For users limited to **SAS 9.2**, we provide a SAS 9.2 boxplot approach in the script [WPCT-F.07.01-sas92-QCshewhart.sas](http://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/WPCT/WPCT-F.07.01-sas92-QCshewhart.sas))

* Standard R scripts target **R 4.3.0** or later, using the following ecosystem:
   * **Tidyverse** packages for data manipulation and visualization: dplyr, tidyr, ggplot2, purrr, stringr, lubridate, forcats
   * **Pharmaverse** packages for clinical reporting: admiral (ADaM derivations), Tplyr (clinical tables), r2rtf (RTF/PDF output), mmrm (mixed models)
   * **haven** for SAS dataset I/O (`read_xpt()`, `read_sas()`)
   * **survival** + **survminer** for survival analysis (Kaplan-Meier, Cox PH models)
   * **renv** for reproducible package management — run `renv::restore()` to install exact package versions pinned in `renv.lock`

#### WPCT Package Scripts & Conventions

Each Central Tendency data display will have a same-name script in SAS and R in the [WPCT folder](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/WPCT). For example, SAS and R scripts for Fig. 7.1, Fig. 7.2, etc.

##### Script basics

  * We refer to these as "Standard Scripts" because they produce one of the standard displays enumerated in the white paper.
  
  * Standard scripts, by default, produce an example output based on PhUSE CS test data.
    * [CDISC publish these data on their website](http://www.cdisc.org/sdtmadam-pilot-project)
    * We have modified the CDISC data to enhance script testing, and [publish these modified data in github](http://github.com/phuse-org/phuse-scripts/tree/master/data/adam/cdisc)
  
  * Standard SAS scripts require our library of macros, which we [publish here in github](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/utilities)
  
  * Standard R scripts require the migrated R utility functions published in [`whitepapers/utilities/R/`](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/utilities/R). R scripts load dependencies via `library()` calls and `source()` for project-specific functions.
    * R scripts use a centralized configuration file (`config/migration_config.yaml`) for parameterized paths — no hardcoded file paths
    * All R scripts include a MIGRATION NOTES block documenting assumptions, potential numerical differences, and package selection rationale
  
  * R scripts read CDISC ADaM XPT files via `haven::read_xpt()`. SAS date values are converted using `as.Date(x, origin = "1960-01-01")`, and SAS-compatible rounding uses `janitor::round_half_up()`.
  
  * Scripts require that users set some parameters based on their particular computing environment and data
    * Scripts are organized to clearly isolate user settings and any pre-processing instructions (e.g., to subset data before performing analyses)
    * By default, scripts use the CDISC ADaM pilot data mentioned above, and include custom pre-processing code to abbreviate treatment labels and subset data to limit the number of outputs.

##### R Script Usage

  * Each WPCT figure has a paired R implementation in the [WPCT folder](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/WPCT): WPCT-F.07.01.R through WPCT-F.07.08.R.

  * R scripts use **ggplot2** for visualization, replacing the SAS PROC SGRENDER, PROC SGPLOT, and PROC SHEWHART approaches.

  * ANCOVA models in Figures 7.3 and later use `car::Anova()` or `stats::aov()`, replacing PROC GLM.

  * Paginated outputs use `facet_wrap()` or manual page splitting, replacing SAS pagination macros.

  * All R scripts load shared utility functions from [`whitepapers/utilities/R/`](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/utilities/R).

  * R output files (TIFF, JPEG, PNG, PDF, RTF) are designed to match the corresponding SAS output specifications.

##### Project conventions

  * The [whitepapers README file](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers) describes our directory structure.

  * Programming Guidelines for this project are in [our PhUSE Wiki](http://www.phusewiki.org/wiki/index.php?title=WG5_P02_Programming_Guidelines)

  * We outline the Qualification Process for this project [in our PhUSE Wiki, as well](http://www.phusewiki.org/wiki/index.php?title=WG5_Project_02#Qualification_Process)

  * We publish Test (Qualification) plans, scripts and results [in GitHub](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/qualification)
  
  * We publish an [Index of Standard Scripts in our GitHub Wiki](http://github.com/phuse-org/phuse-scripts/wiki/Standard-Script-Index)
  
  * We also publish separately an [Index of SAS Macros in our GitHub Wiki](http://github.com/phuse-org/phuse-scripts/wiki/Utility-Macro-Index-(SAS)). The SAS Standard Scripts requires these macros.

  * The migrated R utility functions are published in [`whitepapers/utilities/R/`](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/utilities/R) alongside the SAS macros in `whitepapers/utilities/`.

  * The migrated R qualification harnesses using testthat are in [`whitepapers/qualification/R/`](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/qualification/R).

  * R Programming Guidelines are documented in [ProgrammingGuidelines.md](http://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/ProgrammingGuidelines.md) in this directory.

  * The ADaM derivation R functions (using admiral) are in [`whitepapers/ADaM/R/`](http://github.com/phuse-org/phuse-scripts/tree/master/whitepapers/ADaM/R).
