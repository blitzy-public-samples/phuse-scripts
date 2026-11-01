# Extracted SAS specification: WPCT-F.07.02 and WPCT-F.07.07

This document is the build contract for the Python port in `python/wpct/`. It records every population
rule, record filter, transformation, statistical model and macro dependency of
`whitepapers/WPCT/WPCT-F.07.02.sas` and then of `whitepapers/WPCT/WPCT-F.07.07.sas`, each with its SAS
source and one disposition: ported to a named Python function, or excluded with a reason. Python
docstrings and test names in `python/wpct/wpct/` and `python/wpct/tests/` quote the item IDs below, so
the IDs are stable identifiers and are never renumbered or renamed. The document quotes a SAS number
only where it appears verbatim in a cited SAS listing or log. It claims no equivalence between SAS and
Python; that is established only by the comparisons in `python/wpct/reports/equivalence_report.md`.

## Conventions

### Item format

Every item is a level-4 heading whose first token is its ID, followed by these fields:

| Field | Content |
|-------|---------|
| `Category:` | One code from the category legend below |
| `SAS source:` | `[file:lines]` plus the macro or step |
| `Rule:` | The SAS behaviour, stated as a rule, with any reference-output evidence |
| `Disposition:` | `Ported → <module.function>`, `Excluded — <reason>`, or `Partly ported → … ; excluded — …` |
| `Relies on:` | The Part C entries the port depends on, or `—` |
| `Port notes:` | Optional: the Python behaviour fixed for edge states |

Module names are relative to the package `python/wpct/wpct/`: `selection.flag_filter` is
`wpct.selection.flag_filter`. Entries in Parts C to E use the same heading form (`#### D-01 — …`) with
fields suited to their kind.

### Categories

| Code | Meaning |
|------|---------|
| `POP` | Population rule |
| `FLT` | Record filter, including halting checks (marked "halting") |
| `TRN` | Transformation |
| `MOD` | Statistical model or summary statistic |
| `DEP` | Dependency: one item per macro a program reaches |
| `PRS` | Presentation-only step |

### Identifiers

| Pattern | Scope |
|---------|-------|
| `S02-<CAT>-nn` | Steps of `whitepapers/WPCT/WPCT-F.07.02.sas` |
| `S07-<CAT>-nn` | Steps of `whitepapers/WPCT/WPCT-F.07.07.sas` |
| `LMM-<CAT>-nn` | Steps of `%derive_lastminmax_measure` as called by 07.07 |
| `S02-DEP-<macro>`, `S07-DEP-<macro>` | One macro reached by that program; macro name in lower case, without `%` |
| `D-nn` | SAS-versus-Python default difference (Part C) |
| `ENV-nn` | Legacy environment assumption (Part D) |
| `DIV-nn`, `U-nn` | Divergence and unverified SAS behaviour (Part E) |

### Citations

`[path:a-b]` cites lines a to b, counted from 1, of the file at upstream commit
`398a6d33ced9359ffb58c46650a6d488811176b1`. Paths are repository-relative. The SAS programs and macros
are stored with CRLF line endings; line numbers count lines, so they are the same after LF
normalisation. A `.docx` citation names the XML part and the paragraph.

### Disposition rule

A SAS step or macro is ported if and only if its effect reaches a selected record set, an exported
intermediate dataset, a PROC SUMMARY statistic or a GLM `SOLUTION` row, or it halts processing on a data
condition. A step whose effect is limited to graphics, titles, labels, display formats, file paths, the
SAS session environment, macro-argument text handling or WORK cleanup is excluded. Every disposition
below follows this rule. A macro with effects on both sides is "Partly ported" and names both parts.

Where SAS and a Python library differ by default, the port follows SAS, and Part C records the
difference.

## Sources

### Commit and SAS version

- Upstream commit: `398a6d33ced9359ffb58c46650a6d488811176b1`. Every citation refers to it.
- SAS version of the reference outputs: SAS 9.4 (TS1M2) with SAS/STAT 13.2, on the X64_7PRO platform,
  Windows 6.1.7601 [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:2-17;
  whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:2-17].
- Both manifests declare SAS version 9.4M2 [whitepapers/WPCT/WPCT-F.07.02_sas.yml:21-23;
  whitepapers/WPCT/WPCT-F.07.07_sas.yml:14-16].

### Files read

| Path | Role |
|------|------|
| `whitepapers/WPCT/WPCT-F.07.02.sas` | Source of the S02 items (Part A) |
| `whitepapers/WPCT/WPCT-F.07.07.sas` | Source of the S07 items (Part B) |
| `whitepapers/WPCT/WPCT-F.07.02_sas.yml`, `whitepapers/WPCT/WPCT-F.07.07_sas.yml` | Metadata only (SAS version; ENV-08) |
| `whitepapers/ADaM/derive_lastminmax_measure.sas` | Source of the LMM items (Part B) |
| `whitepapers/utilities/assert_depend.sas`, `assert_continue.sas`, `assert_var_nonmissing.sas`, `assert_unique_keys.sas`, `assert_dset_exist.sas`, `assert_var_exist.sas`, `assert_macro_exist.sas`, `util_resolve_sasautos.sas`, `util_labels_from_var.sas`, `util_count_unique_values.sas`, `util_get_var_min_max.sas`, `util_value_format.sas`, `util_delete_dsets.sas`, `util_proc_template.sas`, `util_boxplot_block_ranges.sas`, `util_axis_order.sas`, `util_access_test_data.sas` (all in `whitepapers/utilities/`) | The 17 repository macros of the dependency closure (DEP items) |
| `data/adam/cdisc/advs.xpt` | 07.02 input: 32139 rows × 44 variables, matching the SAS log [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:31-32] |
| `data/adam/cdisc-split/advs.xpt` | 07.07 input: 32139 rows × 44 variables, matching the SAS log [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:31-32] |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst`, `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst` | GLM ParameterEstimates tables, rounded for display |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log`, `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log` | Dataset record counts, assertion results and derivation warnings |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf`, `…_SYSBP_Change_by_visit_for_timepoint_816.pdf`, `whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_815.pdf`, `…_817.pdf` | Box plots; their statistics are raster images only and are not used as references |
| `whitepapers/specification/WPCT_Fig_7.2_RequirementsSpecification.docx`, `whitepapers/specification/WPCT_Fig_7.7_RequirementsSpecification.docx` | Requirement context for DIV-01 |

### Reference provenance

- The listings and logs were last committed in `6fc757d` (2016-02-26). The input ADVS files are
  unchanged since `6c47a90` (2015-12-28).
- `whitepapers/WPCT/WPCT-F.07.02.sas` was edited afterwards only in `f2bd8d0` (2017-01-03), and
  `whitepapers/WPCT/WPCT-F.07.07.sas` only in `1788bb7` (2017-02-27).
- A raw `git diff 6fc757d HEAD` of either program is dominated by line-ending and blank-line churn.
  `git diff -w --ignore-cr-at-eol --ignore-blank-lines 6fc757d HEAD -- <program>` shows only environment
  and parameterisation edits; no analysis step changed:
  - 07.02: the commented-out `sasautos` block became an active `OPTIONS sasautos=` with an empty path
    entry (line 76); the remote `%util_access_test_data(advs)` call was commented out (line 82) and the
    local call activated with a blank `local=` (line 85); `outputs_folder` became blank (line 126);
    comments were added or removed.
  - 07.07: the `%let` block moved above the data step and gained the `DS` and `param` macro variables
    (lines 94-117); `outputs_folder`, `sasautos` and `local=` became UNC paths (lines 119-141); the
    subset, the derivation call and the post-baseline step use `&ds`, `&param`, `&a_fl`, `&tn_var` and
    `&p_fl` in place of literals (lines 146-171).
- The logs echo the statements actually submitted. Both runs read local copies of the data:
  `C:\CSS\phuse-scripts\data\adam\cdisc\advs.xpt` [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:26-29]
  and `C:\CSS\phuse-scripts\data\adam\cdisc-split\advs.xpt`
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:26-29]. At `6fc757d` the 07.02 program had the remote
  call active, the local call commented out and the `OPTIONS sasautos=` statement inside a comment, so
  the submitted 07.02 text differed from that revision in these environment statements
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:23-26]. The echoed analysis statements match the
  current programs once macro variables resolve, for example the 07.02 subset step
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:42-55] and the 07.07 derivation call with literal
  arguments [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:70-76].

## Part A — `whitepapers/WPCT/WPCT-F.07.02.sas`

Figure 7.2: change from baseline by analysis timepoint, visit and treatment. The program subsets ADVS,
restricts it to the safety population and analysis records, and then, for each parameter and
timepoint, computes summary statistics by visit and treatment and an endpoint ANCOVA p-value per active
treatment.

### A.1 Index

| ID | Category | SAS source | Disposition |
|----|----------|------------|-------------|
| S02-TRN-01 | TRN | [whitepapers/WPCT/WPCT-F.07.02.sas:108-124] | Ported → `programs.wpct_f_07_02.Config` |
| S02-FLT-01 | FLT | [whitepapers/WPCT/WPCT-F.07.02.sas:90-93] | Ported → `selection.subset_param_timepoints` |
| S02-TRN-02 | TRN | [whitepapers/WPCT/WPCT-F.07.02.sas:95-102] | Ported → `selection.abbreviate_treatment` |
| S02-POP-01 | POP | [whitepapers/WPCT/WPCT-F.07.02.sas:164] | Ported → `selection.flag_filter` |
| S02-FLT-02 | FLT | [whitepapers/WPCT/WPCT-F.07.02.sas:164] | Ported → `selection.flag_filter` |
| S02-FLT-03 | FLT | [whitepapers/WPCT/WPCT-F.07.02.sas:166-167] | Ported → `selection.exclude_visit` |
| S02-TRN-03 | TRN | [whitepapers/WPCT/WPCT-F.07.02.sas:144,163] | Ported → `schemas` column list |
| S02-TRN-04 | TRN | [whitepapers/WPCT/WPCT-F.07.02.sas:188,220-245] | Ported → `programs.wpct_f_07_02.run` |
| S02-MOD-01 | MOD | [whitepapers/WPCT/WPCT-F.07.02.sas:258-263] | Ported → `summary.proc_summary` |
| S02-MOD-02 | MOD | [whitepapers/WPCT/WPCT-F.07.02.sas:267-280] | Ported → `model.glm_solution` |
| S02-TRN-05 | TRN | [whitepapers/WPCT/WPCT-F.07.02.sas:283-305] | Ported → `programs.wpct_f_07_02.run` |
| S02-PRS-01 | PRS | [whitepapers/WPCT/WPCT-F.07.02.sas:191-197,214,248-254,315-389] | Excluded — presentation only |

The 19 dependency items `S02-DEP-*` follow in A.3.

### A.2 Items

#### S02-TRN-01 — User settings

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:108-124], `%let` user settings ("3) Key user settings")
- `Rule:` The analysis runs with `m_lb=work`, `m_ds=advs_sub`, `t_var=trtp_short`, `tn_var=trtpn`,
  `c_var=chg`, `b_var=base`, `ref_trtn=0`, `b_visn=0`, `e_visn=99`, `p_fl=saffl` and `a_fl=anl01fl`.
  `max_boxes_per_page=20` [whitepapers/WPCT/WPCT-F.07.02.sas:124] controls pagination only (S02-PRS-01).
- `Disposition:` Ported → `programs.wpct_f_07_02.Config` defaults
- `Relies on:` —
- `Port notes:` `Config` also carries the parameter/timepoint pairs of S02-FLT-01. `b_var=None` or
  `ref_trtn=None` stands for a blank SAS setting and skips the model (S02-MOD-02).

#### S02-FLT-01 — Parameter and timepoint subset

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:90-93], DATA `advs_sub`, WHERE statement
- `Rule:` From `WORK.ADVS` (the copy made by `%util_access_test_data`, S02-DEP-util_access_test_data),
  keep a record when `(PARAMCD='DIABP' and ATPTN=815) or (PARAMCD='SYSBP' and ATPTN=816)`, written as
  `paramcd in ('DIABP') and atptn in (815)` and `paramcd in ('SYSBP') and atptn in (816)`. A missing
  `ATPTN` fails `IN`, and character comparison ignores trailing blanks. The output keeps all 44 input
  variables and adds `trtp_short` (S02-TRN-02). SAS reference: `WORK.ADVS_SUB` has 5925 observations
  and 45 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:57-59].
- `Disposition:` Ported → `selection.subset_param_timepoints`
- `Relies on:` D-11, D-13

#### S02-TRN-02 — Treatment abbreviation

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:95-102], DATA `advs_sub`, ATTRIB and SELECT
- `Rule:` `trtp_short` is declared `length=$6` (label `Planned Treatment, abbreviated`) and set from
  `TRTP`: `Placebo` → `P`, `Xanomeline High Dose` → `X-high`, `Xanomeline Low Dose` → `X-low`. Any other
  value, a blank `TRTP` included, takes the OTHERWISE branch, `UNEXPECTED`, which the `$6` length
  stores as `UNEXPE`. WHEN comparisons ignore trailing blanks.
- `Disposition:` Ported → `selection.abbreviate_treatment`
- `Relies on:` D-11, D-12

#### S02-POP-01 — Safety population

- `Category:` POP
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:164], DATA `css_anadata`, WHERE `&p_fl = 'Y'`
- `Rule:` Keep records with `SAFFL='Y'`. A blank or any other value is excluded.
- `Disposition:` Ported → `selection.flag_filter`
- `Relies on:` D-11

#### S02-FLT-02 — Analysis record flag

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:164], DATA `css_anadata`, WHERE `&a_fl = 'Y'`
- `Rule:` Keep records with `ANL01FL='Y'`. A blank or any other value is excluded.
- `Disposition:` Ported → `selection.flag_filter`
- `Relies on:` D-11

#### S02-FLT-03 — Baseline visit excluded

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:166-167], DATA `css_anadata`, WHERE ALSO
  `avisitn ne &b_visn`
- `Rule:` Drop records with `AVISITN = 0` (`&b_visn`). A missing `AVISITN` is kept, because a missing
  value is not equal to 0. SAS combines the clauses as `(saffl='Y') and (avisitn not = 0) and
  (anl01fl='Y')` [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:200-201].
- `Disposition:` Ported → `selection.exclude_visit`
- `Relies on:` D-13

#### S02-TRN-03 — Analysis variables

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:144,163], `%let ana_variables` and `KEEP=` on the
  SET statement of DATA `css_anadata`
- `Rule:` `css_anadata` keeps exactly the 14 variables `STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN
  PARAM PARAMCD CHG BASE AVISIT AVISITN ATPT ATPTN`. `KEEP=` does not reorder variables: they stay in
  their `advs_sub` order, which for the repository input is `STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM
  ATPTN ATPT AVISIT AVISITN BASE CHG ANL01FL trtp_short`. SAS reference: `WORK.CSS_ANADATA` has 3548
  observations and 14 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:202].
- `Disposition:` Ported → `schemas` column list
- `Relies on:` —

#### S02-TRN-04 — Strata and record order

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:188,220-245], `%util_labels_from_var`, loops 1 and 2
  of `%boxplot_each_param_tp`, PROC SORT
- `Rule:` `%util_labels_from_var(css_anadata, paramcd, param)` [whitepapers/WPCT/WPCT-F.07.02.sas:188]
  lists each `PARAMCD` present in `css_anadata` in ascending order (PROC SORT NODUPKEY
  [whitepapers/utilities/util_labels_from_var.sas:59-82]). Loop 1 selects each parameter into
  `css_nextparam` [whitepapers/WPCT/WPCT-F.07.02.sas:220-227], and
  `%util_labels_from_var(css_nextparam, atptn, atpt)` [whitepapers/WPCT/WPCT-F.07.02.sas:230] lists each
  `ATPTN` present for it, ascending. Loop 2 selects each timepoint into `css_nexttimept`, sorted BY
  `AVISITN TRTPN` [whitepapers/WPCT/WPCT-F.07.02.sas:236-245]. Only values present in the data form
  strata. SAS reference, DIABP/815 then SYSBP/816: `CSS_NEXTPARAM` and `CSS_NEXTTIMEPT` have 1774
  observations each [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:726,803,1141,1219].
- `Disposition:` Ported → `programs.wpct_f_07_02.run`
- `Relies on:` D-04, D-14
- `Port notes:` An empty analysis population gives `PARAMCD_N = 0`
  [whitepapers/utilities/util_labels_from_var.sas:59-82], so the loop runs zero times: zero strata, no
  statistics or models, and no error. A configured parameter/timepoint pair with no records yields no
  stratum.

#### S02-MOD-01 — Summary statistics by visit and treatment

- `Category:` MOD
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:258-263], `%boxplot_each_param_tp` / PROC SUMMARY
- `Rule:` `PROC SUMMARY data=css_nexttimept; BY avisitn trtpn avisit trtp_short; VAR chg;` with output
  `n mean std median min=datamin max=datamax q1 q3` and `_TYPE_ _FREQ_` dropped, giving 12 columns in
  the order BY variables, then `n mean std median datamin datamax q1 q3`. The defaults apply:
  `QNTLDEF=5` and `VARDEF=DF`. Only `CHG` is analysed: a missing `CHG` is excluded from `n` and from
  every statistic, but its record still forms its BY group, and `BASE` is never consulted. The data are
  sorted only BY `AVISITN TRTPN`; the BY statement relies on `AVISIT` being constant within `AVISITN`
  and `trtp_short` within `TRTPN`, as in the repository data. SAS reference: `CSS_STATS` has 30
  observations and 12 variables per stratum [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:978,1400].
- `Disposition:` Ported → `summary.proc_summary` (quantiles through `summary.sas_quantile_def5`)
- `Relies on:` D-01, D-02, D-03, D-04, D-14

#### S02-MOD-02 — Endpoint ANCOVA

- `Category:` MOD
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:267-280], `%boxplot_each_param_tp` / PROC GLM
- `Rule:` Runs only when `%length(&b_var) > 0 and %length(&ref_trtn) > 0`
  [whitepapers/WPCT/WPCT-F.07.02.sas:267]. ODS selects and outputs `ParameterEstimates` to
  `css_pvalues` [whitepapers/WPCT/WPCT-F.07.02.sas:273-274]. `PROC GLM data=css_nexttimept; WHERE
  avisitn = 99; CLASS trtpn (ref="0"); MODEL chg = base trtpn / solution;`
  [whitepapers/WPCT/WPCT-F.07.02.sas:276-280]. The listing shows the rows `Intercept`, `BASE`,
  `TRTPN 54`, `TRTPN 81` and `TRTPN 0`; the Intercept and `TRTPN` rows carry the flag `B`, and
  `TRTPN 0` has estimate `0.00000000` with missing standard error, t and p
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:8-14,23-29]. SAS reference: `CSS_PVALUES` has 5
  observations and 7 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:992,1414].
- `Disposition:` Ported → `model.glm_solution`
- `Relies on:` D-05, D-06, D-07, D-08, D-09
- `Port notes:` In the repository data 20 analysis records, all of subject `01-718-1150`, have both
  `BASE` and `CHG` missing. Two of them are endpoint records, one per parameter, so each endpoint model
  uses 226 of 227 records. `Config(b_var=None)` or `Config(ref_trtn=None)` skips the fit, and
  `css_stats` then has no `pval` column. A stratum whose reference level 0 is absent from the model data
  gets the status `not fitted: reference level 0 absent` and no parameter rows (U-02).

#### S02-TRN-05 — Endpoint p-value merge

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:283-305], `%boxplot_each_param_tp`, DATA steps and
  PROC SORT after PROC GLM
- `Rule:` `temp` takes the structure of `css_stats (keep=avisitn trtpn)` with no rows
  [whitepapers/WPCT/WPCT-F.07.02.sas:283-287]. From `css_pvalues` keep the rows whose `Parameter` starts
  with `TRTPN` (`=:` prefix test against the upper-cased `&tn_var`
  [whitepapers/WPCT/WPCT-F.07.02.sas:292]), with `pval = Probt`, `AVISITN = 99` and
  `TRTPN = input(scan(parameter,-1,' '), best8.)` [whitepapers/WPCT/WPCT-F.07.02.sas:289-296]. Sort BY
  `AVISITN TRTPN` and match-merge into `css_stats` BY `AVISITN TRTPN`
  [whitepapers/WPCT/WPCT-F.07.02.sas:298-305]. Each `AVISITN 99` row receives its own level's `pval`;
  the reference level's `Probt` is missing, so its row has a missing `pval`; every other visit row has a
  missing `pval`. The `Parameter` label uses the variable name as stored (`BASE` here, `base` in 07.07
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:11; whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:13]),
  and the case-sensitive prefix test relies on ADVS storing `TRTPN` in upper case. SAS reference: 3
  `TRTPN` rows with 4 variables, then `CSS_STATS` with 30 observations and 13 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:1024,1049,1446,1471].
- `Disposition:` Ported → `programs.wpct_f_07_02.run` (`pval` column)
- `Relies on:` D-05, D-07

#### S02-PRS-01 — Presentation steps

- `Category:` PRS
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:191-197,214,248-254,315-389], `%util_labels_from_var`
  calls for visit labels, `%util_count_unique_values` for `&trtn`, `%util_proc_template`, Y-axis,
  format and pagination macros, DATA `css_plot`, graphics options, titles, footnotes, ODS PDF and PROC
  SGRENDER
- `Rule:` Baseline and endpoint visit labels [whitepapers/WPCT/WPCT-F.07.02.sas:191-194]; `&trtn`, set
  at [whitepapers/WPCT/WPCT-F.07.02.sas:197] and never referenced afterwards; the GTL template
  [whitepapers/WPCT/WPCT-F.07.02.sas:214]; the Y-axis range, display formats and page blocks
  [whitepapers/WPCT/WPCT-F.07.02.sas:248-254]; `css_plot`, `options missing=' '`, titles, footnotes,
  axis ticks, the PDF file and the box-plot pages [whitepapers/WPCT/WPCT-F.07.02.sas:315-389]. Two
  listing effects follow: `options missing=' '` [whitepapers/WPCT/WPCT-F.07.02.sas:324] is set after the
  first model, so missing values print as `.` in the first table and blank in the second
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:14,29]; the TITLE statement
  [whitepapers/WPCT/WPCT-F.07.02.sas:330] runs after the model, so the first table prints under the
  default title and the second under the first stratum's title
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:1,15].
- `Disposition:` Excluded — presentation only (user exclusion; disposition rule)
- `Relies on:` —

### A.3 Dependency list for 07.02

The list is the macro call closure of the program: every macro invoked in code, directly or through
another macro. 07.02 reaches 19 macros. It does not reach `derive_lastminmax_measure`,
`assert_var_nonmissing` or `assert_unique_keys`.

| ID | Calls | Disposition |
|----|-------|-------------|
| S02-DEP-util_access_test_data | [whitepapers/WPCT/WPCT-F.07.02.sas:85] | Ported (local read) → `xpt_reader.read_xpt`; remote-URL branch excluded |
| S02-DEP-assert_depend | [whitepapers/WPCT/WPCT-F.07.02.sas:147-156]; nested | Partly ported → `xpt_reader.require_variables`; session checks excluded |
| S02-DEP-assert_continue | [whitepapers/WPCT/WPCT-F.07.02.sas:158] | Ported → `errors.AnalysisHalted` |
| S02-DEP-assert_dset_exist | nested | Ported → `xpt_reader.read_xpt` |
| S02-DEP-assert_var_exist | nested | Ported → `xpt_reader.require_variables` |
| S02-DEP-assert_macro_exist | nested | Excluded — SAS autocall path |
| S02-DEP-util_resolve_sasautos | nested | Excluded — SAS autocall path |
| S02-DEP-util_labels_from_var | [whitepapers/WPCT/WPCT-F.07.02.sas:188,191,194,230] | Partly ported → stratum order; labels excluded |
| S02-DEP-util_count_unique_values | [whitepapers/WPCT/WPCT-F.07.02.sas:197]; nested | Partly ported → loop counts; `&trtn` excluded |
| S02-DEP-util_get_var_min_max | [whitepapers/WPCT/WPCT-F.07.02.sas:248] | Excluded — Y-axis range |
| S02-DEP-util_value_format | [whitepapers/WPCT/WPCT-F.07.02.sas:251] | Excluded — display formats |
| S02-DEP-util_proc_template | [whitepapers/WPCT/WPCT-F.07.02.sas:214] | Excluded — GTL template |
| S02-DEP-util_boxplot_block_ranges | [whitepapers/WPCT/WPCT-F.07.02.sas:254] | Excluded — pagination |
| S02-DEP-util_axis_order | [whitepapers/WPCT/WPCT-F.07.02.sas:335] | Excluded — axis ticks |
| S02-DEP-util_delete_dsets | [whitepapers/WPCT/WPCT-F.07.02.sas:307,396]; nested | Excluded — WORK cleanup |
| S02-DEP-boxplot_each_param_tp | [whitepapers/WPCT/WPCT-F.07.02.sas:400] | Partly ported → loops, statistics, model, merge; graphics excluded |
| S02-DEP-qtrim | nested | Excluded — SAS-supplied, macro text handling |
| S02-DEP-qleft | nested | Excluded — SAS-supplied, autocall path |
| S02-DEP-datatyp | nested | Excluded — SAS-supplied, presentation argument check |

#### S02-DEP-util_access_test_data — Test-data access

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_access_test_data.sas:43-76]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:85] (the remote call at [whitepapers/WPCT/WPCT-F.07.02.sas:82] is
  commented out)
- `Rule:` With a non-blank `LOCAL=` the macro reads `<local>advs.xpt`
  [whitepapers/utilities/util_access_test_data.sas:47-57]; with a blank `LOCAL=`, as at line 85, it reads
  `https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/advs.xpt`
  [whitepapers/utilities/util_access_test_data.sas:58-60]. It copies member `ADVS` of the transport file
  to `WORK.ADVS` [whitepapers/utilities/util_access_test_data.sas:62-66]. On failure it logs
  `ERROR: (UTIL_ACCESS_TEST_DATA) Please confirm that data set ADVS exists in transport file ADVS.` and
  deletes `WORK.ADVS` [whitepapers/utilities/util_access_test_data.sas:68-71].
- `Disposition:` Ported (local read) → `xpt_reader.read_xpt`, reading the repository copy
  `data/adam/cdisc/advs.xpt`; the remote-URL branch
  [whitepapers/utilities/util_access_test_data.sas:58-60] is excluded — there is no network access
  (ENV-02)
- `Relies on:` D-10, D-11
- `Port notes:` A path that does not exist or is not a regular file raises
  `InputDataError("WPCT-F.07.02: input dataset not found: <absolute path>")`. A file the reader cannot
  read, or that yields no columns, raises
  `InputDataError("WPCT-F.07.02: cannot read SAS transport file <absolute path>: <reader message>")`.

#### S02-DEP-assert_depend — Dependency assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_depend.sas:51-162]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:147-156]; nested [whitepapers/utilities/util_boxplot_block_ranges.sas:38]
- `Rule:` Returns 1 or 0, which the program stores in the global `CONTINUE`. The OS test only warns
  [whitepapers/utilities/assert_depend.sas:61-66]; the SASV test fails when SAS is older than the
  stated release [whitepapers/utilities/assert_depend.sas:68-95]; SYSPROD is not implemented
  [whitepapers/utilities/assert_depend.sas:97-100]; `vars=` fails when a listed dataset is missing or
  lacks a listed variable [whitepapers/utilities/assert_depend.sas:102-128]; `macros=` fails when a
  listed macro file is not in the autocall path [whitepapers/utilities/assert_depend.sas:130-139];
  `symbols=` fails when a listed macro variable does not exist
  [whitepapers/utilities/assert_depend.sas:141-151]. 07.02 checks `WORK.ADVS_SUB` (`&m_lb..&m_ds`) for
  the 14 analysis variables [whitepapers/WPCT/WPCT-F.07.02.sas:144,150]. The nested call checks
  `css_nexttimept` for the page-block variables and the symbol `max_boxes_per_page`.
- `Disposition:` Partly ported → `xpt_reader.require_variables` for the `vars=` check
  [whitepapers/utilities/assert_depend.sas:102-128]; excluded — the OS, SASV, SYSPROD, macros and
  symbols checks, which test the SAS session (the Python environment is fixed by `requirements.txt`),
  and the nested pagination call (S02-DEP-util_boxplot_block_ranges)
- `Relies on:` —
- `Port notes:` A missing variable raises
  `InputDataError("WPCT-F.07.02: <absolute path> lacks required variables: <names>")`. Required
  variables match case-insensitively, as `varnum()` does [whitepapers/utilities/assert_var_exist.sas:23],
  and are returned under their spelling in `schemas.REQUIRED_INPUT_VARIABLES`; a required variable
  stored under more than one spelling raises
  `InputDataError("WPCT-F.07.02: <absolute path> has variables that differ only in case: <columns>")`.

#### S02-DEP-assert_continue — Halt on failed assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_continue.sas:22-39]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:158]
- `Rule:` When the global `CONTINUE` is 0 the macro logs an ERROR and runs an invalid DATA step
  (`set css_does_not_exist`), forcing syntax-checking mode so that no later step processes data
  [whitepapers/utilities/assert_continue.sas:22-36]. When `CONTINUE` does not exist it only logs an
  ERROR [whitepapers/utilities/assert_continue.sas:38]. In 07.02 only the dependency assertion feeds it.
- `Disposition:` Ported → `errors.AnalysisHalted` stops the driver before any export
- `Relies on:` —

#### S02-DEP-assert_dset_exist — Dataset existence

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_dset_exist.sas:19-45]; nested
  [whitepapers/utilities/assert_depend.sas:109; whitepapers/utilities/util_labels_from_var.sas:51;
  whitepapers/utilities/util_count_unique_values.sas:33; whitepapers/utilities/util_get_var_min_max.sas:37;
  whitepapers/utilities/util_value_format.sas:35; whitepapers/utilities/assert_var_exist.sas:14]
- `Rule:` Returns 1 only when every listed dataset exists (`exist()`), otherwise 0 with an ERROR
  [whitepapers/utilities/assert_dset_exist.sas:23-44].
- `Disposition:` Ported → file-existence check in `xpt_reader.read_xpt` (see
  S02-DEP-util_access_test_data)
- `Relies on:` —

#### S02-DEP-assert_var_exist — Variable existence

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_var_exist.sas:11-39]; nested
  [whitepapers/utilities/assert_depend.sas:114; whitepapers/utilities/util_labels_from_var.sas:52-53;
  whitepapers/utilities/util_count_unique_values.sas:34; whitepapers/utilities/util_get_var_min_max.sas:38;
  whitepapers/utilities/util_value_format.sas:36]
- `Rule:` Returns 1 when the dataset exists and `varnum()` finds the upper-cased variable name, otherwise
  0 with an ERROR [whitepapers/utilities/assert_var_exist.sas:14-36].
- `Disposition:` Ported → variable-existence check in `xpt_reader.require_variables`
- `Relies on:` —

#### S02-DEP-assert_macro_exist — Macro availability

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_macro_exist.sas:33-68]; nested
  [whitepapers/utilities/assert_depend.sas:134], once per name in the `macros=` list
  [whitepapers/WPCT/WPCT-F.07.02.sas:151-153]
- `Rule:` Returns 1 when `<name>.sas` exists in a SASAUTOS path resolved by `%util_resolve_sasautos`
  [whitepapers/utilities/assert_macro_exist.sas:39-66].
- `Disposition:` Excluded — inspects the SAS autocall path; there is no Python counterpart
- `Relies on:` —

#### S02-DEP-util_resolve_sasautos — Autocall path list

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_resolve_sasautos.sas:18-118]; nested
  [whitepapers/utilities/assert_macro_exist.sas:41]
- `Rule:` Returns the SASAUTOS option as a pipe-delimited list of paths, resolving filerefs through
  `SASHELP.VEXTFL` [whitepapers/utilities/util_resolve_sasautos.sas:22-116].
- `Disposition:` Excluded — inspects the SAS autocall path; there is no Python counterpart
- `Relies on:` —

#### S02-DEP-util_labels_from_var — Value and label symbols

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_labels_from_var.sas:45-94]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:188,191,194,230]
- `Rule:` Sets `<prefix>_N` to the count of distinct non-missing values of VAR
  [whitepapers/utilities/util_labels_from_var.sas:59], and `<prefix>_VAL<i>` and `<prefix>_LAB<i>` from
  PROC SORT NODUPKEY BY VAR LAB, in ascending VAR order
  [whitepapers/utilities/util_labels_from_var.sas:66-82]; logs an ERROR when a value has more than one
  label [whitepapers/utilities/util_labels_from_var.sas:75-78]. Calls 188 (`PARAMCD`/`PARAM`) and 230
  (`ATPTN`/`ATPT`) drive the loops; calls 191 and 194 produce baseline and endpoint visit labels for
  titles.
- `Disposition:` Partly ported → the sorted value lists of calls 188 and 230 drive the stratum order in
  `programs.wpct_f_07_02.run` (S02-TRN-04); excluded — the label symbols and calls 191 and 194, which
  feed titles only
- `Relies on:` —

#### S02-DEP-util_count_unique_values — Distinct-value count

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_count_unique_values.sas:27-51]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:197]; nested [whitepapers/utilities/util_labels_from_var.sas:59]
- `Rule:` `select count(unique(VAR)) into :SYM` [whitepapers/utilities/util_count_unique_values.sas:38-42];
  SQL `count` ignores missing values.
- `Disposition:` Partly ported → the nested counts set the loop bounds of S02-TRN-04 in
  `programs.wpct_f_07_02.run`; excluded — `&trtn` [whitepapers/WPCT/WPCT-F.07.02.sas:197], which is
  never referenced afterwards (it appears only in comments at lines 178 and 196)
- `Relies on:` —

#### S02-DEP-util_get_var_min_max — Y-axis range

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_get_var_min_max.sas:32-69]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:248] (the alternative at line 233 is commented out)
- `Rule:` Stores the minimum and maximum of `CHG` in `&chg_min_max` for the Y axis.
- `Disposition:` Excluded — Y-axis range only
- `Relies on:` —

#### S02-DEP-util_value_format — Display formats

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_value_format.sas:31-79]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:251]
- `Rule:` Derives display formats for MEAN (one extra decimal) and STD (two extra decimals) from the
  precision of `CHG`; they are used only in the FORMAT statement of `css_plot`
  [whitepapers/WPCT/WPCT-F.07.02.sas:319].
- `Disposition:` Excluded — display formats only; statistics stay at full precision
- `Relies on:` —

#### S02-DEP-util_proc_template — Graph template

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_proc_template.sas:39-218]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:214]
- `Rule:` Compiles the GTL template `PhUSEboxplot` that PROC SGRENDER uses.
- `Disposition:` Excluded — GTL template (user exclusion)
- `Relies on:` —

#### S02-DEP-util_boxplot_block_ranges — Page blocks

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_boxplot_block_ranges.sas:33-133]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:254]
- `Rule:` Splits the visit-by-treatment boxes into pages of at most `&max_boxes_per_page` boxes, stored
  in `&boxplot_block_ranges`; it calls `%assert_depend`
  [whitepapers/utilities/util_boxplot_block_ranges.sas:38] and `%util_delete_dsets`
  [whitepapers/utilities/util_boxplot_block_ranges.sas:123].
- `Disposition:` Excluded — pagination
- `Relies on:` —

#### S02-DEP-util_axis_order — Axis ticks

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_axis_order.sas:29-64]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:335]
- `Rule:` Returns the Y-axis minimum, maximum and tick increment.
- `Disposition:` Excluded — axis ticks
- `Relies on:` —

#### S02-DEP-util_delete_dsets — WORK cleanup

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_delete_dsets.sas:34-53]; called
  [whitepapers/WPCT/WPCT-F.07.02.sas:307,396]; nested
  [whitepapers/utilities/util_access_test_data.sas:70; whitepapers/utilities/util_boxplot_block_ranges.sas:123]
- `Rule:` Deletes the listed WORK datasets with PROC DATASETS.
- `Disposition:` Excluded — SAS WORK cleanup; Python holds no WORK library
- `Relies on:` —

#### S02-DEP-boxplot_each_param_tp — Inline driver macro

- `Category:` DEP
- `SAS source:` defined inline [whitepapers/WPCT/WPCT-F.07.02.sas:216-398]; invoked
  [whitepapers/WPCT/WPCT-F.07.02.sas:400] with the defaults `plotds=css_anadata, cleanup=1`
- `Rule:` Loops over parameters and timepoints (S02-TRN-04), computes the statistics (S02-MOD-01), fits
  the endpoint model and merges its p-values (S02-MOD-02, S02-TRN-05), draws the box plots (S02-PRS-01)
  and deletes its WORK datasets (S02-DEP-util_delete_dsets).
- `Disposition:` Partly ported → the loops, statistics, model and p-value merge, in
  `programs.wpct_f_07_02.run`, `summary.proc_summary` and `model.glm_solution`; excluded — the graphics
  (S02-PRS-01) and the cleanup
- `Relies on:` —

#### S02-DEP-qtrim — SAS-supplied `%qtrim`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/assert_depend.sas:106; whitepapers/utilities/util_resolve_sasautos.sas:102-103]
- `Rule:` Removes trailing blanks from macro text and returns it quoted: the variable list of each
  `vars=` entry [whitepapers/utilities/assert_depend.sas:106] and the autocall path strings
  [whitepapers/utilities/util_resolve_sasautos.sas:102-103].
- `Disposition:` Excluded — source not in the repository; macro-argument text handling (the port
  receives variable lists as data)
- `Relies on:` —

#### S02-DEP-qleft — SAS-supplied `%qleft`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/util_resolve_sasautos.sas:75,84]
- `Rule:` Removes leading blanks from macro text and returns it quoted, while the SASAUTOS string is
  parsed.
- `Disposition:` Excluded — source not in the repository; serves the excluded autocall-path inspection
- `Relies on:` —

#### S02-DEP-datatyp — SAS-supplied `%datatyp`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/util_axis_order.sas:33,36]
- `Rule:` Returns `NUMERIC` or `CHAR` for macro text; it validates the MIN, MAX and TICKS arguments of
  `%util_axis_order`.
- `Disposition:` Excluded — source not in the repository; validates arguments of the presentation-only
  `%util_axis_order`
- `Relies on:` —

**Not dependencies of 07.02**

- `%util_autocallpath` appears only in comments [whitepapers/utilities/assert_depend.sas:11;
  whitepapers/utilities/assert_macro_exist.sas:13-15], and `%assert_sym_non_miss` only in a TO-DO
  comment [whitepapers/utilities/assert_depend.sas:8]. Neither is invoked and neither has a file
  (ENV-09).
- Intrinsic macro functions and statements (`%scan`, `%qscan`, `%upcase`, `%qupcase`, `%sysfunc`,
  `%qsysfunc`, `%qsubstr`, `%length`, `%index`, `%eval`, `%sysevalf`, `%str`, `%quote`, `%unquote`,
  `%symexist`) are part of SAS and are not dependencies.

## Part B — `whitepapers/WPCT/WPCT-F.07.07.sas`

Figure 7.7: change from the minimum baseline to the minimum post-baseline measure by treatment and
analysis timepoint, for multiple studies. The program subsets the split-study ADVS, derives one
baseline and one post-baseline record per subject, parameter and timepoint, keeps the post-baseline
records of the safety population, stacks a pooled copy, numbers the studies, and then, per timepoint,
computes summary statistics by study and treatment with per-study and pooled ANCOVA p-values.

### B.1 Index

| ID | Category | SAS source | Disposition |
|----|----------|------------|-------------|
| S07-TRN-01 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:96-117] | Ported → `programs.wpct_f_07_07.Config` |
| S07-FLT-01 | FLT | [whitepapers/WPCT/WPCT-F.07.07.sas:146-148] | Ported → `selection.subset_param_timepoints` |
| S07-TRN-02 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:150-157] | Ported → `selection.abbreviate_treatment` |
| S07-TRN-03 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:161-166] | Ported → `derivation.derive_lastminmax_measure` |
| S07-FLT-02 | FLT | [whitepapers/WPCT/WPCT-F.07.07.sas:168-171] | Ported → `selection.post_baseline_filter` |
| S07-POP-01 | POP | [whitepapers/WPCT/WPCT-F.07.07.sas:212-214] | Ported → `selection.flag_filter` |
| S07-FLT-03 | FLT | [whitepapers/WPCT/WPCT-F.07.07.sas:213] | Ported → `selection.flag_filter` |
| S07-TRN-04 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:190,213] | Ported → `schemas` column list |
| S07-FLT-04 | FLT, halting | [whitepapers/WPCT/WPCT-F.07.07.sas:219-220] | Ported → `selection.assert_var_nonmissing` |
| S07-TRN-05 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:222-230] | Ported → `selection.stack_pooled` |
| S07-TRN-06 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:233-241] | Ported → `selection.assign_studynum` |
| S07-FLT-05 | FLT, halting | [whitepapers/WPCT/WPCT-F.07.07.sas:244-245] | Ported → `selection.assert_unique_keys` |
| S07-TRN-07 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:259,262,290-316] | Ported → `programs.wpct_f_07_07.run` |
| S07-MOD-01 | MOD | [whitepapers/WPCT/WPCT-F.07.07.sas:329-334] | Ported → `summary.proc_summary` |
| S07-MOD-02 | MOD | [whitepapers/WPCT/WPCT-F.07.07.sas:338-352] | Ported → `model.glm_solution` per study |
| S07-MOD-03 | MOD | [whitepapers/WPCT/WPCT-F.07.07.sas:355-363] | Ported → `model.glm_solution`, pooled |
| S07-TRN-08 | TRN | [whitepapers/WPCT/WPCT-F.07.07.sas:366-394] | Ported → `programs.wpct_f_07_07.run` |
| S07-PRS-01 | PRS | [whitepapers/WPCT/WPCT-F.07.07.sas:216,284,319-325,404-473] | Excluded — presentation only |
| LMM-FLT-01 | FLT | [whitepapers/ADaM/derive_lastminmax_measure.sas:92] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-01 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:94-95] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-02 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:81,115-130] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-03 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:133-147] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-04 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:150-154] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-FLT-02 | FLT | [whitepapers/ADaM/derive_lastminmax_measure.sas:159-169,177-189] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-05 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:171-174] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-06 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:191-193] | Ported → `derivation.derive_lastminmax_measure` |
| LMM-TRN-07 | TRN | [whitepapers/ADaM/derive_lastminmax_measure.sas:97-110,197-206] | Ported → `derivation.derive_lastminmax_measure` |

The 22 dependency items `S07-DEP-*` follow in B.4.

### B.2 Items

#### S07-TRN-01 — User settings

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:96-117], `%let` user settings ("3) Key user settings")
- `Rule:` The analysis runs with `DS=ADVS`, `param='DIABP'`, `m_lb=work`, `m_ds=&ds._&LMM` (which
  resolves to `ADVS_MIN` when used; ENV-05), `t_var=trtp_short`, `tn_var=trtpn`, `c_var=chg`,
  `b_var=base`, `ref_trtn=0`, `p_fl=saffl`, `lmm=MIN`, `c_mode=&LMM` (`MIN`, a label only, S07-PRS-01)
  and `a_fl=anl02fl`. `max_boxes_per_page=20` [whitepapers/WPCT/WPCT-F.07.07.sas:117] controls
  pagination only.
- `Disposition:` Ported → `programs.wpct_f_07_07.Config` defaults
- `Relies on:` —
- `Port notes:` `Config` also carries the parameter/timepoint selection of S07-FLT-01. `b_var=None` or
  `ref_trtn=None` stands for a blank SAS setting and skips both models (S07-MOD-02, S07-MOD-03).

#### S07-FLT-01 — Parameter and timepoint subset

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:146-148], DATA `&ds._sub` (`ADVS_SUB`), WHERE
  statement
- `Rule:` From `WORK.ADVS` (S07-DEP-util_access_test_data), keep a record when
  `PARAMCD in ('DIABP') and ATPTN in (815 817)`. A missing `ATPTN` fails `IN`, and character comparison
  ignores trailing blanks. The output keeps all 44 input variables and adds `trtp_short` (S07-TRN-02).
  SAS reference: `WORK.ADVS_SUB` has 5926 observations and 45 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:59-61].
- `Disposition:` Ported → `selection.subset_param_timepoints`
- `Relies on:` D-11, D-13

#### S07-TRN-02 — Treatment abbreviation

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:150-157], DATA `&ds._sub`, ATTRIB and SELECT
- `Rule:` The same mapping as S02-TRN-02: `trtp_short` `$6` from `TRTP`, `Placebo` → `P`,
  `Xanomeline High Dose` → `X-high`, `Xanomeline Low Dose` → `X-low`, otherwise `UNEXPECTED` stored as
  `UNEXPE`.
- `Disposition:` Ported → `selection.abbreviate_treatment`
- `Relies on:` D-11, D-12

#### S07-TRN-03 — Minimum baseline and post-baseline derivation

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:161-166], `%derive_lastminmax_measure` call
- `Rule:` `%derive_lastminmax_measure(ADVS_SUB, MIN, flvars=anl02fl, grpvars=STUDYID USUBJID TRTPN
  PARAMCD ATPTN, ordvars=AVISITN, incl=trtp_short SAFFL PARAM ATPT, dsout=ADVS_MIN)`, with the default
  `cleanup=1`. The macro's behaviour is items LMM-FLT-01 to LMM-TRN-07 (B.3).
- `Disposition:` Ported → `derivation.derive_lastminmax_measure` (items LMM-*)
- `Relies on:` D-10, D-14, D-15

#### S07-FLT-02 — Post-baseline records only

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:168-171], DATA `&ds._&LMM` (`ADVS_MIN`), WHERE
  `avisit =: 'Post-baseline'`
- `Rule:` Keep the records whose `AVISIT` begins with `Post-baseline` (`=:` prefix test), i.e. the
  LMM-TRN-06 records; the LMM-TRN-05 baseline records are dropped. SAS reference: `ADVS_MIN` goes from
  996 to 498 observations, 16 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:205,229].
- `Disposition:` Ported → `selection.post_baseline_filter`
- `Relies on:` D-11

#### S07-POP-01 — Safety population

- `Category:` POP
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:212-214], DATA `css_safana`, `where=(&p_fl = 'Y'
  …)` on the SET statement
- `Rule:` Keep records with `SAFFL='Y'`. A blank or any other value is excluded.
- `Disposition:` Ported → `selection.flag_filter`
- `Relies on:` D-11

#### S07-FLT-03 — Analysis record flag

- `Category:` FLT
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:213], DATA `css_safana`, `where=(… &a_fl = 'Y')`
- `Rule:` Keep records with `ANL02FL='Y'`. The derivation sets this flag on every record it outputs
  (LMM-TRN-07). SAS reference: 498 records read under `WHERE (saffl='Y') and (anl02fl='Y')`
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:365-366].
- `Disposition:` Ported → `selection.flag_filter`
- `Relies on:` D-11

#### S07-TRN-04 — Analysis variables

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:190,213], `%let ana_variables` and `keep=` on the SET
  statement of DATA `css_safana`
- `Rule:` `css_safana` keeps exactly the 12 variables `STUDYID USUBJID SAFFL ANL02FL trtp_short TRTPN
  PARAM PARAMCD CHG BASE ATPT ATPTN`; `AVISIT` and `AVISITN` are not kept. The variables stay in their
  `ADVS_MIN` order (LMM-TRN-07): `STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT trtp_short BASE
  CHG ANL02FL`. SAS reference: `CSS_SAFANA` has 498 observations and 12 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:367].
- `Disposition:` Ported → `schemas` column list
- `Relies on:` —

#### S07-FLT-04 — Halt on missing change from baseline

- `Category:` FLT, halting
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:219-220], `%assert_var_nonmissing` and
  `%assert_continue`
- `Rule:` `%assert_var_nonmissing(css_safana, chg)` opens `css_safana` with `where=(missing(chg))` and
  fails when that subset is not empty (`NLOBSF > 0`)
  [whitepapers/utilities/assert_var_nonmissing.sas:43-56]; its 0 is stored in `CONTINUE`
  [whitepapers/WPCT/WPCT-F.07.07.sas:219]. `%assert_continue` then forces syntax-checking mode through an
  invalid DATA step [whitepapers/utilities/assert_continue.sas:22-39], so no later step processes data.
  SAS reference: PASS with 0 missing values [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:383].
- `Disposition:` Ported → `selection.assert_var_nonmissing`, raising `errors.AnalysisHalted`
- `Relies on:` —
- `Port notes:` The message is `WPCT-F.07.07: assert_var_nonmissing failed: <k> missing CHG in
  CSS_SAFANA`. The driver exports nothing; the CLI writes the halt into the report and exits 1.

#### S07-TRN-05 — Pooled copy

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:222-230], DATA `css_anadata`
- `Rule:` Output every `css_safana` record, then a second copy (`in=in_pool`). On the copy
  `STUDYID = 'A0'x !! 'Pooled'` and `substr(USUBJID,1,1) = 'P'`, so `USUBJID` keeps its length with the
  first character replaced. The 7-byte pooled identifier fits the `STUDYID` storage length (12 in the
  repository input). SAS reference: `CSS_ANADATA` has 996 observations and 12 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:399].
- `Disposition:` Ported → `selection.stack_pooled`
- `Relies on:` D-16

#### S07-TRN-06 — Study number

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:233-241], PROC SORT and DATA `css_anadata`
- `Rule:` Sort `css_anadata` BY `STUDYID TRTPN` (stable). In a DATA step BY `STUDYID TRTPN`,
  `if first.studyid then studynum+1;`: the sum statement retains `studynum` and starts it at 0, so the
  studies are numbered 1, 2, … in `STUDYID` sort order, and the pooled study, which sorts last, gets
  the highest number. `studynum` is the 13th variable. SAS reference: `CSS_ANADATA` has 996
  observations and 13 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:427].
- `Disposition:` Ported → `selection.assign_studynum`
- `Relies on:` D-04, D-14, D-16
- `Port notes:` With `CDISCPILOT02` absent from the input, study 1 is `CDISCPILOT01` and the pooled
  `studynum` is 2.

#### S07-FLT-05 — Halt on duplicate keys

- `Category:` FLT, halting
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:244-245], `%assert_unique_keys` and
  `%assert_continue`
- `Rule:` `%assert_unique_keys(css_anadata, studyid usubjid paramcd atptn)` groups by the four keys and
  selects the groups with `count(atptn) > 1` [whitepapers/utilities/assert_unique_keys.sas:84-97].
  `count` of the last key counts its non-missing values, so a duplicated group whose `ATPTN` is missing
  is not detected. Any selected group logs an ERROR and sets the global `CONTINUE` to 0
  [whitepapers/utilities/assert_unique_keys.sas:99-102]; the macro sets `CONTINUE` itself
  [whitepapers/utilities/assert_unique_keys.sas:47,51] and leaves the offending keys in
  `WORK.FAIL_AUK`. `%assert_continue` [whitepapers/WPCT/WPCT-F.07.07.sas:245] then halts processing.
  SAS reference: PASS [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:459].
- `Disposition:` Ported → `selection.assert_unique_keys`, raising `errors.AnalysisHalted` with a message
  that names the keys and the duplicate count
- `Relies on:` D-13

#### S07-TRN-07 — Strata, record order and study count

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:259,262,290-316], `%util_labels_from_var`,
  `%util_count_unique_values`, loops 1 and 2 of `%boxplot_each_param_tp`, PROC SORT
- `Rule:` As S02-TRN-04: each `PARAMCD` present in `css_anadata`, ascending
  [whitepapers/WPCT/WPCT-F.07.07.sas:259], crossed with each `ATPTN` present for it, ascending
  [whitepapers/WPCT/WPCT-F.07.07.sas:300]; only values present form strata. Rows of a stratum are sorted
  BY `STUDYID TRTPN` [whitepapers/WPCT/WPCT-F.07.07.sas:313-316].
  `%util_count_unique_values(css_anadata, studyid, stdyn)` [whitepapers/WPCT/WPCT-F.07.07.sas:262] sets
  `&stdyn` to `count(unique(STUDYID))` over `css_anadata`, the pooled study included; SQL `count`
  ignores a missing value [whitepapers/utilities/util_count_unique_values.sas:38-42]. SAS reference:
  `CSS_NEXTPARAM` 996 observations, then `CSS_NEXTTIMEPT` 500 (ATPTN 815) and 496 (ATPTN 817)
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:859,936,1295].
- `Disposition:` Ported → `programs.wpct_f_07_07.run`
- `Relies on:` D-04, D-14, D-16
- `Port notes:` An empty population after the derivation gives zero strata, exports with header rows
  only, and checkpoints of 0.

#### S07-MOD-01 — Summary statistics by study and treatment

- `Category:` MOD
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:329-334], `%boxplot_each_param_tp` / PROC SUMMARY
- `Rule:` `PROC SUMMARY data=css_nexttimept; BY studyid trtpn studynum trtp_short; VAR chg;` with the
  statistics, column order and missing-value handling of S02-MOD-01 (12 columns). The data are sorted
  only BY `STUDYID TRTPN`; the BY statement relies on `studynum` being constant within `STUDYID` and
  `trtp_short` within `TRTPN`. SAS reference: `CSS_STATS` has 9 observations and 12 variables per
  timepoint [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1111,1476].
- `Disposition:` Ported → `summary.proc_summary` (quantiles through `summary.sas_quantile_def5`)
- `Relies on:` D-01, D-02, D-03, D-04, D-14

#### S07-MOD-02 — Per-study ANCOVA

- `Category:` MOD
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:338-352], `%boxplot_each_param_tp` / PROC GLM BY
  `studynum`
- `Rule:` Runs only when `%length(&b_var) > 0 and %length(&ref_trtn) > 0`
  [whitepapers/WPCT/WPCT-F.07.07.sas:338]. ODS outputs `ParameterEstimates` to `css_pval_stdy`
  [whitepapers/WPCT/WPCT-F.07.07.sas:343-344]. `PROC GLM data=css_nexttimept; CLASS trtpn (ref="0");
  BY studynum; WHERE studyid ^= 'A0'x !! 'Pooled'; MODEL chg = base trtpn / solution;`
  [whitepapers/WPCT/WPCT-F.07.07.sas:346-352]. The data are in `studynum` order because `studynum`
  increases with `STUDYID`, the leading sort key. Each BY group has the rows `Intercept`, `base`,
  `TRTPN 54`, `TRTPN 81` and `TRTPN 0` [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:3-32,51-106].
  SAS reference: `CSS_PVAL_STDY` has 10 observations and 8 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1128,1493].
- `Disposition:` Ported → `model.glm_solution` per study: one fit per `studynum` present, ascending,
  with the terms `BASE TRTPN`
- `Relies on:` D-05, D-06, D-07, D-08, D-09, D-16

#### S07-MOD-03 — Pooled ANCOVA

- `Category:` MOD
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:355-363], `%boxplot_each_param_tp` / PROC GLM
- `Rule:` ODS outputs `ParameterEstimates` to `css_pval_pool` [whitepapers/WPCT/WPCT-F.07.07.sas:355-356].
  `PROC GLM data=css_nexttimept; CLASS trtpn (ref="0"); WHERE studyid = 'A0'x !! 'Pooled'; MODEL chg =
  base trtpn studynum / solution;` [whitepapers/WPCT/WPCT-F.07.07.sas:358-363]. `studynum` is not a
  CLASS variable, so it enters as one continuous column. It is constant on the pooled records, hence
  linearly dependent on the Intercept, and GLM reports it with estimate `0.00000000`, flag `B` and
  missing standard error, t and p [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:47,147]. SAS reference:
  `CSS_PVAL_POOL` has 6 observations and 7 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1144,1509].
- `Disposition:` Ported → `model.glm_solution` with `studynum` as a continuous term (terms
  `BASE TRTPN studynum`)
- `Relies on:` D-05, D-06, D-07, D-08, D-09, D-16; see DIV-01

#### S07-TRN-08 — Per-study and pooled p-value merge

- `Category:` TRN
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:366-394], `%boxplot_each_param_tp`, DATA steps and
  PROC SORT after the models
- `Rule:` Stack the rows of `css_pval_stdy` and `css_pval_pool` whose `Parameter` starts with `TRTPN`;
  on the pooled rows `studynum = &stdyn` [whitepapers/WPCT/WPCT-F.07.07.sas:366-370]. Prepend the
  structure of `css_stats (keep=studynum trtpn)` [whitepapers/WPCT/WPCT-F.07.07.sas:374-378]; set
  `pval = Probt` and `TRTPN = input(scan(parameter,-1,' '), best8.)`
  [whitepapers/WPCT/WPCT-F.07.07.sas:380-385]; sort BY `studynum TRTPN` and match-merge into `css_stats`
  BY `studynum TRTPN` [whitepapers/WPCT/WPCT-F.07.07.sas:387-394]. Every study-by-treatment row receives
  its own level's `pval`, the reference level's is missing, and the pooled rows match because their
  `studynum` equals `&stdyn` (S07-TRN-06, S07-TRN-07). SAS reference: 9 `TRTPN` rows, then `CSS_STATS`
  with 9 observations and 13 variables
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1162,1215,1528,1581].
- `Disposition:` Ported → `programs.wpct_f_07_07.run` (`pval` column)
- `Relies on:` D-05, D-07

#### S07-PRS-01 — Presentation steps

- `Category:` PRS
- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:216,284,319-325,404-473], `&c_mode` label,
  `%util_proc_template`, Y-axis, format and pagination macros, DATA `css_plot`, graphics options,
  titles, footnotes, ODS PDF and PROC SGRENDER
- `Rule:` The upper-cased `&c_mode` label [whitepapers/WPCT/WPCT-F.07.07.sas:216]; the GTL template
  [whitepapers/WPCT/WPCT-F.07.07.sas:284]; the Y-axis range, display formats and page blocks
  [whitepapers/WPCT/WPCT-F.07.07.sas:319-325]; `css_plot`, `options missing=' '`, titles, footnotes,
  axis ticks, the PDF file and the box-plot pages [whitepapers/WPCT/WPCT-F.07.07.sas:404-473]. As in
  07.02, `options missing=' '` [whitepapers/WPCT/WPCT-F.07.07.sas:413] is set after the first
  timepoint's models, so missing values print as `.` in the first three tables and blank in the last
  three [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:16,32,46-47,64,106,146-147].
- `Disposition:` Excluded — presentation only (user exclusion; disposition rule)
- `Relies on:` —

### B.3 `%derive_lastminmax_measure` items

Source: `whitepapers/ADaM/derive_lastminmax_measure.sas`, macro `%derive_lastminmax_measure`, defined
at lines 77-213 and called by S07-TRN-03. GRPVARS, ORDVARS and INCL below are the call's arguments.

#### LMM-FLT-01 — Records without dates or value

- `Category:` FLT
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:92], DATA `lmm_base lmm_post`
- `Rule:` Delete a record when `n(adt, trtsdt) < 2` (either date missing) or `AVAL` is missing. SAS
  reference: 5926 records read; `LMM_BASE` 1514 and `LMM_POST` 4411 observations, 45 variables each
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:78-80].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` D-10, D-13

#### LMM-TRN-01 — Baseline or post-baseline phase

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:94-95], DATA `lmm_base lmm_post`
- `Rule:` A record with `ADT <= TRTSDT` goes to `LMM_BASE` (baseline); every other record goes to
  `LMM_POST` (post-baseline). Both dates are SAS day numbers and non-missing after LMM-FLT-01.
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` D-10, D-13

#### LMM-TRN-02 — Change modes and flags

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:81,115-130], `%let` and `%do %while`
  over C_MODES
- `Rule:` C_MODES is upper-cased [whitepapers/ADaM/derive_lastminmax_measure.sas:81]. Each token is
  processed in order with index idx = 1, 2, … [whitepapers/ADaM/derive_lastminmax_measure.sas:113-117,208].
  `MIN` selects `MIN(AVAL)`, `MAX` selects `MAX(AVAL)`, and `LAST` selects `MAX(ADT)`; any other token
  logs a WARNING and is processed as `LAST` [whitepapers/ADaM/derive_lastminmax_measure.sas:119-128].
  The flag is the idx-th FLVARS name, or `anl<mode>fl` when FLVARS has no idx-th name
  [whitepapers/ADaM/derive_lastminmax_measure.sas:117,130]. A blank DSOUT defaults to
  `WORK.<ds>_chgmodes` [whitepapers/ADaM/derive_lastminmax_measure.sas:82]. 07.07 uses the single mode
  `MIN` with flag `anl02fl`.
- `Disposition:` Ported → `derivation.derive_lastminmax_measure` (all modes)
- `Relies on:` —

#### LMM-TRN-03 — Group extremum records

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:133-147], PROC SQL
- `Rule:` Separately for `LMM_BASE` and `LMM_POST`: `select aval, adt, GRPVARS, ORDVARS, INCL … group by
  GRPVARS having <func>(<var>) = <var> order by GRPVARS, ORDVARS`. The query remerges the group
  statistic, so every record whose value equals its group's extremum is kept, ties included. The order
  among records equal on every ORDER BY key is not defined (D-15, U-01). SAS reference: `LMM_BASE_ANL`
  629 rows and `LMM_POST_ANL` 794 rows, 12 columns each
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:95,98].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` D-15

#### LMM-TRN-04 — One record per group

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:150-154], PROC SORT NODUPKEY BY GRPVARS
- `Rule:` Each of the two tables keeps the first record per GRPVARS group in its LMM-TRN-03 order, so
  among tied records the one with the lowest ORDVARS (`AVISITN`) survives. SAS reference: 121 and 296
  duplicates deleted, leaving 508 baseline and 498 post-baseline records
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:106-108,115-117].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` D-14, D-15

#### LMM-FLT-02 — Groups with both phases

- `Category:` FLT
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:159-169,177-189], DATA `lmm_base_anl`
  and DATA `lmm_post_anl`, match-merges BY GRPVARS
- `Rule:` A baseline record is kept only when its group has a post-baseline record; otherwise the log
  shows `WARNING: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure, such as`
  and the record is deleted [whitepapers/ADaM/derive_lastminmax_measure.sas:159-169]. A post-baseline
  record is then kept only when its group still has a baseline record; otherwise the log shows
  `WARNING: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a baseline measure, such as` and the record
  is deleted [whitepapers/ADaM/derive_lastminmax_measure.sas:177-189]. SAS reference: the log has 10
  warnings, all `Omitting obs without a post-baseline measure`
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-183]: baseline records dropped because their group
  has no post-baseline record, 508 → 498 [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:184-186]. No
  post-baseline record was dropped, 498 → 498 [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:193-195].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure` (omission counts kept)
- `Relies on:` —
- `Port notes:` Each omission is logged as a warning through Python `logging`, and the omitted-baseline
  and omitted-post-baseline counts are returned in `counts`.

#### LMM-TRN-05 — Baseline record values

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:171-174], DATA `lmm_base_anl`
- `Rule:` On each kept baseline record: `AVISIT = 'Baseline (<mode>)'`, `AVISITN = 9900 + 10·idx + 1`
  (9911 for the first mode), `BASE = AVAL` and `CHG = 0`.
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` —

#### LMM-TRN-06 — Post-baseline record values

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:191-193], DATA `lmm_post_anl`
- `Rule:` On each kept post-baseline record: `AVISIT = 'Post-baseline (<mode>)'`,
  `AVISITN = 9900 + 10·idx + 2` (9912 for the first mode), `BASE` taken from the group's baseline
  record by the merge [whitepapers/ADaM/derive_lastminmax_measure.sas:180-182], and `CHG = AVAL − BASE`.
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` —

#### LMM-TRN-07 — Output structure

- `Category:` TRN
- `SAS source:` [whitepapers/ADaM/derive_lastminmax_measure.sas:97-110,197-206], CALL SYMPUT of the
  AVISIT length, DATA `&dsout` structure step and append step
- `Rule:` `avlen` receives the storage length of the input `AVISIT`
  [whitepapers/ADaM/derive_lastminmax_measure.sas:98]. When it is below 20, `AVISIT` is declared `$20`
  [whitepapers/ADaM/derive_lastminmax_measure.sas:101,106,178], so `Post-baseline (LAST)` (20 characters)
  is not truncated; otherwise no LENGTH statement is generated
  [whitepapers/ADaM/derive_lastminmax_measure.sas:102]. DSOUT starts with no rows, as `AVISIT` followed
  by the GRPVARS, ORDVARS and INCL variables in their order in the input dataset (`KEEP=` does not
  reorder) [whitepapers/ADaM/derive_lastminmax_measure.sas:105-110]. For each mode the baseline records
  and then the post-baseline records are appended; this adds `AVAL ADT BASE CHG` and the mode's flag
  (`$1`), which is `'Y'` on the appended records and blank on records of earlier modes
  [whitepapers/ADaM/derive_lastminmax_measure.sas:197-206]. For 07.07 the result is `AVISIT STUDYID
  USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISITN trtp_short AVAL ADT BASE CHG ANL02FL`. SAS
  reference: `ADVS_MIN` has 0 observations and 11 variables after the structure step, then 996
  observations and 16 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:88,205].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure`
- `Relies on:` D-12
- `Port notes:` Empty input returns an empty frame with these columns and zero counts (U-03 guard). The
  port never truncates `AVISIT`; the repository inputs store `AVISIT` with length 16, so SAS takes the
  `$20` branch, under which no value written by LMM-TRN-05 or LMM-TRN-06 is truncated.

### B.4 Dependency list for 07.07

07.07 reaches 22 macros: the 19 of 07.02 plus `derive_lastminmax_measure`, `assert_var_nonmissing` and
`assert_unique_keys`.

| ID | Calls | Disposition |
|----|-------|-------------|
| S07-DEP-util_access_test_data | [whitepapers/WPCT/WPCT-F.07.07.sas:141] | Ported (local read) → `xpt_reader.read_xpt`; remote-URL branch excluded |
| S07-DEP-derive_lastminmax_measure | [whitepapers/WPCT/WPCT-F.07.07.sas:161-166] | Ported → `derivation.derive_lastminmax_measure` |
| S07-DEP-assert_depend | [whitepapers/WPCT/WPCT-F.07.07.sas:193-202]; nested | Partly ported → `xpt_reader.require_variables`; session checks excluded |
| S07-DEP-assert_continue | [whitepapers/WPCT/WPCT-F.07.07.sas:204,220,245] | Ported → `errors.AnalysisHalted` |
| S07-DEP-assert_var_nonmissing | [whitepapers/WPCT/WPCT-F.07.07.sas:219] | Ported → `selection.assert_var_nonmissing` |
| S07-DEP-assert_unique_keys | [whitepapers/WPCT/WPCT-F.07.07.sas:244] | Ported → `selection.assert_unique_keys` |
| S07-DEP-assert_dset_exist | nested | Ported → `xpt_reader.read_xpt` |
| S07-DEP-assert_var_exist | nested | Ported → `xpt_reader.require_variables` |
| S07-DEP-assert_macro_exist | nested | Excluded — SAS autocall path |
| S07-DEP-util_resolve_sasautos | nested | Excluded — SAS autocall path |
| S07-DEP-util_labels_from_var | [whitepapers/WPCT/WPCT-F.07.07.sas:259,300] | Partly ported → stratum order; labels excluded |
| S07-DEP-util_count_unique_values | [whitepapers/WPCT/WPCT-F.07.07.sas:262]; nested | Ported → `&stdyn` and loop counts |
| S07-DEP-util_get_var_min_max | [whitepapers/WPCT/WPCT-F.07.07.sas:319] | Excluded — Y-axis range |
| S07-DEP-util_value_format | [whitepapers/WPCT/WPCT-F.07.07.sas:322] | Excluded — display formats |
| S07-DEP-util_proc_template | [whitepapers/WPCT/WPCT-F.07.07.sas:284] | Excluded — GTL template |
| S07-DEP-util_boxplot_block_ranges | [whitepapers/WPCT/WPCT-F.07.07.sas:325] | Excluded — pagination |
| S07-DEP-util_axis_order | [whitepapers/WPCT/WPCT-F.07.07.sas:424] | Excluded — axis ticks |
| S07-DEP-util_delete_dsets | [whitepapers/WPCT/WPCT-F.07.07.sas:396,481]; nested | Excluded — WORK cleanup |
| S07-DEP-boxplot_each_param_tp | [whitepapers/WPCT/WPCT-F.07.07.sas:485] | Partly ported → loops, statistics, models, merge; graphics excluded |
| S07-DEP-qtrim | nested | Excluded — SAS-supplied, macro text handling |
| S07-DEP-qleft | nested | Excluded — SAS-supplied, autocall path |
| S07-DEP-datatyp | nested | Excluded — SAS-supplied, presentation argument check |

#### S07-DEP-util_access_test_data — Test-data access

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_access_test_data.sas:43-76]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:141] (the remote call with `folder=cdisc-split` at
  [whitepapers/WPCT/WPCT-F.07.07.sas:138] is commented out)
- `Rule:` As S02-DEP-util_access_test_data. The call passes a UNC `local=` path (ENV-04), so the macro
  reads `<local>ADVS.xpt` [whitepapers/utilities/util_access_test_data.sas:47-57] and copies member
  `ADVS` to `WORK.ADVS` [whitepapers/utilities/util_access_test_data.sas:62-66]; the remote branch would
  read `…/data/adam/cdisc-split/ADVS.xpt` [whitepapers/utilities/util_access_test_data.sas:58-60].
- `Disposition:` Ported (local read) → `xpt_reader.read_xpt`, reading the repository copy
  `data/adam/cdisc-split/advs.xpt`; the remote-URL branch
  [whitepapers/utilities/util_access_test_data.sas:58-60] is excluded — there is no network access
  (ENV-04)
- `Relies on:` D-10, D-11
- `Port notes:` A path that does not exist or is not a regular file raises
  `InputDataError("WPCT-F.07.07: input dataset not found: <absolute path>")`. A file the reader cannot
  read, or that yields no columns, raises
  `InputDataError("WPCT-F.07.07: cannot read SAS transport file <absolute path>: <reader message>")`.

#### S07-DEP-derive_lastminmax_measure — Last/minimum/maximum derivation

- `Category:` DEP
- `SAS source:` defined [whitepapers/ADaM/derive_lastminmax_measure.sas:77-213]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:161-166]
- `Rule:` Items LMM-FLT-01 to LMM-TRN-07 (B.3). With `cleanup=1` it finally deletes its four work tables
  through `%util_delete_dsets` [whitepapers/ADaM/derive_lastminmax_measure.sas:211].
- `Disposition:` Ported → `derivation.derive_lastminmax_measure` (items LMM-*); its `%util_delete_dsets`
  cleanup [whitepapers/ADaM/derive_lastminmax_measure.sas:211] is excluded
- `Relies on:` D-10, D-14, D-15

#### S07-DEP-assert_depend — Dependency assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_depend.sas:51-162]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:193-202]; nested [whitepapers/utilities/util_boxplot_block_ranges.sas:38]
- `Rule:` As S02-DEP-assert_depend. 07.07 checks `WORK.ADVS_MIN` (`&m_lb..&m_ds`) for the 12 analysis
  variables [whitepapers/WPCT/WPCT-F.07.07.sas:190,196], the macros listed at
  [whitepapers/WPCT/WPCT-F.07.07.sas:197-199] and the symbols listed at
  [whitepapers/WPCT/WPCT-F.07.07.sas:200-201].
- `Disposition:` Partly ported → `xpt_reader.require_variables` for the `vars=` check
  [whitepapers/utilities/assert_depend.sas:102-128]; excluded — the OS, SASV, SYSPROD, macros and
  symbols checks, which test the SAS session (the Python environment is fixed by `requirements.txt`),
  and the nested pagination call (S07-DEP-util_boxplot_block_ranges)
- `Relies on:` —
- `Port notes:` A missing variable raises
  `InputDataError("WPCT-F.07.07: <absolute path> lacks required variables: <names>")`. Required
  variables match case-insensitively, as `varnum()` does [whitepapers/utilities/assert_var_exist.sas:23],
  and are returned under their spelling in `schemas.REQUIRED_INPUT_VARIABLES`; a required variable
  stored under more than one spelling raises
  `InputDataError("WPCT-F.07.07: <absolute path> has variables that differ only in case: <columns>")`.

#### S07-DEP-assert_continue — Halt on failed assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_continue.sas:22-39]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:204,220,245]
- `Rule:` As S02-DEP-assert_continue. 07.07 tests `CONTINUE` after the dependency assertion (line 204),
  after the missing-value assertion (S07-FLT-04, line 220) and after the unique-key assertion
  (S07-FLT-05, line 245).
- `Disposition:` Ported → `errors.AnalysisHalted` stops the driver before any export
- `Relies on:` —

#### S07-DEP-assert_var_nonmissing — Non-missing assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_var_nonmissing.sas:31-73]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:219]
- `Rule:` Returns 1 when the dataset has no record with a missing value of VAR (within WHR, when given),
  0 with a WARNING otherwise; 0 with an ERROR when the dataset or variable is not accessible
  [whitepapers/utilities/assert_var_nonmissing.sas:34-69].
- `Disposition:` Ported → `selection.assert_var_nonmissing` (S07-FLT-04)
- `Relies on:` —

#### S07-DEP-assert_unique_keys — Unique-key assertion

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_unique_keys.sas:45-112]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:244]
- `Rule:` Sets the global `CONTINUE` to 1 or 0 according to S07-FLT-05
  [whitepapers/utilities/assert_unique_keys.sas:47-102]; on PASS it deletes `WORK.FAIL_AUK` through
  `%util_delete_dsets` [whitepapers/utilities/assert_unique_keys.sas:105].
- `Disposition:` Ported → `selection.assert_unique_keys` (S07-FLT-05); its `%util_delete_dsets` call
  [whitepapers/utilities/assert_unique_keys.sas:105] is excluded
- `Relies on:` D-13

#### S07-DEP-assert_dset_exist — Dataset existence

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_dset_exist.sas:19-45]; nested
  [whitepapers/utilities/assert_depend.sas:109; whitepapers/utilities/util_labels_from_var.sas:51;
  whitepapers/utilities/util_count_unique_values.sas:33; whitepapers/utilities/util_get_var_min_max.sas:37;
  whitepapers/utilities/util_value_format.sas:35; whitepapers/utilities/assert_var_exist.sas:14;
  whitepapers/utilities/assert_var_nonmissing.sas:34; whitepapers/utilities/assert_unique_keys.sas:51]
- `Rule:` As S02-DEP-assert_dset_exist.
- `Disposition:` Ported → file-existence check in `xpt_reader.read_xpt` (see
  S07-DEP-util_access_test_data)
- `Relies on:` —

#### S07-DEP-assert_var_exist — Variable existence

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_var_exist.sas:11-39]; nested
  [whitepapers/utilities/assert_depend.sas:114; whitepapers/utilities/util_labels_from_var.sas:52-53;
  whitepapers/utilities/util_count_unique_values.sas:34; whitepapers/utilities/util_get_var_min_max.sas:38;
  whitepapers/utilities/util_value_format.sas:36; whitepapers/utilities/assert_var_nonmissing.sas:40;
  whitepapers/utilities/assert_unique_keys.sas:60,64]
- `Rule:` As S02-DEP-assert_var_exist. The INCL checks of `%assert_unique_keys`
  [whitepapers/utilities/assert_unique_keys.sas:72,76] are not reached, because 07.07 passes no INCL.
- `Disposition:` Ported → variable-existence check in `xpt_reader.require_variables`
- `Relies on:` —

#### S07-DEP-assert_macro_exist — Macro availability

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/assert_macro_exist.sas:33-68]; nested
  [whitepapers/utilities/assert_depend.sas:134], once per name in the `macros=` list
  [whitepapers/WPCT/WPCT-F.07.07.sas:197-199]
- `Rule:` As S02-DEP-assert_macro_exist.
- `Disposition:` Excluded — inspects the SAS autocall path; there is no Python counterpart
- `Relies on:` —

#### S07-DEP-util_resolve_sasautos — Autocall path list

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_resolve_sasautos.sas:18-118]; nested
  [whitepapers/utilities/assert_macro_exist.sas:41]
- `Rule:` As S02-DEP-util_resolve_sasautos.
- `Disposition:` Excluded — inspects the SAS autocall path; there is no Python counterpart
- `Relies on:` —

#### S07-DEP-util_labels_from_var — Value and label symbols

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_labels_from_var.sas:45-94]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:259,300]
- `Rule:` As S02-DEP-util_labels_from_var. Call 259 (`PARAMCD`/`PARAM` on `css_anadata`) and call 300
  (`ATPTN`/`ATPT` on `css_nextparam`) drive the loops; their label symbols feed titles.
- `Disposition:` Partly ported → the sorted value lists drive the stratum order in
  `programs.wpct_f_07_07.run` (S07-TRN-07); excluded — the label symbols, which feed titles only
- `Relies on:` —

#### S07-DEP-util_count_unique_values — Distinct-value count

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_count_unique_values.sas:27-51]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:262]; nested [whitepapers/utilities/util_labels_from_var.sas:59]
- `Rule:` As S02-DEP-util_count_unique_values. The direct call sets `&stdyn`, the number of distinct
  `STUDYID` values including the pooled study [whitepapers/WPCT/WPCT-F.07.07.sas:262]; the nested calls
  set the loop bounds.
- `Disposition:` Ported → `programs.wpct_f_07_07.run`: `&stdyn` (S07-TRN-07, S07-TRN-08) and the loop
  bounds (S07-TRN-07)
- `Relies on:` —

#### S07-DEP-util_get_var_min_max — Y-axis range

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_get_var_min_max.sas:32-69]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:319] (the alternative at line 304 is commented out)
- `Rule:` Stores the minimum and maximum of `CHG` in `&aval_min_max` for the Y axis.
- `Disposition:` Excluded — Y-axis range only
- `Relies on:` —

#### S07-DEP-util_value_format — Display formats

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_value_format.sas:31-79]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:322]
- `Rule:` As S02-DEP-util_value_format; the formats are used only in the FORMAT statement of `css_plot`
  [whitepapers/WPCT/WPCT-F.07.07.sas:408].
- `Disposition:` Excluded — display formats only; statistics stay at full precision
- `Relies on:` —

#### S07-DEP-util_proc_template — Graph template

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_proc_template.sas:39-218]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:284]
- `Rule:` Compiles the GTL template `PhUSEboxplot` that PROC SGRENDER uses.
- `Disposition:` Excluded — GTL template (user exclusion)
- `Relies on:` —

#### S07-DEP-util_boxplot_block_ranges — Page blocks

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_boxplot_block_ranges.sas:33-133]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:325] with `blockvar=studynum`
- `Rule:` As S02-DEP-util_boxplot_block_ranges, with study blocks in place of visit blocks.
- `Disposition:` Excluded — pagination
- `Relies on:` —

#### S07-DEP-util_axis_order — Axis ticks

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_axis_order.sas:29-64]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:424]
- `Rule:` Returns the Y-axis minimum, maximum and tick increment.
- `Disposition:` Excluded — axis ticks
- `Relies on:` —

#### S07-DEP-util_delete_dsets — WORK cleanup

- `Category:` DEP
- `SAS source:` defined [whitepapers/utilities/util_delete_dsets.sas:34-53]; called
  [whitepapers/WPCT/WPCT-F.07.07.sas:396,481]; nested
  [whitepapers/utilities/util_access_test_data.sas:70; whitepapers/utilities/util_boxplot_block_ranges.sas:123;
  whitepapers/ADaM/derive_lastminmax_measure.sas:211; whitepapers/utilities/assert_unique_keys.sas:105]
- `Rule:` Deletes the listed WORK datasets with PROC DATASETS.
- `Disposition:` Excluded — SAS WORK cleanup; Python holds no WORK library
- `Relies on:` —

#### S07-DEP-boxplot_each_param_tp — Inline driver macro

- `Category:` DEP
- `SAS source:` defined inline [whitepapers/WPCT/WPCT-F.07.07.sas:286-483]; invoked
  [whitepapers/WPCT/WPCT-F.07.07.sas:485] with the defaults `plotds=css_anadata, cleanup=1`
- `Rule:` Loops over parameters and timepoints (S07-TRN-07), computes the statistics (S07-MOD-01), fits
  the per-study and pooled models and merges their p-values (S07-MOD-02, S07-MOD-03, S07-TRN-08), draws
  the box plots (S07-PRS-01) and deletes its WORK datasets (S07-DEP-util_delete_dsets).
- `Disposition:` Partly ported → the loops, statistics, models and p-value merge, in
  `programs.wpct_f_07_07.run`, `summary.proc_summary` and `model.glm_solution`; excluded — the graphics
  (S07-PRS-01) and the cleanup
- `Relies on:` —

#### S07-DEP-qtrim — SAS-supplied `%qtrim`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/assert_depend.sas:106; whitepapers/utilities/util_resolve_sasautos.sas:102-103]
- `Rule:` As S02-DEP-qtrim.
- `Disposition:` Excluded — source not in the repository; macro-argument text handling (the port
  receives variable lists as data)
- `Relies on:` —

#### S07-DEP-qleft — SAS-supplied `%qleft`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/util_resolve_sasautos.sas:75,84]
- `Rule:` As S02-DEP-qleft.
- `Disposition:` Excluded — source not in the repository; serves the excluded autocall-path inspection
- `Relies on:` —

#### S07-DEP-datatyp — SAS-supplied `%datatyp`

- `Category:` DEP
- `SAS source:` SAS-supplied autocall macro, source not in the repository; nested
  [whitepapers/utilities/util_axis_order.sas:33,36]
- `Rule:` As S02-DEP-datatyp.
- `Disposition:` Excluded — source not in the repository; validates arguments of the presentation-only
  `%util_axis_order`
- `Relies on:` —

**Not dependencies of 07.07**

- `%util_autocallpath` appears only in comments [whitepapers/utilities/assert_depend.sas:11;
  whitepapers/utilities/assert_macro_exist.sas:13-15], and `%assert_sym_non_miss` only in a TO-DO
  comment [whitepapers/utilities/assert_depend.sas:8]. Neither is invoked and neither has a file
  (ENV-09).
- Intrinsic macro functions and statements are not dependencies, as for 07.02.

## Part C — SAS-versus-Python default differences

Each entry names the SAS behaviour, the default of the Python library that would otherwise apply, the
setting the port uses to follow SAS, and the items that rely on it.

### Default-difference entries

#### D-01 — Quantile definition

- `SAS behaviour:` PROC MEANS/SUMMARY defaults to `QNTLDEF=5`. For n sorted non-missing values
  x_1 … x_n and probability p, let j = ⌊np⌋ and g = np − j; the quantile is (x_j + x_{j+1})/2 when
  g = 0, and x_{j+1} otherwise.
- `Python default:` `numpy.quantile` and `pandas.Series.quantile` use linear interpolation
  (Hyndman–Fan type 7). On the repository data the two differ: the DIABP/815 Week 2 (`AVISITN 2`)
  placebo Q3 is 5.5 under `linear` and 6.0 under definition 5, both computed from `CHG` by the
  definitions; no SAS statistic is quoted.
- `Port setting:` `summary.sas_quantile_def5` computes np as an exact `fractions.Fraction`
  (p ∈ {1/4, 1/2, 3/4}), so g = 0 is decided without floating-point error. It is cross-checked against
  `numpy.quantile(method='averaged_inverted_cdf')` (type 2, which equals definition 5).
- `Relied on by:` S02-MOD-01, S07-MOD-01

#### D-02 — Standard deviation divisor

- `SAS behaviour:` `VARDEF=DF`: the divisor is n − 1, and STD is missing when n < 2.
- `Python default:` `numpy.std` uses `ddof=0`; `pandas.Series.std` uses `ddof=1`.
- `Port setting:` `ddof=1`, with NaN when n < 2.
- `Relied on by:` S02-MOD-01, S07-MOD-01

#### D-03 — Missing analysis values

- `SAS behaviour:` Only the analysis variable `CHG` is examined. A missing `CHG` is excluded from N and
  from every statistic, but the record still forms its BY group; a missing `BASE` has no effect. A
  group whose `CHG` values are all missing gets n = 0 and missing statistics.
- `Python default:` numpy propagates NaN; `pandas.Series.count` excludes it.
- `Port setting:` Drop NaN `CHG` per group before computing, and never consult `BASE`. `n` is the
  non-missing `CHG` count; when n = 0 every statistic is NaN.
- `Relied on by:` S02-MOD-01, S07-MOD-01

#### D-04 — Missing BY keys and BY-group order

- `SAS behaviour:` Missing values sort lowest, and BY groups follow the sorted order.
- `Python default:` pandas sorts NaN last, and `groupby` drops NaN keys by default.
- `Port setting:` `sas_semantics.sas_sort` sorts stably with missing values first; groups use
  `groupby(dropna=False, sort=False)` over the sorted frame.
- `Relied on by:` S02-TRN-04, S02-MOD-01, S07-TRN-06, S07-TRN-07, S07-MOD-01

#### D-05 — Less-than-full-rank GLM design and `SOLUTION` output

- `SAS behaviour:` The design columns follow the MODEL statement: the Intercept, then each term in
  order, a CLASS term contributing one indicator per level. `SOLUTION` writes a ParameterEstimates row
  for every column: Parameter, Estimate, the `B` (biased) flag, StdErr, tValue and Probt, where Probt
  is the two-sided t probability on the error degrees of freedom.
- `Python default:` statsmodels `params`, `bse`, `tvalues`, `pvalues` and `df_resid` compute the same
  quantities on a full-rank design.
- `Port setting:` Fit statsmodels on the retained columns (D-06); `DFE = n − rank`; emit a row for every
  design column, `TRTPN 0` included.
- `Relied on by:` S02-MOD-02, S02-TRN-05, S07-MOD-02, S07-MOD-03, S07-TRN-08

#### D-06 — Sequential aliasing versus `pinv`

- `SAS behaviour:` The modified sweep sets every column that is linearly dependent on the preceding
  columns to 0, flags it `B`, and leaves its StdErr, tValue and Probt missing.
- `Python default:` statsmodels `fit()` uses `method='pinv'`, which returns the minimum-norm solution:
  for the pooled model it would split the Intercept between the Intercept and the constant `studynum`.
- `Port setting:` In design-column order, column j is aliased when the norm of its least-squares
  residual on the retained earlier columns is at most 1e-9 × ‖x_j‖. Aliased columns are dropped before
  `statsmodels.api.OLS(y, X_retained).fit()` and reported with `ESTIMATE = 0.0`, NaN `STDERR`, `TVALUE`
  and `PROBT`, and `ALIASED` true.
- `Relied on by:` S02-MOD-02, S07-MOD-02, S07-MOD-03

#### D-07 — CLASS level order and `REF=`

- `SAS behaviour:` Levels of an unformatted numeric CLASS variable are ordered by value (the default
  `ORDER=FORMATTED` reduces to value order). `REF="0"` matches the formatted value, here the raw value,
  and moves that level last; its indicator is dependent on the Intercept and its sibling indicators.
- `Python default:` patsy `Treatment(reference=0)` drops the reference column and orders the reference
  level first.
- `Port setting:` Ascending numeric levels with the reference level last, all indicators retained;
  parameter labels such as `TRTPN 54` through `sas_semantics.format_level`.
- `Relied on by:` S02-MOD-02, S02-TRN-05, S07-MOD-02, S07-MOD-03, S07-TRN-08

#### D-08 — GLM missing values

- `SAS behaviour:` An observation with a missing value in the response or in any variable of the
  current MODEL statement is excluded from that fit.
- `Python default:` statsmodels defaults to `missing='none'`, which propagates NaN.
- `Port setting:` Complete-case exclusion over the response and the terms of the current fit only:
  `CHG BASE TRTPN` for the 07.02 and per-study 07.07 fits, plus `studynum` for the pooled fit. A frame
  without a `studynum` column fits normally. In 07.02 each endpoint model uses 226 of 227 records.
- `Relied on by:` S02-MOD-02, S07-MOD-02, S07-MOD-03

#### D-09 — Zero error degrees of freedom

- `SAS behaviour:` Estimates are computed; StdErr, tValue and Probt are missing.
- `Python default:` statsmodels returns `inf` or NaN with warnings.
- `Port setting:` When `DFE == 0`, `STDERR`, `TVALUE` and `PROBT` are NaN. A model with no usable records
  emits no parameter rows and gets the status `no observations`.
- `Relied on by:` S02-MOD-02, S07-MOD-02, S07-MOD-03

#### D-10 — Dates in the transport file

- `SAS behaviour:` Dates are numeric day counts from 1960-01-01; `ADT`, `TRTSDT` and `TRTEDT` carry the
  `DATE9.` format.
- `Python default:` `pyreadstat.read_xport` converts `DATE9.` values to `datetime.date`.
- `Port setting:` `read_xport(path, disable_datetime_conversion=True)`, which keeps the day numbers.
- `Relied on by:` S07-TRN-03, LMM-FLT-01, LMM-TRN-01, S02-DEP-util_access_test_data,
  S07-DEP-util_access_test_data

#### D-11 — Character missing values and trailing blanks

- `SAS behaviour:` A blank character value is missing, and comparisons ignore trailing blanks.
- `Python default:` `pyreadstat` strips trailing blanks and NULs and returns a blank value as `''`.
- `Port setting:` `''` is missing (`sas_semantics.is_char_missing`); `'Y'` matches only `'Y'`; the `=:`
  prefix test is `sas_semantics.prefix_match`.
- `Relied on by:` S02-FLT-01, S02-TRN-02, S02-POP-01, S02-FLT-02, S07-FLT-01, S07-TRN-02, S07-FLT-02,
  S07-POP-01, S07-FLT-03, S02-DEP-util_access_test_data, S07-DEP-util_access_test_data

#### D-12 — Fixed character length

- `SAS behaviour:` Assignment to a `$6` variable truncates a longer value: `UNEXPECTED` is stored as
  `UNEXPE`.
- `Python default:` Python strings are unbounded.
- `Port setting:` `sas_semantics.assign_char(value, 6)`.
- `Relied on by:` S02-TRN-02, S07-TRN-02, LMM-TRN-07

#### D-13 — Comparisons with missing values

- `SAS behaviour:` `AVISITN ne 0` is true for a missing value, `ATPTN in (815)` is false for one, and a
  numeric missing value compares lower than any number, which would affect `ADT <= TRTSDT` had
  LMM-FLT-01 not deleted records with a missing date first. PROC SQL `count(var)` counts non-missing
  values only.
- `Python default:` pandas `NaN != 0` is True and `isin` is False for NaN, which match SAS.
- `Port setting:` The same expressions, asserted in tests; comparisons on dates run only after
  LMM-FLT-01, and duplicate-key counting follows SAS `count`.
- `Relied on by:` S02-FLT-01, S02-FLT-03, S07-FLT-01, S07-FLT-05, LMM-FLT-01, LMM-TRN-01

#### D-14 — Sort stability

- `SAS behaviour:` PROC SORT is stable (`EQUALS`), and NODUPKEY keeps the first record of each key in
  that order.
- `Python default:` pandas `sort_values` is not stable by default.
- `Port setting:` `kind='stable'` inside `sas_semantics.sas_sort`.
- `Relied on by:` S02-TRN-04, S02-MOD-01, S07-TRN-03, S07-TRN-06, S07-TRN-07, S07-MOD-01, LMM-TRN-04

#### D-15 — SQL tie order

- `SAS behaviour:` PROC SQL leaves the order among rows equal on every ORDER BY key undefined.
- `Python default:` `groupby().idxmin()` keeps one row by position and ignores the ORDVARS order.
- `Port setting:` Keep every row equal to the group extremum, `sas_semantics.sas_sort` by
  GRPVARS + ORDVARS + `_ROW` (input order), then keep the first row per group. Input order is the stated
  tie rule (U-01).
- `Relied on by:` S07-TRN-03, LMM-TRN-03, LMM-TRN-04

#### D-16 — The `'A0'x` pooled identifier

- `SAS behaviour:` `'A0'x !! 'Pooled'` sorts after ASCII study identifiers, so the pooled `studynum` is
  the last.
- `Python default:` No counterpart.
- `Port setting:` `STUDYID = '\u00a0Pooled'` (code point 160 sorts after ASCII) and
  `USUBJID = 'P' + USUBJID[1:]`. User-supplied SAS export CSVs are Latin-1, so the byte `A0` survives.
- `Relied on by:` S07-TRN-05, S07-TRN-06, S07-TRN-07, S07-MOD-02, S07-MOD-03

### Documentation sources for Part C

| Topic | Source |
|-------|--------|
| PROC MEANS/SUMMARY statistics, `QNTLDEF=`, `VARDEF=`, missing values | https://documentation.sas.com/api/docsets/proc/9.4/content/proc.pdf?locale=en |
| PROC GLM CLASS statement and `REF=` | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_syntax04.htm |
| PROC GLM syntax overview, `ORDER=` | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_syntax01.htm |
| PROC GLM parameterisation | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_details06.htm |
| PROC GLM estimability and the `B` flag | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_details64.htm |
| PROC GLM missing values | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_details60.htm |
| PROC GLM ODS table names | https://support.sas.com/documentation/cdl/en/statug/67523/HTML/default/statug_glm_details70.htm |
| Biased estimates in a less-than-full-rank model | https://support.sas.com/kb/22/585.html |
| numpy quantile methods | https://numpy.org/doc/stable/reference/generated/numpy.quantile.html |
| pandas standard deviation | https://pandas.pydata.org/docs/reference/api/pandas.Series.std.html |
| statsmodels OLS fit | https://www.statsmodels.org/stable/generated/statsmodels.regression.linear_model.OLS.fit.html |
| scipy t distribution | https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.t.html |
| pyreadstat transport reading | https://github.com/Roche/pyreadstat/blob/master/README.md |

## Part D — Legacy environment assumptions

Each entry is a SAS-era assumption about the computing environment. It is recorded and not fixed: the
SAS files stay unchanged, and the port neither depends on nor reproduces the assumption.

### Environment entries

#### ENV-01 — Empty autocall path entry (07.02)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:76]
- `Assumption:` `OPTIONS sasautos=(%sysfunc(getoption(sasautos)) "")` appends an empty path, so the
  PhUSE CS macros must already be in the session's autocall path.
- `Effect on the port:` None; the port imports its modules from `python/wpct/wpct/`.

#### ENV-02 — Blank `local=` reads the GitHub URL (07.02)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:85; whitepapers/utilities/util_access_test_data.sas:47-60]
- `Assumption:` With `local=` blank, the macro reads
  `https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/advs.xpt` instead of the
  repository file. The reference log instead read a local copy under `C:\CSS\`
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:26-29].
- `Effect on the port:` The port reads `data/adam/cdisc/advs.xpt` from the checkout and opens no network
  connection (S02-DEP-util_access_test_data).

#### ENV-03 — Blank `outputs_folder` (07.02)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:126]
- `Assumption:` With `outputs_folder` blank, the PDF path built at
  [whitepapers/WPCT/WPCT-F.07.02.sas:339] starts with `\WPCT-F.07.02_…`, the root of the current drive.
- `Effect on the port:` None; the port writes no PDF, and its outputs go to fixed locations under
  `python/wpct/`.

#### ENV-04 — UNC paths (07.07)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:119-141]
- `Assumption:` `outputs_folder` [whitepapers/WPCT/WPCT-F.07.07.sas:119], the `sasautos` paths
  [whitepapers/WPCT/WPCT-F.07.07.sas:121-122] and `local=` [whitepapers/WPCT/WPCT-F.07.07.sas:141] point
  to a `\\quintiles.net\…` network share. The reference log instead read a local copy under `C:\CSS\`
  [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:26-29].
- `Effect on the port:` The port reads `data/adam/cdisc-split/advs.xpt` from the checkout
  (S07-DEP-util_access_test_data).

#### ENV-05 — `m_ds` refers to `&LMM` before it is set (07.07)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.07.sas:100,111,114]
- `Assumption:` `%let m_ds = &ds._&LMM;` [whitepapers/WPCT/WPCT-F.07.07.sas:100] precedes
  `%let lmm = MIN;` [whitepapers/WPCT/WPCT-F.07.07.sas:111], so `&LMM` is unresolved when `m_ds` is
  assigned and resolves to `MIN` only when `&m_ds` is used afterwards, giving `ADVS_MIN`.
  `%let c_mode = &LMM;` [whitepapers/WPCT/WPCT-F.07.07.sas:114] follows line 111 and resolves directly.
  The reference log predates this ordering [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:70-76], so it
  does not show the behaviour.
- `Effect on the port:` None; the port has no macro-variable resolution, and the derived dataset it
  works on is `ADVS_MIN` (S07-TRN-03, S07-FLT-02).

#### ENV-06 — Operating-system and SAS-version gates

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:147-149; whitepapers/WPCT/WPCT-F.07.07.sas:193-195]
- `Assumption:` `%assert_depend` warns unless the OS is one of `AIX,WIN,HP IPF` and fails unless SAS is
  at least `9.4M2`.
- `Effect on the port:` None; the Python environment is fixed by `python/wpct/requirements.txt`
  (S02-DEP-assert_depend, S07-DEP-assert_depend).

#### ENV-07 — Incomplete `macros=` lists

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02.sas:151-153; whitepapers/WPCT/WPCT-F.07.07.sas:197-199]
- `Assumption:` Neither list names `util_access_test_data` or `derive_lastminmax_measure`, so the
  dependency assertion does not check the macro that reads the data (both programs) or the one that
  derives it (07.07); both run before the assertion.
- `Effect on the port:` None; the macros check is excluded (S02-DEP-assert_depend,
  S07-DEP-assert_depend).

#### ENV-08 — Placeholder manifest inputs and outputs (07.02)

- `SAS source:` [whitepapers/WPCT/WPCT-F.07.02_sas.yml:34-42]
- `Assumption:` The manifest's `Inputs` (`dm.xpt,ae.xpt,testfile.xlsx`, with generic parameters
  `p1`–`p3`) and `Outputs` (`out1, out2, out3`) are template text, not the program's real inputs and
  outputs.
- `Effect on the port:` None; the inputs are those of S02-DEP-util_access_test_data.

#### ENV-09 — Macro names that appear only in comments

- `SAS source:` [whitepapers/utilities/assert_depend.sas:8,11; whitepapers/utilities/assert_macro_exist.sas:13-15]
- `Assumption:` `%util_autocallpath` [whitepapers/utilities/assert_depend.sas:11;
  whitepapers/utilities/assert_macro_exist.sas:13-15] and `%assert_sym_non_miss`
  [whitepapers/utilities/assert_depend.sas:8] are named only in comments; neither is invoked and neither
  has a file in the repository.
- `Effect on the port:` None; they are listed under "Not dependencies" in A.3 and B.4.

## Part E — Divergences and unverified SAS behaviours

A divergence is a place where the port follows SAS although a requirement text describes something
else. An unverified behaviour is a SAS outcome that neither the repository nor the cited documentation
establishes; the port's choice for it is a Python decision.

### Divergences

#### DIV-01 — The pooled study term is aliased

- `Sources:` The migration request describes the pooled model as `CHG = BASE + TRT + STUDY`, and the
  Figure 7.7 requirement states "For Pooled data, the model includes STUDY ID, as well."
  [whitepapers/specification/WPCT_Fig_7.7_RequirementsSpecification.docx:word/document.xml, ANCOVA
  requirement paragraph].
- `SAS behaviour:` Every pooled record carries the same `STUDYID` before the studies are numbered
  [whitepapers/WPCT/WPCT-F.07.07.sas:227-241], so the non-CLASS `studynum` in the pooled model
  [whitepapers/WPCT/WPCT-F.07.07.sas:362] is constant and aliased with the Intercept: estimate
  `0.00000000`, flag `B`, missing standard error [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:47,147].
  No study effect is estimated.
- `Port decision:` The port reproduces SAS (S07-MOD-03, D-06). The pooled treatment estimates therefore
  equal those of `CHG = BASE + TRT` fitted to the pooled records.

### Unverified SAS behaviours

#### U-01 — Tie order inside the derivation's SQL step

- `SAS behaviour:` Not defined. PROC SQL does not fix the order among records equal on every ORDER BY
  key [whitepapers/ADaM/derive_lastminmax_measure.sas:133-147], and NODUPKEY then keeps the first
  [whitepapers/ADaM/derive_lastminmax_measure.sas:150-154] (D-15).
- `Port decision:` Input order is the stated tie rule (LMM-TRN-03, LMM-TRN-04).

#### U-02 — Reference level absent from a model's data

- `SAS behaviour:` Not established by the repository or the cited documentation: no reference output
  has a model without `TRTPN 0` records.
- `Port decision:` `model.glm_solution` emits no parameter rows and the status
  `not fitted: reference level 0 absent`; the report lists the stratum as not compared (S02-MOD-02,
  S07-MOD-02, S07-MOD-03).

#### U-03 — Derivation with no input records

- `SAS behaviour:` Not established. `avlen` is set only when the first DATA step reaches its last input
  record [whitepapers/ADaM/derive_lastminmax_measure.sas:98], so with no records the local `avlen`
  [whitepapers/ADaM/derive_lastminmax_measure.sas:79] is still blank when `%if &avlen < 20` is
  evaluated [whitepapers/ADaM/derive_lastminmax_measure.sas:101-102].
- `Port decision:` `derivation.derive_lastminmax_measure` returns an empty frame with the LMM-TRN-07
  columns and zero counts, and later steps behave as for an empty population; the report notes "SAS
  behaviour for empty derivation input not established (U-03)". This guard is a Python decision and a
  recorded legacy limitation, not a proven SAS equivalent.
