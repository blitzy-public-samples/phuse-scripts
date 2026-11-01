## Full-precision SAS reference exports (user-supplied)

This folder is the default location for optional full-precision SAS reference exports of WPCT-F.07.02 and WPCT-F.07.07. You supply them. The repository ships none, and the Python run never produces any. This README is the contract the Python port reads them against. It names the seven files, the point in each SAS program they come from, their format, and how they are compared.

### Purpose

* The repository already holds SAS references in [`whitepapers/WPCT/outputs_sas/`](../../../../whitepapers/WPCT/outputs_sas/): `WPCT-F.07.02.lst`, `WPCT-F.07.02.log`, `WPCT-F.07.07.lst` and `WPCT-F.07.07.log`.
  * The listings carry the GLM ParameterEstimates rounded for display: Estimate and SE to 8 decimals, t to 2, and `Pr > |t|` to 4 or as `<.0001`.
  * The logs carry exact dataset record counts.
  * The box-plot PDFs carry the summary statistics only as raster images, so they are not used.
* Without these exports, the [equivalence report](../../reports/equivalence_report.md) lists the following as NOT COMPARED:
  * the record-level sets and their categorical fields;
  * the per-stratum record counts;
  * every PROC SUMMARY statistic (`n mean std median datamin datamax q1 q3`);
  * the full-precision GLM values and the merged `pval`.
* Nothing in this project runs SAS. No SAS value is invented, approximated or filled in from Python output, so these quantities stay NOT COMPARED until you supply the matching exports.

### Expected files

Supply any subset of the seven files below, named exactly as shown. Line references are to [`WPCT-F.07.02.sas`](../../../../whitepapers/WPCT/WPCT-F.07.02.sas) and [`WPCT-F.07.07.sas`](../../../../whitepapers/WPCT/WPCT-F.07.07.sas) at upstream commit `398a6d33`. File names, columns and keys are quoted from [`wpct/schemas.py`](../../wpct/schemas.py), which is the binding source.

| File | SAS source | Content | Stratum columns to add | Record key |
|---|---|---|---|---|
| `WPCT-F.07.02_css_anadata.csv` | `WORK.CSS_ANADATA` after `WPCT-F.07.02.sas:162-168` | Analysis set (`SAFFL='Y'`, `ANL01FL='Y'`, `AVISITN ne 0`): `STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISIT AVISITN BASE CHG ANL01FL trtp_short` | none | `STUDYID USUBJID PARAMCD ATPTN AVISITN` |
| `WPCT-F.07.02_css_stats.csv` | `WORK.CSS_STATS` after the p-value merge at `WPCT-F.07.02.sas:302-305`, per stratum (DIABP/815, SYSBP/816) | Statistics by visit and treatment, with the merged p-value: `AVISITN TRTPN AVISIT trtp_short n mean std median datamin datamax q1 q3 pval` | `PARAMCD ATPTN` | `PARAMCD ATPTN AVISITN TRTPN` |
| `WPCT-F.07.02_parameter_estimates.csv` | ODS `ParameterEstimates` from `WPCT-F.07.02.sas:273-280`, per stratum | `Parameter Estimate StdErr tValue Probt` | `PARAMCD ATPTN` | `PARAMCD ATPTN EFFECT LEVEL` |
| `WPCT-F.07.07_advs_min.csv` | `WORK.ADVS_MIN` after the post-baseline filter at `WPCT-F.07.07.sas:168-171` | Minimum baseline and post-baseline derivation, post-baseline rows only: `AVISIT STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISITN trtp_short AVAL ADT BASE CHG ANL02FL` | none | `STUDYID USUBJID TRTPN PARAMCD ATPTN AVISITN` |
| `WPCT-F.07.07_css_anadata.csv` | `WORK.CSS_ANADATA` after the study numbering at `WPCT-F.07.07.sas:237-241` | Individual-study plus pooled rows: `STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT trtp_short BASE CHG ANL02FL studynum` | none | `STUDYID USUBJID PARAMCD ATPTN` |
| `WPCT-F.07.07_css_stats.csv` | `WORK.CSS_STATS` after the merge at `WPCT-F.07.07.sas:391-394`, per stratum (DIABP/815, DIABP/817) | Statistics by study and treatment, with the merged p-value: `STUDYID TRTPN studynum trtp_short n mean std median datamin datamax q1 q3 pval` | `PARAMCD ATPTN` | `PARAMCD ATPTN studynum TRTPN` |
| `WPCT-F.07.07_parameter_estimates.csv` | ODS `ParameterEstimates`, per study (BY `studynum`) and pooled, from `WPCT-F.07.07.sas:343-363` | `Parameter Estimate StdErr tValue Probt` | `PARAMCD ATPTN studynum` | `PARAMCD ATPTN STUDYNUM EFFECT LEVEL` |

Notes on the table:

* **Names, not positions.** Columns are matched by name, so their order in the file does not matter. Content columns are listed in SAS dataset order. Header names are matched case-insensitively: `studynum` and `STUDYNUM` are the same column, as are `BASE` and `base`. One file must not hold two columns with the same name ignoring case, whether spelled identically (`CHG,CHG`) or in different case (`CHG,chg`).
* **Missing columns fail.** Every listed content column and stratum column is read. A column missing from a file is reported as FAIL, not skipped.
* **Extra columns are ignored.** ODS also writes `Dependent` and `Biased`. That is why `WORK.CSS_PVALUES` has 7 variables at `WPCT-F.07.02.log:992`, and `WORK.CSS_PVAL_STDY` has 8, including the BY variable, at `WPCT-F.07.07.log:1128`. These columns, and any other extra ones, may stay in the file: the loader keeps them as text and no comparison reads them.
* **`EFFECT` and `LEVEL` are derived, not supplied.** [`sas_reference.split_parameter`](../../wpct/sas_reference.py) derives them from `Parameter`:
  * `TRTPN     54` gives `TRTPN` / `54`;
  * `Intercept` gives `INTERCEPT` / empty;
  * `BASE` or `base` gives `BASE` / empty;
  * `studynum` gives `STUDYNUM` / empty.

  SAS writes `BASE` in 07.02 but `base` in 07.07, and the derived key does not depend on that case. Any `EFFECT` or `LEVEL` column already in the file is replaced. A `Parameter` label of more than two words is a load error.
* **`studynum` in `WPCT-F.07.07_parameter_estimates.csv`.**
  * Per-study rows carry their BY `studynum` (1, 2), as ODS writes it.
  * On pooled rows, set `studynum` to `&stdyn`, the number of distinct `STUDYID` values including the pooled one. The shipped log reports `STDYN = 3` (`WPCT-F.07.07.log:564`), and the program assigns that value to the pooled p-values (`WPCT-F.07.07.sas:369`).
  * The pooled table also contains a `Parameter` row named `studynum`. It is the aliased continuous model term (DIV-01 in [`sas_specification.md`](../../docs/sas_specification.md)), not the stratum column of the same name.

### Row counts reported by the shipped SAS logs

Use these counts to check that a capture is complete. They come from the logs in [`whitepapers/WPCT/outputs_sas/`](../../../../whitepapers/WPCT/outputs_sas/). The variable counts exclude the stratum columns you add.

* 07.02 `CSS_ANADATA`: 3548 rows × 14 variables (`WPCT-F.07.02.log:202`).
* 07.02 `CSS_STATS`: 30 rows × 13 variables per stratum after the merge (`WPCT-F.07.02.log:1049,1471`), so 60 rows in the file.
* 07.02 ParameterEstimates (`CSS_PVALUES`): 5 rows per stratum (`WPCT-F.07.02.log:992,1414`), so 10 rows in the file.
* 07.07 `ADVS_MIN`: 498 rows × 16 variables (`WPCT-F.07.07.log:229`).
* 07.07 `CSS_ANADATA`: 996 rows × 13 variables (`WPCT-F.07.07.log:427`).
* 07.07 `CSS_STATS`: 9 rows × 13 variables per stratum (`WPCT-F.07.07.log:1215,1581`), so 18 rows in the file.
* 07.07 ParameterEstimates, per timepoint: 10 per-study rows (2 studies × 5; `CSS_PVAL_STDY`, `WPCT-F.07.07.log:1128,1493`) plus 6 pooled rows (`CSS_PVAL_POOL`, `WPCT-F.07.07.log:1144,1509`), so 32 rows in the file.

### Format

* **Layout.** Comma-separated CSV with exactly one header row of SAS variable names, then one row per record. Use standard CSV double-quote quoting for any value that contains a comma, a double quote or a line break.
* **Numbers.** Write every numeric variable with `BEST32.`, including `ADT`, so dates are SAS day numbers rather than `DATE9.` text.
  * Write missing numerics as `.` or as an empty field. The special missing values `._` and `.A` to `.Z` also read as missing.
  * Any other text in a numeric column, a `DATE9.` date for example, stops the load with an input error.
  * Write missing character values as an empty field.
* **Encoding.** Use Latin-1 (ISO-8859-1), so the pooled `STUDYID` keeps its single leading byte `'A0'x`. SAS builds it as `'A0'x !! 'Pooled'`, and Python reads it as `schemas.POOLED_STUDYID`, which is `"\u00a0Pooled"`. A UTF-8 file stores that character as two bytes, so the pooled keys would not match. The pooled `USUBJID` keeps its leading `P` (`WPCT-F.07.07.sas:222-230`).
* **Values as stored.** Do not reformat, round, relabel or apply display formats to any value. Blanks around a number and trailing blanks of character values are ignored.

### Capturing the exports

The shipped programs overwrite `CSS_STATS` and the ODS ParameterEstimates output on every parameter and timepoint iteration. At cleanup they delete those WORK datasets (`WPCT-F.07.02.sas:396`, `WPCT-F.07.07.sas:481`). A finished run therefore leaves none of them to export.

* **Where to capture.** Capture each dataset in a SAS session you control, at the program point named in the table:
  * `css_anadata` and `advs_min` once;
  * `css_stats` and ParameterEstimates once per stratum, adding the stratum columns.

  Then concatenate the strata into one file per name.
* **Inputs.** For a like-for-like comparison, use the ADVS inputs the Python port reads: `data/adam/cdisc/advs.xpt` for 07.02 and `data/adam/cdisc-split/advs.xpt` for 07.07. The shipped programs point elsewhere: 07.02 reads a GitHub URL through a blank `local=` (ENV-02), and 07.07 uses UNC paths (ENV-04). Both are recorded in the legacy-environment entries of [`sas_specification.md`](../../docs/sas_specification.md).
* **Repository files.** Work on copies outside this repository. The repository's SAS programs, macros and saved outputs stay byte-for-byte unchanged. No SAS helper program is provided, and none should be committed.
* **SAS version.** Keep the log of the capture session with the exports, so the SAS and SAS/STAT releases that produced them are on record. The shipped references come from SAS 9.4 TS1M2 with SAS/STAT 13.2 (header of `WPCT-F.07.02.log`).

### How the exports are used

* **Where the files are read.**
  * Place the files in this folder, or pass `--sas-exports DIR` to `python -m wpct reproduce` to read another existing directory. `--sas-exports` is the command's only option.
  * `python -m pytest` reads only this folder.
  * Run both commands from `python/wpct/`. See [`python/wpct/README.md`](../../README.md) for setup.
* **Read-only.** The folder is read, never written: the port writes only below `python/wpct/outputs/` and to `python/wpct/reports/equivalence_report.md`.
* **Load errors.** `python -m wpct reproduce` stops with an input error (exit code 2) and writes nothing, and the tests that use the exports error, when:
  * a present file cannot be parsed as CSV (an unterminated quote or a NUL byte, for example) or has no header row;
  * a header column has no name;
  * a file has two columns with the same name ignoring case, whether identical or case variants;
  * a record has more or fewer fields than the header;
  * a numeric column holds a value that is not a SAS number;
  * a ParameterEstimates file has no usable `Parameter` column.

  The one-line message names the file and the header field, column or data row at fault, never a value from the file.
* **Comparisons.** Each present file is compared on its record key with the in-memory Python result:
  * Key sets must be equal. Each key present on one side only is listed individually in the report and counted as FAIL, and so is each repeated key.
  * Record counts must match exactly, and so must the per-stratum record counts of `css_anadata` and `advs_min`.
  * Categorical fields must match exactly.
  * Each numeric quantity uses its class in [`tolerances.json`](../../tolerances.json):
    * `n` under `counts`;
    * `mean std median datamin datamax q1 q3` under `summary_statistics`;
    * `Estimate StdErr tValue` under `glm_estimates`;
    * `Probt` and the merged `pval` under `p_values`;
    * `AVAL BASE CHG ADT` under `record_numeric`.
  * Under `missing`, a value missing on both sides passes and a value missing on one side fails.
  * `tolerances.json` is the single tolerance source.
* **Absent files.** An absent file is never passed silently. The report lists its quantities as NOT COMPARED with the reason, and [`tests/test_sas_exports.py`](../../tests/test_sas_exports.py) skips its case with this message:

  `SAS reference export <file> not found in <dir>; see python/wpct/reference/sas_exports/README.md`

  Skip reasons are printed because `pyproject.toml` sets `-ra`.
* **Present files.** A present file passes only when its comparisons number more than zero and every one is PASS. A FAIL fails the case, and so does a NOT COMPARED row.
  * For `css_anadata` and `advs_min`, both sides must also hold the record count the SAS log reports.
  * The report gains one comparison group per present file, `Export <file>`, and NOT COMPARED is never counted as PASS.
  * When a halting check stops a program, that program's present exports are listed as NOT COMPARED with the halt message.
* **Scope.** Full-precision agreement is established only for the quantities a supplied file carries.

### Data terms

* The exports are derivatives of the CDISC-pilot-derived ADVS datasets, and `css_anadata` and `advs_min` are subject-level. They are governed by the terms described in [`data/adam/README.md`](../../../../data/adam/README.md), [`data/adam/cdisc/README.md`](../../../../data/adam/cdisc/README.md) and [`data/adam/cdisc-split/README.md`](../../../../data/adam/cdisc-split/README.md).
* `python/wpct/.gitignore` does not ignore CSV files in this folder, so do not commit exports unless those terms permit it. For record-level exports, the report names only the keys of mismatched records, never their values.
* Repository code is under the MIT licence in [`LICENSE.md`](../../../../LICENSE.md).
