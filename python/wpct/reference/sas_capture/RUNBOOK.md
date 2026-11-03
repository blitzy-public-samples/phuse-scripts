## WPCT-F.07.02 SAS capture runbook

**UNVERIFIED IN SAS until run in a licensed session.**

* The launcher [`WPCT-F.07.02_capture.sas`](WPCT-F.07.02_capture.sas) was written without a SAS engine. Nobody has run it, and it is not claimed to work. Expect a debug round in the first licensed session: [Likely failure points](#9-likely-failure-points) says where to look.
* Nothing in this folder claims SAS equivalence. The launcher only captures the full-precision SAS reference exports that the Python port compares against. Only that comparison, run after the capture, shows whether the port agrees with SAS.
* Scope: WPCT-F.07.02 only. There is no WPCT-F.07.07 capture, and nothing here starts SAS automatically: a person runs the launcher once in a licensed SAS session.
* This folder holds three files: the launcher, this runbook and [`test_sas_capture_contract.py`](test_sas_capture_contract.py), a contract check that needs no SAS ([section 6.1](#61-contract-check-of-the-launcher)).

Line numbers without a file name refer to [`whitepapers/WPCT/WPCT-F.07.02.sas`](../../../../whitepapers/WPCT/WPCT-F.07.02.sas). `<repo>`, `<work>` and `<csv>` stand for the launcher settings `wpct_repo`, `wpct_workdir` and `wpct_csvdir`.

### 1. Capture design

#### 1.1 The WPCT-F.07.02 export files

The export README ([`../sas_exports/README.md`](../sas_exports/README.md), Expected files) assigns three files to WPCT-F.07.02. They are the WPCT-F.07.02 entries of `schemas.SAS_EXPORT_FILES` in [`wpct/schemas.py`](../../wpct/schemas.py), which is binding:

| File | Captured from | Columns the launcher writes | Rows |
|---|---|---|---|
| `WPCT-F.07.02_css_anadata.csv` | `WORK.CSS_ANADATA`, after the program returns | `STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISIT AVISITN BASE CHG ANL01FL trtp_short` | 3548 |
| `WPCT-F.07.02_css_stats.csv` | `css_stats` after the merge at lines 302-305, once per stratum | `PARAMCD ATPTN AVISITN TRTPN AVISIT trtp_short n mean std median datamin datamax q1 q3 pval` | 60 (30 per stratum) |
| `WPCT-F.07.02_parameter_estimates.csv` | The ODS ParameterEstimates table `&css_pval_ds` after line 280, once per stratum | `PARAMCD ATPTN Parameter Estimate StdErr tValue Probt` | 10 (5 per stratum) |

* The row counts are those of the shipped SAS log (export README, Row counts reported by the shipped SAS logs). The launcher checks each file against them.
* The strata are DIABP/815 and SYSBP/816, told apart by `PARAMCD` and `ATPTN`. Treatment is carried by `TRTPN` and `trtp_short`, and in the estimates by the `Parameter` label (`TRTPN     54`, for example). The study is carried by `STUDYID` in `css_anadata`; WPCT-F.07.02 reads one study, `CDISCPILOT01`.
* The `.sas7bdat` copies keep every variable, including the ODS columns `Dependent` and `Biased`. The CSV leaves those two out, because no comparison reads them (export README, Notes on the table).

#### 1.2 Approach: a staged working copy, generated at run time

The ParameterEstimates table and `css_stats` exist only inside the parameter and timepoint loops of the program, and the unmodified program offers no way to capture them from outside ([section 1.3](#13-exports-the-unmodified-program-makes-impossible-to-capture)). The launcher therefore takes these steps:

1. It reads `whitepapers/WPCT/WPCT-F.07.02.sas` read-only and removes the trailing CR of each line, since the file has CRLF line endings.
2. It writes `<work>/WPCT-F.07.02_staged.sas` in the capture work directory, outside the repository. The staged copy is the original line for line except for four anchored edits:
   * (a) Line 85 becomes `%util_access_test_data(advs, local=<repo>/data/adam/cdisc/) ;`, so ADVS is read from the repository file without a network connection.
   * (b) Line 126 becomes `%let outputs_folder = <work>/pdf;`, so no PDF lands in the repository.
   * (c) After line 280 (`run; quit;` of PROC GLM), the full ODS ParameterEstimates table `&css_pval_ds` is appended to the capture library, before line 289 overwrites it. `PARAMCD` comes from `&&paramcd_val&pdx` and `ATPTN` from `&&atptn_val&tdx`.
   * (d) After line 308 (the `%end;` that closes the GLM block, after the merge at lines 302-305), the merged `css_stats` is appended to the capture library, with the same `PARAMCD` and `ATPTN`.
3. It checks each anchor by line number and expected text. On any mismatch it stops before anything runs.
4. It runs the staged copy. The repository macros run unmodified from `whitepapers/utilities` through SASAUTOS.
5. After the staged copy returns, it copies `WORK.CSS_ANADATA`, which nothing later changes or deletes, to the capture library.
6. It writes the three CSVs from the capture library.

The repository originals are only read, never written. The comments after the statements on lines 85 and 126 are not kept in the staged copy; the launcher header gives the reason.

#### 1.3 Exports the unmodified program makes impossible to capture

* **`WPCT-F.07.02_parameter_estimates.csv`.** The `ods output` at line 274 creates the full table `css_pvalues` (`&css_pval_ds`). Line 289 overwrites it under the same name, keeping only the `TRTPN` rows, with `Probt` renamed to `pval`. No macro call falls between lines 280 and 289, so nothing can hook in there. A persistent ODS OUTPUT request made before the program starts is modified by the program's own ODS OUTPUT statement at line 274.
* **`WPCT-F.07.02_css_stats.csv`.** `css_stats` is overwritten on every parameter and timepoint iteration and deleted at cleanup (line 396). The only hook is the repository macro `util_delete_dsets` at line 307, and replacing that macro would mean the repository macros no longer run unmodified.
* **`WPCT-F.07.02_css_anadata.csv`.** `WORK.CSS_ANADATA` survives an unmodified run and can be captured afterwards. An unmodified run, however, reads the GitHub URL through the blank `local=` at line 85 (ENV-02 in [`sas_specification.md`](../../docs/sas_specification.md)) instead of the like-for-like `data/adam/cdisc/advs.xpt`.

With the staged copy, no WPCT-F.07.02 export is impossible to capture.

#### 1.4 CSV format, and how it relates to the export README

The export README predates this launcher. Line 64 of the README asks for `BEST32.`. Line 81 says "No SAS helper program is provided, and none should be committed", but this folder now provides one.

* **Numbers.** The launcher removes every display format and writes each CSV numeric with the `PUTN` function and the explicit format `E32.17` (setting `wpct_numfmt`), without leading blanks; the pre-flight check tests the format with the same function. Estimates are therefore not rounded, and small p-values are not written as text such as `<.0001`.
* **Why the loader accepts it.** `sas_reference._SAS_NUMBER` in [`wpct/sas_reference.py`](../../wpct/sas_reference.py) accepts a signed mantissa with an optional `E` exponent, and README line 69 says that blanks around a number are ignored.
* **Precision.** SAS documents formatted output beyond about 15 significant digits as possibly inaccurate. An exact CSV round trip is therefore unverified until the first licensed run, and the launcher reports it column by column ([failure point 4](#9-likely-failure-points)). The `.sas7bdat` copies are the exact, authoritative capture, and `BEST32.` is the documented fallback.
* **Other values.**
  * Missing numerics are written as `.`, and missing character values as an empty field.
  * Character values are written as stored, without trailing blanks; leading and interior blanks are kept. A value that holds a comma, a double quote or a line break is enclosed in double quotes, with each double quote in it doubled (README line 63). Each record is written whole by one formatted `PUT`, so no DSD rule changes a value.
  * The encoding is Latin-1 (setting `wpct_encoding`).
  * No WPCT-F.07.02 export column is a date, so no date text is written.

### 2. Prerequisites

* **SAS.** A licensed SAS 9.4 session with Base SAS and SAS/STAT, operated by a person.
  * Match the historical references where possible: SAS 9.4 TS1M2 with SAS/STAT 13.2. That is the release in the header of [`whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log`](../../../../whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log), which was produced on Windows (`X64_7PRO`).
  * A newer release yields results for that release, not for SAS 9.4 TS1M2. Record which release ran ([section 3](#3-values-to-record)).
  * The program halts on a SAS release older than 9.4M2 (`SASV=9.4M2` at line 148). On an OS other than AIX, Windows or HP IPF it writes a WARNING and carries on (line 147).
* **Repository.** A git checkout of this repository at a recorded commit, readable by the SAS session, with `whitepapers/`, `data/` and `python/` in place. SAS reads the program, the macros and `data/adam/cdisc/advs.xpt` from it. When SAS runs on a server, as SAS Studio and SAS Enterprise Guide commonly do, the checkout and every path must be on that server.
* **Work directory.** A writable capture work directory outside the checkout, whose parent exists. The launcher creates only one directory level.
* **Nothing else on the SAS side.** No network connection and no credentials are needed.
* **Python side.** For the comparison, the Python 3.10 environment of [`python/wpct/README.md`](../../README.md) (Reproduce, steps 1 and 2). It needs no SAS.

### 3. Values to record

Record these values with the exports. No manifest or hashing script is provided: run the commands by hand. The shell commands are for a POSIX shell such as `sh` or `bash`; on Windows, run them in Git Bash. Besides `git`, `python` and `sas`, they use standard utilities and `sha256sum`, which comes from GNU coreutils and is not a POSIX utility. Where `sha256sum` is missing, `shasum -a 256` with the same arguments prints the same lines; macOS includes `shasum`.

First set the three directories in the shell. Every later shell command in this runbook uses them, with the same absolute paths as the launcher settings:

```sh
WPCT_REPO=/absolute/path/to/phuse-scripts
WPCT_WORKDIR=/absolute/path/to/wpct-capture
WPCT_CSVDIR="$WPCT_WORKDIR/csv"
```

**The commit and the input hashes**, at the repository root:

```sh
cd "$WPCT_REPO"
git rev-parse HEAD
git status --porcelain
set -- data/adam/cdisc/advs.xpt \
  whitepapers/WPCT/WPCT-F.07.02.sas \
  python/wpct/reference/sas_capture/WPCT-F.07.02_capture.sas \
  whitepapers/utilities/util_access_test_data.sas \
  whitepapers/utilities/util_delete_dsets.sas \
  whitepapers/utilities/assert_depend.sas \
  whitepapers/utilities/assert_continue.sas \
  whitepapers/utilities/assert_dset_exist.sas \
  whitepapers/utilities/assert_var_exist.sas \
  whitepapers/utilities/assert_macro_exist.sas \
  whitepapers/utilities/util_resolve_sasautos.sas \
  whitepapers/utilities/util_labels_from_var.sas \
  whitepapers/utilities/util_count_unique_values.sas \
  whitepapers/utilities/util_get_var_min_max.sas \
  whitepapers/utilities/util_value_format.sas \
  whitepapers/utilities/util_boxplot_block_ranges.sas \
  whitepapers/utilities/util_proc_template.sas \
  whitepapers/utilities/util_axis_order.sas
git hash-object --no-filters -- "$@"
sha256sum "$@"
```

* `git rev-parse HEAD` prints the full commit. `git status --porcelain` must print nothing, but it does not show line-ending conversion. With `core.autocrlf=true`, the Git for Windows default, git writes the LF files, such as the macros and the launcher, with CRLF line endings on checkout, and `git status --porcelain` still prints nothing. The files on disk are the bytes of that commit only in a checkout made with `core.autocrlf=false`, for example `git clone -c core.autocrlf=false <url>`. The `--no-filters` hashes below confirm it: each must equal `git rev-parse HEAD:<path>`.
* The 15 macro files are the repository macros WPCT-F.07.02 calls, directly or through another macro. Before anything runs, the launcher checks that each of them can be read under `<repo>/whitepapers/utilities`; for each one it cannot read, it writes `Not found or not readable: <path>`, and the program does not run.
* `git hash-object --no-filters` prints the git blob of each file's unchanged bytes on disk, one line per file in the order given. `--no-filters` stops `core.autocrlf`, which Git for Windows turns on by default, or another clean filter from converting the bytes before hashing; `wpct/provenance.py` hashes the same way. For the repository inputs, these are the working-tree blobs that `tests/test_sources_unchanged.py` compares with the upstream commit. `sha256sum` prints the SHA-256 of the bytes on disk.

**The SAS session**, submitted in the SAS session that runs the launcher, before or after it. The launcher writes the same values to its capture log ([section 4](#4-run-steps), step 5), so in a batch run that log is the record.

```sas
%put SYSVLONG=&sysvlong SYSSCP=&sysscp SYSSCPL=&sysscpl SYSENCODING=&sysencoding;
proc product_status;
run;
proc options;
run;
```

* `SYSVLONG` gives the SAS release, and PROC PRODUCT_STATUS gives the SAS/STAT release.
* `SYSSCP` and `SYSSCPL` give the OS.
* `SYSENCODING` gives the session encoding.
* PROC OPTIONS lists every option in effect.

**The run itself:**

* the five settings of the edited launcher copy. The capture log carries them as `NOTE: (WPCT CAPTURE) WPCT_REPO=...` and so on, and `WPCT_ENCODING` is the CSV encoding;
* the SHA-256 of the edited launcher copy and of the staged copy, after the run:

  ```sh
  sha256sum "$WPCT_WORKDIR/WPCT-F.07.02_capture.sas" "$WPCT_WORKDIR/WPCT-F.07.02_staged.sas"
  ```

* the session log and `<work>/WPCT-F.07.02_capture.log`, kept with the exports (export README line 82). Together they record the SAS and SAS/STAT releases, the options and every check of the run.

### 4. Run steps

1. **Record the git values** of [section 3](#3-values-to-record).
2. **Create the work directory and copy the launcher into it.** The checkout stays unmodified:

   ```sh
   mkdir -p "$WPCT_WORKDIR"
   cp "$WPCT_REPO/python/wpct/reference/sas_capture/WPCT-F.07.02_capture.sas" "$WPCT_WORKDIR/"
   ```

3. **Edit the USER SETTINGS block** at the top of the copy, `$WPCT_WORKDIR/WPCT-F.07.02_capture.sas`, and nothing else:

   ```sas
   %let wpct_repo     = /absolute/path/to/phuse-scripts;
   %let wpct_workdir  = /absolute/path/to/wpct-capture;
   %let wpct_csvdir   = &wpct_workdir/csv;
   %let wpct_encoding = latin1;
   %let wpct_numfmt   = E32.17;
   ```

   * Use the same absolute paths as in the shell. The settings take `/` or `\` as the separator, as in `C:/work/phuse-scripts` on Windows.
   * A path must not contain `& % ; , " ' ( ) |`, or a `.` or `..` segment.
   * `wpct_workdir` must lie outside the checkout.
   * `wpct_csvdir` must not be the checkout root, and must not lie under `whitepapers/` or `data/`. Keep the default, inside the work directory.
   * On Windows, set `wpct_encoding` to `wlatin1` if the session refuses `latin1` ([failure point 10](#9-likely-failure-points)).

   Then confirm that only the settings lines differ:

   ```sh
   diff "$WPCT_REPO/python/wpct/reference/sas_capture/WPCT-F.07.02_capture.sas" "$WPCT_WORKDIR/WPCT-F.07.02_capture.sas"
   ```

4. **Run the copy once, in a fresh licensed SAS 9.4 session.** A session already in syntax-check mode is refused.
   * **Batch.** Run from the work directory, so no log lands in the checkout. `sas` is the SAS command of the installation; on Windows it is `sas.exe`, under `SASHome\SASFoundation\9.4`.

     ```sh
     cd "$WPCT_WORKDIR"
     sas -sysin WPCT-F.07.02_capture.sas -log WPCT-F.07.02_session.log -print WPCT-F.07.02_session.lst
     ```

   * **Interactive** (SAS windowing environment, SAS Enterprise Guide or SAS Studio). Open the copy in a new session and submit the whole program once. Then save the Log window as `WPCT-F.07.02_session.log` in the work directory.
5. **Check the logs:**

   ```sh
   grep -n '(WPCT CAPTURE)' "$WPCT_WORKDIR/WPCT-F.07.02_session.log" "$WPCT_WORKDIR/WPCT-F.07.02_capture.log"
   grep -n -E '^(ERROR|WARNING)' "$WPCT_WORKDIR/WPCT-F.07.02_capture.log"
   ```

   A complete capture shows the following lines, in this order.
   * **Session log, before the redirect:**
     * no `ERROR: (WPCT CAPTURE)` line from the settings and guard checks;
     * the `WPCT_NUMFMT pre-flight` lines;
     * `From here on the log goes to <work>/WPCT-F.07.02_capture.log.`
   * **Capture log:**
     * `UNVERIFIED IN SAS until run in a licensed session.`;
     * the settings and the `SYSVLONG=... SYSSCP=... SYSSCPL=... SYSENCODING=...` line, then the PROC PRODUCT_STATUS and PROC OPTIONS output;
     * `SASAUTOS=... with WPCTAUTO=...`;
     * `Wrote the staged copy from <n> lines of WPCT-F.07.02.sas, with 4 anchors matched.` The original has 402 lines.
   * **The staged program's own log:**
     * the expected ERROR lines from lines 76 and 82 of the original ([failure point 8](#9-likely-failure-points));
     * for each stratum, the `MPRINT(BOXPLOT_EACH_PARAM_TP)` echo of `DATA _WPCT_PE` and `DATA _WPCT_STATS`;
     * no `The program halted`, `syntax-check mode` or `No CSV is written` line ([failure point 11](#9-likely-failure-points)).
   * **The exports, for each file:**
     * `<data> has <n> rows, the count the shipped SAS log reports for <file>.`, with 3548, 60 and 10;
     * `Wrote <csv>/<file> from <data>.`;
     * for each column, `every CSV value reads back as the native value.`, or a WARNING ([failure point 4](#9-likely-failure-points) for a numeric column, [failure point 3](#9-likely-failure-points) for a character one).
   * **The session log's last line**, after the redirect ends: `Capture finished: native copies in ..., CSVs in ..., log in .... Review every WARNING there.` A failed run ends with `Capture failed.` instead.

   To compare the program's own record counts with the shipped SAS log, list the `CSS_` dataset notes of both logs:

   ```sh
   grep -n 'NOTE: The data set WORK.CSS_' "$WPCT_WORKDIR/WPCT-F.07.02_capture.log"
   grep -n 'NOTE: The data set WORK.CSS_' "$WPCT_REPO/whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log"
   ```

6. **Locate the outputs** ([section 5](#5-files-produced)). Check the CSV line counts, which are the header plus one line per row:

   ```sh
   wc -l "$WPCT_CSVDIR"/WPCT-F.07.02_*.csv
   ```

7. **Compare** ([section 6](#6-compare-commands)).

A rerun with the same settings first removes the captures and CSVs of the earlier run.

### 5. Files produced

Every file lands below the settings-block directories, never in the repository.

| Path | Content |
|---|---|
| `<work>/WPCT-F.07.02_staged.sas` | The staged working copy ([section 1.2](#12-approach-a-staged-working-copy-generated-at-run-time)). |
| `<work>/WPCT-F.07.02_session.log`, `<work>/WPCT-F.07.02_session.lst` | The session log and listing of a batch run (step 4). In an interactive run, the saved Log window. |
| `<work>/WPCT-F.07.02_capture.log`, `<work>/WPCT-F.07.02_capture.lst` | The log and listing from the redirect on: the environment record, the staged program and every self-check. |
| `<work>/capture/css_anadata.sas7bdat` | The native copy of `WORK.CSS_ANADATA`. Authoritative. |
| `<work>/capture/css_stats.sas7bdat` | The native merged `css_stats` of both strata, with `PARAMCD ATPTN`. Authoritative. |
| `<work>/capture/parameter_estimates.sas7bdat` | The native ParameterEstimates of both strata, with `PARAMCD ATPTN` and every ODS column. Authoritative. |
| `<work>/pdf/WPCT-F.07.02_Box_plot_<PARAMCD>_Change_by_visit_for_timepoint_<ATPTN>.pdf` | The box plots the program itself draws, one per stratum, on Windows. On UNIX they are files named `pdf\WPCT-F.07.02_Box_plot_...pdf` in `<work>` ([failure point 12](#9-likely-failure-points)). No comparison uses them. |
| `<csv>/WPCT-F.07.02_css_anadata.csv` | The contract CSV: 3548 rows. |
| `<csv>/WPCT-F.07.02_css_stats.csv` | The contract CSV: 60 rows. |
| `<csv>/WPCT-F.07.02_parameter_estimates.csv` | The contract CSV: 10 rows. |

**Commit none of these files.**
* `css_anadata` is subject-level: one record per `USUBJID`, `PARAMCD`, `ATPTN` and `AVISITN`.
* The exports derive from the CDISC-pilot-derived ADVS data, so the terms in [`data/adam/README.md`](../../../../data/adam/README.md), [`data/adam/cdisc/README.md`](../../../../data/adam/cdisc/README.md) and [`data/adam/cdisc-split/README.md`](../../../../data/adam/cdisc-split/README.md) govern them (export README, Data terms).

Keep the whole capture set together, outside the repository: the recorded values, both logs, the staged copy, the `.sas7bdat` files and the CSVs.

### 6. Compare commands

Run every command in this section from `python/wpct/`, in the activated Python 3.10 environment of [`python/wpct/README.md`](../../README.md), with the shell variables of [section 3](#3-values-to-record):

```sh
cd "$WPCT_REPO/python/wpct"
```

#### 6.1 Contract check of the launcher

This check needs no SAS and no exports:

```sh
python -m pytest reference/sas_capture/test_sas_capture_contract.py
```

* It reports `1 passed`.
* It checks that every WPCT-F.07.02 export name and column in `wpct/schemas.py` appears in the launcher's export statements, and that the launcher exports no other file.
* It shows nothing about how the launcher behaves in SAS.
* The default `python -m pytest` does not collect it, because `pyproject.toml` sets `testpaths = ["tests"]`.

#### 6.2 The equivalence report

```sh
python -m wpct reproduce --sas-exports "$WPCT_CSVDIR"
```

* **Output and exit code.** It prints PASS, FAIL and NOT COMPARED counts per program and overall. It exits with:
  * `0` when no comparison failed and no program halted;
  * `1` when a comparison failed or a program halted;
  * `2` on an input error or a rejected destination. An export that does not load is an input error (export README, Load errors), and then nothing is written.
* **What it writes.** It rewrites `outputs/` and regenerates the committed `reports/equivalence_report.md`. In the report, read:
  * the Summary;
  * the three `Export WPCT-F.07.02_...` groups of the WPCT-F.07.02 section;
  * the Not compared section ([section 8](#8-the-14-not-compared-quantities-of-wpct-f0702)).
* **Afterwards.** The regenerated report names the keys of mismatched records, and those keys include `USUBJID`. Whether to commit a report regenerated from SAS exports is a separate decision. Until it is made, restore the committed copy:

  ```sh
  git checkout -- reports/equivalence_report.md
  ```

#### 6.3 The export test cases

```sh
cp "$WPCT_CSVDIR"/WPCT-F.07.02_*.csv reference/sas_exports/
python -m pytest tests/test_sas_exports.py
python -m pytest
rm reference/sas_exports/WPCT-F.07.02_*.csv
git status --porcelain
```

* **Why the copy.** `tests/test_sas_exports.py` reads exports only from `reference/sas_exports/` ([section 7](#7-how-teststest_sas_exportspy-finds-the-exports)). The three CSVs are copied there for the run and removed afterwards.
* **Commit none of them.** `.gitignore` does not ignore CSV files in that folder, and `css_anadata` is subject-level.
* **What runs.** `test_d01_02_05_to_09_sas_export_equivalence` has seven cases.
  * The three WPCT-F.07.02 cases run. Each passes only when every comparison of its file is PASS.
  * The four WPCT-F.07.07 cases skip with `SAS reference export <file> not found in <dir>; see python/wpct/reference/sas_exports/README.md`.
* **The full suite.** `python -m pytest` then runs the same three cases among all the others. One more case reads the copied CSVs: `tests/test_report.py::test_env03_reproduce_under_write_root[default]`.
  * It runs `reproduce` without `--sas-exports`, under a temporary write root, and requires exit code `0`.
  * Any export comparison that is FAIL makes `reproduce` exit `1`, so this case fails too. One disagreeing file therefore shows as two failures: its own export case and this one.
* **Cleanup.** `git status --porcelain` prints nothing once the CSVs are removed and the report is restored ([section 6.2](#62-the-equivalence-report)).

#### 6.4 The clean-checkout baseline

With the CSVs removed:

```sh
python -m pytest
```

At the commit that added this runbook, it reports `795 passed, 7 skipped`. The seven skips are the export cases.

### 7. How tests/test_sas_exports.py finds the exports

The lookup follows the export README, How the exports are used:

* `test_d01_02_05_to_09_sas_export_equivalence` takes the fixtures `sas_exports_dir` and `sas_exports` from [`tests/conftest.py`](../../tests/conftest.py).
* `sas_exports_dir` returns `repo_paths.sas_exports_dir`, and `repo_paths` is `RepoPaths.checkout()` from [`wpct/paths.py`](../../wpct/paths.py).
* `RepoPaths.sas_exports_dir` is `repo_root / "python/wpct/reference/sas_exports"`. `repo_root` is `Path(__file__).resolve().parents[3]` of `paths.py`: the checkout that holds the imported `wpct` package.
  * Run from `python/wpct/`, pytest imports that package through `pythonpath = ["."]` in `pyproject.toml`.
  * The directory is therefore `python/wpct/reference/sas_exports/` of the same checkout.
* `sas_exports` loads that directory once per session with `sas_reference.load_sas_exports`. That function opens `<dir>/<file>` for each name of `schemas.SAS_EXPORT_FILES`, exactly as named, and searches no other folder or subfolder.
  * An absent file loads as `None`, and its case skips with the message of [section 6.3](#63-the-export-test-cases).
  * A present file that does not load is an error.
* `tests/test_report.py::test_env03_reproduce_under_write_root[default]` reads the same directory ([section 6.3](#63-the-export-test-cases)).
  * It calls `reproduce` of [`wpct/__main__.py`](../../wpct/__main__.py) with no export directory, so `reproduce` reads `RepoPaths.sas_exports_dir`, the default of `python -m wpct reproduce`.
  * Its `RepoPaths` comes from `RepoPaths.with_write_root`, which moves only the write root under pytest's `tmp_path` and keeps `repo_root`. The case asserts that this directory equals the `sas_exports_dir` fixture.
* Nothing points the tests at another directory: no pytest option, environment variable or setting. `--sas-exports DIR` belongs to `python -m wpct reproduce` alone, as its only option.

### 8. The 14 NOT COMPARED quantities of WPCT-F.07.02

The committed [equivalence report](../../reports/equivalence_report.md) lists 14 WPCT-F.07.02 quantities as NOT COMPARED. Each one is resolved by one capture file:

| # | Quantity, as the report's Not compared section names it | Resolved by | Report group once the file is present and loads |
|---|---|---|---|
| 1 | css_anadata: record-level set and categorical fields (STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD AVISIT AVISITN ATPT ATPTN) | `WPCT-F.07.02_css_anadata.csv` | `Export WPCT-F.07.02_css_anadata.csv` |
| 2 | css_anadata: record-level numeric values (BASE CHG) | `WPCT-F.07.02_css_anadata.csv` | `Export WPCT-F.07.02_css_anadata.csv` |
| 3 | css_anadata: per-stratum record counts (PARAMCD × ATPTN × AVISITN × TRTPN) | `WPCT-F.07.02_css_anadata.csv` | `Export WPCT-F.07.02_css_anadata.csv` |
| 4 | css_stats: PROC SUMMARY statistic n | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 5 | css_stats: PROC SUMMARY statistic mean | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 6 | css_stats: PROC SUMMARY statistic std | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 7 | css_stats: PROC SUMMARY statistic median | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 8 | css_stats: PROC SUMMARY statistic datamin | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 9 | css_stats: PROC SUMMARY statistic datamax | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 10 | css_stats: PROC SUMMARY statistic q1 | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 11 | css_stats: PROC SUMMARY statistic q3 | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 12 | css_stats: merged pval at full precision | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 13 | css_stats: statistics rows and categorical fields (AVISIT trtp_short) | `WPCT-F.07.02_css_stats.csv` | `Export WPCT-F.07.02_css_stats.csv` |
| 14 | parameter_estimates: full-precision GLM values (ESTIMATE STDERR TVALUE PROBT) | `WPCT-F.07.02_parameter_estimates.csv` | `Export WPCT-F.07.02_parameter_estimates.csv` |

* **When a file is present and loads.** `python -m wpct reproduce` removes the file's rows from Not compared. It compares their quantities in the report group `Export <file>`, where every row is PASS or FAIL. One exception: when no record key of `WPCT-F.07.02_css_anadata.csv` matches a Python record, every field its group compares is NOT COMPARED, with the reason `no matched records`, and its test case then fails.
* **When all three files are supplied and load:**
  * the 14 missing-export rows of the table above leave the Not compared section;
  * the WPCT-F.07.02 NOT COMPARED count reaches 0 only when every quantity can be compared. The `no matched records` rows of that exception remain NOT COMPARED, and the Not compared section lists them under their `Export` group;
  * the 17 NOT COMPARED rows of WPCT-F.07.07 remain, because no WPCT-F.07.07 file is captured;
  * the WPCT-F.07.02 log-checkpoint and listing groups stay as they are.
* **Outcome.** This runbook predicts no outcome. Which rows become PASS and which become FAIL is for the comparison to show.

### 9. Likely failure points

The numbers match the launcher header, and the launcher cites them in its comments and log messages. For each point, the entry gives the symptom, then the debug action. SAS statements are submitted in the same session after the run. Shell commands run at the repository root, unless they use `$WPCT_WORKDIR` or `$WPCT_CSVDIR`.

1. **ODS table name and columns.** The capture reads the PROC GLM ODS table `ParameterEstimates`. The CSV requires its variables `Parameter Estimate StdErr tValue Probt`. If this SAS/STAT release names one of them differently, the `parameter_estimates` export stops with an ERROR naming the missing variable. `Dependent` and `Biased` are kept in the native copy as ODS extras, left out of the CSV and not checked.
   * *Symptom:* `No ParameterEstimates table was captured.`, or `wpctcap.parameter_estimates has no variable <var>, which WPCT-F.07.02_parameter_estimates.csv requires.`, or an ODS WARNING that the output was not created.
   * *Debug:* `proc contents data=wpctcap.parameter_estimates; run;` lists what was captured. In an interactive session, submit `ods trace on;` before the launcher, and the log then names every table PROC GLM creates.
2. **Macro variable scoping.** The inserted DATA steps run inside `%boxplot_each_param_tp`. They resolve its local `pdx`, `tdx` and `css_pval_ds`, and the global `paramcd_val<n>` and `atptn_val<n>` that `util_labels_from_var` creates.
   * *Symptom:* `Apparent symbolic reference ... not resolved` next to `DATA _WPCT_PE` or `DATA _WPCT_STATS`, or a blank `PARAMCD` or missing `ATPTN` in the captures.
   * *Debug:* in the MPRINT lines of those two steps, `PARAMCD` must read DIABP or SYSBP, and `ATPTN` 815 or 816. Then list the strata, which should show 30 `css_stats` rows and 5 estimate rows for each of DIABP/815 and SYSBP/816:

     ```sas
     proc freq data=wpctcap.css_stats;
       tables PARAMCD*ATPTN / list;
     run;
     proc freq data=wpctcap.parameter_estimates;
       tables PARAMCD*ATPTN / list;
     run;
     ```

3. **Format stripping.** The CSV step removes every format (`FORMAT _ALL_`) and writes each numeric with `PUTN` and `WPCT_NUMFMT`. Character values are written as stored, without trailing blanks, and are enclosed in double quotes, with each double quote in them doubled, when they hold a comma, a double quote or a line break. The read-back check reads the CSV with SAS list input, which removes leading blanks and enclosing quotes, so a character value with leading blanks, a quoted value or one with a line break can draw the WARNING `... CSV values read back as other text than the native value.`, or a row-count WARNING, although the CSV holds it as stored.
   * *Symptom:* rounded values, `<.0001`, or other text in a numeric CSV column. `python -m wpct reproduce` reports such text as an input error (exit code 2), naming the file, column and row.
   * *Debug:* run `head -3 "$WPCT_CSVDIR/WPCT-F.07.02_parameter_estimates.csv"`. With the default `E32.17`, every numeric field is a number in scientific notation or `.`; with `BEST32.` (point 4), ordinary decimal and integer tokens are valid too. Text such as `<.0001` is never valid, and neither is a value rounded against the native one. Compare with the native values:

     ```sas
     proc print data=wpctcap.parameter_estimates;
       format _numeric_ best32.;
     run;
     ```

4. **E32.17.** The SAS 9.4 documentation lists the E format as `Ew.` only, so whether `E32.17` is accepted is unverified. SAS also documents formatted values beyond about 15 significant digits as possibly inaccurate.
   * *Symptom:* the pre-flight `WPCT_NUMFMT E32.17 did not write a number that SAS reads back`. Alternatively, a read-back WARNING `... CSV values read back as another number than the native value, largest absolute difference ...`.
   * *Debug:* after the pre-flight ERROR, set `%let wpct_numfmt = BEST32.;` in the copy and run again. Record any read-back WARNING, with its largest difference, alongside the exports. The comparison limits are in [`tolerances.json`](../../tolerances.json), and the `.sas7bdat` copies stay exact either way.
5. **CRLF line endings.** `WPCT-F.07.02.sas` has CRLF line endings, so on UNIX each record ends with a CR, which the staged-copy step removes.
   * *Symptom:* anchor mismatches on an unchanged file, or CR characters left in the staged copy.
   * *Debug:* `file whitepapers/WPCT/WPCT-F.07.02.sas` reports `with CRLF line terminators`. The next command must print `0`:

     ```sh
     grep -c "$(printf '\r')" "$WPCT_WORKDIR/WPCT-F.07.02_staged.sas"
     ```

6. **Anchor mismatch.** Lines 85, 126, 280 and 308 are checked.
   * *Symptom:* `Line <n> of WPCT-F.07.02.sas does not hold the expected anchor text. It reads: ...`, then `Staged copy rejected`. Nothing runs.
   * *Debug:* `git status --porcelain whitepapers/WPCT/WPCT-F.07.02.sas` must print nothing. The two hashes below, of the unchanged bytes on disk and of the upstream file, must be equal, and the last command shows the four anchor lines. A different revision of the program needs a change to the launcher.

     ```sh
     git hash-object --no-filters -- whitepapers/WPCT/WPCT-F.07.02.sas
     git rev-parse 398a6d33ced9359ffb58c46650a6d488811176b1:whitepapers/WPCT/WPCT-F.07.02.sas
     sed -n '85p;126p;280p;308p' whitepapers/WPCT/WPCT-F.07.02.sas
     ```

7. **SASAUTOS.** `util_resolve_sasautos`, which `assert_macro_exist` uses, parses one flat list of filerefs and double-quoted paths. Line 76 wraps the current SASAUTOS value in another pair of parentheses, with an empty `""` entry. The launcher therefore sets SASAUTOS to the single fileref `WPCTAUTO`. It concatenates the double-quoted `"<repo>/whitepapers/utilities"` first, then every entry of the SASAUTOS option the session started with, in order. A quoted path is carried as given. A fileref is expanded to its directories from `SASHELP.VEXTFL`, or through `PATHNAME` when the view has no row for it. An empty `""` entry is skipped with a NOTE.
   * *Symptom:* `ERROR: (ASSERT_DEPEND) Macro <NAME> is required but not in the AUTOCALL path(s).`, or `Apparent invocation of macro ... not resolved`. A `WARNING: (WPCT CAPTURE) SASAUTOS entry <n> ... is not carried into WPCTAUTO` names an entry that gave no directory.
   * *Debug:* read the log line `SASAUTOS before the launcher: ...`, the `SASAUTOS entry <n> ...` NOTE or WARNING of each entry, and the line `SASAUTOS=... with WPCTAUTO=...`. Then list the fileref, whose first directory must be `<repo>/whitepapers/utilities`:

     ```sas
     proc print data=sashelp.vextfl;
       where fileref = 'WPCTAUTO';
     run;
     ```

8. **Lines 76 and 82 of the original.** Line 76 continues after its asterisk comment with text that SAS reads as an invalid statement. The asterisk comment on line 82 does not stop its macro call from running. The invalid statement absorbs that call's URL FILENAME statement, so no network connection is made, but the call's DATA step fails. The launcher sets NOSYNTAXCHECK for the run and tests the program's halting symbol CONTINUE instead.
   * *Symptom:* ERROR lines at the start of the staged program's log.
   * *Debug:* the ERROR lines from these two lines are expected. Any other ERROR is not, so check the halt messages of point 11.
9. **MISSING= option.** Line 324 sets `options missing=' '` for the rest of the session, and the launcher sets `missing='.'` again before writing the CSVs.
   * *Symptom:* blank numeric fields where `.` was expected.
   * *Debug:* both load as missing (export README, Format), so act only if other text appears. `proc options option=missing; run;` shows the value in effect.
10. **FILE ENCODING=.** `WPCT_ENCODING` is `latin1`, the UNIX name.
    * *Symptom:* an ERROR on the FILE statement that names the encoding.
    * *Debug:* on Windows, set `%let wpct_encoding = wlatin1;` in the copy and run again. `wlatin1` and `latin1` differ only in the bytes 80 to 9F hexadecimal. Record which encoding ran.
11. **Halts through assert_continue.** When the program's dependency checks fail, it sets `CONTINUE=0` and forces syntax-check mode (`OBS=0`). NOSYNTAXCHECK keeps that mode from taking effect in batch. The launcher detects `OBS=0`, or a `CONTINUE` other than 1, and writes no CSV.
    * *Symptom:* `The program halted: CONTINUE=0.` or `The program left SAS in syntax-check mode (OBS=0).`, then `No CSV is written.`
    * *Debug:* the `(ASSERT_DEPEND)` lines before these messages name the cause. It is a SAS release older than 9.4M2 (an ERROR), a missing variable, or a missing macro (point 7). An OS outside AIX, WIN and HP IPF gives only a WARNING. Start a new session before the next run.
12. **Backslash in the PDF path.** Line 339 writes `"&outputs_folder\WPCT-F.07.02_Box_plot_..."`.
    * *Symptom:* on UNIX the PDFs are files named `pdf\WPCT-F.07.02_Box_plot_...pdf` inside `<work>`, not in `<work>/pdf`.
    * *Debug:* nothing to fix, since no comparison uses the PDFs. `ls "$WPCT_WORKDIR"` lists them.
13. **SASUSER template store.** `util_proc_template` compiles the `PhUSEboxplot` template. The launcher puts `WORK.TEMPLAT(UPDATE)` first on the ODS path, so a read-only SASUSER store does not matter and nothing is written to it.
    * *Symptom:* a PROC TEMPLATE ERROR about a template store that cannot be updated.
    * *Debug:* the ODS PATH statement that puts `work.templat(update)` first must appear in the capture log before the staged program runs:

      ```sh
      grep -n -i 'work.templat' "$WPCT_WORKDIR/WPCT-F.07.02_capture.log"
      ```
14. **Directory creation.** DCREATE creates one level only, so the parents of `wpct_workdir` and `wpct_csvdir` must exist.
    * *Symptom:* `Cannot create the directory <dir>. Its parent must exist and be writable.`
    * *Debug:* create the missing parent directory and run again. Step 2 creates the work directory itself with `mkdir -p "$WPCT_WORKDIR"`.
15. **Log location.** Settings and guard messages go to the session log. From the redirect on, the log goes to `<work>/WPCT-F.07.02_capture.log`, and the final status line goes back to the session log.
    * *Symptom:* a SAS log or listing in the checkout. `git status --porcelain` lists it.
    * *Debug:* move it into the work directory, and in batch start SAS there, as in step 4.
16. **PROC APPEND runs without FORCE.** If the second stratum has a variable of another length, the append stops with an ERROR instead of truncating values.
    * *Symptom:* a PROC APPEND ERROR for `wpctcap.parameter_estimates` or `wpctcap.css_stats`.
    * *Debug:* `proc contents data=wpctcap.css_stats; run;` shows the attributes the first stratum set. The fix is a change to the launcher.

Any change to the launcher is a repository change. Commit it, run the contract check of [section 6.1](#61-contract-check-of-the-launcher), and record the new commit.
