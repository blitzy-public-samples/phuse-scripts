## Python port of WPCT Figures 7.2 and 7.7

### About

This folder holds a Python port of two PHUSE Central Tendency programs: [`WPCT-F.07.02.sas`](../../whitepapers/WPCT/WPCT-F.07.02.sas) (Figure 7.2, change from baseline by visit) and [`WPCT-F.07.07.sas`](../../whitepapers/WPCT/WPCT-F.07.07.sas) (Figure 7.7, change from MIN baseline to MIN post-baseline, per study and pooled). The port reproduces the record selection, the MIN baseline and post-baseline derivation of 07.07 (`%derive_lastminmax_measure`), the PROC SUMMARY statistics `n mean std median datamin datamax q1 q3`, the PROC GLM `SOLUTION` ParameterEstimates and the `pval` merged into the statistics. It then checks these results against the SAS references in [`whitepapers/WPCT/outputs_sas/`](../../whitepapers/WPCT/outputs_sas/). It does not reproduce the box plots, PDFs, titles, display formats or GTL template. The SAS programs, macros, datasets and saved SAS outputs are read and never modified.

Where SAS and a Python library differ by default, the port follows SAS. Each difference is recorded in Part C (`D-01` to `D-16`) of [docs/sas_specification.md](docs/sas_specification.md); examples are the quantile definition, the GLM less-than-full-rank design and the order of missing BY values. The pooled 07.07 model keeps the continuous `studynum` term, which is constant on the pooled records, so it is aliased to 0 exactly as in SAS (`DIV-01`). Docstrings and test names cite the specification item IDs they implement or verify.

### Contents

* [docs/sas_specification.md](docs/sas_specification.md) - the extracted SAS specification (Deliverable 1). It holds the items `S02-*` (07.02), `S07-*` and `LMM-*` (07.07), the default differences `D-*`, the legacy environment assumptions `ENV-*`, the divergence `DIV-01` and the unverified SAS behaviours `U-01` to `U-03`.
* [wpct/](wpct/) - the importable package. The per-program drivers are [wpct/programs/wpct_f_07_02.py](wpct/programs/wpct_f_07_02.py) and [wpct/programs/wpct_f_07_07.py](wpct/programs/wpct_f_07_07.py).
* [tests/](tests/) - the `pytest` equivalence suite.
* [tolerances.json](tolerances.json) - the single tolerance source. Edit it to change the pass/fail limits of both the tests and the report.
* [reference/sas_exports/](reference/sas_exports/) - where optional full-precision SAS exports go. Its [README](reference/sas_exports/README.md) is the contract for their names, format and comparison.
* [reports/equivalence_report.md](reports/equivalence_report.md) - the generated equivalence report (Deliverable 3), committed as produced and never edited by hand.
* `outputs/` - the generated intermediate CSVs, ignored by git (see [.gitignore](.gitignore)).
* [requirements.txt](requirements.txt) - the pinned Python 3.10 environment; [pyproject.toml](pyproject.toml) - `pytest` settings only.

### Reproduce

Run every command from `python/wpct/`. The package runs from there without installation.

1. Create and activate a Python 3.10 virtual environment. Keep it outside the checkout, because `.gitignore` does not cover virtual environments:

   ```sh
   python3.10 -m venv ~/.venvs/wpct
   . ~/.venvs/wpct/bin/activate
   ```

2. Install the pinned packages from binary wheels:

   ```sh
   pip install --only-binary :all: -r requirements.txt
   ```

3. Re-run both programs and regenerate the report:

   ```sh
   python -m wpct reproduce
   ```

   This reads `data/adam/cdisc/advs.xpt` (07.02) and `data/adam/cdisc-split/advs.xpt` (07.07) from the repository root, writes `outputs/WPCT-F.07.02/` and `outputs/WPCT-F.07.07/`, and regenerates `reports/equivalence_report.md`. It prints PASS, FAIL and NOT COMPARED counts per program and overall; the warnings for records the 07.07 derivation omits go to stderr as `WARNING: wpct.derivation: ...` lines, as SAS writes WARNING lines to its log, and each names only the omitted group's `STUDYID`, `USUBJID`, `PARAMCD` and `ATPTN`. The only option is `--sas-exports DIR`, which names a read-only directory of full-precision SAS exports (default `reference/sas_exports/`); a directory named this way must exist:

   ```sh
   python -m wpct reproduce --sas-exports DIR
   ```

4. Run the tests:

   ```sh
   python -m pytest
   ```

   On a clean checkout every test on repository data passes. The seven export cases in `tests/test_sas_exports.py` read SAS exports from `reference/sas_exports/` only; each case whose file is absent is skipped with a reason that `pytest` prints (`pyproject.toml` sets `-ra`).

`python -m wpct reproduce` exits with:

* `0` - no comparison failed and no program halted;
* `1` - a comparison failed or a program halted (a halted program exports nothing, and the report records the halt);
* `2` - an input error or a rejected write destination. Input errors are a missing or unreadable ADVS file, SAS listing, SAS log or tolerance file, a malformed SAS export, an input lacking a required variable, an ADVS file storing a required variable under more than one spelling (columns that differ only in case), a `--sas-exports` that is not an existing directory, and a command-line usage error. Inputs are read before anything is written, so an input error writes nothing. An absent SAS export is not an error: its quantities are reported as NOT COMPARED.

`git` should be on the `PATH`: the report's provenance and source-integrity sections use it. Without it, the report says that integrity was not verified.

### Outputs

`python -m wpct reproduce` writes these files under `outputs/`. Names, SAS counterparts and record keys are quoted from [wpct/schemas.py](wpct/schemas.py), the binding source.

| File | SAS counterpart | Record key |
|---|---|---|
| `WPCT-F.07.02/css_anadata.csv` | WORK.CSS_ANADATA, 3548 × 14 | `STUDYID USUBJID PARAMCD ATPTN AVISITN` |
| `WPCT-F.07.02/css_stats.csv` | WORK.CSS_STATS per stratum, with PARAMCD ATPTN prepended | `PARAMCD ATPTN AVISITN TRTPN` |
| `WPCT-F.07.02/parameter_estimates.csv` | ODS ParameterEstimates per stratum | `PARAMCD ATPTN EFFECT LEVEL` |
| `WPCT-F.07.02/models.csv` | Model-level context; no direct SAS dataset | `PARAMCD ATPTN` |
| `WPCT-F.07.02/checkpoints.csv` | SAS log record counts | `SAS_DATASET OCCURRENCE` |
| `WPCT-F.07.07/advs_min.csv` | WORK.ADVS_MIN after the post-baseline filter, 498 × 16 | `STUDYID USUBJID TRTPN PARAMCD ATPTN AVISITN` |
| `WPCT-F.07.07/css_anadata.csv` | WORK.CSS_ANADATA with the pooled rows and studynum, 996 × 13 | `STUDYID USUBJID PARAMCD ATPTN` |
| `WPCT-F.07.07/css_stats.csv` | WORK.CSS_STATS per stratum, with PARAMCD ATPTN prepended | `PARAMCD ATPTN studynum TRTPN` |
| `WPCT-F.07.07/parameter_estimates.csv` | ODS ParameterEstimates per study and pooled | `PARAMCD ATPTN STUDYNUM EFFECT LEVEL` |
| `WPCT-F.07.07/models.csv` | Model-level context; no direct SAS dataset | `PARAMCD ATPTN STUDYNUM` |
| `WPCT-F.07.07/checkpoints.csv` | SAS log record counts | `SAS_DATASET OCCURRENCE` |

Every file has the same format:

* CSV in UTF-8, with one header row of SAS variable names in SAS dataset order; stratum columns come first.
* Numbers are written with `%.17g`, which round-trips exactly. Integers are written as integers, and `ALIASED` as `True` or `False`.
* A missing value is an empty field.
* Rows are sorted by the record key, stably and with missing values first. An empty dataset still has its header row.

SAS variable names are case-insensitive, and every comparison folds them to upper case, so the `studynum` column of 07.07 `css_stats.csv` and `STUDYNUM` in its parameter rows name the same variable. `EFFECT` is `INTERCEPT`, `BASE`, `TRTPN` or `STUDYNUM`, and `LEVEL` is the CLASS level (`54`, `81`, `0`) or empty.

### Write policy

* Data are read only through `wpct/xpt_reader.py`, and SAS references only through `wpct/sas_reference.py`. Nothing opens a network connection.
* The only writes go to `python/wpct/outputs/**` and `python/wpct/reports/equivalence_report.md`.
* Before any directory is created or file opened, every write target passes `check_write_destination` in `wpct/paths.py`. It resolves the target with every symlink followed and raises `OutputPathError` for a target outside those two destinations, such as a SAS-era file or a path outside the checkout. Each writer's fixed-target check then requires the resolved path to be its own fixed destination, so a destination reached through a symlink below the write root (the checkout, for the command line) also raises `OutputPathError`. On either refusal the refused writer call creates no directory and writes no file, and `python -m wpct reproduce` exits with `2`. Files written by earlier calls of the run stay: the command writes the 07.02 exports, then the 07.07 exports, then the report, each in its own call.
* The command line has no output, report or write-root option.

### Reference results

The run executes no SAS and invents no SAS value. The GLM listings and logs in [`whitepapers/WPCT/outputs_sas/`](../../whitepapers/WPCT/outputs_sas/) come from commit `6fc757d` and were produced by SAS 9.4 TS1M2. The listings are compared at their displayed precision, and the log record counts exactly. The box-plot PDFs hold their statistics only as raster images, so they are not compared. Full-precision equivalence requires the optional exports described in [reference/sas_exports/README.md](reference/sas_exports/README.md). The report lists every quantity without a reference as NOT COMPARED and claims no equivalence for it.

### Licence

* This folder falls under the repository's MIT licence in [LICENSE.md](../../LICENSE.md). No separate licence file is added.
* The code reads the CDISC-pilot-derived datasets in `data/adam/cdisc/` and `data/adam/cdisc-split/` in place, without copying, modifying or redistributing them. Record-level exports stay in the git-ignored `outputs/`.
* The terms described in [data/adam/README.md](../../data/adam/README.md), [data/adam/cdisc/README.md](../../data/adam/cdisc/README.md) and [data/adam/cdisc-split/README.md](../../data/adam/cdisc-split/README.md) continue to govern those datasets: CDISC pilot submission data modified by PhUSE, whose complete CDISC package is available to CDISC members only.
