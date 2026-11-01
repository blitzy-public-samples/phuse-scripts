# WPCT Python port — equivalence report

This report compares the Python port of WPCT-F.07.02 and WPCT-F.07.07 under `python/wpct/` with the SAS reference results: the listings and logs in `whitepapers/WPCT/outputs_sas/` and any user-supplied full-precision SAS exports. It covers the intermediate datasets, the PROC SUMMARY statistics and the PROC GLM SOLUTION tables; the box-plot graphics are not reproduced. The specification is [python/wpct/docs/sas_specification.md](../docs/sas_specification.md). `python -m wpct reproduce` generates this file; it is not edited by hand.

## Provenance

- Upstream commit, the commit the port was built against: `398a6d33ced9359ffb58c46650a6d488811176b1`
- Checkout `HEAD` when the report was generated (`git rev-parse HEAD`): `989f000d6fbfc41c888475eaf8ae3e4abbe04dfa`
- Python: `3.10.22`
- Source integrity: all 36 read-only inputs match their blobs at the upstream commit.

### Library versions

| Package | Version |
|---|---|
| numpy | 2.2.6 |
| pandas | 2.3.3 |
| scipy | 1.15.3 |
| statsmodels | 0.15.0 |
| pyreadstat | 1.3.6 |
| pytest | 9.2.0 |

### Tolerances

Rendered from `python/wpct/tolerances.json` (schema version 1), the single tolerance source; every comparison below applies these values. The Fields column lists the export variables each class compares, as `python/wpct/wpct/schemas.py` defines them.

File description: Single tolerance source for the WPCT-F.07.02 and WPCT-F.07.07 Python-versus-SAS equivalence checks, holding the values proposed by the request; edit this file to change them.

| Class | Label | Rule | Limit | Settings | Fields | Applies to |
|---|---|---|---|---|---|---|
| `counts` | Counts | exact | 0 | — | n | Log checkpoints, per-stratum record counts, summary n, number of parameter rows |
| `categorical` | Keys and categorical fields | exact | — | strip_trailing_blanks = true | STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD AVISIT AVISITN ATPT ATPTN ANL02FL studynum | Record keys, treatment labels, flags, visit numbers, study identifiers, EFFECT, LEVEL |
| `summary_statistics` | Summary statistics | absolute | 1e-08 | — | mean std median datamin datamax q1 q3 | PROC SUMMARY statistics against full-precision exports |
| `glm_estimates` | GLM estimate, SE, t | relative | 1e-06 | zero_reference = exact | ESTIMATE STDERR TVALUE | ParameterEstimates against full-precision exports |
| `p_values` | p-values | absolute | 1e-08 | — | pval PROBT | PROBT and the merged CSS_STATS pval against full-precision exports |
| `record_numeric` | Record-level numeric values | absolute | 1e-08 | — | BASE CHG AVAL ADT | Record-by-record comparisons; the request names no class for these, so the plan sets this value |
| `rounded_listing` | Rounded listing values | displayed_precision | — | half_unit = 0.5; epsilon = 1e-12; bound_token = <.0001; bound_upper = 0.0001 | — | Every value parsed from WPCT-F.07.02.lst and WPCT-F.07.07.lst, including the TRTPN Probt used for merged pval cells |
| `missing` | Missing values | missing_agreement | — | both_missing = PASS; one_missing = FAIL | — | All classes |

### Reference provenance

- The SAS listings and logs used as references come from commit `6fc757d` (2016-02-26) and were produced by SAS 9.4 TS1M2 with SAS/STAT 13.2 on Windows.
- The SAS listings and logs were last committed in 6fc757d (2016-02-26). The programs were edited afterwards only in f2bd8d0 (whitepapers/WPCT/WPCT-F.07.02.sas, 2017-01-03) and 1788bb7 (whitepapers/WPCT/WPCT-F.07.07.sas, 2017-02-27). Those edits changed only environment settings and parameterisation: git diff -w --ignore-cr-at-eol --ignore-blank-lines 6fc757d HEAD on either program shows no analysis step changed. The ADVS input files are unchanged since 6c47a90 (2015-12-28).
- The Sources section of `python/wpct/docs/sas_specification.md` details these later edits.
- The box-plot PDFs hold their statistics only as raster images and are not used as references.

## Summary

| Program | PASS | FAIL | NOT COMPARED | Halted |
|---|---|---|---|---|
| WPCT-F.07.02 | 61 | 0 | 14 | no |
| WPCT-F.07.07 | 175 | 0 | 17 | no |
| Overall | 236 | 0 | 31 | — |

NOT COMPARED rows are counted on their own, never as PASS; no equivalence is claimed for them (see Not compared).

Result: no comparison failed and no program halted (236 PASS).

## WPCT-F.07.02

PASS 61, FAIL 0, NOT COMPARED 14; the NOT COMPARED quantities are listed under Not compared.

Halt: none; every halting check passed.

### Models

| Model | Stratum | NOBS | DFE | Status |
|---|---|---|---|---|
| endpoint | PARAMCD=DIABP ATPTN=815 | 226 | 222 | fitted |
| endpoint | PARAMCD=SYSBP ATPTN=816 | 226 | 222 | fitted |

### Log checkpoints (WPCT-F.07.02.log)

Record counts at the pinned lines of whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log, compared under the counts class.

PASS 15, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result |
|---|---|---|---|---|---|---|
| ADVS #1 | — | 32139/44 | 32139/44 | 0/0 | exact | PASS |
| ADVS_SUB #1 | — | 5925/45 | 5925/45 | 0/0 | exact | PASS |
| CSS_ANADATA #1 | — | 3548/14 | 3548/14 | 0/0 | exact | PASS |
| CSS_NEXTPARAM #1 | PARAMCD=DIABP | 1774 | 1774 | 0 | exact | PASS |
| CSS_NEXTTIMEPT #1 | PARAMCD=DIABP ATPTN=815 | 1774 | 1774 | 0 | exact | PASS |
| CSS_STATS #1 | PARAMCD=DIABP ATPTN=815 | 30/12 | 30/12 | 0/0 | exact | PASS |
| CSS_PVALUES #1 | PARAMCD=DIABP ATPTN=815 | 5 | 5 | 0 | exact | PASS |
| CSS_PVALUES #2 | PARAMCD=DIABP ATPTN=815 | 3 | 3 | 0 | exact | PASS |
| CSS_STATS #2 | PARAMCD=DIABP ATPTN=815 | 30/13 | 30/13 | 0/0 | exact | PASS |
| CSS_NEXTPARAM #2 | PARAMCD=SYSBP | 1774 | 1774 | 0 | exact | PASS |
| CSS_NEXTTIMEPT #2 | PARAMCD=SYSBP ATPTN=816 | 1774 | 1774 | 0 | exact | PASS |
| CSS_STATS #3 | PARAMCD=SYSBP ATPTN=816 | 30/12 | 30/12 | 0/0 | exact | PASS |
| CSS_PVALUES #3 | PARAMCD=SYSBP ATPTN=816 | 5 | 5 | 0 | exact | PASS |
| CSS_PVALUES #4 | PARAMCD=SYSBP ATPTN=816 | 3 | 3 | 0 | exact | PASS |
| CSS_STATS #4 | PARAMCD=SYSBP ATPTN=816 | 30/13 | 30/13 | 0/0 | exact | PASS |

### ParameterEstimates against the listing (WPCT-F.07.02.lst)

Rounded reference only: agreement is shown at the precision displayed in `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst` (`8 (Estimate, StdErr), 2 (t Value), 4 (Pr > |t|)` decimals; `<.0001` as a bound). Equivalence within the full-precision tolerance `relative ≤ 1e-06 for Estimate, StdErr and t Value; absolute ≤ 1e-08 for Pr > |t|` is not established for these quantities.

PASS 40, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result | Reason |
|---|---|---|---|---|---|---|---|
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 33.33892013 | 33.33892012894261 | -1.06e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 3.90872985 | 3.9087298540294992 | 4.03e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 8.53 | 8.52934875879785 | -0.000651 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=815 AVISITN=99 | <.0001 | 2.3024884404195836e-15 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -0.46079578 | -0.46079577501949254 | 4.98e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 0.05043555 | 0.050435549082498345 | -9.18e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -9.14 | -9.13632910520634 | 0.00367 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=815 AVISITN=99 | <.0001 | 4.1519226930145117e-17 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -1.29895697 | -1.298956974314804 | -4.31e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 1.25848618 | 1.258486183826882 | 3.83e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -1.03 | -1.0321583113172177 | -0.00216 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 0.3031 | 0.30312108990742137 | 2.11e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -1.35496023 | -1.354960231552629 | -1.55e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 1.25345307 | 1.2534530743479069 | 4.35e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | -1.08 | -1.0809820162254817 | -0.000982 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 0.2809 | 0.2808779992605109 | -2.2e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=815 AVISITN=99 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=815 AVISITN=99 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=815 AVISITN=99 | . | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 65.22524443 | 65.22524442577146 | -4.23e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 7.46577357 | 7.465773572108252 | 2.11e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 8.74 | 8.736568795690461 | -0.00343 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | <.0001 | 5.929462692067913e-16 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | -0.51737994 | -0.5173799360809068 | 3.92e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.05370400 | 0.05370400067655171 | 6.77e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | -9.63 | -9.633917949558006 | -0.00392 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | <.0001 | 1.413863164368079e-18 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.53010081 | 0.5301008109522358 | 9.52e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 2.43466899 | 2.4346689888454027 | -1.15e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.22 | 0.2177301363679941 | -0.00227 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.8278 | 0.8278393733071719 | 3.94e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | -4.16039704 | -4.1603970393239695 | 6.76e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 2.42781490 | 2.427814895812768 | -4.19e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | -1.71 | -1.713638484754077 | -0.00364 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.0880 | 0.08799102576140513 | -8.97e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=SYSBP ATPTN=816 AVISITN=99 | (blank) | . |  | missing agreement | PASS | both missing |

### Merged CSS_STATS.pval against the listing's TRTPN Probt (WPCT-F.07.02.lst): verifies the CSS_STATS cells filled by the merge, not the model rows

Rounded reference only: agreement is shown at the precision displayed in `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst` (`4` decimals; `<.0001` as a bound). Equivalence within the full-precision tolerance `absolute ≤ 1e-08` is not established for these quantities.

PASS 6, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result | Reason |
|---|---|---|---|---|---|---|---|
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 AVISITN=99 TRTPN=54 | 0.3031 | 0.30312108990742137 | 2.11e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 AVISITN=99 TRTPN=81 | 0.2809 | 0.2808779992605109 | -2.2e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 AVISITN=99 TRTPN=0 | . | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=SYSBP ATPTN=816 AVISITN=99 TRTPN=54 | 0.8278 | 0.8278393733071719 | 3.94e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=SYSBP ATPTN=816 AVISITN=99 TRTPN=81 | 0.0880 | 0.08799102576140513 | -8.97e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=SYSBP ATPTN=816 AVISITN=99 TRTPN=0 | (blank) | . |  | missing agreement | PASS | both missing |

## WPCT-F.07.07

PASS 175, FAIL 0, NOT COMPARED 17; the NOT COMPARED quantities are listed under Not compared.

Halt: none; every halting check passed.

Derivation omission counts: LMM_BASE_OMITTED 10, LMM_POST_OMITTED 0.

### Models

| Model | Stratum | NOBS | DFE | Status |
|---|---|---|---|---|
| study | PARAMCD=DIABP ATPTN=815 studynum=1 | 124 | 120 | fitted |
| study | PARAMCD=DIABP ATPTN=815 studynum=2 | 126 | 122 | fitted |
| pooled | PARAMCD=DIABP ATPTN=815 studynum=3 | 250 | 246 | fitted |
| study | PARAMCD=DIABP ATPTN=817 studynum=1 | 122 | 118 | fitted |
| study | PARAMCD=DIABP ATPTN=817 studynum=2 | 126 | 122 | fitted |
| pooled | PARAMCD=DIABP ATPTN=817 studynum=3 | 248 | 244 | fitted |

### Log checkpoints (WPCT-F.07.07.log)

Record counts at the pinned lines of whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log, compared under the counts class.

PASS 29, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result |
|---|---|---|---|---|---|---|
| ADVS #1 | — | 32139/44 | 32139/44 | 0/0 | exact | PASS |
| ADVS_SUB #1 | — | 5926/45 | 5926/45 | 0/0 | exact | PASS |
| LMM_BASE #1 | — | 1514 | 1514 | 0 | exact | PASS |
| LMM_POST #1 | — | 4411 | 4411 | 0 | exact | PASS |
| LMM_BASE_ANL #1 | — | 629 | 629 | 0 | exact | PASS |
| LMM_POST_ANL #1 | — | 794 | 794 | 0 | exact | PASS |
| LMM_BASE_ANL #2 | — | 508 | 508 | 0 | exact | PASS |
| LMM_POST_ANL #2 | — | 498 | 498 | 0 | exact | PASS |
| LMM_BASE_OMITTED #1 | — | 10 | 10 | 0 | exact | PASS |
| LMM_BASE_ANL #3 | — | 498 | 498 | 0 | exact | PASS |
| LMM_POST_ANL #3 | — | 498 | 498 | 0 | exact | PASS |
| ADVS_MIN #1 | — | 996/16 | 996/16 | 0/0 | exact | PASS |
| ADVS_MIN #2 | — | 498/16 | 498/16 | 0/0 | exact | PASS |
| CSS_SAFANA #1 | — | 498/12 | 498/12 | 0/0 | exact | PASS |
| CSS_ANADATA #1 | — | 996/12 | 996/12 | 0/0 | exact | PASS |
| CSS_ANADATA #2 | — | 996/13 | 996/13 | 0/0 | exact | PASS |
| CSS_NEXTPARAM #1 | PARAMCD=DIABP | 996 | 996 | 0 | exact | PASS |
| CSS_NEXTTIMEPT #1 | PARAMCD=DIABP ATPTN=815 | 500 | 500 | 0 | exact | PASS |
| CSS_STATS #1 | PARAMCD=DIABP ATPTN=815 | 9/12 | 9/12 | 0/0 | exact | PASS |
| CSS_PVAL_STDY #1 | PARAMCD=DIABP ATPTN=815 | 10 | 10 | 0 | exact | PASS |
| CSS_PVAL_POOL #1 | PARAMCD=DIABP ATPTN=815 | 6 | 6 | 0 | exact | PASS |
| CSS_PVAL_STDY #2 | PARAMCD=DIABP ATPTN=815 | 9 | 9 | 0 | exact | PASS |
| CSS_STATS #2 | PARAMCD=DIABP ATPTN=815 | 9/13 | 9/13 | 0/0 | exact | PASS |
| CSS_NEXTTIMEPT #2 | PARAMCD=DIABP ATPTN=817 | 496 | 496 | 0 | exact | PASS |
| CSS_STATS #3 | PARAMCD=DIABP ATPTN=817 | 9/12 | 9/12 | 0/0 | exact | PASS |
| CSS_PVAL_STDY #3 | PARAMCD=DIABP ATPTN=817 | 10 | 10 | 0 | exact | PASS |
| CSS_PVAL_POOL #2 | PARAMCD=DIABP ATPTN=817 | 6 | 6 | 0 | exact | PASS |
| CSS_PVAL_STDY #4 | PARAMCD=DIABP ATPTN=817 | 9 | 9 | 0 | exact | PASS |
| CSS_STATS #4 | PARAMCD=DIABP ATPTN=817 | 9/13 | 9/13 | 0/0 | exact | PASS |

### ParameterEstimates against the listing (WPCT-F.07.07.lst)

Rounded reference only: agreement is shown at the precision displayed in `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst` (`8 (Estimate, StdErr), 2 (t Value), 4 (Pr > |t|)` decimals; `<.0001` as a bound). Equivalence within the full-precision tolerance `relative ≤ 1e-06 for Estimate, StdErr and t Value; absolute ≤ 1e-08 for Pr > |t|` is not established for these quantities.

PASS 128, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result | Reason |
|---|---|---|---|---|---|---|---|
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=1 | 15.75244923 | 15.752449233485113 | 3.49e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=815 studynum=1 | 4.36419465 | 4.364194645284503 | -4.72e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=815 studynum=1 | 3.61 | 3.6094744881522596 | -0.000526 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.0004 | 0.00044874617096499496 | 4.87e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=1 | -0.31053675 | -0.3105367514473665 | -1.45e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.06185448 | 0.06185447923231381 | -7.68e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=815 studynum=1 | -5.02 | -5.020440803988483 | -0.000441 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=815 studynum=1 | <.0001 | 1.8119144044972648e-06 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=1 | 2.80528530 | 2.805285301851728 | 1.85e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=815 studynum=1 | 1.43665370 | 1.4366537020564392 | 2.06e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=1 | 1.95 | 1.9526524017835454 | 0.00265 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.0532 | 0.05318921439386113 | -1.08e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=1 | 1.18013419 | 1.180134192498465 | 2.5e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=815 studynum=1 | 1.43947011 | 1.439470106410328 | -3.59e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.82 | 0.8198393195128028 | -0.000161 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.4139 | 0.41393313828451395 | 3.31e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=1 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=815 studynum=1 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=1 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=815 studynum=1 | . | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=2 | 21.38299546 | 21.382995460159925 | 1.6e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=815 studynum=2 | 5.48028297 | 5.480282973762911 | 3.76e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=815 studynum=2 | 3.90 | 3.901804991189676 | 0.0018 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.0002 | 0.00015682918576740673 | -4.32e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=2 | -0.38218673 | -0.3821867349145419 | -4.91e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.07228203 | 0.0722820344747097 | 4.47e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=815 studynum=2 | -5.29 | -5.287437434377456 | 0.00256 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=815 studynum=2 | <.0001 | 5.526405434619107e-07 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.94439174 | 0.9443917411766691 | 1.18e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=815 studynum=2 | 1.54434326 | 1.5443432645859232 | 4.59e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.61 | 0.6115167287176171 | 0.00152 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.5420 | 0.5419953823039609 | -4.62e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=2 | 1.10341752 | 1.1034175151101966 | -4.89e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=815 studynum=2 | 1.56204177 | 1.562041770519625 | 5.2e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.71 | 0.7063943717351018 | -0.00361 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.4813 | 0.4812898017449755 | -1.02e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=2 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=815 studynum=2 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=2 | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=815 studynum=2 | . | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 18.15533627 | 18.155336273371802 | 3.37e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 3.39592981 | 3.395929809159588 | -8.4e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 5.35 | 5.346204808003619 | -0.0038 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | <.0001 | 2.0499342176614186e-07 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | -0.34193814 | -0.3419381442534497 | -4.25e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 0.04633025 | 0.046330246201124865 | -3.8e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | -7.38 | -7.3804516982072 | -0.000452 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | <.0001 | 2.4241557031779827e-12 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.94462248 | 1.9446224840788355 | 4.08e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.04357442 | 1.0435744202413055 | 2.41e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.86 | 1.863424827554877 | 0.00342 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 0.0636 | 0.06359408020168611 | -5.92e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.21700468 | 1.2170046816041036 | 1.6e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.05076701 | 1.0507670118079036 | 1.81e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 1.16 | 1.1582060227701465 | -0.00179 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 0.2479 | 0.24790315315367645 | 3.15e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| STUDYNUM ESTIMATE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| STUDYNUM STDERR | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| STUDYNUM TVALUE | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| STUDYNUM PROBT | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) | . | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=1 | 20.55095420 | 20.550954202439627 | 2.44e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=817 studynum=1 | 4.43571607 | 4.435716071309411 | 1.31e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=817 studynum=1 | 4.63 | 4.63306349461025 | 0.00306 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=817 studynum=1 | <.0001 | 9.352860082772907e-06 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=1 | -0.37573046 | -0.3757304623426662 | -2.34e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.06181915 | 0.06181915324388047 | 3.24e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=817 studynum=1 | -6.08 | -6.077897263658493 | 0.0021 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=817 studynum=1 | <.0001 | 1.5373458751338856e-08 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.00134690 | 0.0013469012952579832 | 1.3e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=817 studynum=1 | 1.48321417 | 1.4832141710527735 | 1.05e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.00 | 0.000908096296236142 | 0.000908 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.9993 | 0.9992769775187007 | -2.3e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.86114576 | 0.8611457576275143 | -2.37e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=817 studynum=1 | 1.47436875 | 1.4743687467327395 | -3.27e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.58 | 0.5840775989967557 | 0.00408 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.5603 | 0.5602830264511478 | -1.7e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=1 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=817 studynum=1 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=1 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=817 studynum=1 | (blank) | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=2 | 23.13858592 | 23.13858591672568 | -3.27e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=817 studynum=2 | 4.97748811 | 4.9774881144495495 | 4.45e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=817 studynum=2 | 4.65 | 4.648647145847486 | -0.00135 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=817 studynum=2 | <.0001 | 8.529011651914375e-06 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=2 | -0.40344207 | -0.40344207107316604 | -1.07e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.06466107 | 0.06466107201256856 | 2.01e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=817 studynum=2 | -6.24 | -6.239334711226975 | 0.000665 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=817 studynum=2 | <.0001 | 6.590085752986066e-09 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=2 | 3.67788578 | 3.677885783474767 | 3.47e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=817 studynum=2 | 1.43370903 | 1.4337090308620999 | 8.62e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=2 | 2.57 | 2.5652944246736222 | -0.00471 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.0115 | 0.011519985820559762 | 2e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.49404411 | 0.4940441061568832 | -3.84e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=817 studynum=2 | 1.44767567 | 1.4476756729392815 | 2.94e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.34 | 0.34126711900449574 | 0.00127 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.7335 | 0.7334895516605672 | -1.04e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=2 | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=817 studynum=2 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=2 | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=817 studynum=2 | (blank) | . |  | missing agreement | PASS | both missing |
| INTERCEPT ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 21.28566588 | 21.285665882118614 | 2.12e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 3.27904285 | 3.279042853244372 | 3.24e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| INTERCEPT TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 6.49 | 6.49142656402249 | 0.00143 | displayed (2 dp): ±0.005 | PASS |  |
| INTERCEPT PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | <.0001 | 4.712935016355088e-10 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| BASE ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | -0.38233299 | -0.38233299477187227 | -4.77e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.04400844 | 0.0440084390300158 | -9.7e-10 | displayed (8 dp): ±5e-09 | PASS |  |
| BASE TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | -8.69 | -8.687719973687397 | 0.00228 | displayed (2 dp): ±0.005 | PASS |  |
| BASE PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | <.0001 | 5.439961242185841e-16 |  | bound: 0 ≤ py < 0.0001 | PASS |  |
| TRTPN 54 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 1.94854669 | 1.948546693326398 | 3.33e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 1.03496892 | 1.034968922135782 | 2.14e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 54 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 1.88 | 1.8827103419736884 | 0.00271 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 54 PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.0609 | 0.0609294163854471 | 2.94e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 81 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.66710743 | 0.6671074349617997 | 4.96e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 1.04256192 | 1.0425619229557614 | 2.96e-09 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 81 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.64 | 0.6398732010761405 | -0.000127 | displayed (2 dp): ±0.005 | PASS |  |
| TRTPN 81 PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.5229 | 0.522855387658776 | -4.46e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| TRTPN 0 ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| TRTPN 0 STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |
| TRTPN 0 PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |
| STUDYNUM ESTIMATE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | 0.00000000 | 0 | 0 | displayed (8 dp): ±5e-09 | PASS |  |
| STUDYNUM STDERR | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |
| STUDYNUM TVALUE | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |
| STUDYNUM PROBT | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) | (blank) | . |  | missing agreement | PASS | both missing |

### Merged CSS_STATS.pval against the listing's TRTPN Probt (WPCT-F.07.07.lst): verifies the CSS_STATS cells filled by the merge, not the model rows

Rounded reference only: agreement is shown at the precision displayed in `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst` (`4` decimals; `<.0001` as a bound). Equivalence within the full-precision tolerance `absolute ≤ 1e-08` is not established for these quantities.

PASS 18, FAIL 0, NOT COMPARED 0.

| Quantity | Stratum | SAS value | Python value | Difference | Tolerance | Result | Reason |
|---|---|---|---|---|---|---|---|
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=1 TRTPN=54 | 0.0532 | 0.05318921439386113 | -1.08e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=1 TRTPN=81 | 0.4139 | 0.41393313828451395 | 3.31e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=1 TRTPN=0 | . | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=2 TRTPN=54 | 0.5420 | 0.5419953823039609 | -4.62e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=2 TRTPN=81 | 0.4813 | 0.4812898017449755 | -1.02e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=2 TRTPN=0 | . | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) TRTPN=54 | 0.0636 | 0.06359408020168611 | -5.92e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) TRTPN=81 | 0.2479 | 0.24790315315367645 | 3.15e-06 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=815 studynum=3 (pooled) TRTPN=0 | . | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=1 TRTPN=54 | 0.9993 | 0.9992769775187007 | -2.3e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=1 TRTPN=81 | 0.5603 | 0.5602830264511478 | -1.7e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=1 TRTPN=0 | (blank) | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=2 TRTPN=54 | 0.0115 | 0.011519985820559762 | 2e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=2 TRTPN=81 | 0.7335 | 0.7334895516605672 | -1.04e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=2 TRTPN=0 | (blank) | . |  | missing agreement | PASS | both missing |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) TRTPN=54 | 0.0609 | 0.0609294163854471 | 2.94e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) TRTPN=81 | 0.5229 | 0.522855387658776 | -4.46e-05 | displayed (4 dp): ±5e-05 | PASS |  |
| CSS_STATS.pval | PARAMCD=DIABP ATPTN=817 studynum=3 (pooled) TRTPN=0 | (blank) | . |  | missing agreement | PASS | both missing |

## Not compared

No equivalence is claimed for any quantity listed here: each has no SAS reference value to compare with, or the run did not produce it. Each row gives the reason.

| Program | Comparison | Quantity | Stratum | Reason |
|---|---|---|---|---|
| WPCT-F.07.02 | — | css_anadata: record-level set and categorical fields (STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD AVISIT AVISITN ATPT ATPTN) | all strata | no full-precision SAS export WPCT-F.07.02_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_anadata: record-level numeric values (BASE CHG) | all strata | no full-precision SAS export WPCT-F.07.02_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_anadata: per-stratum record counts (PARAMCD × ATPTN × AVISITN × TRTPN) | all strata | no full-precision SAS export WPCT-F.07.02_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic n | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic mean | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic std | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic median | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic datamin | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic datamax | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic q1 | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: PROC SUMMARY statistic q3 | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: merged pval at full precision | all strata | only rounded listings exist (whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst); no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | css_stats: statistics rows and categorical fields (AVISIT trtp_short) | all strata | no full-precision SAS export WPCT-F.07.02_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.02 | — | parameter_estimates: full-precision GLM values (ESTIMATE STDERR TVALUE PROBT) | all strata | only rounded listings exist (whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst); no full-precision SAS export WPCT-F.07.02_parameter_estimates.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | advs_min: record-level set and categorical fields (AVISIT STUDYID USUBJID TRTPN PARAMCD ATPTN AVISITN trtp_short SAFFL PARAM ATPT ANL02FL) | all strata | no full-precision SAS export WPCT-F.07.07_advs_min.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | advs_min: record-level numeric values (AVAL ADT BASE CHG) | all strata | no full-precision SAS export WPCT-F.07.07_advs_min.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | advs_min: per-stratum record counts (PARAMCD × ATPTN × STUDYID × TRTPN × AVISITN) | all strata | no full-precision SAS export WPCT-F.07.07_advs_min.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_anadata: record-level set and categorical fields (STUDYID USUBJID SAFFL ANL02FL trtp_short TRTPN PARAM PARAMCD ATPT ATPTN studynum) | all strata | no full-precision SAS export WPCT-F.07.07_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_anadata: record-level numeric values (BASE CHG) | all strata | no full-precision SAS export WPCT-F.07.07_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_anadata: per-stratum record counts (PARAMCD × ATPTN × STUDYID × TRTPN) | all strata | no full-precision SAS export WPCT-F.07.07_css_anadata.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic n | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic mean | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic std | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic median | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic datamin | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic datamax | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic q1 | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: PROC SUMMARY statistic q3 | all strata | exists only as PDF raster images (whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_*.pdf); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: merged pval at full precision | all strata | only rounded listings exist (whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst); no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | css_stats: statistics rows and categorical fields (STUDYID trtp_short) | all strata | no full-precision SAS export WPCT-F.07.07_css_stats.csv supplied (see python/wpct/reference/sas_exports/README.md) |
| WPCT-F.07.07 | — | parameter_estimates: full-precision GLM values (ESTIMATE STDERR TVALUE PROBT) | all strata | only rounded listings exist (whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst); no full-precision SAS export WPCT-F.07.07_parameter_estimates.csv supplied (see python/wpct/reference/sas_exports/README.md) |

## Source integrity

Each read-only input's working-tree blob (`git hash-object --no-filters`) against its blob at the upstream commit `398a6d33ced9359ffb58c46650a6d488811176b1`: all 36 read-only inputs match their blobs at the upstream commit.

| Path | Blob at upstream commit | Working-tree blob | Result |
|---|---|---|---|
| `whitepapers/WPCT/WPCT-F.07.02.sas` | `69c31dd1774e0c16ec63b28ca1b885c760d98e21` | `69c31dd1774e0c16ec63b28ca1b885c760d98e21` | unchanged |
| `whitepapers/WPCT/WPCT-F.07.07.sas` | `810e50cb57fd1bbc815a20eb5b692824440aeb14` | `810e50cb57fd1bbc815a20eb5b692824440aeb14` | unchanged |
| `whitepapers/WPCT/WPCT-F.07.02_sas.yml` | `6996a813c9651024a34df30d2e2b28664a62dcba` | `6996a813c9651024a34df30d2e2b28664a62dcba` | unchanged |
| `whitepapers/WPCT/WPCT-F.07.07_sas.yml` | `93688344ee1bdc88bd04d154fe914c4d1449787a` | `93688344ee1bdc88bd04d154fe914c4d1449787a` | unchanged |
| `whitepapers/utilities/assert_depend.sas` | `f76aaa348dce4d946a4b56d4b5eabfa3c71d0c73` | `f76aaa348dce4d946a4b56d4b5eabfa3c71d0c73` | unchanged |
| `whitepapers/utilities/assert_continue.sas` | `37f847299902d12298af046b3a78e92c90e1fff0` | `37f847299902d12298af046b3a78e92c90e1fff0` | unchanged |
| `whitepapers/utilities/assert_var_nonmissing.sas` | `5ba85dd067f98a1287d02da8c2ccff5769c2db16` | `5ba85dd067f98a1287d02da8c2ccff5769c2db16` | unchanged |
| `whitepapers/utilities/assert_unique_keys.sas` | `7fcddb4c95fa8c55c3341480bc060d21b4693de7` | `7fcddb4c95fa8c55c3341480bc060d21b4693de7` | unchanged |
| `whitepapers/utilities/assert_dset_exist.sas` | `3af4cf73b1f10fdf7071141157aa3f346df585e1` | `3af4cf73b1f10fdf7071141157aa3f346df585e1` | unchanged |
| `whitepapers/utilities/assert_var_exist.sas` | `850da6de32502ec00216b16bcd5bce1572261af5` | `850da6de32502ec00216b16bcd5bce1572261af5` | unchanged |
| `whitepapers/utilities/assert_macro_exist.sas` | `d80f8d2250c047a95451a18d5d8d9f09a6e503b2` | `d80f8d2250c047a95451a18d5d8d9f09a6e503b2` | unchanged |
| `whitepapers/utilities/util_resolve_sasautos.sas` | `774e8cfd2059f7d8f3a1837c1fd3d6040c1eb019` | `774e8cfd2059f7d8f3a1837c1fd3d6040c1eb019` | unchanged |
| `whitepapers/utilities/util_labels_from_var.sas` | `3be7bfd2aaa550eaf44350dad07009f6a807f116` | `3be7bfd2aaa550eaf44350dad07009f6a807f116` | unchanged |
| `whitepapers/utilities/util_count_unique_values.sas` | `2a8dc8fe3d798bc6d8449894ac6247a2774ff5ed` | `2a8dc8fe3d798bc6d8449894ac6247a2774ff5ed` | unchanged |
| `whitepapers/utilities/util_get_var_min_max.sas` | `7ed49145a3711b7dd0b601829bb20d8db6ee1566` | `7ed49145a3711b7dd0b601829bb20d8db6ee1566` | unchanged |
| `whitepapers/utilities/util_value_format.sas` | `086c56b335c64962556ff6cf992c1b1c353c3ddc` | `086c56b335c64962556ff6cf992c1b1c353c3ddc` | unchanged |
| `whitepapers/utilities/util_delete_dsets.sas` | `06d80cf94e06873b86be66977ffd6d1d878f2453` | `06d80cf94e06873b86be66977ffd6d1d878f2453` | unchanged |
| `whitepapers/utilities/util_proc_template.sas` | `52d904240447a71ea52a859bcc50cc70e448f497` | `52d904240447a71ea52a859bcc50cc70e448f497` | unchanged |
| `whitepapers/utilities/util_boxplot_block_ranges.sas` | `3c30fbe85a43a6ee65c6d62b51bd714b60c57e9f` | `3c30fbe85a43a6ee65c6d62b51bd714b60c57e9f` | unchanged |
| `whitepapers/utilities/util_axis_order.sas` | `c17feb5eb4003547cda37d496a54fd4ab7168551` | `c17feb5eb4003547cda37d496a54fd4ab7168551` | unchanged |
| `whitepapers/utilities/util_access_test_data.sas` | `3915e4cdf47cb4d297b8a4240c867d085b7d4b62` | `3915e4cdf47cb4d297b8a4240c867d085b7d4b62` | unchanged |
| `whitepapers/ADaM/derive_lastminmax_measure.sas` | `df9d97227cfd917476a11c60be841446ff06558a` | `df9d97227cfd917476a11c60be841446ff06558a` | unchanged |
| `data/adam/cdisc/advs.xpt` | `f987dbf3c7986c1a0c4a469b5679480b9487bfb9` | `f987dbf3c7986c1a0c4a469b5679480b9487bfb9` | unchanged |
| `data/adam/cdisc-split/advs.xpt` | `35b267e35e6ec77004287ee7f68b7548d0c21448` | `35b267e35e6ec77004287ee7f68b7548d0c21448` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst` | `13cf894a674a17bb6679722ec755a0350410f60c` | `13cf894a674a17bb6679722ec755a0350410f60c` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log` | `16bee5ad13bf13d1083242b71e3b2ffc1e5a40af` | `16bee5ad13bf13d1083242b71e3b2ffc1e5a40af` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst` | `81b0f03d991a4aa2029c73debcb6ec6cd3898d1b` | `81b0f03d991a4aa2029c73debcb6ec6cd3898d1b` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log` | `636aa3cbd2198925d488c7dbc9ebf04da7900128` | `636aa3cbd2198925d488c7dbc9ebf04da7900128` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf` | `b91ba6f4cfcc31832ed3540870cee1e85a38ddd4` | `b91ba6f4cfcc31832ed3540870cee1e85a38ddd4` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.02_Box_plot_SYSBP_Change_by_visit_for_timepoint_816.pdf` | `ade0393b7aee8c1e8ab58dca5fb8d42d45f2b2d7` | `ade0393b7aee8c1e8ab58dca5fb8d42d45f2b2d7` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_815.pdf` | `080af66eb4d33347e1a709ccb4ae72eea62a66ef` | `080af66eb4d33347e1a709ccb4ae72eea62a66ef` | unchanged |
| `whitepapers/WPCT/outputs_sas/WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_817.pdf` | `df0dddc3d955d5358038e86e1298d10ad2be41d4` | `df0dddc3d955d5358038e86e1298d10ad2be41d4` | unchanged |
| `LICENSE.md` | `5cb7c689b1663d31fc52bd10e7692890214303d1` | `5cb7c689b1663d31fc52bd10e7692890214303d1` | unchanged |
| `data/adam/README.md` | `97a04d8ea25ff0280c234a275063dc4a92738153` | `97a04d8ea25ff0280c234a275063dc4a92738153` | unchanged |
| `data/adam/cdisc/README.md` | `63162ecee86340ef172fa9b7f6d184be8af675b4` | `63162ecee86340ef172fa9b7f6d184be8af675b4` | unchanged |
| `data/adam/cdisc-split/README.md` | `94707430197d424b239d92323aa8891f7d079bc7` | `94707430197d424b239d92323aa8891f7d079bc7` | unchanged |
