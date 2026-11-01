"""Tests of ``wpct.model.glm_solution``, the PROC GLM ``CLASS ... (REF=) / SOLUTION`` port.

Items verified (``docs/sas_specification.md``): the model steps S02-MOD-02, S07-MOD-02 and
S07-MOD-03, through the default differences D-05 (less-than-full-rank design, one SOLUTION row per
column), D-06 (sequential aliasing rather than the ``pinv`` minimum-norm solution), D-07 (CLASS
levels ascending with the ``REF=`` level last), D-08 (complete-case exclusion over the current MODEL
variables only) and D-09 (zero error degrees of freedom), and the port decision for the unverified
behaviour U-02 (reference level absent).

Every frame is synthetic and built here, and nothing is written. Agreement with the SAS listings on
the repository data is verified by the driver tests.
"""

from __future__ import annotations

from collections.abc import Callable

import numpy as np
import pandas as pd
import pytest
from scipy import stats

from wpct import compare, model
from wpct.sas_semantics import format_level
from wpct.schemas import PARAMETER_ESTIMATE_COLUMNS

RESPONSE = "CHG"
CLASS_VAR = "TRTPN"
REF_LEVEL = 0
#: ``MODEL chg = base trtpn`` of the 07.02 and per-study 07.07 fits (S02-MOD-02, S07-MOD-02).
TERMS = ("BASE", "TRTPN")
#: ``MODEL chg = base trtpn studynum`` of the pooled 07.07 fit (S07-MOD-03).
POOLED_TERMS = ("BASE", "TRTPN", "studynum")
#: Treatment codes in the order the synthetic records carry them, deliberately not ascending.
TREATMENT_CYCLE = (81.0, 0.0, 54.0)
N_RECORDS = 30
#: Records per treatment level in :func:`_analysis_frame`.
N_PER_LEVEL = N_RECORDS // len(TREATMENT_CYCLE)
#: Columns that an aliased or zero-DFE row leaves missing (D-06, D-09).
INFERENCE_COLUMNS = ("STDERR", "TVALUE", "PROBT")


def _analysis_frame() -> pd.DataFrame:
    """Return a deterministic endpoint-like analysis frame of :data:`N_RECORDS` records.

    ``TRTPN`` cycles 81, 0, 54, so the levels arrive out of order. ``BASE`` is continuous, and
    ``CHG`` is linear in ``BASE`` and treatment plus a fixed, non-random disturbance. ``studynum``
    is the constant 3, as on the pooled 07.07 records, which all share one study number (DIV-01).
    """
    index = np.arange(N_RECORDS, dtype="float64")
    trtpn = np.resize(np.array(TREATMENT_CYCLE), N_RECORDS)
    base = 72.0 + 0.85 * index + 6.0 * np.cos(1.7 * index)
    treatment_effect = np.select([trtpn == 54.0, trtpn == 81.0], [2.5, 1.25], default=0.0)
    chg = 15.0 - 0.3 * base + treatment_effect + 3.0 * np.sin(2.3 * index + 0.4)
    return pd.DataFrame(
        {
            "USUBJID": [f"01-701-{1001 + record}" for record in range(N_RECORDS)],
            "TRTPN": trtpn,
            "BASE": base,
            "CHG": chg,
            "studynum": 3.0,
        }
    )


def _fit(
    frame: pd.DataFrame, terms: tuple[str, ...] = TERMS, ref_level: object = REF_LEVEL
) -> tuple[pd.DataFrame, model.GlmFitRecord]:
    """Return ``glm_solution`` of ``CHG`` on ``terms`` with ``CLASS TRTPN (REF=ref_level)``."""
    return model.glm_solution(frame, RESPONSE, terms, CLASS_VAR, ref_level)


def _full_rank_ols(frame: pd.DataFrame) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Solve ``CHG`` on the full-rank design ``[1, BASE, I(TRTPN=54), I(TRTPN=81)]`` independently.

    Omitting the aliased reference indicator gives the reparameterisation whose estimates GLM
    reports for the retained columns (D-05, D-07). Least squares gives the coefficients,
    ``s² (X'X)⁻¹`` their covariance, and the p value is the two-sided Student t probability on
    ``n - 4`` degrees of freedom.
    """
    trtpn = frame["TRTPN"].to_numpy(dtype="float64")
    design = np.column_stack(
        [
            np.ones(len(frame)),
            frame["BASE"].to_numpy(dtype="float64"),
            (trtpn == 54.0).astype("float64"),
            (trtpn == 81.0).astype("float64"),
        ]
    )
    response = frame["CHG"].to_numpy(dtype="float64")
    estimate, _, rank, _ = np.linalg.lstsq(design, response, rcond=None)
    assert rank == 4
    dfe = len(response) - 4
    residual = response - design @ estimate
    variance = residual @ residual / dfe
    stderr = np.sqrt(np.diag(variance * np.linalg.inv(design.T @ design)))
    tvalue = estimate / stderr
    probt = 2.0 * stats.t.sf(np.abs(tvalue), dfe)
    return estimate, stderr, tvalue, probt


def _rule_class(column: str) -> str:
    """The one tolerance class whose ``compare.compared_fields()`` (``wpct.schemas``) name
    ``column``, matched case-insensitively as SAS names are."""
    owners = [
        key
        for key, fields in compare.compared_fields().items()
        if column.upper() in {field.upper() for field in fields}
    ]
    assert len(owners) == 1, (column, owners)
    return owners[0]


def _assert_finite_close(actual, expected, column: str, tolerances: compare.Tolerances) -> None:
    """Assert both value sets are finite and agree, value by value through
    ``compare.check_value``, under the limits ``tolerances.json`` sets for the class
    ``compare.compared_fields()`` assigns ``column``; a missing value never passes."""
    actual = np.asarray(actual, dtype="float64")
    expected = np.asarray(expected, dtype="float64")
    assert actual.shape == expected.shape, (column, actual.shape, expected.shape)
    assert np.isfinite(actual).all(), (column, actual)
    assert np.isfinite(expected).all(), (column, expected)
    class_key = _rule_class(column)
    for position, (py, reference) in enumerate(zip(actual.ravel(), expected.ravel(), strict=True)):
        outcome = compare.check_value(class_key, py, reference, tolerances)
        assert outcome.status == compare.PASS, (column, position, py, reference, outcome)


def _assert_aliased(row: pd.Series) -> None:
    """Assert a row is aliased as GLM reports it: estimate 0, flag ``B``, no SE, t or p (D-06)."""
    assert row["ALIASED"], row
    assert row["ESTIMATE"] == 0.0, row
    for column in INFERENCE_COLUMNS:
        assert np.isnan(row[column]), (column, row)


def _assert_same_solution(
    actual: pd.DataFrame, expected: pd.DataFrame, tolerances: compare.Tolerances
) -> None:
    """Assert two ParameterEstimates tables of fits with ``DFE > 0`` describe the same solution.

    Labels and aliasing flags must be identical. Every estimate must be finite and agree under the
    ``glm_estimates`` rule of ``tolerances.json``; the standard error, t and p must be missing
    exactly on the aliased rows and agree on the others under the class that
    ``compare.compared_fields()`` assigns them (``glm_estimates`` for SE and t, ``p_values`` for p).
    """
    assert list(actual.columns) == list(PARAMETER_ESTIMATE_COLUMNS)
    labels = ["EFFECT", "LEVEL", "PARAMETER", "ALIASED"]
    assert actual[labels].to_dict("records") == expected[labels].to_dict("records")
    _assert_finite_close(actual["ESTIMATE"], expected["ESTIMATE"], "ESTIMATE", tolerances)
    aliased = expected["ALIASED"].to_numpy(dtype=bool)
    for column in INFERENCE_COLUMNS:
        for table in (actual, expected):
            np.testing.assert_array_equal(table[column].isna().to_numpy(), aliased, err_msg=column)
        _assert_finite_close(
            actual.loc[~aliased, column], expected.loc[~aliased, column], column, tolerances
        )


def _without_records(frame: pd.DataFrame, mask: pd.Series) -> pd.DataFrame:
    """Drop the records selected by ``mask``."""
    return frame.loc[~mask]


def _with_missing_chg(frame: pd.DataFrame, mask: pd.Series) -> pd.DataFrame:
    """Keep every record but set ``CHG`` missing on those selected by ``mask`` (D-08)."""
    altered = frame.copy()
    altered.loc[mask, RESPONSE] = np.nan
    return altered


#: The two ways a TRTPN level can lack complete records: none at all, or only incomplete ones.
LEVEL_ABSENCE = pytest.mark.parametrize(
    "make_absent", [_without_records, _with_missing_chg], ids=["no-records", "incomplete-records"]
)


def test_s02_mod_02_matches_full_rank_ols(tolerances: compare.Tolerances) -> None:
    """S02-MOD-02, D-05: the retained columns reproduce an independent full-rank least-squares fit.

    Estimates, standard errors and t values agree under the ``glm_estimates`` rule of
    ``tolerances.json`` and p values under its ``p_values`` rule; ``DFE = n - rank``, with rank 4
    once ``TRTPN 0`` is aliased.
    """
    frame = _analysis_frame()
    rows, fit = _fit(frame)
    estimate, stderr, tvalue, probt = _full_rank_ols(frame)

    assert isinstance(fit, model.GlmFitRecord)
    assert fit == model.GlmFitRecord(
        NOBS=N_RECORDS, DFE=N_RECORDS - 4, RANK=4, STATUS=model.STATUS_FITTED
    )
    assert list(rows.columns) == list(PARAMETER_ESTIMATE_COLUMNS)
    assert rows["PARAMETER"].tolist() == ["Intercept", "BASE", "TRTPN 54", "TRTPN 81", "TRTPN 0"]
    retained = rows.iloc[:4]
    assert not retained["ALIASED"].any()
    for column, independent in zip(
        ("ESTIMATE", "STDERR", "TVALUE", "PROBT"), (estimate, stderr, tvalue, probt), strict=True
    ):
        _assert_finite_close(retained[column], independent, column, tolerances)
    _assert_aliased(rows.iloc[4])


@pytest.mark.parametrize(
    ("ref_level", "level_order", "trtpn_parameters"),
    [
        (0, (54, 81, 0), ("TRTPN 54", "TRTPN 81", "TRTPN 0")),
        (54, (0, 81, 54), ("TRTPN 0", "TRTPN 81", "TRTPN 54")),
    ],
    ids=["ref-0", "ref-54"],
)
def test_d05_07_column_order_model_order_reference_last(
    ref_level: int,
    level_order: tuple[int, ...],
    trtpn_parameters: tuple[str, ...],
    tolerances: compare.Tolerances,
) -> None:
    """D-05, D-07: rows follow MODEL order, CLASS levels ascend and the ``REF=`` level comes last.

    The records carry TRTPN as 81, 0, 54, so the order is the port's, not the input's. The reference
    indicator depends on the Intercept and its siblings and is reported aliased (D-06). The BASE
    slope does not depend on which level is the reference.
    """
    rows, fit = _fit(_analysis_frame(), ref_level=ref_level)

    expected = [("INTERCEPT", "", "Intercept"), ("BASE", "", "BASE")] + [
        ("TRTPN", format_level(level), parameter)
        for level, parameter in zip(level_order, trtpn_parameters)
    ]
    assert list(rows[["EFFECT", "LEVEL", "PARAMETER"]].itertuples(index=False, name=None)) == (
        expected
    )
    assert rows["ALIASED"].tolist() == [False, False, False, False, True]
    _assert_aliased(rows.iloc[-1])
    retained_values = rows.iloc[:-1][["ESTIMATE", *INFERENCE_COLUMNS]].to_numpy(dtype="float64")
    assert np.isfinite(retained_values).all()
    assert (fit.RANK, fit.DFE) == (4, N_RECORDS - 4)

    reference_rows, _ = _fit(_analysis_frame(), ref_level=REF_LEVEL)
    base = rows["EFFECT"] == "BASE"
    for column in ("ESTIMATE", "STDERR"):
        _assert_finite_close(
            rows.loc[base, column], reference_rows.loc[base, column], column, tolerances
        )


def test_s07_mod_03_constant_column_aliased(tolerances: compare.Tolerances) -> None:
    """S07-MOD-03, D-06, DIV-01: the constant pooled ``studynum`` is aliased, not estimated.

    As in [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:47], ``studynum`` gets estimate 0 with
    missing standard error, t and p, and every other row equals the fit without ``studynum``. The
    minimum-norm solution that ``pinv`` would return instead spreads the Intercept over
    ``studynum``; the contrast shows that this data would expose it, since that solution fails the
    ``glm_estimates`` rule of ``tolerances.json`` against the port's.
    """
    frame = _analysis_frame()
    pooled_rows, pooled_fit = _fit(frame, POOLED_TERMS)
    rows, fit = _fit(frame)

    assert len(pooled_rows) == len(rows) + 1
    last = pooled_rows.iloc[-1]
    assert (last["EFFECT"], last["LEVEL"], last["PARAMETER"]) == ("STUDYNUM", "", "studynum")
    _assert_aliased(last)
    _assert_same_solution(pooled_rows.iloc[:-1], rows, tolerances)
    assert pooled_fit == fit

    trtpn = frame["TRTPN"].to_numpy(dtype="float64")
    full_design = np.column_stack(
        [
            np.ones(N_RECORDS),
            frame["BASE"].to_numpy(dtype="float64"),
            *((trtpn == level).astype("float64") for level in (54.0, 81.0, 0.0)),
            frame["studynum"].to_numpy(dtype="float64"),
        ]
    )
    response = frame["CHG"].to_numpy(dtype="float64")
    minimum_norm = np.linalg.lstsq(full_design, response, rcond=None)[0]
    # The design columns follow the pooled rows, Intercept first and studynum last. The port's
    # studynum estimate is 0, which only an exact 0 matches, and the Intercepts differ.
    estimate_rule = _rule_class("ESTIMATE")
    port_estimates = pooled_rows["ESTIMATE"].to_numpy(dtype="float64")
    for position in (-1, 0):
        outcome = compare.check_value(
            estimate_rule, minimum_norm[position], port_estimates[position], tolerances
        )
        assert outcome.status == compare.FAIL, (pooled_rows["PARAMETER"].iat[position], outcome)


def _assert_incomplete_record_dropped(column: str, tolerances: compare.Tolerances) -> None:
    """Assert a record missing ``column`` is excluded: the fit equals the fit without it (D-08)."""
    frame = _analysis_frame()
    frame.loc[7, column] = np.nan
    rows, fit = _fit(frame)
    expected_rows, expected_fit = _fit(frame.drop(index=7))

    assert fit.NOBS == N_RECORDS - 1
    assert fit == expected_fit
    _assert_same_solution(rows, expected_rows, tolerances)


def test_missing_base_dropped(tolerances: compare.Tolerances) -> None:
    """D-08: a record with missing BASE is left out of S02-MOD-02 / S07-MOD-02 fits."""
    _assert_incomplete_record_dropped("BASE", tolerances)


def test_missing_chg_dropped(tolerances: compare.Tolerances) -> None:
    """D-08: a record with a missing response CHG is left out of the fit."""
    _assert_incomplete_record_dropped("CHG", tolerances)


def test_fit_without_studynum_column(tolerances: compare.Tolerances) -> None:
    """D-08: complete cases are judged over the response and the current terms only.

    A frame without ``studynum`` fits ``CHG = BASE TRTPN`` (S02-MOD-02, S07-MOD-02), and missing
    ``studynum`` values remove records only from a fit that has ``studynum`` as a term (S07-MOD-03).
    """
    frame = _analysis_frame()
    rows, fit = _fit(frame.drop(columns="studynum"))
    assert fit == model.GlmFitRecord(
        NOBS=N_RECORDS, DFE=N_RECORDS - 4, RANK=4, STATUS=model.STATUS_FITTED
    )

    partly_missing = frame.copy()
    partly_missing.loc[[2, 11, 25], "studynum"] = np.nan
    unaffected_rows, unaffected_fit = _fit(partly_missing)
    assert unaffected_fit == fit
    _assert_same_solution(unaffected_rows, rows, tolerances)

    _, pooled_fit = _fit(partly_missing, POOLED_TERMS)
    assert pooled_fit.NOBS == N_RECORDS - 3


@LEVEL_ABSENCE
def test_absent_level(make_absent: Callable[[pd.DataFrame, pd.Series], pd.DataFrame]) -> None:
    """D-05, D-07: a TRTPN level without complete records has no design column and no row."""
    frame = _analysis_frame()
    rows, fit = _fit(make_absent(frame, frame["TRTPN"] == 81.0))

    assert rows["PARAMETER"].tolist() == ["Intercept", "BASE", "TRTPN 54", "TRTPN 0"]
    assert rows["ALIASED"].tolist() == [False, False, False, True]
    _assert_aliased(rows.iloc[-1])
    remaining = 2 * N_PER_LEVEL
    assert fit == model.GlmFitRecord(
        NOBS=remaining, DFE=remaining - 3, RANK=3, STATUS=model.STATUS_FITTED
    )


# model.py suppresses the RuntimeWarning statsmodels raises for the 0/0 residual variance, so any
# RuntimeWarning escaping the zero-DFE fit is an error.
@pytest.mark.filterwarnings("error::RuntimeWarning")
def test_zero_dfe(tolerances: compare.Tolerances) -> None:
    """D-09: with as many retained columns as records, estimates stand and SE, t and p are missing.

    The three records solve ``CHG = -1 + 2 BASE + 1 I(TRTPN=54)`` exactly; ``TRTPN 0`` is aliased.
    """
    frame = pd.DataFrame(
        {"BASE": [1.0, 2.0, 1.0], "TRTPN": [0.0, 0.0, 54.0], "CHG": [1.0, 3.0, 2.0]}
    )
    rows, fit = _fit(frame)

    assert fit == model.GlmFitRecord(NOBS=3, DFE=0, RANK=3, STATUS=model.STATUS_FITTED)
    assert rows["PARAMETER"].tolist() == ["Intercept", "BASE", "TRTPN 54", "TRTPN 0"]
    assert rows["ALIASED"].tolist() == [False, False, False, True]
    _assert_finite_close(rows["ESTIMATE"], [-1.0, 2.0, 1.0, 0.0], "ESTIMATE", tolerances)
    for column in INFERENCE_COLUMNS:
        assert rows[column].isna().all(), (column, rows[column].tolist())


def test_no_observations() -> None:
    """D-08, D-09: when no record is complete nothing is fitted and no parameter row is emitted."""
    frame = _analysis_frame()
    frame[RESPONSE] = np.nan
    rows, fit = _fit(frame)

    assert rows.empty
    assert list(rows.columns) == list(PARAMETER_ESTIMATE_COLUMNS)
    assert model.STATUS_NO_OBSERVATIONS == "no observations"
    assert fit == model.GlmFitRecord(NOBS=0, DFE=None, RANK=0, STATUS=model.STATUS_NO_OBSERVATIONS)


@LEVEL_ABSENCE
def test_reference_absent(make_absent: Callable[[pd.DataFrame, pd.Series], pd.DataFrame]) -> None:
    """U-02: without complete ``REF=`` level records nothing is fitted and no row is emitted.

    SAS behaviour for this case is not established, so the port reports the stratum's status
    instead of choosing another reference (S02-MOD-02, S07-MOD-02, S07-MOD-03).
    """
    frame = _analysis_frame()
    rows, fit = _fit(make_absent(frame, frame["TRTPN"] == 0.0))

    assert rows.empty
    assert list(rows.columns) == list(PARAMETER_ESTIMATE_COLUMNS)
    assert model.status_reference_absent(REF_LEVEL) == "not fitted: reference level 0 absent"
    assert fit == model.GlmFitRecord(
        NOBS=2 * N_PER_LEVEL, DFE=None, RANK=0, STATUS=model.status_reference_absent(REF_LEVEL)
    )
