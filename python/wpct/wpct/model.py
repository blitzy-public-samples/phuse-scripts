"""PROC GLM ``CLASS ... (REF=) / SOLUTION`` port, shared by both drivers.

One implementation serves the three model steps of ``docs/sas_specification.md``:

- S02-MOD-02, the 07.02 endpoint ANCOVA ``MODEL chg = base trtpn / solution`` with
  ``CLASS trtpn (ref="0")`` [whitepapers/WPCT/WPCT-F.07.02.sas:267-280];
- S07-MOD-02, the same model BY ``studynum`` over the individual studies
  [whitepapers/WPCT/WPCT-F.07.07.sas:338-352];
- S07-MOD-03, the pooled model ``MODEL chg = base trtpn studynum / solution``, where ``studynum`` is
  not a CLASS variable [whitepapers/WPCT/WPCT-F.07.07.sas:355-363].

Each fit returns the ODS ``ParameterEstimates`` table as SAS builds it, following SAS wherever the
library defaults differ (Part C of the specification):

- D-05: the design is less than full rank. It holds the Intercept, then each MODEL term in order, a
  CLASS term contributing one indicator per level, and a row is emitted for every design column.
- D-06: a column linearly dependent on the columns before it is aliased, as GLM's modified sweep
  does: estimate 0, missing standard error, t and p. statsmodels' default ``pinv`` would return the
  minimum-norm solution instead, spreading the Intercept over the constant pooled ``studynum``.
- D-07: CLASS levels ascend by value and the ``REF=`` level is moved last, so its indicator is the
  one aliased by the Intercept and its siblings.
- D-08: complete-case exclusion over the response and the terms of the current fit only.
- D-09: with zero error degrees of freedom the estimates stand and the standard error, t and p are
  missing.

The design is built as an explicit numpy matrix and handed to ``statsmodels.api.OLS``. patsy and
formulaic formulas are never used, because their treatment coding drops the reference column and
orders it first, which would change the SAS parameterisation.

DIV-01: the pooled records share one ``STUDYID``, so the pooled ``studynum`` is constant and aliased
by the Intercept. The port reproduces that, and the pooled treatment estimates equal those of
``CHG = BASE + TRTPN`` on the pooled records.

U-02: SAS behaviour when the reference level is absent from a model's data is not established. The
port then fits nothing and reports :func:`status_reference_absent`.
"""

import warnings
from collections.abc import Sequence
from dataclasses import dataclass

import numpy as np
import pandas as pd
import statsmodels.api as sm

from wpct.sas_semantics import format_level
from wpct.schemas import PARAMETER_ESTIMATE_COLUMNS

__all__ = [
    "ALIAS_TOLERANCE",
    "STATUS_FITTED",
    "STATUS_NO_OBSERVATIONS",
    "GlmFitRecord",
    "glm_solution",
    "status_reference_absent",
]

#: Relative residual norm at or below which a design column counts as linearly dependent on the
#: retained columns before it (D-06). Exact dependencies leave residuals near 1e-15 of the column
#: norm, and real columns stay many orders of magnitude above this threshold.
ALIAS_TOLERANCE = 1e-9

#: Model status of a completed fit.
STATUS_FITTED = "fitted"
#: Model status when no record survives the complete-case exclusion (D-08, D-09).
STATUS_NO_OBSERVATIONS = "no observations"

_INTERCEPT_EFFECT = "INTERCEPT"
_INTERCEPT_PARAMETER = "Intercept"

_ESTIMATE_DTYPES = {
    "EFFECT": object,
    "LEVEL": object,
    "PARAMETER": object,
    "ESTIMATE": "float64",
    "STDERR": "float64",
    "TVALUE": "float64",
    "PROBT": "float64",
    "ALIASED": bool,
}


def status_reference_absent(ref_level: object) -> str:
    """Return the status of a model whose ``REF=`` level has no complete record (U-02).

    ``status_reference_absent(0)`` is ``"not fitted: reference level 0 absent"``.
    """
    return f"not fitted: reference level {format_level(ref_level)} absent"


@dataclass(frozen=True)
class GlmFitRecord:
    """Model-level facts of one :func:`glm_solution` call.

    ``NOBS`` counts the records used after complete-case exclusion (D-08). ``RANK`` is the number of
    retained design columns and ``DFE = NOBS - RANK`` the error degrees of freedom (D-05); ``DFE`` is
    ``None`` and ``RANK`` 0 when nothing was fitted. ``STATUS`` is :data:`STATUS_FITTED`,
    :data:`STATUS_NO_OBSERVATIONS` or :func:`status_reference_absent`.
    """

    NOBS: int
    DFE: int | None
    RANK: int
    STATUS: str


@dataclass(frozen=True, eq=False)
class _DesignColumn:
    """One column of the less-than-full-rank design and its ParameterEstimates labels (D-05)."""

    effect: str
    level: str
    parameter: str
    values: np.ndarray


def glm_solution(
    df: pd.DataFrame,
    response: str,
    terms: Sequence[str],
    class_var: str,
    ref_level: object,
) -> tuple[pd.DataFrame, GlmFitRecord]:
    """Fit ``MODEL response = terms / SOLUTION`` with ``CLASS class_var (REF=ref_level)``.

    Ports S02-MOD-02, S07-MOD-02 and S07-MOD-03. ``terms`` lists the MODEL effects in MODEL order,
    for example ``("BASE", "TRTPN")`` or, for the pooled 07.07 model, ``("BASE", "TRTPN",
    "studynum")``. ``class_var`` must be one of ``terms``; every other term is continuous. Column
    names are matched exactly. The caller applies the WHERE and BY selection before the call.

    - D-08: a record missing ``response`` or any of ``terms`` is excluded. No other column is
      consulted, so a frame without ``studynum`` fits normally when ``studynum`` is not a term.
    - D-07: the CLASS levels are the values of ``class_var`` among the complete records, ascending,
      with ``ref_level`` moved last. A level without records has no column.
    - D-05: the design columns are the Intercept, then each term in order, ``class_var`` expanding
      into one 0/1 indicator per level.
    - D-06: walking the columns in order, a column whose least-squares residual on the retained
      earlier columns has norm at most :data:`ALIAS_TOLERANCE` times its own norm is aliased, as is
      an all-zero column. The reference indicator is aliased by the Intercept and its siblings, and
      a constant continuous term by the Intercept (DIV-01).
    - The retained columns are fitted with ``statsmodels.api.OLS(y, X).fit()``. ``RANK`` is their
      number, ``DFE = NOBS - RANK``, and PROBT is the two-sided t probability on ``DFE``.
    - D-09: when ``DFE`` is 0, STDERR, TVALUE and PROBT are NaN.

    Returns the ParameterEstimates rows with the columns of
    :data:`wpct.schemas.PARAMETER_ESTIMATE_COLUMNS`, one per design column in design order. An
    aliased column has ``ESTIMATE`` 0.0, NaN ``STDERR``, ``TVALUE`` and ``PROBT``, and ``ALIASED``
    true. ``EFFECT`` is ``INTERCEPT`` or the upper-cased term, ``LEVEL`` the CLASS level label
    (``54``) or ``''``, and ``PARAMETER`` the SAS label: ``Intercept``, the term as passed, or
    ``TRTPN 54``.

    No rows are returned, and nothing is fitted, when no record is complete
    (:data:`STATUS_NO_OBSERVATIONS`) or when ``ref_level`` has no complete record (U-02,
    :func:`status_reference_absent`).

    Raises ``TypeError`` when ``terms`` is a single string or a model variable names several
    columns or holds character values, ``ValueError`` when ``class_var`` is not one of ``terms``,
    and ``KeyError`` when a model variable is absent from ``df``.
    """
    model_terms = _check_terms(terms, class_var)
    columns = _numeric_columns(df, (response, *model_terms))

    complete = np.ones(len(df), dtype=bool)
    for values in columns.values():
        complete &= ~np.isnan(values)
    nobs = int(complete.sum())
    if nobs == 0:
        return _estimates_frame([]), GlmFitRecord(
            NOBS=0, DFE=None, RANK=0, STATUS=STATUS_NO_OBSERVATIONS
        )
    data = {name: values[complete] for name, values in columns.items()}

    levels = _class_levels(data[class_var], ref_level)
    if levels is None:
        return _estimates_frame([]), GlmFitRecord(
            NOBS=nobs, DFE=None, RANK=0, STATUS=status_reference_absent(ref_level)
        )

    design = _design_columns(data, model_terms, class_var, levels, nobs)
    retained = _retained_columns(design)
    rank = len(retained)
    dfe = nobs - rank
    exog = np.column_stack([design[index].values for index in retained])
    estimate, stderr, tvalue, probt = _fit(data[response], exog, dfe)

    position = {index: fitted for fitted, index in enumerate(retained)}
    rows = []
    for index, column in enumerate(design):
        fitted = position.get(index)
        if fitted is None:
            rows.append(
                (column.effect, column.level, column.parameter, 0.0, np.nan, np.nan, np.nan, True)
            )
        else:
            rows.append(
                (
                    column.effect,
                    column.level,
                    column.parameter,
                    estimate[fitted],
                    stderr[fitted],
                    tvalue[fitted],
                    probt[fitted],
                    False,
                )
            )
    return _estimates_frame(rows), GlmFitRecord(NOBS=nobs, DFE=dfe, RANK=rank, STATUS=STATUS_FITTED)


def _check_terms(terms: Sequence[str], class_var: str) -> tuple[str, ...]:
    """Return ``terms`` as a tuple after checking that ``class_var`` is one of them."""
    if isinstance(terms, str):
        raise TypeError(
            "glm_solution: terms must be a sequence of variable names, not the single string "
            f"{terms!r}"
        )
    model_terms = tuple(terms)
    if class_var not in model_terms:
        raise ValueError(
            f"glm_solution: CLASS variable {class_var!r} is not a MODEL term "
            f"({' '.join(map(str, model_terms)) or 'none'})"
        )
    return model_terms


def _numeric_columns(df: pd.DataFrame, names: Sequence[str]) -> dict[str, np.ndarray]:
    """Return each named column as float64, with every missing value as NaN."""
    unique_names = list(dict.fromkeys(names))
    absent = [name for name in unique_names if name not in df.columns]
    if absent:
        raise KeyError(
            f"glm_solution: model variables not in the frame: {', '.join(map(str, absent))}"
        )
    columns: dict[str, np.ndarray] = {}
    for name in unique_names:
        column = df[name]
        if isinstance(column, pd.DataFrame):
            raise TypeError(f"glm_solution: model variable {name!r} names more than one column")
        # A character variable may enter a MODEL statement only through CLASS, and numpy would
        # otherwise turn numeric-looking strings into numbers.
        if not pd.api.types.is_numeric_dtype(column.dtype) and any(
            isinstance(value, str) for value in column.to_numpy(dtype=object)
        ):
            raise TypeError(f"glm_solution: model variable {name!r} holds character values")
        columns[name] = column.to_numpy(dtype="float64", na_value=np.nan)
    return columns


def _class_levels(values: np.ndarray, ref_level: object) -> list[tuple[float, str]] | None:
    """Return the CLASS levels as ``(value, label)`` in GLM order, or None without the reference.

    Levels ascend by value, the unformatted-numeric reading of ``ORDER=FORMATTED``, and ``REF=``
    moves the level whose label equals that of ``ref_level`` last (D-07).
    """
    reference_label = format_level(ref_level)
    others: list[tuple[float, str]] = []
    reference: list[tuple[float, str]] = []
    for level in np.unique(values):
        label = format_level(level)
        (reference if label == reference_label else others).append((float(level), label))
    if not reference:
        return None
    return others + reference


def _design_columns(
    data: dict[str, np.ndarray],
    terms: tuple[str, ...],
    class_var: str,
    levels: list[tuple[float, str]],
    nobs: int,
) -> list[_DesignColumn]:
    """Return the less-than-full-rank design in MODEL order (D-05, D-07)."""
    design = [_DesignColumn(_INTERCEPT_EFFECT, "", _INTERCEPT_PARAMETER, np.ones(nobs))]
    for term in terms:
        if term == class_var:
            class_values = data[class_var]
            design.extend(
                _DesignColumn(
                    class_var.upper(),
                    label,
                    f"{class_var.upper()} {label}",
                    (class_values == level).astype("float64"),
                )
                for level, label in levels
            )
        else:
            design.append(_DesignColumn(term.upper(), "", term, data[term]))
    return design


def _retained_columns(design: list[_DesignColumn]) -> list[int]:
    """Return the indices of the design columns the modified sweep keeps (D-06)."""
    retained: list[int] = []
    for index, column in enumerate(design):
        norm = np.linalg.norm(column.values)
        if norm == 0.0:
            continue
        if retained:
            basis = np.column_stack([design[kept].values for kept in retained])
            coefficients = np.linalg.lstsq(basis, column.values, rcond=None)[0]
            residual = column.values - basis @ coefficients
            if np.linalg.norm(residual) <= ALIAS_TOLERANCE * norm:
                continue
        retained.append(index)
    return retained


def _fit(
    endog: np.ndarray, exog: np.ndarray, dfe: int
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Fit OLS on the full-rank retained design; return estimates, SEs, t values and p values."""
    if dfe == 0:
        # D-09: the residual variance is 0/0, so statsmodels warns and yields inf or NaN for the
        # standard errors; SAS leaves them missing.
        with warnings.catch_warnings(), np.errstate(divide="ignore", invalid="ignore"):
            warnings.simplefilter("ignore", RuntimeWarning)
            estimate = np.asarray(sm.OLS(endog, exog).fit().params, dtype="float64")
        missing = np.full(estimate.shape, np.nan)
        return estimate, missing, missing, missing
    result = sm.OLS(endog, exog).fit()
    return (
        np.asarray(result.params, dtype="float64"),
        np.asarray(result.bse, dtype="float64"),
        np.asarray(result.tvalues, dtype="float64"),
        np.asarray(result.pvalues, dtype="float64"),
    )


def _estimates_frame(rows: list[tuple]) -> pd.DataFrame:
    """Return ParameterEstimates rows as a frame with the schema columns and dtypes."""
    frame = pd.DataFrame.from_records(rows, columns=list(PARAMETER_ESTIMATE_COLUMNS))
    return frame.astype({name: _ESTIMATE_DTYPES[name] for name in PARAMETER_ESTIMATE_COLUMNS})
