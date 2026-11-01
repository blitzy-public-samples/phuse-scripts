"""Comparison rules and statuses of the WPCT Python-versus-SAS equivalence checks.

This module holds the only comparison rules of the port; the tests and the report use it and never
re-implement it. Every limit comes from ``python/wpct/tolerances.json`` through
:func:`load_tolerances`, so the code carries no tolerance value. Comparisons run on the in-memory
:class:`~wpct.programs.ProgramResult` of each driver, never on re-read CSV exports, and never re-run
SAS logic. Every SAS value compared was parsed by ``wpct.sas_reference`` from an unchanged
repository file or a user-supplied export.

Tolerance classes (AAP 0.7.1), dispatched by class key:

- ``counts``: exact integer equality, so its ``limit`` must be 0: log checkpoints, record and
  stratum counts, summary ``n``.
- ``categorical``: exact equality after stripping trailing blanks; numbers compare exactly.
- ``summary_statistics``, ``record_numeric``, ``p_values``: absolute, ``|py - sas| <= limit``.
- ``glm_estimates``: relative, ``|py - sas| <= limit * |sas|``; a SAS value of 0 passes only an
  exact 0.
- ``rounded_listing``: displayed precision, ``|py - v| <= half_unit * 10**-d + epsilon`` for a token
  with ``d`` decimals; the ``bound_token`` ``<.0001`` passes only ``0 <= py < bound_upper``.
- ``missing``: both missing passes, exactly one missing fails; it applies before every other rule.

Groups (AAP 0.7.2, 0.7.3), per program in the order :func:`compare_program` returns them, which is
the report order. A normal run:

- ``log_checkpoints``: one row per pinned SAS log checkpoint (15 for 07.02, 29 for 07.07).
- ``listing_parameter_estimates``: every ParameterEstimates value of the listings, 40 for 07.02 and
  128 for 07.07 (S02-MOD-02, S07-MOD-02, S07-MOD-03).
- ``listing_merged_pval``: the ``CSS_STATS.pval`` cells the merge fills, 6 for 07.02 and 18 for
  07.07 (S02-TRN-05, S07-TRN-08).
- ``export:<file>``: one group per user-supplied full-precision export: record-level sets
  (``css_anadata``, ``advs_min``), statistics (S02-MOD-01, S07-MOD-01) and ParameterEstimates.
- ``not_compared``: every quantity without a SAS reference, each with its reason (AAP 0.7.5).

A run stopped by a halting check (S07-FLT-04, S07-FLT-05) has no listing or ``export:<file>`` group:

- ``halt``: one FAIL row carrying the halt message.
- ``log_checkpoints``: still one row per pinned SAS log checkpoint; the checkpoints the run reached
  are compared, and each step it never reached is NOT COMPARED.
- ``not_compared``: the listing ParameterEstimates, the merged ``pval`` and every supplied export
  are NOT COMPARED, beside the rows for the exports not supplied.

A row's status is :data:`PASS`, :data:`FAIL` or :data:`NOT_COMPARED`. NOT COMPARED is never counted
as PASS: the group counters and :func:`count_statuses` keep the three apart. Row order is
deterministic: map order for the pinned references, then the SAS sort order of the record keys.
"""

from __future__ import annotations

import dataclasses
import decimal
import json
import math
import os
import re
from collections.abc import Iterable, Iterator, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from types import MappingProxyType
from typing import Any

import numpy as np
import pandas as pd

from wpct.programs import MODEL_POOLED, MODEL_STUDY, Checkpoint, ProgramResult
from wpct.sas_reference import ListingTable, SasCheckpoint, split_parameter
from wpct.sas_semantics import is_char_missing, is_num_missing, sas_sort
from wpct.schemas import (
    CHAR,
    EXPORTS,
    PROGRAMS,
    ExportSpec,
    export_spec,
    sas_export_spec,
    upper_columns,
)

__all__ = [
    "FAIL",
    "GROUP_HALT",
    "GROUP_LISTING_MERGED_PVAL",
    "GROUP_LISTING_PARAMETER_ESTIMATES",
    "GROUP_LOG_CHECKPOINTS",
    "GROUP_NOT_COMPARED",
    "MERGED_PVAL_NOTE",
    "NOT_COMPARED",
    "PASS",
    "STATUSES",
    "TOLERANCE_CLASSES",
    "ComparisonGroup",
    "ComparisonRow",
    "RuleOutcome",
    "Tolerances",
    "check_categorical",
    "check_count",
    "check_rounded",
    "check_value",
    "compare_counts",
    "compare_estimates",
    "compare_listing",
    "compare_merged_pval",
    "compare_program",
    "compare_records",
    "compare_summary",
    "compared_fields",
    "count_statuses",
    "export_group_key",
    "load_tolerances",
    "not_compared_rows",
]

PASS = "PASS"
FAIL = "FAIL"
NOT_COMPARED = "NOT COMPARED"
#: The row statuses, in report order.
STATUSES = (PASS, FAIL, NOT_COMPARED)

GROUP_LOG_CHECKPOINTS = "log_checkpoints"
GROUP_LISTING_PARAMETER_ESTIMATES = "listing_parameter_estimates"
GROUP_LISTING_MERGED_PVAL = "listing_merged_pval"
GROUP_NOT_COMPARED = "not_compared"
GROUP_HALT = "halt"
_EXPORT_GROUP_PREFIX = "export:"

#: Heading note of the merged-pval group (AAP 0.7.3, 0.7.5).
MERGED_PVAL_NOTE = "verifies the CSS_STATS cells filled by the merge, not the model rows"

_REFERENCE_DIR = "whitepapers/WPCT/outputs_sas"
_EXPORT_README = "python/wpct/reference/sas_exports/README.md"

#: ``ModelRecord.STATUS`` of a completed fit, as ``wpct.programs.ModelRecord`` documents it; every
#: other status means the model produced no ParameterEstimates rows.
_STATUS_FITTED = "fitted"

#: The displayed ParameterEstimates columns of a listing row, in table order.
_LISTING_FIELDS = ("ESTIMATE", "STDERR", "TVALUE", "PROBT")
_PVAL = "PVAL"

#: Field-mismatch rows of record-level exports name at most this many keys, then the remainder's
#: count; keys present on one side only are always listed one row each (AAP 0.7.2).
_MAX_LISTED_KEYS = 20

_NO_STRATUM = "—"
_ALL_STRATA = "all strata"
#: ``python_value`` of a listing ParameterEstimates or merged-pval row for which Python produced no
#: value to compare; ``"."`` is kept for a Python value that is SAS missing.
_NO_PYTHON_VALUE = ""


# --- Tolerances -----------------------------------------------------------------------------------

#: Every tolerance class of ``tolerances.json``, in file order, with the rule this module applies
#: to it. ``rule`` in the file is display text, but it must name the rule applied here, so the
#: report's tolerance table cannot describe a rule the comparisons do not use.
TOLERANCE_CLASSES: Mapping[str, str] = MappingProxyType(
    {
        "counts": "exact",
        "categorical": "exact",
        "summary_statistics": "absolute",
        "glm_estimates": "relative",
        "p_values": "absolute",
        "record_numeric": "absolute",
        "rounded_listing": "displayed_precision",
        "missing": "missing_agreement",
    }
)

_SUPPORTED_SCHEMA_VERSION = 1
#: The top-level keys of ``tolerances.json``; ``description`` is optional.
_TOP_LEVEL_KEYS = ("schema_version", "description", "classes")
_COMMON_KEYS = ("label", "rule", "applies_to")
_LIMIT_CLASSES = ("counts", "summary_statistics", "glm_estimates", "p_values", "record_numeric")
_NUMBER_KEYS: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {"rounded_listing": ("half_unit", "epsilon", "bound_upper")}
)
_TEXT_KEYS: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {
        "glm_estimates": ("zero_reference",),
        "rounded_listing": ("bound_token",),
        "missing": ("both_missing", "one_missing"),
    }
)
_BOOL_KEYS: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {"categorical": ("strip_trailing_blanks",)}
)
#: The only ``glm_estimates.zero_reference`` rule: a SAS value of 0 passes only an exact 0.
_ZERO_REFERENCE_EXACT = "exact"


def _freeze(value: Any) -> Any:
    """Return a read-only deep copy of parsed JSON: mappings as proxies, lists as tuples."""
    if isinstance(value, Mapping):
        return MappingProxyType({str(key): _freeze(item) for key, item in value.items()})
    if isinstance(value, (list, tuple)):
        return tuple(_freeze(item) for item in value)
    return value


def _thaw(value: Any) -> Any:
    """Return a mutable, JSON-serialisable deep copy of a :func:`_freeze` result."""
    if isinstance(value, Mapping):
        return {key: _thaw(item) for key, item in value.items()}
    if isinstance(value, tuple):
        return [_thaw(item) for item in value]
    return value


def _is_number(value: object) -> bool:
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def _validate_tolerances(raw: Mapping[str, Any]) -> None:
    """Raise ``ValueError`` naming the first missing, invalid or unknown class or key of ``raw``.

    Each class holds only the keys this module reads, so the report's tolerance table cannot show
    a value no comparison applies; the field lists come from ``wpct.schemas``, never from the file.
    An object of the wrong JSON type (a class that is not an object, a flag that is not a boolean)
    raises ``TypeError`` instead; :func:`load_tolerances` reports both as ``ValueError``.
    """
    where = "tolerances.json"
    if not isinstance(raw, Mapping):
        raise TypeError(f"{where}: the top level must be an object, got {type(raw).__name__}")
    if "schema_version" not in raw:
        raise ValueError(f"{where}: missing key 'schema_version'")
    version = raw["schema_version"]
    if not _is_number(version) or version != _SUPPORTED_SCHEMA_VERSION:
        raise ValueError(
            f"{where}: schema_version {version!r} is not supported "
            f"(expected {_SUPPORTED_SCHEMA_VERSION})"
        )
    unknown_top = [key for key in raw if key not in _TOP_LEVEL_KEYS]
    if unknown_top:
        raise ValueError(
            f"{where}: unknown top-level keys {', '.join(map(repr, unknown_top))}; "
            f"known: {', '.join(_TOP_LEVEL_KEYS)}"
        )
    if "description" in raw and not isinstance(raw["description"], str):
        raise ValueError(f"{where}: description must be a string")
    if "classes" not in raw:
        raise ValueError(f"{where}: missing key 'classes'")
    classes = raw["classes"]
    if not isinstance(classes, Mapping):
        raise TypeError(f"{where}: 'classes' must be an object")
    unknown = [name for name in classes if name not in TOLERANCE_CLASSES]
    if unknown:
        raise ValueError(
            f"{where}: unknown tolerance classes {', '.join(map(repr, unknown))}; "
            f"known: {', '.join(TOLERANCE_CLASSES)}"
        )
    for name, rule in TOLERANCE_CLASSES.items():
        if name not in classes:
            raise ValueError(f"{where}: missing tolerance class 'classes.{name}'")
        entry = classes[name]
        label = f"{where}: classes.{name}"
        if not isinstance(entry, Mapping):
            raise TypeError(f"{label} must be an object")
        number_keys = (("limit",) if name in _LIMIT_CLASSES else ()) + _NUMBER_KEYS.get(name, ())
        known = (*_COMMON_KEYS, *number_keys, *_TEXT_KEYS.get(name, ()), *_BOOL_KEYS.get(name, ()))
        unknown_keys = [f"classes.{name}.{key}" for key in entry if key not in known]
        if unknown_keys:
            raise ValueError(
                f"{where}: unknown keys {', '.join(unknown_keys)}; classes.{name} takes only: "
                f"{', '.join(known)}"
            )
        for key in _COMMON_KEYS:
            if key not in entry:
                raise ValueError(f"{label}: missing key '{key}'")
            if not isinstance(entry[key], str) or not entry[key].strip():
                raise ValueError(f"{label}.{key} must be a non-empty string")
        if entry["rule"] != rule:
            raise ValueError(
                f"{label}.rule is {entry['rule']!r}, but this class is compared by the {rule!r} "
                "rule; only its limits are configurable"
            )
        for key in number_keys:
            if key not in entry:
                raise ValueError(f"{label}: missing key '{key}'")
            value = entry[key]
            try:
                valid = _is_number(value) and math.isfinite(value) and value >= 0
            except OverflowError:  # an int beyond the double range cannot convert to a float
                raise ValueError(
                    f"{label}.{key} must be a finite number >= 0, got an integer outside the "
                    "floating-point range"
                ) from None
            if not valid:
                raise ValueError(f"{label}.{key} must be a finite number >= 0, got {value!r}")
        for key in _TEXT_KEYS.get(name, ()):
            if key not in entry:
                raise ValueError(f"{label}: missing key '{key}'")
            if not isinstance(entry[key], str) or not entry[key]:
                raise ValueError(f"{label}.{key} must be a non-empty string")
        for key in _BOOL_KEYS.get(name, ()):
            if key not in entry:
                raise ValueError(f"{label}: missing key '{key}'")
            if not isinstance(entry[key], bool):
                raise TypeError(f"{label}.{key} must be true or false")
    counts_limit = classes["counts"]["limit"]
    if counts_limit != 0:
        raise ValueError(
            f"{where}: classes.counts.limit must be 0, got {counts_limit!r}; counts are compared "
            "by exact integer equality"
        )
    missing_rule = classes["missing"]
    for key in ("both_missing", "one_missing"):
        if missing_rule[key] not in (PASS, FAIL):
            raise ValueError(
                f"{where}: classes.missing.{key} must be {PASS!r} or {FAIL!r}, "
                f"got {missing_rule[key]!r}"
            )
    zero_reference = classes["glm_estimates"]["zero_reference"]
    if zero_reference != _ZERO_REFERENCE_EXACT:
        raise ValueError(
            f"{where}: classes.glm_estimates.zero_reference {zero_reference!r} is not supported "
            f"(expected {_ZERO_REFERENCE_EXACT!r})"
        )


@dataclass(frozen=True)
class Tolerances:
    """The validated contents of ``tolerances.json`` (AAP 0.7.1).

    Attributes:
        raw: The parsed file, kept whole and read-only (mappings as read-only proxies, lists as
            tuples) for the report's tolerance table. Construction validates it as
            :func:`load_tolerances` does and raises ``ValueError`` naming a missing, invalid or
            unknown class or key (``TypeError`` for a wrongly typed object), so
            ``dataclasses.replace(tolerances, raw=edited)`` and
            :meth:`from_mapping` re-check an edited copy.

    The accessors read every limit from ``raw``; nothing here holds a tolerance of its own.
    """

    # Excluded from the hash: a read-only mapping is unhashable; equality still compares it.
    raw: Mapping[str, Any] = dataclasses.field(hash=False)

    def __post_init__(self) -> None:
        _validate_tolerances(self.raw)
        object.__setattr__(self, "raw", _freeze(self.raw))

    @classmethod
    def from_mapping(cls, raw: Mapping[str, Any]) -> Tolerances:
        """Validate ``raw``, a parsed ``tolerances.json`` or an edited :meth:`to_dict` copy."""
        return cls(raw=raw)

    def to_dict(self) -> dict[str, Any]:
        """Return a mutable, JSON-serialisable deep copy of :attr:`raw`."""
        return _thaw(self.raw)

    @property
    def schema_version(self) -> int:
        """The file's ``schema_version``; construction rejects every unsupported version."""
        return int(self.raw["schema_version"])

    @property
    def classes(self) -> tuple[str, ...]:
        """The tolerance class keys in file order."""
        return tuple(self.raw["classes"])

    def entry(self, class_key: str) -> Mapping[str, Any]:
        """Return the read-only entry of one tolerance class; ``KeyError`` names unknown keys."""
        try:
            return self.raw["classes"][class_key]
        except KeyError:
            raise KeyError(
                f"unknown tolerance class {class_key!r}; known: {', '.join(self.classes)}"
            ) from None

    def limit(self, class_key: str) -> float:
        """The ``limit`` of ``counts``, ``summary_statistics``, ``glm_estimates``, ``p_values``
        or ``record_numeric``."""
        entry = self.entry(class_key)
        if "limit" not in entry:
            raise KeyError(f"tolerance class {class_key!r} has no limit")
        return float(entry["limit"])

    @property
    def strip_trailing_blanks(self) -> bool:
        """``categorical.strip_trailing_blanks``: whether text compares without trailing blanks."""
        return bool(self.entry("categorical")["strip_trailing_blanks"])

    @property
    def zero_reference(self) -> str:
        """``glm_estimates.zero_reference``: the rule for a SAS value of 0.

        Only ``"exact"`` is accepted, under which only an exact Python 0 agrees.
        """
        return str(self.entry("glm_estimates")["zero_reference"])

    @property
    def half_unit(self) -> float:
        """``rounded_listing.half_unit``: the allowed difference from a displayed token.

        It is in units of the token's last displayed decimal: ``half_unit * 10**-d`` for a token
        with ``d`` decimals, before :attr:`epsilon` is added.
        """
        return float(self.entry("rounded_listing")["half_unit"])

    @property
    def epsilon(self) -> float:
        """``rounded_listing.epsilon``: the absolute allowance added to the half-unit bound."""
        return float(self.entry("rounded_listing")["epsilon"])

    @property
    def bound_token(self) -> str:
        """``rounded_listing.bound_token``: the listing token that stands for a bounded value.

        The token, such as ``<.0001``, replaces the digits of a value below :attr:`bound_upper`.
        """
        return str(self.entry("rounded_listing")["bound_token"])

    @property
    def bound_upper(self) -> float:
        """``rounded_listing.bound_upper``: the exclusive upper bound of :attr:`bound_token`.

        A Python value compared with that token passes only when ``0 <= py < bound_upper``.
        """
        return float(self.entry("rounded_listing")["bound_upper"])

    @property
    def both_missing(self) -> str:
        """``missing.both_missing``: the status given when both values are missing.

        It is :data:`PASS` or :data:`FAIL`; construction rejects any other value.
        """
        return str(self.entry("missing")["both_missing"])

    @property
    def one_missing(self) -> str:
        """``missing.one_missing``: the status given when exactly one value is missing.

        It is :data:`PASS` or :data:`FAIL`; construction rejects any other value.
        """
        return str(self.entry("missing")["one_missing"])


def _reject_duplicate_keys(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """``json`` object hook: a repeated key would silently keep only its last value."""
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate key {key!r}")
        result[key] = value
    return result


def load_tolerances(path: str | os.PathLike[str]) -> Tolerances:
    """Read and validate ``tolerances.json``, the single tolerance source (AAP 0.7.1).

    Raises:
        OSError: The file cannot be opened or read; the message names the absolute path.
        ValueError: The file is not UTF-8, is not valid JSON, nests too deeply to parse, repeats a
            key, or lacks, mistypes or adds a class or key; the message names the absolute path
            and, where one applies, the key.
    """
    abs_path = Path(path).absolute()
    try:
        text = abs_path.read_text(encoding="utf-8")
        raw = json.loads(text, object_pairs_hook=_reject_duplicate_keys)
        return Tolerances(raw=raw)
    # UnicodeDecodeError and json.JSONDecodeError are ValueErrors; a value nested deeper than the
    # interpreter's recursion limit makes the parser raise RecursionError. OSError passes through.
    except (RecursionError, TypeError, ValueError) as exc:
        raise ValueError(f"invalid tolerance file {abs_path}: {exc}") from exc


# --- Primitive rules ------------------------------------------------------------------------------


@dataclass(frozen=True)
class RuleOutcome:
    """The verdict of one primitive rule: status plus the row's Difference, Tolerance and Reason."""

    status: str
    difference: str
    tolerance: str
    reason: str


def _render_limit(limit: float) -> str:
    return f"{limit:g}"


def _render_difference(value: float) -> str:
    return f"{value:.3g}"


def _tolerance_text(class_key: str, tolerances: Tolerances) -> str:
    # counts.limit is validated to be 0, so counts, like categorical fields, compare exactly.
    if class_key in ("counts", "categorical"):
        return "exact"
    if class_key == "glm_estimates":
        return f"rel ≤ {_render_limit(tolerances.limit(class_key))}"
    return f"abs ≤ {_render_limit(tolerances.limit(class_key))}"


def _as_float(value: object) -> float:
    """Return a non-missing numeric value as a built-in ``float``; ``TypeError`` otherwise."""
    if isinstance(value, (bool, np.bool_, str)):
        raise TypeError(f"expected a number, got {value!r}")
    return float(value)  # type: ignore[arg-type]


def _missing_outcome(
    py_missing: bool, sas_missing: bool, tolerance: str, tolerances: Tolerances
) -> RuleOutcome | None:
    """Apply the ``missing`` class: a verdict when either side is missing, else ``None``."""
    if py_missing and sas_missing:
        return RuleOutcome(tolerances.both_missing, "", tolerance, "both missing")
    if sas_missing:
        return RuleOutcome(tolerances.one_missing, "", tolerance, "SAS value missing, Python not")
    if py_missing:
        return RuleOutcome(tolerances.one_missing, "", tolerance, "Python value missing, SAS not")
    return None


def _numeric_inputs(
    py: object, sas: object, tolerance: str, tolerances: Tolerances
) -> tuple[float, float] | RuleOutcome:
    """Settle missing values first, then return both sides as floats (or a FAIL for non-numbers).

    A non-number FAIL names its side, never its value, so no record value reaches the reason that
    record-level comparisons report (AAP 0.11.2).
    """
    verdict = _missing_outcome(is_num_missing(py), is_num_missing(sas), tolerance, tolerances)
    if verdict is not None:
        return verdict
    values = []
    for side, value in (("Python", py), ("SAS", sas)):
        try:
            values.append(_as_float(value))
        except (TypeError, ValueError):
            return RuleOutcome(FAIL, "", tolerance, f"{side} value is not a number")
    return values[0], values[1]


_COUNTS_DIFFER = "counts differ"


def check_count(py: object, sas: object, tolerances: Tolerances) -> RuleOutcome:
    """``counts`` class: exact integer equality (``counts.limit`` is 0, the only value allowed)."""
    limit = tolerances.limit("counts")
    tolerance = _tolerance_text("counts", tolerances)
    inputs = _numeric_inputs(py, sas, tolerance, tolerances)
    if isinstance(inputs, RuleOutcome):
        return inputs
    py_value, sas_value = inputs
    if not (py_value.is_integer() and sas_value.is_integer()):
        return RuleOutcome(FAIL, "", tolerance, "a count is not an integer")
    difference = int(py_value) - int(sas_value)
    status = PASS if abs(difference) <= limit else FAIL
    reason = "" if status == PASS else _COUNTS_DIFFER
    return RuleOutcome(status, str(difference), tolerance, reason)


def _char_value(value: object, strip: bool) -> str:
    text = str(value)
    return text.rstrip(" ") if strip else text


def _exact_outcome(equal: bool, tolerance: str) -> RuleOutcome:
    """The ``categorical`` verdict on two non-missing values: PASS, or FAIL because they differ."""
    if equal:
        return RuleOutcome(PASS, "", tolerance, "")
    return RuleOutcome(FAIL, "", tolerance, "values differ")


def check_categorical(py: object, sas: object, tolerances: Tolerances) -> RuleOutcome:
    """``categorical`` class: exact equality, trailing blanks stripped when the file says so.

    Two numbers compare exactly as numbers (``815`` equals ``815.0``); anything else compares as
    text. A blank string is the SAS character missing value.
    """
    tolerance = _tolerance_text("categorical", tolerances)
    py_is_number = _is_number(py) or isinstance(py, (np.integer, np.floating))
    sas_is_number = _is_number(sas) or isinstance(sas, (np.integer, np.floating))
    if py_is_number and sas_is_number:
        inputs = _numeric_inputs(py, sas, tolerance, tolerances)
        if isinstance(inputs, RuleOutcome):
            return inputs
        py_value, sas_value = inputs
        return _exact_outcome(py_value == sas_value, tolerance)
    verdict = _missing_outcome(is_char_missing(py), is_char_missing(sas), tolerance, tolerances)
    if verdict is not None:
        return verdict
    strip = tolerances.strip_trailing_blanks
    return _exact_outcome(_char_value(py, strip) == _char_value(sas, strip), tolerance)


def _check_absolute(class_key: str, py: object, sas: object, tolerances: Tolerances) -> RuleOutcome:
    limit = tolerances.limit(class_key)
    tolerance = _tolerance_text(class_key, tolerances)
    inputs = _numeric_inputs(py, sas, tolerance, tolerances)
    if isinstance(inputs, RuleOutcome):
        return inputs
    py_value, sas_value = inputs
    difference = py_value - sas_value
    if not math.isfinite(difference):
        return RuleOutcome(FAIL, "", tolerance, "non-finite value")
    status = PASS if abs(difference) <= limit else FAIL
    reason = "" if status == PASS else f"values differ by more than {_render_limit(limit)}"
    return RuleOutcome(status, _render_difference(difference), tolerance, reason)


def _check_relative(py: object, sas: object, tolerances: Tolerances) -> RuleOutcome:
    limit = tolerances.limit("glm_estimates")
    tolerance = _tolerance_text("glm_estimates", tolerances)
    inputs = _numeric_inputs(py, sas, tolerance, tolerances)
    if isinstance(inputs, RuleOutcome):
        return inputs
    py_value, sas_value = inputs
    difference = py_value - sas_value
    if not math.isfinite(difference):
        return RuleOutcome(FAIL, "", tolerance, "non-finite value")
    if sas_value == 0:
        # zero_reference "exact": no relative scale exists, so only an exact 0 agrees.
        status = PASS if py_value == 0 else FAIL
        reason = "" if status == PASS else "SAS value is 0; only an exact 0 agrees"
        return RuleOutcome(status, _render_difference(difference), tolerance, reason)
    status = PASS if abs(difference) <= limit * abs(sas_value) else FAIL
    relative = difference / abs(sas_value)
    reason = (
        ""
        if status == PASS
        else f"values differ by more than {_render_limit(limit)} × |SAS value|"
    )
    return RuleOutcome(status, f"{_render_difference(relative)} (rel)", tolerance, reason)


def check_value(class_key: str, py: object, sas: object, tolerances: Tolerances) -> RuleOutcome:
    """Compare one full-precision value under tolerance class ``class_key``.

    ``counts`` and ``categorical`` are exact, ``summary_statistics``, ``record_numeric`` and
    ``p_values`` absolute, and ``glm_estimates`` relative; the ``missing`` rule applies first.
    Displayed listing tokens go through :func:`check_rounded` instead.
    """
    if class_key == "counts":
        return check_count(py, sas, tolerances)
    if class_key == "categorical":
        return check_categorical(py, sas, tolerances)
    if class_key in ("summary_statistics", "record_numeric", "p_values"):
        return _check_absolute(class_key, py, sas, tolerances)
    if class_key == "glm_estimates":
        return _check_relative(py, sas, tolerances)
    raise ValueError(f"no full-precision rule for tolerance class {class_key!r}")


_DISPLAYED_DECIMAL = re.compile(r"^-?(?:\d+\.?\d*|\.\d+)$")


def check_rounded(token: str, py: object, tolerances: Tolerances) -> RuleOutcome:
    """``rounded_listing`` class: a Python value against a token displayed in a SAS listing.

    - ``.`` or blank is SAS missing, so the ``missing`` rule decides.
    - ``bound_token`` (``<.0001``) passes only ``0 <= py < bound_upper``.
    - A token with ``d`` decimals passes when ``|py - v| <= half_unit * 10**-d + epsilon``.

    The ``Tolerance`` text shows the half unit, e.g. ``displayed (8 dp): ±<half unit>``.
    """
    if not isinstance(token, str):
        raise TypeError(f"check_rounded expects the displayed token as a string, got {token!r}")
    text = token.strip(" ")
    sas_missing = text in ("", ".")
    if sas_missing or is_num_missing(py):
        verdict = _missing_outcome(is_num_missing(py), sas_missing, "missing agreement", tolerances)
        if verdict is not None:
            return verdict
    try:
        py_value = _as_float(py)
    except (TypeError, ValueError) as exc:
        return RuleOutcome(FAIL, "", "", f"not a number: {exc}")
    upper = tolerances.bound_upper
    if text == tolerances.bound_token:
        bound = f"0 ≤ py < {_render_limit(upper)}"
        status = PASS if 0 <= py_value < upper else FAIL
        reason = "" if status == PASS else f"Python value outside the bound {bound}"
        return RuleOutcome(status, "", f"bound: {bound}", reason)
    if not _DISPLAYED_DECIMAL.match(text):
        return RuleOutcome(FAIL, "", "", f"unrecognised displayed token {token!r}")
    exponent = decimal.Decimal(text).as_tuple().exponent
    decimals = -exponent if isinstance(exponent, int) and exponent < 0 else 0
    half_unit = tolerances.half_unit * 10.0**-decimals
    tolerance = f"displayed ({decimals} dp): ±{_render_limit(half_unit)}"
    difference = py_value - float(text)
    if not math.isfinite(difference):
        return RuleOutcome(FAIL, "", tolerance, "non-finite value")
    status = PASS if abs(difference) <= half_unit + tolerances.epsilon else FAIL
    reason = (
        "" if status == PASS else f"values differ beyond the displayed precision ({decimals} dp)"
    )
    return RuleOutcome(status, _render_difference(difference), tolerance, reason)


# --- Rows and groups ------------------------------------------------------------------------------


@dataclass(frozen=True)
class ComparisonRow:
    """One compared quantity: a report table row.

    Attributes:
        program: ``"WPCT-F.07.02"`` or ``"WPCT-F.07.07"``.
        group: The key of the :class:`ComparisonGroup` holding the row.
        quantity: What is compared, e.g. ``"TRTPN 54 PROBT"`` or ``"CSS_STATS #2"``.
        stratum: The stratum or record key, e.g. ``"PARAMCD=DIABP ATPTN=815 AVISITN=99"``.
        sas_value: The SAS value as displayed or parsed; ``"."`` for SAS missing.
        python_value: The Python value, shortest round-trip form; ``"."`` for missing. In the
            listing ParameterEstimates and merged-pval groups it is ``""`` when Python produced no
            value to compare (GLM skipped, model absent or not fitted, dataset columns colliding
            in case, no single matching row or cell), and it still shows the Python value when only
            the SAS listing side fails.
        difference: ``py - sas`` (relative where marked), ``""`` when not applicable.
        tolerance: The rule applied, rendered from the limits, e.g. ``"abs ≤ <limit>"``,
            ``"rel ≤ <limit>"``, ``"exact"`` or ``"displayed (4 dp): ±<half unit>"``.
        status: :data:`PASS`, :data:`FAIL` or :data:`NOT_COMPARED`.
        reason: Why the row failed or was not compared; ``""`` for a plain PASS.
    """

    program: str
    group: str
    quantity: str
    stratum: str
    sas_value: str
    python_value: str
    difference: str
    tolerance: str
    status: str
    reason: str

    def __post_init__(self) -> None:
        for name in (
            "program",
            "group",
            "quantity",
            "stratum",
            "sas_value",
            "python_value",
            "difference",
            "tolerance",
            "reason",
        ):
            if not isinstance(getattr(self, name), str):
                raise TypeError(
                    f"ComparisonRow.{name} must be a string, got {getattr(self, name)!r}"
                )
        if self.status not in STATUSES:
            raise ValueError(f"ComparisonRow.status must be one of {STATUSES}, got {self.status!r}")


@dataclass(frozen=True)
class ComparisonGroup:
    """One report table: the rows of one comparison of one program.

    Attributes:
        program: The program compared.
        key: ``log_checkpoints``, ``listing_parameter_estimates``, ``listing_merged_pval``,
            ``export:<file>``, ``not_compared`` or ``halt``.
        title: The table heading.
        heading_note: A sentence the report prints under the heading, or ``None``.
        rounded: True when the SAS side is a rounded listing, so agreement holds only at the
            displayed precision (AAP 0.7.5 wording).
        rows: The rows in deterministic order; each row's ``program`` and ``group`` match.
        tolerances: The validated :class:`Tolerances` snapshot the rows were judged under, so the
            report can print the limits that produced these verdicts without reading
            ``tolerances.json`` again; ``None`` for a group that applies no tolerance
            (``not_compared``, ``halt``).
    """

    program: str
    key: str
    title: str
    heading_note: str | None
    rounded: bool
    rows: tuple[ComparisonRow, ...]
    tolerances: Tolerances | None = dataclasses.field(default=None, repr=False)

    def __post_init__(self) -> None:
        if self.tolerances is not None and not isinstance(self.tolerances, Tolerances):
            raise TypeError(
                "ComparisonGroup.tolerances must be a Tolerances or None, "
                f"got {type(self.tolerances).__name__}"
            )
        rows = tuple(self.rows)
        for position, row in enumerate(rows):
            if not isinstance(row, ComparisonRow):
                raise TypeError(f"ComparisonGroup.rows[{position}] must be a ComparisonRow")
            if row.program != self.program or row.group != self.key:
                raise ValueError(
                    f"ComparisonGroup {self.program}/{self.key}: row {position} belongs to "
                    f"{row.program}/{row.group}"
                )
        object.__setattr__(self, "rows", rows)
        object.__setattr__(self, "rounded", bool(self.rounded))

    def _count(self, status: str) -> int:
        return sum(1 for row in self.rows if row.status == status)

    @property
    def n_pass(self) -> int:
        """The number of :data:`PASS` rows."""
        return self._count(PASS)

    @property
    def n_fail(self) -> int:
        """The number of :data:`FAIL` rows."""
        return self._count(FAIL)

    @property
    def n_not_compared(self) -> int:
        """The number of :data:`NOT_COMPARED` rows; :attr:`n_pass` never counts them."""
        return self._count(NOT_COMPARED)


def count_statuses(groups: Iterable[ComparisonGroup]) -> dict[str, int]:
    """Return the PASS, FAIL and NOT COMPARED row counts of ``groups``, each kept separate."""
    counts = dict.fromkeys(STATUSES, 0)
    for group in groups:
        for row in group.rows:
            counts[row.status] += 1
    return counts


def export_group_key(file_name: str) -> str:
    """The group key of the comparison against one user-supplied SAS export file."""
    return f"{_EXPORT_GROUP_PREFIX}{file_name}"


class _RowBuilder:
    """Collects the rows of one group, stamping program, group key and the tolerances applied."""

    def __init__(self, program: str, group: str, tolerances: Tolerances | None = None) -> None:
        self.program = program
        self.group = group
        self.tolerances = tolerances
        self.rows: list[ComparisonRow] = []

    def add(
        self,
        quantity: str,
        stratum: str,
        sas_value: str,
        python_value: str,
        outcome: RuleOutcome,
        reason: str = "",
    ) -> None:
        reasons = "; ".join(text for text in (outcome.reason, reason) if text)
        self.rows.append(
            ComparisonRow(
                program=self.program,
                group=self.group,
                quantity=quantity,
                stratum=stratum,
                sas_value=sas_value,
                python_value=python_value,
                difference=outcome.difference,
                tolerance=outcome.tolerance,
                status=outcome.status,
                reason=reasons,
            )
        )

    def fail(
        self, quantity: str, stratum: str, reason: str, sas_value: str = "", python_value: str = ""
    ) -> None:
        self.add(quantity, stratum, sas_value, python_value, RuleOutcome(FAIL, "", "", reason))

    def not_compared(self, quantity: str, stratum: str, reason: str) -> None:
        self.add(quantity, stratum, "", "", RuleOutcome(NOT_COMPARED, "", "", reason))

    def group_of(self, title: str, heading_note: str | None, rounded: bool) -> ComparisonGroup:
        return ComparisonGroup(
            program=self.program,
            key=self.group,
            title=title,
            heading_note=heading_note,
            rounded=rounded,
            rows=tuple(self.rows),
            tolerances=self.tolerances,
        )


# --- Rendering ------------------------------------------------------------------------------------


def _render_text(value: str) -> str:
    # The pooled identifier starts with the invisible byte 'A0'x (D-16); show it.
    return value.replace("\u00a0", "<A0>")


def _render_value(value: object) -> str:
    """Render a report cell value: SAS missing as ``.``, numbers in shortest round-trip form."""
    if isinstance(value, str):
        return "." if is_char_missing(value) else _render_text(value.rstrip(" "))
    if is_num_missing(value):
        return "."
    if isinstance(value, (bool, np.bool_)):
        return str(bool(value))
    if isinstance(value, (int, np.integer)):
        return str(int(value))
    try:
        number = float(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return _render_text(str(value))
    if number.is_integer() and abs(number) < 1e16:
        return str(int(number))
    return repr(number)


def _render_token(token: str) -> str:
    """Render a displayed listing token, keeping it as printed; blank as ``(blank)``."""
    return "(blank)" if is_char_missing(token) else token


def _render_pairs(names: Sequence[str], values: Sequence[object]) -> str:
    return " ".join(f"{name}={_render_value(value)}" for name, value in zip(names, values))


def _stratum(paramcd: object, atptn: object, *extra: tuple[str, object]) -> str:
    """``PARAMCD=DIABP ATPTN=815`` plus any extra ``name=value`` pairs; ``—`` when all are unset."""
    pairs = [("PARAMCD", paramcd), ("ATPTN", atptn), *extra]
    present = [(name, value) for name, value in pairs if value is not None]
    if not present:
        return _NO_STRATUM
    return _render_pairs([name for name, _ in present], [value for _, value in present])


def _trimmed(value: object) -> str:
    return "" if is_char_missing(value) else str(value).rstrip(" ")


# --- Log checkpoints ------------------------------------------------------------------------------


def _checkpoint_text(nobs: int | None, nvars: int | None, has_nvars: bool) -> str:
    if nobs is None:
        return "."
    if has_nvars:
        return f"{nobs}/{'.' if nvars is None else nvars}"
    return str(nobs)


def _count_problem(count: str, outcome: RuleOutcome) -> str:
    """The log-checkpoint reason of a failed ``observation`` or ``variable`` count.

    Unequal counts read ``<count> count differs``; any other failure keeps the rule's own reason
    after ``<count> count: ``.
    """
    if outcome.reason == _COUNTS_DIFFER:
        return f"{count} count differs"
    return f"{count} count: {outcome.reason}"


def compare_counts(
    program: str,
    python_checkpoints: Sequence[Checkpoint],
    sas_checkpoints: Sequence[SasCheckpoint],
    tolerances: Tolerances,
    *,
    halt_message: str | None = None,
) -> ComparisonGroup:
    """Compare the log-equivalent record counts with the pinned SAS log lines (AAP 0.7.3).

    One row per SAS checkpoint, in log order (15 for 07.02, 29 for 07.07), matched to the Python
    ``Checkpoint`` of the same ``(SAS_DATASET, OCCURRENCE)``. A row passes only when NOBS agree
    under the ``counts`` class, NVARS agree where the log shows ``obs/vars``, and the Python
    checkpoint belongs to the spec's ``PARAMCD``/``ATPTN`` stratum. A SAS line that failed its
    verification, or a Python checkpoint that is absent, is a FAIL; Python checkpoints without a
    SAS counterpart are not counted. With ``halt_message`` set, a checkpoint the halted run never
    reached is NOT COMPARED rather than FAIL: the halt group carries the failure.

    Evidences S02-MOD-01, S02-MOD-02, S02-TRN-05 and S07-MOD-01..03, S07-TRN-08 through the
    statistics and p-value dataset sizes, and the selection and derivation items through theirs.
    """
    _check_program(program)
    builder = _RowBuilder(program, GROUP_LOG_CHECKPOINTS, tolerances)
    by_key = {(cp.SAS_DATASET, cp.OCCURRENCE): cp for cp in python_checkpoints}
    for sas in sas_checkpoints:
        if not isinstance(sas, SasCheckpoint):
            raise TypeError(f"sas_checkpoints must hold SasCheckpoint records, got {sas!r}")
        spec = sas.spec
        quantity = f"{spec.SAS_DATASET} #{spec.OCCURRENCE}"
        stratum = _stratum(spec.PARAMCD, spec.ATPTN)
        sas_text = _checkpoint_text(sas.NOBS, sas.NVARS, spec.has_nvars)
        python = by_key.get((spec.SAS_DATASET, spec.OCCURRENCE))
        python_text = (
            "." if python is None else _checkpoint_text(python.NOBS, python.NVARS, spec.has_nvars)
        )
        if python is None and halt_message is not None:
            builder.add(
                quantity,
                stratum,
                sas_text,
                python_text,
                RuleOutcome(NOT_COMPARED, "", "", "not reached: the run halted before this step"),
            )
            continue
        if not sas.ok:
            builder.fail(
                quantity,
                stratum,
                sas.problem or "SAS log line failed verification",
                sas_text,
                python_text,
            )
            continue
        if python is None:
            builder.fail(quantity, stratum, "no Python checkpoint recorded", sas_text, python_text)
            continue
        outcome = check_count(python.NOBS, sas.NOBS, tolerances)
        problems = []
        if outcome.status != PASS:
            problems.append(_count_problem("observation", outcome))
        status = outcome.status
        difference = outcome.difference
        if spec.has_nvars:
            nvars = check_count(python.NVARS, sas.NVARS, tolerances)
            difference = f"{difference or '.'}/{nvars.difference or '.'}"
            if nvars.status != PASS:
                status = FAIL
                problems.append(_count_problem("variable", nvars))
        if (python.PARAMCD, python.ATPTN) != (spec.PARAMCD, spec.ATPTN):
            status = FAIL
            problems.append(
                f"Python checkpoint stratum {_stratum(python.PARAMCD, python.ATPTN)} differs"
            )
        builder.add(
            quantity,
            stratum,
            sas_text,
            python_text,
            RuleOutcome(status, difference, outcome.tolerance, "; ".join(problems)),
        )
    log_file = f"{_REFERENCE_DIR}/{program}.log"
    return builder.group_of(
        f"Log checkpoints ({program}.log)",
        f"Record counts at the pinned lines of {log_file}, compared under the counts class.",
        rounded=False,
    )


# --- Listing comparisons --------------------------------------------------------------------------


def _upper_frame(frame: pd.DataFrame | None) -> pd.DataFrame:
    """Return ``frame`` with SAS names folded to upper case; empty when ``frame`` is ``None``."""
    if frame is None:
        return pd.DataFrame()
    columns = upper_columns(frame.columns)
    duplicated = sorted({name for name in columns if columns.count(name) > 1})
    if duplicated:
        raise ValueError(f"columns differ only in case: {' '.join(duplicated)}")
    return frame.set_axis(list(columns), axis=1)


def _check_program(program: str) -> str:
    if program not in PROGRAMS:
        raise ValueError(f"unknown program {program!r}; expected one of {', '.join(PROGRAMS)}")
    return program


def _table_studynum(spec: Any, result: ProgramResult) -> int | None:
    """The ``studynum`` of a 07.07 table: the BY value, or ``&stdyn`` for the pooled model."""
    if spec.MODEL == MODEL_STUDY:
        return spec.STUDYNUM
    if spec.MODEL == MODEL_POOLED:
        return result.pooled_studynum
    return None


def _table_extra(spec: Any, result: ProgramResult) -> list[tuple[str, object]]:
    """The stratum labels after ``PARAMCD ATPTN`` of a listing table: its visit or study number."""
    extra: list[tuple[str, object]] = []
    if spec.AVISITN is not None:
        extra.append(("AVISITN", spec.AVISITN))
    if spec.MODEL == MODEL_STUDY:
        extra.append(("studynum", spec.STUDYNUM))
    elif spec.MODEL == MODEL_POOLED:
        studynum = result.pooled_studynum
        extra.append(("studynum", "pooled" if studynum is None else f"{studynum} (pooled)"))
    return extra


def _model_blocker(spec: Any, result: ProgramResult) -> str | None:
    """Why the Python side has no ParameterEstimates for the table of ``spec``, or ``None``.

    Covers the GLM skipped by setting, a missing pooled ``&stdyn``, a model absent from
    ``result.models`` and a model whose status is not ``fitted`` (U-02, D-08, D-09).
    """
    if result.glm_skipped:
        return "not produced: GLM skipped by setting"
    studynum = _table_studynum(spec, result)
    if spec.MODEL == MODEL_POOLED and studynum is None:
        return "no pooled studynum (&stdyn): no study is present"
    matches = [
        record
        for record in result.models
        if record.MODEL == spec.MODEL
        and record.PARAMCD == spec.PARAMCD
        and record.ATPTN == spec.ATPTN
        and (spec.MODEL not in (MODEL_STUDY, MODEL_POOLED) or record.STUDYNUM == studynum)
    ]
    if not matches:
        return f"no Python {spec.MODEL} model for this stratum"
    if len(matches) > 1:
        return f"{len(matches)} Python {spec.MODEL} models for this stratum"
    if matches[0].STATUS != _STATUS_FITTED:
        return _not_fitted_reason(matches[0].STATUS)
    return None


def _not_fitted_reason(status: str) -> str:
    """The reason given for a model whose ``ModelRecord.STATUS`` is not ``fitted``.

    A status that already says so, such as ``not fitted: reference level 0 absent`` (U-02), is the
    reason verbatim; any other, such as ``no observations`` (D-09), follows ``model not fitted: ``.
    """
    return status if status.startswith("not fitted") else f"model not fitted: {status}"


def _joined(*reasons: str | None) -> str:
    """The non-empty ``reasons`` joined with ``"; "``, in the order given."""
    return "; ".join(reason for reason in reasons if reason)


def _python_dataset(result: ProgramResult, name: str) -> tuple[pd.DataFrame, str]:
    """Dataset ``name`` of ``result`` with upper-case columns, and why it is unusable or ``""``.

    Columns that collide once folded to upper case leave the dataset unusable: the frame returned
    is empty and the reason names the dataset, so every value looked up in it fails with that
    reason rather than aborting the comparison.
    """
    try:
        return _upper_frame(result.datasets.get(name)), ""
    except ValueError as exc:
        return pd.DataFrame(), f"Python {name}: {exc}"


def _python_blocker(spec: Any, result: ProgramResult, dataset_problem: str) -> str:
    """Why no value of the table of ``spec`` has a Python side; ``""`` when that side is usable.

    Names each that applies: a missing or unfitted model (:func:`_model_blocker`) and an unusable
    Python dataset (:func:`_python_dataset`).
    """
    return _joined(_model_blocker(spec, result), dataset_problem)


def _listing_reason(table: ListingTable, listing_row: object | None) -> str:
    """Why the SAS side of one listing value fails: the table's problems, else a missing row."""
    if table.problems:
        return "; ".join(table.problems)
    return "no listing row" if listing_row is None else ""


def _value_mask(frame: pd.DataFrame, column: str, value: object) -> np.ndarray:
    """Rows of ``frame`` whose ``column`` equals ``value``: text after trimming, numbers exactly."""
    if column not in frame.columns:
        return np.zeros(len(frame), dtype=bool)
    values = frame[column].to_numpy(dtype=object)
    if isinstance(value, str):
        return np.fromiter(
            (_trimmed(item) == value for item in values), dtype=bool, count=len(values)
        )
    target = float(value)  # type: ignore[arg-type]
    return np.fromiter(
        (not is_num_missing(item) and _numeric_equal(item, target) for item in values),
        dtype=bool,
        count=len(values),
    )


def _numeric_equal(item: object, target: float) -> bool:
    try:
        return _as_float(item) == target
    except (TypeError, ValueError):
        return False


def _select(frame: pd.DataFrame, conditions: Sequence[tuple[str, object]]) -> pd.DataFrame:
    mask = np.ones(len(frame), dtype=bool)
    for column, value in conditions:
        mask &= _value_mask(frame, column, value)
    return frame.loc[mask]


def _python_estimates(
    estimates: pd.DataFrame, spec: Any, result: ProgramResult
) -> dict[tuple[str, str], list[Mapping[str, Any]]]:
    """The Python ParameterEstimates rows of one listing table, by canonical ``(EFFECT, LEVEL)``.

    Rows are selected by ``PARAMCD`` and ``ATPTN`` and, for 07.07, by ``STUDYNUM`` (the BY value,
    or ``&stdyn`` for the pooled model). SAS prints ``BASE`` in 07.02 and ``base`` in 07.07, so
    both sides key on the canonical upper-case effect.
    """
    values = {
        "PARAMCD": spec.PARAMCD,
        "ATPTN": spec.ATPTN,
        "STUDYNUM": _table_studynum(spec, result),
    }
    record_key = upper_columns(export_spec(result.program_id, "parameter_estimates").key)
    # The schema key decides which stratum columns select the table's rows; EFFECT and LEVEL
    # identify the rows within it.
    conditions = [
        (column, values[column]) for column in record_key if column not in ("EFFECT", "LEVEL")
    ]
    selected = _select(estimates, conditions)
    rows: dict[tuple[str, str], list[Mapping[str, Any]]] = {}
    for record in selected.to_dict(orient="records"):
        pair = (_trimmed(record.get("EFFECT")).upper(), _trimmed(record.get("LEVEL")))
        rows.setdefault(pair, []).append(record)
    return rows


def _python_row_problem(matches: Sequence[Mapping[str, Any]], field: str) -> str:
    """Why ``matches`` hold no single Python value of ``field``; ``""`` when one row holds it."""
    if len(matches) > 1:
        return f"{len(matches)} Python rows"
    if not matches:
        return "no Python ParameterEstimates row"
    if field not in matches[0]:
        return f"Python rows lack {field}"
    return ""


def _parameter_label(effect: str, level: str) -> str:
    return f"{effect} {level}" if level else effect


def compare_listing(
    program: str,
    listing_tables: Sequence[ListingTable],
    result: ProgramResult,
    tolerances: Tolerances,
) -> ComparisonGroup:
    """Compare the Python ParameterEstimates with the rounded SAS listing (AAP 0.7.3).

    For every listing table and every ``(EFFECT, LEVEL)`` its map entry expects, four rows:
    ``ESTIMATE``, ``STDERR``, ``TVALUE`` and ``PROBT`` under the ``rounded_listing`` class, 40 for
    07.02 (S02-MOD-02) and 128 for 07.07 (S07-MOD-02 per study, S07-MOD-03 pooled, whose
    ``studynum`` row is aliased: estimate 0 and missing SE, t and p, D-06, DIV-01). A table with
    listing problems, a stratum without a fitted Python model, ``parameter_estimates`` columns that
    collide in case, a missing row on either side, or a duplicated Python row makes each affected
    value a FAIL whose reason names the SAS side's problem, then the Python side's; nothing is
    skipped. A FAIL row still shows the Python value whenever Python has one for it.
    """
    _check_program(program)
    builder = _RowBuilder(program, GROUP_LISTING_PARAMETER_ESTIMATES, tolerances)
    estimates, dataset_problem = _python_dataset(result, "parameter_estimates")
    for table in listing_tables:
        if not isinstance(table, ListingTable):
            raise TypeError(f"listing_tables must hold ListingTable records, got {table!r}")
        spec = table.spec
        if spec is None:
            builder.fail(
                f"listing table {table.index}",
                _NO_STRATUM,
                "; ".join(table.problems) or "table not in the listing map",
            )
            continue
        stratum = _stratum(spec.PARAMCD, spec.ATPTN, *_table_extra(spec, result))
        python_blocker = _python_blocker(spec, result, dataset_problem)
        listing_rows = {(row.EFFECT, row.LEVEL): row for row in table.rows}
        python_rows = {} if python_blocker else _python_estimates(estimates, spec, result)
        for effect, level in spec.parameters:
            label = _parameter_label(effect, level)
            listing_row = listing_rows.get((effect, level))
            listing_reason = _listing_reason(table, listing_row)
            matches = python_rows.get((effect, level), [])
            for field in _LISTING_FIELDS:
                quantity = f"{label} {field}"
                token = getattr(listing_row, field) if listing_row is not None else None
                sas_text = "." if token is None else _render_token(token)
                python_reason = python_blocker or _python_row_problem(matches, field)
                python_value = None if python_reason else matches[0][field]
                python_text = _NO_PYTHON_VALUE if python_reason else _render_value(python_value)
                reason = _joined(listing_reason, python_reason)
                if reason:
                    builder.fail(quantity, stratum, reason, sas_text, python_text)
                else:
                    outcome = check_rounded(token, python_value, tolerances)
                    builder.add(quantity, stratum, sas_text, python_text, outcome)
    return builder.group_of(
        f"ParameterEstimates against the listing ({program}.lst)", None, rounded=True
    )


def _merged_pval_cell(
    stats: pd.DataFrame,
    stats_key: Sequence[str],
    stratum_values: Mapping[str, object],
    effect: str,
    level: str,
) -> tuple[object, str]:
    """The merged ``pval`` of the ``css_stats`` cell at one stratum and CLASS level.

    The css_stats record key of the schema names the cell: ``PARAMCD ATPTN AVISITN TRTPN`` for
    07.02, ``PARAMCD ATPTN studynum TRTPN`` for 07.07. Returns ``(pval, "")`` for the one matching
    row, otherwise ``(None, reason)``: a non-numeric level, a key without a value, no ``pval``
    column, or not exactly one matching row.
    """
    try:
        cell_values = {**stratum_values, effect: float(level)}
    except ValueError:
        return None, f"CLASS level {level!r} is not numeric"
    unset = [column for column in stats_key if cell_values.get(column) is None]
    if unset:
        return None, f"no value for css_stats key {' '.join(unset)}"
    if _PVAL not in stats.columns:
        return None, "css_stats has no pval column"
    cell = _select(stats, [(column, cell_values[column]) for column in stats_key])
    if len(cell) != 1:
        return None, f"{len(cell)} css_stats rows match"
    return cell[_PVAL].iloc[0], ""


def compare_merged_pval(
    program: str,
    listing_tables: Sequence[ListingTable],
    result: ProgramResult,
    tolerances: Tolerances,
) -> ComparisonGroup:
    """Compare the merged ``CSS_STATS.pval`` cells with the listing's CLASS-level ``Pr > |t|``.

    The SAS merge copies each ``TRTPN`` row's Probt into ``CSS_STATS`` (S02-TRN-05 at
    ``AVISITN = 99``; S07-TRN-08 by ``studynum``, the pooled rows through ``&stdyn``). This group
    therefore checks the statistics cells, not the model rows: for each table, each CLASS level of
    its map entry gives the cell at the table's stratum and that ``TRTPN`` value, compared at
    displayed precision (``rounded_listing``). The reference level prints ``.`` or blank, so its
    cell is a missing-agreement check. 3 rows per table: 6 for 07.02, 18 for 07.07. A missing
    cell, a missing ``pval`` column, a duplicated cell, ``css_stats`` columns that collide in case,
    a table with problems or a stratum without a fitted model is a FAIL whose reason names the SAS
    side's problem, then the Python side's. A FAIL row still shows the cell's ``pval`` whenever
    Python has one for it.
    """
    _check_program(program)
    builder = _RowBuilder(program, GROUP_LISTING_MERGED_PVAL, tolerances)
    stats, dataset_problem = _python_dataset(result, "css_stats")
    stats_key = upper_columns(export_spec(program, "css_stats").key)
    for table in listing_tables:
        if not isinstance(table, ListingTable):
            raise TypeError(f"listing_tables must hold ListingTable records, got {table!r}")
        spec = table.spec
        if spec is None:
            builder.fail(
                f"listing table {table.index}",
                _NO_STRATUM,
                "; ".join(table.problems) or "table not in the listing map",
            )
            continue
        python_blocker = _python_blocker(spec, result, dataset_problem)
        listing_rows = {(row.EFFECT, row.LEVEL): row for row in table.rows}
        stratum_values: dict[str, object] = {
            "PARAMCD": spec.PARAMCD,
            "ATPTN": spec.ATPTN,
            "AVISITN": spec.AVISITN,
            "STUDYNUM": _table_studynum(spec, result),
        }
        for effect, level in (pair for pair in spec.parameters if pair[1]):
            quantity = "CSS_STATS.pval"
            stratum = _stratum(
                spec.PARAMCD, spec.ATPTN, *_table_extra(spec, result), (effect, level)
            )
            listing_row = listing_rows.get((effect, level))
            token = None if listing_row is None else listing_row.PROBT
            sas_text = "." if token is None else _render_token(token)
            value, python_reason = None, python_blocker
            if not python_reason:
                value, python_reason = _merged_pval_cell(
                    stats, stats_key, stratum_values, effect, level
                )
            python_text = _NO_PYTHON_VALUE if python_reason else _render_value(value)
            reason = _joined(_listing_reason(table, listing_row), python_reason)
            if reason:
                builder.fail(quantity, stratum, reason, sas_text, python_text)
            else:
                outcome = check_rounded(token, value, tolerances)
                builder.add(quantity, stratum, sas_text, python_text, outcome)
    return builder.group_of(
        f"Merged CSS_STATS.pval against the listing's TRTPN Probt ({program}.lst)",
        MERGED_PVAL_NOTE,
        rounded=True,
    )


# --- Full-precision export comparisons ------------------------------------------------------------


# eq=False: the frame's ``==`` is element-wise, so field-wise equality would raise.
@dataclass(frozen=True, eq=False)
class _Keyed:
    """One side of an export comparison: its upper-case frame and normalised record keys."""

    frame: pd.DataFrame
    keys: tuple[tuple[Any, ...], ...]
    first: Mapping[tuple[Any, ...], int]
    duplicates: Mapping[tuple[Any, ...], int]


def _column_kinds(spec: ExportSpec) -> dict[str, str]:
    """Map each upper-case spec column to its ``schemas`` type code."""
    return dict(zip(upper_columns(spec.column_types), spec.column_types.values()))


def _key_value(value: object, kind: str, strip: bool) -> Any:
    """Normalise one key value of type code ``kind``: ``CHAR`` keys as text, all others as floats.

    A ``CHAR`` character missing value becomes ``''``; any other ``CHAR`` value becomes its text,
    with trailing blanks removed only when ``strip`` is true. For any other kind, a numeric missing
    value becomes ``None`` and any other value a built-in ``float``; a non-number raises
    ``TypeError``.
    """
    if kind == CHAR:
        if is_char_missing(value):
            return ""
        return _char_value(value, strip)
    if is_num_missing(value):
        return None
    return _as_float(value)


def _keyed(
    frame: pd.DataFrame, columns: Sequence[str], kinds: Mapping[str, str], strip: bool
) -> _Keyed:
    column_values = [frame[column].to_numpy(dtype=object) for column in columns]
    column_kinds = [kinds.get(column, CHAR) for column in columns]
    keys = tuple(
        tuple(_key_value(item, kind, strip) for kind, item in zip(column_kinds, row))
        for row in zip(*column_values)
    )
    first: dict[tuple[Any, ...], int] = {}
    counts: dict[tuple[Any, ...], int] = {}
    for position, key in enumerate(keys):
        first.setdefault(key, position)
        counts[key] = counts.get(key, 0) + 1
    duplicates = {key: count for key, count in counts.items() if count > 1}
    return _Keyed(frame=frame, keys=keys, first=first, duplicates=duplicates)


def _sorted_keys(
    keys: Iterable[tuple[Any, ...]], columns: Sequence[str], kinds: Mapping[str, str]
) -> list[tuple[Any, ...]]:
    """Return distinct ``keys`` in PROC SORT order of ``columns`` (missing lowest, D-04)."""
    distinct = list(dict.fromkeys(keys))
    if not distinct:
        return []
    data: dict[str, Any] = {}
    for position, column in enumerate(columns):
        items = [key[position] for key in distinct]
        if kinds.get(column, CHAR) == CHAR:
            data[column] = pd.Series(items, dtype=object)
        else:
            data[column] = pd.Series(
                [np.nan if item is None else item for item in items], dtype="float64"
            )
    order_column = "__ORDER__"
    data[order_column] = np.arange(len(distinct))
    ordered = sas_sort(pd.DataFrame(data), list(columns))
    return [distinct[position] for position in ordered[order_column]]


def _render_key(columns: Sequence[str], key: tuple[Any, ...]) -> str:
    return _render_pairs(columns, key)


def _listed_keys(columns: Sequence[str], keys: Sequence[tuple[Any, ...]]) -> str:
    shown = "; ".join(_render_key(columns, key) for key in keys[:_MAX_LISTED_KEYS])
    if len(keys) > _MAX_LISTED_KEYS:
        shown += f"; and {len(keys) - _MAX_LISTED_KEYS} more"
    return shown


def _prepare_sides(
    builder: _RowBuilder,
    py_frame: pd.DataFrame | None,
    sas_frame: pd.DataFrame | None,
    spec: ExportSpec,
    tolerances: Tolerances,
) -> tuple[_Keyed, _Keyed, tuple[str, ...], dict[str, str]] | None:
    """Fold both frames, check the key columns and build both key sets; ``None`` after a FAIL."""
    if sas_frame is None:
        builder.fail("SAS export", _ALL_STRATA, f"no SAS export {spec.sas_export_file} supplied")
        return None
    if py_frame is None:
        builder.fail("Python dataset", _ALL_STRATA, f"the run produced no {spec.name} dataset")
        return None
    sides = []
    for label, frame in (("SAS export", sas_frame), ("Python dataset", py_frame)):
        try:
            sides.append(_upper_frame(frame))
        except ValueError as exc:
            builder.fail(f"{label} columns", _ALL_STRATA, str(exc))
            return None
    sas, python = sides
    columns = upper_columns(spec.key)
    kinds = _column_kinds(spec)
    for label, frame in (("SAS export", sas), ("Python dataset", python)):
        absent = [column for column in columns if column not in frame.columns]
        if absent:
            builder.fail(
                "record key columns",
                _ALL_STRATA,
                f"key columns absent from the {label}: {' '.join(absent)}",
            )
            return None
    strip = tolerances.strip_trailing_blanks
    try:
        keyed = (_keyed(python, columns, kinds, strip), _keyed(sas, columns, kinds, strip))
    except (TypeError, ValueError) as exc:
        builder.fail(
            "record key values", _ALL_STRATA, f"a numeric key value is not a number: {exc}"
        )
        return None
    return keyed[0], keyed[1], columns, kinds


def _key_set_rows(
    builder: _RowBuilder,
    python: _Keyed,
    sas: _Keyed,
    columns: Sequence[str],
    kinds: Mapping[str, str],
    tolerances: Tolerances,
) -> list[tuple[Any, ...]]:
    """Add the key-set rows (AAP 0.7.2) and return the matched keys in sort order.

    Each key present on one side only is its own FAIL row; repeated keys are FAIL rows too; then
    one key-set agreement row and one record-count row.
    """
    only_sas = _sorted_keys((key for key in sas.first if key not in python.first), columns, kinds)
    only_python = _sorted_keys(
        (key for key in python.first if key not in sas.first), columns, kinds
    )
    for key in only_sas:
        builder.fail(
            "record key only in SAS",
            _render_key(columns, key),
            "no Python record has this key",
            "present",
            "absent",
        )
    for key in only_python:
        builder.fail(
            "record key only in Python",
            _render_key(columns, key),
            "no SAS record has this key",
            "absent",
            "present",
        )
    for label, side in (("SAS", sas), ("Python", python)):
        for key in _sorted_keys(side.duplicates, columns, kinds):
            builder.fail(
                f"repeated record key in {label}",
                _render_key(columns, key),
                f"{side.duplicates[key]} {label} records share this key",
            )
    problems = []
    if only_sas or only_python:
        problems.append(f"{len(only_sas)} keys only in SAS, {len(only_python)} only in Python")
    if sas.duplicates or python.duplicates:
        problems.append(
            f"{len(sas.duplicates)} repeated SAS keys, "
            f"{len(python.duplicates)} repeated Python keys"
        )
    builder.add(
        "key-set agreement",
        " ".join(columns),
        str(len(sas.first)),
        str(len(python.first)),
        RuleOutcome(FAIL if problems else PASS, "", "exact", "; ".join(problems)),
    )
    builder.add(
        "record count",
        _ALL_STRATA,
        str(len(sas.frame)),
        str(len(python.frame)),
        check_count(len(python.frame), len(sas.frame), tolerances),
    )
    return _sorted_keys((key for key in sas.first if key in python.first), columns, kinds)


def _field_presence(
    builder: _RowBuilder,
    field: str,
    python: pd.DataFrame,
    sas: pd.DataFrame,
    optional: bool,
) -> bool:
    """True when ``field`` can be compared; a FAIL row when exactly one side (or a required field
    on either side) lacks it. An optional field absent from both sides is not compared."""
    in_python, in_sas = field in python.columns, field in sas.columns
    if in_python and in_sas:
        return True
    if optional and not in_python and not in_sas:
        return False
    where = " and ".join(
        label
        for label, present in (("SAS export", in_sas), ("Python dataset", in_python))
        if not present
    )
    builder.fail(field, _ALL_STRATA, f"column absent from the {where}")
    return False


def _schema_field_classes(spec: ExportSpec, per_record: bool) -> Iterator[tuple[str, str, str]]:
    """Yield ``(schema field name, upper-case field, tolerance class)`` for each field an export
    compares, in spec order.

    A per-key comparison (``per_record`` false) matches the record-key fields instead of comparing
    them, so it leaves them out.
    """
    key = set(upper_columns(spec.key))
    for names, class_key in (
        (spec.categorical, "categorical"),
        (spec.count_fields, "counts"),
        (spec.summary_fields, "summary_statistics"),
        (spec.glm_fields, "glm_estimates"),
        (spec.p_value_fields, "p_values"),
        (spec.record_numeric, "record_numeric"),
    ):
        for name, upper in zip(names, upper_columns(names)):
            if per_record or upper not in key:
                yield name, upper, class_key


def _field_classes(spec: ExportSpec, per_record: bool) -> list[tuple[str, str]]:
    """The ``(upper-case field, tolerance class)`` pairs an export compares, in spec order."""
    return [(upper, class_key) for _, upper, class_key in _schema_field_classes(spec, per_record)]


def compared_fields() -> Mapping[str, tuple[str, ...]]:
    """The export variables each tolerance class compares, as ``wpct.schemas`` defines them.

    Keyed by every class of :data:`TOLERANCE_CLASSES`, in that order. The fields are the ones the
    comparisons against full-precision SAS exports judge under the class: every export of both
    programs that has a SAS export file, in program and export order, each name once (SAS names are
    case-insensitive) and spelled as its schema spells it. The statistics and ParameterEstimates
    exports are compared per record key, so their key fields are matched rather than compared, as
    :func:`compare_summary` and :func:`compare_estimates` do. A class no export field uses
    (``rounded_listing``, ``missing``) maps to an empty tuple.
    """
    fields: dict[str, dict[str, str]] = {class_key: {} for class_key in TOLERANCE_CLASSES}
    for program in PROGRAMS:
        for spec in EXPORTS[program]:
            if spec.sas_export_file is None:
                continue
            # The dispatch of _compare_export: statistics and estimates per key, others per record.
            per_record = not (spec.summary_fields or spec.glm_fields)
            for name, upper, class_key in _schema_field_classes(spec, per_record):
                fields[class_key].setdefault(upper, name)
    return MappingProxyType({key: tuple(names.values()) for key, names in fields.items()})


def _cell(frame: pd.DataFrame, row: int, column: str) -> Any:
    return frame[column].iat[row]


def _export_group(builder: _RowBuilder, spec: ExportSpec) -> ComparisonGroup:
    return builder.group_of(
        f"Export {spec.sas_export_file}",
        f"Full-precision SAS export of {spec.sas_counterpart}; Python dataset {spec.name}.",
        rounded=False,
    )


def _export_builder(program: str, spec: ExportSpec, tolerances: Tolerances) -> _RowBuilder:
    _check_program(program)
    if spec.program_id != program:
        raise ValueError(f"export spec {spec.program_id}/{spec.name} is not of program {program}")
    if spec.sas_export_file is None:
        raise ValueError(f"export {spec.program_id}/{spec.name} has no SAS export counterpart")
    return _RowBuilder(program, export_group_key(spec.sas_export_file), tolerances)


def compare_records(
    program: str,
    py_frame: pd.DataFrame | None,
    sas_frame: pd.DataFrame | None,
    spec: ExportSpec,
    tolerances: Tolerances,
) -> ComparisonGroup:
    """Compare a record-level dataset with its full-precision SAS export (AAP 0.7.2).

    Applies to ``css_anadata`` (S02-FLT-01..03, S02-POP-01, S02-TRN-02..03; S07-TRN-04..06) and
    ``advs_min`` (S07-TRN-03, S07-FLT-02, LMM-*). Column names are folded to upper case on both
    sides, and records are matched on ``spec.key``. Rows, in order:

    - one FAIL per key present on one side only, listed individually, and per repeated key;
    - ``key-set agreement`` and ``record count``;
    - per categorical field, exact agreement over the matched records, and per ``record_numeric``
      field, the absolute ``record_numeric`` limit; a FAIL names the mismatched keys only, never a
      record value, so no subject-level value reaches the report (AAP 0.11.2);
    - per ``count_strata`` stratum, the record counts under the ``counts`` class.
    """
    builder = _export_builder(program, spec, tolerances)
    prepared = _prepare_sides(builder, py_frame, sas_frame, spec, tolerances)
    if prepared is None:
        return _export_group(builder, spec)
    python, sas, columns, kinds = prepared
    matched = _key_set_rows(builder, python, sas, columns, kinds, tolerances)
    optional = set(upper_columns(spec.optional_columns))
    for field, class_key in _field_classes(spec, per_record=True):
        if not _field_presence(builder, field, python.frame, sas.frame, field in optional):
            continue
        tolerance = _tolerance_text(class_key, tolerances)
        stratum = f"{len(matched)} matched records"
        if not matched:
            builder.add(
                field,
                stratum,
                "",
                "",
                RuleOutcome(NOT_COMPARED, "", tolerance, "no matched records"),
            )
            continue
        mismatched = []
        reasons: dict[str, int] = {}
        for key in matched:
            outcome = check_value(
                class_key,
                _cell(python.frame, python.first[key], field),
                _cell(sas.frame, sas.first[key], field),
                tolerances,
            )
            if outcome.status != PASS:
                mismatched.append(key)
                if outcome.reason:
                    reasons[outcome.reason] = reasons.get(outcome.reason, 0) + 1
        if mismatched:
            detail = "; ".join(f"{count} × {text}" for text, count in reasons.items())
            reason = f"mismatched keys: {_listed_keys(columns, mismatched)}"
            if detail:
                reason = f"{reason} ({detail})"
            outcome = RuleOutcome(FAIL, f"{len(mismatched)} differ", tolerance, reason)
        else:
            outcome = RuleOutcome(PASS, "0 differ", tolerance, "")
        builder.add(field, stratum, "", "", outcome)
    _strata_rows(builder, python.frame, sas.frame, spec, kinds, tolerances)
    return _export_group(builder, spec)


def _strata_rows(
    builder: _RowBuilder,
    python: pd.DataFrame,
    sas: pd.DataFrame,
    spec: ExportSpec,
    kinds: Mapping[str, str],
    tolerances: Tolerances,
) -> None:
    """One row per ``count_strata`` stratum present on either side, counts compared exactly."""
    columns = upper_columns(spec.count_strata)
    if not columns:
        return
    for label, frame in (("SAS export", sas), ("Python dataset", python)):
        absent = [column for column in columns if column not in frame.columns]
        if absent:
            builder.fail(
                "stratum record count",
                _ALL_STRATA,
                f"stratum columns absent from the {label}: {' '.join(absent)}",
            )
            return
    strip = tolerances.strip_trailing_blanks
    try:
        counts = [_keyed(frame, columns, kinds, strip) for frame in (python, sas)]
    except (TypeError, ValueError) as exc:
        builder.fail("stratum record count", _ALL_STRATA, f"a stratum value is not a number: {exc}")
        return
    tallies = []
    for side in counts:
        tally: dict[tuple[Any, ...], int] = {}
        for key in side.keys:
            tally[key] = tally.get(key, 0) + 1
        tallies.append(tally)
    python_tally, sas_tally = tallies
    for key in _sorted_keys([*sas_tally, *python_tally], columns, kinds):
        python_count, sas_count = python_tally.get(key, 0), sas_tally.get(key, 0)
        builder.add(
            "stratum record count",
            _render_key(columns, key),
            str(sas_count),
            str(python_count),
            check_count(python_count, sas_count, tolerances),
        )


def _compare_per_key(
    builder: _RowBuilder,
    py_frame: pd.DataFrame | None,
    sas_frame: pd.DataFrame | None,
    spec: ExportSpec,
    tolerances: Tolerances,
) -> None:
    """Key-set rows, then one row per matched key and compared field, under its class."""
    prepared = _prepare_sides(builder, py_frame, sas_frame, spec, tolerances)
    if prepared is None:
        return
    python, sas, columns, kinds = prepared
    matched = _key_set_rows(builder, python, sas, columns, kinds, tolerances)
    optional = set(upper_columns(spec.optional_columns))
    fields = [
        (field, class_key)
        for field, class_key in _field_classes(spec, per_record=False)
        if _field_presence(builder, field, python.frame, sas.frame, field in optional)
    ]
    for key in matched:
        stratum = _render_key(columns, key)
        for field, class_key in fields:
            python_value = _cell(python.frame, python.first[key], field)
            sas_value = _cell(sas.frame, sas.first[key], field)
            builder.add(
                field,
                stratum,
                _render_value(sas_value),
                _render_value(python_value),
                check_value(class_key, python_value, sas_value, tolerances),
            )


def compare_summary(
    program: str,
    py_stats: pd.DataFrame | None,
    sas_stats: pd.DataFrame | None,
    spec: ExportSpec,
    tolerances: Tolerances,
) -> ComparisonGroup:
    """Compare the PROC SUMMARY statistics and merged p-values with a ``css_stats`` export.

    S02-MOD-01 and S02-TRN-05 (07.02, key ``PARAMCD ATPTN AVISITN TRTPN``), S07-MOD-01 and
    S07-TRN-08 (07.07, key ``PARAMCD ATPTN studynum TRTPN``). After the key-set rows, each matched
    statistics row gives its categorical labels (exact), ``n`` (``counts``),
    ``mean std median datamin datamax q1 q3`` (``summary_statistics``, absolute; D-01 quantiles,
    D-02 STD, D-03 missing values) and ``pval`` (``p_values``, absolute).
    """
    builder = _export_builder(program, spec, tolerances)
    _compare_per_key(builder, py_stats, sas_stats, spec, tolerances)
    return _export_group(builder, spec)


def compare_estimates(
    program: str,
    py_pe: pd.DataFrame | None,
    sas_pe: pd.DataFrame | None,
    spec: ExportSpec,
    tolerances: Tolerances,
) -> ComparisonGroup:
    """Compare the Python ParameterEstimates with a full-precision ``parameter_estimates`` export.

    S02-MOD-02 (key ``PARAMCD ATPTN EFFECT LEVEL``), S07-MOD-02 and S07-MOD-03 (plus ``STUDYNUM``,
    the pooled rows carrying ``&stdyn``). ``EFFECT`` and ``LEVEL`` are the canonical
    :func:`~wpct.sas_reference.split_parameter` pair, derived from ``PARAMETER`` where a frame
    lacks them, so ``BASE`` and ``base`` match. Each matched row gives ``ESTIMATE STDERR TVALUE``
    (``glm_estimates``, relative; D-05 to D-09) and ``PROBT`` (``p_values``, absolute).
    """
    builder = _export_builder(program, spec, tolerances)
    frames = []
    for label, frame in (("SAS export", sas_pe), ("Python dataset", py_pe)):
        if frame is None or ("EFFECT" in upper_columns(frame.columns)):
            frames.append(frame)
            continue
        try:
            frames.append(_with_effect_level(frame))
        except (KeyError, TypeError, ValueError) as exc:
            builder.fail("EFFECT LEVEL", _ALL_STRATA, f"{label}: {exc}")
            return _export_group(builder, spec)
    _compare_per_key(builder, frames[1], frames[0], spec, tolerances)
    return _export_group(builder, spec)


def _with_effect_level(frame: pd.DataFrame) -> pd.DataFrame:
    """Add the canonical ``EFFECT``/``LEVEL`` of each ``PARAMETER`` label to a copy of ``frame``."""
    upper = _upper_frame(frame)
    if "PARAMETER" not in upper.columns:
        raise KeyError("no EFFECT/LEVEL and no Parameter column to derive them from")
    pairs = [split_parameter(_trimmed(label)) for label in upper["PARAMETER"]]
    upper = upper.copy()
    upper["EFFECT"] = [effect for effect, _ in pairs]
    upper["LEVEL"] = [level for _, level in pairs]
    return upper


# --- Not compared and per-program orchestration ---------------------------------------------------


def _absent_export_reason(spec: ExportSpec) -> str:
    return f"no full-precision SAS export {spec.sas_export_file} supplied (see {_EXPORT_README})"


def not_compared_rows(
    program: str, result: ProgramResult, exports_present: Iterable[str]
) -> ComparisonGroup:
    """List every quantity of ``program`` that has no SAS reference to compare with (AAP 0.7.5).

    ``exports_present`` names the SAS export files supplied for this run. Rows, all NOT COMPARED
    with a reason:

    - a halted run (S07-FLT-04, S07-FLT-05): the listing ParameterEstimates, the merged ``pval``
      and every present export, since the run produced nothing to compare;
    - per export file not supplied: the record-level set, categorical fields, numeric values and
      per-stratum counts (``css_anadata``, ``advs_min``); every PROC SUMMARY statistic
      ``n mean std median datamin datamax q1 q3``, which SAS output holds only as PDF raster
      images (S02-MOD-01, S07-MOD-01), and the full-precision merged ``pval`` (S02-TRN-05,
      S07-TRN-08); the full-precision GLM values, of which only rounded listings exist (S02-MOD-02,
      S07-MOD-02, S07-MOD-03);
    - each stratum whose model was not fitted (U-02), the GLM skipped by setting, and each run
      note such as the empty-derivation-input statement (U-03).
    """
    _check_program(program)
    present = set(exports_present)
    builder = _RowBuilder(program, GROUP_NOT_COMPARED)
    listing = f"{_REFERENCE_DIR}/{program}.lst"
    pdfs = f"{_REFERENCE_DIR}/{program}_Box_plot_*.pdf"
    if result.halted:
        halted = f"program halted: {result.halt_message}"
        builder.not_compared("ParameterEstimates against the listing", _ALL_STRATA, halted)
        builder.not_compared("merged CSS_STATS.pval against the listing", _ALL_STRATA, halted)
    for spec in EXPORTS[program]:
        if spec.sas_export_file is None:
            continue
        if spec.sas_export_file in present:
            if result.halted:
                builder.not_compared(
                    f"export {spec.sas_export_file}",
                    _ALL_STRATA,
                    f"program halted: {result.halt_message}",
                )
            continue
        absent = _absent_export_reason(spec)
        if spec.summary_fields:
            for statistic in (*spec.count_fields, *spec.summary_fields):
                builder.not_compared(
                    f"{spec.name}: PROC SUMMARY statistic {statistic}",
                    _ALL_STRATA,
                    f"exists only as PDF raster images ({pdfs}); {absent}",
                )
            for field in spec.p_value_fields:
                builder.not_compared(
                    f"{spec.name}: merged {field} at full precision",
                    _ALL_STRATA,
                    f"only rounded listings exist ({listing}); {absent}",
                )
            if spec.categorical:
                builder.not_compared(
                    f"{spec.name}: statistics rows and categorical fields "
                    f"({' '.join(spec.categorical)})",
                    _ALL_STRATA,
                    absent,
                )
        elif spec.glm_fields:
            fields = " ".join((*spec.glm_fields, *spec.p_value_fields))
            builder.not_compared(
                f"{spec.name}: full-precision GLM values ({fields})",
                _ALL_STRATA,
                f"only rounded listings exist ({listing}); {absent}",
            )
        else:
            builder.not_compared(
                f"{spec.name}: record-level set and categorical fields "
                f"({' '.join(spec.categorical)})",
                _ALL_STRATA,
                absent,
            )
            if spec.record_numeric:
                builder.not_compared(
                    f"{spec.name}: record-level numeric values ({' '.join(spec.record_numeric)})",
                    _ALL_STRATA,
                    absent,
                )
            if spec.count_strata:
                builder.not_compared(
                    f"{spec.name}: per-stratum record counts ({' × '.join(spec.count_strata)})",
                    _ALL_STRATA,
                    absent,
                )
    for record in result.models:
        if record.STATUS != _STATUS_FITTED:
            extra = () if record.STUDYNUM is None else (("studynum", record.STUDYNUM),)
            builder.not_compared(
                f"GLM ParameterEstimates and merged pval ({record.MODEL} model)",
                _stratum(record.PARAMCD, record.ATPTN, *extra),
                _not_fitted_reason(record.STATUS),
            )
    if result.glm_skipped:
        builder.not_compared(
            "GLM ParameterEstimates and merged pval",
            _ALL_STRATA,
            "not produced: GLM skipped by setting",
        )
    for note in result.notes:
        builder.not_compared("run note", _ALL_STRATA, note)
    return builder.group_of(
        "Not compared",
        "Quantities without a SAS reference; no equivalence is claimed for them.",
        rounded=False,
    )


def _halt_group(result: ProgramResult) -> ComparisonGroup:
    builder = _RowBuilder(result.program_id, GROUP_HALT)
    builder.fail(
        "program run",
        _NO_STRATUM,
        result.halt_message or "",
        sas_value="completed",
        python_value="halted",
    )
    return builder.group_of(
        "Halt", "A halting check stopped the run before any output was produced.", rounded=False
    )


def _program_exports(
    program: str, sas_exports: Mapping[str, pd.DataFrame | None] | None
) -> list[tuple[ExportSpec, pd.DataFrame]]:
    """The supplied SAS exports of ``program``, in export order; unknown file names raise."""
    supplied = dict(sas_exports or {})
    for file_name in supplied:
        sas_export_spec(file_name)  # KeyError for a name the schemas do not define
    pairs = []
    for spec in EXPORTS[program]:
        frame = supplied.get(spec.sas_export_file) if spec.sas_export_file else None
        if frame is not None:
            if not isinstance(frame, pd.DataFrame):
                raise TypeError(f"SAS export {spec.sas_export_file} must be a DataFrame or None")
            pairs.append((sas_export_spec(spec.sas_export_file), frame))
    return pairs


def _compare_export(
    result: ProgramResult, spec: ExportSpec, sas_frame: pd.DataFrame, tolerances: Tolerances
) -> ComparisonGroup:
    """Dispatch one present export by the field classes its schema declares."""
    python_frame = result.datasets.get(spec.name)
    if spec.summary_fields:
        return compare_summary(result.program_id, python_frame, sas_frame, spec, tolerances)
    if spec.glm_fields:
        return compare_estimates(result.program_id, python_frame, sas_frame, spec, tolerances)
    return compare_records(result.program_id, python_frame, sas_frame, spec, tolerances)


def compare_program(
    result: ProgramResult,
    listing_tables: Sequence[ListingTable],
    sas_checkpoints: Sequence[SasCheckpoint],
    sas_exports: Mapping[str, pd.DataFrame | None] | None,
    tolerances: Tolerances,
) -> tuple[ComparisonGroup, ...]:
    """Run every comparison of one program run, in report order (AAP 0.7.3, 0.7.5).

    Args:
        result: The driver's run, or ``ProgramResult.from_halt`` for a halted one.
        listing_tables: ``sas_reference.parse_glm_listing`` of the program's listing.
        sas_checkpoints: ``sas_reference.parse_log_checkpoints`` of the program's log.
        sas_exports: ``sas_reference.load_sas_exports`` of the export directory (all seven
            names, ``None`` where absent); only this program's files are used.
        tolerances: :func:`load_tolerances` of ``tolerances.json``.

    Returns:
        Log checkpoints, ParameterEstimates against the listing, merged ``pval`` against the
        listing, one ``export:<file>`` group per supplied export of the program, then the
        not-compared group. For a halted run: a ``halt`` group with one FAIL row carrying the
        halt message, the checkpoint group over what the run reached, and the not-compared group
        for everything else. Every group except ``halt`` and ``not_compared`` carries
        ``tolerances`` as :attr:`ComparisonGroup.tolerances`.
    """
    if not isinstance(result, ProgramResult):
        raise TypeError(f"compare_program expects a ProgramResult, got {type(result).__name__}")
    program = _check_program(result.program_id)
    exports = _program_exports(program, sas_exports)
    present = [spec.sas_export_file for spec, _ in exports]
    if result.halted:
        return (
            _halt_group(result),
            compare_counts(
                program,
                result.checkpoints,
                sas_checkpoints,
                tolerances,
                halt_message=result.halt_message,
            ),
            not_compared_rows(program, result, present),
        )
    groups = [
        compare_counts(program, result.checkpoints, sas_checkpoints, tolerances),
        compare_listing(program, listing_tables, result, tolerances),
        compare_merged_pval(program, listing_tables, result, tolerances),
    ]
    groups.extend(_compare_export(result, spec, frame, tolerances) for spec, frame in exports)
    groups.append(not_compared_rows(program, result, present))
    return tuple(groups)
