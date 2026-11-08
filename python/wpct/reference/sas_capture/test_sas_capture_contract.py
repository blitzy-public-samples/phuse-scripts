"""Contract check of the WPCT-F.07.02 SAS capture launcher, which needs no SAS.

``WPCT-F.07.02_capture.sas`` in this folder writes the full-precision WPCT-F.07.02 reference
exports that ``python/wpct/reference/sas_exports/README.md`` specifies. SAS cannot import
``wpct.schemas``, so the launcher's export statements, one ``%wpct_export_csv(file=..., vars=...)``
call per file, restate the file names and columns literally. This module checks that restatement
against ``wpct.schemas``, which stays binding (AAP 0.9.5). It reads the launcher as text and runs
no SAS, so it shows nothing about how the launcher behaves in a SAS session.

The module sits beside the launcher, outside ``testpaths``, so the default ``python -m pytest``
does not collect it. Run it explicitly from ``python/wpct/``::

    python -m pytest reference/sas_capture/test_sas_capture_contract.py
"""

from __future__ import annotations

import re
from pathlib import Path

from wpct import schemas

LAUNCHER = Path(__file__).with_name("WPCT-F.07.02_capture.sas")
EXPORT_MACRO = "wpct_export_csv"

# SAS comments and quoted literals, lexed left to right; an unterminated one runs to the end.
_SOURCE_TOKEN = re.compile(
    r"/\*.*?(?:\*/|\Z)"  # block comment
    r"|%\*(?:'(?:[^']|'')*(?:'|\Z)|\"(?:[^\"]|\"\")*(?:\"|\Z)|[^'\";])*;?"  # macro comment
    r"|'(?:[^']|'')*(?:'|\Z)"  # single-quoted literal, '' escaping a quote
    r"|(?P<double_quoted>\"(?:[^\"]|\"\")*(?:\"|\Z))",  # double-quoted literal, "" escaping
    re.DOTALL,
)
_MACRO_DEFINITION = re.compile(
    rf"%macro\s+{EXPORT_MACRO}\b.*?%mend\s+{EXPORT_MACRO}\s*;", re.IGNORECASE | re.DOTALL
)
_INVOCATION = re.compile(rf"%{EXPORT_MACRO}\s*\((?P<args>[^)]*)\)", re.IGNORECASE)
_ARGUMENT = re.compile(r"(?P<name>\w+)\s*=\s*(?P<value>[^,]*)")


def _export_statements(text: str) -> list[dict[str, str]]:
    """The keyword arguments of every export-macro invocation in launcher source ``text``.

    Block and macro comments, single-quoted literals (where SAS resolves no macro call) and the
    export macro's own definition are removed first; double-quoted literals stay, since SAS
    resolves macro calls inside them. The remaining invocation-shaped source is collected without
    evaluating SAS control flow. Argument names are folded to lower case, as SAS folds them, and
    each value has its blanks and line breaks collapsed to single blanks.
    """
    code = _SOURCE_TOKEN.sub(lambda token: token["double_quoted"] or " ", text)
    code = _MACRO_DEFINITION.sub(" ", code)
    return [
        {
            argument["name"].lower(): " ".join(argument["value"].split())
            for argument in _ARGUMENT.finditer(invocation["args"])
        }
        for invocation in _INVOCATION.finditer(code)
    ]


def _required_columns(file_name: str) -> tuple[str, ...]:
    """The SAS columns of one export, as ``tests/test_sas_exports.py::_sas_columns`` takes them.

    A ParameterEstimates export holds its stratum columns plus the ODS columns
    ``Parameter Estimate StdErr tValue Probt``; the loader derives ``EFFECT`` and ``LEVEL``. Every
    other export holds its spec's columns, the stratum columns among them.
    """
    spec = schemas.sas_export_spec(file_name)
    if spec.glm_fields:
        return (
            *schemas.SAS_EXPORT_STRATUM_COLUMNS[file_name],
            *schemas.SAS_PARAMETER_ESTIMATE_COLUMNS,
        )
    return spec.columns


def test_s02_trn_03_mod_01_mod_02_capture_launcher_exports_07_02_contract() -> None:
    """Every WPCT-F.07.02 export file and column of ``wpct.schemas`` is in the launcher's exports.

    Covers the captures of S02-TRN-03 (the ``css_anadata`` keep list), S02-MOD-01 (the PROC
    SUMMARY statistics), S02-TRN-05 (the merged ``pval``) and S02-MOD-02 (the GLM
    ParameterEstimates). The launcher must exist, and the files it exports must be exactly the
    WPCT-F.07.02 files of ``schemas.SAS_EXPORT_FILES``, each once, so no file is missing, invented
    or taken from WPCT-F.07.07. Each file's ``vars=`` list must hold every required column, and
    extra columns are allowed. File names, like columns, match case-insensitively as whole tokens;
    the loader matches columns the same way.
    """
    assert LAUNCHER.is_file(), f"the capture launcher is missing: {LAUNCHER}"
    # Folded name -> the schema's spelling, which _required_columns and the missing-file
    # diagnostic take.
    expected = {
        file_name.upper(): file_name
        for file_name in schemas.SAS_EXPORT_FILES
        if schemas.sas_export_spec(file_name).program_id == schemas.PROGRAM_07_02
    }
    assert len(expected) > 0, f"wpct.schemas lists no {schemas.PROGRAM_07_02} SAS export file"

    problems: list[str] = []
    # Folded name -> (the launcher's spelling, its vars= value).
    exported: dict[str, tuple[str, str]] = {}
    for position, arguments in enumerate(_export_statements(LAUNCHER.read_text("latin-1")), 1):
        file_name = arguments.get("file", "")
        if not file_name:
            problems.append(f"export statement {position} has no file= argument")
        elif file_name.upper() in exported:
            problems.append(f"{file_name} is exported more than once")
        else:
            exported[file_name.upper()] = (file_name, arguments.get("vars", ""))

    absent = [file_name for key, file_name in expected.items() if key not in exported]
    if absent:
        problems.append(f"no export statement for: {', '.join(absent)}")
    unexpected = [file_name for key, (file_name, _) in exported.items() if key not in expected]
    if unexpected:
        problems.append(
            f"export statements for files outside the {schemas.PROGRAM_07_02} contract: "
            f"{', '.join(unexpected)}"
        )
    if len(exported) != len(expected):
        problems.append(
            f"the launcher exports {len(exported)} files; the {schemas.PROGRAM_07_02} contract "
            f"has {len(expected)}"
        )
    for key, file_name in expected.items():
        if key not in exported:
            continue
        launcher_name, columns = exported[key]
        tokens = set(schemas.upper_columns(columns.split()))
        lacking = [
            column for column in _required_columns(file_name) if column.upper() not in tokens
        ]
        if lacking:
            problems.append(f"{launcher_name}: vars= lacks {' '.join(lacking)}")

    assert not problems, "\n".join(problems)
