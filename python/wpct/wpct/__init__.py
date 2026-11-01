"""Python port of the PHUSE Central Tendency programs WPCT-F.07.02 and WPCT-F.07.07.

The ``wpct`` package reproduces, from the repository ADVS transport files, the
intermediate analysis datasets, the PROC SUMMARY statistics and the PROC GLM
``SOLUTION`` tables of ``whitepapers/WPCT/WPCT-F.07.02.sas`` and
``whitepapers/WPCT/WPCT-F.07.07.sas``. It reproduces no box-plot graphics.

The build contract is ``python/wpct/docs/sas_specification.md``. Module
docstrings and test names quote its item IDs: ``S02-*`` for 07.02, ``S07-*``
for 07.07, ``LMM-*`` for ``%derive_lastminmax_measure`` and ``D-*`` for the
SAS-versus-Python default differences the port settles in favour of SAS.

Run ``python -m wpct reproduce`` from ``python/wpct/`` to re-run both programs,
export the intermediate datasets and regenerate the equivalence report.

Importing this package loads no submodule and no third-party library and does
not touch the filesystem; import each submodule as ``wpct.<module>``.
"""

__version__ = "0.1.0"

__all__ = ["__version__"]
