"""modelstats: run analysis tools for REMIND and MAgPIE.

The Python port of the R package of the same name (``DESCRIPTION``, version mirrored in
``__version__``). The package grows phase by phase (``migration/06-user-requests.md`` section 7):
the pure helpers (phase 1), the run inspection layer and the public ``Run`` object (phase 2), the
tables (phase 3), the ``rs`` command line (phase 4) and the REMIND-only automated model tests
(phase 5).
"""

__version__ = "0.31.0"

__all__ = ["__version__"]
