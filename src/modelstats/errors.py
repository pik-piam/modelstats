"""Exceptions of the modelstats package.

R source: none. R signalled failures with ``stop()``; the command line tools map
``ModelstatsError`` to exit status 1, and the support modules raise its subclasses where the R
code wrapped a call in ``try()`` or let a corrupt file abort the status.
"""

from __future__ import annotations

__all__ = ["ConfigReadError", "GdxReadError", "ModelstatsError", "RDataReadError"]


class ModelstatsError(Exception):
    """Base class of every error raised by modelstats."""


class GdxReadError(ModelstatsError):
    """A GDX file is missing, corrupt or truncated."""


class RDataReadError(ModelstatsError):
    """An ``.rda``/``.Rdata``/``.rds`` file is missing, corrupt or truncated."""


class ConfigReadError(ModelstatsError):
    """A ``config.Rdata`` or ``config.yml`` cannot be read."""
