"""Exceptions of the modelstats port."""


class RParityError(Exception):
    """An error path that the R package also takes.

    The message is exactly R's ``conditionMessage`` text for that path (for
    example ``argument is of length zero`` or ``undefined columns selected``)
    so that error goldens produced by the R oracle can be compared verbatim.
    """
