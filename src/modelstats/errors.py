"""Exceptions of the modelstats port."""


class RParityError(Exception):
    """An error path that the R package also takes.

    The message is exactly R's ``conditionMessage`` text for that path (for
    example ``argument is of length zero`` or ``undefined columns selected``)
    so that error goldens produced by the R oracle can be compared verbatim.
    ``call`` optionally carries the deparsed R call of the failing expression
    (like :class:`RWarning`), so that a ``try()`` emulation can print
    ``Error in <call> : <message>`` exactly as R does; without it the text is
    ``Error : <message>``.
    """

    def __init__(self, message: str, *, call: str | None = None) -> None:
        super().__init__(message)
        self.call = call


class RWarning(UserWarning):
    """A warning R raises on this path, deferred like R's ``Warning message:``.

    ``call`` is the deparsed R call and ``text`` the message. The CLI (``rs``) collects
    them with ``warnings.catch_warnings(record=True)`` and prints them as ``Rscript``
    does (``Warning message:`` / ``In <call> :`` / ``  <text>``).
    """

    def __init__(self, call: str, text: str) -> None:
        super().__init__(text)
        self.call = call
        self.text = text
