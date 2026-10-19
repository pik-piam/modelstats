"""The process that reads GDX files for ``_gdx``.

R source: none. ``_gdx`` starts it through a tiny bootstrap that caps the address space before
anything of the package is imported, with the working directory kept off the import path. It
answers pickled requests ``(function name, arguments)`` from stdin with pickled ``("ok", result)``
or ``("error", GdxReadError)`` frames on stdout until stdin closes. Every failure inside the
libraries, whatever its type, is reported as ``GdxReadError``; a crash simply ends the process,
which ``_gdx`` notices and replaces.
"""

from __future__ import annotations

import os
import pickle
import struct
import sys
from typing import IO, Any

_HEADER = struct.Struct(">I")


def write_frame(stream: IO[bytes], payload: bytes) -> None:
    """Write one length-prefixed frame."""
    stream.write(_HEADER.pack(len(payload)) + payload)
    stream.flush()


def read_frame(stream: IO[bytes]) -> bytes | None:
    """Read one length-prefixed frame; ``None`` at end of stream."""
    header = stream.read(_HEADER.size)
    if len(header) < _HEADER.size:
        return None
    (length,) = _HEADER.unpack(header)
    payload = stream.read(length)
    return payload if len(payload) == length else None


def main(argv: list[str] | None = None) -> None:
    """Serve GDX read requests until stdin closes (the memory cap is set by the bootstrap)."""
    del argv
    frames_out = os.fdopen(os.dup(1), "wb")  # the frame channel; anything printed to stdout goes to stderr
    os.dup2(2, 1)
    frames_in = sys.stdin.buffer
    from modelstats import _gdx
    from modelstats.errors import GdxReadError

    while (frame := read_frame(frames_in)) is not None:
        name, arguments = pickle.loads(frame)  # the parent process wrote it
        try:
            result: tuple[str, Any] = ("ok", getattr(_gdx, name)(*arguments))
        except Exception as error:  # any failure of the GAMS libraries is a read error
            result = ("error", GdxReadError(f"{arguments[0]}: {error}"))
        write_frame(frames_out, pickle.dumps(result))


if __name__ == "__main__":
    main()
