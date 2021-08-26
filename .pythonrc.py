import sys

from pathlib import Path


try:
    from ptpython.repl import embed
except ImportError:
    print("ptpython is not available: falling back to standard prompt")
else:
    sys.exit(
        embed(
            history_filename=str(Path.home()) + "/.local/share/ptpython/history",
            globals=globals(),
            locals=locals()
        )
    )
