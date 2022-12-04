from os import environ
from pathlib import Path

if "PTPYTHON_CONFIG_HOME" not in environ:
    environ["PTPYTHON_CONFIG_HOME"] = str(Path.home() / ".config/ptpython")

try:
    from ptpython.repl import embed
except ImportError:
    print("ptpython is not available: falling back to standard prompt")
else:
    import logging
    import sys

    logger = logging.getLogger()
    logger.setLevel(10)

    # The following prevent ptpython's buggy (?) logging filling the repl :)
    parso_logger1 = logging.getLogger("parso.python.diff")
    parso_logger1.setLevel(100)
    parso_logger2 = logging.getLogger("parso.cache")
    parso_logger2.setLevel(100)
    asyncio_logger = logging.getLogger("asyncio")
    asyncio_logger.setLevel(100)

    sys.exit(
        embed(
            history_filename=str(
                Path.home() / ".local/share/ptpython/history"
            ),
            globals=globals(),
            locals=locals(),
            patch_stdout=True,
            title="repl"
        )
    )
