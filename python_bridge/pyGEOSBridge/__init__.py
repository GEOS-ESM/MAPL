"""Python GEOS bridge from Fortran

See README.md for informations.
"""

from .geos_interface_code import GEOSInterfaceCode  # isort: skip
from .hooks import global_initialize
from .hooks import named_init, named_run, named_finalize
from .mapl import get_MAPLPy

__all__ = [
    "global_initialize",
    #
    "named_init",
    "named_run",
    "named_finalize",
    "get_fpy_converter",
    #
    "GEOSInterfaceCode",
    #
    "get_MAPLPy",
]
