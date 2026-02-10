import importlib
import sys
from pyGEOSBridge import GEOSInterfaceCode

from pyGEOSBridge.types import FFI, CVoidPointer
from pyGEOSBridge.memory import get_fortran_python_converter
from pyGEOSBridge.mapl import set_MAPLPy

# See file for explanation
from pyGEOSBridge.ieee_import_bypass import *  # noqa: F403

# - - - DSL Grid Comp Interfacing - - - #


def global_initialize(IM, JM, LM):
    """Initialize the global MAPLPy object"""
    from pyGEOSBridge.mapl.mapl_bridge import MAPL_BRIDGE  # Spin the MAPL backbridge

    fpy_converter = get_fortran_python_converter(IM, JM, LM)
    set_MAPLPy(MAPL_BRIDGE, fpy_converter)


# - - - DSL Grid Comp Interfacing - - - #


def _get_code_object_from_package_name(c_package_name: FFI.CData) -> GEOSInterfaceCode:
    """Dynamically load the user module and return the `CODE` variable.

    Args:
        c_package_name: Package name in the python format, e.g. `mypackage.module` which
            _must_ contain a global `CODE` object
    """
    package_name = FFI.string(c_package_name).decode("utf-8").rstrip()

    try:
        user_module = importlib.import_module(package_name)
    except ModuleNotFoundError:
        raise ModuleNotFoundError(f"Module {package_name} could not be found in any of the paths: {sys.path}")

    try:
        CODE = getattr(user_module, "CODE")
    except AttributeError:
        raise AttributeError(f"Module {package_name} doesn't have a CODE object")

    if not isinstance(CODE, GEOSInterfaceCode):
        raise TypeError(f"CODE object from module {package_name} does not derive from GridCompInterface")

    return CODE


def named_init(
    c_package_name: FFI.CData,
    mapl_state: CVoidPointer,
    import_state: CVoidPointer,
    export_state: CVoidPointer,
):
    """Python hook for Fortran's `gc_init`."""
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.init(mapl_state, import_state, export_state)


def named_run(
    c_package_name: FFI.CData,
    mapl_state: CVoidPointer,
    import_state: CVoidPointer,
    export_state: CVoidPointer,
):
    """Python hook for Fortran's `gc_run`."""
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.run(mapl_state, import_state, export_state)


def named_finalize(
    c_package_name: FFI.CData,
    mapl_state: CVoidPointer,
    import_state: CVoidPointer,
    export_state: CVoidPointer,
):
    """Python hook for Fortran's `gc_finalize`."""
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.finalize(mapl_state, import_state, export_state)
