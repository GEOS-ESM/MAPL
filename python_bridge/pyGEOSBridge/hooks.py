import importlib
import sys
from pyGEOSBridge import GEOSInterfaceCode

from pyGEOSBridge.types import FFI
from pyGEOSBridge.memory_ingest import FortranPythonConversion

import numpy as np

# - - - DSL Grid Comp Interfacing - - - #

FPY_CONVERTER: FortranPythonConversion | None = None


def get_fpy_converter() -> FortranPythonConversion:
    global FPY_CONVERTER
    if FPY_CONVERTER is None:
        raise ValueError("Fortran<>Python converter is None - Initialization failed.")
    return FPY_CONVERTER


def global_initialize(IM, JM, LM):
    print(f"global_initialize: Initialize -> {IM}x{JM}x{LM}")
    global FPY_CONVERTER
    FPY_CONVERTER = FortranPythonConversion(IM, JM, LM, np)


# - - - DSL Grid Comp Interfacing - - - #


def _get_code_object_from_package_name(c_package_name: FFI.CData) -> GEOSInterfaceCode:
    package_name = FFI.string(c_package_name).decode("utf-8").rstrip()
    print(f"Retrieving package {package_name}")

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


def named_init(c_package_name: FFI.CData, grid_comp, import_state, export_state):
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.init(grid_comp, import_state, export_state)


def named_run(c_package_name: FFI.CData, grid_comp, import_state, export_state):
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.run(grid_comp, import_state, export_state)


def named_finalize(c_package_name: FFI.CData, grid_comp, import_state, export_state):
    CODE = _get_code_object_from_package_name(c_package_name)
    CODE.finalize(grid_comp, import_state, export_state)
