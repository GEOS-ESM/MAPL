from sysconfig import get_config_var
import cffi

TMPFILEBASE = "mapl_fortan_python_cffi_bridge"

ffi = cffi.FFI()


source = """
from {} import ffi
from datetime import datetime
import MAPL_PythonBridge
import traceback

@ffi.def_extern()
def mapl_fortran_python_bridge_global_initialize_PyHook(IM, JM, LM) -> int:
    try:
        MAPL_PythonBridge.global_initialize(IM, JM, LM)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def mapl_fortran_python_bridge_user_init_PyHook(name, mapl_state, import_state, export_state) -> int:
    try:
        MAPL_PythonBridge.named_init(name, mapl_state, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def mapl_fortran_python_bridge_user_run_PyHook(name, mapl_state, import_state, export_state) -> int:
    try:
        MAPL_PythonBridge.named_run(name, mapl_state, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def mapl_fortran_python_bridge_user_run_with_internal_PyHook(name, mapl_state, import_state, export_state, internal_state) -> int:
    try:
        MAPL_PythonBridge.named_run_with_internal(name, mapl_state, import_state, export_state, internal_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def mapl_fortran_python_bridge_user_finalize_PyHook(name, mapl_state, import_state, export_state) -> int:
    try:
        MAPL_PythonBridge.named_finalize(name, mapl_state, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0
""".format(TMPFILEBASE)

with open("fortran_python_bridge.h") as f:
    data = "".join([line for line in f if not line.startswith("#")])
    data = data.replace("CFFI_DLLEXPORT", "")
    ffi.embedding_api(data)

ffi.set_source(
    TMPFILEBASE,
    '#include "fortran_python_bridge.h"',
    library_dirs=[get_config_var("LIBDIR")],
)

ffi.embedding_init_code(source)  # more code
ffi.compile(target="lib" + TMPFILEBASE + ".*", verbose=True)
