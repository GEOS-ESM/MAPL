from distutils.sysconfig import get_config_var
import cffi

TMPFILEBASE = "PythonBridge_interface_py"

ffi = cffi.FFI()


source = """
from {} import ffi
from datetime import datetime
import pyGEOSBridge
import traceback

@ffi.def_extern()
def MAPL_PythonBridge_Py_global_initialize(IM, JM, LM) -> int:
    print("Init bridge, Py side", IM, JM, LM)
    try:
        pyGEOSBridge.global_initialize(IM, JM, LM)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def pyGEOSBridge_Py_init(name, grid_comp, import_state, export_state) -> int:
    try:
        pyGEOSBridge.named_init(name, grid_comp, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def pyGEOSBridge_Py_run(name, grid_comp, import_state, export_state) -> int:
    try:
        pyGEOSBridge.named_run(name, grid_comp, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0

@ffi.def_extern()
def pyGEOSBridge_Py_finalize(name, grid_comp, import_state, export_state) -> int:
    try:
        pyGEOSBridge.named_finalize(name, grid_comp, import_state, export_state)
    except Exception as err:
        print("Error in Python:")
        print(traceback.format_exc())
        return -1
    return 0
""".format(TMPFILEBASE)

with open("bridge.h") as f:
    data = "".join([line for line in f if not line.startswith("#")])
    data = data.replace("CFFI_DLLEXPORT", "")
    ffi.embedding_api(data)

ffi.set_source(
    TMPFILEBASE,
    '#include "bridge.h"',
    library_dirs=[get_config_var("LIBDIR")],
)

ffi.embedding_init_code(source)  # more code
ffi.compile(target="lib" + TMPFILEBASE + ".*", verbose=True)
