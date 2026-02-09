import cffi
import os
from pyGEOSBridge.types import MAPLState, CVoidPointer, FFI
from typing import Any
import numpy as np
import numpy.typing as npt
import platform

ESMF_MAXSTR = 256


class MAPLBridge:
    """Pure C bridge to Fortran forwarded MAPL API. See `backward_bridge_to_MAPL.F90`
    for Fortran exposure.

    This should remain as close to the metal as possible. User-facing API to be (re)define
    int `maplpy`
    """

    def __init__(self, ffi: cffi.FFI) -> None:
        # We leverage an environment variable to know where to look for the bridge
        # library
        geos_dir = os.getenv("GEOSDIR", "Not found")
        if geos_dir == "Not found":
            raise RuntimeError(
                "[MAPLPyish] Libary loads require a GEOSDIR environment variable"
                "pointing to the install directory of GEOS."
            )

        # FFI & C library setup
        # TODO: we should be using the out-of-line API system and link to .a
        # that way we don't have to rely on a shady .so load
        self.ffi = ffi
        if platform.system() == "Linux":
            ext = "so"
        elif platform.system() == "Darwin":
            ext = "dylib"
        else:
            raise SystemError(f"MAPLPyish unavailable on {platform.system()}")
        self.mapl_c_bridge = self.ffi.dlopen(f"{geos_dir}/lib/libMAPL.python_bridge.{ext}")

        # We use CFFI ABI mode, so we need to describe each function cdef
        # to the system

        # ESMF_AttributeGet
        self.ffi.cdef(
            "int MAPLPy_ESMF_AttributeGet_1D_int(void* esmf_state_c_ptr, char* name_c_ptr, int name_len);"
        )

        # MAPLPy_ESMF_MethodExecute
        self.ffi.cdef(
            "void MAPLPy_ESMF_MethodExecute(void* esmf_state_c_ptr, char* label_c_ptr, int label_len);"
        )

        # MAPLpy_GetPointer_via_ESMFAttr
        self.ffi.cdef(
            "void* MAPLpy_GetPointer_via_ESMFAttr(void* esmf_state_c_ptr, char* name_c_ptr, int name_len);"
        )

        # MAPLpy_GetPointer
        self.ffi.cdef(
            "void* MAPLpy_GetPointer_2D(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, bool alloc);"
        )
        self.ffi.cdef(
            "void* MAPLpy_GetPointer_3D(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, bool alloc);"
        )

        # MAPL_GetResource
        self.ffi.cdef(
            "bool MAPLpy_GetResource_Bool"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, bool default_value"
            ");"
        )
        self.ffi.cdef(
            "int32_t MAPLpy_GetResource_Int32"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, int32_t default_value"
            ");"
        )
        self.ffi.cdef(
            "int64_t MAPLpy_GetResource_Int64"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, int64_t default_value"
            ");"
        )
        self.ffi.cdef(
            "float MAPLpy_GetResource_Float"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, float default_value"
            ");"
        )
        self.ffi.cdef(
            "double MAPLpy_GetResource_Double"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, double default_value"
            ");"
        )

        self.ffi.cdef("void* MAPLpy_ESMF_TimeIntervalGet(void* esmf_time_state_c_ptr);")

        self.ffi.cdef(
            "bool MAPLpy_GetPointer_2D_associated"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, bool alloc"
            ");"
        )

        self.ffi.cdef(
            "bool MAPLpy_GetPointer_3D_associated"
            "(void* esmf_state_c_ptr, char* name_c_ptr, int name_len, bool alloc"
            ");"
        )

        self.ffi.cdef("char* MAPLpy_ESMF_GridCompGetName(void* c_grid_comp);")

    def __del__(self):
        self.ffi.dlclose(self.mapl_c_bridge)

    def ESMF_AttributeGet(self, state: MAPLState, name: str) -> Any:
        # TODO: depending on value type, redirect to correct bridge function
        return self.mapl_c_bridge.MAPLPy_ESMF_AttributeGet_1D_int(  # type: ignore
            state, self.ffi.new("char[]", name.encode()), len(name)
        )

    def ESMF_MethodExecute(self, state: MAPLState, label: str) -> Any:
        self.mapl_c_bridge.MAPLPy_ESMF_MethodExecute(  # type: ignore
            state, self.ffi.new("char[]", label.encode()), len(label)
        )

    def MAPL_GetPointer_via_ESMFAttr(
        self,
        state: MAPLState,
        name: str,
    ) -> CVoidPointer:
        return self.mapl_c_bridge.MAPLpy_GetPointer_via_ESMFAttr(  # type: ignore
            state, self.ffi.new("char[]", name.encode()), len(name)
        )

    def MAPL_GetPointer_2D(self, state: MAPLState, name: str, alloc: bool = False) -> CVoidPointer:
        return self.mapl_c_bridge.MAPLpy_GetPointer_2D(  # type: ignore
            state, self.ffi.new("char[]", name.encode()), len(name), alloc
        )

    def MAPL_GetPointer_3D(self, state: MAPLState, name: str, alloc: bool = False) -> CVoidPointer:
        return self.mapl_c_bridge.MAPLpy_GetPointer_3D(  # type: ignore
            state, self.ffi.new("char[]", name.encode()), len(name), alloc
        )

    def MAPL_GetResource(
        self,
        state: MAPLState,
        name: str,
        default: npt.DTypeLike | bool,
    ) -> Any:
        dtype = type(default)
        if dtype in [int, np.int64]:  # type: ignore
            return self.mapl_c_bridge.MAPLpy_GetResource_Int64(  # type: ignore
                state, self.ffi.new("char[]", name.encode()), len(name), default
            )
        elif dtype in [np.int32]:  # type: ignore
            return self.mapl_c_bridge.MAPLpy_GetResource_Int32(  # type: ignore
                state, self.ffi.new("char[]", name.encode()), len(name), default
            )
        elif dtype in [np.float32]:  # type: ignore
            return self.mapl_c_bridge.MAPLpy_GetResource_Float(  # type: ignore
                state, self.ffi.new("char[]", name.encode()), len(name), default
            )
        elif dtype in [float, np.float64]:  # type: ignore
            return self.mapl_c_bridge.MAPLpy_GetResource_Double(  # type: ignore
                state, self.ffi.new("char[]", name.encode()), len(name), default
            )
        elif dtype in [bool]:
            return self.mapl_c_bridge.MAPLpy_GetResource_Bool(  # type: ignore
                state, self.ffi.new("char[]", name.encode()), len(name), default
            )
        raise NotImplementedError(f"MAPL_GetResource for type {dtype} not implemented.")

    def ESMF_TimeIntervalGet(self, time_state: MAPLState) -> np.float64:
        return self.mapl_c_bridge.MAPLpy_ESMF_TimeIntervalGet(time_state)  # type: ignore

    def ESMF_GridCompGetName(self, state: MAPLState) -> str:
        cdata_string = self.mapl_c_bridge.MAPLpy_ESMF_GridCompGetName(state)
        byte_string = self.ffi.string(cdata_string)
        return byte_string.decode("utf-8").rstrip()

    def associated_2d(self, state: MAPLState, name: str, alloc: bool = False) -> bool:
        return self.mapl_c_bridge.MAPLpy_GetPointer_2D_associated(  # type: ignore
            state,
            self.ffi.new("char[]", name.encode()),
            len(name),
            alloc,
        )

    def associated_3d(self, state: MAPLState, name: str, alloc: bool = False) -> bool:
        return self.mapl_c_bridge.MAPLpy_GetPointer_3D_associated(  # type: ignore
            state,
            self.ffi.new("char[]", name.encode()),
            len(name),
            alloc,
        )


MAPL_BRIDGE = MAPLBridge(FFI)
