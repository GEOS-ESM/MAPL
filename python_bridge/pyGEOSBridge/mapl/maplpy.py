from pyGEOSBridge.mapl.mapl_bridge import MAPLBridge
from pyGEOSBridge.memory.fortran_python_converter import FortranPythonConverter
from pyGEOSBridge.types import CVoidPointer

import numpy as np
import numpy.typing as npt
from typing import Any, Tuple


class MAPLPyAPI:
    """Public facing MAPL Python API of the original Fortran's MAPL."""

    def __init__(self, mapl_bridge: MAPLBridge, fpy_converter: FortranPythonConverter) -> None:
        self._mapl_bridge = mapl_bridge
        self._fpy_converter = fpy_converter

    def get_pointer(
        self,
        name: str,
        state: CVoidPointer,
        dtype: npt.DTypeLike = np.float32,
        dims: Tuple[int, ...] | None = None,
        alloc: bool = False,
    ) -> np.ndarray | None:
        """Equivalent of Fortran's `MAPL_GetPointer`

        Args:
            name: string describing the pointer in the state
            state: MAPL or ESMF state - expressed as C void*
            dtype: data type of pointer (default to `np.float32`)
            dims: dimensions of buffer, `None` will default to the full 3D grid
            alloc: equivalent of MAPL alloc on the Fortran side
        """
        if dims is None:
            dims = self.grid_dims
        if len(dims) == 3:
            is_associated = self._mapl_bridge.associated_3d(name=name, state=state, alloc=alloc)
            if not is_associated:
                return None
            voir_ptr = self._mapl_bridge.MAPL_GetPointer_3D(name=name, state=state, alloc=alloc)
        elif len(dims) == 2:
            is_associated = self._mapl_bridge.associated_2d(name=name, state=state, alloc=alloc)
            if not is_associated:
                return None
            voir_ptr = self._mapl_bridge.MAPL_GetPointer_2D(name=name, state=state, alloc=alloc)
        else:
            raise TypeError(
                f"[MAPLPy] Fields of {len(dims)} dimensions are not yet supported. Contact the team."
            )
        casted_ptr = self._fpy_converter.cast(dtype, voir_ptr)
        array = self._fpy_converter.fortran_to_python(casted_ptr, dims)

        return array

    def get_resource(self, name: str, state: CVoidPointer, default: npt.DTypeLike | bool) -> Any:
        """Equivalent to MAPL_GetResource

        Args:
            name: string describing the pointer in the state
            state: MAPL or ESMF state - expressed as C void*
            dtype: expected data type of resource
            default: default value if resource does not exist on the state
        """
        return self._mapl_bridge.MAPL_GetResource(state=state, name=name, default=default)

    @property
    def grid_dims(self) -> Tuple[int, int, int]:
        return [self._fpy_converter.im, self._fpy_converter.jm, self._fpy_converter.lm]


MAPLPy: MAPLPyAPI | None = None


def set_MAPLPy(mapl_bridge: MAPLBridge, fpy_converter: FortranPythonConverter) -> None:
    """Initialization of the global MAPLPy accessor"""
    global MAPLPy
    if MAPLPy is not None:
        raise RuntimeError("[MAPLPy] Double initialization is forbidden")
    MAPLPy = MAPLPyAPI(mapl_bridge, fpy_converter)


def get_MAPLPy() -> MAPLPyAPI:
    """Retrieve the global MAPLPy accessor"""
    global MAPLPy
    if MAPLPy is None:
        raise RuntimeError("[MAPLPy] Missing initialization")
    return MAPLPy
