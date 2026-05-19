import dataclasses
from MAPL_PythonBridge.python2fortran.python_fortran_bridge import (
    MAPLPythonFortranBridge,
    MAPL_PYHTON_FORTRAN_BRIDGE,
)
from MAPL_PythonBridge.memory.fortran_python_converter import FortranPythonConverter
from MAPL_PythonBridge.types import CVoidPointer

import numpy as np
import numpy.typing as npt
from typing import Any


@dataclasses.dataclass(frozen=True)
class MAPLGridInfo:
    im: int = 0
    jm: int = 0
    lm: int = 0
    nx: int = 0
    ny: int = 0


class MAPLPyAPI:
    """Public facing MAPL Python API of the original Fortran's MAPL."""

    def __init__(self, mapl_bridge: MAPLPythonFortranBridge, fpy_converter: FortranPythonConverter) -> None:
        self._mapl_bridge = mapl_bridge
        self._fpy_converter = fpy_converter

    def get_pointer(
        self,
        name: str,
        state: CVoidPointer,
        dtype: npt.DTypeLike = np.float32,
        dims: list[int] | None = None,
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

    def get_grid_infos(self, mapl_state: CVoidPointer) -> MAPLGridInfo:
        """Retrieve basic grid information"""

        return MAPLGridInfo(
            self._mapl_bridge.MAPL_GetIM(mapl_state),
            self._mapl_bridge.MAPL_GetJM(mapl_state),
            self._mapl_bridge.MAPL_GetLM(mapl_state),
            self._mapl_bridge.MAPL_GetNX(mapl_state),
            self._mapl_bridge.MAPL_GetNY(mapl_state),
        )

    @property
    def fpy_converter(self) -> FortranPythonConverter:
        """Fortran to Python converter helper"""
        return self._fpy_converter

    @property
    def grid_dims(self) -> list[int]:
        """Local 3D grid dimensions"""
        return [self._fpy_converter.im, self._fpy_converter.jm, self._fpy_converter.lm]


MAPLPy: MAPLPyAPI | None = None


def set_MAPLPy(fpy_converter: FortranPythonConverter) -> None:
    """Initialization of the global MAPLPy accessor"""
    global MAPLPy
    if MAPLPy is not None:
        raise RuntimeError("[MAPLPy] Double initialization is forbidden")
    MAPLPy = MAPLPyAPI(MAPL_PYHTON_FORTRAN_BRIDGE, fpy_converter)


def get_MAPLPy() -> MAPLPyAPI:
    """Retrieve the global MAPLPy accessor"""
    global MAPLPy
    if MAPLPy is None:
        raise RuntimeError("[MAPLPy] Missing initialization")
    return MAPLPy
