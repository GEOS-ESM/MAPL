from pyGEOSBridge.mapl.mapl_bridge import MAPLBridge
from pyGEOSBridge.memory.fortran_python_converter import FortranPythonConverter
from pyGEOSBridge.types import MAPLState

import numpy as np
from typing import Tuple


class MAPLPyAPI:
    def __init__(self, mapl_bridge: MAPLBridge, fpy_converter: FortranPythonConverter) -> None:
        self._mapl_bridge = mapl_bridge
        self._fpy_converter = fpy_converter

    def get_pointer(
        self,
        name: str,
        state: MAPLState,
        dtype: np.dtype = np.float32,
        dims: Tuple[int, ...] | None = None,
    ) -> np.ndarray:
        voir_ptr = self._mapl_bridge.MAPL_GetPointer_3D(name=name, state=state)
        casted_ptr = self._fpy_converter.cast(dtype, voir_ptr)
        if dims is not None:
            dims = self.grid_dims
        array = self._fpy_converter.fortran_to_python(casted_ptr, dims)

        return array

    @property
    def grid_dims(self) -> Tuple[int, int, int]:
        return [self._fpy_converter.im, self._fpy_converter.jm, self._fpy_converter.lm]


MAPLPy: MAPLPyAPI | None = None


def set_MAPLPy(mapl_bridge: MAPLBridge, fpy_converter: FortranPythonConverter) -> None:
    global MAPLPy
    if MAPLPy is not None:
        raise RuntimeError("Double initialization")
    MAPLPy = MAPLPyAPI(mapl_bridge, fpy_converter)
    print("MAPLPy is set")


def get_MAPLPy() -> MAPLPyAPI:
    global MAPLPy
    return MAPLPy
