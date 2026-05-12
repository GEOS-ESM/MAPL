from __future__ import annotations

from functools import lru_cache
from math import prod
from types import ModuleType
from typing import Tuple, TypeAlias

import cffi
import numpy as np
import numpy.lib as npl
import numpy.typing as npt

try:
    import cupy as cp
except ModuleNotFoundError:
    cp = None

from _cffi_backend import _CDataBase as CFFIObj


# Dev note: we would like to use cp.ndarray for Device and
# Union of np and cp ndarray for Python but we can't
# because cp might not be importable!
DeviceArray: TypeAlias = npt.NDArray
PythonArray: TypeAlias = npt.NDArray


class NullStream:
    def __init__(self):
        pass

    def synchronize(self):
        pass

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_value, traceback):
        pass


@lru_cache
def _compute_fortran_strides(shape: Tuple[int, ...], dtype: npt.DTypeLike) -> tuple[int, ...]:
    cshp = np.cumprod(shape)
    return np.concatenate(([1], cshp[:-1])) * np.dtype(dtype).itemsize


class FortranPythonConverter:
    """
    Convert Fortran arrays to NumPy and vice-versa.

    The python memory will be laid out Fortran-like,
        e.g. column-order,
        e.g. 3D strides will be [1, dim0, dim0*dim1].
    """

    def __init__(
        self,
        im: int,
        jm: int,
        lm: int,
        numpy_module: ModuleType,
    ):
        # Python numpy-like module is given by the caller leaving
        # optional control of upload/download in the case
        # of GPU/CPU system
        self._target_np = numpy_module

        # Device parameters
        #  Pace targets gpu: we want the Pace layout to be on device
        self._python_targets_gpu = self._target_np == cp
        if self._python_targets_gpu:
            self._stream_A = cp.cuda.Stream(non_blocking=True)
            self._stream_B = cp.cuda.Stream(non_blocking=True)
        else:
            self._stream_A = NullStream()
            self._stream_B = NullStream()
        self._current_stream = self._stream_A

        # Layout & indexing
        self.im, self.jm, self.lm = im, jm, lm

        # cffi init
        self._ffi = cffi.FFI()
        self._TYPEMAP = {
            "float": np.float32,
            "double": np.float64,
            "int": np.int32,
            "_Bool": bool,
            "bool": bool,
        }

    def device_sync(self):
        """Synchronize the working CUDA streams"""
        self._stream_A.synchronize()
        self._stream_B.synchronize()

    def _fortran_to_numpy(
        self,
        fptr: cffi.FFI.CData,
        dims: list[int] | None,
    ) -> np.ndarray:
        """
        Input: Fortran data pointed to by fptr and of shape dim = (i, j, k)
        Output: F-ordered NumPy data of shape (i, j, k) - strides are the same as input
        """
        if not dims:
            dims = [self._im, self._jm, self._lm]
        ftype = self._ffi.getctype(self._ffi.typeof(fptr).item)
        if ftype not in self._TYPEMAP:
            raise ValueError(f"Fortran Python memory converter: cannot convert type {ftype}")
        return np.frombuffer(
            self._ffi.buffer(fptr, prod(dims) * self._ffi.sizeof(ftype)),
            self._TYPEMAP[ftype],
        )

    def _upload_and_transform(
        self,
        host_array: np.ndarray,
        dims: list[int] | None,
    ) -> DeviceArray:
        """Upload to device & transform to Pace compatible layout"""
        device_array = cp.asarray(host_array.reshape(dims, copy=False))
        self._current_stream = self._stream_A if self._current_stream == self._stream_B else self._stream_B
        return device_array

    def _transform_from_fortran_layout(
        self,
        array: PythonArray,
        dims: list[int] | None,
    ) -> PythonArray:
        """Transform from Fortran layout into a Pace compatible layout"""
        if not dims:
            dims = [self._im, self._jm, self._lm]
        trf_array = npl.stride_tricks.as_strided(
            array,
            shape=dims,
            strides=_compute_fortran_strides(tuple(dims), array.dtype),
        )
        return trf_array

    def fortran_to_python(
        self,
        fptr: cffi.FFI.CData,
        dims: list[int] | None,
        *,
        allow_device_transfer: bool = True,
    ) -> PythonArray:
        """Move fortran memory into python space."""
        np_array = self._fortran_to_numpy(fptr, dims)
        if allow_device_transfer and self._python_targets_gpu:
            return self._upload_and_transform(np_array, dims)
        else:
            return self._transform_from_fortran_layout(
                np_array,
                dims,
            )

    def _transform_and_download(self, device_array: DeviceArray, dtype: type) -> np.ndarray:
        host_array = cp.asnumpy(device_array.astype(dtype).flatten(order="F"))
        self._current_stream = self._stream_A if self._current_stream == self._stream_B else self._stream_B
        return host_array

    def _transform_from_python_layout(
        self,
        array: PythonArray,
        dtype: type,
    ) -> np.ndarray:
        """Copy back a numpy array in python layout to Fortran"""

        if self._python_targets_gpu:
            numpy_array = self._transform_and_download(array, dtype)
        else:
            numpy_array = array.astype(dtype).flatten(order="F")
        return numpy_array

    def python_to_fortran(
        self,
        array: PythonArray,
        fptr: cffi.FFI.CData,
        ptr_offset: int = 0,
    ) -> None:
        """
        Input: Fortran data pointed to by fptr and of shape dim = (i, j, k)
        Output: F-ordered NumPy data of shape (i, j, k)
        """
        ftype = self._ffi.getctype(self._ffi.typeof(fptr).item)
        assert ftype in self._TYPEMAP
        dtype = self._TYPEMAP[ftype]
        numpy_array = self._transform_from_python_layout(array, dtype)
        self._ffi.memmove(fptr + ptr_offset, numpy_array, 4 * numpy_array.size)

    def cast(self, dtype: npt.DTypeLike, void_ptr: CFFIObj) -> CFFIObj:
        if dtype in [int, np.int64]:  # type: ignore
            return self._ffi.cast("int64_t*", void_ptr)
        elif dtype in [np.int32]:  # type: ignore
            return self._ffi.cast("int32_t*", void_ptr)
        elif dtype in [np.float32]:  # type: ignore
            return self._ffi.cast("float*", void_ptr)
        elif dtype in [float, np.float64]:  # type: ignore
            return self._ffi.cast("double*", void_ptr)
        elif dtype in [bool]:
            return self._ffi.cast("bool*", void_ptr)

        raise NotImplementedError(f"Cannot cast void* to C-equivalent of {dtype}")


def get_fortran_python_converter(im: int, jm: int, lm: int) -> FortranPythonConverter:
    return FortranPythonConverter(im, jm, lm, np)
