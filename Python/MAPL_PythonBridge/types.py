from _cffi_backend import _CDataBase as CFFIObj
from typing import TypeAlias
import cffi

FFI = cffi.FFI()

MAPLState: TypeAlias = CFFIObj
CVoidPointer: TypeAlias = CFFIObj

CData: TypeAlias = FFI.CData
