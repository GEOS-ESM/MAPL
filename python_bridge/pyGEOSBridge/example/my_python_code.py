from pyGEOSBridge import GEOSInterfaceCode, MAPL, get_fpy_converter
import numpy as np


class MyInterfacingCode(GEOSInterfaceCode):
    def __init__(self) -> None:
        pass

    def init(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.init")
        pass

    def run(self, grid_comp, import_state, export_state) -> None:
        print(f"pyIS: {import_state}")
        print("Getting T")

        # TODO: This will be refactored into a single function call
        T_as_voir_ptr = MAPL.MAPL_GetPointer_3D(name="T", state=import_state)
        T_as_float_ptr = get_fpy_converter().cast(np.float32, T_as_voir_ptr)
        T = get_fpy_converter().fortran_to_python(
            T_as_float_ptr,
            dims=[
                get_fpy_converter().im,
                get_fpy_converter().jm,
                get_fpy_converter().lm,
            ],
        )
        print(f"Now running MyInterfacingCode.run: T {T[0, 0, :]}")

    def finalize(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.finalize")


CODE = MyInterfacingCode()
