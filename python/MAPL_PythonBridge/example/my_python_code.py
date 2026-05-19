from MAPL_PythonBridge import UserCode, get_MAPLPy
import numpy as np


class MyInterfacingCode(UserCode):
    def __init__(self) -> None:
        pass

    def init(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.init")
        pass

    def run(self, grid_comp, import_state, export_state) -> None:
        print("Getting T")

        MAPLPy = get_MAPLPy()

        T = MAPLPy.get_pointer("T", state=import_state, dtype=np.float32, dims=MAPLPy.grid_dims)
        t_before = T[0, 0, 0]
        T[0, 0, 0] = 216.4242

        print(f"Now running MyInterfacingCode.run: T before {t_before}, after {T[0, 0, :]}")

    def run_with_internal(self, grid_comp, import_state, export_state, internal_state) -> None:
        print("Now running MyInterfacingCode.run_with_internal")

    def finalize(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.finalize")


CODE = MyInterfacingCode()
