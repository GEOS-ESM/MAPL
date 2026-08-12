# Generic Fortran to Python bridge with limited MAPL support

⚠️⚠️⚠️ This is an experimental feature it could disappear without warnings - please contact florian.g.deconinck - at - nasa.gov to discuss your needs ⚠️⚠️⚠️

This code aims at providing a way to execute Python code from GEOS and executing _some_ `MAPL` code.

The target use case is:

- No infrastructure code in python: GridComp and their setup remains in Fortran
- Tight numerics: the system is not optimize for an heavy runtime code (e.g. called many time a timestamp). One call per timestep is optimal.
- Required input & outputs are _simple_ buffers or scalars reachable via MAPL (with `MAPL_GetPointer`, `MAPL_GetResource`...)

## Usage

### Define Python code

Your Python code must a class deriving from `MAPL_PythonBridge.UserCode` and redefine `init`, `run`, `run_with_internal` and `finalize`. You then must instantiate this class in a `CODE` global variable for the system to properly find it.

E.g.

```python
# This code would live in an hypothetical mypackage.my_code
from MAPL_PythonBridge import UserCode


class MyInterfacingCode(UserCode):
    def __init__(self) -> None:
        pass

    def init(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.init")

    def run(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.run")

        # Grab the system-wide MAPL API accessor
        MAPLPy = get_MAPLPy()

        # Retrieve T like you would in Fortran
        # - for implementation reason, you have to give the dimensions. The API
        #   give a shortcut to the grid dimensions in `MAPLPy.grid_dims`
        # - if the pointer is "not associated" it will be returned as a None value
        T = MAPLPy.get_pointer("T", state=import_state, dtype=np.float32, dims=MAPLPy.grid_dims)

    def run_with_internal(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.run_with_internal")

    def finalize(self, grid_comp, import_state, export_state) -> None:
        print("Now running MyInterfacingCode.finalize")


CODE = MyInterfacingCode()
```

### Call Python code from Fortran

In the fortran, you can call your python code by giving:

- the package path that python should load as a C string
- the `ESMF_State` as a C pointer

E.g.

```fortran
    call MAPL_pybridge_gcinit( "mypackage.my_code", GC, IMPORT, EXPORT )
```

## Build

The system is gatekeep by a CMake option: `-DBUILD_PYTHONBRIDGE`. This will defined a `PYTHONBRIDGE_INTEGRATION` available at compile-time.

Good practice is to surround Fortran code with this `ifdef` as shown above.

The Python code relies on you code accessible via a classic `import` paradigm. Please make sure that `PYTHONPATH` or alternatives are properly set.
