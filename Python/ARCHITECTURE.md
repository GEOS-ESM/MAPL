# MAPL Python

**Audience:** coding agents and developers working on MAPL `Python/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `Python/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Two Python packages and a Fortran module that lets a Fortran gridded component call Python code. The bridge is marked experimental in [`README.md`](README.md) ("could disappear without warnings").

## Build (`Python/CMakeLists.txt`)
- Library `MAPL.python_bridge` (`esma_set_this(OVERRIDE MAPL.python_bridge)`), `TYPE SHARED`, always built from `PythonBridge.F90`; dependencies `MAPL.base MAPL.generic MAPL.shared ESMF::ESMF`.
- Option `BUILD_PYTHONBRIDGE` (documented in `README.md`; set by the user at configure time, not defined in MAPL's CMake or elsewhere in the fixture). When on: adds `MAPL_PythonBridge/fortran2python/fortran_python_bridge.{F90,c}` and `python2fortran/python_fortran_bridge.F90`, `add_definitions(-DPYTHONBRIDGE_INTEGRATION)`, requires MPI and Python3, and links the `PythonBridge_interface_py` INTERFACE target.
- A custom command runs `mpiexec -n 1 python fortran_python_bridge.py` to produce `libmapl_fortan_python_cffi_bridge.so` and a header (cffi); a POST_BUILD step copies the library to `${CMAKE_INSTALL_PREFIX}/lib`.
- Always installed: `MAPL/` and `MAPL_PythonBridge/` to `lib/Python`.
- `add_subdirectory(Python)` is unconditional in the top `CMakeLists.txt`.

## Contents
| Path | Role |
|---|---|
| `PythonBridge.F90` | `module MAPL_PythonBridge`; all functional code is under `#ifdef PYTHONBRIDGE_INTEGRATION` (module comment); without the option it compiles to stubs. |
| `MAPL_PythonBridge/fortran2python/` | cffi build script and C/Fortran glue: Fortran calls into Python. |
| `MAPL_PythonBridge/python2fortran/` | Fortran side exposing `MAPL` calls back to Python (for example `GetPointer`, `GetResource` per `README.md`). |
| `MAPL_PythonBridge/{user_code,hooks,types,memory}.py`, `example/` | `UserCode` base class (`init`, `run`, `run_with_internal`, `finalize`), a `CODE` global instance, type helpers. |
| `MAPL/` | Pure-Python utilities (`Date.py`, `config.py`, `eta.py`, `history.py`, `job.py`, `run.py`, `exp.py`, ...); independent of the bridge. |

## Who uses it
- Fixture: pyMoist inside GEOSmoist_GridComp imports `get_MAPLPy`, `UserCode` from `MAPL_PythonBridge` (`GEOSmoist_GridComp/pyMoist/pyMoist/fortran/geos_pymoist.py`, `.../param_interfaces/convection/GF2020_interface.py`, `UW_interface.py`).
- *Unverified:* which Fortran components call `MAPL_PythonBridge` routines; not searched beyond the pyMoist Python side.

## Known traps
- Intended use (`README.md`): one call per timestep, simple buffers or scalars; not for heavy per-call work.
- The bridge library is built by running Python under `mpiexec` (comment in `CMakeLists.txt`: needs compute node on some systems).
- The typo `fortan` in library and header names is as in the CMake.

## Observations (noted, not fixed)
- `README.md` says the bridge is gated by `-DBUILD_PYTHONBRIDGE`; the `MAPL.python_bridge` library exists either way.
