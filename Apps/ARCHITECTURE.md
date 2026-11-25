# MAPL Apps

**Audience:** coding agents and developers working on MAPL `Apps/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `Apps/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Command-line tools shipped with MAPL: the Automatic Code Generator (ACG) that GEOS components run at build time, a component-stub generator, restart and file utilities, and a few Fortran programs built against the MAPL library.

## Build and install (`Apps/CMakeLists.txt`)
- `file(COPY ...)` at configure time into `${esma_etc}/MAPL`: `MAPL_GridCompSpecs_ACG.py`, `mapl_acg.pl`, `mapl_stub.pl`.
- `install(PROGRAMS ... TYPE SYSCONF)`: `MAPL_GridCompSpecs_ACG.py`, `combine_restarts.py`, `split_restart.py`, `mapl_acg.pl`, `mapl_stub.pl`.
- `install(PROGRAMS ... DESTINATION bin/forcing_converter)`: the five `forcing_converter/*.py` files.
- Executables (`ecbuild_add_executable`, each linking `MAPL`, MPI, ESMF, OpenMP): `Regrid_Util.x`, `time_ave_util.x`, `Comp_Testing_Driver.x`.
- Not installed or compiled by this file: `subset_fields.py`, `MAPL_GridCompSpecs_ACG_writer.py`.

## Contents
| Item | Role |
|---|---|
| `MAPL_GridCompSpecs_ACG.py` | Current ACG: reads a `*_StateSpecs.rc` file, writes Fortran include files (import/export/internal specs, GetPointer, DeclarePointer). |
| `mapl_acg.pl` | Older Perl ACG, reads a "Grid Component Registry" (header comment) and writes code fragments and resource files. |
| `MAPL_GridCompSpecs_ACG_writer.py` | Reverse direction: writes a spec file from `MAPL_Add_STATE_Specs` calls. Documented in [`README_ACG_WRITER.md`](README_ACG_WRITER.md). |
| `mapl_stub.pl` | Creates MAPL no-op grid components (header comment). |
| `combine_restarts.py`, `split_restart.py` | netCDF restart utilities (Python: netCDF4, numpy, argparse; `combine_restarts.py` also yaml). |
| `subset_fields.py` | Subsets a netCDF file by index box (`-x1 -y1 -x2 -y2`). |
| `time_ave_util.F90` | Program `time_ave`: time-averages files using MAPL and `MAPL_FileMetadataUtilsMod`. |
| `Regrid_Util/` | `Regrid_Util.F90` regrids MAPL-IO files between lat-lon, cubed-sphere and tripolar grids via ESMF, driven by command-line options; [`Regrid_Util/README.md`](Regrid_Util/README.md). |
| `forcing_converter/` | Python: `binary_forcing_converter.py` with `ll_`, `cs_`, `tile_formatter.py`, `utilities.py`. |
| `Component_Testing/` | `Comp_Testing_Driver.F90` runs one component standalone from before/after restarts captured with `COMPONENT_TO_RECORD`/`TEST_FRAMEWORK`/`MAPL_GridCapture`; [`Component_Testing/README.md`](Component_Testing/README.md) says it was usable only for GWD and GOCART and was last confirmed for GWD. |

## How components invoke the ACG
- CMake function `mapl_acg(target specs_file <options>)` in `cmake/mapl_acg.cmake` (included from the top `CMakeLists.txt`). It adds a `add_custom_command` running `MAPL_GridCompSpecs_ACG.py` (from `${MAPL_BASE_DIR}/etc` if defined, else `${esma_etc}/MAPL`) in the binary directory, with default output names `<gc>_Import___.h`, `_Export___.h`, `_Internal___.h`, `_GetPointer___.h`, `_DeclarePointer___.h`.
- 16 `CMakeLists.txt` outside MAPL in the fixture `src/` call `mapl_acg (...)` (17 with MAPL's tutorial example under `docs/tutorial`), for example the ocean gridcomps (`GEOS_Ocean_StateSpecs.rc`, `MOM6_GEOSPlug_StateSpecs.rc`, `GEOS_DataSea_StateSpecs.rc`).
- HEMCO_GridComp's `CMakeLists.txt` instead finds `mapl_acg.pl` with `find_file` and runs it in its own custom command (`@GEOSchem_GridComp/HEMCO_GridComp/CMakeLists.txt`).
- Spec-file syntax and generated-file use: tutorial `docs/tutorial/grid_comps/automatic_code_generator_example/`.

## Who uses it
- Fixture: `mapl_acg` as above. `mapl_stub.pl` is referenced only by the MIT ocean build config `ESMA_base.mk` (`STUB = $(ESMABIN)/mapl_stub.pl`).
- *Unverified:* no caller of `Regrid_Util.x`, `time_ave_util.x`, `combine_restarts.py`, `split_restart.py`, `subset_fields.py` or `forcing_converter` was found in `src/Applications` or `src/Components` (greps returned none); they appear to be run by hand or by external scripts.

## Known traps
- Two ACGs exist (Perl and Python) with different input formats; the CMake function uses only the Python one.
- `Regrid_Util.x` defaults to 6 MPI tasks; `NX*NY` must equal the task count; cubed-sphere needs at least 6 (`Regrid_Util/README.md`).
- `subset_fields.py` and the restart scripts are not built from this CMake beyond install (`subset_fields.py` not at all).

## Observations (noted, not fixed)
- `split_restart.py` and `combine_restarts.py` both carry the argparse description "Flatten a lat-lon to 1D", which does not match their names.
- `Component_Testing/README.md` states the framework has limited usability; whether it is still maintained is *Unverified*.
