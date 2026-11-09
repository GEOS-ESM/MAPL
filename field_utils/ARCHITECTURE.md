# field_utils

**Audience:** coding agents and developers working on MAPL `field_utils/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `field_utils/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Arithmetic on `ESMF_Field`s: BLAS-like operations, unary functions, binary
operations, pointer access and conformability checks.

## Build (`field_utils/CMakeLists.txt`)
- Library `MAPL.field_utils`, `TYPE ${MAPL_LIBRARY_TYPE}`; dependencies `MAPL.shared PFLOGGER::pflogger`; links `ESMF::ESMF` PUBLIC.
- Compiled: `FieldUtils.F90 FieldBLAS.F90 FieldPointerUtilities.F90 FieldUtilities.F90 FieldUnaryFunctions.F90 FieldBinaryOperations.F90`.
- Present but **not compiled** (not in `srcs`): `GeomManager.F90`, `MaplGeom.F90`, `Regridder.F90`, `EsmfRegridder.F90`, `StateSupplement.F90`, `FieldBLAS_IntrinsicFunctions.F90`. The templates `FieldUnaryFunctionTemplate.H`, `FieldBinaryOperatorTemplate.H` and `function_overload.macro`/`undo_function_overload.macro` are included by the compiled unary/binary files.
- Tests: `tests/` (`Test_FieldBLAS.pf`, `Test_FieldArithmetic.pf`, `field_utils_setup.F90`), when pFUnit is found, `EXCLUDE_FROM_ALL`.

## Contents
| Module | Role |
|---|---|
| `MAPL_FieldUtils` | umbrella; re-exports the five below |
| `mapl_FieldBLAS` | `FieldSCAL`, `FieldAXPY`, `FieldGEMV`, `FieldConvertPrec`, `FieldSpread`, `FieldsAreConformable` |
| `MAPL_FieldUnaryFunctions` | elementwise functions generated from the template |
| `MAPL_FieldBinaryOperations` | elementwise binary operators generated from the template |
| `MAPL_FieldUtilities` | field helpers |
| `MAPL_FieldPointerUtilities` | typed pointer access to field data |
| (not built) `mapl_GeomManager`, `mapl_MaplGeom`, `mapl_Regridder`, `mapl_EsmfRegridder` | an abstract geometry and regridder design |

## Who uses it
- `../state/` (all state modules `use MAPL_FieldUtils`).
- `../gridcomps/ExtData2G/ExtDataBracket.F90` and `ExtDataGridCompNG.F90`; `../base/Base/Base_Base_implementation.F90`.
- Re-exported by `use MAPL` (`../MAPL/ARCHITECTURE.md`).
- The base directory has its own `RegridderSpec`/`MAPL_TransposeRegridder` modules (`../base/ARCHITECTURE.md`) that are separate from the uncompiled `mapl_Regridder` here.
- Fixture: no direct use of `MAPL_FieldUtils` found in the sampled search.

## Known traps
- The regridder and geometry files look like part of this library but are not built; names `mapl_Regridder` and `mapl_MaplGeom` are not available from it.
- Module `FieldBLASIntrinicFunctions` (sic, file not built) is misspelled.

## Observations
- `StateSupplement.F90`, also not built, deals with `ESMF_State`, not fields.
