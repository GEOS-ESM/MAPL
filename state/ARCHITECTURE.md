# state

**Audience:** coding agents and developers working on MAPL `state/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `state/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Operations on an `ESMF_State` as a whole: evaluate arithmetic expressions
over its fields, build masks from expressions, and filter a state by them.
Used for derived fields in ExtData and similar.

## Build (`state/CMakeLists.txt`)
- Library `MAPL.state_utils`, `TYPE ${MAPL_LIBRARY_TYPE}`.
- Dependencies `MAPL.base MAPL.shared PFLOGGER::pflogger`; links `ESMF::ESMF` PUBLIC.
- Tests: `tests/` (`Test_StateArithmetic.pf`, `Test_StateFilter.pf`, `Test_StateMask.pf`, `state_utils_setup.F90`), when pFUnit is found, `EXCLUDE_FROM_ALL`.

## Contents
| Module (file) | Role |
|---|---|
| `MAPL_StateUtils` (`StateUtils.F90`) | umbrella; re-exports `MAPL_StateMaskMod`, `MAPL_StateArithmeticParserMod`, and `StateFilterItem` renamed `MAPL_StateFilterItem` |
| `MAPL_StateArithmeticParserMod` (`StateArithmeticParser.F90`) | function parser ("Fortran 90 function parser v1.1" in its header) compiling expression strings to bytecode and evaluating them on fields; public `parser_variables_in_expression` and the parser entry points |
| `MAPL_StateMaskMod` (`StateMasking.F90`) | type `StateMask`, built from an expression |
| `MAPL_StateFilter` (`StateFilter.F90`) | type `StateFilterItem` |

It builds on `MAPL_FieldUtils` (`../field_utils/ARCHITECTURE.md`) for field arithmetic.

## Who uses it
- `../gridcomps/ExtData/ExtDataGridCompMod.F90` and `../gridcomps/ExtData2G/` (`ExtDataDerived.F90`, `ExtDataGridCompNG.F90`, `ExtDataOldTypesCreator.F90`) for derived/expression fields.
- Re-exported by `use MAPL` (`../MAPL/ARCHITECTURE.md`).

## Known traps
- The library target is `MAPL.state_utils`, the directory is `state/`.
- The parser is separate from the `base/ArthParser.md` arithmetic parser documented in `../base/`.

## Observations
- Header comments in `StateArithmeticParser.F90` describe the origin of the code as a third-party function parser; licence terms were not checked.
