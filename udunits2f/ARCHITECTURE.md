# udunits2f

**Audience:** coding agents and developers working on MAPL `udunits2f/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `udunits2f/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Fortran bindings (via `iso_c_binding`) to the C UDUNITS2 library: parse unit
strings, test whether two units are convertible, and convert values.

## Build (`udunits2f/CMakeLists.txt`)
- Library `udunits2f` (`esma_set_this (OVERRIDE udunits2f)`), `TYPE SHARED` (always shared, ignoring `BUILD_SHARED_MAPL`).
- Dependency `MAPL.shared`; `find_package(udunits REQUIRED)` and `find_package(EXPAT REQUIRED)`, linked PUBLIC as `udunits::udunits` and `EXPAT::EXPAT`.
- Sources: `CptrWrapper.F90 UDSystem.F90 udunits2f.F90 encoding.F90 interfaces.F90 status_codes.F90 ut_set_ignore_error_message_handler.c`. `error_handling.h` is an include.
- Tests: `tests/` (`Test_UDSystem.pf`, `Test_udunits2f.pf`), added when pFUnit is found, `EXCLUDE_FROM_ALL`.

## Contents
| Module | Role |
|---|---|
| `udunits2f` | umbrella; re-exports the four modules below |
| `ud2f_interfaces` | `bind(C)` interfaces to UDUNITS2 |
| `ud2f_UDSystem` | Fortran type wrapping a unit system |
| `ud2f_encoding`, `ud2f_status_codes` | character-encoding constants and UDUNITS status codes |
| `ud2f_CptrWrapper` | C-pointer wrapper type |
| `ut_set_ignore_error_message_handler.c` | C helper to silence UDUNITS' own error printing |

## Who uses it
- `../base/ApplicationSupport.F90`: `use udunits2f, initialize_udunits => initialize, finalize_udunits => finalize` (start and stop of the unit system with the application).
- `../vertical/VerticalCoordinate.F90` and `VerticalRegridConserveInterface.F90`: `UDUNITS_are_convertible` for unit checks (see `../vertical/ARCHITECTURE.md`).
- Fixture: not searched in full; the library is reached through MAPL.

## Known traps
- A separate shared library with its own link line (UDUNITS and EXPAT), unlike the other MAPL libraries.
- `initialize`/`finalize` must be called by the application; they are renamed on import in `ApplicationSupport.F90`.

## Observations
- Not re-exported by `use MAPL` (see `../MAPL/ARCHITECTURE.md`).
