# vertical

**Audience:** coding agents and developers working on MAPL `vertical/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `vertical/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Vertical coordinates and vertical regridding: describing a file's or model's
vertical coordinate, remapping between pressure-level sets (eta to eta), and
conservative remapping of mixing ratios and emissions.

## Build (`vertical/CMakeLists.txt`)
- Library `MAPL.vertical`, `TYPE ${MAPL_LIBRARY_TYPE}`.
- Dependencies: `MAPL.shared MAPL.base MAPL.pfio PFLOGGER::pflogger`; links `ESMF::ESMF` PUBLIC.
- No tests directory. Linked into the `MAPL` umbrella library, but not re-exported by `use MAPL`.

## Contents
| Module (file) | Role |
|---|---|
| `VerticalCoordinateMod` (`VerticalCoordinate.F90`) | type `VerticalCoordinate` and constructors/kinds `model_pressure`, `simple_coord`, `no_coord`, `fixed_height`, `fixed_pressure`; reads coordinate metadata (uses pFIO file metadata, udunits2f) |
| `MAPL_Eta2EtaMod` (`Eta2Eta.F90`) | `gmap`: remap between two sets of layer edges in log-pressure |
| `VerticalRegridConserveInterfaceMod` | `vremap_conserve_mass_mixing`, `vremap_conserve_emission`, `vremap_conserve_vol_mixing`; selectors `mass_mixing`, `volume_mixing`, `emission`; uses `MAPL_GRAV`, `MAPL_AIRMW`, `MAPL_H2OMW` |
| `VerticalRegridUtilitiesMod` | `check_conservation`; uses `MAPL_GRAV` |

## Who uses it
- In MAPL only `../gridcomps/ExtData2G/` (`ExtDataGridCompNG.F90` uses `VerticalCoordinateMod` and `VerticalRegridConserveInterfaceMod`; `ExtDataTypeDef.F90` uses `VerticalCoordinateMod`); see `../gridcomps/ExtData2G/ARCHITECTURE.md`.
- `../base/Base.F90` uses `MAPL_EtaHybridVerticalCoordinateMod` and `MAPL_VerticalDataMod`; these live in `../base/`, not in this library (`MAPL.vertical` depends on `MAPL.base`).
- Fixture: no use found in a sample search of components; not searched exhaustively.

## Known traps
- Module names do not carry a `MAPL_` prefix (`VerticalCoordinateMod`).
- `Eta2Eta.F90` still has its `implicit none` commented out.

## Observations
- A dependency on `MAPL.base` and `MAPL.pfio` means vertical cannot be used below those layers.
