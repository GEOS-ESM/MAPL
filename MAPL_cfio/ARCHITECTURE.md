# MAPL_cfio: legacy CFIO file I/O (libraries `MAPL_cfio_r4`, `MAPL_cfio_r8`)

**Audience:** coding agents and developers working on legacy CFIO file I/O.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `MAPL_cfio/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Older ESMF/CFIO-style reader and writer for netCDF/HDF files (modules
`ESMF_CFIO*Mod`), plus the `ShaveMantissa` bit-reduction C routine. New
History/ExtData2G paths use pFIO instead ([../pfio/ARCHITECTURE.md](../pfio/ARCHITECTURE.md)).
The top-level README (item 8) calls it "a deprecated lower-level I/O layer
that is generally replaced by GMAO_pFIO. Not all of the strings have been cut
yet. Sometime soon, this directory will be eliminated." ([../README.md](../README.md)).

## Build
- Built twice from the same directory by the top-level CMake:
  `add_subdirectory (MAPL_cfio MAPL_cfio_r4)` and `(MAPL_cfio MAPL_cfio_r8)`
  (CMakeLists.txt). The target name is `MAPL_cfio_${precision}`, precision taken
  from the binary directory name (MAPL_cfio/CMakeLists.txt).
- `r8` adds `FREAL8` compile flags to Fortran sources; `r4` uses defaults.
- Sources: `ESMF_CFIO{Base,File,Grid,Sdf,Util,VarInfo}Mod.F90`,
  `ESMF_CFIOMod.F90`, `ShaveMantissa.c`. Depends on ESMF and netCDF-Fortran.
- Not compiled: `ESMF_CFIOEosMod.F90`, `utDownBit.F90`, `unused_dummy.H`,
  `examples/`; `netcdf_stub.F90` only when `MFHDF3_NOT_PRESENT`.
- With `USE_F2PY` and `r4` only: f2py module `ShaveMantissa_` and
  `shavemantissa.py` installed under `lib/Python/`.

## Who uses it
- `r4` (MAPL itself): `MAPL.base`, `MAPL.griddedio`, `MAPL` umbrella and
  `gridcomps` link it (base/CMakeLists.txt, griddedio/CMakeLists.txt,
  MAPL/CMakeLists.txt, gridcomps/CMakeLists.txt).
  `use ESMF_CFIOMod`/`MAPL_CFIOMod` (the latter is defined in
  base/MAPL_CFIO.F90) in Cap, History, ExtData, ExtData2G, base
  (gridcomps/Cap/MAPL_Cap.F90, gridcomps/History/MAPL_HistoryGridComp.F90, ExtDataGridCompMod.F90, ExtDataGridCompNG.F90).
- Fixture, `r4`: GEOSmkiau, GEOSpchem, StratChem, GEOSCHEMchem, GOCART CO, CO2, CH4, CFC, Rn, O3
  CMakeLists.
- `GMAO_stoch/CMakeLists.txt` mentions `MAPL_cfio_r4` only in a commented-out line.
- Fixture, `r8`: GMAO_hermes executables `GFIO_remap.x`, `GFIO_Shave.x`,
  `lcv2prs.x` (`use ESMF_CFIOMod` in lcv2prs.F90), and `convert_aerosols.x` in
  `@GEOS_Util/post`. See the fixture documents
  `../../@GMAO_Shared/GMAO_hermes/ARCHITECTURE.md` and
  `../../@GMAO_Shared/GMAO_gfio/ARCHITECTURE.md`.

## Known traps
- Two libraries from one source: a change compiles twice with different
  real kinds.
- `MAPL_CFIOMod` is in `base/`, not here.

## Observations (noted, not fixed)
- `shared/DownBit.F90` also mentions CFIO (grep match); relation to
  `ShaveMantissa.c` not examined.
- `ESMF_CFIOrGrADSMod.F90` is named in a CMake workaround that applies only
  when `MISMATCH` is set; the file is absent.
