# MAPL Orbit: architecture

**Audience:** coding agents and developers working on the MAPL nominal-orbit gridded component.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/Orbit/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [../ARCHITECTURE.md](../ARCHITECTURE.md). No existing user document.

## 1. Purpose

`MAPL_OrbGridCompMod` is, in its own header words, "an ESMF gridded component
implementing the MAPL-5 Nominal Orbit Calculator" (author GMAO SI-Team,
initial version 30 Nov 2010). Each run step it computes, for a list of
satellite instruments, a horizontal mask on the model grid showing where the
satellite's swath passes during the time interval of the step. The masks are
exports, so a History collection can sample model fields along nominal orbits.

## 2. Build

`CMakeLists.txt` defines `MAPL.orbit` from `MAPL_OrbGridCompMod.F90`.
Dependencies: `MAPL.shared`, `MAPL.constants`, `MAPL.base`, `MAPL.generic`;
public link to gftl, gftl-shared, ESMF, NetCDF Fortran; private link to MPI and
OpenMP. `MAPL.orbit` is listed in `MAPL/CMakeLists.txt` but is not a
dependency of `MAPL.gridcomps`.

## 3. Contents

- `MAPL_OrbGridCompMod.F90` (about 1600 lines): public `SetServices` only.
  `SetServices` registers `Initialize_` and `Run_`, and
  adds one export per instrument (units `days`, horizontal-only) plus the
  export `SATORB`, a field bundle (`MAPL_BundleItem`). Private
  routines: `extract_`, masking for lat-lon (`orb_mask_lonlat`,
  `orb_swath_mask_lonlat`) and cubed-sphere grids (`orb_mask_xy`,
  `orb_swath_mask_xy`, `cube_xy`, `check_face`), and `orb_halo`.
- `MAPL_OrbGridComp.rc`: example resource file, read from the run directory
  under the name `MAPL_OrbGridComp.rc`. Keys: `verbose:` and the table
  `Nominal_Orbits::` (instrument, satellite, swath in km, long name). Listed
  satellites: TERRA, AQUA, AURA, CALIPSO, CLOUDSAT with instruments such as
  MODIS, AIRS, OMI, MISR.

If `MAPL_OrbGridComp.rc` is absent from the run directory, the component
sets the number of masks to zero, adds only `SATORB`, and `Run_` returns at
once.

## 4. Who uses it

- `GEOS_AgcmGridComp.F90` (GEOSagcm_GridComp): `use MAPL_OrbGridCompMod, only:
  ORB_SetServices => SetServices`; `ORB = MAPL_AddChild(GC, NAME='ORBIT',
  SS=ORB_SetServices)`; a `MAPL_AddConnectivity` for `SATORB` from `ORB` to
  `PHYS`; and `ESMF_GridCompRun(GCS(ORB), ...)` inside the AGCM run, timed
  as `ORBIT`. See the AGCM document in the fixture
  (`src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/ARCHITECTURE.md`).
- No file named `MAPL_OrbGridComp.rc` and no mention of `Nominal_Orbits`
  was found under `src/Applications` or `src/Components`; whether a
  deployed run directory supplies it was not checked. *Inferred:* in the
  fixture the component runs but, without that file, computes no masks.

## 5. Known traps

- The `Nominal_Orbits::` reader takes four columns per row: instrument,
  satellite, swath, and an integer halo width; the example rc rows have
  a long-name text after the swath instead (see Observations).
- Mask count comes from the rc file at `SetServices` time, so exports are
  fixed then; a History collection naming an instrument absent from the rc file
  has no source.

## 6. Observations

- The example `MAPL_OrbGridComp.rc` begins "Resource FIle for the H2O Grid
  Component", apparently a copy-paste from another component.
- The example rc table has columns (instrument, satellite, swath, long name)
  while the reader in `SetServices` fetches a fourth integer `halo`; with
  the example rows that read would meet text. Not tested.
- `_ASSERT(self%no>0, 'needs informative message')` is the message
  text in the source when the table is empty.
