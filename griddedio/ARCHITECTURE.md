# griddedio: ESMF fields and bundles to and from pFIO (library `MAPL.griddedio`)

**Audience:** coding agents and developers working on MAPL gridded I/O.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `griddedio/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
Translates ESMF field bundles into pFIO metadata and data messages (write) and
reads files into bundles (read), including regridding and optional
quantization of output. Library-level view only.

## Build
- Library `MAPL.griddedio`; depends on `MAPL.shared MAPL.constants MAPL.base
  MAPL.pfio MAPL_cfio_r4`; links ESMF, netCDF-Fortran, gFTL, pFlogger
  (griddedio/CMakeLists.txt). Seven source files, all compiled.

## Contents
| File | Module | Role |
|---|---|---|
| GriddedIO.F90 | `MAPL_GriddedIOMod` | Main type `MAPL_GriddedIO`: builds `FileMetadata` from a bundle (`metadata`), regrids, stages data, reads back; quantization attributes (bitround etc.) |
| GriddedIOitem.F90 | `MAPL_GriddedIOitemMod`, `MAPL_GriddedIOitemVectorMod` | One requested item (name, expression) and its vector |
| FieldBundleWrite.F90 | `MAPL_ESMFFieldBundleWrite` | `FieldBundleWriter` for a bundle to a file |
| FieldBundleRead.F90 | `MAPL_ESMFFieldBundleRead` | Read a file into a bundle |
| TileGridIO.F90 | `MAPL_TileGridIOMod` | Same idea for tile (land) grids |
| DataCollection.F90, DataCollectionManager.F90 | `MAPL_DataCollectionMod` (also `MAPL_CollectionVectorMod`), `MAPL_DataCollectionManagerMod` | Cache of file metadata by template/file name for readers |

## Who uses it
- History: `use MAPL_GriddedIOMod`, `MAPL_GriddedIOitemVectorMod`; allocates
  `MAPL_GriddedIO()` per collection and passes its `metadata` to
  `o_Clients%add_hist_collection`
  (gridcomps/History/MAPL_HistoryGridComp.F90, MAPL_HistoryCollection.F90).
- ExtData2G: `MAPL_GriddedIOMod` and item modules
  (gridcomps/ExtData2G/ExtData_IOBundleMod.F90, ExtDataGridCompNG.F90,
  ExtDataTypeDef.F90); the legacy `gridcomps/ExtData` also links the library
  (gridcomps/ExtData/CMakeLists.txt).
- `MAPL.F90` re-exports the bundle read/write modules; `Apps/Regrid_Util`
  and `base/tests/mapl_bundleio_test.F90` use them.
- Links: [../pfio/ARCHITECTURE.md](../pfio/ARCHITECTURE.md),
  [../gridcomps/History/ARCHITECTURE.md](../gridcomps/History/ARCHITECTURE.md),
  [../gridcomps/ExtData2G/ARCHITECTURE.md](../gridcomps/ExtData2G/ARCHITECTURE.md).

## Known traps
- Depends on `MAPL.base` (regridders, grid metadata), so it sits above base
  while pfio sits below it.
- Quantization attributes follow a draft CF convention (comments in
  GriddedIO.F90); formulas are marked as possibly not final there.

## Observations (noted, not fixed)
- Module names do not match file names (for example `MAPL_ESMFFieldBundleRead`
  in FieldBundleRead.F90).
- No directory-level `.md` or tests live here; bundle I/O test is in
  `base/tests`.
