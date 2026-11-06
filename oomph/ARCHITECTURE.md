# oomph

**Audience:** coding agents and developers working on MAPL `oomph/`.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `oomph/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose
An unfinished, object-oriented re-implementation of the state-item spec
layer (field specs, dimension specs, staggering, connection points). The
top-level `CMakeLists.txt` says it plainly:

```
add_subdirectory (oomph) # temporary - will rename to generic when done
```

## Build (`oomph/CMakeLists.txt`)
- Library `MAPL.oomph` (`esma_set_this (OVERRIDE MAPL.oomph)`), `TYPE ${MAPL_LIBRARY_TYPE}`.
- Dependencies: `MAPL.base`, `GFTL_SHARED::gftl-shared-v2`, `GFTL::gftl-v2`.
- Compiled: `oomph.F90` and `specs/{UngriddedDimSpec,VerticalStaggerLoc,HorizontalStaggerLoc,DimSpec,CouplingSpec,AbstractStateItemSpec,FieldSpec,ConnectionPoint}.F90`.
- Commented out in the source list: `DimensionsSpec`, `StateItemSpec`, `BundleSpec`, `ServiceSpec`, `StateItemSpecMap`, `ConnectionSpec`, `ConnectionSpecVector`, `ComponentSpec`, `ComponentSpecMap`.
- No tests directory.

## Contents
Umbrella `oomph.F90` (module `oomph`) re-exports `oomph_HorizontalStaggerLoc`, `oomph_VerticalStaggerLoc`, `oomph_UngriddedDimSpec`, `oomph_DimsSpec`, `oomph_AbstractStateItemSpec`, `oomph_FieldSpec`, `oomph_ConnectionPoint`.

| Module (`specs/`) | Role |
|---|---|
| `oomph_HorizontalStaggerLoc`, `oomph_VerticalStaggerLoc` | staggering location types |
| `oomph_DimsSpec` (file `DimSpec.F90`), `oomph_UngriddedDimSpec` | dimension specs |
| `oomph_AbstractStateItemSpec`, `oomph_FieldSpec` | state-item spec base and field spec |
| `oomph_CouplingSpec` | coupling spec (compiled, but not in the umbrella's `use` list) |
| `oomph_ConnectionPoint` | connection endpoint |

## Who uses it
Nothing outside this directory was found: a search of `*.F90`, `CMakeLists.txt` and `*.cmake` in MAPL for `oomph` hits only `oomph/` and the top-level `CMakeLists.txt`. The `MAPL` umbrella library links `MAPL.oomph` (see `../MAPL/ARCHITECTURE.md`) but `MAPL.F90` does not `use oomph`. A search of `src/Components` in the fixture found no `use oomph`; components use the `generic/` API (`../generic/ARCHITECTURE.md`).

## Known traps
- Name: the directory is not `generic`; the real spec layer is `../generic/`.
- File `specs/DimSpec.F90` defines module `oomph_DimsSpec`.

## Observations
- Linking `MAPL.oomph` into `MAPL` adds build time and a dependency for code nobody calls.
