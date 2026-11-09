# MAPL gridcomps: architecture

**Audience:** coding agents and developers working on MAPL's service gridded components.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `gridcomps/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## 1. Purpose

`gridcomps/` holds the gridded components MAPL itself supplies: the driver
(`Cap`), the output component (`History`), the input component (`ExtData`
and its second generation `ExtData2G`) and a nominal-orbit calculator
(`Orbit`). Every GEOS run uses Cap, History and one ExtData generation.

## 2. Build

`CMakeLists.txt` defines the umbrella library `MAPL.gridcomps` (override
name via `esma_set_this`) from the single source `MAPL_GridComps.F90`. It
depends on `MAPL.base`, `MAPL.pfio`, `MAPL_cfio_r4`, `MAPL.cap`, and, when
`BUILD_WITH_FLAP` / `BUILD_WITH_FARGPARSE` are set, on FLAP / fargparse
(with compile definitions `USE_FLAP` / `USE_FARGPARSE`). It then adds the
subdirectories in this order: `Cap`, `History`, `Orbit`, `ExtData`, and
`ExtData2G` only `if(USE_EXTDATA2G)`.

`USE_EXTDATA2G` is an option declared in MAPL's top-level `CMakeLists.txt`
(default `ON`). The same file sets `EXTDATA2G_TARGET` to `MAPL.ExtData2G`
when it is on and to the empty string when off. `Cap` lists
`MAPL.ExtData` and `${EXTDATA2G_TARGET}` as dependencies and gets the
compile definition `BUILD_WITH_EXTDATA2G` when on. `ExtData` (first
generation) is always built. Which generation a run uses is then a run-time
choice made by Cap (`USE_EXTDATA2G:` in `CAP.rc`); see
[Cap/ARCHITECTURE.md](Cap/ARCHITECTURE.md).

## 3. Subdirectories

| Directory | Target | Role | Document |
|---|---|---|---|
| `Cap/` | `MAPL.cap` | driver object, Cap gridded component, NUOPC wrapper | [Cap/ARCHITECTURE.md](Cap/ARCHITECTURE.md) |
| `History/` | `MAPL.history` | output (collections of exports) | [History/ARCHITECTURE.md](History/ARCHITECTURE.md) |
| `ExtData/` | `MAPL.ExtData` | first-generation external data input | [ExtData/ARCHITECTURE.md](ExtData/ARCHITECTURE.md) |
| `ExtData2G/` | `MAPL.ExtData2G` | second-generation external data input | [ExtData2G/ARCHITECTURE.md](ExtData2G/ARCHITECTURE.md) |
| `Orbit/` | `MAPL.orbit` | nominal satellite orbit masks | [Orbit/ARCHITECTURE.md](Orbit/ARCHITECTURE.md) |

Existing hand-written documents: `Cap/CapGridComp.md`,
`History/HistoryGridComp.md`, `History/README.md`,
`ExtData2G/ExtDataGridComp.md`, `ExtData2G/README.md`.

## 4. Who uses it

The top-level `MAPL` library (`MAPL/CMakeLists.txt`) lists `MAPL.gridcomps`
and `MAPL.orbit` among its dependencies (`MAPL.orbit` is not a dependency
of `MAPL.gridcomps`; `MAPL.cap` pulls in `MAPL.history` and ExtData). The GEOSgcm application builds `Cap` (via
`MAPL_Cap`) and the AGCM uses `Orbit`; citations are in the two
subdirectory documents.
