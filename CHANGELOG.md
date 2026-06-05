# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Added

- `FieldBundleFilter` for filtering field bundles by predicate; refactored
  `StateGet` and `RestartHandler` accordingly.
- `GridComp` checkpoint directory helper (`MAPL_GridCompCheckpointDir`).
- `StatisticsGridComp`: extended to support variance of a single field.
- `FieldBundleGetPointerToData`: added REAL64 overloads for 2D/3D index/name variants.
- `MAPL_STATEITEM_VECTOR` item type support in ACG spec files.
- Re-export `PackedDateCreate`, `PackedTimeCreate`, `PackedDateTimeCreate`, and
  `StrTemplate` through the top-level `MAPL` umbrella module.

### Changed

- **MAPL v3 directory restructuring complete**: consolidated sources into
  `infrastructure/`, `superstructure/`, `enums/`, `utils/`, `mp_utils/`, and
  `base/`; renamed `gridcomps/` subdirectories to canonical lowercase names;
  removed all `3g` suffixes from module and directory names; unified the
  `mapl3g_` module namespace under `mapl_`.
- **Public API lockdown**: all `Export.F90` umbrella modules now carry explicit
  `private` + `public ::` declarations. Legacy `API.F90` shim files dissolved;
  symbols routed through proper export umbrellas. Removed symbols with no
  external consumers; added confirmed external-consumer symbols.
- **Namespace standardization**: all internal module names follow the
  `mapl_<Name>_mod` convention. Unprefixed enum constants and types renamed to
  `MAPL_`-prefixed equivalents (`FIELDBUNDLETYPE_*`, `STATEITEM_ALLOCATION_*`,
  `GENERIC_COUPLER_*`, `VectorBasisKind`, etc.).
- `VerticalStaggerLoc` moved from `MAPL.vertical_grid` into `MAPL.enums`.
- `VerticalCoordinate` now takes `FileMetadata` (pfio) directly instead of the
  `FileMetadataUtils` wrapper; `udunits2f` is now an explicit dependency of
  `MAPL.vertical`.
- CI updated to Baselibs 8.32.0 and circleci-tools orb v5; `components.yaml`
  updated to ESMA_env v5.22.0 / GEOSpyD 26.3.2.

### Fixed

- `RoutehandleParam.F90`: fixed uninitialized error variable.
- `VariableSpec`/`VectorClassAspect`: fixed vector component naming lifecycle
  (names now resolved at create-time rather than deferred to add-to-state).
- ACG lookup mappings made bidirectional so aliases and actual values are
  interchangeable in spec files.
- `OpenMP_Support.F90`: fixed NVHPC build failure by using MAPL wrapper
  equivalents for ESMF internal-state calls.
- `Test_FieldFill.pf`: suppressed IEEE invalid-operation trap for sNaN queries
  under `-Ktrap=fp`; guarded IEEE halting-mode calls on macOS/ARM (NAG) where
  the feature is unsupported.
- `grid_is_ok`: relaxed comparison to handle r4 grids.
- `MAPL.F90`: fixed `ifx` linker issue with bare `mapl_return_` thunks by
  using `mapl_ErrorHandling` directly.

<!-- mlc-disable -->
## [v3.0.0-alpha-0] - 2026-05-15
<!-- mlc-enable -->

### Added

- Add [`docs/mapl3/diffs-from-mapl2.md`](docs/mapl3/diffs-from-mapl2.md) — a comprehensive
  overview of the architectural and user-facing differences between MAPL v3 and MAPL v2.
  This document covers component structure, connections, field specifications, resource
  files, Cap/time-loop changes, History3G, ExtData, the new Statistics component,
  clocks, and build system changes.  It is intended as the primary migration reference
  for developers and users moving from MAPL2 to MAPL3.
- Add [`docs/mapl3/api-changes.md`](docs/mapl3/api-changes.md) — a procedure-level
  reference of core framework API changes: stubbed-out V2 procedures, new MAPL3
  framework entry points (`MAPL_initialize`, `MAPL_finalize`, `MaplFramework`),
  and replacements for lifecycle, child management, field specs, connectivity,
  resource access, and timer APIs.

## Previous Versions

- **Note to Developers**: For MAPL v2 changes, please refer to the CHANGELOG.md for specific tags or for the [CHANGELOG.md in the `release/v2` branch}(https://github.com/GEOS-ESM/MAPL/blob/release/v2/CHANGELOG.md). From now on, all MAPL v3 changes will be documented in this CHANGELOG.md file. The `release/v2` branch will continue to maintain its own CHANGELOG.md for v2-specific changes until the end of support for MAPL v2.
