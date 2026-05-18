# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- mlc-disable -->
## [Unreleased]
<!-- mlc-enable -->

### Fixed

- Relaxed the comparison standard for grid_is_ok in case a grid is r4

### Added

### Changed

- Create `infrastructure/esmf/` (`MAPL.esmf`, Tier 3) consolidating
  `esmf_utils/`, `vm/`, `alarm/`, `hconfig/`, `hconfig_utils/`, and
  ESMF-related files from `generic3g/`. Backward-compatibility INTERFACE
  aliases provided for all five former library names.
- Phase 4 of MAPL v3 directory restructuring (#4905, closes #4925): consolidate
  `geom/`, `GeomIO/`, `regridder_mgr/`, `field/`, `field_bundle/`, `state/`,
  `vertical/`, `vertical_grid/`, and ESMF-related sources into `infrastructure/`
  subdirectories. Several files from `base3g/` moved to `mp_utils/`. Old
  top-level directories retained as stubs for backward compatibility.

- Create `utils/` directory (`MAPL.utils`, Tier 1) as an MPI-free library
  consolidating serial-only sources from `shared/`, `utilities/`, `udunits2f/`,
  and `generic3g/`. `MAPL.utils` depends only on ESMF and OpenMP — no MPI.
  Part of the MAPL v3 directory restructuring (#4905, phase 2a, closes #4915).

- Remove direct MPI dependencies from `ErrorHandling.F90` and `MAPL_Throw.F90`
  using a lazy-init procedure pointer pattern. Both modules now default to serial
  `error stop` behavior and are fully MPI-free. MPI-aware error handling (rank in
  error messages, `MPI_Abort`) is provided by the new `MAPL_MpiErrorHandling.F90`
  module; call `MAPL_initialize_error_handling()` once after `MPI_Init` to
  register MPI-aware handlers. Serial and pFUnit programs require no changes.
  `MAPL_ExceptionHandling` is now an alias defined in `ErrorHandling.F90` rather
  than a separate file. Closes #4917, part of #4905.

- Create `enums/` directory (`MAPL.enums`, Tier 0) and relocate 11 enum-like
  type definitions into it from `esmf_utils/`, `generic3g/`, and `component/`.
  No module names or interfaces changed; this is a pure file relocation.
  Part of the MAPL v3 directory restructuring (#4905, phase 1a, closes #4907).

Several files were renamed, MAPL_InfoSet was replaced with ESMF_InfoSet.
### Removed

### Deprecated

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
