## Why

MAPL restart callbacks currently have no phase identity for an in-memory checkpoint operation, which prevents components from distinguishing future internal checkpoint handling from existing netCDF restart I/O. Adding explicit internal read/write phases establishes the lifecycle hook without changing existing restart behavior.

## What Changes

- Add named generic phases for internal restart reads and writes in `GenericPhases.F90`.
- Leave `GENERIC_INIT_PHASE_SEQUENCE` order unchanged; new internal restart phases are not added to that array.
- Register the existing generic read/write restart entry points for the new phases in `GenericGridComp.F90`.
- Have `OuterMetaComponent` read and write restart procedures retrieve the current phase with `ESMF_GridCompGet`.
- Add explicit internal-phase branches in `OuterMetaComponent` read/write restart procedures; leave those branches empty for this proposal.
- Preserve existing netCDF restart reads and writes unchanged for their existing phases.

## Capabilities

### New Capabilities

- `internal-checkpoint`: Provides phase-aware lifecycle hooks for future in-memory restart reads and writes while retaining existing netCDF restart behavior.

### Modified Capabilities

- None.

## Impact

- Affected Fortran modules: `enums/GenericPhases.F90`, `superstructure/generic/GenericGridComp.F90`, and `superstructure/generic/OuterMetaComponent` restart procedures.
- Generic phase/API constants may be exposed through existing MAPL enum exports as needed by phase registration and dispatch.
- Existing netCDF restart file names, checkpoint controls, and read/write operations remain supported without behavior changes.
- Build and test validation uses `module load nag-stack` and the existing MAPL NAG workflow.
