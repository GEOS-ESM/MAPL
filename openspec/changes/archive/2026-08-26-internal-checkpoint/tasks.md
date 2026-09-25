## 1. Define internal restart phases

- [x] 1.1 Add generic internal read and internal write phase constants in `enums/GenericPhases.F90`.
- [x] 1.2 Export new constants through existing MAPL enum API if required by downstream phase registration.
- [x] 1.3 Leave `GENERIC_INIT_PHASE_SEQUENCE` order unchanged and do not add new internal restart phases to that array.

## 2. Register and dispatch phases

- [x] 2.1 Add new `read_restart` ESMF entry point procedure in `superstructure/generic/GenericGridComp.F90` that dispatches to `outer_meta%read_restart`, and register it for `ESMF_METHOD_READRESTART` at the new internal read phase. This is a new procedure; no existing `READRESTART`-method entry point exists to reuse.
- [x] 2.2 Register the existing `write_restart` entry point in `superstructure/generic/GenericGridComp.F90` for `ESMF_METHOD_WRITERESTART` at the new internal write phase, in addition to its existing registration.
- [x] 2.3 Add a new `OuterMetaComponent%read_restart` type-bound procedure (declared in `OuterMetaComponent.F90`, implemented in a new `OuterMetaComponent/read_restart.F90` submodule) that retrieves `currentPhase` with `ESMF_GridCompGet` and branches on it. This procedure is new; it is distinct from existing `initialize_read_restart`.
- [x] 2.4 Add empty internal branch in `OuterMetaComponent%read_restart` for the internal read phase; leave existing netCDF read logic (`initialize_read_restart`) untouched and unrelated to this new procedure.
- [x] 2.5 Retrieve `currentPhase` with `ESMF_GridCompGet` in the existing `OuterMetaComponent%write_restart` procedure.
- [x] 2.6 Add empty internal write branch in `OuterMetaComponent%write_restart` for the internal write phase while preserving existing netCDF write branch for its existing phase.
- [x] 2.7 Register the new `read_restart.F90` submodule in `superstructure/generic/CMakeLists.txt` and declare the new interface in `OuterMetaComponent.F90`.

## 3. Validate compatibility

- [x] 3.1 Build MAPL with `module load nag-stack` and existing NAG build workflow.
- [x] 3.2 Run existing generic and restart-related tests with `module load nag-stack`.
- [x] 3.3 Confirm existing netCDF restart behavior remains covered and unchanged.
