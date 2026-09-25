## Why

The `internal-checkpoint` capability defined phase-aware hooks (`GENERIC_INTERNAL_READ_RESTART` / `GENERIC_INTERNAL_WRITE_RESTART`) for an in-memory checkpoint but left both `OuterMetaComponent%read_restart` and `OuterMetaComponent%write_restart` internal branches empty. Components have no way to save and restore state in memory (e.g. for ensemble perturbation, ESMF `ESMF_METHOD_WDTIMEAVG`-style rewind, or fast intermediate checkpoints) without paying netCDF I/O cost. This change implements that deferred behavior.

## What Changes

- Add a `memory_checkpoint` type component of type `ESMF_State` to `OuterMetaComponent`, containing three nested `ESMF_State`s (import, export, internal), mirroring the shape of `MultiState`.
- Implement the `GENERIC_INTERNAL_WRITE_RESTART` branch in `OuterMetaComponent%write_restart` to perform a deep copy of the current import/export/internal states (per existing `checkpoint_controls` flags) into `memory_checkpoint`.
- Implement the `GENERIC_INTERNAL_READ_RESTART` branch in `OuterMetaComponent%read_restart` to perform a data-only copy from `memory_checkpoint` back into the live import/export/internal states (per existing `restart_controls` flags).
- Factor the field/state enumeration logic shared by netCDF write/read (`write_restart.F90`, `initialize_read_restart.F90`) and the new in-memory write/read into a common helper that builds the bundle of restart-eligible fields for a given `ESMF_State`, so both file-based and in-memory paths use the same selection logic.
- Add `FieldBundleClone`, a new MAPL infrastructure procedure (alongside existing `FieldClone`) that creates a new `ESMF_FieldBundle` whose fields are structural clones (via `FieldClone`) of the fields in a source bundle, since no such procedure exists today.
- A single most-recent in-memory snapshot is retained per component instance; a new write overwrites any prior snapshot. Multiple concurrent/keyed snapshots are out of scope.
- Scope is a single `OuterMetaComponent` instance only: no recursion into child components, and no production call site is added to trigger these phases (they are only exercised directly by the new tests via `ESMF_GridCompWriteRestart`/`ReadRestart`). Subtree recursion (to support predictor/corrector-style checkpointing of most of the model) and a real driver-level trigger (e.g. on `GriddedComponentDriver`/`Cap`) are explicitly deferred to follow-up work.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `internal-checkpoint`: The internal read/write restart branches, previously specified to perform no operation, now SHALL perform an in-memory deep-copy checkpoint write and a data-only checkpoint read, gated by the existing `checkpoint_controls`/`restart_controls` per-state flags.

## Impact

- Affected Fortran modules: `superstructure/generic/OuterMetaComponent.F90`, `superstructure/generic/OuterMetaComponent/write_restart.F90`, `superstructure/generic/OuterMetaComponent/read_restart.F90`, `superstructure/generic/OuterMetaComponent/initialize_read_restart.F90` (refactor to shared helper).
- New infrastructure procedure: `FieldBundleClone` in `infrastructure/field_bundle/` (or adjacent module), exported through `mapl_field_bundle_api`.
- `OuterMetaComponent` gains a new private state field (`memory_checkpoint`); existing public API (`OuterMetaComponent` constructor, accessors) is unaffected.
- No change to existing netCDF checkpoint/restart file formats, filenames, or controls.
- Build and test validation uses `module load nag-stack` and the existing MAPL NAG workflow; new pFUnit tests exercise write-then-read round trips for in-memory checkpoint.
