## Context

Existing generic restart handling is split between ESMF entry-point registration, generic phase constants, and `OuterMetaComponent` restart procedures. Existing read/write procedures also perform netCDF restart operations, including internal-state files controlled by restart configuration. See proposal.md and the internal-checkpoint spec for required behavior.

## Goals / Non-Goals

**Goals:**

- Introduce separate phase identifiers for internal restart read and write callbacks.
- Reuse current generic restart callbacks and make their dispatch phase-aware.
- Keep existing netCDF restart paths and controls intact.
- Keep the existing `GENERIC_INIT_PHASE_SEQUENCE` order unchanged and exclude new internal restart phases from that array.
- Establish empty branches as future extension points for in-memory state transfer.

**Non-Goals:**

- Implementing in-memory serialization, storage, transfer, or restoration.
- Changing checkpoint file formats, file naming, restart controls, or `RestartHandler` behavior.
- Removing or renaming existing restart phases.

## Decisions

- **Use separate read/write phases:** Define one phase for internal reads and one for internal writes rather than overloading existing phases. This preserves unambiguous callback intent and allows later implementation to evolve independently. These phases are registered directly for restart entry points and are not initialization-sequence members.
- **Write reuses existing entry point:** The outer `write_restart` ESMF entry point in `GenericGridComp.F90` already exists and is registered for `ESMF_METHOD_WRITERESTART`. Register that same entry point for the new internal write phase as an additional phase, rather than adding a parallel callback. `OuterMetaComponent%write_restart` gains a `currentPhase` branch to select internal vs. existing netCDF behavior.
- **Read requires a new outer entry point:** No outer-level `ESMF_METHOD_READRESTART` entry point exists today; existing netCDF restart reads run under `ESMF_METHOD_INITIALIZE` phase `GENERIC_INIT_READ_RESTART` via `initialize_read_restart`, which is unrelated to the `READRESTART` method. Add a new `read_restart` procedure in `GenericGridComp.F90`, register it for `ESMF_METHOD_READRESTART` at the new internal read phase, and add a corresponding `OuterMetaComponent%read_restart` type-bound procedure that dispatches based on `currentPhase`. This new procedure is dedicated to the internal read phase; it does not touch or replace `initialize_read_restart`.
- **Read phase from grid component:** In `OuterMetaComponent%write_restart` and the new `OuterMetaComponent%read_restart`, call `ESMF_GridCompGet` with `currentPhase` and branch on that value. This follows existing generic dispatch patterns and avoids inferring phase from method arguments.
- **Leave internal branches empty:** Internal branches intentionally perform no work in this proposal. Existing netCDF logic remains outside those branches, so current restart operations are not accidentally executed for future in-memory callbacks.
- **Expose constants through existing enum surface:** If consumers need phase constants outside `GenericPhases.F90`, export them through the established API enum aliases rather than introducing a separate phase namespace.

## Risks / Trade-offs

- [Empty internal branches provide no checkpoint data yet] -> Treat this change as lifecycle plumbing only; implement state storage in a follow-up change.
- [ESMF phase registration compatibility varies by method] -> Build and test with `module load nag-stack` using existing MAPL validation targets.
- [Phase constants could be exposed inconsistently] -> Keep names and aliases aligned with existing generic phase export conventions.
- [Future code could accidentally fall through to netCDF logic] -> Make internal cases explicit and keep existing file-I/O logic in the non-internal path.
- [Initialization flow could change accidentally] -> Leave `GENERIC_INIT_PHASE_SEQUENCE` untouched and verify its existing order remains unchanged.

## Migration Plan

Add phase constants, registrations, and dispatch branches. Run existing restart and generic tests with NAG. No data migration is needed; rollback consists of removing the new phase registrations and branches while retaining existing restart code.

## Open Questions

None. In-memory checkpoint representation and transfer semantics are intentionally deferred.
