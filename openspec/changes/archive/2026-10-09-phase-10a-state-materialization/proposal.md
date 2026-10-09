## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.8 names Phase 10 as
the still-missing validation suite Phase 6's own entry criterion ("do not
retire `StateRegistry`/`ExtensionFamily`/the aspect system until every
real production code path has a graph-native replacement that has been
*validated*, not merely built") already presupposes. Direct inspection of
the landed Phase 3-5 `GraphBuilder.F90` shows today's gap concretely: an
ordinary (exact-match) `StateItemNode` is never given a real `ESMF_Field`
— `advertise_one()` leaves its payload default, and `resolve_one()`'s
exact-match branch only adds a dependency edge; real `FieldCreate()`
(`materialize_field_extension()`) fires only on the mismatch/extension
branch, gated off by default. Nothing populates a real `ESMF_State` from
graph-side `StateItemNode`s at all — `Test_Scenarios.pf`, the ~30-fixture
real-YAML regression suite this project treats as realistic client-code
coverage, only ever inspects the `ESMF_State` tree legacy's own
`StateRegistry%add_to_states` populates. Every later Phase 10 sub-change
(10b's equivalence harness, 10c's characteristic-chain fixture, 10d's
run-loop wiring) needs this gap closed first — §20.4.8 names this
sub-change (10a) as the one with "no dependency on 3c's extension
machinery beyond what already exists," making it the correct first slice
to land on its own.

## What Changes

- Ordinary (exact-match, non-mismatched) `StateItemNode`s get a real
  `FieldCreate()`'d payload at `GENERIC_INIT_ACCEPT_TRANSFER` time —
  `resolve_one()`'s no-mismatch branch materializes the export side's
  `StateItemNode` the same way `try_materialize_field_extension()`
  already does for the mismatch branch (reusing its existing geom/vgrid
  resolution helpers), instead of leaving it an unallocated placeholder.
- A new graph-side equivalent of `StateRegistry%add_to_states` walks a
  component's own advertised `StateItemNode` ports (import and export)
  and, when the new graph mode (below) is `graph`, inserts each
  resolved item's real `ESMF_Field` into the real `ESMF_State` under its
  own short name — resolving an import's real field by following its
  dependency edge back to whichever node actually holds the
  materialized payload (direct export, or an extension chain's final
  node), mirroring legacy's own producer-aliasing semantics.
- `mapl_GraphMode_mod` gains a 3-way selector (`registry` / `graph` /
  `both`) in place of today's binary `graph_native_enabled()`:
  - `registry` (default, unchanged): zero behavior change from today —
    no ordinary-item materialization, no graph-side state population.
  - `both`: both the new materialization step above and the legacy
    imperative coupler run side by side; the graph-materialized payload
    is compared out-of-band (`node%get_payload()%get_field()`, the same
    read `Test_GraphBuilderEquivalence.pf` already performs for its two
    existing tests) and is **never** inserted into the shared
    `ESMF_State` legacy's own `add_to_states`/`check()`-style assertions
    observe — no name-collision is possible by construction.
  - `graph`: the true-replacement mode — graph-side population is the
    only producer of the relevant `ESMF_State` entries, legacy's own
    `add_to_states`/`process_connections` skipped entirely for that
    component. **Not attempted by this sub-change or any other 10a-10d
    sub-change** — the selector and the population code path both exist
    and are directly unit-testable, but no fixture in this phase turns
    this mode on end-to-end; it remains available once `both` mode has
    run clean across the fixture set this phase builds.
- No change to `TypeKindCharacteristic` wiring, extension-chain
  construction, or run-loop `MethodGraphNode` construction — those are
  10b/10c/10d's own scope (§20.4.8), sequenced after this sub-change.

## Capabilities

### New Capabilities
- `graph/graph-mode`: the 3-way `registry`/`graph`/`both` selector
  governing which producer (legacy imperative coupler, graph-native
  population, or both side-by-side with no shared-state collision)
  populates a component's real `ESMF_State`.

### Modified Capabilities
- `graph/graph-builder`: ordinary (exact-match) connection resolution
  now also materializes a real `ESMF_Field` for the export side instead
  of leaving it an unallocated placeholder, and gains a graph-side
  state-population step that is the graph-native analog of
  `StateRegistry%add_to_states`.

## Impact

- **Affected code:** `superstructure/generic/GraphBuilder.F90` (new
  materialization call in `resolve_one`'s no-mismatch branch; new
  state-population procedure, called from
  `initialize_accept_transfer.F90` alongside the existing
  `run_connect_hook`); `superstructure/generic/graph/GraphMode.F90`
  (binary flag replaced by 3-way selector; existing
  `graphbuilder_run_geometry_hook` call site updated to consult it);
  `superstructure/generic/graph/ExtensionMaterialization.F90` (reused,
  possibly extended with an optional field name so a materialized item
  is findable by name once inserted into a real `ESMF_State` — the
  mismatch branch's own items are never inserted into a real state
  today, so this is a new need, not a behavior change for that branch).
- **No change to default (`registry` mode) production behavior.** Every
  real MAPL run today behaves identically unless something explicitly
  calls the new mode setter — today that remains test code only, same
  as the binary flag it replaces.
- **Downstream effect:** unblocks 10b (generalized equivalence harness
  exercising `both` mode across new flat fixtures), which depends on
  this sub-change's materialization and state-population mechanics
  existing first.
- **Additional affected code, found during implementation (design.md
  "Correction: pre-existing geometry-hook phase-timing bug"):**
  `superstructure/generic/OuterMetaComponent/initialize_advertise.F90`
  and `initialize_geom_b.F90` - the already-landed (Phase 4e) geometry
  hook's production call site was moved from the former to the latter,
  fixing a pre-existing timing bug (the hook ran before a
  `GEOMETRY_PROVIDER` component's own geometry was actually resolved)
  that turning the shared mode selector on against a real fixture for
  the first time exposed. Not a behavior change for `registry` mode
  (the hook remains a no-op there either way); no change to
  `Test_GraphGeometryHook.pf`, which exercises the hook directly and is
  unaffected by which lifecycle phase calls it in production.
