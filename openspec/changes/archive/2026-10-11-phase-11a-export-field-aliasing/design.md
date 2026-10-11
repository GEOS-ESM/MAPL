## Context

`graph-child-state-population` (landed) fixed the import side of a
sibling connection's state population: `populate_one`
(`GraphBuilder.F90`) always wraps whatever real field it finds in a
`MAPL_NamedAlias(field, name=short_name)` before inserting it into the
import child's own real `ESMF_State`, so re-entrant calls (the two-pass
`GENERIC_INIT_ACCEPT_TRANSFER` schedule, `enums/GenericPhases.F90`'s
`GENERIC_INIT_PHASE_SEQUENCE`) always add-or-replace under the same
stable name.

The export side never received the mirrored treatment. `resolve_one`'s
two `export_node_id` materialization call sites (the exact-match branch
at line ~849, and the mismatch branch's own chain-input materialization
at line ~904, both in `GraphBuilder.F90`) call
`try_materialize_field_extension` -> `materialize_field_extension`
(`mapl_ExtensionMaterialization_mod`), which calls `FieldCreate` -
allocating a brand-new field with the right geom/typekind/units, but
with no connection at all to `comp_src`'s own real export field, the one
`ConfigurableGridComp%run()` actually writes the YAML-configured `run:`
value into. `Test_GraphModeDemonstration.pf`'s own header comment
records the confirmed symptom: a real run phase's changed value never
reaches the proxy (a silent `NaN`, not a crash).

Two real `ESMF_Field` objects already exist for the same logical item by
the time this gap matters: `comp_src`'s own native `StateItemNode`
(`advertise_one`'s `item_key`-indexed node, in `comp_src`'s own
`ComponentGraph`) and the parent's proxy `StateItemNode`
(`get_or_make_local_node_id`'s `proxy_key`-indexed node, in the parent's
own `ComponentGraph`) - two distinct `NodeId`s, two distinct graphs,
materialized independently today. The fix makes the proxy's own payload
a `MAPL_NamedAlias` of whatever real field is sitting in `comp_src`'s own
real export `ESMF_State` at materialization time, rather than a second,
independent `FieldCreate`.

## Goals / Non-Goals

**Goals:**
- Make a sibling connection's export-side proxy payload a real,
  shallow alias of the export component's own real materialized field -
  so that any later write into that real field (a run phase) is visible
  through the proxy, the same way the import side already is.
- Cover both of `resolve_one`'s existing `export_node_id`
  materialization call sites (exact-match branch and mismatch-branch
  chain-input), with one shared procedure - no divergent logic between
  them.
- Tolerate the two-pass `GENERIC_INIT_ACCEPT_TRANSFER` schedule: the
  export component's own real field may not exist yet on the first
  pass (not yet realized/populated into its own real `ESMF_State`) and
  must be found on the second.

**Non-Goals:**
- The extension chain's own FINAL node (the transform's real output,
  materialized with the import's own typekind/units) is unchanged -
  that node's payload is a real transform *output*, never a
  producer-side alias; `11b`'s own scope (multi-hop intermediate
  materialization) is the only remaining gap on that side, untouched
  here.
- No change to `try_materialize_field_extension`'s or
  `materialize_field_extension`'s own signature or the `FieldCreate`
  path itself - that remains the correct fallback for the case this
  design's own Decision 2 below identifies (export not yet realized).
- No change to `11b`'s own scope (multi-hop chain intermediate
  materialization, `ExtensionResolution.F90`) - confirmed independent
  per §20.4.10, not revisited here.
- No change to `graphbuilder_run_pull_hook`'s own re-evaluation timing
  or `populate_one`'s own import-side logic - both already correct for
  this change's purposes.

## Decisions

**D1: Where the real field lives, and how to find it.**
`comp_src`'s own real export field, once realized, lives in
`comp_src`'s own `GriddedComponentDriver%get_states()%exportState` -
reached from a parent `OuterMetaComponent` the same one-way,
already-established path `graphbuilder_populate_state`'s own
cross-component loop already uses: `component_for(this, comp_name)` (an
existing helper, already used by `try_materialize_field_extension`
itself to resolve `export_meta`) -> `export_meta%get_user_gc_driver()`
-> `%get_states()%exportState`. Read the real field by name via a plain
`ESMF_StateGet(export_state, itemName=short_name, field=field, rc=status)`
call - no `StateRegistry` involvement, matching every other graph-layer
materialization call site's own "never call into StateRegistry"
constraint (`mapl_ExtensionMaterialization_mod`'s own module header).
`ESMF_StateGet` returns a non-zero `rc` when the named item is absent
(not yet added) - treated as "not yet available," not a hard failure
(see D2).

**D2: What to do when the export's own real field is not yet available.**
Two facts, each confirmed by direct code reading, bound on when
`comp_src`'s own real export field becomes available by name in its own
real export `ESMF_State`: (a) `self_advertise`
(`initialize_advertise.F90`) inserts every advertised export's own
`FieldClassAspect` payload into `user_states` via
`this%registry%add_to_states(user_states, mode='user', _RC)` at
`GENERIC_INIT_ADVERTISE` time - a real, named `ESMF_Field` handle, even
though its own underlying memory is not yet allocated; (b)
`StateRegistry%allocate` (`allocate_items`,
`StateRegistry_Actions_smod.F90`) is what backs that handle with real
memory (`FieldClassAspect%allocate` -> `mapl_FieldEmptyComplete`), and
is called unconditionally at both `GENERIC_INIT_REALIZE_PROVIDED` and
`GENERIC_INIT_REALIZE_ACCEPTED` (`initialize_realize_provided.F90`/
`initialize_realize_accepted.F90`) - both of which run, per
`GENERIC_INIT_PHASE_SEQUENCE`, strictly before the FIRST
`GENERIC_INIT_ACCEPT_TRANSFER` pass (the point `resolve_one`, and hence
this change's new procedure, first runs). So `comp_src`'s own real
export field is expected to already be a real, named, memory-backed
item in its own real export `ESMF_State` by the time this new
procedure's `ESMF_StateGet` call runs, on the FIRST pass already - not
only the second.

This is a static trace, not something confirmed by running the fixture
(outside this planning change's own scope) - some NUOPC-level
realize/accept-transfer detail could still make the handle unavailable
by that exact name at that exact point for a reason this trace missed.
The new procedure is written defensively regardless: `ESMF_StateGet`
returning non-zero (or the item simply not found by name) is treated as
"not yet available," not a hard failure - fall through to the existing
`materialize_field_extension`/`FieldCreate` path unchanged (today's
behavior) rather than failing the whole resolution. Confirming whether
the fallback is ever actually exercised for this fixture (versus the
alias path succeeding on the very first call) is implementation work
(tasks.md), not assumed here either way. This keeps `resolve_one`'s own
existing two call sites idempotent and side-effect-free to reorder; no
change to call order is needed.

**D3: Idempotency across the two-pass schedule and re-resolution.**
`advertise_one`'s own "already indexed - nothing to do" guard already
makes `export_node_id` stable across repeated `graphbuilder_advertise`
calls; `resolve_one` itself is reachable more than once per real run
(the two-pass schedule) with the SAME `export_node_id` each time. The
new procedure re-reads `comp_src`'s own real export state and
re-aliases on every call - cheap (one `ESMF_StateGet` plus one
`ESMF_NamedAlias`) and correct even if the underlying field object
changed identity between passes (it does not, in practice, but the
procedure does not assume otherwise). `materialize_field_extension`'s
own existing idempotent "already has a real field - no-op" guard means
a SECOND pass's call into the fallback `FieldCreate` path (D2) is also
already safe, unchanged from today.

**D4: Shared procedure, not two divergent copies.**
Both of `resolve_one`'s own call sites pass `export_node_id` as the
materialization target with `export_var_spec%short_name` as the name
(the exact-match branch, line ~849, and the mismatch branch's own
chain-input materialization, line ~904) - identical shape, differing
only in which `VariableSpec` argument pair is passed. One new procedure,
`try_alias_export_field(this, export_comp_name, export_var_spec, graph,
node_id, found, rc)`, called from both sites BEFORE the existing
`try_materialize_field_extension` call; if `found` is true, skip the
`try_materialize_field_extension` call entirely (the alias already gives
the node a real field - calling `FieldCreate` afterward would silently
overwrite it, defeating the whole point). If `found` is false, fall
through to today's unchanged `try_materialize_field_extension` call
(D2's fallback).

**D5: Reuse `MAPL_NamedAlias`, not a hand-rolled `ESMF_NamedAlias` call.**
`populate_one`'s own header comment already explains why: a raw
`ESMF_NamedAlias` call does not propagate MAPL's own per-alias-id
metadata (`standard_name`/`long_name`/`restart_mode`,
`infrastructure/field/FieldNamedAlias.F90`'s own module header). Reuse
`mapl_FieldNamedAlias_mod::MAPL_NamedAlias` exactly as `populate_one`
already does, with `name=export_var_spec%short_name` - the proxy node's
own alias then carries the same name-based stability
`ESMF_StateAddReplace`-based re-insertion already depends on, though
this proxy is never itself inserted into a real `ESMF_State` (only
`populate_one`'s import-side insertion is) - naming it anyway costs
nothing and keeps the convention uniform.

**D6: Test change scope.**
`Test_GraphModeDemonstration.pf`'s own existing static assertions (item
count, typekind, units, export-proxy-field-allocated) are kept as-is.
Add, after the existing init-phase sequence, a real
`ESMF_GridCompRun(cap_gc, ..., phase=GENERIC_RUN_USER, ...)` call
(mirroring `Test_GraphEquivalenceHarness.pf`'s own
`equivalence_fixture_setup` precedent for the identical call shape), then
read `comp_dst`'s own real import field's value via `ESMF_FieldGet(...,
farrayPtr=...)` (the same accessor
`Test_3DConservativeMixingRatio.pf`/`Test_CopyTransform.pf` already use
for a real numeric assertion) and assert it equals the fixture's own
hand-checked `1000.` (`10.` hPa -> `1000.` Pa, per
`units_mismatch_run_phase/description.md`'s own documented conversion).
No fixture file changes - `units_mismatch_run_phase`'s existing
`comp_src.yaml`/`comp_dst.yaml`/`cap.yaml` already provide everything
needed (`run: {T: [10]}` on `comp_src`).

## Risks / Trade-offs

- **[Risk]** D2's own phase-ordering trace is a static read, not a
  confirmed runtime observation - some NUOPC-level detail could still
  leave `comp_src`'s own real export field unavailable by name at the
  point this change's new procedure first runs, for a current or future
  fixture. The fallback (today's unchanged `FieldCreate` path) makes
  this never worse than before the change, but a case that silently
  takes the fallback would reproduce today's exact bug rather than
  fixing it, with no distinguishing signal to say so. Mitigation: D6's
  own new numeric assertion is the regression guard for the fixture
  this change targets - if the fallback is taken unexpectedly for
  `units_mismatch_run_phase`, that assertion fails loudly rather than
  passing silently; tasks.md's own implementation step confirms, by
  direct observation (not re-derivation), which path actually executes.
- **[Risk]** Aliasing a field that is later destroyed/recreated by
  `comp_src`'s own component (e.g. a geometry change under freeze,
  `17-open-questions.md`'s Q-level deferred concern) would leave the
  proxy's alias pointing at a stale handle. Out of scope - this
  capability's own existing freeze/validate discipline
  (`ComponentGraph%freeze()`) already assumes a component's structure,
  including its real fields, is stable post-freeze; time-dependent
  renewal is explicitly deferred elsewhere (§13.4/§14.4), unaffected by
  this change.
- **[Trade-off]** `try_alias_export_field` duplicates
  `component_for`'s own existing resolution of `export_meta` (already
  computed once inside `try_materialize_field_extension` itself) -
  accepted as the simpler, less invasive shape (D4) rather than
  threading `export_meta` through both call sites' own existing
  signatures a second time.
