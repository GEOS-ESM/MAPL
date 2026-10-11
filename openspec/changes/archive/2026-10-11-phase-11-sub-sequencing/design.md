## Context

See proposal.md - Why for the full gap inventory. This document records
how §20.4.9's own two-item flat list is split into ordered sub-changes
and what each one is bound to before its own `openspec` change is filed.

Confirmed by direct code reading (not merely restating
`graph-child-state-population`'s own design.md D8):

- `ExtensionResolution.F90`'s `build_chain` (lines 192-254) allocates an
  `extension_node_id` per mismatched characteristic with `empty_payload`
  (line 212/227) and never materializes it — only
  `GraphBuilder.F90`'s own `try_materialize_field_extension` call sites
  touch a chain node's payload, and there are exactly two of them: one
  on `export_node_id` (the chain's INPUT, lines 904-905, landed by
  `graph-child-state-population`) and one on `final_node_id` (the
  chain's OUTPUT, lines 920-921, pre-existing). For a chain of length
  >= 2 (two or more mismatched characteristics), `build_chain`'s own
  loop (line 218) produces one or more INTERMEDIATE `extension_node_id`
  values that are never passed to either call site — confirmed by
  reading `find_or_build_extension_chain`'s own signature (lines
  138-149), which returns only `final_node_id`, discarding every
  intermediate `NodeId` `build_chain` computed internally (line 247,
  `current_node_id = extension_node_id`, overwritten each iteration
  with no accumulation).
- `try_materialize_field_extension` (lines 1266-1340) always calls
  `materialize_field_extension` (an independent `FieldCreate`, per
  `graph-child-state-population` design.md's own gap-3 finding) — never
  `MAPL_NamedAlias` — regardless of which node it is materializing.
  `populate_one` (lines 1963-2018) is the only place in `GraphBuilder.F90`
  that already uses `MAPL_NamedAlias` (line 2014), and it is only ever
  called against a destination `ESMF_State` on the IMPORT side
  (`graphbuilder_populate_state`'s two call sites, lines 1882/1920) —
  never against an export's own real field.

Two existing, already-landed mechanisms this phase reuses rather than
re-designs:

- `MAPL_NamedAlias` (`infrastructure/field/FieldNamedAlias_mod`,
  `mapl_FieldNamedAlias_mod`): `graph-child-state-population`'s own D1-D6
  already established the "always alias by name, never insert/duplicate
  the raw field" pattern for the import side's real `ESMF_StateAddReplace`
  insertion. 11a mirrors this same call, pointed at the export side's
  real field instead of a state-insertion destination.
- `find_or_build_extension_chain`'s own reuse-key/resource-index
  mechanism (`chain_key`, `graph%get_resource_index`/
  `add_resource_index`, lines 157-171): unaffected by either sub-change
  — 11a touches no chain-building code at all; 11b only widens what the
  procedure *returns*, not how it looks up or caches a chain.

## Goals / Non-Goals

**Goals:**
- Give §20.4.9's own two-item list the same ordered-sub-change treatment
  §20.4.1/§20.4.3/§20.4.4/§20.4.8 already gave their own phases: explicit
  scope boundary, dependency relationship, and recommended ordering for
  each item before either is implemented.
- Record why the two items are independent (neither needs the other's
  *code* to land) while still recommending an order (11a is useful and
  demonstrable alone; 11b is only useful for Phase 11's own stated goal
  once 11a has also landed).

**Non-Goals:**
- Implementing either sub-change. This proposal only records the
  sub-sequencing plan.
- Reopening Phase 10d's own double-invocation hazard, 3b2, or Phase 7c —
  all remain exactly as recorded elsewhere; neither 11a nor 11b touches
  invocation count or cross-component/pattern-based fan-in.
- Resolving `CopyTransform`-successor naming or any other Phase 10c/10d
  concern — unrelated to either gap.

## Decisions

**11a and 11b are independent sub-changes, not a strict chain — but 11a
is the recommended first pick.**

- **11a. Export-side field aliasing** — scope: give
  `try_materialize_field_extension`'s own `export_node_id` call site
  (`GraphBuilder.F90` lines 904-905) the same "always alias by name"
  treatment `populate_one` already applies on the import side —
  `MAPL_NamedAlias` of the real field `ConfigurableGridComp%run()`
  actually writes into, under the export item's own known
  `short_name`, rather than today's independent `materialize_field_extension`
  `FieldCreate`. Demonstrable immediately against the already-landed
  `units_mismatch_run_phase` fixture
  (`superstructure/generic/tests/graph-scenarios/`) — a one-hop chain,
  so no intermediate-node gap (11b's own concern) is in the way. Once
  11a lands, `Test_GraphModeDemonstration.pf`'s existing assertions can
  be extended to assert a real propagated numeric value for that
  fixture, which is Phase 11's own stated goal for the
  single-characteristic case. Depends only on landed
  `graph-child-state-population` work (D1-D6's alias pattern, D7's pull
  hook, the export-node materialization call site this sub-change
  modifies) — no dependency on 11b.
- **11b. Multi-hop extension-chain intermediate materialization** —
  scope: change `find_or_build_extension_chain`'s own public interface
  (`ExtensionResolution.F90` lines 138-149) to also return every
  intermediate `NodeId` `build_chain` computes (not only
  `final_node_id`), and have `GraphBuilder.F90`'s own call sites
  (lines 867-869, 1658, 1752, 2911) materialize each intermediate node
  with its own correctly-evolved typekind/units/geom (the state after
  an earlier characteristic's own conversion has notionally applied,
  before the next characteristic's own conversion runs) — mirroring
  `try_materialize_field_extension`'s existing per-node materialization
  call, invoked once per intermediate node instead of only at the
  chain's start and end. Needed before any two-or-more-characteristic
  chain (e.g. `units_and_typekind_mismatch`) can execute `compute()` for
  real without hitting `GraphStateItem: esmf_field is not allocated`.
  Depends only on landed Phase 1-5/10 work (`ExtensionResolution.F90`,
  `try_materialize_field_extension`) — no dependency on 11a's own code
  path (export aliasing is a different call site, same procedure).

**Recommended order: 11a before 11b, but not a hard gate.** The two
sub-changes touch disjoint code (11a: `try_materialize_field_extension`'s
export-node call site; 11b: `find_or_build_extension_chain`'s own return
signature plus every call site's handling of intermediate nodes) and
neither change's own correctness depends on the other having landed —
11b's intermediate-materialization fix is correct regardless of whether
the chain's final export-facing alias is real or synthetic, and 11a's
export-aliasing fix is correct regardless of how many hops a chain has.
The recommendation to do 11a first is purely about **payoff order, not
a technical dependency**: 11a alone already completes Phase 11's own
stated goal for the single-characteristic case (a real propagated run-
phase value, demonstrated against the existing `units_mismatch_run_phase`
fixture, zero new chain-interface risk); 11b alone, without 11a, would
let a multi-hop chain materialize cleanly but still could not
demonstrate a *real* propagated value end-to-end, because the chain's
own input would still be the independent-`FieldCreate`, not-aliased
proxy 11a is scoped to fix. Both landing is what unlocks a real
`units_and_typekind_mismatch`-style two-characteristic run-phase
demonstration — the recommended order gets there via the
lower-interface-risk path first.

**Neither sub-change reopens Phase 10d's double-invocation hazard.**
Both are state-materialization/aliasing concerns (what a `NodeId`'s own
payload points to), not invocation-count concerns (how many times a
`MethodGraphNode` or `GriddedComponentDriver%run()` fires) —
`graph-child-state-population`'s own D7/architectural-safety argument
(demand-driven `update()` never touches a `MethodGraphNode`) is
unaffected by either change and is not re-litigated by either
sub-change's own future design.md.

## Risks / Trade-offs

- **[Risk]** 11a's own `MAPL_NamedAlias` call could collide on name with
  an alias `populate_one` already inserted for the same item under a
  different code path (e.g. if a future change makes the export side
  also flow through `populate_one` directly) → **Mitigation**: 11a's own
  design.md (filed separately) must trace every call site that could
  insert a differently-named alias for the same underlying field before
  picking the export item's own alias name, following the same
  `ESMF_StateAddReplace`-name-matching discipline
  `graph-child-state-population`'s D1-D6 already established.
- **[Risk]** 11b's interface change to `find_or_build_extension_chain`
  touches four existing call sites in `GraphBuilder.F90` (lines
  867-869, 1658, 1752, 2911) — a signature change missed at even one
  call site would silently leave that site's own chains with
  unmaterialized intermediates → **Mitigation**: 11b's own design.md
  must enumerate all four call sites explicitly (not assume "the obvious
  one") and 11b's own tasks.md must include a repo-wide search for any
  other caller before closing out.
- **[Risk]** Doing 11a first (as recommended) without 11b could create
  an impression that Phase 11 is "done" once the single-characteristic
  fixture demonstrates a real value, understating that the
  two-characteristic case (11b) remains open → **Mitigation**: 11a's
  own proposal.md must explicitly scope itself to the single-
  characteristic case only and name 11b as the separately-tracked,
  still-open multi-hop gap, matching this document's own framing.

## Open Questions

None — both sub-changes' scope, independence, and recommended order are
settled by this document; each sub-change's own future design.md will
resolve its own implementation-level decisions (alias-naming collision
check for 11a; exact per-call-site materialization treatment for 11b)
when it is filed.
