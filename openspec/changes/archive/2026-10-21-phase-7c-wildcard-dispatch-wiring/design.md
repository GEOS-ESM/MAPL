## Context

`is_ordinary_resolvable_import` (`GraphBuilder_ConnectionResolution_
smod.F90`, line ~1989) currently excludes `MAPL_STATEITEM_WILDCARD` and
`MAPL_STATEITEM_SERVICE` uniformly (`MAPL_STATEITEM_EXPRESSION` was
removed from this list by Phase 7b, landed). `SERVICE` stays excluded —
out of scope here (Phase 6b's own retire-via-callbacks concern).

A `WILDCARD`-typed import is declared with a regex as its own short
name (`to_itemtype.F90`'s `case ('wildcard')`; e.g.
`history_wildcard/collection_1.yaml`'s `A/E_A.*`) and reaches
`resolve_one` exactly like any other import: `MatchConnection%activate`
(legacy) or, in graph-native code, `for_each_matching_import`
(`GraphBuilder_ConnectionResolution_smod.F90` line ~1054) already finds
it as a destination — `dst_pt%v_pt%matches(VirtualConnectionPt(IMPORT,
generic_node%get_short_name()))` is a regex comparison
(`VirtualConnectionPt%matches`, `regcomp`/`regexec`,
`connection/VirtualConnectionPt.F90` line 233) run unconditionally for
every import, wildcard or not. Confirmed directly: nothing in
`for_each_matching_import` special-cases `WILDCARD` — the one thing
missing is entirely on the source side. Every existing source-resolution
call site resolves to exactly one producer by exact-name equality
(`find_export_node_id`, `resolve_one`'s own `effective_source_name`/
`has_export` branch at line ~320-340); none of them re-run the
destination's own matched pattern against the source namespace, and
none of them accumulate more than one match.

Legacy's real mechanism (`WildcardClassAspect.F90`) confirms what the
accumulated result should look like: `matched_items`
(`ActualPtFieldAspectMap`) holds one entry per matched export;
`get_payload()` is an explicit no-op (`StateItemAspect.F90`'s own
contract — a Wildcard import never has one single payload of its own);
`add_to_state` (line 156) flushes `matched_items` into the import's
`MultiState` one entry at a time, under each match's own effective name
— each match optionally relabeled via `ActualConnectionPt%extend()`
(label `+1` per repeat) when more than one source match would otherwise
collide under the same name reaching the same destination slot
(confirmed directly: `extend_`, `connection/ActualConnectionPt.F90` line
86, `get_extension_string` renders the label as `(N)` — this is exactly
the `A/E_A1(1)` suffix `history_wildcard/expectations.yaml` line 54
shows for `history`'s own propagated-and-re-matched import, as opposed
to `A/E_A1` unsuffixed at `history/collection_1` one level down,
confirming the suffix means "already matched once elsewhere in this
same destination's own accumulation," not a naming defect). Legacy
therefore already models a `WILDCARD` import as "one logical slot, N
independently-named sub-items" — the same shape `composite-state-spec`'s
member mechanism (`declare_member`/`add_state_member`/`GraphStateItem%
state_members_map`) already gives graph-native code three times over
(`vertical-grid-graph-state-item`,
`callback-wiring`'s `materialize_callback_collection`,
`griddedcomponentdriver-integration-lifecycle`'s composite-state
handling).

`build_qualified_export_namespace`/`collect_qualified_exports`
(`GraphBuilder_CallbackWiring_smod.F90` lines 36-122) already builds
exactly the cross-level-qualified `(qualified_name, comp_path,
var_spec)` entry set (`QualifiedExportEntry`, `GraphBuilder.F90` line
397) that a source-side pattern scan needs — the same primitive
`resolve_callback_import` (`GraphBuilder.F90` line 1153) already drives
with `src_pt%v_pt%matches(VirtualConnectionPt(EXPORT,
namespace(i)%qualified_name))` to do exactly this kind of pattern-match-
against-flattened-namespace scan, just gated additionally on callback-
interface conformance. `resolve_qualified_export_node_id`
(`GraphBuilder_CallbackWiring_smod.F90` line 158) already resolves one
matched `QualifiedExportEntry` into its own real `NodeId`, wherever it
actually lives (`is_self`/`resolve_descendant`), with no proxy node of
any kind. `materialize_callback_collection` (line 269) already
demonstrates the exact "N matches -> one `ESMF_State`-backed
`GraphStateItem`, one `add_state_member` per match, zero
`add_dependency` edges" construction this change needs — only
difference: it always synthesizes a brand-new collection `NodeId`
(`graph%next_node_id()`), whereas a `WILDCARD` import already has its
own real, already-advertised `NodeId` (`advertise_one` already ran for
it, same as any other import) that this change should populate in
place instead (D1).

`DependencyNetwork%validate_producer_counts`
(`graph/DependencyNetwork.F90` line 352) enforces at most one producer
per state item, checked by walking each state item's own predecessor
set in the *default* dependency network. A composite member binding
(`add_state_member`) is not a dependency edge at all — zero
`add_dependency` calls, confirmed in both `materialize_callback_
collection` and this change's own planned construction (D1) — so this
invariant is never consulted for, and never conflicts with, any of the
N matched producers; each matched export keeps whatever producer
relationship (zero or one) it already has in its *own* component's
network, entirely unaffected by also being referenced as a composite
member somewhere else.

## Goals / Non-Goals

**Goals:**
- Add a new `resolve_one` branch, dispatched on
  `dst_node%get_declared_item_type() == MAPL_STATEITEM_WILDCARD`,
  parallel to the existing callback-interface and inout-borrower
  branches there.
- Re-run the destination's own already-matched pattern against the
  connection's own source export namespace (not an exact-name lookup),
  using the identical `VirtualConnectionPt%matches()` primitive
  `for_each_matching_import` already uses destination-side, and the
  identical `build_qualified_export_namespace` cross-level namespace
  `callback-wiring` already uses.
- On one or more matches: materialize the wildcard import's own
  already-advertised `NodeId` in place as a composite (real
  `ESMF_State` payload, one member per match, keyed by that match's own
  qualified name), reusing `materialize_callback_collection`'s
  construction pattern but targeting an existing node instead of
  synthesizing a fresh one.
- On zero matches: report a resolution failure for that import, not a
  silently "resolved" empty collection.
- Narrow `is_ordinary_resolvable_import` so `WILDCARD` is no longer
  unconditionally excluded.

**Non-Goals:**
- No change to `SERVICE` resolution or exclusion (Phase 6b's own
  concern).
- No change to legacy `WildcardClassAspect`/`MatchConnection`/
  `StateRegistry` (unaffected, remains the `GRAPH_MODE_REGISTRY` path).
- No support for a matched export whose characteristics mismatch the
  wildcard import's own declared characteristics (e.g. a units
  conversion needed on one specific match) — every matched export is
  bound as-is, by `NodeId` reference, exactly as
  `materialize_callback_collection` already does for callback matches;
  `find_mismatched_characteristics`/`find_or_build_extension_chain` are
  not invoked per match. `history_wildcard`'s own fixture declares no
  such mismatch (`collection_1.yaml`'s `vertical_dim_spec: MIRROR`); if
  a future fixture needs this, it is a follow-on, matching Phase 7b's
  own accepted "no fixture currently forces this gap closed" precedent
  for its own analogous per-reference-conversion gap.
- No new `Transform`/`TransformGraphNode` of any kind — this is pure
  reference binding (composite membership), not a value computation;
  there is nothing for a `Transform%compute()` to do here, unlike Phase
  7a (`Bracket`->`Field`) or Phase 7b (`Expression` evaluation).
- No relabeling/disambiguation scheme for two matches that would
  otherwise collide under the same qualified member name within one
  wildcard import's own collection (legacy's `ActualConnectionPt%
  extend()`/`(N)`-suffix mechanism, Context above) — not needed: a
  `QualifiedExportEntry%qualified_name` is already unique within one
  `build_qualified_export_namespace` scan by construction (one entry
  per advertised export, no two exports share the same `comp_path/
  short_name` pair), so two distinct matches for the same wildcard
  import can never produce the same member key in the first place. The
  `(N)` suffix legacy applies to the `history` level in `history_
  wildcard` is legacy's own `MatchConnection`-chain artifact (each
  intermediate `all_unsatisfied`/`MatchConnection` hop re-mints its own
  label when the SAME already-matched name is re-examined again one
  level up — confirmed by reading `MatchConnection%activate`,
  `connection/MatchConnection.F90` line 65: each matched `dst_pattern`
  loop iteration independently re-derives `src_pattern` and re-filters
  `src_registry`, with no shared state across hierarchy levels), not a
  property this change's own single-pass, qualified-namespace-based
  scan (D2) ever reproduces or needs to reproduce — this change
  resolves a wildcard import directly against the flattened, already-
  qualified namespace in one step, never through a chain of
  re-matched intermediate hops.
- No attempt to make wildcard resolution re-run/update when the source
  side's own export set changes after the first resolution (matches
  `materialize_callback_collection`'s own "resolved once" contract,
  guarded the same way — D4).

## Decisions

**D1: The wildcard import's own already-advertised `NodeId` is populated in place as a composite via `set`/`add_state_member`, not routed through `materialize_callback_collection`'s "always synthesize a fresh collection node" shape.**
`advertise_one` already gave this import a real `NodeId` and registered
it (`graph%register_node`), with an unallocated `GraphStateItem` payload
(line 117's "advertised, not yet realized"). Unlike a callback's
matched-interface collection (which has no corresponding
already-declared node of its own — `materialize_callback_collection`
must mint one), a `WILDCARD` import IS already a declared item with a
real identity other code already references (the same `NodeId`
`for_each_matching_import` found it by, already present in `dst_graph`'s
own import-port table). Reusing that identity directly — `get_payload_
ptr()` (the same in-place-mutation accessor `populate_characteristics`
already uses, chosen for the identical reason: avoiding the
intrinsic-assignment defined-assignment copy risk `StateItemNode.F90`'s
own header documents for a populated member map) — means every existing
reference to this import's `NodeId` (the destination end of whatever
connection matched it, any ancestor's own mirrored import-port entry)
continues to resolve to the one real node that now carries the
composite payload, with no second node and no rebinding needed anywhere
else. Synthesizing a fresh node instead (mirroring `materialize_
callback_collection` exactly) was considered and rejected: it would
require also rewriting the import's own `NodeId` wherever already
referenced (the import-port table entry itself, and any ancestor's
mirrored copy of it) — strictly more moving parts for no benefit, since
nothing here needs a second identity the way a callback interface's
collection does (an interface has no `NodeId` of its own before
`resolve_callback_destination` invents one; a `WILDCARD` import already
does).

Mechanically: `payload => dst_node%get_payload_ptr()`; `call payload%set(
state)` where `state = ESMF_StateCreate()` (same two calls `materialize_
callback_collection` makes, applied to the existing node's own payload
pointer instead of a fresh local `GraphStateItem`); then one `call
payload%add_state_member(match%qualified_name, member_id)` per match
(D2/D3), exactly as `materialize_callback_collection`'s own loop already
does.

**D2: Source-side matching re-runs `dst_pt%v_pt%matches()` against `build_qualified_export_namespace`'s qualified entries, mirroring `resolve_callback_import`'s own loop exactly — not a new matching primitive.**
`resolve_callback_import` (`GraphBuilder.F90` line 1153) already proves
this exact pattern works for cross-level pattern matching:
`candidate_v_pt = VirtualConnectionPt(EXPORT, namespace(i)%
qualified_name); if (.not. src_pt%v_pt%matches(candidate_v_pt)) cycle`.
The new branch does the same loop, over the same
`build_qualified_export_namespace(component_for(this, src_pt%
component_name))` call, with `dst_pt%v_pt` (the already-matched
destination pattern that got `resolve_one` invoked for this `dst_node`
in the first place) as `this` in `matches()` — i.e. `dst_pt%v_pt%
matches(candidate_v_pt)`, read right-to-left exactly as `for_each_
matching_import`'s own existing destination-side call already is (`dst_
pt%v_pt%matches(VirtualConnectionPt(IMPORT, generic_node%get_short_
name()))`), just with the EXPORT-side candidate substituted for the
IMPORT-side one and the regex-bearing operand (`dst_pt%v_pt`) unchanged
— the wildcard import's own declared pattern is reused as-is, not
re-derived.

No new helper type or matching primitive is introduced — this is a
second call site for an existing function, following an existing
caller's own exact usage, not new matching logic.

**D3: On a match, the member key is the matched `QualifiedExportEntry%qualified_name`; the bound `NodeId` is `resolve_qualified_export_node_id` applied to that same entry — both reused unmodified from `callback-wiring`.**
Identical to `materialize_callback_collection`'s own per-match loop body
(`member_id = resolve_qualified_export_node_id(this, src_pt%
component_name, matches(i)); call payload%add_state_member(matches(i)%
qualified_name, member_id)`) — no new resolution step; this change's own
matches array (D2) is the same `QualifiedExportEntry(:)` shape `resolve_
callback_import` already returns, so the exact same consumption code
applies with no adaptation.

**D4: Idempotency follows `resolve_callback_destination`'s own resource-index guard, keyed off the wildcard import's own `NodeId` — not off a connection identity.**
`resolve_callback_destination` guards itself via `'CALLBACK_COLLECTION:'
// import_id%to_string()` in `graph%get_resource_index`, checked before
doing any work. The new branch guards the same way:
`'WILDCARD_COLLECTION:' // dst_node%get_node_id()%to_string()`. Keyed
off the import's own `NodeId` (not, say, the matched connection) because
a `WILDCARD` import could in principle be reached by more than one
matching connection across repeated `resolve_one` invocations (the same
reason `resolve_callback_destination` keys off `import_id`, not off its
own `conn` argument) — the first successful resolution wins; a repeat
call for the same already-resolved import is a no-op, matching the
proposal's own "re-resolving does not duplicate members" scenario.

**D5: Zero matches is reported via the existing `unresolved`/`GraphResolutionEntry` convention, not `_FAIL`.**
Every other `resolve_one` branch that can legitimately fail to find a
producer (the ordinary exact-match branch's own `has_export` check at
line 335, `resolve_inout_destination`'s own owner-lookup) reports
through `unresolved%push_back(GraphResolutionEntry(dst_pt%component_
name, dst_node%get_short_name()))` or `unsupported%push_back(...)`,
never `_FAIL`s outright — a `_FAIL` would abort the entire connection-
resolution pass for an unrelated reason (one wildcard import having
zero matches is not a structural/programming error, exactly like one
ordinary import having no matching export is not). The new branch
reports through `unresolved` (the "no producer found at all" channel,
matching the ordinary branch's own semantics for the same underlying
situation — zero producers — rather than `unsupported`, which this file
reserves for "a producer was found but could not be adapted/combined,"
not applicable here since there is no adaptation step to fail (Non-
Goals)).

**D6: `is_ordinary_resolvable_import` drops the `MAPL_STATEITEM_WILDCARD` arm; `SERVICE` remains excluded, unchanged.**
Mirrors Phase 7b's own D6 exactly, one line: once the new branch gives a
`WILDCARD` import a real resolution path (D1-D5), the "no graph-native
resolution path exists for this item class" rationale the exclusion
documents no longer applies to it. Unlike Phase 7b's own expression
hook (which runs once per component, independent of any connection —
design.md D2/D7 there), this change's resolution runs exactly where
`WILDCARD` items are already reached today (`resolve_one`, driven by a
real `MatchConnection`/`SimpleConnection`/`all_unsatisfied` connection) —
there is no separate "has the hook run yet" ordering hazard analogous to
Phase 7b's D7, because this is not a new standalone hook; it is a new
branch inside the dispatch `resolve_one` already is.

## Risks / Trade-offs

- **[Risk] A `WILDCARD` import reached by zero connections at all (no
  `MatchConnection`/`SimpleConnection`/`all_unsatisfied` names it as a
  destination) never reaches `resolve_one`, and therefore never gets
  D6's narrowed gate benefit — it remains reported as unconditionally
  unresolved by `graphbuilder_unresolved_import_node_ids`'s own
  `is_ordinary_resolvable_import` check, exactly as it is today.** →
  Accepted: this is identical, in kind, to Phase 7b's own accepted
  D7 risk (an item with no real resolution attempt made against it yet
  cannot be anything other than excluded) and is not a regression —
  today, with zero narrowing, every `WILDCARD` import is unconditionally
  excluded regardless of connections; after this change, a `WILDCARD`
  import that some real connection actually matches is resolvable, and
  the no-connection-at-all case remains exactly as excluded as before.
- **[Trade-off] Non-Goals' "no per-match characteristic mismatch
  handling" is a real functional gap relative to legacy's own
  `WildcardClassAspect`/`FieldClassAspect%add_to_state`, which can
  route an individual match through the full `ExtensionFamily`
  conversion machinery.** → Accepted for this change's scope:
  `history_wildcard`'s own fixture (the only real forcing-function
  fixture for this capability) declares no such mismatch; revisit if a
  future fixture needs it, matching Phase 7b's own identical acceptance
  of the parallel gap in its own scope.
- **[Trade-off] D1's in-place payload mutation means a `WILDCARD`
  import's `GraphStateItem` transitions from "advertised, unallocated"
  to "composite, STATE-kind" only at resolution time, not at
  advertise time — any code that reads this node's payload between
  those two points (none currently does, for any import, before
  resolution runs) would see the pre-resolution unallocated state.** →
  Accepted: matches every other import's own identical "advertised,
  not yet realized" window (`advertise_one`'s own line 117 comment)
  already in effect for ordinary imports today; this change does not
  shorten or lengthen that window, only what the node looks like once
  it closes.

## Migration Plan

No data migration or rollback concerns — purely additive (one new
`resolve_one` branch, one narrowed exclusion arm, no new `Transform`,
no new graph-native type, no change to any existing stored format,
wire protocol, or restart file). No change to legacy `WildcardClassAspect`/
`MatchConnection`/`StateRegistry` or the `GRAPH_MODE_REGISTRY` path. A
build containing this change is not required to run in any particular
order relative to other in-flight graph changes. Rollback, if needed,
is a plain revert — no forward-only state is created.
