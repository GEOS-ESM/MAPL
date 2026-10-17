## Context

See `proposal.md` - Why for full motivation. In short: `ComponentGraph`
is currently owned one-per-`OuterComponent` (REQ-HIER-004), making
`NodeId` only valid within the instance that issued it. Every landed
cross-component feature (3a/3b ordinary connections, 4e geometry
push/pull, 4c/4d callback wiring's flattened export namespace) invented
its own mechanism to reference a node across that boundary; 3b2
(cross-component unresolved-import propagation) was attempted and
abandoned specifically because neither existing crossing mechanism
(`item_key` nor `proxy_key`) fit the "search a subtree, don't bubble a
copy" shape it actually needed. Legacy's own export-side propagation
(unconditional push into a nested per-component `ESMF_State` substate
tree, feeding History's deep-hierarchy visibility) has no graph-native
analog at all today.

This change moves only `NodeId → GraphNode` and `DependencyNetworkId →
DependencyNetwork` storage to a single hierarchy-wide instance. Every
other per-component concern (resource index, ports, child-port
bindings, lifecycle flags) stays exactly as component-local as it is
today — this is a storage-ownership change, not a redesign of
`ComponentGraph`'s public shape.

## Goals / Non-Goals

**Goals:**
- Make a `NodeId` issued anywhere in the hierarchy a valid reference
  everywhere in the hierarchy, so a cross-component dependency edge
  needs no proxy/boundary node on either side.
- Retire, as a structural consequence, every proxy/qualified-path
  mechanism built to work around per-component graph storage:
  `proxy_key`/`get_or_make_local_node_id`'s cross-graph branch,
  `parent_geometry_proxy_key`/`push_geometry_to_child`, and
  `resolve_qualified_export_node_id`'s proxy-creation half.
- Replace 3b2's abandoned import-propagation concept with a single
  recursive mirror step per hierarchy level, reusing the existing,
  unmodified `graphbuilder_check_unsatisfied_imports` read-only check —
  not a search, not a new resolution mechanism, not a materialized
  copy (see design.md D3). The bubbled item is the same real `NodeId`,
  additionally reachable through one more component's own import-port
  table.
- Make History's own deep-hierarchy export visibility an ordinary
  shared-store query, with no push/propagation mechanism required.
- Keep ESMF-level component encapsulation (REQ-HIER-003: a child cannot
  reach its parent) exactly as strong as it is today, via an
  algorithmic invariant (connection-name resolution never walks upward
  past its declaring component) rather than via separate graph storage.
- Make component-name uniqueness, already an informal convention relied
  on by History, an enforced invariant.

**Non-Goals:**
- Not introducing any new capability-check / typed-reference mechanism
  to protect `NodeId` from component-author misuse. `NodeId` is, and
  remains, purely `GraphBuilder`'s own internal bookkeeping token — no
  component-author-facing API ever constructs, holds, or dereferences
  one (confirmed: component authors interact only through YAML/
  `VariableSpec` declarations and `src_comp`/`dst_comp` name strings).
  There is nothing for a runtime ownership check to protect against
  that the connection-name-resolution-direction invariant does not
  already cover.
- Not changing `ComponentGraph`'s resource index, port tables,
  child-port-binding storage, or lifecycle-state shape — these remain
  exactly as component-local as today.
- Not changing wildcard/callback/reexport connection resolution logic
  itself (Phase 4/7c scope) — only the mechanism by which a resolved
  match's node reference is used once found.
- Not attempting Phase 7c's wildcard/Service fan-in redesign as part of
  this change, though it is expected to simplify as a side effect once
  this lands (noted, not scoped here).
- Not changing `DependencyNetwork` *ownership* — which component's
  declared connections populate which network stays exactly as
  declaration-scoped as it is today. Only the storage map holding
  `DependencyNetworkId → DependencyNetwork` instances becomes shared;
  which network a given edge gets added to, and which component is
  considered the "owner" for validation/freeze-authority purposes, is
  unchanged.

## Decisions

### D1: Shared storage lives in a private, HierarchyId-keyed registry, not threaded pointers and not a bare singleton
Earlier drafts of this decision considered two alternatives, both
rejected on review (see below). The adopted mechanism:

- **`HierarchyId`**: a new identity type, generated exactly like
  `NodeId`/`DependencyNetworkId` (`IdTemplate.inc` instantiation, its
  own `HierarchyIdGenerator`). A leaf value type with no dependency on
  `OuterMetaComponent` or `ComponentGraph`.
- **`HierarchyGraph`**: the actual hierarchy-wide `NodeId → GraphNode`
  map, `DependencyNetworkId → DependencyNetwork` map, and the one
  `NodeIdGenerator`/`DependencyNetworkIdGenerator` pair that issue
  fresh ids for the whole hierarchy. (`PortIdGenerator` is unaffected —
  port identities stay scoped to the single component that declares
  them, exactly as today; only the node/network generators move.)
- **`HierarchyResources`**: a container type holding one `HierarchyGraph`
  field today, deliberately named and shaped to also hold future
  hierarchy-wide shared resources (e.g. a geometry registry) without
  requiring another parallel threading mechanism when those land —
  that extension is explicitly not done in this change.
- **A private, module-level `HierarchyId → HierarchyResources` registry**
  (not exposed directly — same wrapper-procedure convention
  `CallbackInterfaceRegistry` already establishes), with three public
  entry points: `request_new_hierarchy()` (mints a fresh `HierarchyId`,
  allocates a fresh `HierarchyResources` entry, returns the id),
  `get_hierarchy_resources(id)` (looked up fresh on every call, never
  cached across statements by any caller), and `release_hierarchy(id)`
  (erases that one entry). `release_hierarchy` is safe to call while
  other hierarchies' entries remain live in the same registry: gFTL's
  map container is deep-copy-on-move, so erasing one entry does not
  invalidate a pointer held into a different entry returned by an
  earlier `get_hierarchy_resources` call — only a pointer into the
  released entry itself becomes stale, exactly as `erase()`'s ordinary
  contract already implies. A standalone test fixture may call
  `release_hierarchy` on its own `HierarchyId` once done (eliminating
  the registry-growth concern noted below); nothing in this change
  requires every existing test to do so, since not doing so is a
  bounded memory cost, not a correctness one (see Risks).

`OuterMetaComponent` holds a `HierarchyId` (a plain value, not a
pointer) rather than any direct reference to shared storage. The root
of a hierarchy calls `request_new_hierarchy()` once, at construction;
`add_child_by_spec` passes `this`'s own `hierarchy_id` straight down to
each child's construction, so every descendant at any depth resolves
to the same registry entry. `ComponentGraph` likewise holds only a
`HierarchyId`, calling `get_hierarchy_resources(this%hierarchy_id)`
internally wherever it needs the shared maps/generators, rather than
caching any pointer into its own stored components.

A standalone/synthetic `ComponentGraph()` (today's ~40 existing test
files' call shape, no `OuterMetaComponent` involved) keeps working
unchanged: with no `HierarchyId` supplied, the constructor calls
`request_new_hierarchy()` for itself, exactly as the per-component
registry mechanism already does elsewhere — no new argument is
required at any existing test call site.

**Why not threaded pointers** (an earlier draft of this decision): a
`SharedGraphStorage` pointer, allocated once at a hierarchy's root and
passed down through `OuterMetaComponent`'s own constructor/`init_meta`
argument lists, was considered first. It foundered on a concrete
wiring problem: a child component that declares its own
`user_setservices` is constructed through the exact same public
`mapl_GridCompCreate` entry point ordinary user code calls to create an
entirely new hierarchy's *root* (`GenericGridComp.F90`'s
`create_grid_comp_primary`) — so giving that single public entry point
an extra "inherit this pointer instead of allocating fresh" argument
would have meant growing a widely-used, user-facing public signature
with a pointer-shaped plumbing concern, and still would not have
composed cleanly with a future `Hierarchy`-object redesign of that
same entry point (deferred — see `20-implementation-roadmap.md`'s new
phase entry) without risking a circular module dependency between that
future type and `ComponentGraph`/`OuterMetaComponent`. A plain
`HierarchyId` value has none of that circular-dependency risk: it is a
leaf type, so `create_grid_comp_primary`/`mapl_GridCompCreate` only
needed one new **optional** keyword argument (`hierarchy_id`, absent
by default — every existing call site is source-compatible, unchanged
in behavior) rather than a structural pointer-threading change; every
other construction path (`add_child_by_spec`'s own direct
`OuterMetaComponent()`/`init_meta()` branch) threads it exactly like
`child_spec`'s other scalar fields already are, through the existing
call shape, with no signature change at all.

**Why not a bare module-level singleton** (`CallbackInterfaceRegistry`'s
own actual, non-keyed design): rejected for the same
multiple-independent-hierarchies hazard the pointer-threading draft
also had to guard against — two separate test fixtures constructed in
the same process (which pFUnit already does routinely) would silently
share one global store. `CallbackInterfaceRegistry` accepts that hazard
already (REQ-CB-008: "a full MAPL application generally has one
component hierarchy"), but node/network storage is mutated far more
heavily and far earlier in every synthetic-graph test than callback
interface registration ever is, making accidental cross-test bleed a
real risk rather than a theoretical one. Keying the registry by
`HierarchyId` (rather than being bare) is what gets the singleton
pattern's simplicity (no pointer-threading, no circular-dependency
risk) without that hazard: every hierarchy, including every
independent test fixture, gets its own distinct registry entry.

Moving the node/network generators into `HierarchyGraph` (not just the
maps) is required, not optional: two independent per-`ComponentGraph`
generators each restarting at their own first value (today's actual
behavior, `IdTemplate.inc`'s `next_value = 1` default) would let two
different components' generators both issue, say, `NodeId(1)` —
harmless today because each lives in its own private map, but a direct
collision once both land in one shared map (`graph/identities`'s "two
different components' nodes must never share a `NodeId`" requirement
cannot hold otherwise). One shared generator, not a per-instance one
with after-the-fact collision checking, is what the "fresh and
non-repeating" framing of that requirement actually commits to.

### D2: Ownership tracking is explicit, not inferred from storage locality
Today, "which `NodeId`s belong to me" is answered implicitly by a
`ComponentGraph` instance's own private `nodes`/`networks` map keys.
Once storage moves into the shared `HierarchyGraph` (D1), that
implicit signal disappears — every component's ids now live together
in the one registry entry its `HierarchyId` resolves to — so each
`ComponentGraph` instance instead keeps its own explicit, private
`NodeIdSet`/`DependencyNetworkIdSet` of the ids it itself registered
(populated at the same point a node or network is registered) -
a forward, per-instance record ("what's mine"), not a reverse,
hierarchy-wide lookup ("who owns an arbitrary id").

This explicit per-instance set is what `graph_add_dependency` and
`graph_validate` already consult today via `owned_node_ids()`
(`ComponentGraph.F90`); after this change `owned_node_ids()` returns
that explicit set instead of walking private map keys, with no change
in call shape or query direction at either existing call site.

No reverse `NodeId → owning component` lookup is introduced. The only
candidate real use for one - the connection-endpoint-resolution-
direction invariant - is actually checked by `resolve_qualified_component`'s
own tree walk (`GraphBuilder.F90`), which resolves a declared component
*name* to a component by walking the `OuterMetaComponent` tree starting
from the declaring component; it never starts from a `NodeId` and asks
"whose is this," so it needs no reverse map either. Searching the
existing call sites found no place that genuinely needs to go from an
arbitrary `NodeId` to its owning component - this corrects an earlier,
overbuilt draft of this decision that proposed exactly that reverse
map.

**Alternative considered (the earlier draft of this decision)**: a
single hierarchy-wide `NodeId → owning component` map, populated
alongside the shared node store. Rejected on review: every real
consumer identified (`add_dependency`'s/`validate`'s own-ids check, the
connection-direction invariant) only ever needs "is this id mine"
(forward, per-instance), never "whose is this id" (reverse,
hierarchy-wide) - building the reverse map would be strictly more
mechanism than the per-instance set, for no caller that needs it.

**D2 correction (discovered during Group 3/4/5 implementation):** the
paragraph above (and REQ-CG-004's "Invalid foreign node is rejected"
scenario) describes `add_dependency`'s ownership check as "the own-ids
check," unqualified - this was written before proxy removal and
assumed every edge endpoint would still be one of `this` instance's own
ids (a proxy, standing in locally). Once proxies are retired (tasks.md
Groups 3-5), a cross-component edge's two endpoints are legitimately
two *different* components' own real `NodeId`s - neither is `this`
instance's own by the forward-only `own_node_ids` record D2 describes.
`add_dependency`/`validate`/`bind_port` (`ComponentGraph.F90`) are
therefore relaxed to accept an id if it is EITHER in `this` instance's
own `own_node_ids` OR genuinely registered somewhere in this exact
hierarchy's shared `HierarchyGraph` (`allowed_foreign_node`,
`ComponentGraph.F90`) - checked via `get_node(id)` being associated,
never via a reverse map (D2's own "no reverse lookup" decision is
preserved; `allowed_foreign_node` answers "does this id exist
anywhere in this hierarchy," never "who owns it"). This is
deliberately NOT "any id is valid" - an id never registered by any
component in this hierarchy (a genuinely foreign/garbage value) is
still rejected, preserving REQ-CG-004's scenario. The actual
descendant-only restriction (which ids a caller can even obtain to
pass to `add_dependency` in the first place) is enforced earlier and
separately, by `resolve_qualified_component`'s one-directional tree
walk (graph/component-hierarchy's "Connection endpoint resolution never
resolves upward past its declaring component") - `ComponentGraph`
itself still does not, and structurally cannot (REQ-CG-002), re-derive
that tree-shaped invariant.

This correction only applies to Groups 3-5 (ordinary connections,
geometry, callback wiring - the proxy-removal groups). Group 9
(hierarchy-wide freeze) was not implemented in this pass; until it
lands, `validate()`/`freeze()` still run per-component, so each
`ComponentGraph` instance validates its own owned networks using this
relaxed, per-call check (every id it references, whether its own or a
resolved cross-component one, must be registered somewhere in the
hierarchy) - not a hierarchy-wide validation pass.

### D3: Propagation is one recursive mirror step per level, reusing `check_unsatisfied_imports` unmodified — not a search, not new resolution logic
**Superseded design, kept for the record.** An earlier draft of this
decision proposed replacing 3b2's propagation with a search: resolving
a connection declared at component P, naming destination D, would
search D's own subtree for an unresolved import matching the
connection's name, with no node materialized to represent the bubbled
item. **This was wrong, on two counts, both caught in review before
any code was written against it:**

1. **It requires a connection to trigger.** `scenarios/
   graphbuilder_propagation/comp_mid.yaml` declares *no* connection of
   its own, deliberately — the fixture's own comment states legacy's
   propagation is unconditional, independent of whether anything at
   that level names the propagating component. A search triggered by
   resolving a *named* connection cannot reproduce this: nothing at
   `cap`'s level names `comp_mid` specifically for the purpose of
   making `comp_mid`'s own interface complete; `cap`'s connection only
   happens to name `comp_mid` as its own declared destination, which is
   a different thing from "every level propagates, always."
2. **It never materializes anything at the intermediate level,** so
   `comp_mid`'s own import-port table (what `for_each_matching_import`
   — the existing, unmodified, single-level scan every other
   connection-resolution path already uses — actually walks) never
   gains a real entry for `comp_dst`'s `T`. A search bypassing that
   table is a second, parallel resolution mechanism living alongside
   the first, not a reuse of it — exactly the kind of GraphBuilder-
   grown-large problem this whole change exists to undo.

**Corrected design.** MAPL's own init-phase recursion already walks the
hierarchy bottom-up (`initialize_advertise.F90`'s `recurse()`, called
before this component's own advertise/connection work runs) — by the
time any component's own advertise-phase processing resumes after that
call, every direct child has *already* finished its own full advertise
pass, including (by induction) mirroring *its own* children's
still-unresolved imports into itself. Propagation therefore needs no
new traversal of its own; it needs exactly one new step, run once per
level, right where legacy's own `propagate_unsatisfied_imports` already
sits in that same file:

```
graphbuilder_run_propagate_hook(this):
  for each DIRECT child C of `this` (unconditionally — no connection
      naming C is required, matching legacy's own unconditional
      behavior):
    call graphbuilder_check_unsatisfied_imports(C, unresolved_imports=unresolved)
       — the EXISTING, UNMODIFIED, read-only procedure (already landed
       in 3b/3c), invoked here on behalf of C instead of C invoking it
       on itself — the same "parent reaches into child" framework-
       internal carve-out GraphBuilder already documents for
       get_child_component_graph/get_child_component_spec.
    for each entry in `unresolved`:
       id = C's own graph's resource index lookup (item_key, C's own
            graph) — C's own real NodeId, unchanged, no copy
       this_graph%add_import_port(fresh_port_id, id)
            — the SAME real NodeId, now ALSO reachable through `this`'s
            own import-port table, in addition to C's own. No new
            node. No proxy. No composite wrapper.
```

**Why no new predicate, no new flag, and no mutation-contract change
were needed**: `graphbuilder_check_unsatisfied_imports` already answers
exactly the question propagation needs — "did a connection declared
*on this component itself* resolve this import" — because that is
already its entire existing job (3b's own single-level match check).
Propagation's only novelty is calling that same existing procedure
*reflectively*, from the parent's perspective about a child, instead of
a component calling it on itself, and *consuming* its existing
unresolved-entries output for an additional purpose (materialize a port
entry) beyond today's sole purpose (log a warning). The procedure's own
non-mutating contract is unchanged — it still creates no edge, no
proxy; only the NEW caller-side step (`add_import_port`) mutates, and
it mutates in exactly the same way `advertise_one` already does for an
ordinary item (an existing, already-specified operation), not in any
new way.

**Why this needs no change anywhere in ordinary connection resolution**
(the thing D3's superseded draft got right for the wrong reason, by
accident): once a bubbled import is a real `PortId → NodeId` entry in
`this`'s own `get_import_ports()`, `for_each_matching_import`'s
existing scan finds it exactly as it would find any item `this`
advertised directly — same short name, same state intent, same
`StateItemNode` shape. Two further properties fall out for free, with
zero new code, because `for_each_matching_import`/`resolve_one` were
already shaped to allow them:

- **Multiple distinct children bubbling the same short name** (e.g.
  several descendants each needing `T`, satisfied by one distant
  cousin's export): `for_each_matching_import` already iterates every
  port matching a connection's pattern, not a single lookup assuming
  uniqueness — a second bubbled `T` entry (a second `PortId`, a
  different real `NodeId`) is simply a second iteration of the same
  loop, calling `resolve_one` a second time.
- **Each one independently taking its own mismatch/extension path**
  (per the user's own stated requirement: MAPL3, unlike MAPL2, allows
  distinct bubbled items of the same name to have different specs and
  inserts couplers as needed): `resolve_one` already runs
  `find_mismatched_characteristics`/`find_or_build_extension_chain`
  once per matched node, inside the per-match callback — a second
  bubbled `T` with different units takes a different (chain-building)
  branch than a first bubbled `T` with matching units, automatically,
  because each invocation of `resolve_one` is already independent.

Neither property required this decision to invent anything — both are
consequences of reusing the existing single-level machinery unchanged,
which is the entire point of mirroring upward rather than searching
downward.

**The "second map" the user asked about is the existing import-port
table itself, not a new type.** `graphbuilder_populate_state`'s
existing var_spec-driven walk (both its "this component's own
`var_specs`" branch and its "each declared child's own `var_specs`"
branch) can never pick up a bubbled import, because a bubbled import
has no backing `VariableSpec` on `this` at all — it exists only as a
port-table entry. This already gives the required separation (bubbled
items are structurally invisible to any var_spec-driven, user-facing
state population path) with no new storage: `ComponentGraph`'s
existing `PortIdNodeIdMap` import-port table is already distinct from
`ComponentSpec%var_specs`, and a bubbled import is only ever present in
the former.

**Genuinely open, not resolved by this decision, named explicitly
rather than silently dropped**: inserting a bubbled import into the
GenericGridComp's own *real* `ESMF_State` (as opposed to the graph-side
port-table entry this decision covers) needs a new branch in
`graphbuilder_populate_state`, which currently has no "insert a
port-table-only item with no backing var_spec" case at all. Per the
user's own ESMF-notation correction: `ESMF_StateGet` resolves a
slash-containing name (`comp_dst/T`) as "item `T` in nested state
`comp_dst`" automatically on retrieval, so a child-qualified insertion
name gives correct addressing for free on the read side — but the
*write* side still needs the same manual `get_substate`-style nesting
`FieldClassAspect%add_to_state` already does for the ordinary case,
applied to this new bubbled-item branch, which this change does not
yet implement. Tracked as follow-up scope (tasks.md Group 6, revised),
not silently assumed solved.

### D4: Export visibility (History) is a query, not a push
Legacy's `propagate_exports_all`/`propagate_exports_subregistry`
unconditionally pushes every descendant export into a nested
`ESMF_State` substate tree on every init pass. The graph-native
equivalent needs no equivalent push step: `build_qualified_export_
namespace`'s existing recursive walk (today recomputed from scratch on
every call, specifically because no shared storage existed to index
once) becomes an ordinary query against the shared node store, keyed by
`(owning component's qualified path, state_intent, short_name)` — a
component's own exports are already discoverable through its own
component's local resource index; History (or any other hierarchy-wide
consumer) walks the `OuterMetaComponent` tree once to resolve component
names (per D5 below) and reads each component's own local index
directly, with no separate "has this already been pushed" bookkeeping
needed, since there is nothing to push.

### D5: Qualified-path resolution stays a tree walk; only the proxy step at the end is removed
Per the user's own correction during discovery: component names are
unique across the entire hierarchy, so finding "the component named X"
is a different problem from finding "item T within that component."
The first (`resolve_qualified_component`'s existing `OuterMetaComponent`
tree walk) is unaffected by this change — it already works, and remains
necessary regardless of node storage, because the hierarchy tree itself
is still the only structure that encodes parent/child relationships.
What changes is only what happens after that walk succeeds: today,
`resolve_qualified_export_node_id` and `get_or_make_local_node_id`
additionally create-or-cache a proxy node in the *calling* component's
own graph once the target component and item are found. That proxy step
is deleted outright; the real `NodeId` found via the target component's
own local resource index is used directly as the edge endpoint.

### D6: Composite-member non-addressability and connection-direction invariant are stated as requirements, not new code
Both were confirmed, during discovery, to already hold structurally in
the current implementation (`advertise_one` only ever registers a
composite's own top-level identity into the port table, never its
members; `resolve_qualified_component`'s walk has no "look at my own
parent" code path to begin with). This change promotes both from
"incidentally true because of how the code happens to be written" to
explicitly stated, test-covered requirements (see the delta specs) —
specifically because, once storage is shared, the thing that used to
make a violation structurally impossible (separate per-component
storage) is gone, and these two invariants are what take over its job.
No implementation change is required for either; only new scenario
coverage confirming they still hold once storage is shared.

### D7: Freeze becomes one hierarchy-wide call
Per explicit user direction: freeze changes from "once per
`ComponentGraph` instance, triggered independently by each component's
own `ACCEPT_TRANSFER` phase" to one call, made once the whole
hierarchy's init-phase sequence has run to completion. This is simpler
than today's per-component freeze-ordering concerns (11a/11b both had
to reason about which component freezes before another's edges might
still need to reach into it) and is justified by ESMF's own init-phase
structure already applying coherently across the whole hierarchy in one
coordinated pass — there is no real case where one component's
structure is "done" while a sibling's is still being wired, that this
design needs to accommodate.

## Risks / Trade-offs

- **[Risk] A single shared store becomes a bottleneck or an accidental
  single point of failure for the whole hierarchy's graph state** → not
  expected to matter at MAPL's target scale (roadmap's own ~10,000-node
  whole-hierarchy figure, `02-component-hierarchy.md` §2.2) — a gFTL map
  of that size is not a performance concern; mitigation if ever needed
  is sharding by component subtree, deferred unless evidence appears.
- **[Risk] Multiple independent hierarchies in one process (pFUnit test
  suite) accidentally share state** → mitigated by D1's registry being
  keyed by `HierarchyId` (not a bare singleton): each hierarchy, real or
  synthetic-test, gets its own distinct `HierarchyResources` entry;
  flagged here so a future "simplify by making the registry bare"
  refactor does not silently reintroduce the cross-test-bleed hazard D1
  was chosen to avoid.
- **[Risk] A standalone `ComponentGraph()` test fixture's registry entry
  could accumulate unboundedly if never released** → mitigated by D1's
  own `release_hierarchy(id)` entry point; a test fixture that wants to
  release its entry explicitly may do so (safe to call mid-suite, with
  other hierarchies' entries unaffected — see D1). Not required at
  every existing test call site, since production use constructs
  exactly one hierarchy for the life of the process regardless (no
  accumulation possible there) and unreleased test entries are a
  bounded footprint cost, not a correctness one.
- **[Trade-off] Ownership tracking (D2) is a new piece of bookkeeping
  that did not exist before** → accepted; it replaces an *implicit*
  signal (storage locality) that is being deliberately removed, not
  adding net new conceptual surface — "which ids are mine" was always a
  real fact for a `ComponentGraph` instance to know, now it is tracked
  explicitly instead of being derived from which map happened to
  contain the key.
- **[Trade-off] A full `Hierarchy`-as-factory redesign of component
  construction (threading a user-visible object through
  `mapl_GridCompCreate` itself, eventually also owning other
  hierarchy-wide shared resources such as a geometry registry) is a
  materially larger, cross-cutting change than this one** → deferred to
  a future change (tracked in
  `docs/graph/spec/20-implementation-roadmap.md`'s new phase entry, task
  10.3); `HierarchyId`/`HierarchyResources` as designed here deliberately
  leave room for that later redesign (a `Hierarchy` type could become a
  thin, user-facing wrapper around one registry entry, reached by its
  own `HierarchyId`) without requiring this change's own mechanism to be
  reworked first.

## Migration Plan

No runtime data migration — this is a source-level architecture change
with no persisted state crossing a version boundary (freeze/revision
state is in-memory only, recreated each run). Implementation sequencing
(left to `tasks.md`, not fixed here): extract shared storage first
behind the existing `ComponentGraph` API shape (so most call sites don't
change yet) → remove each proxy mechanism one at a time, in the same
order they were originally landed (ordinary connections, then geometry,
then callback-wiring) so each removal's own existing test coverage
continues to validate it in isolation → implement the recursive
import-mirroring propagation hook (3b2's replacement, D3 above),
reusing `graphbuilder_check_unsatisfied_imports` unmodified → implement
the hierarchy-wide freeze call site → add the component-name-uniqueness
check. Each step should be independently testable against the existing
scenario-fixture suite (`superstructure/generic/tests/scenarios/`,
`graph-scenarios/`) before the next step begins, per this project's own
"validated, not merely built" discipline (roadmap §20.4.2's entry
criterion, applied here even though this is not itself a Phase 6
retirement).
