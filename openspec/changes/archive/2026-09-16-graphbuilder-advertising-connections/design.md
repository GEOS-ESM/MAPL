## Context

Phase 1–2 shipped `ComponentGraph`/`DependencyNetwork`/`StateItemNode` as a
graph-neutral core (`superstructure/generic/graph/ComponentGraph.F90`), with
`register_node`, `create_network`/`get_default_network_id`,
`add_import_port`/`add_export_port`/`add_child_port_binding`, and network
`add_dependency`/`validate`/`freeze` all already implemented and tested with
synthetic data. Phase 3a (`component-hierarchy-foundation`) gave every
`OuterMetaComponent` exactly one such `ComponentGraph`
(`get_component_graph`), but nothing calls any of the above APIs from real
component wiring yet.

Real wiring today happens entirely through the legacy imperative path:
`StateRegistry`/`ComponentSpec` hold advertised-variable data
(`superstructure/generic/registry/`, `superstructure/generic/specs/`), and
`OuterMetaComponent%connect_all` (`superstructure/generic/OuterMetaComponent/connect_all.F90`)
builds a `MatchConnection` (`superstructure/generic/connection/`) that
`StateRegistry_Actions_smod` executes to bind imports to exports by exact
short-name match — precisely the case REQ-GB-003's "advertising" and
"resolving connections" rows describe as `GraphBuilder`'s job, and precisely
what `17-open-questions.md` Q10 step 5 says must be reproduced *exactly*
before any legacy code path is touched.

See `proposal.md` for full motivation and scope boundary (exact-match only;
mismatch/wildcard/callback/compilation excluded).

## Goals / Non-Goals

**Goals:**
- Populate each component's local `ComponentGraph` with `StateItemNode`s for
  every advertised item, using the existing Phase 1–2 API as-is (no core
  API changes).
- Resolve ordinary (exact short-name match) import/export connections into
  `DependencyNetwork` edges in the component's default network, and
  populate public ports / child proxies for the connections that need them.
- Prove, on at least one real configuration, that this produces the same
  import→export pairings as the existing `MatchConnection`/
  `StateRegistry_Actions_smod` path, without deleting or modifying that
  path.

**Non-Goals:**
- Extension/mismatch-chain creation (3c), wildcard/callback resolution
  (Phase 4), or compiling the graph into direct ESMF wrappers (Phase 5/Q9) —
  none of these are touched here.
- Any change to `ComponentGraph`/`DependencyNetwork`/`StateItemNode` public
  APIs — Phase 1–2's surface is sufficient for this slice; if it turns out
  not to be, that is a signal to stop and revisit rather than quietly
  extending Phase 1–2 API from inside a Phase 3 change.
- Removing or disabling the legacy imperative coupler path — it remains the
  equivalence oracle until a later phase's decision, per Q10's explicit
  recommendation to keep it as a fallback.

## Decisions

**GraphBuilder is a stateless-per-call procedure set, not a persistent
object.** `08-graph-builder.md` describes `GraphBuilder` as a layer/role,
not a specific data structure. Modeled here as a set of module procedures
(`mapl_GraphBuilder_mod`) taking an `OuterMetaComponent` (or its
`ComponentSpec`/`StateRegistry`) and operating on its already-attached
`ComponentGraph` via the public accessor from 3a
(`get_component_graph`). Alternative considered: a `GraphBuilder` type
instance owned somewhere per component. Rejected for this slice — there is
no cross-call state to hold (no partial-resolution checkpointing is in
scope), and a bare procedure set keeps REQ-GB-001's one-directional
dependency easy to verify (the module `use`s `StateRegistry`/
`OuterMetaComponent` types directly; nothing in the reverse direction).

**Invocation point: alongside the legacy phases, not replacing them —
and split across two phases, not one.** Corrected mid-implementation
(the original version of this decision put all of `GraphBuilder`'s
connection work at `GENERIC_INIT_ADVERTISE`; that was wrong). The legacy
imperative coupler itself is two-phase, and the phases mean different
things:

- `initialize_advertise.F90`'s `process_connections()` calls
  `Connection%activate()`. This does NOT form real wiring - it only marks
  which imports/exports are "active," which existing MAPL machinery uses
  immediately afterward to decide (a) whether an unresolved import needs
  to bubble up to the parent (`propagate_unsatisfied_imports()`) and
  (b) whether an export actually needs to be allocated at all (only
  allocated if some active import needs it).
- `initialize_accept_transfer.F90`'s `process_connections()` calls
  `Connection%connect()` - the real wiring step (aliasing, producer
  assignment). `initialize_modify_advertised.F90` has a second
  `process_connections()`/`connect()` pairing too, but its call site is
  commented-out dead code in the current tree - `connect()` only actually
  runs at `GENERIC_INIT_ACCEPT_TRANSFER`.

`GraphBuilder` mirrors this with two separate entry points instead of
one, called from the two corresponding lifecycle hooks:
- `graphbuilder_check_unsatisfied_imports()` - the `activate()`-time
  analog, called from `initialize_advertise.F90`. Read-only: determines
  which ordinary-match imports would be satisfied, for the
  unresolved-imports report, WITHOUT creating any graph structure (no
  proxy nodes, no dependency edges).
- `graphbuilder_resolve_connections()` - the `connect()`-time analog,
  called from `initialize_accept_transfer.F90`. The real graph mutation:
  proxy nodes and dependency edges. `graphbuilder_freeze()` runs
  immediately after, in the same hook - freezing before the real wiring
  exists would be premature.

Both remain *additional* to the legacy calls, never replacements; the
legacy path stays the ESMF-visible source of truth in this slice.
Removing it is explicitly deferred (see proposal.md Impact / Q10).

**Idempotency: `graphbuilder_resolve_connections()` must tolerate being
called more than once per component.** Discovered mid-implementation via
a real end-to-end test failure (`ComponentGraph: add_dependency called on
a frozen graph`): ESMF's Provide/Accept/Realize transfer-negotiation
protocol can invoke `GENERIC_INIT_ACCEPT_TRANSFER` more than once per
component within a single `Initialize` sequence. Legacy's own
`MatchConnection%connect()`/`SimpleConnection%connect()` already guard
against exactly this with a per-connection `consumed` flag
(`superstructure/generic/connection/*.F90`). `GraphBuilder` has no
per-connection object to hang an equivalent flag on, so it guards at the
graph level instead: `graphbuilder_resolve_connections()` returns
immediately (success, no-op) if the component's graph is already frozen,
since freezing only happens after a first successful resolution.

**Ordinary-connection matching reuses `MatchConnection`'s semantics, not
its implementation.** `GraphBuilder` recomputes exact-short-name matches
directly against `StateRegistry`'s advertised-item names (read-only) rather
than calling into `Connection`/`ConnectionPt` machinery, because that
machinery is built to *execute* a resolved connection against ESMF states,
not to hand back a graph-friendly `(export NodeId, import NodeId)` pairing
list. Two independent implementations of "exact match" is exactly the risk
the equivalence test (proposal.md, spec `Ordinary resolution matches
existing coupler behavior`) exists to catch — it is treated as a required
regression check, not an optional nice-to-have, specifically because of
this duplication.

**Default network creation is idempotent via `get_default_network_id`.**
`ComponentGraph` already creates its default network in its own
constructor (per 3a's task notes on `ComponentGraph()`), so "create first
resolution creates the default network" (spec) resolves in practice to
"the graph already has one from construction" — `GraphBuilder` calls
`get_default_network_id()` and adds edges there; no separate
`create_network()` call is needed for the default-network case. This
matches the spec's observable behavior (a network exists after first
resolution) without requiring `GraphBuilder` to special-case "is this the
first call."

**Unresolved imports are reported, not silently dropped.** Per the spec's
"Import with no matching export is left unresolved" scenario,
`GraphBuilder` returns/logs an explicit list of imports it could not
resolve at the ordinary-match level, distinguishing that case from "this
import needs extension-chain handling" (3c) only insofar as 3b does not
attempt to tell the two apart yet — both currently surface as "unresolved
by this slice." 3c's job is to consume that same unresolved-import
information and attempt mismatch/extension resolution on it.

**Scope boundary discovered during implementation: no `StateRegistry`
propagation awareness.** `GraphBuilder` reads `ComponentSpec%var_specs`
directly per component (per the decision above), not `StateRegistry`'s
`propagate_exports`/`propagate_unsatisfied_imports` aggregated view
(`StateRegistry_Propagation_smod.F90`), which is how a grandchild's
export/import becomes visible on an ancestor under a `childname/itemname`
alias today. Every existing real `connect_all` scenario fixture in this
repo turned out to be of that propagated shape (the connected components'
own `var_specs` are empty; the real items live on a further-nested
grandchild) — none could serve directly as the task 7 equivalence check.
This was not anticipated when this design was written; it is now recorded
here as an explicit scope boundary (see proposal.md "Explicitly out of
scope") rather than silently worked around. **Not indefinitely deferred
polish**: `docs/graph/spec/20-implementation-roadmap.md` §20.4.1 now
commits to this as sub-change **3b2**, slotted between this change (3b)
and 3c - a check for unsatisfied imports with no cross-component
mechanism to satisfy them (task 6's `graphbuilder_check_unsatisfied_imports`)
is not useful standing alone. The mechanism itself (mirror
`propagate_unsatisfied_imports()`'s "re-advertise an unresolved import one
level up" behavior using 3b's own proxy-node/resource-index machinery, so
an ancestor's connections get a chance to resolve it) is designed in
3b2, not here - this change's own scope stays the flat, single-level
case it already implements.

## Risks / Trade-offs

- **[Risk] Divergence between `GraphBuilder`'s recomputed matching and
  `MatchConnection`'s actual executed behavior** on some edge case neither
  implementation's author anticipated (e.g. case sensitivity, whitespace in
  short names, an item advertised twice with different aspects) →
  **Mitigation:** the equivalence spec requirement is checked against at
  least one real (non-synthetic) configuration as part of this change's
  task list, not deferred to a future change.
- **[Risk] Running two resolution passes (legacy + graph) doubles
  advertising/accept-transfer-time cost for every component during this
  transitional phase** → **Mitigation:** accepted for this phase per
  Q10's explicit side-by-side recommendation; cost is bounded to
  component initialization, not the run loop, and the legacy path is
  expected to be retired once later phases build confidence, at which
  point this cost disappears.
- **[Risk] Phase-timing mismatch between GraphBuilder and the legacy
  Connection lifecycle** (found and fixed mid-implementation - see
  "Invocation point" above) → **Mitigation:** `GraphBuilder`'s connection
  work is now split to mirror `activate()`/`connect()`'s actual timing
  exactly, and `graphbuilder_resolve_connections()` is idempotent against
  the repeat-invocation behavior that surfaced this in the first place.
  Any future GraphBuilder work added to a lifecycle hook should re-check
  which of `activate()`/`connect()` (or neither) it actually corresponds
  to before choosing where to call it.
- **[Risk] `GraphBuilder` reading `StateRegistry` internals it wasn't
  originally designed to expose** could pull in incidental coupling not
  covered by REQ-GB-002's "MAY depend on" allowance → **Mitigation:**
  scope `GraphBuilder`'s `StateRegistry`/`ComponentSpec` reads to the
  minimum needed (advertised item identities/short names, intent, declared
  connections); do not reach into aspect-specific details (units, vertical
  grid, etc.) — those belong to 3c's mismatch-detection work, not 3b's
  exact-match resolution.

## Open Questions

- Exact on-disk location for the new `GraphBuilder` module
  (`superstructure/generic/graph/` alongside `ComponentGraph`, vs. a new
  `superstructure/generic/graph_builder/` directory) — does not affect
  specs, approach, or task breakdown; left for the task-breakdown/
  implementation step to decide following whichever existing
  one-procedure-per-file convention the surrounding code uses.
