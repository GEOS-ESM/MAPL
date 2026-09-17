## Why

Phase 3a (`component-hierarchy-foundation`, archived) gave every
`OuterMetaComponent` a local `ComponentGraph` with reachable port/proxy
storage, but nothing populates it yet: `StateItemNode`s are never created
from advertised variables, and ordinary import/export connections are
still resolved entirely by the legacy imperative path
(`ConnectionPt`/`Connection`/`MatchConnection`, `StateRegistry_Actions_smod`,
`OuterMetaComponent%connect_all`). Per
`docs/graph/spec/20-implementation-roadmap.md` §20.4.1, this is sub-change
3b: the first `GraphBuilder` slice, and per `17-open-questions.md` Q10 it
is "the first point where existing MAPL coupler behavior must be
reproduced exactly" — so it must ship alongside a way to prove equivalence
against the current coupler on real configurations before that legacy path
is ever removed.

## What Changes

- Introduce `GraphBuilder` as the integration layer described in
  `08-graph-builder.md` §8.1–8.3 (REQ-GB-001/002): it MAY depend on
  `StateRegistry`/`OuterMetaComponent`/`ComponentSpec`, and constructs
  `ComponentGraph` content but never the reverse.
- **Advertising**: for each advertised import/export/internal variable
  known to `StateRegistry`/`ComponentSpec`, `GraphBuilder` creates a
  corresponding `StateItemNode` in the owning `OuterMetaComponent`'s local
  `ComponentGraph` (REQ-GB-003 "Advertising state items" / "Creating
  StateItemNodes").
- **Ordinary connection resolution, split into two phases matching the
  legacy `Connection` lifecycle** (corrected mid-implementation — see
  design.md "Invocation point"): legacy's own `Connection%activate()`
  (called at `GENERIC_INIT_ADVERTISE`) only marks which imports/exports
  are active, feeding the existing unsatisfied-import-propagation and
  export-allocation-avoidance decisions; the real wiring only happens at
  `Connection%connect()` (`GENERIC_INIT_ACCEPT_TRANSFER`). `GraphBuilder`
  mirrors this with `graphbuilder_check_unsatisfied_imports()`
  (advertise-time, read-only, no graph mutation) and
  `graphbuilder_resolve_connections()` (accept-transfer-time, the real
  `DependencyNetwork` edges between the corresponding `StateItemNode`s,
  and the default per-`ComponentGraph` dependency network to hold them —
  REQ-GB-003 "Resolving connections", "Creating dependency networks").
  Restricted to `connect_all`/`MatchConnection`'s "magic connect" behavior
  for the exact-match case (non-wildcard, non-callback).
- **Public ports and child proxies**: populates the `ComponentGraph`
  import/export port tables and `child_bindings` proxy-node storage that
  3a made reachable but left empty, per `02-component-hierarchy.md` §2.3
  (REQ-HIER-006) — this is the "population logic" 3a's proposal explicitly
  deferred to 3b.
- **Validate/freeze**: calls `DependencyNetwork.validate()` and
  `ComponentGraph.freeze()` once resolution for a component completes, at
  `GENERIC_INIT_ACCEPT_TRANSFER` time alongside real resolution — not at
  advertise time, since freezing before real wiring exists would be
  premature (REQ-GB-003 last row, scoped to what this slice produces).
  `graphbuilder_resolve_connections()` tolerates being invoked more than
  once per component (a no-op once the graph is already frozen), since
  ESMF's Provide/Accept/Realize transfer negotiation can invoke
  `GENERIC_INIT_ACCEPT_TRANSFER` more than once per component within one
  `Initialize` sequence — discovered via a real end-to-end test failure,
  matching the same repeat-invocation hazard legacy's own
  `Connection%connect()` implementations already guard against with a
  per-connection `consumed` flag.
- Adds an equivalence-verification path (harness or test suite, per Q10)
  that runs `GraphBuilder`'s resolution side-by-side with the existing
  imperative coupler (`ConnectionPt`/`MatchConnection`/`connect_all`,
  `StateRegistry_Actions_smod`) on real configurations and reports any
  divergence, without removing or modifying the legacy path.
- **Explicitly out of scope** (left to later sub-changes/phases per
  `20-implementation-roadmap.md` §20.4.1 and `08-graph-builder.md` §8.2):
  mismatch/extension-chain creation (3c, `09-extension-reuse.md`),
  visualization enrichment (3d), wildcard/virtual connection-point
  resolution and callback connection resolution (Phase 4,
  `15-callbacks.md`), and compiling graph relationships into direct
  ESMF/MAPL runtime relationships (Phase 5/Q9). REQ-EXT-003's exact-match/
  no-op case is the only case handled here — any mismatch is left
  unresolved by this change, not silently coerced.
- **Also out of scope, discovered during implementation**: `StateRegistry`'s
  cross-level propagation (`propagate_exports`/`propagate_unsatisfied_imports`,
  `StateRegistry_Propagation_smod.F90`), which aliases a grandchild's
  advertised item up into an ancestor's own registry view under a
  `childname/itemname` naming convention. `GraphBuilder` reads each
  component's own `ComponentSpec%var_specs` directly (design.md
  Decisions) and does not consult this propagated/aggregated view, so a
  connection between two components whose *own* items are what's being
  matched (the flat case) is handled, but a connection resolved today only
  through propagation from a deeper descendant is not — `GraphBuilder`
  reports it unresolved even though the legacy coupler correctly resolves
  it. Every existing `connect_all`-based scenario fixture under
  `superstructure/generic/tests/scenarios/` (`statistics`,
  `statistics_real`, `history_1`, `history_wildcard`, `extdata_1`) is of
  this propagated shape; none of them can serve as-is for this change's
  equivalence check (task 7). A new flat fixture
  (`scenarios/graphbuilder_equivalence/`) was added instead. **This is
  not optional polish deferred indefinitely**: a check for unsatisfied
  imports with no mechanism to satisfy them across component boundaries
  is not useful on its own (`graphbuilder_check_unsatisfied_imports`'
  own activate-time result is currently logged and discarded, connecting
  to nothing). `docs/graph/spec/20-implementation-roadmap.md` §20.4.1 now
  names this a required, committed sub-change — **3b2, cross-component
  unresolved-import propagation** — slotted between this change (3b) and
  3c, before 3c's mismatch-driven work needs to run on whatever is still
  unresolved after propagation.
- No change to `ComponentGraph`'s graph-neutrality (REQ-CG-002) or to
  `OuterMetaComponent`'s public API surface beyond what is needed to invoke
  `GraphBuilder` — `StateItemNode`/proxy internals stay hidden from
  user-facing APIs per REQ-HIER-006.

## Capabilities

### New Capabilities
- `graph/graph-builder`: the `GraphBuilder` integration layer —
  advertising, `StateItemNode` creation, ordinary (exact-match) connection
  resolution, default dependency-network creation, public-port/child-proxy
  population, and validate/freeze invocation, scoped to REQ-GB-001..003's
  advertising/ordinary-connection subset only.

### Modified Capabilities
(none — `component-graph`, `component-hierarchy`, `dependency-network`,
`state-item`, `graph-node-hierarchy`, `identities` are consumed as-is,
unmodified, by `GraphBuilder`)

## Impact

- **Affected code**: new `GraphBuilder` module,
  `superstructure/generic/GraphBuilder.F90` (top-level, alongside
  `OuterMetaComponent.F90` — see design.md's resolved Open Question),
  invoked from `initialize_advertise.F90` (advertising +
  activate()-time unresolved-imports check) and
  `initialize_accept_transfer.F90` (connect()-time real resolution +
  freeze) without replacing those files' existing behavior.
- **Existing code read, not modified**: `StateRegistry`/`ComponentSpec`
  (`superstructure/generic/registry/`, `superstructure/generic/specs/`) for
  advertised-variable data; `superstructure/generic/connection/` types
  (`Connection`, `ConnectionPt`, `VirtualConnectionPt`, `MatchConnection`)
  as the semantics `GraphBuilder`'s ordinary-connection resolution must
  match exactly for the equivalence check.
- **New Fortran types**: `GraphBuilder` itself; no changes to `StateItemNode`/
  `ComponentGraph`/`DependencyNetwork` types from Phase 1–2.
- **Tests**: new pFUnit coverage for advertising → `StateItemNode` creation,
  ordinary connection resolution → `DependencyNetwork` edges, and
  public-port/proxy population, plus the side-by-side equivalence check
  against the legacy coupler on at least one real (non-synthetic)
  configuration, per Q10's recommendation.
- **Dependencies**: none new; reuses `ComponentGraph`/`DependencyNetwork`
  from Phase 1–2 and `StateRegistry`/`ComponentSpec`/`OuterMetaComponent`
  from the existing component hierarchy and 3a.
- **Out of scope**: extension/mismatch resolution (3c), visualization
  enrichment (3d), wildcard and callback connection resolution (Phase 4),
  compiled execution (Phase 5/Q9), and removal of the legacy imperative
  coupler path (kept as the equivalence oracle until a later phase decides
  otherwise).
