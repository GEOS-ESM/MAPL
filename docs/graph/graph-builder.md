# GraphBuilder: the integration layer

Source: `superstructure/generic/GraphBuilder.F90` (1465 lines — public
API, shared types/helpers, submodule interface declarations) plus ten
`GraphBuilder_*_smod.F90` submodules (procedure bodies, ~3000 lines
combined). This is the one place in the graph layer that **is**
allowed to depend on `OuterMetaComponent`/`StateRegistry`/
`GriddedComponentDriver` — everything under `graph/` proper must not
(REQ-GB-001/002; see `hierarchy-and-component-graph.md`'s REQ-CG-002
note).

## Why it's a stateless type, not an object

```fortran
type :: GraphBuilder
contains
   procedure, nopass :: advertise => graphbuilder_advertise
   ...
end type
```

(`GraphBuilder.F90:252`.) Every binding is `NOPASS` — `GraphBuilder`
carries **no state of its own**. Every procedure takes the
`OuterMetaComponent` it operates on as an explicit argument and
reads/writes only that component's own `ComponentGraph` (plus, for
cross-boundary resolution, a named child's graph/spec reached via two
framework-internal `OuterMetaComponent` accessors). This is a thin
calling-convention wrapper (`gb = GraphBuilder(); call
gb%run_advertise_hook(this)`) over what were previously free functions
— nothing more.

## Submodule map

| Submodule | Responsibility |
|---|---|
| `GraphBuilder_Advertising_smod.F90` | Creating `StateItemNode`s for advertised import/export/internal items (`advertise_one`) |
| `GraphBuilder_ConnectionResolution_smod.F90` | Ordinary (exact short-name match) connection resolution — `MatchConnection` only, not `SimpleConnection`/`ReexportConnection` |
| `GraphBuilder_CallbackWiring_smod.F90` | Wildcard/named callback resolution, `CallbackStateBinding`/`CallbackMethodBinding` construction |
| `GraphBuilder_Geometry_smod.F90` | Horizontal geometry advertise/resolve, parent↔child geometry push/pull |
| `GraphBuilder_StatePopulation_smod.F90` | Graph-native equivalent of `StateRegistry%add_to_states` — inserts materialized fields into the real `ESMF_State` |
| `GraphBuilder_RunPhaseNode_smod.F90` | Constructs the ordinary-phase `MethodGraphNode` (construction only — no invocation hook; see its own header before adding one) |
| `GraphBuilder_RunPhasePull_smod.F90` | Post-run re-propagation pull (plain `ComponentGraph%update()`, never a second driver call) |
| `GraphBuilder_PortsAndProxies_smod.F90` | Public port / child-proxy population |
| `GraphBuilder_LifecycleHooks_smod.F90` | Thin per-init-phase hook wrappers called from `OuterMetaComponent` |
| `GraphBuilder_Visualization_smod.F90` | `NodeIdLabelMap` enrichment for `GraphExport` |
| `GraphBuilder_Freeze_smod.F90` | Validate-and-freeze |

All ten were mechanically split out of one originally-monolithic
`GraphBuilder.F90` (`graphbuilder-submodule-split`) — procedure bodies
moved verbatim, `subroutine`→`module subroutine`/`function`→`module
function` only. The interface declarations for every moved procedure
still live in `GraphBuilder.F90` itself (`:375` onward).

## Two-phase timing: advertise vs. real wiring

Mirrors the legacy imperative coupler's `Connection%activate()`/
`connect()` split (`GraphBuilder.F90:40-58`):

- **`GENERIC_INIT_ADVERTISE`**: `graphbuilder_check_unsatisfied_imports()`
  — read-only, determines which ordinary-match imports *would* be
  satisfied, with **no graph structure created** (no proxy nodes, no
  dependency edges). Called from `initialize_advertise.F90`.
- **`GENERIC_INIT_ACCEPT_TRANSFER`**: `graphbuilder_resolve_connections()`
  — the real graph mutation (proxies + dependency edges). Called from
  `initialize_accept_transfer.F90`, alongside the legacy `connect()`
  call, followed by `graphbuilder_freeze()` there (freezing before real
  wiring exists would be premature).

Real call sites, all under `superstructure/generic/OuterMetaComponent/`:
`initialize_advertise.F90`, `initialize_accept_transfer.F90`,
`initialize_geom_b.F90`, `run_user.F90` — each constructs a local
`type(GraphBuilder) :: gb` and calls the matching hook.

## Cross-component (parent/child) connections

A dependency edge can only be added to a network owned by the graph
that owns **both** its endpoint `NodeId`s (`ComponentGraph%add_dependency`
enforces this — see `hierarchy-and-component-graph.md`). A
`MatchConnection` naming a child therefore cannot be wired directly
into the child's own network — nor would that be legal (REQ-HIER-005:
a parent may not reach into a child's internal `DependencyNetwork`).
Instead, for any endpoint that isn't `<self>`
(`SELF_COMPONENT_NAME`, matches `StateRegistry_Hierarchy_smod`'s own
sentinel), `GraphBuilder` creates (or reuses a cached) **parent-local
proxy `StateItemNode`** standing in for the child's item (REQ-HIER-006),
registers it in the *parent's* own graph, and records it via
`add_child_port_binding`. The actual dependency edge always lands on
the calling component's own default network, between two of its own
(real-or-proxy) `NodeId`s.

## GraphMode: the legacy/graph-native switch

`GraphMode.F90:1`. Global 3-way selector, read via `graph_mode()`:

| Mode | Behavior |
|---|---|
| `GRAPH_MODE_REGISTRY` (default) | Zero behavior change — only the legacy `StateRegistry`-based imperative coupler populates real `ESMF_State`s. Graph-native machinery runs nowhere. |
| `GRAPH_MODE_BOTH` | Graph-native materialization runs *alongside* legacy, but never inserts into the shared `ESMF_State` the legacy coupler populates — avoids a duplicate-insertion conflict. A graph-materialized item's value is readable only out-of-band, directly from its node payload. Used to compare graph-native vs. legacy behavior with no collision risk. |
| `GRAPH_MODE_GRAPH` | True-replacement mode — graph-native state population is the real producer. |

Most of `GraphBuilder`'s hooks (`populate_state`, `run_geometry_hook`,
`run_pull_hook`, ...) are explicit no-ops unless `graph_mode()` reports
something other than `GRAPH_MODE_REGISTRY` — i.e. a no-op for every real
production run today unless a test explicitly selects a non-registry
mode. **Check `graph_mode()` gating before assuming a hook does
anything in production.**

## Qualified-export resolution (shared primitive)

`resolve_qualified_component`/`enumerate_qualified_exports`
(`graph-builder-resolution-index`) generalize an earlier
callback-wiring-only recursion (`resolve_descendant`) into a shared
multi-level qualified-path resolution primitive, used by both ordinary
connection resolution and callback wiring. `QualifiedExportIndexEntry`
deliberately carries no `VariableSpec` — the primitive's whole point is
not to reintroduce a `var_spec` dependency into resolution's own
vocabulary. Contrast with `QualifiedExportEntry` (callback-wiring's own
earlier type), which *does* carry a `VariableSpec` because interface
conformance checking needs it.

## A real gfortran gotcha

Several procedures that look purely internal
(`component_for`/`is_self`/`item_key`/`resolve_qualified_component`/...)
are `public` for a non-obvious reason: a gfortran-specific linkage
limitation emits ordinary (non-`module`) procedures defined in a
private-by-default module's `CONTAINS` with internal/static linkage
when nothing marks them public — a submodule's separately-compiled
`.o` then can't link against them, even though F2008+ host-association
rules say it should work. NAG doesn't have this limitation. If you add
a new submodule-shared helper, mark it `public` in `GraphBuilder.F90`
even if it has no legitimate external caller — see the comment at
`GraphBuilder.F90:184` before "cleaning up" an apparently-unused public
export.
