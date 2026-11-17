# GraphBuilder: the integration layer

Source: `superstructure/generic/GraphBuilder.F90` (about 1050 lines — public
API, shared types/helpers, submodule interface declarations) plus fifteen
`GraphBuilder_*_smod.F90` submodules (procedure bodies, about 5,100 lines
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

(`GraphBuilder.F90:145`.) Every binding is `NOPASS` — `GraphBuilder`
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
| `GraphBuilder_ConnectionResolution_smod.F90` | Connection-resolution helpers shared by the next five submodules |
| `GraphBuilder_ConnectionMatching_smod.F90` | Ordinary connection resolution — `SimpleConnection` and `MatchConnection` matching/dispatch |
| `GraphBuilder_ConnectionDestinations_smod.F90` | Inout-borrower, callback and Wildcard destination resolution for an already-matched connection |
| `GraphBuilder_ImportExportPropagation_smod.F90` | Hierarchy import/export propagation (`ReexportConnection` goes through `run_reexport_hook`, `reexport_one`) |
| `GraphBuilder_PayloadMaterialization_smod.F90` | Export payload creation/aliasing and extension-chain materialization |
| `GraphBuilder_ExpressionResolution_smod.F90` | EXPRESSION-item advertise check and per-consumer evaluation |
| `GraphBuilder_CallbackWiring_smod.F90` | Wildcard/named callback resolution, `CallbackStateBinding`/`CallbackMethodBinding` construction |
| `GraphBuilder_Geometry_smod.F90` | Horizontal geometry advertise/resolve, parent↔child geometry push/pull |
| `GraphBuilder_StatePopulation_smod.F90` | Graph-native equivalent of `StateRegistry%add_to_states` — inserts materialized fields into the real `ESMF_State` |
| `GraphBuilder_RunPhaseNode_smod.F90` | Dormant: constructs the ordinary-phase `MethodGraphNode`, but no lifecycle hook builds it and nothing invokes it (see its own header before reviving it) |
| `GraphBuilder_RunPhasePull_smod.F90` | Post-run re-propagation pull (plain `ComponentGraph%update()`, never a second driver call) |
| `GraphBuilder_LifecycleHooks_smod.F90` | Thin per-init-phase hook wrappers called from `OuterMetaComponent` |
| `GraphBuilder_Visualization_smod.F90` | `NodeIdLabelMap` enrichment for `GraphExport` |
| `GraphBuilder_Freeze_smod.F90` | Validate-and-freeze |

All ten were originally mechanically split out of one monolithic
`GraphBuilder.F90` (`graphbuilder-submodule-split`) — procedure bodies
moved verbatim, `subroutine`→`module subroutine`/`function`→`module
function` only. `GraphBuilder_ConnectionResolution_smod.F90` was later
split by concern into the five further submodules listed above (plan
task D1, `docs/graph/plans/separation-of-concerns.md`), so the tree now
has fifteen. The interface declarations for every moved procedure
still live in `GraphBuilder.F90` itself (the interface block from `:280`).

## Two-phase timing: advertise vs. real wiring

Mirrors the legacy imperative coupler's `Connection%activate()`/
`connect()` split (`GraphBuilder.F90:31-38`):

- **`GENERIC_INIT_ADVERTISE`**: `graphbuilder_check_unsatisfied_imports()`
  — read-only, determines which ordinary-match imports *would* be
  satisfied, with **no graph structure created** (no dependency
  edges). Called from `initialize_advertise.F90`.
- **`GENERIC_INIT_ACCEPT_TRANSFER`**: `graphbuilder_resolve_connections()`
  — the real graph mutation (dependency edges and extension chains). Called from
  `initialize_accept_transfer.F90`, alongside the legacy `connect()`
  call, followed by `graphbuilder_freeze()` there (freezing before real
  wiring exists would be premature).

Real call sites, all under `superstructure/generic/OuterMetaComponent/`:
`initialize_advertise.F90`, `initialize_accept_transfer.F90`,
`initialize_geom_b.F90`, `run_user.F90` — each constructs a local
`type(GraphBuilder) :: gb` and calls the matching hook.

## Cross-component (parent/child) connections

Node storage is shared across the hierarchy, so a connection that names a
child needs no stand-in. For any endpoint that isn't `<self>`
(`SELF_COMPONENT_NAME`, matches `StateRegistry_Hierarchy_smod`'s own
sentinel), `GraphBuilder` finds the descendant component
(`resolve_qualified_component`, a downward walk only) and uses that
component's own real `NodeId`. Ordinary connection resolution takes it from
the descendant's export-port table (`find_export_node_id` over
`find_export_node`, `GraphBuilder_ConnectionResolution_smod.F90:323-345`):
a resource-index lookup by short name would fail for a propagated export,
whose port name is qualified. Callback wiring takes it from the owner's
resource index (`resolve_qualified_export_node_id`,
`GraphBuilder_CallbackWiring_smod.F90:125-160`). No proxy node is created
(`spec/02-component-hierarchy.md` §2.3), and
`ComponentGraph%add_dependency` accepts an id issued by another component
(`allowed_foreign_node`, `ComponentGraph.F90:439-447`; see
`hierarchy-and-component-graph.md`). The dependency edge lands in the
hierarchy's single forward network, shared by every component
(`HierarchyGraph.F90:14-20`).

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

`resolve_qualified_component` generalizes an earlier
callback-wiring-only recursion (`resolve_descendant`) into a shared
multi-level qualified-path resolution primitive, used by both ordinary
connection resolution and callback wiring. `QualifiedExportEntry`
(callback-wiring's own type) carries a `VariableSpec` because interface
conformance checking needs it.

## A real gfortran gotcha

Several procedures that look purely internal
(`is_self`/`item_key`/`resolve_qualified_component`/...)
are `public` for a non-obvious reason: a gfortran-specific linkage
limitation emits ordinary (non-`module`) procedures defined in a
private-by-default module's `CONTAINS` with internal/static linkage
when nothing marks them public — a submodule's separately-compiled
`.o` then can't link against them, even though F2008+ host-association
rules say it should work. NAG doesn't have this limitation. If you add
a new submodule-shared helper, mark it `public` in `GraphBuilder.F90`
even if it has no legitimate external caller — see the comment at
`GraphBuilder.F90:100` before "cleaning up" an apparently-unused public
export.
