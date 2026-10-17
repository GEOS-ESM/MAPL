# 8. GraphBuilder

Status: `[SETTLED]` responsibilities list; individual sub-mechanisms detailed
in their own documents. `[RE-EVALUATED]` §8.2's responsibility table —
amended below per the landed `hierarchy-wide-component-graph` change
(proxy-node mechanisms retired; recursive import/export propagation and
hierarchy-wide freeze added; export-visibility-as-a-query added for
History support).

## 8.1 Role

`GraphBuilder` is the integration layer between the graph-neutral
`ComponentGraph`/`DependencyNetwork`/`GraphNode`/`GraphValue` types and the
rest of MAPL (component hierarchy, StateRegistry, connection-point config).

**REQ-GB-001.** `ComponentGraph` MUST NOT depend on `OuterComponent`,
`StateRegistry`, or component-hierarchy implementation (restated from
REQ-CG-002 — this is the boundary `GraphBuilder` exists to cross).

**Withdrawn:** an earlier draft of this requirement also excluded
"connection-point parsing" from `ComponentGraph`. That restriction is
withdrawn — no confirmed reason `ComponentGraph` (or a dependency-free
parsing utility usable by both layers) cannot handle syntactic parsing of
connection-point strings; the actual layering concern is *semantic
resolution* of a connection point (turning a name into an actual
component/port reference), which does genuinely need StateRegistry/
component-hierarchy context and stays a `GraphBuilder` responsibility. See
`07-component-graph.md` REQ-CG-002.

**REQ-GB-002.** `GraphBuilder` MAY depend on all of the above.

## 8.2 Responsibilities

**REQ-GB-003.** `GraphBuilder` MUST provide the following capabilities
(each detailed in the cross-referenced document):

| Responsibility | Detail |
|---|---|
| Advertising state items | creates `StateItemNode`s for advertised items |
| Creating `StateItemNode`s | — |
| Creating `TransformGraphNode`s | `10-transforms-and-ports.md` |
| Resolving connections | ordinary connection-point wiring; cross-component edges wire real `NodeId`s directly, no proxy (`02-component-hierarchy.md` §2.3, `[RE-EVALUATED]`) |
| Searching source-rooted graph regions for reusable extensions | `09-extension-reuse.md` |
| Creating framework-managed extension items | `09-extension-reuse.md` |
| Adding extensions to OuterComponent states | `09-extension-reuse.md` REQ-EXT-004 |
| Registering extensions in StateRegistry | `09-extension-reuse.md` |
| Creating dependency networks | one or more per `ComponentGraph`, incl. per-callback-method networks (`15-callbacks.md`) |
| Creating public ports | `02-component-hierarchy.md` §2.1 (REQ-HIER-002) — no child proxies are created; a parent reaches a child's real node directly (§2.3, `[RE-EVALUATED]`) |
| Mirroring unresolved imports up one hierarchy level, recursively `[NEW]` | for each direct child, mirrors any still-unresolved advertised import into the parent's own import-port table under the same real `NodeId` — unconditional, no connection needs to name the child; composed into multi-level propagation by the hierarchy's own existing recursion, not a search (`graphbuilder_run_propagate_hook`, `GraphBuilder_ConnectionResolution_smod.F90`; `openspec/specs/graph/graph-builder/spec.md` "An unresolved import is mirrored into every ancestor's own import-port table") |
| Mirroring exports up one hierarchy level, recursively, under a child-qualified name `[NEW]` | symmetric to import mirroring: an exported item bubbles into the parent's own export-port table under `child_name/short_name`, left unchanged at every further ancestor level (`graphbuilder_run_export_propagate_hook`, same submodule) — required for ordinary resolution to find a propagated export's own ESMF-sub-state-qualified name (e.g. History's `A/E_A1` shape) |
| Resolving wildcard/virtual connection points | `15-callbacks.md` §15.8 |
| Resolving callback connections | `15-callbacks.md` |
| Querying one component's own local export index directly, with no propagation step `[NEW]` | `query_component_exports(root, path, rc)` — composes the existing qualified-component-path resolution with a single-level resource-index read; added for History-style deep-hierarchy export visibility (Task Group 7) |
| Validating and freezing the graph | now a single, hierarchy-wide operation invoked once at the true hierarchy root (`GENERIC_INIT_GRAPH_FREEZE` phase, `graphbuilder_run_graph_freeze_hook`, `GraphBuilder_Freeze_smod.F90`), not a per-component call — see `07-component-graph.md` §7.3 `[RE-EVALUATED]` |
| Compiling graph relationships into direct ESMF/MAPL runtime relationships | eventual; `11-revision-and-update.md` §11.5 |

**Retired (no longer a `GraphBuilder` responsibility):** creating and
caching parent-local boundary/proxy nodes standing in for a child's
published port. This mechanism (`proxy_key`,
`parent_geometry_proxy_key`, `resolve_qualified_export_node_id`'s
proxy-creation half) is fully removed — see `02-component-hierarchy.md`
§2.3 `[RE-EVALUATED]` and
`openspec/specs/graph/component-hierarchy/spec.md`'s "REMOVED
Requirements" section ("Proxy-node storage for cross-graph
boundaries").

## 8.3 Layering summary

```
OuterComponent, StateRegistry, connection-point config
            │  (GraphBuilder may depend on these)
            ▼
       GraphBuilder
            │  (constructs/wires)
            ▼
       ComponentGraph, DependencyNetwork, GraphNode*, GraphValue*
       (must not depend upward)
```

This one-directional dependency is load-bearing for testability: the graph
core can be exercised with synthetic nodes/values in unit tests with no
ESMF component hierarchy present at all.
