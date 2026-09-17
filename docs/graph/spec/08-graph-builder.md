# 8. GraphBuilder

Status: `[SETTLED]` responsibilities list; individual sub-mechanisms detailed
in their own documents.

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
| Resolving connections | ordinary connection-point wiring |
| Searching source-rooted graph regions for reusable extensions | `09-extension-reuse.md` |
| Creating framework-managed extension items | `09-extension-reuse.md` |
| Adding extensions to OuterComponent states | `09-extension-reuse.md` REQ-EXT-004 |
| Registering extensions in StateRegistry | `09-extension-reuse.md` |
| Creating dependency networks | one or more per `ComponentGraph`, incl. per-callback-method networks (`15-callbacks.md`) |
| Creating public ports and child proxies | `02-component-hierarchy.md` §2.3 |
| Resolving wildcard/virtual connection points | `15-callbacks.md` §15.8 |
| Resolving callback connections | `15-callbacks.md` |
| Validating and freezing the graph | calls `DependencyNetwork.validate()`, `ComponentGraph.freeze()` |
| Compiling graph relationships into direct ESMF/MAPL runtime relationships | eventual; `11-revision-and-update.md` §11.5 |

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
