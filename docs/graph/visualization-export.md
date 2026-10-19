# Visualization export

Source: `superstructure/generic/graph/GraphExport.F90` (496 lines).
Enrichment layer: `superstructure/generic/GraphBuilder_Visualization_smod.F90`.

## Scope and layering

`GraphExport.F90` is graph-neutral — depends only on `ComponentGraph`'s
and `DependencyNetwork`'s public query API plus `NodeId%to_string()`.
Nodes are labeled only by `NodeId%to_string()` at this layer; it never
executes a `TransformGraphNode` or advances a `NodeRevision` (read-only:
`get_node()`/`get_network()`/`get_predecessors()`/`get_successors()`/
`get_port_bindings()`/`get_revision()` only).

Human-readable label enrichment is a **separate, higher** layer
(`GraphBuilder_Visualization_smod.F90`, REQ-VIZ-004) — it builds a
`NodeIdLabelMap` and passes it in; `GraphExport` itself resolves no
names.

## Public API

```fortran
integer, parameter :: GRAPH_EXPORT_SCHEMA_VERSION = 1

function export_graph_dot(graph, include_revisions, label_map, rc) result(dot_text)
function export_graph_json(graph, include_revisions, label_map, rc) result(json_text)
```

Both take an **optional** `NodeIdLabelMap` (`NodeId -> NodeLabel`): when
an entry exists, its text replaces `NodeId%to_string()` in the label,
and `is_proxy` adds `shape="doublecircle"` (DOT) / `"proxy": true`
(JSON). Omitting `label_map` entirely preserves byte-for-byte prior
output — purely additive.

## Output schema (versioned, not spec-frozen)

```
DOT:  digraph ComponentGraph {
        "<NodeId>" [label="...", kind="...", payload="..."?,
                    revision="..."?, shape="doublecircle"?];
        "<src>" -> "<dst>" [network="...", label="<port>"?];
      }
JSON: { "schema_version": <int>,
        "nodes": [ {"id", "kind", "payload_kind"?, "revision"?,
                     "label"?, "proxy"?} ],
        "edges": [ {"source", "target", "network", "port"?} ] }
```

Deliberately **not fixed in the spec text** (`open-questions.md` Q15)
— versioned via `GRAPH_EXPORT_SCHEMA_VERSION`, documented here/at the
code instead, since schema needs were contingent on now-settled
questions (payload representation, port-binding storage) at the time
this was written.

Port-binding edge labels (`network`/`port` fields): resolved once port
bindings became an external table (`transforms-and-update.md`) —
`get_binding()` returning null for a missing entry is exactly what lets
this degrade gracefully (omit the label, keep the edge) rather than
fail.

## Not implemented here

Hierarchy-wide (multi-`ComponentGraph`) export/composition at proxy-node
boundaries, and repeated/time-series export calls across runtime — both
open, see `open-questions.md` (Q16, Q18). Current scope is per-component,
callable at least once after freeze.
