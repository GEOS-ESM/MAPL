## Why

Phase 2's graph-neutral exporter (`graph/visualization-export`,
`superstructure/generic/graph/GraphExport.F90`) can dump a
`ComponentGraph`'s topology to DOT/JSON, but every node is labeled only
by its opaque `NodeId%to_string()` (REQ-VIZ-003) — by design, since that
layer must not depend on `OuterComponent`/`StateRegistry`
(REQ-CG-002/REQ-GB-001). That makes the export technically correct but
practically unreadable: a real component's dump shows synthetic ids
instead of the import/export/internal names and port names a developer
actually recognizes. Per `docs/graph/spec/20-implementation-roadmap.md`
§20.4.1, this is sub-change 3d — the `GraphBuilder`-level enrichment
layer REQ-VIZ-004 calls for, needed only for 3a's `OuterComponent` shape
plus name lookup, independent of 3b/3c's harder wiring/extension work.

## What Changes

- Add a `NodeId -> label` lookup that `GraphExport`'s DOT/JSON producers
  can consult when rendering a node, falling back to
  `NodeId%to_string()` when no label is available (REQ-VIZ-004,
  REQ-VIZ-014) — implemented as a plain, pre-populated map passed in by
  the caller, not a dependency `GraphExport.F90` itself resolves; the
  core exporter still does not `use` `OuterComponent`/`StateRegistry`
  (REQ-VIZ-003 unchanged).
- Add a `GraphBuilder`-level enrichment procedure that builds that label
  map for one `OuterMetaComponent`'s own `ComponentGraph`: for each of
  the component's own advertised items (`ComponentSpec%var_specs`, the
  same iteration `graphbuilder_advertise` already performs) it looks up
  the item's `NodeId` via the existing `get_resource_index(item_key(...))`
  accessor and records `short_name` as its label; for each child's
  proxy node cached in this component's own graph (`proxy_key`) it
  records `"<child_name>:<short_name>"` and marks the node as a proxy
  (REQ-VIZ-015). No new dependency on `StateRegistry`: names come from
  `VariableSpec`/`ComponentSpec`, the same graph-native source
  `GraphBuilder` already reads for advertising, consistent with the
  precedent set by `extension-registry-visibility` (3c2) of not taking a
  runtime dependency on machinery slated for retirement.
- Mark boundary/proxy nodes distinguishably in both DOT and JSON output
  (REQ-VIZ-015): a distinct DOT node shape/style and a `"proxy": true`
  JSON field, so a rendered graph does not present a child's proxy as an
  ordinary local node.
- Scope is a single component's own local graph only. Hierarchy-wide
  (multi-`ComponentGraph`) composition (REQ-VIZ-005, `19-visualization-
  export.md` §19.9 Q18) stays `[OPEN]` and out of scope — this change
  does not attempt DOT `subgraph cluster_*` nesting across components
  (REQ-VIZ-013), only single-graph labeling.
- Add a standalone Python script (`tools/graph_visualization/
  render_graph.py`) that turns an enriched `.dot` file into a rendered
  image, per REQ-VIZ-002's "delegated to established external tools"
  posture: it shells out to an installed Graphviz `dot` binary (no new
  Python package dependency) and is the first concrete instance of the
  "script that consumes a JSON/text dump" `19-visualization-export.md`
  §19.1 already anticipates. This is non-Fortran tooling with no CMake/
  build-system footprint; it consumes the `.dot` files this change's
  Fortran side produces but has no calling relationship the other
  direction.

## Capabilities

### New Capabilities
- `graph/visualization-enrichment`: the `GraphBuilder`-level procedure
  that builds a `NodeId -> label` map for one component's own graph from
  its advertised items and cached child-proxy nodes, and calls the
  Phase 2 exporter with it.
- `graph/visualization-render-script`: the standalone Python CLI that
  renders a `.dot` file (produced by `graph/visualization-export`) to an
  image via the installed Graphviz `dot` binary.

### Modified Capabilities
- `graph/visualization-export`: the exporter accepts an optional,
  pre-built label lookup and, when a label is supplied for a node, uses
  it in place of `NodeId%to_string()` in both DOT and JSON output
  (REQ-VIZ-004); a node identified as a boundary/proxy node is rendered
  distinguishably from an ordinary node in both formats (REQ-VIZ-015).
  Absent a label lookup, or for a node the lookup has no entry for,
  behavior is unchanged from the existing spec (REQ-VIZ-003/014).

## Impact

- **Affected code**: `superstructure/generic/graph/GraphExport.F90`
  (new optional label-lookup parameter on `export_graph_dot`/
  `export_graph_json`, and proxy-flag rendering); a new, small
  `NodeId`-keyed label map container in
  `superstructure/generic/graph/containers/` (gFTL map, mirroring the
  existing `PortNameRevisionMap`/`PortIdNodeIdMap` pattern in that
  directory); `superstructure/generic/GraphBuilder.F90` (new enrichment
  procedure, built from the same `ComponentSpec%var_specs` iteration and
  `item_key`/`proxy_key` helpers `graphbuilder_advertise` and the
  proxy-node machinery already use — no change to their existing
  behavior). No changes to `ComponentGraph`/`DependencyNetwork` public
  APIs, to `StateRegistry`, or to any Phase 1–2 core type.
- **New Fortran surface**: one new label-map container type; one new
  `GraphBuilder` enrichment procedure (`build_label_map` or similar,
  taking an `OuterMetaComponent` and returning the populated map); two
  new optional parameters on the existing `export_graph_dot`/
  `export_graph_json` functions. No existing public signature loses
  compatibility (new parameters are optional).
- **Tests**: pFUnit coverage asserting (a) a component's own advertised
  import/export/internal items appear in DOT/JSON output under their
  real short names rather than raw `NodeId` strings; (b) a node with no
  label-map entry still falls back to `NodeId%to_string()`, unchanged
  from today; (c) a cached child-proxy node is labeled
  `"<child_name>:<short_name>"` and marked distinguishably as a proxy in
  both formats; (d) `GraphExport.F90`'s existing unlabeled-export test
  coverage is unaffected when the new parameter is omitted.
- **Out of scope**: hierarchy-wide/multi-component export (REQ-VIZ-005,
  Q18), DOT cluster nesting (REQ-VIZ-013), any dependency on
  `StateRegistry`, any change to `ComponentGraph`/`DependencyNetwork`
  public query APIs.
- **New non-Fortran surface**: `tools/graph_visualization/
  render_graph.py`, a CLI taking a `.dot` file path and an output image
  path (format inferred from its extension, e.g. `.svg`/`.png`), that
  invokes the `dot` binary via `subprocess` and reports a clear error if
  `dot` is not on `PATH` or the layout fails. No Python package
  dependency beyond the standard library; no JSON input support (DOT
  only, per decision above) — JSON remains available for a future,
  different consumer (REQ-VIZ-009). Not covered by pFUnit; tested with a
  Python-level test (or a documented manual smoke test if the CI
  environment cannot assume Graphviz is installed — see design.md).
