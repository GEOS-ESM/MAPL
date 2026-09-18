## 1. Label-map container

- [x] 1.1 Add a new `NodeId`-keyed gFTL map container in
      `superstructure/generic/graph/containers/` (e.g.
      `NodeIdLabelMap.F90`), mirroring the existing `NodeId`-keyed
      containers in that directory (`NodeIdGraphNodeMap`,
      `NodeId_NodeIdSet_Map`). Each entry carries a label string and an
      `is_proxy` logical (design.md Decisions, "Proxy marking travels
      alongside the label").
- [x] 1.2 Unit-test the container in isolation (insert, lookup,
      not-found case) following the existing container test pattern in
      that directory, if one exists for a comparable map.

## 2. Exporter: optional label lookup

- [x] 2.1 Add an optional label-lookup parameter (the new
      `NodeIdLabelMap`) to `export_graph_dot` and `export_graph_json`
      (`superstructure/generic/graph/GraphExport.F90`). When present and
      an entry exists for a node, use its label text in place of
      `NodeId%to_string()` for that node's identity in the output
      (spec.md "Exporter accepts an optional node-label lookup").
- [x] 2.2 When the parameter is absent, or a node has no entry, confirm
      output is byte-for-byte unchanged from the current implementation
      (spec.md "Export without a label lookup is unchanged" / "Node with
      no lookup entry falls back to its identity string").
- [x] 2.3 When an entry's `is_proxy` flag is set, render that node
      distinguishably in both formats: a distinct DOT shape/style
      attribute, and a `"proxy": true` JSON field (spec.md "Boundary/
      proxy nodes are marked distinguishably"). A node with no entry, or
      an entry with `is_proxy` false, carries no such marking.
- [x] 2.4 Update `GraphExport.F90`'s header comment (schema
      documentation, REQ-VIZ-009a) to describe the new optional label/
      proxy fields in the DOT and JSON schemas.

## 3. GraphBuilder: enrichment procedure

- [x] 3.1 Add a new `GraphBuilder`-level procedure (e.g.
      `build_label_map(this, rc) result(label_map)` in
      `superstructure/generic/GraphBuilder.F90`) that, given an
      `OuterMetaComponent`, builds and returns a populated
      `NodeIdLabelMap` for that component's own `ComponentGraph`.
- [x] 3.2 In that procedure, iterate `this%get_component_spec()%
      var_specs` (same pattern as `graphbuilder_advertise`) and for each
      `var_spec`, look up its `NodeId` via
      `graph%get_resource_index(item_key(var_spec%state_intent,
      var_spec%short_name))`; when found, add an entry labeled
      `var_spec%short_name`, `is_proxy = .false.` (design.md Decisions,
      "Enrichment builds the map by walking `var_specs`").
- [x] 3.3 For each child (`this%get_child_name(i)` over
      `this%get_num_children()` or the existing equivalent iteration),
      read that child's own `this%get_child_component_spec(child_name)%
      var_specs` and for each entry look up
      `graph%get_resource_index(proxy_key(child_name,
      var_spec%state_intent, var_spec%short_name))` in **this**
      component's own graph; when found, add an entry labeled
      `child_name // ':' // var_spec%short_name`, `is_proxy = .true.`
- [x] 3.4 Confirm a child item with no cached proxy (never actually
      connected) simply produces no map entry — not an error (spec.md
      "Enrichment never fails export when a name is unavailable").
- [x] 3.5 Confirm this procedure only reads `ComponentSpec`/`var_specs`
      data and this component's own `ComponentGraph`'s resource index —
      no child `ComponentGraph`, `NodeId`, or `DependencyNetwork` is
      touched (spec.md "Enrichment layer does not cross into a child's
      own graph").

## 4. Wire-up and tests

- [x] 4.1 Add pFUnit coverage asserting a component's own advertised
      import/export/internal items appear under their own names in both
      DOT and JSON export when the enrichment procedure's label map is
      supplied.
- [x] 4.2 Add pFUnit coverage asserting a cached child-proxy node is
      labeled `"<child_name>:<short_name>"` and marked as a proxy in
      both formats.
- [x] 4.3 Add pFUnit coverage asserting a node absent from the label map
      (e.g. an extension item or `TransformGraphNode`) still falls back
      to `NodeId%to_string()`, and export does not fail.
- [x] 4.4 Confirm existing `Test_GraphExport.pf` coverage (no label map
      supplied) passes unchanged.
- [x] 4.5 Confirm no new code path in this change references
      `StateRegistry`, `StateItemSpec`, or any `ClassAspect` subclass
      (design.md Goals; matches the precedent set by
      `extension-registry-visibility`).

## 5. Render script

- [x] 5.1 Create `tools/graph_visualization/render_graph.py`: a CLI
      taking an input `.dot` path and an output image path, that
      invokes the installed `dot` executable via `subprocess` to render
      the output in the format implied by the output path's extension
      (design.md Decisions, "Render script shells out to the `dot`
      binary").
- [x] 5.2 Handle the missing-`dot` case: catch the executable-not-found
      condition and exit non-zero with a message identifying that
      Graphviz's `dot` could not be found (spec.md "Missing Graphviz
      installation fails clearly").
- [x] 5.3 Handle a `dot` layout failure (non-zero exit from `dot`
      itself, e.g. malformed input): exit non-zero, include `dot`'s own
      error output, and do not leave behind an output file that implies
      success (spec.md "Graphviz layout failure is reported, not
      swallowed").
- [x] 5.4 Confirm the script imports nothing beyond the Python standard
      library (spec.md "Script has no dependency beyond the Python
      standard library").
- [x] 5.5 Add a `unittest` test module (matching
      `apps/tests/acg3/acg3_unittests.py`'s existing convention)
      covering: successful render with a minimal hand-written `.dot`
      fixture, the missing-`dot` case (mockable via `subprocess`
      patching so it does not require an environment without Graphviz),
      and the malformed-DOT case. Skip (not fail) the successful-render
      case on a test machine where `dot` is not installed.
- [x] 5.6 Add a short usage note (README or module docstring) showing
      the end-to-end flow: Fortran export produces a `.dot` file (task
      groups 1-4) -> `render_graph.py graph.dot graph.svg`.
- [x] 5.7 Manually demonstrate `render_graph.py` end to end against a
      small, hand-written `.dot` file exercising the new label/proxy
      attributes (a handful of nodes, at least one plain node and one
      proxy-marked node) and visually inspect the rendered image looks
      correct. One-off sanity check for review purposes only — the
      fixture and rendered output do not need to be committed to the
      source tree (task 5.5's `unittest` fixture is the permanent,
      automated equivalent).
