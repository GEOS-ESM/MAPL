## Context

See proposal.md for motivation. Two existing pieces this change builds
on, unchanged by it:

- `GraphExport.F90`'s `export_graph_dot`/`export_graph_json`
  (`graph/visualization-export`, REQ-VIZ-001..003/006..017): graph-neutral,
  labels every node only by `NodeId%to_string()`, no `OuterComponent`/
  `StateRegistry` dependency.
- `GraphBuilder.F90`'s advertising/connection machinery (3a-3c2, all
  archived): `advertise_one` already creates one `StateItemNode` per
  advertised item, keyed in the owning `ComponentGraph`'s resource index
  under `item_key(state_intent, short_name)` (`'IMPORT:'`/`'EXPORT:'`/
  `'INTERNAL:'` + short name); a parent-local proxy standing in for a
  named child's item is keyed under `proxy_key(child_name, intent,
  short_name)` (`'PROXY:' // child_name // ':' // item_key(...)'`) in the
  *parent's own* graph. Both keys are already public functions
  (`GraphBuilder_mod`'s `item_key`/`proxy_key`); `ComponentGraph`'s
  `get_resource_index(key)` is a plain, already-public forward lookup
  (key -> `NodeId`), with no reverse/enumeration accessor.

## Goals / Non-Goals

**Goals:**
- Give a human a readable DOT/JSON export of one component's own graph:
  its own advertised items under their own names, and any cached
  child-proxy nodes under a child-qualified name, marked as proxies.
- Do this without adding any new dependency to `GraphExport.F90` beyond
  a plain, caller-supplied lookup, and without adding any new
  `StateRegistry` dependency anywhere in the graph layer (consistent
  with 3c2's precedent of not depending on machinery slated for
  retirement).
- Do this without any new `ComponentGraph`/`DependencyNetwork` public
  API — reuse the existing forward `get_resource_index` lookup plus
  `ComponentSpec%var_specs` iteration, exactly as `graphbuilder_advertise`
  already does.

**Non-Goals:**
- Hierarchy-wide (multi-`ComponentGraph`) export, DOT `subgraph
  cluster_*` nesting across components (REQ-VIZ-005/013, `19-
  visualization-export.md` §19.9 Q18) — per the roadmap, 3d needs only
  3a's `OuterComponent` shape and name lookup for a single component;
  composing multiple components' exports together is a distinct, later
  capability.
- Any reverse-enumeration API on `ComponentGraph`'s resource index. Not
  needed: the label map is built by walking the *known* sources (this
  component's own `var_specs`, and each child's own published
  `var_specs`) and doing a forward `get_resource_index` lookup for each
  — the same access pattern `advertise_one`/`get_or_make_local_node_id`
  already use, just read instead of written.
- Any dependency on `StateRegistry` for names. `19-visualization-
  export.md` REQ-VIZ-004 describes names as coming "from `StateRegistry`",
  but `GraphBuilder` never reads `StateRegistry` at all today — advertised
  names already live on `VariableSpec%short_name` /
  `ComponentSpec%var_specs`, which `GraphBuilder` already reads directly
  for advertising. Reusing that same source keeps this change on the
  graph-native path 3b/3c/3c2 already established rather than introducing
  a fresh dependency on machinery slated for removal (`20-implementation-
  roadmap.md` §20.4.2).
- Revision/staleness overlay (REQ-VIZ-016/016a) — already implemented in
  Phase 2's `GraphExport.F90`; unaffected by and unrelated to this
  change.

## Decisions

**Label lookup is a plain, pre-built map passed into `export_graph_dot`/
`export_graph_json`, not a callback.** REQ-VIZ-004 allows either "a
`NodeId -> label` lookup (or equivalent callback/adapter)". A plain map
(new `NodeIdLabelMap` gFTL container, `NodeId -> label` — a small
Data-only structure attached per-entry with an `is_proxy` flag, mirroring
the existing `NodeId`-keyed containers already in
`superstructure/generic/graph/containers/`, e.g. `NodeIdGraphNodeMap`)
is simpler than an abstract callback interface here: the enrichment
layer already has everything it needs before calling the exporter (a
component's `var_specs` and cached proxies do not change mid-export,
and the exporter is read-only/non-mutating per REQ-VIZ-002/016a), so
there is no benefit to on-demand resolution via a callback, only added
interface surface. *Rejected alternative:* an abstract
`label_resolver(NodeId) -> string` callback interface. Rejected as
unnecessary indirection for a lookup that is always fully known before
the export call.

**Enrichment builds the map by walking `var_specs`, not by reversing
the resource index.** For "this component's own items": iterate
`this%get_component_spec()%var_specs` (identical iteration
`graphbuilder_advertise` already performs) and for each entry call
`graph%get_resource_index(item_key(var_spec%state_intent,
var_spec%short_name))` to recover its `NodeId`, labeling it
`var_spec%short_name`. For "child proxy nodes": for each child name
(`this%get_child_name(i)`, already existing accessor) walk that child's
own `get_child_component_spec(child_name)%var_specs` (the same
framework-internal carve-out `GraphBuilder`'s connection-resolution code
already uses, REQ-GB-002 — reads only the child's already-published
declarative spec, not its internal graph) and for each entry call *this
component's own* `graph%get_resource_index(proxy_key(child_name,
var_spec%state_intent, var_spec%short_name))`; if found, label it
`child_name // ':' // var_spec%short_name` and mark it a proxy. An entry
not found (child item never actually connected, so no proxy was ever
cached) is simply absent from the map — not an error. *Rejected
alternative:* add a `ComponentGraph` accessor to enumerate all
resource-index key/`NodeId` pairs and strip the `'IMPORT:'/'EXPORT:'/
'PROXY:...'` key-string prefix to recover the name. Rejected: parsing a
key string to recover a name it happens to embed is more fragile than
reading the name from its actual source (`VariableSpec%short_name`), and
would add a new Phase 1–2 `ComponentGraph` public API for something the
existing `var_specs` iteration already provides without one.

**Proxy marking travels alongside the label, not as a separate lookup.**
The `NodeIdLabelMap` entry carries both the label string and an
`is_proxy` logical, set only by the child-proxy branch above. Simpler
than a second `NodeId`-keyed set: the enrichment layer already visits
proxy nodes exactly once, at the point it has both pieces of
information.

**Unnamed nodes (extension items, `TransformGraphNode`s, any node kind
the enrichment layer does not resolve) are simply absent from the map.**
`GraphExport.F90`'s existing REQ-VIZ-014 fallback (`NodeId%to_string()`
when unlabeled) already handles this — the enrichment layer does not
need its own separate not-found handling; it only needs to leave such
nodes out of the map it hands to the exporter.

**Render script shells out to the `dot` binary via `subprocess`, takes
DOT input only, and adds no Python package dependency.** Three options
were on the table: (a) `subprocess` + the `dot` CLI, (b) the `graphviz`
PyPI package (a thin wrapper over the same CLI), (c) parse the JSON
export directly and lay the graph out in pure Python. Chose (a): it is
the simplest layer over the same tool REQ-VIZ-008 already assumes is
installed for this workflow, needs no `pip install` step for anyone who
already has Graphviz for the CLI use case in `19-visualization-
export.md` §19.1, and keeps the script's own dependency surface at
"Python stdlib + Graphviz," matching this capability's general
"external tool, not a new heavy dependency" posture (REQ-VIZ-002).
DOT-only input (not JSON) follows directly from picking `dot` as the
rendering engine — JSON's own consumer (REQ-VIZ-009, "a Python/D3
script, or a custom viewer") is a different, not-yet-needed tool, not
this one. *Rejected alternative (b):* `graphviz` package. Rejected as
unnecessary — it wraps the exact same `dot` subprocess call this design
makes directly, for a marginally nicer call syntax not worth a new pip
dependency. *Rejected alternative (c):* pure-Python JSON layout.
Rejected as substantially more code (reimplementing what `dot`'s layout
engine already does) for no benefit when `dot` is already the assumed
external tool.

**Script location and testing.** `tools/graph_visualization/
render_graph.py`, a new top-level `tools/` directory (this repo has
`apps/` for existing end-user Python utilities, e.g. `apps/mapl_tree.py`;
`tools/` is used here instead since this script is a development/
diagnostic aid paired with the graph work, not an end-user application
in the sense the other `apps/` scripts are — open to relocating into
`apps/` later if that distinction turns out not to matter in practice).
No CMake integration — it is invoked directly (`python3 render_graph.py
graph.dot graph.svg`), not built or installed. Testing: a `unittest`
test module (matching this repo's existing Python test precedent,
`apps/tests/acg3/acg3_unittests.py`) covering the three CLI behaviors in
spec.md (success, missing `dot`, malformed DOT) using a minimal
hand-written `.dot` fixture; if CI cannot assume Graphviz is installed,
the missing-`dot` and malformed-input cases can still be tested by
mocking `subprocess.run`, while the success case is skipped (not
failed) when `dot` is not discoverable on the test machine.

## Risks / Trade-offs

- **[Risk] A child's `var_specs` walk duplicates work
  `GraphBuilder`'s own connection-resolution code already does for a
  different purpose** (matching import/export names across components).
  **Mitigation:** both uses are read-only, cheap (component-local
  `var_specs` vectors, not full graph traversal), and already an
  accepted access pattern (REQ-GB-002); no shared state or ordering
  dependency between them.
- **[Risk] A label collision is possible in principle** — an
  `INTERNAL` item and an `EXPORT` item could theoretically share a
  `short_name` if MAPL's own naming rules ever allowed it, and the label
  map does not disambiguate by intent the way `item_key` does.
  **Mitigation:** this is a diagnostic visualization, not an identity
  system — `NodeId` remains the actual unique identity in the export;
  a duplicate-looking label at worst makes the dump momentarily
  ambiguous to a human reader, never a incorrect graph. Not treated as
  a defect to solve in this change.
- **[Risk] New optional parameters on `export_graph_dot`/
  `export_graph_json` are a public API change to the Phase 1–2
  exporter**, even though additive/backward-compatible.
  **Mitigation:** both parameters are optional with the same
  no-lookup-supplied behavior as today; every existing call site
  (including `Test_GraphExport.pf`) is unaffected without modification.

## Open Questions

None. Scope (single component only, no `StateRegistry` dependency, map
over callback) was resolved above rather than left open.
