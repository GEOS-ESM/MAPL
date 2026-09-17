# 19. Graph Visualization Export

Status: `[SPECULATIVE]` — direction believed correct; exact schema and
trigger mechanics are `[OPEN]` (see §19.9). No implementation exists yet.

## 19.1 Motivation and scope

**Problem.** The graph (nodes, dependencies, ports, revisions) is the one
artifact that fully describes a component's runtime coupling topology, but
it exists only as in-memory Fortran derived types. There is no way to see
it. Debugging a misconnected extension chain, reviewing a component's
actual (as opposed to intended) wiring, or explaining the graph to someone
who does not read Fortran all currently require manual log inspection or
guesswork.

**REQ-VIZ-001.** The design MUST provide a mechanism to serialize graph
topology — nodes, dependencies, ports, and enough metadata to label them
meaningfully — to a plain-text data file at runtime, suitable for
consumption by an external (non-Fortran) rendering tool.

**REQ-VIZ-002.** MAPL/Graph itself MUST NOT render images. Rendering is
explicitly out of scope for the Fortran side; it is delegated to
established external tools (e.g. Graphviz `dot`, or a script that consumes
a JSON/text dump). This keeps no new heavy dependency (graphics libraries,
image codecs) anywhere near the Fortran build.

**Non-goals:**

- Not a live/interactive viewer. This is a batch dump-then-render workflow.
- Not a mechanism for exporting field *data* (array contents). Only
  topology and metadata (names, kinds, revisions, status) are in scope.
  Dumping payload data is explicitly excluded — see §19.7.
- Not a replacement for `11-revision-and-update.md`'s runtime execution
  model. The exporter only *observes* graph state; it MUST NOT trigger
  Transform execution, revision advancement, or any other mutation as a
  side effect of exporting (except where explicitly permitted for a frozen
  graph, which by REQ-CG-006 cannot mutate structurally in any case).

## 19.2 Layering

The exporter follows the same two-layer split already established for the
rest of the design (`08-graph-builder.md` §8.3): a graph-neutral core that
knows only `ComponentGraph`/`DependencyNetwork`/`GraphNode` public query
operations, and a `GraphBuilder`-level enrichment layer that has access to
human-meaningful names.

**REQ-VIZ-003.** A graph-neutral exporter MUST exist that depends only on
`ComponentGraph`'s and `DependencyNetwork`'s public query API (REQ-DEP-005:
`get_predecessors`, `get_successors`, `contains_dependency`, etc.,
`NodeId%to_string()`) and on `GraphNode`'s dispatchable kind (`StateItemNode`
/ `TransformGraphNode` / `MethodGraphNode`). It MUST NOT depend on
`OuterComponent`, `StateRegistry`, or connection-point configuration
(mirrors REQ-CG-002/REQ-GB-001). At this layer, nodes are labeled only by
their opaque `NodeId%to_string()` — this layer alone is enough to unit-test
the export format against synthetic graphs, consistent with the testing
rationale in `08-graph-builder.md` §8.3.

**REQ-VIZ-004.** A `GraphBuilder`-level enrichment layer MUST wrap the
graph-neutral exporter to supply human-readable labels — state item names
(from `StateRegistry`), component names (from the `OuterComponent`
hierarchy), and port names (`10-transforms-and-ports.md`) — without the
core exporter itself needing to know where those names come from. The
enrichment layer supplies a `NodeId → label` lookup (or equivalent
callback/adapter) that the core exporter calls when a label is available,
falling back to `NodeId%to_string()` (REQ-VIZ-003) when it is not.

**REQ-VIZ-005.** Per `02-component-hierarchy.md` REQ-HIER-005/006, the
enrichment layer MUST only traverse a child `OuterComponent`'s *published*
ports and proxy nodes when composing a multi-component (hierarchy-wide)
export — it MUST NOT reach into a child's internal `NodeId`s or
`DependencyNetwork`s directly. A hierarchy-wide export is therefore a
composition of per-`ComponentGraph` exports stitched together at proxy-node
boundaries, not a single traversal that crosses encapsulation.

## 19.3 Trigger and timing

**REQ-VIZ-006.** Export MUST be an explicit, opt-in operation — either an
API call (`export_graph(...)`) or a configuration-gated hook — never
unconditional/always-on behavior. Rationale: even metadata-only traversal
of a ~10,000-node hierarchy (`02-component-hierarchy.md` §2.2) on every
timestep would be wasted cost for the overwhelming majority of runs that
never look at the dump.

**REQ-VIZ-007.** Export MUST be usable at least once after the graph is
frozen (`07-component-graph.md` REQ-CG-006), to capture final structural
topology (nodes, dependencies, ports) once wiring is complete.

**REQ-VIZ-007a `[OPEN]`.** Whether export should also be usable
post-freeze, at arbitrary later points during runtime execution, to
capture the then-current `NodeRevision` values for a "staleness snapshot"
(§19.7), or repeatedly across timesteps to support producing a time series
of such snapshots (e.g. one file per call, animated externally), is not
settled. Structural topology cannot change post-freeze (REQ-CG-006) so
repeated calls would only ever differ in revision/status metadata, not
shape. Left open pending a concrete use case; the mechanism in REQ-VIZ-006
MUST NOT preclude it (i.e., MUST NOT assume "exactly once, at freeze" as
the only supported call pattern).

## 19.4 Output format

**REQ-VIZ-008.** The primary output format MUST be Graphviz DOT
(`.dot`/`.gv`), chosen because: (a) it is plain text, requiring no binary
serialization library dependency in the Fortran build; (b) `dot` and
related Graphviz layout engines are widely available and free; (c) DOT's
native `subgraph cluster_*` construct maps directly onto the nesting this
design already has (component hierarchy, `DependencyNetwork` membership)
without inventing a new grouping convention.

**REQ-VIZ-009.** A secondary JSON output MUST also be supported, for
tools that prefer a structured, non-DOT-specific format (e.g. a Python/D3
script, or a custom viewer). The JSON schema MUST carry the same
information content as the DOT output (§19.5) — same node/edge set, same
metadata fields — so that neither format is a lossy subset of the other.

**REQ-VIZ-009a `[OPEN]`.** Exact DOT attribute conventions (shape/color per
node kind, edge style per network) and exact JSON schema (field names,
nesting) are not specified here. This is intentionally left to a reference
implementation rather than fixed in the spec text, to avoid the same kind
of premature over-specification `17-open-questions.md` warns against
elsewhere for still-fluid mechanisms. Whatever schema is chosen MUST be
documented alongside the exporter code, versioned, and MUST be stable
enough that an external rendering script written against it does not break
on point releases.

## 19.5 Content model

**REQ-VIZ-010.** For each exported node, the dump MUST include at minimum:

- its `NodeId` (or `NodeId%to_string()`, at the graph-neutral layer)
- its node kind (`StateItemNode` / `TransformGraphNode` / `MethodGraphNode`
  — `03-graph-node-hierarchy.md`)
- for `StateItemNode`: payload kind, if statically knowable without
  depending on `04-graph-value-hierarchy.md`'s still-open payload
  representation question (REQ-NODE-003a's `[OPEN, narrowed]` note) — see
  REQ-VIZ-010a
- a human-readable label, where available (§19.2, REQ-VIZ-004)

**REQ-VIZ-010a `[OPEN]`.** The exact per-node-kind metadata fields depend
on the still-open payload representation question
(`03-graph-node-hierarchy.md` REQ-NODE-003a, `17-open-questions.md` Q11).
The exporter's per-kind metadata extraction MUST be isolated behind a
narrow interface (one function/method per payload kind reporting its
"describe for visualization" fields) so that resolving Q11 does not
require redesigning the exporter — only updating this one seam.

**REQ-VIZ-011.** For each exported dependency (edge), the dump MUST
include at minimum: source `NodeId`, target `NodeId`, and the owning
`DependencyNetworkId` (or a network label, at the enrichment layer) it
belongs to (REQ-DEP-002/003). Per REQ-DEP-004, a `StateItemNode` MAY have
different producers across different networks (e.g. get/put,
`15-callbacks.md` §15.10); the network attribution on each edge is what
lets a rendering tool disambiguate this rather than silently merging
distinct networks' edges into one indistinguishable set.

**REQ-VIZ-012.** For `TransformGraphNode`s, named port bindings — now
resolvable via the external `(DependencyNetworkId, NodeId) → port name →
NodeId` table (`10-transforms-and-ports.md` REQ-XFORM-005) — MUST be
included as edge labels (which named input/output port an edge fills).
The exporter SHOULD still degrade gracefully (omit the port label, keep
the edge) for any node/network pair with no binding-table entry, rather
than treating a missing entry as an export error.

**REQ-VIZ-013.** Hierarchy nesting (§19.2, REQ-VIZ-005) SHOULD be
represented as DOT `subgraph cluster_*` blocks (or the JSON equivalent
nested-object structure) keyed by owning `OuterComponent`, so that an
external renderer can visually group each component's nodes without the
exporter having to compute layout itself.

## 19.6 Node labeling and name resolution

**REQ-VIZ-014.** Label resolution (REQ-VIZ-004) MUST NOT fail the export
if a name cannot be found (e.g., an internal/framework-managed extension
item with no user-facing name, `09-extension-reuse.md`). The exporter
MUST fall back to `NodeId%to_string()` for that node rather than aborting
or omitting the node.

**REQ-VIZ-015.** Boundary/proxy nodes (`02-component-hierarchy.md`
REQ-HIER-006) MUST be labeled distinguishably from ordinary nodes (e.g. a
distinct DOT shape/style, or a `"proxy": true` JSON field) — they are
hidden from user-facing *APIs* per REQ-HIER-006, but that requirement does
not extend to a diagnostic visualization; a rendering that silently
presents proxy nodes as if they were ordinary local nodes would misrepresent
the graph's actual encapsulation boundaries.

## 19.7 Revision/staleness overlay `[SPECULATIVE]`

**REQ-VIZ-016 `[SPECULATIVE]`.** The export MAY optionally include each
`StateItemNode`'s current `NodeRevision` value (`11-revision-and-update.md`
REQ-REV-001/003) as metadata, to let an external renderer visually
distinguish "fresh" from "stale" values at the moment of the snapshot
(e.g. color intensity by revision recency). This is additive metadata
only; it MUST NOT be required for a valid export (a purely structural
export, with no revision data, remains valid and useful on its own).

**REQ-VIZ-016a.** Reading a `NodeRevision` for export purposes MUST use
the existing accessor (`get_revision()`, REQ-REV-003) and MUST NOT itself
trigger `advance_revision()` or any Transform execution
(`11-revision-and-update.md` REQ-REV-005/006) — export is read-only with
respect to graph state, full stop (restated from REQ-VIZ-002).

## 19.8 Explicitly excluded: payload data dump

**REQ-VIZ-017.** The exporter MUST NOT serialize `ESMF_Field`/
`ESMF_FieldBundle`/`ESMF_State` array contents, RouteHandle internals, or
any other bulk payload data. Scope is topology and metadata only (§19.1).
Rationale: (a) this is a topology/debugging tool, not a data-export
mechanism — MAPL/history already own the latter; (b) unbounded array dumps
at graph-export granularity would make export cost scale with data volume
rather than graph size, defeating the "cheap, opt-in diagnostic" intent of
REQ-VIZ-006.

## 19.9 Open questions

**`[OPEN]`** The following are tracked as `17-open-questions.md` Q15,
Q16, Q18 (Q17 is resolved — see below):

- Exact DOT/JSON schema (REQ-VIZ-009a) — Q15.
- Whether repeated/time-series export (REQ-VIZ-007a) is a real requirement
  or speculative until a use case appears — Q16.
- Whether a hierarchy-wide (multi-`ComponentGraph`) export (REQ-VIZ-005)
  is needed for v1.0, or whether per-component export is sufficient until
  the future global graph view (`02-component-hierarchy.md` §2.4)
  materializes — Q18.

**Resolved.** Q17 (port-binding labels vs. port-binding storage location)
is resolved: `10-transforms-and-ports.md` REQ-XFORM-005 settles port
binding storage as an external `(DependencyNetworkId, NodeId)`-keyed
table, so REQ-VIZ-012 no longer depends on an open question — port-binding
edge labels are implementable now.
</content>
</invoke>
