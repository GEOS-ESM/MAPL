# 2. Component Hierarchy

Status: `[SETTLED]` core structure; `[SPECULATIVE]` future global graph view
(§2.4).

## 2.1 OuterComponent

**REQ-HIER-001.** Every user component MUST be wrapped by an `OuterComponent`.

**REQ-HIER-002.** An `OuterComponent` MUST contain:

- A `GriddedComponentDriver` for its own user component
- One `GriddedComponentDriver` per child `OuterComponent`
- One local `ComponentGraph`
- Framework-managed import, export, and internal states
- Public ports visible to its parent

**REQ-HIER-003.** A parent `OuterComponent` MAY access its children's public
ports and drivers. Children MUST NOT access their parent. This is a hard
encapsulation boundary, not just a convention.

## 2.2 One graph per component, not one global graph

**REQ-HIER-004.** Each `OuterComponent` owns exactly one local
`ComponentGraph`. There is no single global graph instance in v1.0.

**Rationale:** child graphs remain encapsulated; a parent never needs to
reach into a child's internal wiring, only its published ports. This bounds
graph size per component (target ~10,000 nodes is a whole-hierarchy figure,
not necessarily per-component, but locality still helps validation,
diagnostics, and freeze semantics scale sanely).

## 2.3 Child graphs and boundary/proxy nodes

**REQ-HIER-005.** A parent MUST refer to a child's graph only through the
child's published ports. Direct reference to a child's internal `NodeId`s or
`DependencyNetwork`s is prohibited.

**REQ-HIER-006.** A parent MAY generate parent-local boundary or proxy nodes
that represent a child's ports. These proxy nodes MUST be generated and
cached by `GraphBuilder`, and MUST be hidden from user-facing APIs (they are
an implementation detail of cross-graph wiring, not something a component
author names or sees).

## 2.4 Future global graph view `[SPECULATIVE]`

A future global graph view MAY be built that inspects frozen local graphs
across the hierarchy for diagnostics or optimization purposes. It:

- MUST NOT own nodes (ownership stays with each local `ComponentGraph`)
- MUST operate only on frozen graphs (never mutate)
- Is out of scope for v1.0

See `17-open-questions.md` Q1 for the open question of whether this remains
sufficient once `MethodGraphNode` (parent-invokes-child-method) is fully
accounted for.
