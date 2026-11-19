# 10. Transform Nodes and Named Ports

Status: `[SETTLED]` node model and port-binding storage location (§10.3,
`17-open-questions.md` Q3).

## 10.1 Transforms are nodes, not edges

**REQ-XFORM-001.** Computations (Transforms) MUST be represented as
`TransformGraphNode`s, not as annotations on dependency edges. Edges
(`06-dependency-network.md`) express only traversal topology.

## 10.2 Multiple named inputs/outputs

**REQ-XFORM-002.** A `TransformGraphNode` MAY have multiple named inputs and
multiple named outputs. Example (vertical transform):

```
source_field
source_vertical_grid
destination_vertical_grid
    -> VerticalTransformNode
    -> destination_field
```

**REQ-XFORM-003.** Transform subclasses MUST expose their named argument
specifications (input/output names, expected `GraphValue` kind) rather than
relying on generic ESMF state-item names such as `"import"`/`"export"`.
Named ports are a graph-level concept independent of whatever ESMF State
naming convention originally motivated a given Transform.

## 10.3 Topology vs. semantic binding — location `[SETTLED]`

Two distinct concerns exist and MUST NOT be conflated:

- **Adjacency** (`DependencyNetwork` predecessor/successor sets) represents
  *traversal topology*: "this node must be updated before that one."
- **Named port bindings** represent *semantic argument binding*: "this
  specific `NodeId` fills the `source_vertical_grid` argument of this
  specific `TransformGraphNode`."

**REQ-XFORM-004.** Port *declarations* (the named-argument specification
itself, e.g. "I have an input named `source_field` expecting a
`FieldValue`") belong to the Transform (type-level or instance-level
metadata on `TransformGraphNode` / its subclass).

**REQ-XFORM-005 `[resolves 17-open-questions.md Q3]`.** Port *bindings*
(which concrete `NodeId` fills which named port, for a specific network)
MUST be stored externally, keyed by `(DependencyNetworkId,
TransformGraphNode NodeId)`, mapping to `port name → NodeId`. This
generalizes the callback approach (`CallbackStateBinding`,
`15-callbacks.md` REQ-CB-007, already external) to ordinary Transforms,
rather than special-casing single-network transforms with on-node storage.

**Rationale.** The snapshot originally proposed on-node storage as viable
"if each transform node belongs to only one relevant network," but
callback method bindings were already specified as external precisely
because callback methods routinely belong to two networks (get/put).
Selecting storage strategy per-instance based on how many networks a node
happens to participate in would force all binding-walking code to branch
on "is this on-node or external" per node. A uniform external table
removes that branch entirely and costs nothing for the single-network
case (a 1-entry lookup); any future lookup-overhead concern at scale is
not on the long-term hot path, since `11-revision-and-update.md` §11.5
already anticipates compiling the frozen graph into direct wrappers for
performance. Confirmed by the spec author (`17-open-questions.md` Q3).

Do not reintroduce on-node (Option A) storage as a "simple case"
optimization without revisiting this decision explicitly — it was
considered and rejected as the general-purpose answer, not merely
undecided.
