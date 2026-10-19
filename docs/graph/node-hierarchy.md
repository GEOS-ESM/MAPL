# Graph node hierarchy

Source: `superstructure/generic/graph/{GraphNode,BaseGraphNode,OperationGraphNode,StateItemNode,TransformGraphNode,MethodGraphNode,NodeRevision}.F90`

## Shape

```
GraphNode (abstract, data-free)
  └─ BaseGraphNode (NodeId + lifecycle-status; the only place NodeId is stored)
       ├─ StateItemNode        (concrete "value" node)
       └─ OperationGraphNode   (abstract "runs" node)
            ├─ TransformGraphNode   (concrete; demand-driven, in DependencyNetwork traversal)
            └─ MethodGraphNode      (concrete; explicitly invoked, NOT in demand-driven traversal)
```

- `GraphNode` (`GraphNode.F90:1`): one deferred method, `get_node_id()`.
  No stored data anywhere in this type.
- `BaseGraphNode` (`BaseGraphNode.F90:1`): adds the `NodeId` (set once at
  construction via the structure constructor, no public mutator) and a
  lifecycle-status placeholder. **No adjacency is ever stored on a
  node** — adjacency lives entirely in `DependencyNetwork`
  (`dependency-network.md`).
- `OperationGraphNode` (`OperationGraphNode.F90:1`): pure stub, adds
  nothing. Exists only to distinguish "runs" nodes from `StateItemNode`'s
  "value" nodes for dispatch purposes (see `ComponentGraph%update()`,
  `transforms-and-update.md`).

## StateItemNode

`StateItemNode.F90:1`. Holds exactly one `GraphStateItem` payload
(`graph-state-item.md`) and one `NodeRevision`, plus identity metadata
set once by `GraphBuilder_Advertising_smod.F90`'s `advertise_one`:

- `short_name`, `state_intent` (`ESMF_StateIntent_Flag`)
- `is_inout_borrower` (`StateItemNode.F90:54`)
- `callback_interface_id`

Key accessors (`StateItemNode.F90:160` onward):

- `get_payload()` — returns a **copy**. `get_payload_ptr()`
  (`StateItemNode.F90:200`) returns a pointer for in-place
  read/mutate — prefer this when not replacing the payload wholesale;
  intrinsic-assignment copy of a populated composite `GraphStateItem`
  has triggered nondeterministic gfortran heap corruption (deep-copy
  through `StateItemMemberMap`'s defined assignment) — comment at
  `StateItemNode.F90:195` has the full story. NAG does not reproduce it.
- `advance_revision()` — the **only** path that means "this node's
  value changed." Never fabricate a revision value; `set_revision()`
  exists only for construction-time setup.

## NodeRevision

`NodeRevision.F90:1`. 64-bit monotonic stamp, internal integer private.
`is_valid()` distinguishes "never produced" from an ordinary revision —
default-constructed is a distinct `INVALID_VALUE`, not `0`.
`advance()` detects overflow before it happens. `to_string()` is
diagnostic-only (used by `GraphExport`).

## TransformGraphNode

`TransformGraphNode.F90:1`. Holds exactly one `Transform` (component
name `transformer`) and delegates every port-query method to it
unchanged. Adds only graph-plumbing the `Transform` itself doesn't
carry: staleness/baseline bookkeeping for the demand-driven update
algorithm (`has_run()`/`needs_execution()`/`record_run()`). Port
*bindings* (which `NodeId` fills which named port, per network) are
**not** stored here — see `transforms-and-update.md` on
`PortBindingTable`.

## MethodGraphNode

`MethodGraphNode.F90:1`. Represents either a GridComp init/run phase or
an attached ESMF State callback method, through **one** node type —
the node never branches on which. Holds exactly one
`MethodInvocationAdapter` and delegates invocation to it.

- Named argument declarations + bindings are stored **on-node**
  (`ArgumentSpecMap` + `StateItemMemberMap`), unlike `TransformGraphNode`'s
  externally-stored port bindings — deliberately different: a
  `MethodGraphNode` is permanently excluded from `ComponentGraph`'s
  demand-driven dispatch (see below), so there's no shared traversal
  code that would need a per-node-kind branch for binding storage.
- **Never scheduled by `ComponentGraph%update()`** — a `class default`
  no-op branch in `ComponentGraph_DemandDrivenUpdate.F90` explicitly
  skips any `GraphNode` kind that isn't `StateItemNode`/
  `TransformGraphNode`. Methods are invoked explicitly by
  `GriddedComponentDriver`/callback machinery, not pulled by the graph.
  See `methods-and-drivers.md`.

## Dispatch summary (who runs when)

| Node kind | In `update()` traversal? | Triggered by |
|---|---|---|
| `StateItemNode` | Target of update, not itself "run" | consumer pulling a value |
| `TransformGraphNode` | Yes — recursively, demand-driven | a stale `StateItemNode` successor being pulled |
| `MethodGraphNode` | No — explicit `class default` skip | `GriddedComponentDriver` phase call / callback invocation |

See `transforms-and-update.md` for the actual `update()` algorithm.
