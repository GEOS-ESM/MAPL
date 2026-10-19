# Transforms, ports, and the update algorithm

Source: `superstructure/generic/graph/{TransformBase,Transform,TransformGraphNode,PortSpec,PortBindingKey,PortBindingTable}.F90`,
containers `PortSpecMap`/`PortNameRevisionMap`/`PortNameGraphStateItemMap`/`PortBindingKeyPortMap`.
Update algorithm lives in `ComponentGraph_DemandDrivenUpdate.F90`
(described in `hierarchy-and-component-graph.md`; this file covers the
Transform/port side it drives).

## Transform / TransformBase split

Split across two modules specifically to break a circular dependency
(`openspec/changes/merge-characteristic-families`):

- **`TransformBase`** (`TransformBase.F90:1`) — abstract, port
  *declaration* only: named, multi-input/multi-output ports
  (`declare_input_port()`/`declare_output_port()`, backed by
  `PortSpecMap`). No dependency on `GraphStateItem`/
  `PortNameGraphStateItemMap` at all.
- **`Transform`** (`Transform.F90:1`) — extends `TransformBase`, adds
  exactly one deferred binding:
  ```fortran
  subroutine compute(this, inputs, outputs, rc)
     class(Transform), intent(inout) :: this
     type(PortNameGraphStateItemMap), intent(in) :: inputs
     type(PortNameGraphStateItemMap), intent(in) :: outputs
     integer, intent(out) :: rc
  end subroutine
  ```
  (`Transform.F90:58-66`.) This one binding is why `Transform` needs
  the `GraphStateItem`-reaching dependency chain and `TransformBase`
  doesn't — `StateItemCharacteristic`'s `build_transform` deferred
  method needs to name a `transformer` dummy argument's type without
  itself pulling in `GraphStateItem` (a real cycle otherwise, since
  `GraphStateItem` holds a `StateItemCharacteristicMap`). See
  `characteristics.md`.

A real `Transform` subclass (`ConvertUnitsTransform`,
`TypeKindConverterTransform`, ...) declares its fixed port shape once,
in its own constructor — port shape is subclass domain knowledge, never
rediscovered by whatever wires an instance into a graph.

## PortSpec

`PortSpec.F90:1` — declaration-only: a name plus an optional expected
`MAPL_StateItem_Flag` kind constraint. **Never holds a bound `NodeId`**
— that's the external binding table's job (below).

## TransformGraphNode

`TransformGraphNode.F90:1`. Holds one `Transform`, delegates every
port-query method to it unchanged. Adds only: NodeId/lifecycle
(inherited) and execution-history bookkeeping —
`has_run()`/`needs_execution()`/`record_run()` — for the demand-driven
algorithm below.

## Port bindings: external, not on-node (REQ-XFORM-005)

**Settled, after an explicit design back-and-forth** (`open-questions.md`
Q3): bindings are stored externally in `ComponentGraph`'s
`PortBindingTable`, keyed `(DependencyNetworkId, NodeId) -> port name
-> NodeId` (`PortBindingTable.F90:1`), **uniformly** — not on-node even
for the (common) single-network case. This generalizes the pattern
callback method bindings already used (`CallbackMethodBinding`, which
genuinely needs two networks per attachment, get/put) to ordinary
Transforms too, so code that walks bindings never branches on
"on-node vs. external storage" per node kind.

`PortBindingTable` itself knows nothing about ownership or declared
port names — those checks (does this `NodeId` belong to this component,
does the target node actually declare this port name, kind-constraint
matching) are `ComponentGraph%bind_port()`'s job, one layer up
(mirrors `DependencyNetwork`'s own "no ownership knowledge" stance).
`bind()` rejects rebinding an already-bound port name for the same
`(network, node)` pair — no silent replace. `get_binding()` returns
null (not an error) for a missing binding — used by `GraphExport` to
degrade gracefully (omit the label, keep the edge) rather than fail.

## Demand-driven update, from the Transform side

See `hierarchy-and-component-graph.md`'s "Demand-driven `update()`"
section for the full traversal. From a `Transform` author's point of
view, the contract is:

1. `ComponentGraph%update()` recursively ensures every declared input
   port's `NodeId` is current first.
2. It assembles the current input revision snapshot via
   `get_port_binding()` and asks the node's own staleness bookkeeping
   whether a run is needed (compares against the last recorded
   baseline).
3. If stale, calls `compute(inputs, outputs, rc)` with the resolved
   `PortNameGraphStateItemMap`s.
4. Advances **every declared output's revision together** (REQ-REV-007
   — never a partial update across outputs), records the new baseline.

A `Transform` never sees a `NodeId`, a `ComponentGraph`, or a
`DependencyNetwork` — only resolved `GraphStateItem` values by port
name. This is deliberate (`decouple-transform-from-graph`): a
`Transform` is unit-testable with synthetic `GraphStateItem` values,
no graph machinery present at all.
