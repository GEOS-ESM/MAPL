# StateItemCharacteristics and extension reuse

Source: `superstructure/generic/graph/{StateItemCharacteristic,CharacteristicStatus,StateItemCharacteristicKind,StateItemDescriptor,ExtensionResolution,ExtensionMaterialization}.F90`
plus one module per concrete characteristic: `PhysicalUnitsCharacteristic`,
`TypeKindCharacteristic`, `GeometryCharacteristic`,
`VerticalGridCharacteristic`, `UngriddedDimsCharacteristic`,
`ConservationCharacteristic`, `NormalizationCharacteristic`,
`QuantityTypeCharacteristic`, `AttributesCharacteristic`,
`StandardNameCharacteristic`.

## What a StateItemCharacteristic is

An atomic, independently-adaptable aspect of a state item (units,
type/kind, geometry, ...), each reconciled by its own Transform
(`ConvertUnitsTransform`, a regrid Transform, ...). Lives in
`StateItemDescriptor%characteristics` (`StateItemCharacteristicMap`, keyed by
`StateItemCharacteristicKind`), independent of which ESMF component is
allocated.

```
StateItemCharacteristic (abstract)
  ├─ ValueCharacteristic      (holds value inline, never shared)
  │     e.g. PhysicalUnitsCharacteristic, TypeKindCharacteristic
  └─ ReferenceCharacteristic  (holds a NodeId to a shared graph node)
        e.g. GeometryCharacteristic, VerticalGridCharacteristic
```

(`StateItemCharacteristic.F90:77-96`.) `ReferenceCharacteristic`'s
referenced node is an **ordinary graph node** with real `NodeId`/
`NodeRevision` identity — no bespoke "characteristic as graph node"
concept exists (see below).

Four deferred methods on the common ancestor (never duplicated per
kind, REQ-CHAR-009's detection-completeness requirement):
`get_kind()` (NOPASS — a type property, used as the map key),
`needs_extension_for(goal)`, `build_transform(...)`, `get_signature()`.

## CharacteristicStatus

Five values only (`CharacteristicStatus.F90:1`): `INVALID` (never
touched), `SPECIFIED` (resolved/authoritative), `MIRRORED` (will be
made to match a source once connected), `UNCHECKED` (connection allowed
without reconciliation despite a known mismatch — dangerous, must stay
diagnostically distinguishable from `SPECIFIED`), `DEFERRED` (deferred
to a known later point, distinct from `INVALID`'s "not yet touched, no
known future point"). No sixth `ERROR` value — a reconciliation failure
is reported as an ordinary `rc` failure at the call site.

## Why characteristics are NOT graph nodes

Resolved after three rounds of discussion (full history:
`spec/18-state-item-characteristics.md` §18.8–18.9,
`spec/17-open-questions.md` Q12):

1. **Round 1** (superseded): make `StateItemCharacteristic` itself a
   node, reuse `NodeRevision.advance()` + demand-driven update
   uniformly for change propagation.
2. **Round 2** (the counter-example that broke it): a component's `Run`
   phase may mutate a shared geometry characteristic and *immediately*,
   in the same call, populate correctly-shaped export Fields — no
   graph-traversal boundary exists between mutation and use for a pull
   model to hook into.
3. **Round 3** (rejected): require geometry mutation in a dedicated
   "ChangeGeom" phase, giving pull a boundary. Rejected: no mechanism to
   mark a phase special in `SetServices`, and no way to prevent user
   code inside such a phase from calling a child's methods directly
   anyway.

**Adopted:** split by consequence kind.
- **Structural** consequences (Field reallocation) use an eager,
  synchronous, non-reentrant mutator that never invokes user code — safe
  to call from any context, no framework-recognized boundary needed
  (REQ-CHAR-016/017).
- **Content** consequences (Transform recompute) stay fully lazy/pull,
  anchored via REQ-REV-011 (ordinary `MethodGraphNode` invocation
  triggers `update()` on its bound arguments — see
  `hierarchy-and-component-graph.md`).
- No new node kind: a `ReferenceCharacteristic` for geometry references
  an ordinary `StateItemNode` whose `esmf_field` is a geometry proxy in
  `ESMF_FIELDSTATUS_GRIDSET` (see `graph-state-item.md`).

**Still open:** the precise mechanism for identifying "direct
structural dependents" across `ComponentGraph` boundaries (within one
graph, dispatch on `DependencyNetwork` successor kind is natural;
cross-component is only sketched). See `open-questions.md`.

## Status resolution and mismatch detection

There is one detection path. `StateItemDescriptor.F90`'s `resolve_pair`
resolves each kind present on both export and import from the two
statuses (export x import table: SPECIFIED/SPECIFIED compares via
`needs_extension_for`; SPECIFIED/MIRRORED adopts the export value;
UNCHECKED matches except against MIRRORED; DEFERRED always conflicts;
a MIRRORED export matches a SPECIFIED import only if allowlisted by
`is_mirrored_export_allowed`). A kind on only one side is not compared.
`resolve_descriptors` records adoptions on the import descriptor;
`ExtensionResolution%find_mismatched_characteristics` is the map-only
form and fails if an adoption would be needed. A `DEFERRED` or
`INVALID` entry left at freeze fails `StateItemDescriptor%check_resolved`.

## Transform-insertion order (open)

Given two or more mismatches, what order are their reconciling
Transforms chained in? `GraphStateItem` must expose this, delegating to
something that varies by kind — mechanism not yet settled, no
"primary" characteristic is designated for kinds that may not have an
obvious one. See `open-questions.md`.
