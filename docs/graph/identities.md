# Identities

Source: `superstructure/generic/graph/{NodeId,DependencyNetworkId,PortId,CallbackInterfaceId,HierarchyId}.F90`,
template `superstructure/generic/graph/include/IdTemplate.inc`.

## Pattern

Every id type is generated from one FPP template
(`IdTemplate.inc:1`), instantiated per type with a two-line file:

```fortran
module mapl_NodeId_mod
#define ID_NAME NodeId
#include "IdTemplate.inc"
end module mapl_NodeId_mod
```

(`NodeId.F90:1`; same shape for the other four.)

Each generated type gets, for free:

- a private `INT32` value (`INVALID_VALUE = -1`), opaque to callers
- `is_valid()` — true unless default-constructed/explicitly invalid
- `to_string()` — diagnostic formatting only, not a serialization
  contract; returns `''` for an invalid id
- `operator(==)`, `operator(/=)`, `operator(<)` (the last for ordered
  containers)
- a sibling `<Name>Generator` type with one method, `next()`, returning
  monotonically increasing fresh ids starting at `1`
- a module parameter `INVALID_<Name>`

No id type exposes its raw integer, no public constructor from an
arbitrary integer, and no arithmetic. The only way to get a valid id is
`generator%next()`.

## The five id types

| Type | Generator owner | Scope |
|---|---|---|
| `NodeId` | `HierarchyGraph` (one generator per hierarchy) | identifies any `GraphNode` hierarchy-wide |
| `DependencyNetworkId` | `HierarchyGraph` | identifies a `DependencyNetwork` hierarchy-wide |
| `PortId` | `ComponentGraph` (per-component) | identifies a public import/export/child port |
| `CallbackInterfaceId` | `CallbackInterfaceRegistry` | identifies a registered `CallbackInterface` |
| `HierarchyId` | `HierarchyRegistry` | identifies one component hierarchy (one per `OuterMetaComponent` tree, or one per standalone test graph) |

`NodeId`/`DependencyNetworkId` generation moved from per-`ComponentGraph`
to per-`HierarchyGraph` (see `hierarchy-and-component-graph.md`) so that
ids are unique across an entire hierarchy, not just within one
component — required once parent-local proxy nodes for child items
exist in the same shared id space as the child's own nodes.

## Adding a new id type

Copy one of the two-line files, substitute the name, add it to
`CMakeLists.txt`. If you need a non-zero default (`DEFAULT_ID`), define
`DEFAULT_ID` before including the template (see template's
`#if defined(DEFAULT_ID)` branch, `IdTemplate.inc:33`) — none of the
five current instantiations do this.
