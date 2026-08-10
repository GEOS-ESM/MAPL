# Graph Layer Class Roles

This document describes ownership and responsibilities for major classes in
`superstructure/generic/graph`. It is an implementation guide, not an API
reference.

## Short Version

`ComponentGraph` owns executable graph topology.

`GraphBuilder` derives representations and automatic transform paths.

`GraphAssembly` collects component declarations, resolves names and aliases,
then materializes a graph.

`ComponentInterface` describes component-boundary ports. It does not expose a
component's graph.

## ComponentGraph

`ComponentGraph` is authoritative runtime graph storage. It owns:

- `GraphNode` objects;
- `GraphEdge` objects;
- dependency networks;
- node and edge identifiers;
- validity and revision state;
- frozen topology state.

Use it when topology already exists and code needs to validate, update,
invalidate, or execute it.

`ComponentGraph` does not decide which representation or transform should
satisfy a connection. It enforces graph invariants after callers choose those
objects.

## GraphBuilder

`GraphBuilder` is a representation and transform derivation algorithm. It
starts with registered `ItemSpec` representations and answers:

> Which graph representation satisfies this required specification?

When specifications differ only in supported metadata such as precision or
units, `GraphBuilder` inserts and reuses automatic transform chains. Its
output is a frozen `ComponentGraph`.

Use `GraphBuilder` for specification-driven wiring where callers provide source
nodes and required value metadata. It is not a component declaration API and
does not manage public port names, aliases, child boundaries, or assembly
phases.

## GraphAssembly

`GraphAssembly` is a declaration and lifecycle facade for one local component
graph. It manages:

- named field and geometry endpoints;
- aliases;
- local named connections;
- child export proxy bindings;
- advertisement and readiness phases;
- realization into `ComponentGraph`;
- publication of explicitly selected exports.

Its central question is:

> Given component-local declarations and child boundary ports, is this
> assembly ready, and what local graph should be built?

`GraphAssembly` resolves names and declarations first. It then creates graph
nodes and edges. Current implementation performs local declaration-time
resolution; future work may delegate specification adaptation to
`GraphBuilder`.

## GraphBuilder Versus GraphAssembly

These classes overlap in final graph construction but solve different problems:

| Concern | `GraphBuilder` | `GraphAssembly` |
| --- | --- | --- |
| Input | `NodeId` and `ItemSpec` | names, aliases, ports, child bindings |
| Main abstraction | representations and transforms | component-local declarations |
| Automatic adaptation | precision and units | currently limited; should use builder where needed |
| Child boundaries | not aware of them | explicit proxy and `PortId` bindings |
| Lifecycle | initialize, derive, freeze | declare, advertise, realize, build |
| Output | frozen `ComponentGraph` | frozen `ComponentGraph` plus assembly metadata |

Use `GraphAssembly` at component composition boundaries. Use `GraphBuilder`
inside graph construction when a required representation must be derived from
another representation.

## ComponentInterface

`ComponentInterface` is a public boundary description. It stores public import
and export `PublicPort` values. Each port contains:

- stable `PortId` scoped to the interface;
- canonical `VirtualConnectionPt` name and state intent;
- graph `ItemSpec` value metadata.

`VirtualConnectionPt` owns port identity metadata. `ItemSpec` owns value
metadata. Runtime `StateItemSpec` remains a state-registry payload and
allocation object; it is not the graph port record.

Direction semantics:

- import: component consumes an incoming representation;
- export: component produces a representation for consumers;
- internal: component-local point, never published through
  `ComponentInterface`.

The parent may copy a child `ComponentInterface` and create local proxy nodes,
but must not retain or traverse the child's `ComponentGraph`.

## Supporting Classes

`GraphNode` is the base for graph-owned nodes. `ValueGraphNode` represents a
concrete value or data representation. `TransformGraphNode` represents an
operation that derives output representations from inputs.

`GraphEdge` describes local directed topology. It never crosses component
boundaries.

`DependencyNetwork` selects graph-owned edges for one dependency or execution
view. It does not own edge objects.

`GraphValue` is the runtime payload abstraction for values stored by
`ValueGraphNode`. `GraphValueSpec` is the lightweight matching description
used by generic graph transforms; `ItemSpec` carries richer MAPL value metadata
for field-oriented graph construction.

`GraphAssemblyStatus` tracks assembly lifecycle state. It is not graph runtime
validity; `ComponentGraph` owns runtime node validity and topology freeze.

## Ownership Rules

- Outer component owns one local `ComponentGraph`.
- Graph owns nodes, edges, networks, and graph IDs.
- Assembly owns declarations and boundary binding records until realization.
- Builder owns derivation bookkeeping only until its result graph is built.
- Parent owns local child proxies, never child internal node IDs.
- Public interfaces expose ports, not graph storage.
