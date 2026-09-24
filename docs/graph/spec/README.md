# MAPL3 Graph Architecture Specification — v1.3

**Status:** Draft 1.2 (design snapshot, pre-implementation)
**Purpose:** Formal specification of the explicit dependency-graph architecture
for MAPL3, suitable as input to implementation agents and as the reference
baseline for future design and code review.

## Scope

MAPL3 builds automatic couplers (Transforms) between component import/export
state items implicitly. This specification defines an explicit graph
architecture — nodes, values, dependency networks, and a builder layer — that
makes that mechanism first-class, and extends it to support multi-input/output
transforms, callbacks, time-dependent geometry, shared resources (route
handles), lazy revision-based execution, and eventual inout support.

This is a **snapshot**, not a finished design. Several areas are explicitly
marked open (see `17-open-questions.md`). Implementers must not silently
resolve open items; they must be escalated or resolved through an explicit
design decision that amends this document.

## Conventions

- **MUST / SHALL** — mandatory; non-conformance is a defect.
- **SHOULD** — strong recommendation; deviation requires recorded justification.
- **MAY** — permitted, not required.
- **Status tags**, applied per section or per requirement:
  - `[SETTLED]` — decided. Implement as specified.
  - `[SPECULATIVE]` — direction is believed correct but details are
    unconfirmed. Implement behind a seam (abstract interface, isolated
    module) that can change without disturbing settled clients.
  - `[OPEN]` — unresolved. Requires a design decision before implementation.
    Cross-referenced to `17-open-questions.md`.
- **Requirement IDs** — `REQ-<AREA>-<NNN>`, stable once assigned. Never
  renumber; if a requirement is retired, mark it `RETIRED` and keep the ID
  out of circulation.

## Document map

| File | Contents |
|---|---|
| `01-objectives-and-constraints.md` | Motivation; ESMF structural constraints the design must respect |
| `02-component-hierarchy.md` | OuterComponent, ComponentGraph ownership, encapsulation rules |
| `03-graph-node-hierarchy.md` | GraphNode / BaseGraphNode / StateItemNode / OperationGraphNode / TransformGraphNode / MethodGraphNode |
| `04-graph-value-hierarchy.md` | GraphValue payload hierarchy (Field, FieldBundle, State, Grid, ... ) |
| `05-identities.md` | NodeId and sibling ID types; generator template |
| `06-dependency-network.md` | Adjacency-list dependency storage, acyclicity, validation |
| `07-component-graph.md` | ComponentGraph ownership, lifecycle, freezing |
| `08-graph-builder.md` | GraphBuilder integration layer and responsibilities |
| `09-extension-reuse.md` | Backward-compatible automatic-coupler / extension-reuse behavior |
| `10-transforms-and-ports.md` | TransformGraphNode, named ports, argument specs |
| `11-revision-and-update.md` | NodeRevision, demand-driven update algorithm, runtime-vs-compiled execution |
| `12-methods-and-drivers.md` | MethodGraphNode, invocation adapters, GriddedComponentDriver, SetServices lifecycle |
| `13-geometry-and-vertical-grids.md` | Geometry as GraphValue, vertical grid representation |
| `14-route-handles.md` | RouteHandleValue, RouteHandleKey, sharing and renewal |
| `15-callbacks.md` | CallbackInterface model, registry, Handler/Invoker roles, wildcard aggregation |
| `16-inout-items.md` | Forward/return network model for borrowed and inout items (speculative) |
| `17-open-questions.md` | The original ten open design questions plus later additions (Q11–Q14), current recommendation, and status |
| `18-state-item-characteristics.md` | `StateItemCharacteristic` hierarchy (Value/Reference split), `CharacteristicStatus`, `CharacteristicType`, sharing, and the eager-structural/lazy-content propagation split (speculative) |
| `19-visualization-export.md` | Runtime export of graph topology (nodes, dependencies, ports, revisions) to DOT/JSON for rendering by external, non-Fortran tools (speculative) |
| `20-implementation-roadmap.md` | Phased implementation plan (core-vs-MAPL-integration split), repo strategy rationale, and readiness assessment against open questions |
| `templates/IdTemplate.inc` | Authoritative CPP/FPP template for `NodeId` and sibling ID types (`05-identities.md` REQ-ID-004) |

## Glossary

- **StateItem** — *informal* sense only, used elsewhere in MAPL/ESMF
  documentation and prose: a named member of an ESMF State (Field,
  FieldBundle, nested State, ...) as tracked by MAPL today, made
  graph-visible by being backed by a `StateItemNode`. Not a type name in
  this specification or its implementation.
- **GraphStateItem** — the *formal* type (`04-graph-value-hierarchy.md`
  §4.6, `[SETTLED]`) — a concrete, non-polymorphic derived type with
  allocatable `esmf_field`/`esmf_field_bundle`/`esmf_state` components
  (at most one allocated; a route handle is `esmf_state` in a wrapper
  role, not a separate component), the payload of `StateItemNode`. This
  fully supersedes `GraphValue` (below) for these kinds — see
  `17-open-questions.md` Q11 (fully resolved). Named `GraphStateItem`
  rather than the more obvious `StateItem` specifically to avoid
  colliding with MAPL's own, unrelated `mapl_StateItem_mod`
  (`ESMF_StateItem_Flag`-based classification constants) once this
  code is integrated into MAPL proper (`20-implementation-roadmap.md`
  §20.4.1) — discovered only once real MAPL source was available to
  check against, recorded here so it is not "helpfully" reverted later.
- **GraphNode** — abstract, data-free interface at the root of the node
  hierarchy.
- **StateItemNode** — concrete node (renamed from `ValueGraphNode`) holding
  one payload (originally "one polymorphic `GraphValue`"; possibly amended
  to "one concrete `GraphStateItem`," see above) and one `NodeRevision`.
- **GraphValue** — polymorphic payload (Field, FieldBundle, State, Grid,
  Mesh, LocStream, VerticalGrid, RouteHandle, ...) held by a StateItemNode
  under the original (§4.1–§4.5) design. Graph-neutral: no dependency on
  Graph/ComponentGraph/GraphBuilder.
- **StateItemCharacteristic** — (`18-state-item-characteristics.md`,
  `[SPECULATIVE]`) an atomic, independently-adaptable aspect of a
  `GraphStateItem` (e.g. physical units, type/kind, geometry), each reconciled
  by an associated Transform (`ConvertUnitsTransform`, `CopyTransform`,
  `RegridTransform`).
- **CharacteristicStatus** — (working name, `[OPEN]`) status of a
  `StateItemCharacteristic`: `INVALID`, `SPECIFIED`, `MIRRORED`,
  `UNCHECKED`, or `DEFERRED`.
- **CharacteristicType** — (working name, `[OPEN]`) map key type with one
  unique value per `StateItemCharacteristic` subclass.
- **NodeId** — encapsulated, generated identity; authoritative map key.
- **DependencyNetwork** — one acyclic adjacency-list view over a subset of a
  ComponentGraph's nodes.
- **ComponentGraph** — per-OuterComponent owner of nodes, networks, ID
  generators, ports, and semantic indexes.
- **GraphBuilder** — integration layer that connects ComponentGraph to the
  component hierarchy, StateRegistry, and connection-point resolution.
  ComponentGraph does not depend on GraphBuilder or anything above it.
- **OuterComponent** — an ESMF GridComp that wraps a user component. Owning
  a GriddedComponentDriver, child drivers, a local ComponentGraph, and
  framework-managed states.
- **OuterMetaComponent** — the derived type holding `OuterComponent`'s
  private state (the actual owner of the properties listed above). `Outer
  Component` and `OuterMetaComponent` are sometimes used interchangeably in
  this document's discussion sections (a holdover from early
  shorthand) — where the distinction matters (e.g.
  `11-revision-and-update.md` REQ-REV-011a, `12-methods-and-drivers.md`
  REQ-MTH-003a), `OuterMetaComponent` specifically means the private-state
  type that invokes methods on the wrapped user component.
- **GriddedComponentDriver** — wraps an ESMF GridComp + import/export/
  internal states + clock; exposes high-level calls.
- **CallbackInterface** — reusable named contract (arguments + per-method
  access) for a callback State.
- **Handler** — owns a callback State and its attached implementation.
- **Invoker** — calls an attached method on a callback State it does not own.
- **RouteHandleKey** — semantic key (geometries, method, masks, ...)
  identifying a reusable RouteHandleValue.
- **NodeRevision** — 64-bit monotonically-advancing version stamp on a
  StateItemNode's logical value.
