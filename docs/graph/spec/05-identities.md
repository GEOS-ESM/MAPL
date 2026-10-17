# 5. Graph Identities

Status: `[SETTLED]`; §5.1/§5.2/§5.3 `[RE-EVALUATED]` per the landed
`hierarchy-wide-component-graph` change (`NodeId`/`DependencyNetworkId`
scope — see `openspec/specs/graph/identities/spec.md` for the
authoritative current requirements).

## 5.1 NodeId

**REQ-ID-001.** `NodeId` MUST be an encapsulated derived type wrapping a
private integer. No client code may construct a `NodeId` from a raw integer
or inspect its raw value.

**REQ-ID-002 `[re-evaluated]`.** The original wording — "fresh,
non-repeating `NodeId`s within a `ComponentGraph`'s lifetime" —
predates the landed `hierarchy-wide-component-graph` change and is
superseded. There is now exactly one `NodeIdGenerator` (and one
`DependencyNetworkIdGenerator`) instance for the entire component
hierarchy under a given root, owned by hierarchy-wide shared storage
(`HierarchyGraph`/`HierarchyResources`,
`superstructure/generic/graph/HierarchyGraph.F90`), not one per
`ComponentGraph` instance. Every `ComponentGraph`'s `next_node_id()`
delegates to that one shared generator, so no two components in the
same hierarchy can ever be issued the same `NodeId`. `NodeIdGenerator`
MUST still detect and report exhaustion (integer-kind overflow) rather
than silently wrapping or duplicating — this requirement is unchanged,
only its scope (hierarchy-wide, not per-`ComponentGraph`) is. See
`openspec/specs/graph/identities/spec.md` "NodeId generation is fresh
and non-repeating" for the current authoritative wording.
`PortIdGenerator` is NOT affected by this change — port identities
remain scoped to the single component that declares them, exactly as
before.

## 5.2 Sibling ID types

**REQ-ID-003 `[re-evaluated, extended]`.** The same pattern
(encapsulated ID + generator with exhaustion checking) MUST be used for
at least:

- `DependencyNetworkId`
- `PortId`
- `CallbackInterfaceId`
- `HierarchyId` (new, `hierarchy-wide-component-graph` change) — the
  identity type keying the private, module-level
  `HierarchyId → HierarchyResources` registry
  (`request_new_hierarchy()`/`get_hierarchy_resources(id)`/
  `release_hierarchy(id)`) that holds one hierarchy's own
  `HierarchyGraph`. A leaf value type with no dependency on
  `OuterMetaComponent` or `ComponentGraph`, generated from the same
  `IdTemplate.inc` template as every other id type in this list.

**REQ-ID-004.** A single CPP/FPP template MUST generate each ID type and its
generator, parameterized by type name. Do not hand-write near-duplicate ID
types.

**Reference implementation.** The template is provided at
`templates/IdTemplate.inc` and is authoritative within this MAPL-contained
specification corpus — implementers MUST
use it (or a direct evolution of it) rather than re-deriving an equivalent
template independently. Usage: define `ID_NAME` (and optionally
`DEFAULT_ID` if a default-valued instance is needed, as `ComponentGraph`'s
default `DependencyNetworkId` requires — REQ-CG-004) before `#include`-ing
the template; it generates:

- `type :: ID_NAME` — encapsulated, private integer value (`INT32`),
  `is_valid()`, `to_string()`
- `type :: <ID_NAME>Generator` — `next()` returns a fresh `ID_NAME` or
  reports exhaustion via an optional `status` argument (REQ-ID-002)
- `<ID_NAME>` parameter `INVALID_<ID_NAME>` — the distinguished invalid
  value
- `==`, `/=`, `<` operators

This satisfies REQ-ID-001/002/003 by construction for any ID type
generated from it.

**Resolved (was a flagged discrepancy).** REQ-ID-004 originally required
parameterization by "type name and integer kind," but the template fixes
the integer kind at `INT32` internally with no `ID_KIND` macro parameter.
Narrowed REQ-ID-004 to "type name" only: `INT32` is sufficient for every
current ID type (`NodeId`, `DependencyNetworkId`, `PortId`,
`CallbackInterfaceId`, `HierarchyId`) and no anticipated future one is
expected to need a wider range before exhaustion (REQ-ID-002) would
already indicate a different underlying problem. If a genuinely
different-width ID type is ever needed, add an `ID_KIND` macro
parameter to the template at that point rather than pre-generalizing
now.

## 5.3 NodeId as authoritative key

**REQ-ID-005 `[re-evaluated]`.** The original wording — "`NodeId` is
the authoritative key in the `ComponentGraph`'s `NodeId → GraphNode`
map" — is superseded in one respect: the `NodeId → GraphNode` map is no
longer owned per-`ComponentGraph` (§5.1); it is the one hierarchy-wide
map inside `HierarchyGraph`, which every `ComponentGraph` instance in
the hierarchy references via its own stored `HierarchyId`. `NodeId`
remains the authoritative lookup/membership key for that map — only
*which* map (shared, not per-instance) changed. A `ComponentGraph`
instance additionally keeps its own explicit, private
`NodeIdSet`/`DependencyNetworkIdSet` recording which ids *it itself*
registered (`owned_node_ids()`); this ownership-tracking set is
forward-only ("which ids are mine"), not a second index into the
shared map, and is what `add_dependency`/`validate`/`bind_port`/
`add_import_port` consult to decide whether an id is this instance's
own versus a legitimate foreign id elsewhere in the hierarchy
(`allowed_foreign_node`, checked via the shared map's own `get_node(id)`
being associated — never via a hierarchy-wide reverse lookup from id to
owning component; see
`openspec/changes/hierarchy-wide-component-graph/design.md` D2 for the
full reasoning, including a documented, rejected "reverse map"
alternative).

**Amended.** This section originally continued: "A `GraphNode` (including
`StateItemNode`) is not required to store its own `NodeId` internally;
identity is established by map membership, not by a field on the node."
That position is reversed — see `03-graph-node-hierarchy.md` REQ-NODE-002a:
`BaseGraphNode` now MUST store its own `NodeId`. The map is still
authoritative (a node cannot make itself a member by claiming an ID; only
insertion into the map does that), and the node's self-reported `NodeId`
MUST always agree with the key it is stored under — it is a convenience
cache of the same fact, not a second source of truth permitted to diverge.

## 5.4 Container strategy

**REQ-ID-006.** The graph MUST use gFTL containers that support polymorphic
mapped values directly. Separate "Box" wrapper types to smuggle polymorphic
values through non-polymorphic containers are unnecessary and MUST NOT be
introduced.
