# 5. Graph Identities

Status: `[SETTLED]`

## 5.1 NodeId

**REQ-ID-001.** `NodeId` MUST be an encapsulated derived type wrapping a
private integer. No client code may construct a `NodeId` from a raw integer
or inspect its raw value.

**REQ-ID-002.** `NodeIdGenerator` MUST generate fresh, non-repeating
`NodeId`s within a `ComponentGraph`'s lifetime, and MUST detect and report
exhaustion (integer-kind overflow) rather than silently wrapping or
duplicating.

## 5.2 Sibling ID types

**REQ-ID-003.** The same pattern (encapsulated ID + generator with
exhaustion checking) MUST be used for at least:

- `DependencyNetworkId`
- `PortId`
- `CallbackInterfaceId`

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
`CallbackInterfaceId`) and no anticipated future one is expected to need a
wider range before exhaustion (REQ-ID-002) would already indicate a
different underlying problem. If a genuinely different-width ID type is
ever needed, add an `ID_KIND` macro parameter to the template at that
point rather than pre-generalizing now.

## 5.3 NodeId as authoritative key

**REQ-ID-005.** `NodeId` is the authoritative key in the `ComponentGraph`'s
`NodeId → GraphNode` map for lookup and membership purposes.

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
