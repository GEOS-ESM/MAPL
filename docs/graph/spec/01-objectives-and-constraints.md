# 1. Objectives and Constraints

Status: `[SETTLED]`

## 1.1 Motivation

MAPL3 constructs automatic couplers between component import and export
state items. A mismatch in grid, precision, units, or other specification
causes MAPL to insert one or more extension items and associated Transforms.
This works, but the dependency graph it implies exists only implicitly, as
side effects of imperative coupler-construction code.

**REQ-OBJ-001.** The architecture MUST make this dependency graph explicit
(nodes + dependency edges), so that it can support:

- Existing automatic couplers and extension reuse (see `09-extension-reuse.md`)
- Multiple-input and multiple-output Transforms (`10-transforms-and-ports.md`)
- Callback states and attached ESMF methods (`15-callbacks.md`)
- Time-dependent geometries and vertical grids (`13-geometry-and-vertical-grids.md`)
- Shared RouteHandles and other resources (`14-route-handles.md`)
- Lazy, revision-based transform execution (`11-revision-and-update.md`)
- Potentially modifiable or inout items (`16-inout-items.md`, speculative)
- Better validation, diagnostics, and future optimization

**REQ-OBJ-002.** The new architecture MUST preserve existing MAPL coupler
behavior as observed by user components; it is an internal restructuring,
not a user-visible behavior change, except where explicitly extending
capability (multi-I/O transforms, callbacks, etc.).

## 1.2 ESMF Structural Constraints

These are external facts about ESMF that the design must respect; they are
not design choices.

**REQ-ESMF-001.** ESMF import/export States MAY only directly contain ESMF
objects: `Field`, `FieldBundle`, `State`, `RouteHandle`. They cannot directly
contain arbitrary MAPL graph objects (`StateItemNode`, `GraphValue`, etc.)
or general geometry objects (`Grid`, `Mesh`, `LocStream` as bare objects).

**REQ-ESMF-002.** `ESMF_Field`, `ESMF_FieldBundle`, and `ESMF_State` are
shallow handles. Copying a handle does not copy underlying data.

**REQ-ESMF-003.** `ESMF_NamedAlias` creates a distinct ESMF handle (different
name, different reference ID) that shares underlying data and Info with the
original. Graph design MUST treat named aliases as sharing identity at the
`StateItemNode` level even though they are distinct ESMF-level handles (see
`04-graph-value-hierarchy.md` §4.3, `11-revision-and-update.md` §11.2).

**REQ-ESMF-004.** MAPL metadata is stored in a private namespace within
ESMF Info and accessed only through MAPL wrappers. Graph code MUST NOT read
or write this namespace directly; it MUST go through the MAPL Info wrapper
API.

**REQ-ESMF-005.** A `Grid`, `Mesh`, `LocStream`, or other geometry object
that must be visible inside an ESMF State MUST be represented through a
proxy `Field` (a "geometry proxy field"), because ESMF States cannot hold
geometry objects directly. See `13-geometry-and-vertical-grids.md`.

## 1.3 Consequence for the graph/ESMF boundary

**REQ-ESMF-006.** Every `GraphValue` that must ultimately be visible to a
user or framework ESMF State MUST have a well-defined ESMF representation
(Field, FieldBundle, State, RouteHandle, or a proxy Field for geometry
types). The graph MUST NOT create graph-only values with no eventual ESMF
materialization path if those values are expected to reach a State (see
`09-extension-reuse.md` REQ-EXT-004, "the graph must not create invisible
ESMF payloads").
