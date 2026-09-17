# 14. Route Handles

Status: `[SETTLED]` key structure and sharing model; `[OPEN]` renewal
mechanics under freeze (§14.4, shared with `13-geometry-and-vertical-grids.md`
§13.4).

## 14.1 RouteHandleValue

**REQ-RH-001.** `RouteHandleValue` represents a shared `ESMF_RouteHandle`
resource. Many `RegridTransform` nodes MAY reference the same
`RouteHandleValue` node.

## 14.2 RouteHandleKey

**REQ-RH-002.** A semantic `RouteHandleKey` MUST include at least:

- source geometry
- destination geometry
- regridding method
- masks
- extrapolation options
- normalization options
- other relevant ESMF settings

**REQ-RH-003.** The same geometry pair MAY legitimately require several
distinct `RouteHandle`s (e.g., linear vs. conservative) — `RouteHandleKey`
MUST be specific enough to distinguish these as different keys, not
collapse them.

## 14.3 Semantic index

**REQ-RH-004.** `ComponentGraph` MAY maintain a semantic index:

```
RouteHandleKey -> NodeId
```

**REQ-RH-005.** This index MUST only *locate* nodes for reuse; it MUST NOT
own them. Node ownership remains solely in the `NodeId → GraphNode` map
(REQ-CG-001). The index is a lookup accelerator, not a second source of
truth.

## 14.4 Time-dependent renewal `[OPEN]`

**REQ-RH-006 (goal, not yet fully mechanized).** Time-dependent geometry
SHOULD cause dependent `RouteHandle`s to be recreated or otherwise renewed
lazily.

Per `07-component-graph.md` REQ-CG-008, this renewal, if it happens after
graph freeze (the common case — geometry changing at runtime, well after
initialization), MUST be implemented as an in-place update to the existing
`RouteHandleValue` node (same `NodeId`, revision advanced), not as
insertion of a new node. If a given renewal scenario cannot be expressed
this way (e.g., the new `RouteHandle` genuinely needs new
`TransformGraphNode` dependency wiring that didn't exist before), that
scenario is **not currently supported** and must be raised as a design gap
before being implemented ad hoc. See `17-open-questions.md` (related
follow-up, not one of the original ten but flagged by this document).
