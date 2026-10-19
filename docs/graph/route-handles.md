# Route handles

Source: `superstructure/generic/graph/RouteHandleKey.F90` (382 lines).
Representation details inside `GraphStateItem.F90` (variant tagging —
see `graph-state-item.md`).

## Representation: a persistent wrapper State, not a bare handle

A route handle is **not** a separate `GraphStateItem` component.
It's `esmf_state` allocated in a persistent **wrapper role**, holding
exactly one `ESMF_RouteHandle` member — distinguished from an ordinary
nested state via `variant() == MAPL_STATEITEM_ROUTEHANDLE`, checked
structurally (`state_is_route_handle_wrapper`), never via `ESMF_Info`
tagging. This is settled, not speculative — see `open-questions.md`
summary table ("RouteHandle representation").

Why "persistent wrapper State" rather than a bare `ESMF_RouteHandle`:
sharing/renewal/comparison all key off `RouteHandleKey` equality (below)
and need a stable `NodeId`/`StateItemNode` identity to attach to, the
same identity machinery every other node kind already has.

## RouteHandleKey: the sharing/reuse key

`RouteHandleKey.F90:12`. Immutable value type, semantically identifies
"this exact regrid/redist configuration," so two independently
requested route handles that are actually identical reuse one
`StateItemNode` instead of allocating twice. Carries:

- `source_geometry`/`destination_geometry` — **`NodeId`s**, not raw
  geometry values (referencing the geometry `StateItemNode`s directly —
  consistent with geometry being an ordinary node, see
  `graph-state-item.md`/`characteristics.md`)
- `regridmethod` (`ESMF_RegridMethod_Flag`)
- `srcMaskValues`/`dstMaskValues` (allocatable integer arrays)
- `extrapmethod`, `extrapNumSrcPnts`, `extrapDistExponent`,
  `extrapNumLevels` (allocatable — legitimately absent)
- `normtype`, `polemethod`, `regridPoleNPnts` (allocatable),
  `linetype`, `unmappedaction`, `ignoreDegenerate`

Equality (`operator(==)`) is defined via `to_string()` comparison
(`:302`) — two keys are equal iff their rendered strings match, which
also makes `to_string()` double as the reuse-lookup/cache key, not
just a diagnostic.

### A real portability gotcha, if you touch this file

Every per-field `*_code()` helper (`regrid_code`, etc., `:70` onward)
**must leave its allocatable-character result allocated on every
branch, including the failing one** — the `_FAIL`/error-macro expansion
assigns into the caller's variable *before* the subsequent status
check runs. Leaving the result unallocated on a failing branch is
non-conforming and has caused a real SIGSEGV on Intel ifort (reading
garbage length metadata) while gfortran/NAG silently tolerate it. Every
branch pre-sets `code = ''` up front specifically to guard against this
— preserve that pattern if you add a new flag-to-string helper here.

## Open items

Time-dependent renewal (when geometry changes under a route handle
already in use) is not fully worked out — see `open-questions.md`.
