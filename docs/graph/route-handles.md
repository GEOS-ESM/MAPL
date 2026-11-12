# Route handles

Representation details inside `GraphStateItem.F90` (variant tagging —
see `graph-state-item.md`). There is no semantic route-handle key in the
graph layer; no code builds or looks up one.

## Representation: a persistent wrapper State, not a bare handle

A route handle is **not** a separate `GraphStateItem` component.
It's `esmf_state` allocated in a persistent **wrapper role**, holding
exactly one `ESMF_RouteHandle` member — distinguished from an ordinary
nested state via `variant() == MAPL_STATEITEM_ROUTEHANDLE`, checked
structurally (`state_is_route_handle_wrapper`), never via `ESMF_Info`
tagging. This is settled, not speculative — see `open-questions.md`
summary table ("RouteHandle representation").

Why "persistent wrapper State" rather than a bare `ESMF_RouteHandle`:
a route handle needs a stable `NodeId`/`StateItemNode` identity to
attach to, the same identity machinery every other node kind already
has.

## Open items

Time-dependent renewal (when geometry changes under a route handle
already in use) is not fully worked out — see `open-questions.md`.
