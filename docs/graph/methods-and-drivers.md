# Methods and drivers

Source: `superstructure/generic/graph/{MethodGraphNode,MethodInvocation,MethodInvocationAdapter,GridCompMethodInvocation,StateMethodInvocation,GridCompDriverPhaseInvoker,ArgumentSpec}.F90`,
containers `ArgumentSpecMap`.

See `node-hierarchy.md` for `MethodGraphNode`'s place in the node
hierarchy (never scheduled by `ComponentGraph%update()`).

## Why one node type for two different invocation kinds

A GridComp init/run phase and an attached ESMF State callback method
have different ESMF call signatures but the same graph-level shape:
named argument bindings in, invoke, done. `MethodGraphNode` represents
both through one node type and never branches on which — the branch
lives one layer down, in the adapter.

## MethodInvocationAdapter: the abstraction seam

`MethodInvocationAdapter.F90:1` — abstract, one deferred `invoke()`.
Two concrete subtypes:

| Adapter | Represents | Carries |
|---|---|---|
| `GridCompMethodInvocation` | GridComp phase | stable `driver_key` string (REQ-MTH-009: never a raw pointer to the driver) + `phase_idx` (integer, matching `GriddedComponentDriver%run/initialize/finalize`'s own signature — not a phase name) |
| `StateMethodInvocation` | Attached State callback method | callback State's `NodeId` + method name |

Each delegates to a small **injected** interface
(`GridCompPhaseInvoker`/`StateMethodInvoker`, declared in
`MethodInvocationAdapter.F90` itself) rather than calling anything
ESMF-specific directly. This satisfies REQ-MTH-003 ("must not
duplicate invocation logic already in `GriddedComponentDriver`") **by
construction** — a concrete adapter's `invoke()` does nothing but
gather bound arguments and call the one injected entry point.

If you ever find yourself adding ESMF-specific logic inside
`GridCompMethodInvocation`/`StateMethodInvocation` directly, that's a
signal invocation logic is leaking into the graph layer — push it into
`GriddedComponentDriver` or the callback attachment wrapper instead
(this was flagged explicitly as a watch-item, `open-questions.md` Q2).

## The real GridCompPhaseInvoker: GridCompDriverPhaseInvoker

`GridCompDriverPhaseInvoker.F90:1` — backed by the existing
`GriddedComponentDriver` (`superstructure/component/GriddedComponentDriver.F90`)'s
own `initialize`/`run`/`finalize` entry points. Holds:

- which one method (`ESMF_Method_Flag`, fixed at construction) this
  instance calls
- a `DriverResolver` turning a `driver_key` string into the concrete
  driver

`invoke_phase()` does exactly: resolve driver → dispatch to the one
call matching the held method flag → propagate `rc`. The clock is
**not** threaded through this call — `GriddedComponentDriver`'s own
clock member (set at construction/`set_clock`) is authoritative; a
second clock parameter here would be redundant.

The real `StateMethodInvoker` (ESMF_MethodExecute-backed) is a later
piece than the phase invoker above — check for its presence before
assuming callback method invocation is fully wired; `StateMethodInvocation.F90`'s
own header notes what has/hasn't landed as of its own change.

## Argument bindings: on-node, not external

Unlike `TransformGraphNode`'s port bindings (external
`PortBindingTable`, `transforms-and-update.md`), `MethodGraphNode`
stores its own named argument declarations (`ArgumentSpecMap`) and
bindings (`StateItemMemberMap`) **on-node**. Deliberate: there's no
shared traversal code (`ComponentGraph%update()`) that would need a
per-node-kind branch for binding storage, since `MethodGraphNode` is
permanently excluded from that traversal.
