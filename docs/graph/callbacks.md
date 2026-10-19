# Callbacks

Source: `superstructure/generic/graph/{CallbackInterface,CallbackArgumentSpec,CallbackMethodSpec,AccessSpec,CallbackInterfaceRegistry,CallbackStateBinding,CallbackMethodBinding}.F90`,
containers `CallbackArgumentSpecMap`/`AccessSpecMap`/`CallbackMethodSpecMap`/
`CallbackInterfaceIdInterfaceMap`/`CallbackServiceNameIdMap`/
`CallbackMethodAttachmentMap`. Wiring lives in
`superstructure/generic/GraphBuilder_CallbackWiring_smod.F90`.

## No special node type

A callback State is an ordinary `StateItemNode` holding a `STATE`-kind
`GraphStateItem` — real ESMF State with attached methods via
`ESMF_MethodAdd`/`ESMF_MethodExecute`. No special `GraphNode` subclass.

## Concepts and where they live

| Concept | Module | Role |
|---|---|---|
| `CallbackInterface` | `CallbackInterface.F90` | reusable contract: argument name → `CallbackArgumentSpec`, method name → `CallbackMethodSpec`. Carries **no** service-name field — that's the registry's key, not a duplicated field. |
| `CallbackArgumentSpec` | `CallbackArgumentSpec.F90` | one declared argument of an interface |
| `CallbackMethodSpec` | `CallbackMethodSpec.F90` | one method's argument-access declarations: argument name → `AccessSpec`. Lets one argument shared across methods carry a different access mode per method (e.g. PassiveTracer's `tracers` is OUT in `get`, IN in `put`) |
| `AccessSpec` | `AccessSpec.F90` | general MAPL access-mode classification (not callback-specific) — introduced for `MethodGraphNode`'s own argument declarations, reused here unchanged |
| `CallbackInterfaceRegistry` | `CallbackInterfaceRegistry.F90` | the one shared, cross-`ComponentGraph` lookup for `CallbackInterface` values, keyed by service name / `CallbackInterfaceId` |
| `CallbackStateBinding` | `CallbackStateBinding.F90` | one callback State's realized contract: which interface, its `StateItemNode`'s `NodeId`, argument→member `NodeId` map, method→attachment map |
| `CallbackMethodBinding` | `CallbackMethodBinding.F90` | the method-level binding: invoked `MethodGraphNode`'s `NodeId`, plus **separate get/put `DependencyNetworkId`s** and their own argument→`NodeId` endpoint maps |

## CallbackInterfaceRegistry: singleton, hidden behind wrappers

A private module-level singleton (`CallbackInterfaceRegistry.F90:46`),
never exposed directly — only four public wrapper procedures:
`register_callback_interface`, `get_callback_interface`,
`get_callback_interface_by_name`, `lookup_callback_interface_id`.
Rationale: callback interfaces must be shared across every
`ComponentGraph` in a hierarchy; threading a pointer down from a root
owner is awkward, so a keyed/wrapped singleton is used instead (same
"narrower wrapper procedures hiding a singleton" idiom as
`HierarchyRegistry` — see `hierarchy-and-component-graph.md`).
`register_callback_interface` rejects a duplicate service name,
leaving the original registration unchanged.

## Why get/put need two networks, not one

A callback attachment commonly needs data flowing both ways relative
to the invocation: a `get` fills arguments from elsewhere before
calling, a `put` writes results somewhere after. `CallbackMethodBinding`
therefore carries **two independent `DependencyNetworkId`s** (get, put)
and two independent `StateItemMemberMap`s — not one map keyed by a
compound `(name, direction)` key. If an argument is only ever read (a
"get"-only argument) or only ever written (a "put"-only argument), the
unused network id/map is simply empty — no special-casing needed.

This get/put pattern is also the precedent that `transforms-and-update.md`'s
Q3 resolution (external, uniform port-binding storage) generalizes:
callback bindings were already external for exactly this reason
(one node routinely belongs to two networks); the resolution made
ordinary Transform port bindings follow the same external-storage shape
rather than special-casing the single-network case on-node.

## Handler / Invoker

- **Handler**: owns a callback State and its attached implementation.
- **Invoker**: calls an attached method on a callback State it does
  not own.

These terms are kept deliberately (see `open-questions.md`) because
"service provider" framing breaks on real examples: in one case (DYN
invoking Advection) the invoker does not own/provide the data; in
another (Aerosol) the same component both handles callbacks *and*
provides the science.

## Wildcard aggregation

A wildcard callback connection (one Invoker destination, N Handler
sources matched by **regex** against the flattened qualified-export
namespace — not glob; adopting existing legacy-application precedent)
is resolved entirely inside `GraphBuilder`
(`GraphBuilder_CallbackWiring_smod.F90`), not in the graph core: match
at wiring time (once sibling/descendant advertisement is complete),
validate each match against the named `CallbackInterface`, build the
destination collection State + `NamedAlias` members, register as a
normal `StateItemNode`, then wire ordinary per-member dependencies as
if each match had been named explicitly. The **collection** (not each
individual match) is what gets exposed as a public port, if visibility
further up the hierarchy is needed.

## Open items

See `open-questions.md`: Handler/Invoker term choice (low-stakes), and
distinguishing ESMF placement (`StateIntent`) from callback-role
semantics without overloading `StateIntent` itself.
