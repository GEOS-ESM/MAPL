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
| `CallbackMethodBinding` | `CallbackMethodBinding.F90` | the method-level binding: invoked `MethodGraphNode`'s `NodeId`, the **put** `DependencyNetworkId` only (get edges live in the hierarchy's forward network, so no get network id is stored), and separate get and put argument→`NodeId` endpoint maps (`CallbackMethodBinding.F90:11-19,43-46`) |

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

## Get and put: forward network plus a put network per attachment

A callback attachment commonly needs data flowing both ways relative
to the invocation: a `get` fills arguments from elsewhere before
calling, a `put` writes results somewhere after. The two directions
use different networks:

- **get** edges carry data toward the method before it runs, so they
  join the hierarchy's one forward network
  (`graph%get_forward_network_id()`, `GraphBuilder_CallbackWiring_smod.F90:382-384`).
  `invoke_callback_method` pulls the get arguments through that network.
- **put** edges are the method's write-back: each attachment creates its
  own put network (`graph%create_network()`, `:385`), and
  `CallbackMethodBinding` stores only that put `DependencyNetworkId`.
  Put targets are advanced directly by `NodeId` after the method runs
  (`:476-508`).

`CallbackMethodBinding` keeps two independent `StateItemMemberMap`s
(get, put) — not one map keyed by a compound `(name, direction)` key. If
an argument is only ever read (a "get"-only argument) or only ever
written (a "put"-only argument), the unused map is simply empty — no
special-casing needed.

A write-back network is owned by the `ComponentGraph` that created it
(`own_network_ids`, `ComponentGraph.F90:96-100`); the forward network
belongs to the hierarchy (`HierarchyGraph.F90:14-20`). Port bindings
for a Transform are keyed by `(DependencyNetworkId, NodeId)`, so the
same node can still be bound in the forward network and in a put
network (`transforms-and-update.md`).

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
