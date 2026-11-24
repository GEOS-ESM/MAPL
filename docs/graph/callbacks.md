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

A literal `src_name` (no regex metacharacters) aggregates too, by a
different rule, **through re-export, not a subtree search**: "same-named
Handlers merge through re-export" (design.md decision 21) — a bare
connection name means an export of `src_comp` itself, as in legacy, with
re-export staying the sole control over what a parent makes visible at
each level. For a literal `src_name` on a callback-interface import, the
candidates are exactly the exports *of `src_comp` itself* under that
name:
- one of `src_comp`'s own direct exports (not a descendant's) whose
  *declared callback name* (`VariableSpec%get_callback_declared_name()`
  — its own `short_name`, unless a template expansion sets
  `callback_declared_name` to a different name all its instances share)
  equals `src_name`, in addition to the exact-qualified-name match (so an
  undeclared Handler at `src_comp` itself, matched by shape alone, is
  still a candidate);
- any export, at any depth, whose real `NodeId` is registered under a
  port named `src_name` in `src_comp`'s own export-port table — i.e.
  every source a `ReexportConnection` has merged under that name.
  `reexport_one` (`GraphBuilder_ConnectionResolution_smod.F90`) normally
  fails when a re-export's destination name is already taken by a
  different node ("Re-export destination already exists"); when both the
  already-taken export and the new source are callback exports that
  declare the *same* `callback_interface_id`, it adds the new source as a
  second export port under that name instead of failing. A name merged
  this way and then re-exported one level further carries every source
  along (`reexport_one` collects every node registered under the source
  name, not just the first).

There is no search of `src_comp`'s descendants by bare name: a
descendant's export reaches a plain `src_name` only via an explicit
re-export. An undeclared look-alike below `src_comp` is reachable only
by an explicit path or a regex. A **plain (unqualified)** `src_name`
with zero matches *and* zero rejected candidates — no Handler of
`src_comp` itself under that plain name, whether its own export or
merged in by re-export; a Handler that exists somewhere below
`src_comp` but was never re-exported is indistinguishable, from
`src_comp`'s own export-port table, from one that does not exist at
all — resolves to an empty collection (the Invoker loops zero times
over it), rather than staying an unresolved import. If a candidate
existed and was rejected (wrong interface, or declared but
non-conforming), **or if `src_name` is an explicit `/`-qualified
path** (even one with zero matches and zero rejected candidates), the
import stays unresolved, the same way a regex `src_name`'s zero-match
case always stays unresolved (bubbling either up to the parent is a
plausible fix) — an explicit path that resolves to nothing is a typo
or a stale reference, not "no Handler". Implemented in
`resolve_callback_import` (`GraphBuilder.F90`); the resolved-vs-unresolved
exception is one shared predicate, `callback_zero_match_is_empty`, used
by both
`check_one`/`connection_pt_pair_resolves`
(`GraphBuilder_ConnectionResolution_smod.F90`).

## Invoker provider declarations and production binding

An Invoker's own callback IMPORT (the one declaring
`callback_interface:`) may carry two optional YAML maps, parsed into
`VariableSpec%callback_provides`/`callback_receives`
(`VariableSpec.F90`, `StringStringMap` of interface argument name →
the Invoker's own item short name):

- `provides: {<argument>: <own item>}` — the Invoker's own item that
  feeds an IN/INOUT argument (`MAPL_ACCESS_IN`/`MAPL_ACCESS_INOUT`) on a
  bound method.
- `receives: {<argument>: <own item>}` — the Invoker's own item that an
  OUT/INOUT argument (`MAPL_ACCESS_OUT`/`MAPL_ACCESS_INOUT`) writes
  into.

`parse_callback_provider_map` (`ComponentSpecParser/parse_var_specs.F90`)
rejects either key anywhere except an IMPORT declaring
`callback_interface:`, and rejects an argument name the named
interface does not declare; it does not check that the named item
exists — that is resolved later, against the Invoker's own
`ComponentGraph`.

`resolve_callback_destination` (`GraphBuilder_ConnectionResolution_smod.F90`,
CONNECT phase, same pass as ordinary connection resolution) is the
production call site for `build_callback_state_binding`/
`build_callback_method_binding`, run once per match against the
Invoker's wildcard collection. `build_callback_provider_maps` resolves
each `provides:`/`receives:` entry's item name to a `NodeId` among the
Invoker's own items (`resolve_invoker_item_id`, trying IMPORT, EXPORT,
then INTERNAL by short name) once per Invoker IMPORT, and
`build_callback_bindings_for_match` builds one `CallbackStateBinding`
plus one `CallbackMethodBinding` per interface method for every matched
member, binding every method the interface declares (not just the ones
a particular `provides:`/`receives:` map happens to cover — a method
whose required argument has no provider fails
`build_callback_method_binding` with a named error, rather than binding
a partial method).

The resulting bindings are stored in a new `CallbackMethodBindingMap`
(`graph/containers/CallbackMethodBindingMap.F90`) field,
`callback_method_bindings`, on the collection's own `StateItemNode`
(not on each member, and not on the `CallbackStateBinding`'s own
`NodeId`), keyed by
`callback_method_binding_key(member_qualified_name, method_name)` —
compound string key, same keying style as `GraphBuilder.F90`'s
`item_key()`. This is where an Invoker's own method-invocation code is
expected to look up a matched member's bound methods.

A Handler member wired as a `get`-direction destination this way needs
its own realized Field, at REALIZE_ACCEPTED, to actually *be* the
extension chain's output rather than an independent Field of its own:
`realize_callback_member` (`GraphBuilder_CallbackRealization_smod.F90`)
checks the hierarchy's forward network for a predecessor of the
member's `NodeId` (`find_materialized_predecessor`, the same walk
`graphbuilder_populate_state`'s `populate_one` already uses for an
ordinary mismatched IMPORT), otherwise creating a brand-new
Field/FieldBundle for the member.

**Known gap (reported, not fixed by this binding work):** nothing
actually moves data in the put/`receives:` direction yet.
`advance_callback_put_arguments` only advances each put argument's
`NodeRevision` (bookkeeping), and `ComponentGraph%update()` only walks
the hierarchy's one forward network
(`ComponentGraph_DemandDrivenUpdate.F90`) — a put network is never
pulled. The `provides:`/get direction *does* move real data, the same
way an ordinary mismatched connection does
(`find_or_build_extension_chain`/`materialize_extension_chain`), because
get edges join the forward network and `invoke_callback_method` pulls
it before calling. Until a put-network execution path exists (or some
other design choice replaces it), an OUT/INOUT argument's `receives:`
item will not reflect what the method wrote — note this is the
argument's access direction, not the method's name: a method named
`put` can have an IN argument (whose value *does* reach it, via
`provides:`), while a method named `get` can have the OUT argument
that writes to a `receives:` item.

## Open items

See `open-questions.md`: Handler/Invoker term choice (low-stakes), and
distinguishing ESMF placement (`StateIntent`) from callback-role
semantics without overloading `StateIntent` itself.
