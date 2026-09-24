## Context

See proposal.md - Why/What Changes for motivation and scope. Relevant
existing state this design builds on:

- `AccessSpec` (`AccessSpec.F90`) — small value type, `IN`/`OUT`/
  `INOUT`/`UNSPECIFIED`, `operator(==)`/`operator(/=)`, `to_string()`.
  Already general-purpose (REQ-CB-005), introduced in Phase 4a
  specifically so this change could reuse it.
- `CallbackInterfaceId` (`CallbackInterfaceId.F90`) — empty
  `IdTemplate.inc`-generated identity type, already provides
  `CallbackInterfaceIdGenerator`, `INVALID_CallbackInterfaceId`,
  `operator(==)`/`operator(/=)`/`operator(<)`, `to_string()` for free.
- `StateItemMemberMap` (`containers/StateItemMemberMap.F90`) — gFTL
  `character -> NodeId` map, already used by `GraphStateItem` (REQ-SI-006)
  and `MethodGraphNode`'s own argument bindings.
- `StateMethodInvocation` (`StateMethodInvocation.F90`) — concrete
  `MethodInvocationAdapter` carrying `state_node_id` + `method_name` +
  an optional injected `StateMethodInvoker`; `invoke()` fails loudly if
  no invoker is attached. Phase 4a's own header comment already flags
  this change as the point where its real ESMF-backed invoker becomes
  meaningful to construct (though the real invoker implementation
  itself remains Phase 4d's job, see Non-Goals).
- `ArgumentSpec`/`ArgumentSpecMap`/`MethodGraphNode`'s
  declare/bind pattern (`ArgumentSpec.F90`, `MethodGraphNode.F90`) — the
  established style this change's `CallbackArgumentSpec`/
  `CallbackInterface`/`CallbackStateBinding` APIs mirror: gFTL map keyed
  by name, `%at(key)` pointer access for in-place mutation, no-silent-
  replace on duplicate declaration, `_ASSERT`/`_RC` error-handling
  macros throughout.
- `ExtensionResolution.F90`'s `materialize_extensions` module-level
  `logical, save` flag + `set_`/`_enabled()` accessor pair — the only
  existing module-level-singleton-state precedent in this module
  family, generalized here from one flag to `CallbackInterfaceRegistry`'s
  full owned state.

## Goals / Non-Goals

**Goals:**
- Provide a complete, registrable `CallbackInterface` data model
  (REQ-CB-002/003/004) sufficient to express `15-callbacks.md` §15.3's
  `PassiveTracer` example exactly.
- Provide `CallbackInterfaceRegistry` as the one shared, cross-
  `ComponentGraph` lookup point for interfaces (REQ-CB-008/009/010).
- Provide `CallbackStateBinding` as a real, validated (against a
  registered `CallbackInterface`) association between one callback
  State's `NodeId`, its declared argument member `NodeId`s, and its
  per-method invocation adapters (REQ-CB-007).
- Keep every new type synthetic-graph testable: no `ComponentGraph`,
  `GraphBuilder`, `OuterMetaComponent`, or real ESMF component/state
  required to construct or exercise any type introduced here.

**Non-Goals:**
- Resolving member `NodeId`s from a real composite callback State's
  declared member tree (`composite-state-spec`'s `GraphBuilder`
  recursion) — `CallbackStateBinding` accepts caller-supplied `NodeId`s.
  Wiring a real advertised composite State into a real
  `CallbackStateBinding` is Phase 4d's job.
- A real `ESMF_MethodAdd`/`ESMF_MethodExecute`-backed `StateMethodInvoker`
  — this change constructs `StateMethodInvocation` adapters with no
  invoker attached (or, in tests, a synthetic test-double invoker,
  reusing Phase 4a's existing `Test_StateMethodInvocation.pf` fixture
  pattern), matching `StateMethodInvocation.F90`'s own header comment
  that the real invoker is Phase 4c/4d's job — this change supplies the
  *binding* structure; Phase 4d supplies the real invoker and the
  `GraphBuilder` wiring that attaches it.
- Wildcard/regex expansion, per-method `DependencyNetwork`s, invoke-once
  discipline (§15.9-15.10) — Phase 4d, per proposal.md's explicit
  deferrals.
- Resolving REQ-CB-005/006's `[OPEN]` Handler/Invoker terminology
  question (Q6) or REQ-CB-011/012's `[OPEN]` ESMF-placement question
  (Q7) — this change's API surface is agnostic to both; neither question
  constrains any decision below.

## Decisions

**1. `CallbackArgumentSpec` carries no `AccessSpec` field.**
REQ-CB-004 is explicit that access is per-*method*, not per-argument
("allowing one shared argument to have different access in different
methods"). Putting `AccessSpec` on `CallbackArgumentSpec` (mirroring
`ArgumentSpec` exactly) would let it silently disagree with a
`CallbackMethodSpec`'s own per-method access, inventing a second, unused
source of truth. `CallbackArgumentSpec` therefore holds only `name` +
`expected_kind : MAPL_StateItem_Flag` (required, not optional —
REQ-CB-003's own `PassiveTracer` example always states a kind, "tracers,
ESMF FieldBundle"; unlike `ArgumentSpec`'s optional
`expected_kind`, there is no unconstrained-callback-argument use case in
the spec to support). *Alternative considered:* reuse `ArgumentSpec`
directly instead of introducing `CallbackArgumentSpec`. Rejected:
`ArgumentSpec`'s `AccessSpec` field would be meaningless/unused dead
weight on every callback argument declaration, and would invite exactly
the two-sources-of-truth bug this decision avoids.

**2. `CallbackMethodSpec` is a thin wrapper around a new `AccessSpecMap`
(character -> `AccessSpec`), not a bespoke map.**
Mirrors `ArgumentSpecMap`'s own "wrap the gFTL map in its own module"
pattern (`containers/ArgumentSpecMap.F90`'s header: gFTL's
`map/template.inc` emits its own `implicit none`/`private` that
conflicts with a preceding one). `AccessSpecMap` is introduced as a
standalone, reusable container (not nested inside
`CallbackMethodSpec.F90`) since it is a generic `character -> AccessSpec`
map with no callback-specific meaning of its own — matching
`AccessSpec`'s own "general MAPL concept" framing (REQ-CB-005).
*Alternative considered:* give `CallbackMethodSpec` a private
`type(ArgumentSpecMap)` reusing the existing name-keyed map machinery
wholesale (its `T` already carries an `AccessSpec` internally).
Rejected: `ArgumentSpec`'s `AccessSpec` accessor would be the only field
of `ArgumentSpec` ever read, and the unused `kind_constrained`/
`expected_kind` fields would be silently meaningless on every entry —
same objection as Decision 1, with a bigger dead-weight type (`ArgumentSpec`
vs. a bare `AccessSpec`).

**3. `CallbackInterface` builds `CallbackMethodSpec` entries via
`%at()` pointer mutation into its own `CallbackMethodSpecMap`, exactly
like `MethodGraphNode`'s own `get_argument`/`bind_argument` accesses
its `ArgumentSpecMap`.**
`declare_method(name, rc)` inserts an empty `CallbackMethodSpec` under
`name`; `set_method_argument_access(method_name, argument_name, access,
rc)` fetches `spec_ptr => this%methods%at(method_name)`, asserts
`this%is_argument(argument_name)` (validates against the *interface's*
own argument declarations — the cross-reference REQ-CB-004's shared-
argument model requires), then calls
`spec_ptr%declare_argument_access(argument_name, access, rc)` on the
map's own stored value in place. This is the same "declare then mutate
via `%at()` pointer" idiom already established by
`MethodGraphNode.F90`'s `node_declare_argument`/`node_bind_argument`,
applied one level deeper (map-of-map-like-thing rather than map-of-
value). *Alternative considered:* have `CallbackInterface` own a flat
`(method_name, argument_name) -> AccessSpec` map instead of nesting
`CallbackMethodSpec` per method. Rejected: REQ-CB-003 states the
contract explicitly as two separate maps (`argument name ->
CallbackArgumentSpec`, `method name -> CallbackMethodSpec`) — flattening
would satisfy the same queries but diverge from the spec's own stated
shape for no benefit, and would lose "one `CallbackMethodSpec` value
object representing one whole method" as a reusable, independently
constructible/passable unit (useful later e.g. for logging one method's
full signature).

**4. `CallbackInterfaceRegistry` is a derived type with a private
module-level singleton instance, plus free-function wrappers — not a
bare set of module-level maps.**
`type :: CallbackInterfaceRegistry` privately owns
`ids : CallbackInterfaceIdSet`... actually a `CallbackInterfaceMap`
(`CallbackInterfaceId -> CallbackInterface`), a `ServiceNameIdMap`
(`character -> CallbackInterfaceId`), and one
`CallbackInterfaceIdGenerator`. A private module variable
`the_registry : type(CallbackInterfaceRegistry), save` plus a private
`get_registry()` returning `class(CallbackInterfaceRegistry), pointer`
via `target`+pointer-to-module-variable backs public wrapper
subroutines/functions: `register_callback_interface(service_name,
interface, id, rc)`, `get_callback_interface(id, rc)` /
`get_callback_interface_by_name(service_name, rc)`,
`lookup_callback_interface_id(service_name, rc)` — REQ-CB-010's
explicit "MAY be accessed through narrower wrapper procedures that hide
the singleton" option, chosen over exposing the type. Registration
rejects a duplicate `service_name` (no-silent-replace, same convention
as everywhere else in this module family). *Alternative considered:*
a bare pair of module-level `save` maps with no wrapping derived type
(the minimal extension of `ExtensionResolution.F90`'s own single-flag
precedent). Rejected: three pieces of correlated state (two maps that
must stay mutually consistent, plus a generator) belong in one type
with an invariant-preserving API (`register_callback_interface` updates
both maps and consumes the generator atomically) rather than three
independent module variables a future maintainer could update out of
sync.

**5. `CallbackStateBinding`'s "method attachment" map value type is the
existing `StateMethodInvocation` (Phase 4a), not a new
`CallbackMethodAttachment` wrapper type.**
See proposal.md - What Changes for the rationale (a callback method
attachment *is* exactly "an invocation adapter for this method name on
this callback State's `NodeId`"). Binding a method
(`bind_method(method_name, invoker, rc)`, `invoker` optional) internally
constructs `StateMethodInvocation(this%state_node_id, method_name,
invoker)` and inserts it into the map under `method_name` — so the
binding always carries the correct `state_node_id` by construction, the
caller never supplies it redundantly. *Alternative considered:*
introduce `CallbackMethodAttachment` as a thin empty/placeholder type
(mirroring `CallbackInterfaceId`'s own "empty stub, filled in later"
precedent from Phase 4a). Rejected: unlike `CallbackInterfaceId` (an
identity type with no natural content yet), a method attachment has an
obvious, already-built, already-tested real shape available today
(`StateMethodInvocation`); introducing an empty stub instead would
create a second, redundant "future work" placeholder for something
Phase 4a already finished building, and would require a translation
step in Phase 4d to convert the stub into the adapter type that
actually gets invoked.

**6. `CallbackStateBinding.bind_argument`/`bind_method` validate names
against a `CallbackInterface` supplied at binding time (or at
construction), not against a `CallbackInterfaceRegistry` lookup by
`CallbackInterfaceId`.**
`CallbackStateBinding` is constructed with the `CallbackInterfaceId` and
a `CallbackInterface` value (`CallbackStateBinding(interface_id,
interface, state_node_id)`) — the caller (a test today; Phase 4d's
`GraphBuilder` wiring later) is expected to have already resolved the
id via the registry, exactly the same "caller resolves, this module
just validates the result" split `MethodGraphNode.bind_argument` uses
for kind-constraint checks (`actual_kind` supplied by the caller, not
looked up). *Alternative considered:* have `CallbackStateBinding` call
`get_callback_interface(interface_id)` itself internally. Rejected:
would make every `CallbackStateBinding` test depend on global registry
state (register-then-construct-then-unregister bookkeping per test),
where a plain synthetic `CallbackInterface` value passed directly is
sufficient and keeps tests fully isolated - matching this module
family's existing preference for synthetic-graph/unit-local fixtures
over shared global state in tests.

## Risks / Trade-offs

- **[Risk]** `CallbackMethodSpec`'s per-method validation
  (`set_method_argument_access` checking the argument name against the
  *interface's* declarations) creates an ordering requirement — an
  argument must be declared on the `CallbackInterface` before any method
  can declare access to it — that isn't explicit in REQ-CB-003/004's
  prose. → **Mitigation:** document the required order in the
  `CallbackInterface` module header and enforce it with an explicit
  `_ASSERT` (clear failure message) rather than a silent no-op; capture
  it as an explicit spec scenario ("Method argument access for an
  undeclared argument is rejected") so the ordering is part of the
  tested contract, not just a comment.
- **[Risk]** `CallbackInterfaceRegistry` as global mutable module state
  makes tests order-sensitive if two test cases pick the same
  `service_name`. → **Mitigation:** matches `ExtensionResolution.F90`'s
  existing precedent exactly (that module's own flag is a shared
  singleton too); follow the same convention its test suite already
  uses (unique per-test names) and, if this proves fragile in practice,
  add a test-only `reset_registry()` the same way
  `set_materialize_extensions()` exists purely for test control - not
  planned up front, added only if the risk materializes.
- **[Trade-off]** `CallbackStateBinding` trusts caller-supplied member
  `NodeId`s with no verification that they actually exist in any real
  graph (Non-Goals). This is consistent with `MethodGraphNode.bind_argument`'s
  own established scope (it doesn't verify `target_id` against a real
  `ComponentGraph` either) but means a real Phase 4d integration bug
  (binding a `NodeId` from the wrong graph) would surface later, at
  invocation time, not at binding time. Acceptable: the same trade-off
  already shipped in Phase 4a without issue, and catching it earlier
  would require exactly the `ComponentGraph` dependency this change is
  chartered to avoid.

## Migration Plan

Not applicable — purely additive new modules, no existing code path
changes, no data migration, nothing to roll back beyond reverting the
new files.
