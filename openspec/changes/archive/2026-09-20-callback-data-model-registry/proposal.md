## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.3 identifies Phase
4c as the next Phase 4 sub-change: the callback data model and registry
(`15-callbacks.md` §15.2-15.7) — `CallbackInterface`/
`CallbackArgumentSpec`/`CallbackMethodSpec`/`CallbackStateBinding`/
`CallbackInterfaceRegistry`. Phase 4a (`method-graph-node`, landed)
already introduced `AccessSpec` and `MethodGraphNode` specifically so
this capability could reuse them unchanged, and already stubbed
`CallbackInterfaceId` (empty identity type) and `StateMethodInvoker`
(abstract injection point, `MethodInvocationAdapter.F90`) as forward
placeholders for this change. Without `CallbackInterface`, there is no
way to declare a reusable callback contract (e.g. `15-callbacks.md`
§15.3's `PassiveTracer` example: a `tracers` argument, a `get` method
exporting it, a `put` method importing it) or register one for lookup —
which blocks Phase 4d (`GraphBuilder` wildcard/regex callback wiring,
`15-callbacks.md` §15.9-15.10), since 4d's wiring has nothing to
validate a matched export against without this data model existing
first.

This is greenfield work (no existing `CallbackInterface`/
`CallbackStateBinding`/registry code) on top of the completed Phase 1-3
foundation plus Phase 4a's `MethodGraphNode`/`AccessSpec`/
`StateMethodInvocation`. Per the roadmap, it stays synthetic/unit-test
only: no `GraphBuilder` wiring, no real ESMF `ESMF_MethodAdd`/
`ESMF_MethodExecute` attachment, no wildcard/regex expansion — those are
explicitly Phase 4d's job. It depends on `composite-state-spec`
(landed) only for the *shape* `CallbackStateBinding` assumes (an
argument's member `NodeId` may come from a composite callback State's
declared member tree) — this change does not itself walk that tree or
require a real `GraphBuilder`-built one; a caller (test or, later, 4d)
supplies member `NodeId`s directly, the same posture `MethodGraphNode`'s
own `bind_argument` already established for argument bindings.

## What Changes

- **New `CallbackArgumentSpec`** (`15-callbacks.md` REQ-CB-003): a
  callback interface's named argument declaration — name plus expected
  `MAPL_StateItem_Flag` kind (e.g. `MAPL_STATEITEM_FIELDBUNDLE` for the
  `PassiveTracer` example's `tracers` argument). Declaration-only,
  mirroring `ArgumentSpec`'s existing shape but without an `AccessSpec`
  field — access is per-method here (`CallbackMethodSpec`, next), not
  per-argument, per REQ-CB-004's explicit design ("one shared argument
  MAY have different access in different methods").
- **New `CallbackMethodSpec`** (REQ-CB-004): one callback method's
  argument-access declarations — `argument name -> AccessSpec`,
  reusing the existing `AccessSpec` type unchanged (introduced by Phase
  4a specifically for this reuse). Validated at declaration time only
  against argument *names* already known to its owning
  `CallbackInterface` (no cross-reference back to `CallbackArgumentSpec`
  kind — that check belongs to whoever binds a real value later, per
  `ArgumentSpec`'s own established precedent of leaving kind-matching to
  the binder).
- **New `CallbackInterface`** (REQ-CB-002/003): the reusable contract —
  `argument name -> CallbackArgumentSpec` plus `method name ->
  CallbackMethodSpec`, built through a declare-argument /
  declare-method / set-method-argument-access sequence with the
  established no-silent-replace rejection on duplicate names at each
  level (matching `MethodGraphNode.declare_argument`'s own convention).
  Does not store its own service name (REQ-CB-002: that key lives only
  in the registry's lookup map, never duplicated on the interface
  itself).
- **New `CallbackInterfaceRegistry`** (REQ-CB-008/009/010): a
  module-level singleton owning `CallbackInterfaceId -> CallbackInterface`,
  `service name -> CallbackInterfaceId`, and a
  `CallbackInterfaceIdGenerator` (already available for free from
  `CallbackInterfaceId`'s existing `IdTemplate.inc`-generated
  `CallbackInterfaceIdGenerator`). Accessed through narrow
  registration/lookup wrapper procedures (REQ-CB-010) rather than
  exposing the singleton type itself, following the existing
  module-level-state precedent already in this module family
  (`ExtensionResolution.F90`'s `materialize_extensions` flag +
  accessors) generalized from a single flag to a full owned-state
  singleton.
- **New `CallbackStateBinding`** (REQ-CB-007): explicit storage of one
  callback State's realized contract — `CallbackInterfaceId`, the
  `NodeId` of the callback State's own `StateItemNode`, an `argument
  name -> member NodeId` map (reusing the existing `StateItemMemberMap`
  type unchanged, the same map `GraphStateItem`'s membership and
  `MethodGraphNode`'s own argument bindings already use), and a `method
  name -> method attachment` map. The "method attachment" value type is
  Phase 4a's own existing `StateMethodInvocation` adapter
  (`StateMethodInvocation.F90`) — a callback method attachment *is*
  "an invocation adapter bound to this method name on this callback
  State's `NodeId`," which that type already models exactly; no new
  wrapper type is introduced to duplicate it. Validates argument/method
  names against the bound `CallbackInterface` at binding time (no
  argument or method name outside the interface's own declarations is
  accepted), but does not itself resolve member `NodeId`s from a real
  composite State tree (caller-supplied, see Why).
- **Restated, not newly implemented:** REQ-CB-001 (no special
  `GraphNode` subclass for callbacks — nothing here introduces one) and
  REQ-CB-005 (`AccessSpec` as a general, non-callback-specific concept —
  already true since Phase 4a; this change is simply its second real
  consumer, exercising the reuse the roadmap called for).

**Explicit deferrals** (later Phase 4 sub-changes' jobs, per
`20-implementation-roadmap.md` §20.4.3):

- Physical ESMF placement convention (import vs. export State) for
  callback States (§15.8, `[OPEN]`, REQ-CB-011/012) — not addressed by
  this change; `CallbackStateBinding` stores a bare `NodeId`, agnostic
  to where in the graph that `NodeId`'s State ultimately sits.
- Wildcard/regex expansion against the flattened qualified-export
  namespace, per-method get/put `DependencyNetwork`s, and the
  invoke-once-after-all-args-ready discipline (§15.9-15.10, REQ-CB-016
  through REQ-CB-020) — Phase 4d.
- Real `ESMF_MethodAdd`/`ESMF_MethodExecute`-backed `StateMethodInvoker`
  implementation and any `GraphBuilder` wiring that constructs a real
  `CallbackStateBinding` from an advertised composite callback State —
  Phase 4d (this change continues to supply only the synthetic
  test-double `StateMethodInvoker` Phase 4a already established).
- Handler/Invoker terminology finalization (§15.5, `[OPEN]`, Q6) — used
  only in comments/docs here, not settled by this change.

## Capabilities

### New Capabilities
- `graph/callback-data-model`: `CallbackArgumentSpec`, `CallbackMethodSpec`,
  `CallbackInterface`, `CallbackInterfaceRegistry`, and
  `CallbackStateBinding` — the reusable callback-contract data model and
  its shared, cross-`ComponentGraph` registry.

### Modified Capabilities
(none — no existing capability's requirements change. `AccessSpec`,
`StateItemMemberMap`, `StateMethodInvocation`, and `CallbackInterfaceId`
are reused unmodified, exactly as Phase 4a's own design anticipated.)

## Impact

- **Affected code (all new files, `superstructure/generic/graph/`)**:
  - `CallbackArgumentSpec.F90` (`mapl_CallbackArgumentSpec_mod`)
  - `containers/CallbackArgumentSpecMap.F90`
    (`mapl_CallbackArgumentSpecMap_mod`, gFTL map, mirrors
    `containers/ArgumentSpecMap.F90`)
  - `containers/AccessSpecMap.F90` (`mapl_AccessSpecMap_mod`, gFTL map,
    character -> `AccessSpec`, new small container `CallbackMethodSpec`
    needs that does not exist yet)
  - `CallbackMethodSpec.F90` (`mapl_CallbackMethodSpec_mod`)
  - `containers/CallbackMethodSpecMap.F90`
    (`mapl_CallbackMethodSpecMap_mod`, gFTL map, character ->
    `CallbackMethodSpec`)
  - `CallbackInterface.F90` (`mapl_CallbackInterface_mod`)
  - `CallbackInterfaceRegistry.F90` (`mapl_CallbackInterfaceRegistry_mod`)
  - `containers/CallbackMethodAttachmentMap.F90`
    (`mapl_CallbackMethodAttachmentMap_mod`, gFTL map, character ->
    `StateMethodInvocation`)
  - `CallbackStateBinding.F90` (`mapl_CallbackStateBinding_mod`)
  - Corresponding `CMakeLists.txt` entries
    (`superstructure/generic/graph/CMakeLists.txt` and its `containers/`
    subdirectory listing).
- **No changes** to `ComponentGraph.F90`, `GraphBuilder.F90`,
  `MethodGraphNode.F90`, `MethodInvocationAdapter.F90`,
  `StateMethodInvocation.F90`, `AccessSpec.F90`,
  `containers/StateItemMemberMap.F90`, or `CallbackInterfaceId.F90` —
  this change is additive, new-module-only, consuming those existing
  types unchanged, exactly like Phase 4a's own posture.
- **Dependencies**: `CallbackArgumentSpec` depends on
  `MAPL_StateItem_Flag`. `CallbackMethodSpec` depends on `AccessSpec`
  and the new `AccessSpecMap`. `CallbackInterface` depends on both plus
  their new map containers. `CallbackInterfaceRegistry` depends on
  `CallbackInterface` and the existing `CallbackInterfaceId`/
  `CallbackInterfaceIdGenerator`. `CallbackStateBinding` depends on
  `CallbackInterfaceId`, `NodeId`, the existing `StateItemMemberMap`,
  the existing `StateMethodInvocation`, and (for validation only)
  `CallbackInterface`. None of the new modules depend on
  `ComponentGraph`, `GraphBuilder`, `OuterMetaComponent`, or ESMF
  component/state machinery beyond the `MAPL_StateItem_Flag`/
  `ESMF_Clock` types already used elsewhere in this module family.
- **Tests**: new unit tests exercising `CallbackArgumentSpec`/
  `CallbackMethodSpec` construction and accessors; `CallbackInterface`
  argument/method declaration (including duplicate-name rejection at
  each level and rejecting a method-argument-access declaration for an
  argument name the interface never declared); `CallbackInterfaceRegistry`
  registration, service-name lookup, and duplicate-service-name
  rejection; `CallbackStateBinding` construction, argument/method
  binding against a registered `CallbackInterface` (including rejecting
  an argument or method name outside that interface's declarations),
  and retrieval of bound member `NodeId`s / method attachments —
  reproducing `15-callbacks.md` §15.3's `PassiveTracer` example
  end-to-end as a synthetic-graph scenario (declare `tracers`
  `FieldBundle` argument, declare `get`/`put` methods with `OUT`/`IN`
  access respectively, register, bind to a synthetic callback State
  `NodeId` with a synthetic member `NodeId`).
- **Out of scope**: everything under "Explicit deferrals" above.
