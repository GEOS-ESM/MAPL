## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.8's 10d entry is
the last of Phase 10's four strictly-ordered sub-changes. Today no
production code path ever constructs a `MethodGraphNode` for an
ordinary component's own `GENERIC_RUN_USER` phase - the only real
construction site that exists, `GraphBuilder.F90`'s
`build_callback_method_binding`, is deliberately narrower (Phase 4c/4d
callback wiring, keyed to a `CallbackInterface`'s declared arguments).

**Re-scoped during implementation (see design.md's own "Scope
narrowed" note).** This change was originally planned to also wire a
real invocation call site into `run_user.F90` and extend the 10c
equivalence fixture to verify re-materialization across two timesteps.
Both were abandoned, not merely deferred by choice, once direct code
tracing surfaced two real blockers:

1. **Double-invocation.** The planned invocation path
  (`invoke_on_default_network` -> `method_node%invoke()` ->
  `GridCompDriverPhaseInvoker%invoke_phase()`) ends in
  `driver%run(phase_idx=...)` on the *same* `GriddedComponentDriver`
  `run_user.F90`'s own existing legacy call already invokes for that
  same phase. Calling both back-to-back is a second real execution of
  the user's own GridComp run phase every timestep - not inert
  revision-counter bookkeeping, and (for a component with children,
  e.g. `ConfigurableGridComp`) a second real execution of every child's
  own run phase too. This directly violates the "run-phase invocation
  does not alter the legacy run call's own effects" property any
  invocation-wiring sub-change would need. Avoiding it requires the
  legacy run call itself to be skippable under a non-default graph
  mode (mirroring `initialize_accept_transfer.F90`'s own existing
  `if (graph_mode() /= GRAPH_MODE_GRAPH) call ... add_to_states(...)`
  skip for materialization) - no such skip exists for the run call
  today, and adding one is real new design, not a wiring detail.
2. **No safe mode to test it under even if (1) were fixed.** The
  skip in (1) can only ever be safe under `GRAPH_MODE_GRAPH` (sole
  producer, legacy fully skipped) - never under `GRAPH_MODE_BOTH`,
  which is the *only* mode `graph/equivalence-harness` validates under
  by design (its own Purpose section). `BOTH` mode's existing
  no-collision safety for materialization relies on graph-native
  materialization having no observable side effect of its own; a
  GridComp phase invocation has no such property - "run it alongside
  legacy, just don't let it collide" is not a coherent safety story for
  real user code execution the way it is for a `FieldCreate`. Separately,
  `GRAPH_MODE_GRAPH`-alone cross-component population is itself already
  confirmed broken (10b's own direct-experiment finding, roadmap
  §20.4.8's own "Explicitly out of scope" - `ESMF_RC_ARG_INCOMP` at
  `ESMF_StateAdd`), so even the one mode where a legacy-skip could in
  principle be safe has no working materialization story to validate
  the chain-re-propagation claim against today.

Both are genuinely new design problems - a legacy-run-call skip
mechanism, and (separately) whatever makes `GRAPH_MODE_GRAPH`-alone
population actually work - neither of which this change invents an
answer for. Roadmap §20.4.8 itself already names the second one as a
blocker for "10c/10d or Phase 6 itself," so resolving it here would be
scope well beyond one sub-change.

**What this change still delivers, and why it is worth landing on its
own:** the construction half - `GraphBuilder` building exactly one real
`MethodGraphNode` per component for its own run phase, keyed to that
component's own advertised ports - is independently useful, synthetic-
graph-testable (no real driver run involved, mirroring every other
`Test_GraphBuilder.pf` construction test), and is a real prerequisite
any future invocation-wiring attempt will still need once the two
blockers above are separately resolved. Landing it now, with its own
direct unit coverage, avoids re-deriving the same construction logic
later under time pressure once the legacy-skip and `GRAPH_MODE_GRAPH`
population mechanisms exist.

## What Changes

- `GraphBuilder.F90` gains a new construction call site -
  `graphbuilder_build_run_phase_node` - that builds exactly one
  `MethodGraphNode` per component, wrapping that component's own
  `GENERIC_RUN_USER` phase. It follows `build_callback_method_binding`'s
  own `declare_argument`/`bind_argument`/`register_node` shape, but
  keyed to the component's own advertised import/export
  `VariableSpec`s (resolved via `ComponentSpec%var_specs` and each
  item's own already-resolved resource-index `NodeId`, the same lookup
  `advertise_one` itself populates) instead of a `CallbackInterface`'s
  declared arguments. No new invocation adapter or driver-resolution
  type is introduced: the already-landed, already-generic
  `GridCompDriverPhaseInvoker` (Phase 4b) and
  `OuterMetaComponentDriverResolver` (Phase 4b) are reused as-is to
  build the node's attached adapter, with driver key
  `SELF_COMPONENT_NAME` - but the adapter is never invoked by this
  change (see Non-Goal below).
- The constructed node's `NodeId` is retained on `OuterMetaComponent`
  (a new, plain `type(NodeId)` field, default `INVALID_NodeId`) so a
  future invocation call site does not need to re-derive it.
- Construction is called from `graphbuilder_run_connect_hook`'s own
  body (the hook `initialize_accept_transfer.F90` already runs),
  immediately after connection resolution and before freeze - gated by
  `graph_mode() /= GRAPH_MODE_REGISTRY`, a no-op under the default
  mode.

## Non-Goals (explicit, not merely deferred)

- **No invocation call site.** `run_user.F90` is not touched by this
  change. No code anywhere calls `invoke()`/`invoke_on_default_network`
  on the constructed node. The double-invocation hazard described above
  means doing so safely requires a legacy-run-call skip mechanism this
  change does not build.
- **No fixture/harness changes.** The 10c `units_and_typekind_mismatch`
  fixture and `Test_GraphEquivalenceHarness.pf` are untouched - the
  "verify re-materialization across two timesteps" check this change
  originally planned is not attempted, since nothing invokes the node
  it would need to observe, and (independently) `BOTH` mode has no safe
  story for ever invoking a real run phase in the first place (see
  Why).
- **No `GRAPH_MODE_GRAPH`-alone fix, no legacy-run-call skip
  mechanism.** Both named above as the real blockers; neither is
  attempted here. A future change proposing real invocation wiring
  will need to resolve at least the legacy-skip mechanism first (and
  very likely needs `GRAPH_MODE_GRAPH`-alone population working too, to
  have anything to validate against).

## Capabilities

### New Capabilities
- `graph/ordinary-phase-wiring`: `GraphBuilder` constructs exactly one
  `MethodGraphNode` per component for that component's own
  `GENERIC_RUN_USER` phase, keyed to the component's own advertised
  ports rather than a `CallbackInterface`. Invocation is explicitly out
  of scope for this capability as currently specified; a later change
  may extend it once the invocation-safety blockers above are
  resolved.

### Modified Capabilities
None. `graph/equivalence-harness` is untouched (Non-Goals above).

## Impact

- **Affected code:** `superstructure/generic/GraphBuilder.F90` (new
  `graphbuilder_build_run_phase_node`),
  `superstructure/generic/OuterMetaComponent.F90` (new
  `run_phase_method_node_id` field + accessor/setter pair).
- **Not affected:** `superstructure/generic/OuterMetaComponent/run_user.F90`
  (no new call site - reverted from an earlier implementation attempt,
  see Why), `mapl_MethodInvocation_mod`/`GridCompDriverPhaseInvoker`/
  `OuterMetaComponentDriverResolver` (reused, by reference only, to
  build the node's adapter - never invoked by this change),
  `build_callback_method_binding`/`CallbackMethodBinding` (callback-
  specific construction path, untouched), `Test_GraphEquivalenceHarness.pf`
  and the `units_and_typekind_mismatch` fixture (untouched).
- **Downstream:** this is still the first production construction site
  for an ordinary-phase `MethodGraphNode`, directly reusable once a
  future change resolves the legacy-run-call-skip and
  `GRAPH_MODE_GRAPH`-alone-population blockers named above. That future
  change is not scoped or committed to by this proposal.
