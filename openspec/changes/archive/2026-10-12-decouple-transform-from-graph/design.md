## Context

See proposal.md - Why. Two concrete types matter for the mechanism:

- `mapl_Transform_mod`'s abstract `compute(this, rc)` (`Transform.F90`)
  takes no data arguments at all. `TransformGraphNode%execute(this, rc)`
  (`TransformGraphNode.F90`) also takes none — it just calls
  `this%transformer%compute(_RC)`. Neither has ever had the bound
  input/output values to hand to the other; the only reason this has
  worked so far is that each concrete `Transform` self-serves by storing
  a `class(ComponentGraph), pointer` plus both bound `NodeId`s at
  construction time (`UnitsConverterTransform.F90:58-60`,
  `TypeKindConverterTransform.F90:50-52`) and calling
  `this%graph%get_node(...)` itself inside `compute()`.
- The orchestrator that actually knows the bound `NodeId`s —
  `ComponentGraph_DemandDrivenUpdate.F90`'s `finish_transform` — already
  resolves them today, immediately before calling `execute()`, for a
  different purpose: assembling `current_input_revisions`
  (`ComponentGraph_DemandDrivenUpdate.F90:248-271`) and, after
  execution, advancing each bound output's revision
  (`ComponentGraph_DemandDrivenUpdate.F90:277-298`). The same
  `graph%get_port_binding`/`graph%get_node` calls that produce a
  `NodeRevision` for each port today can equally produce the port's
  `GraphStateItem` payload — nothing new needs to be looked up, only
  carried one step further.

## Goals / Non-Goals

**Goals:**
- Give `Transform%compute` real arguments — the caller-supplied,
  already-resolved input and output `GraphStateItem` values, one per
  declared port name — so no concrete `Transform` needs its own
  `ComponentGraph`/`NodeId` reference to do its job.
- Keep `TransformGraphNode` itself free of any stored `ComponentGraph`
  reference (unchanged from today) — the newly-required
  inputs/outputs are passed into `execute()` by its one caller
  (`finish_transform`), the same way `needs_execution`/`record_run`
  already receive externally-assembled `PortNameRevisionMap` arguments
  today, not stored on the node.
- Remove `ComponentGraph`/`NodeId` from `Characteristic%build_transform`'s
  own interface, since its only use of them was forwarding into exactly
  the constructor pattern being removed.
- Preserve every existing `graph/transform-graph-node` and
  `graph/extension-reuse` observable scenario exactly (proposal.md
  Capabilities) — zero behavior change, mechanism only.

**Non-Goals:**
- Merging `Characteristic`/`StateItemCharacteristic` into one family.
  This change removes the one obstacle (the `ComponentGraph` dependency
  that would otherwise create a module-use cycle through `GraphStateItem`)
  but does not perform the merge — left to a follow-up change, which can
  now proceed because `Characteristic.F90` no longer needs
  `use mapl_ComponentGraph_mod` at all once this change lands.
- Any change to `mapl_ExtensionResolution_mod`'s chain-building
  algorithm, reuse-key logic, or materialization gating
  (`materialize_extensions_enabled`) — `build_chain` keeps doing its own
  `NodeId`/port-binding/dependency-edge work exactly as today; only its
  one `build_transform` call site's argument list shrinks.
- Any change to `GraphBuilder.F90`'s connection-resolution callers
  (`resolve_one`, `try_materialize_field_extension`, etc.) — out of
  scope, left to `graph-builder-stateitem-resolution`.
- A general-purpose "port name -> value" container usable beyond this
  one need — the new map type exists solely to carry
  `GraphStateItem` values between `finish_transform` and `compute()`,
  mirroring `PortNameRevisionMap`'s own existing scope exactly.

## Decisions

**New container type `PortNameGraphStateItemMap`
(`mapl_PortNameGraphStateItemMap_mod`), a gFTL map of port name
(character, deferred-length key) -> `GraphStateItem`, instantiated the
same way `PortNameRevisionMap` already is (same `map/template.inc`,
same `Key = __CHARACTER_DEFERRED` convention).** Mirrors
`PortNameRevisionMap.F90` line for line, substituting `GraphStateItem`
for `NodeRevision` as the mapped type. Two instances of this map (one
for a transform's declared inputs, one for its declared outputs) are
assembled by `finish_transform` and passed into `execute()`/`compute()`
together — not a single combined map — because inputs and outputs are
already two structurally distinct `PortSpecMap`s on every existing
caller (`get_input_ports()`/`get_output_ports()`), and `compute()`'s own
job (read inputs, write outputs) is clearer with that same distinction
preserved at the call boundary.

**`Transform%compute`'s interface becomes
`compute(this, inputs, outputs, rc)`, with `inputs`/`outputs` both
`type(PortNameGraphStateItemMap), intent(in)`.** `intent(in)` for both,
including `outputs`: a concrete `Transform`'s `compute()` does not
mutate the `GraphStateItem` *value* passed in (which component is
allocated, which membership map it holds) — it writes through the
*already-allocated* `ESMF_Field`/`ESMF_FieldBundle`/`ESMF_State` handle
`GraphStateItem%get_field()` (etc.) returns, which is itself a reference
-semantics ESMF handle, not a value copy of the underlying data. This
matches the current code's own existing pattern exactly
(`output_item = output_node%get_payload()` then
`output_field = output_item%get_field(_RC)` then
`x_out = converter%convert(x_in)` — already writing through a by-value
`GraphStateItem`/`ESMF_Field` copy today, since `assign_fptr` and ESMF's
own field-pointer semantics are what actually makes the write visible,
not Fortran argument aliasing). No behavior changes by making this
`intent(in)` explicit; it only states what was already true.

**Alternative considered:** pass `outputs` as `intent(inout)` to allow a
future `Transform` to call `set_characteristic`/structural-reset methods
on its own output `GraphStateItem` during `compute()`. Rejected for this
change — no existing or currently-planned `compute()` implementation
does this (both concrete subclasses only read `get_field()` off their
output value and write through its ESMF handle), and widening the
interface speculatively ahead of a real need contradicts this change's
own narrow, mechanism-only scope. Revisit if/when a real `compute()`
needs it.

**`TransformGraphNode%execute`'s interface becomes
`execute(this, inputs, outputs, rc)`, forwarding both maps unchanged
into `this%transformer%compute(inputs, outputs, rc)`.** `TransformGraphNode`
itself remains free of any stored `ComponentGraph`/`NodeId` reference —
unchanged from today — these two maps are supplied by its one caller
(`finish_transform`), exactly the same calling convention
`needs_execution(current_input_revisions)`/
`record_run(current_input_revisions)` already use for externally
-assembled `PortNameRevisionMap` arguments on the very same type. No new
architectural pattern is introduced; this is that same existing pattern
applied to one additional pair of arguments.

**`finish_transform` (`ComponentGraph_DemandDrivenUpdate.F90`) gains two
new assembly loops, each a near-duplicate of an existing loop already
there.** The existing input-port loop
(`ComponentGraph_DemandDrivenUpdate.F90:250-271`) already resolves each
declared input port's bound `NodeId` and `StateItemNode` to extract a
`NodeRevision`; it is extended to also extract that same node's
`GraphStateItem` (via the already-public `get_payload()`) into a new
`PortNameGraphStateItemMap`, in the same loop, no new traversal. A
second, new loop over `output_ports` (the existing output-port loop at
`ComponentGraph_DemandDrivenUpdate.F90:280-298` already walks these same
ports for `advance_revision()` after `execute()` succeeds — this is a
*second*, separate loop run *before* `execute()`, to resolve values
`compute()` needs to write to, not a modification of the existing
post-execution revision-advance loop) resolves each declared output
port's bound `NodeId`/`StateItemNode`'s `GraphStateItem` into a second
map. Both maps are passed to `transform_node%execute(inputs, outputs,
_RC)` in place of today's `execute(_RC)`.

**Alternative considered:** have `TransformGraphNode` itself resolve its
bound values, by giving it a stored `ComponentGraph`/`DependencyNetworkId`
reference (mirroring the very pattern being removed from `Transform`,
one layer up). Rejected: this would just relocate the problem this
change exists to fix, not solve it — `TransformGraphNode` would then
need the graph dependency `Transform` is being relieved of, and the
proposal's own goal (make `compute()`'s *data source* explicit at the
call site, not self-fetched) would not be achieved, merely moved up one
layer. `finish_transform` already has every piece of information needed
and already performs the structurally identical resolution for
revisions one line above — reusing that existing vantage point costs
nothing new.

**`UnitsConverterTransform`/`TypeKindConverterTransform` drop their
stored `graph`/`input_node_id`/`output_node_id` fields; `compute()`
reads `inputs%at('source_field')`/`outputs%at('destination_field')`
(the same literal port-name constants each already declares at
construction time) instead of self-fetching via `this%graph%get_node(...)`.**
Each constructor keeps declaring its own fixed port shape exactly as
today (`declare_input_port(INPUT_PORT_NAME)`/
`declare_output_port(OUTPUT_PORT_NAME)`) — only the *source* of the
bound value at `compute()` time changes, not the port shape or naming
convention itself.

**`Characteristic%build_transform`'s interface drops
`class(ComponentGraph), target, intent(in) :: graph`,
`input_node_id`, and `output_node_id`, becoming
`build_transform(this, goal, transformer, rc)`.** Every existing
implementation (`units_build_transform`, `adapter_build_transform`,
`vgrid_build_transform`, `geom_build_transform`) only ever forwarded
these three arguments, unused by the method itself, into whichever
concrete `Transform` constructor it called — removing them from
`compute()`'s own self-service pattern makes them dead parameters here
too; dropping them is mechanical, not a new design question. The one
production call site (`ExtensionResolution.F90`'s `build_chain`,
line ~250) already has `graph`/`current_node_id`/`extension_node_id` in
scope for its own, separate, still-needed work (registering the
`TransformGraphNode`, binding its two ports via `graph%bind_port`) — it
simply stops passing them into `build_transform` itself.

## Risks / Trade-offs

- **[Risk] Test doubles that implement `Transform`/`Characteristic`
  directly (`CountingCompute`/`SelfDescribingTransform` in
  `Test_TransformGraphNode.pf`, `FakeCompute`/`FakeCharacteristic` in
  `Test_ExtensionResolution.pf`) have hard-coded signatures that must be
  updated in lockstep with the interface change, or the test suite fails
  to compile, not merely fails assertions** → **Mitigation:** both files
  are already identified and read; tasks.md enumerates each one by name,
  and "every existing assertion passes unchanged" is this change's own
  stated correctness bar (mirrors `graph-builder-stateitem-resolution`'s
  own precedent for a mechanism-only change) — a compile failure in
  either file is caught immediately by the existing build, not a latent
  risk.
- **[Risk] `finish_transform`'s two new assembly loops are near
  -duplicates of its own existing loops, growing an already-dense
  procedure further** → **Mitigation:** accepted; factoring the
  shared "walk declared ports, resolve bound NodeId, extract one field
  off the bound StateItemNode" shape into a shared helper is a
  legitimate follow-up cleanup, not required for this change's own
  narrow goal (remove `ComponentGraph` from `Transform`/`Characteristic`),
  and premature extraction here would risk conflating this change's
  scope with an unrelated readability improvement.
- **[Risk] `GraphStateItem` passed into `compute()` by value (not by
  pointer/target) means a `Transform` reading a very large field
  bundle's membership map pays a copy on every call** → **Mitigation:**
  `GraphStateItem` itself is already a small, cheap-to-copy value type
  (three allocatable ESMF handles plus two small membership maps and a
  characteristics map, all of which are themselves normally
  empty/singleton for the field-typed items this capability's two real
  `Transform`s operate on) — this is the same copy cost
  `output_item = output_node%get_payload()` already incurs today inside
  each `compute()` implementation; moving the copy one layer out to
  `finish_transform` does not add a new copy, it relocates an existing
  one.
