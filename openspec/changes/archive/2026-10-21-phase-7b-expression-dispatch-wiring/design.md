## Context

`is_ordinary_resolvable_import` (`GraphBuilder_ConnectionResolution_
smod.F90`, added by Phase 7a's own task 6.8 regression fix) currently
excludes `MAPL_STATEITEM_WILDCARD`, `MAPL_STATEITEM_SERVICE`, and
`MAPL_STATEITEM_EXPRESSION` uniformly — "no graph-native resolution path
exists for this item class, so never treat it as unresolved-and-
propagatable." Confirmed by direct reading: nothing else in `GraphBuilder`
or `ExtensionResolution.F90` branches on `MAPL_STATEITEM_EXPRESSION` today.
`MAPL_STATEITEM_EXPRESSION` itself already exists at the ESMF-level flag
tier (`infrastructure/esmf/StateItem.F90`, value `208`) and is already
imported into `GraphBuilder.F90` (unused for dispatch) — only the
graph-native `MAPL_StateItem_Flag` variant vocabulary
(`MAPLStateItemFlag.F90`) and the actual resolution logic are missing.

`ExpressionClassAspect%make_transform` (`superstructure/generic/specs/
ExpressionClassAspect.F90`) is legacy's own real mechanism: parses the
expression via `parser_variables_in_expression`
(`superstructure/state/StateArithmeticParser.F90`), resolves each
referenced variable through `registry%extend()` into a `MultiState`, and
builds an `EvalTransform` holding that `ESMF_State` plus the expression
string, whose `update()` calls `StateEval(input_state, expression, f,
rc)` — a fparser-derived byte-code evaluator, not reimplemented
arithmetic. This change's own `ExpressionTransform` follows the same
evaluation strategy (reuse `StateEval` directly) but wires its inputs
through the graph-native `PortBindingTable`/named-port mechanism instead
of `registry%extend()`/`MultiState`.

`find_mismatched_characteristics`/`build_chain` (`ExtensionResolution.F90`)
and `StateItemCharacteristic%build_transform`'s own interface
(`this`/`goal` — exactly one established value, one goal value) are
fixed-arity 1:1. An expression's referenced-variable count is not fixed at
one, so this mechanism — the one Phase 7a already wired for `Bracket`->
`Field` — cannot represent Expression's N:1 case without changing that
interface for every other characteristic too (proposal.md - Why). This
confirms the roadmap's own framing: Expression needs "a dedicated hook
analogous to `4e`'s own `run_geometry_hook` precedent," not an extension
of 7a's `StateItemCharacteristic` mechanism.

`CompositeStateMaterialization.F90`'s `materialize_composite`/
`VariableSpec%declare_member` (investigated as the roadmap's own "working
hypothesis") do not fit: `declare_member` asserts `itemType ==
MAPL_STATEITEM_STATE`, and `materialize_composite` immediately allocates a
real `ESMF_State` — the wrong terminal shape for an expression result,
which must end as a plain `Field` (`materialize_field_extension` remains
Field-only, unaffected by this change). An expression's referenced
variables are already-existing sibling exports, not sub-structure *of* the
expression item — see D1.

## Goals / Non-Goals

**Goals:**
- Add graph-native vocabulary for the `EXPRESSION` item class
  (`MAPLStateItemFlag.F90`).
- Resolve an expression item's N named variable references against its
  own component's sibling export namespace, through a dedicated
  resolution step run once per advertised expression item — not through
  `resolve_one`'s pairwise connection loop and not through
  `ExtensionResolution.F90`'s 1:1 characteristic mechanism.
- Build a genuine multi-input `TransformGraphNode` (first real production
  consumer of `PortSpec`/`PortBindingTable`'s already-generic N-port
  support) wiring each resolved reference to its own named input port and
  the expression item's own node to the output port.
- Perform real expression evaluation (`StateArithmeticParser_mod::
  StateEval`, reused directly) in the new `Transform`'s `compute()`.
- Narrow `is_ordinary_resolvable_import` so `EXPRESSION` is no longer
  unconditionally excluded, once this change's own resolution step makes
  it genuinely resolvable.

**Non-Goals:**
- No change to `Wildcard`/`Service` resolution (Phase 7c) —
  `is_ordinary_resolvable_import`'s exclusion for those two is untouched.
- No change to Phase 7a's `Bracket`/`VectorBracket` shape-dispatch
  mechanism — unaffected, already landed.
- No support for an expression referencing a variable on a *different*
  component (cross-component reference) or a variable that itself needs
  further extension-chain conversion (e.g. an expression referencing a
  bracket-shaped export) — legacy's own `ExpressionClassAspect` resolves
  each reference through the full registry `extend()`/`ExtensionFamily`
  machinery (arbitrary conversion before evaluation); this change resolves
  each reference via the same plain exact-name sibling-export lookup
  ordinary connection resolution already uses (`find_export_node_id`),
  with no further mismatch/extension-chain step layered on top. A
  referenced variable requiring its own conversion is out of scope here;
  revisit if a real fixture needs it.
- No cross-level reference resolution (an expression referencing an
  ancestor's or descendant's export, not a direct sibling) — matches
  `GraphBuilder`'s own existing, pre-existing scope boundary for ordinary
  connection resolution generally (module header, "Explicitly NOT in
  scope here").
- No attempt to make expression resolution idempotent-safe against a
  *changing* expression string across re-advertisement — mirrors
  `advertise_one`'s own existing "same identity, re-advertised" contract
  (identity never changes after first advertisement in any existing
  scenario).

## Decisions

**D1: An expression's N referenced variables are bound directly as named input ports on a new `ExpressionTransform`, resolved against already-advertised sibling `StateItemNode`s — no composite/member `GraphStateItem` structure of any kind.**
Rejected alternative (the roadmap's own "working hypothesis"): model the N
references as declared composite members (`VariableSpec%declare_member`/
`CompositeStateMaterialization.F90`), reusing the mechanism `4c`
(`CallbackStateBinding`) and `4f` (`VerticalGrid`) already apply.
Investigated directly and rejected — two concrete incompatibilities, not a
style preference: (1) `declare_member` hard-asserts `this%itemType ==
MAPL_STATEITEM_STATE` at declaration time; an expression's own itemType is
`EXPRESSION`, so declaring members on it would require loosening that
assertion for a type it was never meant to admit, risking silently
widening what every *other* `declare_member` caller can do too. (2)
`materialize_composite` unconditionally calls `ESMF_StateCreate` to give
the composite's own top-level node a real `ESMF_State` payload — but an
expression's own resolved payload must end as a plain `Field`
(`try_materialize_field_extension`'s existing Field-only contract,
unaffected by this change); forcing a `STATE`-shaped intermediate node
purely to satisfy the member-declaration mechanism would need an
additional, bespoke "now unwrap the State back into the expression's own
Field identity" step with no existing precedent. Neither obstacle is
present for the chosen approach: each referenced variable is already an
ordinary, already-advertised sibling `StateItemNode` (someone else's real
export) — it needs no new structure of its own, only a *binding*, and
`PortBindingTable`/`graph%bind_port` already exist exactly for "a
`NodeId` elsewhere in the graph fills this transform's named port,"
unmodified by this change.

**D2: A dedicated resolution step (`graphbuilder_run_expression_hook`), not an extension of `resolve_one` or `ExtensionResolution.F90`'s `build_chain`.**
`resolve_one` is pairwise by construction (one destination import matched
against one source export via `for_each_matching_import`); an expression
item's own producers are a *set* of sibling exports named by parsing a
string, discovered independently of any connection the user declared —
there is no destination/import side to this resolution at all in the
ordinary sense (the expression item IS the producer-to-be, not a
consumer). `build_chain`'s own `StateItemCharacteristic%build_transform`
interface is fixed at one `this`/one `goal` (D-equivalent of 7a's own
finding) and cannot grow to N without changing every other
characteristic's interface too. The new hook instead follows the already-
established `run_geometry_hook` precedent (Phase 4e) exactly: a
graph-wide, per-component step invoked once from the lifecycle, scanning
that component's own advertised items for a specific declared item class
and performing whatever graph-wide (non-pairwise) wiring that class needs,
entirely separate from `resolve_one`'s per-connection loop.

Mechanically: for each advertised `StateItemNode` whose
`get_declared_item_type() == MAPL_STATEITEM_EXPRESSION`:
1. Read the expression string off the originating `VariableSpec`
   (threaded through at advertise time — see D3 for exactly which field
   carries it onto the node).
2. `parser_variables_in_expression(expression, rc)` ->
   `StringVector` of referenced names (reused unmodified from
   `superstructure/state/StateArithmeticParser.F90`; `MAPL.generic`
   already depends on `MAPL.state`, so this is an already-permitted
   dependency, not a new one).
3. For each referenced name, `find_export_node_id(this, <self component
   name>, name, ...)` (reused unmodified — the same exact-name sibling
   lookup ordinary resolution already uses). A miss is reported exactly
   like the existing "import with no matching export is left unresolved"
   case (spec scenario), not silently dropped and not a hard failure.
4. Build a new `ExpressionTransform(expression)`, declaring one named
   input port per referenced variable (named after the variable itself —
   D4) plus one `destination_field` output port.
5. Register the `TransformGraphNode`, bind every input port
   (`graph%bind_port(net_id, transform_id, variable_name,
   resolved_export_node_id)`) and the output port
   (`graph%bind_port(net_id, transform_id, 'destination_field',
   expression_item_node_id)`), then `graph%add_dependency(net_id,
   transform_id, expression_item_node_id)` — the transform's own input
   dependencies are implied by the port bindings exactly as `build_chain`
   already establishes for its own single-input case
   (`add_dependency(net_id, current_node_id, transform_node_id)`,
   mirrored here once per referenced variable).
6. Idempotency: guarded the same way `resolve_callback_destination`
   already guards its own "has this already been built" check — a
   resource-index entry keyed off the expression item's own `NodeId`
   (`'EXPRHOOK:' // node_id%to_string()`), checked before step 4 runs.

**D3: The expression string reaches the graph-native node via a new `GraphStateItem` accessor (`get/set_declared_expression`), populated at advertise time from `VariableSpec%expression` — mirroring Phase 7a's own `declared_item_type` precedent, not the characteristics map.**
`VariableSpec%expression` (`superstructure/generic/specs/VariableSpec.F90`
line ~119) is a plain allocatable string, already present and already
threaded into legacy's `ExpressionClassAspect` constructor
(`make_ClassAspect`, line ~908). `populate_characteristics`
(`GraphBuilder_Advertising_smod.F90`) already captures several per-item
`VariableSpec` fields this same way onto `GraphStateItem` directly
(`declared_geom`/`declared_vertical_grid`/`declared_item_type`/
`declared_restart_mode`) rather than through the `StateItemCharacteristicMap`
— the expression string is the same kind of "materialization input," read
once at advertise time, never compared for mismatch (there is no
`ExpressionCharacteristic`; D2's hook reads this field directly, bypassing
`find_mismatched_characteristics` entirely, consistent with D2's "not an
extension of `build_chain`" decision). Allocatable, unallocated for every
non-`EXPRESSION` item — same "absent means not applicable" convention
`declared_vertical_stagger` already uses.

Alternative considered: carry the expression string as a new
`StateItemCharacteristic` subclass (mirroring 7a's `ItemShapeCharacteristic`
exactly). Rejected — a characteristic's entire purpose (REQ-CHAR-009) is
pairwise mismatch comparison through `find_mismatched_characteristics`;
there is no "other side" to compare an expression string against (an
import never declares its own expected expression), so modeling it as a
characteristic would add machinery (map entry, `needs_extension_for`,
`get_signature`) with no real comparison ever performed — pure overhead
for a value D2's hook already reads directly off the node it is already
iterating.

**D4: Input port names are the referenced variable's own short name (e.g. `"A"`, `"B"`, `"C"`), not a positional/generic convention (`source_field_1`, `source_field_2`, ...).**
Every existing `Transform` subclass uses a small, fixed, hardcoded port
name set (`source_field`/`destination_field`, etc.) because each one has a
fixed, known arity — appropriate for 1:1/fixed-shape conversions. An
`ExpressionTransform`'s arity is determined per-instance, from the
expression string itself; using the referenced name directly as the port
name requires no separate name-generation convention, makes a bound port
self-describing when inspected (matches the existing `PortSpec`
reasoning: "named ports are a graph-level concept... not whatever ESMF
State naming convention"), and is already guaranteed unique within one
expression — `parser_variables_in_expression`'s own return is a set of
distinct referenced names, and `TransformBase%declare_input_port` already
rejects a duplicate name outright, which would only ever fire here on a
genuinely malformed/repeated-token expression, an existing-tool concern,
not a new one this change introduces.

**D5: `ExpressionTransform%compute` assembles its bound named inputs into a plain `ESMF_State` and calls `StateArithmeticParser_mod::StateEval` directly — not a reimplementation of expression evaluation, and not a call into legacy's `EvalTransform`.**
Mirrors `UnitsConverterTransform`'s own precedent for "reuse the
general-purpose evaluation library directly, do not route through
legacy's `ExtensionTransform`/`StateRegistry` machinery": `compute(this,
inputs, outputs, rc)` receives `inputs` as a `PortNameGraphStateItemMap`
(one `GraphStateItem` per declared port name, i.e. per referenced
variable) and `outputs` with the single `destination_field` entry.
`compute` builds a fresh `ESMF_State`, adds each input's own
`get_field()` under its own port name (same name the expression string
itself uses — D4 — so `StateEval`'s own internal `ESMF_StateGet` lookup by
name resolves correctly with no translation step), then calls
`StateEval(assembled_state, expression, outputs%at('destination_field')%
get_field(), rc)` — the exact same byte-code evaluator legacy's own
`EvalTransform%update` already calls, just supplied a graph-native-
assembled `ESMF_State` instead of legacy's `MultiState%exportState`.
`EvalTransform` itself (`superstructure/generic/transforms/
EvalTransform.F90`) is read only as the faithful behavioral reference for
this assembly step, never called into — the same "independent
Transform, legacy module read-only as reference" relationship Phase 7a's
own D4 established for `BracketTimeInterpolationTransform`/
`TimeInterpolateTransform`.

**D6: `is_ordinary_resolvable_import` drops `MAPL_STATEITEM_EXPRESSION` from its exclusion list; `WILDCARD`/`SERVICE` remain excluded, unchanged.**
A one-line change (removing the `.or. declared_type ==
MAPL_STATEITEM_EXPRESSION` arm). Safe only because D2's hook has, by the
time this check is ever reached for a real expression item, already given
that item a real dependency edge from its own resolved transform chain
(or reported it unresolved, per D2 step 3) — the same "has a real
resolution path now" precondition 7a's own task 6.8 comment already
documents as the reason this exclusion exists at all ("no mechanism moved
it there... restoring a pre-existing invisibility, not introducing a new
one" — this change is the mechanism that now exists). Ordering dependency:
the new hook (D2) MUST run, for a given component, before that
component's own `graphbuilder_unresolved_import_node_ids` scan is invoked
against it by an ancestor — see D7 for the exact call-site placement that
guarantees this.

**D7: The new hook is invoked from `initialize_accept_transfer.F90`, alongside `run_connect_hook` — not from `initialize_advertise.F90`.**
Expression resolution needs every sibling export already advertised and
already assigned a real `NodeId` (`find_export_node_id` reads the
exporting component's own export-port table) — satisfied at advertise
time for a same-component sibling, the only case this change supports
(Non-Goals). It also needs to run before `is_ordinary_resolvable_import`
is ever consulted for one of this component's own expression items by an
ancestor's mirroring step (D6) — `run_propagate_hook`
(`initialize_advertise.F90`) is exactly that consulting step, and it runs
during the *advertise* phase recursion. Placing the new hook in
`initialize_advertise.F90` risks an ordering hazard against sibling
advertisement within the *same* component (self-advertisement completes
before `run_advertise_hook` returns, but a sibling's own export may not
yet be advertised if expression resolution ran too early relative to
`self_advertise`'s own loop). `run_connect_hook`'s own existing timing
(`initialize_accept_transfer.F90`, after the whole hierarchy's advertise
phase — including every sibling's own advertisement — has completed) is
the first point every same-component sibling export is guaranteed
present; placed immediately alongside it (same call site, same timing
rationale: "the real wiring step," matching `run_connect_hook`'s own
module-header "Two-phase timing" framing). This does mean D6's "already
resolvable" state is reached only during the *accept-transfer* phase, not
earlier — `graphbuilder_unresolved_import_node_ids`'s own mirroring scan
(invoked from `run_propagate_hook`, during *advertise*) therefore still
sees an expression item as excluded (D6's `is_ordinary_resolvable_import`
check still returns false for it at that point in the lifecycle, since
the hook has not run yet) — accepted as consistent with Non-Goals (no
cross-level/propagated expression support): an expression item is only
ever resolved against a same-component sibling, so it never needs to be
*mirrored* to an ancestor in the first place; D6's exclusion removal is
really about not blocking something that newly has a resolution path
within its own component, not about enabling cross-level propagation for
it.

## Risks / Trade-offs

- **[Risk] D7's phase placement means `is_ordinary_resolvable_import`
  still reports `EXPRESSION` as excluded during the advertise-phase
  mirroring scan, even after this change lands.** → Mitigation: this is
  provably harmless given Non-Goals (no cross-level expression support is
  being added) — an expression item is never a real candidate for
  mirroring under this change's own scope, so the scan's "treated as
  unresolved-and-propagatable" distinction never actually applies to it
  either way. Flagged explicitly here (not silently left as an apparent
  inconsistency) for a future cross-level-expression follow-on to revisit
  if that scope ever expands.
- **[Risk] D3's new `declared_expression` field duplicates data already
  reachable via `VariableSpec%expression` for the lifetime of advertise-
  time processing, similar in kind to 7a's own D1 duplication risk.** →
  Mitigation: same mitigation 7a's own D1 already accepted — set once,
  unconditionally, from the same source, in the same procedure
  (`populate_characteristics`/`advertise_one`), immediately adjacent to
  the other declared-* fields it already sets.
- **[Trade-off] No support for a referenced variable needing its own
  extension-chain conversion (Non-Goals) is a real functional gap
  relative to legacy, which resolves each reference through full
  `registry%extend()`.** → Accepted for this change's scope: the three
  existing `Test_Scenarios` fixtures (`expression`, `expression_match`,
  `expression_defer_geom`) exercise only same-shape, same-units direct
  sibling references — no fixture currently forces this gap to be closed,
  matching 7a's own "lowest urgency, zero forcing fixture" acceptance
  pattern for its own deferred scope (VectorBracket->Vector).

## Migration Plan

No data migration or rollback concerns — purely additive (one new
`MAPL_StateItem_Flag` constant, one new `Transform` subclass, one new
resolution hook, one narrowed exclusion list) with no change to any
existing stored format, wire protocol, or restart file, and no change to
legacy `ExpressionClassAspect`/`EvalTransform` or the `GRAPH_MODE_REGISTRY`
path. A build containing this change is not required to run in any
particular order relative to other in-flight graph changes. Rollback, if
needed, is a plain revert — no forward-only state is created.
