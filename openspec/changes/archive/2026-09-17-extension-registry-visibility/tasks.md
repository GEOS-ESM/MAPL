## 1. Materialization gate

- [x] 1.1 Add a module-level flag (e.g. in `ExtensionResolution.F90` or a
      small adjacent module in `superstructure/generic/graph/`),
      `logical, save :: materialize_extensions = .false.`, with a public
      getter and a public setter (`set_materialize_extensions(enabled)`)
      (design.md Decisions, "Gate real materialization").
- [x] 1.2 Confirm no production initialization code
      (`initialize_advertise.F90`, `initialize_accept_transfer.F90`, or
      anywhere else in `OuterMetaComponent`) calls the setter — the flag
      must stay `.false.` in every real run unless a test explicitly
      enables it.
- [x] 1.3 Confirm chain *structure* (`NodeId`s, edges, resource-index
      entries — 3c's own existing work) is unaffected by the flag and
      continues to run unconditionally exactly as it does today; only
      the real `FieldCreate` call (task group 2) is gated.

## 2. Field-class materialization

- [x] 2.1 At the point `ExtensionResolution`/`GraphBuilder` (3c) builds
      or reuses an extension chain for a mismatched pair, check the
      materialization gate (task group 1) first — if disabled, leave the
      extension item's payload exactly as 3c already left it
      (unallocated placeholder) and stop; no other task in this group
      applies.
- [x] 2.2 If enabled, check `VariableSpec%itemType` for the export (and
      import) — if it is not `MAPL_STATEITEM_FIELD`, route to task group
      3 (explicit failure) instead of materialization.
- [x] 2.3 Resolve `geom`/`vertical_grid` per design.md's "Geom/vgrid
      resolution order" Decision: (a) use the export's own
      `VariableSpec%geom`/`vertical_grid` if explicitly allocated, else
      (b) the owning `OuterMetaComponent`'s component-wide default via
      its existing public `has_geom()`/`get_geom(rc)`/
      `get_vertical_grid()` accessors, else (c) fail explicitly and
      distinguishably (task group 3 territory) — do not attempt
      cross-component mirror propagation.
- [x] 2.4 For a `Field`-typed pair with geom/vgrid resolved, call
      `mapl_FieldCreate_mod::FieldCreate` (`infrastructure/field/
      FieldCreate.F90`) with `typekind`/`ungridded_dims` taken from the
      export's own `VariableSpec`, `units` taken from the import's
      required value, and `geom`/`vgrid` from task 2.3 — producing a
      real, fully allocated `ESMF_Field` — no `StateRegistry`/
      `StateItemSpec`/`AspectMap` involvement anywhere in this call
      chain (design.md Decisions).

## 3. Non-field item classes / unresolved geom fail explicitly

- [x] 3.1 Implement the explicit, distinguishable failure path for a
      mismatched pair whose item is not `Field`-typed (`Vector`,
      `Bracket`, `VectorBracket`, `Service`, `Expression`, `State`, ...)
      — extends 3c's existing "unregistered characteristic fails
      loudly" posture to also cover "item class this capability cannot
      materialize a payload for" (design.md Decisions).
- [x] 3.2 Confirm the failure message/reason distinguishes this case
      from "no registered provider for this characteristic" (3c) and
      from "no matching export at all" (3b/3b2), matching the existing
      distinguishability requirements those capabilities already
      established.
- [x] 3.3 Implement the same explicit-failure path for task 2.3's case
      (c): neither the `VariableSpec` nor the owning `OuterMetaComponent`
      has a concrete geom/vertical_grid — distinguishable from both
      3.1's "unsupported item class" and 3c's "unregistered
      characteristic" failures.

## 4. Wire the real field into the graph's extension node

- [x] 4.1 Assign the field materialized in task 2.4 to the graph's
      extension `StateItemNode`'s payload via the existing public
      `GraphStateItem%set(field)` / `StateItemNode%set_payload` calls —
      no change to `GraphStateItem`/`StateItemNode`'s public API.
- [x] 4.2 Confirm this runs exactly once per resolved connection and
      does not re-materialize the payload on a reused extension
      (spec.md "Reused extension is not re-materialized") — reuse
      continues to work exactly as 3c already built it (REQ-EXT-005,
      `ComponentGraph`'s resource index), unchanged by this capability.
- [x] 4.3 Confirm `UnitsConverterTransform%compute()`
      (`superstructure/generic/graph/UnitsConverterTransform.F90`) can
      now successfully call `output_item%get_field(rc)` and perform a
      real unit conversion for a `units`-mismatch chain built through
      this path, when the gate is enabled.

## 5. No `StateRegistry` dependency

- [x] 5.1 Verify no code path added by this change references
      `StateRegistry`, `StateItemSpec`, `VariableSpec%make_StateItemSpec`/
      `make_aspects`, or any `ClassAspect` subclass at runtime
      (design.md Context/Non-Goals) — those modules may be read during
      development for reference only, never called into or `use`d by
      the new code.
- [x] 5.2 Verify all new/changed source files are within
      `superstructure/generic/graph/`, `superstructure/generic/
      GraphBuilder.F90`, and one small, additive `OuterMetaComponent`
      accessor (`get_child_outer_meta`, added during implementation —
      see proposal.md Impact revision note), plus the general-purpose
      `infrastructure/field/FieldCreate.F90` dependency — no changes to
      any existing `OuterMetaComponent` accessor,
      `superstructure/generic/registry/`, or
      `superstructure/generic/connection/`.

## 6. Tests

- [x] 6.1 Assert the default-off case: with the gate untouched
      (`.false.`), a `units`-mismatch extension chain's item keeps
      exactly the unallocated placeholder payload 3c already left it
      with, and no `ESMF_Field` is created by this capability — i.e.
      confirm zero behavior change from 3c when the gate is off (design.md
      Goals, "preserve the non-load-bearing shadow invariant").
- [x] 6.2 With the gate explicitly enabled in the test (reset in
      `tearDown`), extend existing extension-reuse pFUnit coverage:
      after resolution, assert the graph's extension `StateItemNode` has
      a real, non-empty `ESMF_Field` for a `units`-mismatch, field-typed
      case, with the correct geom/typekind/units.
- [x] 6.3 Assert `UnitsConverterTransform%compute()` produces a
      numerically correct converted value using the materialized field
      (gate enabled).
- [x] 6.4 Assert no duplicate materialization occurs when a second
      import reuses an already-resolved extension (spec.md "Reused
      extension is not re-materialized"), gate enabled.
- [x] 6.5 Assert the existing "unregistered characteristic fails loudly"
      behavior (3c) is unchanged, gate enabled.
- [x] 6.6 Assert a `Vector`-typed (or other non-`Field`) item mismatching
      on `units` fails explicitly and distinguishably (spec.md "A
      non-field item class fails explicitly"), gate enabled, rather than
      crashing or producing an incomplete payload.
- [x] 6.7 Where practical, extend the `graphbuilder_equivalence`
      real-configuration fixture (3c's own precedent) so its `units`
      mismatch case also asserts the graph-side field is real and
      correctly shaped when the gate is enabled — decision parity (same
      reuse/creation choice as legacy), not byte-for-byte field identity
      with legacy's own extension (design.md Risks).

## 7. Housekeeping

- [ ] 7.1 Rename `UnitsConverterTransform` to `ConvertUnitsTransform`
      once the legacy `mapl_ConvertUnitsTransform_mod` module is removed
      (tracked since 3c; `superstructure/generic/graph/
      UnitsConverterTransform.F90:26`) — only if legacy removal has
      actually landed by the time this change is implemented; otherwise
      leave as a still-open follow-up and do not rename prematurely.
      Confirmed still blocked as of this change's own implementation
      (2026-09-17): legacy `UnitsAspect%make_transform`
      (`superstructure/generic/specs/UnitsAspect.F90:128`) still
      instantiates the legacy type from the live
      `StateRegistry_Extensions_smod.F90` dispatch path. Canonical
      tracking moved to the roadmap's growable cleanup-phase list
      (`docs/graph/spec/20-implementation-roadmap.md` §20.4.2, Phase 6)
      — this task stays open here as a pointer, not a duplicate
      tracker.
