## Context

See proposal.md - Why for the confirmed gap. Two code facts shape this
design, both confirmed by direct reading:

- `ExtensionResolution.F90`'s `build_chain` is graph-neutral (module
  header: "depends only on Characteristic/CharacteristicMap/
  CharacteristicId... not on OuterMetaComponent/StateRegistry/
  VariableSpec") - it has no access to a `VariableSpec`, so it cannot
  itself decide what typekind/units/geom an intermediate node's real
  `ESMF_Field` should carry. Only the caller (`GraphBuilder.F90`, which
  already holds both `export_var_spec`/`import_var_spec`) can compute
  that.
- Across `GraphBuilder.F90`'s four `find_or_build_extension_chain` call
  sites, only `resolve_one`'s own ordinary-connection path
  (`build_characteristics`) can ever produce a mismatch set with more
  than one `CharacteristicId` - up to three (`UNITS`, `TYPE_KIND`,
  `VERTICAL_GRID`). The geometry hook's own local maps carry exactly one
  entry (`GEOM_CHARACTERISTIC_ID`) and callback wiring's maps are always
  empty - both call sites' own loops are correct by construction but
  never iterate more than once (geometry) or zero times (callback)
  today. This bounds where a real multi-hop chain can occur today
  without narrowing this change's own scope (the fix is still applied
  uniformly at all four sites, per proposal.md).

## Goals / Non-Goals

**Goals:**
- Give every intermediate link of a multi-step mismatch chain a real,
  allocated payload - not only the chain's own final link - using each
  link's own correctly-evolved typekind/units/geom.
- Keep `ExtensionResolution.F90` graph-neutral: it returns NodeIds and
  chain-order information only; it never computes or receives a
  `VariableSpec`-shaped value itself.
- Make the final link's own materialization a special case of the same
  general per-link formula, rather than maintaining two separate
  formulas that must agree by convention.

**Non-Goals:**
- No change to `11a`'s own export-side aliasing
  (`try_alias_export_field`) or to the chain's own `export_node_id`
  input materialization - both unaffected, per proposal.md.
- No change to `find_mismatched_characteristics`, `chain_key`, or the
  reuse-search mechanism itself - only to what `find_or_build_extension_chain`
  additionally reports about a newly-built chain.
- No attempt to generalize beyond the three `CharacteristicId`s that
  already govern a concrete field-materialization parameter today
  (`TYPE_KIND_CHARACTERISTIC_ID` -> typekind, `UNITS_CHARACTERISTIC_ID`
  -> units, `VERTICAL_GRID_CHARACTERISTIC_ID` -> vertical_grid). A
  future characteristic kind that also needs this treatment is out of
  scope here (see Risks).

## Decisions

**D1: `find_or_build_extension_chain` gains an `intermediate_node_ids(:)`
output, populated only on a genuine new build.**
Add `type(NodeId), allocatable, intent(out) :: intermediate_node_ids(:)`
to `find_or_build_extension_chain`'s signature
(`ExtensionResolution.F90`), parallel to `mismatched_kinds` in order:
entry `i` is the `NodeId` of the extension item that holds the state
after `mismatched_kinds(i)`'s own conversion has been applied - entry
`size(mismatched_kinds)` is therefore always equal to `final_node_id`
(returned as a distinct value for backward compatibility with existing
callers; not a second copy of the array). On the reuse-hit path (an
existing chain found via `get_resource_index`), `intermediate_node_ids`
is returned as a zero-size array - a reused chain's own links were
already fully materialized by whichever call first built it (mirrors
`graph/extension-reuse`'s own existing "Reused extension is not
re-materialized" guarantee); the caller's own job (materialize each
returned id) is correctly a no-op when nothing is returned, with no
separate "is this a reuse" flag needed. On an unsupported-characteristic
failure partway through `build_chain`, `intermediate_node_ids` holds
only the links successfully built before the failure (the failed step's
own orphaned extension node, created but never wired, is deliberately
excluded - `unsupported_characteristic` already signals the caller to
not use any of this output).

Alternative considered: reconstruct intermediate ids from graph
structure on a reuse hit (walking `TransformGraphNode` predecessors
back from `final_node_id`) instead of returning an empty array.
Rejected: unnecessary, since reuse never needs re-materialization at
all; also `Characteristic.F90`'s own module header already notes a
`TransformGraphNode`'s bound `Transform` has no accessor, making a
structural walk fragile for a benefit this design does not need.

**D2: A new `GraphBuilder.F90` helper computes each link's own
"evolved" `VariableSpec`, keyed off `mismatched_kinds`' own prefix.**
`ExtensionResolution.F90` stays graph-neutral (Goals). The caller
already has `export_var_spec`/`import_var_spec`, and already knows
(from `find_or_build_extension_chain`'s own `mismatched_kinds` input,
which it supplied) the resolution order. For link `i`, the materialized
payload's typekind/units/vertical_grid is: `import_var_spec`'s own
value for every `CharacteristicId` in `mismatched_kinds(1:i)` (already
"converted" by that point), and `export_var_spec`'s own value for every
other field - geom is resolved once (`resolve_export_geom`, unchanged)
and reused for every link, since `GEOM_CHARACTERISTIC_ID` never appears
in `build_characteristics`' own mismatch set (Context). This exactly
reduces to today's existing final-node formula at `i =
size(mismatched_kinds)`: every mismatched kind has import's value,
matching `try_materialize_field_extension`'s own current unconditional
`import_var_spec%typekind`/`import_var_spec%units` arguments.

The helper asserts explicitly (`_FAIL`, not a silent fallback) if asked
to resolve a `CharacteristicId` outside the three it knows how to
translate into a `VariableSpec` field - "unregistered characteristic
fails loudly" applies to this translation step too, not only to
`Characteristic%build_transform` itself.

**D3: Replace each call site's own single final-node materialization
call with one loop over the full `intermediate_node_ids` array.**
Since D2's formula already produces the correct final-node payload as
its own `i = size(mismatched_kinds)` case, maintaining a separate
"final node only" call alongside a new "intermediate nodes" loop would
require the two to agree by convention rather than by construction.
Instead, each of the four call sites (`resolve_one`,
`resolve_geometry_from_child`, `push_geometry_to_child`,
`wire_callback_argument`) replaces its existing single
`try_materialize_field_extension(..., final_node_id, ...)` call with one
loop over `intermediate_node_ids` (in order), calling the new helper
(D4) once per entry. For the three call sites whose own mismatch set
never exceeds one entry today (geometry, callback), this loop runs
exactly once (or zero times, callback's always-empty case) - same
behavior as today, expressed as the general case rather than a special
one.

**D4: New shared procedure, `materialize_extension_chain`, replacing
today's single-final-node `try_materialize_field_extension` call at
each of the four sites.**
Signature: `materialize_extension_chain(this, export_comp_name,
export_var_spec, import_var_spec, mismatched_kinds, intermediate_node_ids,
graph, failure_reason, rc)`. Internally: resolve geom/vgrid once
(reusing `resolve_export_geom`/`resolve_export_vgrid`, unchanged), then
loop `i = 1, size(intermediate_node_ids)`, build link `i`'s own merged
`VariableSpec` via D2's helper, and call `materialize_field_extension`
directly (the same primitive `try_materialize_field_extension` already
calls) with that link's own node id and merged values. Stops at the
first link whose materialization fails, returning the same
distinguishable `failure_reason` the existing single-call contract
already produces (unsupported item class / unresolved geom) - the
caller's own existing failure-reporting (`unsupported%push_back`) is
unchanged. `try_materialize_field_extension` itself is left in place,
unchanged, for any caller that still only ever has one node to
materialize (none remain after this change, but removing it is not
required and keeps the diff smaller).

**D5: Test scope.**
`Test_ExtensionResolution.pf` gains assertions that
`find_or_build_extension_chain` returns `intermediate_node_ids` of the
expected length and that the array's own last entry equals
`final_node_id`, for a synthetic two-characteristic mismatch (no real
`VariableSpec`/`GraphBuilder` involvement - this module's own existing
synthetic-node test style, per its module header). A new or extended
`Test_GraphModeDemonstration.pf` fixture (`units_and_typekind_mismatch`,
named in §20.4.9's own gap description) exercises the real,
two-characteristic, run-phase case end to end under
`materialize_extensions_enabled()`, asserting every link's payload is
allocated before any `compute()` call and, as a stretch goal (per
proposal.md's own "Impact" section), the real converted numeric value
reaching the import side.

## Risks / Trade-offs

- **[Risk]** A future `Characteristic` subclass that also needs a
  concrete field-materialization parameter (beyond typekind/units/
  vertical_grid) would hit D2's own explicit `_FAIL` rather than being
  silently mismaterialized → acceptable by design (mirrors "unregistered
  characteristic fails loudly"); whoever adds that subclass must extend
  D2's helper, not work around it.
- **[Risk]** The geometry-hook and callback-wiring call sites' own loops
  are exercised at cardinality 0 or 1 only by any landed test today, so
  this change's real multi-hop test coverage comes entirely from
  `resolve_one`'s own fixture → mitigated by D2/D3's own
  construction-not-convention argument: the three call sites share one
  loop and one helper, so `resolve_one`'s own coverage is evidence for
  all four, not evidence for one with the other three merely assumed
  correct by analogy.
- **[Trade-off]** D3's unification changes all four call sites' own code
  shape (a loop replaces a single call) even though three of them keep
  identical runtime behavior → accepted over a narrower "intermediates
  only, leave final alone" patch, to avoid two formulas that must be
  kept in sync by convention (Goals).
- **[Trade-off]** `materialize_extension_chain` duplicates
  `try_materialize_field_extension`'s own geom/vgrid resolution call
  shape rather than refactoring that procedure to be loop-friendly
  internally → accepted as the smaller, less invasive change; revisit
  only if a fifth call site needs the same shape again.
