# units_mismatch_run_phase

Single-characteristic (`units` only - no `typekind` mismatch) extension
chain, deliberately narrower than `units_and_typekind_mismatch`
(openspec/changes/phase-10c-typekind-characteristic). That fixture's
own two-characteristic chain has an intermediate node (between
`UnitsConverterTransform` and `TypeKindConverterTransform`) that
`try_materialize_field_extension` never materializes - only the
chain's final node is ever given a real field (`GraphBuilder.F90`'s
own single call site, keyed to the import's own typekind/units).
Harmless as long as nothing ever executes the chain's own `compute()`
for real (every fixture before this one only checked static
attributes, never a run phase) - but a real `GENERIC_RUN_USER` phase
under `GRAPH_MODE_GRAPH` (openspec/changes/graph-child-state-population)
does execute it, and `UnitsConverterTransform%compute()`'s own
`output_item%get_field()` call on an unallocated intermediate payload
fails outright (`GraphStateItem: esmf_field is not allocated`).

Tracked as a real, confirmed gap - see
`docs/graph/spec/20-implementation-roadmap.md` §20.4.8's own "work
items" / this phase's own follow-up list - not attempted here.

This fixture has exactly ONE mismatched characteristic (`units`), so
its chain is exactly one hop: `comp_src`'s own export node IS the
chain's only link's input, and the chain's final node (materialized by
the existing single `try_materialize_field_extension` call,
unmodified) is also its only intermediate node - there is no
distinct "intermediate, never-materialized" node for a one-hop chain
to expose this gap on. Safe to execute a real run phase against today.

`comp_src` exports `T` as `hPa`; `comp_dst` imports `T` as `Pa`.
Resolved by one `UnitsConverterTransform` step (hPa -> Pa, factor 100).

Hand-checkable value:

- Timestep 1 - export value: `10.` hPa
- After units step (hPa -> Pa, factor 100): `1000.` Pa
