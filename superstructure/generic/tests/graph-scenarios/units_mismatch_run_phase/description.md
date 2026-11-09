# units_mismatch_run_phase

Single-characteristic (`units` only - no `typekind` mismatch) extension
chain, deliberately narrower than `units_and_typekind_mismatch`, and
exercised through a real `GENERIC_RUN_USER` phase under
`GRAPH_MODE_GRAPH`.

This fixture has exactly ONE mismatched characteristic (`units`), so its
chain is exactly one hop: `comp_src`'s own export node is the chain's
only input, and the chain's final node is its only link. It isolates a
single `UnitsConverterTransform` executing for real from the multi-hop
chain of `units_and_typekind_mismatch`.

`comp_src` exports `T` as `hPa`; `comp_dst` imports `T` as `Pa`.
Resolved by one `UnitsConverterTransform` step (hPa -> Pa, factor 100).

Hand-checkable value:

- Timestep 1 - export value: `10.` hPa
- After units step (hPa -> Pa, factor 100): `1000.` Pa
