# units_and_typekind_mismatch

Exercises a two-characteristic (`units` + `typekind`) extension chain in
one connection, isolating "chain exists, correct values once" from
`scenarios/invalidate/`'s own `numsteps: 2` runtime-re-invalidation
fixture, which this fixture deliberately does not reuse or modify
(openspec/changes/phase-10c-typekind-characteristic).

`comp_src` exports `T` as `hPa`/`R4`; `comp_dst` imports `T` as
`Pa`/`R8`. Both mismatches are resolved by one extension chain: a
`UnitsConverterTransform` step (hPa -> Pa) followed by a
`TypeKindConverterTransform` step (R4 -> R8) - matching
`scenarios/invalidate/`'s own observed chain ordering (units first,
then typekind).

Hand-checkable value, chosen to be exact in both precisions (no
rounding ambiguity):

- Export value: `10.` hPa
- After units step (hPa -> Pa, factor 100): `1000.` Pa
- After typekind step (R4 -> R8, same numeric value, different kind):
  `1000.d0` Pa
