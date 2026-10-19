# standard_name_and_ungridded_dims_mismatch

openspec/changes/phase-12-characteristic-dispatch-wiring, design.md D4:
exercises two of the six newly-wired `StateItemCharacteristic` axes
(`standard_name`, `ungridded_dims`) on one connection - `ungridded_dims`
established and compared (matching on both sides, confirming the
unconditional-insertion axis is live and does not falsely mismatch when
equal) while `standard_name` genuinely mismatches, the "a real
production mismatch on a newly-wired axis is now visible to detection"
case. Neither axis has a `build_transform` provider (see
`StandardNameCharacteristic`/`UngriddedDimsCharacteristic`'s own module
headers), so this is a detection-only fixture, not a
`units_and_typekind_mismatch`-style case with a real adaptation.

`comp_src` exports `T` with `standard_name: air_temperature` and one
ungridded dimension (`foo1`, extent 3); `comp_dst` imports `T` with
`standard_name: surface_temperature` and the same ungridded dimension
(`foo1`, extent 3 - deliberately matching, not mismatching, so this
fixture isolates the `standard_name` axis as the sole mismatch; an
earlier draft mismatched both axes simultaneously, but legacy's own
`UngriddedDimsAspect%can_connect_to` rejects any ungridded-dims
disagreement unconditionally - regardless of `ValidationMode` - which
would hard-fail `process_connections()`/`Connection%connect()` before
`GraphBuilder`'s own resolution ever ran; `standard_name` disagreement,
by contrast, is only rejected by legacy under strict `ValidationMode`,
so it can be exercised permissively while `GraphBuilder`'s own
detection is independently checked). `units` matches (`K`/`K`) on both
sides.

Expected outcome: `GraphBuilder%resolve_connections` detects the
`standard_name` mismatch (`find_mismatched_characteristics` returns
`STANDARD_NAME`), `find_or_build_extension_chain` attempts to build a
chain, `StandardNameCharacteristic%build_transform` fails explicitly
(no real provider), and the connection is reported through
`unsupported_characteristics` with reason `STANDARD_NAME` - not wired
through any chain, and never a fatal `rc` out of
`graphbuilder_run_connect_hook` (caught and logged as a warning, same
as every other `GraphBuilder` lifecycle hook failure). No chain node
and no edge between `comp_src`'s export and `comp_dst`'s import exists
in the graph afterward.

Because the suite this fixture's own test runs in
(`MAPL.generic.scenarios`) forces strict field-dictionary
`ValidationMode` process-wide (`Initialize_strict`), and legacy's
`StandardNameAspect%matches()` independently enforces that same
convention on the identical connection via `process_connections()`
(run unconditionally, even under `GRAPH_MODE_GRAPH` -
`initialize_accept_transfer.F90`'s own header comment), the exercising
test temporarily overrides the field-dictionary config to permissive
for its own duration (save/restore, mirroring `Test_Aspects.pf`'s own
convention) - so legacy accepts the connection (with its own warning)
and `GraphBuilder`'s own, independent detection is what is actually
under test here.

See `Test_GraphBuilder_ConnectionResolution.pf`'s own
`test_standard_name_mismatch_fails_explicitly` for the synthetic-node
unit-level version of this same detection path (asserting the exact
`unsupported_characteristics` reason string), and
`Test_GraphModeDemonstration.pf`'s own
`test_graph_mode_leaves_newly_wired_axis_mismatch_unwired` for this
fixture exercised through the real YAML/init-phase pipeline.
