# standard_name_and_ungridded_dims_mismatch

One connection with two characteristics checked: `ungridded_dims` matches
(`foo1`, extent 3, on both sides) and `standard_name` disagrees
(`air_temperature` exported, `surface_temperature` imported). `units`
matches (`K`/`K`). The ungridded dims match because legacy rejects any
ungridded-dims disagreement in every validation mode, which would stop the
run before the graph resolved anything.

The suite (`MAPL.generic.scenarios`) runs in strict field-dictionary
`ValidationMode`, where both legacy and the graph reject a standard_name
disagreement. This scenario runs permissive instead (`permissive=.true.`
in `Test_Scenarios.pf`'s `scenario_table()`).

Expected outcome, permissive: legacy connects with a warning, and the
graph resolves the pair as a match (`StandardNameCharacteristic`
tolerates the disagreement outside strict mode), wires the export
directly to the import, and logs the same warning through the connect
hook. The export's standard_name wins, because the import aliases the
export's field. `expectations.yaml` asserts the edge (`wired: true`).

`Test_GraphBuilder_ConnectionResolution.pf` covers both modes at unit
level: `test_standard_name_mismatch_wires_with_warning_when_permissive`
and `test_standard_name_mismatch_is_conflict_when_strict`.
