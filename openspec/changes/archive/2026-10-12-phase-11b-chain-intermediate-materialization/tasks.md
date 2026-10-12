## 1. ExtensionResolution.F90 interface change

- [x] 1.1 Add `intermediate_node_ids(:)` as a new `allocatable,
  intent(out)` array argument to `find_or_build_extension_chain`,
  parallel in order to `mismatched_kinds` (design.md D1).
- [x] 1.2 Thread the array through `build_chain`: allocate it to
  `size(mismatched_kinds)` up front, record each link's own
  `extension_node_id` into entry `i` as it is created, truncate it to
  the number of links actually built on an early
  `unsupported_characteristic` exit (D1).
- [x] 1.3 On the reuse-hit path (`existing` found via
  `get_resource_index`), return `intermediate_node_ids` as a
  zero-size array (D1) - do not reconstruct it from graph structure.
- [x] 1.4 Update `Test_ExtensionResolution.pf`: add a synthetic
  two-characteristic-mismatch case asserting
  `intermediate_node_ids` has length 2, its last entry equals
  `final_node_id`, and a reuse-hit second call returns a zero-size
  array (D5).

## 2. GraphBuilder.F90: new per-link materialization helper

- [x] 2.1 Add the `materialize_extension_chain` procedure (design.md
  D4) alongside `try_materialize_field_extension`: resolves geom/vgrid
  once via the existing `resolve_export_geom`/`resolve_export_vgrid`
  helpers, then loops over `intermediate_node_ids` calling
  `materialize_field_extension` once per entry with that link's own
  merged typekind/units (D2's helper, task 2.2).
- [x] 2.2 Add the small internal helper that computes link `i`'s own
  merged typekind/units/vertical_grid from `export_var_spec`,
  `import_var_spec`, and `mismatched_kinds(1:i)` (design.md D2):
  import's own value for every `CharacteristicId` in the prefix,
  export's own value otherwise. `_FAIL` explicitly for any
  `CharacteristicId` outside `UNITS_CHARACTERISTIC_ID`/
  `TYPE_KIND_CHARACTERISTIC_ID`/`VERTICAL_GRID_CHARACTERISTIC_ID`.
- [x] 2.3 Confirm by direct trace (not re-derivation) that this
  helper's own formula at `i = size(mismatched_kinds)` produces
  exactly the same typekind/units values
  `try_materialize_field_extension`'s existing final-node call
  produces today, for every landed fixture that exercises the
  mismatch branch - no behavior change for the single-characteristic
  case.

## 3. Call-site updates (four sites, design.md D3)

- [x] 3.1 `GraphBuilder_ConnectionResolution_smod.F90`'s `resolve_one`:
  replace the mismatch branch's existing single
  `try_materialize_field_extension(..., final_node_id, ...)` call
  (materialize_extensions_enabled() branch) with a call to
  `materialize_extension_chain` passing the new
  `intermediate_node_ids` output from `find_or_build_extension_chain`.
- [x] 3.2 **Corrected during implementation** (discovered gap, same
  precedent as this project's own 3b/3b2 and 4b/4b2): design.md's own
  premise - "replace the existing single
  `try_materialize_field_extension` call" - does not hold here;
  `resolve_geometry_from_child` never called
  `try_materialize_field_extension` at all (its mismatch branch goes
  straight from `find_or_build_extension_chain` to `add_dependency`,
  guarded by an `_ASSERT(unsupported == '', ...'should be
  unreachable'...)`). Confirmed by trace: `GeomCharacteristic%build_transform`
  always fails explicitly (no real horizontal-regrid Transform exists -
  Phase 13/REQ-GEO-002a), so this branch is unreachable in practice and
  there is no existing call to replace. Per explicit user decision
  (expand scope, but no fabricated materialization against data that
  cannot support it): updated only to compile against the new
  `find_or_build_extension_chain` signature (added
  `intermediate_node_ids` output, discarded) and added a comment
  recording why no `materialize_extension_chain` call was added. Real
  materialization here is deferred to a future horizontal-regrid
  capability, not attempted by this change.
- [x] 3.3 `GraphBuilder_Geometry_smod.F90`'s `push_geometry_to_child`:
  same correction and same resolution as 3.2 (symmetric push-direction
  call site, same `_ASSERT`-guarded unreachable branch, same
  `GeomCharacteristic%build_transform` always-fails confirmation).
- [x] 3.4 **Corrected during implementation**, same precedent as 3.2:
  `wire_callback_argument`'s mismatch branch is reachable only when
  `mismatched` is non-empty, which never happens today
  (`empty_characteristics` on both sides - no real
  `CallbackArgumentSpec` declares units/typekind/vertical_grid yet).
  Confirmed by trace, not just by design.md's own prior claim. Updated
  only to compile against the new signature; no
  `materialize_extension_chain` call added - this call site also has no
  `VariableSpec` at all, so `materialize_extension_chain`'s own
  signature could not be called here even if the branch were reachable.
  Deferred to whenever `CallbackArgumentSpec` gains real
  characteristics.
- [x] 3.5 Confirm every one of `find_or_build_extension_chain`'s four
  call sites was updated - no call site left calling the old
  single-final-node `try_materialize_field_extension` path for a
  chain's own final node. Confirmed by grep: the only two remaining
  `try_materialize_field_extension` calls (`resolve_one`'s exact-match
  branch and its chain-input materialization) are unaffected,
  out-of-scope single-node materializations (`export_node_id`, never a
  chain's final node), per proposal.md.

## 4. Real multi-hop demonstration (stretch goal, per proposal.md)

- [x] 4.1 **Already existed** (`openspec/changes/
  phase-10c-typekind-characteristic`): `units_and_typekind_mismatch`
  fixture under
  `superstructure/generic/tests/graph-scenarios/units_and_typekind_mismatch/`
  already declares both a units mismatch (hPa->Pa) and a typekind
  mismatch (R4->R8) between `comp_src`'s export and `comp_dst`'s
  import, and `comp_src.yaml` already has a real `run: {T: [10]}`
  section - no new fixture needed. (Already used, statically only, by
  `Test_GraphEquivalenceHarness.pf`'s own `check[units_and_typekind_mismatch]`
  case - that case never executes a run phase or `compute()`, so it did
  not previously exercise this change's own gap.)
- [x] 4.2 Added `test_graph_mode_propagates_through_two_characteristic_chain`
  to `Test_GraphModeDemonstration.pf`, mirroring the existing
  `test_graph_mode_populates_sibling_states` case's shape exactly but
  using `units_and_typekind_mismatch`: asserts the export proxy (11a's
  scope) and the chain's own intermediate node (this change's own
  scope - reached by walking two predecessor hops back from the
  import's own predecessor, through `TypeKindConverterTransform`'s own
  `TransformGraphNode`, since the chain alternates
  `StateItemNode -> TransformGraphNode -> StateItemNode -> ...`) both
  have real, allocated payloads before any `GENERIC_RUN_USER` phase
  runs.
- [x] 4.3 Stretch goal fully achieved, not merely attempted: the same
  test case runs a real `GENERIC_RUN_USER` phase and asserts the real
  converted numeric value (`10.` hPa/R4 -> `1000.` Pa/R8, both
  conversions composed correctly) reaches `comp_dst`'s own real import
  state. No gap beyond this change's own scope was surfaced - 11a
  (export-side aliasing) and 11b (intermediate materialization)
  together were sufficient, confirming §20.4.10's own prediction
  ("Both landing is what unlocks a real
  units_and_typekind_mismatch-style two-characteristic run-phase
  demonstration").

## 5. Verification

- [x] 5.1 Ran `MAPL.generic.graph` ctest (`Test_ExtensionResolution.pf`
  and the full graph test suite, NAG build): all pass, including the
  new `intermediate_node_ids` assertions and the single-characteristic
  reuse-hit zero-size-array case - no regression in any existing
  single-characteristic-mismatch fixture.
- [x] 5.2 Ran `MAPL.generic.scenarios` ctest (NAG build): all pass,
  including the existing `test_graph_mode_populates_sibling_states`
  (`units_mismatch_run_phase`) case unchanged, alongside the new
  `test_graph_mode_propagates_through_two_characteristic_chain`
  (`units_and_typekind_mismatch`) case (task 4). Also ran the full
  repo-wide `ctest -j4`: 71/78 pass; the 7 failures (`ll-ll`, `cs-cs`,
  `cs-ll`, `ll-cs`, `MAPL3G_Comp_Test_case02`/`11`/`23`) are the
  user-confirmed pre-existing failures, unrelated to any file this
  change touches.
- [x] 5.3 Updated `docs/graph/spec/20-implementation-roadmap.md`
  §20.4.10 to mark 11b landed, mirroring 11a's own landed-entry edit,
  including the discovered scope-correction note (three of the four
  call sites never had a materialization call to extend) and
  confirming the "Recommended order" paragraph's own prediction was
  realized by the task 4 demonstration test.
