# Graph-scenario naming and layout

Rules for fixtures under `superstructure/generic/tests/graph-scenarios/`.
Later ports of legacy scenarios (plan tasks A2b onward) follow this file.
Where the plan and this file disagree, this file wins.

Graph scenarios differ from the legacy `tests/scenarios/` in that the
names say what each part is *for*, so an `expectations.yaml` can be read
without opening the YAML configs.

## 1. Directory layout

```
graph-scenarios/<scenario_name>/
    cap.yaml            root config (the file the test loads)
    comp_src.yaml       exporting child
    comp_dst.yaml       importing child
    comp_mid.yaml       optional intermediate parent (only when the scenario is about hierarchy)
    expectations.yaml   what the ESMF states must contain after init (+ run)
    description.md      what the scenario exercises and why
```

- All three of `cap.yaml`, `expectations.yaml`, `description.md` are
  required. Component files are named for their role.
- `config_file:` entries in `cap.yaml` are relative to the test working
  directory: `graph-scenarios/<scenario_name>/comp_src.yaml`.
- Only files in the scenario directory; shared inputs (field
  dictionary) stay in `tests/scenarios/FieldDictionary.yml` and
  `tests/field_dictionary_test.yaml`.

## 2. Scenario names

`snake_case`, `<what is exercised>[_<outcome or variant>]`. Name the
characteristic axis or structure, not the history of the scenario.

| Pattern | Examples |
|---|---|
| Matching baseline for an axis | `plain_field`, `vertical_field`, `ungridded_dims` |
| Mismatch on axes, with the axes named in canonical order (units, typekind, standard_name, ungridded_dims, ...) | `units_mismatch_run_phase`, `units_and_typekind_mismatch` |
| Hierarchy / structure | `graphbuilder_propagation` |

Add a suffix only when it distinguishes a real variant (`_run_phase`
means the scenario has a run phase and checks values). Do not number
(`scenario_3`), and do not use letters from legacy component names.

## 3. Component names

| Role | Name |
|---|---|
| Root | `cap` (config file) / `<root>` (in `expectations.yaml`) |
| Exporter | `comp_src` |
| Importer | `comp_dst` |
| Intermediate parent | `comp_mid` |
| Several exporters/importers | `comp_src_<role>` / `comp_dst_<role>`, e.g. `comp_src_dyn` |

Legacy `A`/`B`/`child_A` names are replaced when porting.

## 4. Item (short) names

- Single connection: the item has the **same short name on both sides**,
  `T`. Extension outputs appear as `T(1)`, `T(2)`, ... in the exporter's
  own state, in extension order.
- Several connections: pick short names that say what the item is
  (`T`, `PL`, `Q`), identical on both sides of each connection. Do not
  encode component or connection role in the name (`E_A1`, `I_B1`).
- Where a scenario deliberately uses *different* names across a
  connection (testing name matching), say so in `description.md`.

## 5. Standard names

- Every standard name must exist in
  `tests/field_dictionary_test.yaml` (strict validation mode is on for
  `MAPL.generic.scenarios`). Add the entry there, with a one-line
  comment naming the scenario, if none fits.
- Prefer CF-style names already in the dictionary (`air_temperature`,
  `surface_temperature`, `height`). Use the same standard name on both
  sides unless the scenario is about standard-name mismatch.
- Existing fixtures use the placeholder `A1 standard name` (inherited
  from the legacy scenarios); it is allowed there but should not be
  copied into new scenarios.
- A scenario that provokes a standard-name mismatch must run in
  permissive mode (the exercising test saves, overrides and restores the
  field-dictionary config) and say so in `description.md`.

## 6. Units and values

- Choose units with an exact, obvious factor: `hPa` -> `Pa` (x100),
  `km` -> `m` (x1000).
- Where a value matters, put it in `run:` of the exporter
  (`run: T: [42]`, one entry per time step) and make the expected value
  exactly representable in R4 and R8. Say how it is derived in
  `description.md`.

## 7. `description.md`

First line `# <scenario_name>`. Then, in prose:

1. What the scenario exercises (one paragraph).
2. The setup: which component exports/imports what, with
   standard name, units, typekind, dimensions.
3. Expected outcome, including hand-checked values.
4. Anything the checkers do **not** cover (graph structure, expected
   exceptions) and where it is checked instead.

No `openspec/changes/...` paths, task numbers, or change history; no
description of earlier drafts of the fixture (see
`docs/graph/CONVENTIONS.md` §1).

## 8. `expectations.yaml`

Same format as the legacy scenarios, consumed by the same checkers as
`Test_Scenarios.pf` (`status`, `typekind`, `rank`, `standard_name`,
`long_name`, `geom_name`, `value`, `vertical_profile`, `tolerance`,
`class`, `fieldcount`).

- One entry per component, in this order: every child as `<child>/<user>`
  then `<child>` (children in cap order, children of `comp_mid` as
  `comp_mid/comp_dst/<user>`, `comp_mid/comp_dst`), then `<user>`, then
  `<root>`.
  - `<child>/<user>` is the user gridcomp's own states; `<child>` is the
    outer component's states (where extension outputs live); `<root>` is
    the cap's own export state, with children's exports as
    `comp_src/T`.
- Every item gets `status`, `typekind` and `rank`. Add `value` whenever
  the scenario has a run phase, `standard_name` when the scenario names
  an item, `vertical_profile` for vertical regridding.
- Use `status: complete` for items that must be fully allocated and
  `status: gridset` for items intentionally left unconnected.
- Assert the **extension items** (`T(1)`, `T(2)`) explicitly: this is
  how the chain order and intermediate values are recorded.
- A short trailing `# comment` on a line explains a non-obvious
  expectation (`# hPa --> Pa`).
- Add `units` (checked against the field's units) on items whose units
  matter, notably extension outputs.
- Graph structure goes under a `graph:` key on the `<root>` entry:

  ```yaml
  - component: <root>
    export: {...}
    graph:
      nodes: 6
      connections:
        - {export: comp_src/T, import: comp_dst/T, chain: [units, typekind]}
        - {export: comp_src/T, import: comp_dst/T, wired: false}
  ```

  `nodes` (optional) is the number of nodes the root's own graph owns;
  record the value after checking it by hand against the scenario.
  Paths are `<component path>/<item>`, relative to the root. Each
  connection must be one the root declares, since chains live in the
  root's graph.
  `chain` lists the extension steps from export to import (`units`,
  `typekind`; `[]` means a direct edge). `wired: false` asserts there is
  no edge and no predecessor on the import. Checked in graph and both
  modes only.
- A scenario that must raise a pFUnit exception, or must run with a
  permissive field dictionary, says so in its `scenario_table()` entry in
  `Test_Scenarios.pf` (`expected_exception`, `permissive`), not in
  `expectations.yaml`. A scenario with `expected_exception` runs in graph
  and both modes only.

## 9. Status of existing fixtures

The seven fixtures present when this file was written
(`plain_field`, `vertical_field`, `ungridded_dims`,
`units_mismatch_run_phase`, `units_and_typekind_mismatch`,
`graphbuilder_propagation`, `standard_name_and_ungridded_dims_mismatch`)
already use `comp_src`/`comp_dst`/`comp_mid` and the short name `T`, and
now have `expectations.yaml` and `description.md`. They were *not*
renamed: tests refer to their directory names and item names
(`Test_GraphModeDemonstration.pf`, `Test_GraphEquivalenceHarness.pf`).
Their placeholder standard name (`A1 standard name`) and the history in
some `description.md` files are known deviations from §5 and §7.

All seven fixtures are in `Test_Scenarios.pf`'s `scenario_table()`
(scenario name `../graph-scenarios/<name>`, `graph_ready` true) and run
in `registry`, `graph` and `both` modes.
`standard_name_and_ungridded_dims_mismatch` uses `permissive`.
