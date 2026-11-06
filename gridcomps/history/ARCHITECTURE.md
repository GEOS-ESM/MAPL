# MAPL history gridded component

**Audience:** coding agents and developers working on MAPL diagnostic output.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [gridcomps layer](../ARCHITECTURE.md), [repository architecture](../../docs/ARCHITECTURE.md),
[AGENTS.md](../../AGENTS.md). Sibling: [cap](../cap/ARCHITECTURE.md).

## 1. Purpose

History writes time series of fields from other components to files.
One history component owns one child per output **collection**; each
collection child gathers the requested fields and stages them to an output
server through parallel I/O (`pfio`). For time-averaged or other statistics
collections history also adds a statistics child per collection. MAPL3
history is configured in YAML and builds its structure in `setServices`.

## 2. Place in the hierarchy

- Parent: the cap (`../cap/ARCHITECTURE.md`) loads it from
  `mapl: children: HIST` with `setServices: sharedObj: libMAPL.history`
  (`gridcomps/cap/tests/basic_captest/history.yaml`). The cap runs it last each step and connects
  `src=root, dst=history` with `MAPL_GridCompConnectAll`.
- Children, added in `setServices` for each name in `active_collections`
  (`HistoryGridComp.F90`), in this order:
  - when the collection `time_spec: mode` is present and is not
    `instantaneous`, a statistics child `stats_<child_name>` (`add_stats_gc`,
    `HistoryGridComp_private.F90`), with one `MAPL_GridCompAddConnection` per
    variable to the collection child (called first);
  - the collection child, name from `make_child_name` (each `.` in the
    collection name is escaped as `\.`), via `MAPL_GridCompAddChild(..., collection_setServices, child_hconfig, timeStep=...)`.
- Collection child module: `HistoryCollectionGridComp.F90`; statistics child:
  `gridcomps/statistics/`.

## 3. Entry points

History (`HistoryGridComp.F90`, module `mapl_HistoryGridComp_mod`):
- `setServices`: initialize `GENERIC::INIT_ADVERTISE` (`init`), run `run`; private
  state `History` (the output `server_name`); builds children (above). With no
  `active_collections` it logs a warning and adds no children.
- `init`: when `server` is not the default output server, calls
  `MAPL_ConnectToServer`.
- `run`: `MAPL_GridCompRunChildren(phase_name='run')`, then on the client of
  `server_name`: `done_collective_stage` and `post_wait_all`.

Collection (`HistoryCollectionGridComp.F90`, private state `HistoryCollection`):
- `setServices`: initialize `GENERIC::INIT_GEOM_A` (`init_geom`), initialize
  default phase (`init`), run `run`; registers one import spec per variable
  (`register_imports`).
- `init_geom`: builds the output geometry from the collection's `geom` key and
  vertical grid from `vertical_grid`, if present (`make_geom`, `make_vertical_grid`).
- `init`: makes the output field bundle from the import state, creates the
  parallel-I/O writer for the bundle's metadata (`mapl_make_geom_pfio`,
  `init_with_metadata`), reads `time_spec` (`frequency`, `mode`, optional
  `start`, `stop`), `template`, `run_next_step`.
- `run`: uses the next clock time if `run_next_step`, else the current time;
  skips outside `[start, stop]`; computes the file time stamp
  (`compute_file_timestamp`), expands the file template
  (`mapl_fill_grads_template_esmf`), starts a new file when the name changes
  (`update_time_on_server`), then stages time, coordinates (first record) and
  data to the writer (`stage_*_to_file`).
- Statistics child (`StatisticsGridComp.F90`): initialize
  `GENERIC::INIT_REALIZE_PROVIDED`, run `run`.
- No finalize in any of them.

## 4. State

- Collection imports are declared by `MAPL_GridCompAddSpec` with
  `typekind=MAPL_TYPEKIND_MIRROR` and mirrored ungridded dimensions
  (`add_var_specs`); there are no exports. Field names are the `var_list`
  keys; for statistics collections the name after `/` in the key is used
  (`add_var_specs`).
- Couplings: collection imports are filled by the cap's `ConnectAll` from the
  root (or by the statistics child for statistics variables).
- `var_list` entries: parsed by `parse_item` (`HistoryUtilities.F90`); entry
  forms in tests: `E1: {source: E_1}` (`basic_captest/history.yaml`). Vector
  entries have an alias of the form `[u,v]` (`add_var_specs`).
- Restarts: none (no restart specs in history).

## 5. Configuration that changes structure

History YAML top level (`HistoryGridComp.F90`, `HistoryGridComp_private.F90`):
`active_collections` (list), `collections:` (map), `server` (output-server name;
copied into each collection's config), `run_next_step` (default true; copied to
each collection). Per collection: `var_list`, `template`, `geom`,
`vertical_grid`, `time_spec: {frequency, mode, start, stop, ref_datetime}`,
per-variable `mode`, `typekind`, `units`, `regrid` (`HistoryConstants.F90`
key constants). `time_spec: frequency` becomes the child's time step.
A per-variable `mode` of `instantaneous` in a non-instantaneous collection is an
assertion failure, as is a per-variable `typekind` differing from the collection's.

## 6. Dependencies

Uses `MAPL`, `MAPL.statistics`, `pflogger`; the output server and client types
from the pfio layer (`ClientThread`, `mapl_get_client`,
`MAPL_DEFAULT_OUTPUT_SERVER`). Child routines of the generic layer
(`MAPL_GridCompAddChild`, `MAPL_GridCompAddConnection`).

## 7. Known traps

- A collection name containing `.` is stored with `\.` as the child name;
  other code must use `make_child_name`.
- The collection `run` writes with the time of the *next* step by default
  (`run_next_step` is true unless the history YAML sets it false).
- Collection-level `mode` decides whether a statistics child exists; collections
  with no `mode` get no statistics child (`add_stats_gc` returns early).
- When `ref_datetime` is absent, `add_child_ref_datetime` sets the literal
  `'YYYY-MM-DDT00:00:00'` in the collection config, and `add_stats_gc` (which
  runs first) uses `'YYYY-MM-DDTHH:NN:SS'` for the statistics entries; both are
  placeholder patterns.
- `history/schema.yml` (has `???` placeholders) and `history/collection.yml`
  (keys `time_handling`, `archive`, `file_format`, `regrid_method`) are design
  sketches; the keys the code reads are `geom`, `vertical_grid`,
  `time_spec`, `var_list`, `template` (not `horizontal_grid`, `time_handling`, `fields`).

## 8. Decisions

### One child component per collection
- Pattern: each collection is a child gridcomp with its own time step (`HistoryGridComp.F90`, `setServices`). [Verified]
- Likely reason: the generic layer already schedules children by time step. [Inferred]
- Question for maintainers: is the collection child intended to be the permanent extension point for new output formats?

### Statistics as a separate helper child
- Pattern: history generates a `stats:` list as an in-memory HConfig and adds a statistics child (`add_stats_gc`). [Verified]
- Likely reason: keep statistics reusable outside history. [Inferred]

## 9. Observations (noted, not fixed)

- `setServices` carries a commented-out alternative registration at
  `GENERIC::INIT_REALIZE_ACCEPTED` (`HistoryGridComp.F90`).
- Tests: `history/tests/Test_HistoryGridComp.pf`, `Test_HistoryCollectionGridComp.pf`
  (target `MAPL.history.tests`, added when pFUnit is found); end-to-end cases:
  `gridcomps/cap/tests/*` (each has a `history.yaml`).
- No separate document exists for statistics; see `gridcomps/statistics/` sources.
