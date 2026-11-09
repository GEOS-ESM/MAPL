# MAPL architectural decisions (MAPL3 line)

**Audience:** coding agents and developers working on MAPL and on GEOS gridded components.
**Describes:** the MAPL3 line, branch `develop`.
**Verified against:** `MAPL` @ `1a5a5acf` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `56ce4e8`, GEOSgcm `release/MAPL-v3`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Up: [docs/ARCHITECTURE.md](ARCHITECTURE.md), [AGENTS.md](../AGENTS.md).

Only decisions recorded in this tree are listed: code comments, archived change proposals (`openspec/changes/archive/`), and `docs/mapl3/`. Format per entry:
- Pattern: what the code does (citation). [Verified]
- Likely reason: author's reading. [Inferred]; where the reason is written in a cited design, "Reason (recorded in <path>)" replaces it. [Verified]
- Question for maintainers: optional.

Paths of the form `2026-...` are under `openspec/changes/archive/`.

### Layered directory structure with one library per layer
- Pattern: the top `CMakeLists.txt` adds `enums`, `utils`, `mp_utils`, `pfio`, `infrastructure`, `include`, `base`, `superstructure`, `mapl`, `gridcomps` in that order; layer CMake files carry "Tier N in MAPL v3 restructuring" comments (`utils/CMakeLists.txt`, `mp_utils/CMakeLists.txt`, `infrastructure/esmf/CMakeLists.txt`, `superstructure/CMakeLists.txt`). [Verified]
- Reason (recorded in `infrastructure/CMakeLists.txt`): `MAPL.infrastructure` is "being built up incrementally; subdirectories are absorbed here as their circular dependencies are resolved. See GitHub issue #5014." [Verified]

### Build directory for `superstructure/generic/specs` is named `generic_specs`
- Pattern: `add_subdirectory(specs generic_specs)` (`superstructure/generic/CMakeLists.txt`). [Verified]
- Reason (recorded in the comment above it): a binary directory named `specs` makes GCC, invoked by ifort under the Spack Intel toolchain, read it as a compiler spec file and fail with a misleading error. [Verified]

### Separate internal-checkpoint read and write phases
- Pattern: named phases `GENERIC_INTERNAL_READ_RESTART` and `GENERIC_INTERNAL_WRITE_RESTART` (`enums/GenericPhases.F90`), registered in `superstructure/generic/GenericGridComp.F90`. [Verified]
- Reason (recorded in `2026-08-26-internal-checkpoint/design.md`): unambiguous callback intent; the in-memory behavior can evolve independently of netCDF restart I/O. [Verified]

### In-memory checkpoint stored as a nested `ESMF_State` in `OuterMetaComponent`
- Pattern: `memory_checkpoint` holds nested import, export and internal states, created on first write; the internal write branch clones fields, the read branch copies data into the live fields; the existing `checkpoint_controls` and `restart_controls` flags gate both (`superstructure/generic/OuterMetaComponent/write_restart.F90`, `read_restart.F90`; `2026-08-26-add-memory-checkpoint/design.md`, "Decisions"). [Verified for the branches existing; details from the design]
- Reason (recorded in the same `design.md`): one object to traverse; live fields keep identity so downstream pointers stay valid; one configuration surface for file and memory checkpoints. [Verified]
- Question for maintainers: the design says no production caller triggers the internal phases. On this tree, `GENERIC_INTERNAL_*_RESTART` appears only in `enums/GenericPhases.F90`, `GenericGridComp.F90`, `OuterMetaComponent/write_restart.F90`, `OuterMetaComponent/read_restart.F90` and pFUnit tests (grep), and `CapDriver.F90` calls `driver%write_restart` without a phase index. Is a driver-level trigger planned?

### Grid matching in `GeomManager` tolerates coordinate noise
- Pattern: optional tolerance on LatLon grid equality, read from the `coordinate_tolerance` attribute of `FileMetadata`; ExtData sets it with default 0.1; the geom layer's own default is 0 (`2026-09-15-grid-blurring/design.md`, decisions 1-6; `openspec/specs/geom/latlon-coordinate-tolerance`). [Verified from the design; not checked in code]
- Reason (recorded in the same `proposal.md`): avoid a new `MaplGeom` and a new route handle and regridder for every file of a rolling file collection (GEOS-ESM/MAPL#5385). [Verified]

### `standard_name` and `long_name` kept per connection endpoint
- Pattern: each endpoint stores its names in a per-`ESMF_NamedAlias` namespace of the shared `ESMF_Info`; propagation only in `connect_to_export` (`2026-09-21-field-name-propagation/design.md`; `openspec/specs/generic/field-name-propagation`). [Verified from the design; not checked in code]
- Reason (recorded in the same `proposal.md`): aliased fields share one `ESMF_Info` host, so names declared on the import side were discarded. [Verified] A later change (`2026-09-24-enforce-standard-name-convention/design.md`, decision 2) makes `standard_name` field-wide again, in its own `StandardNameAspect`.

### Expression exports carry names and derive the vertical dimension
- Pattern: `ExpressionClassAspect` gains `standard_name`/`long_name` and a default no-op hook on `StateItemAspect` (`2026-09-21-expression-field-name-propagation/design.md`); vertical stagger is resolved in `check_vertical_stagger_consistency`, called from `create()` (best effort) and `make_transform` (authoritative) (`2026-09-22-derive-expression-vertical-dim-spec/design.md`). [Verified from the designs; not checked in code]
- Reason (recorded in the two `proposal.md` files): declared names were lost on expression exports; users had to restate a vertical dimension fixed by the expression's inputs. [Verified]

### Standard-name convention enforced through the field dictionary
- Pattern: `StandardNameAspect`; mismatch reported in `matches()`, severity chosen by the caller from `FieldDictionaryConfig`, a module-level singleton; dictionary defaulting of `long_name` and `units` is unconditional and the `use_field_dictionary` flag is retired (`2026-09-24-enforce-standard-name-convention/design.md`, decisions 1-5; `openspec/specs/generic/standard-name-enforcement`). [Verified from the design; not checked in code]
- Reason (recorded in the same `proposal.md`): GEOS-ESM/MAPL#5413; a runtime switch relaxes enforcement while the GEOS field dictionary catches up. [Verified]

### Scenario tests run under strict field-dictionary validation
- Pattern: all scenarios run with `EXTRA_INITIALIZE Initialize_strict` as the single ctest `MAPL.generic.scenarios`, using `superstructure/generic/tests/field_dictionary_test.yaml` (`superstructure/generic/tests/CMakeLists.txt`, comment). [Verified]
- Reason (recorded in that comment and in `2026-09-24-use-field-dictionary-in-scenario-tests/design.md`, decision 6): a permissive/`.strict` pair of ctests was used while scenarios were migrated; once all were strict-clean the pair was dropped. [Verified]

### Vertical alignment separate from coordinate direction
- Pattern: `vertical_alignment` (`downward`, `upward`, `with_grid` default) describes memory order, independent of `coordinate_direction` (`docs/mapl3/vertical-alignment.md`). [Unverified: from the guide, not checked in code]
- Reason (recorded in the same guide, "Motivation"): different models store vertical data in different orders. [Unverified]
