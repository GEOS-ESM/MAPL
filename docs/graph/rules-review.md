# Rules review: defaults, connections and intents

Status: owner decisions recorded (session 2026-11-02 and the R2
follow-up, §4a). Review document: the matrix describes the tree as reviewed; landed follow-ups are marked in §4a. §4
keeps the questions as asked, as the record; §4a gives the decisions and
the follow-up tasks (tasks.md 3.28-3.36). Still undecided: whether the
§1 matrix replaces design §5.1 as a stable doc.

Tree reviewed: branch `feature/#5286-graph-phase-5`, 2026-11-02; shape sets, QuantityType and EXPRESSION rows updated at `cc98f77c3`, 2026-11-06.

Numbering: this report's questions are R1-R16 (§4), decisions in §4a. "Q<n>" and "A<n>"
always mean the open questions and owner decisions in `design.md`
(for example Q9 = B3-QuantityType, Q13 = B3-InternalNoMirror).

Terms not in the legacy code, `GLOSSARY.md` or `design.md`:

- **undeclared result**: the status (and value) a graph factory in
  `make_characteristics.F90` gives a characteristic when the `VariableSpec`
  does not set it (advertise time), and what GEOM_B resolution and harvest
  make of it before freeze (§1, two columns).
- **value wildcard**: a SPECIFIED value that the comparison treats as
  "matches anything" (BASIS_NONE; QUANTITY_UNKNOWN was one before 3.36). It differs from the
  UNCHECKED status, which is a status, not a value.
- **adopt**: the import takes the export's value on connect (legacy:
  `connect_to_export` copies it; graph: design §2.2 copy on SPECIFIED x MIRRORED,
  or UNCHECKED export x MIRRORED import, A20).

## 0. One finding that changes several seeds

Legacy `StateItemSpec%connect_to_export` (`specs/StateItemSpec.F90:501`)
calls only the CLASS aspect's `connect_to_export`:

```fortran
src_aspect => export%aspects%at(CLASS_ASPECT_ID, _RC)
dst_class_aspect => to_ClassAspect(this%aspects, _RC)
call dst_class_aspect%connect_to_export(src_aspect, actual_pt, _RC)
```

`StateItemSpec%connect` (`:519`) calls this, then the export's
`connect_to_import` (`:496`, also CLASS aspect only). A grep of
`superstructure/generic` and the whole repo for `connect_to_export(` gives
three groups:

(a) The value aspects held in the outer `AspectMap` are never connected
in production. Nothing in `StateItemSpec` iterates the map for
`connect_to_export`; the CLASS aspect's `connect_to_export` does not call
them either. This covers `QuantityTypeAspect`, `ConservationAspect`,
`NormalizationAspect`, `GeomAspect`, `UnitsAspect`, `TypekindAspect`,
`UngriddedDimsAspect`, `VerticalGridAspect`, `AttributesAspect`, and
`StandardNameAspect` in its outer-map slot (FIELD).

(b) One exception is live in production. `VectorClassAspect%connect_to_export`
(`VectorClassAspect.F90:289-296`) calls
`StandardNameAspect%connect_to_export` on its own per-component
`standard_name_aspects(i)`. So for a VECTOR, each component's
standard_name does adopt the export's when the export declares one
(`StandardNameAspect.F90:213-217`).

(c) Of the group (a) procedures, only four have direct unit-test callers:
QuantityType (`Test_QuantityTypeAspect.pf:241`), Conservation
(`Test_ConservationAspect.pf:176`), Normalization
(`Test_NormalizationAspect.pf:196`) and Geom (`Test_GeomAspect.pf:83`).
Units, Typekind, UngriddedDims, VerticalGrid, Attributes (and StandardName
outside VECTOR) have no caller at all.

Consequences:

- The 3.26 seed "legacy `QuantityTypeAspect%connect_to_export` copies the
  export's quantity type and basis onto the import unconditionally" is
  true of the procedure, but the procedure never runs in production.
- For a FIELD, "the export's standard_name wins" (permissive warning text,
  `StandardNameAspect.F90:161`) holds in effect: the import's payload is
  dropped and it aliases the export's field, where standard_name is a
  field-wide Info key. No value is copied between aspects. For a VECTOR
  the per-component copy also runs (group b).

What legacy production does on connect, for a Field
(`FieldClassAspect%connect_to_export`, `FieldClassAspect.F90:292`, then
`connect_to_import`, `:274`):

1. `this%destroy`, then `this%payload = export_%payload`: the import
   becomes an alias of the export (or of its extension).
2. `mirror_name(this%long_name, export_%long_name)`: the import keeps its
   own declared long_name, else takes the export's.
3. `mirror(this%fill_value, export_%fill_value)` (`:327-348`): the import
   aspect takes the export's fill_value only if the import declared none.
   If both are declared and differ, it logs at info "... (src value wins)"
   but assigns nothing, so the import aspect keeps its own value. The log
   text reads "(dst value kept)", which matches the code.
4. `export%connect_to_import(import)` (`:282-287`): if the export has no
   fill_value, it takes the import's ("import wins (for now)").

What the aspect-level fill_value changes in data: little. `FieldClassAspect%allocate`
(`:241-256`) applies `fill_value` with `FieldSet` only if the field is not
yet COMPLETE. The export side is allocated in `connect_sibling` before
`connect` (`SimpleConnection.F90:184-193`), with the export's own
fill_value. The import aspect's fill_value is then set, but its payload is
the export's already-allocated field, so a later `allocate` returns at
`:250`. So under `SimpleConnection`, by reading, the import's kept
fill_value never reaches the data: the shared field is already COMPLETE,
filled with the export's value if it declared one. Step (4) changes only
the export aspect's stored value, after its field is complete. Not run.
In production `StateItemSpec%connect` has one caller,
`SimpleConnection%connect_sibling` (`SimpleConnection.F90:193`; the
other `%connect(` call is in the test gridcomp `ProtoExtDataGC.F90:180`,
which goes through a connection object). Any other direct caller of
`connect_to_export`, now or later, would need its own review.

Mirror resolution in legacy happens in `make_extension`
(`StateItemSpec.F90:300`): `needs_extension_for` is false when the
destination aspect is a mirror (`StateItemAspect.F90:240`), so no step is
made and the import shares the export's field.

Consequence for §2.8: "should matches-anything also mean adopt?" has no
production precedent in legacy for QuantityType. The owner's "fairly
certain the legacy rule is incorrect" concerns a rule that never ran in
production (R1).

## 1. Rules matrix

Source for the graph columns: the factories in
`superstructure/generic/specs/VariableSpec/make_characteristics.F90`
(cited by procedure) and the `*Characteristic.F90` classes under
`superstructure/generic/graph/characteristics/`. Source for legacy:
`VariableSpec%make_*Aspect` (`specs/VariableSpec.F90:670-970`) and
`specs/*Aspect.F90`.

Shape sets used below (`make_characteristics.F90:684-707`):

- **CFD** (`carries_field_description`): FIELD, BRACKET, VECTOR,
  VECTORBRACKET, FIELDBUNDLE, EXPRESSION.
- **HFM** (`has_field_metadata`): CFD (since 3.12 it delegates to `carries_field_description`, so EXPRESSION is included; HDS still excludes EXPRESSION).
- **HDS** (`has_horizontal_dims_spec`): CFD minus EXPRESSION.

Intents: every intent may declare every characteristic, except that a
mirror request is allowed only on an IMPORT or an EXPRESSION export, and
an EXPRESSION only as an EXPORT (`intent_rule_violation`,
`VariableSpec_private.F90:29`; applied in `make_VariableSpec`,
`VariableSpec.F90:398`, and the backstop `check_intent_rules`,
`check_intent_rules` in `make_characteristics.F90`). No factory branches on intent, except
LongName (IMPORT vs. the rest). So the undeclared columns are the same
for every intent unless the row says otherwise.

"Undeclared" means absent from the user's input: no YAML key in
`parse_var_specs`, and no argument to `make_VariableSpec` (or field
assignment on the `VariableSpec`). Defaults that `make_VariableSpec`
fills in before the factories run do not count as declared. The only such
defaults are the field dictionary's: `apply_field_dictionary_defaults_`
(`VariableSpec.F90:407-421`, `:456-519`) sets `units` and `long_name`
when they were not passed, for every intent, from the dictionary entry
for the declared standard_name, else for the short_name as an alias. It
is skipped for the exempt shapes FIELDBUNDLE, STATE, WILDCARD, EXPRESSION
and SERVICE (`FieldDictionaryConfig%is_exempt`,
`infrastructure/field_dictionary/FieldDictionaryConfig.F90:140-153`). It
sets nothing else (not standard_name, not the three metadata kinds). The
matrix shows the dictionary outcome as a condition in the PhysicalUnits
and LongName rows; every other row is unaffected.

The two undeclared columns: "at advertise" is the factory's status;
"after GEOM_B / before freeze" is what `graphbuilder_resolve_deferred`
(`GraphBuilder_Geometry_smod.F90:91-131`: `resolve_deferred`, then
`drop_unsupplied`) and the connect-hook harvest
(`graphbuilder_harvest_deferred`, `:136`) make of it. Only the four
vertical/geometry kinds override `resolve_deferred`; only VerticalGrid and
VerticalOrientation override `drop_if_unsupplied`
(`StateItemCharacteristic.F90:304-323` defaults: no-op, never drop).

"Compared": the graph's `needs_extension_for`; "Transform": what
`build_transform` returns on a mismatch (UNSUPPORTED = legal, not built
yet; INCOMPATIBLE = can never connect, A23).

"Info write": field-wide (the shared ESMF field's Info) or per-alias
(`is_per_alias()`, written by `sync_per_alias_to_info` on each alias in
`GraphBuilder_StatePopulation_smod.F90:219`).

| Characteristic | Shapes | Undeclared: status at advertise (factory) | Undeclared: after GEOM_B / before freeze | Mirror (graph; where; spelling) | Compared / Transform (graph) | Info write | Legacy, and where it differs |
|---|---|---|---|---|---|---|---|
| ItemShape | all | SPECIFIED, the item class (`make_item_shape_characteristic`); default FIELD from the parser (`to_itemtype.F90:18`) | unchanged | none | `shape_outcome` (`ItemShapeCharacteristic.F90:166`); BRACKET->FIELD builds `BracketTimeInterpolationTransform`; VECTORBRACKET->VECTOR UNSUPPORTED; other mismatches INCOMPATIBLE | bundle type only, structural | CLASS aspect `matches`/`supports_conversion`; see §1.2. Differs: legacy VECTORBRACKET->VECTOR converts (`VectorBracketClassAspect.F90:357`); graph UNSUPPORTED. |
| TypeKind | CFD | SPECIFIED R4 | unchanged | MIRRORED for `MAPL_TYPEKIND_MIRROR`; IMPORT or EXPRESSION | equal; `TypeKindConverterTransform` | field-wide, structural | `TypekindAspect%matches`: equal or exactly one side MIRROR (`TypekindAspect.F90:89`); converts always. Same. |
| PhysicalUnits | CFD | Field-dictionary entry found (non-exempt shape) -> SPECIFIED with its units; otherwise UNCHECKED (also for `'<unknown>'`) (`make_units_characteristic`) | unchanged | none (factory never MIRRORED) | equal; `UnitsConverterTransform` | field-wide, structural | `UnitsAspect%matches`: UNCHECKED or `<unknown>` on either side matches (`UnitsAspect.F90:105-112`); else `are_convertible`. Same in effect. |
| StandardName | FIELD, BRACKET (since 3.27) | UNCHECKED (also `UNKNOWN_STANDARD_NAME`); the field dictionary never supplies it | unchanged | MIRRORED for `MAPL_STRING_MIRROR`; IMPORT only (A13) | equal; differ: strict INCOMPATIBLE, permissive match with a warning (3.27, design A25) | field-wide; UNCHECKED writes `UNKNOWN_STANDARD_NAME` (`StandardNameCharacteristic.F90:126`) | `StandardNameAspect%matches` (`:110`): dst UNCHECKED matches; src UNCHECKED matches with a warning; differ: STRICT rejects, PERMISSIVE matches with a warning. Built for every shape but consulted only via the class's mandatory/order lists (FIELD; VECTOR per component). Same rule in the graph since 3.27. |
| LongName | FIELD, EXPRESSION | Field-dictionary entry found (FIELD only; EXPRESSION is exempt) -> SPECIFIED with its long_name, for every intent. Otherwise: IMPORT -> UNCHECKED, writes nothing; other intents -> the declared standard_name as long_name (SPECIFIED), else UNCHECKED (`make_long_name_characteristic`) | unchanged | MIRRORED for `MAPL_STRING_MIRROR`; IMPORT only | never (`needs = .false.`); UNSUPPORTED is unreachable | per-alias | `FieldClassAspect` carries it, not compared; undeclared import takes the export's (`mirror_name`). No standard_name fallback. Differs: fallback (graph only); undeclared-import inheritance (legacy only; graph gap, tasks.md 4.2). |
| UngriddedDims | HFM (EXPRESSION included, 3.12) | SPECIFIED empty | unchanged | MIRRORED for `ungridded_dims%is_mirror()`; IMPORT only | equal; INCOMPATIBLE | field-wide, structural | `UngriddedDimsAspect%matches` equal; no conversion. Same. |
| Geometry | CFD + SERVICE | DEFERRED; UNCHECKED if `horizontal_dims_spec` NONE (not compared); MIRRORED for EXPRESSION (`make_geometry_characteristic`) | DEFERRED -> SPECIFIED with the component geom at GEOM_B; else stays DEFERRED and may be filled from the realized field (`graphbuilder_harvest_deferred`, connect hook); still DEFERRED at freeze -> error (`check_resolved`). The uncompared UNCHECKED one takes the component geom and stays UNCHECKED (`geometry_resolve_deferred`). Never dropped. | MIRRORED for `mirror_geom` (YAML `geometry_mirror: true`); IMPORT or EXPRESSION | node identity / `mapl_SameGeom` / geom_id; UNSUPPORTED (no regrid before C1) | field-wide, structural | `make_GeomAspect` (`VariableSpec.F90:756`): item geom, else component geom, else item id, else component id (FROM_COMP), else **MIRRORED**. An item's own geom survives the GEOM_A push (private "declared by the item" flag, legacy's stand-in for DEFERRED; fixed 3.38). Differs: undeclared is DEFERRED in graph, MIRRORED in legacy (A12). |
| HorizontalDimsSpec | HDS | SPECIFIED GEOM | unchanged | MIRRORED for `HORIZONTAL_DIMS_MIRROR` (YAML `dims: mirror`); IMPORT only | equal; INCOMPATIBLE | field-wide, structural | Inside `GeomAspect%matches` with `dims_specs_agree` (mirror agrees with any); NONE x NONE matches (A22). Same. |
| VerticalGrid | CFD (and the VERTICALGRID variant) | DEFERRED; MIRRORED for EXPRESSION or `mirror_vgrid`; no entry for stagger NONE (`make_vertical_grid_characteristic`) | DEFERRED -> SPECIFIED from the component vgrid (`vcoord_resolve_deferred`); -> dropped if the component has none (`vcoord_drop_if_unsupplied`, `VerticalGridCharacteristic.F90:210-215`) | MIRRORED for `mirror_vgrid` (YAML `vertical_grid_mirror: true`) or EXPRESSION; IMPORT or EXPRESSION | identity / grid_id / dims set; UNSUPPORTED | field-wide, structural | `make_VerticalGridAspect` (`:854`): item grid, else component grid, else **MIRRORED** (SPECIFIED when the item's stagger is NONE). An item's own grid survives the GEOM_A push (declared flag; fixed 3.38). `VerticalGridAspect%matches` true if dst is mirror. Differs: undeclared DEFERRED vs MIRRORED. |
| VerticalStagger | CFD | DEFERRED; MIRRORED for EXPRESSION or `mirror_vgrid` | component has no vgrid -> SPECIFIED NONE at GEOM_B; with a vgrid stays DEFERRED until harvested from a realized field (`stagger_resolve_deferred`, `VerticalStaggerCharacteristic%harvest`); still DEFERRED at freeze -> error. Never dropped. | MIRRORED for `VERTICAL_STAGGER_MIRROR` (YAML `vertical_dim_spec: MIRROR`); IMPORT or EXPRESSION | equal; UNSUPPORTED | field-wide, structural | Inside `VerticalGridAspect`; unallocated stagger treated as mirror (design §5.1). Differs: undeclared DEFERRED (A17) vs mirror. |
| VerticalOrientation | CFD minus stagger NONE | `with_grid`: SPECIFIED from the item grid, else DEFERRED; MIRRORED for EXPRESSION or `mirror_vgrid` | DEFERRED -> SPECIFIED from the component vgrid's direction (`orientation_resolve_deferred`); -> dropped if the component has none (`orientation_drop_if_unsupplied`, `VerticalOrientationCharacteristic.F90:182-187`) | MIRRORED for `vertical_alignment: mirror`; IMPORT or EXPRESSION | equal direction; UNSUPPORTED | field-wide, structural | Inside `VerticalGridAspect` (alignment). Graph-only split (Q7). |
| Attributes | HFM (EXPRESSION included, 3.12) | SPECIFIED empty | unchanged | none | import's set included in export's; INCOMPATIBLE | none written | `AttributesAspect%matches` includes (`AttributesAspect.F90:77`). Same. |
| QuantityType | HFM (EXPRESSION included, 3.12) | Declared -> SPECIFIED. Undeclared (or declared QUANTITY_UNKNOWN): DEFERRED on EXPORT/INTERNAL, UNCHECKED on IMPORT (`make_quantity_type_characteristic`; landed 3.36) | Unsupplied DEFERRED: non-fatal falls back to UNCHECKED with one summary warning; fatal (freeze error) only with `field_dictionary: require_quantity_type` (`quantity_type_required()`) | none | quantity type equal; basis equal or either BASIS_NONE; INCOMPATIBLE for a quantity-type mismatch, UNSUPPORTED for a basis-only mismatch | field-wide (`quantity_type_metadata`) | `make_QuantityTypeAspect` ignores the declared value: always UNKNOWN (Q9). `matches` same rule. Differs: declared value used in graph only. |
| Conservation | HFM (EXPRESSION included, 3.12) | SPECIFIED CONSERVE_NONE | unchanged | MIRRORED for `conservation%is_mirror()`; IMPORT only | `metadata ==`; INCOMPATIBLE | field-wide | `make_ConservationAspect` ignores the declared value (CONSERVE_NONE). Differs as QuantityType. |
| Normalization | HFM (EXPRESSION included, 3.12) | SPECIFIED NORMALIZE_NONE, scale 1 | unchanged | MIRRORED for `normalization%is_mirror()`; IMPORT only | `metadata ==`; INCOMPATIBLE | field-wide | `make_NormalizationAspect` ignores the declared value. Differs as QuantityType. |
| RestartMode | FIELD, EXPRESSION (3.12) | SPECIFIED RESTART_REQUIRED (A15) | unchanged | none | never (`needs = .false.`) | per-alias | `FieldClassAspect%restart_mode`, written per alias in `FieldClassAspect%add_to_state` (`:498`). Same. |

Notes on the matrix:

- "Info write": `descriptor_sync_to_info` (field-wide, all entries) is called by
  materialization (`sync_to_info(STRUCTURAL)` on the node descriptor);
  `sync_per_alias_to_info` runs in `populate_one`. The column says where
  each entry writes.
- EXPRESSION gets MIRRORED by default for Geometry, VerticalGrid,
  VerticalStagger and VerticalOrientation without asking
  (`requests_mirror` is false for these defaults; `VariableSpec.F90:427`).
  TypeKind is allowlisted for EXPRESSION in design §2.4, but the factory gives it
  SPECIFIED R4 unless `MAPL_TYPEKIND_MIRROR` is declared (design Q3 rule "R4 acts
  as take the consumer's").

### 1.1 Mirror spellings

| Characteristic | `VariableSpec` field | YAML key | Style |
|---|---|---|---|
| TypeKind | `typekind == MAPL_TYPEKIND_MIRROR` | `typekind: mirror` | sentinel value |
| StandardName | `standard_name_mirror` (from `MAPL_STRING_MIRROR`) | `standard_name: '<mirror>'` | string sentinel, stripped into a flag |
| LongName | `long_name_mirror` (from `MAPL_STRING_MIRROR`) | `long_name: '<mirror>'` | string sentinel, stripped into a flag |
| Geometry | `mirror_geom` | `geometry_mirror: true` | flag |
| VerticalGrid | `mirror_vgrid` | `vertical_grid_mirror: true` | flag (also drives Stagger and Orientation when undeclared) |
| HorizontalDimsSpec | `HORIZONTAL_DIMS_MIRROR` | `dims: mirror` | sentinel value |
| UngriddedDims | `ungridded_dims%is_mirror()` | (parser) | type predicate |
| VerticalStagger | `vertical_stagger%is_mirror()` | `vertical_dim_spec: MIRROR` | sentinel value |
| VerticalOrientation | `vertical_alignment == 'mirror'` | `vertical_alignment: mirror` | string, case-folded |
| Conservation | `conservation%is_mirror()` | none (no keyword) | type predicate |
| Normalization | `normalization%is_mirror()` | none (no keyword) | type predicate |

Source: `VariableSpec%requests_mirror` (`VariableSpec.F90:430-447`),
`parse_var_specs.F90:105-137`.

### 1.2 Shape x intent

| Shape | IMPORT | EXPORT | INTERNAL | What enforces it |
|---|---|---|---|---|
| FIELD | yes | yes | yes | - |
| FIELDBUNDLE | yes | yes | yes | - |
| STATE | yes | yes | yes | - |
| VECTOR | yes | yes | yes (statistics internals) | - |
| BRACKET | rejected at declaration (`intent_rule_violation`, both modes; S1, 3.28) | yes | yes (allowed) | `intent_rule_violation` at `make_VariableSpec` and the `check_intent_rules` backstop. Legacy also `_FAIL`s at connect (`BracketClassAspect.F90:285`), now unreachable for a declared import. |
| VECTORBRACKET | rejected at declaration (`intent_rule_violation`, both modes; S1, 3.28) | yes | yes (allowed) | `intent_rule_violation` at `make_VariableSpec` and the backstop. Legacy also `_FAIL`s at connect (`VectorBracketClassAspect.F90:281`), now unreachable for a declared import. YAML: only the root-level `superstructure/generic/to_itemtype.F90` knows `vector_bracket`; the compiled one (`ComponentSpecParser/to_itemtype.F90`, built per `CMakeLists.txt:130`) does not. |
| EXPRESSION | no | yes | no | `intent_rule_violation` at `make_VariableSpec` (both modes) |
| WILDCARD | yes | rejected at declaration (`intent_rule_violation`, both modes; S1 wildcard half, R5, 3.28) | rejected at declaration (same) | `intent_rule_violation` at `make_VariableSpec` and the backstop. Legacy `WildcardClassAspect.F90:56,145` had comments only; `matches` and `supports_conversion_*` are false for a Wildcard export, so it never connected as a source. |
| SERVICE | yes | yes | - | excluded from graph resolution |

Shape pairs (export -> import):

| Export \ Import | FIELD | FIELDBUNDLE | STATE | VECTOR | WILDCARD |
|---|---|---|---|---|---|
| FIELD | legacy match; graph match | - | - | - | legacy match (`FieldClassAspect%matches_a`); graph match |
| FIELDBUNDLE | - | match both | - | - | legacy `matches_a` true (`FieldBundleClassAspect.F90:300-310`), but `WildcardClassAspect%connect_to_export` casts with `to_FieldClassAspect`, which `_FAIL`s 'aspect is not FieldClassAspect' (`WildcardClassAspect.F90:84-95`, `FieldClassAspect.F90:399`); graph skips it (not a member, R6; `resolve_wildcard_destination`) |
| STATE | - | - | match both | - | as FIELDBUNDLE |
| VECTOR | - | - | - | legacy match if per-component standard names match (`VectorClassAspect.F90:136`); graph match (no standard_name entry for VECTOR) | graph INCOMPATIBLE as a pair; legacy no |
| BRACKET | legacy converts (time interpolation); graph converts | - | - | - | - |
| VECTORBRACKET | - | - | - | legacy converts; graph UNSUPPORTED | - |
| EXPRESSION | legacy converts (`ExpressionClassAspect.F90:518`); graph match (expression hook) | - | - | - | - |

Wildcard detail: a Wildcard import is matched by name per export, not as a
shape pair: `resolve_wildcard_destination`
fans in every FIELD export in `build_qualified_export_namespace` (filtered
by `state_intent == EXPORT`, `GraphBuilder_CallbackWiring_smod.F90:67`)
whose qualified name matches the regex (R6, owner (a)). An export of any
other shape (FIELDBUNDLE, STATE, VECTOR, BRACKET, VECTORBRACKET,
EXPRESSION, SERVICE) that matches by name is skipped: one debug log line
per skip, and one warning when every match was skipped (the import is then
unresolved, as for zero matches). A skip is not a connection conflict. The
read-only check `wildcard_import_has_match` uses the same predicate
(`is_wildcard_member_shape`), so the two cannot disagree.

## 2. Irregularities and proposed generalizations

Each: what it is, a proposal, and consequences. Questions are in §4, decisions in §4a.

### 2.1 Mirror mechanisms: flags vs. sentinels

Eleven spellings in five styles (§1.1). Proposal (already owner-scheduled
as later cleanup, design §5.1): one named sentinel per characteristic,
held as the value itself; drop `mirror_geom`/`mirror_vgrid` flags and the
string-to-flag stripping. `requests_mirror` then becomes one loop over
characteristics rather than a hand-kept list.

Consequences: YAML keys `geometry_mirror`/`vertical_grid_mirror` change or
become aliases; legacy `make_*Aspect` must read the new spelling;
`mirror_vgrid` currently also drives Stagger and Orientation when they are
undeclared (design Q-mirror, under Q6), so a sentinel on VerticalGrid alone loses that
coupling unless the factory keeps it. Tests: `Test_VariableSpecCharacteristics`
and the parser tests. No current task is blocked.

### 2.2 Several "Declared by" keywords

Geometry: `geom` and `geom_id`; VerticalStagger: `vertical_stagger` and
`vertical_dim_spec`; HorizontalDimsSpec: `horizontal_dims_spec` and
`dims:`. Owner-scheduled cleanup (design §5.1). No new finding.

### 2.3 DEFERRED vs. UNCHECKED vs. SPECIFIED default for undeclared values

Today, undeclared gives, at advertise (factory status; ItemShape, which
is always declared, left out), when no field-dictionary default applies:

- SPECIFIED built-in default (7): TypeKind R4, UngriddedDims empty,
  HorizontalDimsSpec GEOM, Attributes empty,
  Conservation NONE, Normalization NONE, RestartMode REQUIRED.
- UNCHECKED (3, plus QuantityType on an IMPORT): PhysicalUnits,
  StandardName, LongName (IMPORT, or no fallback).

When the field dictionary has an entry for the item (non-exempt shape),
PhysicalUnits and LongName are SPECIFIED from it instead, leaving one
UNCHECKED kind (StandardName, if not declared).
- DEFERRED (4, plus QuantityType on an EXPORT or INTERNAL): Geometry,
  VerticalGrid, VerticalStagger, VerticalOrientation.
- Value wildcard inside SPECIFIED: QuantityType BASIS_NONE.

After GEOM_B and before freeze, the four DEFERRED ones end as follows
(§1, last undeclared column):

- Geometry: SPECIFIED from the component geom or the realized field;
  else still DEFERRED, and freeze fails.
- VerticalGrid, VerticalOrientation: SPECIFIED from the component vgrid,
  or **dropped** when the component has none.
- VerticalStagger: SPECIFIED NONE when the component has no vgrid; else
  SPECIFIED from the realized field, or still DEFERRED and freeze fails.

The rule behind it (A12): DEFERRED if the component supplies it later;
SPECIFIED if there is a built-in constant; UNCHECKED if neither and the
value is descriptive or "matches anything". The owner's rule "almost all
characteristics default to UNSPECIFIED" has no single counterpart: no
"UNSPECIFIED" status exists. The closest are UNCHECKED (matches anything,
writes `UNKNOWN`/nothing) and DEFERRED (must be supplied before freeze).

Proposal A: read the owner's "UNSPECIFIED" as UNCHECKED for descriptive
kinds and DEFERRED for kinds the component supplies, and record that
mapping as the rule. Then the irregular rows are those with a SPECIFIED
built-in default that is really "no value": UngriddedDims empty,
Attributes empty, QuantityType UNKNOWN, Conservation NONE, Normalization
NONE.

- UngriddedDims empty and Attributes empty are real values (an item with
  no ungridded dims; an import requiring none), so they stay SPECIFIED.
- QuantityType UNKNOWN is a value wildcard. Making undeclared QuantityType
  UNCHECKED instead (and dropping the "either side UNKNOWN" rule) would
  make the status table carry "matches anything", as for Units. A
  *declared* UNKNOWN would then need a spelling, or be treated as
  undeclared.
- Conservation NONE / Normalization NONE: NONE is a real value ("not
  conservable") and compares by equality, so an export declaring
  CONSERVE_MASS and an undeclared import mismatch (INCOMPATIBLE). Whether
  that is wanted depends on Q9's future (field dictionary supplies them).

Consequences: `Test_StatusResolution` and
`Test_VariableSpecCharacteristics` QuantityType rows; legacy unaffected
(legacy ignores the declared value). See R2.

### 2.4 LongName per-intent fallback

LongName is the only factory that reads `state_intent`. Owner decision
recorded: the fallback applies to every intent but IMPORT. The rule's
reason is that an import's own long_name should come from its export
(legacy `mirror_name`), which the graph cannot do until 4.2 or the
"share only data" idea. No conflict with the owner's list, except that the
owner's VariableSpec rule 2 names long_name among the field-dictionary
defaults with no intent qualifier: the field dictionary fills an import's
long_name too (`apply_field_dictionary_defaults_` runs for every intent,
`VariableSpec.F90:413`), so an IMPORT with a dictionary standard_name is
SPECIFIED, not UNCHECKED. Only the standard_name fallback is per intent.

Example (graph, today):

```yaml
import:
  T:
    standard_name: air_temperature   # in the dictionary
# -> long_name SPECIFIED from the dictionary ("Air temperature"),
#    not the export's long_name.
import:
  T2:
    standard_name: my_private_name   # not in the dictionary
# -> long_name UNCHECKED; legacy reads the export's long_name;
#    graph reads 'unknown' (4.2 gap).
```

Legacy has the same first case: the dictionary fills `var_spec%long_name`
before `FieldClassAspect` is built, and `mirror_name` keeps the import's
value. So both paths give a dictionary-backed import its dictionary
long_name, not the export's. Not a question, but worth knowing for the
"share only data" analysis.

### 2.5 EXPRESSION exemptions

EXPRESSION differs from FIELD in: no HorizontalDimsSpec entry (since 3.12
UngriddedDims, Attributes, QuantityType, Conservation and Normalization
have one); default MIRRORED
Geometry/VerticalGrid/VerticalStagger/VerticalOrientation; MIRRORED allowed
on export (design §2.4 allowlist); field dictionary exempt (`is_exempt`); legacy
`connect_to_export` is a no-op (`ExpressionClassAspect.F90:232`). All
follow from "an expression is a recipe with no payload of its own"
(design Q2/Q3).
Proposal: state that once in the stable doc as the reason, and stop
listing EXPRESSION per factory. No behavior change.

### 2.6 Rules in the graph only, or in legacy only

Graph only:

- LongName standard_name fallback (A8).
- QuantityType/Conservation/Normalization use the declared value (Q9).
- Undeclared Geometry/VerticalGrid/VerticalStagger are DEFERRED, not
  MIRRORED (A12, A17); freeze fails if never supplied.
- A second connection to an already-settled import is silently skipped
  (`resolve_one`, `GraphBuilder_ConnectionResolution_smod.F90:234`, "first
  wins").

Legacy only:

- PERMISSIVE standard_name mismatch connects (3.27).
- An undeclared import inherits the export's long_name (4.2).
- fill_value (§0 steps 3-4): on connect, an import aspect with no
  fill_value takes the export's; when both declare one, the import aspect
  keeps its own (despite the "src value wins" log text); an export with
  none takes the import's (`FieldClassAspect%connect_to_import`, `:282`,
  "import wins (for now)"). In the shared field's data, by reading, the
  export's value wins when it has one (the field is allocated before
  connect). No graph characteristic or GraphBuilder code handles
  fill_value (grep of `superstructure/generic/graph` and
  `GraphBuilder*.F90`). Not in the 3.26 seeds; see R14.
- *(Landed in 3.33: `run_reexport_hook` / `reexport_one`, `GraphBuilder_ConnectionResolution_smod.F90:919-970`, gives a re-export an export port.)* At review time, re-export (`ReexportConnection`) was resolved by the registry only; the
  graph skipped it (`class default`,
  `GraphBuilder_ConnectionResolution_smod.F90:977`). Export propagation
  (`run_export_propagate_hook`) covers child exports, but an
  INTERNAL->EXPORT re-export has no graph path: internals get no port
  (`GraphBuilder_Advertising_smod.F90:115`). Scenarios using it:
  `scenario_1`, `scenario_2`, `propagate_geom` (child_A, `src_intent:
  internal`). See R9, R11.
- Geometry at GEOM_A (fixed, 3.38): `registry%set_geometry` pushes the
  component geom / vertical grid with `set_component_geom` /
  `set_component_vertical_grid`, which are no-ops on an aspect the item
  declared itself (a private "declared by the item" flag set by
  `make_GeomAspect` / `make_VerticalGridAspect`; legacy's stand-in for the
  graph's DEFERRED, A12). Item wins, as in the graph. The item's `geom_id`
  follows the geom it ends with unless the item declared its own `geom_id`
  (an item declaring only a `geom_id` takes the component geom and keeps its
  id). The explicit `MAPL_GridCompSetGeometry` replaces even a declared value
  and marks the item declared, so a later component push leaves it alone. A
  component geom still replaces an earlier component geom. Pinned by
  `Test_Couplers.pf`, `Test_GeomAspect.pf`,
  `Test_VerticalGridAspect_NoneMatch.pf`. R16.

### 2.7 Rules found late

- Q13 (internal never mirrors) and "expression only as export" were found
  in 3.25, after their characteristics landed. Both are intent rules; now
  one function (`intent_rule_violation`). Proposal: the stable doc's matrix
  gets an "intents" column per characteristic and a shape x intent table
  (§1.2), so the next intent rule is checked against every row when it is
  added.
- Q13 is not in the owner's rules list (owner list MIRROR section covers
  exports and expressions only). Proposal: add "INTERNAL: no mirror of any
  kind" to the list (R4).

### 2.8 Value wildcards and adopt-on-match

QuantityType UNKNOWN and BASIS_NONE match anything (graph and legacy
`matches`, identical). Neither path adopts in production (§0): legacy
`QuantityTypeAspect%connect_to_export` is reached only by its unit test
(§0 group c).
After connect, legacy's import shares the export's field, so the import
*reads* the export's `quantity_type_metadata` from the shared Info (and in
legacy both are always UNKNOWN anyway, Q9). The graph import is an alias
of the export's field too, so the import also reads the export's. The question "should matches-anything also mean
adopt?" therefore only matters if import and export stop sharing a field
("share only data"), or for per-alias kinds.

Proposal: no adopt rule now. A value wildcard is "match, no step"; what the
import reads is whatever the shared field holds. Revisit with the "share
only data" analysis, where every field-wide kind needs an adopt/keep rule
anyway. See R1.

### 2.9 `bootstrapped` (develop #5470) vs. RestartMode

- `RestartHandler%read` (`RestartHandler.F90:100`) drops RESTART_SKIP
  fields by reading the per-alias `restart_mode` of the handle in the
  state being read (`remove_skip_restart_`, `:271-288`), then marks each
  remaining field missing from the file with field-wide
  `bootstrapped=.true.` (`remove_missing_field_`, `:263-266`), or fails if
  `bootstrap` is off.
- Called for the import state and the internal state only
  (`initialize_read_restart.F90:47,54`).
- Readers of `bootstrapped`: `MAPL_FieldGet(bootstrapped=)` /
  `FieldGet` (`infrastructure/field/FieldGet.F90:123`); used only in
  `Test_RestartBootstrap.pf` and `Test_FieldInfo.pf`. No production reader.
- Interaction: `bootstrapped` is field-wide; RestartMode is per alias. One
  ESMF field with an export alias (REQUIRED, not read: exports are not
  restart-read) and an import alias (REQUIRED, read) gets
  `bootstrapped=.true.` field-wide if the import's file lacks it; the
  export alias then also reports bootstrapped. Two imports of one export
  in different components (A15: they may differ) with one SKIP and one
  REQUIRED: the REQUIRED one's read can set `bootstrapped` on the field the
  SKIP one also sees.
- The graph has no `bootstrapped` notion and does not need one for
  connection resolution: it is a runtime fact set after INIT, not a
  declared characteristic. Proposal: not a characteristic; keep it as Info
  owned by RestartHandler. Decide whether it should be per alias like
  RestartMode (R15).

### 2.10 One export per import

- Legacy: the check is commented out (`SimpleConnection.F90:105`,
  `!# _ASSERT(.not. spec%is_active(), 'Imports can only be activated by one connection.')`).
  By reading, a second SimpleConnection to the same import runs
  `connect_sibling` again and `FieldClassAspect%connect_to_export`
  replaces the payload, so the last connection wins. Not run.
- Graph: `resolve_one` returns early for a settled import, so the first
  connection wins and the second is silently ignored. Not reported.
- `DependencyNetwork` `validate` asserts at most one producer
  (`DependencyNetwork.F90:368`) and the demand-driven update does too
  (`ComponentGraph_DemandDrivenUpdate.F90:135`), but these count producer
  operation nodes, not connections; the silent skip means a second edge is
  never made, so they do not fire. Not verified as a full guarantee.

Neither path enforces the owner's connection rule 5; each silently picks
one. See R8.

### 2.11 SimpleConnection intents are not validated

`parse_connections` (`parse_connections.F90:57-70`) reads `src_intent` and
`dst_intent` from YAML (defaults export/import). `dst_intent: export`
gives a ReexportConnection; anything else a SimpleConnection, with no
check that src is EXPORT and dst is IMPORT. So `src_intent: internal,
dst_intent: import` (internal feeding a sibling's import) parses. Owner
rule "a simple connection is always src EXPORT, dst IMPORT" is unenforced.
`ComponentSpec%reexport` (`ComponentSpec.F90:139-148`) allows src EXPORT or
INTERNAL. See R10.

## 3. Owner's rules list, checked against source

Each rule: violations in source or legacy, and use cases against it.

### Connections

C1. *A simple connection is always src EXPORT, dst IMPORT.*
- Unenforced (§2.11). No scenario found that uses another pair for a
  SimpleConnection; all `src_intent: internal` uses are re-exports
  (`dst_intent: export`). No use case against.

C2. *MatchAll uses SimpleConnection under the hood.*
- Holds: `MatchConnection%activate`/`connect` build one `SimpleConnection`
  per filtered pair (`MatchConnection.F90:109,166`). The graph resolves
  both through the same `resolve_match_connection`. Note:
  `MatchConnection%activate` `_FAIL`s when an import pattern has no
  export (`MatchConnection.F90:98`); the graph reports it as unresolved.

C3. *Reexport is not really a connection; it exposes an INTERNAL item in
the EXPORT state.*
- Partly violated: `ReexportConnection` also re-exports a child's EXPORT
  under the parent (`scenario_reexport_twice/parent.yaml`,
  `src_intent: export`), and `ComponentSpec%reexport` defaults
  `src_intent` to export. So it exposes an EXPORT *or* INTERNAL item.
- Mechanism matches the rule: `connect_export_to_export` adds the source's
  extension family under the new virtual point
  (`ReexportConnection.F90:116-117`): no extension, no comparison.
- Graph: not handled at all at review time (§2.6); landed in 3.33 (`reexport_one`). See R9, R11.

C4. *An EXPORT may connect to multiple IMPORTs with differing
characteristics.*
- Holds in both: each import gets its own extension chain (legacy
  `make_extension` per dst; graph per pair). Exception by design: MIRROR
  on an export (A1) would need cloning, hence disallowed.

C5. *An IMPORT connects to only one EXPORT, except Wildcard.*
- Unenforced in both; each silently keeps one (§2.10). Wildcard holds.

### VariableSpec

V1. *Almost all characteristics default to UNSPECIFIED.*
- No "UNSPECIFIED" status. At advertise, when no field-dictionary
  default applies, undeclared gives SPECIFIED defaults for eight kinds,
  UNCHECKED for three, DEFERRED for four; two of the DEFERRED ones may be
  dropped at GEOM_B (§2.3). R2.

V2. *Exceptions: typekind R4; field-dictionary items (units, long_name,
physical dimensions, conservative regridding?).*
- typekind R4: holds.
- units, long_name: filled by `apply_field_dictionary_defaults_` (by
  standard_name, else short_name as alias) for non-exempt shapes; holds.
- physical dimensions: not used as a default anywhere
  (`FieldDictionaryItem%get_physical_dimension` has no caller in
  `superstructure/generic`).
- regridding: `get_regrid_param` (`VariableSpec.F90:569`) reads the
  dictionary's regrid method but has no caller; the regrid parameter is
  never defaulted from the dictionary. Out of scope; possible dead code.
- Undeclared RestartMode REQUIRED, UngriddedDims empty, Attributes empty,
  HorizontalDimsSpec GEOM, QuantityType UNKNOWN, Conservation/
  Normalization NONE are further exceptions not on the list. R2.

V3. *A system-wide setting for enforcing standard_name on Fields.*
- Does not exist. Closest: `ValidationMode` (default PERMISSIVE,
  `FieldDictionaryConfig.F90:92`) governs a declared name missing from the
  dictionary and a mismatch on connect; nothing requires a FIELD to
  declare one. Fold into 3.27 (R13).

### ItemShape

S1. *Bracket and VectorBracket are never IMPORT; Wildcard is only IMPORT.*
- Legacy: bracket imports fail only at connect (`_FAIL` in `connect_to_export`).
  Graph: enforced at declaration by `intent_rule_violation` (3.28, R5), in
  both modes: a BRACKET or VECTORBRACKET import is rejected, and a WILDCARD
  export or internal is rejected (the wildcard half is now enforced too).
  INTERNAL bracket stays allowed (owner: conceivable).

S2. *Expression is only ever EXPORT.*
- Enforced in `make_VariableSpec` and the backstop (3.25).

S3. *Composites generally may be IMPORT and EXPORT.*
- Holds for FIELDBUNDLE, STATE, VECTOR.

S4. *Like with like, except Bracket/VectorBracket -> Field via time
interpolation; Expression -> Field; Field -> Wildcard.*
- VectorBracket: legacy converts VectorBracket -> **Vector**
  (`VectorBracketClassAspect.F90:357`), not -> Field; graph marks
  VECTORBRACKET -> VECTOR UNSUPPORTED and -> FIELD INCOMPATIBLE. The rule
  as written ("-> Field") disagrees with both. Likely a slip in the list
  (R7).
- Wildcard: legacy `matches_a` lets FIELDBUNDLE and STATE match a Wildcard
  (`FieldBundleClassAspect.F90:300-310`, `StateClassAspect.F90:287-297`),
  but `WildcardClassAspect%connect_to_export` `_FAIL`s for them
  (`WildcardClassAspect.F90:84-95`), so legacy production is Field-only.
  The graph is Field-only too (R6, 3.30): it skips a non-Field match
  instead of failing.
- Expression -> Field: holds (both).

### MIRROR

M1. *MIRROR on exports is problematic, not disallowed.*
- Conflicts with A1/A21 as implemented: a mirror request on a non-EXPRESSION
  export is a hard error at `make_VariableSpec`
  (`'mirror is only valid on an import or an expression export'`), in both
  graph and legacy-only modes. R3.

M2. *Supporting it needs export cloning; no compelling use case.*
- Consistent with A1's wording ("further entries need a compelling,
  demonstrated use case"). The conflict with M1 is only "disallowed" vs.
  "problematic"; the code treats it as disallowed.

M3. *MIRROR on expression exports is fine.*
- Holds (design §2.4 allowlist, `intent_rule_violation`).

(Missing from the list: Q13, INTERNAL never mirrors; §2.7, R4.)

### Per-alias

P1. *Import and export may differ in standard/long names, restart mode,
etc.*
- long_name and restart_mode: per-alias in both paths; holds.
- standard_name: field-wide in both (legacy `StandardNameAspect%update_payload`
  writes `mapl_FieldSet(field, standard_name=)`, `:326`; graph
  `StandardNameCharacteristic%write_info`, `:131`; landed spec
  `field-name-propagation`: "SHALL be stored as a single field-wide
  value"). An import and export *can declare* different names (PERMISSIVE
  connects), but cannot *hold* different ones: they share one field. This
  is the known conflict (tasks.md 3.26 item 3). R12.
- "etc." Other kinds that could differ per item (units, typekind, geom)
  differ by getting their own extension field, not by alias. fill_value is
  held per aspect in legacy (an import aspect keeps its own declared
  value), but the data it fills is the shared field's, which the export
  allocated first (§0, R14).

P2. *The import is an alias of the export (or of its extension), so
per-alias ids are needed. Consider sharing only data.*
- Holds as a description (`FieldClassAspect%connect_to_export` payload
  replacement; graph `populate_one` `MAPL_NamedAlias`). The "share only
  data" analysis is not scheduled (plan, after B4). R12 option (c).

## 4. Open questions for the owner session

Ordered so that earlier answers feed later ones: value rules (R1-R2),
mirror rules (R3-R4), shape rules (R5-R7), connection rules (R8-R11),
per-alias rules (R12-R15), then the one that waits on a probe (R16). Each
is one decision.

YAML examples leave out the `mapl: states:` nesting. Every key they use
is one the parser accepts (`parse_var_specs.F90:105-165`,
`to_itemtype.F90`, `parse_geometry_spec.F90`, `parse_connections.F90`).
Settings with no YAML key are shown in their `VariableSpec` form.

**R1. Value wildcards: should "matches anything" also mean "adopt the
export's value"?**
Legacy: `QuantityTypeAspect%connect_to_export` copies the export's
quantity type and basis, but it never runs in production (§0 group a);
`make_QuantityTypeAspect` makes both sides UNKNOWN anyway (Q9). So legacy
has no production adopt rule. Graph: QUANTITY_UNKNOWN and BASIS_NONE match
anything and nothing is copied (`QuantityTypeCharacteristic.F90:87-93`).
The import aliases the export's field, so once B4 writes field-wide
entries it reads the export's value from the shared Info. Example (the
YAML key `quantity_type:` and the `make_VariableSpec` keyword
`quantity_type=` set it; the legacy aspect ignores both):

```fortran
! component A, export E
spec_a = make_VariableSpec(ESMF_STATEINTENT_EXPORT, 'E', units='K', &
     quantity_type=QUANTITY_TEMPERATURE, _RC)
! component B, import E: quantity_type left at QUANTITY_UNKNOWN
spec_b = make_VariableSpec(ESMF_STATEINTENT_IMPORT, 'E', units='K', _RC)
! legacy: both UNKNOWN (declared value ignored); connects; nothing copied
! graph: connects; B's descriptor stays UNKNOWN; the shared field's Info
!        says temperature after B4
```

- (a) No adopt rule. A value wildcard is "match, no step". Correct the 3.26
  seed and design §4.3 / L487 so they no longer describe the
  `connect_to_export` copy as legacy behavior. No code change.
- (b) Add an adopt rule for value wildcards (UNKNOWN import takes the
  export's value on connect, like SPECIFIED x MIRRORED). Changes
  `Test_StatusResolution`; the descriptor then disagrees with what the
  import declared, and an UNKNOWN that meant "I do not care" becomes a
  claim.
- (c) Decide with "share only data": if imports stop sharing the export's
  field, every field-wide kind needs an adopt/keep rule anyway.
Recommendation: (a) now, revisit under (c). (Deleting the uncalled legacy
procedures is not part of this question; see §5.)

**R2. What "UNSPECIFIED by default" means.**
Legacy: no single rule; undeclared gives MIRRORED geom/vgrid, UNCHECKED
units/standard_name, fixed defaults elsewhere. Graph: §2.3 (at advertise,
with no field-dictionary default, SPECIFIED defaults for eight kinds,
UNCHECKED three, DEFERRED four).
Example:

Not expressible in YAML today (no conservation key); `VariableSpec`
form, as in `Test_GraphBuilder_Advertising.pf:457`:

```fortran
spec_a = make_VariableSpec(ESMF_STATEINTENT_EXPORT, 'E', units='kg', _RC)
spec_a%conservation = ConservationMetadata(CONSERVE_MASS, is_conservable=.true.)
spec_b = make_VariableSpec(ESMF_STATEINTENT_IMPORT, 'E', units='kg', _RC)
! B undeclared -> SPECIFIED CONSERVE_NONE
! graph: INCOMPATIBLE (Conservation); legacy: connects (ignores both)
```

- (a) UNSPECIFIED = UNCHECKED for descriptive/value-wildcard kinds and
  DEFERRED for component-supplied kinds. Change undeclared QuantityType,
  Conservation, Normalization to UNCHECKED (and drop QuantityType's
  UNKNOWN/NONE value wildcard). Keep TypeKind R4, RestartMode REQUIRED,
  UngriddedDims/Attributes empty, HorizontalDimsSpec GEOM as listed
  exceptions. Test changes in 3.10's tests; no legacy change.
- (b) Keep today's table; add the eight SPECIFIED defaults to the owner
  list as exceptions.
- (c) (a) but only for QuantityType; Conservation/Normalization wait for
  the field-dictionary defaults (Q9 future).
Recommendation: (c). It removes the only value wildcard; Conservation and
Normalization have no YAML key, so no user sees their default yet. If R1
is (b), revisit this.

**R3. MIRROR on a non-EXPRESSION export: hard error or not?**
Legacy: before A21, `StateItemAspect%can_connect_to` allowed one mirror
side (`case (1)` true) and the table comment marks mirror-export
connections "?" (`StateItemAspect.F90:12-14`). Graph/design: A1/A21 make a
mirror request on a non-EXPRESSION export an error at `make_VariableSpec`,
in both modes. Owner list M1: "problematic, not disallowed". Example:

```yaml
export:
  Q: {typekind: mirror}
# today: error "mirror is only valid on an import or an expression export: Q"
```

- (a) Keep the hard error; reword M1 to "disallowed until a use case and
  export cloning exist". No code change.
- (b) Downgrade to a warning and leave Q MIRRORED; the graph then reports
  it unresolved. Loosens 3.19b/3.25 tests; legacy would accept it and its
  behavior with two consumers is undefined (comment ^1).
- (c) Implement export cloning now. Large; no use case.
Recommendation: (a).

**R4. Add "INTERNAL never mirrors" (Q13) to the owner's rules list?**
Legacy: no rule; a mirror request on an internal was accepted (before
3.25). Graph/design: Q13 and A21, enforced in `intent_rule_violation`
(VariableSpec only, owner decision). The owner list's MIRROR section
covers exports and expressions only. Example:

```yaml
internal:
  Z: {typekind: mirror}
# today: error "mirror is only valid on an import or an expression export: Z"
```

- (a) Add it as M4: "INTERNAL: no mirror of any kind; a re-exported
  internal is mirrored, if at all, through its export". List only.
- (b) Leave the list as is; Q13 stays a design.md rule only. Risk: the
  next review against the list misses it again (§2.7).
Recommendation: (a).

**R5. Shape x intent rules: enforce at declaration?**
Legacy: BRACKET/VECTORBRACKET imports fail only at connect
(`BracketClassAspect.F90:285`, `VectorBracketClassAspect.F90:281`); an
unconnected one is never rejected. WILDCARD exports/internals are never
rejected (comments only, `WildcardClassAspect.F90:56,145`). Graph: no
check; a BRACKET import gets INCOMPATIBLE at connect. Example:

```yaml
import:
  B: {class: bracket}       # accepted at advertise in both paths
export:
  W: {class: wildcard}      # accepted; never connects
```

- (a) Add "BRACKET/VECTORBRACKET not IMPORT" and "WILDCARD only IMPORT" to
  `intent_rule_violation` (one place, both modes). Small; a scenario grep
  finds no BRACKET import and one Wildcard (an import,
  `history_wildcard/collection_1.yaml:5`). INTERNAL BRACKET stays allowed
  (owner: conceivable).
- (b) Graph only: report them at advertise in graph mode; legacy keeps its
  connect-time failure.
- (c) Leave as is.
Recommendation: (a).

**R6. Which export shapes may a Wildcard import accept?**
*Resolved: option (a), landed in 3.30 (graph members are FIELD only).*
Legacy: `FieldBundleClassAspect%matches_a` and `StateClassAspect%matches_a`
accept a Wildcard (`FieldBundleClassAspect.F90:300-310`,
`StateClassAspect.F90:287-297`), but `WildcardClassAspect%connect_to_export`
casts the export with `to_FieldClassAspect`, which `_FAIL`s for anything
but a Field (`WildcardClassAspect.F90:84-95`, `FieldClassAspect.F90:399`).
So legacy production accepts FIELD only, and a matching bundle is a hard
failure. Graph: `resolve_wildcard_destination` adds every export whose
name matches, with no shape filter. Example:

```yaml
import:
  'A/.*': {class: wildcard}
# A exports T (field), B1 (bundle), V (vector)
# legacy: fails on B1 ('aspect is not FieldClassAspect')
# graph: T, B1 and V all become members
```

- (a) FIELD only, in the graph too (legacy production parity); a matching
  non-Field export is skipped (or reported). Small graph change.
- (b) FIELD, FIELDBUNDLE and STATE (the `matches_a` behavior); the legacy
  cast is a bug, fixed in a small legacy task. Needs a History use case.
- (c) Leave the graph accepting any shape.
Recommendation: (a), unless History has a bundle use case, then (b).

**R7. Correct the owner list's "VectorBracket -> Field" to "-> Vector"?**
Legacy: `VectorBracketClassAspect` converts to a Vector by time
interpolation (`VectorBracketClassAspect.F90:357`); there is no
VectorBracket -> Field path. Graph: VECTORBRACKET -> VECTOR is UNSUPPORTED,
-> FIELD INCOMPATIBLE (`shape_outcome`). Owner list S4 says "Bracket /
VectorBracket -> Field".

- (a) Yes: S4 reads "Bracket -> Field and VectorBracket -> Vector, via
  time interpolation". List only; the graph's UNSUPPORTED stays a known
  gap.
- (b) No: VectorBracket -> Field is intended; then both paths lack it and
  it needs a design (which component?).
Recommendation: (a).

**R8. One export per import: enforce?**
Legacy: the check is commented out (`SimpleConnection.F90:105`); by
reading, the last connection wins. Graph: first wins, second silently
skipped (`resolve_one`, `:222`). Example:

```yaml
connections:
  - {src_comp: A, src_name: T, dst_comp: C, dst_name: T}
  - {src_comp: B, src_name: T, dst_comp: C, dst_name: T}
# legacy: C/T aliases B's T (by reading); graph: A's T; no message in either
```

- (a) Make it an error in both (restore the legacy assert; the graph
  reports a connection conflict on the second). Wildcard exempt. Risk: a
  config relying on overwrite fails, including a MatchConnection plus an
  explicit SimpleConnection to the same import.
- (b) Error in the graph only, as a connection conflict; legacy unchanged.
- (c) Leave silent.
Recommendation: (a), after a grep of real configs.
Follow-up, only if that grep finds MatchConnection/SimpleConnection
overlaps: decide whether the explicit SimpleConnection may win.

**R9. Which source intents may a re-export take?**
*Resolved: option (a); the graph resolves both source intents in `run_reexport_hook` (3.33; `reexport_one`, `GraphBuilder_ConnectionResolution_smod.F90:919-970`).*
Legacy: `ReexportConnection` exposes an EXPORT (a child's) or an INTERNAL
(own) item as a new export, with no comparison; `ComponentSpec%reexport`
accepts `export` (default) or `internal` and fails otherwise
(`ComponentSpec.F90:139-148`). Owner list C3: re-export exposes an
INTERNAL item. Example:

```yaml
# parent.yaml (scenario_reexport_twice)
connections:
  - {src_comp: child_B, src_name: E_B1, src_intent: export,
     dst_comp: <self>, dst_name: Eparent_B1, dst_intent: export}
```

- (a) EXPORT or INTERNAL, as legacy; reword C3 ("exposes an INTERNAL item,
  or a child's EXPORT, in this component's EXPORT state"). No code change.
- (b) INTERNAL only (C3 literal); child-export re-export becomes graph
  export propagation (`run_export_propagate_hook`). Breaks
  `scenario_reexport_twice` and `scenario_2/parent.yaml` in legacy.
Recommendation: (a).

**R10. Validate SimpleConnection intents in `parse_connections`?**
Legacy: `parse_connections` (`parse_connections.F90:57-70`, `get_intents`
`:120-140`) builds a ReexportConnection when `dst_intent: export`, else a
SimpleConnection, with no check that src is EXPORT and dst is IMPORT.
Graph: same parser. Owner list C1: a simple connection is always EXPORT ->
IMPORT. Example:

```yaml
connections:
  - {src_comp: A, src_name: Z, src_intent: internal, dst_comp: B, dst_name: Z}
# parses today as SimpleConnection internal -> import
```

- (a) `_ASSERT` in `parse_connections`: dst `export` -> re-export with src
  `export` or `internal` (per R9); otherwise src must be `export` and dst
  `import`. Small parser task; a scenario grep finds no violator.
- (b) Leave unvalidated.
Recommendation: (a).

**R11. Schedule a graph task for INTERNAL re-export?**
*Resolved: option (a), landed in 3.33: a re-exported internal gets an export port bound to the internal's node (`run_reexport_hook`). Owner, 2026-11-07: an unconnected re-exported export stays created, not allocated. 3.41b B3-ExportPortPopulation: with the user-state switch on, the graph puts each re-export port's Field into the user export state under the port name.*
Legacy: an INTERNAL -> EXPORT re-export makes the internal appear in the
export state (`ReexportConnection.F90:116-117`). Graph, at review time (no
longer current; see "Resolved" above and `reexport_one`,
`GraphBuilder_ConnectionResolution_smod.F90:919-970`): re-export was
skipped (`class default`) and internals have no port
(`GraphBuilder_Advertising_smod.F90:115`, still true), so in graph mode the
export had no node. Used by `scenario_1`, `scenario_2`,
`propagate_geom` (`child_A.yaml`, `src_intent: internal`). Example:

```yaml
# scenario_1/child_A.yaml
states:
  internal: {Z_A1: {standard_name: 'Z_A1 standard name', units: 'm'}}
connections:
  - {src_comp: <self>, src_name: Z_A1, src_intent: internal,
     dst_comp: <self>, dst_name: Z_A1, dst_intent: export}
# legacy: Z_A1 in the export state; graph: no export node for Z_A1
```

- (a) New graph task: an INTERNAL re-export gets an export port bound to
  the internal node (no mirror on the internal, Q13). Place with B4 or
  later; required before graph-only mode.
- (b) Defer until legacy retirement planning; until then these scenarios
  depend on the registry path.
Recommendation: (a), placed after B4.

**R12. standard_name: field-wide or per-alias?**
Legacy: field-wide (`StandardNameAspect%update_payload`, `:326`);
PERMISSIVE lets two declared names connect with a warning; the import
then reads the shared field's value. For a VECTOR, each component's
standard_name also adopts the export's (§0 group b). Graph: field-wide; a
mismatch is UNSUPPORTED (never connects). Owner list P1: import and export
may differ. Example:

```yaml
# export A/T: {standard_name: air_temperature}
# import B/T: {standard_name: surface_temperature}
# legacy PERMISSIVE: connects, warning; B reads 'air_temperature'
# graph: not connected (UNSUPPORTED)
```

- (a) Field-wide stays; P1 is amended ("standard_name must agree;
  long_name and restart_mode may differ"). 3.27 then chooses
  STRICT/PERMISSIVE for the mismatch.
- (b) Per-alias (both paths): each endpoint keeps its declared name.
  Changes the landed `field-name-propagation` spec,
  `StandardNameAspect%update_payload` and the VECTOR copy; a mismatch
  becomes metadata, not a connection failure.
- (c) Defer to the "share only data" analysis (plan, after B4): if imports
  own their fields, a per-item standard_name follows.
Recommendation: (a) for now; revisit with (c).

**R13. Put a system-wide "standard_name required on Fields" switch (V3)
in 3.27's scope?**
Legacy: no such switch. `ValidationMode` (default PERMISSIVE,
`FieldDictionaryConfig.F90:92`) governs a declared name missing from the
dictionary and a mismatch on connect; nothing requires a FIELD to declare
one. Graph: same (undeclared is UNCHECKED). Example:

```yaml
export:
  T: {units: K}     # no standard_name: accepted in both paths
# with the switch on: error (or warning) at advertise
```

- (a) Yes: 3.27 designs it with the validation mode (one config, one
  place, both paths).
- (b) No: a separate later task after 3.27.
- (c) Drop V3.
Recommendation: (a); it is the same configuration surface.

**R14. fill_value: what rule should the graph carry?**
Legacy, aspect level (`FieldClassAspect.F90:274-348`): on connect, an
import with no fill_value takes the export's; if both declare one and
they differ, the import keeps its own and an info message says "(src value
wins)", which is the opposite of what the code does (§5); an export with
no fill_value takes the import's (`connect_to_import`). Legacy, data
level (by reading): `connect_sibling` allocates the export before
`connect` (`SimpleConnection.F90:184-193`), and `FieldClassAspect%allocate`
applies `fill_value` only to a field that is not yet COMPLETE
(`:249-256`). So the shared field is filled with the export's value when it
has one, and the import's value, kept or copied, never reaches the data.
Not run. Graph: no characteristic, no code. Example:

```yaml
export: {T: {fill_value: 0}}
import: {T: {fill_value: -999}}
# legacy: import aspect keeps -999; log says "src value wins";
#         data filled with 0 (by reading)
# graph: no fill_value handling at all
```

- (a) Field-wide characteristic, never compared: the owner of the field
  (the export) fills it; an import's declared value is ignored. Matches
  legacy data behavior; drops the aspect-level import value and the
  reverse flow (`connect_to_import`).
- (b) Per-alias characteristic, like long_name: each endpoint keeps its
  declared value as metadata; data is still filled from the export's.
  Matches legacy aspect behavior; a value nobody reads.
- (c) Not a characteristic: materialization fills the field from the
  owning `VariableSpec`'s fill_value. Same data result as (a), no
  descriptor entry; an import's fill_value becomes a no-op (or an error).
Recommendation: (c) if no reader of an import's fill_value exists (none
found), else (a). Whichever is chosen, the legacy log text gets a small
fix task.

**R15. `bootstrapped`: field-wide or per-alias, and a graph concern?**
Legacy: `RestartHandler` sets field-wide `bootstrapped` for a field
missing from the restart file (`RestartHandler.F90:266`); RestartMode is
per alias; only tests read `bootstrapped`. Graph: nothing. Example: export
A/T and import B/T share one field; B's import restart lacks T; T is
marked bootstrapped and A's export alias also reports
`bootstrapped=.true.`.
- (a) Leave field-wide, not a characteristic; document that it means
  "this field's data was not read from some restart".
- (b) Make it per alias (keyed like RestartMode), so only the alias whose
  read missed it reports it. Infrastructure change (`FieldInfo`) plus
  `RestartHandler`.
Recommendation: (a) until a production reader exists; ask the #5470
author who reads it.

**R16. Legacy GEOM_A overwrites item-level geometry (confirmed by probe; fixed, 3.38).**
Legacy (before the fix): `registry%set_geometry` sets every primary spec's GeomAspect to the
component geom, SPECIFIED (`StateRegistry_SetGeometry_smod.F90:27`,
`GeomAspect.F90:205-224`), including an item that declared its own (by
reading). Graph: an item geom is SPECIFIED and kept. Example:

```yaml
geometry: {esmf_geom: {...}}          # component geom G1
states:
  export:
    E: {geometry: {esmf_geom: {...}}} # item geom G2
# legacy (by reading): E ends on G1; graph: E on G2
```

- (a) Run a probe test before deciding; if confirmed, treat legacy as a
  bug (item wins) in a small legacy task.
- (b) Accept legacy (component wins) and change the graph.
Recommendation: (a). Probe result: the overwrite is confirmed (§2.6); the
owner chose (a) (§4a). Fixed by 3.38 B3-GeomAItemWins: an item that declares
its own geom or vertical grid keeps it at GEOM_A (§2.6).

## 4a. Decisions (owner session, 2026-11-02)

Owner's words are quoted where he added any. "Task" is the follow-up in
tasks.md §3; "none" means a decision with no code or doc work beyond this
record.

| Q | Decision | Owner's words | Follow-up |
|---|---|---|---|
| R1 | (a) No adopt rule for value wildcards; correct the docs that describe the legacy `connect_to_export` copy as behavior. | "(a)" | Done in this task: tasks.md 3.26 seed; design.md §4.3 (BRACKET -> FIELD standard_name note), §5.1 (VerticalOrientation legacy cell), A23 ("export's name wins" qualified), A24 (new); `adding-a-characteristic.md` (QuantityType connect note, BRACKET -> FIELD note, HorizontalDimsSpec mirror note). |
| R2 | (c), only QuantityType changes now, and the rule reads "almost all characteristics default to DEFERRED", not "UNSPECIFIED". Follow-up decisions: (1) DEFERRED is supplied by the user with `MAPL_FieldSet` in component code; never supplied, freeze fails, but only for a kind the item's shape carries (structural already: the factory adds no entry for other shapes, and freeze checks only present entries). (2) Rollout (b): suppliers first (YAML key, `make_VariableSpec` keyword, harvest of a `MAPL_FieldSet` value); then a ValidationMode-style switch: an unsupplied DEFERRED QuantityType at freeze is a warning with a fallback by default, an error in strict mode; strict becomes the default once real configs comply. (3) Imports (a): an undeclared IMPORT is UNCHECKED; only exports and internals carry the DEFERRED/freeze rule. Conservation and Normalization unchanged for now. | "I used the wrong term. I meant "DEFERRED" where I said "UNSPECIFIED". (c)"; "Users can use FieldSet to modify DEFERRED characteristics"; freeze fails "but *only* if it is a characteristic used by that ItemShape. E.g., a State with DEFERRED physical units is ok."; rollout: "Later I will work through more concrete use cases and we may tweak the rules."; imports: "will try to work through realistic use cases later to refine this." | 3.35 B3-QuantityTypeSuppliers, 3.36 B3-QuantityTypeDeferred. 3.36 landed. Consistency with R1 and the UNKNOWN wildcard: see the note below the table. |
| R3 | (a) Keep the hard error for MIRROR on a non-EXPRESSION export; M1 reads "disallowed until a use case and export cloning exist". | "(a)" | none (owner's list). |
| R4 | (a) Add "INTERNAL: no mirror of any kind" (Q13) to the owner's list as M4. | "(a)" | none (owner's list). |
| R5 | (a) BRACKET/VECTORBRACKET never IMPORT, WILDCARD only IMPORT, in `intent_rule_violation`, both modes. | "(a)" | 3.28 B3-ShapeIntentRules. |
| R6 | (a) A Wildcard import accepts FIELD exports only, in the graph too. | "(a) We will relax if we encounter reasonable use cases." | 3.30 B3-WildcardFieldOnly. |
| R7 | (a) S4 reads "Bracket -> Field and VectorBracket -> Vector, via time interpolation". | "(a)" | none (owner's list). |
| R8 | (a) A second connection to an import is an error in both paths; Wildcard exempt; after a grep of real configs. | "(a)" | 3.31 B3-OneExportPerImport. |
| R9 | (a) A re-export takes an EXPORT (child) or INTERNAL (own) source, as legacy; C3 is reworded. | "(a)" | none (owner's list). Graph side: 3.33 B3-InternalReexport resolves a child-export re-export as one rule with the internal case (`run_reexport_hook`). |
| R10 | (a) for SimpleConnection only: `parse_connections` asserts src EXPORT, dst IMPORT. A ReexportConnection (`dst_intent: export`) is exempt; its sources are EXPORT or INTERNAL (R9). | "(a) Except for Re-export connections!" | 3.29 B3-ConnectionIntentCheck. |
| R11 | (a) Graph task: an INTERNAL re-export gets an export port bound to the internal node; after B4. | "(a)" | 3.33 B3-InternalReexport (done: `run_reexport_hook`). Owner, 2026-11-07 (3.41a): an unconnected re-exported export stays created, not allocated; legacy allocates it, a legacy bug (row in scenario-parity.md). 3.41b B3-ExportPortPopulation: the user export state is filled from the port, not from the declared item. |
| R12 | (a) standard_name stays field-wide, for now; P1 amended ("standard_name must agree; long_name and restart_mode may differ"). | "(a) for now." | none now; revisit with "share only data". |
| R13 | (a) The system-wide "standard_name required on Fields" switch (V3) is in 3.27's scope. | "(a)" | 3.27 text updated. |
| R14 | (c) fill_value is not a characteristic: materialization fills the field from the owning `VariableSpec`'s fill_value. The legacy "(src value wins)" log text gets fixed. | "(c)" | 3.34 B3-FillValue. |
| R15 | (a) `bootstrapped` stays field-wide and is not a characteristic. | "Interesting use case. Ideally, if any component actually reads a value then `bootstrapped` should be false. But that is probably hard/impossible to analyze during wiring. (a)" | none. Note: the owner's ideal is "false if any component reads the value", which wiring cannot decide. |
| R16 | (a) Run a probe test before deciding; if legacy does overwrite, treat it as a legacy bug (item wins). | "(a)" | 3.32 B3-GeomAProbe. |

R2 and R1 together. R1 (a) says no adopt rule; R2 does not add one.
After 3.36 (landed):

- An undeclared EXPORT or INTERNAL QuantityType is DEFERRED, not SPECIFIED
  `QUANTITY_UNKNOWN`. Until it is supplied, a connection to it waits
  (`resolve_pair` gives PAIR_WAIT for a DEFERRED side); once supplied it
  is SPECIFIED and compared.
- An undeclared IMPORT is UNCHECKED. `resolve_pair` gives PAIR_MATCH for an
  UNCHECKED side, with no copy (PAIR_ADOPT is only for a MIRRORED import).
  So the import's UNCHECKED status replaces the UNKNOWN value wildcard for
  the undeclared case, and the import still copies nothing (R1).
- A declared `QUANTITY_UNKNOWN` counts as undeclared (as the
  PhysicalUnits factory does for `'<unknown>'`), and both UNKNOWN
  disjuncts are gone from `pair_outcome`'s quantity-axis match. Before the
  change, the wildcard in `QuantityTypeCharacteristic%needs_extension_for`
  was only an explicitly declared `QUANTITY_UNKNOWN`, which becomes possible once 3.35 adds a declaration
  path. 3.36 proposed treating a declared UNKNOWN as undeclared, as the
  PhysicalUnits factory already does for `'<unknown>'`, and dropping the
  UNKNOWN wildcard from `needs_extension_for`. BASIS_NONE is a separate
  case: `VariableSpec` has no basis field, so nothing declares a basis;
  whether its wildcard stays is a preflight question in 3.36, not decided
  here.

Not decided here, by design: the "share only data" analysis (plan, after
B4), which R1 and R12 name as the point to revisit.

## 5. Out-of-scope issues found

- `superstructure/generic/specs/VariableSpec.F90:569` `get_regrid_param`:
  no caller; the field-dictionary regrid default is never applied.
- The stray `superstructure/generic/to_itemtype.F90` is deleted (D3-cleanup);
  `ComponentSpecParser/to_itemtype.F90` does not parse `vector_bracket`, so
  YAML cannot declare a VECTORBRACKET.
- `superstructure/generic/graph/characteristics/LongNameCharacteristic.F90:90`:
  `build_transform` reports UNSUPPORTED, but `needs_extension_for` is
  always false, so it is unreachable; INCOMPATIBLE or a "never called"
  comment would be clearer. Same for `RestartModeCharacteristic.F90:91`.
- `superstructure/generic/OuterMetaComponent/initialize_advertise.F90:48-69,73-107`:
  long change-history comments ("Phase 3b", archived task numbers), the
  style AGENTS.md asks to remove (task A4).
- `superstructure/generic/specs/FieldClassAspect.F90:345`: the fill_value
  mismatch message says "(src value wins)", but `mirror` (`:327-348`)
  leaves `dst` unchanged when both are allocated, so the import (dst)
  keeps its own value. Legacy log/behavior mismatch (R14).
- Legacy-retirement note, not a task: the outer-map value aspects'
  `connect_to_export` procedures (`QuantityTypeAspect`,
  `ConservationAspect`, `NormalizationAspect`, `GeomAspect`, `UnitsAspect`,
  `TypekindAspect`, `UngriddedDimsAspect`, `VerticalGridAspect`,
  `AttributesAspect`) have no production caller (§0). The first four have
  unit tests; the rest have no caller at all. `StandardNameAspect%connect_to_export`
  is live (VECTOR, `VectorClassAspect.F90:289-296`) and must stay.
- `openspec/changes/characteristic-separation-of-concerns/design.md:487`
  says legacy `StandardNameAspect%connect_to_export` adopts the export's
  name across BRACKET -> FIELD; that procedure is not called for a
  BRACKET or FIELD connect (§0). The conclusion (no check, PERMISSIVE-like)
  still holds.
