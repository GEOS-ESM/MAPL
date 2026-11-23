## 0. Probes (no production code) — done, see `tasks-done.md`

## 1. Data model — done, see `tasks-done.md`

## 2. Info support — done, see `tasks-done.md`

## 3. Registration file

- [x] 3.1 CRC-32 utility over a file's bytes (utils), with a known-vector
  test.
- [x] 3.2 Loader: read `./callback_interfaces.yaml` with HConfig, build
  and validate every entry, register all or none; errors name file,
  interface, key and legal names. Tests: `toy_scaling` loads exactly;
  F9; MF2; MF3; unknown key; missing file leaves an empty registry
  marked "no file". Depends on 1.x.
- [x] 3.3 Call the loader from `MaplFramework%initialize`; "no file
  found" error on first interface lookup with an empty no-file
  registry. Test under a `mapl_pFUnit_Initialize_mod` executable.
- [x] 3.4 Freeze and startup report in `mapl_cap_create` before
  `make_driver` (path, CRC-32, sorted interfaces with version and
  summary). Test F10 (registration after freeze) and the report text.
- [x] 3.5 Shipped files: `callback_interfaces_test.yaml` (toy_scaling)
  and `callback_interfaces_geos.yaml` (ships no interfaces yet;
  `turbulent_mixing` is 9.1's own deliverable); header comments
  documenting duplicate-key behavior (from 0.1, scoped to scalar keys --
  a duplicated name inside `arguments:`/`methods:`/`attributes:`/
  `call_scalars:` is a hard load error, not silently shadowed); CMake
  `install(FILES ... DESTINATION etc)`; per-test copy into the ctest
  working directory. The MF1b/MF2/MF3/F11/F12 test-only variants belong
  to their own slice test directories under decision 18, not this
  shared file; they land with the tasks that build those directories
  (4.3, 8.3, ...), not here.
- [x] 3.6 Hand-written constants modules for `toy_scaling` and
  `turbulent_mixing`, with a test that each constant names something in
  the loaded file.
- [x] 3.7 YAML key `callback_interface: <name>` for imports and STATE
  exports in `parse_var_specs`, and the matching `make_VariableSpec`
  keyword; resolve by name through the registry; unregistered-name
  error. Tests.

## 4. Conformance

- [x] 4.1 Violation list type and message formatter (header plus one
  line per violation, wording as in the conformance spec). Unit tests on
  formatted text.
- [x] 4.2 Extend `export_conforms_to_interface`: optional arguments,
  dims, all violations returned; declared-Handler mismatch becomes a
  wiring error, undeclared keeps skip-and-report. Update
  `Test_GraphBuilder_CallbackWiring.pf`. Depends on 1.1, 3.7, 4.1.
- [x] 4.3 `check_callback_state_conformance` on a realized `ESMF_State`:
  presence, kind, attribute presence/type/allowed, unknown keys, rules
  after attributes, weights and weight geometry; defaults recorded.
  Tests on hand-built States covering F1-F8 and MF1, MF1b, MF4-MF8,
  P4, P5. Depends on 1.x, 2.1, 4.1.
  (P5 is unlabeled in the spec; it is the design note's pairing P5,
  the spec scenario "Extra member and foreign attribute are accepted".)
- [x] 4.4 `MAPL_CallbackGetAttribute` (namespace from the State's
  recorded interface, host-argument member or State host, default,
  undeclared name lists legal names) and `MAPL_CallbackSetScalar` (name,
  type -- the declared writer is a documented convention, not checked,
  design.md decision 11). Tests on hand-built States. Depends on 2.1,
  4.3.

## 5. Invoker-side execution (critical path; owner designs, subagents author)

- [x] 5.0 `CallbackArgumentSpec` gains a `units` field, interface side
  only: scalar YAML key `units:` on an interface argument (`$<name>`
  declares a placeholder, any other value is a concrete unit --
  distinguish by the leading `$`, no HConfig-shape polymorphism needed
  here since an interface's `units:` is always a scalar).
  `declare_argument`/the loader (3.2) store it and expose placeholder
  name vs. concrete unit; two arguments in one interface naming the same
  placeholder are thereby linked (whatever unit a Handler eventually
  binds applies to both). No Handler-side binding or validation here --
  that needs the Handler's own `units: {<name>: '<unit>'}` YAML, which
  does not exist until 5.1. Tests on hand-built interfaces (`toy_scaling`
  or a new fixture). Depends on 1.1, 3.2. Design: design.md decision 21
  (unit placeholder YAML), decision notes topic A-units (D1-D3).
- [x] 5.1 Realize each Handler's callback State as an `ESMF_State`
  holding its member items. A plain `declare_member` is a new,
  framework-allocated field: geometry from the Handler's component;
  units from the interface's concrete value (5.0), or, for a placeholder,
  from the Handler's own `units: {<name>: '<unit>'}` binding (new YAML
  key on `declare_member`, told apart from a concrete scalar with
  `ESMF_HConfigIsScalar`/`ESMF_HConfigIsMap` as `parse_var_specs.F90`
  already does for other polymorphic keys); every placeholder the
  interface declares must be bound before the callback State is
  realized. `alias: <name>` makes the member a zero-copy view of an
  existing Handler item (geometry/typekind/units come from the aliased
  item, no units binding needed). A minimal `members:` YAML map (name ->
  `{alias:, units:}`) on a callback-State EXPORT reaches `declare_member`
  for plain/aliased members (nested composite members stay
  Fortran-API-only for now); a `members:` entry only answers a FIELD-kind
  interface argument -- reject loudly at parse/advertise time otherwise.
  Record the interface name in each callback State's Info. Template
  expansion (`alias: <pattern>` with `template: true`) is its own task,
  5.1b. Depends on 5.0. Design: design.md decision 21 (unit placeholder
  binding, alias YAML, minimal `members:` YAML, FIELD-only `members:`),
  decision notes topics A-units (D1-D3) and B.
- [ ] 5.1b `alias: <pattern>` with `template: true` on `declare_member`:
  expand it, via the existing wildcard-matching regex code already used
  for import names, into one callback State per matching internal item,
  each containing one member (e.g. `alias: 'TR.*'` with `template: true`
  on an interface's `tracer` argument yields three sibling callback
  States, one per `TR001`/`TR002`/`TR003`, each with its own single
  `tracer` member). No YAML shape required yet (Fortran-API-only, same as
  nested composite members). Split out from 5.1 (owner 2026-11-23) after
  cumulative review found it unimplemented; landing 5.1 without it is
  additive since nothing downstream needs it. Depends on 5.1. Design:
  design.md decision 21 (alias/template YAML).
- [ ] 5.1a A plain `members:` YAML entry (5.1) can only declare a 2D
  field today (vertical stagger hard-defaulted to none); give it a way
  to declare a vertical grid/stagger, matching what an ordinary
  top-level item's YAML can already express. Owner priority: important,
  not optional -- GEOS physics callback arguments are routinely 3D.
  Needs its own design pass (a composite member has no connection to
  mirror a vertical grid from the way a top-level item does via
  GEOM_A/B's DEFERRED machinery; a `members:` entry likely needs an
  explicit `vertical_grid:`/`vertical_stagger:` key instead, or some
  other resolution this task should propose). Depends on 5.1.
- [ ] 5.2 Fill the Invoker's collection State with each matched callback
  State, then `ESMF_MethodExecute`-backed `StateMethodInvoker`; production
  calls to `build_callback_state_binding` /
  `build_callback_method_binding` for every collection member.
- [ ] 5.3 Invoker API: an iterator over one collection's bound Handlers
  (advance, done, current) -- not count-and-index; qualified-name access
  and detailed error behavior (loud `_FAIL` vs. a `found` out-argument)
  deliberately left open past this task. `current()` returns a handle
  (exact type name open) wrapping the member's `ESMF_State` plus its
  already-resolved `ComponentGraph` pointer and method `NodeId`, with
  `%execute(method, rc)` (check-if-needed, pre-call pull, the procedure,
  post-call advance; failure propagation; unknown-method error) and
  `%get_state()` for `MAPL_CallbackGetAttribute`/`MAPL_CallbackSetScalar`,
  which stay free functions. Same-grid get/put test. Design: design.md
  decision 21 (Invoker API shape, amended 2026-11-23).
- [ ] 5.4 Hook the lazy conformance check (4.3) into 5.3 and add the
  explicit collection check request.

## 6. Callback-argument transforms

- [x] 6.1 `wire_callback_argument` compares real member descriptors;
  Invoker-side copies take the Invoker's geometry; default regrid param
  first-order conservative. Test: Handler on another grid gets a regrid
  on `get` and on `put`. Depends on 5.1.
- [x] 6.2 `SnapshotTransform` (value copy on the `get` return path, only
  when a regrid lies between). Stand-alone test.
- [x] 6.3 `WeightedDeltaBackTransform` (ports value, snapshot,
  weight_invoker, weight_owner, owner). Stand-alone test M2 with exact
  numbers, and an unweighted case.
- [x] 6.4 `ConservativeColumnFillTransform` with fill diagnostic;
  `non_negative` as a never-compared characteristic. Stand-alone test
  with the M4 numbers.
- [ ] 6.5 Wire 6.2-6.4 into the `put` network for `put_flavour: value`
  arguments; same-grid case writes directly (no snapshot). Depends on
  5.x, 6.1-6.4.

## 7. Test tools

- [x] 7.1 Configurable Handler: callback States from the registry,
  `state_attributes`, `member_attributes`, `geometry`, `supply`
  (constant, profile), `record` under `mapl_mock/` and as export fields,
  `non_negative`. Tests of construction and conformance without an
  Invoker. Recording tests depend on 5.x (not done here).
- [x] 7.2 Configurable Handler `supply: function` using the state
  arithmetic parser extended with `lon`, `lat`, `level`. Check first
  that `superstructure/state/ArthParser.md`'s parser fits; stop and
  report if not.
- [ ] 7.3 Configurable Invoker: collection by regex, schedule, record.
  Depends on 5.x.
- [ ] 7.4 Configurable Handler expectations (call order, counts,
  bounds, budget identity). Later scope, after 8.x.

## 8. First slice: toy_scaling

- [ ] 8.1 Small real Handler and small real Invoker test gridcomps
  (Invoker uses `select case` with an error default).
- [ ] 8.2 Pairings P1-P5 on the two-grid setup.
- [ ] 8.3 Failing setups F1-F12 end to end, each asserting its message.
  F11 asserts the behavior recorded in 0.1.

## 9. GEOS slice: turbulent_mixing

- [x] 9.1 `turbulent_mixing` entry in the GEOS file, as in the design
  note (`mix_like` with `host_argument: mixed_quantity`).
- [ ] 9.2 `TurbulentMixingTestInvoker`: per `mix_like` class one implicit
  diffusion column operator using the delivered `air_mass`; four bottom
  conditions; realized flux; unused-argument check (MF9). Column solver
  unit-tested on its own first (M-matrix positivity, M9).
- [x] 9.3 Mock tracer owners on the three owner geometries.
- [ ] 9.4 Test matrix M1, M3-M9 and MF1-MF9 (M2 is 6.3).

## 10. Docs

- [ ] 10.1 `docs/graph/callbacks.md`: contracts, registration file,
  conformance, execution, transforms, test tools.
- [ ] 10.2 User guide section on the registration file (location,
  format, report, duplicate-key behavior, Invoker coding rule).
- [ ] 10.3 `docs/graph/ARCHITECTURE.md` callback rows (meta batch).
