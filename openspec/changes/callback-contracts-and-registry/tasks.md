## 0. Probes (no production code)

- [ ] 0.1 HConfig duplicate keys: a test loads a file with two
  `toy_scaling:` entries (versions 1 and 2) and records which wins and
  whether any HConfig call reports the duplicate. Done: test asserts the
  observed behavior; result written to design.md Open Questions and to
  the plan's Status line.
- [ ] 0.2 ESMF Info typing: a test stores i4, i8, r4, r8, logical,
  string, and arrays of each, then records what `ESMF_InfoInquire`
  reports and whether `ESMF_InfoGet` converts (real into integer,
  string into real). Also record whether keys under a nested prefix can
  be enumerated. Done: test asserts the observed behavior; result
  recorded in design.md Decision 4.

## 1. Data model

- [ ] 1.1 `CallbackArgumentSpec`: add `dims`, `optional`, `extensive`,
  `weight_for` (optional constructor arguments, defaults keep today's
  meaning); reject a field argument without dims. Unit tests.
- [ ] 1.2 New `CallbackAttributeSpec` (type incl. list types, scope,
  required, default, allowed set) with its container and declaration
  checks. Unit tests.
- [ ] 1.3 New `CallbackCallScalarSpec` (type, writer) with container.
  Unit tests.
- [ ] 1.4 New `CallbackRule` (when attribute = value, requires list)
  with container. Unit tests.
- [ ] 1.5 `CallbackMethodSpec`: per-argument options (`put_flavour`);
  reject unknown option names. Unit tests.
- [ ] 1.6 `CallbackInterface`: version, summary, description;
  declare/get for attributes, call scalars, rules; enforce slash-free
  names, rule consistency, weight consistency, host-argument rules. Unit
  tests for every rejection in the data-model delta spec.
- [ ] 1.7 Registry: `freeze_callback_registry`, frozen-registration
  error naming the interface, `list_callback_interface_names` (sorted).
  Unit tests.

## 2. Info support

- [ ] 2.1 `InfoUtilities.F90`: stored-type query, `r8_1d`, string-list
  getters/setters, State-own-Info getters/setters for logical, i4, r4,
  r8, key enumeration under a prefix. Unit tests in the infrastructure
  test suite. Depends on 0.2.

## 3. Registration file

- [ ] 3.1 CRC-32 utility over a file's bytes (utils), with a known-vector
  test.
- [ ] 3.2 Loader: read `./callback_interfaces.yaml` with HConfig, build
  and validate every entry, register all or none; errors name file,
  interface, key and legal names. Tests: `toy_scaling` loads exactly;
  F9; MF2; MF3; unknown key; missing file leaves an empty registry
  marked "no file". Depends on 1.x.
- [ ] 3.3 Call the loader from `MaplFramework%initialize`; "no file
  found" error on first interface lookup with an empty no-file
  registry. Test under a `mapl_pFUnit_Initialize_mod` executable.
- [ ] 3.4 Freeze and startup report in `mapl_cap_create` before
  `make_driver` (path, CRC-32, sorted interfaces with version and
  summary). Test F10 (registration after freeze) and the report text.
- [ ] 3.5 Shipped files: `callback_interfaces_test.yaml` (toy_scaling,
  plus test-only variants used by MF1b/MF2/MF3/F11/F12 kept in test
  directories) and `callback_interfaces_geos.yaml` (turbulent_mixing);
  header comments documenting duplicate-key behavior (from 0.1); CMake
  `install(FILES ... DESTINATION etc)`; per-test copy into the ctest
  working directory.
- [ ] 3.6 Hand-written constants modules for `toy_scaling` and
  `turbulent_mixing`, with a test that each constant names something in
  the loaded file.
- [ ] 3.7 YAML key `callback_interface: <name>` for imports and STATE
  exports in `parse_var_specs`, and the matching `make_VariableSpec`
  keyword; resolve by name through the registry; unregistered-name
  error. Tests.

## 4. Conformance

- [ ] 4.1 Violation list type and message formatter (header plus one
  line per violation, wording as in the conformance spec). Unit tests on
  formatted text.
- [ ] 4.2 Extend `export_conforms_to_interface`: optional arguments,
  dims, all violations returned; declared-Handler mismatch becomes a
  wiring error, undeclared keeps skip-and-report. Update
  `Test_GraphBuilder_CallbackWiring.pf`. Depends on 1.1, 3.7, 4.1.
- [ ] 4.3 `check_callback_state_conformance` on a realized `ESMF_State`:
  presence, kind, attribute presence/type/allowed, unknown keys, rules
  after attributes, weights and weight geometry; defaults recorded.
  Tests on hand-built States covering F1-F8 and MF1, MF1b, MF4-MF8,
  P4, P5. Depends on 1.x, 2.1, 4.1.
- [ ] 4.4 `MAPL_CallbackGetAttribute` (namespace from the State's
  recorded interface, host-argument member or State host, default,
  undeclared name lists legal names) and `MAPL_CallbackSetScalar` (name,
  type, writer). Tests on hand-built States. Depends on 2.1, 4.3.

## 5. Invoker-side execution (owner track; critical path)

- [ ] 5.1 Realize each Handler's callback State as an `ESMF_State`
  holding its member items; fill the Invoker's collection State with
  the matched callback States; record the interface name in each
  callback State's Info.
- [ ] 5.2 `ESMF_MethodExecute`-backed `StateMethodInvoker`; production
  calls to `build_callback_state_binding` /
  `build_callback_method_binding` for every collection member.
- [ ] 5.3 Invoker API: collection count and access by position or
  qualified name; `MAPL_CallbackExecute(state, method, rc)` running
  check-if-needed, pre-call pull, the procedure, post-call advance;
  failure propagation; unknown-method error. Same-grid get/put test.
- [ ] 5.4 Hook the lazy conformance check (4.3) into 5.3 and add the
  explicit collection check request.

## 6. Callback-argument transforms

- [ ] 6.1 `wire_callback_argument` compares real member descriptors;
  Invoker-side copies take the Invoker's geometry; default regrid param
  first-order conservative. Test: Handler on another grid gets a regrid
  on `get` and on `put`. Depends on 5.1.
- [ ] 6.2 `SnapshotTransform` (value copy on the `get` return path, only
  when a regrid lies between). Stand-alone test.
- [ ] 6.3 `WeightedDeltaBackTransform` (ports value, snapshot,
  weight_invoker, weight_owner, owner). Stand-alone test M2 with exact
  numbers, and an unweighted case.
- [ ] 6.4 `ConservativeColumnFillTransform` with fill diagnostic;
  `non_negative` as a never-compared characteristic. Stand-alone test
  with the M4 numbers.
- [ ] 6.5 Wire 6.2-6.4 into the `put` network for `put_flavour: value`
  arguments; same-grid case writes directly (no snapshot). Depends on
  5.x, 6.1-6.4.

## 7. Test tools

- [ ] 7.1 Configurable Handler: callback States from the registry,
  `state_attributes`, `member_attributes`, `geometry`, `supply`
  (constant, profile), `record` under `mapl_mock/` and as export fields,
  `non_negative`. Tests of construction and conformance without an
  Invoker. Recording tests depend on 5.x.
- [ ] 7.2 Configurable Handler `supply: function` using the state
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

- [ ] 9.1 `turbulent_mixing` entry in the GEOS file, as in the design
  note (`mix_like` with `host_argument: mixed_quantity`).
- [ ] 9.2 `TurbulentMixingTestInvoker`: per `mix_like` class one implicit
  diffusion column operator using the delivered `air_mass`; four bottom
  conditions; realized flux; unused-argument check (MF9). Column solver
  unit-tested on its own first (M-matrix positivity, M9).
- [ ] 9.3 Mock tracer owners on the three owner geometries.
- [ ] 9.4 Test matrix M1, M3-M9 and MF1-MF9 (M2 is 6.3).

## 10. Docs

- [ ] 10.1 `docs/graph/callbacks.md`: contracts, registration file,
  conformance, execution, transforms, test tools.
- [ ] 10.2 User guide section on the registration file (location,
  format, report, duplicate-key behavior, Invoker coding rule).
- [ ] 10.3 `docs/graph/ARCHITECTURE.md` callback rows (meta batch).
