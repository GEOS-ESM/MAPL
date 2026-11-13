## Why

The callback data model (`graph/callback-data-model`) and its wiring
(`graph/callback-wiring`) exist, but an interface today is only argument
names, item kinds and per-method access. It cannot say which attributes a
Handler must set, which values are legal, which arguments a mode needs, or
which argument is the mass weight of another. Only unit tests register
interfaces, and an Invoker has no way to run a method on a callback State
it finds in its collection. The GEOS callback design (Turbulence, Physics)
needs all of this. The design track that produced it is finished, so the
whole remaining scope comes in as one change, not as later increments.

## What Changes

- **Exact contracts.** A registered interface states its name, version,
  summary, arguments (kind, gridded dims `xyz|xy`, optional, extensive,
  `weight_of`), per-method access (with per-argument options such as
  `put_flavour`), attributes (value type, scope `member|state`, required
  or default, allowed set), call scalars, and rules ("when attribute `a`
  is `v`, arguments `x, y` are required"). R4 and R8 are both "real".
- **Structural conformance, checked against the realized ESMF State.**
  A Handler's callback State conforms if it provides at least what the
  contract asks for. Extra members, extra attributes, and keys under
  other interfaces' namespaces are ignored. Missing required items,
  wrong kinds, wrong value types, values outside the allowed set,
  unmet rules, unknown keys in the interface's own namespace, and
  missing or wrongly placed weights are errors. An attribute's own
  checks run before any rule that uses it. All errors for one callback
  State are reported together, then wiring fails. Defaults are reported
  as defaults.
- **One registration file per experiment.** Interfaces are defined in
  `./callback_interfaces.yaml` (current directory), keyed by interface
  name. MAPL loads and validates it in `mapl_initialize`, before any
  component exists. The Cap freezes the registry before the root
  `SetServices`. MAPL logs the file path, its checksum, and each
  interface with its version and summary. Production code never
  registers interfaces; unit tests still may, after loading.
- **Shipped files.** MAPL ships a test registration file (starting with
  `toy_scaling`) and a GEOS registration file (starting with
  `turbulent_mixing`), both installed to `etc/`. A few hand-written
  name-constants modules come with them. There is no code generation.
- **Namespaced attribute access.** Per-member attributes live in the
  member's Info and per-State attributes and call scalars in the
  callback State's Info, all under `<interface>/<name>`. A MAPL accessor
  takes the namespace from the callback State the method was invoked
  under.
- **Invoker-side execution** (critical path, owner track): an Invoker
  runs a named method on a callback State from its collection. The
  framework runs the pre-call network, then the method, then the
  post-call network.
- **Callback-argument transforms.** Callback arguments carry
  characteristics, so a Handler on another grid gets regrid transforms.
  First-order conservative regrid is the default in both directions.
  The return path uses one horizontal map for a delta's mass part and
  for per-area outputs.
- **Weighted delta-back** for the value put flavour: the change of an
  intensive argument returns as `R†(m_T Δ_T) / m_O`, where `m` is the
  argument marked `weight_of`. It is followed by conservative column
  filling (with a diagnostic) when the owner declares `non_negative`.
- **Test tools.** A configurable Handler (first supply and record,
  under the reserved Info namespace `mapl_mock/`; expectations later)
  and a configurable Invoker. Both build their callback States from the
  registry.
- **Slices.** First `toy_scaling` (Handlers on another grid, no
  delta-back, test matrix P1-P5 and F1-F12). Then `turbulent_mixing`
  (weighted delta-back, a test Invoker, mock tracer owners, test matrix
  M1-M9 and MF1-MF9).
- **Handlers declare the interface they implement.** A Handler's
  callback-State export may declare its interface, as an Invoker's
  import already does. A pattern-matched export that declares the
  expected interface and does not conform is a wiring error, with the
  full report. A matched export that declares no interface keeps
  today's behavior: it is skipped if its members or kinds do not fit,
  and the skip is reported. This is additive: no export declares an
  interface today.

## Capabilities

### New Capabilities
- `graph/callback-registration-file`: the per-experiment YAML
  registration file. Covers its format, location, loading in
  `mapl_initialize`, validation, the freeze by the Cap, the startup
  report, documented duplicate-key behavior, and the shipped and
  installed files.
- `graph/callback-contract-conformance`: what the framework checks on a
  realized Handler callback State against its contract. Covers check
  order, collected and worded errors, defaults, namespacing, unknown
  keys, weight checks, and the namespaced attribute and call-scalar
  accessors.
- `graph/callback-test-tools`: the configurable Handler (supply, record,
  later expectations) and the configurable Invoker.

### Modified Capabilities
- `graph/callback-data-model`: interfaces gain version, summary,
  argument properties, attribute schema, call scalars, rules and
  per-argument method options. The registry gains a freeze and an
  enumeration of registered interfaces.
- `graph/callback-wiring`: adds Invoker-side execution by method name,
  characteristic-driven transforms on callback arguments (first-order
  conservative by default), weighted delta-back with conservative
  filling for the value put flavour, and a hard error for a matched
  export that declares the expected interface but does not conform.

## Impact

- **Code:** `superstructure/generic/graph/callback/` (data model,
  registry, new loader and conformance modules);
  `GraphBuilder_CallbackWiring_smod.F90` and the callback branch of
  `GraphBuilder_ConnectionResolution_smod.F90`;
  `MethodInvocationAdapter.F90` / `StateMethodInvocation.F90` (a real
  `ESMF_MethodExecute`-backed invoker); `infrastructure/esmf/InfoUtilities.F90`
  (new `MAPL_InfoGetShared` / `MAPL_InfoSetShared` specifics and a
  stored-type query); `mapl/MaplFramework.F90` (loader call);
  `mapl/CapDriver.F90` (freeze and report); new graph `Transform`s
  (weighted delta-back, conservative column fill); new test gridcomps
  under `gridcomps/` and their tests.
- **Files:** two YAML registration files plus their CMake `install`
  rules to `etc/`; a few constants modules.
- **Outside MAPL:** the GEOS setup scripts copy the GEOS file into the
  run directory and warn when it differs from the installed copy. That
  work belongs to the GEOSgcm fixture and is tracked here only.
- **Docs:** `docs/graph/callbacks.md`, `docs/graph/ARCHITECTURE.md`
  (callback rows), and the user guide section on the registration file.
- **Compatibility:** additive. No export declares an interface today,
  and nothing in production registers interfaces, so the files-only
  rule removes no existing caller.
