Completed sections moved out of `tasks.md` to keep it short (meta reads the
live file on every task scope); each section here is fully checked off. See
`tasks.md` for the one-line pointer and the remaining in-progress sections.

## 0. Probes (no production code)

- [x] 0.1 HConfig duplicate keys: a test loads a file with two
  `toy_scaling:` entries (versions 1 and 2) and records which wins and
  whether any HConfig call reports the duplicate. Done: test asserts the
  observed behavior; result written to design.md Open Questions and to
  the plan's Status line.
- [x] 0.2 ESMF Info typing: a test stores i4, i8, r4, r8, logical,
  string, and arrays of each, then records what `ESMF_InfoInquire`
  reports and whether `ESMF_InfoGet` converts (real into integer,
  string into real). Also record whether keys under a nested prefix can
  be enumerated. Done: test asserts the observed behavior; result
  recorded in design.md Decision 4.

## 1. Data model

- [x] 1.1 `CallbackArgumentSpec`: add `dims`, `optional`, `extensive`,
  `weight_for` (optional constructor arguments, defaults keep today's
  meaning); reject a field argument without dims. Unit tests.
- [x] 1.2 New `CallbackAttributeSpec` (type incl. list types, scope,
  required, default, allowed set) with its container and declaration
  checks. Unit tests.
- [x] 1.3 New `CallbackCallScalarSpec` (type, writer) with container.
  Unit tests.
- [x] 1.4 New `CallbackRule` (when attribute = value, requires list)
  with container. Unit tests.
- [x] 1.5 `CallbackMethodSpec`: per-argument options (`put_flavour`);
  reject unknown option names. Unit tests.
- [x] 1.6 `CallbackInterface`: version, summary, description;
  declare/get for attributes, call scalars, rules; enforce slash-free
  names, rule consistency, weight consistency, host-argument rules. Unit
  tests for every rejection in the data-model delta spec.
- [x] 1.7 Registry: `freeze_callback_registry`, frozen-registration
  error naming the interface, `list_callback_interface_names` (sorted).
  Unit tests.

## 2. Info support

- [x] 2.1 `InfoUtilities.F90`: stored-type query, `r8_1d`, string-list
  getters/setters, State-own-Info getters/setters for logical, i4, r4,
  r8, key enumeration under a prefix. Unit tests in the infrastructure
  test suite. Depends on 0.2.
