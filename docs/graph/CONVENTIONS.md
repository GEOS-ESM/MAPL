# Graph-layer conventions

Rules for anyone (human or agent) editing `superstructure/generic/graph/`
and `superstructure/generic/GraphBuilder*.F90`. Where this file and
`docs/graph/plans/separation-of-concerns.md` disagree, this file wins
once it exists. Root `AGENTS.md` carries the build/test procedure.

## 1. Source comments

- Comments explain **why**: a non-obvious constraint, an ordering
  requirement, a must-not-mutate rule, a compiler workaround. Not
  *what* the code does, and not *when* or *by whom* it was added.
- One to two lines per comment. A compiler-workaround comment is one
  line naming the compiler and symptom.
- Do not put `openspec/changes/...` paths, task or decision numbers
  (`task 6.8`, `design.md D3`), or change history ("previously",
  "now-removed", "Phase N added", "moved here verbatim") in source. Put
  the change name in the commit message. Git history and
  `openspec/changes/archive/` keep the rest.
- A spec reference is allowed only as a stable requirement ID that
  exists under `openspec/specs/` (for example `REQ-CG-002`), never as a
  change path.
- Module header comments state what the module is for and its layering
  constraints, not how it came to exist.
- Existing violations are being removed (plan task A4). Do not copy
  their style and do not add to them.

## 2. Layering

- `ComponentGraph`, `DependencyNetwork`, `GraphNode*`, `GraphStateItem`
  and everything else under `graph/` must not depend on
  `OuterMetaComponent`, `StateRegistry`, or any component-hierarchy
  implementation type (REQ-CG-002).
- `GraphBuilder*` is the only layer allowed to depend on both the graph
  core and `OuterMetaComponent`/`StateRegistry`.

## 3. Duplicate, do not delegate

The end state is: Aspects replaced by Characteristics, legacy
`ExtensionTransform` subclasses replaced by graph `Transform`
subclasses, and `registry/` deleted. Until then graph code must not call
into `specs/*Aspect`, `registry/`, or legacy `transforms/`. A
near-duplicate of the legacy logic in the graph layer is acceptable and
expected; a call into the legacy layer is not. (Phases 7a/7b/7c are the
precedent: `ItemShapeCharacteristic`, `ExpressionTransform`, wildcard
fan-in are independent reimplementations.)

## 4. GraphBuilder is characteristic-agnostic (target rule)

`GraphBuilder.F90` and its submodules should not name any
`*_CHARACTERISTIC_KIND` constant or concrete `*Characteristic` type;
characteristic-specific behavior belongs in the characteristic. This is
the target (plan decision D6) and **is not true of the current code**:
`GraphBuilder_Advertising_smod.F90`, `GraphBuilder_Geometry_smod.F90`,
`GraphBuilder_ConnectionResolution_smod.F90`, and `GraphBuilder.F90`
still name kinds. Do not add new references. Plan task B4 removes the
remaining ones.

## 5. Fortran and compiler pitfalls

- **`public ::` for submodule helpers.** A plain procedure defined in
  `GraphBuilder.F90` and called from a submodule needs an explicit
  `public ::`. NAG accepts a missing one; gfortran fails at link
  (`Undefined symbols`). Always build both compilers.
- **gFTL containers: pointer access, not copies.** Prefer
  `container%at(k)` / iterator pointers over assigning a populated
  container, because deep defined-assignment can break compilers. Treat
  problems as exceptional; do not add speculative workarounds.
- **Long `_ASSERT`/`_FAIL` messages.** Build the message in a local
  `character(:), allocatable` variable first. A macro argument cannot be
  split across lines: gfortran's preprocessor does not accept `&`
  continuation inside a macro argument list, and a split without `&`
  fails under NAG.
- **pflogger `%a` substitution** is unreliable under gfortran for
  allocatable-character actuals; pre-concatenate the string.
- **gFTL iteration:** `iter = c%ftn_begin()`, and call `iter%next()` at
  the *top* of the loop body before first use.
- Error handling uses the MAPL macros (`_RC`, `_ASSERT`, `_FAIL`,
  `_RETURN`, `_VERIFY`); see `.opencode/skills/mapl-error-handling`.
- Mutate through named accessors, not raw component access, even inside
  the same file family.

## 5a. Info access

Graph code (`graph/`, `GraphBuilder*.F90`) never calls `ESMF_Info*`,
`MAPL_Info*`, `FieldInfo*` or `FieldBundleInfo*`. It reads and writes
framework-managed Info only through `MAPL_FieldSet/Get`,
`MAPL_FieldBundleSet/Get` and `MAPL_StateSet/Get`; key
strings live in `utils/MAPL_ESMF_InfoKeys.F90` and the infrastructure Info
modules. Not enforced by a test; fix violations when noticed. A
missing setter argument is added to the setter, not worked around with raw
Info.

## 6. Tests

- `ctest -L ESSENTIAL` must be green on NAG 7.2.41 and on gfortran
  before every commit. NAG must be exactly 7.2.41: newer NAG releases
  have a compiler bug that breaks MAPL.
- Pending or expected-fail work is gated by an explicit toggle, never by
  a red test.
- A test that provokes a mismatch on a characteristic axis without a
  real `build_transform` must consume the `_FAIL` exception first
  (`@assertExceptionRaised(...)`); write that expectation first, not
  after discovering it.
- Never call a failure "pre-existing" without reproducing it on a clean
  baseline: a temporary `git worktree` of `HEAD`, removed afterward.
  Reverting only your own newest file proves nothing about the rest of
  your diff.
- Negative-path tests that log expected warnings belong in the `quiet`
  suite (`MAPL.generic.components.quiet`, run at `--level ERROR`).
- Check each legacy `*Aspect%can_connect_to`/`matches` for a
  `ValidationMode` gate before co-mismatching axes in a new scenario:
  `UngriddedDimsAspect` rejects unconditionally, `StandardNameAspect`
  only under strict mode, and legacy runs alongside the graph path in
  every `graph_mode()`.

## 7. Commits and workflow

- One plan task = one commit (or a short series). The message ends with
  a trailer naming the task: `Plan-Task: <label>`.
- The body contains both `ctest -L ESSENTIAL` summary lines (NAG and
  gfortran).
- Commit only locally. Never push; never rewrite commits that are not
  your task's. Do not commit files you did not create.
- Do not record commit hashes in docs; the trailer is the reference
  (`git log --grep='^Plan-Task: B3-Units' -p`).
- On an ICE, a nondeterministic segfault, or a gFTL copy crash: stop and
  report. Do not invent workarounds.
- If the plan and the source disagree: stop and report.

## 8. Documentation upkeep

- `docs/graph/ARCHITECTURE.md` describes the current tree only. Put
  session narratives and per-change accounting in `docs/graph/history/`.
- If a doc and the source disagree, trust the source and fix the doc in
  the same change.
- Terminology: see `docs/graph/GLOSSARY.md` for the legacy-to-graph
  mapping.
