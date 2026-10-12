## Why

`docs/graph/spec/20-implementation-roadmap.md`'s own phase-sequencing
sections (§20.4.1, §20.4.3, §20.4.4, §20.4.8) each independently
document the same complaint: a unit of work "does not fit a single
spec-driven change proposal without an unreasonable context/cost
footprint." `superstructure/generic/GraphBuilder.F90` is the single
largest concrete instance of that cost today: 3457 lines, 57
module-level procedures, covering nine functionally distinct concerns
(advertising, ordinary connection resolution, public ports/child
proxies, horizontal geometry, freeze, graph-native state population,
run-phase pull, visualization label-mapping, callback wiring,
ordinary-phase method-node construction, lifecycle hook wrappers) in
one file. Confirmed directly this session (`phase-11a-export-field-
aliasing` planning): tracing one call path (a sibling connection's
export-side proxy materialization) required reading roughly fifteen
separate excerpts scattered across the full 3457-line span, because the
procedures involved sit far apart with no file-level boundary to narrow
the search.

This codebase already has an established, working answer to the same
problem for `StateRegistry`'s own comparable-sized job:
`superstructure/generic/registry/StateRegistry.F90` (312 lines - type
declaration, public API, module-procedure interface block) plus six
`StateRegistry_*_smod.F90` files (`_Actions_smod`, `_Propagation_smod`,
`_Lifecycle_smod`, `_Hierarchy_smod`, `_Extensions_smod`,
`_SetGeometry_smod` - 79 to 261 lines each). `GraphBuilder.F90` has no
equivalent split, despite the module header's own existing "Task N"
section comments already marking almost exactly the same seams a
submodule split would use.

## What Changes

- Split `GraphBuilder.F90`'s procedure bodies into Fortran submodules,
  one per functionally-independent "Task" section the file's own header
  comment already names, mirroring `StateRegistry.F90`'s own established
  shape: the ancestor module keeps the `GraphBuilder` type declaration,
  its public type-bound procedure bindings, the small set of private
  helper procedures genuinely shared across more than one section
  (identified below), and a `module function`/`module subroutine`
  interface block for every procedure whose body moves to a submodule;
  each submodule provides the bodies for one section's own procedures.
- **BREAKING**: none - this is a pure internal reorganization. No
  procedure's name, signature, calling convention, or externally
  observable behavior changes. Every existing caller (`OuterMetaComponent/
  *.F90`'s hook call sites, every pFUnit test file referencing
  `mapl_GraphBuilder_mod`) continues to compile and run unmodified.
- No change to `mapl_GraphBuilder_mod`'s own public interface
  (`proxy_key`, `item_key`, `QualifiedExportEntry`, and the callback-
  wiring internals already exposed `public` for direct test coverage,
  per the module's own existing header comments explaining why) - every
  existing `use mapl_GraphBuilder_mod, only: ...` statement in test code
  continues to resolve identically.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None - no requirement in any `openspec/specs/` capability changes
behavior. `.openspec.yaml` for this change sets `skip_specs: true`,
matching the existing `phase-5-sub-sequencing`/`phase-10-sub-sequencing`/
`phase-11-sub-sequencing` precedent for a planning-or-refactor-only
change with no spec-level behavior change.

## Impact

- **Affected code:** `superstructure/generic/GraphBuilder.F90` only -
  split into the ancestor file (type, public API, shared helpers,
  interface block) plus new `GraphBuilder_*_smod.F90` sibling files in
  the same directory. No other file's own source changes; every caller
  outside this one module is unaffected by construction (same public
  interface).
- **Build system:** new submodule files need adding to
  `superstructure/generic/CMakeLists.txt`'s own existing source list
  (wherever `GraphBuilder.F90` is listed today) - mirroring exactly how
  `StateRegistry.F90`'s own six `_smod.F90` siblings are already listed
  alongside it.
- **No behavior change, no test change required** - every existing
  pFUnit test exercising `mapl_GraphBuilder_mod` (`Test_GraphBuilder.pf`,
  `Test_GraphBuilderEquivalence.pf`, `Test_GraphModeDemonstration.pf`,
  `Test_GraphEquivalenceHarness.pf`, and others) continues to pass
  unmodified - this change's own verification is "the existing test
  suite still passes," not new test coverage.
- **Downstream effect:** future changes touching one of `GraphBuilder`'s
  nine sections (e.g. a future callback-wiring or geometry sub-change)
  need only read that section's own, much smaller submodule file, not
  the full 3457-line original - directly addressing the context-cost
  problem named in the Why section above.
