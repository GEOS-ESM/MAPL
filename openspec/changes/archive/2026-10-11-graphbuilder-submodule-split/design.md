## Context

`StateRegistry.F90` (312 lines) + six `StateRegistry_*_smod.F90` files
(79-261 lines each) is this codebase's own established, working
precedent for exactly this split: ancestor module keeps the type
declaration, public API, and a `module function`/`module subroutine`
interface block; each submodule (`submodule (mapl_StateRegistry_mod)
..._smod`) provides bodies for one functional area. `ComponentGraph.F90`
+ `ComponentGraph_DemandDrivenUpdate.F90` is a second, smaller precedent
(one submodule, one procedure) - its own header comment records a real
toolchain landmine found and worked around: a submodule declaring its
own module-level derived type triggered an ifx shared-library
symbol-versioning failure (`ld: version node not found for symbol
...@..._UPDATEFRAME`); the documented, now-established rule is "every
submodule only implements interface bodies, never declares its own
types." `GraphBuilder.F90`'s own two module-level types
(`GraphBuilder`, `QualifiedExportEntry`) both stay in the ancestor
module under this plan (see Decision D3) - this rule is not violated,
but is worth stating explicitly since this is the precedent that
caught it the first time.

A direct cross-reference analysis of all 57 of `GraphBuilder.F90`'s own
module-level procedures (every call site matched against every
definition, not assumed) found the file's own existing "Task N" header
comments already correspond almost exactly to a correct submodule
boundary - eleven sections, each internally cohesive - with a small,
fully-enumerated set of procedures called across section boundaries
(listed in Decision D2 below). This matches `StateRegistry`'s own
precedent shape closely: that split also required a handful of
genuinely cross-cutting helpers to stay in the ancestor module.

(Correction, found while applying this change, tasks.md: the original
analysis's own count of 53 omitted four procedures that do exist in the
file - `is_self`, `find_export_node_id`, `try_alias_export_field`,
`collect_qualified_exports`. Each is classified below by the exact same
rule D1/D2 already use for every other procedure - checked against
every one of its own call sites, not assumed: `is_self` is called from
both PortsAndProxies-section call sites (`component_spec_for`/
`component_for`/`get_or_make_local_node_id`, already D2 shared helpers)
AND from CallbackWiring-section call sites
(`resolve_qualified_export_node_id`, `build_callback_state_binding`) -
a genuine cross-section call pattern, so it joins D2's shared-helper
list. `find_export_node_id` and `try_alias_export_field` are each
called only from within ConnectionResolution (`resolve_match_connection`'s
own `resolve_one`, plus `resolve_inout_destination` for the former) -
single-section, so each joins D1's ConnectionResolution row.
`collect_qualified_exports` is called only from
`build_qualified_export_namespace`, itself in CallbackWiring - joins
D1's CallbackWiring row. 57 total, now fully accounted for.)

## Goals / Non-Goals

**Goals:**
- Reduce the file an agent (or human) must read to understand or modify
  one functional area of `GraphBuilder` from 3457 lines to one
  submodule's own, much smaller span (79-700 lines, matching
  `StateRegistry`'s own observed range).
- Zero behavior change - every procedure's name, signature, and calling
  convention stays exactly as it is today.
- Follow the already-established `StateRegistry.F90` shape exactly, not
  invent a new convention - lowest-risk option, already proven to build
  and work in this codebase under both gfortran and NAG (and, per
  `ComponentGraph`'s own precedent, with ifx's own known landmine
  already avoided by construction).

**Non-Goals:**
- No change to `GraphBuilder`'s own public interface - every `public ::`
  statement, every type-bound procedure name, every explicitly-exposed
  internal (`proxy_key`, `item_key`, `QualifiedExportEntry`, the
  callback-wiring internals) stays exported exactly as today.
- No change to any call site outside this one module
  (`OuterMetaComponent/*.F90`'s hooks, every pFUnit test file) - this is
  the change's own primary correctness invariant, not merely a
  convenience.
- No change to procedure bodies themselves beyond what moving them
  between files mechanically requires (removing the `subroutine`/
  `function`/`end subroutine`/`end function` lines in favor of `module
  subroutine`/`module function` in the submodule, matching
  `StateRegistry_Actions_smod.F90`'s own exact convention) - no
  refactoring of logic, no renaming, no comment rewording beyond moving
  the comment with its procedure.
- Not attempting to also re-examine whether the eleven sections
  themselves are the "right" decomposition from a design standpoint -
  reusing the module's own existing, already-reviewed "Task N" boundary
  as given, not re-deriving it.

## Decisions

**D1: Eleven submodule files, one per existing "Task N" header section.**
Confirmed by direct line-range + call-site analysis (not merely the
header comment's own prose) that these eleven groupings are each
self-contained aside from the shared helpers named in D2:

| Submodule file | Section (module header's own name) | Procedures | Approx. lines |
|---|---|---|---|
| `GraphBuilder_Advertising_smod.F90` | Task 2: Advertising | `graphbuilder_advertise`, `advertise_one` | ~95 |
| `GraphBuilder_ConnectionResolution_smod.F90` | Task 3: Ordinary connection resolution | `graphbuilder_check_unsatisfied_imports`, `for_each_matching_import`, `check_match_connection_unsatisfied`, `graphbuilder_resolve_connections`, `resolve_match_connection`, `build_characteristics`, `resolve_inout_destination`, `find_export_var_spec`, `find_inout_borrower_var_spec`, `find_export_node_id`, `try_alias_export_field` | ~800 |
| `GraphBuilder_PortsAndProxies_smod.F90` | Task 4: Public ports and child proxies | `component_spec_for`, `component_for`, `try_materialize_field_extension`, `resolve_export_geom`, `resolve_export_vgrid`, `itemtype_name`, `get_or_make_local_node_id` | ~315 |
| `GraphBuilder_Geometry_smod.F90` | Horizontal geometry (Phase 4e) | `graphbuilder_run_geometry_hook`, `graphbuilder_advertise_geometry`, `parent_geometry_proxy_key`, `graphbuilder_resolve_geometry`, `resolve_geometry_from_child`, `push_geometry_to_child` | ~265 |
| `GraphBuilder_Freeze_smod.F90` | Task 5: Validate and freeze | `graphbuilder_freeze` | ~15 |
| `GraphBuilder_StatePopulation_smod.F90` | graph-native state population (phase-10a) | `graphbuilder_populate_state`, `populate_one`, `get_field_if_materialized`, `find_materialized_predecessor` | ~330 |
| `GraphBuilder_RunPhasePull_smod.F90` | re-propagation pull (graph-child-state-population D7) | `graphbuilder_run_pull_hook` | ~40 |
| `GraphBuilder_Visualization_smod.F90` | Task 6: Visualization enrichment | `graphbuilder_build_label_map` | ~60 |
| `GraphBuilder_CallbackWiring_smod.F90` | Callback wiring (Phase 4d) | `build_qualified_export_namespace`, `collect_qualified_exports`, `resolve_descendant`, `resolve_qualified_export_node_id`, `export_conforms_to_interface`, `resolve_callback_import`, `materialize_callback_collection`, `resolve_callback_destination`, `build_callback_state_binding`, `build_callback_method_binding`, `wire_callback_argument`, `invoke_callback_method`, `pull_callback_get_arguments`, `advance_callback_put_arguments` | ~800 |
| `GraphBuilder_RunPhaseNode_smod.F90` | Ordinary-phase MethodGraphNode (phase-10d) | `graphbuilder_build_run_phase_node` | ~90 |
| `GraphBuilder_LifecycleHooks_smod.F90` | Lifecycle-hook wrappers (task 2.3/3.4) | `graphbuilder_run_advertise_hook`, `graphbuilder_run_activate_hook`, `graphbuilder_run_connect_hook`, `graphbuilder_run_populate_state_hook`, `assert_converged`, `report_if_failed` | ~175 |

Totals (~2840 of the original ~3457 lines move; the remainder - type
declaration, public bindings, shared helpers, interface block, module
header comment - stays in the ancestor file, now roughly 500-550 lines).

**D2: Shared helpers that stay in the ancestor module, not any one
submodule.**
Cross-reference analysis (every call site checked against every
definition) found exactly these procedures called from a section other
than the one that defines them - each must stay directly `contains`-ed
in `mapl_GraphBuilder_mod` itself (ordinary host association, visible to
every submodule automatically, same as `StateRegistry.F90`'s own
`SELF` parameter and any ancestor-module-level helper) rather than
living in any one submodule:

- `item_key`, `proxy_key` (Advertising's own key-builders - called from
  Geometry, StatePopulation, Visualization, CallbackWiring,
  RunPhaseNode, PortsAndProxies, and the type-bound-procedure
  dispatch table itself)
- `component_for`, `component_spec_for`, `get_or_make_local_node_id`,
  `try_materialize_field_extension`, `itemtype_name`
  (PortsAndProxies's own cross-component accessors - called from
  ConnectionResolution, Geometry, CallbackWiring)
- `resolve_callback_import`, `resolve_callback_destination`
  (CallbackWiring's own entry points - called from
  ConnectionResolution's `resolve_match_connection`, per the
  callback-wiring change's own Decision 0 dispatch)
- `is_self` (PortsAndProxies's own self-vs-child discriminator -
  called from `component_spec_for`/`component_for`/
  `get_or_make_local_node_id`, already in this list, AND from
  CallbackWiring's `resolve_qualified_export_node_id`/
  `build_callback_state_binding` - a genuine cross-section call,
  found during this change's own task 1.2 cross-check against the
  file's actual 57 procedures, not the original 53-count analysis)

These move to a new, clearly-labeled section near the top of the
ancestor module (`! ---- Shared helpers, used across more than one
submodule ----`), ahead of the interface block - the same place
`StateRegistry.F90` keeps its own `SELF` parameter. No procedure in
this list changes signature or behavior; only its physical location
within the ancestor file changes (it stays `contains`-ed directly in
the module, never moves to a submodule).

Every type-bound-procedure dispatch target itself
(`graphbuilder_advertise`, `graphbuilder_check_unsatisfied_imports`,
`graphbuilder_resolve_connections`, `graphbuilder_freeze`,
`graphbuilder_build_label_map`, `graphbuilder_run_advertise_hook`,
`graphbuilder_run_activate_hook`, `graphbuilder_run_connect_hook`,
`graphbuilder_run_geometry_hook`, `graphbuilder_populate_state`,
`graphbuilder_run_populate_state_hook`,
`graphbuilder_build_run_phase_node`) is called only from the `type ::
GraphBuilder` binding table itself (which stays in the ancestor module,
D3) or from one other hook within `LifecycleHooks` - never from
arbitrary external code - so none of these need special handling beyond
an ordinary `module subroutine`/`module function` interface declaration.

**D3: What stays in the ancestor module (`GraphBuilder.F90` itself).**
- The module header comment (unchanged).
- `use` statements (redistributed: each submodule gets only the `use`
  lines its own moved procedures actually need, checked individually -
  not a blanket `use` of everything into every file).
- The `type :: GraphBuilder` declaration and its full type-bound
  procedure table (unchanged - every binding already uses `nopass`,
  per the type's own existing "stateless-per-call" design, so moving
  the bodies elsewhere requires no change here beyond the body itself
  becoming a `module subroutine`/`module function` reference).
- The `type :: QualifiedExportEntry` declaration (used across
  CallbackWiring's own procedures, but it is a type declaration, not a
  procedure - per `ComponentGraph`'s own documented ifx landmine
  (Context), stays in the ancestor module rather than moving to the
  CallbackWiring submodule, to avoid even approaching that known-bad
  pattern).
- D2's shared helpers.
- A `module function`/`module subroutine` interface block, one entry
  per procedure that moves to a submodule - mirrors
  `StateRegistry.F90`'s own interface block exactly in style (grouped
  by section with a one-line comment heading each group, matching that
  file's own "! Lifecycle procedures" / "! Hierarchy procedures" style).

**D4: Submodule naming and `submodule` statement form.**
`submodule (mapl_GraphBuilder_mod) GraphBuilder_<Section>_smod`,
mirroring `StateRegistry_Actions_smod`'s own exact naming convention
(ancestor module name as the parent, `_smod` suffix on the submodule
name itself). File name matches the submodule name
(`GraphBuilder_Advertising_smod.F90`), matching
`StateRegistry_Actions_smod.F90`'s own file-naming precedent exactly
(not `ComponentGraph_DemandDrivenUpdate.F90`'s own differently-styled
descriptive name - `StateRegistry`'s naming is the closer precedent
here since this split, like that one, is organized by functional area
rather than by one single named algorithm).

**D5: Build system registration.**
Add each new `GraphBuilder_*_smod.F90` file to
`superstructure/generic/CMakeLists.txt`'s own existing source list,
immediately alongside the existing `GraphBuilder.F90` entry (confirmed:
that file lists `GraphBuilder.F90` directly, line 18 as of this
writing; `StateRegistry.F90`'s own six `_smod.F90` siblings are listed
the identical way, in their own directory's sibling
`superstructure/generic/registry/CMakeLists.txt` - same mechanism,
different directory, since `StateRegistry` already has its own
subdirectory and `GraphBuilder` does not). No new CMake target,
library, subdirectory, or dependency - these are additional source
files compiled into the same existing `MAPL.generic` target
`GraphBuilder.F90` already belongs to.

**D6: Verification strategy - rebuild + full existing test suite, no
new tests.**
Since D goals explicitly exclude any behavior change, this change's own
correctness bar is "builds clean under every compiler this project
supports (gfortran, NAG, Intel/ifx per `compiler-switching` skill) and
every existing test referencing `mapl_GraphBuilder_mod` still passes
unmodified" - not new test coverage. `Test_GraphBuilder.pf`,
`Test_GraphBuilderEquivalence.pf`, `Test_GraphModeDemonstration.pf`,
`Test_GraphEquivalenceHarness.pf`, `Test_GraphGeometryHook.pf`, and any
other existing suite touching this module are the regression gate.

## Risks / Trade-offs

- **[Risk]** A `use` statement redistributed too narrowly (D3) could
  leave a submodule procedure missing a dependency it actually needs,
  caught only at compile time - mitigation: compile incrementally, one
  submodule at a time (tasks.md), rather than moving all eleven sections
  in one shot, so a missing `use` is immediately localized to the
  section just moved rather than requiring a bisection across the whole
  file afterward.
- **[Risk]** Fortran submodule compilation order matters to some build
  systems (a submodule's `.mod`/`.smod` dependency on its ancestor
  module must be expressed correctly in CMake's own dependency
  tracking) - mitigation: D5's "mirror `StateRegistry`'s own existing
  CMakeLists.txt entry exactly" sidesteps this; that pattern is already
  proven working in this exact build system, not a new mechanism being
  tried for the first time.
- **[Trade-off]** Eleven files instead of one means an agent needing to
  understand `GraphBuilder` as a *whole* (rare, but not impossible - the
  module header comment's own cross-cutting discussion, e.g. "Two-phase
  timing," spans multiple sections) now needs to open more than one
  file. Accepted: the module header comment itself (which contains that
  cross-cutting narrative) stays in the ancestor file regardless, so
  the single highest-value entry point for understanding the module's
  own overall shape is unchanged; only per-section implementation detail
  moves out.
- **[Trade-off]** D2's shared-helper list is a one-time snapshot of
  today's cross-section call graph. A future change could add a new
  cross-section call (e.g. a new section needing `item_key` for the
  first time) - no different in kind from `StateRegistry`'s own
  already-live equivalent risk, and already mitigated the same way that
  file handles it: a shared helper lives in the ancestor module
  specifically so any submodule can reach it via ordinary host
  association, with no further change needed when a new submodule
  starts calling an existing shared helper.
