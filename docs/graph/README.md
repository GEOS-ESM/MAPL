# Graph layer documentation

High-level, implementation-tracking documentation for
`superstructure/generic/graph/` and the `GraphBuilder` integration layer
(`superstructure/generic/GraphBuilder*.F90`). Written for AI agents making
further changes in this layer — short, points at real `file:line`
locations, describes current behavior rather than design history.

**This supersedes `spec/` as the primary context for agent-assisted work
in the graph layer.** `spec/` is kept, unmodified, as the historical
design record (rationale, alternatives considered, the original
open-questions discussion) — read it only when you need the *why* behind
a decision these docs just state as fact. Do not load `spec/` into
context by default; it is ~5600 lines across 20 files and will burn
budget fast for little marginal benefit on a typical change.

## How to use these docs

1. Start with this file to find the right topic file.
2. Read the one or two topic files relevant to your change. Each is
   self-contained and short (most under 150 lines).
3. Follow the `file:line` references into real source when you need
   exact signatures — these docs describe shape and intent, not every
   argument.
4. If source and docs disagree, trust source, then fix the doc in the
   same change (these docs are expected to be kept in sync, unlike
   `spec/`, which is a frozen record).
5. Check `open-questions.md` before making a design decision in an area
   it covers — a handful of real architectural questions are still
   genuinely unresolved.

## Topic map

| File | Covers |
|---|---|
| `node-hierarchy.md` | `GraphNode`/`BaseGraphNode`/`StateItemNode`/`OperationGraphNode`/`TransformGraphNode`/`MethodGraphNode`, `NodeRevision` |
| `graph-state-item.md` | `GraphStateItem` payload type, membership maps, variant/itemType classification |
| `identities.md` | `NodeId` and sibling id types, the `IdTemplate.inc` generator pattern |
| `dependency-network.md` | `DependencyNetwork` adjacency, cycle rejection, validation |
| `hierarchy-and-component-graph.md` | `HierarchyGraph`/`HierarchyRegistry` (shared storage) vs. `ComponentGraph` (per-component ownership), ports, lifecycle |
| `transforms-and-update.md` | `Transform`/`TransformBase`/`TransformGraphNode`, port bindings, the demand-driven `update()` algorithm |
| `methods-and-drivers.md` | `MethodGraphNode`, invocation adapters, `GriddedComponentDriver` integration |
| `characteristics.md` | `StateItemCharacteristic` hierarchy, extension/mismatch reconciliation |
| `callbacks.md` | `CallbackInterface`/registry, Handler/Invoker, callback State wiring |
| `route-handles.md` | `RouteHandleKey`, route handle sharing/representation |
| `visualization-export.md` | `GraphExport` DOT/JSON topology export |
| `graph-builder.md` | The `GraphBuilder` integration layer: advertising, connection resolution, extension reuse, geometry, state population, submodule map |
| `open-questions.md` | Condensed list of genuinely unresolved design questions (full discussion remains in `spec/17-open-questions.md`) |

## Source layout at a glance

- `superstructure/generic/graph/` — graph-neutral core. Must not depend on
  `OuterComponent`/`StateRegistry`/`GriddedComponentDriver`/`GraphBuilder`.
  ~11,800 lines across ~70 modules + `containers/` (gFTL instantiations)
  + `tests/`.
- `superstructure/generic/GraphBuilder*.F90` — the integration layer
  between the graph core and the rest of MAPL (`OuterMetaComponent`,
  `StateRegistry`, connection resolution). ~4,500 lines: `GraphBuilder.F90`
  (public API + shared helpers) plus ten `GraphBuilder_*_smod.F90`
  submodules, one per responsibility area.

## Conventions seen throughout the source

- Every module header comment already cites the `spec/NN-*.md`
  requirement IDs and/or `openspec/changes/...` design docs that drove
  it — when a module header already answers your question, prefer it
  over re-deriving the same information here.
- Mutation only through named accessor methods, never raw component
  access, even within the same file family (e.g. `NodeRevision`,
  `StateItemNode`, `StateItemCharacteristic`).
- gFTL map/set iteration convention: `iter = c%ftn_begin()`, call
  `%next()` at the *top* of the loop body before first use.
- `_ASSERT`/`_RETURN`/`_VERIFY` MAPL error-handling macros
  (`mapl_ErrorHandling_mod`) are used uniformly; see
  `.opencode/skills/mapl-error-handling/SKILL.md` for the project-wide
  convention.
