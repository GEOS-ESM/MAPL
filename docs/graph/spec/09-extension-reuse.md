# 9. Existing Extension Behavior to Preserve

Status: `[SETTLED]` — this is a backward-compatibility requirement, not new
design.

## 9.1 Extension chains

**REQ-EXT-001.** For an ordinary export/import pair differing in two
respects (e.g., grid and units), `GraphBuilder` MAY create a chain such as:

```
E -> T1 -> E1 -> T2 -> E2
```

where `E` is the original export value, `T1`/`T2` are Transforms, and
`E1`/`E2` are framework-created extension values.

**REQ-EXT-002.** The import `GraphStateItem` MUST remain a distinct item (its own
`StateItemNode`/map entry), but its ESMF payload MUST be shallow-assigned
or aliased from `E2` (the final extension in the chain) — it does not get
its own independently-materialized storage.

### 9.1.1 Speculative alternative: shared-data payload instead of alias `[SPECULATIVE]`

**Motivation.** REQ-EXT-002's alias-based payload sharing has a known limit
for some NUOPC use cases: `StateRegistry` currently does not support an
import payload that is independent-but-pointer-associated ("shared data")
rather than a true `ESMF_NamedAlias`, because aliasing is what makes
reallocation transparent — when the export/extension Field is reallocated
(e.g. due to a geometry change, `18-state-item-characteristics.md` §18.8),
an alias automatically observes the new data array with no extra step,
whereas a shared-data import's independent Field object would keep
pointing at the *old*, now-stale memory unless something re-establishes the
pointer association.

**REQ-EXT-002a `[SPECULATIVE]`.** If Graph can account for this, a
shared-data payload mode (an import `GraphStateItem` with its own distinct
`ESMF_Field`/handle, manually pointer-associated to the export's/
extension's data array, rather than a true alias) is the preferred
mechanism over the current alias-only approach, for whatever NUOPC use
cases require the import to have independent Field identity/Info.

**Proposed mechanism — not a new one.** This is structurally the same
problem already solved for Field reallocation in
`18-state-item-characteristics.md` §18.8: a shared-data import is simply
one more kind of **direct structural dependent**. When the eager mutator
(REQ-CHAR-016) reallocates an export/extension Field, the same synchronous,
non-reentrant walk (REQ-CHAR-017 — no user code invoked) would, for each
shared-data-linked import, re-establish that import's raw data pointer
against the new data array — analogous to the `ESMF_FieldEmptyReset` step
for an ordinary alias-holding dependent, just a different concrete action
for this payload mode.

**`[OPEN]`** The concrete ESMF-level mechanics of establishing and
re-establishing a "shared data" pointer association (as opposed to a true
alias) are not specified here and need to be worked out before this
sub-section can move past `[SPECULATIVE]`.

## 9.2 No-op case

**REQ-EXT-003.** If the original export already matches the import
requirement exactly (`import payload = export payload`, no grid/precision/
units/other mismatch), `GraphBuilder` MUST NOT create any new field storage
or Transform. The import item's payload is the export's payload, full stop.

## 9.3 Visibility of extensions

**REQ-EXT-004.** Every framework-created extension item MUST appear in the
appropriate `OuterComponent` state and MUST be registered in
`StateRegistry`. The graph MUST NOT create ESMF payloads that are invisible
to the existing state/registry machinery — anything the graph materializes
as an ESMF object must be discoverable the same way pre-graph MAPL
extensions were discoverable.

## 9.4 Reuse search becomes a graph search

**REQ-EXT-005.** The existing "extension-family search" (finding whether a
suitable extension already exists so it can be reused rather than
duplicated) MUST become a search over the `DependencyNetwork` region rooted
at the original export's `StateItemNode`. This is a direct restatement of
an existing imperative algorithm as a graph traversal — it MUST NOT change
observable behavior (which extensions get reused) as part of this
migration; that is a separate, explicitly-scoped optimization if pursued
later.
