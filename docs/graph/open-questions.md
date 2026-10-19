# Open design questions (condensed)

Full discussion, confidence levels, and resolution history for every
item below (and every now-settled item) lives in
`spec/17-open-questions.md` (Q1–Q18) — read that file only if you need
the *reasoning*, not just the current state. This file lists only what
is **still genuinely open** as of the last pass through that document.

| # | Question | Current lean / constraint |
|---|---|---|
| Q2 | Invocation adapter design: is the `GridCompMethodInvocation`/`StateMethodInvocation` split the right shape? | Shape is a recommendation, not yet confirmed against a real driver "invoke with named bindings" signature. See `methods-and-drivers.md`. |
| Q6 | Are "Handler"/"Invoker" the best terms? | Low-stakes; keep unless a stakeholder objects. See `callbacks.md`. |
| Q7 | Distinguishing ESMF placement (`StateIntent`) from callback Handler/Invoker role, without overloading `StateIntent` | Lean: add a separate `CallbackRole` attribute on `CallbackStateBinding`, don't touch `StateIntent`. Not checked against every `StateIntent`-branching call site. |
| Q8 | Composite (State/FieldBundle) revision granularity: container-authoritative vs. independent member-level revisions | Lean: container-authoritative (matches REQ-REV-008). Not stress-tested against `StateValue`'s more heterogeneous membership. |
| Q9 | Compiling the frozen graph to direct ESMF/MAPL wrapper calls (performance) | Confirmed: later phase, gated on the interpreted reference implementation being validated first. Mechanism unspecified. Scoped per-`ComponentGraph` (no global-graph compilation step). |
| Q12 (partial) | Mechanism for identifying "direct structural dependents" of a characteristic change across `ComponentGraph` boundaries | Within one graph: dispatch on `DependencyNetwork` successor kind (settled). Across components: a parent-orchestrated recursive walk through proxy/port wiring is sketched, not implemented. See `characteristics.md`. |
| Q13 | Characteristic transform-insertion order delegation mechanism | Lean: static per-`variant()`-kind strategy table (ordered list of `CharacteristicType`), not a "primary characteristic" method. No concrete end-to-end case worked through yet. See `characteristics.md`. |
| Q15 | Visualization export exact DOT/JSON attribute conventions | Deliberately not fixed in spec text — reference implementation (`GraphExport.F90`) defines and versions it (`GRAPH_EXPORT_SCHEMA_VERSION`). See `visualization-export.md`. |
| Q16 | Visualization export: repeated/time-series snapshot calls across runtime | Speculative pending a real use case. Constraint: the mechanism must not be designed in a way that *precludes* repeated calls (no hidden single-shot assumption). |
| Q18 | Visualization export: hierarchy-wide (multi-`ComponentGraph`) composition | Lean: per-component export alone is likely sufficient for now; defer composition until/unless a global graph view is undertaken. |
| §16 (whole doc) | Ordinary (non-callback) inout items — forward/return dual-network model | **Deferred** at the spec author's request, speculative beyond the direct-alias case. Only the direct-alias special case (`is_inout_borrower`, shared ESMF payload via `ESMF_NamedAlias`, no mismatch) is implemented. See `StateItemNode.F90`'s `is_inout_borrower` field. |
| RouteHandle time-dependent renewal | What happens when geometry changes under a route handle already shared/in-use | Not fully worked out. See `route-handles.md`. |

## Settled items worth knowing exist (no longer open, but non-obvious)

These are **not** open, but are easy to get wrong by re-deriving from
first principles instead of checking — each is covered in its own
topic file:

- Port binding storage is **external**, uniform for Transforms and
  callbacks alike (`transforms-and-update.md`, was Q3).
- `StateItemCharacteristic` instances are **not** graph nodes; no
  bespoke observer node exists (`characteristics.md`, was Q12 main).
- A route handle is `esmf_state` in a persistent wrapper role, **not**
  a separate `GraphStateItem` component (`route-handles.md`, was part
  of Q11).
- `VerticalGrid` is represented as an `ESMF_State` (member Fields per
  physical dimension), requiring **no** structural change to
  `GraphStateItem` (`graph-state-item.md`, was Q14).
- Parent-invokes-child-method is **not** represented as a dependency
  edge in the parent's graph for mid-`Run` invocation (init-phase
  invocation may still be) — was Q1.
- The same `StateItemNode` must never be written by more than one
  producer within a single update pass, enforced across all networks
  at construction time, not just per-network — was Q4.
