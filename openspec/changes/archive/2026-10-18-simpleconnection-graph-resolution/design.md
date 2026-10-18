## Context

See `proposal.md` for motivation. Relevant current state, verified by
reading source directly:

- `Connection` (abstract, `connection/Connection.F90:8`) has three
  concrete subtypes: `SimpleConnection` (exact 1:1,
  `connection/SimpleConnection.F90:28`), `MatchConnection` (regex
  enumerator, `connection/MatchConnection.F90:24`), and
  `ReexportConnection` (export-to-export passthrough,
  `connection/ReexportConnection.F90:22`). Legacy's own
  `MatchConnection%activate`/`connect` filter the destination's
  declared imports against the destination pattern, then for each
  match construct a `SimpleConnection(s_pt, d_pt)` and delegate to its
  `activate`/`connect` (`MatchConnection.F90:109-111,166-167`).
  `SimpleConnection` is legacy's real wiring primitive;
  `MatchConnection` is a thin enumerator on top of it.
- `GraphBuilder_ConnectionResolution_smod.F90` has three
  `select type (c)` dispatch points, all with the identical shape:
  `class is (MatchConnection)` → call the real resolver; `class
  default` → no-op (two sites,
  `:427-433`/`:1028-1034`) or a narrow name-matching heuristic (one
  site, `connection_resolves_import`, `:602-617`).
- `resolve_match_connection` (`:122-397`) and
  `check_match_connection_unsatisfied` (`:42-108`) both take `type(MatchConnection),
  intent(in) :: conn` as their dummy argument, but touch `conn` only
  through `conn%get_source()`/`conn%get_destination()` — both declared
  on the abstract `Connection` base
  (`connection/Connection.F90:10-11`). Every line of real resolution
  logic inside `resolve_one`
  (`GraphBuilder_ConnectionResolution_smod.F90:154-395`: callback
  detection, inout detection, characteristic-mismatch extension chains,
  materialization) operates on the already-matched `StateItemNode`
  (`dst_node`) and the found export (`export_node`/`export_node_id`),
  never on `conn` itself beyond the two accessor calls made once at
  the top of the enclosing procedure.
- `for_each_matching_import` (`:923-956`) is the shared destination-side
  filter: it walks `dst_graph`'s own import-port table and calls
  `dst_pt%v_pt%matches(...)` (`VirtualConnectionPt.F90:233-246`), which
  is already a regex match (`regcomp`/`regexec`) — an exact name is a
  valid degenerate regex, so this filter already works unchanged for a
  `SimpleConnection`'s single exact-name destination.
- `connection_resolves_import`'s `class default` branch
  (`:602-617`) exists to answer "has propagation's own mirroring step
  already had this import resolved by something other than
  `MatchConnection`" — it currently answers by string-matching the
  connection's declared destination name, not by checking the graph
  for an actual producer. (An earlier draft of this document proposed
  `DependencyNetwork%has_predecessors(node_id)` as the fix here;
  **that was wrong** — see D3 below for why, and for the corrected
  fix.)

## Goals / Non-Goals

**Goals:**
- `SimpleConnection` gets the same resolution outcome
  (dependency-edge-or-unresolved, exact-match-or-extension-chain,
  materialization-gated-on-graph-mode) that a `MatchConnection` whose
  pattern degenerates to the same single exact pair already gets.
- `MatchConnection`'s own resolution becomes, in implementation as well
  as in the spec text, "expand the pattern, then resolve each match the
  way a `SimpleConnection` would" — restoring legacy's own
  delegation shape instead of two independently-maintained resolution
  bodies that happen to currently look similar.
- `connection_resolves_import`'s propagation-suppression check becomes
  a real graph query (predecessor existence) instead of a
  connection-declaration name match, closing the false-negative
  described in `proposal.md`.
- Zero behavior change for any existing `MatchConnection`-only
  configuration, and zero behavior change for legacy
  (`StateRegistry`-based) resolution of either connection type — this
  change is additive to graph-native (`graph`/`both` mode) resolution
  only.

**Non-Goals:**
- `ReexportConnection` resolution. Stays `class default`/unresolved.
  Different shape entirely (export-to-export passthrough via
  `ExtensionFamily` reuse — `connect()` is already a no-op in legacy;
  `activate()` manipulates `ExtensionFamily`, not a dependency edge) —
  folding it in here would conflate two unrelated fixes.
- Wildcard (beyond `MatchConnection`'s own regex, e.g.
  `MAPL_STATEITEM_WILDCARD` item-type special-casing), callback, or
  inout resolution changes. Those already have their own delegation
  branches inside `resolve_one` (callback-interface check at
  `:188-211`, inout-borrower check at `:219-222`) and are unaffected —
  a `SimpleConnection`'s destination item is subject to the same
  branches, in the same order, as a `MatchConnection` match's
  destination item is today.
- Any change to the legacy `Connection`/`SimpleConnection`/
  `MatchConnection`/`ReexportConnection` types themselves
  (`connection/*.F90`) or to `StateRegistry`. This is purely a
  `GraphBuilder`-side widening of what gets resolved into graph
  structure; legacy's own imperative coupler is untouched.
- Cross-level (propagated/mirrored) `SimpleConnection` resolution
  beyond what import/export mirroring already provides. A
  `SimpleConnection` naming a mirrored import/export resolves exactly
  as one naming a directly-advertised item does (same guarantee the
  graph-builder spec already states for `MatchConnection`) — no new
  mirroring mechanism is introduced.

## Decisions

### D1 — Widen the resolution bodies' dummy-argument type; compute the per-match exact source name through one shared helper, mirroring legacy's own per-match SimpleConnection construction

**[CORRECTED during implementation.]** The first-drafted version of
this decision (dummy-argument widening with *no other change*) was
implemented, built, and run against the real scenario suite
(`ctest -L ESSENTIAL`), which surfaced a real correctness bug: 211 of
326 tests failed. Root cause, traced through one failure
(`scenarios/names_1/`, whose connection is `src_name: E_A1, dst_name:
I_B1` — different names on each side):

`resolve_one`'s export lookup
(`GraphBuilder_ConnectionResolution_smod.F90:269`) is
`find_export_node_id(this, src_pt%component_name,
dst_node%get_short_name(), ...)` — it looks up the source's export
using **the destination's own matched short name**, not the source's
own declared name (`check_one`'s `find_export_node(src_meta,
node%get_short_name())` at `:110` does the same substitution; so does
`resolve_inout_destination`'s identical `find_export_node_id(this,
src_pt%component_name, dst_node%get_short_name(), ...)` call at
`:1301`, which also has no current test fixture reaching it — no
`is_inout_borrower` scenario fixture exists at all today, so this
instance is latent, not yet observed as a real-fixture failure, but is
the identical bug class and is fixed alongside the others for
consistency. `resolve_callback_destination` does NOT have this bug,
on inspection: its own `dst_short_name` parameter is used only for the
destination-side import-node lookup; the actual source-side match goes
through `resolve_callback_import`'s own pattern-match against
`src_pt%v_pt%matches(...)`, which never substitutes the destination's
name — confirmed clean, left untouched). This is correct for every
real `MatchConnection` case only because legacy's own
`MatchConnection%activate()`/`connect()`
(`MatchConnection.F90:91-92,148-149`) construct each per-match
`SimpleConnection` with the source's effective name **forced equal
to** the matched destination's own name:

```fortran
src_pattern = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, &
     dst_pattern%get_esmf_name(), comp_name=dst_pattern%get_comp_name())
```

"Same short name on both sides" is `MatchConnection`'s own defining
wildcard semantic (a `connect_all`-style magic connect inherently
connects same-named items), not an implementation detail —
`dst_node%get_short_name()` was always a valid stand-in for the
source's name for every real `MatchConnection` call, which is why this
substitution was invisible before this change. A `SimpleConnection`,
by contrast, is defined to allow `src_name /= dst_name`
(`names_1`/`statistics_real`'s whole reason for existing as
`SimpleConnection` configurations rather than `all_unsatisfied`
wildcards) — so reusing `dst_node`'s own name for the source lookup is
simply wrong for it.

**The corrected fix is a more literal, more correct realization of
"`MatchConnection` should leverage `SimpleConnection`" than the
original D1/D2 draft, not a retreat from it.** Legacy's own
`MatchConnection%activate`/`connect` do not call one shared body that
happens to ignore which concrete type it was given — they construct,
per match, the EXACT `(src_name, dst_name)` pair a declared
`SimpleConnection` would have, then delegate to a real
`SimpleConnection`. The graph-native fix mirrors that exactly: a new,
small helper

```fortran
function effective_source_name(conn, dst_node) result(name)
   class(Connection), intent(in) :: conn
   class(StateItemNode), intent(in) :: dst_node
   character(:), allocatable :: name

   select type (conn)
   class is (SimpleConnection)
      ! Already exact - this IS the connection's own declared source
      ! name, independent of whatever the destination's own name is.
      name = conn%get_source()%get_esmf_name()
   class default
      ! MatchConnection (and any other pattern-matching subtype):
      ! legacy's own same-name wildcard rule - the effective source
      ! name for THIS match is the matched destination's own name,
      ! exactly as MatchConnection%activate()/connect() construct
      ! their own per-match SimpleConnection (MatchConnection.F90
      ! :91-92,148-149).
      name = dst_node%get_short_name()
   end select
end function effective_source_name
```

placed once in `GraphBuilder_ConnectionResolution_smod.F90`, and used
at every point `resolve_one`/`check_one` currently substitute
`dst_node%get_short_name()` for a *source*-side lookup. This includes
`resolve_inout_destination`, which turned out to have **two** instances
of the same substitution bug, not one: (a) its own
`find_export_node_id(this, src_pt%component_name,
dst_node%get_short_name(), owner_node_id, owner_node, _RC)` call (the
owner lookup — the instance originally identified), and (b), found
only while actually editing this subroutine's body, its own
chained-borrower scan, which checks each of the owner's own import
ports for `candidate_node%get_short_name() ==
dst_node%get_short_name()` to detect "is the value this destination
would borrow itself borrowed by its own owning component" — that
comparison is checking the OWNER's own import ports against the
DESTINATION's own name, which is wrong whenever `src_name /=
dst_name`; it must compare against `owner_node%get_short_name()` (the
owner's own, already-correctly-resolved name from fix (a)) instead.
Fix (a) by adding an `effective_src_name` parameter to this
subroutine's signature (the caller passes
`effective_source_name(conn, dst_node)`); fix (b) needs no additional
parameter, since `owner_node` is already in scope by the time the
chained-borrower scan runs. `resolve_callback_destination` needs no
change (confirmed clean above). Everywhere a call site passes a short
name on to a source-side lookup, it now uses the connection's own
correctly-resolved source name instead of `dst_node%get_short_name()`
directly; every other line of
`resolve_one`/`check_one`/`resolve_inout_destination` is unchanged —
the fix is confined to naming the source correctly. Since no current
fixture exercises an inout-borrower declaration at all, a new test is
added (Group 4) specifically to exercise `resolve_inout_destination`'s
own corrected path — this bug class would otherwise ship unverified by
anything.

This keeps D1's own original point intact: `resolve_match_connection`/
`check_match_connection_unsatisfied` still have one shared body, not
two parallel ones, their dummy argument is still widened to
`class(Connection)`, and the only place either connection subtype's
own dynamic type is inspected is this one small, explicit, legacy-
mirroring helper — not scattered through the ~240-line resolution
body.

**Alternative considered and rejected (the original draft):** dummy-
argument widening with no further change, reasoning that the body
"only touches `conn` via `get_source()`/`get_destination()`." This was
factually true but incomplete: the body also reads `dst_node`'s own
short name and silently assumes it equals the source's short name
too — a `MatchConnection`-only invariant the original draft missed by
not actually running the real scenario suite before finalizing the
decision. Recorded here rather than silently corrected, per this
project's own documented practice (`docs/graph/ARCHITECTURE.md` §3's
correction-note precedent) of keeping a wrong-then-fixed claim visible
rather than rewriting history.

**Alternative considered and rejected (second):** write a second,
parallel `resolve_simple_connection`/`check_simple_connection_unsatisfied`
pair that duplicates `resolve_one`'s ~240 lines, with the source-name
lookup hardcoded to `conn%get_source()%get_esmf_name()` throughout.
Rejected per the same reasoning as the original D1: this reintroduces
the inversion this change fixes — two independently-maintained bodies
that must be kept in sync by hand — and is strictly worse than the
corrected fix above, which needs only one new ~10-line helper function
to get the same correctness.

### D2 — Dispatch structure: `MatchConnection` and `SimpleConnection` share one resolution body; the helper above is the only type-aware step

The three `select type` sites change from:

```fortran
select type (c)
class is (MatchConnection)
   call resolve_match_connection(this, c, unresolved, unsupported, _RC)
class default
   ! deliberately skipped
end select
```

to:

```fortran
select type (c)
class is (MatchConnection)
   call resolve_match_connection(this, c, unresolved, unsupported, _RC)
class is (SimpleConnection)
   call resolve_match_connection(this, c, unresolved, unsupported, _RC)
class default
   ! ReexportConnection, wildcard/callback-only forms: still
   ! deliberately out of scope - see proposal.md
end select
```

exactly as originally drafted — this part of D2 was correct and is
unchanged. What D1's correction adds is that `resolve_match_connection`
itself (and `check_match_connection_unsatisfied`) now call
`effective_source_name(conn, dst_node)` once, at the one or two points
each needs a source-side short name, instead of reaching for
`dst_node%get_short_name()` directly. The procedure name stays
`resolve_match_connection` for the same reason the original D2 gave
(minimal diff; reviewer discretion to rename). The "alternative
considered and rejected" entries from the original D2 (making
`SimpleConnection extends(MatchConnection)`) stand unchanged — that
alternative is unaffected by D1's correction.

**Alternative considered and rejected:** make `SimpleConnection`
`extends(MatchConnection)` so `class is (MatchConnection)` already
covers it via Fortran's polymorphic dispatch. Rejected: `SimpleConnection`
is legacy, load-bearing production code
(`connection/SimpleConnection.F90`) outside this change's stated
impact (`proposal.md` "No change to ... the legacy `Connection`
hierarchy itself"); retyping its inheritance would risk legacy
`StateRegistry` behavior that has nothing to do with graph resolution,
for a cosmetic dispatch simplification.

### D3 — Give `SimpleConnection` the same `class is (MatchConnection)` branch's existing structural check, via one shared helper — not a graph-edge query

**[CORRECTED during implementation — the first-drafted version of this
decision was wrong; kept here, struck through in spirit, replaced
below, because the reasoning error is instructive: see the final
paragraph of this subsection.]**

`connection_resolves_import` is called only from
`graphbuilder_unresolved_import_node_ids`
(`GraphBuilder_ConnectionResolution_smod.F90:511,515`), itself called
only from `graphbuilder_run_propagate_hook`, which runs at
**advertise time** (`GraphBuilder.F90:509-514`'s own comment; see
`docs/graph/ARCHITECTURE.md` §2.4 for the full lifecycle ordering).
Real dependency-edge wiring (`graphbuilder_resolve_connections`, the
only procedure in this file that calls `add_dependency`) does not run
until the later, separate **ACCEPT_TRANSFER** phase. At the point
`connection_resolves_import` runs, **no dependency edge exists yet for
any connection of any type** — a `DependencyNetwork%has_predecessors`
check would always return `.false.` here, regardless of connection
type, making the originally-drafted "real graph query" fix in this
decision wrong: it would have caused every import resolved by a
`SimpleConnection` (or, for that matter, by a `MatchConnection`, had
the `class is (MatchConnection)` branch been changed to use it too) to
be reported unresolved and needlessly mirrored to the parent — a
regression, not a fix.

The existing `class is (MatchConnection)` branch (`:577-601`) already
answers "is this resolved" correctly at this advertise-time point,
and it does so without any reference to dependency-edge wiring: it
resolves the destination component, confirms the connection's own
declared pattern matches `node`'s short name, then (callback case
aside) calls `find_export_node(src_meta, node%get_short_name())` — a
*structural* existence check (does a same-named export exist),
exactly the same kind of check `resolve_match_connection`'s own
`resolve_one` performs before wiring, just without the wiring step
itself. Critically, like the other two call sites D1 identifies, this
branch's entire body touches `c` only through
`c%get_source()`/`c%get_destination()` (`Connection`-base) — nothing
in it is actually specific to `MatchConnection`'s own dynamic type.

The fix, consistent with D1/D2's own "one body, no duplicate logic"
pattern applied to this third call site: extract this branch's body
into a shared helper function, and call that same helper from a new
`class is (SimpleConnection)` arm placed alongside the existing
`class is (MatchConnection)` arm. `class default` (now covering only
`ReexportConnection` and any other genuinely out-of-scope subtype)
keeps `resolved = .false.` — correctly still mirrored, since nothing
in this change gives `ReexportConnection` any resolution path.

```fortran
select type (c)
class is (MatchConnection)
   resolved = connection_pt_pair_resolves(owner, target_meta, node, c, _RC)
class is (SimpleConnection)
   ! A SimpleConnection's own destination is an exact (non-regex)
   ! VirtualConnectionPt - the same structural "does this connection's
   ! own declared source/destination resolve this import" check a
   ! MatchConnection's own single match already gets, via the same
   ! shared helper - not a separately maintained check.
   resolved = connection_pt_pair_resolves(owner, target_meta, node, c, _RC)
class default
   ! ReexportConnection and any other out-of-scope Connection subtype:
   ! not resolved by this capability - correctly still mirrored.
end select
if (resolved) exit
```

where `connection_pt_pair_resolves(owner, target_meta, node, c, rc)`
is `connection_resolves_import`'s current `class is (MatchConnection)`
body, moved verbatim into its own function taking
`class(Connection), intent(in) :: c`, **with one additional fix
required by D1's own correction**: that body's own source-export
lookup (`has_export = associated(find_export_node(src_meta,
node%get_short_name()))`, originally `:597`) has the identical
same-name-substitution bug D1 found and fixed — it must become
`associated(find_export_node(src_meta, effective_source_name(c, node)))`,
using the same shared `effective_source_name` helper D1 introduces, so
a `SimpleConnection` with `src_name /= dst_name` is checked against
its own real declared source name here too, not against `node`'s own
destination-side name.

**Alternative considered and rejected (the original draft of this
decision):** a `DependencyNetwork%has_predecessors` query. Rejected
because it is factually wrong at this call site's point in the
lifecycle (no edges exist yet for anything, as shown above) — not
merely a worse design choice, an incorrect one. Recorded here, instead
of silently replaced, because the error is a useful caution: "the
graph has a direct way to ask this" does not imply "the direct way is
valid at every call site that asks a similar-sounding question" —
lifecycle timing has to be checked, not assumed, exactly as
`docs/graph/ARCHITECTURE.md` §2.4 already warns for this same hook.

**Alternative considered and rejected (second):** keep a
`SimpleConnection`-specific name-matching branch, separate from
`MatchConnection`'s own structural check, reasoning that the two
"happen to look similar." Rejected per explicit direction: this
reintroduces exactly the kind of parallel, independently-maintained
logic D1/D2 already reject for the other two call sites — the fix
must be one shared body `SimpleConnection` and `MatchConnection` both
call, not two versions that coincidentally agree today and can drift
apart tomorrow.

### D4 — `statistics_real` is the equivalence-harness fixture to add, not a new synthetic one

`statistics_real/history.yaml` declares 16 `SimpleConnection`s, all
sibling-to-sibling at `history`'s own level (`stat` → `collection_1`,
no cross-level propagation involved), and exercises both an exact
field match and real extension-chain cases (vertical `MIRROR` regrid,
vector-bundle characteristics) — broader coverage than the existing
synthetic `graphbuilder_equivalence` fixture (a single scalar field,
no mismatch). Using it directly in
`Test_GraphBuilderEquivalence.pf`, rather than writing a new synthetic
fixture, directly validates the motivating claim in `proposal.md`: that
a real, pre-existing, previously-invisible configuration now resolves
with identical dependency-edge pairings to legacy.

**[CORRECTED during implementation]** This decision originally said
"...resolves under `GRAPH_MODE_BOTH` with identical pairings to
legacy." Running the fixture under `GRAPH_MODE_BOTH` surfaced an
unrelated, pre-existing crash: `collection_1.yaml`'s own imports
(e.g. `TS_avg`) declare no `units:` at all — legal and
legacy-tolerated (the existing imperative coupler never requires
declared units up front) — but `GRAPH_MODE_BOTH`'s additional
materialization step (`try_materialize_field_extension`,
`GraphBuilder.F90:1230`) hard-`_ASSERT`s non-empty import units,
treating "no declared units" as an "internal inconsistency" it is not.
This is a materialization-layer gap, orthogonal to connection-type
dispatch — it would fire for *any* connection type (including a
pre-existing `MatchConnection`) wiring into a units-less import under
`GRAPH_MODE_BOTH`; it has simply never been exercised before because
no prior `GRAPH_MODE_BOTH` test used a fixture with units-less
imports. Filed as a separate, explicitly out-of-scope follow-up
(see proposal.md's own precedent for `ReexportConnection` scoping —
named, not silently absorbed), not fixed in this change.

The test therefore uses the default `GRAPH_MODE_REGISTRY` (no
`set_graph_mode` call at all), matching
`test_ordinary_resolution_matches_legacy_coupler`'s own existing
pattern: `add_dependency` — the actual claim this test needs to prove
— happens unconditionally in `resolve_one` regardless of graph mode;
only the materialization calls are gated behind `graph_mode() /=
GRAPH_MODE_REGISTRY`. Proving dependency-edge parity therefore needs
no mode change, and sidesteps the unrelated materialization gap
entirely.

`history_1` was considered and rejected for this role: on inspection,
`history_1/cap.yaml`'s own connection is `all_unsatisfied: true`
(`MatchConnection`), not a plain `SimpleConnection` — it does not
exercise this change's new coverage and stays out of scope.

## Risks / Trade-offs

- **[Risk]** Widening `resolve_match_connection`'s dummy-argument type
  to accept `SimpleConnection` could silently change behavior for some
  existing `MatchConnection`-based scenario if any code path implicitly
  relied on `conn`'s dynamic type being exactly `MatchConnection`, or
  if the new `effective_source_name` helper's own `class default`
  branch (D1's correction) produced a different name for
  `MatchConnection` than the pre-change code did.
  → **Mitigation:** `effective_source_name`'s `class default` branch
  returns `dst_node%get_short_name()` — byte-for-byte the same
  expression every pre-change call site already used directly, so
  `MatchConnection`'s own resolution outcome is provably unchanged
  (this is exactly why the bug was invisible until a `SimpleConnection`
  with `src_name /= dst_name` exercised the `class is (SimpleConnection)`
  branch for the first time, in real-fixture testing, not in
  design review). Full regression (`ctest -L ESSENTIAL` plus full
  suite, both NAG and gfortran, run against real scenario fixtures —
  not just synthetic unit tests — before landing, per
  `docs/graph/ARCHITECTURE.md` §4) confirms no other call path depends
  on the narrower type or the old name-substitution behavior.
- **[Risk]** D3's shared helper (`connection_pt_pair_resolves`) is
  called from both the `class is (MatchConnection)` and
  `class is (SimpleConnection)` arms at an advertise-time call site
  that must never create graph structure (per the "Unsatisfied-import
  determination happens without graph mutation" requirement) — if a
  future edit to this helper added any mutating call (e.g.
  `add_dependency`), it would silently violate that requirement for
  both connection types at once, not just one. → **Mitigation:** the
  helper's body (moved verbatim from the existing `class is
  (MatchConnection)` branch) already contains no mutating call today;
  the existing `test_check_unsatisfied_imports_reports_without_mutating_graph`-style
  test pattern (`Test_GraphBuilder.pf`) should be extended to cover a
  `SimpleConnection` case, making a future regression here visible via
  the same no-mutation assertion that already protects
  `MatchConnection`.
- **[Trade-off]** `ReexportConnection` staying unresolved means a
  config mixing `SimpleConnection` and `ReexportConnection` imports at
  the same level still has a partially-graph-visible, partially-legacy
  wiring picture. This is an existing condition (true today for
  `MatchConnection`+`ReexportConnection` mixes too), not introduced by
  this change, and is explicitly named rather than silently left
  unaddressed (`proposal.md` "What Changes").
- **[Trade-off]** Not renaming `resolve_match_connection` (D2) leaves a
  procedure name that, post-change, is slightly misleading (it now
  resolves `SimpleConnection` too). Accepted in favor of a smaller
  diff; flagged for reviewer judgment per D2's own note.

## Migration Plan

No migration — this is a pure additive widening of
`GraphBuilder`'s own graph-native resolution coverage, gated the same
way all `graph`/`both`-mode behavior already is (`graph_mode()`
checks already present at every materialization call site are
untouched). No schema, config, or YAML change; no change to legacy
(`registry` mode) behavior at all. Rollback is a plain revert — no
data or state to migrate back.
