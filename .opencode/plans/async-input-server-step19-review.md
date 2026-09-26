## Step 19 Code Review

Date: 2026-09-23

Scope:

- Uncommitted Step 19 changes in `pfio/AsyncInputServer.F90`.
- Step 19 status update in
  `.opencode/plans/async-input-server-status.md`.
- Requirements in Step 19 of
  `.opencode/plans/async-input-server-plan.md`.

### Findings

#### High: Cache-only requests can be discarded using stale directory metadata

`serve_warm_requests` removes a cache-only request when its key matches a
captain directory entry (`pfio/AsyncInputServer.F90:917-930`). A worker can,
however, invalidate and begin replacing that slot before the captain receives
the replacement completion (`pfio/AsyncInputServer.F90:1441-1446`). The
captain updates its directory only after processing the worker completion
(`pfio/AsyncInputServer.F90:852-869`).

During this interval, a cache-only request for the evicted key is removed
without reaching the worker and therefore does not repopulate the cache.
Demand requests are protected by worker-side generation and key validation,
but discarded cache-only requests receive no worker-side validation.

This violates Step 19's requirement to invalidate stale captain directory
entries when a slot is replaced. It can turn an accepted lookahead request
into a later demand miss. The previous check that deferred warm handling while
the owning worker was busy would have prevented this particular race.

#### Medium: The shared window still reserves the removed cache-mirror region

`worker_segment_words` still reserves
`num_cache_slots * shared_mailbox_words` ahead of the model mailboxes
(`pfio/AsyncInputServer.F90:1342-1343`), and `mailbox_offset` skips that same
region (`pfio/AsyncInputServer.F90:1350-1351`).

After removing `publish_shared_cache_slot`, this region is unused. Worker cache
payloads remain separately allocated in private `LocalMemReference` objects.
With the default configuration, the unused region is approximately 32 MiB per
worker and scales with the worker count and configured slot count.

This also conflicts with the Step 19 status statement that the shared window
now contains only the necessary worker-owned mailbox storage.

#### Medium: Warm ownership is not taken from the captain directory

`AsyncInputWarmRecord` stores the owning worker, but `serve_warm_requests`
copies only the slot and generation into the request
(`pfio/AsyncInputServer.F90:923-925`). Assignment and dispatch independently
recompute the worker from the filename hash
(`pfio/AsyncInputServer.F90:512-514` and
`pfio/AsyncInputServer.F90:1000-1004`).

This works only while every request uses the same fixed filename hash. It does
not implement the Step 19 contract to instruct and return the directory's
owning worker. It will become incorrect when Step 20 replaces hash-only
scheduling: a warm hint could be sent to another worker, fail validation, and
cause an unnecessary duplicate read.

The selected worker should be carried explicitly from the warm directory into
assignment and dispatch.

#### Medium: Required Step 19 regression coverage is missing

`pfio/tests/Test_AsyncInputServer.pf:10-133` tests topology, protocol request
IDs, mailbox layout, and lifecycle, but it does not exercise Step 19 warm-cache
behavior.

Missing automated cases include:

- prefetch followed by demand, proving no second physical read;
- worker-side warm publication rather than captain publication;
- slot replacement followed by a request for the evicted key;
- cache-only submission while a matching directory entry is being replaced;
- two-worker warm ownership and assignment.

The passing component tests do not specifically exercise the slot-replacement
race. Step 19 explicitly calls for warm-cache integration tests.

### Additional Note

`captain_warm_hits` is incremented before the worker validates the supplied
generation (`pfio/AsyncInputServer.F90:923-926`). A stale hint that falls back
to a cache miss is therefore still reported as a captain warm hit. The current
counter cannot be used as proof that a warm request avoided a physical read.

### Recommendation

Do not commit Step 19 until the stale cache-only race is fixed. The unused
shared-window cache region and explicit directory-owner routing should also be
corrected as part of Step 19 rather than deferred to Step 20. Add focused
integration coverage for replacement invalidation and worker-served warm
demands before considering the step complete.

### Assessment of This Review (2026-09-23)

Verdict: the review is fair, accurate, and worth acting on. Every finding was
re-checked directly against the current file and confirmed, not just
plausible-sounding.

- Finding 1 (High, stale cache-only discard): Confirmed. `serve_warm_requests`
  retires a cache-only request purely from directory metadata
  (`AsyncInputServer.F90:917-930`) with no worker contact, while the directory
  is refreshed only on the next completion (`update_warm_record`, called at
  line 868 inside `poll_reader_completions`). A worker bumps `generation` and
  invalidates its slot before that completion is polled
  (`read_global_slab_into_slot`, lines 1444/1446). A cache-only request for
  the evicted key arriving in that window is silently dropped instead of
  repopulating the cache, which can convert a planned prefetch into a later
  blocking demand miss. Correctly rated High: this contradicts the plan's own
  invariant that "reused cache slots cannot satisfy requests with stale
  data."
- Finding 2 (Medium, wasted shared-window region): Confirmed.
  `worker_segment_words` (lines 1342-1343) and `mailbox_offset` (lines
  1350-1351) still reserve/skip `num_cache_slots * shared_mailbox_words`
  even though `publish_shared_cache_slot` was deleted and cache payloads now
  live only in private `LocalMemReference` objects. This also means my own
  status-log claim ("the shared window now only ever holds worker-private
  cache slots and worker-owned model mailboxes") was itself imprecise/wrong.
- Finding 3 (Medium, hint doesn't carry worker identity): Confirmed.
  `serve_warm_requests` copies only `slot_index`/`generation`
  (lines 923-925), never `warm_records(warm_index)%worker_rank`. Assignment
  (line 512-513) and dispatch (line 1000-1001) both independently re-derive
  the worker via `select_file_worker` filename hashing. Works today only
  because routing is 100% deterministic-by-filename; becomes a real bug once
  Step 20 introduces load-based scheduling. This is also a literal gap versus
  the plan text ("return that worker identity to the model").
- Finding 4 (Medium, missing tests): Confirmed. `Test_AsyncInputServer.pf`
  only covers topology/roles/IDs/lifecycle; no cache-warmth or
  replacement-race coverage exists.
- Additional note (`captain_warm_hits` counted before worker validation):
  Confirmed and useful — the counter currently measures "hint attempted," not
  "hint honored," which makes prior benchmark log readings for warm-hit rate
  optimistic.
- One nuance: the review's claim that the old busy-worker check "would have
  prevented this particular race" is true but only for the narrow
  worker-busy sub-case, not the general staleness window; still not wrong,
  just slightly generous to the prior code.

Net conclusion: Step 19 should not have been marked complete. At minimum
Finding 1 is a real correctness regression against the plan's stated
invariant, and Finding 3 is a real fragility that will break in Step 20.
Accept this review's recommendation as-is.

### Plan for Next Session

1. Fix Finding 1: `serve_warm_requests` must not retire a cache-only request
   purely from directory metadata. Either (a) still dispatch cache-only warm
   hits to the worker with the hint attached (worker re-validates and no-ops
   quickly if already warm, matches informs completion either way), or
   (b) keep the immediate-retire fast path but only when the directory entry
   itself carries proof of freshness that can't go stale within the same
   captain-tick (e.g., require the owning worker to be idle before trusting a
   cache-only directory hit, restoring something like the old busy-worker
   guard). Prefer (a) for simplicity and symmetry with demand hints.
2. Fix Finding 3: carry `warm_records(warm_index)%worker_rank` explicitly
   into the pending request's assignment/dispatch instead of re-deriving via
   `select_file_worker`. Add an assertion that the re-derived hash-based
   worker equals the directory's worker while hash-only scheduling is still
   in effect, to catch drift early.
3. Fix Finding 2: remove the now-dead `num_cache_slots * shared_mailbox_words`
   reservation from `worker_segment_words`/`mailbox_offset` (or repurpose it
   if a future step needs shared cache mirroring again).
4. Add the missing focused tests called out in Finding 4, at minimum:
   prefetch-then-demand single-read proof, worker-side (not captain-side)
   warm publication, and a slot-replacement-then-request-for-evicted-key
   case.
5. Re-run the full Step 19 verification matrix (build-tests, MAPL.pfio.tests,
   pfio_case01-05) after the fixes, then update the Step 19 status section
   with corrected findings before marking it complete again.
6. Do not commit until 1-4 are done per the review's recommendation.
