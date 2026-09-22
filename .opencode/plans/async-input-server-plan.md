## Async Input Server Plan

### Step 0
- Goal: lock the route without changing behavior.
- Change:
  - Add optional `input_server_name` support for ExtData reads.
  - Default remains `MAPL_DEFAULT_INPUT_SERVER`.
- Files:
  - `gridcomps/extdata/ExtDataFileReader.F90`
  - `infrastructure/geom_io/GeomPFIO.F90`
- Verify:
  - Existing behavior unchanged.
  - Run one ExtData case with `input_server_name: mapl_default_input_server` explicitly set and confirm identical results.

### Step 1
- Goal: prove ExtData can use a named custom input server.
- Change:
  - Add a new YAML `servers:` entry, for example `async_input_server`.
  - Point it at a new server subclass, but keep implementation synchronous and behavior identical to current input server.
- Files:
  - `mapl/PfioServerGridComp.F90`
  - new `pfio/AsyncInputServer.F90`
  - one test `cap*.yaml`
- Verify:
  - Run one 2-step ExtData case through `async_input_server`.
  - Output fields and `extdata_files_read.yaml` match the default server case.

### Step 2
- Goal: define the communicator contract correctly before moving any data path.
- Change:
  - Keep the model-facing communicator equal to `model_comm`.
  - Do not inspect `MPI_COMM_WORLD` inside `pfio`.
  - Require the caller/program to supply the communicator split inputs.
  - Extend `AsyncInputServer` so it can accept `comm` and `model_comm` explicitly.
  - Derive `node_comm` internally from `comm` using `MPI_Comm_split_type`.
  - Derive the model-side node communicator from `model_comm` using `MPI_Comm_split_type`.
  - Compute `reader_capacity_on_node = node_size_on_node - model_size_on_node`.
  - If `reader_capacity_on_node == 0`, fall back automatically to the synchronous input-server behavior.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - Build and run unchanged when `comm == model_comm` and there are no extra node-local PEs.
  - Startup logs show `model_size_on_node`, `node_size_on_node`, `reader_capacity_on_node`, and whether synchronous fallback is active.
  - Existing science result remains identical.

### Step 3
- Goal: attach the extra node-local reader pool to `AsyncInputServer` while preserving synchronous behavior.
- Change:
  - The caller builds the mixed per-node communicator: model PEs on a node plus extra reader PEs on that node.
  - The caller also builds the reader-only per-node communicator if needed.
  - Pass those communicators into `AsyncInputServer`.
  - Keep front ranks client-facing.
  - Reader ranks exist and are addressable, but reads are still synchronous.
- Files:
  - `pfio/AsyncInputServer.F90`
  - communicator plumbing near server creation if needed
- Verify:
  - Add temporary logging of per-node reader pool size and membership.
  - Confirm the configured reader capacity matches `node_size - model_size_on_node`.
  - Science result remains identical.

### Step 4
- Goal: make the reader pool perform synchronous reads for its node.
- Change:
  - Front ranks gather the node's read requests.
  - Reader ranks on that node perform the read work.
  - Results are shared back to front ranks on the same node.
- Scope limit:
  - No prefetch yet.
  - Keep the existing current-timestep behavior.
- Files:
  - `pfio/AsyncInputServer.F90`
  - possibly one small helper for request aggregation/sharing
- Verify:
  - Logs show reader ranks, not front ranks, doing the actual file read.
  - Field values remain identical.

### Step 5
- Goal: add shared-memory cache for one dataset at a time.
- Change:
  - After a read rank reads a dataset, it stores it in node-local shared memory.
  - Front ranks serve client requests from that shared memory instead of rereading.
- Scope limit:
  - Cache only one resolved dataset key at a time.
  - No current/next yet.
- Files:
  - `pfio/AsyncInputServer.F90`
  - maybe one new small cache helper file if needed
- Verify:
  - Two identical requests for the same resolved dataset cause only one actual file read.
  - Add log counters for cache hits and misses.
  - Field values remain identical.

### Step 6
- Goal: support current + next requests on the server, but client still consumes only current.
- Change:
  - Extend the server-side request format or add a second request path so the client can submit current and next datasets.
  - Server returns current normally.
  - Server starts reading next into cache immediately after.
- Scope limit:
  - Only one-step lookahead.
  - Only one variable/dataset family first.
- Files:
  - `pfio/ClientThread.F90` or a new specialized input client helper
  - `pfio/ServerThread.F90` or `pfio/AsyncInputServer.F90`
  - `gridcomps/extdata/ExtDataFileReader.F90`
- Verify:
  - Logs show current served and next launched before next timestep consumes it.
  - No change in model answers.

### Step 7
- Goal: make ExtData use the rolling two-slot pattern.
- Change:
  - At timestep `N`, ExtData asks for `N` and `N+1`.
  - It waits only for `N`.
  - At timestep `N+1`, it uses cached `N+1` if ready, then requests `N+2`.
- Scope limit:
  - Implement only for the collective prefetch path.
- Files:
  - `gridcomps/extdata/ExtDataFileReader.F90`
  - possibly `gridcomps/extdata/ExtDataGridComp.F90`
- Verify:
  - Logs show first timestep is cold and later timesteps hit prefetched data.
  - Same output fields as baseline.

### Step 8
- Goal: harden and generalize.
- Change:
  - Handle cache invalidation.
  - Handle multiple active fields.
  - Handle end-of-segment cleanup.
- Verify:
  - Run a longer ExtData case.
  - Confirm no stale data, no leaks, and no duplicate reads beyond expected misses.

### Step 9
- Goal: utilize all reader workers, not just reader-comm rank 1.
- Problem (2026-09-11 code inspection):
  - `forward_request_to_reader` always targets `this%reader_ranks_on_node(1)`.
  - The captain's dispatch/poll helpers (`dispatch_next_request`,
    `send_reader_request`, `poll_reader_completion`) hardcode `reader_comm`
    destination rank `1`.
  - Any worker at `reader_comm` local rank `2` or higher enters the passive
    worker loop but is never dispatched to.
  - This means `reader_capacity_on_node > 2` currently wastes PEs.
- Change:
  - Add per-worker busy-state tracking (array indexed by reader-comm local
    rank `1..reader_capacity_on_node-1`).
  - Replace the hardcoded rank-`1` destination with worker selection that
    routes deterministically by file identity (e.g. hash of `file_name`
    modulo the number of workers), falling back to first-idle only when a
    file has no assigned owner yet. Deterministic-by-file routing is a
    prerequisite for Step 10, which gives each worker exclusive ownership of
    the files it reads.
  - Extend `poll_reader_completion` to probe all busy workers, not just one.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - New regression case with `reader_capacity_on_node=3` (one captain + two
    workers).
  - Add temporary logging identifying which worker rank serviced each
    request; confirm both non-captain workers appear, and that a given file
    always routes to the same worker.
  - `case01`-`case05` (and `case45`-`case49` if restored) still pass unchanged
    with `reader_capacity_on_node<=1`.

### Step 10
- Goal: give each worker exclusive ownership of the files it reads (so no
  two workers ever read the same file), and deliver read results directly
  through shared memory between the model rank and its owning worker,
  instead of relaying the data through the captain.
- Problem:
  - `AsyncInputCacheSlot` storage is private to whichever worker rank holds
    it (`LocalMemReference` in reader-local memory). If two workers ever
    cache-miss on the same key, the file gets read twice.
  - Even on a cache hit, today's data path is: worker -> `MPI_Send` to
    captain's model-facing loop (`send_reader_request`/
    `poll_reader_completion`) -> captain `MPI_Send` to the model rank
    (`this%comm`). That is an extra hop and an extra blocking wait beyond
    what shared physical memory on the same node requires.
- Change:
  - File ownership, not a shared cache: rely on Step 9's deterministic
    file-to-worker routing so each unique `file_name` is always read by the
    same single worker. Because ownership is exclusive per file, there is no
    need for a cross-worker shared cache or directory — a worker never needs
    to see another worker's cached data.
  - Model<->worker shared memory instead of captain relay: allocate a
    shared-memory buffer scoped to just the requesting model rank and its
    file's owning worker (both are on the same node by construction of
    `node_comm`), e.g. one `MPI_Win_allocate_shared` segment per
    (model-rank, worker) pair, or a small fixed pool of per-worker segments
    reused across requests.
  - The worker reads the file, writes the resulting local slice into the
    shared buffer, and sets a completion flag for that buffer. It does not
    wait for the model rank to consume the data — it returns to idle
    immediately and is eligible for its next request.
  - The requesting model rank waits on the completion flag (not on an
    `MPI_Recv` of the payload) and then reads the slice directly out of
    shared memory. Once it has read the data, both sides have already moved
    on: the worker moved on when it set the flag, and the model rank moves
    on as soon as it has copied/used the data.
- Scope limit:
  - Only applies when the model rank and its assigned worker share a node
    (already guaranteed by how `node_comm`/`reader_comm` are constructed).
  - Keep the existing per-slot key (`file_name`, `var_name`, `type_kind`,
    `global_start`, `global_count`) for the worker's own single-owner cache;
    just drop the cross-worker sharing requirement since ownership is
    exclusive.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - New case with `reader_capacity_on_node=3` (two workers) and at least two
    distinct source files: confirm each file is always serviced by the same
    worker (log the owning worker per file) and that no file is ever read by
    more than one worker.
  - Add temporary logging/timestamps showing the worker sets its completion
    flag and returns to idle before the model rank has necessarily consumed
    the shared buffer, demonstrating the two sides are decoupled.
  - Confirm no data corruption from buffer reuse: a shared segment must not
    be overwritten by a new read until the previous consumer has read it (or
    use double-buffering / one segment per outstanding request).
  - `case01`-`case05` still pass; extend `case46`'s duplicate-request pattern
    across two workers to confirm single-read-per-file behavior.

### Step 11
- Goal: let an already-warm "current" read skip the blocking worker round
  trip.
- Problem:
  - Every "current" read blocks the model rank on a live worker round trip,
    even if the requested global slab was already cached by an earlier
    prefetch.
  - A previous attempt at a synchronous `PROBE_PREPARED` RPC deadlocked
    PFIO case02 and was reverted (see status log, 2026-09-04).
- Change:
  - Track cache-key warmth captain-side only, updated when a worker reports
    completion (already flowing through `poll_reader_completion`).
  - Do not add any new synchronous query to a worker. If the captain's own
    warm-key table has the answer, serve immediately from local knowledge;
    otherwise fall back to the existing blocking dispatch path unchanged.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - `case47`-style rolling case shows reduced captain-side wait time on
    steady-state timesteps.
  - No new deadlocks; full `case01`-`case05` regression still passes.

### Step 12
- Goal: make the cache slot count configurable instead of a hardcoded
  compile-time constant.
- Problem:
  - `ASYNC_INPUT_NUM_CACHE_SLOTS = 2` is a `parameter` in
    `pfio/AsyncInputServer.F90`. The status log already flags that 2 slots
    may evict too aggressively for the 4-unique-slab interpolation pattern.
- Change:
  - Read an optional `MAPL_ASYNC_INPUT_CACHE_SLOTS` environment variable at
    server construction (same pattern already used for
    `MAPL_PERF_READER_SLEEP_SEC`), defaulting to the current value of `2`.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - Rerun the `case48`/`case49` interpolation cases with the slot count set
    to `4` and confirm improved hit rate versus the `2`-slot baseline.
  - Confirm default behavior (unset variable) is unchanged for
    `case01`-`case05`.

### Step 13
- Goal: replace the bracket-coincidence lookahead with an explicit
  selector-aware `current_time + dt` preview.
- Problem:
  - The current "next" prefetch correctness for the no-interpolation path
    relies on the coincidence that the bracket's current right node is the
    dataset consumed at the next timestep (status log, Step 7 finding). A
    naive fresh `current_time + dt` selector preview was tried and shown to
    be wrong for `case46` because it ignored carried-forward bracket state.
- Change:
  - Add a non-mutating "what dataset would be selected at time T" query on
    the existing selector, keeping current bracket/state mutation untouched.
  - Wire ExtData's cache-only next-prefetch submission to use this query
    instead of relying on the right-node coincidence, still scoped to
    `async_input_server` only.
- Files:
  - `gridcomps/extdata/ExtDataFileReader.F90`
  - `gridcomps/extdata/PrimaryExport.F90`
  - possibly the selector module used for bracket resolution
- Verify:
  - Add a case with an irregular time step (e.g. a `dt` that does not evenly
    divide the source cadence) where the old right-node-coincidence
    heuristic would prefetch the wrong dataset; confirm the new query
    prefetches correctly.
  - `case45`-`case49` still pass unchanged.

### Step 14
- Goal: harden multi-worker shutdown once Step 9 lands.
- Change:
  - Extend `ASYNC_INPUT_CMD_TERMINATE` broadcast to every worker rank in
    `reader_comm` (currently only reader-comm rank `1` receives it in
    `start`).
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - Multi-worker regression case (from Step 9) shuts down cleanly with no
    hung ranks and no `MPI_Abort` on finalize.

### Step 15
- Goal: cluster verification.
- Change: none (verification only).
- Verify:
  - Run `case01`-`case05` and the benchmark scripts in
    `tests/MAPL3G_Component_Testing_Framework/benchmark/` on the actual
    cluster where `reader_capacity_on_node > 0` spans multiple nodes.
  - Confirm the Step 9 multi-worker log lines appear and multi-worker
    dispatch actually improves wall time versus the single-worker baseline.

## Active Redesign: MultiGroup-Style Input Service

### Objective

Restructure `AsyncInputServer` around the useful topology and scheduling
patterns in `MultiGroupServer`:

```text
model/client ranks
        |
        | request metadata
        v
node-local reader captain
        |
        | worker assignment
        v
node-local reader worker(s)
        |
        | worker-owned shared-memory result
        v
model/client rank
```

Each shared-memory node that contains model ranks must contain one reader
captain and at least one reader worker. Models submit requests only to the
captain. The captain schedules the request and tells the model which worker
owns or will produce the data. The model then copies the result directly from
worker-owned shared memory. The captain handles control metadata only and does
not copy payload data.

`model_comm` is a caller-owned, initialization-only input. It is not a
component of `AsyncInputServer`, is not duplicated or freed by the server, and
must not be accessed after topology initialization.

Until a non-shared-memory transport is implemented, this design supports only
`local: true` because each model, captain, and assigned worker must belong to
the same MPI shared-memory domain.

### Step 16: Initialization Contract and Roles

- Remove the persistent `model_comm` component from `AsyncInputServer`.
- Continue accepting `model_comm` as a constructor argument initially to
  preserve the public call shape.
- During construction, convert model membership into immutable runtime state:
  `is_model_rank`, `is_reader_rank`, `is_captain`, `is_worker`, and
  `is_model_node_root`.
- Store the node-local captain's service-communicator rank and explicit maps
  among service, node, and reader rank spaces.
- Rename ambiguous variables such as `model_rank` when they actually contain
  a service-communicator rank.
- Replace all runtime tests of `model_comm == MPI_COMM_NULL` with initialized
  role fields.
- Do not assume equal numerical ranks in `model_comm` and the service
  communicator.
- Validate one captain and at least one worker on every model-containing node.
- Reject non-local `AsyncInputServer` configurations early with a clear error.

Files:

- `pfio/AsyncInputServer.F90`
- `mapl/MaplFramework.F90`
- `mapl/PfioServerGridComp.F90`
- focused PFIO MPI topology tests

Verify:

- Construct using a temporary caller-owned `model_comm`, complete
  initialization, and prove runtime role queries no longer use that handle.
- Test a non-prefix or reordered model/service rank mapping.
- Verify one model, one captain, and two workers resolve the expected rank
  spaces and shut down cleanly.
- Run existing PFIO component cases 01-05.

### Step 17: Explicit Control Protocol

- Give every request a unique request ID.
- Define explicit metadata for model-to-captain requests, captain-to-worker
  commands, captain-to-model assignments, and worker-to-captain completions.
- Include source service rank, source node/model index, file and variable,
  type, global and local bounds, and `cache_only` in request metadata.
- Keep MPI tags separate for request, assignment, worker command, completion,
  termination, and termination acknowledgment.
- Document the rank space of every transmitted rank.

Files:

- `pfio/AsyncInputServer.F90`
- focused protocol tests under `pfio/tests/`

Verify:

- A model sends request metadata only to its captain.
- The captain returns a valid node-local worker and the same request ID.
- Worker completion is associated with the correct request and cache key.
- Multiple outstanding requests cannot consume another request's response.

### Step 18: Worker-Owned Shared-Memory Results

- Change shared-memory ownership so each worker allocates:
  - its private cache-slot payloads;
  - one result mailbox for each node-local model rank.
- Put state, request ID/generation, payload size, status, and payload in each
  mailbox.
- Use explicit mailbox states such as `EMPTY`, `FILLING`, `READY`, `OVERFLOW`,
  and `ERROR`.
- Have the worker extract the model's local slice and publish it into its own
  shared segment.
- Have the model query the selected worker's shared segment, wait for the
  matching request ID, copy the data, and release the mailbox.
- Preserve `MAPL_ASYNC_INPUT_SHMEM_WORDS` for the first implementation; dynamic
  payload allocation is a later enhancement.

Files:

- `pfio/AsyncInputServer.F90`
- shared-memory MPI tests under `pfio/tests/`

Verify:

- The payload path is worker to model through shared memory; no payload MPI
  message passes through the captain.
- Exercise `EMPTY -> FILLING -> READY -> EMPTY` transitions.
- Exercise two workers and at least two model ranks without mailbox overlap.
- Verify INT32, INT64, REAL32, and REAL64 payloads where supported.
- Verify controlled overflow and size-mismatch failures.

### Step 19: Control-Only Captain and Warm Hits

- Remove captain-side payload extraction and publication from the warm-cache
  path.
- Keep cache-directory metadata at the captain, including worker, slot, key,
  and generation.
- On a warm demand request, instruct the owning worker to serve the cached
  value and return that worker identity to the model.
- On cache-slot replacement, invalidate stale captain directory entries.
- Preserve cache-only lookahead semantics: lookahead returns after acceptance,
  while the worker later reports cache population to the captain.

Files:

- `pfio/AsyncInputServer.F90`
- warm-cache integration tests

Verify:

- A prefetched key is served without another file read.
- The worker, not the captain, publishes the warm payload.
- Reused cache slots cannot satisfy requests with stale data.
- Existing demand and cache-only message behavior remains unchanged.

### Step 20: MultiGroup-Style Scheduling

- Replace hash-only dispatch with explicit per-worker idle/busy and load state,
  following the backend scheduler pattern in `MultiGroupServer`.
- Select workers in this order:
  1. idle worker already owning the exact cache key;
  2. idle worker with useful file affinity;
  3. least-loaded idle worker;
  4. queue the request when no worker is available.
- Track identical in-flight keys so concurrent requests share one physical
  read and receive separate local slices afterward.
- Keep deterministic filename hashing only as a tie-breaker if useful.

Files:

- `pfio/AsyncInputServer.F90`
- scheduler unit or focused MPI tests

Verify:

- Independent files run concurrently on two workers.
- Repeated keys use the owning worker and avoid duplicate reads.
- A busy affinity worker does not block unrelated work when another worker is
  idle.
- Queue draining is fair and does not starve a worker or request.

### Step 21: Hierarchical Shutdown and Framework Integration

- Use stored roles rather than `model_comm` during shutdown.
- Have one model-node root terminate its captain.
- Have the captain stop accepting work, drain pending and active requests,
  terminate every worker, collect acknowledgments, and acknowledge the model.
- Release the shared window and all server-created communicators through one
  idempotent cleanup path.
- Ensure local async readers still run when unrelated remote server GridComps
  are configured; the current `run_servers` early branch must not suppress
  them.
- Keep communicator ownership changes narrowly scoped to `AsyncInputServer`;
  do not change all `AbstractServer` ownership semantics in this work.

Files:

- `pfio/AsyncInputServer.F90`
- `mapl/MaplFramework.F90`
- lifecycle tests

Verify:

- Initiate shutdown with queued and active work and confirm that it drains.
- Confirm every worker acknowledges termination.
- Run a mixed local async-input plus remote output-server configuration.
- Run under a CTest timeout so protocol deadlocks fail deterministically.

### Step 22: Regression Coverage and Documentation

- Add focused collective-prefetch and next-prefetch serialization tests.
- Add a two-model, one-captain, two-worker integration case.
- Enhance PFIO case05 with machine-checkable worker ownership or counters.
- Add a prefetch-then-demand warm-hit case.
- Add a negative test for remote `AsyncInputServer` configuration.
- Document topology, minimum process count, local-only restriction,
  environment variables, and communicator ownership in `pfio/pfio.md`.

Verify:

- Build all tests and run focused async-input tests first.
- Run PFIO component cases 01-05.
- Run the full `ESSENTIAL` test label before considering the redesign complete.

## Build and Status Discipline

- After each numbered step, update
  `.opencode/plans/async-input-server-status.md` with:
  - completion date and state;
  - files changed;
  - design decisions or deviations;
  - exact build and test commands;
  - pass/fail counts and relevant log paths;
  - remaining risks and the next step.
- Load `nag-stack` in the same shell invocation as every configure, build, or
  test command because module state does not persist between tool calls.
- Use `build/` as the NAG build directory. Do not configure that directory
  with another compiler.
- Preserve complete logs in `build/` with `tee`.
- Standard commands are:

```bash
module load nag-stack && cmake -B build -DCMAKE_BUILD_TYPE=Debug 2>&1 | tee build/cmake-config.log
module load nag-stack && cmake --build build -j 8 2>&1 | tee build/build.log
module load nag-stack && cmake --build build -j 8 --target build-tests 2>&1 | tee build/build-tests.log
module load nag-stack && ctest --test-dir build --output-on-failure 2>&1 | tee build/ctest.log
```

- During implementation, use focused test selections and append their output
  to step-specific files in `build/`. Run the full `ESSENTIAL` selection at
  the end:

```bash
module load nag-stack && ctest --test-dir build -L ESSENTIAL --output-on-failure 2>&1 | tee build/ctest-essential.log
```
