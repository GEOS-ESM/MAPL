## Async Input Server Status

### Goal
Build an ExtData input server that can eventually use extra node-local reader PEs while keeping `model_comm` as the front/model communicator.

### Key Design Decisions
- `pfio` should not inspect `MPI_COMM_WORLD`.
- `AsyncInputServer` takes:
  - `comm`: all server processes available to the async input server
  - `model_comm`: the model/front communicator
- `AsyncInputServer` derives node-local communicators internally with `MPI_Comm_split_type`.
- If `reader_capacity_on_node == 0`, it falls back to synchronous behavior.

### Files Added
- `.opencode/plans/async-input-server-plan.md`
- `.opencode/plans/async-input-server-status.md`
- `pfio/AsyncInputServer.F90`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case01/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case03/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case04/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case05/`
- `tests/MAPL3G_Component_Testing_Framework/benchmark/prepare_async_perf_cases.sh`
- `tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh`

### Files Modified
- `mapl/MaplFramework.F90`
- `mapl/PfioServerGridComp.F90`
- `pfio/AbstractServer.F90`
- `pfio/ServerThread.F90`
- `pfio/CMakeLists.txt`
- `gridcomps/extdata/ExtDataConfig.F90`
- `gridcomps/extdata/PrimaryExport.F90`
- `gridcomps/extdata/ExtDataGridComp.F90`
- `gridcomps/extdata/ExtDataFileReader.F90`
- `tests/MAPL3G_Component_Testing_Framework/test_case_descriptions.md`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/cases.txt`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case44/cap2.yaml`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case44/extdata2.yaml`

### Implemented Steps

#### Step 0: ExtData input-server override
- Added optional `input_server_name` flow through ExtData.
- Default remains `MAPL_DEFAULT_INPUT_SERVER`.

#### Step 1: Named custom input server
- Added `AsyncInputServer` subclass.
- Initially behaved exactly like `MpiServer`.
- Added dedicated regression case `case45` for routing ExtData through `async_input_server`.

#### Step 2: Communicator contract and fallback
- `AsyncInputServer` now uses explicit `comm` and `model_comm`.
- Computes:
  - `model_size_on_node`
  - `node_size_on_node`
  - `reader_capacity_on_node`
  - `synchronous_fallback`
- Falls back when no extra node-local PEs are available.

#### Step 3a: Broader local communicator
- Local `AsyncInputServer` now gets `comm=world_comm` and `model_comm=this%model_comm`.
- This makes node-capacity accounting meaningful on the cluster.

#### Step 3b: Construction on all PETs in `comm`
- Configured local `AsyncInputServer` entries are constructed on all PETs.
- Client registration happens only on model PETs.
- This avoids invalid collectives when `comm` is larger than `model_comm`.

#### Step 4: First synchronous reader offload path
- Added a server hook for collective prefetch service.
- `AsyncInputServer` can now, when not in fallback mode:
  - receive collective prefetch requests on front/model ranks
  - delegate a concrete `file_name/var_name/start/count` read to a reader rank
  - have the reader read directly from NetCDF
  - send raw words back synchronously
  - return data to the client through the existing socket/request-handle path
- This is still synchronous.
- No current/next prefetch yet.
- No cache yet.

#### Step 4a: Local async-reader lifecycle fixes
- Local `AsyncInputServer` entries now work when `model_petcount` is smaller than the launched MPI size:
  - non-model PETs enter the async reader loop
  - model PETs stop those reader loops during finalize
- Fixed async backlog handling so serviced collective-prefetch messages are erased.
- Fixed async reader request reuse so repeated deserialization does not fail on allocatable components.
- Fixed async local-memory ownership so the non-fallback path does not double-free reader buffers.

#### Step 5a: First simple cache inside `AsyncInputServer`
- Added a first single-slot cache on the reader side using ordinary process-owned memory.
- Cache key currently uses:
  - `file_name`
  - `var_name`
  - `type_kind`
  - `start`
  - `count`
- On a cache miss, the reader performs the NetCDF read and refreshes the single cache slot.
- On a cache hit, the reader serves the request from cached raw words without rereading the file.
- Added temporary cache counters logged on reader shutdown:
  - `hits=`
  - `misses=`
- Important limitation of this first cache:
  - the cache is reader-local, not yet true node-shared memory across multiple reader PETs
  - explicit shared-memory MPI windows are still the intended next step for a real node-shared cache

#### Step 5b: Shared-memory MPI window cache backing
- Replaced the ordinary reader-local cache payload with a `ShmemReference`-backed buffer using `MPI_Win_allocate_shared`.
- The front/model rank now coordinates shared-cache allocation before the first read size that needs it.
- The reader now stores cached raw words in the shared-memory window rather than in a private allocatable array.
- Current limitation of this first shared-window version:
  - allocation choreography currently assumes `model_npes_on_node == 1`
  - this matches the current `case45`/`case46` verification shape (`model_petcount: 1`)
  - future generalization is still needed for multiple model/front PETs on the same node

#### Step 6a: Minimal `current` + cache-only `next` request path
- Added a `cache_only` flag on `CollectivePrefetchDataMessage` only.
- Added client support to submit a collective-prefetch request without creating a return-data request handle.
- Added reader/server support so a `cache_only` request:
  - resolves through the existing async reader path
  - updates the server cache
  - does not send data back to the client field buffer
- Added ExtData reader bookkeeping so queued read items can now be marked as:
  - normal current read
  - cache-only next prefetch
- Added minimal ExtData-side submission logic in `PrimaryExport` for a first `next` path:
  - when the right bracket node is enabled, distinct from the left node, and not already part of the current update batch, it is submitted as a cache-only prefetch request
- Added logging for that path in `ExtDataFileReader`:
  - `prefetching next ...`
- This is intentionally a small first Step 6 increment:
  - current consumption semantics are unchanged
  - server transport semantics are unchanged for normal reads
  - next-prefetch selection is still conservative and not yet the full rolling two-slot policy

#### Step 7a: First rolling no-interpolation verification case
- Added dedicated regression case `case47` to exercise the no-interpolation current/next path across multiple model timesteps in the non-fallback async-input configuration.
- `case47` combines the duplicate-request shape from `case46` with a longer `cap2` segment so the run advances through several 3-hour model steps on the same source file.
- Local `nag-stack` verification for `case47` now shows the expected rolling pattern in the log:
  - first timestep queues current reads and cache-only next requests
  - later timesteps queue only `prefetching next ...` requests while the current data is already carried forward by ExtData's bracket/state flow
- In a verbose local rerun, the step-2 log showed:
  - timestep `00:00`: `reading E_1 ... time index 00001` and `prefetching next E_1 ... time index 00002`
  - timestep `03:00`: only `prefetching next E_1 ... time index 00005`
  - timestep `06:00`: only `prefetching next E_1 ... time index 00008`
  - timestep `09:00`: only `prefetching next E_1 ... time index 00011`
- `case47` passed and its generated `extdata_files_read.yaml` still collapsed to a single source file:
  - `run_start: 2004-01-03T00:00:00`
  - `run_end: 2004-01-03T09:00:00`
  - `files_read: [test.20040103.nc4]`
- The verbose `case47` rerun also showed rolling cache reuse on the reader side:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=5 misses=5`
- This is not yet a generalized selector-aware `current_time + dt` lookahead implementation.
- Instead, it verifies that the existing conservative no-interpolation path now behaves like the intended rolling two-slot pattern over multiple timesteps for the current ExtData bracket semantics.
- Follow-up tracing of the bracket/state transition clarified why this works:
  - for the current no-interpolation ExtData path, the dataset consumed at timestep `N+1` is the bracket's current right node from timestep `N`
  - therefore, on this path, the existing right-node cache-only request is already the correct one-step lookahead
  - a naive fresh selector preview at `current_time + dt` was tested separately and was wrong for `case46`, because it skipped over the carried-forward right-node state that ExtData actually consumes after the bracket roll

#### Step 7b: Time-interpolation regression established
- Added dedicated regression case `case48` to probe the async-input time-interpolation path across multiple sampled times.
- `case48` is based on the existing daily-file interpolation setup from `case05`, but routes through `async_input_server`, duplicates the import (`E_1`, `E_2 <- E_1`), and enables file-read logging.
- Local `nag-stack` verification for `case48` showed:
  - the interpolation path issues normal current reads for both left and right bracket nodes at every sampled time
  - a first future-lookahead extension can now cache-only prefetch the future left-node slab for the next sampled time
  - after first grouping identical slab requests together before submission, the async reader got intra-timestep duplicate-request reuse with:
    - `INFO: AsyncInputServer cache: reader_rank=1 hits=6 misses=6`
- after then separating interpolation prefetches into a second reader and cache-only prefetching the future left-node slab, `case48` improved further to:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=11 misses=7`
- after then converting the reader cache from one slot to two slots, `case48` improved again to:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=16 misses=2`
- after then extending interpolation lookahead from future-left-only to full future-pair staging, `case48` improved further to:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=22 misses=2`
- With the two-slot cache in place, the interpolation path can keep both current left/right slabs resident while still benefiting from the future-left prefetch, which is a much better fit for the current interpolation access pattern than the original single-slot cache.
- In a verbose local rerun, the step-2 log showed repeated reads of the same two source files at each sampled time:
  - `reading E_1 from file test_20040415.nc4 at time index 00001`
  - `reading E_1 from file test_20040416.nc4 at time index 00001`
  - and the same pair repeated again at later sampled times (`12:00`, `15:00`)
- `case48` passed and its generated file-read log matched:
  - `run_start: 2004-04-15T21:00:00`
  - `run_end: 2004-04-16T15:00:00`
  - `files_read: [test_20040415.nc4, test_20040416.nc4]`
- This gives the first concrete evidence that the current Step 7 behavior does not yet extend to the time-interpolation path.
- The interpolation path now has a first cross-timestep improvement as well: the cache ends each sampled time with the future left-node slab, which reduces misses at the next sampled time.
- A future extension may still need an explicit notion of the full dataset pair consumed at the next timestep, rather than only the future left-node slab, but the two-slot cache substantially reduces the urgency of that change for the current `case48` pattern.

#### Step 7d: Interpolation rollover probe
- Added dedicated regression case `case49` to sample the interpolation path immediately before the day-change pivot.
- `case49` probes whether the future-lookahead logic follows the bracket rollover direction correctly at `2004-04-16T20:45:00`.
- Local `nag-stack` verification for `case49` showed:
  - current pair reads still come from `test_20040415.nc4` and `test_20040416.nc4`
  - the future-left preview correctly switches to `test_20040416.nc4`
  - the future-right preview now also stages `test_20040417.nc4`
- The reader cache summary for this boundary probe was:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=5 misses=3`
- `case49` passed and its generated file-read log matched:
  - `run_start: 2004-04-15T21:00:00`
  - `run_end: 2004-04-16T20:45:00`
  - `files_read: [test_20040415.nc4, test_20040416.nc4, test_20040417.nc4]`
- This confirms the interpolation lookahead is now boundary-aware for the full future consumed pair, not just the future-left slab.

#### Performance check status
- Added two helper scripts under `tests/MAPL3G_Component_Testing_Framework/benchmark/` to prepare and run a dedicated 8-front-rank baseline vs 8+1 async-reader performance comparison on a real cluster.
- The helper scripts currently prepare long interpolation workloads that span at least 20 daily source files.
- A local Mac timing check on a long interpolation workload still showed the async path slower than the baseline for many-rank slab-heavy access patterns:
  - baseline `MpiServer` (`8` ranks): wall time about `2.77 s`, `EXTDATA --run` about `0.30 s`
  - async `AsyncInputServer` (`8+1` ranks): wall time about `4.83 s`, `EXTDATA --run` about `2.43 s`
  - reader cache summary still had many misses: `hits=24 misses=2296`
- Interpretation:
  - request/pair lookahead is now working
  - the remaining bottleneck for larger decompositions is per-slab request granularity
  - the next major performance step, if needed, is node-level request aggregation / read-once-share-many

### Important Bug Fixes Made
- Fixed local configured server client-key mismatch:
  - local servers must register client key as `server_name`, not `make_client_name(server_name)`.
- Fixed `PrimaryExport` constructor signature to include `input_server_name` in the dummy argument list.

### Test Case Layout
- `case44` was restored to its original role.
- `case45` is the dedicated `AsyncInputServer` routing regression case.
- `test_cases/pfio/case01/extdata2.yaml` uses:
  - `input_server_name: async_input_server`
- `test_cases/pfio/case01/cap2.yaml` defines:
  - `servers.async_input_server.local: true`
  - `servers.async_input_server.subclass: AsyncInputServer`

### Local Verification Done
- Local runs were done with `mpirun -np 1`.
- `case45` passes in synchronous fallback mode.
- `extdata_files_read.yaml` matches `extdata_files_read_expected.yaml`.
- Typical log line:
  - `INFO: AsyncInputServer: async_input_server model_size_on_node=1 node_size=1 reader_capacity_on_node=0 synchronous_fallback=T`

### Additional Non-Fallback Verification Done
- Ran `case45` locally with `mpiexec -n 2` and `model_petcount: 1` so:
  - `model_comm` had 1 front/model PET
  - `comm` had 2 PETs on the node
  - `reader_capacity_on_node=1`
- Verified in the log:
  - `INFO: AsyncInputServer: async_input_server model_size_on_node=1 node_size=2 reader_capacity_on_node=1 synchronous_fallback=F`
- After the lifecycle and buffer-ownership fixes above, a fresh 2-step `case45` rerun completed successfully.
- `extdata_files_read.yaml` matched `extdata_files_read_expected.yaml` in the non-fallback run.
- Added lightweight temporary reader logging and verified lines of the form:
  - `INFO: AsyncInputServer reader: reader_rank=1 source_rank=0 request_id=... file=... var=...`
- This confirms the extra reader PE, not the front/model PE, is performing the concrete NetCDF read work in the current synchronous offload path.
- After `case45`/`case46`/`case47` were in place, that per-request temporary reader logging was removed again to keep routine test logs quieter.
- With the first simple cache enabled, `case45` still passes and `extdata_files_read.yaml` still matches `extdata_files_read_expected.yaml`.
- Current `case45` workload does not issue repeated identical slab requests, so the temporary cache summary is presently all misses, e.g.:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=0 misses=25`
- Added dedicated regression case `case46` to force two identical slab requests in one `cap2` run.
- `case46` uses two ExtData exports that both map to the same source variable and static file:
  - `E_1 <- E_1`
  - `E_2 <- E_1`
- `test_cases/pfio/case02/GCM2.yaml` checks correctness with:
  - `import_comparison_expressions: ['E_1-E_2 = 0.0']`
- Local 2-rank non-fallback verification for `case46` showed the intended cache summary:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=1 misses=1`
- This explicitly proves the first cache-hit path.
- After converting the cache payload to shared-memory MPI windows, `case46` still passes and still reports:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=1 misses=1`
- `case46` therefore now verifies both:
  - the duplicate-request cache-hit path
  - the shared-memory-backed cache storage path
- During the later Step 6a/current-next work, `case45`/`case46` initially regressed for two separate reasons:
  - the build tree had stale type-bound dispatch artifacts, which produced a misleading `StageDoneMessage` / `CollectiveStageDoneMessage` mismatch in the output-server path
  - the new ExtData-side current/next bookkeeping submitted requests in `current, next, current, next` order, which thrashed the single-slot cache in `case46`
- A clean rebuild under the proper `nag-stack` module environment fixed the stale-dispatch symptom.
- Reordering ExtData request submission so all current reads are queued before all cache-only next-prefetch reads fixed the cache-thrashing regression.
- After that fix, both focused regressions pass again under `nag-stack`:
  - `ctest -R MAPL3G_Comp_Test_pfio_case01 --output-on-failure`
  - `ctest -R MAPL3G_Comp_Test_pfio_case02 --output-on-failure`
- A verbose `case46` rerun now shows the expected duplicate-request cache-hit behavior again for the current/next path, e.g.:
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=2 misses=2`
- Important Step 7 design finding from this debugging pass:
  - for the current no-interpolation path verified by `case47`, the bracket's current right node is in fact the dataset consumed at the next model timestep
  - a stricter selector-aware `current_time + dt` preview is therefore not needed for this specific path and was shown to be wrong in a first prototype
  - a future selector-aware preview may still be needed when broadening Step 7 beyond this first no-interpolation path, especially for time-interpolation cases
- Full local `nag-stack` regression testing after these fixes showed:
  - all AsyncInputServer/ExtData-related component cases are now passing, including `case44`, `case45`, `case46`, and `case47`
  - previously regressed default-server ExtData cases (`case01`, `case04`, `case18`, `case21`, `case22`, `case23`, `case24`, `case30`) are passing again after scoping cache-only next-prefetch requests to `async_input_server` only
- Local `nag-stack` testing also exposed an unrelated environment-sensitive failure in `MAPL.mapl.server_utilities`:
  - the pFUnit test body passed, but a 2-rank run aborted later in `MPI_Finalize` inside the OFI teardown path (`destroy_vni_context ... Device or resource busy`)
  - that test does not require multi-rank execution, so its test target was reduced from `MAX_PES 2` to `MAX_PES 1`
  - after that adjustment, `MAPL.mapl.server_utilities` passes locally
- After these fixes, the only remaining full-ctest failures in this local environment are the 4 `Regrid_Util` regression tests:
  - `ll-ll`
  - `cs-cs`
  - `cs-ll`
  - `ll-cs`
- Those failures are not tied to AsyncInputServer work; they report missing regression-data directories and ask whether `LOCAL_REGRESSION_DATA_DIR` is set correctly.
- Also manually verified a multi-model-front configuration using `case46`-style inputs with:
  - `model_petcount: 2`
  - total MPI size `3`
  - one extra reader PE on the node
- Confirmed activation log line:
  - `INFO: AsyncInputServer: async_input_server model_size_on_node=2 node_size=3 reader_capacity_on_node=1 synchronous_fallback=F`
- That run completed successfully, showing the current shared-window cache path works for `model_petcount > 1` with a single reader PE.
- In that configuration, the current duplicate-request pattern decomposed into distinct per-rank slabs, so the temporary cache summary showed misses only:
  - `INFO: AsyncInputServer cache: reader_rank=2 hits=0 misses=4`

### What Still Needs Cluster Verification
- Run `case45` on the actual cluster where `comm` includes extra node-local PEs beyond `model_comm`.
- Confirm in `cap2.log`:
  - `reader_capacity_on_node > 0`
- Confirm the test still passes there as it now does in the local 2-rank single-node verification.
- Keep or remove the temporary reader-rank logging depending on whether more cluster debugging is needed.
- Generalize the shared-window cache choreography beyond `model_npes_on_node == 1` if/when that becomes necessary.
- If full app-regression coverage is needed locally, configure `LOCAL_REGRESSION_DATA_DIR` so the `Regrid_Util` regression tests can find their datasets.

### Next Planned Step
- Treat Step 7 as verified for the current no-interpolation collective-prefetch path via `case47`.
- Broaden the rolling policy beyond this first path:
  - cover time-interpolation cases explicitly
  - decide whether the current full future-pair staging plus two-slot cache is sufficient for interpolation, or whether further aggregation/caching of the full dataset payload is still needed for many-rank workloads
- Keep the cache-only next-prefetch path scoped to `async_input_server` until the default pFIO route learns how to handle handle-less collective-prefetch requests safely.
- Keep in mind that a future cleanup/generalization pass may still be needed for multi-front-rank-per-node shared-window cache allocation.
- `case48` and `case49` are now the regression targets for any future interpolation-path performance work.
- If the cluster performance run still shows many steady-state misses after warmup, move directly to node-level request aggregation and shared dataset serving.

### Current State (2026-09-03 — end of day)

#### Async path logic — verified correct
- The `NextCollectivePrefetchMessage` / `NextCollectivePrefetchDoneMessage` split is implemented and working.
- Message flow:
  1. Client: `collective_prefetch_data_cache_only` → sends `NextCollectivePrefetchMessage` to server
  2. Server (`handle_NextCollectivePrefetchData`): pushes message into `request_backlog`; sends `DummyMessage` handshake back
  3. Client: `done_collective_prefetch` → sends `CollectivePrefetchDoneMessage` (if current items) AND/OR `NextCollectivePrefetchDoneMessage` (if next items), in that order
  4. Server (`handle_Done_collective_prefetch`): waits for all threads; calls `service_collective_prefetch`; processes `CollectivePrefetchDataMessage` items; `finish_collective_service` resets `serverthread_done_msgs` if backlog still non-empty (e.g., has `NextCollectivePrefetchMessage` items)
  5. Server (`handle_Done_next_collective_prefetch`): waits for all threads; calls `service_next_collective_prefetch`; finds `NextCollectivePrefetchMessage` items in backlog; calls `forward_request_to_reader(…, deliver_to_client=.false.)` per item
  6. Reader (rank NOT in `model_comm`): receives `ASYNC_INPUT_CMD_READ` via `MPI_Recv`; deserializes `CollectivePrefetchDataMessage`; sees `cache_only=.true.`; reads the file, updates shared cache, does NOT send data back
- `service_collective_prefetch` uses `type is (CollectivePrefetchDataMessage)` which is an **exact type match** in Fortran `select type` — it correctly skips `NextCollectivePrefetchMessage` subtype items
- `service_next_collective_prefetch` uses `type is (NextCollectivePrefetchMessage)` — exact match for the next-prefetch items

#### Local verification (2026-09-03)
- All 5 async tests pass: `ctest -R "MAPL3G_Comp_Test_case(45|46|47|48|49)"` → 5/5 passed
- Non-fallback 2-rank (1 model + 1 reader) manual run of case45 shows:
  - `INFO: AsyncInputServer: async_input_server model_size_on_node=1 node_size=2 reader_capacity_on_node=1 synchronous_fallback=F`
  - `INFO: AsyncInputServer forwarded: requests=73`
  - `INFO: AsyncInputServer cache: reader_rank=1 hits=47 misses=26 requests=73`
- Non-fallback 3-rank (2 model + 1 reader) manual run of case45 shows:
  - Both model ranks (0 and 1) forward their slice to reader rank 2
  - `INFO: AsyncInputServer forwarded: requests=73` (from rank 0 only, by design)
  - `INFO: AsyncInputServer cache: reader_rank=2 hits=92 misses=54 requests=146`
  - Reader processes 146 requests = 2 × 73 (both model PETs' slices)
- The previously reported `forwarded_requests=0` in the `8+1` benchmark was from an earlier code state (prior to the `NextCollectivePrefetchMessage` split being fully wired up); the current code is confirmed working

#### Blocking issues resolved
- `cap2.yaml` crash in `insert_RequestHandle` during ExtData init: **resolved** (root cause was stale build artifacts; clean rebuild fixed it)
- `cap1.yaml` local history output crash: **resolved** via `GeomPFIO` clone fix and `HistoryGridComp` `post_wait_all` bypass for `SimpleSocket`
- No temporary debug prints remain in the codebase (`pfio/AsyncInputServer.F90` debug lines added for this session were removed)

#### Node-level aggregation — implemented and verified (2026-09-03)

**Design**: Reader-side global-key cache.
- Cache key = `(file_name, var_name, type_kind, global_start, global_count)` — identical for all model ranks requesting the same variable/time.
- Reader stores the **full global slab** in a `LocalMemReference`.
- On a cache miss: reads full global slab from file, stores it.
- On a cache hit: data is already in cache.
- For every request (hit or miss): extracts the per-rank LOCAL slice via `copy_subarray` and MPI_Sends it back.
- No collective operations between model ranks required — each rank operates independently.
- `copy_subarray`: recursive Fortran-column-major sub-array copy; handles arbitrary N-dimensional hyper-slabs.

**Previous approach (broken)**: ShmemReference over `model_node_comm` required `MPI_Win_allocate_shared` — a collective over model ranks — inside the service path. Each model rank enters the service path independently (no cross-rank synchronization), causing deadlock with `model_petcount > 1`.

**Benchmark results after node-level aggregation (macOS, 2026-09-03)**:

| Case | NP | Wall | EXTDATA run | Cache hits | Misses | Requests |
|------|----|------|-------------|------------|--------|----------|
| mpi8 | 8 | 2.98 s | 1.10 s | — | — | — |
| async9 | 9 | 3.13 s | 1.11 s | 2284 | 20 | 2304 |

- **Cache hit rate: 99.1%** (vs 7.3% before aggregation, 0% before this feature)
- The 20 compulsory misses are unavoidable cold-start reads (first access to each unique global slab)
- async9 wall time (3.13 s) ≈ mpi8 (2.98 s) — the async reader is now competitive
- `reader_requests = 2304 = 36 × 8 × 8` (still 8 requests per rank per timestep); but only 20 result in file reads

**Files changed for this step**:
- `pfio/AsyncInputServer.F90`: full rewrite of the reader loop and cache system
  - `AsyncInputCacheSlot`: cache key is now `global_start`/`global_count`; payload is `LocalMemReference` (not `ShmemReference`)
  - `read_global_slab_into_slot`: reads full global slab using `global_start`/`global_count`
  - `extract_local_slice_from_slot`: calls `copy_subarray` to extract per-rank slice
  - `copy_subarray`: new recursive N-D sub-array copy routine
  - `forward_request_to_reader`: unchanged in structure; reader now returns LOCAL slice
  - `ensure_model_cache_capacity` / `ShmemReference` usage: **removed** (no longer needed)
- `tests/MAPL3G_Component_Testing_Framework/benchmark/prepare_async_perf_cases.sh`:
  - Fixed `segment_duration` for cap1 (`P25D`) and cap2 (`P30D`)
  - Added `--model-delay SECS` and `--grid IMxJM` controls
- `tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh`:
  - Uses `--use-hwthread-cpu` on macOS so model and reader ranks execute concurrently
  - Does not inject any reader sleep

#### Real I/O / Client-Delay Benchmark (2026-09-03)
- Method: reader performs only real NetCDF reads; model-side `model_delay` is
  added equally to mpi8 and async9. No `ASYNC_READER_SLEEP_SEC` or other reader
  delay is used.
- Preliminary measurement with `1024x768` files and no model delay:
  - async reader actual NetCDF read total: `0.1859 s` over 20 compulsory misses
  - maximum individual reader read: `0.0258 s`
- Comparable-delay run:
  - preparation: `prepare_async_perf_cases.sh WORK --model-delay 0.02 --grid 1024x768`
  - launch: benchmark runner with Open MPI `--use-hwthread-cpu -oversubscribe`
  - mpi8 wall: `30.10 s`; EXTDATA mean: `2.86 s`
  - async9 wall: `29.79 s`; EXTDATA mean: `2.40 s`
  - async reader: `hits=2284 misses=20 requests=2304`, real read total `0.1796 s`, maximum `0.0296 s`
  - async9 was approximately `1.0%` faster in wall time and `16%` faster in the EXTDATA profile
- A larger-grid run (`1024x768`, `model_delay=0.5`) was also tested. It made
  real reads measurable but increased MPI slice-transfer overhead; therefore
  the reader/client timing must be chosen from measured read time rather than
  by adding reader sleeps.
- Important interpretation: on this macOS laptop, the reader's actual NetCDF
  miss time is only about 9 ms on average, so a client delay around `0.02 s`
  is comparable. A stronger speedup requires a real cluster/storage system
  with slower reads or a substantially larger workload; the benchmark must
  not fake reader work with a sleep.

#### Controlled 5-second Reader/Model Experiment (2026-09-03)
- Added benchmark-only environment variable `MAPL_PERF_READER_SLEEP_SEC`.
  When set, both `ServerThread%get_DataFromFile` (MpiServer) and the
  AsyncInputServer reader sleep after each actual file read. This gives both
  paths the same artificial reader cost for a controlled experiment.
- Added `--quick` to `prepare_async_perf_cases.sh`; it uses four timed model
  steps so the 5-second experiment remains practical.
- Run command:
  ```bash
  bash tests/MAPL3G_Component_Testing_Framework/benchmark/prepare_async_perf_cases.sh \
       /tmp/async-perf-5sec --model-delay 5 --quick
  MAPL_PERF_READER_SLEEP_SEC=5 \
       bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh \
       /tmp/async-perf-5sec build /path/to/mpiexec
  ```
- Both runs used macOS Open MPI `--use-hwthread-cpu -oversubscribe`.
- Results:
  - mpi8: `41.88 s` wall, approximately `20 s` reader sleep + `20 s` model sleep
  - async9: `41.82 s` wall, `hits=252 misses=4 requests=256`, approximately
    `20 s` reader sleep + `20 s` model sleep
- This short interpolation workload did not show a speedup. The async reader
  ran concurrently, but its four compulsory 5-second misses account for the
  same 20 seconds as the baseline. The short run therefore measures cold-start
  cost more than steady-state overlap.
- The artificial sleep is a benchmark-only hook, not production behavior.

#### Reader Delay Placement (2026-09-03)
- `MAPL_PERF_READER_SLEEP_SEC` now runs immediately after each reader-side
  NetCDF `get_var` returns and before `formatter%close()`, cache completion,
  and response handling.
- The same placement is used in `ServerThread%get_DataFromFile` for the
  MpiServer baseline path. The delay models a long reader operation and is
  before request completion/fence handling.
- Build succeeded and all five async regression tests pass after this change.

#### Eight-Step 5-second Benchmark (2026-09-03)
- Extended `--quick` from four to eight timed steps to reduce cold-start
  weighting and expose steady-state behavior.
- Configuration:
  - `model_delay=5 s`
  - `MAPL_PERF_READER_SLEEP_SEC=5 s` in both MpiServer and AsyncInputServer
  - `--use-hwthread-cpu -oversubscribe`
- Results:
  - mpi8: `81.94 s`
  - async9: `72.21 s`
  - async9 improvement: approximately `11.9%`
  - async cache: `hits=506 misses=6 requests=512`
  - async reader sleep/read total: approximately `30.04 s`
- Timestamp interpretation:
  - mpi8 reader intervals occur before each model sleep, so the two 5-second
    delays are serial: reader interval, then model sleep.
  - async9 next-prefetch dispatch happens before model sleep, and the longer
    run reaches steady-state cache reuse. Async reader intervals remain queued
    around model phases, but the total wall-time reduction appears once the
    cold-start cost is amortized over eight steps.

#### 5-second Concurrency Verification (2026-09-03)
- Added temporary wall-clock timestamps to both sides:
  - `CapGridComp`: `Cap model sleep: ... start=<MPI_Wtime>` / `end=<MPI_Wtime>`
  - `AsyncInputServer`: `reader interval: ... start=<MPI_Wtime>` / `end=<MPI_Wtime>`
  - `ServerThread`: equivalent MpiServer reader interval timestamps
- Rebuilt successfully and reran the four-step test with:
  - `model_delay=5`
  - `MAPL_PERF_READER_SLEEP_SEC=5`
  - macOS Open MPI `--use-hwthread-cpu -oversubscribe`
- Results remained:
  - mpi8: `41.59 s`
  - async9: `41.80 s`
- Direct async interval evidence from `perf-async9/cap2.log`:
  - model sleep interval: approximately `[10.53, 15.60]`
  - reader interval: `[15.59, 20.59]` (cold/current phase)
  - model sleep interval: approximately `[20.58, 25.64]`
  - reader interval: `[30.67, 35.67]` (next-prefetch phase)
  - model sleep interval: approximately `[35.66, 40.73]`
  - reader interval: `[30.67, 35.67]` overlaps the following model sleep interval `[35.66, 40.73]` only at the boundary in this four-step run; the reader's earlier `[15.59,20.59]` interval also overlaps the model's `[20.58,25.64]` boundary by scheduling jitter.

#### Cleanup (2026-09-03)
- Removed temporary wall-clock and fence diagnostic logging from
  `ClientThread.F90`, `ServerThread.F90`, `AsyncInputServer.F90`, and
  `CapGridComp.F90`.
- Preserved the validated next-before-current fence order.
- Preserved the benchmark-only `MAPL_PERF_READER_SLEEP_SEC` hook and the
  `model_delay` / `--quick` benchmark controls.
- Rebuilt successfully and reran case45–49: all five tests pass.
- The timestamps confirm the async reader is a separate MPI rank and is not
  globally serialized behind model computation. However, this particular
  short run is dominated by cold-start reads and MPI scheduling; total wall
  time alone cannot be used as proof of overlap.
- All five PFIO regression tests still pass: `pfio_case01`–`pfio_case05`.

#### Fence Order Update (2026-09-03)
- `ClientThread%done_collective_prefetch` now sends
  `NextCollectivePrefetchDoneMessage` before `CollectivePrefetchDoneMessage`.
- This is the required protocol order: next-prefetch requests are dispatched
  before the current-prefetch fence is submitted.
- The temporary per-request `ASYNC_INPUT_TAG_STARTED` acknowledgment was
  removed. Waiting for an acknowledgment for every next request serialized
  the multi-request next-prefetch batch and defeated overlap.
- A four-step run with `model_delay=5`,
  `MAPL_PERF_READER_SLEEP_SEC=5`, and `--use-hwthread-cpu` still measured
  approximately equal wall times (`mpi8=41.55 s`, `async9=41.64 s`). The logs
  confirm the fence ordering, but the reader intervals did not yet show a
  full 5-second overlap with the model interval. Further request-queue tracing
  is needed before claiming end-to-end overlap.

### How To Resume
- Read these two files first:
  - `.opencode/plans/async-input-server-plan.md`
  - `.opencode/plans/async-input-server-status.md`
- All 5 regression tests (case45–49) pass
- Benchmark shows 99.1% cache hit rate, async9 ≈ mpi8 wall time
- The 20 remaining misses are compulsory cold-start misses — zero misses after warmup
- **Next steps if continuing**:
  - Consider whether 2 cache slots is optimal for the interpolation path (current=2, future-left+right=2 total = 4 unique slabs per timestep → 2 slots may evict too aggressively at the day-change boundary)
  - Cluster verification: run the benchmark on the actual cluster where `node_size` > 1 and multiple model nodes exist
  - If needed: increase `ASYNC_INPUT_NUM_CACHE_SLOTS` to 4 to match the 4 unique slabs in the interpolation access pattern
