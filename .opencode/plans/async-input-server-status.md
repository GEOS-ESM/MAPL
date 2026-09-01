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
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case45/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case47/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case48/`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/case49/`

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

### Important Bug Fixes Made
- Fixed local configured server client-key mismatch:
  - local servers must register client key as `server_name`, not `make_client_name(server_name)`.
- Fixed `PrimaryExport` constructor signature to include `input_server_name` in the dummy argument list.

### Test Case Layout
- `case44` was restored to its original role.
- `case45` is the dedicated `AsyncInputServer` routing regression case.
- `case45/extdata2.yaml` uses:
  - `input_server_name: async_input_server`
- `case45/cap2.yaml` defines:
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
- `case46/GCM2.yaml` checks correctness with:
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
  - `ctest -R MAPL3G_Comp_Test_case45 --output-on-failure`
  - `ctest -R MAPL3G_Comp_Test_case46 --output-on-failure`
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

### How To Resume Tomorrow
- Read these two files first:
  - `.opencode/plans/async-input-server-plan.md`
  - `.opencode/plans/async-input-server-status.md`
- Mention `case45` and `AsyncInputServer`.
- State whether cluster verification of the non-fallback path has happened yet.
