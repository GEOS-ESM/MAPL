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
