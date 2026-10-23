## Async Input Server Project Review

Date: 2026-09-30
Reviewer: automated code review
Scope:

- `.opencode/plans/async-input-server-plan.md` (Steps 0-22)
- `.opencode/plans/async-input-server-status.md`
- Implementation at `HEAD` = `9f9a066a6` ("remove local for AsyncInputServer")
- `pfio/AsyncInputServer.F90`, `pfio/AbstractServer.F90`, `pfio/ServerThread.F90`
- `mapl/MaplFramework.F90`, `mapl/MaplServerUtilities.F90`, `mapl/PfioServerGridComp.F90`
- `gridcomps/extdata/ExtDataFileReader.F90`, `ExtDataGridComp.F90`, `PrimaryExport.F90`,
  `NonClimDataSetFileSelector.F90`, `ClimDataSetFileSelector.F90`
- `pfio/tests/Test_AsyncInputServer.pf`, `mapl/tests/Test_MaplServerUtilities.pf`
- `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case01`-`case05`
- `tests/MAPL3G_Component_Testing_Framework/benchmark/README.md`

### Independent Verification Performed

Re-ran build and tests at `HEAD` in this review (the last full `ESSENTIAL` run
recorded in the status log predates the `HEAD` commit, which modified
`MaplFramework.F90` and `MaplServerUtilities.F90`):

```bash
zsh -lic 'module load nag-stack && cmake --build build -j 8 --target build-tests'
zsh -lic 'module load nag-stack && MAPL_ASYNC_INPUT_SHMEM_WORDS=16 MAPL_ASYNC_INPUT_CACHE_SLOTS=1 ctest --test-dir build -R "^MAPL.pfio.tests$" --output-on-failure --timeout 120'
zsh -lic 'module load nag-stack && ctest --test-dir build -L ESSENTIAL --output-on-failure --timeout 300'
```

Results:

- `build-tests`: passed, target reached 100%.
- `MAPL.pfio.tests`: 1/1 passed, 5.74 s.
- Full `ESSENTIAL` label: 69/69 passed, 399.73 s.

### Step Traceability

The claim that all plan steps are implemented holds structurally. Each of
Steps 16-22 was traced to concrete source, not only to the status log.

| Step | Evidence |
| --- | --- |
| 16 roles | `pfio/AsyncInputServer.F90:206-229` (role flags, no `model_comm` component), `:297-370` (`initialize_role_accounting`), `_UNUSED_DUMMY(comm)` at `:369` |
| 17 protocol | `AsyncInputRequestMetadata`/`Assignment`/`Completion` at `:75-103`, ten distinct tags at `:33-42`, pack/unpack at `:1405-1471` |
| 18 worker mailboxes | `worker_segment_words` `:1611`, allocation only on `worker_role` at `:390`, five mailbox states `:47-51`, `consume_shared_result` `:1536` |
| 19 control-only captain | `serve_warm_requests` `:1168` annotates hints only; worker re-validates at `:846-853` |
| 20 scheduling | `find_key_owner` `:969`, `select_worker_for_request` `:931`, `assign_key_group` `:1039` |
| 21 shutdown | `ASYNC_INPUT_TAG_TERMINATED` handshake `:594-597` and `:652-662`; unconditional `run_local_async_servers` at `mapl/MaplFramework.F90:715` |
| 22 coverage/docs | 12 pFUnit tests in `Test_AsyncInputServer.pf`, `pfio/case05/output_checks.rc`, `pfio/pfio.md:192-270` |

Earlier Steps 0-15 are largely superseded by the Step 16-22 redesign. Step 13
(selector-aware lookahead) survives as `preview_bracket` on both selectors.
Step 15 (cluster verification) was never performed.

### Findings

#### 1. High: the design's purpose is not demonstrated, and is measurably negative

`tests/MAPL3G_Component_Testing_Framework/benchmark/README.md` records
`AsyncInputServer` at 22.2% slower than `MpiServer` (10.92 s versus 8.93 s
mean over three runs), while using 8 PETs instead of 5. That is 60% more
processes for worse wall time.

The README correctly attributes this to fast local storage plus CPU
oversubscription on a laptop, and correctly states that a representative
conclusion requires the target cluster. However, plan Step 15 (cluster
verification) was never executed, and it is the only thing that can justify
the redesign. At present the project carries roughly 2000 lines of new server
code, a new MPI control protocol, and MPI shared-memory windows with no
evidence of benefit on any machine.

Recommendation: treat Step 15 as a blocker for declaring the project done,
not as a leftover item.

#### 2. Medium: the `local: true` removal weakened rather than tightened validation

`is_local_server_configuration` (`mapl/MaplServerUtilities.F90:55`) hardcodes
`subclass == 'AsyncInputServer'` implies local placement. Three call sites in
`MaplFramework.F90` (`:580`, `:746`, `:912`) then immediately re-derive
`subclass_name` and re-test `subclass_name == 'AsyncInputServer'`. The subclass
string is parsed twice per iteration at each site, and the placement rule now
lives in two files.

`validate_server_configuration` still accepts an explicit `local: true`
(`mapl/MaplServerUtilities.F90:48-49`) while `pfio/pfio.md` says the key is
unnecessary, so two spellings of the same configuration are now permanently
valid.

Recommendation: have `is_local_server_configuration` return the resolved
subclass, or expose a single `resolve_server_placement` helper, so the rule and
the subclass parse exist once.

#### 3. Medium: Step 22's typed-payload verification is not implemented

Plan Step 18/22 require verifying INT32, INT64, REAL32, and REAL64 payloads.
Every test in `pfio/tests/Test_AsyncInputServer.pf` uses `pFIO_REAL32` only.
`read_global_slab_into_slot` (`pfio/AsyncInputServer.F90:1732-1759`) has four
type branches; three are covered by nothing.

Step 18's "verify controlled overflow and size-mismatch failures" likewise has
no test. The status log at lines 267-268 acknowledges the difficulty ("a
dedicated negative test cannot use MAPL assertions without terminating that MPI
test"), but the plan item was still recorded as complete.

Recommendation: either add typed fixtures, or amend the plan to record these as
explicitly deferred with a reason.

#### 4. High: shared-memory spin loops rely on `MPI_Win_sync` with no `volatile`

`publish_result_to_mailbox` (`pfio/AsyncInputServer.F90:1509-1533`) and
`consume_shared_result` (`:1568-1605`) spin on
`mailboxes(offset + ASYNC_INPUT_MAILBOX_STATE_WORD)` through a plain
`integer, pointer`. There is no `volatile` attribute anywhere in the file.

`MPI_Win_sync` is a barrier for the MPI RMA memory model, but it does not
prevent the Fortran compiler from hoisting the load out of the spin loop. The
code works under the current NAG Debug build; it is not guaranteed under Intel
or GNU at `-O2`.

This is the highest-risk construct in the new code and carries no comment
acknowledging the hazard.

Recommendation: declare the mailbox pointer target `volatile`, or route the
state read through a procedure the compiler cannot inline away, and add a
comment explaining why.

#### 5. High: reader-side error status can never reach a model rank

`ASYNC_INPUT_MAILBOX_ERROR` is only ever written by
`publish_result_to_mailbox` when `result_status /= MPI_SUCCESS`
(`pfio/AsyncInputServer.F90:1521-1522`). The sole caller,
`publish_shared_result`, is invoked with a literal `MPI_SUCCESS`
(`:486-487`).

`execute_reader_request` uses `_RC`, so a NetCDF read failure aborts the worker
rather than propagating a status to the waiting model rank. The model rank then
spins indefinitely in `consume_shared_result`. The mailbox state machine has an
ERROR state with no producer.

Recommendation: capture the read status in `execute_reader_request`, pass it
through to `publish_shared_result`, and let the model rank fail deterministically
instead of hanging.

#### 6. Medium: `publish_shared_result` leaves `rc` undefined on its failure path

`publish_shared_result` (`pfio/AsyncInputServer.F90:1485`) executes
`if (ierr /= MPI_SUCCESS) return` without assigning `rc`, which is
`optional, intent(out)`. The caller at `:486` uses `_RC`, so a mailbox-rank
error yields an undefined `status`.

The comparable early returns in `poll_reader_completions` (`:1071`, `:1079`,
`:1091`) are correct because that routine returns `ierr` directly.

Recommendation: `_VERIFY(ierr)` or `_RETURN` with the failure status.

#### 7. Medium: Step 13's own verification criterion is unmet

`preview_bracket` is implemented on both selectors
(`NonClimDataSetFileSelector.F90:144-251`, `ClimDataSetFileSelector.F90:109-176`)
and correctly omits the `set_last_update` mutation that
`update_file_bracket` performs (compare `NonClimDataSetFileSelector.F90:140`).
That is the right design and is a genuine improvement over the previous
bracket-coincidence heuristic.

However, Step 13 asked for "a case with an irregular time step where the old
right-node-coincidence heuristic would prefetch the wrong dataset."
`pfio/case03` uses `dt: PT2H30M`, which is irregular, but its `compare.rc`
diffs only `extdata_files_read.yaml`. Both a correct and an incorrect prefetch
would produce the same file set, so no existing test would fail if
`preview_bracket` returned the wrong node.

Recommendation: add an assertion on which dataset was prefetched, for example
through captain/worker counters or an `output_checks.rc` regex, rather than on
the file list alone.

#### 8. Medium: mixed local-async plus remote-output remains structurally untested

`run_local_async_servers` (`mapl/MaplFramework.F90:750-768`) contains a real
behavioral branch: when `is_model_node()` is false the PET calls `shutdown()`
instead of `start()` and continues to its remote server GridComp. No test
reaches that branch. All five pfio component cases use `model_petcount: 1` with
no remote server section.

Step 21 and Step 22 both acknowledge this and attribute it to the single-node
CTest environment, which is accurate. It remains an untested code path in a
shipped feature.

Recommendation: fold this into Step 15 cluster verification with an explicit
mixed-configuration case.

#### 9. Medium: the default mailbox size scales badly

`ASYNC_INPUT_DEFAULT_MAILBOX_WORDS = 4 * 1024 * 1024`
(`pfio/AsyncInputServer.F90:60`) and
`worker_segment_words = model_size * (header + mailbox_words)` (`:1618`).

With 40 model ranks on a node and 2 workers this reserves roughly 2.5 GiB of
shared memory per node regardless of the actual slice size. `pfio/pfio.md`
documents the variable but not this multiplication.

`tests/MAPL3G_Component_Testing_Framework/CMakeLists.txt:71-74` already forces
`MAPL_ASYNC_INPUT_SHMEM_WORDS=4096` to "keep concurrent tests below restrictive
cluster shared-memory limits," which is evidence that the default is wrong
rather than that the tests are unusual.

Recommendation: size the mailbox from the request rather than from a fixed
default, or at minimum document the per-node product and lower the default.

### Minor

- Indentation is inconsistent throughout `pfio/AsyncInputServer.F90`: 24
  distinct leading-space counts, with 5-, 6-, 7-, 8-, 9-, 10-, and 11-space
  bodies mixed inside single procedures (for example `start` at `:453-603`).
  The house standard is 3 spaces. A mechanical reformat pass would reduce diff
  noise substantially.
- `new_AsyncInputServer` reuses `sleep_string`, `sleep_length`, and
  `sleep_status` (`:270-288`) to parse `MAPL_ASYNC_INPUT_SHMEM_WORDS` and
  `MAPL_ASYNC_INPUT_CACHE_SLOTS`. These names are leftovers from the removed
  `MAPL_PERF_READER_SLEEP_SEC` code and are actively misleading.
- `MAPL_Sleep(0.0001)` is hard-coded at four sites (`:529`, `:1074`, `:1513`,
  `:1573`) with no named constant.
- `.opencode/plans/async-input-server-plan.md` still presents Steps 0-15 as
  active content. Steps 9-14 are superseded by Steps 16-22, and Step 15 is the
  only live item; pruning would make the remaining blocker visible.
- `.opencode/plans/async-input-server-status.md` is 2293 lines with all
  pre-Step-16 history retained inline, which makes the current state hard to
  locate. Consider archiving the historical sections.

### Assessment

Implementation quality is high. The captain/worker split is clean.
`find_key_owner` (`:969`) correctly handles the busy-owner and
already-queued-follower cases that the Step 20 review flagged. The worker-side
hint re-validation (`:840-853`) is a sound design that makes stale captain
directory metadata harmless rather than dangerous. The test suite is
substantive rather than decorative, and the whole `ESSENTIAL` label passes.

The problems are not in the code that exists. They are:

- the feature has never been shown to help anything (Finding 1);
- three correctness hazards are unguarded (Findings 4, 5, 6);
- several plan verification criteria were marked complete without the test the
  criterion names (Findings 3, 7, 8).

Recommendation: do not treat the project as complete. Fix Findings 4, 5, and 6
first, since they are small and are genuine correctness risks under other
compilers. Then execute Step 15 on the target cluster, including a mixed
local-async plus remote-output configuration, before adding further
functionality.
