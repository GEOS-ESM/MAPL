## Async Input Server Fix Plan

Date: 2026-09-30
Source: `.opencode/plans/async-input-server-review.md`
Baseline: `HEAD` = `9f9a066a6`, full `ESSENTIAL` 69/69 passing.

This plan continues the step numbering of
`.opencode/plans/async-input-server-plan.md` (which ends at Step 22) so both
plans share one status log, `.opencode/plans/async-input-server-status.md`.

### Ordering Rationale

Steps 23-25 come first because they are correctness hazards that the current
NAG Debug build happens to hide. They are small, local, and independent of any
cluster access.

Step 23 (memory model) precedes Step 24 (error propagation) because Step 24's
new test asserts on an observed mailbox state transition, which is exactly the
read that Step 23 makes safe.

Step 30 (mechanical reformat) is deliberately last. Doing it first would make
every correctness diff in Steps 23-29 clean, but it would also entangle a
whole-file rewrite with those fixes, making any later revert or cherry-pick of
a correctness change painful. The Steps 23-29 diffs are localized enough to
review against current `HEAD` without the reformat.

Step 31 (cluster verification) is last among the substantive work because it is
the only step requiring external resources, and because its outcome determines
whether this feature should ship at all.

---

### Step 23: Make shared-memory mailbox reads safe under optimization

- Goal: remove the undefined-behavior hazard in the mailbox spin loops.
- Problem (review Finding 4):
  - `publish_result_to_mailbox` (`pfio/AsyncInputServer.F90:1509-1533`) and
    `consume_shared_result` (`:1568-1605`) spin on
    `mailboxes(offset + ASYNC_INPUT_MAILBOX_STATE_WORD)` through a plain
    `integer, pointer`.
  - There is no `volatile` attribute anywhere in the file.
  - `MPI_Win_sync` is a barrier for the MPI RMA memory model; it does not stop
    the Fortran compiler from hoisting the load out of the loop.
  - Works today under NAG Debug. Not guaranteed under Intel or GNU at `-O2`.
- Change:
  - Declare the mailbox pointers `volatile` in both routines, so accesses
    through them are not cached across `MPI_Win_sync`.
  - Add a comment at each spin loop stating why `volatile` is required and that
    `MPI_Win_sync` alone is insufficient.
  - Keep the existing `MPI_Win_sync` calls; `volatile` and the sync are
    complementary, not alternatives.
- Fallback if `volatile` proves unusable:
  - Move the single state read into a small non-inlinable accessor function
    taking the window and offset, and document that the call boundary is what
    forces the reload. Record the reason in the status log.
- Scope limit:
  - Only the state word and the words read before the state is trusted. Do not
    make the bulk payload copy volatile; that would defeat vectorization for no
    benefit, because the payload is only read after a `READY` state is observed.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - NAG `build-tests` builds with no new warning class. NAG is strict about
    `volatile` interacting with `c_f_pointer`; capture any diagnostic verbatim
    in the status log.
  - `MAPL.pfio.tests` passes, including all multi-worker mailbox tests.
  - PFIO component cases 01-05 pass.
  - If a gfortran or Intel build is available locally, build `pfio` at `-O2`
    with both and confirm no new diagnostics. This is the compiler class the
    change exists to protect, so note explicitly in the status log whether that
    confirmation was possible.
- Risk:
  - `volatile` can suppress optimizations in the surrounding routine. Payload
    copy performance is the thing to watch; the scope limit above is what keeps
    that contained.

---

### Step 24: Propagate reader-side read failures to the requesting model rank

- Goal: make a NetCDF read failure produce a deterministic error on the model
  rank instead of a three-way hang.
- Problem (review Finding 5):
  - `ASYNC_INPUT_MAILBOX_ERROR` has no producer.
    `publish_result_to_mailbox` only sets it when `result_status /= MPI_SUCCESS`
    (`pfio/AsyncInputServer.F90:1521-1522`), but its sole caller passes a
    literal `MPI_SUCCESS` (`:486-487`).
  - `execute_reader_request` uses `_RC` (`:483`), so a read failure returns from
    the worker's `start` loop before any completion is sent.
  - The captain then blocks in `poll_reader_completions` and the model rank
    spins forever in `consume_shared_result`.
- Change:
  - Have `execute_reader_request` return a read status instead of aborting the
    worker: capture the failure, leave the cache slot invalid, and report the
    status to the caller.
  - In the worker loop, on a failed read of a demand request, publish the
    mailbox with that failure status so the model rank observes
    `ASYNC_INPUT_MAILBOX_ERROR` rather than spinning.
  - Send the completion record with the failure status in every case, so the
    captain's bookkeeping stays consistent and the worker stays in its loop.
  - The captain already treats `completion%status /= MPI_SUCCESS` as
    `MPI_ERR_OTHER` (`:1081-1089`); confirm that path unwinds cleanly rather
    than leaving workers busy, and fix it if it does not.
  - For a failed cache-only request, do not publish a mailbox (there is no
    waiting consumer). Report the failure through the completion only, and do
    not record a warm directory entry for the failed key.
- Scope limit:
  - The goal is a deterministic, diagnosable failure, not recovery. The model
    rank is expected to fail with a clear message. Do not attempt retry or
    fallback-to-synchronous-read in this step.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - New focused test: 3 ranks (1 model, 1 captain, 1 worker), demand request
    naming a file that does not exist. Assert the model rank returns a nonzero
    status within the CTest timeout rather than hanging, and that the captain
    and workers still reach `finalize_runtime`.
  - Run that test under an explicit `--timeout` so a regression to the hanging
    behavior fails deterministically instead of stalling CI.
  - `MAPL.pfio.tests` and PFIO cases 01-05 unchanged.
- Risk:
  - A test that deliberately induces failure can leave the MPI job in a state
    pFUnit cannot clean up. If the error path cannot be tested without
    aborting the MPI test binary, say so explicitly in the status log rather
    than marking the verification complete, and keep the production fix.

---

### Step 25: Fix the undefined `rc` on the mailbox publish failure path

- Goal: remove an undefined `intent(out)` status.
- Problem (review Finding 6):
  - `publish_shared_result` (`pfio/AsyncInputServer.F90:1485`) executes
    `if (ierr /= MPI_SUCCESS) return` without assigning `rc`, which is
    `optional, intent(out)`.
  - The caller at `:486` uses `_RC`, so a mailbox-rank error yields an
    undefined `status`.
  - The comparable early returns in `poll_reader_completions` (`:1071`,
    `:1079`, `:1091`) are correct, because that routine returns `ierr`
    directly.
- Change:
  - Replace the bare `return` with `_VERIFY(ierr)` so the failure is reported
    through the established error path.
  - Audit the whole file for any other `intent(out)` status left unassigned on
    an early return and fix the same way.
- Scope limit:
  - Mechanical. No protocol or behavior change beyond correct status
    reporting.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - NAG `build-tests` passes. NAG's undefined-variable checking in Debug may
    now flag previously masked paths; record any new diagnostic.
  - `MAPL.pfio.tests` and PFIO cases 01-05 unchanged.
- Note:
  - Can be folded into Step 24's commit if the audit turns up only this one
    site, since both touch the same failure path. Keep the status log entries
    separate either way.

---

### Step 26: Size mailboxes from the request instead of a fixed default

- Goal: stop reserving fixed multi-gigabyte shared segments per node.
- Problem (review Finding 9):
  - `ASYNC_INPUT_DEFAULT_MAILBOX_WORDS = 4 * 1024 * 1024`
    (`pfio/AsyncInputServer.F90:60`) and
    `worker_segment_words = model_size * (header + mailbox_words)` (`:1618`).
  - With 40 model ranks on a node and 2 workers that is roughly 2.5 GiB of
    shared memory per node regardless of actual slice size.
  - `tests/MAPL3G_Component_Testing_Framework/CMakeLists.txt:71-74` already
    forces `MAPL_ASYNC_INPUT_SHMEM_WORDS=4096` to stay "below restrictive
    cluster shared-memory limits," which is evidence that the default is
    wrong rather than that the tests are unusual.
  - `pfio/pfio.md` documents the variable but not the per-node product.
- Change (preferred):
  - Derive the required mailbox capacity from the largest local slice the model
    ranks will actually request, established during initialization, and
    allocate that.
  - Keep `MAPL_ASYNC_INPUT_SHMEM_WORDS` as an explicit override for cases where
    the bound cannot be derived, and lower the fallback default substantially.
- Change (minimum acceptable, if derivation proves infeasible this step):
  - Lower the default to a value that is reasonable per model rank rather than
    per worker segment, and document the multiplication and the per-node total
    in `pfio/pfio.md`.
  - Remove the CMake override if the new default makes it unnecessary; that
    removal is itself the evidence the default is now sane.
- Files:
  - `pfio/AsyncInputServer.F90`
  - `pfio/pfio.md`
  - `tests/MAPL3G_Component_Testing_Framework/CMakeLists.txt`
- Verify:
  - PFIO cases 01-05 pass without the `MAPL_ASYNC_INPUT_SHMEM_WORDS=4096`
    override, or with a documented reason if the override is still needed.
  - Existing overflow handling still triggers correctly when a slice genuinely
    exceeds the mailbox: keep a case that sets a tiny
    `MAPL_ASYNC_INPUT_SHMEM_WORDS` and confirms the overflow assertion fires
    with its diagnostic rather than corrupting data.
  - Report the computed per-node shared-memory total for the benchmark
    topology before and after, in the status log.

---

### Step 27: Cover the untested payload types and the overflow path

- Goal: satisfy the Step 18/22 verification criteria that were recorded
  complete without tests.
- Problem (review Finding 3):
  - Every test in `pfio/tests/Test_AsyncInputServer.pf` uses `pFIO_REAL32`.
  - `read_global_slab_into_slot` (`pfio/AsyncInputServer.F90:1732-1759`) has
    four type branches; three are covered by nothing.
  - Step 18's "verify controlled overflow and size-mismatch failures" has no
    test. The status log at lines 267-268 acknowledges the difficulty but the
    plan item was still marked complete.
- Change:
  - Generalize the existing `create_real32_fixture` helper, or add siblings, so
    a fixture can be written for INT32, INT64, REAL32, and REAL64.
  - Extend the existing warm-cache round trip to run once per supported type,
    asserting the delivered payload matches the fixture. The
    `CaptureSocket` helper currently hardcodes `real(REAL32)` and needs
    widening to match.
  - Add the overflow and size-mismatch negative coverage if Step 24's work
    makes those paths reportable rather than aborting. If it does not, record
    that explicitly instead of claiming coverage.
- Scope limit:
  - Payload correctness per type only. Do not add new types to
    `read_global_slab_into_slot`; cover what already exists.
- Files:
  - `pfio/tests/Test_AsyncInputServer.pf`
- Verify:
  - `MAPL.pfio.tests` passes with the new per-type cases, and the new cases
    genuinely exercise distinct branches. Confirm by temporarily breaking one
    non-REAL32 branch and observing exactly that case fail.
  - Record the count of pFUnit cases before and after.

---

### Step 28: Resolve server placement in one place

- Goal: remove the duplicated placement rule and the double subclass parse
  introduced by the `local: true` removal.
- Problem (review Finding 2):
  - `is_local_server_configuration` (`mapl/MaplServerUtilities.F90:55`)
    hardcodes `subclass == 'AsyncInputServer'` implies local.
  - Three call sites in `MaplFramework.F90` (`:580`, `:746`, `:912`)
    immediately re-derive `subclass_name` and re-test the same condition, so
    the subclass string is parsed twice per iteration at each site and the rule
    lives in two files.
  - `validate_server_configuration` still accepts an explicit `local: true`
    (`mapl/MaplServerUtilities.F90:48-49`) while `pfio/pfio.md` says the key is
    unnecessary, leaving two permanently valid spellings.
- Change:
  - Introduce one resolver that returns both the subclass name and the resolved
    placement from a single parse, and use it at all three
    `MaplFramework.F90` sites plus `get_ssis_per_server`.
  - Decide and document one policy for an explicit `local: true` on
    `AsyncInputServer`: either keep accepting it as a deprecated no-op with a
    warning, or reject it so there is exactly one valid spelling. State the
    choice in `pfio/pfio.md` and `CHANGELOG.md`.
- Scope limit:
  - Do not change placement semantics for `MpiServer` or `MultiGroupServer`.
    Their explicit `local` handling must stay exactly as it is.
- Files:
  - `mapl/MaplServerUtilities.F90`
  - `mapl/MaplFramework.F90`
  - `mapl/tests/Test_MaplServerUtilities.pf`
  - `pfio/pfio.md`
  - `CHANGELOG.md`
- Verify:
  - `MAPL.mapl.server_utilities` passes, with the existing
    `test_mpi_server_placement_remains_explicit` unchanged, proving no
    collateral change to other subclasses.
  - PFIO cases 01-05 pass.
  - A case using the deprecated `local: true` spelling behaves per the
    documented decision.

---

### Step 29: Make the lookahead prefetch assertion machine-checkable

- Goal: give Step 13 a test that would actually fail if the wrong dataset were
  prefetched.
- Problem (review Finding 7):
  - `preview_bracket` is implemented correctly on both selectors
    (`NonClimDataSetFileSelector.F90:144-251`,
    `ClimDataSetFileSelector.F90:109-176`) and correctly omits the
    `set_last_update` mutation that `update_file_bracket` performs.
  - But Step 13 asked for a case where the old right-node-coincidence heuristic
    would prefetch the wrong dataset. `pfio/case03` has an irregular
    `dt: PT2H30M`, yet its `compare.rc` diffs only
    `extdata_files_read.yaml`.
  - A correct and an incorrect prefetch produce the same file set, so no test
    would fail if `preview_bracket` returned the wrong node.
- Change:
  - Assert on which dataset was prefetched, not on the file list. The
    captain and worker already print cache counters
    (`AsyncInputServer.F90:504-508`, `:581-582`), and
    `output_checks.rc` regexes are already wired into the runner
    (`run_comp_tester.cmake:39-55`). Use that mechanism, as `pfio/case05`
    already does for worker activity.
  - Choose the expected hit/miss counts so that a wrong-node prefetch changes
    them: a correct lookahead yields a warm hit at the next step, an incorrect
    one yields a demand miss.
  - Prefer extending `pfio/case03`, which already has the irregular timestep,
    over adding a sixth case.
- Scope limit:
  - Verification only. Do not change `preview_bracket` behavior; the review
    found it correct.
- Files:
  - `tests/MAPL3G_Component_Testing_Framework/test_cases/pfio/case03/output_checks.rc`
  - possibly that case's `cap2.yaml` or `extdata2.yaml` to make the
    distinction observable
- Verify:
  - The case passes as written, and fails when `append_future_left_to_reader`
    or `append_future_right_to_reader` is temporarily pointed at the wrong
    node. Record that negative confirmation in the status log; without it the
    test proves nothing.

---

### Step 30: Mechanical style pass on `AsyncInputServer.F90`

- Goal: bring the file to the house 3-space standard in one isolated commit.
- Problem (review Minor items):
  - 24 distinct leading-space counts, with 5-, 6-, 7-, 8-, 9-, 10-, and
    11-space bodies mixed inside single procedures, for example `start`
    (`:453-603`).
  - `new_AsyncInputServer` reuses `sleep_string`, `sleep_length`, and
    `sleep_status` (`:270-288`) to parse `MAPL_ASYNC_INPUT_SHMEM_WORDS` and
    `MAPL_ASYNC_INPUT_CACHE_SLOTS`. These are leftovers from the removed
    `MAPL_PERF_READER_SLEEP_SEC` code and are misleading.
  - `MAPL_Sleep(0.0001)` is hard-coded at four sites (`:529`, `:1074`,
    `:1513`, `:1573`) with no named constant.
- Change:
  - Reindent to 3 spaces throughout.
  - Rename the environment-parsing locals to describe what they parse.
  - Introduce one named poll-interval parameter for the sleep duration.
- Scope limit:
  - No semantic change whatsoever. This commit must be reviewable as
    whitespace, renames, and one constant extraction.
- Files:
  - `pfio/AsyncInputServer.F90`
- Verify:
  - `MAPL.pfio.tests`, PFIO cases 01-05, and full `ESSENTIAL` all pass.
  - Confirm semantic neutrality mechanically, not by eye: compare compiler
    output or a whitespace-insensitive diff of the pre- and post-change source.
    A reformat this large is exactly where an accidental logic change hides.

---

### Step 31: Cluster verification, and the decision it forces

- Goal: determine whether this feature delivers its intended benefit, and act
  on the answer.
- Problem (review Findings 1 and 8):
  - `benchmark/README.md` records `AsyncInputServer` at 22.2% slower than
    `MpiServer` (10.92 s versus 8.93 s mean over three runs) while using 8 PETs
    instead of 5. That is 60% more processes for worse wall time.
  - The README correctly attributes this to fast local storage and CPU
    oversubscription, and correctly says a representative conclusion needs the
    target cluster. Plan Step 15 was never executed.
  - `run_local_async_servers` (`mapl/MaplFramework.F90:750-768`) has a real
    branch where a non-model-node reader calls `shutdown()` instead of
    `start()`. No test reaches it. All five PFIO cases use
    `model_petcount: 1` with no remote server section.
- Change: none to the feature. Verification and a decision.
- Verify:
  - Run `prepare_async_perf_cases.sh` and `run_async_perf_cases.sh` on the
    target cluster, on multiple physical nodes, without oversubscription, with
    `reader_capacity_on_node > 0` spanning nodes.
  - Report `MpiServer` versus `AsyncInputServer` wall time at equal model PET
    count, and state the extra PET cost explicitly so the comparison is
    resource-honest rather than wall-time-only.
  - Run a mixed configuration with a local `AsyncInputServer` and a remote
    output server on distinct SSIs, which is the only way to exercise the
    `is_model_node()` false branch.
  - Confirm multi-worker dispatch improves wall time versus the single-worker
    baseline, and that shutdown is clean with no hung ranks.
- Decision point, to be recorded in the status log either way:
  - If the cluster shows a real benefit, record the topology and margin, and
    the feature is justified.
  - If it does not, the honest outcome is to keep `AsyncInputServer` clearly
    experimental and opt-in, not to default any workflow to it. State that in
    `pfio/pfio.md`. Do not let the absence of a result stand in for a positive
    one.

---

### Step 32: Plan and status document hygiene

- Goal: make the remaining work visible.
- Problem (review Minor items):
  - `.opencode/plans/async-input-server-plan.md` still presents Steps 0-15 as
    active. Steps 9-14 are superseded by Steps 16-22, and Step 15 is the only
    live item from that range.
  - `.opencode/plans/async-input-server-status.md` is 2293 lines with all
    pre-Step-16 history inline, making the current state hard to locate.
- Change:
  - Mark Steps 0-14 as superseded implementation history, and mark Step 15 as
    open and tracked by Step 31 here.
  - Move pre-Step-16 status history to an archive file, leaving the active
    redesign and this fix plan's entries in the main status document.
- Files:
  - `.opencode/plans/async-input-server-plan.md`
  - `.opencode/plans/async-input-server-status.md`
  - new archive file for the pre-Step-16 history
- Verify:
  - No content is deleted, only relocated. Confirm with a line-count
    reconciliation across the split.

---

### Build and Status Discipline

Unchanged from `.opencode/plans/async-input-server-plan.md`. Restated because
it applies to every step above.

- After each numbered step, update
  `.opencode/plans/async-input-server-status.md` with completion date and
  state, files changed, design decisions or deviations, exact build and test
  commands, pass/fail counts and log paths, remaining risks, and the next step.
- Load `nag-stack` in the same shell invocation as every configure, build, or
  test command, because module state does not persist between tool calls. The
  non-login tool shell does not define `module`, so use `zsh -lic`.
- Use `build/` as the NAG build directory. Do not configure it with another
  compiler.
- Preserve complete logs in `build/` with `tee`.

```bash
zsh -lic 'module load nag-stack && cmake --build build -j 8 --target build-tests 2>&1 | tee build/stepNN-build-tests.log'
zsh -lic 'module load nag-stack && MAPL_ASYNC_INPUT_SHMEM_WORDS=16 MAPL_ASYNC_INPUT_CACHE_SLOTS=1 ctest --test-dir build -R "^MAPL.pfio.tests$" --output-on-failure --timeout 120 2>&1 | tee build/stepNN-pfio-tests.log'
zsh -lic 'module load nag-stack && ctest --test-dir build -R "^MAPL.mapl.server_utilities$" --output-on-failure --timeout 90 2>&1 | tee build/stepNN-server-utilities.log'
zsh -lic 'module load nag-stack && ctest --test-dir build -R "^MAPL3G_Comp_Test_pfio_case0[1-5]$" --output-on-failure --timeout 180 2>&1 | tee build/stepNN-pfio-components.log'
zsh -lic 'module load nag-stack && ctest --test-dir build -L ESSENTIAL --output-on-failure --timeout 300 2>&1 | tee build/stepNN-ctest-essential.log'
```

Run the focused selections during each step. Run the full `ESSENTIAL` label at
the end of Step 25, at the end of Step 30, and before closing the plan.

### Summary

| Step | Finding | Severity | Needs cluster |
| --- | --- | --- | --- |
| 23 | 4 memory model | High | no |
| 24 | 5 error propagation | High | no |
| 25 | 6 undefined `rc` | Medium | no |
| 26 | 9 mailbox sizing | Medium | no |
| 27 | 3 typed and overflow tests | Medium | no |
| 28 | 2 placement resolution | Medium | no |
| 29 | 7 lookahead assertion | Medium | no |
| 30 | minor style items | Low | no |
| 31 | 1, 8 benefit and mixed config | High | yes |
| 32 | minor doc items | Low | no |

Steps 23-25 are the ones worth doing immediately: they are small, independent
of cluster access, and each is a genuine correctness risk under a compiler
other than the one currently in use. Step 31 is the one that decides whether
the rest of this work is worth maintaining.
