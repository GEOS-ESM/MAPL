# AsyncInputServer Benchmark

This benchmark compares the default `MpiServer` input path with the local
`AsyncInputServer` path. Both cases process the same 512x384 input files for 16
15-minute timesteps.

The fixed single-node topology is:

- `MpiServer`: 5 model PETs, 5 PETs total.
- `AsyncInputServer`: 5 model PETs, 1 reader captain, and 2 reader workers,
  8 PETs total.

Run the benchmark from the MAPL source directory with a configured build:

```bash
bash tests/MAPL3G_Component_Testing_Framework/benchmark/prepare_async_perf_cases.sh \
  /tmp/mapl-async-benchmark

bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh \
  /tmp/mapl-async-benchmark build
```

The runner defaults `MAPL_ASYNC_INPUT_CACHE_SLOTS` to four and prints wall
time, the ExtData profile, and the async captain and worker cache summaries.

## Local Real-I/O Result

The benchmark was run three times on macOS with the NAG Debug build on
2026-09-28. No artificial model delay, reader delay, or dry-read mode was
enabled.

| Run | MpiServer (5 PETs) | AsyncInputServer (8 PETs) |
| --- | ---: | ---: |
| 1 | 8.99 s | 10.77 s |
| 2 | 9.00 s | 10.83 s |
| 3 | 8.80 s | 11.15 s |
| Mean | 8.93 s | 10.92 s |

For this local real-I/O workload, `AsyncInputServer` was approximately 22.2%
slower in total wall time. Fast local storage leaves little read latency to
hide, while the async path still pays for three additional MPI processes,
captain scheduling, request metadata, and shared-memory mailbox coordination.

The async diagnostics were identical in all three runs:

```text
AsyncInputServer captain cache: warm_hits=8 prefetch_hits=120
AsyncInputServer cache: reader_rank=6 slots=4 hits=64 misses=16 demand_misses=1 prefetch_misses=15 requests=80
AsyncInputServer cache: reader_rank=7 slots=4 hits=64 misses=16 demand_misses=1 prefetch_misses=15 requests=80
```

Both workers handled the same number of requests. Each worker incurred one
cold demand miss and then loaded the 15 subsequent datasets through the
prefetch path. This confirms that the redesigned captain/worker topology,
multi-worker scheduling, cache, shared-memory result path, and shutdown all
operated during the benchmark.

This laptop result should not be treated as the expected cluster speedup. The
intended benefit is hiding slower storage reads behind model computation. A
representative performance conclusion therefore requires running the same
scripts on the target cluster with multiple physical nodes and without CPU
oversubscription.
