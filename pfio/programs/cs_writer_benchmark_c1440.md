## Parallel write benchmark: ADIOS2 BP5 vs NetCDF-4, C1440 L181 (801.7 GB)

![BP5 vs NetCDF-4 write benchmark](cs_writer_benchmark_c1440.png)

**Setup.** C1440 cubed-sphere grid, 181 levels, 89 float32 3-D variables (9.0 GB each,
801.7 GB total), one time step. 864 MPI ranks on 8 Milan nodes (NCCS Discover, GPFS),
Intel 2021.13 + Intel MPI, ESMA Baselibs 8.34.1 (ADIOS2 2.12.1, NetCDF-4 with
HDF5 MPI-IO). Each rank writes a 120 x 120 x 181 block of every variable.
Programs: `adios2_parallel_cs_writer.x` (BP5, sync puts, default engine
parameters) and `nc4_parallel_cs_writer.x` (contiguous storage, collective
`nf90_put_var`, no fill).

| | ADIOS2 BP5 | NetCDF-4 | BP5 / NC4 |
|---|---:|---:|---:|
| setup (init/create/define/open) | 7.5 s | 1.3 s | |
| data write | 89.1 s (put 2.7 + end_step 86.4) | 137.0 s | **1.54x faster** |
| close | 0.02 s | 0.41 s | |
| end-to-end | 94.8 s | 138.7 s | **1.46x faster** |
| data-write bandwidth | 9.00 GB/s | 5.85 GB/s | |
| end-to-end bandwidth | 8.46 GB/s | 5.78 GB/s | |
| max memory per node | 194 GB | 13.5 GB | 14x |
| output | directory, 747 GiB | single file, 801,687,045,988 B | |

**Observations.**
- BP5 is 1.54x faster on data write and 1.46x faster end to end. Its `put` calls only
  copy into a buffer (about 0.03 s per variable); all disk I/O happens in one
  `EndStep` at 9.0 GB/s.
- NetCDF-4 writes each variable in a separate collective call. Its steady state is about
  7.1 GB/s (median 1.27 s per variable), close to BP5. Two things lower its average to
  5.85 GB/s:
  - the first 4 variables are slower (3.1–5.3 GB/s);
  - two calls stalled for about 10 s each (var0028, var0077), costing about 18 s
    compared with the median.
- BP5 opens much more slowly (7.5 s versus 1.3 s), which matters for many small files.
- BP5 needs much more memory: it holds the whole 800 GB step in memory, 194 GB on the
  busiest node. To stay inside node memory, write larger outputs as several steps or
  use more ranks.

**Caveats.** Each library ran once, in separate jobs about 3 minutes apart on a shared
GPFS file system, so the comparison is affected by other jobs using it at the same
time; repeat it to get error bars. BP5 used the default aggregation (no `NumAggregators`
tuning), and NetCDF-4 used contiguous layout with ROMIO collective buffering enabled.
