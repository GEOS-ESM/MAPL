This benchmark simulates writing a series of 3D variables of a given cubed-sphere resolution to a file using the same strategies as used by the real checkpoint code in MAPL

The code has the following command line options:
```
 optional arguments:
  -h, --help                 This message.
  --config_file              The configuration file to use
  --nx                       The number of cells in the x direction (default: 4)
  --ny                       The number of cells in the y direction (default: 4)
  --im_world                 The resolution of the cubed sphere (default: 90)
  --lm                       The number of levels in each 3D variable (default: 137)
  --num_writers              The number of processes that will write (default: 1)
  --num_arrays               The number of 3D arrays to write (default: 5)
  --ntrials                  The number of trials to run (default: 3)
  --split_file               Split the file into multiple files (default: do not split)
  --gather_3d                Gather all levels at once instead of one at a time (default: gather one at a time)
  --write_barrier            Add a barrier after every write (default: no barrier)
  --static_data              Use static data (rank of process) instead of random data (default: random data)
  --suppress_writes          Do not write data (default: write data)
  --write_binary             Write binary data instead of NetCDF (default: write NetCDF)
  --no_chunking              Do not chunk output (default: chunk the output)
```

NOTE 1: If you specify a `config_file` it must be an ESMF Config file with the following options:

- "NX:" the x distribution for each face
- "NY:" the y distribution for each face
- "IM\_WORLD:" the cube resolution
- "LM:" the number of levels
- "NUM\_WRITERS:" the number of writing processes either to a single or independent files
- "NUM\_ARRAYS:" the number of 3D variables to write to the file
- "CHUNK:" whether to chunk, default `.true.`
- "GATHER\_3D:" gather all levels at once (default is `.false.` which means a level at a time is gathered)
- "SPLIT\_FILE:" default `.false.`, if `.true.`, each writer writes to and independent file
- "WRITE\_BARRIER:" default `.false.`, add a barrier before each write to for synchronization
- "DO\_WRITES:" default `.true.`, if `.false.` skips writing (so just an mpi test at that point)
- "NTRIALS:" default 3, the number of trials to make writing
- "RANDOM\_DATA:" default `.true.`, if `.true.` will arrays with random data, if `.false.` sets the array to the rank of the process

NOTE 2: that whatever you set NX and NY to the program must be run on `6*NX*NY` processors and the number of writers must evenly divide `6*NY`
