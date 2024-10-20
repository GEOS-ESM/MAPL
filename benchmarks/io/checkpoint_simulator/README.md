This benchmark simulates writing a series of 3D variables of a given cubed-sphere resolution to a file using the same strategies as used by the real checkpoint code in MAPL

The code has the following options and needs an ESMF rc file named checkpoint\_benchmark.rc

- "NX:" the x distribution for each face
- "NY:" the y distribution for each face
- "IM\_WORLD:" the cube resolution
- "LM:" the number of levels
- "NUM\_WRITERS:" the number of writing processes either to a single or independent files
- "NUM\_ARRAYS:" the number of 3D variables to write to the file
- "CHUNK:" whether to chunk, default true
- "GATHER\_3D:" gather all levels at once (default is false which means a level at a time is gathered)
- "SPLIT\_FILE:" default false, if true, each writer writes to and independent file
- "WRITE\_BARRIER:" default false, add a barrier before each write to for synchronization
- "DO\_WRITES:" default true, if false skips writing (so just an mpi test at that point)
- "NTRIALS:" default 1, the number of trials to make writing
- "RANDOM\_DATA:" default true, if true will arrays with random data, if false sets the array to the rank of the process

Note that whatever you set NX and NY to the program must be run on `6*NX*NY` processors and the number of writers must evenly divide `6*NY`
