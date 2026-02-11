# MAPL Build and Test Instructions

## Environment Setup

### Local macOS - NAG Fortran (PRIMARY)
```
module load nag mpi baselibs
cmake -B nag -DCMAKE_BUILD_TYPE=Debug
cmake --build nag
```

### Local macOS - GNU Fortran
```
module load gfortran mpi baselibs
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug
cmake --build gfortran
```

### Remote Linux (bucy) - Intel Fortran
NOTE: Building on bucy requires SSH with PIV card authentication (badge + PIN)

**IMPORTANT**: Commands must load modules in the same shell session. Use:
```
ssh bucy "cd ~/swdev/VS/MAPL && source /etc/profile.d/modules.sh && module load ifx-stack && cmake -B intel -DCMAKE_BUILD_TYPE=Debug"
ssh bucy "cd ~/swdev/VS/MAPL && source /etc/profile.d/modules.sh && module load ifx-stack && cmake --build intel -j 6"
ssh bucy "cd ~/swdev/VS/MAPL/intel && source /etc/profile.d/modules.sh && module load ifx-stack && ctest --output-on-failure"
```

## Build Directory Convention
Use separate build directories per compiler: ./nag, ./gfortran, ./intel
Never mix compilers in the same build directory

## Generic3g Development Workflow
Problem: Running ctest is too slow
Running ctest at the top level rebuilds everything. For generic3g work, use this faster workflow:

### Fast Build and Test (generic3g only)
```
cd generic3g/tests
make
export DYLD_LIBRARY_PATH=$BUILD/generic3g/tests/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests
```
**IMPORTANT**: The DYLD_LIBRARY_PATH must include $BUILD/generic3g/tests/gridcomps or tests will fail to load libraries. This path also contains NAG compiler license information.

## Test Debugging Options
When running ./MAPL.generic3g.tests, use these flags AFTER the executable name:
### -d flag: Diagnostic output
```
mpirun -np 1 ./MAPL.generic3g.tests -d
```
Prints diagnostics when starting/ending each test. Essential when a test crashes and the framework cannot report which test failed.

### -f pattern flag: Filter tests
```
mpirun -np 1 ./MAPL.generic3g.tests -f ComponentDriver
```
Runs only tests matching pattern.
**IMPORTANT**: This is simple substring matching, NOT regex or glob patterns.
Correct: -f GridComp
Wrong: -f Grid* (wildcards do not work)
Wrong: -f .Grid. (regex does not work)
Combine flags
```
mpirun -np 1 ./MAPL.generic3g.tests -d -f GridComp
```
## Platform-Specific Tests
Some tests only run on specific platforms:
### Test_MemInfoWrite (Linux only)
This test requires `/proc/self/status` which only exists on Linux. The test is automatically excluded on macOS and other non-Linux platforms via CMake configuration in `utilities/tests/CMakeLists.txt`.

## Common Issues
### CMake fails because it could not find a compiler
Forgot to use `module load ...`
### Tests fail to run with library load errors
Forgot to set DYLD_LIBRARY_PATH. Always include $BUILD/generic3g/tests/gridcomps
### NAG license error
Check that DYLD_LIBRARY_PATH includes the gridcomps directory (contains license info)
### Test crashes but framework does not report which one
Use -d flag to see diagnostic output showing which test started but did not finish
### Want to run just one test but pattern matching is not working
Do not use wildcards or regex. Just use plain substring: -f MyTestName
### Full ctest is too slow during generic3g development
Use the fast workflow: `cd $BUILD/generic3g/tests && make && mpirun -np 1 ./MAPL.generic3g.tests`

## Switching Compilers

When switching between compilers:
1. Use a different build directory (do not reuse)
2. Load the correct modules BEFORE running cmake
3. Verify with module list before building

## Remote Building on bucy

### SSH ControlMaster Setup (Avoid Repeated PIN Entry)
Add to `~/.ssh/config`:
```
Host bucy bucy.gsfc.nasa.gov
    ControlMaster auto
    ControlPath ~/.ssh/cm-%r@%h:%p
    ControlPersist 8h
```

Establish master connection once (requires PIN):
```
ssh -fN bucy
```
All subsequent ssh commands will reuse this connection for 8 hours without asking for PIN.

### Syncing Code to bucy
Sync uncommitted changes from macOS to bucy:
```
rsync -avz --exclude='.git' --exclude='build' --exclude='nag' --exclude='gfortran' --exclude='intel' --exclude='.venv' \
  ~/swdev/VS/MAPL/ bucy:~/swdev/VS/MAPL/
```

**CRITICAL**: Ensure you're on the same git branch on both systems before syncing to avoid corruption!
Check branches:
```
git branch --show-current  # on macOS
ssh bucy "cd ~/swdev/VS/MAPL && git branch --show-current"  # on bucy
```

### Limitations
* SSH requires physical badge in card reader plus PIN entry (but ControlMaster reduces to once per 8 hours)
* Manual branch synchronization needed when using rsync
* Consider committing and pushing to GitHub for safer synchronization if working on different branches
