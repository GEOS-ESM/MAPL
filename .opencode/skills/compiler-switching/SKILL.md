---
name: compiler-switching
description: Safely switch between NAG, gfortran, and Intel compilers when building MAPL
compatibility: opencode
---

# Compiler Switching

This skill guides you through safely switching between different Fortran compilers (NAG, gfortran, Intel) when building MAPL. Proper compiler switching prevents build errors, runtime failures, and mysterious bugs.

## Quick Reference

### The Golden Rule

**CRITICAL:** Never mix compilers in the same build directory.

### Safe Compiler Switch Workflow

```bash
# 1. Use separate build directory for each compiler
mkdir -p nag gfortran intel

# 2. Build with NAG
cd nag
module load nag  # or your NAG module
cmake .. -DCMAKE_Fortran_COMPILER=nagfor ...
make -j8 install

# 3. Switch to gfortran
cd ../gfortran
module purge
module load gfortran  # or your gfortran module
cmake .. -DCMAKE_Fortran_COMPILER=gfortran ...
make -j8 install

# 4. Switch to Intel
cd ../intel
module purge
module load intel  # or your Intel module
cmake .. -DCMAKE_Fortran_COMPILER=ifort ...
make -j8 install
```

## Why Separate Build Directories?

### Binary Incompatibility

Different compilers produce incompatible:
- **Object files** (`.o`) - Cannot be mixed
- **Module files** (`.mod`) - Format is compiler-specific
- **Libraries** (`.a`, `.so`) - ABI differences
- **Name mangling** - Different symbol naming conventions

### Example Problem

**BAD - Will fail:**
```bash
mkdir build
cd build

# Build with NAG
cmake .. -DCMAKE_Fortran_COMPILER=nagfor
make -j8 install

# Switch to gfortran WITHOUT cleaning
cmake .. -DCMAKE_Fortran_COMPILER=gfortran
make -j8 install  # WILL FAIL with mysterious errors
```

**GOOD - Will succeed:**
```bash
# NAG build
mkdir nag
cd nag
cmake .. -DCMAKE_Fortran_COMPILER=nagfor
make -j8 install

# gfortran build
cd ..
mkdir gfortran
cd gfortran
cmake .. -DCMAKE_Fortran_COMPILER=gfortran
make -j8 install
```

## User Configuration

### Using mapl-config.yaml

If you have `~/.opencode/mapl-config.yaml`, it should define compiler settings for each compiler:

```yaml
compilers:
  nag:
    module: "nag"  # or "compiler/nag-fortran" - your system's module name
    compiler: "nagfor"
    build_type: "Debug"
    basedir: "${BASEDIR}/nag/Darwin"
    
  gfortran:
    module: "gfortran"  # or "gcc/12" - your system's module name
    compiler: "gfortran"
    build_type: "Debug"
    basedir: "${BASEDIR}/gfortran/Darwin"
    
  intel:
    module: "intel"  # or "comp/intel-2021.3.0" - your system's module name
    compiler: "ifort"
    build_type: "Debug"
    basedir: "${BASEDIR}/ifort_18.0.3.185/Darwin"
```

**IMPORTANT:** Module names vary by system. Check your available modules with `module avail`.

### Environment Variables

Some users prefer environment variables over config files:

```bash
# In ~/.bashrc or ~/.zshrc
export BASEDIR=/Users/tclune/swdev/VS/baselibs/install
export NAG_BASEDIR=$BASEDIR/nag/Darwin
export GFORTRAN_BASEDIR=$BASEDIR/gfortran/Darwin
export INTEL_BASEDIR=$BASEDIR/ifort_18.0.3.185/Darwin
```

## Compiler-Specific Configurations

### NAG Compiler

**Recommended for development** - strictest checking, catches most bugs.

```bash
cd nag
module load nag  # or your NAG module name

cmake .. \
  -DCMAKE_Fortran_COMPILER=nagfor \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBASEDIR=/path/to/baselibs/nag/Darwin

make -j8 install
```

**Build time:** ~81 seconds (fastest)

**Advantages:**
- Strictest standards conformance
- Best runtime error checking
- Fastest compilation
- Excellent error messages

**Disadvantages:**
- Not widely used in production
- May reject code that other compilers accept

### gfortran Compiler

**Good for portability** - widely available, free, open source.

```bash
cd gfortran
module purge
module load gfortran  # or your gfortran module name

cmake .. \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBASEDIR=/path/to/baselibs/gfortran/Darwin

make -j8 install
```

**Build time:** ~6m43s (slowest)

**Advantages:**
- Freely available on all platforms
- Good standards conformance
- Active development
- Good error checking with `-fcheck=all`

**Disadvantages:**
- Slower compilation than NAG
- Runtime checks slower than NAG
- Error messages sometimes cryptic

### Intel Compiler

**Production compiler** - often used in HPC environments.

```bash
cd intel
module purge
module load intel  # or your Intel module name

cmake .. \
  -DCMAKE_Fortran_COMPILER=ifort \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBASEDIR=/path/to/baselibs/ifort_*/Darwin

make -j8 install
```

**Build time:** ~5-10 minutes (medium)

**Advantages:**
- Excellent optimization
- Good performance
- Widely used in HPC
- Good OpenMP support

**Disadvantages:**
- Proprietary (requires license)
- Less strict checking than NAG
- May hide bugs that NAG would catch

## Module System Management

### Checking Current Modules

```bash
# List loaded modules
module list

# Show module details
module show nag
```

### Loading Modules Safely

**IMPORTANT:** Always `module purge` before loading new compiler modules to avoid conflicts.

```bash
# Purge existing modules
module purge

# Load desired compiler
module load nag  # or gfortran, intel, etc.

# Verify correct compiler is loaded
which nagfor  # or gfortran, ifort
nagfor --version  # verify version
```

### Module Conflicts

**Symptom:**
```
module load gfortran
ERROR: Module conflict: compiler/nag already loaded
```

**Solution:**
```bash
module purge
module load gfortran
```

### Module Not Available

**Symptom:**
```
module load nag
ERROR: Module 'nag' not found
```

**Solution:**
Check available modules:
```bash
module avail
module avail compiler
module avail nag
```

Module may be named differently (e.g., `compiler/nag-fortran`, `nag/7.0`, etc.).

## BaseLibs Path Management

### Why BaseLibs Path Matters

MAPL depends on external libraries (ESMF, NetCDF, etc.) that were compiled with a specific compiler. **You must use BaseLibs compiled with the same compiler you're using to build MAPL.**

### BaseLibs Directory Structure

Typical BaseLibs organization:
```
baselibs/
├── install/
│   ├── nag/
│   │   └── Darwin/
│   │       ├── include/
│   │       └── lib/
│   ├── gfortran/
│   │   └── Darwin/
│   │       ├── include/
│   │       └── lib/
│   └── ifort_18.0.3.185/
│       └── Darwin/
│           ├── include/
│           └── lib/
```

### Matching BaseLibs to Compiler

**CRITICAL:** The BASEDIR must match your compiler:

| Compiler | BASEDIR Example |
|----------|-----------------|
| NAG      | `${BASEDIR}/nag/Darwin` |
| gfortran | `${BASEDIR}/gfortran/Darwin` |
| Intel    | `${BASEDIR}/ifort_18.0.3.185/Darwin` |

### Verifying BaseLibs Compiler

Check which compiler was used for BaseLibs:

```bash
# Look for compiler artifacts
ls ${BASEDIR}/nag/Darwin/lib/*.a
ldd ${BASEDIR}/nag/Darwin/lib/libesmf.so  # Linux
otool -L ${BASEDIR}/nag/Darwin/lib/libesmf.dylib  # macOS
```

## Common Compiler Switching Issues

### Issue: "Undefined reference" errors

**Symptom:**
```
undefined reference to `__module_MOD_function'
```

**Cause:** Mixed compiler object files.

**Solution:**
1. Delete build directory
2. Recreate with correct compiler
3. Rebuild from scratch

```bash
rm -rf nag  # or whichever build directory
mkdir nag
cd nag
module load nag
cmake .. -DCMAKE_Fortran_COMPILER=nagfor ...
make -j8 install
```

### Issue: ".mod file version mismatch"

**Symptom:**
```
Fatal Error: File 'some_module.mod' opened at (1) is not a GNU Fortran module file
```

**Cause:** Trying to use .mod file from different compiler.

**Solution:**
Clean build and rebuild:
```bash
make clean  # or rm -rf CMakeFiles/ CMakeCache.txt
cmake .. [correct compiler options]
make -j8 install
```

### Issue: CMake finds wrong compiler

**Symptom:**
CMake uses different compiler than specified.

**Solution:**
1. Delete CMake cache:
   ```bash
   rm -rf CMakeCache.txt CMakeFiles/
   ```

2. Ensure correct compiler is first in PATH:
   ```bash
   module purge
   module load nag  # or desired compiler
   which nagfor  # verify
   ```

3. Reconfigure:
   ```bash
   cmake .. -DCMAKE_Fortran_COMPILER=nagfor ...
   ```

### Issue: Runtime library not found

**Symptom (macOS):**
```
dyld: Library not loaded: @rpath/libnag_nag.dylib
```

**Symptom (Linux):**
```
error while loading shared libraries: libnagfor.so
```

**Solution:**
Set library path for your compiler:

**NAG (macOS):**
```bash
export DYLD_LIBRARY_PATH=/usr/local/nag/lib:$DYLD_LIBRARY_PATH
```

**NAG (Linux):**
```bash
export LD_LIBRARY_PATH=/usr/local/nag/lib:$LD_LIBRARY_PATH
```

**gfortran (macOS):**
```bash
export DYLD_LIBRARY_PATH=/usr/local/gfortran/lib:$DYLD_LIBRARY_PATH
```

**Note:** For Intel, this is usually handled by the module system.

### Issue: Tests pass with one compiler, fail with another

**This is expected behavior!** Different compilers have different:
- Floating-point precision handling
- Optimization strategies
- Runtime checking
- Default initialization of variables

**What to do:**
1. **If NAG passes, others fail:** Other compilers may be exposing bugs (uninitialized variables, etc.)
2. **If NAG fails, others pass:** NAG is being more strict - fix to satisfy NAG
3. **Numerical differences:** Check if within acceptable tolerance

**Best practice:** Always develop with NAG first, then verify with other compilers.

## Recommended Workflow

### Development Workflow

1. **Primary development:** Use NAG for strictest checking
   ```bash
   cd nag
   # edit code
   make -j8 install
   ctest
   ```

2. **Before committing:** Verify with gfortran (most common free compiler)
   ```bash
   cd ../gfortran
   make -j8 install
   ctest
   ```

3. **Before PR:** Verify with Intel (if targeting HPC environments)
   ```bash
   # On bucy or local if Intel available
   cd ../intel
   make -j8 install
   ctest
   ```

### Quick Testing Workflow

For rapid development iterations:

```bash
# Fast build and test with NAG only
cd nag
make -j8 install
cd generic3g/MAPL_cfio/MAPL_cfio_r4.tests
export DYLD_LIBRARY_PATH=/usr/local/nag/lib:$DYLD_LIBRARY_PATH
./generic3g.x
```

Only switch to other compilers when:
- Preparing for PR
- Debugging compiler-specific issues
- Verifying portability

## Performance Comparison

### Compilation Speed

1. **NAG:** Fastest (~81s for full build)
2. **Intel:** Medium (~5-10 min)
3. **gfortran:** Slowest (~6m43s)

### Runtime Performance

1. **Intel (Release):** Fastest (best optimization)
2. **gfortran (Release):** Medium
3. **NAG (Release):** Varies by code

**Note:** Debug builds are always slower due to runtime checking.

### Error Checking Thoroughness

1. **NAG:** Most thorough
2. **gfortran with -fcheck=all:** Good
3. **Intel:** Less thorough by default

## Advanced Topics

### Using Compiler Wrappers

Some systems use MPI compiler wrappers:

```bash
# Instead of direct compilers
cmake .. \
  -DCMAKE_Fortran_COMPILER=mpif90 \
  -DCMAKE_C_COMPILER=mpicc
```

**IMPORTANT:** Ensure wrapper uses correct underlying compiler:
```bash
mpif90 --version  # check underlying compiler
```

### Custom Compiler Flags

Different compilers support different flags:

**NAG:**
```bash
cmake .. \
  -DCMAKE_Fortran_COMPILER=nagfor \
  -DCMAKE_Fortran_FLAGS="-g -C=all -gline"
```

**gfortran:**
```bash
cmake .. \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_Fortran_FLAGS="-g -fbacktrace -fcheck=all"
```

**Intel:**
```bash
cmake .. \
  -DCMAKE_Fortran_COMPILER=ifort \
  -DCMAKE_Fortran_FLAGS="-g -traceback -check all"
```

### Cross-Compiler Comparison

To compare results across compilers:

```bash
# Build and run test with each compiler
cd nag
make -j8 install
ctest -R generic3g > ../nag-results.txt

cd ../gfortran
make -j8 install
ctest -R generic3g > ../gfortran-results.txt

cd ../intel
make -j8 install
ctest -R generic3g > ../intel-results.txt

# Compare results
cd ..
diff nag-results.txt gfortran-results.txt
diff nag-results.txt intel-results.txt
```

### Parallel Builds with Multiple Compilers

Build all compilers simultaneously (if you have resources):

```bash
# Terminal 1
cd nag
make -j4 install

# Terminal 2
cd gfortran
make -j4 install

# Terminal 3
cd intel
make -j4 install
```

**Caution:** This uses significant CPU and memory. Use lower `-j` values.

## Troubleshooting Checklist

When switching compilers, verify:

- [ ] `module purge` executed before loading new compiler module
- [ ] Correct compiler module loaded (`module list`)
- [ ] Compiler executable is correct (`which nagfor`, etc.)
- [ ] Using separate build directory for this compiler
- [ ] BASEDIR matches compiler being used
- [ ] CMake cache cleared if reusing build directory (`rm CMakeCache.txt`)
- [ ] Library paths set correctly for runtime (`DYLD_LIBRARY_PATH` or `LD_LIBRARY_PATH`)

## Related Skills

- **mapl-build** - General building guide covering all compilers
- **mapl-setup** - Initial environment setup for compilers
- **remote-build** - Building with Intel on bucy
- **pfunit-troubleshooting** - Debugging compiler-specific test failures
- **mapl-testing** - Running tests with different compilers

## Quick Reference Commands

```bash
# Check current compiler
module list
which nagfor  # or gfortran, ifort

# Switch to NAG
module purge
module load nag
cd nag
cmake .. -DCMAKE_Fortran_COMPILER=nagfor -DBASEDIR=$NAG_BASEDIR
make -j8 install

# Switch to gfortran
module purge
module load gfortran
cd ../gfortran
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DBASEDIR=$GFORTRAN_BASEDIR
make -j8 install

# Switch to Intel
module purge
module load intel
cd ../intel
cmake .. -DCMAKE_Fortran_COMPILER=ifort -DBASEDIR=$INTEL_BASEDIR
make -j8 install
```

---

**When to use this skill:**
- Switching between compilers during development
- Debugging compiler-specific issues
- Verifying code portability
- Preparing code for different deployment environments
- Troubleshooting build or runtime errors after compiler change
