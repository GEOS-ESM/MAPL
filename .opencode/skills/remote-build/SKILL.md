---
name: remote-build
description: Guide for building MAPL on bucy remote server with Intel compiler
compatibility: opencode
---

# Remote Build on Bucy (Intel Compiler)

This skill guides you through building MAPL on the bucy remote server using the Intel compiler. Bucy is typically used for Intel-specific builds and testing.

## Prerequisites

- SSH access to bucy configured
- MAPL repository cloned locally
- Understanding of basic MAPL build process (see `mapl-build` skill)

## Quick Reference

### SSH Connection
```bash
ssh bucy
```

### Standard Intel Build on Bucy
```bash
# On bucy
cd ~/swdev/VS/MAPL  # or your MAPL path

# Load Intel compiler and dependencies
module purge
module load comp/intel-2021.3.0
module load cmake/3.21.0

# Create and enter build directory
mkdir -p intel
cd intel

# Configure
cmake .. \
  -DCMAKE_Fortran_COMPILER=ifort \
  -DCMAKE_C_COMPILER=icc \
  -DCMAKE_CXX_COMPILER=icpc \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBASEDIR=/discover/swdev/gmao_SIteam/BaseLibs/ESMA-Baselibs-7.17.2/x86_64-pc-linux-gnu/ifort_2021.3.0-intelmpi_2021.3.0

# Build
make -j8 install
```

## Configuration Details

### User-Specific Settings

**IMPORTANT:** Bucy configuration is standardized. All developers use the same module names and paths on bucy.

If you have a `~/.opencode/mapl-config.yaml` file with bucy settings, use those. Otherwise, use these standard values:

```yaml
bucy:
  hostname: "bucy"
  modules:
    - "comp/intel-2021.3.0"
    - "cmake/3.21.0"
  compilers:
    fortran: "ifort"
    c: "icc"
    cxx: "icpc"
  basedir: "/discover/swdev/gmao_SIteam/BaseLibs/ESMA-Baselibs-7.17.2/x86_64-pc-linux-gnu/ifort_2021.3.0-intelmpi_2021.3.0"
  mapl_path: "~/swdev/VS/MAPL"  # Update if your path differs
```

### Module System

**CRITICAL:** Always start with `module purge` to avoid conflicts from your default environment.

Standard modules on bucy:
- `comp/intel-2021.3.0` - Intel Fortran/C/C++ compilers
- `cmake/3.21.0` - CMake build system

Check available modules:
```bash
module avail
```

### BaseLibs Path

The BASEDIR must match the Intel compiler version you're using. The standard path is:
```
/discover/swdev/gmao_SIteam/BaseLibs/ESMA-Baselibs-7.17.2/x86_64-pc-linux-gnu/ifort_2021.3.0-intelmpi_2021.3.0
```

**IMPORTANT:** If you're using a different Intel version, adjust the BaseLibs path accordingly.

## Step-by-Step Build Process

### 1. Connect to Bucy

```bash
ssh bucy
```

### 2. Navigate to MAPL Directory

```bash
cd ~/swdev/VS/MAPL  # or wherever your MAPL repo is located
```

### 3. Load Modules

```bash
module purge
module load comp/intel-2021.3.0
module load cmake/3.21.0
```

Verify modules loaded:
```bash
module list
```

### 4. Create Build Directory

```bash
mkdir -p intel
cd intel
```

**CRITICAL:** Use a dedicated build directory for Intel (e.g., `intel`). Never mix compilers in the same build directory.

### 5. Configure with CMake

```bash
cmake .. \
  -DCMAKE_Fortran_COMPILER=ifort \
  -DCMAKE_C_COMPILER=icc \
  -DCMAKE_CXX_COMPILER=icpc \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBASEDIR=/discover/swdev/gmao_SIteam/BaseLibs/ESMA-Baselibs-7.17.2/x86_64-pc-linux-gnu/ifort_2021.3.0-intelmpi_2021.3.0
```

**Common CMake Options:**
- `-DCMAKE_BUILD_TYPE=Debug` - Debug build (default for development)
- `-DCMAKE_BUILD_TYPE=Release` - Optimized build
- `-DCMAKE_INSTALL_PREFIX=<path>` - Custom install location

### 6. Build

```bash
make -j8 install
```

**Build Performance:**
- Intel builds typically take 5-10 minutes
- Adjust `-j8` based on available cores and system load
- Use `-j4` if system is busy, `-j16` if you have priority

### 7. Verify Build

```bash
# Check that libraries were built
ls -lh lib/

# Check that executables were built (if applicable)
ls -lh bin/
```

## Running Tests on Bucy

After building, you can run tests:

```bash
# From the build directory (e.g., intel/)
ctest

# Run tests in parallel
ctest -j4

# Run specific test
ctest -R TestName

# Verbose output for debugging
ctest -V -R TestName
```

See the `mapl-testing` skill for comprehensive testing guidance.

## Common Issues and Solutions

### Issue: "Module not found"

**Symptom:**
```
module: command not found
```

**Solution:**
Ensure you're on bucy and your shell is properly configured for modules:
```bash
source /usr/share/modules/init/bash  # for bash
source /usr/share/modules/init/tcsh  # for tcsh
```

### Issue: "Compiler not found" after loading modules

**Symptom:**
```
CMake Error: CMAKE_Fortran_COMPILER not set
```

**Solution:**
1. Verify modules are loaded: `module list`
2. Check compiler is in PATH: `which ifort`
3. If not found, reload modules:
   ```bash
   module purge
   module load comp/intel-2021.3.0
   ```

### Issue: "Cannot find BaseLibs"

**Symptom:**
```
CMake Error: Could not find ESMF
```

**Solution:**
Verify BASEDIR path exists and matches your Intel version:
```bash
ls /discover/swdev/gmao_SIteam/BaseLibs/ESMA-Baselibs-7.17.2/x86_64-pc-linux-gnu/
```

Look for directory matching your Intel compiler version (e.g., `ifort_2021.3.0-intelmpi_2021.3.0`).

### Issue: Build fails with "out of memory"

**Solution:**
Reduce parallel jobs:
```bash
make -j2 install  # or even -j1 if necessary
```

### Issue: Stale build cache

**Symptom:**
CMake configuration seems wrong or inconsistent.

**Solution:**
Clear CMake cache and rebuild:
```bash
rm -rf CMakeCache.txt CMakeFiles/
cmake .. [options]
make -j8 install
```

## Local vs Remote Development Workflow

### Typical Workflow Pattern

1. **Develop locally** with your preferred compiler (NAG recommended for strict checking)
2. **Test locally** with fast tests
3. **Push to feature branch**
4. **Build on bucy** with Intel to verify portability
5. **Run full test suite** on bucy if needed

### Syncing Code to Bucy

**Option 1: Git (Recommended)**
```bash
# Local: commit and push
git add .
git commit -m "Description"
git push

# On bucy: pull changes
ssh bucy
cd ~/swdev/VS/MAPL
git fetch
git checkout your-feature-branch
git pull
```

**Option 2: rsync**
```bash
# From local machine
rsync -avz --exclude 'build*' --exclude '*.o' \
  ~/swdev/VS/MAPL/ bucy:~/swdev/VS/MAPL/
```

**CRITICAL:** Always prefer git workflow. Only use rsync for temporary testing of uncommitted changes.

## Intel-Specific Compiler Warnings

Intel compiler may produce different warnings than NAG or gfortran. Common Intel-specific issues:

### Uninitialized Variables
Intel is less strict about uninitialized variables. Code that passes Intel may fail NAG.

**Best Practice:** Always develop with NAG first, then verify with Intel.

### Optimization Issues
Intel's aggressive optimization can expose race conditions or undefined behavior.

**Solution:** If Release build fails but Debug works, try:
```bash
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo
```

### OpenMP Issues
Intel has different OpenMP default behaviors.

**Solution:** Explicitly specify OpenMP flags if needed:
```bash
cmake .. -DCMAKE_Fortran_FLAGS="-qopenmp"
```

## Performance Considerations

### Build Performance
- **Parallel jobs:** Intel builds benefit from high parallelism (`-j8` or higher)
- **Network filesystem:** Bucy may use networked home directories; build times depend on I/O
- **Local scratch:** For faster builds, consider building in `/tmp` or local scratch space

### Test Performance
- Intel-compiled code may have different performance characteristics
- Some tests may be faster or slower than with other compilers
- Numerical results should match within tolerance (see `mapl-testing` skill)

## Related Skills

- **mapl-build** - General MAPL building guide (NAG, gfortran, Intel locally)
- **compiler-switching** - How to safely switch between compilers
- **mapl-testing** - Running and debugging tests
- **github-workflow** - Git/GitHub workflow for pushing changes before building on bucy

## Advanced Topics

### Using Different Intel Versions

To use a different Intel compiler version:

1. Check available versions:
   ```bash
   module avail comp/intel
   ```

2. Load desired version:
   ```bash
   module load comp/intel-<version>
   ```

3. **CRITICAL:** Update BASEDIR to match the Intel version in the BaseLibs path.

### Building in Custom Location

```bash
cmake .. \
  -DCMAKE_INSTALL_PREFIX=/discover/nobackup/$USER/MAPL-intel \
  [other options]

make -j8 install
```

This installs to a custom location instead of the default `install/` directory.

### Debugging Build Issues

Enable verbose build output:
```bash
make VERBOSE=1 -j8 install
```

This shows full compiler commands, useful for diagnosing configuration issues.

---

**When to use this skill:**
- Building MAPL with Intel compiler
- Verifying compiler portability before PR
- Debugging Intel-specific issues
- Running tests on bucy infrastructure
