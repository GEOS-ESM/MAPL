---
name: mapl-build
description: Build MAPL with NAG, gfortran, or Intel compilers on macOS and Linux
compatibility: opencode
---

## What I Do

Provide step-by-step instructions for building MAPL with different compilers on different platforms, including:
- macOS with NAG Fortran (primary/fastest)
- macOS with gfortran (portable)
- Remote Linux (bucy) with Intel compiler
- Build directory conventions and best practices
- Troubleshooting common build failures

## When to Use Me

Use this skill when:
- Building MAPL for the first time
- Switching between compilers
- Build failures or configuration issues
- Setting up continuous integration
- Need to verify builds across multiple compilers

## Branch Selection

**IMPORTANT: First question to answer - Which branch should we start with?**

- **`develop`** - For legacy MAPL (MAPL 2.x) work
- **`release/MAPL-v3`** - For MAPL v3 development

Your actual work will be on a feature/bugfix/hotfix branch, but these are the base branches.

## Build Directory Convention

**Golden Rule: Use separate build directories for each compiler. Never mix compilers!**

### Standard Directory Names

```bash
./nag          # For NAG Fortran builds
./gfortran     # For gfortran builds  
./intel        # For Intel compiler builds (on bucy)
```

Alternative pattern (if you prefer):
```bash
./build-nag
./build-gfortran
./build-intel
```

Configure your preference in `~/.opencode/mapl-config.yaml`:
```yaml
paths:
  build_dir_prefix: ""  # For ./nag pattern
  # OR
  build_dir_prefix: "build-"  # For ./build-nag pattern
```

## macOS Build - NAG Fortran (Primary)

NAG is the **fastest** compiler for MAPL (~81 seconds from scratch) and provides strictest checking.

### Load Modules

Check `~/.opencode/mapl-config.yaml` for your module names, or use common defaults:

```bash
module load nag mpi baselibs
```

Common variations you might need:
```bash
module load compiler/nag-fortran openmpi MAPL-baselibs
# Or
module load nag-fortran/7.1 mpi/openmpi baselibs
```

### Verify Modules

```bash
module list
which nagfor
```

### Configure and Build
**IMPORTANT:** Always log build output to track progress and diagnose issues:

```bash
module load nag mpi baselibs && cmake -B nag -DCMAKE_BUILD_TYPE=Debug 2>&1 | tee nag/cmake-config.log
module load nag mpi baselibs && cmake --build nag -j 8 2>&1 | tee nag/build.log
```
### Testing

**IMPORTANT:** Tests are no longer built automatically with the main build. You must explicitly build them first.

```bash
# Build all tests
module load nag mpi baselibs && cmake --build nag -j 8 --target build-tests 2>&1 | tee nag/build-tests.log

# Run all tests (from source directory)
module load nag mpi baselibs && ctest --test-dir nag --output-on-failure 2>&1 | tee nag/ctest.log

# Build and run a specific test target
module load nag mpi baselibs && cmake --build nag -j 8 --target <target> 2>&1 | tee nag/build-target.log
module load nag mpi baselibs && ctest --test-dir nag -R <test-name-pattern> --output-on-failure
```

**AI Agents:** When building MAPL, ALWAYS use `tee` to create logs in the build directory.

### Build Times (M2 Max, -j 8)

- **Configuration:** ~11 seconds
- **Full build:** ~81 seconds (1m 21s)
- **Incremental builds:** < 3 minutes (after code changes)

### Recommended Timeout for Automation

120 seconds (2 minutes) - provides 25% padding over measured time

## macOS Build - gfortran

gfortran is **slower** (~6m 43s from scratch) but more portable and widely available.

### Load Modules

```bash
module load gfortran mpi baselibs
```

Common variations:
```bash
module load gcc/13 openmpi MAPL-baselibs
# Or
module load gnu-fortran mpi/openmpi baselibs
```

### Verify Modules

```bash
module list
which gfortran
gfortran --version  # Should be 10.x or newer
```

### Configure and Build

```bash
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug
cmake --build gfortran
```

Parallel build (recommended):
```bash
cmake --build gfortran -j 8
```

### Build Logging (Recommended)

**IMPORTANT:** Always log build output to track progress and diagnose issues:

```bash
# Log configuration
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug 2>&1 | tee gfortran/cmake-config.log

# Log build
cmake --build gfortran -j 8 2>&1 | tee gfortran/build.log
```

**Why log:**
- `tail` can hide progress issues from user
- Full log allows reviewing warnings and errors
- Logs persist in build directory for later review
- Standard locations: `gfortran/cmake-config.log` and `gfortran/build.log`

**AI Agents:** When building MAPL, ALWAYS use `tee` to create logs in the build directory.

### Build Times (M2 Max, -j 8)

- **Configuration:** ~15 seconds
- **Full build:** ~403 seconds (6m 43s)
- **Incremental builds:** < 3 minutes

### Recommended Timeout for Automation

540 seconds (9 minutes) - provides 25% padding

## Remote Linux Build - Intel Compiler (bucy)

For Intel compiler testing, use the remote bucy server. This requires PIV card authentication.

**See the `remote-build` skill for complete bucy workflow**, including:
- SSH ControlMaster setup
- Code synchronization with rsync
- Building and testing on bucy

Quick reference for bucy build:
```bash
ssh bucy "cd ~/swdev/VS/MAPL && source /etc/profile.d/modules.sh && module load ifx-stack && cmake -B intel -DCMAKE_BUILD_TYPE=Debug"
ssh bucy "cd ~/swdev/VS/MAPL && source /etc/profile.d/modules.sh && module load ifx-stack && cmake --build intel -j 6"
```

**IMPORTANT:** Commands must load modules in the same shell session (see `remote-build` skill for details).

## CMake Build Types

### Debug (Recommended for Development)
```bash
cmake -B <compiler> -DCMAKE_BUILD_TYPE=Debug
```
- Full debugging symbols
- No optimization
- Assertions enabled
- Best for development and debugging

### Release
```bash
cmake -B <compiler> -DCMAKE_BUILD_TYPE=Release
```
- Full optimization
- No debugging symbols
- Faster execution
- Use for production runs

### RelWithDebInfo
```bash
cmake -B <compiler> -DCMAKE_BUILD_TYPE=RelWithDebInfo
```
- Optimization enabled
- Debugging symbols included
- Good for performance testing with debugging capability

## Parallel Builds

Use `-j` flag to speed up builds:

```bash
# Auto-detect available cores
cmake --build nag -j

# Specify number of cores
cmake --build nag -j 8

# Conservative (leave cores for other work)
cmake --build nag -j 4
```

**NOTE:** NAG builds are so fast (~81s) that parallel builds give minimal improvement. Gfortran benefits much more from parallelization.

## Common Build Issues

### "Could not find compiler"

**Problem:** CMake cannot find the Fortran compiler
**Solution:** Forgot to load compiler module

```bash
# Check loaded modules
module list

# Load compiler module
module load nag mpi baselibs  # Or gfortran

# Verify compiler accessible
which nagfor  # or which gfortran
```

### "Mixing compilers" Errors

**Problem:** Reused build directory after switching compilers
**Solution:** Use separate build directories, never reuse

```bash
# WRONG - reusing directory
cmake -B build -DCMAKE_BUILD_TYPE=Debug  # with NAG
module load gfortran
cmake -B build -DCMAKE_BUILD_TYPE=Debug  # ERROR!

# CORRECT - separate directories
cmake -B nag -DCMAKE_BUILD_TYPE=Debug
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug
```

If you made this mistake:
```bash
rm -rf ./build  # Delete confused directory
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug  # Start fresh
```

### Missing Dependencies

**Problem:** Cannot find MPI, NetCDF, or other dependencies
**Solution:** Load baselibs module

```bash
module load baselibs  # Provides ESMF, NetCDF, HDF5, etc.
```

### Old Build Artifacts

**Problem:** Build uses cached old values
**Solution:** Clean and reconfigure

```bash
rm -rf ./nag
cmake -B nag -DCMAKE_BUILD_TYPE=Debug
cmake --build nag
```

### Module Environment Mismatch

**Problem:** Loaded wrong module combination
**Solution:** Purge and reload

```bash
module purge           # Clear all modules
module load nag mpi baselibs
module list            # Verify correct modules loaded
```

## Incremental Builds

After making code changes, just rebuild (no need to re-run cmake):

```bash
cmake --build nag
```

CMake automatically detects changed files and rebuilds only what's needed.

**Typical incremental build times:** < 3 minutes

### Incremental Build Logging

For incremental builds, also use logging:

```bash
cmake --build nag -j 8 2>&1 | tee -a nag/build.log
```

Note: Using `-a` flag appends to existing log rather than overwriting.

### Fast Workflow for generic3g Development

If working specifically on generic3g code, see the `mapl-testing` skill for a faster incremental build workflow that rebuilds only generic3g tests.

## Switching Compilers

**See the `compiler-switching` skill** for detailed instructions on safely switching between compilers.

Key points:
- Always use separate build directories
- Load new modules before configuring
- Verify modules with `module list`
- Never reuse build directories across compilers

## Clean Builds

Sometimes you need to start completely fresh:

```bash
# Remove all build directories
rm -rf ./nag ./gfortran ./intel

# Or remove just one
rm -rf ./nag

# Reconfigure
cmake -B nag -DCMAKE_BUILD_TYPE=Debug
cmake --build nag
```

## Testing After Build

**IMPORTANT:** Tests must be built explicitly before running (they are no longer auto-built).

After successful build, build and run tests with:

```bash
# Build tests
module load nag mpi baselibs && cmake --build nag -j 8 --target build-tests

# Run tests (from source directory)
module load nag mpi baselibs && ctest --test-dir nag --output-on-failure
```

**See the `mapl-testing` skill** for detailed testing workflows, including the fast generic3g-only testing method.

## Branch-Specific Notes

### develop Branch

Standard build process applies. Most features are stable.

### release/MAPL-v3 Branch

MAPL v3 pre-release branch. Build process is the same, but may have:
- Newer dependencies
- Different CMake options
- Additional features under development

Check branch-specific README or INSTALL.md for any special requirements.

## CI/CD Considerations

For continuous integration:

1. **Test multiple compilers** - Build with both NAG and gfortran minimum
2. **Use recommended timeouts** - NAG: 2min, gfortran: 9min  
3. **Separate build directories** - Parallel CI jobs need separate dirs
4. **Cache builds** - Cache build directories between CI runs for speed
5. **Test on Linux** - Use bucy or similar for Linux testing

## Quick Reference

### NAG on macOS
```bash
module load nag mpi baselibs
cmake -B nag -DCMAKE_BUILD_TYPE=Debug 2>&1 | tee nag/cmake-config.log
cmake --build nag -j 8 2>&1 | tee nag/build.log
cmake --build nag -j 8 --target build-tests 2>&1 | tee nag/build-tests.log
ctest --test-dir nag --output-on-failure 2>&1 | tee nag/ctest.log
```

### gfortran on macOS
```bash
module load gfortran mpi baselibs
cmake -B gfortran -DCMAKE_BUILD_TYPE=Debug 2>&1 | tee gfortran/cmake-config.log
cmake --build gfortran -j 8 2>&1 | tee gfortran/build.log
cmake --build gfortran -j 8 --target build-tests 2>&1 | tee gfortran/build-tests.log
ctest --test-dir gfortran --output-on-failure 2>&1 | tee gfortran/ctest.log
```

### Intel on bucy (remote)
See `remote-build` skill for complete workflow.

## CRITICAL: Module Persistence for AI Agents

⚠️ **WARNING FOR AI AGENTS:** Module commands do NOT persist across separate bash tool invocations for ANY compiler (NAG, GFortran, Intel).

Each bash tool call runs in a fresh shell. This means:

```bash
# WRONG - modules loaded in first call are lost in second call
bash: module load nag mpi baselibs
bash: cmake --build nag  # ERROR: modules not loaded!

# CORRECT - chain commands in single bash call
bash: module load nag mpi baselibs && cmake --build nag
```

**Impact:**
- Affects ALL compilers: NAG, GFortran, Intel (local and remote)
- Modules must be loaded in SAME bash invocation as the command that needs them
- Use `&&` to chain: `module load ... && cmake --build ...`
- This is NOT a compiler-specific issue - it's how the bash tool works

**When to ask user to run builds:**
- Generally prefer asking user to run builds rather than doing it yourself
- Only run builds directly if explicitly requested AND you chain module load with build command
- User's interactive shell maintains module state between commands
- AI tool invocations do not maintain state

## Summary Checklist

- [ ] Know which branch to base work on (develop vs release/MAPL-v3)
- [ ] Modules loaded for chosen compiler
- [ ] Verified compiler accessible (`which nagfor` or `which gfortran`)
- [ ] Using separate build directory for compiler (./nag, ./gfortran)
- [ ] CMake configuration succeeded
- [ ] Build completed without errors
- [ ] (Optional) Tests pass via ctest
