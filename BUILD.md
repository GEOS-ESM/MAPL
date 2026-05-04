# MAPL Build System

This directory contains simplified build and test scripts for MAPL development.

## Quick Start

### Building MAPL

```bash
./build.sh [compiler] [build_type]
```

**Examples:**
```bash
# Build with NAG compiler (Debug mode, default)
./build.sh nag

# Build with GFortran compiler (Debug mode)
./build.sh gfortran

# Build with NAG compiler (Release mode)
./build.sh nag Release
```

### Running Tests

```bash
./test.sh [compiler] [test_pattern]
```

**Examples:**
```bash
# Run all tests with NAG compiler
./test.sh nag

# Run all tests with GFortran compiler
./test.sh gfortran

# Run specific test(s) matching pattern
./test.sh nag ConservationAspect
```

## How It Works

The build system uses **meta-modules** (compiler stacks) and **wrapper scripts** to simplify the workflow:

### Meta-Modules

- `nag-stack` - Loads NAG compiler + OpenMPI + baselibs
- `gfortran-stack` - Loads GFortran compiler + OpenMPI + baselibs
- `ifort-stack` - Loads Intel Fortran compiler + OpenMPI + baselibs

These meta-modules automatically handle:
- `module purge` and `module load` sequences
- Compiler dependencies (e.g., NAG requires clang)
- MPI configuration
- Baselibs setup

### Wrapper Scripts

**`build.sh`**:
- Loads the appropriate compiler stack module
- Creates build directory (`build-<compiler>`)
- Configures CMake with appropriate settings
- Builds the project

**`test.sh`**:
- Loads the appropriate compiler stack module
- Uses `ctest` to run tests (automatically handles DYLD_LIBRARY_PATH on macOS)
- Supports test filtering with regex patterns

## Build Directories

By convention, builds are organized by compiler:
- `build-nag/` - NAG compiler builds
- `build-gfortran/` - GFortran compiler builds
- `build-ifort/` - Intel compiler builds

## Manual Module Loading

If you need more control, you can still use modules manually:

```bash
module purge
module load nag-stack
cmake -B build-nag -DCMAKE_BUILD_TYPE=Debug
cmake --build build-nag -j8
```

## Supported Compilers

- **nag** - NAG Fortran compiler (default: 7.2.41)
- **gfortran** - GNU Fortran compiler (default: 15.2.0)
- **ifort** - Intel Fortran compiler

## System Requirements

- Lmod module system
- CMake 3.24+
- Appropriate compiler stack modules installed

## Troubleshooting

**"Module not found" errors:**
- Ensure you have the meta-modules installed in `~/modulefiles/core/`
- Check `module avail` to see available modules

**Build failures:**
- Check that all dependencies are loaded: `module list`
- Verify the build directory is clean or rebuild from scratch

**Test failures on macOS:**
- The `test.sh` script uses `ctest` which automatically handles DYLD_LIBRARY_PATH
- If running tests manually, ensure proper library paths are set

## For AI Agents

The simplified workflow is designed to reduce cognitive load:

1. **To build**: `./build.sh <compiler>`
2. **To test**: `./test.sh <compiler>`

No need to remember:
- Module loading sequences
- DYLD_LIBRARY_PATH settings
- CMake configuration options
- Build directory naming conventions
