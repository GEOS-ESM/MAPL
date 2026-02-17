---
name: mapl-setup
description: First-time MAPL development environment setup and configuration
compatibility: opencode
---

## What I Do

Guide you through initial MAPL development environment setup, including:
- Module discovery and configuration
- Creating personalized environment config
- Verifying compiler and build tool access
- SSH setup for remote builds (optional)
- Understanding the MAPL branch strategy

## When to Use Me

Use this skill when:
- Setting up MAPL development for the first time on a new machine
- Configuring a new development environment
- Troubleshooting environment setup issues
- Learning about available MAPL development workflows

## Environment Configuration

MAPL supports customizable module names via `~/.opencode/mapl-config.yaml`. This file allows you to map generic names (like "nag") to your system's actual module names (like "compiler/nag-fortran").

### Creating Your Configuration File

A template configuration file has been created at `~/.opencode/mapl-config.yaml`. Edit this file to match your system's module names.

### Finding Your Module Names

**Step 1: List available modules**
```bash
module avail
```

**Step 2: Search for specific modules**
```bash
# Find Fortran compilers
module avail 2>&1 | grep -i fortran
module avail 2>&1 | grep -i nag
module avail 2>&1 | grep -i gfortran
module avail 2>&1 | grep -i gcc

# Find MPI implementations
module avail 2>&1 | grep -i mpi
module avail 2>&1 | grep -i openmpi

# Find baselibs
module avail 2>&1 | grep -i baselibs
module avail 2>&1 | grep -i mapl
```

**Step 3: Update your config file**

Edit `~/.opencode/mapl-config.yaml` with the module names found above:

```yaml
modules:
  nag: "compiler/nag-fortran"      # Use the exact name from module avail
  gfortran: "gcc/13"                # Or gnu-fortran, compiler/gnu, etc.
  mpi: "openmpi/4.1"               # Or mpi, mpich, intel-mpi
  baselibs: "MAPL-baselibs"        # Or baselibs, mapl-deps
  ifx: "ifx-stack"                 # For bucy (usually same for everyone)
  
paths:
  mapl_root: "~/swdev/VS/MAPL"     # Your MAPL checkout location
  build_dir_prefix: ""              # Empty for ./nag, or "build-" for ./build-nag
```

## Verify Your Environment

### Test Module Loading

```bash
# Test loading NAG compiler
module load nag mpi baselibs  # Or use your configured names
module list

# Verify compiler is accessible
which nagfor  # Should show path to compiler
nagfor --version

# Test gfortran (if available)
module purge
module load gfortran mpi baselibs
which gfortran
gfortran --version
```

### Verify Build Tools

```bash
# Check CMake is available
which cmake
cmake --version  # Should be 3.17 or newer

# Check Git
which git
git --version
```

## Branch Strategy Overview

**CRITICAL:** Always clarify which base branch to use before starting work.

### Two Main Branches

1. **`develop`** - Legacy MAPL (MAPL 2.x maintenance)
   - Use for bug fixes and features for MAPL 2.x
   - Most stable, production-ready code
   
2. **`release/MAPL-v3`** - MAPL v3 pre-release
   - Use for new MAPL v3 development
   - Active development branch for next major version

### Feature Branch Naming

When starting new work, create a feature branch following the pattern:
```bash
feature/#ISSUE-description
bugfix/#ISSUE-description
hotfix/#ISSUE-description
```

Where `#ISSUE` is the GitHub issue number (e.g., `feature/#4392-add-opencode-skills`).

**Always create a GitHub issue first** to get the issue number before creating your feature branch.

## SSH Setup for Remote Builds (Optional)

If you'll be building on the remote `bucy` server with Intel compilers, set up SSH ControlMaster to avoid repeated PIN entries.

### Add to `~/.ssh/config`:

```
Host bucy bucy.gsfc.nasa.gov
    ControlMaster auto
    ControlPath ~/.ssh/cm-%r@%h:%p
    ControlPersist 8h
```

### Establish Initial Connection

```bash
ssh -fN bucy  # Enter PIN once
```

This creates a persistent SSH connection that lasts 8 hours. All subsequent SSH commands will reuse this connection without asking for PIN.

**See the `remote-build` skill for complete bucy workflow details.**

## Quick Start Workflow

### 1. Configure Environment
```bash
# Edit your config file
vi ~/.opencode/mapl-config.yaml

# Or use the template directly and test
module load nag mpi baselibs
module list
```

### 2. Clone MAPL (if not done already)
```bash
cd ~/swdev/VS  # Or your preferred location
git clone git@github.com:GEOS-ESM/MAPL.git
cd MAPL
```

### 3. Choose Your Base Branch
```bash
# For MAPL 2.x work:
git checkout develop

# For MAPL v3 work:
git checkout release/MAPL-v3
```

### 4. Try a Test Build

See the `mapl-build` skill for detailed build instructions.

Quick test:
```bash
module load nag mpi baselibs
cmake -B build-test -DCMAKE_BUILD_TYPE=Debug
cmake --build build-test -j 4
```

## Common Setup Issues

### "module: command not found"

**Problem:** Shell doesn't have module command
**Solution:** Source the modules init script
```bash
source /etc/profile.d/modules.sh
# Or add to your ~/.bashrc or ~/.zshrc
```

### Modules Not Found

**Problem:** Module names don't match your system
**Solution:** Use `module avail` to find exact names, update your config file

### CMake Too Old

**Problem:** CMake version < 3.17
**Solution:** Load newer cmake module or install via package manager
```bash
module avail cmake  # Look for newer version
# Or: brew install cmake (macOS)
```

### Build Directory Conflicts

**Problem:** Old build directories from different compilers
**Solution:** Use separate build directories per compiler
```bash
rm -rf ./build ./build-test  # Remove old directories
# Use: ./nag, ./gfortran, ./intel (see mapl-build skill)
```

## Next Steps

After setup, use these skills for specific tasks:
- **`mapl-build`** - Build MAPL with different compilers
- **`mapl-testing`** - Run and debug tests
- **`fortran-style`** - Learn MAPL coding standards
- **`github-workflow`** - Understand Git/GitHub conventions
- **`remote-build`** - Build on bucy with Intel compiler

## Summary Checklist

- [ ] Created/edited `~/.opencode/mapl-config.yaml` with your module names
- [ ] Verified compiler modules load correctly (`module load`, `module list`)
- [ ] Verified cmake and git are accessible
- [ ] Understand `develop` vs `release/MAPL-v3` branch strategy
- [ ] (Optional) SSH ControlMaster configured for bucy
- [ ] Successfully completed a test build
