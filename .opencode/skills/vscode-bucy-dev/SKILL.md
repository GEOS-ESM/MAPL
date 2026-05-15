---
name: vscode-bucy-dev
description: Set up VS Code on bucy for MAPL development workflows
compatibility: opencode
---

# VS Code Setup Agent for MAPL on Bucy

## What I Do

Set up and verify a practical VS Code development environment on `bucy` for:
- MAPL standalone development
- Build/test/debug loops using workspace tasks

## When to Use Me

Use this skill when:
- You are onboarding a new machine/account for MAPL work
- VS Code Remote-SSH is not configured for `bucy`
- You want a repeatable local workflow for build/test/debug

## Assumptions

- You have SSH access to `bucy`
- You can authenticate with your normal NASA workflow (PIV or key)
- Modules are available on `bucy`
- `git`, `cmake`, and `mepo` are available (or can be installed in your user env)

## 1) Configure Remote-SSH

On your local machine, add or update `~/.ssh/config`:

```sshconfig
Host bucy
    HostName gs6101-bucy.gsfc.nasa.gov
    User <your-username>
    ControlMaster auto
    ControlPath ~/.ssh/cm-%r@%h:%p
    ControlPersist 8h
```

Then connect from VS Code with:
- `Remote-SSH: Connect to Host...`
- Select `bucy`

Optional keepalive connection:
```bash
ssh -fN bucy
```

## 2) Clone MAPL on Bucy

Use a dedicated parent directory for MAPL development:

```bash
mkdir -p ~/workspace
cd ~/workspace

# MAPL
git clone git@github.com:GEOS-ESM/MAPL.git
cd MAPL
mepo clone
```

## 3) Open MAPL in VS Code

In VS Code (connected to `bucy`):
1. `File -> Open Folder...`
2. Select `~/workspace/MAPL`

## 4) Install Recommended Extensions (Remote)

Install these in the **remote (SSH:bucy)** context:
- `ms-vscode-remote.remote-ssh`
- `ms-vscode.cpptools`
- `ms-vscode.cmake-tools`
- `fortran-lang.linter-gfortran`
- `hansec.fortran-ls`

If using GitHub PR workflows:
- `GitHub.vscode-pull-request-github`

## 5) Workspace Settings Baseline

Use these settings in each repo (or workspace-level settings):

```jsonc
{
  "git.detectSubmodules": true,
  "git.repositoryScanMaxDepth": 12,
  "cmake.configureOnOpen": false,
  "files.associations": {
    "*.F90": "fortran-modern",
    "*.H": "cpp"
  }
}
```

## 6) MAPL Build/Test Tasks on Bucy

MAPL already has `.vscode/tasks.json` in-repo. Use:
- `Build`
- `Test`
- `Run GEOS.x (mpirun --n <numRanks> /path/to/GEOS.x cap.yaml)`

Typical choices on `bucy`:
- Compiler: `ifort`
- Build type: `Debug`
- Test label: `ESSENTIAL` first, then `NIGHTLY`

## 7) Debugging Setup

This repo already includes a working MPI debug pattern with `gdbserver` + rank-specific attach configs.

### Existing VS Code assets

- Task: `Run GEOS.x under gdbserver (6 ranks)`
  - Starts 6 MPI ranks and assigns ports per rank: `2345 + PMI_RANK`
  - Loads `${input:compiler}-stack`
- Launch configs (`.vscode/launch.json`):
  - `Attach MPI rank 0 (:2345)` through `Attach MPI rank 5 (:2350)`
  - Compound: `Attach all MPI ranks (0-5)`

### Debug workflow (recommended)

1. Run `Build` with `compiler=ifort`, `buildType=Debug`
2. Start task `Run GEOS.x under gdbserver (6 ranks)`
3. In Run and Debug, launch `Attach all MPI ranks (0-5)`
4. Set breakpoints in MAPL sources and continue execution

### Notes

- Current launch configs use `program: ${workspaceFolder}/MAPL.geom.tests` for symbol/source mapping.
- If debugging `GEOS.x` directly, set `program` to `${workspaceFolder}/build/ifort/Debug/install/bin/GEOS.x` in a dedicated config.
- For first-pass triage, run with `numRanks=1`; once stable, scale to multi-rank.

## 8) Quick Health Checks

Run these in each repo:

```bash
module load ifort-stack
which cmake
which ifort
mepo status
```

For MAPL from repo root:

```bash
cmake -S . -B build/ifort/Debug -DUSE_F2PY=Off
cmake --build build/ifort/Debug -j8
ctest --test-dir build/ifort/Debug -L ESSENTIAL --output-on-failure
```

## Common Failures

- `module: command not found`
  - Source modules init first (system-dependent path) before `module load`
- `mepo` missing
  - Install in your user environment (`pip install --user mepo`)
- Mixed compiler build directory
  - Remove build dir and reconfigure with one compiler stack only

## Completion Checklist

- [ ] Remote-SSH to `bucy` works in VS Code
- [ ] MAPL cloned with `mepo clone`
- [ ] MAPL folder opened in VS Code
- [ ] Recommended extensions installed remotely
- [ ] MAPL build/test tasks run successfully
- [ ] Debug attach works for a running process
