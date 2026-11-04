# MAPL Release Guide

This document describes the process for making a release of MAPL.

## Overview

### Branch Model

| Branch | Purpose |
|---|---|
| `release/v2` | Accumulates MAPL v2 features and fixes between releases |
| `main` | Stable, operational MAPL v2 — all releases are tagged here |
| `develop` | MAPL v3 alpha development — **not covered in this guide** |

### Two Release Flows

- **Regular release** — new features or non-trivial fixes accumulate on `release/v2` and are merged into `main` for the release.
- **Hotfix release** — a targeted bug fix starts directly on `main`.

The steps within each flow are largely the same; the main difference is the starting branch and the GEOSgcm branch used for testing.

### Versioning

MAPL follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html) with the form `x.y.z`:

- **Patch** (`x.y.Z`): true bug fixes only — the kind of change that goes into a hotfix.
- **Minor** (`x.Y.0`): anything beyond a pure bug fix (new features, refactors, deprecations, etc.).

The version is set in the `project()` call in the top-level `CMakeLists.txt`:

```cmake
project (
  MAPL
  VERSION 2.69.1
  LANGUAGES Fortran CXX C)
```

---

## Regular Release (`release/v2` → `main`)

Use this flow when features or fixes have accumulated on `release/v2` and are ready to ship.

### Step 1 — Prepare for Release PR

Create a branch off `release/v2` named after the upcoming version:

```bash
git checkout release/v2
git pull
git checkout -b feature/prepare-for-x.y.z-release
```

Make two changes on this branch:

#### 1a. Bump the version in `CMakeLists.txt`

Edit the `VERSION` field in the `project()` call at the top of `CMakeLists.txt`:

```cmake
project (
  MAPL
  VERSION x.y.z   # <-- update this
  LANGUAGES Fortran CXX C)
```

#### 1b. Update `CHANGELOG.md`

Add a new dated section for the release and reset the `[Unreleased]` section:

1. Below `## [Unreleased]`, add a new section heading:
   ```markdown
   ## [x.y.z] - YYYY-MM-DD
   ```
2. Move all content from the `[Unreleased]` subsections (Fixed, Added, Changed, etc.) into the new section. Remove any empty subsections from the new version block.
3. Reset `[Unreleased]` to a blank skeleton:
   ```markdown
   ## [Unreleased]

   ### Fixed

   ### Added

   ### Changed

   ### Removed

   ### Deprecated
   ```

Commit, push, and open a PR from `feature/prepare-for-x.y.z-release` into `release/v2`. The PR title convention is:

> `v2: Prepare for x.y.z release`

Get the PR reviewed and merged before proceeding.

---

### Step 2 — Test with GEOSgcm

Clone GEOSgcm fresh and build it against your prepare-for-release branch (or `release/v2` if the prepare PR has already been merged):

```bash
git clone https://github.com/GEOS-ESM/GEOSgcm.git
cd GEOSgcm
git checkout release/v2
# Edit components.yaml (or equivalent) to point MAPL at your branch
```

Build the model and run a standard test case.

**Expected outcome: zero-diff to the previous release.**

MAPL almost never changes model answers. If the run is *not* zero-diff, you must document exactly what changed and why before proceeding. This information will go into the release summary (Step 4).

---

### Step 3 — Merge `release/v2` into `main`

Open a PR from `release/v2` into `main`. This is the GitFlow merge that delivers the release content to the stable branch.

> **Note:** After the merge is pushed to `main`, the `push-to-main.yml` GitHub Actions workflow fires automatically. It opens a back-merge PR (`main` → `release/v2`) to keep the two branches in sync. Review and merge that PR as well.

---

### Step 4 — Create the GitHub Release

1. Go to the MAPL repository on GitHub and navigate to **Releases → Draft a new release**.
2. Enter the new tag: `vx.y.z` (targeting `main`).
3. Click **Generate release notes** to populate the auto-generated *What's Changed* section.
4. Change the release title from `vx.y.z` to `MAPL x.y.z`.
5. **Prepend** the following three sections above the auto-generated notes. Each section is separated by a horizontal rule (`---`).

#### Section 1 — Summary

A short paragraph describing what this release is and its zero-diff status. Example:

```
This is a minor release of MAPL 2.y. <Brief description of the main changes.>

Tests show it is zero-diff to MAPL x.y-1.z.
```

If the release is *not* zero-diff, replace the zero-diff statement with a precise description of what changed and why.

---

#### Section 2 — Compilers and Libraries

Copy this section verbatim from the previous release, then review and update as needed:

- Add any new compilers that are now tested.
- Remove any compilers that are no longer tested.
- Update Baselibs version and its sub-library versions if Baselibs was updated.
- Update `ESMA_cmake` version if it changed.

The format used in releases looks like this (update versions as appropriate):

```markdown
This version of MAPL was tested with:

- Intel Fortran Classic (`ifort`) 2021.13
- Intel Fortran (`ifx`) 2025.3
- GCC 14.2.0 and 15.2.0
- NAG 7.2
- Flang 22.1

The libraries this version of MAPL is currently tested with are below.

- Baselibs 8.27.0
  - netcdf-c 4.9.2
  - netcdf-fortran 4.6.1
  - ESMF 9.0.0b10 (Note: MAPL only requires 8.6.1 at the moment)
  - GFE 1.23.0
    - gFTL 1.17.0
    - gFTL-shared 1.12.0
    - pFUnit 4.16.0 (optional)
    - fArgParse 1.10.0 (if `-DBUILD_WITH_FARGPARSE=YES`, default=`YES`)
    - pFlogger 1.18.0 (if `-DBUILD_WITH_PFLOGGER=YES`, default=`YES`)
  - UDUNITS2 2.28.8

Also, if you build with Baselibs and/or `-DUSE_F2PY=ON`, you should use (at least):

- ``ESMA_cmake` v4.37.0

We recommend most external users set `-DUSE_F2PY=OFF`

We also require CMake 3.24 or higher.
```

---

#### Section 3 — From `CHANGELOG.md`

Copy and paste the `## [x.y.z]` section from `CHANGELOG.md` verbatim. Use this heading:

```markdown
## From `CHANGELOG.md`
```

---

After all three sections are in place, click **Publish release**.

---

### Step 5 — Add the Zenodo DOI (post-release)

Zenodo automatically picks up new GitHub releases and mints a DOI. This takes a short time after the release is published.

1. Go to [zenodo.org](https://zenodo.org) and find the new MAPL entry.
2. Copy the DOI badge markdown. It looks like:

   ```markdown
   [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
   ```

3. Edit the release notes and prepend the DOI badge at the very top, before the summary section.

---

## Hotfix Release (`main` → `release/v2`)

Use this flow for targeted bug fixes that need to ship immediately without picking up unreleased `release/v2` content.

### Step 1 — Prepare for Release PR

Create a branch off `main` (not `release/v2`):

```bash
git checkout main
git pull
git checkout -b feature/prepare-for-x.y.z-release
```

Apply the fix on this branch, then follow the same version bump and `CHANGELOG.md` update steps as in the regular flow ([Step 1](#step-1--prepare-for-release-pr) above), using a **patch** version increment.

Open the PR targeting `main` (not `release/v2`).

### Step 2 — Test with GEOSgcm

Clone GEOSgcm fresh and use the `main` branch (or the hotfix PR branch) as the MAPL source:

```bash
git clone https://github.com/GEOS-ESM/GEOSgcm.git
cd GEOSgcm
git checkout main
# Point MAPL at your hotfix branch or main
```

Build and run the model. The run should be zero-diff to the previous release unless the bug being fixed was itself causing incorrect answers — in which case document the change precisely.

### Steps 3–5

Follow Steps 3–5 from the [Regular Release](#regular-release-releasev2--main) flow above. The GitHub Release and Zenodo steps are identical.

> **Note:** The `push-to-main.yml` workflow fires on any push to `main`, including hotfix merges. It will automatically open a back-merge PR (`main` → `release/v2`) to carry the fix back to the development branch.

---

## Release Notes Template

Use this as a starting point when drafting a new release. Replace all placeholder text in angle brackets.

```markdown
<One or two sentences describing the release. State whether it is a minor or patch
release and give a brief summary of the main changes.>

Tests show it is zero-diff to MAPL <previous version>.

---

This version of MAPL was tested with:

- Intel Fortran Classic (`ifort`) <version>
- Intel Fortran (`ifx`) <version>
- GCC <version(s)>
- NAG <version>
- Flang <version>

The libraries this version of MAPL is currently tested with are below.

- Baselibs <version>
  - netcdf-c <version>
  - netcdf-fortran <version>
  - ESMF <version> (Note: MAPL only requires <minimum> at the moment)
  - GFE <version>
    - gFTL <version>
    - gFTL-shared <version>
    - pFUnit <version> (optional)
    - fArgParse <version> (if `-DBUILD_WITH_FARGPARSE=YES`, default=`YES`)
    - pFlogger <version> (if `-DBUILD_WITH_PFLOGGER=YES`, default=`YES`)
  - UDUNITS2 <version>

Also, if you build with Baselibs and/or `-DUSE_F2PY=ON`, you should use (at least):

- `ESMA_cmake` <version>

We recommend most external users set `-DUSE_F2PY=OFF`

We also require CMake 3.24 or higher.

---

## From `CHANGELOG.md`

<Paste the ## [x.y.z] section from CHANGELOG.md here>

---

## What's Changed

<Auto-generated by GitHub — do not edit>
```
