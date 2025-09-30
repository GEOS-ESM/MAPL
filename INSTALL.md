# MAPL Installation Instructions

## Dependent Libraries

### CMake

MAPL requires CMake 3.24+ to build. It is tested with both the GNU Make
and Ninja generators.

### Compilers

MAPL requires a Fortran 2003 compliant compiler. It is currently tested and
supported with:

- Intel Fortran Classic `ifort` 2021.6.0 and 2021.13.0
- Intel Fortran LLVM `ifx` 2025.0
- GCC 13.2.0, 14.2.0, 15.2.0
- NAG Fortran 7.2.28

Note that at the moment MAPL does not support LLVM Flang or NVHPC.
Efforts are underway to support these.

### MPI

MAPL requires MPI and has been tested to run with:

- Open MPI
- Intel MPI
- MPICH 4 (only MAPL 2.41 and higher)
- MVAPICH2

### Libraries

MAPL currently depends on many libraries for full use of its capabilities. These include:

- [ESMF](https://github.com/esmf-org/esmf)
  - [netCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
    - [netCDF-C](https://github.com/Unidata/netcdf-c)
      - [HDF5](https://github.com/HDFGroup/hdf5)
- [GFE](https://github.com/Goddard-Fortran-Ecosystem/GFE)
  - [gFTL](https://github.com/Goddard-Fortran-Ecosystem/gFTL)
  - [gFTL-shared](https://github.com/Goddard-Fortran-Ecosystem/gFTL-shared)
  - [fArgParse](https://github.com/Goddard-Fortran-Ecosystem/fArgParse)
  - [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) (for unit testing)
  - [yaFyaml](https://github.com/Goddard-Fortran-Ecosystem/yaFyaml)
  - [pFlogger](https://github.com/Goddard-Fortran-Ecosystem/pFlogger)
- [UDUNITS2](https://github.com/GMAO-SI-Team/UDUNITS-2)

MAPL is currently tested with the following library versions:

| Package        | Tested Version |
|:---------------|:---------------|
| netCDF-C       | v4.9.2         |
| netCDF-Fortran | v4.6.1         |
| UDUNITS2       | v2.2.28        |
| ESMF           | v8.9.0         |
| GFE            | v1.19.0        |

#### ESMF Versions

NOTE: MAPL only requires ESMF 8.6.1, but we currently build with
ESMF 8.9.0 and have support for ESMF 9 betas.

#### ESMA Baselibs

The above libraries are equivalent to ESMA-Baselibs v8.19.0. This is used
internally by GEOS-ESM users at the GMAO.

## Getting MAPL

### Obtaining MAPL from git clone

Obtaining MAPL via a `git clone` is a bit complex due to how we handle
sub-repositories. Rather than use Git submodules or
ExternalProject/FetchContent, we use a homegrown tool called
[`mepo`](https://github.com/GEOS-ESM/mepo/) to manage them. `mepo` uses the
`components.yaml` file to know what tag of each sub-repository to clone, where
to put it, and what to name it.

`mepo` is a fairly simple Python3 tool that can be installed by `pip`,
`spack`, or `brew`.

Then you can run `mepo clone` in your MAPL clone and you'll get
three subrepos:

- [ESMA_env](https://github.com/GEOS-ESM/ESMA_env)
  - This is we use internally to control our compilers, libraries, etc. for external users it's a bit of a no-op
- [ESMA_cmake](https://github.com/GEOS-ESM/ESMA_cmake)
  - This has most of our CMake controls, macros, etc.
- [ecbuild](https://github.com/GEOS-ESM/ecbuild)
  - This is cloned within ESMA_cmake and gives us access to the ecbuild macros

### Obtaining MAPL from a complete release tarfile

A simpler way to obtain MAPL is to download a "complete" release tarfile from
the Releases page. Each release has a "complete" tarfile that has had the `mepo clone`
step run within it. This file will be named `MAPL-vX.YY.Z.COMPLETE.tar.xz`
where `X.YY.Z` is the version number of MAPL. We provide this for users that do
not want to deal with `mepo` or the sub-repositories.

### Spack

MAPL is also available via [spack](https://spack.io). The spack package is
maintained by GEOS-ESM and is used by external users to provide MAPL. As such,
it has many of the ideosyncracies of MAPL's clone-build-install process "baked"
into it. If you need MAPL-as-library, that could be an easier way to go by
running:
```
spack install mapl
```

## Building MAPL

Once you have all the dependent libraries, the build process should be pretty standard:

```
cmake -B build-dir -S . --install-prefix=/path/to/install-dir < -DCMAKE_Fortran_COMPILER=XXX >
cmake --build build-dir --target install -j N
```
where `N` is the number of parallel build jobs you want to run.

Note: If you have `FC` set in the environment, then there is no need for
`CMAKE_Fortran_COMPILER` but many environments do not provide `FC` and might
default to `gfortran` which might not be what you want.

### Available CMake Options

- `USE_EXTDATA2G` (default: `ON`, recommended: `ON`)
  - If `ON`, will build MAPL's ExtData2G library. All current GEOS-ESM projects
    use ExtData2G (rather than the original ExtData) for reading external data.
- `USE_F2PY` (default: `ON`, recommended: `OFF`)
  - If `ON`, will build an f2py-based interface to MAPL. This is not recommended
    for general use, as f2py + CMake can be a challenge.
- `BUILD_SHARED_MAPL` (default: `ON`, recommended: `ON`)
  - If `ON`, will build MAPL as a shared library. If `OFF`, will build MAPL as
    a static library. Note: unlike many packages, the `ON` option does not build
    *both* a shared and static library. It builds *only* a shared library.
- `BUILD_WITH_FARGPARSE` (default: `ON`, recommended: `ON`)
  - If `ON`, will build MAPL with the `fArgParse` library. Much of MAPL's
    utilities use `fArgParse` for command-line argument parsing.
- `BUILD_WITH_PFLOGGER` (default: `ON`, recommended: `ON`)
  - If `ON`, will build MAPL with the `pFlogger` library. This is the logging
    library used by MAPL and while not required yet, it is highly recommended.
- `INSTALL_SOURCE_TARFILE` (default: `OFF`, recommended: `OFF`)
  - If `ON`, will install a tarfile of the source code in the install directory.
    This is useful for users that want to have the source code as an "archive"
    of what was built.
- `USE_CODATA_2018_CONSTANTS` (default: `OFF`, recommended: `OFF`)
  - This option enables newer CODATA constants for use in MAPL. It is not
    yet defaulted to `ON` as it would change answers in codes using MAPL's
    constants.

## Running MAPL Unit Tests

If MAPL was built with pFUnit, then the unit tests can be run with:

```
ctest --test-dir build-dir -j N
```
where `N` is the number of tests you want to run in parallel.

Note that some MAPL tests are quite expensive to run. To avoid running them,
you can instead run:
```
ctest --test-dir build-dir -j N -L 'ESSENTIAL'
```
