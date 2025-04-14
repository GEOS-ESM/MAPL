---
preprocessor: cpp -traditional-cpp -E
src_dir: ../../
output_dir: dev-doc
search: true
graph: true
coloured_edges: true
graph_maxdepth: 4
graph_maxnodes: 32
include: ../../include/
         ../../gFTL/install/GFTL-1.15/include/v1
         ../../gFTL/install/GFTL-1.15/include/v2
exclude: **/EsmfRegridder.F90
         **/FieldBLAS_IntrinsicFunctions.F90
         **/GeomManager.F90
         **/MaplGeom.F90
         **/Regridder.F90
         **/StateSupplement.F90
exclude_dir: ../../docs
             ../../Doxygen
             ../../ESMA_cmake
             ../../ESMA_env
             ../../build
             ../../gFTL
             ../../esmf
             ../../pFUnit
             ../../fArgParse
             ../../pFlogger
macro: USE_MPI=1
       BUILD_WITH_PFLOGGER=1
       BUILD_WITH_EXTDATA2G=1
       USE_FLAP=1
       H5_HAVE_PARALLEL=1
       TWO_SIDED_COMM=1
       MAPL_MODE=1
fixed_length_limit: false
source: true
display: public
         private
         protected
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
external: remote = https://mathomp4.github.io/esmf
project: MAPL
project_github: https://github.com/GEOS-ESM/MAPL
project_website: https://github.com/GEOS-ESM/MAPL
summary: MAPL is a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF)
author: The MAPL Developers
github: https://github.com/GEOS-ESM
email: matthew.thompson@nasa.gov
print_creation_date: true
sort: type-alpha
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
extensions: f90
            F90
            pf
fpp_extensions: F90
                pf
                F
externalize: true
---

{!../../README.md!}
