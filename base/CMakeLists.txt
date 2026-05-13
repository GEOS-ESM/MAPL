esma_set_this (OVERRIDE MAPL.base)

set (srcs
  FileMetadataUtilities.F90
  FileMetadataUtilitiesVector.F90       SimpleBundleMod.F90
  MAPL_ESMFTimeVectorMod.F90
                                        MAPL_VerticalInterpMod.F90
                                        MAPL_VerticalMethods.F90
  )

# Comms.F90, StringTemplate.F90, SatVapor.F90 have been moved to base3g/

# SimulationTime.F90, LDE.F90 have been moved to base3g/

esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES MAPL.geom MAPL.shared MAPL.constants MAPL.profiler MAPL.pfio MAPL.field MAPL.field_bundle MAPL.base3g MAPL.utilities udunits2f PFLOGGER::pflogger
               GFTL_SHARED::gftl-shared-v2 GFTL_SHARED::gftl-shared-v1  GFTL::gftl-v2 GFTL::gftl-v1
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE SHARED)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

if(DISABLE_GLOBAL_NAME_WARNING)
  target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${DISABLE_GLOBAL_NAME_WARNING}>)
endif()
target_compile_definitions (${this} PRIVATE TWO_SIDED_COMM MAPL_MODE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PUBLIC "-Xlinker -rpath -Xlinker ${dir}")
endforeach()

if (EXTENDED_SOURCE)
  esma_fortran_generator_list (${this} ${EXTENDED_SOURCE})
endif()

# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

