esma_set_this (OVERRIDE MAPL.base)

set (srcs
  MAPL_Profiler.F90
  ESMFL_Mod.F90
  FileMetadataUtilities.F90
  FileMetadataUtilitiesVector.F90       MAPL_SimpleBundleMod.F90
  MaplGrid.F90
  MAPL_stubs.F90
  MAPL_ESMFTimeVectorMod.F90
  MAPL_TimeMethods.F90
  FileIOShared.F90                      BinIO.F90                           NCIO.F90
  MAPL_IO.F90
  MAPL_Config.F90                       MAPL_LocStreamMod.F90
  MAPL_MaxMinMod.F90                    MAPL_VerticalInterpMod.F90
                                        MAPL_VerticalMethods.F90
  Base.F90
  MAPL_NominalOrbitsMod.F90
  ServerManager.F90 ApplicationSupport.F90
  MAPL_SphericalGeometry.F90
  c_mapl_locstream_F.c  getrss.c  memuse.c
  Base/Base_Base.F90 Base/Base_Base_implementation.F90
  TimeStringConversion.F90
  MAPL_Resource.F90
  MAPL_DateTime_Parsing_ESMF.F90
  MAPL_EASEConversion.F90
  # Orphaned program: should not be in this library.
  # tstqsat.F90
  )

# Comms.F90, StringTemplate.F90, SatVapor.F90 have been moved to base3g/

# SimulationTime.F90, LDE.F90 have been moved to base3g/

esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES MAPL.geom MAPL.shared MAPL.constants MAPL.profiler MAPL.pfio MAPL.field MAPL.base3g udunits2f PFLOGGER::pflogger
               GFTL_SHARED::gftl-shared-v2 GFTL_SHARED::gftl-shared-v1  GFTL::gftl-v2 GFTL::gftl-v1
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE SHARED)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE FileIOShared.F90 BinIO.F90 NCIO.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

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

# Users guide
if (LATEX_FOUND)
  add_subdirectory (TeX)
endif ()

# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

install (
   PROGRAMS red_ma.pl
   DESTINATION bin
   )
