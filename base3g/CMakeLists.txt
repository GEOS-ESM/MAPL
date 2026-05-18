esma_set_this (OVERRIDE MAPL.base3g)

set (srcs
  FileMetadataUtilities.F90
  FileMetadataUtilitiesVector.F90
  FileIOShared.F90
  NCIO.F90
  c_mapl_locstream_F.c
  MAPL_LocStreamMod.F90
  PackedTime.F90
  SimulationTime.F90
  Comms.F90
  SatVapor.F90
  StringTemplate.F90
  LocalDisplacementEnsemble.F90
  MemUtils.F90
  FileIO.F90
  TimeInterpolation.F90
  SunOrbit.F90
  SimpleBundleMod.F90
  API.F90
)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES ESMF::ESMF PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 MAPL.shared MAPL.geom MAPL.esmf_utils MAPL.pfio MAPL.field_bundle MAPL.utils MAPL.constants MPI::MPI_Fortran NetCDF::NetCDF_Fortran
  TYPE SHARED
)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE FileIOShared.F90 NCIO.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_compile_definitions (${this} PRIVATE MAPL_MODE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
