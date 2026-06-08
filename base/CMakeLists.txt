esma_set_this (OVERRIDE MAPL.base)

set (srcs
  # Geom-dependent files (remaining after mp_utils split)
  FileIO.F90
  FileIOShared.F90
  FileMetadataUtilities.F90
  FileMetadataUtilitiesVector.F90
  NCIO.F90
  c_mapl_locstream_F.c
  MAPL_LocStreamMod.F90
  MemUtils.F90
  SunOrbit.F90
  SimpleBundleMod.F90
  # Umbrella modules (#5004)
  API.F90
  # Moved to mp_utils/:
  #   PackedTime.F90, SimulationTime.F90, TimeInterpolation.F90
  #   LocalDisplacementEnsemble.F90, SatVapor.F90 (+ headers), StringTemplate.F90
)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES ESMF::ESMF PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2
               MAPL.utils MAPL.geom MAPL.esmf MAPL.pfio MAPL.mp_utils
               MAPL.field_bundle MAPL.utils MAPL.constants
               MPI::MPI_Fortran NetCDF::NetCDF_Fortran
  TYPE SHARED
)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE FileIOShared.F90 NCIO.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_compile_definitions (${this} PRIVATE MAPL_MODE)
