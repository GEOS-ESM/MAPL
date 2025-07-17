esma_set_this (OVERRIDE MAPL.base)

set (srcs
  MAPL_Profiler.F90
  CFIOCollection.F90                    MAPL_RegridderManager.F90
  NewRegridderManager.F90
  RegridderTypeSpec.F90                 RegridderSpec.F90                   RegridderSpecRouteHandleMap.F90
  RegridMethods.F90                     HorizontalFluxRegridder.F90
  MAPL_FractionalRegridder.F90          MAPL_RegridderTypeSpecRegridderMap.F90
  ESMF_CFIOPtrVectorMod.F90             MAPL_RegridderVector.F90
  ESMFL_Mod.F90                         MAPL_SatVapor.F90
  FileMetadataUtilities.F90             MAPL_GetLatLonCoord.F90             MAPL_SimpleAlarm.F90
  FileMetadataUtilitiesVector.F90       MAPL_GridManager.F90                MAPL_SimpleBundleMod.F90
  MAPL_AbstractGridFactory.F90          MAPL_GridSpec.F90                   MAPL_StringGridFactoryMap.F90
  MAPL_GridType.F90                     MAPL_StringGridMap.F90              MaplGrid.F90
  MAPL_AbstractRegridder.F90
  MAPL_StringRouteHandleMap.F90
  MAPL_IdentityRegridder.F90            MAPL_stubs.F90
  MAPL_Integer64GridFactoryMap.F90      MAPL_sun_uc.F90
  MAPL_TilingRegridder.F90
  MAPL_CFIO.F90                         MAPL_TimeMethods.F90
  FileIOShared.F90                      BinIO.F90                           NCIO.F90
  MAPL_IO.F90
  MAPL_LatLonGridFactory.F90          MAPL_TransposeRegridder.F90
  MAPL_Comms.F90                        MAPL_LatLonToLatLonRegridder.F90    MAPL_TripolarGridFactory.F90
  MAPL_LlcGridFactory.F90               MAPL_SwathGridFactory.F90
  MAPL_Config.F90                       MAPL_LocStreamMod.F90
  MAPL_ConservativeRegridder.F90        MAPL_MaxMinMod.F90                  MAPL_VerticalInterpMod.F90
  MAPL_CubedSphereGridFactory.F90       MAPL_MemUtils.F90                   MAPL_VerticalMethods.F90
  MAPL_DefGridName.F90                  Base.F90                        MAPL_VotingRegridder.F90
  MAPL_EsmfRegridder.F90
  MAPL_ESMFTimeVectorMod.F90            Regrid_Functions_Mod.F90
  MAPL_EtaHybridVerticalCoordinate.F90
  MAPL_NominalOrbitsMod.F90
  MAPL_LocStreamFactoryMod.F90          MAPL_LocstreamRegridder.F90
  MAPL_ExternalGridFactory.F90
  ServerManager.F90 ApplicationSupport.F90
  ESMF_CFIOPtrVectorMod.F90
  CFIOCollection.F90                    MAPL_CFIO.F90
  regex_module.F90 StringTemplate.F90 MAPL_SphericalGeometry.F90
  regex_F.c  MAPL_ObsUtil.c
  c_mapl_locstream_F.c  getrss.c  memuse.c
  Base/Base_Base.F90 Base/Base_Base_implementation.F90
  TimeStringConversion.F90
  MAPL_ISO8601_DateTime_ESMF.F90
  MAPL_Resource.F90
  MAPL_XYGridFactory.F90
  MAPL_NetCDF.F90                       Plain_netCDF_Time.F90
  MAPL_DateTime_Parsing_ESMF.F90	MAPL_ObsUtil.F90
  MAPL_EASEConversion.F90
  MAPL_EASEGridFactory.F90
  # Orphaned program: should not be in this library.
  # tstqsat.F90
  )

# Add SimulationTime.F90 to srcs if we are building with pflogger
if (BUILD_WITH_PFLOGGER)
  list(APPEND srcs SimulationTime.F90)
endif()

esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.constants MAPL.profiler MAPL.pfio MAPL_cfio_r4 MAPL.field_utils udunits2f PFLOGGER::pflogger
               GFTL_SHARED::gftl-shared-v2 GFTL_SHARED::gftl-shared-v1  GFTL::gftl-v2 GFTL::gftl-v1
               ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE ${MAPL_LIBRARY_TYPE})
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

ecbuild_add_executable (TARGET cub2latlon.x SOURCES cub2latlon_regridder.F90 DEPENDS ESMF::ESMF MAPL.shared)
target_link_libraries (cub2latlon.x PRIVATE ${this} MAPL.pfio MPI::MPI_Fortran OpenMP::OpenMP_Fortran)
set_target_properties(cub2latlon.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

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
