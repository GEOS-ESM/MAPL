esma_set_this (OVERRIDE MAPL.base)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()


set (srcs
  MAPL_ExtData_IOBundleMod.F90        MAPL_Profiler.F90
  CFIOCollection.F90                    MAPL_ExtData_IOBundleVectorMod.F90  MAPL_RegridderManager.F90
  NewRegridderManager.F90
  RegridderTypeSpec.F90                 RegridderSpec.F90                   RegridderSpecRouteHandleMap.F90
  RegridMethods.F90                     HorizontalFluxRegridder.F90
  MAPL_FractionalRegridder.F90          MAPL_RegridderTypeSpecRegridderMap.F90
  ESMF_CFIOPtrVectorMod.F90             MAPL_GenericCplComp.F90             MAPL_RegridderVector.F90
  ESMFL_Mod.F90                         MAPL_Generic.F90                    MAPL_SatVapor.F90
  FileMetadataUtilities.F90             MAPL_GetLatLonCoord.F90             MAPL_SimpleAlarm.F90
  FileMetadataUtilitiesVector.F90       MAPL_GridManager.F90                MAPL_SimpleBundleMod.F90
  MAPL_AbstractGridFactory.F90          MAPL_GridSpec.F90                   MAPL_StringGridFactoryMap.F90
  MAPL_GridType.F90                     MAPL_StringGridMap.F90
  MAPL_AbstractRegridder.F90            MAPL_HistoryCollection.F90          
  MAPL_Base.F90                         MAPL_HistoryGridComp.F90            MAPL_StringRouteHandleMap.F90
  MAPL_IdentityRegridder.F90            MAPL_stubs.F90
  MAPL_Integer64GridFactoryMap.F90      MAPL_sun_uc.F90
  MAPL_CapOptions.F90                   MAPL_TilingRegridder.F90
  MAPL_CFIO.F90                         MAPL_IO.F90                         MAPL_TimeMethods.F90
  MAPL_CFIOServer.F90                   MAPL_LatLonGridFactory.F90          MAPL_TransposeRegridder.F90
  MAPL_Comms.F90                        MAPL_LatLonToLatLonRegridder.F90    MAPL_TripolarGridFactory.F90
  MAPL_LlcGridFactory.F90
  MAPL_Config.F90                       MAPL_LocStreamMod.F90               MAPL_VarSpecMod.F90
  MAPL_ConservativeRegridder.F90        MAPL_MaxMinMod.F90                  MAPL_VerticalInterpMod.F90
  MAPL_CubedSphereGridFactory.F90       MAPL_MemUtils.F90                   MAPL_VerticalMethods.F90
  MAPL_DefGridName.F90                  MAPL_Mod.F90                        MAPL_VotingRegridder.F90
  MAPL_EsmfRegridder.F90                MAPL_NewArthParser.F90              
  MAPL_ESMFTimeVectorMod.F90            MAPL_newCFIO.F90                    Regrid_Functions_Mod.F90
  MAPL_EtaHybridVerticalCoordinate.F90  MAPL_newCFIOitem.F90                
  MAPL_ExtDataCollection.F90            MAPL_NominalOrbitsMod.F90           
  MAPL_ExtDataCollectionManager.F90                 
  MAPL_ExtDataGridCompMod.F90           MAPL_OrbGridCompMod.F90             tstqsat.F90
  MAPL_LocStreamFactoryMod.F90          MAPL_HistoryTrajectoryMod.F90       MAPL_LocstreamRegridder.F90
  MAPL_ExternalGridFactory.F90
  ServerManager.F90 ApplicationSupport.F90
  regex_module.F90 StringTemplate.F90
  regex_F.c
  c_mapl_locstream_F.c  getrss.c  memuse.c
  )

if (BUILD_WITH_FLAP)
  list (APPEND srcs MAPL_FlapCapOptions.F90)
endif()

esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.profiler MAPL.pfio MAPL_cfio_r4 pflogger gftl-shared
               esmf NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  TYPE SHARED)

target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${OpenMP_Fortran_FLAGS}>)
if(DISABLE_GLOBAL_NAME_WARNING)
  target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${DISABLE_GLOBAL_NAME_WARNING}>)
endif()
target_compile_definitions (${this} PRIVATE TWO_SIDED_COMM MAPL_MODE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

if (BUILD_WITH_FLAP)
  target_link_libraries(${this} PRIVATE FLAP::FLAP)
  target_compile_definitions (${this} PRIVATE USE_FLAP)
endif()

# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PUBLIC "-Xlinker -rpath -Xlinker ${dir}")
endforeach()

ecbuild_add_executable (TARGET cub2latlon.x SOURCES cub2latlon_regridder.F90 DEPENDS esmf)
target_link_libraries (cub2latlon.x ${this} MAPL.pfio ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (cub2latlon.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util.F90 DEPENDS esmf)
target_link_libraries (Regrid_Util.x ${this} MAPL.pfio ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (Regrid_Util.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

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
