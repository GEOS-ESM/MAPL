esma_set_this ()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()


set (srcs
  ESMFL_Mod.F90            MAPL_HeapMod.F90       MAPL_VarSpecMod.F90
  MAPL_Base.F90            MAPL_InterpMod.F90     hinterp.F
  MAPL_ErrorHandling.F90
  MAPL_Throw.F90
  MAPL_Range.F90
  MAPL_Config.F90
  MAPL_CFIO.F90            MAPL_newCFIO.F90       MAPL_IO.F90
  MAPL_Comms.F90           MAPL_LocStreamMod.F90  MAPL_Mod.F90
  MAPL_Constants.F90       MAPL_Profiler.F90      sort.c
  MAPL_GenericCplComp.F90  MAPL_Sort.F90
  MAPL_Generic.F90         MAPL_sun_uc.F90        MAPL_SatVapor.F90
  MAPL_HistoryCollection.F90
  MAPL_HistoryGridComp.F90 MAPL_CapOptions.F90    $<$<BOOL:${FLAP_FOUND}>:MAPL_FlapCapOptions.F90> 
  MAPL_Cap.F90         hash.c
  MAPL_CapGridComp.F90
  MAPL_GridType.F90 
  MAPL_GridSpec.F90        MAPL_VerticalMethods.F90 MAPL_newCFIOitem.F90
  MAPL_VerticalInterpMod.F90 MAPL_ESMFTimeVectorMod.F90  MAPL_TimeMethods.F90 MAPL_ioClients.F90

  MAPL_DirPath.F90
  MAPL_AbstractRegridder.F90 MAPL_IdentityRegridder.F90
  Regrid_Functions_Mod.F90
  MAPL_EsmfRegridder.F90 MAPL_LatLonToLatLonRegridder.F90 MAPL_TilingRegridder.F90
  MAPL_ConservativeRegridder.F90 MAPL_VotingRegridder.F90 MAPL_FractionalRegridder.F90
  MAPL_CapGridComp.F90
  MAPL_KeywordEnforcer.F90
  MAPL_MinMax.F90  
  MAPL_AbstractGridFactory.F90 
  MAPL_Integer64GridFactoryMap.F90 
  MAPL_StringGridFactoryMap.F90 
  MAPL_StringGridMap.F90
  MAPL_StringRouteHandleMap.F90 
  MAPL_GridManager.F90 
  MAPL_RegridderManager.F90 
  MAPL_RegridderSpec.F90 
  MAPL_RegridderVector.F90 
  MAPL_RegridderTypeSpecRegridderMap.F90
  MAPL_LatLonGridFactory.F90  MAPL_LatLonToLatLonRegridder.F90 MAPL_TripolarGridFactory.F90 
  AbstractCommSplitter.F90 SimpleCommSplitter.F90 SplitCommunicator.F90 CommGroupDescription.F90
  c_mapl_locstream_F.c
  memuse.c                 MAPL_MemUtils.F90      MAPL_Hash.F90 
  MAPL_MaxMinMod.F90     MAPL_SimpleBundleMod.F90 
  MAPL_OrbGridCompMod.F90  MAPL_NominalOrbitsMod.F90 MAPL_ExtDataGridCompMod.F90 
  MAPL_ExtData_IOBundleMod.F90
  MAPL_ExtData_IOBundleVectorMod.F90
  MAPL_NewArthParser.F90   MAPL_ShmemMod.F90      MAPL_LoadBalance.F90 MAPL_CFIOServer.F90
  CFIOCollection.F90 ESMF_CFIOPtrVectorMod.F90
  MAPL_TransposeRegridder.F90 MAPL_GetLatLonCoord.F90 MAPL_EsmfRegridder.F90
  MAPL_NUOPCWrapperMod.F90 MAPL_CubedSphereGridFactory.F90
  MAPL_ExtDataCollection.F90 MAPL_ExtDataCollectionManager.F90
  FileMetadataUtilities.F90 FileMetadataUtilitiesVector.F90
  )

esma_add_library(${this} SRCS ${srcs} DEPENDENCIES GMAO_pFIO MAPL_cfio_r4 gftl-shared $<$<BOOL:${FLAP_IS_REQUIRED}>:FLAP::FLAP>)
#target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${OpenMP_Fortran_FLAGS}>)
target_compile_definitions (${this} PRIVATE TWO_SIDED_COMM MAPL_MODE)

# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PUBLIC "-Xlinker -rpath -Xlinker ${dir}")
endforeach()


ecbuild_add_executable (TARGET cub2latlon.x SOURCES cub2latlon_regridder.F90)
target_link_libraries (cub2latlon.x ${this} GMAO_pFIO ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (cub2latlon.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util.F90)
target_link_libraries (Regrid_Util.x ${this} GMAO_pFIO ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (Regrid_Util.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

if (EXTENDED_SOURCE)
#    target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${EXTENDED_SOURCE}>)
esma_fortran_generator_list (${this} ${EXTENDED_SOURCE})
endif()

#--------------------
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})
target_link_libraries (${this} PUBLIC ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES})
if(FLAP_FOUND)
  target_compile_definitions(${this} PRIVATE USE_FLAP)
endif()

#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.
file (COPY unused_dummy.H DESTINATION ${esma_include}/${this})
file (COPY MAPL_Generic.h DESTINATION ${esma_include}/${this})
file (COPY MAPL_Exceptions.h DESTINATION ${esma_include}/${this})
file (COPY MAPL_ErrLog.h DESTINATION ${esma_include}/${this})
file (COPY mapl_acg.pl DESTINATION ${esma_include}/${this})
file (COPY mapl_stub.pl DESTINATION ${esma_include}/${this})

# Users guide 
if (LATEX_FOUND)
  add_subdirectory (TeX)
endif ()


# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

install (
   DIRECTORY Python
   DESTINATION lib
   USE_SOURCE_PERMISSIONS
   )

install (
   PROGRAMS red_ma.pl
   DESTINATION bin
   )

