esma_set_this ()

set (srcs
    MAPL_AbstractGridFactory.F90
    MAPL_AbstractRegridder.F90
    MAPL_ConservativeRegridder.F90
    MAPL_CubedSphereGridFactory.F90
    MAPL_EsmfRegridder.F90
    MAPL_FractionalRegridder.F90
    MAPL_GetLatLonCoord.F90
    MAPL_GridManager.F90
    MAPL_GridSpec.F90
    MAPL_GridType.F90
    MAPL_IdentityRegridder.F90
    MAPL_Integer64GridFactoryMap.F90
    MAPL_LatLonGridFactory.F90
    MAPL_LatLonToLatLonRegridder.F90
    MAPL_RegridderManager.F90
    MAPL_RegridderSpec.F90
    MAPL_RegridderTypeSpecRegridderMap.F90
    MAPL_RegridderVector.F90
    MAPL_StringGridFactoryMap.F90
    MAPL_StringGridMap.F90
    MAPL_StringRouteHandleMap.F90
    MAPL_TransposeRegridder.F90
    MAPL_TilingRegridder.F90
    MAPL_TripolarGridFactory.F90
    MAPL_VerticalInterpMod.F90
    MAPL_VotingRegridder.F90
    Regrid_Functions_Mod.F90
   )
esma_add_library (${this} SRCS ${srcs} DEPENDENCIES Shared GMAO_pFIO Comms IO pflogger  gftl-shared gftl MPI::MPI_Fortran)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include ${INC_ESMF} ${INC_NETCDF})
target_link_libraries (${this} PUBLIC ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

