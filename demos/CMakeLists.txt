
ecbuild_add_executable (TARGET cub2latlon.x SOURCES cub2latlon_regridder.F90)
target_link_libraries (cub2latlon.x GridsRegridding Shared GMAO_pFIO Comms IO ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (cub2latlon.x PROPERTIES Fortran_MODULE_DIRECTORY "${OpenMP_Fortran_FLAGS}")
target_include_directories (cub2latlon.x PUBLIC ${INC_ESMF} ${INC_NETCDF})

ecbuild_add_executable (TARGET regrid_util.x SOURCES Regrid_Util.F90)
target_link_libraries (regrid_util.x GridsRegridding Shared GMAO_pFIO Comms IO MAPL_Base GridComps Cap ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (regrid_util.x PROPERTIES Fortran_MODULE_DIRECTORY "${OpenMP_Fortran_FLAGS}")
target_include_directories (regrid_util.x PUBLIC ${INC_ESMF} ${INC_NETCDF})
target_include_directories (regrid_util.x PRIVATE ${MAPL_SOURCE_DIR}/include)
