set (SRCS
  MAPL_GetHorzIJIndex_Mod.F90
  driver_MAPL_GetHorzIJIndex.F90
  )

#set(dependencies MAPL GFTL_SHARED::gftl-shared esmf OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
  TARGET driver_MAPL_GetHorzIJIndex.x
  SOURCES ${SRCS} )
target_link_libraries(driver_MAPL_GetHorzIJIndex.x  PRIVATE MAPL FARGPARSE::fargparse ESMF::ESMF  OpenMP::OpenMP_Fortran)
