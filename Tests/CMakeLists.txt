set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90 
    ExtDataDriverMod.F90 
    VarspecDescription.F90
    )

if (BUILD_WITH_FLAP)
  ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ${srcs})
  target_link_libraries (ExtDataDriver.x PRIVATE MAPL FLAP::FLAP esmf ${MKL_LIBRARIES})
  # CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
  if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
     target_link_libraries(ExtDataDriver.x PRIVATE OpenMP::OpenMP_Fortran)
  endif ()
endif ()

#install(PROGRAMS ExtDataDriver.x DESTINATION bin)
#install(TARGETS ExtDataDriver.x DESTINATION bin)
    
