set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90 
    ExtDataDriverMod.F90 
    VarspecDescription.F90
    )

# There is currently an internal compiler error with NAG 7.0.7036
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
if (BUILD_WITH_FLAP)
  ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ${srcs})
  target_link_libraries (ExtDataDriver.x PRIVATE MAPL FLAP::FLAP esmf ${MKL_LIBRARIES})
  # CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
  if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
     target_link_libraries(ExtDataDriver.x PRIVATE OpenMP::OpenMP_Fortran)
  endif ()
endif ()
endif ()

#install(PROGRAMS ExtDataDriver.x DESTINATION bin)
#install(TARGETS ExtDataDriver.x DESTINATION bin)
    
