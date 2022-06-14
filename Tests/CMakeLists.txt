set(MODULE_DIRECTORY "${esma_include}/Tests")

set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90
    ExtDataDriverMod.F90
    VarspecDescription.F90
    )

if (BUILD_WITH_FLAP)

  ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ${srcs})
  target_link_libraries (ExtDataDriver.x PRIVATE MAPL FLAP::FLAP esmf)
  # CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
  if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
    target_link_libraries(ExtDataDriver.x PRIVATE OpenMP::OpenMP_Fortran)
  endif ()
  set_target_properties(ExtDataDriver.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
  target_compile_definitions (ExtDataDriver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

  ecbuild_add_executable (TARGET pfio_MAPL_demo.x SOURCES pfio_MAPL_demo.F90)
  target_link_libraries (pfio_MAPL_demo.x PRIVATE MAPL FLAP::FLAP esmf)
  # CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
  if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
    target_link_libraries(pfio_MAPL_demo.x PRIVATE OpenMP::OpenMP_Fortran)
  endif ()
  set_target_properties(pfio_MAPL_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
  add_subdirectory(ExtData_Testing_Framekwork EXCLUDE_FROM_ALL)

endif ()
