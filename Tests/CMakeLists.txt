esma_set_this (OVERRIDE MAPL.test_utilities)

set(MODULE_DIRECTORY "${esma_include}/Tests")

set (srcs
    ExtDataRoot_GridComp.F90
    VarspecDescription.F90
    )

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE ExtDataDriverMod.F90 ExtDataDriverGridComp.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL NetCDF::NetCDF_Fortran NetCDF::NetCDF_C TYPE SHARED)

if (BUILD_WITH_FARGPARSE)

  ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ExtDataDriver.F90 ExtDataDriverGridComp.F90 ExtDataDriverMod.F90)
  target_link_libraries (ExtDataDriver.x PRIVATE MAPL MAPL.test_utilities FARGPARSE::fargparse ESMF::ESMF OpenMP::OpenMP_Fortran)
  set_target_properties(ExtDataDriver.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
  target_compile_definitions (ExtDataDriver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)
  add_subdirectory(ExtData_Testing_Framework EXCLUDE_FROM_ALL)

  ecbuild_add_executable (TARGET pfio_MAPL_demo.x SOURCES pfio_MAPL_demo.F90)
  target_link_libraries (pfio_MAPL_demo.x PRIVATE MAPL FARGPARSE::fargparse ESMF::ESMF OpenMP::OpenMP_Fortran)
  set_target_properties(pfio_MAPL_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

  ecbuild_add_executable (TARGET MAPL_demo_fargparse.x SOURCES MAPL_demo_fargparse.F90)
  target_link_libraries (MAPL_demo_fargparse.x PRIVATE MAPL FARGPARSE::fargparse ESMF::ESMF OpenMP::OpenMP_Fortran)
  set_target_properties(MAPL_demo_fargparse.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

  ecbuild_add_executable (TARGET CapDriver.x SOURCES CapDriver.F90)
  target_link_libraries (CapDriver.x PRIVATE MAPL MAPL.test_utilities FARGPARSE::fargparse ESMF::ESMF OpenMP::OpenMP_Fortran)
  set_target_properties(CapDriver.x PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

  add_subdirectory(GetHorzIJIndex)

endif ()

