esma_set_this (OVERRIDE MAPL.cap)
set (srcs
     MAPL_Cap.F90
     MAPL_CapGridComp.F90
     MAPL_NUOPCWrapperMod.F90
     CapOptions.F90
     ExternalGCStorage.F90
    )
if (BUILD_WITH_FARGPARSE)
  list (APPEND srcs FargparseCLI.F90)
endif()


esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.profiler MAPL.history
                  MAPL.ExtData2G TYPE SHARED)
# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE MAPL_CapGridComp.F90 MAPL_NUOPCWrapperMod.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran OpenMP::OpenMP_Fortran
                               $<$<BOOL:${BUILD_WITH_FARGPARSE}>:FARGPARSE::fargparse>)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
