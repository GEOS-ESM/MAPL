esma_set_this (OVERRIDE MAPL.FieldBundle)

set (srcs
        FieldBundleRead.F90
        FieldBundleWrite.F90
        MAPL_SimpleBundleMod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.constants MAPL.base MAPL.generic MAPL.pfio MAPL.ESMFcfio
                                                    MAPL.ExtData MAPL_cfio_r4  TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared esmf NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran)

# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

ecbuild_add_executable (TARGET Regrid_Util.x SOURCES Regrid_Util.F90 DEPENDS esmf )
target_link_libraries (Regrid_Util.x PRIVATE ${this} MAPL.pfio ${MPI_Fortran_LIBRARIES})
# CMake has an OpenMP issue with NAG Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
           target_link_libraries(Regrid_Util.x PRIVATE OpenMP::OpenMP_Fortran)
   endif ()
   set_target_properties(Regrid_Util.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
