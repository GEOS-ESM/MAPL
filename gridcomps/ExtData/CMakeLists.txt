esma_set_this (OVERRIDE MAPL.extdata)

set (srcs
     MAPL_ExtDataCollectionManager.F90  MAPL_ExtData_IOBundleMod.F90
     MAPL_ExtDataCollection.F90  MAPL_ExtDataGridCompMod.F90 
     MAPL_ExtData_IOBundleVectorMod.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base)
target_link_libraries (${this} PUBLIC gftl gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
