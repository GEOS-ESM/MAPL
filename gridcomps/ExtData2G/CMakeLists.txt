esma_set_this (OVERRIDE MAPL.ExtData2G)

set (srcs
     ExtDataFileStream.F90
     ExtDataRule.F90
     ExtDataDerived.F90
     ExtDataConfig.F90
     ExtDataGridCompNG.F90
     ExtDataTypeDef.F90
     ExtDataOldTypesCreator.F90
     ExtDataBracket.F90
     ExtDataUpdatePointer.F90
     ExtDataAbstractFileHandler.F90
     ExtDataClimFileHandler.F90
     ExtDataSimpleFileHandler.F90
     ExtDataNode.F90
     ExtDataLgr.F90
     ExtDataConstants.F90
     ExtDataSample.F90
     ExtData_IOBundleMod.F90
     ExtData_IOBundleVectorMod.F90
     ExtDataMasking.F90
     ExtDataPrimaryExportVector.F90
     ExtDataDerivedExportVector.F90
    )


esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base MAPL.generic MAPL.griddedio udunits2f MAPL.vertical TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

# NOTE: ExtDataGridCompNG.F90 takes 401 seconds to compile at O3 and 8 seconds at O1
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel AND CMAKE_BUILD_TYPE MATCHES Release)
  set_source_files_properties(ExtDataGridCompNG.F90 PROPERTIES COMPILE_OPTIONS ${FOPT1})
endif ()

if(PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif()
