esma_set_this (OVERRIDE MAPL.extdata3g)

set(srcs
 ExtDataGridComp.F90
 ExtDataGridComp_private.F90
 DataSetNode.F90
 DataSetBracket.F90
 AbstractDataSetFileSelector.F90
 NonClimDataSetFileSelector.F90
 ExtDataUtilities.F90
 ExtDataCollection.F90
 ExtDataCollectionMap.F90
 ExtDataConfig.F90
 ExtDataConstants.F90
 ExtDataDerived.F90
 ExtDataDerivedMap.F90
 ExtDataRule.F90
 ExtDataRuleMap.F90
 ExtDataSample.F90
 ExtDataSampleMap.F90
 PrimaryExport.F90
 PrimaryExportVector.F90
 ExtDataFileReader.F90
 )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES mapl3g MAPL.pfio MAPL.base MAPL.vertical MAPL.regridder_mgr PFLOGGER::pflogger TYPE SHARED)

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
  target_compile_definitions(${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
