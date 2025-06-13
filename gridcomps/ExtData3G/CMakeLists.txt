esma_set_this (OVERRIDE MAPL.extdata3g)

set(srcs
 ExtDataGridComp.F90
 ExtDataGridComp_private.F90
 DataSetNode.F90
 DataSetBracket.F90
 AbstractFileHandler.F90
 SimpleFileHandler.F90
 ExtDataUtilities.F90
 )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g MAPL.pfio MAPL.base PFLOGGER::pflogger TYPE SHARED)

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
  target_compile_definitions(${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
