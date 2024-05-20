esma_set_this (OVERRIDE MAPL.history3g)

set(srcs
 HistoryGridComp_private.F90
 HistoryGridComp.F90
 HistoryCollectionGridComp.F90
 HistoryCollectionGridComp_private.F90
 OutputInfo.F90
 UngriddedDimInfo.F90
 UngriddedDimInfoSet.F90
 UngriddedDimsInfo.F90
 StringUngriddedDimMap.F90
 UngriddedDimSet.F90
 MAPL3G_ESMF_Info_Keys.F90 
 )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g PFLOGGER::pflogger TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
