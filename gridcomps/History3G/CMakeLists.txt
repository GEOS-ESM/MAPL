esma_set_this (OVERRIDE MAPL.history3g)

set(srcs
 HistoryGridComp_private.F90
 HistoryGridComp.F90
 HistoryCollectionGridComp.F90
 HistoryCollectionGridComp_private.F90
 )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES mapl3g PFLOGGER::pflogger TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
