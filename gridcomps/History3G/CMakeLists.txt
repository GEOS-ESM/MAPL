esma_set_this (OVERRIDE MAPL.history3g)

set(srcs
 HistoryGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g PFLOGGER::pflogger TYPE ${MAPL_LIBRARY_TYPE})

