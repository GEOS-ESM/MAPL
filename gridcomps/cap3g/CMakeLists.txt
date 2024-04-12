esma_set_this (OVERRIDE MAPL.cap3g)

set(srcs
  Cap.F90
  CapGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g TYPE ${MAPL_LIBRARY_TYPE})

add_subdirectory(tests EXCLUDE_FROM_ALL)
