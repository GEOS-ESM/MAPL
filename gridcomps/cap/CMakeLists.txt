esma_set_this (OVERRIDE MAPL.cap)

set(srcs
  CapGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL TYPE SHARED)

add_subdirectory(tests EXCLUDE_FROM_ALL)
