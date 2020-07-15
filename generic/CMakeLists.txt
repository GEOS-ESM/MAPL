esma_set_this (OVERRIDE MAPL.generic)

set (srcs
  VariableSpec.F90
  Connection.F90
  #ConnectionManager.F90
  )

#esma_add_library(${this} SRCS ${srcs} DEPENDENCIES MAPL_Shared)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
