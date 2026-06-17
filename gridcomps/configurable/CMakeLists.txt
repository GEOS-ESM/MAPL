esma_set_this (OVERRIDE configurable_gridcomp)
#esma_set_this (OVERRIDE MAPL.cinfigurable)

esma_add_library(${this}
  SRCS ConfigurableGridComp.F90
  DEPENDENCIES MAPL.generic MAPL
  TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
