esma_set_this (OVERRIDE callback_test_gridcomp)

esma_add_library(${this}
  SRCS ConfigurableCallbackHandler.F90
  DEPENDENCIES MAPL.generic MAPL
  TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
