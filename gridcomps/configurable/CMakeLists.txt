esma_set_this (OVERRIDE configurable_gridcomp)

esma_add_library(${this}
  SRCS ConfigurableGridComp.F90
  DEPENDENCIES MAPL.generic3g MAPL.state
  TYPE SHARED)
