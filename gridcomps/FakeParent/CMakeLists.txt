esma_set_this (OVERRIDE fakeparent_gridcomp)

esma_add_library(${this}
  SRCS FakeParentGridComp.F90
  DEPENDENCIES MAPL.generic MAPL
  TYPE SHARED)
