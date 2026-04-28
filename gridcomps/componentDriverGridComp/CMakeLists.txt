esma_set_this (OVERRIDE MAPL.componentDriverGridComp)

esma_add_library(${this}
  SRCS componentDriverGridComp.F90 time_support.F90
  DEPENDENCIES MAPL.generic3g MAPL
  NOINSTALL
  TYPE SHARED)

# Ensure this library is built when running 'make tests'
add_dependencies(build-tests ${this})
