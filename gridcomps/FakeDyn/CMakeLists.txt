esma_set_this (OVERRIDE MAPL.fakedyn)

set(srcs
  FakeDynGridComp.F90
 )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.generic3g MAPL.field MAPL.esmf_utils TYPE SHARED)
