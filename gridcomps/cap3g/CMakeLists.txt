esma_set_this (OVERRIDE MAPL.cap3g)

set(srcs
  Cap.F90
  CapGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES mapl3g TYPE SHARED)

add_subdirectory(tests EXCLUDE_FROM_ALL)

ecbuild_add_executable(TARGET GEOS.x SOURCES GEOS.F90 DEPENDS mapl3g MAPL.cap3g ESMF::ESMF)
target_link_libraries(GEOS.x PRIVATE ${this})

# GEOS.x is needed for 'make tests'
add_dependencies(build-tests GEOS.x)

apply_esmf_fpe0_workaround()
