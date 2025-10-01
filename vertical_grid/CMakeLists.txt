esma_set_this (OVERRIDE MAPL.vertical_grid)

set(srcs
  API.F90
  IntegerPair.F90
  VerticalStaggerLoc.F90
  VerticalGridSpec.F90
  VerticalGrid.F90
  VerticalGridFactory.F90
  VerticalGridManager.F90
  BasicVerticalGrid.F90

  # gftl containers
  IntegerVerticalGridMap.F90
  VerticalGridFactoryMap.F90
)
esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.component MAPL.pfio MAPL.shared GFTL::gftl-v2
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

 if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
 endif ()

