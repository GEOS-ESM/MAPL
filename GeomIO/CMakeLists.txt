esma_set_this (OVERRIDE MAPL.GeomIO)

set(srcs
  GeomIO.F90 # package
  SharedIO.F90
  Geom_PFIO.F90
  Grid_PFIO.F90
  GeomCatagorizer.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.geom_mgr MAPL.pfio MAPL.base MAPL.shared MAPL.hconfig_utils GFTL::gftl-v2
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

 #if (PFUNIT_FOUND)
   #add_subdirectory(tests EXCLUDE_FROM_ALL)
 #endif ()

