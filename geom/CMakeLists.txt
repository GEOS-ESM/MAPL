esma_set_this (OVERRIDE MAPL.geom)

set(srcs
  API.F90 # package
  GeomUtilities.F90

  GeomSpec.F90
  NullGeomSpec.F90
  MaplGeom.F90

  GeomFactory.F90

  CoordinateAxis.F90

  GeomManager.F90

  GeomGet.F90

  # gFTL containers
  GeomFactoryVector.F90
  GeomSpecVector.F90
  IntegerMaplGeomMap.F90

  VectorBasis.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.pfio MAPL.base MAPL.shared MAPL.hconfig_utils GFTL::gftl-v2
  TYPE SHARED
  )

add_subdirectory(API)
add_subdirectory(MaplGeom)
add_subdirectory(CoordinateAxis)
add_subdirectory(LatLon)
add_subdirectory(GeomManager)
add_subdirectory(VectorBasis)
add_subdirectory(CubedSphere)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

 if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
 endif ()

