set (this MAPL.geom)

set(srcs
  GeomUtilities.F90

  GeomSpec.F90
  NullGeomSpec.F90
  MaplGeom.F90

  GeomFactory.F90

  CoordinateAxis.F90

  GeomManager.F90

  GeomGet.F90       # also contains GeomGetHorzIJIndex and GridGetHorzIJIndex (merged, #4944)
  GridGet.F90
  GridGetGlobal.F90
  GridComms.F90

  # gFTL containers
  GeomFactoryVector.F90
  GeomSpecVector.F90
  IntegerMaplGeomMap.F90

  VectorBasis.F90

  Subgrid.F90

  # Umbrella module
  API.F90
)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.pfio MAPL.enums MAPL.utils MAPL.esmf MAPL.mp_utils GFTL::gftl-v2
  TYPE SHARED
)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

add_subdirectory (GridGet)
add_subdirectory (MaplGeom)
add_subdirectory (CoordinateAxis)
add_subdirectory (LatLon)
add_subdirectory (GeomManager)
add_subdirectory (VectorBasis)
add_subdirectory (CubedSphere)
add_subdirectory (LocStream)
add_subdirectory (Mesh)
add_subdirectory (XY)
add_subdirectory (EASE)

if (PFUNIT_FOUND)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
