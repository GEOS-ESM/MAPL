esma_set_this (OVERRIDE MAPL.geom_mgr)

set(srcs
  geom_mgr.F90 # package
  GeomUtilities.F90
  
  GeomSpec.F90
  NullGeomSpec.F90
  MaplGeom.F90
  MaplGeom_smod.F90

  GeomFactory.F90

  CoordinateAxis.F90
  CoordinateAxis_smod.F90
  HConfigUtils.F90

  latlon/LonAxis.F90
  latlon/LonAxis_smod.F90
  latlon/LatAxis.F90
  latlon/LatAxis_smod.F90
  latlon/LatLonDecomposition.F90
  latlon/LatLonDecomposition_smod.F90
  latlon/LatLonGeomSpec.F90
  latlon/LatLonGeomSpec_smod.F90
  latlon/LatLonGeomFactory.F90
  latlon/LatLonGeomFactory_smod.F90
  
  GeomManager.F90
  GeomManager_smod.F90

# gFTL containers
  GeomFactoryVector.F90
  GeomSpecVector.F90
  IntegerMaplGeomMap.F90

  VectorBasis.F90
  VectorBasis_smod.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.pfio MAPL.base MAPL.shared MAPL.field_utils GFTL::gftl-v2
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC esmf)

 if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
 endif ()

