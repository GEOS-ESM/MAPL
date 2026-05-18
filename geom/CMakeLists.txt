esma_set_this (OVERRIDE MAPL.geom)

# Top-level .F90 files moved to infrastructure/geom/geom/ (in-progress migration).
set(geom_dir ${CMAKE_CURRENT_SOURCE_DIR}/../infrastructure/geom/geom)

set(srcs
  ${geom_dir}/API.F90
  ${geom_dir}/GeomUtilities.F90

  ${geom_dir}/GeomSpec.F90
  ${geom_dir}/NullGeomSpec.F90
  ${geom_dir}/MaplGeom.F90

  ${geom_dir}/GeomFactory.F90

  ${geom_dir}/CoordinateAxis.F90

  ${geom_dir}/GeomManager.F90

  ${geom_dir}/GeomGet.F90
  ${geom_dir}/GridGet.F90
  ${geom_dir}/GridGetGlobal.F90
  ${geom_dir}/GridGetHorzIJIndex.F90
  ${geom_dir}/GeomGetHorzIJIndex.F90

  # gFTL containers
  ${geom_dir}/GeomFactoryVector.F90
  ${geom_dir}/GeomSpecVector.F90
  ${geom_dir}/IntegerMaplGeomMap.F90

  ${geom_dir}/VectorBasis.F90

  ${geom_dir}/Subgrid.F90
)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.pfio MAPL.shared MAPL.hconfig_utils MAPL.esmf_utils GFTL::gftl-v2
  TYPE SHARED
)

add_subdirectory(${geom_dir}/GridGet geom_GridGet)
add_subdirectory(${geom_dir}/MaplGeom geom_MaplGeom)
add_subdirectory(${geom_dir}/CoordinateAxis geom_CoordinateAxis)
add_subdirectory(${geom_dir}/LatLon geom_LatLon)
add_subdirectory(${geom_dir}/GeomManager geom_GeomManager)
add_subdirectory(${geom_dir}/VectorBasis geom_VectorBasis)
add_subdirectory(${geom_dir}/CubedSphere geom_CubedSphere)
add_subdirectory(${geom_dir}/LocStream geom_LocStream)
add_subdirectory(${geom_dir}/Mesh geom_Mesh)
add_subdirectory(${geom_dir}/XY geom_XY)
add_subdirectory(${geom_dir}/EASE geom_EASE)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

 if (PFUNIT_FOUND)
   add_subdirectory(${geom_dir}/tests geom_tests EXCLUDE_FROM_ALL)
 endif ()

