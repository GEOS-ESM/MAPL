target_sources(MAPL.geom PRIVATE

  MeshDecomposition.F90
  MeshDecomposition_smod.F90
  
  MeshGeomSpec.F90
  MeshGeomSpec_smod.F90
  
  MeshGeomFactory.F90
  MeshGeomFactory_smod.F90

)

if (BUILD_WITH_FARGPARSE)
  add_subdirectory(raster_to_mesh)
endif()
