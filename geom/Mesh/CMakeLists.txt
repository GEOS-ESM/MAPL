target_sources(MAPL.geom PRIVATE

  MeshDecomposition.F90
  MeshDecomposition/get_local_indices.F90
  MeshDecomposition/make_MeshDecomposition_current_vm.F90
  MeshDecomposition/make_MeshDecomposition_vm.F90
  MeshDecomposition/equal_to.F90
  
  MeshGeomSpec.F90
  MeshGeomFactory.F90

)

add_subdirectory(utils)
