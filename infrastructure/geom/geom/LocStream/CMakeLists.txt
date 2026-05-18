target_sources(MAPL.geom PRIVATE

  LocStreamDecomposition.F90
  LocStreamDecomposition/get_local_indices.F90
  LocStreamDecomposition/make_LocStreamDecomposition_current_vm.F90
  LocStreamDecomposition/make_LocStreamDecomposition_vm.F90
  LocStreamDecomposition/equal_to.F90
  
  LocStreamGeomSpec.F90
  LocStreamGeomFactory.F90

)
