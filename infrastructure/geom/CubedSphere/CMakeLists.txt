target_sources(MAPL.geom PRIVATE

  CubedSphereGeomSpec.F90
  CubedSphereGeomSpec_smod.F90
  CubedSphereGeomFactory.F90
  CubedSphereGeomFactory_smod.F90
  CubedSphereDecomposition.F90
  CubedSphereDecomposition_smod.F90
)

esma_add_fortran_submodules(
  TARGET MAPL.geom
  SUBDIRECTORY CubedSphereGeomSpec
  SOURCES get_horz_ij_index.F90)
