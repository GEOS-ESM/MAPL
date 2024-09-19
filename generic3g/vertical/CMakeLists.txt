target_sources(MAPL.generic3g PRIVATE

  VerticalGrid.F90
  BasicVerticalGrid.F90
  MirrorVerticalGrid.F90
  FixedLevelsVerticalGrid.F90
  ModelVerticalGrid.F90
  VerticalRegridMethod.F90
  CSR_SparseMatrix.F90
  WeightComputation.F90
)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY BasicVerticalGrid
  SOURCES can_connect_to.F90
)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY ModelVerticalGrid
  SOURCES can_connect_to.F90
)

