target_sources(MAPL.generic PRIVATE
  # CSR_SparseMatrix.F90, VerticalConservativeMap.F90, VerticalLinearMap.F90,
  # VerticalRegridMethod.F90 moved to infrastructure/vertical/vertical/
  FixedLevelsVerticalGrid.F90
  ModelVerticalGrid.F90
)
