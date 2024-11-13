target_sources(MAPL.generic3g PRIVATE

  ExtensionAction.F90
  NullAction.F90
  ActionVector.F90

  RegridAction.F90
  VerticalRegridAction.F90
  CopyAction.F90
  ConvertUnitsAction.F90

  TimeInterpolateAction.F90
  AccumulatorAction.F90
  MeanAction.F90
  MaxAction.F90
  MinAction.F90
)
