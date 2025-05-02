target_sources(MAPL.generic3g PRIVATE

  TransformId.F90
  ExtensionTransform.F90
  NullTransform.F90
  TransformVector.F90

  RegridTransform.F90
  VerticalRegridTransform.F90
  CopyTransform.F90
  ConvertUnitsTransform.F90

  TimeInterpolateTransform.F90
  AccumulatorTransform.F90
  MeanTransform.F90
  MaxTransform.F90
  MinTransform.F90
  AccumulatorTransformInterface.F90
)
