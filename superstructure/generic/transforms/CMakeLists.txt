target_sources(MAPL.generic PRIVATE

  TransformId.F90
  ExtensionTransform.F90
  NullTransform.F90
  ExtendTransform.F90
  TransformVector.F90

  RegridTransform.F90
  VerticalRegridTransform.F90
  CopyTransform.F90
  ConvertUnitsTransform.F90
  NormalizationTransform.F90

  TimeInterpolateTransform.F90

  EvalTransform.F90
  ExtensionTransformUtils.F90

  # Absorbed from couplers/
  CouplerMetaComponent.F90
  GenericCoupler.F90
)
