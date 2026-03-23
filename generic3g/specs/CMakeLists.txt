target_sources(MAPL.generic3g PRIVATE
  AspectId.F90
  StateItemAspect.F90
  ClassAspect.F90

  FieldClassAspect.F90
  FieldClassAspect_smod.F90
  FieldBundleClassAspect.F90
  StateClassAspect.F90

  VectorClassAspect.F90
  ActualPtFieldAspectMap.F90
  WildcardClassAspect.F90
  ServiceClassAspect.F90
  BracketClassAspect.F90
  VectorBracketClassAspect.F90
  ExpressionClassAspect.F90

  AttributesAspect.F90
  GeomAspect.F90
  TypekindAspect.F90
  VerticalAlignment.F90
  VerticalGridAspect.F90
  UngriddedDimsAspect.F90
  UnitsAspect.F90
  FrequencyAspect.F90
  QuantityTypeAspect.F90
  ConservationAspect.F90
  NormalizationAspect.F90

  VariableSpec.F90
  StateItem.F90
  VariableSpecVector.F90

  GeometrySpec.F90

  GridSpec.F90
  
  StateItemSpec.F90
  StateItemSpecVector.F90
  StateItemSpecPtrVector.F90
  StateItemSpecMap.F90

  ChildSpec.F90
  ChildSpecMap.F90

  ComponentSpec.F90
  is_in_set.h
  VariableSpec_private.F90
)

esma_add_fortran_submodules(
  TARGET MAPL.generic3g
  SUBDIRECTORY GeomAspect
  SOURCES make_transform.F90)
