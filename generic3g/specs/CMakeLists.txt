target_sources(MAPL.generic3g PRIVATE
  AspectId.F90
  StateItemAspect.F90
  ClassAspect.F90
  FieldClassAspect.F90
  WildcardClassAspect.F90
  ServiceClassAspect.F90

  AspectCollection.F90
  AttributesAspect.F90
  GeomAspect.F90
  TypekindAspect.F90
  VerticalGridAspect.F90
  UngriddedDimsAspect.F90
  UnitsAspect.F90
  FrequencyAspect.F90

  VariableSpec.F90
  StateItem.F90
  VariableSpecVector.F90

  GeometrySpec.F90

  HorizontalDimsSpec.F90
  VerticalDimSpec.F90
  GridSpec.F90
  
  StateItemSpec.F90
  StateItemSpecMap.F90
  InvalidSpec.F90
  FieldSpec.F90
  WildcardSpec.F90
  BracketSpec.F90
#  FieldSpecVector.F90
#  ServiceProviderSpec.F90
#  ServiceRequesterSpec.F90
  ServiceSpec.F90
  StateSpec.F90
#  StateIntentsSpec.F90


  ChildSpec.F90
  ChildSpecMap.F90

  ComponentSpec.F90

  AbstractActionSpec.F90
  make_itemSpec.F90
)
