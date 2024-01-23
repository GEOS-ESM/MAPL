target_sources(MAPL.generic3g PRIVATE

  StateExtension.F90
  ExtensionVector.F90

  ExtensionAction.F90
  NullAction.F90
  ActionVector.F90

  CopyAction.F90
  ConvertUnitsAction.F90
  RegridAction.F90

  BundleAction.F90
  SequenceAction.F90
  
)
