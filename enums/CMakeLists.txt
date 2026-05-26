esma_set_this (OVERRIDE MAPL.enums)

set (srcs
  ConservationType.F90
  NormalizationType.F90
  QuantityType.F90
  VectorBasisKind.F90
  ConservationMetadata.F90
  NormalizationMetadata.F90
  QuantityTypeMetadata.F90
  ValidationMode.F90
  VerificationStatus.F90
  GenericPhases.F90
  CouplerPhases.F90
  StateItemAllocation.F90
  FieldBundleType_Flag.F90
  API.F90
  )

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared ESMF::ESMF
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
