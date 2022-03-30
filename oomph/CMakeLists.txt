esma_set_this (OVERRIDE MAPL.oomph)

set (srcs
  oomph.F90
  specs/UngriddedDimSpec.F90
  specs/VerticalStaggerLoc.F90
  specs/HorizontalStaggerLoc.F90
  specs/DimSpec.F90
  specs/CouplingSpec.F90
  specs/AbstractStateItemSpec.F90
  specs/FieldSpec.F90
  specs/ConnectionPoint.F90
  
#  specs/DimensionsSpec.F90
#
#  specs/StateItemSpec.F90
#  specs/FieldSpec.F90
#  specs/BundleSpec.F90
#  specs/ServiceSpec.F90
#  specs/StateItemSpecMap.F90
#
#  specs/ConnectionPoint.F90
#  specs/ConnectionSpec.F90
#  specs/ConnectionSpecVector.F90
#
#  specs/ComponentSpec.F90
#  specs/ComponentSpecMap.F90
  )
  

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.base GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2 TYPE ${MAPL_LIBRARY_TYPE}
  )
