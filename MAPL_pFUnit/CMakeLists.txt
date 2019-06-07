esma_set_this()

set (srcs
  ESMF_TestCase.F90
  ESMF_TestMethod.F90
  ESMF_TestParameter.F90
  MAPL_pFUnit_Throw.F90
)  

esma_add_library (${this} EXCLUDE_FROM_ALL SRCS ${srcs})

include_directories (${INC_ESMF})
include_directories (${INC_NETCDF})
include_directories (${PFUNIT_INCLUDE_DIRS})

target_link_libraries (${this} ${ESMF_LIBRARIES} ${PFUNIT_LIBRARIES})
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

