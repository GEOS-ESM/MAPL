esma_set_this()

set (srcs
  ESMF_TestCase.F90
  ESMF_TestMethod.F90
  ESMF_TestParameter.F90
  MAPL_pFUnit_Throw.F90
)  

esma_add_library (${this} EXCLUDE_FROM_ALL SRCS ${srcs})

target_include_directories (${this} PRIVATE ${INC_ESMF})
target_include_directories (${this} PRIVATE ${INC_NETCDF})

target_link_libraries (${this} ${ESMF_LIBRARIES} pfunit)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

