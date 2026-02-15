esma_set_this (OVERRIDE MAPL.hconfig_utils)

set(srcs
  HConfig3G.F90
  hconfig_get.F90
  hconfig_params.F90
  hconfig_get_private.F90
  generalized_equality.F90
  get_hconfig.F90
  HConfigUtilities.F90
  )

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared PFLOGGER::pflogger
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
  target_compile_definitions(${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
