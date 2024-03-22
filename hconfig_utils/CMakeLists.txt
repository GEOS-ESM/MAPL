esma_set_this (OVERRIDE MAPL.hconfig_utils)

set(srcs
#  hconfig_value_mod.F90
#  hconfig_i4.F90
#  hconfig_i8.F90
#  hconfig_r4.F90
#  hconfig_r8.F90
#  hconfig_logical.F90
#  hconfig_string.F90
#  hconfig_i4seq.F90
#  hconfig_i8seq.F90
#  hconfig_r4seq.F90
#  hconfig_r8seq.F90
#  hconfig_logical_seq.F90
  mapl3g_hconfig_get.F90
#  mapl3g_hconfig_getter.F90
  mapl3g_hconfig_get_private.F90
  HConfig3G.F90
  )

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared PFLOGGER::pflogger
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC esmf)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
