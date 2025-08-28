esma_set_this (OVERRIDE MAPL.hconfig)

set(srcs
  API.F90 # package
  HConfigAs.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared ESMF::ESMF GFTL::gftl-v2
  TYPE SHARED
  )
target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

 if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
 endif ()

