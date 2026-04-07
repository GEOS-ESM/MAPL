esma_set_this (OVERRIDE ${MAPL_TARGET_PREFIX}.state_utils)

set(srcs
  StateUtils.F90
  StateArithmeticParser.F90
  StateMasking.F90 
  StateFilter.F90
  )
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES ${MAPL_TARGET_PREFIX}.base ${MAPL_TARGET_PREFIX}.shared PFLOGGER::pflogger
  TYPE ${MAPL_LIBRARY_TYPE}
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

