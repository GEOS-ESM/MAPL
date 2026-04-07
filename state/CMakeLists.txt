esma_set_this (OVERRIDE ${MAPL_TARGET_PREFIX}.state)

set(srcs
  API.F90
  StateGet.F90
  StateSet.F90
  StateGetPointer.F90

  StateUtils.F90
  StateArithmeticParser.F90
  StateMasking.F90
  StateFilter.F90
  StateDestroy.F90
)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES ${MAPL_TARGET_PREFIX}.vertical_grid ${MAPL_TARGET_PREFIX}.base ${MAPL_TARGET_PREFIX}.field ${MAPL_TARGET_PREFIX}.field_bundle ${MAPL_TARGET_PREFIX}.shared ${MAPL_TARGET_PREFIX}.esmf_utils ESMF::ESMF PFLOGGER::pflogger
  TYPE SHARED
)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

# BMA Remove if not needed.
#target_include_directories (${this} PUBLIC
  #$<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
#target_link_libraries (${this} PUBLIC ESMF::ESMF)
