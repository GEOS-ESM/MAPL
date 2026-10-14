esma_set_this (OVERRIDE udunits2f)

set(srcs
  CptrWrapper.F90
  UDSystem.F90
  udunits2f.F90
  encoding.F90
  interfaces.F90
  status_codes.F90
  ut_set_ignore_error_message_handler.c
  )
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared
  TYPE SHARED
)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

find_package(udunits REQUIRED)
find_package(EXPAT REQUIRED)

target_link_libraries(${this} PUBLIC udunits::udunits)
target_link_libraries(${this} PUBLIC EXPAT::EXPAT)

if (PFUNIT_FOUND)
  # Turning off until test with GNU can be fixed
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
