esma_set_this (OVERRIDE MAPL.utilities)

set(srcs utilities.F90)

esma_add_library(${this} SRCS ${srcs} DEPENDENCIES MAPL.shared TYPE SHARED)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

add_subdirectory(arrays)
add_subdirectory(regex)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
