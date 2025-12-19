esma_set_this (OVERRIDE MAPL.test_utilities)

set(MODULE_DIRECTORY "${esma_include}/Tests")

if (BUILD_WITH_FARGPARSE)

#  add_subdirectory(GetHorzIJIndex)
  add_subdirectory(MAPL3G_Component_Testing_Framework)

endif ()

