esma_set_this (OVERRIDE MAPL.Apps.tests)

if (PFUNIT_FOUND)
  add_subdirectory(acg3 EXCLUDE_FROM_ALL)
  add_subdirectory(Regrid_Util)
endif ()
