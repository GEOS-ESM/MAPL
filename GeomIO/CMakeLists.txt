esma_set_this (OVERRIDE MAPL.GeomIO)

set(srcs
  GeomIO.F90 # package
  SharedIO.F90
  Geom_PFIO.F90
  Grid_PFIO.F90
  GeomCatagorizer.F90
  pFIOServerBounds.F90
  DataCollection.F90
  DataCollectionVector.F90
  DataCollectionManager.F90
  )

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.field MAPL.field_bundle MAPL.geom MAPL.pfio MAPL.base MAPL.shared MAPL.esmf_utils MAPL.hconfig_utils GFTL::gftl-v2
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)

# Testing has shown that ifx with Release flags seems to have issues with
# some code in GeomCatagorizer.F90. For now we add a workaround via ifdef
# so that development can continue as ifx flags are finalized. Note
# that all other compilers seem happy save ifx in Release mode.
if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_BUILD_TYPE STREQUAL "Release")
  message(STATUS "[ifx] Workaround for ifx Release build enabled")
  target_compile_definitions(${this} PRIVATE IFX_RELEASE_BUG)
endif()

if (PFUNIT_FOUND)
   add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

