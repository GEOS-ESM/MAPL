esma_set_this (OVERRIDE MAPL.component)

set (srcs
  CouplerPhases.F90
  ComponentDriver.F90
  ComponentDriverVector.F90
  ComponentDriverPtrVector.F90
  GriddedComponentDriver.F90
  GriddedComponentDriverMap.F90
  GriddedComponentDriverVector.F90

  MultiState.F90)

esma_add_library(${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.esmf_utils MAPL.shared
    TYPE ${MAPL_LIBARRY_TYPE}
    )

esma_add_fortran_submodules(
  TARGET MAPL.component
  SUBDIRECTORY GriddedComponentDriver
  SOURCES initialize.F90 run.F90 finalize.F90 get_states.F90
          get_clock.F90 set_clock.F90 run_export_couplers.F90
          run_import_couplers.F90 clock_advance.F90
          get_gridcomp.F90 get_name.F90 add_export_coupler.F90
          add_import_coupler.F90 write_restart.F90)


target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)
