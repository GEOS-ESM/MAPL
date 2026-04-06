if (MAPL_SUPPORT_MAPL3)
  esma_set_this (OVERRIDE ${MAPL_TARGET_PREFIX})
else ()
  esma_set_this ()
endif ()

# Common dependencies always required
set (mapl_dependencies
  ${MAPL_TARGET_PREFIX}.base
  ${MAPL_TARGET_PREFIX}.pfio
  MAPL_cfio_r4
  ${MAPL_TARGET_PREFIX}.griddedio
  ${MAPL_TARGET_PREFIX}.field_utils
  ${MAPL_TARGET_PREFIX}.state_utils
  ${MAPL_TARGET_PREFIX}.vertical
  ${MAPL_TARGET_PREFIX}.oomph
  ESMF::ESMF
  NetCDF::NetCDF_Fortran
  MPI::MPI_Fortran
  $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>
  )

# Full-MAPL mode adds the generic layer, gridcomps, and Python bridge
if (NOT MAPL_SUPPORT_MAPL3)
  list (APPEND mapl_dependencies
    MAPL.generic
    MAPL.gridcomps
    MAPL.orbit
    MAPL.python_bridge
    ${EXTDATA_TARGET}
    )
endif ()

esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES ${mapl_dependencies}
  TYPE SHARED
  )

target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

