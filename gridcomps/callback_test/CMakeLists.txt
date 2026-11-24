esma_set_this (OVERRIDE callback_test_gridcomp)

# ImplicitColumnDiffusion.F90 is mapl_TurbulentMixingStep_mod's solver;
# it lives under superstructure/generic/tests (its own test suite owns
# its numerics) and that directory's library is only built when
# PFUNIT_FOUND, so it is compiled again here rather than linked, to
# keep this always-built library independent of PFUNIT_FOUND.
esma_add_library(${this}
  SRCS ConfigurableCallbackHandler.F90 TurbulentMixingStep.F90
       ${CMAKE_CURRENT_SOURCE_DIR}/../../superstructure/generic/tests/ImplicitColumnDiffusion.F90
  DEPENDENCIES MAPL.generic MAPL
  TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
