# Test-only component used by the callback_freeze_captest case. Not part
# of all and not installed; build-tests builds it.
add_library(callback_freeze_gridcomp SHARED CallbackFreezeGridComp.F90)
target_link_libraries(callback_freeze_gridcomp MAPL.generic)
set_target_properties(callback_freeze_gridcomp PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
add_dependencies(build-tests callback_freeze_gridcomp)
