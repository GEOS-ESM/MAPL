esma_set_this ()

add_library(simple_leaf_gridcomp SHARED SimpleLeafGridComp.F90)
target_link_libraries(simple_leaf_gridcomp MAPL.generic3g scratchpad)
target_include_directories(simple_leaf_gridcomp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

add_library(simple_parent_gridcomp SHARED SimpleParentGridComp.F90)
target_link_libraries(simple_parent_gridcomp MAPL.generic3g scratchpad)
target_include_directories(simple_parent_gridcomp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

# These targets are not part of all, nor do the tests directly depend upon them (by design).
# So, we need to ensure that build-tests builds them.
add_dependencies(build-tests simple_leaf_gridcomp simple_parent_gridcomp)
