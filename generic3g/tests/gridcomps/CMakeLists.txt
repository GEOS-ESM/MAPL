esma_set_this ()

add_library(simple_leaf_gridcomp SHARED SimpleLeafGridComp.F90)
target_link_libraries(simple_leaf_gridcomp scratchpad)

add_library(simple_parent_gridcomp SHARED SimpleParentGridComp.F90)
target_link_libraries(simple_parent_gridcomp scratchpad)

add_library(proto_extdata_gc SHARED ProtoExtDataGC.F90)

add_library(fakedyn_gridcomp SHARED FakeDynGridComp.F90)

set (comps simple_parent_gridcomp simple_leaf_gridcomp proto_extdata_gc fakedyn_gridcomp)
foreach (comp ${comps})
  target_link_libraries(${comp} MAPL.generic3g)
  target_include_directories(${comp} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)
endforeach()  
target_link_libraries(fakedyn_gridcomp MAPL)

#add_library(parameterized_gridcomp SHARED ParameterizedGridComp.F90)
#target_link_libraries(parameterized_gridcomp MAPL.generic3g scratchpad)
#target_include_directories(parameterized_gridcomp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

# These targets are not part of all, nor do the tests directly depend upon them (by design).
# So, we need to ensure that build-tests builds them.
add_dependencies(build-tests ${comps}) # parameterized_gridcomp)
