esma_set_this ()

add_library(proto_extdata_gc SHARED
  ProtoExtDataGC.F90)

add_library(proto_stat_gc SHARED
  ProtoStatGridComp.F90)

set (comps proto_extdata_gc proto_stat_gc)
foreach (comp ${comps})
  target_link_libraries(${comp} MAPL.generic3g)
  target_include_directories(${comp} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)
endforeach()  

#add_library(parameterized_gridcomp SHARED ParameterizedGridComp.F90)
#target_link_libraries(parameterized_gridcomp MAPL.generic3g scratchpad)
#target_include_directories(parameterized_gridcomp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

# These targets are not part of all, nor do the tests directly depend upon them (by design).
# So, we need to ensure that build-tests builds them.
add_dependencies(build-tests ${comps}) # parameterized_gridcomp)
