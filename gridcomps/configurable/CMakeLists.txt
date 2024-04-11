esma_set_this ()

esma_add_library(configurable_leaf_gridcomp SRCS ConfigurableLeafGridComp.F90 DEPENDENCIES MAPL.generic3g TYPE ${MAPL_LIBRARY_TYPE})

set (comps configurable_leaf_gridcomp )
foreach (comp ${comps})
  target_include_directories(${comp} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)
endforeach()  

