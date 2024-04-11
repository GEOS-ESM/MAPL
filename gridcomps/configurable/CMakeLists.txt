esma_set_this ()

esma_add_library(mapl3g_simple_leaf_gridcomp SRCS SimpleLeafGridComp.F90 DEPENDENCIES MAPL.generic3g TYPE ${MAPL_LIBRARY_TYPE})

set (comps mapl3g_simple_leaf_gridcomp )
foreach (comp ${comps})
  target_include_directories(${comp} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)
endforeach()  

