esma_set_this ()

esma_add_library(configurable_gridcomp SRCS ConfigurableGridComp.F90 DEPENDENCIES MAPL.generic3g TYPE SHARED)
target_include_directories(configurable_gridcomp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

