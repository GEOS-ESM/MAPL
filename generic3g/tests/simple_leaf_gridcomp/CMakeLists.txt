esma_set_this ()

add_library(${this} SHARED SimpleLeafGridComp.F90)
target_link_libraries(${this} MAPL.generic3g scratchpad)
target_include_directories(${this} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

