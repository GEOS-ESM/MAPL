esma_set_this ()

add_library(${this} SHARED SimpleParentGridComp.F90)
target_link_libraries(${this} MAPL.generic3g scratchpad)
target_include_directories(${this} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)

