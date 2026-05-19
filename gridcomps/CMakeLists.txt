esma_set_this(OVERRIDE MAPL.gridcomps)

esma_add_library (${this}
     SRCS MAPL_GridComps.F90
     DEPENDENCIES MAPL.pfio
                  $<$<BOOL:${BUILD_WITH_FARGPARSE}>:FARGPARSE::fargparse>
     TYPE SHARED
     )

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
if (BUILD_WITH_FARGPARSE)
  target_link_libraries(${this} PRIVATE FARGPARSE::fargparse)
  target_compile_definitions (${this} PRIVATE USE_FARGPARSE)
endif()

add_subdirectory(orbit)
add_subdirectory(cap)
add_subdirectory(history)
add_subdirectory(configurable)
add_subdirectory(extdata)
add_subdirectory(statistics)
add_subdirectory(FakeParent)
add_subdirectory(componentDriverGridComp)
