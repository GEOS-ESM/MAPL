esma_set_this(OVERRIDE MAPL.gridcomps)

esma_add_library (${this}
     SRCS MAPL_GridComps.F90
     DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.cap
                  $<$<BOOL:${BUILD_WITH_FARGPARSE}>:FARGPARSE::fargparse>
     TYPE SHARED
     )

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
if (BUILD_WITH_FARGPARSE)
  target_link_libraries(${this} PRIVATE FARGPARSE::fargparse)
  target_compile_definitions (${this} PRIVATE USE_FARGPARSE)
endif()

add_subdirectory(Cap)
add_subdirectory(History)
add_subdirectory(Orbit)
add_subdirectory(ExtData)
add_subdirectory(cap3g)
add_subdirectory(History3G)
add_subdirectory(configurable)
add_subdirectory(ExtData3G)
if(USE_EXTDATA2G)
  add_subdirectory(ExtData2G)
endif()
add_subdirectory(StatisticsGridComp)
add_subdirectory(FakeParent)
