esma_set_this(OVERRIDE MAPL.gridcomps)
esma_add_library (${this}
     SRCS MAPL_GridComps.F90
     DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.cap
                  $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>
                  $<$<BOOL:${FARGPARSE_FOUND}>:FARGPARSE::fargparse>
     TYPE ${MAPL_LIBRARY_TYPE}
     )

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
if (BUILD_WITH_FLAP)
  target_link_libraries(${this} PRIVATE FLAP::FLAP)
  target_compile_definitions (${this} PRIVATE USE_FLAP)
endif()
if (FARGPARSE_FOUND)
  target_link_libraries(${this} PRIVATE FARGPARSE::fargparse)
  target_compile_definitions (${this} PRIVATE USE_FARGPARSE)
endif()

add_subdirectory(Cap)
add_subdirectory(History)
add_subdirectory(Orbit)
add_subdirectory(ExtData)
if(USE_EXTDATA2G)
  add_subdirectory(ExtData2G)
endif()
