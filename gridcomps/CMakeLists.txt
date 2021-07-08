esma_set_this(OVERRIDE MAPL.gridcomps)
esma_add_library (${this}
     SRCS MAPL_GridComps.F90
     DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 MAPL.cap
                  $<$<BOOL:${BUILD_WITH_FLAP}>:FLAP::FLAP>
     TYPE ${MAPL_LIBRARY_TYPE} 
     )

target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

add_subdirectory(Cap)
add_subdirectory(History)
