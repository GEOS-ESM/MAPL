esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90 MAPL_Mod.F90 MAPL_NUOPCWrapperMod.F90
  DEPENDENCIES MAPL_Base GridComps Cap GMAO_pFIO MAPL_cfio_r4
  INCLUDES ${INC_ESMF}
  )
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)
file (COPY mapl_acg.pl DESTINATION ${esma_include}/${this})
  


