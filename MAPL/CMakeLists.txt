esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90
#-----
# GCHP before MAPL 2.2
#  DEPENDENCIES MAPL_Base GMAO_pFIO MAPL_cfio_r4 ESMF
#-----
# New in MAPL 2.2
  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4
  INCLUDES ${INC_ESMF}
#-----
  )

  


