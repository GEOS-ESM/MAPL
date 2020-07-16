esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90

# GCHP: Add ESMF to dependencies and do not put in includes
#  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4
#  INCLUDES ${INC_ESMF}
  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4 ESMF
  )

  


