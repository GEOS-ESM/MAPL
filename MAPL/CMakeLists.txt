esma_set_this()


esma_add_library (${this}
  SRCS MAPL.F90
  DEPENDENCIES MAPL.base MAPL.pfio MAPL_cfio_r4
  INCLUDES ${INC_ESMF}
  TYPE SHARED
  )

  


