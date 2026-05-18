# MAPL.utilities sources have been moved to MAPL.utils (utils/).
# This stub keeps the target alive for any CMake consumers not yet updated.
esma_set_this (OVERRIDE MAPL.utilities)

esma_add_library (${this}
  SRCS ""
  DEPENDENCIES MAPL.utils
  TYPE SHARED
  )
