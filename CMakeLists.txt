cmake_policy (SET CMP0048 NEW) # VERSION in project()

project (
  MAPL
  VERSION 1.1
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

# Special case - MAPL_cfio is built twice with two different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (GMAO_pFIO)
add_subdirectory (MAPL_Base)

