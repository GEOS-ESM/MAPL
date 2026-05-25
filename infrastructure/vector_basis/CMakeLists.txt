esma_set_this (OVERRIDE MAPL.vector_basis)

set(srcs
  VectorBasis.F90
  create_fields.F90
  destroy_fields.F90
  get_unit_vector.F90
  grid_get_coords_1d.F90
  grid_get_coords_2d.F90
  latlon2xyz.F90
  MAPL_GeomGetCoords.F90
  mid_pt_sphere.F90
  new_GridVectorBasis.F90
  new_NS_Basis.F90
  xyz2latlon.F90
)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.constants MAPL.shared MAPL.esmf_utils GFTL::gftl-v2
  TYPE SHARED
)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_link_libraries (${this} PUBLIC ESMF::ESMF)
