esma_set_this (OVERRIDE MAPL.raster_to_mesh)
set(srcs
  Direction.F90
  MeshVertex.F90
  MeshVertexVector.F90
  MeshElement.F90
  MeshElementVector.F90
  Mesh.F90
)
find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.shared MAPL.base
)

target_link_libraries (${this} PUBLIC GFTL::gftl-v2 GFTL_SHARED::gftl-shared-v2 ESMF::ESMF NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

ecbuild_add_executable (TARGET raster_to_mesh.x SOURCES raster_to_mesh.F90)

target_link_libraries(raster_to_mesh.x PRIVATE
    ${this} MAPL  NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
    FARGPARSE::fargparse
)

ecbuild_add_executable (TARGET mesh_benchmark.x SOURCES mesh_benchmark.F90)

target_link_libraries(mesh_benchmark.x PRIVATE
    ${this} MAPL.geom NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
    FARGPARSE::fargparse
)
