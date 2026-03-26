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

ecbuild_add_executable (TARGET raster_to_mesh.x SOURCES ${srcs} raster_to_mesh.F90)

target_link_libraries(raster_to_mesh.x PRIVATE
    MAPL NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
    FARGPARSE::fargparse
)

ecbuild_add_executable (TARGET mesh_benchmark.x SOURCES ${srcs} mesh_benchmark.F90)

target_link_libraries(mesh_benchmark.x PRIVATE
    MAPL MAPL.geom NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
    FARGPARSE::fargparse
)
