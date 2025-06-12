set(srcs
  Direction.F90
  Vertex.F90
  VertexVector.F90
  Element.F90
  ElementVector.F90
  Mesh.F90
)
find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)

ecbuild_add_executable (TARGET to_mesh2.x SOURCES ${srcs} driver.F90)

target_link_libraries(to_mesh2.x PRIVATE
    MAPL NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
)

ecbuild_add_executable (TARGET xgrid.x SOURCES ${srcs} xgrid.F90)

target_link_libraries(xgrid.x PRIVATE
    MAPL NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
)

ecbuild_add_executable (TARGET xgrid2.x SOURCES ${srcs} xgrid2.F90)

target_link_libraries(xgrid2.x PRIVATE
    MAPL MAPL.geom NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
)
