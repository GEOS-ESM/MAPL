set(srcs
  Point.F90
  Pixel.F90
  MeshElement.F90
  PixelVector.F90
  MeshElementVector.F90
  driver.F90
)
find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)
ecbuild_add_executable (TARGET to_mesh.x SOURCES ${srcs})

target_link_libraries(to_mesh.x PRIVATE
    MAPL NetCDF::NetCDF_Fortran
    GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
    MPI::MPI_Fortran
)
