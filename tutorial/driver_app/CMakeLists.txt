set (srcs
    Example_Driver.F90
    )

ecbuild_add_executable (TARGET Example_Driver.x SOURCES ${srcs})
target_link_libraries (Example_Driver.x PRIVATE MAPL $<$<BOOL:BUILD_WITH_OPENMP>:OpenMP::OpenMP_Fortran> FLAP::FLAP esmf)
target_compile_definitions (Example_Driver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

