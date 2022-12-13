set (srcs
    ESMF_Driver.F90
    )

ecbuild_add_executable (TARGET ESMF_Driver.x SOURCES ${srcs})
target_link_libraries (ESMF_Driver.x PRIVATE MAPL OpenMP::OpenMP_Fortran FLAP::FLAP esmf)
target_compile_definitions (ESMF_Driver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

