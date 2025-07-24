set (srcs
    Example_Driver.F90
    )

ecbuild_add_executable (TARGET Example_Driver.x SOURCES ${srcs})
target_link_libraries(Example_Driver.x PRIVATE MAPL OpenMP::OpenMP_Fortran)
target_compile_definitions (Example_Driver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

apply_esmf_fpe0_workaround()
