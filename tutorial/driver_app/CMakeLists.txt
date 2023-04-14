set (srcs
    Example_Driver.F90
    )

ecbuild_add_executable (TARGET Example_Driver.x SOURCES ${srcs})
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_link_libraries(Example_Driver.x PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_link_libraries(Example_Driver.x PRIVATE MAPL)
target_compile_definitions (Example_Driver.x PRIVATE $<$<BOOL:${USE_EXTDATA2G}>:BUILD_WITH_EXTDATA2G>)

