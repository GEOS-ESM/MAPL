ecbuild_add_executable(
        TARGET WriterApp.x
        SOURCES WriterApp.F90 Writer_Driver.F90 Provider_GridComp.F90 
        LIBS GFTL::gftl GFTL_SHARED::gftl-shared MAPL
        INCLUDES ${esma_include}/MAPL
)
