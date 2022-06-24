ecbuild_add_executable(
        TARGET SyntheticApp.x
        SOURCES SyntheticApp.F90 Synthetic_Driver.F90 Provider_GridComp.F90 Reciever_GridComp.F90 UFS_TestingCap.F90
        LIBS GFTL_SHARED::gftl-shared MAPL
        INCLUDES ${esma_include}/MAPL
)
