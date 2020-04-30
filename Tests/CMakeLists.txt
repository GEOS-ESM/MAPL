set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90 
    ExtDataDriverMod.F90 
    VarspecDescription.F90
    )

include_directories (${INC_ESMF})

#add_executable (ExtDataDriver.x ${srcs})
ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ${srcs})

#install(PROGRAMS ExtDataDriver.x DESTINATION bin)

target_link_libraries (ExtDataDriver.x GridComps Cap MAPL ${MKL_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
target_include_directories (ExtDataDriver.x PRIVATE ${MAPL_SOURCE_DIR}/include)
set_target_properties(ExtDataDriver.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
#install(TARGETS ExtDataDriver.x DESTINATION bin)
    
