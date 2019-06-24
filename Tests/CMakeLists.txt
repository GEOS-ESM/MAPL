set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90 
    ExtDataDriverMod.F90 
    VarspecDescription.F90
    )

include_directories (${include_MAPL_Base} ${include_GMAO_pFIO} ${INC_ESMF} ${include_GMAO_hermes})

ecbuild_add_executable (
  TARGET ExtDataDriver.x 
  SOURCES ${srcs}
  LIBS MAPL_Base GMAO_pFIO ${MKL_LIBRARIES} ${OpenMP_Fortran_LIBRARIES}
  )

set_target_properties(ExtDataDriver.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
    
