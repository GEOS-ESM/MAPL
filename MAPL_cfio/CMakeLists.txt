esma_set_this ()
string (REPLACE MAPL_cfio_ "" precision ${this})


set (srcs
  ESMF_CFIOBaseMod.f
  ESMF_CFIOFileMod.F90    
  ESMF_CFIOGridMod.F90    
  ESMF_CFIOMod.F90        
  ESMF_CFIOSdfMod.F90     
  ESMF_CFIOUtilMod.F90    
  ESMF_CFIOVarInfoMod.F90 
  ShaveMantissa.c
  )

# Allow for varying types sent to common MPI interface.  (NAG workaround)
if (MISMATCH)
  set_source_files_properties(ESMF_CFIOrGrADSMod.F90 PROPERTIES COMPILE_FLAGS ${MISMATCH})
endif ()

if (MFHDF3_NOT_PRESENT)
  list (APPEND SRCS netcdf_stub.F90)
  warning ("Building CFIO with HDF but MFHDF3 not present --- providing stubs")
endif ()

set (EOS )

set (lib MAPL_cfio_${precision})

esma_add_library (${lib}
  SRCS ${srcs}
  DEPENDENCIES ${ESMF_LIBRARIES}
  )
target_link_libraries(${lib} PUBLIC Baselibs)

if (precision MATCHES "r8")
  string (REPLACE " " ";" flags ${FREAL8})
  foreach (flag ${flags})
    target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
  endforeach ()
endif ()


