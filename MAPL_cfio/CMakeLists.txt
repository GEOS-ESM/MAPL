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

if (APPLE AND CMAKE_Fortran_COMPILER_ID MATCHES Intel AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 20.2.1)
   set (LIBRARY_TYPE STATIC)
   ecbuild_warn (
      "Found Intel oneAPI on macOS.\n"
      "MAPL developers have found an issue with Intel oneAPI on macOS\n"
      "where GEOSgcm.x would not work. Debugging found the issue was\n"
      "that command_argument_count() would return -1 which should *NEVER*\n"
      "happen per Fortran Standard and then this broke FlAP.\n"
      "A workaround was found that if the ${this} library was compiled\n"
      "as TYPE STATIC, the model would work. So we are setting ${this} as\n"
      "a TYPE STATIC library. Note: This might interfere with coupled model.")
else ()
   set (LIBRARY_TYPE ${MAPL_LIBRARY_TYPE})
endif ()

esma_add_library (${lib}
  SRCS ${srcs}
  DEPENDENCIES esmf NetCDF::NetCDF_Fortran
  TYPE ${LIBRARY_TYPE}
  )

if (precision MATCHES "r8")
  string (REPLACE " " ";" flags ${FREAL8})
  foreach (flag ${flags})
    target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
  endforeach ()
endif ()
