esma_set_this ()
string (REPLACE MAPL_cfio_ "" precision ${this})


set (srcs
  ESMF_CFIOBaseMod.F90
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
  DEPENDENCIES ESMF::ESMF NetCDF::NetCDF_Fortran
  TYPE SHARED
  )

if (precision MATCHES "r8")
  string (REPLACE " " ";" flags ${FREAL8})
  foreach (flag ${flags})
    target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
  endforeach ()
endif ()

if (USE_F2PY)
  if (precision STREQUAL "r4")
    find_package(F2PY3)
    if (F2PY3_FOUND)
      esma_add_f2py3_module(ShaveMantissa_
        SOURCES ShaveMantissa_py.F90 ShaveMantissa.c
        DESTINATION lib/Python/${this}
        INCLUDEDIRS ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_BINARY_DIR}/lib ${include_${this}}
        )
      add_dependencies(ShaveMantissa_ ${this})

      # Install the Python files
      install (
        FILES shavemantissa.py
        DESTINATION lib/Python/${this}
        )
      endif ()
  endif ()
endif ()
