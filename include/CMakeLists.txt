set( MAPL_PUBLIC_HEADERS
   MAPL_ErrLog.h
   MAPL_ErrLogMain.h
   MAPL_Exceptions.h
   MAPL_Generic.h
   NUOPC_ErrLog.h
   unused_dummy.H
   new_ErrLog.h
   )

# This option is defined in the top-level CMakeLists.txt
if(USE_MAPL_LEGACY_MACROS)
  list(APPEND MAPL_PUBLIC_HEADERS legacy_ErrLog.h)
endif()

foreach( _header ${MAPL_PUBLIC_HEADERS} )
  configure_file( ${_header} ${esma_include}/MAPL/${_header} COPYONLY )
endforeach()

