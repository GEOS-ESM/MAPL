set( MAPL_PUBLIC_HEADERS
   MAPL_ErrLog.h
   MAPL_ErrLogMain.h
   MAPL_Exceptions.h
   MAPL_Generic.h
   NUOPC_ErrLog.h
   unused_dummy.H
   MAPL_Resource_MakeStrings.h
   MAPL_Resource_postscript.h
   MAPL_Resource_preamble.h
   MAPL_Resource_SetValue.h
   )

foreach( _header ${MAPL_PUBLIC_HEADERS} )
  configure_file( ${_header} ${esma_include}/MAPL/${_header} COPYONLY )
endforeach()

