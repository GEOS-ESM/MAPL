set( MAPL_PUBLIC_HEADERS
   MAPL_ErrLog.h
   MAPL_ErrLogMain.h
   MAPL_Exceptions.h
   MAPL_Generic.h
   MAPL.h
   NUOPC_ErrLog.h
   unused_dummy.H
   )

foreach( _header ${MAPL_PUBLIC_HEADERS} )
  configure_file( ${_header} ${esma_include}/MAPL/${_header} COPYONLY )
endforeach()

