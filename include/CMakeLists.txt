set( MAPL_PUBLIC_HEADERS
   MAPL_ErrLog.h
   MAPL_ErrLogMain.h
   MAPL_Exceptions.h
   MAPL_Generic.h
   NUOPC_ErrLog.h
   unused_dummy.H
   )
file(COPY ${MAPL_PUBLIC_HEADERS} DESTINATION ${esma_include}/MAPL)

