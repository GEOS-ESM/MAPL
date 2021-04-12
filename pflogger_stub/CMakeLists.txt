esma_set_this (OVERRIDE pflogger)

set (srcs
   WrapArray.F90
   PFL_KeywordEnforcer.F90
   pflogger_stub.F90
  )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES gftl-shared TYPE SHARED)
