esma_set_this (OVERRIDE pflogger)

set (srcs
  PFL_KeywordEnforcer.F90
  WrapArray.F90
  pflogger_stub.F90
  )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES GFTL_SHARED::gftl-shared TYPE ${MAPL_LIBRARY_TYPE})
add_library(PFLOGGER::pflogger ALIAS ${this})

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

