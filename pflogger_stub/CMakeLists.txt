esma_set_this (OVERRIDE pflogger)

set (srcs
  PFL_KeywordEnforcer.F90
  WrapArray.F90
  pflogger_stub.F90
  )

if (ESMA_USE_GFE_NAMESPACE)
   esma_add_library (${this} SRCS ${srcs} DEPENDENCIES GFTL_SHARED::gftl-shared)
   add_library(PFLOGGER::pflogger ALIAS ${this})
else ()
   esma_add_library (${this} SRCS ${srcs} DEPENDENCIES gftl-shared)
endif ()

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

