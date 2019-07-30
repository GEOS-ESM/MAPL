# Hack to avoid renaming MAPL_Preamble - 
set (LATEX_COMPILER_FLAGS ${LATEX_COMPILER_FLAGS} "-jobname=MAPL_UsersGuide.tex")

add_latex_document (MAPL_Preamble.tex
  TARGET_NAME MAPL_UsersGuide
  INPUTS MAPL_Intro.tex HistoryIntro.tex MAPL_CapIntro.tex
  CFIOServerIntro.tex ExtDataIntro.tex MAPL_ParserIntro.tex
  MAPL_CFIODescr.tex MAPL_HistoryDescr.tex
  IMAGE_DIRS figs
  DEPENDS MAPL_API__.tex # auto-generated
  USE_INDEX
  INDEX_NAMES MAPL_UsersGuide
  EXCLUDE_FROM_ALL
  )

set (mapl_api_srcs)
foreach(src
    MAPL_Cap.F90 MAPL_Generic.F90 MAPL_CFIO.F90
    MAPL_LocStreamMod.F90 MAPL_Base.F90
    ESMFL_Mod.F90 MAPL_HistoryGridComp.F90
    MAPL_GenericCplComp.F90 MAPL_ExtDataGridCompMod.F90
    )
  LIST (APPEND mapl_api_srcs ${CMAKE_CURRENT_SOURCE_DIR}/../${src})
endforeach()

add_custom_command (OUTPUT MAPL_API__.tex
  COMMAND ${protex_exe} ${protex_flags} ${mapl_api_srcs} > MAPL_API__.tex
  DEPENDS ${API_SRCS}
  )
  
