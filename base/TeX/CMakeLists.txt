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

message(STATUS "MATMAT in TeX CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "MATMAT in TeX PROJECT_SOURCE_DIR: ${PROJECT_SOURCE_DIR}")
set (mapl_api_srcs)
foreach(src
    gridcomps/Cap/MAPL_Cap.F90
    generic/MAPL_Generic.F90
    base/MAPL_CFIO.F90
    base/MAPL_LocStreamMod.F90
    base/Base/Base_Base_implementation.F90
    base/ESMFL_Mod.F90
    gridcomps/History/MAPL_HistoryGridComp.F90
    generic/GenericCplComp.F90
    gridcomps/ExtData/ExtDataGridCompMod.F90
    )
  LIST (APPEND mapl_api_srcs ${PROJECT_SOURCE_DIR}/${src})
endforeach()

add_custom_command (OUTPUT MAPL_API__.tex
  COMMAND ${protex_exe} ${protex_flags} ${mapl_api_srcs} > MAPL_API__.tex
  DEPENDS ${API_SRCS}
  )

