esma_set_this (OVERRIDE MAPL.utils)

set (srcs
  # From shared/ (MPI-free)
  hash.c
  sort.c
  MAPL_DirPath.F90
  MAPL_Hash.F90
  KeywordEnforcer.F90
  OS.F90
  MAPL_MinMax.F90
  MAPL_Range.F90
  MAPL_Sort.F90
  MAPL_ExceptionHandling.F90
  String.F90
  StringUtilities.F90
  StringDictionary.F90
  FileSystemUtilities.F90
  DSO_Utilities.F90
  MAPL_ISO8601_DateTime.F90
  MAPL_DateTime_Parsing.F90
  MAPL_Sleep.F90
  MAPL_CF_Time.F90
  MAPL_ESMF_InfoKeys.F90
  TimeUtils.F90
  MaplShared.F90
  Interp/Interp.F90
  Interp/Interp_implementation.F90
  # From utilities/
  utilities.F90
  MemInfo.F90
  TimeUtilities.F90
  OSUtilities.F90
  # From generic3g/
  Validation.F90
  )

esma_add_library (${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL.constants GFTL_SHARED::gftl-shared GFTL_SHARED::gftl-shared-v2 PFLOGGER::pflogger
  TYPE SHARED
  )

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 2025.2 AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2025.3)
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX=\\"${CMAKE_SHARED_LIBRARY_SUFFIX}\\")
else()
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()

add_subdirectory(arrays)
add_subdirectory(regex)
add_subdirectory(udunits2f)
add_subdirectory(Constants)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
