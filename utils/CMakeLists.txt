esma_set_this (OVERRIDE MAPL.utils)

# MPI-free foundational utilities (Tier 1 in MAPL v3 restructuring).
# This library must NOT depend on MPI.

set (srcs
    # Error handling (MPI-free; MPI handlers in MAPL.mp_utils)
    ErrorHandling.F90
    MAPL_Throw.F90
    hash.c
    sort.c
    MAPL_DirPath.F90
    MAPL_Hash.F90
    KeywordEnforcer.F90
    OS.F90
    MAPL_MinMax.F90
    MAPL_Range.F90
    MAPL_Sort.F90
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
    # Umbrella module for the utils layer (#5004)
    API.F90
    # Fortran submodules
    Interp/Interp.F90 Interp/Interp_implementation.F90
    # From utilities/
    # utilities.F90 moved to mp_utils/API.F90 (MPI-dependent via mapl_ArrayReductions)
    MemInfo.F90
    TimeUtilities.F90
    # From generic3g/
    Validation.F90
    # Mantissa shaving (MPI-free compression utility)
    ShaveMantissa.c
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.constants ESMF GFTL_SHARED::gftl-shared GFTL_SHARED::gftl-shared-v2 PFLOGGER::pflogger
    TYPE SHARED
)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

# We need to work around a bug in ifx 2025.2 where we have to use a different preprocessor
# so here we need to escape the double quotes
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 2025.2 AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2025.3)
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX=\\"${CMAKE_SHARED_LIBRARY_SUFFIX}\\")
else()
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()


add_subdirectory(Constants)
add_subdirectory(regex)
add_subdirectory(udunits2f)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
