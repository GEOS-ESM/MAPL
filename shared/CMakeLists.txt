esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hash.c
    hinterp.F90
    MAPL_DirPath.F90
    MAPL_ErrorHandling.F90
    MAPL_Hash.F90
    MAPL_KeywordEnforcer.F90
    MAPL_LoadBalance.F90
    MAPL_MinMax.F90
    MAPL_Range.F90
    MAPL_Sort.F90
    MAPL_Throw.F90
    AbstractCommSplitter.F90
    CommGroupDescription.F90
    SplitCommunicator.F90
    SimpleCommSplitter.F90
    sort.c
    MAPL_ExceptionHandling.F90
    String.F90
    MaplShared.F90
    TimeUtils.F90
    FileSystemUtilities.F90
    DSO_Utilities.F90
    MAPL_ISO8601_DateTime.F90
    MAPL_DateTime_Parsing.F90
    DownBit.F90
    ShaveMantissa.c
    MAPL_Sleep.F90
    MAPL_CF_Time.F90
    Partition.F90
# Fortran submodules
    Interp/Interp.F90 Interp/Interp_implementation.F90
    Shmem/Shmem.F90   Shmem/Shmem_implementation.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.constants GFTL_SHARED::gftl-shared MPI::MPI_Fortran PFLOGGER::pflogger TYPE ${MAPL_LIBRARY_TYPE})
target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

# We need to work around a bug in ifx 2025.2 where we have to use a different preprocesor
# so here we need to escape the double quotes
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 2025.2 AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2025.3)
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX=\\"${CMAKE_SHARED_LIBRARY_SUFFIX}\\")
else()
  target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()

if (PFUNIT_FOUND)
  add_subdirectory (tests)
endif ()

add_subdirectory(Constants)
#add_subdirectory(tests EXCLUDE_FROM_ALL)
