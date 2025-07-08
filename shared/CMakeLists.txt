esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hash.c
    hinterp.F90
    MAPL_DirPath.F90
    ErrorHandling.F90
    MAPL_Hash.F90
    KeywordEnforcer.F90
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
    StringUtilities.F90
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
    MAPL_ESMF_InfoKeys.F90
# Fortran submodules
    Interp/Interp.F90 Interp/Interp_implementation.F90
    Shmem/Shmem.F90   Shmem/Shmem_implementation.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.constants GFTL_SHARED::gftl-shared GFTL_SHARED::gftl-shared-v2 MPI::MPI_Fortran PFLOGGER::pflogger TYPE SHARED)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE Shmem/Shmem.F90 Shmem/Shmem_implementation.F90 MAPL_LoadBalance.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")

if (PFUNIT_FOUND)
  add_subdirectory (tests)
endif ()

add_subdirectory(Constants)
#add_subdirectory(tests EXCLUDE_FROM_ALL)
