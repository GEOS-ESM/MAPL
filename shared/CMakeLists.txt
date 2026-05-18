esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hinterp.F90
    ErrorHandling.F90
    MAPL_Throw.F90
    MAPL_LoadBalance.F90
    AbstractCommSplitter.F90
    CommGroupDescription.F90
    SplitCommunicator.F90
    SimpleCommSplitter.F90
    Partition.F90
    DownBit.F90
    ShaveMantissa.c
# Fortran submodules
    Shmem/Shmem.F90   Shmem/Shmem_implementation.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.utils MPI::MPI_Fortran PFLOGGER::pflogger TYPE SHARED)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE Shmem/Shmem.F90 Shmem/Shmem_implementation.F90 MAPL_LoadBalance.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

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
