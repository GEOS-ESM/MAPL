esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hinterp.F90
    DownBit.F90
    ShaveMantissa.c
    MaplShared.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.mp_utils MPI::MPI_Fortran PFLOGGER::pflogger TYPE SHARED)

# We don't want to disable good NAG debugging flags everywhere, but we still need to do it for
# interfaces (e.g. MPI) that allow multiple types for the same argument (eg buffer).
if (DUSTY)
  set_property( SOURCE Shmem/Shmem.F90 Shmem/Shmem_implementation.F90 MAPL_LoadBalance.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
  add_subdirectory (tests)
endif ()
