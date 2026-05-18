esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hinterp.F90
    DownBit.F90
    ShaveMantissa.c
    MaplShared.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.mp_utils MPI::MPI_Fortran PFLOGGER::pflogger TYPE SHARED)

target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
  add_subdirectory (tests)
endif ()
