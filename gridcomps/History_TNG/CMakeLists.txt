esma_set_this (OVERRIDE MAPL.history_tng)

set (srcs
    FieldEntry.F90
    FieldEntryMap.F90
    FieldRegistry.F90
    FieldGroupEntry.F90
    FieldGroupEntryMap.F90
    FieldGroup.F90
    Group.F90
    GroupMap.F90
    GroupRegistry.F90
    Template.F90
    Frequency.F90
    Collection.F90
    CollectionMap.F90
    CollectionRegistry.F90
    HistoryConfig.F90

    NUOPCmap.F90

    HistoryWriter.F90
    HistoryWriter_GridComp.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base MAPL.profiler TYPE SHARED)
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
    add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

add_subdirectory(testing_app)
add_subdirectory(writer_testing_app)

