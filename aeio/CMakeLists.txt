esma_set_this (OVERRIDE MAPL.aeio)

set (srcs
    Writer.F90
    RHConnector.F90
    RHConnectorMap.F90
    CollectionDescriptor.F90
    #CollectionDescriptorMap.F90
    CollectionDescriptorVector.F90
    IOController.F90
    Client.F90
    ClientMap.F90
    Server.F90
    ServerMap.F90
    Collection.F90
    CollectionMap.F90
    CollectionRegistry.F90
    FieldEntry.F90
    FieldEntryMap.F90
    FieldGroup.F90
    FieldGroupEntry.F90
    FieldGroupEntryMap.F90
    FieldRegistry.F90
    Frequency.F90
    Group.F90
    GroupMap.F90
    GroupRegistry.F90
    HistoryConfig.F90
    Template.F90
    TransferGridComp.F90
    AEIO.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base MAPL.profiler TYPE SHARED)
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
    add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
