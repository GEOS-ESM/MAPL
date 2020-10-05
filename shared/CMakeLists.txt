esma_set_this (OVERRIDE MAPL.shared)

set (srcs
    hash.c
    hinterp.F
    MAPL_Constants.F90
    MAPL_DirPath.F90
    MAPL_ErrorHandling.F90
    MAPL_Hash.F90
    MAPL_HeapMod.F90
    MAPL_InterpMod.F90
    MAPL_KeywordEnforcer.F90
    MAPL_LoadBalance.F90
    MAPL_MinMax.F90
    MAPL_Range.F90
    MAPL_ShmemMod.F90
    MAPL_Sort.F90
    MAPL_Throw.F90
    AbstractCommSplitter.F90
    CommGroupDescription.F90
    SplitCommunicator.F90
    SimpleCommSplitter.F90
    sort.c
    MAPL_ExceptionHandling.F90

    String.F90
  )
esma_add_library (${this} SRCS ${srcs} DEPENDENCIES gftl-shared pflogger MPI::MPI_Fortran)
#target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)
target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

#file (COPY MAPL_ErrLogMain.h DESTINATION ${esma_include}/${this})
#file (COPY MAPL_ErrLog.h DESTINATION ${esma_include}/${this})
#file (COPY MAPL_Exceptions.h DESTINATION ${esma_include}/${this})

#if (PFUNIT_FOUND)
  #add_subdirectory (tests)
#endif ()

add_subdirectory(tests EXCLUDE_FROM_ALL)
