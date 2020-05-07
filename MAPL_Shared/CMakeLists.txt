esma_set_this ()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()

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
    sort.c
  )
esma_add_library (${this} SRCS ${srcs} DEPENDENCIES ${ESMF_LIBRARIES} pflogger  gftl-shared gftl MPI::MPI_Fortran)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PRIVATE "-Xlinker -rpath -Xlinker ${dir}")
endforeach()


