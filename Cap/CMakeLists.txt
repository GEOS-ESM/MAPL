esma_set_this ()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()


set (srcs
    AbstractCommSplitter.F90
    CommGroupDescription.F90
    MAPL_Cap.F90
    MAPL_CapOptions.F90
    MAPL_FlapCapOptions.F90
    SimpleCommSplitter.F90
    SplitCommunicator.F90
)

esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES Shared MAPL_Profiler GMAO_pFIO Comms IO GridsRegridding MAPL_Base GridComps MAPL_cfio_r4 pflogger gftl-shared FLAP::FLAP MPI::MPI_Fortran)
target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${OpenMP_Fortran_FLAGS}>)
if(DISABLE_GLOBAL_NAME_WARNING)
   target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${DISABLE_GLOBAL_NAME_WARNING}>)
endif()
target_compile_definitions (${this} PRIVATE TWO_SIDED_COMM MAPL_MODE)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)


if (NOT (CMAKE_BUILD_TYPE MATCHES Debug))
   target_compile_definitions(${this} PRIVATE BUILD_TYPE_IS_NOT_DEBUG)
endif ()

# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PUBLIC "-Xlinker -rpath -Xlinker ${dir}")
endforeach()


if (EXTENDED_SOURCE)
#    target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${EXTENDED_SOURCE}>)
esma_fortran_generator_list (${this} ${EXTENDED_SOURCE})
endif()

#--------------------
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})
target_link_libraries (${this} PUBLIC ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.
#file (COPY ${MAPL_SOURCE_DIR}/include/unused_dummy.H DESTINATION ${esma_include}/${this})
file (COPY ${MAPL_SOURCE_DIR}/include/MAPL_Generic.h DESTINATION ${esma_include}/${this})

