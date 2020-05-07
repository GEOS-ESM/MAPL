esma_set_this ()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()


set (srcs
ESMFL_Mod.F90
MAPL_DefGridName.F90
MAPL_ESMFTimeVectorMod.F90
MAPL_EtaHybridVerticalCoordinate.F90
MAPL_GenericCplComp.F90
MAPL_Generic.F90
MAPL_ioClients.F90
MAPL_LocStreamMod.F90
MAPL_MaxMinMod.F90
MAPL_MemUtils.F90
MAPL_NewArthParser.F90
MAPL_NominalOrbitsMod.F90
MAPL_Profiler.F90
MAPL_SatVapor.F90
MAPL_SimpleAlarm.F90
MAPL_sun_uc.F90
MAPL_TimeMethods.F90
MAPL_VarSpecMod.F90
MAPL_VerticalMethods.F90
c_mapl_locstream_F.c
getrss.c
memuse.c

    )
esma_add_library(
  ${this} SRCS ${srcs}
  DEPENDENCIES Shared Comms MAPL_Profiler GMAO_pFIO IO GridsRegridding MAPL_cfio_r4 pflogger gftl-shared FLAP::FLAP MPI::MPI_Fortran)
target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${OpenMP_Fortran_FLAGS}>)
if(DISABLE_GLOBAL_NAME_WARNING)
   target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${DISABLE_GLOBAL_NAME_WARNING}>)
endif()
target_compile_definitions (${this} PRIVATE TWO_SIDED_COMM MAPL_MODE)
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)


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
file (COPY mapl_stub.pl DESTINATION ${esma_include}/${this})

# Users guide 
if (LATEX_FOUND)
  add_subdirectory (TeX)
endif ()


# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

install (
   PROGRAMS red_ma.pl
   DESTINATION bin
   )

