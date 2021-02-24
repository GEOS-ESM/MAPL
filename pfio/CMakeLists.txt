esma_set_this (OVERRIDE MAPL.pfio)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 20)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      add_definitions(-D__ifort_18)
    endif()
  endif()
endif()

set (srcs
  # pFIO Files
  KeywordEnforcer.F90
  pFIO_ShaveMantissa.c
  DownBit.F90
  pFIO_Constants.F90
  UnlimitedEntity.F90
  Attribute.F90
  Variable.F90
  CoordinateVariable.F90
  StringVariableMap.F90
  pfio_base.F90 
  
  FileMetadata.F90
  FileMetadataVector.F90
  NetCDF4_FileFormatter.F90
  pFIO_Utilities.F90
  pFIO.F90

  AbstractMessage.F90
  MessageVisitor.F90
  TerminateMessage.F90
  DoneMessage.F90
  PrefetchDoneMessage.F90
  CollectivePrefetchDoneMessage.F90
  StageDoneMessage.F90
  CollectiveStageDoneMessage.F90
  DummyMessage.F90
  HandShakeMessage.F90
  AddExtCollectionMessage.F90
  IDMessage.F90
  AbstractDataMessage.F90
  AbstractCollectiveDataMessage.F90
  PrefetchDataMessage.F90
  StageDataMessage.F90
  CollectivePrefetchDataMessage.F90
  CollectiveStageDataMessage.F90
  AddHistCollectionMessage.F90
  ModifyMetadataMessage.F90
  ForwardDataAndMessage.F90
  ForwardDataMessage.F90
  ProtocolParser.F90

  MpiMutex.F90
  AbstractDirectoryService.F90
  DirectoryService.F90
  AbstractSocket.F90
  AbstractSocketVector.F90
  MpiSocket.F90
  SimpleSocket.F90
  IntegerSocketMap.F90

  AbstractServer.F90
  BaseServer.F90
  MpiServer.F90
  MultiLayerServer.F90
  MultiCommServer.F90
  MultiGroupServer.F90
#  OpenMPServer.F90
  ServerThread.F90
  ServerThreadVector.F90
  BaseThread.F90
  ClientThread.F90
  FastClientThread.F90
  ClientThreadVector.F90
  ClientManager.F90
  ExtDataCollection.F90
  HistoryCollection.F90

  IntegerMessageMap.F90
  IntegerRequestMap.F90
  MessageVector.F90
  AbstractDataReference.F90
  ArrayReference.F90
  LocalMemReference.F90
  ShmemReference.F90
  RDMAReference.F90
  AbstractRequestHandle.F90
  StringIntegerMapUtil.F90
  StringVectorUtil.F90
  )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared NetCDF::NetCDF_Fortran TYPE SHARED)
target_link_libraries (${this} PUBLIC gftl-shared
                               PRIVATE MPI::MPI_Fortran)
# CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(${this} PRIVATE OpenMP::OpenMP_Fortran)
endif ()
target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PRIVATE "-Xlinker -rpath -Xlinker ${dir}")
endforeach()

ecbuild_add_executable (
   TARGET pfio_server_demo.x
   SOURCES pfio_server_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_server_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(pfio_server_demo.x OpenMP::OpenMP_Fortran)
endif ()
   
ecbuild_add_executable (
   TARGET pfio_collective_demo.x
   SOURCES pfio_collective_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_collective_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# CMake has an OpenMP issue with Fortran: https://gitlab.kitware.com/cmake/cmake/-/issues/21280
if (NOT CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
   target_link_libraries(pfio_collective_demo.x OpenMP::OpenMP_Fortran)
endif ()

ecbuild_add_executable (
  TARGET pfio_writer.x
  SOURCES pfio_writer.F90
  LIBS ${this} NetCDF::NetCDF_Fortran MPI::MPI_Fortran)
set_target_properties (pfio_writer.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.

# Users guide 
if (LATEX_FOUND)
#  add_subdirectory (TeX)
endif ()


# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
