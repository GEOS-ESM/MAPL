esma_set_this ()

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
  Throw.F90
  pFIO_ErrorHandling.F90
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
#  OpenMPServer.F90
  ServerThread.F90
  ServerThreadVector.F90
  BaseThread.F90
  ClientThread.F90
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

esma_add_library (${this} SRCS ${srcs})
#target_link_libraries (${this} gftl gftl-shared ${NETCDF_LIBRARIES})
target_link_libraries (${this} PUBLIC Baselibs)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} "-Xlinker -rpath -Xlinker ${dir}")
endforeach()


if (NOT CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  # GFortran gives bizarre LD error for the following targets.
  ecbuild_add_executable (
    TARGET pfio_server_demo.x
    SOURCES pfio_server_demo.F90
    LIBS ${this} ${NETCDF_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
  set_target_properties (pfio_server_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
  set_target_properties(pfio_server_demo.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
    
  ecbuild_add_executable (
    TARGET pfio_collective_demo.x
    SOURCES pfio_collective_demo.F90
    LIBS ${this} ${NETCDF_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
  set_target_properties (pfio_collective_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
  set_target_properties(pfio_collective_demo.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
endif ()

#--------------------
include_directories (${INC_NETCDF})

#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.

file (COPY unused_dummy.H DESTINATION ${include_${this}})

# Users guide 
if (LATEX_FOUND)
#  add_subdirectory (TeX)
endif ()


# Unit testing
if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

