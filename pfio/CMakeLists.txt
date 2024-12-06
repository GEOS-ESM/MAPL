esma_set_this (OVERRIDE MAPL.pfio)

# This is a workaround for a current ifx bug
# Technically, this bug is only due to a bug between
# ifx, OpenMP, and ArrayReference.F90, but in CMake land, it
# is hard to remove OpenMP flags for a *single* file.
if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  set_source_files_properties(ArrayReference.F90 PROPERTIES COMPILE_DEFINITIONS ODD_IFX_BUG)
endif ()

set (srcs
  # pFIO Files
  pFIO_Constants.F90
  UnlimitedEntity.F90
  Attribute.F90
  IntArray.F90
  Variable.F90
  CoordinateVariable.F90
  StringVariableMap.F90
  pfio_base.F90

  FileMetadata.F90
  FileMetadataVector.F90
  NetCDF4_FileFormatter.F90
  pfio_nf90_supplement.c
  NetCDF_Supplement.F90
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
  ReplaceMetadataMessage.F90
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

if (BUILD_WITH_PFLOGGER)
  find_package (PFLOGGER REQUIRED)
endif ()

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.profiler NetCDF::NetCDF_Fortran NetCDF::NetCDF_C TYPE ${MAPL_LIBRARY_TYPE})

target_link_libraries (${this} PUBLIC GFTL::gftl-v2 GFTL_SHARED::gftl-shared-v2 PFLOGGER::pflogger PRIVATE MPI::MPI_Fortran OpenMP::OpenMP_Fortran)
target_include_directories (${this} PUBLIC
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} PRIVATE "-Xlinker -rpath -Xlinker ${dir}")
endforeach()

if (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
  target_compile_definitions(${this} PRIVATE SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
endif ()

###############################################################
# Check to see if quantize capability is present in netcdf-c. #
###############################################################

include(CheckCSourceCompiles)
set(CMAKE_REQUIRED_INCLUDES ${NETCDF_INCLUDE_DIRS})
check_c_source_compiles("
  #include <netcdf_meta.h>
  #if !NC_HAS_QUANTIZE
  choke me
  #endif
  int main(void) {
    return 0;
  }
  "
  NETCDF_HAS_QUANTIZE)
if (NETCDF_HAS_QUANTIZE)
  message(STATUS "netCDF has quantize capability")
  target_compile_definitions(${this} PRIVATE NF_HAS_QUANTIZE)
else ()
  message(STATUS "netCDF does not have quantize capability")
endif ()

################################################################
# Check to see if zstandard capability is present in netcdf-c. #
################################################################

check_c_source_compiles("
  #include <netcdf_meta.h>
  #if !NC_HAS_ZSTD
  choke me
  #endif
  int main(void) {
    return 0;
  }
  "
  NETCDF_HAS_ZSTD)

# NOTE: Even if the check above succeeds, zstandard is *not*
# possible with Baselibs (builds HDF5, netCDF as static)
# so we want to check for Baselibs first.

if (Baselibs_FOUND)
  message(STATUS "Baselibs found, zstandard capability not possible")
  set(NETCDF_HAS_ZSTD FALSE CACHE BOOL "netCDF has zstandard capability" FORCE)
endif ()

if (NETCDF_HAS_ZSTD)
  message(STATUS "netCDF has zstandard capability")
  target_compile_definitions(${this} PRIVATE NF_HAS_ZSTD)
else ()
  message(STATUS "netCDF does not have zstandard capability")
endif ()

ecbuild_add_executable (
   TARGET pfio_open_close.x
   SOURCES pfio_open_close.F90
   LIBS ${this})

set_target_properties (pfio_open_close.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

ecbuild_add_executable (
   TARGET pfio_server_demo.x
   SOURCES pfio_server_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_server_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
target_link_libraries(pfio_server_demo.x OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
   TARGET pfio_collective_demo.x
   SOURCES pfio_collective_demo.F90
   LIBS ${this} MPI::MPI_Fortran)
set_target_properties (pfio_collective_demo.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
target_link_libraries(pfio_collective_demo.x OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
  TARGET pfio_writer.x
  SOURCES pfio_writer.F90
  LIBS ${this} NetCDF::NetCDF_Fortran NetCDF::NetCDF_C MPI::MPI_Fortran)
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
