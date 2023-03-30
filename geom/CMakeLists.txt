esma_set_this (OVERRIDE MAPL.geom)


#set(srcs
#  EsmfRegridder.F90
#  FieldBLAS.F90
#  FieldBLAS_IntrinsicFunctions.F90
#  GeomManager.F90
#  MaplGeom.F90
#  Regridder.F90
#  StateSupplement.F90
#  )
set(srcs
  FieldBLAS.F90
  )
# Workaround for strict NAG Fortran with ESMF implicit interface for private state.
#set_property( SOURCE InnerMetaComponent.F90 OuterMetaComponent.F90
#  PROPERTY COMPILE_FLAGS ${MISMATCH})

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

#find_package (MPI REQUIRED)
#find_package (GFTL REQUIRED)
#find_package (GFTL_SHARED REQUIRED)
#find_package (YAFYAML REQUIRED)
find_package (PFLOGGER REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
#  DEPENDENCIES MAPL.oomph MAPL.shared MAPL.profiler MAPL.base YAFYAML::yafyaml PFLOGGER::pflogger GFTL_SHARED::gftl-shared-v2 GFTL::gftl-v2
  DEPENDENCIES MAPL.shared PFLOGGER::pflogger
  TYPE ${MAPL_LIBRARY_TYPE}
  )
#add_subdirectory(specs)
#add_subdirectory(registry)
#add_subdirectory(connection_pt)

target_include_directories (${this} PUBLIC
  $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
#target_link_libraries (${this} PUBLIC esmf NetCDF::NetCDF_Fortran)
target_link_libraries (${this} PUBLIC esmf)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

