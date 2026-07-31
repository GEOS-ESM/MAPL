esma_set_this (OVERRIDE MAPL.nuopc)

set(srcs
# FieldDictionary* moved to infrastructure/field_dictionary/
    GenericNuopcModel.F90
    NuopcMetaModel.F90
)

find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.component MAPL.utils MAPL.generic
    TYPE SHARED
)

target_link_libraries (${this} PUBLIC GFTL::gftl-v2 GFTL_SHARED::gftl-shared-v2 ESMF::ESMF)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

# We need to work around a bug in ifx 2025.2 where we have to use a different preprocessor
# so here we need to escape the double quotes
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 2025.2 AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2025.3)
    target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX=\\"${CMAKE_SHARED_LIBRARY_SUFFIX}\\")
else()
    target_compile_definitions(${this} PRIVATE SYSTEM_DSO_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()

#if (PFUNIT_FOUND)
#    add_subdirectory(tests EXCLUDE_FROM_ALL)
#endif ()
