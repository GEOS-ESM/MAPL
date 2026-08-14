esma_set_this (OVERRIDE MAPL.constants)

set (srcs
    InternalConstants.F90
    MathConstants.F90
    PhysicalConstants.F90
    EarthConstants.F90
    EarthAtmosphericConstants.F90
    Constants.F90
    )

find_package(Python3 COMPONENTS Interpreter REQUIRED)
set (MAPL_ERROR_CODES_GENERATED
     ${CMAKE_CURRENT_BINARY_DIR}/MAPL_ErrorCodes_generated.F90)
add_custom_command(
  OUTPUT ${MAPL_ERROR_CODES_GENERATED}
  COMMAND ${Python3_EXECUTABLE} ${MAPL_SOURCE_DIR}/cmake/generate_error_codes.py
          ${CMAKE_CURRENT_SOURCE_DIR}/mapl_error_codes.yaml ${MAPL_ERROR_CODES_GENERATED}
  DEPENDS ${MAPL_SOURCE_DIR}/cmake/generate_error_codes.py
          ${CMAKE_CURRENT_SOURCE_DIR}/mapl_error_codes.yaml
  VERBATIM)
add_custom_target(MAPL.error_codes_generated DEPENDS ${MAPL_ERROR_CODES_GENERATED})
list(APPEND srcs ${MAPL_ERROR_CODES_GENERATED})

esma_add_library (${this} SRCS ${srcs} TYPE SHARED)
add_dependencies(${this} MAPL.error_codes_generated)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

option(USE_CODATA_2018_CONSTANTS "Use CODATA 2018 Constants" OFF)
target_compile_definitions (${this} PRIVATE $<$<BOOL:${USE_CODATA_2018_CONSTANTS}>:CODATA_2018_CONSTANTS>)
