################################################################################################
# Automatically generate files from a file that provides specs
# for the states of a gridde component.
#
# Usage:
#
#    mapl_acg (target specs_file <options>)
#
# Options:
#       IMPORT_SPECS [file]  include file for AddImportSpec() code (default <gc>_Import___.h)
#       EXPORT_SPECS [file]  include file for AddExportSpec() code (default <gc>_Export___.h)
#       INTERNAL_SPECS [file]  include file for AddInternalSpec() code (default <gc>_Internal___.h)
#       GET_POINTERS [file]  include file for GetPointer() code (default <gc>_GetPointer___.h)
#       DECLARE_POINTERS [file]  include file for declaring local pointers (default <gc>_DeclarePointer___.h)
#
################################################################################################


function (mapl_acg target specs_file)
  set (options)
  set (oneValueArgs  IMPORT_SPECS EXPORT_SPECS INTERNAL_SPECS GET_POINTERS DECLARE_POINTERS)
  # This list must align with oneValueArgs above (for later ZIP_LISTS)
  set (flags         -i           -x           -p             -g           -d)
  set (defaults      Import       Export       Internal       GetPointer   DeclarePointer)
  set (multiValueArgs)
  cmake_parse_arguments (ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  string (REPLACE "_GridComp" "" component_name ${target})

  if (ARGS_UNPARSED_ARGUMENTS)
    ecbuild_error ("maple_acg() - unparsed arguments: ${ARGS_UNPARSED_ARGUMENTS}")
  endif ()

  set (generated) # empty unless
  set (options "")


  # Handle oneValueArgs with no value (Python provides default)
  foreach (opt flag default IN ZIP_LISTS oneValueArgs flags defaults)

    if (ARGS_${opt})
      string (REPLACE "{component}" component_name fname ${ARGS_${opt}})
      list (APPEND generated ${fname})
      list (APPEND options ${flag} ${ARGS_${opt}})
    elseif (${opt} IN_LIST ARGS_KEYWORDS_MISSING_VALUES)
      string (REPLACE "{component}" component_name fname ${default})
      list (APPEND generated ${fname})
      list (APPEND options ${flag})
    endif ()

  endforeach ()

  if (DEFINED MAPL_BASE_DIR)
    set (_generator_dir ${MAPL_BASE_DIR}/etc)
  else ()
    set (_generator_dir ${esma_etc}/MAPL)
  endif ()
  set(generator ${_generator_dir}/MAPL_GridCompSpecs_ACG.py)

  add_custom_command (
    OUTPUT ${generated}
    COMMAND ${generator} ${CMAKE_CURRENT_SOURCE_DIR}/${specs_file} ${options}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${specs_file} 
    DEPENDS ${generator} ${specs_file}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating automatic code for ${specs_file}"
    )
  add_custom_target (acg_phony_${target} DEPENDS ${generated})
  add_dependencies (${target} acg_phony_${target})

endfunction ()
