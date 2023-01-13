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
    ecbuild_error ("mapl_acg() - unparsed arguments: ${ARGS_UNPARSED_ARGUMENTS}")
  endif ()

  set (generated) # empty unless
  set (options "")
  set (suffix_for_generated_include_files "___.h")

  # Handle oneValueArgs with no value (Python provides default)
  foreach (opt flag default IN ZIP_LISTS oneValueArgs flags defaults)

    if (ARGS_${opt})
      string (REPLACE "{component}" component_name fname ${ARGS_${opt}})
      list (APPEND generated ${fname})
      list (APPEND options ${flag} ${ARGS_${opt}})
    elseif (${opt} IN_LIST ARGS_KEYWORDS_MISSING_VALUES)
      string (REPLACE "{component}" component_name fname ${default})

      # What the ACG does is take the specs_file and then removes the extension and then
      # it removes both "_Registry" and "_StateSpecs" from the resulting string. We need to do the
      # same here in CMake.
      #   Example: ${specs_file1} = GEOS_MyGridComp_Registry.rc
      #            ${specs_file2} = GEOS_MyGridComp_StateSpecs.rc
      #
      #   ${specs_file1} -> GEOS_MyGridComp_Registry.rc -> GEOS_MyGridComp_Registry -> GEOS_MyGridComp
      #   ${specs_file2} -> GEOS_MyGridComp_StateSpecs.rc -> GEOS_MyGridComp_StateSpecs -> GEOS_MyGridComp

      # First get the filename without the extension
      get_filename_component (specs_file_no_ext ${specs_file} NAME_WE)

      # Now remove the _Registry and _StateSpecs
      string (REPLACE "_Registry" "" specs_file_base ${specs_file_no_ext})
      string (REPLACE "_StateSpecs" "" specs_file_base ${specs_file_base})

      # Now we let CMake know the generated file will be named off of the specs_file_base
      list (APPEND generated "${specs_file_base}_${fname}${suffix_for_generated_include_files}")

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
