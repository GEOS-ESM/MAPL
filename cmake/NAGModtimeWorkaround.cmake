# NAGModtimeWorkaround.cmake
#
# Purpose:
#   Suppress NAG Fortran "module needs recompilation" warnings when consuming
#   external ESMF .mod files.  Two common causes are handled:
#
#   1. Epoch/ancient .mod timestamps (e.g. from Spack binary cache installs)
#      where the .mod files were written at time 0 or before 1980.
#
#   2. "Source-inclusive" installations (e.g. ESMA Baselibs) where the ESMF
#      source tree lives alongside the compiled modules.  NAG embeds absolute
#      source-file paths in .mod files; if those paths still exist with a
#      modification time newer than the .mod itself, NAG issues the same
#      "needs recompilation" warning even though timestamps are modern.
#
# Usage:
#   find_package(ESMF REQUIRED)
#   include(cmake/NAGModtimeWorkaround.cmake)
#
# This module defines:
#   mapl_nag_modtime_workaround
#
# Link that interface target to Fortran targets if you want explicit control:
#   target_link_libraries(my_target PRIVATE mapl_nag_modtime_workaround)
#
# Or leave MAPL_NAG_MODTIME_APPLY_DIRECTORY=ON to apply add_compile_options()
# for targets created after this include().

set(MAPL_NAG_MODTIME_POLICY "AUTO" CACHE STRING
    "NAG .mod timestamp policy: AUTO, CHECK, or NOCHECK")
set_property(CACHE MAPL_NAG_MODTIME_POLICY PROPERTY STRINGS AUTO CHECK NOCHECK)

set(MAPL_NAG_MODTIME_OLD_YEAR_THRESHOLD "1980" CACHE STRING
    "Year before which external .mod timestamps are considered epoch/ancient")

option(MAPL_NAG_MODTIME_APPLY_DIRECTORY
  "Apply -nocheck_modtime with add_compile_options() for targets created after this include"
  ON)

add_library(mapl_nag_modtime_workaround INTERFACE)

function(_mapl_collect_esmf_module_dirs outvar)
  set(_dirs)

  if(TARGET ESMF::ESMF)
    get_target_property(_incs ESMF::ESMF INTERFACE_INCLUDE_DIRECTORIES)
    if(_incs)
      list(APPEND _dirs ${_incs})
    endif()

    # Some ESMF find logic may expose include/module dirs through compile options.
    get_target_property(_opts ESMF::ESMF INTERFACE_COMPILE_OPTIONS)
    if(_opts)
      foreach(_opt IN LISTS _opts)
        if(_opt MATCHES "^-I(.+)")
          list(APPEND _dirs "${CMAKE_MATCH_1}")
        endif()
      endforeach()
    endif()
  endif()

  # Fallbacks for non-target-based ESMF find modules.
  foreach(_var IN ITEMS
      ESMF_INCLUDE_DIR
      ESMF_INCLUDE_DIRS
      ESMF_MOD_DIR
      ESMF_MODULE_DIR
      ESMF_MODULE_DIRS
  )
    if(DEFINED ${_var})
      list(APPEND _dirs ${${_var}})
    endif()
  endforeach()

  # Normalize and keep only real directories.
  set(_real_dirs)
  foreach(_dir IN LISTS _dirs)
    if(IS_DIRECTORY "${_dir}")
      get_filename_component(_abs "${_dir}" ABSOLUTE)
      list(APPEND _real_dirs "${_abs}")
    endif()
  endforeach()

  list(REMOVE_DUPLICATES _real_dirs)
  set(${outvar} "${_real_dirs}" PARENT_SCOPE)
endfunction()

function(_mapl_esmf_has_old_mod_files outvar)
  _mapl_collect_esmf_module_dirs(_esmf_dirs)

  if(_esmf_dirs)
    message(DEBUG "NAG modtime workaround: scanning ESMF module/include dirs:")
    foreach(_dir IN LISTS _esmf_dirs)
      message(DEBUG "  ${_dir}")
    endforeach()
  else()
    message(DEBUG "NAG modtime workaround: no ESMF module/include dirs found")
  endif()

  foreach(_dir IN LISTS _esmf_dirs)
    file(GLOB _mods LIST_DIRECTORIES false "${_dir}/*.mod")

    foreach(_mod IN LISTS _mods)
      file(TIMESTAMP "${_mod}" _year "%Y" UTC)

      if(_year LESS MAPL_NAG_MODTIME_OLD_YEAR_THRESHOLD)
        message(DEBUG
          "NAG modtime workaround: old ESMF .mod detected: ${_mod} year=${_year}")

        set(${outvar} TRUE PARENT_SCOPE)
        return()
      endif()
    endforeach()
  endforeach()

  set(${outvar} FALSE PARENT_SCOPE)
endfunction()

# Detect "source-inclusive" ESMF installations (e.g. ESMA Baselibs).
# NAG embeds the absolute source-file path in every .mod file.  If those
# source files still exist on the filesystem (because the build tree was
# kept alongside the install), NAG checks whether they are newer than the
# compiled .mod.  When they are, it emits the same "needs recompilation"
# warning regardless of the .mod timestamp year.
#
# Heuristic: walk up to ${MAPL_NAG_MODTIME_SRC_SEARCH_DEPTH} directory levels
# above each ESMF module/include directory.  If a "src" subdirectory is found
# at any of those levels that contains at least one Fortran source file, we
# treat the installation as source-inclusive and enable -nocheck_modtime.
set(MAPL_NAG_MODTIME_SRC_SEARCH_DEPTH "4" CACHE STRING
    "How many directory levels above the ESMF module dir to search for a source tree")

function(_mapl_esmf_has_adjacent_source_tree outvar)
  _mapl_collect_esmf_module_dirs(_esmf_dirs)

  foreach(_dir IN LISTS _esmf_dirs)
    set(_search "${_dir}")
    foreach(_depth RANGE 1 ${MAPL_NAG_MODTIME_SRC_SEARCH_DEPTH})
      if(IS_DIRECTORY "${_search}/src")
        # Shallow glob: check one and two levels inside src/ for Fortran files.
        file(GLOB _f90s
          "${_search}/src/*.F90"  "${_search}/src/*.f90"
          "${_search}/src/*.F"    "${_search}/src/*.f"
          "${_search}/src/*/*.F90" "${_search}/src/*/*.f90"
          "${_search}/src/*/*.F"   "${_search}/src/*/*.f"
        )
        if(_f90s)
          message(DEBUG
            "NAG modtime workaround: ESMF source tree found at ${_search}/src")
          set(${outvar} TRUE PARENT_SCOPE)
          return()
        endif()
      endif()
      get_filename_component(_search "${_search}" DIRECTORY)
    endforeach()
  endforeach()

  set(${outvar} FALSE PARENT_SCOPE)
endfunction()

set(_mapl_enable_nag_nocheck_modtime FALSE)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  if(MAPL_NAG_MODTIME_POLICY STREQUAL "NOCHECK")
    set(_mapl_enable_nag_nocheck_modtime TRUE)
    message(STATUS "NAG modtime workaround: forcing -nocheck_modtime")

  elseif(MAPL_NAG_MODTIME_POLICY STREQUAL "CHECK")
    message(STATUS "NAG modtime workaround: disabled by MAPL_NAG_MODTIME_POLICY=CHECK")

  elseif(MAPL_NAG_MODTIME_POLICY STREQUAL "AUTO")
    _mapl_esmf_has_old_mod_files(_mapl_esmf_old_mods)
    _mapl_esmf_has_adjacent_source_tree(_mapl_esmf_has_srctree)

    if(_mapl_esmf_old_mods OR _mapl_esmf_has_srctree)
      set(_mapl_enable_nag_nocheck_modtime TRUE)
      if(_mapl_esmf_old_mods)
        message(STATUS
          "NAG modtime workaround: old/epoch ESMF .mod timestamps found; enabling -nocheck_modtime")
      else()
        message(STATUS
          "NAG modtime workaround: ESMF source tree found alongside compiled modules; enabling -nocheck_modtime")
      endif()
    else()
      message(STATUS
        "NAG modtime workaround: no stale indicators found; leaving checks enabled")
    endif()

  else()
    message(FATAL_ERROR
      "MAPL_NAG_MODTIME_POLICY must be AUTO, CHECK, or NOCHECK")
  endif()
endif()

if(_mapl_enable_nag_nocheck_modtime)
  target_compile_options(mapl_nag_modtime_workaround INTERFACE
    $<$<COMPILE_LANGUAGE:Fortran>:-nocheck_modtime>)

  if(MAPL_NAG_MODTIME_APPLY_DIRECTORY)
    add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-nocheck_modtime>)
  endif()
endif()
