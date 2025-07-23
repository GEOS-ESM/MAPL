# MAPL ESMF Workarounds
#
# This module detects and applies workarounds for known ESMF issues
# Detection happens automatically when this module is included
#

# Detect if we need the ESMF 8.9.0 + Intel Fortran + Debug workaround
# see https://github.com/esmf-org/esmf/issues/464
function(_detect_esmf_fpe0_workaround)
  # Check if Fortran compiler is Intel
  set(FORTRAN_COMPILER_IS_INTEL FALSE)
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" OR
     "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "IntelLLVM")
    set(FORTRAN_COMPILER_IS_INTEL TRUE)
  endif()

  # Check all conditions
  if(ESMF_VERSION EQUAL 8.9.0 AND
     FORTRAN_COMPILER_IS_INTEL AND
     CMAKE_BUILD_TYPE STREQUAL "Debug")

    message(STATUS "Using workaround for ESMF 8.9.0 plus Intel Fortran plus Debug CMAKE_BUILD_TYPE")
    set(USE_ESMF_FPE0_WORKAROUND TRUE PARENT_SCOPE)
  else()
    set(USE_ESMF_FPE0_WORKAROUND FALSE PARENT_SCOPE)
  endif()
endfunction()

# Macro to apply the ESMF FPE0 workaround
# Call this in directories that need the workaround applied
macro(apply_esmf_fpe0_workaround)
  if(USE_ESMF_FPE0_WORKAROUND)
    string(REPLACE "${FPE0}" "" CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
    string(REPLACE "-init=snan,arrays" "" CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
  endif()
endmacro()

# Automatically detect workarounds when this module is included
_detect_esmf_fpe0_workaround()
