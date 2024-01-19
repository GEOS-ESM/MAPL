# (C) Copyright 2022- UCAR.
#
# Try to find the udunits headers and library
#
# This module defines:
#
#   - udunits::udunits       - The udunits shared library and include directory, all in a single target.
#   - udunits_FOUND          - True if udunits was found
#   - udunits_INCLUDE_DIR    - The include directory
#   - udunits_LIBRARY        - The library
#   - udunits_LIBRARY_SHARED - Whether the library is shared or not
#
# The following paths will be searched in order if set in CMake (first priority) or environment (second priority):
#
#   - UDUNITS2_INCLUDE_DIRS & UDUNITS2_LIBRARIES - folders containing udunits2.h and libudunits2, respectively.
#   - UDUNITS2_ROOT                 - root of udunits installation
#   - UDUNITS2_PATH                 - root of udunits installation
#
# Notes:
#   - The hint variables are capitalized because this is how they are exposed in the jedi stack.
#     See https://github.com/JCSDA-internal/jedi-stack/blob/develop/modulefiles/compiler/compilerName/compilerVersion/udunits/udunits.lua for details.

find_path (
	udunits_INCLUDE_DIR
	udunits2.h
	HINTS ${UDUNITS2_INCLUDE_DIRS} $ENV{UDUNITS2_INCLUDE_DIRS}
		${UDUNITS2_ROOT} $ENV{UDUNITS2_ROOT}
		${UDUNITS2_PATH} $ENV{UDUNITS2_PATH}
  PATH_SUFFIXES include include/udunits2
	DOC "Path to udunits2.h" )

find_library(udunits_LIBRARY
	NAMES udunits2 udunits
	HINTS ${UDUNITS2_LIBRARIES} $ENV{UDUNITS2_LIBRARIES}
		${UDUNITS2_ROOT} $ENV{UDUNITS2_ROOT}
		${UDUNITS2_PATH} $ENV{UDUNITS2_PATH}
  PATH_SUFFIXES lib64 lib
	DOC "Path to libudunits library" )

# We need to support both static and shared libraries
if (udunits_LIBRARY MATCHES ".*\\.a$")
  set(udunits_LIBRARY_SHARED FALSE)
else()
  set(udunits_LIBRARY_SHARED TRUE)
endif()

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (udunits DEFAULT_MSG udunits_LIBRARY udunits_INCLUDE_DIR)

mark_as_advanced (udunits_LIBRARY udunits_INCLUDE_DIR)

if(udunits_FOUND AND NOT TARGET udunits::udunits)
	add_library(udunits::udunits INTERFACE IMPORTED)
	set_target_properties(udunits::udunits PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${udunits_INCLUDE_DIR})
  set_target_properties(udunits::udunits PROPERTIES INTERFACE_LINK_LIBRARIES ${udunits_LIBRARY})
endif()

