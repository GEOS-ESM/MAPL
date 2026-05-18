# profiler/ sources have moved to mp_utils/.
# This directory is retained only for demo/ and tests/.

add_subdirectory (demo)
if (PFUNIT_FOUND)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
