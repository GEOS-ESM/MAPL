esma_set_this(OVERRIDE MAPL.Python.tests)

# Require Python3 interpreter
find_package(Python3 COMPONENTS Interpreter REQUIRED)

# Check for pytest (optional but preferred over unittest for better output)
execute_process(
  COMMAND ${Python3_EXECUTABLE} -m pytest --version
  RESULT_VARIABLE _pytest_available
  OUTPUT_QUIET ERROR_QUIET
)

if(_pytest_available EQUAL 0)
  set(_python_test_runner pytest)
  set(_python_test_args -m pytest -v)
else()
  # Fallback: run directly with python -m unittest
  set(_python_test_runner unittest)
  set(_python_test_args -m unittest discover -v ${CMAKE_CURRENT_LIST_DIR} -p "test_*.py")
endif()

# ----- constants consistency test -----
# This test parses the Fortran source files in shared/Constants/ directly so
# that any change to a Fortran constant that is not reflected in
# Python/MAPL/constants.py will immediately cause this test to fail.

if(_pytest_available EQUAL 0)
  add_test(
    NAME MAPL.Python.test_constants_consistency
    COMMAND ${Python3_EXECUTABLE} -m pytest -v ${CMAKE_CURRENT_LIST_DIR}/test_constants_consistency.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
else()
  add_test(
    NAME MAPL.Python.test_constants_consistency
    COMMAND ${Python3_EXECUTABLE} -m unittest discover -v
            ${CMAKE_CURRENT_LIST_DIR} -p test_constants_consistency.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()

# Make the Python source tree available so "import MAPL.constants" resolves
# regardless of whether the package has been installed.
set_tests_properties(
  MAPL.Python.test_constants_consistency
  PROPERTIES
    LABELS "ESSENTIAL"
    ENVIRONMENT
      "PYTHONPATH=${CMAKE_CURRENT_LIST_DIR}/../..:${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}"
)
