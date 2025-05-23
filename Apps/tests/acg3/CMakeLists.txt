esma_set_this (OVERRIDE MAPL.Apps.tests.acg3)

set (srcs
    ACG3.F90
    unittests.py
    )

add_library (${this} ${srcs})
target_link_libraries(${this} PRIVATE MAPL.shared MAPL.field MAPL.state MAPL.generic3g MAPL ESMF::ESMF)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

mapl_acg (${this}   compile_test.acg
          IMPORT_SPECS acg3_imports.h
          EXPORT_SPECS acg3_exports.h
          INTERNAL_SPECS acg3_internals.h
          GET_POINTERS acg3_get_pointers.h
          DECLARE_POINTERS acg3_declare_pointers.h
          3g
          )

# Add test 
add_test (NAME "${this}.compile"
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target ${this}
  )
set_tests_properties("${this}.compile" PROPERTIES LABELS "ESSENTIAL")

# Add ACG3 unit tests
set (PYTHON_UNIT_TEST_MODULE unittest)
add_test(NAME "${this}.unittests"
  COMMAND ${Python3_EXECUTABLE} -m ${PYTHON_UNIT_TEST_MODULE} discover -v ${CMAKE_CURRENT_LIST_DIR} -p unittests.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

set_tests_properties("${this}.compile" PROPERTIES LABELS "ESSENTIAL")
<<<<<<< HEAD
=======
set_tests_properties("${this}.unittests" PROPERTIES LABELS "ESSENTIAL")
set_tests_properties("${this}.unittests" PROPERTIES ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_LIST_DIR}/../../:${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}")
>>>>>>> 961477c8aaa (All tests pass)
