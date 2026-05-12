esma_set_this(OVERRIDE MAPL.python_bridge)

set (srcs
  PythonBridge.F90
)

if (BUILD_PYTHONBRIDGE)
  list (APPEND srcs
    MAPL_PythonBridge/fortran2python/fortran_python_bridge.F90
    MAPL_PythonBridge/fortran2python/fortran_python_bridge.c
    MAPL_PythonBridge/python2fortran/python_fortran_bridge.F90)

  message(STATUS "Building MAPL Python Bridge to interface Fortran code with Python")

  add_definitions(-DPYTHONBRIDGE_INTEGRATION)

  # The Python library creation requires mpiexec/mpirun to run on a
  # compute node. Probably a weird SLURM thing?
  find_package(MPI REQUIRED)
  find_package(Python3 COMPONENTS Interpreter REQUIRED)

  # Set up some variables in case names change
  set(PythonBridge_INTERFACE_LIBRARY          ${CMAKE_CURRENT_BINARY_DIR}/libmapl_fortan_python_cffi_bridge${CMAKE_SHARED_LIBRARY_SUFFIX})
  set(PythonBridge_INTERFACE_HEADER_FILE      ${CMAKE_CURRENT_BINARY_DIR}/mapl_fortan_python_cffi_bridge.h)
  set(PythonBridge_INTERFACE_FLAG_HEADER_FILE ${CMAKE_CURRENT_SOURCE_DIR}/MAPL_PythonBridge/fortran2python/fortran_python_bridge.h)
  set(PythonBridge_INTERFACE_SRCS             ${CMAKE_CURRENT_SOURCE_DIR}/MAPL_PythonBridge/fortran2python/fortran_python_bridge.py)

  # This command creates the shared object library from Python
  add_custom_command(
    OUTPUT ${PythonBridge_INTERFACE_LIBRARY}
    # Note below is essentially:
    #  mpirun -np 1 python file
    # but we use the CMake options as much as we can for flexibility
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${PythonBridge_INTERFACE_FLAG_HEADER_FILE} ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 ${Python3_EXECUTABLE} ${PythonBridge_INTERFACE_SRCS}
    BYPRODUCTS ${PythonBridge_INTERFACE_HEADER_FILE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    MAIN_DEPENDENCY ${PythonBridge_INTERFACE_SRCS}
    COMMENT "Building Python Bridge to interface with Python code"
    VERBATIM
  )

  # This creates a target we can use for dependencies and post build
  add_custom_target(generate_PythonBridge_interface_library DEPENDS ${PythonBridge_INTERFACE_LIBRARY})

  # Because of the weird hacking of INTERFACE libraries below, we cannot
  # use the "usual" CMake calls to install() the .so. I think it's because
  # INTERFACE libraries don't actually produce any artifacts as far as
  # CMake is concerned. So we add a POST_BUILD custom command to "install"
  # the library into install/lib
  add_custom_command(TARGET generate_PythonBridge_interface_library
    POST_BUILD
    # We first need to make a lib dir if it doesn't exist. If not, then
    # the next command can copy the script into a *file* called lib because
    # of a race condition (if install/lib/ isn't mkdir'd first)
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_INSTALL_PREFIX}/lib
    # Now we copy the file (if different...though not sure if this is useful)
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${PythonBridge_INTERFACE_LIBRARY}" ${CMAKE_INSTALL_PREFIX}/lib
  )

  # We use INTERFACE libraries to create a sort of "fake" target library we can use
  # to make libPythonBridge_GridComp.a depend on. It seems to work!
  add_library(PythonBridge_interface_py INTERFACE)

  # The target_include_directories bits were essentially stolen from the esma_add_library
  # code...
  target_include_directories(PythonBridge_interface_py INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # stubs
    # modules and copied *.h, *.inc
    $<BUILD_INTERFACE:${esma_include}/${this}>
    $<INSTALL_INTERFACE:include/${this}>
  )
  target_link_libraries(PythonBridge_interface_py INTERFACE ${PythonBridge_INTERFACE_LIBRARY})

  # This makes sure the library is built first
  add_dependencies(PythonBridge_interface_py generate_PythonBridge_interface_library)

  # This bit is to resolve an issue and Google told me to do this. I'm not
  # sure that the LIBRARY DESTINATION bit actually does anything since
  # this is using INTERFACE
  install(TARGETS PythonBridge_interface_py
    EXPORT ${PROJECT_NAME}-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
  )

endif ()

if (BUILD_PYTHONBRIDGE)
  set(dependencies MAPL.base MAPL.generic MAPL.shared ESMF::ESMF PythonBridge_interface_py )
else ()
  set(dependencies MAPL.base MAPL.generic MAPL.shared ESMF::ESMF)
endif()

# Install the Python/MAPL directory
install (
  DIRECTORY MAPL
  DESTINATION lib/Python
  USE_SOURCE_PERMISSIONS
  )

# Install the Python/MAPL_PythonBridge directory
install (
  DIRECTORY MAPL_PythonBridge
  DESTINATION lib/Python
  USE_SOURCE_PERMISSIONS
)

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES ${dependencies} TYPE SHARED)
