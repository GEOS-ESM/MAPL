macro(run_case CASE)
    string(RANDOM LENGTH 24 tempdir)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/test_cases/${CASE} ${tempdir}
      )
    if ("${CASE}" STREQUAL "OMP_4_thread")
       set(ENV{OMP_NUM_THREADS} "4")
    else()
       set(ENV{OMP_NUM_THREADS} "1")
    endif()

    set (num_procs
        6
    )
    execute_process(
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/GetHorzIJIndexDriver.x
      RESULT_VARIABLE CMD_RESULT
      WORKING_DIRECTORY ${tempdir}
      #COMMAND_ECHO STDOUT
      )

    #execute_process(
    #  COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
    #  )
    if(CMD_RESULT)
       message(FATAL_ERROR "Error running ${CASE}")
    endif()
endmacro()
run_case(${TEST_CASE})
