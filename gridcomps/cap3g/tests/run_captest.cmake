macro(run_case CASE)
    string(RANDOM LENGTH 24 tempdir)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/${CASE} ${tempdir}
      )
    set(num_procs "1")
    execute_process(
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/GEOS.x cap.yaml
      RESULT_VARIABLE CMD_RESULT
      WORKING_DIRECTORY ${tempdir}
      )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E cat ${tempdir}/PET0.ESMF_LogFile
      )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
      )
    if(CMD_RESULT)
       message(FATAL_ERROR "Error running ${CASE}")
    endif()
endmacro()
run_case(${TEST_CASE})
