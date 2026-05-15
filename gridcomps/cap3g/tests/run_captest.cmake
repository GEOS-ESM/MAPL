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
    if(EXISTS ${tempdir}/PET0.ESMF_LogFile)
      execute_process(
        COMMAND ${CMAKE_COMMAND} -E cat ${tempdir}/PET0.ESMF_LogFile
        )
    endif()
    if(NOT CMD_RESULT AND "${CASE}" STREQUAL "cap_restart_tests")
      set(EXPECTED "cap_restart_expected.yaml")
      set(ACTUAL "cap_restart.yaml")
      execute_process(
        COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol "${EXPECTED}" "${ACTUAL}"
        WORKING_DIRECTORY "${tempdir}"
        RESULT_VARIABLE CMD_RESULT
        )
    endif()
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
      )
    if(CMD_RESULT)
       set(MSG "Error running ${CASE}")
       message(FATAL_ERROR "${MSG}")
    endif()
endmacro()
run_case(${TEST_CASE})
