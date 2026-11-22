macro(run_case CASE)
    string(RANDOM LENGTH 24 tempdir)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/${CASE} ${tempdir}
      )
    set(num_procs "1")
    set(config_file "mapl.yaml")
    if(EXISTS ${CMAKE_CURRENT_LIST_DIR}/${CASE}/num_procs)
      file(READ ${CMAKE_CURRENT_LIST_DIR}/${CASE}/num_procs num_procs)
      string(STRIP "${num_procs}" num_procs)
    endif()
    # A case with an expected_failure file passes only if GEOS.x fails and
    # its output matches the regular expression in that file.
    set(expected_failure "")
    if(EXISTS ${CMAKE_CURRENT_LIST_DIR}/${CASE}/expected_failure)
      file(READ ${CMAKE_CURRENT_LIST_DIR}/${CASE}/expected_failure expected_failure)
      string(STRIP "${expected_failure}" expected_failure)
    endif()
    # A case with an expected_output file fails unless GEOS.x output
    # matches the regular expression in that file.
    set(expected_output "")
    if(EXISTS ${CMAKE_CURRENT_LIST_DIR}/${CASE}/expected_output)
      file(READ ${CMAKE_CURRENT_LIST_DIR}/${CASE}/expected_output expected_output)
      string(STRIP "${expected_output}" expected_output)
    endif()
    execute_process(
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/GEOS.x ${config_file}
      RESULT_VARIABLE CMD_RESULT
      OUTPUT_VARIABLE CMD_OUTPUT
      ERROR_VARIABLE CMD_OUTPUT
      WORKING_DIRECTORY ${tempdir}
      )
    message("${CMD_OUTPUT}")
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
    if(expected_failure)
      if(NOT CMD_RESULT)
        set(CMD_RESULT 1)
        message("${CASE}: GEOS.x succeeded but was expected to fail")
      elseif("${CMD_OUTPUT}" MATCHES "${expected_failure}")
        set(CMD_RESULT 0)
      else()
        message("${CASE}: GEOS.x failed without the expected message: ${expected_failure}")
      endif()
    endif()
    if(expected_output AND NOT CMD_RESULT)
      if(NOT "${CMD_OUTPUT}" MATCHES "${expected_output}")
        set(CMD_RESULT 1)
        message("${CASE}: GEOS.x output lacks the expected message: ${expected_output}")
      endif()
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
