macro(run_case CASE DESCRIPTION)
    string(RANDOM LENGTH 24 tempdir)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/test_cases/${CASE} ${tempdir}
      )
    if (EXISTS "${tempdir}/nproc.rc")
      file(READ "${tempdir}/nproc.rc" num_procs_temp)
      string(STRIP ${num_procs_temp} num_procs)
    else()
      set(num_procs "1")
    endif()

    file(STRINGS ${tempdir}/steps.rc file_lines)
    list(LENGTH file_lines total_steps)
    set(step_num 1)
    foreach(line IN LISTS file_lines)
			 message(STATUS "${CASE} (${DESCRIPTION}): Running step ${step_num}/${total_steps}: ${line}")
			 execute_process(
				COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/MAPL_Component_Driver.x ${line}
				RESULT_VARIABLE CMD_RESULT
				WORKING_DIRECTORY ${tempdir}
				)
			 if(CMD_RESULT)
				 if(NOT "${DESCRIPTION}" STREQUAL "")
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${line})\nTest Description: ${DESCRIPTION}")
				 else()
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${line})")
				 endif()
			 endif()
			 math(EXPR step_num "${step_num} + 1")
    endforeach()
	 execute_process(
		COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
		)
endmacro()
run_case(${TEST_CASE} ${TEST_DESCRIPTION})
