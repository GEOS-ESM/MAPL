macro(run_case CASE)
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
    foreach(line IN LISTS file_lines)
			 execute_process(
				COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/MAPL_Component_Driver.x ${line}
				RESULT_VARIABLE CMD_RESULT
				WORKING_DIRECTORY ${tempdir}
				)
			 if(CMD_RESULT)
				 message(FATAL_ERROR "Error running ${CASE}")
			 endif()
    endforeach()
	 execute_process(
		COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
		)
endmacro()
run_case(${TEST_CASE})
