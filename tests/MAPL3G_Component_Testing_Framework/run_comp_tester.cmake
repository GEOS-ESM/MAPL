macro(run_case CASE DESCRIPTION)
    string(RANDOM LENGTH 24 tempdir_name)
    set(tempdir "${CMAKE_CURRENT_BINARY_DIR}/${tempdir_name}")
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE_PATH} ${tempdir}
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
				COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/GEOS.x ${line}
				RESULT_VARIABLE CMD_RESULT
				OUTPUT_FILE ${tempdir}/step${step_num}.stdout
				ERROR_FILE ${tempdir}/step${step_num}.stderr
				WORKING_DIRECTORY ${tempdir}
				 )
			 if(CMD_RESULT)
				 file(READ "${tempdir}/step${step_num}.stdout" step_stdout)
				 file(READ "${tempdir}/step${step_num}.stderr" step_stderr)
				 if(NOT "${DESCRIPTION}" STREQUAL "")
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${line})\nTest Description: ${DESCRIPTION}\nstdout:\n${step_stdout}\nstderr:\n${step_stderr}")
				 else()
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${line})\nstdout:\n${step_stdout}\nstderr:\n${step_stderr}")
				 endif()
			 endif()
			 math(EXPR step_num "${step_num} + 1")
    endforeach()

    if (EXISTS "${tempdir}/output_checks.rc")
        file(STRINGS "${tempdir}/output_checks.rc" output_checks)
        foreach(check IN LISTS output_checks)
            if(NOT check MATCHES "^([^ ]+) +(.+)$")
                message(FATAL_ERROR "${CASE} FAILED: malformed output check: ${check}")
            endif()
            set(output_file "${tempdir}/${CMAKE_MATCH_1}")
            set(expected_regex "${CMAKE_MATCH_2}")
            if(NOT EXISTS "${output_file}")
                message(FATAL_ERROR "${CASE} FAILED: output file does not exist: ${CMAKE_MATCH_1}")
            endif()
            file(READ "${output_file}" output_contents)
            if(NOT output_contents MATCHES "${expected_regex}")
                message(FATAL_ERROR "${CASE} FAILED: ${CMAKE_MATCH_1} does not match ${expected_regex}")
            endif()
        endforeach()
    endif()

    if (EXISTS "${tempdir}/compare.rc")
        file(STRINGS "${tempdir}/compare.rc" compare_lines)
        foreach(pair IN LISTS compare_lines)
            string(REGEX MATCH "^([^ ]+) +([^ ]+)$" _ "${pair}")
            set(generated "${tempdir}/${CMAKE_MATCH_1}")
            set(reference "${tempdir}/${CMAKE_MATCH_2}")
            file(READ "${generated}" generated_contents)
            file(READ "${reference}" reference_contents)
            string(STRIP "${generated_contents}" generated_contents)
            string(STRIP "${reference_contents}" reference_contents)
            if(NOT generated_contents STREQUAL reference_contents)
                message(FATAL_ERROR "${CASE} FAILED: ${CMAKE_MATCH_1} does not match reference ${CMAKE_MATCH_2}")
            endif()
        endforeach()
    endif()

	 execute_process(
		COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
		)
endmacro()
run_case(${TEST_CASE} ${TEST_DESCRIPTION})
