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
				set(config_name "${line}")
				message(STATUS "${CASE} (${DESCRIPTION}): Running step ${step_num}/${total_steps}: ${config_name}")
				execute_process(
				COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/GEOS.x ${config_name}
				RESULT_VARIABLE CMD_RESULT
				WORKING_DIRECTORY ${tempdir}
				)
				if(CMD_RESULT)
				 if(NOT "${DESCRIPTION}" STREQUAL "")
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${config_name})\nTest Description: ${DESCRIPTION}")
				 else()
					 message(FATAL_ERROR "${CASE} FAILED at step ${step_num}/${total_steps} (${config_name})")
				 endif()
			 endif()
			 math(EXPR step_num "${step_num} + 1")
    endforeach()

    if (EXISTS "${tempdir}/compare.rc")
        file(STRINGS "${tempdir}/compare.rc" compare_lines)
        foreach(pair IN LISTS compare_lines)
            string(REGEX MATCH "^([^ ]+) +([^ ]+)$" _ "${pair}")
            set(generated "${tempdir}/${CMAKE_MATCH_1}")
            set(reference "${tempdir}/${CMAKE_MATCH_2}")
            execute_process(
                COMMAND ${CMAKE_COMMAND} -E compare_files "${generated}" "${reference}"
                RESULT_VARIABLE COMPARE_RESULT
                )
            if(COMPARE_RESULT)
                message(FATAL_ERROR "${CASE} FAILED: ${CMAKE_MATCH_1} does not match reference ${CMAKE_MATCH_2}")
            endif()
        endforeach()
    endif()

    if (EXISTS "${tempdir}/dryrun.rc")
        file(STRINGS "${tempdir}/dryrun.rc" dryrun_lines)
        foreach(line IN LISTS dryrun_lines)
            if(line MATCHES "^extdata_config=(.+)$")
                set(dryrun_extdata_config "${CMAKE_MATCH_1}")
            elseif(line MATCHES "^run_start=(.+)$")
                set(dryrun_run_start "${CMAKE_MATCH_1}")
            elseif(line MATCHES "^run_end=(.+)$")
                set(dryrun_run_end "${CMAKE_MATCH_1}")
            endif()
        endforeach()
        message(STATUS "${CASE} (${DESCRIPTION}): Running extdata dry run verification ...")
        execute_process(
            COMMAND python3
                ${MY_BINARY_DIR}/extdata_dryrun_check.py
                --config          ${dryrun_extdata_config}
                --run_start       ${dryrun_run_start}
                --run_end         ${dryrun_run_end}
                --check --narrow
                --output          dryrun_estimated.yaml
                --missing_output  dryrun_missing.yaml
                --verify_files_read files_read.yaml
            RESULT_VARIABLE DRYRUN_RESULT
            WORKING_DIRECTORY ${tempdir}
        )
        if(DRYRUN_RESULT)
            message(FATAL_ERROR "${CASE} FAILED: extdata dry run verification failed.")
        endif()
    endif()

	 execute_process(
		COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
		)
endmacro()
run_case(${TEST_CASE} ${TEST_DESCRIPTION})
