# run_case.cmake
# Called via: cmake -P run_case.cmake with the following variables defined:
#   TEST_CASE, MPIEXEC_EXECUTABLE, MPIEXEC_NUMPROC_FLAG, MY_BINARY_DIR,
#   MPIEXEC_PREFLAGS, REGRID_UTIL_PATH
# Reads LOCAL_REGRESSION_DATA_DIR from the environment at test run time.

function(run_case)

  # Construct regression data dir at runtime so LOCAL_REGRESSION_DATA_DIR is
  # read from the environment when the test runs, not at cmake configure time
  set(REGRESSION_DATA_DIR "$ENV{LOCAL_REGRESSION_DATA_DIR}/${REGRID_UTIL_PATH}")
  set(CASE_DATA_DIR "${REGRESSION_DATA_DIR}/${TEST_CASE}")
  set(CASE_SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE}")
  set(TMP_DIR "${CMAKE_CURRENT_LIST_DIR}/${TEST_CASE}_tmp")

  # Read number of processors and command line arguments from the source tree
  file(READ "${CASE_SOURCE_DIR}/npet.txt" NPET)
  string(STRIP "${NPET}" NPET)

  file(READ "${CASE_SOURCE_DIR}/cmd_line.txt" CMD_LINE)
  string(STRIP "${CMD_LINE}" CMD_LINE)
  separate_arguments(CMD_LINE_LIST UNIX_COMMAND "${CMD_LINE}")

  # Verify the regression data directory exists
  if(NOT EXISTS "${CASE_DATA_DIR}")
    message(FATAL_ERROR
      "Test '${TEST_CASE}' failed: regression data directory not found: ${CASE_DATA_DIR}\n"
      "Is LOCAL_REGRESSION_DATA_DIR set correctly?"
    )
  endif()

  # Create temp directory and symlink all regression data files into it
  file(MAKE_DIRECTORY "${TMP_DIR}")
  file(GLOB DATA_FILES "${CASE_DATA_DIR}/*")
  if(NOT DATA_FILES)
    message(FATAL_ERROR
      "Test '${TEST_CASE}' failed: no files found in regression data directory: ${CASE_DATA_DIR}"
    )
  endif()
  foreach(F ${DATA_FILES})
    get_filename_component(FNAME "${F}" NAME)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${F}" "${TMP_DIR}/${FNAME}"
      RESULT_VARIABLE LINK_RESULT
    )
    if(NOT LINK_RESULT EQUAL 0)
      file(REMOVE_RECURSE "${TMP_DIR}")
      message(FATAL_ERROR
        "Test '${TEST_CASE}' failed: could not create symlink for ${F}"
      )
    endif()
  endforeach()

  # --- Run 1: command-line arguments ---

  execute_process(
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${NPET} ${MPIEXEC_PREFLAGS}
            ${MY_BINARY_DIR}/Regrid_Util.x ${CMD_LINE_LIST}
    WORKING_DIRECTORY "${TMP_DIR}"
    RESULT_VARIABLE RUN_RESULT
  )

  if(NOT EXISTS "${TMP_DIR}/output_from_run.nc4")
    file(REMOVE_RECURSE "${TMP_DIR}")
    message(FATAL_ERROR
      "Test '${TEST_CASE}' (cmd-line run) failed: Regrid_Util.x did not produce output_from_run.nc4 "
      "(mpiexec exit code: ${RUN_RESULT})"
    )
  endif()

  # References were generated with ifort. Permit GNU compiler roundoff on
  # large-magnitude fields while retaining an absolute bound near zero.
  execute_process(
    COMMAND nccmp -dmfgsB --tolerance 1e-4 --Tolerance 1e-4 --join-tolerance
            "${TMP_DIR}/output_from_run.nc4" "${TMP_DIR}/output.nc4"
    RESULT_VARIABLE CMP_RESULT
  )
  if(NOT CMP_RESULT EQUAL 0)
    file(REMOVE_RECURSE "${TMP_DIR}")
    message(FATAL_ERROR
      "Test '${TEST_CASE}' (cmd-line run) failed: output_from_run.nc4 differs from reference output.nc4"
    )
  endif()

  # Remove output before YAML run so a stale file cannot mask a real failure
  file(REMOVE "${TMP_DIR}/output_from_run.nc4")

  # --- Run 2: YAML config file ---

  set(YAML_CONFIG "${CASE_SOURCE_DIR}/config.yaml")

  execute_process(
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${NPET} ${MPIEXEC_PREFLAGS}
            ${MY_BINARY_DIR}/Regrid_Util.x --config "${YAML_CONFIG}"
    WORKING_DIRECTORY "${TMP_DIR}"
    RESULT_VARIABLE YAML_RUN_RESULT
  )

  if(NOT EXISTS "${TMP_DIR}/output_from_run.nc4")
    file(REMOVE_RECURSE "${TMP_DIR}")
    message(FATAL_ERROR
      "Test '${TEST_CASE}' (yaml run) failed: Regrid_Util.x did not produce output_from_run.nc4 "
      "(mpiexec exit code: ${YAML_RUN_RESULT})"
    )
  endif()

  execute_process(
    COMMAND nccmp -dmfgsB --tolerance 1e-4 --Tolerance 1e-4 --join-tolerance
            "${TMP_DIR}/output_from_run.nc4" "${TMP_DIR}/output.nc4"
    RESULT_VARIABLE YAML_CMP_RESULT
  )
  if(NOT YAML_CMP_RESULT EQUAL 0)
    file(REMOVE_RECURSE "${TMP_DIR}")
    message(FATAL_ERROR
      "Test '${TEST_CASE}' (yaml run) failed: output_from_run.nc4 differs from reference output.nc4"
    )
  endif()

  # Clean up
  file(REMOVE_RECURSE "${TMP_DIR}")

endfunction()

# --- Entry point ---
run_case()
