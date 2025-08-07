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
    if (${IS_EXTDATA1G} STREQUAL "YES")
       file(APPEND "${tempdir}/CAP1.rc" "USE_EXTDATA2G: .false.")
       file(APPEND "${tempdir}/CAP2.rc" "USE_EXTDATA2G: .false.")
       if (EXISTS  "${tempdir}/CAP3.rc")
          file(APPEND "${tempdir}/CAP3.rc" "USE_EXTDATA2G: .false.")
       endif()
       if (EXISTS  "${tempdir}/CAP4.rc")
          file(APPEND "${tempdir}/CAP4.rc" "USE_EXTDATA2G: .false.")
       endif()
    endif()
    execute_process(
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MPIEXEC_PREFLAGS} ${MY_BINARY_DIR}/ExtDataDriver.x
      RESULT_VARIABLE CMD_RESULT
      WORKING_DIRECTORY ${tempdir}
      #COMMAND_ECHO STDOUT
      )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
      )
    if(CMD_RESULT)
       message(FATAL_ERROR "Error running ${CASE}")
    endif()
endmacro()
run_case(${TEST_CASE})
