set (TEST_SRCS
  test_AdvancedMeter.pf
  test_NameColumn.pf
  test_ExclusiveColumn.pf
  test_PercentageColumn.pf
  test_TimeProfiler.pf
  test_ProfileReporter.pf
  test_MeterNode.pf
  test_MeterNodeIterator.pf
  test_DistributedMeter.pf
  )


add_pfunit_ctest (
  MAPL_Profiler_tests
  TEST_SOURCES ${TEST_SRCS}
  LINK_LIBRARIES MAPL_Profiler
  MAX_PES 8
  )
  

add_dependencies (tests MAPL_Profiler_tests)

