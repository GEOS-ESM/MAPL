esma_set_this (OVERRIDE MAPL.profiler)

set (srcs
  AbstractMeter.F90
  AbstractMeterNode.F90
  AbstractMeterFactory.F90
  MeterNodeVector.F90
  MeterNodeStack.F90

  # Low-level measures
  AbstractGauge.F90
  MpiTimerGauge.F90
  FortranTimerGauge.F90
  RssMemoryGauge.F90
  VmstatMemoryGauge.F90


  
  AdvancedMeter.F90
  DistributedMeter.F90

  MeterNode.F90

  AbstractColumn.F90
  SimpleColumn.F90
  TextColumn.F90
  SimpleTextColumn.F90
  NameColumn.F90

  FormattedTextColumn.F90
  MemoryTextColumn.F90
  SeparatorColumn.F90
  NumCyclesColumn.F90
  InclusiveColumn.F90
  ExclusiveColumn.F90
  StdDevColumn.F90
  MinCycleColumn.F90
  MeanCycleColumn.F90
  MaxCycleColumn.F90
  MinCycleColumn.F90
  PercentageColumn.F90
  ColumnVector.F90
  TextColumnVector.F90
  MultiColumn.F90

  BaseProfiler.F90
  TimeProfiler.F90
  MemoryProfiler.F90
  DistributedProfiler.F90
  ProfileReporter.F90

  # The package
  MAPL_Profiler.F90

  )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES gftl-shared gftl MAPL.shared MPI::MPI_Fortran TYPE SHARED)

target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)

add_subdirectory (demo EXCLUDE_FROM_ALL)
if (PFUNIT_FOUND)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
