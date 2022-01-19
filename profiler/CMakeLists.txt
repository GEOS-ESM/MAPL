esma_set_this (OVERRIDE MAPL.profiler)

set (srcs
  AbstractMeter.F90
  AbstractMeterNode.F90
  AbstractMeterFactory.F90
  MeterNodeVector.F90
  MeterNodePtr.F90
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
  BaseProfiler.F90
  TimeProfiler.F90
  MemoryProfiler.F90
  DistributedProfiler.F90

  reporting/ProfileReporter.F90
  reporting/AbstractColumn.F90
  reporting/SimpleColumn.F90
  reporting/TextColumn.F90
  reporting/SimpleTextColumn.F90
  reporting/NameColumn.F90
  reporting/FormattedTextColumn.F90
  reporting/MemoryTextColumn.F90
  reporting/SeparatorColumn.F90
  reporting/NumCyclesColumn.F90
  reporting/InclusiveColumn.F90
  reporting/ExclusiveColumn.F90
  reporting/StdDevColumn.F90
  reporting/MinCycleColumn.F90
  reporting/MeanCycleColumn.F90
  reporting/MaxCycleColumn.F90
  reporting/MinCycleColumn.F90
  reporting/PercentageColumn.F90
  reporting/ColumnVector.F90
  reporting/TextColumnVector.F90
  reporting/MultiColumn.F90

  # The package
  MAPL_Profiler.F90
  )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES GFTL_SHARED::gftl-shared GFTL::gftl MAPL.shared MPI::MPI_Fortran TYPE ${MAPL_LIBRARY_TYPE})
target_include_directories (${this} PRIVATE ${MAPL_SOURCE_DIR}/include)

add_subdirectory (demo EXCLUDE_FROM_ALL)
if (PFUNIT_FOUND)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
