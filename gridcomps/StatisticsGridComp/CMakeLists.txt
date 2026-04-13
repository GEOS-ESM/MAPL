esma_set_this (OVERRIDE MAPL_StatisticsGridComp)

set(srcs
  AbstractTimeStatistic.F90
  StatisticsVector.F90
  NullStatistic.F90
  TimeAverage.F90
  TimeMin.F90
  TimeMax.F90
  Variance.F90
  AbstractVariance.F90
  WelfordVariance.F90
  ShiftedVariance.F90
  StatisticsGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
