esma_set_this (OVERRIDE MAPL.statistics)

set(srcs
  AbstractTimeStatistic.F90
  StatisticsVector.F90
  NullStatistic.F90
  TimeAverage.F90
  TimeAccumulate.F90
  TimeMin.F90
  TimeMax.F90
  AbstractCovarianceKernel.F90
  ShiftedCovarianceKernel.F90
  WelfordCovarianceKernel.F90
  TimeVariance.F90
  StatisticsGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES MAPL TYPE SHARED)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()
