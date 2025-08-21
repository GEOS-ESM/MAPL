esma_set_this (OVERRIDE MAPL_StatisticsGridComp)

set(srcs
  AbstractTimeStatistic.F90
  StatisticsVector.F90
  NullStatistic.F90
  TimeAverage.F90
  StatisticsGridComp.F90
  )

find_package (MPI REQUIRED)

esma_add_library(${this}
  SRCS ${srcs}
  DEPENDENCIES mapl3g TYPE SHARED)

#add_subdirectory(tests EXCLUDE_FROM_ALL)

