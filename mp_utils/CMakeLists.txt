esma_set_this (OVERRIDE MAPL.mp_utils)

# MPI-dependent utilities (Tier 2 in MAPL v3 restructuring).
# Depends on MAPL.utils + MPI. No ESMF.

set (srcs
    # MPI error handling
    MAPL_MpiErrorHandling.F90
    # Comm splitters
    AbstractCommSplitter.F90
    CommGroupDescription.F90
    SplitCommunicator.F90
    SimpleCommSplitter.F90
    # Load balancing / partitioning
    MAPL_LoadBalance.F90
    Partition.F90
    # Shared memory
    Shmem/Shmem.F90   Shmem/Shmem_implementation.F90
    # Profiler (absorbed from profiler/)
    AbstractGauge.F90
    AbstractMeter.F90
    AbstractMeterFactory.F90
    AbstractMeterNode.F90
    AdvancedMeter.F90
    BaseProfiler.F90
    DistributedMeter.F90
    DistributedProfiler.F90
    FortranTimerGauge.F90
    GlobalProfilers.F90
    MallocGauge.F90
    MAPL_Profiler.F90
    MemoryProfiler.F90
    MeterNode.F90
    MeterNodePtr.F90
    MeterNodeStack.F90
    MeterNodeVector.F90
    MpiTimerGauge.F90
    NullGauge.F90
    RssMemoryGauge.F90
    StubProfiler.F90
    TimeProfiler.F90
    VmstatMemoryGauge.F90
    # Profiler reporting
    reporting/ProfileReporter.F90
    reporting/CsvProfileReporter.F90
    reporting/AbstractColumn.F90
    reporting/SimpleColumn.F90
    reporting/TextColumn.F90
    reporting/SimpleTextColumn.F90
    reporting/NameColumn.F90
    reporting/PlainNameColumn.F90
    reporting/DepthColumn.F90
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
    reporting/PercentageColumn.F90
    reporting/ColumnVector.F90
    reporting/TextColumnVector.F90
    reporting/MultiColumn.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.utils MPI::MPI_Fortran GFTL_SHARED::gftl-shared GFTL::gftl-v1 GFTL::gftl-v2 PFLOGGER::pflogger
    TYPE SHARED
)

# NAG and other strict compilers need relaxed flags for MPI interfaces
if (DUSTY)
  set_property (SOURCE Shmem/Shmem.F90 Shmem/Shmem_implementation.F90 MAPL_LoadBalance.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_link_libraries (${this} PRIVATE OpenMP::OpenMP_Fortran)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

# Backward-compatibility alias for consumers linking MAPL.profiler
add_library (MAPL.profiler INTERFACE)
target_link_libraries (MAPL.profiler INTERFACE MAPL.mp_utils)
install (TARGETS MAPL.profiler EXPORT MAPL-targets)

if (PFUNIT_FOUND)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()
