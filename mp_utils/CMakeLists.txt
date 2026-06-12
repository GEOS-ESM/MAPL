esma_set_this (OVERRIDE MAPL.mp_utils)

# MPI-dependent utilities (Tier 2 in MAPL v3 restructuring).
# Depends on MAPL.utils + MPI. ESMF-dependent files absorbed from base3g/.

set (srcs
    # Layer umbrella
    API.F90
    # MPI error handling
    MAPL_MpiErrorHandling.F90
    # Parallel collective array reductions (MaxMin, AreaMean)
    ArrayReductions.F90
    # Compression utilities (MPI-dependent reduction)
    DownBit.F90
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
    # From base3g/ (ESMF/MPI-dependent, no geom or pfio dependency)
    PackedTime.F90
    SimulationTime.F90
    TimeInterpolation.F90
    LocalDisplacementEnsemble.F90
    SatVapor.F90
    StringTemplate.F90
    # Moved back to base3g/ (pfio/header dependencies):
    #   FileMetadataUtilities.F90, FileMetadataUtilitiesVector.F90, FileIO.F90
    # Profiler (absorbed from profiler/)
    profiler/AbstractGauge.F90
    profiler/AbstractMeter.F90
    profiler/AbstractMeterFactory.F90
    profiler/AbstractMeterNode.F90
    profiler/AdvancedMeter.F90
    profiler/BaseProfiler.F90
    profiler/DistributedMeter.F90
    profiler/DistributedProfiler.F90
    profiler/FortranTimerGauge.F90
    profiler/GlobalProfilers.F90
    profiler/MallocGauge.F90
    profiler/MAPL_Profiler.F90
    profiler/MemoryProfiler.F90
    profiler/MeterNode.F90
    profiler/MeterNodePtr.F90
    profiler/MeterNodeStack.F90
    profiler/MeterNodeVector.F90
    profiler/MpiTimerGauge.F90
    profiler/NullGauge.F90
    profiler/RssMemoryGauge.F90
    profiler/StubProfiler.F90
    profiler/TimeProfiler.F90
    profiler/VmstatMemoryGauge.F90
    # Profiler reporting
    profiler/reporting/ProfileReporter.F90
    profiler/reporting/CsvProfileReporter.F90
    profiler/reporting/AbstractColumn.F90
    profiler/reporting/SimpleColumn.F90
    profiler/reporting/TextColumn.F90
    profiler/reporting/SimpleTextColumn.F90
    profiler/reporting/NameColumn.F90
    profiler/reporting/PlainNameColumn.F90
    profiler/reporting/DepthColumn.F90
    profiler/reporting/FormattedTextColumn.F90
    profiler/reporting/MemoryTextColumn.F90
    profiler/reporting/SeparatorColumn.F90
    profiler/reporting/NumCyclesColumn.F90
    profiler/reporting/InclusiveColumn.F90
    profiler/reporting/ExclusiveColumn.F90
    profiler/reporting/StdDevColumn.F90
    profiler/reporting/MinCycleColumn.F90
    profiler/reporting/MeanCycleColumn.F90
    profiler/reporting/MaxCycleColumn.F90
    profiler/reporting/PercentageColumn.F90
    profiler/reporting/ColumnVector.F90
    profiler/reporting/TextColumnVector.F90
    profiler/reporting/MultiColumn.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.utils MAPL.constants MPI::MPI_Fortran ESMF::ESMF
                 GFTL_SHARED::gftl-shared GFTL::gftl-v1 GFTL::gftl-v2
                 PFLOGGER::pflogger
    TYPE SHARED
)

# NAG and other strict compilers need relaxed flags for MPI interfaces
if (DUSTY)
  set_property (SOURCE Shmem/Shmem.F90 Shmem/Shmem_implementation.F90 MAPL_LoadBalance.F90
                PROPERTY COMPILE_FLAGS ${DUSTY})
endif ()

target_link_libraries (${this} PRIVATE OpenMP::OpenMP_Fortran)

target_compile_definitions (${this} PRIVATE MAPL_MODE)

target_include_directories (${this} PUBLIC $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)
target_include_directories (${this} PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

add_subdirectory (profiler/examples)
if (PFUNIT_FOUND)
  add_subdirectory (profiler/tests EXCLUDE_FROM_ALL)
endif ()
