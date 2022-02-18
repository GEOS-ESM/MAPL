! Package exporter
module mapl_Profiler
   use mapl_AbstractMeter
   use mapl_AbstractMeterNode
   use mapl_AbstractMeterFactory
   use mapl_MeterNodeVector
   use mapl_MeterNode
   use mapl_BaseProfiler
   
   use mapl_AdvancedMeter
   use mapl_MpiTimerGauge
   use mapl_FortranTimerGauge
   use mapl_RssMemoryGauge
   use mapl_VmstatMemoryGauge

   use mapl_TimeProfiler
   use mapl_MemoryProfiler
   use mapl_DistributedMeter
   use mapl_DistributedProfiler

   use mapl_ProfileReporter
   use mapl_AbstractColumn
   use mapl_SimpleColumn
   use mapl_TextColumn
   use mapl_SimpleTextColumn
   use mapl_FormattedTextColumn
   use mapl_MemoryTextColumn
   use mapl_NameColumn
   use mapl_NumCyclesColumn
   use mapl_InclusiveColumn
   use mapl_ExclusiveColumn
   use mapl_StdDevColumn
   use mapl_MinCycleColumn
   use mapl_MaxCycleColumn
   use mapl_MeanCycleColumn
   use mapl_PercentageColumn
   use mapl_TextColumnVector
   use mapl_MultiColumn
   use mapl_SeparatorColumn
   
   implicit none
contains 

   subroutine initialize(comm)
      integer, optional, intent(in) :: comm
      call initialize_global_time_profiler(comm = comm)
      call initialize_global_memory_profiler() !comm = comm)
   end subroutine initialize

   subroutine finalize()
      call finalize_global_time_profiler()
      call finalize_global_memory_profiler()
   end subroutine finalize

end module mapl_Profiler
