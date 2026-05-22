#include "MAPL_ErrLog.h"

! Package exporter
module mapl_Profiler_mod
   use mapl_AbstractMeter_mod
   use mapl_AbstractMeterNode_mod
   use mapl_AbstractMeterFactory_mod
   use mapl_MeterNodeVector_mod
   use mapl_MeterNode_mod
   use mapl_BaseProfiler_mod

   use mapl_AdvancedMeter_mod
   use mapl_MpiTimerGauge_mod
   use mapl_FortranTimerGauge_mod
   use mapl_RssMemoryGauge_mod
   use mapl_VmstatMemoryGauge_mod

   use mapl_DistributedMeter_mod
   use mapl_DistributedProfiler_mod
   use mapl_TimeProfiler_mod
   use mapl_MemoryProfiler_mod

   use mapl_ProfileReporter_mod
   use mapl_CsvProfileReporter_mod
   use mapl_AbstractColumn_mod
   use mapl_SimpleColumn_mod
   use mapl_TextColumn_mod
   use mapl_SimpleTextColumn_mod
   use mapl_FormattedTextColumn_mod
   use mapl_MemoryTextColumn_mod
   use mapl_NameColumn_mod
   use mapl_PlainNameColumn_mod
   use mapl_DepthColumn_mod
   use mapl_NumCyclesColumn_mod
   use mapl_InclusiveColumn_mod
   use mapl_ExclusiveColumn_mod
   use mapl_StdDevColumn_mod
   use mapl_MinCycleColumn_mod
   use mapl_MaxCycleColumn_mod
   use mapl_MeanCycleColumn_mod
   use mapl_PercentageColumn_mod
   use mapl_TextColumnVector_mod
   use mapl_MultiColumn_mod
   use mapl_SeparatorColumn_mod
   use mapl_GlobalProfilers_mod

   implicit none

contains

   subroutine initialize(comm, unusable, enable_global_timeprof, enable_global_memprof, rc)
      use mapl_ErrorHandling_mod
      use mapl_KeywordEnforcer_mod
      integer, optional, intent(in) :: comm
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) ::  enable_global_timeprof
      logical, optional, intent(in) ::  enable_global_memprof
      integer, optional, intent(out) :: rc

      integer :: status

      call initialize_global_time_profiler(name='All', comm=comm, enabled=enable_global_timeprof, _RC)
      call initialize_global_memory_profiler(name='All', comm=comm, enabled=enable_global_memprof, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   subroutine finalize(unusable, rc)
      use mapl_KeywordEnforcer_mod
      use mapl_ErrorHandling_mod
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(DistributedProfiler), pointer :: t_p, m_p

      integer :: status

      t_p => get_global_time_profiler()
      call t_p%stop(_RC)
      m_p => get_global_memory_profiler()
      call m_p%stop(_RC)

      call report_global_profiler()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine finalize

   subroutine report_global_profiler(unusable,comm,rc)
      use mapl_KeywordEnforcer_mod
      use mapl_ErrorHandling_mod
      use mpi
      use pflogger, only: logging
      use pflogger, only: Logger
      use gFTL2_StringVector, only: StringVector, StringVectorIterator, operator(/=)

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc
      type (ProfileReporter) :: reporter
      integer :: world_comm
      type(StringVector) :: report_lines
      type(StringVectorIterator) :: iter
      type (MultiColumn) :: inclusive
      type (MultiColumn) :: exclusive
      integer :: npes, my_rank, ierror
      character(1) :: empty(0)
      class (BaseProfiler), pointer :: t_p
      class (BaseProfiler), pointer :: m_p
      type(Logger), pointer :: lgr

      if (present(comm)) then
         world_comm = comm
      else
         world_comm=MPI_COMM_WORLD
      end if

      call MPI_Comm_size(world_comm, npes, ierror)
      _VERIFY(ierror)
      call MPI_Comm_Rank(world_comm, my_rank, ierror)
      _VERIFY(ierror)


      t_p => get_global_time_profiler()
      if (t_p%get_num_meters() > 0) then
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(50, separator= " "))
         call reporter%add_column(FormattedTextColumn('#-cycles','(i8.0)', 8, NumCyclesColumn(),separator='-'))

         inclusive = MultiColumn(['Inclusive'], separator='=')
         call inclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, InclusiveColumn(), separator='-'))
         call inclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(InclusiveColumn(),'MAX'),separator='-'))
         call reporter%add_column(inclusive)

         exclusive = MultiColumn(['Exclusive'], separator='=')
         call exclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, ExclusiveColumn(), separator='-'))
         call exclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn()), separator='-'))
         call reporter%add_column(exclusive)

         if (my_rank == 0) then
            report_lines = reporter%generate_report(t_p)
            lgr => logging%get_logger('MAPL.profiler')
            call lgr%info('Report on process: %i0', my_rank)
            iter = report_lines%begin()
            do while (iter /= report_lines%end())
               call lgr%info('%a', iter%of())
               call iter%next()
            end do
         end if
      end if

      m_p => get_global_memory_profiler()
      if (m_p%get_num_meters() > 0) then
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(50, separator= " "))

         inclusive = MultiColumn(['Inclusive'], separator='=')
         call inclusive%add_column(MemoryTextColumn(['  MEM  '],'(i4,1x,a2)', 9, InclusiveColumn(), separator='-'))
!!$      call inclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(InclusiveColumn()), separator='-'))
         call reporter%add_column(inclusive)

         exclusive = MultiColumn(['Exclusive'], separator='=')
         call exclusive%add_column(MemoryTextColumn(['  MEM  '],'(i4,1x,a2)', 9, ExclusiveColumn(), separator='-'))
         call exclusive%add_column(FormattedTextColumn(' MEM (KB)','(-3p,f15.3, 0p)', 15, ExclusiveColumn(), separator='-'))
!!$      call exclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn()), separator='-'))
         call reporter%add_column(exclusive)

         if (my_rank == 0) then
            report_lines = reporter%generate_report(m_p)
            lgr => logging%get_logger('MAPL.profiler')
            iter = report_lines%begin()
            do while (iter /= report_lines%end())
               call lgr%info('%a', iter%of())
               call iter%next()
            end do
         end if
      end if

      call MPI_Barrier(world_comm, ierror)
      _VERIFY(ierror)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine report_global_profiler

end module mapl_Profiler_mod
