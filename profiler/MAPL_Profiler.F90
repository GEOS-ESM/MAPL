#include "MAPL_ErrLog.h"

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
   use mapl_GlobalProfilers

   implicit none

contains 

   subroutine initialize(comm, unusable, enable_global_timeprof, enable_global_memprof, rc)
      use mapl_ErrorHandlingMod
      use mapl_KeywordEnforcerMod
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
      use mapl_KeywordEnforcerMod
      use mapl_ErrorHandlingMod
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
      use mapl_KeywordEnforcerMod
      use mapl_ErrorHandlingMod
      use mpi
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc
      type (ProfileReporter) :: reporter
      integer :: i, world_comm
      character(:), allocatable :: report_lines(:)
      type (MultiColumn) :: inclusive
      type (MultiColumn) :: exclusive
      integer :: npes, my_rank, ierror
      character(1) :: empty(0)
      class (BaseProfiler), pointer :: t_p
      class (BaseProfiler), pointer :: m_p

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
            write(*,'(a,1x,i0)')'Report on process: ', my_rank
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
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
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
         end if
      end if

      call MPI_Barrier(world_comm, ierror)
      _VERIFY(ierror)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine report_global_profiler




end module mapl_Profiler
