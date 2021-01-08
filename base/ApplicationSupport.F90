#include "MAPL_ErrLog.h"
module MAPL_ApplicationSupport
 use MPI
 use MAPL_ExceptionHandling
 use MAPL_KeywordEnforcerMod
 use pflogger, only: logging
 use pflogger, only: Logger
 use MAPL_Profiler
 implicit none
 private

 public MAPL_Initialize
 public MAPL_Finalize

 contains

   subroutine MAPL_Initialize(unusable,comm,logging_config,rc)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      character(len=*), optional,intent(in) :: logging_config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: logging_configuration_file
      integer :: comm_world,status
     
      _UNUSED_DUMMY(unusable)

      if (present(logging_config)) then
         logging_configuration_file=logging_config
      else
         logging_configuration_file=''
      end if
      if (present(comm)) then
         call MPI_comm_dup(comm,comm_world,status)
         _VERIFY(status)
      else
         comm_world=MPI_COMM_WORLD
      end if
      call initialize_pflogger(comm=comm_world,logging_config=logging_configuration_file,rc=status)
      _VERIFY(status)
      call start_global_profiler(comm=comm_world,rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end subroutine MAPL_Initialize

   subroutine MAPL_Finalize(unusable,comm,rc)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: comm_world,status

      _UNUSED_DUMMY(unusable)
      
      if (present(comm)) then
         call MPI_comm_dup(comm,comm_world,status)
         _VERIFY(status)
      else
         comm_world=MPI_COMM_WORLD
      end if
      call stop_global_profiler()
      call report_global_profiler(comm=comm_world)
      call finalize_pflogger()

   end subroutine MAPL_Finalize

   subroutine finalize_pflogger()
      call logging%free()
   end subroutine finalize_pflogger

   subroutine initialize_pflogger(unusable,comm,logging_config,rc)
      use pflogger, only: pfl_initialize => initialize
      use pflogger, only: StreamHandler, FileHandler, HandlerVector
      use pflogger, only: MpiLock, MpiFormatter
      use pflogger, only: INFO, WARNING
      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      character(len=*), optional,intent(in) :: logging_config
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      character(:), allocatable :: logging_configuration_file
      integer :: comm_world
      type(Logger), pointer :: lgr

      _UNUSED_DUMMY(unusable)
      if (present(logging_config)) then
         logging_configuration_file=logging_config
      else
         logging_configuration_file=''
      end if
      if (present(comm)) then
         call MPI_Comm_dup(comm,comm_world,status)
         _VERIFY(status)
      else
         comm_world=MPI_COMM_WORLD
      end if

      call pfl_initialize()

      if (logging_configuration_file /= '') then
         call logging%load_file(logging_configuration_file)
      else

         call MPI_COMM_Rank(comm_world,rank,status)
         console = StreamHandler(OUTPUT_UNIT)
         call console%set_level(INFO)
         call console%set_formatter(MpiFormatter(comm_world, fmt='%(short_name)a10~: %(message)a'))
         call handlers%push_back(console)

         file_handler = FileHandler('warnings_and_errors.log')
         call file_handler%set_level(WARNING)
         call file_handler%set_formatter(MpiFormatter(comm_world, fmt='pe=%(mpi_rank)i5.5~: %(short_name)a~: %(message)a'))
         call file_handler%set_lock(MpiLock(comm_world))
         call handlers%push_back(file_handler)

         if (rank == 0) then
            level = INFO
         else
            level = WARNING
         end if

         call logging%basic_config(level=level, handlers=handlers, rc=status)
         _VERIFY(status)

         if (rank == 0) then
            lgr => logging%get_logger('MAPL')
            call lgr%warning('No configure file specified for logging layer.  Using defaults.')            
         end if

      end if
      _RETURN(_SUCCESS)

   end subroutine initialize_pflogger

   subroutine start_global_profiler(unusable,comm,rc)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc
      class (BaseProfiler), pointer :: t_p
      integer :: world_comm,status

      _UNUSED_DUMMY(unusable)
      if (present(comm)) then
         call MPI_Comm_dup(comm,world_comm,status)
         _VERIFY(status)
      else
         world_comm=MPI_COMM_WORLD
      end if
      t_p => get_global_time_profiler()
      t_p = TimeProfiler('All', comm_world = world_comm)
      call t_p%start()
   end subroutine start_global_profiler

   subroutine stop_global_profiler()
      class (BaseProfiler), pointer :: t_p

      t_p => get_global_time_profiler()
      call t_p%stop('All')
   end subroutine stop_global_profiler

   subroutine report_global_profiler(unusable,comm,rc)
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

      _UNUSED_DUMMY(unusable)
      if (present(comm)) then
         call MPI_comm_dup(comm,world_comm,ierror)
         _VERIFY(ierror)
      else
         world_comm=MPI_COMM_WORLD
      end if
      t_p => get_global_time_profiler()

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

      call MPI_Comm_size(world_comm, npes, ierror)
      call MPI_Comm_Rank(world_comm, my_rank, ierror)

      if (my_rank == 0) then
            report_lines = reporter%generate_report(t_p)
            write(*,'(a,1x,i0)')'Report on process: ', my_rank
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
       end if
       call MPI_Barrier(world_comm, ierror)

   end subroutine report_global_profiler

end module MAPL_ApplicationSupport
