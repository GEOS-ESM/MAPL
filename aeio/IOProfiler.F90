module AEIO_IOProfiler
   use MAPL_Profiler
   implicit none
   private
 
   public io_prof
   public generate_io_summary
   public start_io_prof

   type(DistributedProfiler), save :: io_prof

contains

   subroutine start_io_prof(comm)
        integer, intent(in) :: comm
        io_prof = DistributedProfiler('io_controller',MpiTimerGauge(),comm)
        call io_prof%start()
        call io_prof%start('global_io_prof')
   end subroutine start_io_prof

   subroutine generate_io_summary(rank)
      integer, intent(in) :: rank
      type (ProfileReporter) :: reporter
         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)


        call io_prof%stop('global_io_prof')
         call io_prof%finalize()
         call io_prof%reduce()
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(30))
         call reporter%add_column(FormattedTextColumn('Inclusive','(f9.2)', 11, InclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Incl','(f9.2)', 11, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
         call reporter%add_column(FormattedTextColumn('Exclusive','(f9.2)', 11, ExclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Excl','(f9.2)', 11, PercentageColumn(ExclusiveColumn('MEAN'))))
         call reporter%add_column(FormattedTextColumn(' Max Excl)','(f9.2)', 11, ExclusiveColumn('MAX')))
         call reporter%add_column(FormattedTextColumn(' Min Excl)','(f9.2)', 11, ExclusiveColumn('MIN')))
         call reporter%add_column(FormattedTextColumn('Max PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MAX_PE')))
         call reporter%add_column(FormattedTextColumn('Min PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MIN_PE')))
        report_lines = reporter%generate_report(io_prof)
         if (rank==0) then
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
            write(*,'(a)') ''
         end if

   end subroutine


end module AEIO_IOProfiler
