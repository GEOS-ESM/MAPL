module AEIO_WriterProfiler
   use MAPL_Profiler
   implicit none
   private
 
   public writer_prof
   public generate_writer_summary
   public start_writer_prof

   type(DistributedProfiler), save :: writer_prof

contains

   subroutine start_writer_prof(comm)
        integer, intent(in) :: comm
        writer_prof = DistributedProfiler('writer',MpiTimerGauge(),comm)
        call writer_prof%start()
   end subroutine start_writer_prof

   subroutine generate_writer_summary(rank)
      integer, intent(in) :: rank
      type (ProfileReporter) :: reporter
         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)


         call writer_prof%finalize()
         call writer_prof%reduce()
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
        report_lines = reporter%generate_report(writer_prof)
         if (rank==0) then
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
            write(*,'(a)') ''
         end if

   end subroutine


end module AEIO_WriterProfiler
