module AEIO_IOProfiler
   use MAPL_Profiler
   use gFTL_StringVector
   implicit none
   private
 
   public io_prof
   public generate_io_summary
   public start_io_prof

   type(DistributedProfiler), save :: io_prof

contains

   subroutine start_io_prof(comm,collections)
        integer, intent(in) :: comm
        type(StringVector), intent(in) :: collections

        character(len=:), allocatable :: collection_name
        type(StringVectorIterator) :: collection_iter

        io_prof = DistributedProfiler('io_controller',MpiTimerGauge(),comm)
        call io_prof%start()
        call io_prof%start('full_app')
        call io_prof%start('io_initialize')
           call io_prof%start('trans_grid_to_front')
           call io_prof%stop('trans_grid_to_front')
           call io_prof%start('client_server_rh_gen')
           call io_prof%stop('client_server_rh_gen')
           call io_prof%start('server_writer_rh_gen')
           call io_prof%stop('server_writer_rh_gen')
        call io_prof%stop('io_initialize')
        call io_prof%start('server_run')
           call io_prof%start('client-server-trans')
           collection_iter = collections%begin()
           do while(collection_iter/=collections%end())
              collection_name = collection_iter%get()
              call io_prof%start('data_from_client_'//collection_name)
              call io_prof%stop('data_from_client_'//collection_name)
              call collection_iter%next()
           enddo 
           collection_iter = collections%begin()
           do while(collection_iter/=collections%end())
              collection_name = collection_iter%get()
              call io_prof%start('data_to_server_'//collection_name)
              call io_prof%stop('data_to_server_'//collection_name)
              call collection_iter%next()
           enddo 
           call io_prof%stop('client-server-trans')
           call io_prof%start('server-writer-trans')
           collection_iter = collections%begin()
           do while(collection_iter/=collections%end())
              collection_name = collection_iter%get()
              call io_prof%start('transfer_rh_'//collection_name)
              call io_prof%stop('transfer_rh_'//collection_name)
              call collection_iter%next()
           enddo 
           collection_iter = collections%begin()
           do while(collection_iter/=collections%end())
              collection_name = collection_iter%get()
              call io_prof%start('offload_data_'//collection_name)
              call io_prof%stop('offload_data_'//collection_name)
              call collection_iter%next()
           enddo 
           call io_prof%stop('server-writer-trans')
        call io_prof%stop('server_run')
        call io_prof%start('start_writer')
           collection_iter = collections%begin()
           do while(collection_iter/=collections%end())
              collection_name = collection_iter%get()
              call io_prof%start('write_collection_'//collection_name)
                 call io_prof%start('start_write_epoch_'//collection_name)
                 call io_prof%stop('start_write_epoch_'//collection_name)
              call io_prof%stop('write_collection_'//collection_name)
              call collection_iter%next()
           enddo 
        call io_prof%stop('start_writer')
   end subroutine start_io_prof

   subroutine generate_io_summary(rank)
      integer, intent(in) :: rank
      type (ProfileReporter) :: reporter
         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)

         call io_prof%stop('full_app')
         call io_prof%stop()
         !call io_prof%finalize()
         call io_prof%reduce()
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(40))
         call reporter%add_column(FormattedTextColumn('Inclusive','(f9.2)', 11, InclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Incl','(f9.2)', 11, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
         call reporter%add_column(FormattedTextColumn(' Max Incl)','(f9.2)', 11, InclusiveColumn('MAX')))
         call reporter%add_column(FormattedTextColumn(' Min INcl)','(f9.2)', 11, InclusiveColumn('MIN')))
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
