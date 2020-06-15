program main
   use MAPL_Profiler
   use MPI
   implicit none


   type (MemoryProfiler), target :: mem_prof
   type (DistributedProfiler), target :: main_prof
   type (DistributedProfiler), target :: lap_prof
   type (ProfileReporter) :: reporter, main_reporter
   type (ProfileReporter) :: mem_reporter

   character(:), allocatable :: report_lines(:)
   integer :: i
   integer :: rank, ierror

!!$   mem_prof = MemoryProfiler('TOTAL')

   call MPI_Init(ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   main_prof = DistributedProfiler('TOTAL', MpiTimerGauge(), MPI_COMM_WORLD)   ! timer 1
   lap_prof = DistributedProfiler('Lap', MpiTimerGauge(), MPI_COMM_WORLD)
   
   call main_prof%start('init reporter')
   call reporter%add_column(NameColumn(20))
   call reporter%add_column(FormattedTextColumn('#-cycles','(i5.0)', 5, NumCyclesColumn()))
   call reporter%add_column(FormattedTextColumn(' T(inc)','(f9.6)', 9, InclusiveColumn()))
   call reporter%add_column(FormattedTextColumn(' T(exc)','(f9.6)', 9, ExclusiveColumn()))
   call reporter%add_column(FormattedTextColumn('%(inc)','(f6.2)', 6, PercentageColumn(InclusiveColumn())))
   call reporter%add_column(FormattedTextColumn('%(exc)','(f6.2)', 6, PercentageColumn(ExclusiveColumn())))
   call reporter%add_column(FormattedTextColumn('  std. dev ','(f12.4)', 12, StdDevColumn()))
   call reporter%add_column(FormattedTextColumn('  rel. dev ','(f12.4)', 12, StdDevColumn(relative=.true.)))
   call reporter%add_column(FormattedTextColumn('  max cyc ','(f12.8)', 12, MaxCycleColumn()))
   call reporter%add_column(FormattedTextColumn('  min cyc ','(f12.8)', 12, MinCycleColumn()))
   call reporter%add_column(FormattedTextColumn(' mean cyc','(f12.8)', 12, MeanCycleColumn()))

   call main_reporter%add_column(NameColumn(20))
   call main_reporter%add_column(FormattedTextColumn('Inclusive','(f9.6)', 9, InclusiveColumn('MEAN')))
   call main_reporter%add_column(FormattedTextColumn('% Incl','(f6.2)', 6, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
   call main_reporter%add_column(FormattedTextColumn('Exclusive','(f9.6)', 9, ExclusiveColumn('MEAN')))
   call main_reporter%add_column(FormattedTextColumn('% Excl','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN'))))
   call main_reporter%add_column(FormattedTextColumn(' Max Excl)','(f9.6)', 9, ExclusiveColumn('MAX')))
   call main_reporter%add_column(FormattedTextColumn(' Min Excl)','(f9.6)', 9, ExclusiveColumn('MIN')))
   call main_reporter%add_column(FormattedTextColumn('Max PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MAX_PE')))
   call main_reporter%add_column(FormattedTextColumn('Min PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MIN_PE')))

   call mem_reporter%add_column(NameColumn(20))
   call mem_reporter%add_column(FormattedTextColumn('#-cycles','(i5.0)', 5, NumCyclesColumn()))
   !call mem_reporter%add_column(MemoryTextColumn('  RSS  ','(i4,1x,a2)', 7, InclusiveColumn()))
   !call mem_reporter%add_column(MemoryTextColumn('Cyc RSS','(i4,1x,a2)', 7, MeanCycleColumn()))

   call main_prof%stop('init reporter')


!!$   call mem_prof%start('lap')
   call do_lap(lap_prof) ! lap 1
   call lap_prof%finalize()
   call main_prof%accumulate(lap_prof)
!!$   call mem_prof%stop('lap')


   call main_prof%start('use reporter')
   if (rank == 0) then
      report_lines = reporter%generate_report(lap_prof)
      write(*,'(a)')'Lap 1'
      write(*,'(a)')'====='
      do i = 1, size(report_lines)
         write(*,'(a)') report_lines(i)
      end do
      write(*,'(a)')''
   end if
   call main_prof%stop('use reporter')

!!$   call mem_prof%start('lap')
   call lap_prof%reset()
   call do_lap(lap_prof) ! lap 2
   call lap_prof%finalize()
   call main_prof%accumulate(lap_prof)
   call main_prof%start('use reporter')

   if (rank == 0) then
      report_lines = reporter%generate_report(lap_prof)
      write(*,'(a)')'Lap 2'
      write(*,'(a)')'====='
      do i = 1, size(report_lines)
         write(*,'(a)') report_lines(i)
      end do
      write(*,'(a)') ''
   end if

   call main_prof%stop('use reporter')
!!$   call mem_prof%stop('lap')

   call main_prof%finalize()
   call main_prof%reduce()
   report_lines = reporter%generate_report(main_prof)
   if (rank == 0) then
      write(*,'(a)')'Final profile(0)'
      write(*,'(a)')'============='
      do i = 1, size(report_lines)
         write(*,'(a)') report_lines(i)
      end do
      write(*,'(a)') ''
   end if
   call MPI_Barrier(MPI_COMM_WORLD, ierror)
   if (rank == 1) then
      write(*,'(a)')'Final profile (1)'
      write(*,'(a)')'================'
      do i = 1, size(report_lines)
         write(*,'(a)') report_lines(i)
      end do
      write(*,'(a)') ''
   end if
   call MPI_Barrier(MPI_COMM_WORLD, ierror)

   report_lines = main_reporter%generate_report(main_prof)
   if (rank == 0) then
      write(*,'(a)')'Parallel profile'
      write(*,'(a)')'================'
      do i = 1, size(report_lines)
         write(*,'(a)') report_lines(i)
      end do
      write(*,'(a)') ''
   end if
   
!!$   call mem_prof%finalize()
!!$   if (rank == 0) then
!!$      report_lines = mem_reporter%generate_report(mem_prof)
!!$      write(*,'(a)')'Memory profile'
!!$      write(*,'(a)')'=============='
!!$      do i = 1, size(report_lines)
!!$         write(*,'(a)') report_lines(i)
!!$      end do
!!$      write(*,'(a)') ''
!!$   end if

   call MPI_Finalize(ierror)

contains

   subroutine do_lap(prof)
      type (DistributedProfiler), target :: prof

      real, pointer :: x(:)

      call prof%start('timer_1') ! 2
      allocate(x(10**7 * rank))
      call random_number(x)
      print*,sum(x)
      call prof%start('timer_1a')! 3
      call prof%stop('timer_1a')
      call prof%start('timer_1b') ! 4
      call prof%start('timer_1b1') ! 5
      call prof%stop('timer_1b1')
      call prof%stop('timer_1b')
      call prof%stop('timer_1')
      call prof%start('timer_2') ! 6
      call prof%start('timer_2b')! 7
      call prof%stop('timer_2b')
      call prof%stop('timer_2')

      call prof%start('timer_1') ! 2
      block
        real, allocatable :: x(:)
        allocate(x(1000000))
        call random_number(x)
        print*,'sum: ', sum(exp(x))
        deallocate(x)
      end block
      call prof%start('timer_1a')! 3
      call prof%stop('timer_1a')
      call prof%stop('timer_1')

      call prof%start('timer_2') ! 6
      call prof%stop('timer_2')
      call prof%start('timer_2') ! 6
      call prof%stop('timer_2')
   end subroutine do_lap

end program main

