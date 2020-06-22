program main
   use MPI
   use MAPL_Profiler
   implicit none


   !type (MemoryProfiler), target :: mem_prof
   type (TimeProfiler), target :: main_prof
   type (TimeProfiler), target :: lap_prof
   type (ProfileReporter) :: reporter
   !type (ProfileReporter) :: mem_reporter

   character(:), allocatable :: report_lines(:)
   integer :: i
   integer :: ierror

   call MPI_Init(ierror)
   main_prof = TimeProfiler('TOTAL')   ! timer 1
   call main_prof%start()
   lap_prof = TimeProfiler('Lap')
   call lap_prof%start()
   !mem_prof = MemoryProfiler('TOTAL')
   
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

   !call mem_reporter%add_column(NameColumn(20))
   !call mem_reporter%add_column(FormattedTextColumn('#-cycles','(i5.0)', 5, NumCyclesColumn()))
   !call mem_reporter%add_column(MemoryTextColumn('  RSS  ','(i4,1x,a2)', 7, InclusiveColumn()))
   !call mem_reporter%add_column(MemoryTextColumn('Cyc RSS','(i4,1x,a2)', 7, MeanCycleColumn()))

   call main_prof%stop('init reporter')


   !call mem_prof%start('lap')
   call do_lap(lap_prof) ! lap 1
   call lap_prof%stop()
   call main_prof%accumulate(lap_prof)
   !call mem_prof%stop('lap')


   call main_prof%start('use reporter')
   report_lines = reporter%generate_report(lap_prof)
   write(*,'(a)')'Lap 1'
   write(*,'(a)')'====='
   do i = 1, size(report_lines)
      write(*,'(a)') report_lines(i)
   end do
   write(*,'(a)')''
   call main_prof%stop('use reporter')

   !call mem_prof%start('lap')
   call lap_prof%reset()
   call do_lap(lap_prof) ! lap 2
   call lap_prof%stop()
   call main_prof%accumulate(lap_prof)
   call main_prof%start('use reporter')
   report_lines = reporter%generate_report(lap_prof)
   write(*,'(a)')'Lap 2'
   write(*,'(a)')'====='
   do i = 1, size(report_lines)
      write(*,'(a)') report_lines(i)
   end do
   write(*,'(a)') ''
   call main_prof%stop('use reporter')
   !call mem_prof%stop('lap')

   call main_prof%stop()
   report_lines = reporter%generate_report(main_prof)
   write(*,'(a)')'Final profile'
   write(*,'(a)')'============='
   do i = 1, size(report_lines)
      write(*,'(a)') report_lines(i)
   end do
   write(*,'(a)') ''


   call MPI_Finalize(ierror)

   !call mem_prof%finalize()
   !report_lines = mem_reporter%generate_report(mem_prof)
   !write(*,'(a)')'Memory profile'
   !write(*,'(a)')'=============='
   !do i = 1, size(report_lines)
   !   write(*,'(a)') report_lines(i)
   !end do
   !write(*,'(a)') ''

contains

   subroutine do_lap(prof)
      type (TimeProfiler), target :: prof

      real, pointer :: x(:)

      allocate(x(10**7))
      call random_number(x)
      print*,sum(x)
      call prof%start('timer_1') ! 2
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
      call prof%start('timer_1a')! 3
      call prof%stop('timer_1a')
      call prof%stop('timer_1')

      call prof%start('timer_2') ! 6
      call prof%stop('timer_2')
      call prof%start('timer_2') ! 6
      call prof%stop('timer_2')
   end subroutine do_lap


end program main

