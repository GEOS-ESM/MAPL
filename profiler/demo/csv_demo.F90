program csv_demo
   use MAPL_Profiler
   use gFTL2_StringVector
   use esmf, only: ESMF_HConfig, ESMF_HConfigCreate, ESMF_HConfigDestroy

   implicit none

   type(TimeProfiler), target :: prof
   type(CsvProfileReporter) :: csv_reporter
   type(ProfileReporter) :: text_reporter
   type(ESMF_HConfig) :: config
   type(StringVector) :: csv_lines, text_lines
   type(StringVectorIterator) :: iter
   integer :: i
   character(1) :: empty(0)

   ! Create a profiler with nested timers
   prof = TimeProfiler('Application')
   
   call prof%start()
   
   ! Simulate some work with nested timers
   call prof%start('Initialization')
   call simulate_work(0.1)
   call prof%stop('Initialization')

   call prof%start('Main Loop')
   do i = 1, 3
      call prof%start('Iteration')
      
      call prof%start('Physics')
      call simulate_work(0.05)
      call prof%stop('Physics')
      
      call prof%start('Dynamics')
      call simulate_work(0.08)
      call prof%stop('Dynamics')
      
      call prof%start('I/O')
      call simulate_work(0.02)
      call prof%stop('I/O')
      
      call prof%stop('Iteration')
   end do

   call prof%stop('Main Loop')
   
   call prof%start('Finalization')
   call simulate_work(0.05)
   call prof%stop('Finalization')
   
   call prof%stop()
   call prof%finalize()

   ! Create a configuration with multiple column types including nested multi-columns
   ! Note: CSV output uses 'name' (plain names) and 'depth' (numeric level) to show hierarchy
   ! Note: 'name' and 'depth' column types use default headers ("Name" and "Depth")
   config = ESMF_HConfigCreate(content='{ &
      &columns: [ &
      &  {type: depth, format: "(i2)"}, &
      &  {type: name, width: 25}, &
      &  {type: separator, char: "|"}, &
      &  {type: num_cycles, name: "Count", format: "(i6)", width: 6}, &
      &  {type: multi, name: "Timing", separator: "=", &
      &   columns: [ &
      &     {type: multi, name: "Inclusive", separator: "-", &
      &      columns: [ &
      &        {type: inclusive, name: "Inc_Time", format: "(f10.6)", width: 10}, &
      &        {type: percentage_inclusive, name: "Inc_Pct", format: "(f6.2)", width: 6} &
      &      ]}, &
      &     {type: multi, name: "Exclusive", separator: "-", &
      &      columns: [ &
      &        {type: exclusive, name: "Exc_Time", format: "(f10.6)", width: 10}, &
      &        {type: percentage_exclusive, name: "Exc_Pct", format: "(f6.2)", width: 6} &
      &      ]} &
      &   ]}, &
      &  {type: multi, name: "Statistics", separator: "-", &
      &   columns: [ &
      &     {type: mean_cycle, name: "Mean", format: "(f10.6)", width: 10}, &
      &     {type: std_dev, name: "StdDev", format: "(f10.6)", width: 10} &
      &   ]} &
      &]}')

   ! Generate and display TEXT report
   print *, ''
   print *, '========================================='
   print *, 'TEXT FORMAT (ProfileReporter):'
   print *, 'Note: Hierarchical headers with nested multi-columns'
   print *, '========================================='
   text_reporter = ProfileReporter(empty, config)
   text_lines = text_reporter%generate_report(prof)
   
   iter = text_lines%begin()
   do while (iter /= text_lines%end())
      print '(a)', iter%of()
      call iter%next()
   end do

   ! Generate and display CSV report
   print *, ''
   print *, '========================================='
   print *, 'CSV FORMAT (CsvProfileReporter):'
   print *, 'Note: Nested multi-columns are fully flattened'
   print *, 'Note: Timer names are plain (no "--" prefix), depth shown as numeric column'
   print *, '========================================='
   csv_reporter = CsvProfileReporter(config)
   csv_lines = csv_reporter%generate_report(prof)
   
   iter = csv_lines%begin()
   do while (iter /= csv_lines%end())
      print '(a)', iter%of()
      call iter%next()
   end do

   ! Save CSV to file for Excel import
   call save_csv_to_file(csv_lines, 'profiler_report.csv')
   print *, ''
   print *, 'CSV saved to: profiler_report.csv'

   print *, ''
   print *, '========================================='
   print *, 'To import into Excel:'
   print *, '1. Open Excel'
   print *, '2. File > Import > CSV file'
   print *, '3. Select profiler_report.csv'
   print *, '4. Choose "Comma" as delimiter'
   print *, '   (Or just double-click the .csv file)'
   print *, '========================================='
   print *, ''
   print *, '========================================='
   print *, 'Notes:'
   print *, '- TEXT: Hierarchical headers in formatted table'
   print *, '- CSV:  Multi-row headers preserve hierarchy'
   print *, '- CSV Row 1: Timer,Count,Timing,,,,Statistics,'
   print *, '- CSV Row 2: ,,,Inclusive,,Exclusive,,Mean,StdDev'
   print *, '- CSV Row 3: ,,,Inc_Time,Inc_Pct,Exc_Time,Exc_Pct,,'
   print *, '- Empty cells separate grouped columns'
   print *, '- Imports cleanly into Excel/Google Sheets'
   print *, '========================================='

   call ESMF_HConfigDestroy(config)

contains

   subroutine save_csv_to_file(csv_lines, filename)
      type(StringVector), intent(in) :: csv_lines
      character(*), intent(in) :: filename
      type(StringVectorIterator) :: iter
      integer :: unit

      open(newunit=unit, file=filename, status='replace', action='write')
      
      iter = csv_lines%begin()
      do while (iter /= csv_lines%end())
         write(unit, '(a)') iter%of()
         call iter%next()
      end do
      
      close(unit)
   end subroutine save_csv_to_file

   subroutine simulate_work(seconds)
      real, intent(in) :: seconds
      real :: start, finish
      
      call cpu_time(start)
      do while (.true.)
         call cpu_time(finish)
         if (finish - start >= seconds) exit
      end do
   end subroutine simulate_work

end program csv_demo
