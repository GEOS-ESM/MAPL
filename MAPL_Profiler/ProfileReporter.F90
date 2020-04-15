module MAPL_ProfileReporter
   use MAPL_AbstractMeterNode
   use MAPL_TextColumn
   use MAPL_SeparatorColumn
   use MAPL_TextColumnVector
   use MAPL_MultiColumn
   use MAPL_BaseProfiler
   implicit none
   private

   public :: ProfileReporter

   type, extends(MultiColumn) :: ProfileReporter
      private
   contains
      procedure :: generate_report_profiler
      generic :: generate_report => generate_report_profiler
   end type ProfileReporter


   interface ProfileReporter
      module procedure :: new_ProfileReporter
   end interface ProfileReporter

   
contains

   function new_ProfileReporter(header) result(reporter)
      type(ProfileReporter) :: reporter
      character(*), intent(in) :: header(:)
      reporter%MultiColumn = MultiColumn(header)
   end function new_ProfileReporter


   function generate_report_profiler(this, p) result(report_lines)
      character(:), allocatable :: report_lines(:)
      class (ProfileReporter), target, intent(in) :: this
      class (BaseProfiler), target, intent(in) :: p

      integer :: width, height
      integer :: i
      character(:), allocatable :: rows(:)
      character(:), allocatable :: header(:)
      class (AbstractMeterNode), pointer :: node

      call this%get_header(header)
      node => p%get_root_node()
      call this%get_rows(node, rows)
      width = this%get_width()
      height = size(header) + size(rows)

      allocate(character(len=width) :: report_lines(height))
      do i = 1, size(header)
         report_lines(i) = header(i)
      end do
      do i = size(header)+1, height
         report_lines(i) = rows(i - size(header))
      end do

   end function generate_report_profiler
   

   
end module MAPL_ProfileReporter
