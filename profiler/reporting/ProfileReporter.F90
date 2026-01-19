module MAPL_ProfileReporter
   use MAPL_AbstractMeterNode
   use MAPL_TextColumn
   use MAPL_SeparatorColumn
   use MAPL_TextColumnVector
   use MAPL_MultiColumn
   use MAPL_BaseProfiler
   use gFTL2_StringVector
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
      type(StringVector) :: report_lines
      class (ProfileReporter), target, intent(in) :: this
      class (BaseProfiler), target, intent(in) :: p

      integer :: i
      character(:), allocatable :: rows(:)
      character(:), allocatable :: header(:)
      class (AbstractMeterNode), pointer :: node

      call this%get_header(header)
      node => p%get_root_node()
      call this%get_rows(node, rows)
      
      do i = 1, size(header)
         call report_lines%push_back(header(i))
      end do
      do i = 1, size(rows)
         call report_lines%push_back(rows(i))
      end do

   end function generate_report_profiler
   

   
end module MAPL_ProfileReporter
