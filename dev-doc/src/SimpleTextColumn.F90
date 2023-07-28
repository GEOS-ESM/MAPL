module MAPL_SimpleTextColumn
   use MAPL_TextColumn
   use MAPL_AbstractMeterNode
   implicit none
   private

   public :: SimpleTextColumn

   type, abstract, extends(TextColumn) :: SimpleTextColumn
      private
   contains
      procedure :: get_rows_range
      procedure :: get_rows
      procedure(i_get_row), deferred :: get_row
   end type SimpleTextColumn

   abstract interface

      function i_get_row(this, node) result(row)
         use MAPL_AbstractMeterNode
         import SimpleTextColumn
         character(:), allocatable :: row
         class (SimpleTextColumn), intent(in) :: this
         class (AbstractMeterNode), intent(in) :: node
      end function i_get_row

   end interface


contains


   ! Using subroutines instead of functions as a workaround for gfortran 8.2
   ! Reproducer being submitted by Damian Rouson (10/12/2018)
   subroutine get_rows_range(this, begin, end, rows)
      class (SimpleTextColumn), target, intent(in) :: this
      class (AbstractMeterNodeIterator), intent(in) :: begin
      class (AbstractMeterNodeIterator), intent(in) :: end
      character(:), allocatable, intent(inout) :: rows(:)

      class (AbstractMeterNodeIterator), allocatable :: iter
      integer :: i
      integer :: width
      class (AbstractMeterNode), pointer :: subnode

      ! count_nodes
      iter = begin
      i = 0
      do while (iter /= end)
         i = i + 1
         call iter%next()
      end do

      width = this%get_width()
      allocate(character(width) :: rows(i))

      ! Fill rows
      iter = begin
      i = 0
      do while (iter /= end)
         i = i + 1
         subnode => iter%get()
         rows(i) = this%get_row(subnode)
         call iter%next()
      end do
      
   end subroutine get_rows_range


  subroutine get_rows(this, node, rows)
      class (SimpleTextColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      character(:), allocatable, intent(out) :: rows(:)

      class (AbstractMeterNodeIterator), allocatable :: b, e

      b = node%begin()
      e = node%end()

      call this%get_rows_range(b, e, rows)

   end subroutine get_rows
   
end module MAPL_SimpleTextColumn
