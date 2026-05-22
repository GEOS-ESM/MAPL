module mapl_SimpleColumn_mod
   use mapl_AbstractColumn_mod
   use GFTL_UnlimitedVector
   use mapl_AbstractMeterNode_mod
   use mapl_DistributedMeter_mod
   implicit none
   private

   public :: SimpleColumn

   type, abstract, extends(AbstractColumn) :: SimpleColumn
      private
   contains
      procedure :: get_rows
      procedure(i_get_row), deferred :: get_row
   end type SimpleColumn


   abstract interface

      function i_get_row(this, node) result(row)
         import SimpleColumn
         import AbstractMeterNode
         ! Some columns return reals, others return integers
         class(*), allocatable :: row
         class(SimpleColumn), intent(in) :: this
         class(AbstractMeterNode), target, intent(in) :: node

      end function i_get_row

   end interface


contains


   function get_rows(this, node) result(rows)
      type (UnlimitedVector) :: rows
      class (SimpleColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      integer :: i
      class (AbstractMeterNodeIterator), allocatable :: iter
      class (AbstractMeterNode), pointer :: subnode
      

      iter = node%begin()
      i = 0
      do while (iter /= node%end())
         i = i + 1
         subnode => iter%get()
         call rows%push_back(this%get_row(subnode))
         call iter%next()
      end do
      
   end function get_rows


end module mapl_SimpleColumn_mod
