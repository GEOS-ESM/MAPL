#include "unused_dummy.H"

module mapl_NumCyclesColumn_mod
   use mapl_AbstractColumn_mod
   use mapl_SimpleColumn_mod
   use mapl_AbstractMeterNode_mod
   use mapl_AbstractMeter_mod
   implicit none
   private

   public :: NumCyclesColumn

   type, extends(SimpleColumn) :: NumCyclesColumn
      private
   contains
      procedure :: get_row
   end type NumCyclesColumn


contains


   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class(NumCyclesColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      class (AbstractMeter), pointer :: tmr

      _UNUSED_DUMMY(this)

      tmr => node%get_meter()

      allocate(row, source=tmr%get_num_cycles())

   end function get_row


end module mapl_NumCyclesColumn_mod
