module MAPL_NumCyclesColumn
   use MAPL_AbstractColumn
   use MAPL_SimpleColumn
   use MAPL_AbstractMeterNode
   use MAPL_AdvancedMeter
   use MAPL_AbstractMeter
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

      tmr => node%get_meter()

      select type (tmr)
      class is (AdvancedMeter)
         allocate(row, source=tmr%get_num_cycles())
!!$         row = num_cycles
      class default
         print*,'error handling here'
      end select

   end function get_row


end module MAPL_NumCyclesColumn
