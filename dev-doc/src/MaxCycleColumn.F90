module MAPL_MaxCycleColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_SimpleColumn
   use MAPL_AbstractMeterNode
   use MAPL_AbstractMeter
   use MAPL_AdvancedMeter
   use Mapl_DistributedMeter
   implicit none
   private

   public :: MaxCycleColumn

   type, extends(SimpleColumn) :: MaxCycleColumn
      private
      character(:), allocatable :: option
   contains
      procedure :: get_row
      procedure :: get_row_dist
   end type MaxCycleColumn

   interface MaxCycleColumn
      module procedure :: new_MaxCycleColumn
   end interface MaxCycleColumn


contains


   function new_MaxCycleColumn(option) result(column)
      type(MaxCycleColumn) :: column
      character(*), optional, intent(in) :: option
      if (present(option)) column%option = option
   end function new_MaxCycleColumn


   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class (MaxCycleColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      class (AbstractMeter), pointer :: tmr

      tmr => node%get_meter()
      select type (tmr)
      class is (AdvancedMeter)
         if (.not. allocated(this%option)) then
            allocate(row, source=tmr%get_max_cycle())
         else
            call this%get_row_dist(node, row)
         end if
      end select

   end function get_row


   subroutine get_row_dist(this, node, row)
      class (MaxCycleColumn), target, intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      class(*), allocatable, intent(out) :: row

      class(AbstractMeter), pointer :: m
      type(DistributedStatistics) :: stats
      type(DistributedReal64) :: max_cycle

      m => node%get_meter()

      select type (m)
      class is (DistributedMeter)
         stats = m%get_statistics()
         max_cycle = stats%max_cycle

         call this%fill_row(max_cycle, this%option, row)

      end select

   end subroutine get_row_dist

end module MAPL_MaxCycleColumn


