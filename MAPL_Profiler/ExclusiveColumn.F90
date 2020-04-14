module MAPL_ExclusiveColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractColumn
   use MAPL_SimpleColumn
   use MAPL_AbstractMeterNode
   use MAPL_AbstractMeter
   use Mapl_DistributedMeter
   implicit none
   private

   public :: ExclusiveColumn

   type, extends(SimpleColumn) :: ExclusiveColumn
      private
      character(:), allocatable :: option
   contains
      procedure :: get_row
      procedure :: get_row_dist
   end type ExclusiveColumn

   interface ExclusiveColumn
      module procedure :: new_ExclusiveColumn
   end interface ExclusiveColumn


contains


   function new_ExclusiveColumn(option) result(column)
      type(ExclusiveColumn) :: column
      character(*), optional, intent(in) :: option
      if (present(option)) column%option = option
   end function new_ExclusiveColumn


   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class (ExclusiveColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      
      if (.not. allocated(this%option)) then
         allocate(row, source=node%get_exclusive())
      else
         call this%get_row_dist(node, row)
      end if

   end function get_row


   subroutine get_row_dist(this, node, row)
      class(*), allocatable, intent(out) :: row
      class (ExclusiveColumn), target, intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      class(AbstractMeter), pointer :: m
      type(DistributedStatistics) :: stats
      type(DistributedReal64) :: exclusive

      m => node%get_meter()

      select type (m)
      class is (DistributedMeter)
         stats = m%get_statistics()
         exclusive = stats%exclusive
         call this%fill_row(exclusive, this%option, row)
      end select

   end subroutine get_row_dist

end module MAPL_ExclusiveColumn


