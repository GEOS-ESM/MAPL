module MAPL_InclusiveColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractColumn
   use MAPL_SimpleColumn
   use MAPL_AbstractMeterNode
   use MAPL_AbstractMeter
   use Mapl_DistributedMeter
   implicit none
   private

   public :: InclusiveColumn

   type, extends(SimpleColumn) :: InclusiveColumn
      private
      character(:), allocatable :: option
   contains
      procedure :: get_row
      procedure :: get_row_dist
   end type InclusiveColumn

   interface InclusiveColumn
      module procedure :: new_InclusiveColumn
   end interface InclusiveColumn


contains


   function new_InclusiveColumn(option) result(column)
      type(InclusiveColumn) :: column
      character(*), optional, intent(in) :: option
      if (present(option)) column%option = option
   end function new_InclusiveColumn


   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class (InclusiveColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      if (.not. allocated(this%option)) then
         allocate(row, source=node%get_inclusive())
      else
         call this%get_row_dist(node, row)
      end if

   end function get_row


   subroutine get_row_dist(this, node, row)
      class (InclusiveColumn), target, intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      class(*), allocatable, intent(out) :: row

      class(AbstractMeter), pointer :: m
      type(DistributedStatistics) :: stats
      type(DistributedReal64) :: inclusive

      m => node%get_meter()

      select type (m)
      class is (DistributedMeter)
         stats = m%get_statistics()
         inclusive = stats%total

         call this%fill_row(inclusive, this%option, row)
      end select

   end subroutine get_row_dist

end module MAPL_InclusiveColumn
