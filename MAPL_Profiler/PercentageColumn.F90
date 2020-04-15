module MAPL_PercentageColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractMeterNode
   use MAPL_AbstractColumn
   implicit none
   private

   public :: PercentageColumn

   type, extends(AbstractColumn) :: PercentageColumn
      private
      character(:), allocatable :: mode
      class (AbstractColumn), allocatable :: reference_column
   contains
      procedure :: get_rows
      procedure :: get_row
   end type PercentageColumn

   interface PercentageColumn
      module procedure new_PercentageColumn
   end interface PercentageColumn


contains


   function new_PercentageColumn(reference_column, mode) result(column)
      type (PercentageColumn) :: column
      class (AbstractColumn), intent(in) :: reference_column
      character(*), optional, intent(in) :: mode

      column%reference_column = reference_column
      if (present(mode)) then
         column%mode = mode
      else
         column%mode = 'TOTAL'
      end if

   end function new_PercentageColumn


   function get_rows(this, node) result(rows)
      use GFTL_UnlimitedVector
      type (UnlimitedVector) :: rows
      class (PercentageColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      type (UnlimitedVector) :: values
      integer :: i
      real(kind=REAL64) :: s, x

      values = this%reference_column%get_rows(node)

      s = 0
      do i = 1, values%size()
         select type (v => values%at(i))
         type is (real(kind=REAL64))
            x = v
         type is (integer)
            x = v
         end select
         
         select case (this%mode)
         case ('TOTAL')
            s = s + x
         case ('MAX')
            s = max(s, x)
         end select

      end do

      do i = 1, values%size()
         select type (v => values%at(i))
         type is (real(kind=REAL64))
            x = v
         type is (integer)
            x = v
         end select
         call rows%push_back(100*x/s)
      end do

   end function get_rows

   ! Not used - PercentageColumn combines results across rows
   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class (PercentageColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

   end function get_row
   
end module MAPL_PercentageColumn
