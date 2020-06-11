module MAPL_SeparatorColumn
   use MAPL_AbstractColumn
   use MAPL_AbstractMeterNode
   use Mapl_SimpleTextColumn
   implicit none
   private

   public :: SeparatorColumn

   type, extends(SimpleTextColumn) :: SeparatorColumn
      private
      character(:), allocatable :: field
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_row
   end type SeparatorColumn


   interface SeparatorColumn
      module procedure new_SeparatorColumn
   end interface SeparatorColumn


contains


   function new_SeparatorColumn(field) result(column)
      type (SeparatorColumn) :: column
      character(*), intent(in) :: field

      column%field = field
      call column%set_width(len(field))

   end function new_SeparatorColumn


   subroutine get_header(this, header)
      class (SeparatorColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      header = [this%field]

   end subroutine get_header


   function get_row(this, node) result(row)
      character(:), allocatable :: row
      class (SeparatorColumn), intent(in) :: this
      class (AbstractMeterNode), intent(in) :: node

      integer :: n

      if (.false.) print*,shape(node) ! intentionally unused dummy

      n = this%get_width()
      
      allocate(character(n) :: row)
      row = this%field
      
   end function get_row


   integer function get_num_rows_header(this) result(num_rows)
      class(SeparatorColumn), intent(in) :: this
      num_rows = 1
   end function get_num_rows_header
   
end module MAPL_SeparatorColumn

