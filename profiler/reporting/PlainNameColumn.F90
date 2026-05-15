module MAPL_PlainNameColumn
   use MAPL_AbstractMeterNode
   use MAPL_SimpleTextColumn
   implicit none
   private

   public :: PlainNameColumn

   type, extends(SimpleTextColumn) :: PlainNameColumn
      private
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_row
   end type PlainNameColumn

   interface PlainNameColumn
      module procedure new_PlainNameColumn
   end interface PlainNameColumn

contains

   function new_PlainNameColumn(width) result(column)
      type(PlainNameColumn) :: column
      integer, intent(in) :: width

      call column%set_width(width)

   end function new_PlainNameColumn

   subroutine get_header(this, header)
      class(PlainNameColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: w, h

      w = this%get_width()
      h = this%get_num_rows_header()

      allocate(character(len=w) :: header(h))
      header(1) = 'Name'
      if (h <= 1) return
      call this%get_separator(header(2), h-1)
      
   end subroutine get_header

   function get_row(this, node) result(row)
      character(:), allocatable :: row
      class(PlainNameColumn), intent(in) :: this
      class(AbstractMeterNode), intent(in) :: node

      integer :: n

      n = this%get_width()
      allocate(character(len=n) :: row)
      row(:) = node%get_name()

   end function get_row

   integer function get_num_rows_header(this) result(num_rows)
      class(PlainNameColumn), intent(in) :: this
      num_rows = 1 + this%get_num_rows_separator()
   end function get_num_rows_header

end module MAPL_PlainNameColumn
