module MAPL_DepthColumn
   use MAPL_AbstractMeterNode
   use MAPL_SimpleTextColumn
   implicit none
   private

   public :: DepthColumn

   type, extends(SimpleTextColumn) :: DepthColumn
      private
      character(:), allocatable :: format_string
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_row
   end type DepthColumn

   interface DepthColumn
      module procedure new_DepthColumn
   end interface DepthColumn

contains

   function new_DepthColumn(format_string) result(column)
      type(DepthColumn) :: column
      character(*), optional, intent(in) :: format_string

      if (present(format_string)) then
         column%format_string = format_string
      else
         column%format_string = '(i3)'
      end if
      
      call column%set_width(3)

   end function new_DepthColumn

   subroutine get_header(this, header)
      class(DepthColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: w, h

      w = this%get_width()
      h = this%get_num_rows_header()

      allocate(character(len=w) :: header(h))
      header(1) = 'Depth'
      if (h <= 1) return
      call this%get_separator(header(2), h-1)
      
   end subroutine get_header

   function get_row(this, node) result(row)
      character(:), allocatable :: row
      class(DepthColumn), intent(in) :: this
      class(AbstractMeterNode), intent(in) :: node

      integer :: depth, n

      depth = node%get_depth()
      n = this%get_width()
      allocate(character(len=n) :: row)
      write(row, this%format_string) depth

   end function get_row

   integer function get_num_rows_header(this) result(num_rows)
      class(DepthColumn), intent(in) :: this
      num_rows = 1 + this%get_num_rows_separator()
   end function get_num_rows_header

end module MAPL_DepthColumn
