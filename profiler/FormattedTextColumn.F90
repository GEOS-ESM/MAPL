module MAPL_FormattedTextColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractColumn
   use MAPL_AbstractMeterNode
   use MAPL_TextColumn
   use GFTL_UnlimitedVector
   implicit none
   private

   public :: FormattedTextColumn

   type, extends(TextColumn) :: FormattedTextColumn
      private
      
      character(:), allocatable :: header(:)
      character(:), allocatable :: format
      class (AbstractColumn), allocatable :: data_column
   contains
      procedure :: get_header
      procedure :: get_rows
      procedure :: get_num_rows_header
   end type FormattedTextColumn


   interface FormattedTextColumn
      module procedure new_FormattedTextColumn_scalar_header
      module procedure new_FormattedTextColumn_array_header
   end interface FormattedTextColumn


contains


   function new_FormattedTextColumn_scalar_header(header, format, width, data_column, separator) result(column)
      type (FormattedTextColumn) :: column
      character(*), intent(in) :: header
      character(*), intent(in) :: format
      integer, intent(in) :: width
      class (AbstractColumn), intent(in) :: data_column
      character(1), optional :: separator

      column = FormattedTextColumn([header], format, width, data_column, separator=separator)

   end function new_FormattedTextColumn_scalar_header

 
   function new_FormattedTextColumn_array_header(header, format, width, data_column, separator) result(column)
      type (FormattedTextColumn) :: column
      character(*), intent(in) :: header(:)
      character(*), intent(in) :: format
      integer, intent(in) :: width
      class (AbstractColumn), intent(in) :: data_column
      character(1), optional :: separator

      column%header = header
      column%format = format
      call column%set_width(width)
      column%data_column = data_column

      if (present(separator)) then
         call column%set_separator(separator)
      end if

   end function new_FormattedTextColumn_array_header

 

   subroutine get_header(this, header)
      class (FormattedTextColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: w, n, n0
      integer :: i

      w = this%get_width()
      n0 = size(this%header)
      n = this%get_num_rows_header()
      allocate(character(w) :: header(n))

      do i = 1, n0
         header(i)(:) = this%header(i)
      end do

      if (n>n0) call this%get_separator(header(n0+1), n-n0)
      call this%center(header)

   end subroutine get_header

   integer function get_num_rows_header(this) result(num_rows)
      class(FormattedTextColumn), intent(in) :: this

      num_rows = size(this%header) + this%get_num_rows_separator()

   end function get_num_rows_header


   subroutine get_rows(this, node, rows)
      use MAPL_AbstractMeterNode
      class (FormattedTextColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      character(:), allocatable, intent(out) :: rows(:)

      type (UnlimitedVector) :: values

      integer :: i, n

      values = this%data_column%get_rows(node)

      n = this%get_width()
      allocate(character(n) :: rows(values%size()))

      do i = 1, values%size()
         select type (v => values%at(i))
         type is (integer)
            write(rows(i),this%format) v
         type is (real(kind=REAL64))
            write(rows(i),this%format) v
         end select
      end do
      
   end subroutine get_rows
   

end module MAPL_FormattedTextColumn
