module MAPL_TextColumn
   use MAPL_AbstractMeterNode
   implicit none
   private

   public :: TextColumn

   type, abstract :: TextColumn
      private
      integer :: column_width = 0
      character(:), allocatable :: separator
   contains
      procedure :: set_width
      procedure :: get_width
      procedure(i_get_header), deferred :: get_header
      procedure(i_get_num_rows_header), deferred :: get_num_rows_header
      procedure(i_get_rows), deferred :: get_rows
      procedure :: center

      procedure :: set_separator
      procedure :: get_separator
      procedure :: get_num_rows_separator
   end type TextColumn

   abstract interface

      subroutine i_get_header(this, header)
         import TextColumn
         class (TextColumn), intent(in) :: this
         character(:), allocatable, intent(out) :: header(:)
      end subroutine i_get_header

      subroutine i_get_rows(this, node, rows)
         use MAPL_AbstractMeterNode
         import TextColumn
         class (TextColumn), intent(in) :: this
         class (AbstractMeterNode), target, intent(in) :: node
         character(:), allocatable, intent(out) :: rows(:)
      end subroutine i_get_rows
      
      integer function i_get_num_rows_header(this) result(num_rows)
         import TextColumn
         class (TextColumn), intent(in) :: this
      end function i_get_num_rows_header

   end interface


contains


   subroutine set_width(this, column_width)
      class (TextColumn), intent(inout) :: this
      integer, intent(in) :: column_width

      this%column_width = column_width

   end subroutine set_width


   integer function get_width(this) result(column_width)
      class (TextColumn), intent(in) :: this
      column_width = this%column_width
   end function get_width


   subroutine center(this, rows, space)
      class (TextColumn), intent(in) :: this
      character(*), intent(inout) :: rows(:)
      character(1), optional, intent(in) :: space

      integer :: w, i
      integer :: n, n_0, n_1
      character(:), allocatable :: tmp
      character(1) :: space_

      if (present(space)) then
         space_ = space
      else
         space_ = ' '
      end if

      w = this%get_width()
      do i = 1, size(rows)
         tmp = trim(adjustl(rows(i)))
         n = len(tmp)
         n_0 = (w - n)/2
         n_1 = w - (n + n_0)
         rows(i)(:) = repeat(space_,n_0) // tmp // repeat(space_, n_1)
      end do
         
      
   end subroutine center


   subroutine set_separator(this, separator)
      class(TextColumn), intent(inout) :: this
      character(1), intent(in) :: separator
      this%separator = separator
   end subroutine set_separator

   ! Would be a function, but this is a workaround for gfortran 8.2
   ! issue with allocatable arrays of deferred length strings.
   subroutine get_separator(this, separator, k)
      class(TextColumn), intent(in) :: this
      integer, intent(in) :: k
      character(*), intent(inout) :: separator(k)

      integer :: w
      character(1) :: c

      w = this%get_width()
      if (allocated(this%separator)) then
         c = this%separator
         separator(1) = repeat(c, w)
      end if

   end subroutine get_separator


   integer function get_num_rows_separator(this) result(num_rows)
      class (TextColumn), intent(in) :: this
      
      if (allocated(this%separator)) then
         num_rows = 1
      else
         num_rows = 0
      end if

   end function get_num_rows_separator

end module MAPL_TextColumn
