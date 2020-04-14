module MAPL_MultiColumn
   use MAPL_TextColumn
   use MAPL_TextColumnVector
   use MAPL_AbstractMeterNode
   use MAPL_SeparatorColumn
   implicit none
   private

   public :: MultiColumn

   type, extends(TextColumn) :: MultiColumn
      private
      type (TextColumnVector) :: columns
      integer :: num_rows_header = 0
      character(:), allocatable :: shared_header(:)
   contains
      procedure :: add_column
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_rows
   end type MultiColumn

   interface MultiColumn
      module procedure :: new_MultiColumn
   end interface MultiColumn


contains


   function new_MultiColumn(header, separator) result(column)
      character(*), intent(in) :: header(:)
      type(MultiColumn) :: column
      character(1), optional, intent(in) :: separator

      integer :: i, w, n

      w = len(header)
      n = size(header)
      allocate(character(w) :: column%shared_header(n))
      do i = 1, n
         column%shared_header(i) = header(i)
      end do
      if (present(separator)) call column%set_separator(separator)
      column%num_rows_header = column%get_num_rows_separator()
      call column%set_width(0)

   end function new_MultiColumn


   subroutine add_column(this, column)
      class (MultiColumn), intent(inout) :: this
      class (TextColumn), intent(in) :: column

      integer :: h, h0, w

      w = this%get_width()
      
      if (this%columns%size() > 0) then
         call this%columns%push_back(SeparatorColumn(' '))
         w = w + 1
      end if
      call this%columns%push_back(column)

      h0 = size(this%shared_header) + this%get_num_rows_separator()
      h = column%get_num_rows_header()
      this%num_rows_header = max(this%num_rows_header, h0 + h)
      w = w + column%get_width()
      call this%set_width(w)

   end subroutine add_column


   recursive subroutine get_rows(this, node, rows)
      class (MultiColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      character(:), allocatable, intent(out) :: rows(:)

      integer :: i, j
      integer :: w0, w1
      class(TextColumn), pointer :: c

      integer :: total_width, height
      character(:), allocatable :: column(:)
      
      total_width = this%get_width()
      height = node%get_num_nodes()
      
      allocate(character(total_width) :: rows(height))

      w0 = 1
      do i = 1, this%columns%size()
         
         c => this%columns%at(i)
         w1 = w0 + c%get_width() - 1
         call c%get_rows(node, column)

         do j = 1, height
            rows(j)(w0:w1) = column(j)
         end do

         w0 = w1 + 1
      end do

   end subroutine get_rows


   recursive subroutine get_header(this, header)
      class (MultiColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: i, column_width, column_height
      integer :: w, w0, w1, h, h0, h1, h2
      integer :: total_width, total_height, shared_height
      class(TextColumn), pointer :: c
      character(:), allocatable :: column_header(:)
      character(1) :: char
      integer :: n_shared

      total_width = this%get_width()
      total_height = this%num_rows_header
      n_shared = size(this%shared_header)
      shared_height = n_shared + this%get_num_rows_separator()

      allocate(character(total_width) :: header(total_height))

      header(1:n_shared) = this%shared_header
      call this%center(header(1:n_shared))
      call this%get_separator(header(n_shared+1), shared_height - n_shared)

      c => this%columns%at(1)
      column_height = c%get_num_rows_header()
      column_width = c%get_width()
      header(shared_height+1:total_height-column_height) = repeat(' ', column_width)
      call c%get_header(column_header)
      do h = 1, column_height
         h0 = total_height - column_height + h
         header(h0) = column_header(h)
      end do
      deallocate(column_header)

      w0 = 1
      do i = 1, this%columns%size()
         c => this%columns%at(i)
         column_height = c%get_num_rows_header()
         w = c%get_width()
         w1 = w0 + w - 1
         h0 = shared_height + 1
         h1 = total_height-column_height+1
         h2 = total_height

         header(h0:h1-1)(w0:w1) = repeat(' ',w)
         call c%get_header(column_header)
         header(h1:h2)(w0:w1) = column_header
         w0 = w1 + 1 ! space
         deallocate(column_header)
      end do

   end subroutine get_header
   
   integer function get_num_rows_header(this) result(num_rows)
      class(MultiColumn), intent(in) :: this
      num_rows = this%num_rows_header
   end function get_num_rows_header

end module MAPL_MultiColumn
