module MAPL_MemoryTextColumn
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use MAPL_AbstractColumn
   use MAPL_AbstractMeterNode
   use MAPL_TextColumn
   use GFTL_UnlimitedVector
   implicit none
   private

   public :: MemoryTextColumn

   type String
      character(:), allocatable :: string
   end type String
   type, extends(TextColumn) :: MemoryTextColumn
      private
!!$      character(:), allocatable :: header(:)
      type (String), allocatable :: header(:)
      character(:), allocatable :: format
      class (AbstractColumn), allocatable :: data_column
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_rows
   end type MemoryTextColumn


   interface MemoryTextColumn
      module procedure new_MemoryTextColumn
   end interface MemoryTextColumn


contains


   function new_MemoryTextColumn(header, format, width, data_column, separator) result(column)
      type (MemoryTextColumn) :: column
      character(*), intent(in) :: header(:)
      character(*), intent(in) :: format
      integer, intent(in) :: width
      class (AbstractColumn), intent(in) :: data_column
      character(1), optional, intent(in) :: separator

      integer :: i, n
      character(:), allocatable :: word

      n = size(header)
      allocate(column%header(n))
      do i = 1, n
         column%header(i)%string = header(i)
      end do
      
      column%format = format
      call column%set_width(width)

      column%data_column = data_column

      if (present(separator)) then
         call column%set_separator(separator)
      end if


   end function new_MemoryTextColumn

 

   subroutine get_header(this, header)
      class (MemoryTextColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)
      integer :: w, n
      integer :: i

      w = this%get_width()
      n = this%get_num_rows_header()
      allocate(character(w) :: header(n))
      do i = 1, size(this%header)
         header(i)(:) = this%header(i)%string
      end do
      call this%get_separator(header(size(this%header)+1), n - size(this%header))
      call this%center(header)

   end subroutine get_header


   integer function get_num_rows_header(this) result(num_rows)
      class(MemoryTextColumn), intent(in) :: this
      num_rows = size(this%header) + this%get_num_rows_separator()
   end function get_num_rows_header


   subroutine get_rows(this, node, rows)
      use MAPL_AbstractMeterNode
      class (MemoryTextColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      character(:), allocatable, intent(out) :: rows(:)

      integer :: n, i
      character(2) :: suffix
      real(kind=REAL64) :: x
      type (UnlimitedVector) :: values

      n = this%get_width()

      values = this%data_column%get_rows(node)
      allocate(character(n) :: rows(values%size()))

      do i = 1, values%size()
         select type (v => values%at(i))
         type is (integer)
            x = real(v, kind=REAL64)
            suffix = get_suffix(x)
            write(rows(i),this%format) convert(x), suffix
         type is (real(kind=REAL64))
            suffix = get_suffix(v)
            write(rows(i),this%format) convert(v), suffix
         end select
      end do

   contains


      function get_suffix(x) result(suffix)
         character(2) :: suffix
         real(kind=REAL64), intent(in) :: x

         integer(kind=INT64) :: ix
         integer(kind=INT64) :: KB = 1024

         ix = ceiling(abs(x))
         if (ix < KB) then
            suffix = ' B'
         elseif (ix < KB**2) then
            suffix = 'KB'
         elseif (ix < KB**3) then
            suffix = 'MB'
         elseif (ix < KB**4) then
            suffix = 'GB'
         else
            suffix = 'TB'
         end if

      end function get_suffix

      function convert(x) result(ix)
         integer(kind=INT64) :: ix
         real(kind=REAL64), intent(in) :: x


         integer(kind=INT64) :: KB = 1024

         ix = ceiling(abs(x))

         if (ix < KB) then
            ix = ix
         elseif (ix < KB**2) then
            ix = ix / KB
         elseif (ix < KB**3) then
            ix = ix / KB**2
         elseif (ix < KB**4) then
            ix = ix / KB**3
         else
            ix = ix / KB**4
         end if

         ix = sign(1.d0, x) * ix
         
      end function convert
         
   end subroutine get_rows
   

end module MAPL_MemoryTextColumn
