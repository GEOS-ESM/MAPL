module MAPL_FullNameColumn
   use MAPL_AbstractMeterNode
   use MAPL_SimpleTextColumn
   implicit none
   private

   public :: FullNameColumn

   type, extends(SimpleTextColumn) :: FullNameColumn
      private
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_row
      procedure :: get_rows_range
   end type FullNameColumn

   interface FullNameColumn
      module procedure new_FullNameColumn
   end interface FullNameColumn


contains


   function new_FullNameColumn(width, separator) result(column)
      type (FullNameColumn) :: column
      integer, intent(in) :: width
      character(1), optional, intent(in) :: separator

      call column%set_width(width)

      if (present(separator)) call column%set_separator(separator)

   end function new_FullNameColumn


   subroutine get_header(this, header)
      class (FullNameColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: w, h

      w = this%get_width()
      h = this%get_num_rows_header()

      allocate(character(len=w) :: header(h))
      header(1) = 'Full Name'
      if ( h <=1 ) return ! when separator is not in the constructor
      call this%get_separator(header(2), h-1)
      
   end subroutine get_header


   function get_row(this, node) result(row)
      character(:), allocatable :: row
      class (FullNameColumn), intent(in) :: this
      class (AbstractMeterNode), intent(in) :: node

      integer :: n
      integer :: i

      row = node%get_name()

   end function get_row


   integer function get_num_rows_header(this) result(num_row)
      class(FullNameColumn), intent(in) :: this
      num_row = 1 + this%get_num_rows_separator()
   end function get_num_rows_header

   ! Using subroutines instead of functions as a workaround for gfortran 8.2
   ! Reproducer being submitted by Damian Rouson (10/12/2018)
   subroutine get_rows_range(this, begin, end, rows)
      use gftl2_StringVector, only: StringVector
      class (FullNameColumn), target, intent(in) :: this
      class (AbstractMeterNodeIterator), intent(in) :: begin
      class (AbstractMeterNodeIterator), intent(in) :: end
      character(:), allocatable, intent(inout) :: rows(:)

      class (AbstractMeterNodeIterator), allocatable :: iter
      integer :: i, j, depth
      integer :: width
      integer :: previous
      class (AbstractMeterNode), pointer :: subnode
      type(StringVector) :: names

      ! count_nodes
      iter = begin
      i = 0
      do while (iter /= end)
         i = i + 1
         call iter%next()
      end do

      width = this%get_width()
      allocate(character(width) :: rows(i))

      ! Fill rows
      iter = begin
      i = 0
      do while (iter /= end)
         i = i + 1
         subnode => iter%get()
         depth = subnode%get_depth()
         call names%resize(depth+1)

         call names%set(depth+1, subnode%get_name())
         print*,'debug:  ', i, depth, subnode%get_name(), ' :: ', names%of(depth+1)

         ! Concatenate names
         rows(i) = names%of(1)
         do j = 2, names%size()
            rows(i) = trim(rows(i)) // '.' // names%of(j)
         end do
         print*,'debug final:  ', i, depth, trim(rows(i))

         call iter%next()
      end do
      
   end subroutine get_rows_range



end module MAPL_FullNameColumn
