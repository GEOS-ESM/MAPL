module MAPL_NameColumn
   use MAPL_AbstractMeterNode
   use MAPL_SimpleTextColumn
   implicit none
   private

   public :: NameColumn

   type, extends(SimpleTextColumn) :: NameColumn
      private
      character(:), allocatable :: indent
   contains
      procedure :: get_header
      procedure :: get_num_rows_header
      procedure :: get_row
   end type NameColumn

   interface NameColumn
      module procedure new_NameColumn
   end interface NameColumn


contains


   function new_NameColumn(width, indent, separator) result(column)
      type (NameColumn) :: column
      integer, intent(in) :: width
      character(*), optional, intent(in) :: indent
      character(1), optional, intent(in) :: separator

      call column%set_width(width)
      if (present(indent)) then
         column%indent = indent
      else
         column%indent = '--'
      end if

      if (present(separator)) call column%set_separator(separator)

   end function new_NameColumn


   subroutine get_header(this, header)
      class (NameColumn), intent(in) :: this
      character(:), allocatable, intent(out) :: header(:)

      integer :: w, h
      character(:), allocatable :: separator

      w = this%get_width()
      h = this%get_num_rows_header()

      allocate(character(len=w) :: header(h))
      header(1) = 'Name'
      if ( h <=1 ) return ! when separator is not in the constructor
      call this%get_separator(header(2), h-1)
      
   end subroutine get_header


   function get_row(this, node) result(row)
      character(:), allocatable :: row
      class (NameColumn), intent(in) :: this
      class (AbstractMeterNode), intent(in) :: node

      integer :: n

      n = this%get_width()
      allocate(character(len=n) :: row)
      row(:) = repeat(this%indent, ncopies=node%get_depth()) // node%get_name()

   end function get_row


   integer function get_num_rows_header(this) result(num_rows)
      class(NameColumn), intent(in) :: this
      num_rows = 1 + this%get_num_rows_separator()
   end function get_num_rows_header


end module MAPL_NameColumn
