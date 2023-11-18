module MockField_mod

   implicit none

   public :: MockField, MAXLEN

   private

   integer, parameter :: MAXLEN = 80
   integer, parameter :: SUCCESS = 0
   integer, parameter :: ERROR = SUCCESS - 1

   ! Mock for ESMF_Field
   type :: MockField
      private
      real(R64), allocatable :: f_(:, :)
      character(len=MAXLEN) :: unit_name_
      character(len=MAXLEN) :: unit_symbol_
   contains
      procedure, public, pass(this) :: dimensions
      procedure, public, pass(this) :: unit_name
      procedure, public, pass(this) :: unit_symbol
      procedure, public, pass(this) :: get
      procedure, public, pass(this) :: set
      procedure, public, pass(this) :: get_array
      procedure, public, pass(this) :: set_array
      procedure, public, pass(this) :: is_null
      procedure, private, pass(this) :: valid_indices
   end type MockField

   interface MockField
      module procedure :: construct_mock_field
   end interface MockField

!   interface copy
!      module procedure :: copy_matrix
!      module procedure :: copy_vector
!   end interface copy

contains

   function construct_mock_field(f_, unit_name, unit_symbol) result(mf)
      real(R64), intent(in) :: f_(:,:)
      character(len=*), intent(in) :: unit_name
      character(len=*), optional, intent(in) :: unit_symbol
      type(MockField) :: mf

      mf % f_ = f_
      mf % unit_name_ = unit_name
      mf % unit_symbol_ = unit_name
      if(present(unit_symbol_)) mf % unit_symbol_ = unit_symbol

   end function construct_mock_field

   logical is_null(this)
      class(MockField), intent(in) :: this
      integer :: dimensions(2)

      dimensions = mf % dimensions()
      is_null = dimensions(1) == 0 .or. dimensions(2) == 0

   end function is_null

   function dimensions(this)
      class(MockField), intent(in) :: this
      integer :: dimensions(2)

      dimensions = size(this % f_)

   end function dimensions

   function unit_name(this)
      class(MockField), intent(in) :: this
      character(len=MAXLEN) :: unit_name

      unit_name = mf % unit_name_

   end function unit_name

   function unit_symbol(this)
      class(MockField), intent(in) :: this
      character(len=MAXLEN) :: unit_symbol

      unit_symbol = mf % unit_symbol_

   end function unit_symbol

   function get(this, i, j, rc)
      class(MockField), intent(in) :: this
      integer, intent(in) :: i, j
      integer, optional, intent(out) :: rc
      real(R64) :: get
      integer :: status

      if(this % valid_indices(i, j) then
         get = this % f_(i, j)
         status = SUCCESS
      else
         status = ERROR
      end if

      if(present(rc)) rc = status

   end function get
   
   function get_array(this)
      class(MockField), intent(in) :: this
      real(R64), allocatable :: get_array(:, :)

!      get_array = copy(this % f_)
      allocate(get_array, source=this % f_)

   end function get_array

   function set_array(this, array) result(mf)
      class(MockField), intent(in) :: this
      real(R64), intent(in) :: array(:, :)
      type(MockField) :: mf
      real(R64), allocatable :: f_(:, :)
      character(len=MAXLEN) :: unit_name, unit_symbol

      if(this % dimensions() == size(array)) then
         allocate(f_, source=array)
!         f_ = copy(array)
         unit_name = this % unit_name()
         unit_symbol = this % unit_symbol()
      else
         allocate(f_(0, 0))
      end if
      
      mf = MockField(f_, unit_name, unit_symbol)

   end function set_array

!   function copy_matrix(array) result(matrix)
!      real(R64), intent(in) :: array(:,:)
!      real(R64) :: matrix(size(array, 1), size(array,2))
!      integer :: j
!
!      do j = 1, size(matrix, 2)
!         matrix(:, j) = copy(matrix(:, j))
!      end do
!
!   end function copy_matrix

!   function copy_vector(array) result(vector)
!      real(R64), intent(in) :: array(:)
!      real(R64) :: vector(size(array))
!      integer :: i
!
!      do i = 1, size(vector)
!         vector(i) = array(i)
!      end do
!
!   end function copy_vector

   logical function valid_indices(this, i, j)
      class(MockField), intent(in) :: this
      integer, intent(in) :: i, j
      integer :: dimensions(2)

      valid_indices = .not. this % is_null()
      if(valid_indices) then
         dimensions = this % dimensions()
         valid_indices = (i > 0 .and. j > 0 .and. i <= dimensions(1) .and. j <= dimensions(2))
      end if

   end function valid_indices
      
end module MockField_mod
