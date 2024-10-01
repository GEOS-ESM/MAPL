module mapl3g_TimeAccumulationCounter

   implicit none
   private
   public :: Counter

   type :: Counter
      private
      integer :: scalar_count = -1
      integer, pointer :: array_count(:) => null()
      integer :: sz = -1
   contains
      procedure :: count => count_counter
      procedure :: increment => increment_counter
      procedure :: reset => reset_counter
      procedure, private :: is_scalar => is_scalar_counter
      procedure, private :: size => size_counter
      procedure, private :: increment_all
      procedure, private :: increment_where
   end type Counter

   interface Counter
      module procedure :: construct_counter_scalar
      module procedure :: construct_counter_array
   end interface Counter

contains

   function construct_counter_scalar(size, initial_value) result(c)
      type(Counter) :: c
      integer, intent(in) :: size
      integer, optional, intent(in) :: initial_value
      integer :: initial_value_

      initial_value_ = 0
      if(present(initial_value)) initial_value_ = initial_value
      c%sz = size
      c%scalar_count = initial_value_

   end function construct_counter_scalar

   function construct_counter_array(size, counter_values, initial_value) result(c)
      type(Counter) :: c
      integer, intent(in) :: counter_values(:)
      integer, optional, intent(in) :: initial_value

      allocate(c%array_count, SOURCE=counter_values)

   end function construct_counter_array

   logical function is_scalar_counter(this) result(lval)
      class(Counter), intent(in) :: this

      lval = .not. associated(this%array_count)

   end function is_scalar_counter

   integer function size_counter(this) result(sz)
      class(Counter), intent(in) :: this

      sz = this%sz
      if(this%is_scalar()) return
      sz = size(this%array_count)

   end function size_counter

   subroutine increment_all(this, increment_size)
      class(Counter), intent(inout) :: this
      integer, intent(in) :: increment_size

      if(this%is_scalar()) then
         this%scalar_count = this%scalar_count + increment_size
         return
      end if
      this%array_count = this%array_count + increment_size

   end subroutine increment_all

   subroutine increment_where(this, mask, increment_size)
      class(Counter), intent(inout) :: this
      logical, intent(in) :: mask(:)
      integer, intent(in) :: increment_size

      if(this%is_scalar()) then
         allocate(this%array_count(size(mask)), SOURCE=this%scalar_count)
      end if
      where(mask) this%array_count = this%array_count + increment_size

   end subroutine increment_where

   subroutine increment_counter(this, mask, increment_size)
      class(Counter), intent(inout) :: this
      logical, optional, intent(in) :: mask(:)
      integer, optional, intent(in) :: increment_size
      integer :: incr_size

      incr_size = 1
      if(present(increment_size)) incr_size = increment_size
   
      if(present(mask)) then
         call this%increment_where(mask, incr_size)
         return
      end if
      
      call this%increment_all(incr_size)
      
   end subroutine increment_counter

   function count_counter(this) result(count)
      integer, allocatable :: count(:)
      class(Counter), intent(in) :: this

      allocate(count(this%size()))
      if(this%is_scalar()) then
         count = this%scalar_count
         return
      end if
      count = this%array_count

   end function count_counter

   subroutine reset_counter(this, reset_value)
      class(Counter), intent(inout) :: this
      integer, optional, intent(in) :: reset_value

      this%array_count => null()
      this%scalar_count = 0
      if(present(reset_value)) this%scalar_count = reset_value

   end subroutine reset_counter

end module mapl3g_TimeAccumulationCounter
