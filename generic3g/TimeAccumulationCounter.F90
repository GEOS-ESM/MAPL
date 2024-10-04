module mapl3g_AccumulationCounter

   implicit none
   private
   public :: AccumulationCounter
   public :: GetAccumulationCount
   public :: IncrementAccumulationCounter
   public :: ResetAccumulationCounter
   public :: IsScalarCounter

   type :: AccumulationCounter(k, r)
      integer, kind :: k = 0
      integer, len :: r = 0
      type(ESMF_Field), allocatable :: counts
      integer :: count =-1
   end type AccumulationCounter(k, r)

   interface AccumulationCounter
      module procedure :: construct_counter_r4
      module procedure :: construct_counter_r8
   end interface AccumulationCounter

   interface GetAccumulationCount
      module procedure :: get_scalar_count
      module procedure :: get_r4_field
      module procedure :: get_r8_field
   end interface GetAccumulationCount

   interface IncrementAccumulationCounter
      module procedure :: increment_accumulation_counter
   end interface IncrementAccumulationCounter

   interface ResetAccumulationCounter
      module procedure :: reset_accumulation_counter
   end interface ResetAccumulationCounter

   interface IsScalarCounter
      module procedure :: is_scalar_counter
   end interface IsScalarCounter

contains

   function construct_counter_r4(initial_value) result(c)
      type(AccumulationCounter) :: c
      real(ESMF_KIND_R4) intent(in) :: initial_value
      integer :: count

      c = AccumulationCounter(ESMF_KIND_R4, 0)
      count = integer(initial_value)
      c%count = count

   end function construct_counter_r4

   function construct_counter_r8(initial_value) result(c)
      type(AccumulationCounter) :: c
      integer, intent(in) :: size
      real(ESMF_KIND_R8), intent(in) :: initial_value
      integer :: count

      c = AccumulationCounter(ESMF_KIND_R8, 0)
      count = integer(initial_value)
      c%count = count

   end function construct_counter_r8

   subroutine initialize_field(
   logical function is_scalar_counter(this) result(lval)
      class(AccumulationCounter), intent(in) :: this

      lval = .not. associated(this%array_count)

   end function is_scalar_counter

   integer function size_counter(this) result(sz)
      class(AccumulationCounter), intent(in) :: this

      sz = this%sz
      if(this%is_scalar()) return
      sz = size(this%array_count)

   end function size_counter

   subroutine increment_all(this, increment_size)
      class(AccumulationCounter), intent(inout) :: this
      integer, intent(in) :: increment_size

      if(this%is_scalar()) then
         this%count = this%count + increment_size
         return
      end if
      this%array_count = this%array_count + increment_size

   end subroutine increment_all

   subroutine increment_where(this, mask, increment_size)
      class(AccumulationCounter), intent(inout) :: this
      logical, intent(in) :: mask(:)
      integer, intent(in) :: increment_size

      if(this%is_scalar()) then
         allocate(this%array_count(size(mask)), SOURCE=this%count)
      end if
      where(mask) this%array_count = this%array_count + increment_size

   end subroutine increment_where

   subroutine increment_counter(this, mask, increment_size)
      class(AccumulationCounter), intent(inout) :: this
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
      class(AccumulationCounter), intent(in) :: this

      allocate(count(this%size()))
      if(this%is_scalar()) then
         count = this%count
         return
      end if
      count = this%array_count

   end function count_counter

   subroutine reset_counter(this, reset_value)
      class(AccumulationCounter), intent(inout) :: this
      integer, optional, intent(in) :: reset_value

      this%array_count => null()
      this%count = 0
      if(present(reset_value)) this%count = reset_value

   end subroutine reset_counter

end module mapl3g_TimeAccumulationCounter
