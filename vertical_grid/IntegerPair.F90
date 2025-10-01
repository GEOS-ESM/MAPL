module mapl3g_IntegerPair
   implicit none(type,external)
   private
   
   public :: IntegerPair
   public :: operator(<)
   
   type :: IntegerPair
      integer :: first
      integer :: second
   end type IntegerPair
   
   interface operator(<)
      module procedure integer_pair_less_than
   end interface operator(<)

contains

   pure function integer_pair_less_than(lhs, rhs) result(is_less)
      type(IntegerPair), intent(in) :: lhs
      type(IntegerPair), intent(in) :: rhs
      logical :: is_less
      
      ! Lexicographic ordering: compare first, then second if first is equal
      if (lhs%first < rhs%first) then
         is_less = .true.
      else if (lhs%first > rhs%first) then
         is_less = .false.
      else
         ! lhs%first == rhs%first, so compare second
         is_less = (lhs%second < rhs%second)
      end if
   end function integer_pair_less_than

end module mapl3g_IntegerPair
