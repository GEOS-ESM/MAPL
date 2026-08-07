module mapl_AccessSpec
   implicit none(type,external)
   private

   public :: AccessSpec
   public :: operator(==), operator(/=)
   public :: ACCESS_IN
   public :: ACCESS_OUT
   public :: ACCESS_INOUT

   type AccessSpec
      private
      integer :: value = 0
   end type AccessSpec

   type(AccessSpec), parameter :: ACCESS_IN = AccessSpec(1)
   type(AccessSpec), parameter :: ACCESS_OUT = AccessSpec(2)
   type(AccessSpec), parameter :: ACCESS_INOUT = AccessSpec(3)

   interface operator(==)
      procedure :: equal
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal
   end interface operator(/=)

contains

   pure logical function equal(lhs, rhs)
      type(AccessSpec), intent(in) :: lhs, rhs

      equal = lhs%value == rhs%value
   end function equal

   pure logical function not_equal(lhs, rhs)
      type(AccessSpec), intent(in) :: lhs, rhs

      not_equal = .not. (lhs == rhs)
   end function not_equal

end module mapl_AccessSpec
   
