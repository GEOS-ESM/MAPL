module generalized_equality

   implicit none

   interface operator(==)
      module procedure :: equals_l_scalar
      module procedure :: equals_l_array
      module procedure :: equals_i4_array
      module procedure :: equals_i8_array
      module procedure :: equals_r4_array
      module procedure :: equals_r8_array
   end interface

contains

   logical function equals_l_scalar(u, v) result(lval)
      logical, intent(in) :: u, v

      lval = u .eqv. v

   end function equals_l_scalar

   logical function equals_l_array(u, v) result(lval)
      logical, intent(in) :: u(:), v(:)

      lval = all(u .eqv. v)

   end function equals_l_array

   logical function equals_i4array(u, v) result(lval)
      integer(kind=ESMF_KIND_I4), intent(in) :: u(:), v(:)

      lval = all(u == v)

   end function equals_i4array

   logical function equals_i8array(u, v) result(lval)
      integer(kind=ESMF_KIND_I8), intent(in) :: u(:), v(:)

      lval = all(u == v)

   end function equals_i8array

   logical function equals_r4array(u, v) result(lval)
      real(kind=ESMF_KIND_R4), intent(in) :: u(:), v(:)

      lval = all(u == v)

   end function equals_r4array

   logical function equals_r8array(u, v) result(lval)
      real(kind=ESMF_KIND_R8), intent(in) :: u(:), v(:)

      lval = all(u == v)

   end function equals_r8array


end module generalized_equality
