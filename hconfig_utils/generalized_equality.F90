module mapl3g_generalized_equality

   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   implicit none (type, external)
   private

   public :: are_equal

   interface are_equal
      procedure :: equals_i4_scalar
      procedure :: equals_i8_scalar
      procedure :: equals_r4_scalar
      procedure :: equals_r8_scalar
      procedure :: equals_l_scalar
      procedure :: equals_string
      procedure :: equals_i4_array
      procedure :: equals_i8_array
      procedure :: equals_r4_array
      procedure :: equals_r8_array
      procedure :: equals_l_array
   end interface

contains

   logical function equals_i4_scalar(u, v) result(lval)
      integer(kind=ESMF_KIND_I4), intent(in) :: u, v

      lval = (u == v)

   end function equals_i4_scalar

   logical function equals_i8_scalar(u, v) result(lval)
      integer(kind=ESMF_KIND_I8), intent(in) :: u, v

      lval = (u == v)

   end function equals_i8_scalar

   logical function equals_r4_scalar(u, v) result(lval)
      real(kind=ESMF_KIND_R4), intent(in) :: u, v

      lval = (u == v)

   end function equals_r4_scalar

   logical function equals_r8_scalar(u, v) result(lval)
      real(kind=ESMF_KIND_R8), intent(in) :: u, v

      lval = (u == v)

   end function equals_r8_scalar

   logical function equals_l_scalar(u, v) result(lval)
      logical, intent(in) :: u, v

      lval = (u .eqv. v)

   end function equals_l_scalar

   logical function equals_string(u, v) result(lval)
      character(len=:), allocatable, intent(in) :: u
      character(len=*), intent(in) :: v

      lval = (u == v)

   end function equals_string

   logical function equals_i4_array(u, v) result(lval)
      integer(kind=ESMF_KIND_I4), allocatable, intent(in) :: u(:)
      integer(kind=ESMF_KIND_I4), intent(in) :: v(:)

      lval = all(u == v)

   end function equals_i4_array

   logical function equals_i8_array(u, v) result(lval)
      integer(kind=ESMF_KIND_I8), allocatable, intent(in) :: u(:)
      integer(kind=ESMF_KIND_I8), intent(in) :: v(:)

      lval = all(u == v)

   end function equals_i8_array

   logical function equals_r4_array(u, v) result(lval)
      real(kind=ESMF_KIND_R4), allocatable, intent(in) :: u(:)
      real(kind=ESMF_KIND_R4), intent(in) :: v(:)

      lval = all(u == v)

   end function equals_r4_array

   logical function equals_r8_array(u, v) result(lval)
      real(kind=ESMF_KIND_R8), allocatable, intent(in) :: u(:)
      real(kind=ESMF_KIND_R8), intent(in) :: v(:)

      lval = all(u == v)

   end function equals_r8_array

   logical function equals_l_array(u, v) result(lval)
      logical, allocatable, intent(in) :: u(:)
      logical, intent(in) :: v(:)

      lval = all(u .eqv. v)

   end function equals_l_array

end module mapl3g_generalized_equality
