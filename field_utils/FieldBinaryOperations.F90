#include "MAPL_Generic.h"

module MAPL_FieldBinaryOperations
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   implicit none
   private

   public fieldAdd
   public fieldSubtract
   public fieldDivide
   public fieldMultiply
   public fieldPower
   public fieldIntegerDivide

   interface set_quotient
      module procedure :: set_quotient_R4I4
      module procedure :: set_quotient_R8I4
      module procedure :: set_quotient_R4I8
      module procedure :: set_quotient_R8I8
   end interface set_quotient

contains

#define _OP +
#define _FUNC Add
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP -
#define _FUNC Subtract
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP *
#define _FUNC Multiply
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP /
#define _FUNC Divide
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP **
#define _FUNC Power
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

   subroutine fieldIntegerDivide(quotient, dividend, divisor, rc)
      type(ESMF_Field), intent(inout) :: quotient
      type(ESMF_Field), intent(inout) :: dividend
      type(ESMF_Field), intent(inout) :: divisor
      integer, optional, intent(out) :: rc
      integer :: status
      
      type(ESMF_TypeKind_Flag) :: dividend_tk,divisor_tk,quotient_tk
      logical :: has_undef,conformable
      type(ESMF_Field) :: fields(3)
      real(kind=ESMF_KIND_R4), pointer :: dividend_ptr_r4(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: quotient_ptr_r4(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: dividend_ptr_r8(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: quotient_ptr_r8(:) => null()
      integer(kind=ESMF_KIND_I4), pointer :: divisor_ptr_i4(:) => null()
      integer(kind=ESMF_KIND_I8), pointer :: divisor_ptr_i8(:) => null()

      fields(1)=quotient
      fields(2)=dividend
      fields(3)=divisor
      conformable = FieldsAreConformable(dividend,quotient,_RC)
      _ASSERT(conformable,"Dividend and quotient are not conformable.")
      conformable = FieldsAreConformable(divisor,quotient,_RC)
      _ASSERT(conformable,"Divisor and quotient are not conformable.")

      has_undef = FieldsHaveUndef(fields,_RC)
      call ESMF_FieldGet(dividend,typekind=dividend_tk,_RC)
      call ESMF_FieldGet(divisor,typekind=divisor_tk,_RC)
      call ESMF_FieldGet(quotient,typekind=quotient_tk,_RC)

      _ASSERT(dividend_tk==quotient_tk, "Dividend typekind does not match quotient typekind.")
      _ASSERT(divisor_tk == ESMF_TypeKind_I4 .or. divisor_tk == ESMF_TypeKind_I8, "Divisor type must be integer.")

      if (dividend_tk ==ESMF_TypeKind_R4) then
         call assign_fptr(dividend,dividend_ptr_r4,_RC)
         call assign_fptr(quotient,quotient_ptr_r4,_RC)
         if(divisor_tk == ESMF_TypeKind_I4) then
            call assign_fptr(divisor,divisor_ptr_i4,_RC)
            call set_quotient(quotient_ptr_r4, dividend_ptr_r4, divisor_ptr_i4, fields, has_undef, _RC)
         else
            call assign_fptr(divisor,divisor_ptr_i8,_RC)
            call set_quotient(quotient_ptr_r4, dividend_ptr_r4, divisor_ptr_i8, fields, has_undef, _RC)
         end if
      else if (dividend_tk == ESMF_TypeKind_R8) then
         call assign_fptr(dividend,dividend_ptr_r8,_RC)
         call assign_fptr(quotient,quotient_ptr_r8,_RC)
         if(divisor_tk == ESMF_TypeKind_I4) then
            call assign_fptr(divisor,divisor_ptr_i4,_RC)
            call set_quotient(quotient_ptr_r8, dividend_ptr_r8, divisor_ptr_i4, fields, has_undef, _RC)
         else
            call assign_fptr(divisor,divisor_ptr_i8,_RC)
            call set_quotient(quotient_ptr_r8, dividend_ptr_r8, divisor_ptr_i8, fields, has_undef, _RC)
         end if
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   end subroutine fieldIntegerDivide

   subroutine set_quotient_R4I4(ptrQ, ptrDD, ptrDR, fields, has_undef, rc)
      real(kind=ESMF_KIND_R4), intent(inout) :: ptrQ
      real(kind=ESMF_KIND_R4), intent(in) :: ptrDD
      integer(kind=ESMF_KIND_I4), intent(in) :: ptrDR
      type(ESMF_Field), intent(inout) :: fields(:)
      logical, intent(in) :: has_undef
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), allocatable :: real_undef(:)
      real(kind=ESMF_KIND_I4), allocatable :: integer_undef(:)

      if(has_undef)
         call GetFieldsUndef(fields(1:2), real_undef, _RC)
         call GetFieldsUndef(fields(3:3), integer_undef, _RC)
         where((ptrDD /= real_undef(2)) .and. (ptrDR /= integer_undef(1)))
            ptrQ = ptrDD / ptrDR
         elsewhere
            ptrQ = real_undef(1)
         end where 
      else
         ptrQ = ptrDD / ptrDR
      end if
      _RETURN(_SUCCESS)

   end subroutine set_quotient_R4I4

   subroutine set_quotient_R8I4(ptrQ, ptrDD, ptrDR, fields, has_undef, rc)
      real(kind=ESMF_KIND_R8), intent(inout) :: ptrQ
      real(kind=ESMF_KIND_R8), intent(in) :: ptrDD
      integer(kind=ESMF_KIND_I4), intent(in) :: ptrDR
      type(ESMF_Field), intent(inout) :: fields(:)
      logical, intent(in) :: has_undef
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), allocatable :: real_undef(:)
      real(kind=ESMF_KIND_I4), allocatable :: integer_undef(:)

      if(has_undef)
         call GetFieldsUndef(fields(1:2), real_undef, _RC)
         call GetFieldsUndef(fields(3:3), integer_undef, _RC)
         where((ptrDD /= real_undef(2)) .and. (ptrDR /= integer_undef(1)))
            ptrQ = ptrDD / ptrDR
         elsewhere
            ptrQ = real_undef(1)
         end where 
      else
         ptrQ = ptrDD / ptrDR
      end if
      _RETURN(_SUCCESS)

   end subroutine set_quotient_R8I4

   subroutine set_quotient_R4I8(ptrQ, ptrDD, ptrDR, fields, has_undef, rc)
      real(kind=ESMF_KIND_R4), intent(inout) :: ptrQ
      real(kind=ESMF_KIND_R4), intent(in) :: ptrDD
      integer(kind=ESMF_KIND_I8), intent(in) :: ptrDR
      type(ESMF_Field), intent(inout) :: fields(:)
      logical, intent(in) :: has_undef
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), allocatable :: real_undef(:)
      real(kind=ESMF_KIND_I8), allocatable :: integer_undef(:)

      if(has_undef)
         call GetFieldsUndef(fields(1:2), real_undef, _RC)
         call GetFieldsUndef(fields(3:3), integer_undef, _RC)
         where((ptrDD /= real_undef(2)) .and. (ptrDR /= integer_undef(1)))
            ptrQ = ptrDD / ptrDR
         elsewhere
            ptrQ = real_undef(1)
         end where 
      else
         ptrQ = ptrDD / ptrDR
      end if
      _RETURN(_SUCCESS)

   end subroutine set_quotient_R4I8

   subroutine set_quotient_R8I8(ptrQ, ptrDD, ptrDR, fields, has_undef, rc)
      real(kind=ESMF_KIND_R8), intent(inout) :: ptrQ
      real(kind=ESMF_KIND_R8), intent(in) :: ptrDD
      integer(kind=ESMF_KIND_I8), intent(in) :: ptrDR
      type(ESMF_Field), intent(inout) :: fields(:)
      logical, intent(in) :: has_undef
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), allocatable :: real_undef(:)
      real(kind=ESMF_KIND_I8), allocatable :: integer_undef(:)

      if(has_undef)
         call GetFieldsUndef(fields(1:2), real_undef, _RC)
         call GetFieldsUndef(fields(3:3), integer_undef, _RC)
         where((ptrDD /= real_undef(2)) .and. (ptrDR /= integer_undef(1)))
            ptrQ = ptrDD / ptrDR
         elsewhere
            ptrQ = real_undef(1)
         end where 
      else
         ptrQ = ptrDD / ptrDR
      end if
      _RETURN(_SUCCESS)

   end subroutine set_quotient_R8I8

end module MAPL_FieldBinaryOperations
