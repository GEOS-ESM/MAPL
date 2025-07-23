#include "MAPL.h"
module mapl3g_MinTransform
   use mapl3g_AccumulatorTransform
   use MAPL_ExceptionHandling
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_FieldPointerUtilities, only: assign_fptr
   use ESMF
   implicit none
   private
   public :: MinTransform
   public :: construct_MinTransform

   type, extends(AccumulatorTransform) :: MinTransform
   contains
      procedure :: accumulate_R4 => min_accumulate_R4
   end type MinTransform

contains

   function construct_MinTransform(typekind) result(acc)
      type(MinTransform) :: acc
      type(ESMF_TypeKind_Flag), intent(in) :: typekind

      acc%typekind = typekind
      acc%CLEAR_VALUE_R4 = MAPL_UNDEFINED_REAL

   end function construct_MinTransform

   subroutine min_accumulate_R4(this, update_field, rc)
      class(MinTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current == UNDEF)
         current = latest
      elsewhere(latest /= UNDEF)
         current = min(current, latest)
      end where
      _RETURN(_SUCCESS)

   end subroutine min_accumulate_R4

end module mapl3g_MinTransform
