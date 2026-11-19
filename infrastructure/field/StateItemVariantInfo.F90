#include "MAPL.h"

! Reads and writes the state-item variant tag on an ESMF_Info. Shared by the
! Field, FieldBundle and State setters/getters so the key lives in one place.
module mapl_StateItemVariantInfo_mod
   use mapl_esmf_info_keys_mod, only: KEY_STATE_ITEM_VARIANT
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, MAPL_STATEITEM_INVALID, operator(/=)
   use mapl_ErrorHandling_mod
   use esmf, only: ESMF_Info, ESMF_InfoSet, ESMF_InfoGetCharAlloc, ESMF_InfoIsPresent
   implicit none(type, external)
   private

   public :: StateItemVariantInfoSet
   public :: StateItemVariantInfoGet

contains

   subroutine StateItemVariantInfoSet(info, variant, rc)
      type(ESMF_Info), intent(inout) :: info
      type(MAPL_StateItem_Flag), intent(in) :: variant
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=KEY_STATE_ITEM_VARIANT, value=variant%to_string(), _RC)

      _RETURN(_SUCCESS)
   end subroutine StateItemVariantInfoSet

   ! Returns MAPL_STATEITEM_INVALID when no tag was ever attached; fails if a
   ! tag is present but not a known variant name.
   function StateItemVariantInfoGet(info, rc) result(variant)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant

      integer :: status
      logical :: is_present
      character(:), allocatable :: name

      variant = MAPL_STATEITEM_INVALID
      is_present = ESMF_InfoIsPresent(info, key=KEY_STATE_ITEM_VARIANT, _RC)
      _RETURN_UNLESS(is_present)

      call ESMF_InfoGetCharAlloc(info, key=KEY_STATE_ITEM_VARIANT, value=name, _RC)
      variant = MAPL_StateItem_Flag(name)
      _ASSERT(variant /= MAPL_STATEITEM_INVALID, 'unrecognized state item variant: ' // name)

      _RETURN(_SUCCESS)
   end function StateItemVariantInfoGet

end module mapl_StateItemVariantInfo_mod
