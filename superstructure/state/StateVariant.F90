#include "MAPL.h"

! Accessors for a State's own Info. MAPL_StateSet/MAPL_StateGet address an
! item inside a State by name; these address the State itself.
module mapl_StateVariant_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_StateItemVariantInfo_mod, only: StateItemVariantInfoSet, StateItemVariantInfoGet
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   use esmf, only: ESMF_State, ESMF_Info, ESMF_InfoGetFromHost
   implicit none(type, external)
   private

   public :: StateSetVariant
   public :: StateGetVariant

contains

   subroutine StateSetVariant(state, state_item_variant, unusable, rc)
      type(ESMF_State), intent(inout) :: state
      type(MAPL_StateItem_Flag), intent(in) :: state_item_variant
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(state, info, _RC)
      call StateItemVariantInfoSet(info, state_item_variant, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine StateSetVariant

   ! Returns MAPL_STATEITEM_INVALID if no variant was ever attached.
   function StateGetVariant(state, unusable, rc) result(state_item_variant)
      type(ESMF_State), intent(in) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: state_item_variant

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(state, info, _RC)
      state_item_variant = StateItemVariantInfoGet(info, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function StateGetVariant

end module mapl_StateVariant_mod
