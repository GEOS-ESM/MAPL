#include "MAPL.h"

module mapl_StateSet_mod

   use mapl_field_api
   use mapl_UngriddedDims_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_StateItemVariantInfo_mod, only: StateItemVariantInfoSet
   use esmf

   implicit none(type,external)
   private

   public :: StateSet

   interface StateSet
      procedure state_set
      procedure state_set_own
   end interface StateSet

contains

   subroutine state_set(state, itemName, unusable, &
        typekind, &
        num_levels, num_vgrid_levels, &
        units, &
        rc)

      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      integer, optional, intent(in) :: num_vgrid_levels
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      integer :: status

      call ESMF_StateGet(state, itemName=itemName, field=field, _RC)
      call MAPL_FieldSet(field, &
           num_levels=num_levels, &
           units=units, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(num_vgrid_levels)
   end subroutine state_set

   ! Properties of the State itself (as opposed to an item in it).
   subroutine state_set_own(state, unusable, state_item_variant, rc)
      type(ESMF_State), intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(MAPL_StateItem_Flag), optional, intent(in) :: state_item_variant
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      if (present(state_item_variant)) then
         call ESMF_InfoGetFromHost(state, info, _RC)
         call StateItemVariantInfoSet(info, state_item_variant, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_set_own

end module mapl_StateSet_mod
