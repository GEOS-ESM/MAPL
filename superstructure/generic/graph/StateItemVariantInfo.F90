#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_StateItemVariantInfo_mod: single choke point for the reserved ESMF_Info
! key used to attach/read a MAPL_StateItem_Flag ("variant") on a Field's/
! FieldBundle's/State's own Info object (spec/04-graph-value-hierarchy.md
! REQ-SI-002c). No other module constructs the raw key string.
!
! Purely mechanical: reads/writes whatever MAPL_StateItem_Flag is given,
! and reports via `found` whether a tag was present at all. Deciding the
! per-native-kind default when no tag is present (FIELD for a field,
! FIELDBUNDLE for a field bundle, STATE for a state) is mapl_StateItem_mod's
! business logic, not this module's.
!------------------------------------------------------------------------------
module mapl_StateItemVariantInfo_mod
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_State
   use ESMF, only: ESMF_Info, ESMF_InfoGetFromHost, ESMF_InfoIsPresent
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, &
                                     MAPL_StateItemFlag_set_info, MAPL_StateItemFlag_get_info
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: set_variant
   public :: get_variant

   character(*), parameter :: VARIANT_KEY = '/MAPL/GraphStateItem/variant'

   interface set_variant
      module procedure set_variant_field
      module procedure set_variant_field_bundle
      module procedure set_variant_state
   end interface set_variant

   interface get_variant
      module procedure get_variant_field
      module procedure get_variant_field_bundle
      module procedure get_variant_state
   end interface get_variant

contains

   subroutine set_variant_field(field, variant, rc)
      type(ESMF_Field), intent(inout) :: field
      type(MAPL_StateItem_Flag), intent(in) :: variant
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      call MAPL_StateItemFlag_set_info(info, VARIANT_KEY, variant, _RC)
   end subroutine set_variant_field

   subroutine set_variant_field_bundle(field_bundle, variant, rc)
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      type(MAPL_StateItem_Flag), intent(in) :: variant
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field_bundle, info, _RC)
      call MAPL_StateItemFlag_set_info(info, VARIANT_KEY, variant, _RC)
   end subroutine set_variant_field_bundle

   subroutine set_variant_state(state, variant, rc)
      type(ESMF_State), intent(inout) :: state
      type(MAPL_StateItem_Flag), intent(in) :: variant
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(state, info, _RC)
      call MAPL_StateItemFlag_set_info(info, VARIANT_KEY, variant, _RC)
   end subroutine set_variant_state

   function get_variant_field(field, found, rc) result(variant)
      type(ESMF_Field), intent(in) :: field
      logical, optional, intent(out) :: found
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant

      integer :: status
      logical :: is_present
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      is_present = ESMF_InfoIsPresent(info, key=VARIANT_KEY, _RC)
      if (present(found)) found = is_present
      if (.not. is_present) then
         _RETURN(_SUCCESS)
      end if
      variant = MAPL_StateItemFlag_get_info(info, VARIANT_KEY, _RC)
   end function get_variant_field

   function get_variant_field_bundle(field_bundle, found, rc) result(variant)
      type(ESMF_FieldBundle), intent(in) :: field_bundle
      logical, optional, intent(out) :: found
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant

      integer :: status
      logical :: is_present
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field_bundle, info, _RC)
      is_present = ESMF_InfoIsPresent(info, key=VARIANT_KEY, _RC)
      if (present(found)) found = is_present
      if (.not. is_present) then
         _RETURN(_SUCCESS)
      end if
      variant = MAPL_StateItemFlag_get_info(info, VARIANT_KEY, _RC)
   end function get_variant_field_bundle

   function get_variant_state(state, found, rc) result(variant)
      type(ESMF_State), intent(in) :: state
      logical, optional, intent(out) :: found
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: variant

      integer :: status
      logical :: is_present
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(state, info, _RC)
      is_present = ESMF_InfoIsPresent(info, key=VARIANT_KEY, _RC)
      if (present(found)) found = is_present
      if (.not. is_present) then
         _RETURN(_SUCCESS)
      end if
      variant = MAPL_StateItemFlag_get_info(info, VARIANT_KEY, _RC)
   end function get_variant_state

end module mapl_StateItemVariantInfo_mod
