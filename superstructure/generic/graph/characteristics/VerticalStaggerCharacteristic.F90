#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalStaggerCharacteristic: the "vertical stagger" StateItemCharacteristic
! (CENTER, EDGE or NONE: where the levels sit relative to the layers, or no
! vertical dimension at all). A ValueCharacteristic wrapping the existing
! VerticalStaggerLoc enum.
!
! Status: SPECIFIED when constructed with a value. VERTICAL_STAGGER_MIRROR is
! not a value to compare: the VariableSpec factory turns it into MIRRORED
! status. DEFERRED means the component fills it later: from a realized field
! (harvest), or as NONE when the component has no vertical grid
! (resolve_deferred).
!
! needs_extension_for compares the two values exactly. VerticalStaggerLoc's
! own operator(==) treats MIRROR as a wildcard, which never applies here
! because a MIRRORED instance holds no value and is never compared.
! build_transform reports unsupported: no vertical-regrid Transform exists.
!------------------------------------------------------------------------------
module mapl_VerticalStaggerCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_STAGGER_CHARACTERISTIC_KIND
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc, &
        VERTICAL_STAGGER_NONE, VERTICAL_STAGGER_INVALID
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_field_api, only: MAPL_FieldSet, MAPL_FieldGet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: ESMF_FieldGet, ESMF_FieldBundleGet, ESMF_FieldStatus_Flag, &
        ESMF_FIELDSTATUS_COMPLETE, ESMF_ITEMORDER_ADDORDER
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalStaggerCharacteristic

   type, extends(ValueCharacteristic) :: VerticalStaggerCharacteristic
      private
      type(VerticalStaggerLoc) :: vertical_stagger
   contains
      procedure, nopass :: get_kind => stagger_get_kind
      procedure :: needs_extension_for => stagger_needs_extension_for
      procedure :: build_transform => stagger_build_transform
      procedure :: get_signature => stagger_get_signature
      procedure :: write_info => stagger_write_info
      procedure :: is_structural => stagger_is_structural
      procedure :: resolve_deferred => stagger_resolve_deferred
      procedure :: harvest => stagger_harvest
      procedure :: get_vertical_stagger
   end type VerticalStaggerCharacteristic

   interface VerticalStaggerCharacteristic
      module procedure new_VerticalStaggerCharacteristic
   end interface VerticalStaggerCharacteristic

contains

   function new_VerticalStaggerCharacteristic(vertical_stagger) result(characteristic)
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      type(VerticalStaggerCharacteristic) :: characteristic

      characteristic%vertical_stagger = vertical_stagger
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_VerticalStaggerCharacteristic

   function stagger_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = VERTICAL_STAGGER_CHARACTERISTIC_KIND
   end function stagger_get_kind

   ! A valueless instance holds the default (INVALID) stagger.
   function get_vertical_stagger(this) result(vertical_stagger)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      type(VerticalStaggerLoc) :: vertical_stagger

      vertical_stagger = this%vertical_stagger
   end function get_vertical_stagger

   logical function stagger_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalStaggerCharacteristic)
         needs_extension = (this%vertical_stagger%to_string() /= goal%vertical_stagger%to_string())
      class default
         error stop 'VerticalStaggerCharacteristic: needs_extension_for called with a non-VerticalStaggerCharacteristic goal - should be checked by calling procedure'
      end select
   end function stagger_needs_extension_for

   subroutine stagger_build_transform(this, goal, transformer, rc)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine stagger_build_transform

   function stagger_get_signature(this) result(signature)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'vertical_stagger=' // this%vertical_stagger%to_string()
   end function stagger_get_signature

   ! vert_staggerloc= decides the level count at creation, so it is written
   ! only in the STRUCTURAL phase. UNCHECKED is not a stagger state.
   subroutine stagger_write_info(this, handle, phase, rc)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, vert_staggerloc=this%vertical_stagger, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, vert_staggerloc=this%vertical_stagger, _RC)
      else
         _FAIL('VerticalStaggerCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine stagger_write_info

   logical function stagger_is_structural(this) result(structural)
      class(VerticalStaggerCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function stagger_is_structural

   ! A value harvested from a realized field wins. Otherwise, at component
   ! scope, a component with no vertical grid has no vertical dimension, so
   ! NONE. With a grid and nothing harvested yet it stays DEFERRED.
   subroutine stagger_resolve_deferred(this, defaults, rc)
      class(VerticalStaggerCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)

      if (defaults%has_vert_stagger()) then
         this%vertical_stagger = defaults%get_vert_stagger()
         call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
      else if (defaults%is_component_scope() .and. .not. defaults%has_vgrid()) then
         this%vertical_stagger = VERTICAL_STAGGER_NONE
         call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
      end if

      _RETURN(_SUCCESS)
   end subroutine stagger_resolve_deferred

   ! A completed Field, or the first member of a FieldBundle, has recorded
   ! its vertical stagger; that supplies it to the defaults.
   subroutine stagger_harvest(this, handle, defaults, rc)
      class(VerticalStaggerCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(in) :: handle
      type(CharacteristicDefaults), intent(inout) :: defaults
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: member
      type(ESMF_Field), allocatable :: members(:)
      type(ESMF_FieldBundle) :: bundle
      integer :: n_members
      type(ESMF_FieldStatus_Flag) :: field_status
      type(VerticalStaggerLoc) :: stagger
      logical :: recorded

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         member = handle%get_field(_RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call ESMF_FieldBundleGet(bundle, fieldCount=n_members, _RC)
         _RETURN_UNLESS(n_members > 0)
         allocate(members(n_members))
         call ESMF_FieldBundleGet(bundle, fieldList=members, &
              itemOrderflag=ESMF_ITEMORDER_ADDORDER, _RC)
         member = members(1)
      else
         _RETURN(_SUCCESS)
      end if

      call ESMF_FieldGet(member, status=field_status, _RC)
      _RETURN_UNLESS(field_status == ESMF_FIELDSTATUS_COMPLETE)

      ! A field that never recorded a stagger reports INVALID.
      call MAPL_FieldGet(member, vert_staggerloc=stagger, _RC)
      recorded = stagger%to_string() /= VERTICAL_STAGGER_INVALID%to_string()
      _RETURN_UNLESS(recorded)
      call defaults%set_vert_stagger(stagger)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine stagger_harvest

end module mapl_VerticalStaggerCharacteristic_mod
