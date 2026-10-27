#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalOrientationCharacteristic: the "vertical orientation" StateItemCharacteristic
! (UP or DOWN: the order of the vertical index relative to physical up/down).
! A ValueCharacteristic wrapping VerticalCoordinateDirection. Legacy calls
! this "alignment"; with_grid is not a value here, it is resolved.
!
! Status: SPECIFIED when constructed with a value. An item that follows its
! grid ("with_grid", the default) gets the grid's direction as that value:
! from the item's own grid at construction, or from the component's grid
! through resolve_deferred (DEFERRED until then). Comparison reads only the
! two orientation values, never a VerticalGrid. MIRRORED means the import
! adopts the export's orientation.
!
! build_transform reports unsupported: no index-reversal Transform exists.
!------------------------------------------------------------------------------
module mapl_VerticalOrientationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_ORIENTATION_CHARACTERISTIC_KIND
   use mapl_VerticalCoordinateDirection_mod, only: VerticalCoordinateDirection, VCOORD_DIRECTION_UP, VCOORD_DIRECTION_DOWN, operator(==), operator(/=)
   use mapl_VerticalAlignment_mod, only: VerticalAlignment, VALIGN_UP, VALIGN_DOWN
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalOrientationCharacteristic

   type, extends(ValueCharacteristic) :: VerticalOrientationCharacteristic
      private
      type(VerticalCoordinateDirection) :: direction
   contains
      procedure, nopass :: get_kind => orientation_get_kind
      procedure :: needs_extension_for => orientation_needs_extension_for
      procedure :: build_transform => orientation_build_transform
      procedure :: get_signature => orientation_get_signature
      procedure :: write_info => orientation_write_info
      procedure :: is_structural => orientation_is_structural
      procedure :: resolve_deferred => orientation_resolve_deferred
      procedure :: drop_if_unsupplied => orientation_drop_if_unsupplied
      procedure :: get_direction
   end type VerticalOrientationCharacteristic

   interface VerticalOrientationCharacteristic
      module procedure new_VerticalOrientationCharacteristic
   end interface VerticalOrientationCharacteristic

contains

   function new_VerticalOrientationCharacteristic(direction) result(characteristic)
      type(VerticalCoordinateDirection), intent(in) :: direction
      type(VerticalOrientationCharacteristic) :: characteristic

      characteristic%direction = direction
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_VerticalOrientationCharacteristic

   function orientation_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = VERTICAL_ORIENTATION_CHARACTERISTIC_KIND
   end function orientation_get_kind

   ! A valueless instance holds the type's default direction.
   function get_direction(this) result(direction)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      type(VerticalCoordinateDirection) :: direction

      direction = this%direction
   end function get_direction

   logical function orientation_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalOrientationCharacteristic)
         needs_extension = (this%direction /= goal%direction)
      class default
         error stop 'VerticalOrientationCharacteristic: needs_extension_for called with a non-VerticalOrientationCharacteristic goal - should be checked by calling procedure'
      end select
   end function orientation_needs_extension_for

   subroutine orientation_build_transform(this, goal, transformer, rc)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine orientation_build_transform

   function orientation_get_signature(this) result(signature)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'vertical_orientation=' // this%direction%to_string()
   end function orientation_get_signature

   ! vert_alignment= is a creation input like the stagger, so it is written
   ! only in the STRUCTURAL phase. The resolved direction is written rather
   ! than with_grid: readers resolve either against the same grid, and the
   ! resolved value stays correct if the field's grid is later replaced.
   subroutine orientation_write_info(this, handle, phase, rc)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(VerticalAlignment) :: alignment

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      if (this%direction == VCOORD_DIRECTION_UP) then
         alignment = VALIGN_UP
      else if (this%direction == VCOORD_DIRECTION_DOWN) then
         alignment = VALIGN_DOWN
      else
         _FAIL('VerticalOrientationCharacteristic: SPECIFIED entry has no up or down direction')
      end if

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, vert_alignment=alignment, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, vert_alignment=alignment, _RC)
      else
         _FAIL('VerticalOrientationCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine orientation_write_info

   logical function orientation_is_structural(this) result(structural)
      class(VerticalOrientationCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function orientation_is_structural

   ! An item that follows its grid takes the component grid's direction. With
   ! no component grid it stays DEFERRED (it may still be dropped).
   subroutine orientation_resolve_deferred(this, defaults, rc)
      class(VerticalOrientationCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      class(VerticalGrid), allocatable :: vgrid

      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)
      _RETURN_UNLESS(defaults%has_vgrid())

      call defaults%get_vgrid(vgrid)
      this%direction = vgrid%get_coordinate_direction()
      call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)

      _RETURN(_SUCCESS)
   end subroutine orientation_resolve_deferred

   ! Without a vertical grid the item has no vertical index to order.
   logical function orientation_drop_if_unsupplied(this, defaults) result(drop)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      type(CharacteristicDefaults), intent(in) :: defaults

      drop = this%get_status() == CHARACTERISTIC_STATUS_DEFERRED .and. .not. defaults%has_vgrid()
   end function orientation_drop_if_unsupplied

end module mapl_VerticalOrientationCharacteristic_mod
