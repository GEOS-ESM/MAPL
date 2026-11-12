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
! through resolve_deferred (DEFERRED until then). A grid with no direction
! (a single level) leaves the entry SPECIFIED holding UNSPECIFIED, and
! write_info writes nothing for it. Comparison reads only the two
! orientation values, never a VerticalGrid. MIRRORED means the import
! adopts the export's orientation.
!
! A Basic vertical grid knows only its level count, so it has no direction.
! An entry on a Basic grid is "directionless": it holds UNSPECIFIED, or an
! explicit alignment that is stored and written but never reorders data.
! Any pair with a directionless side needs no step. If the two values are
! opposite (UP and DOWN), match_warning says the data are copied in the
! export's level order. The entry records whether its grid came from the
! item itself (grid_known), so the component's grid cannot replace it.
!
! An orientation-only difference on a Field or a VECTOR is a
! VerticalFlipTransform step; other item shapes (a BRACKET becomes a Field
! before this step) report unsupported. When the vertical grids also differ,
! the vertical-grid step covers the orientation.
!------------------------------------------------------------------------------
module mapl_VerticalOrientationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, report_build_transform_unsupported
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristicMap
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_ORIENTATION_CHARACTERISTIC_KIND
   use mapl_StateItemCharacteristicKind_mod, only: &
        ITEM_SHAPE_CHARACTERISTIC_KIND
   use mapl_ItemShapeCharacteristic_mod, only: ItemShapeCharacteristic
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_VECTOR
   use mapl_VerticalFlipTransform_mod, only: VerticalFlipTransform
   use mapl_VerticalCoordinateDirection_mod, only: VerticalCoordinateDirection, VCOORD_DIRECTION_UP, VCOORD_DIRECTION_DOWN, VCOORD_DIRECTION_UNSPECIFIED, operator(==), operator(/=)
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
      ! A Basic vertical grid is involved, so this entry never orders data.
      logical :: directionless = .false.
      ! Built from the item's own grid; the component's grid is not consulted.
      logical :: grid_known = .false.
   contains
      procedure, nopass :: get_kind => orientation_get_kind
      procedure :: needs_extension_for => orientation_needs_extension_for
      procedure :: match_warning => orientation_match_warning
      procedure :: build_transform => orientation_build_transform
      procedure :: build_transform_for_item => &
           orientation_build_transform_for_item
      procedure :: get_signature => orientation_get_signature
      procedure :: write_info => orientation_write_info
      procedure :: is_structural => orientation_is_structural
      procedure :: resolve_deferred => orientation_resolve_deferred
      procedure :: drop_if_unsupplied => orientation_drop_if_unsupplied
      procedure :: get_direction
      procedure :: is_directionless
   end type VerticalOrientationCharacteristic

   interface VerticalOrientationCharacteristic
      module procedure new_VerticalOrientationCharacteristic
   end interface VerticalOrientationCharacteristic

contains

   function new_VerticalOrientationCharacteristic(direction, directionless, &
        grid_known) result(characteristic)
      type(VerticalCoordinateDirection), intent(in) :: direction
      logical, optional, intent(in) :: directionless
      logical, optional, intent(in) :: grid_known
      type(VerticalOrientationCharacteristic) :: characteristic

      characteristic%direction = direction
      if (present(directionless)) characteristic%directionless = directionless
      if (present(grid_known)) characteristic%grid_known = grid_known
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

   logical function is_directionless(this) result(directionless)
      class(VerticalOrientationCharacteristic), intent(in) :: this

      directionless = this%directionless
   end function is_directionless

   logical function orientation_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalOrientationCharacteristic)
         needs_extension = (this%direction /= goal%direction) .and. &
              .not. (this%directionless .or. goal%directionless)
      class default
         error stop 'VerticalOrientationCharacteristic: needs_extension_for called with a non-VerticalOrientationCharacteristic goal - should be checked by calling procedure'
      end select
   end function orientation_needs_extension_for

   ! Only a Basic side that states a direction opposite to its partner is
   ! worth a warning: with no stated direction nothing was contradicted.
   function orientation_match_warning(this, goal) result(msg)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      character(:), allocatable :: msg

      msg = ''
      select type (goal)
      class is (VerticalOrientationCharacteristic)
         if (.not. (this%directionless .or. goal%directionless)) return
         if (.not. (is_up_or_down(this%direction) .and. &
              is_up_or_down(goal%direction))) return
         if (this%direction == goal%direction) return
         msg = 'vertical orientation: a Basic vertical grid carries no ' // &
              'direction; export ' // this%direction%to_string() // &
              ' and import ' // goal%direction%to_string() // &
              ' differ, so the data are copied in the export''s level ' // &
              'order, with no flip.'
      end select
   end function orientation_match_warning

   logical function is_up_or_down(direction) result(is_ud)
      type(VerticalCoordinateDirection), intent(in) :: direction

      is_ud = direction == VCOORD_DIRECTION_UP .or. &
           direction == VCOORD_DIRECTION_DOWN
   end function is_up_or_down

   subroutine orientation_build_transform(this, goal, transformer, rc)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (VerticalOrientationCharacteristic)
         allocate(transformer, source=VerticalFlipTransform())
      class default
         _FAIL('VerticalOrientationCharacteristic: goal is not an orientation')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine orientation_build_transform

   ! Only a Field or a VECTOR has a flip. A tip with no shape entry is not restricted.
   subroutine orientation_build_transform_for_item(this, goal, tip, &
        transformer, rc)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      type(StateItemCharacteristicMap), target, intent(in) :: tip
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), pointer :: item_shape

      if (tip%count(ITEM_SHAPE_CHARACTERISTIC_KIND) > 0) then
         item_shape => tip%at(ITEM_SHAPE_CHARACTERISTIC_KIND, _RC)
         select type (item_shape)
         type is (ItemShapeCharacteristic)
            if (.not. (item_shape%get_item_class() == MAPL_STATEITEM_FIELD &
                 .or. item_shape%get_item_class() == MAPL_STATEITEM_VECTOR)) then
               call report_build_transform_unsupported(rc)
               return
            end if
         class default
            _FAIL('VerticalOrientationCharacteristic: bad ITEM_SHAPE entry')
         end select
      end if

      call this%build_transform(goal, transformer, rc)
   end subroutine orientation_build_transform_for_item

   function orientation_get_signature(this) result(signature)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'vertical_orientation=' // this%direction%to_string()
   end function orientation_get_signature

   ! vert_alignment= is a creation input like the stagger, so it is written
   ! only in the STRUCTURAL phase. The resolved direction is written rather
   ! than with_grid: readers resolve either against the same grid, and the
   ! resolved value stays correct if the field's grid is later replaced.
   ! An UNSPECIFIED direction is not written.
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
      ! Legacy reads an unwritten alignment as with_grid, which is right here.
      _RETURN_IF(this%direction == VCOORD_DIRECTION_UNSPECIFIED)

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
   ! no component grid it stays DEFERRED (it may still be dropped). A Basic
   ! component grid gives no direction, so the entry holds UNSPECIFIED. An
   ! entry that already has a value (an explicit alignment) keeps it, and only
   ! learns from the component grid that it is directionless.
   subroutine orientation_resolve_deferred(this, defaults, rc)
      class(VerticalOrientationCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      class(VerticalGrid), allocatable :: vgrid

      _RETURN_UNLESS(defaults%has_vgrid())

      if (this%get_status() == CHARACTERISTIC_STATUS_DEFERRED) then
         call defaults%get_vgrid(vgrid)
         if (vgrid%carries_direction()) then
            this%direction = vgrid%get_coordinate_direction()
         else
            this%direction = VCOORD_DIRECTION_UNSPECIFIED
            this%directionless = .true.
         end if
         this%grid_known = .true.
         call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
      else if (this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED) then
         _RETURN_IF(this%grid_known)
         call defaults%get_vgrid(vgrid)
         if (.not. vgrid%carries_direction()) this%directionless = .true.
         this%grid_known = .true.
      end if

      _RETURN(_SUCCESS)
   end subroutine orientation_resolve_deferred

   ! Without a vertical grid the item has no vertical index to order.
   logical function orientation_drop_if_unsupplied(this, defaults) result(drop)
      class(VerticalOrientationCharacteristic), intent(in) :: this
      type(CharacteristicDefaults), intent(in) :: defaults

      drop = this%get_status() == CHARACTERISTIC_STATUS_DEFERRED .and. .not. defaults%has_vgrid()
   end function orientation_drop_if_unsupplied

end module mapl_VerticalOrientationCharacteristic_mod
