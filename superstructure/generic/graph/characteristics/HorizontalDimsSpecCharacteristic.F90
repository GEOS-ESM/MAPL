#include "MAPL.h"

!------------------------------------------------------------------------------
! HorizontalDimsSpecCharacteristic: whether an item spans the horizontal
! geometry (HORIZONTAL_DIMS_GEOM) or has no horizontal dimensions at all
! (HORIZONTAL_DIMS_NONE, e.g. a vertical coefficient array). A
! ValueCharacteristic wrapping the existing HorizontalDimsSpec value type.
!
! Status: SPECIFIED with a GEOM or NONE value. GEOM versus NONE in either
! direction is a mismatch that can never be repaired, so build_transform
! reports it incompatible. MIRRORED holds no value: the import
! adopts the export's spec. HORIZONTAL_DIMS_MIRROR is not a value to compare;
! the VariableSpec factory turns it into MIRRORED status.
!
! An item whose spec is NONE does not compare its GEOMETRY characteristic
! (see GeometryCharacteristic), so two such items connect across different
! horizontal grids.
!------------------------------------------------------------------------------
module mapl_HorizontalDimsSpecCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL, report_build_transform_incompatible
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_MIRRORED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND
   use mapl_HorizontalDimsSpec_mod, only: HorizontalDimsSpec, HORIZONTAL_DIMS_GEOM, HORIZONTAL_DIMS_MIRROR, &
        operator(==), operator(/=)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_field_api, only: MAPL_FieldSet
   use ESMF, only: ESMF_Field, ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: HorizontalDimsSpecCharacteristic

   type, extends(ValueCharacteristic) :: HorizontalDimsSpecCharacteristic
      private
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
   contains
      procedure, nopass :: get_kind => dims_spec_get_kind
      procedure :: needs_extension_for => dims_spec_needs_extension_for
      procedure :: build_transform => dims_spec_build_transform
      procedure :: get_signature => dims_spec_get_signature
      procedure :: write_info => dims_spec_write_info
      procedure :: is_structural => dims_spec_is_structural
      procedure :: get_horizontal_dims_spec
   end type HorizontalDimsSpecCharacteristic

   interface HorizontalDimsSpecCharacteristic
      module procedure new_HorizontalDimsSpecCharacteristic
   end interface HorizontalDimsSpecCharacteristic

contains

   function new_HorizontalDimsSpecCharacteristic(horizontal_dims_spec) result(characteristic)
      type(HorizontalDimsSpec), intent(in) :: horizontal_dims_spec
      type(HorizontalDimsSpecCharacteristic) :: characteristic

      if (horizontal_dims_spec == HORIZONTAL_DIMS_MIRROR) then
         call characteristic%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         characteristic%horizontal_dims_spec = horizontal_dims_spec
         call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
      end if
   end function new_HorizontalDimsSpecCharacteristic

   function dims_spec_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND
   end function dims_spec_get_kind

   function get_horizontal_dims_spec(this) result(horizontal_dims_spec)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this
      type(HorizontalDimsSpec) :: horizontal_dims_spec

      horizontal_dims_spec = this%horizontal_dims_spec
   end function get_horizontal_dims_spec

   logical function dims_spec_needs_extension_for(this, goal) result(needs_extension)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (HorizontalDimsSpecCharacteristic)
         needs_extension = (this%horizontal_dims_spec /= goal%horizontal_dims_spec)
      class default
         error stop 'HorizontalDimsSpecCharacteristic: needs_extension_for called with a non-HorizontalDimsSpecCharacteristic goal - should be checked by calling procedure'
      end select
   end function dims_spec_needs_extension_for

   subroutine dims_spec_build_transform(this, goal, transformer, rc)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine dims_spec_build_transform

   function dims_spec_get_signature(this) result(signature)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'horizontal_dims_spec=' // this%horizontal_dims_spec%to_string()
   end function dims_spec_get_signature

   ! horizontal_dims_spec= decides the field's grid-to-field map at
   ! completion, so it is written only in the STRUCTURAL phase. The bundle
   ! setter has no such keyword (members carry their own), so a bundle has
   ! nothing to write.
   subroutine dims_spec_write_info(this, handle, phase, rc)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, horizontal_dims_spec=this%horizontal_dims_spec, _RC)
      else if (.not. (item_type == ESMF_STATEITEM_FIELDBUNDLE)) then
         _FAIL('HorizontalDimsSpecCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine dims_spec_write_info

   logical function dims_spec_is_structural(this) result(structural)
      class(HorizontalDimsSpecCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function dims_spec_is_structural

end module mapl_HorizontalDimsSpecCharacteristic_mod
