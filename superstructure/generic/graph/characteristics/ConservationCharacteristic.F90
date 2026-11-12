#include "MAPL.h"

!------------------------------------------------------------------------------
! ConservationCharacteristic: the "conservation" StateItemCharacteristic, a
! ValueCharacteristic wrapping the ConservationMetadata value type
! (enums/ConservationMetadata.F90). Its operator(==) has its own mirror-aware
! rule, which needs_extension_for reuses. A VariableSpec mirror is not a
! value to compare: the factory turns it into MIRRORED status.
!
! Legacy converts no mismatch of two non-mirror values (its make_transform
! _FAILs): build_transform reports incompatible. write_info writes only
! conservation_metadata=, in either phase, for SPECIFIED entries.
!------------------------------------------------------------------------------
module mapl_ConservationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_incompatible
   use mapl_CharacteristicStatus_mod, only: &
        CHARACTERISTIC_STATUS_SPECIFIED, operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, CONSERVATION_CHARACTERISTIC_KIND
   use mapl_ConservationMetadata_mod, only: ConservationMetadata, operator(==), operator(/=)
   use mapl_ConservationType_mod, only: ConservationType
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ConservationCharacteristic

   type, extends(ValueCharacteristic) :: ConservationCharacteristic
      private
      type(ConservationMetadata) :: metadata
   contains
      procedure, nopass :: get_kind => conservation_get_kind
      procedure :: needs_extension_for => conservation_needs_extension_for
      procedure :: build_transform => conservation_build_transform
      procedure :: get_signature => conservation_get_signature
      procedure :: write_info => conservation_write_info
      procedure :: get_metadata
   end type ConservationCharacteristic

   interface ConservationCharacteristic
      module procedure new_ConservationCharacteristic
   end interface ConservationCharacteristic

contains

   function new_ConservationCharacteristic(metadata) result(characteristic)
      type(ConservationMetadata), intent(in) :: metadata
      type(ConservationCharacteristic) :: characteristic

      characteristic%metadata = metadata
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_ConservationCharacteristic

   function conservation_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = CONSERVATION_CHARACTERISTIC_KIND
   end function conservation_get_kind

   ! `goal` is guaranteed a ConservationCharacteristic by the caller.
   logical function conservation_needs_extension_for(this, goal) result(needs_extension)
      class(ConservationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (ConservationCharacteristic)
         needs_extension = (this%metadata /= goal%metadata)
      class default
         error stop 'ConservationCharacteristic: needs_extension_for called with a non-ConservationCharacteristic goal - should be checked by calling procedure'
      end select
   end function conservation_needs_extension_for

   function get_metadata(this) result(metadata)
      class(ConservationCharacteristic), intent(in) :: this
      type(ConservationMetadata) :: metadata

      metadata = this%metadata
   end function get_metadata

   subroutine conservation_build_transform(this, goal, transformer, rc)
      class(ConservationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine conservation_build_transform

   ! Writes conservation_metadata= only (not a creation input, not a
   ! FieldDelta trigger). Only SPECIFIED entries hold a value.
   subroutine conservation_write_info(this, handle, phase, rc)
      class(ConservationCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _UNUSED_DUMMY(phase)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, conservation_metadata=this%metadata, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, &
              conservation_metadata=this%metadata, _RC)
      else
         _FAIL('ConservationCharacteristic: needs Field or FieldBundle')
      end if

      _RETURN(_SUCCESS)
   end subroutine conservation_write_info

   function conservation_get_signature(this) result(signature)
      class(ConservationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      type(ConservationType) :: ctype

      if (this%metadata%is_mirror()) then
         signature = 'conservation=<mirror>'
      else
         ctype = this%metadata%get_conservation_type()
         signature = 'conservation=' // ctype%to_string()
         if (this%metadata%get_is_conservable()) then
            signature = signature // ';conservable'
         end if
      end if
   end function conservation_get_signature

end module mapl_ConservationCharacteristic_mod
