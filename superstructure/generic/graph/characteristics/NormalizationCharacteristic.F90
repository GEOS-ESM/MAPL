#include "MAPL.h"

!------------------------------------------------------------------------------
! NormalizationCharacteristic: the "normalization" StateItemCharacteristic, a
! ValueCharacteristic wrapping the NormalizationMetadata value type
! (enums/NormalizationMetadata.F90). Its operator(==) has its own
! mirror-aware rule, which needs_extension_for reuses. A VariableSpec mirror
! is not a value to compare: the factory turns it into MIRRORED status.
!
! No Transform exists for this axis (legacy returns NullTransform):
! build_transform reports unsupported. write_info writes only
! normalization_metadata=, in either phase, for SPECIFIED entries.
!------------------------------------------------------------------------------
module mapl_NormalizationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: &
        CHARACTERISTIC_STATUS_SPECIFIED, operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, NORMALIZATION_CHARACTERISTIC_KIND
   use mapl_NormalizationMetadata_mod, only: NormalizationMetadata, operator(==), operator(/=)
   use mapl_NormalizationType_mod, only: NormalizationType
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

   public :: NormalizationCharacteristic

   type, extends(ValueCharacteristic) :: NormalizationCharacteristic
      private
      type(NormalizationMetadata) :: metadata
   contains
      procedure, nopass :: get_kind => normalization_get_kind
      procedure :: needs_extension_for => normalization_needs_extension_for
      procedure :: build_transform => normalization_build_transform
      procedure :: get_signature => normalization_get_signature
      procedure :: write_info => normalization_write_info
      procedure :: get_metadata
   end type NormalizationCharacteristic

   interface NormalizationCharacteristic
      module procedure new_NormalizationCharacteristic
   end interface NormalizationCharacteristic

contains

   function new_NormalizationCharacteristic(metadata) result(characteristic)
      type(NormalizationMetadata), intent(in) :: metadata
      type(NormalizationCharacteristic) :: characteristic

      characteristic%metadata = metadata
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_NormalizationCharacteristic

   function normalization_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = NORMALIZATION_CHARACTERISTIC_KIND
   end function normalization_get_kind

   ! `goal` is guaranteed a NormalizationCharacteristic by the caller.
   logical function normalization_needs_extension_for(this, goal) result(needs_extension)
      class(NormalizationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (NormalizationCharacteristic)
         needs_extension = (this%metadata /= goal%metadata)
      class default
         error stop 'NormalizationCharacteristic: needs_extension_for called with a non-NormalizationCharacteristic goal - should be checked by calling procedure'
      end select
   end function normalization_needs_extension_for

   function get_metadata(this) result(metadata)
      class(NormalizationCharacteristic), intent(in) :: this
      type(NormalizationMetadata) :: metadata

      metadata = this%metadata
   end function get_metadata

   subroutine normalization_build_transform(this, goal, transformer, rc)
      class(NormalizationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine normalization_build_transform

   ! Writes normalization_metadata= only (not a creation input, not a
   ! FieldDelta trigger). Only SPECIFIED entries hold a value.
   subroutine normalization_write_info(this, handle, phase, rc)
      class(NormalizationCharacteristic), intent(in) :: this
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
         call MAPL_FieldSet(field, normalization_metadata=this%metadata, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, &
              normalization_metadata=this%metadata, _RC)
      else
         _FAIL('NormalizationCharacteristic: needs Field or FieldBundle')
      end if

      _RETURN(_SUCCESS)
   end subroutine normalization_write_info

   function normalization_get_signature(this) result(signature)
      class(NormalizationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      character(32) :: scale_buffer
      type(NormalizationType) :: ntype

      if (this%metadata%is_mirror()) then
         signature = 'normalization=<mirror>'
      else
         write(scale_buffer, '(ES12.5)') this%metadata%get_normalization_scale()
         ntype = this%metadata%get_normalization_type()
         signature = 'normalization=' // ntype%to_string() // &
              ';scale=' // trim(adjustl(scale_buffer))
      end if
   end function normalization_get_signature

end module mapl_NormalizationCharacteristic_mod
