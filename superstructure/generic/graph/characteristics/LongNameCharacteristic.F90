#include "MAPL.h"

!------------------------------------------------------------------------------
! LongNameCharacteristic: the "long name" StateItemCharacteristic, a
! ValueCharacteristic holding a plain long_name string. It is descriptive
! only and never compared: needs_extension_for is always false, so a
! connection never needs a step for it. An UNCHECKED instance holds no
! value and writes nothing to Info.
!
! No Transform exists for this axis: build_transform reports unsupported.
!------------------------------------------------------------------------------
module mapl_LongNameCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, &
        ValueCharacteristic, report_build_transform_unsupported
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, LONG_NAME_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, operator(==)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: LongNameCharacteristic

   type, extends(ValueCharacteristic) :: LongNameCharacteristic
      private
      character(:), allocatable :: long_name
   contains
      procedure, nopass :: get_kind => long_name_get_kind
      procedure :: needs_extension_for => long_name_needs_extension_for
      procedure :: build_transform => long_name_build_transform
      procedure :: get_signature => long_name_get_signature
      procedure :: write_info => long_name_write_info
      procedure :: is_per_alias => long_name_is_per_alias
      procedure :: get_long_name
   end type LongNameCharacteristic

   interface LongNameCharacteristic
      module procedure new_LongNameCharacteristic
   end interface LongNameCharacteristic

contains

   function new_LongNameCharacteristic(long_name) result(characteristic)
      character(*), intent(in) :: long_name
      type(LongNameCharacteristic) :: characteristic

      characteristic%long_name = long_name
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_LongNameCharacteristic

   function long_name_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = LONG_NAME_CHARACTERISTIC_KIND
   end function long_name_get_kind

   ! Descriptive metadata: a different long_name never needs a step.
   logical function long_name_needs_extension_for(this, goal) result(needs)
      class(LongNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)
      needs = .false.
   end function long_name_needs_extension_for

   function get_long_name(this) result(long_name)
      class(LongNameCharacteristic), intent(in) :: this
      character(:), allocatable :: long_name

      long_name = ''
      if (allocated(this%long_name)) long_name = this%long_name
   end function get_long_name

   subroutine long_name_build_transform(this, goal, transformer, rc)
      class(LongNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine long_name_build_transform

   ! Descriptive: valid in either phase. MAPL_FieldSet stores a Field's
   ! long_name under the field's own alias; a bundle's is bundle-wide.
   ! An UNCHECKED instance holds no value and writes nothing.
   subroutine long_name_write_info(this, handle, phase, rc)
      class(LongNameCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _UNUSED_DUMMY(phase)

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         _RETURN_UNLESS(allocated(this%long_name))
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, long_name=this%long_name, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         _RETURN_UNLESS(allocated(this%long_name))
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, long_name=this%long_name, _RC)
      else
         _FAIL('LongNameCharacteristic: write_info needs Field/FieldBundle')
      end if

      _RETURN(_SUCCESS)
   end subroutine long_name_write_info

   ! A Field's long_name is stored under its own alias.
   logical function long_name_is_per_alias(this) result(per_alias)
      class(LongNameCharacteristic), intent(in) :: this

      _UNUSED_DUMMY(this)
      per_alias = .true.
   end function long_name_is_per_alias

   function long_name_get_signature(this) result(signature)
      class(LongNameCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'long_name=' // this%get_long_name()
   end function long_name_get_signature

end module mapl_LongNameCharacteristic_mod
