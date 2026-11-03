#include "MAPL.h"

!------------------------------------------------------------------------------
! QuantityTypeCharacteristic: the "quantity type" StateItemCharacteristic, a
! ValueCharacteristic holding the two fields that take part in comparison,
! quantity_type and basis. Dimensions and molecular weight are not carried:
! VariableSpec declares neither.
!
! needs_extension_for matches when quantity_type is equal or either side is
! QUANTITY_UNKNOWN, and basis is equal or either side is BASIS_NONE. These
! are value rules, not status: an undeclared quantity type is SPECIFIED
! QUANTITY_UNKNOWN. The kind is never MIRRORED.
!
! Legacy converts no mismatch of two non-mirror values (its NullTransform
! is only reachable for a time-dependent aspect, which nothing sets):
! build_transform reports incompatible. write_info writes only
! quantity_type_metadata=, in either phase, for SPECIFIED entries.
!
! A DEFERRED entry takes the quantity type and basis a component set on its
! realized item (resolve_deferred); nothing else changes its status.
!------------------------------------------------------------------------------
module mapl_QuantityTypeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_incompatible
   use mapl_CharacteristicStatus_mod, only: &
        CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_DEFERRED, &
        operator(==)
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, QUANTITY_TYPE_CHARACTERISTIC_KIND
   use mapl_QuantityType_mod, only: QuantityType, MixingRatioBasis, &
                                     QUANTITY_UNKNOWN, BASIS_NONE, &
                                     operator(==)
   use mapl_QuantityTypeMetadata_mod, only: QuantityTypeMetadata
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

   public :: QuantityTypeCharacteristic

   type, extends(ValueCharacteristic) :: QuantityTypeCharacteristic
      private
      type(QuantityType) :: quantity_type = QUANTITY_UNKNOWN
      type(MixingRatioBasis) :: basis = BASIS_NONE
   contains
      procedure, nopass :: get_kind => quantity_type_get_kind
      procedure :: needs_extension_for => quantity_type_needs_extension_for
      procedure :: build_transform => quantity_type_build_transform
      procedure :: get_signature => quantity_type_get_signature
      procedure :: write_info => quantity_type_write_info
      procedure :: resolve_deferred => quantity_type_resolve_deferred
      procedure :: get_quantity_type
      procedure :: get_basis
   end type QuantityTypeCharacteristic

   interface QuantityTypeCharacteristic
      module procedure new_QuantityTypeCharacteristic
   end interface QuantityTypeCharacteristic

contains

   function new_QuantityTypeCharacteristic(quantity_type, basis) result(characteristic)
      type(QuantityType), optional, intent(in) :: quantity_type
      type(MixingRatioBasis), optional, intent(in) :: basis
      type(QuantityTypeCharacteristic) :: characteristic

      if (present(quantity_type)) characteristic%quantity_type = quantity_type
      if (present(basis)) characteristic%basis = basis
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_QuantityTypeCharacteristic

   function quantity_type_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = QUANTITY_TYPE_CHARACTERISTIC_KIND
   end function quantity_type_get_kind

   ! `goal` is guaranteed a QuantityTypeCharacteristic by the caller.
   logical function quantity_type_needs_extension_for(this, goal) result(needs_extension)
      class(QuantityTypeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      logical :: quantity_matches, basis_matches

      select type (goal)
      class is (QuantityTypeCharacteristic)
         quantity_matches = (this%quantity_type == goal%quantity_type) .or. &
                             (this%quantity_type == QUANTITY_UNKNOWN) .or. &
                             (goal%quantity_type == QUANTITY_UNKNOWN)
         basis_matches = (this%basis == goal%basis) .or. &
                          (this%basis == BASIS_NONE) .or. &
                          (goal%basis == BASIS_NONE)
         needs_extension = .not. (quantity_matches .and. basis_matches)
      class default
         error stop 'QuantityTypeCharacteristic: needs_extension_for called with a non-QuantityTypeCharacteristic goal - should be checked by calling procedure'
      end select
   end function quantity_type_needs_extension_for

   function get_quantity_type(this) result(quantity_type)
      class(QuantityTypeCharacteristic), intent(in) :: this
      type(QuantityType) :: quantity_type

      quantity_type = this%quantity_type
   end function get_quantity_type

   function get_basis(this) result(basis)
      class(QuantityTypeCharacteristic), intent(in) :: this
      type(MixingRatioBasis) :: basis

      basis = this%basis
   end function get_basis

   subroutine quantity_type_build_transform(this, goal, transformer, rc)
      class(QuantityTypeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine quantity_type_build_transform

   ! Writes quantity_type_metadata= only (not a creation input, not a
   ! FieldDelta trigger). Only SPECIFIED entries hold a value.
   subroutine quantity_type_write_info(this, handle, phase, rc)
      class(QuantityTypeCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(QuantityTypeMetadata) :: metadata

      _UNUSED_DUMMY(phase)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      metadata = QuantityTypeMetadata(quantity_type=this%quantity_type, &
           basis=this%basis)
      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, quantity_type_metadata=metadata, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, quantity_type_metadata=metadata, _RC)
      else
         _FAIL('QuantityTypeCharacteristic: needs Field or FieldBundle')
      end if

      _RETURN(_SUCCESS)
   end subroutine quantity_type_write_info

   ! Only a DEFERRED entry takes a value; the descriptor also offers the
   ! defaults to UNCHECKED ones, which stay as they are.
   subroutine quantity_type_resolve_deferred(this, defaults, rc)
      class(QuantityTypeCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_DEFERRED)
      _RETURN_UNLESS(defaults%has_quantity_type())

      this%quantity_type = defaults%get_quantity_type()
      this%basis = defaults%get_basis()
      call this%set_status(CHARACTERISTIC_STATUS_SPECIFIED)

      _RETURN(_SUCCESS)
   end subroutine quantity_type_resolve_deferred

   function quantity_type_get_signature(this) result(signature)
      class(QuantityTypeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'quantity_type=' // this%quantity_type%to_string() // &
           ';basis=' // this%basis%to_string()
   end function quantity_type_get_signature

end module mapl_QuantityTypeCharacteristic_mod
