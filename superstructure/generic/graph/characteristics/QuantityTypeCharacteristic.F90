#include "MAPL.h"

!------------------------------------------------------------------------------
! QuantityTypeCharacteristic: the "quantity type" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D4) - a
! ValueCharacteristic storing only the two fields legacy
! QuantityTypeAspect%matches() itself uses: quantity_type and basis (both
! mapl_QuantityType_mod enum types, no StateRegistry coupling).
! QuantityTypeAspect's other fields (dimensions, molecular_weight) play no
! role in its own mismatch comparison and are not carried over here
! (REQ-CHAR-002 scopes this hierarchy to mismatch description, not full
! legacy-aspect-object replication).
!
! needs_extension_for ports QuantityTypeAspect%matches()'s own logic
! verbatim: match (no extension needed) when quantity_type is equal or
! either side is QUANTITY_UNKNOWN, AND basis is equal or either side is
! BASIS_NONE.
!
! A `build_transform` is required by the common interface but fails
! explicitly here - no real provider exists for this axis
! (openspec/changes/merge-characteristic-families): legacy
! QuantityTypeAspect%make_transform always returns NullTransform
! ("metadata-only, returns NullTransform"). `get_signature` derives a
! stable string from `quantity_type`/`basis`.
!------------------------------------------------------------------------------
module mapl_QuantityTypeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, QUANTITY_TYPE_CHARACTERISTIC_KIND
   use mapl_QuantityType_mod, only: QuantityType, MixingRatioBasis, &
                                     QUANTITY_UNKNOWN, BASIS_NONE, &
                                     operator(==)
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
   end function new_QuantityTypeCharacteristic

   function quantity_type_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = QUANTITY_TYPE_CHARACTERISTIC_KIND
   end function quantity_type_get_kind

   ! `goal` is guaranteed a QuantityTypeCharacteristic for the real call
   ! chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic).
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

   ! merge-characteristic-families task 3.4: no real adaptation exists
   ! for this axis, graph-native or legacy (module header) - fails
   ! explicitly.
   subroutine quantity_type_build_transform(this, goal, transformer, rc)
      class(QuantityTypeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      _FAIL('QuantityTypeCharacteristic: build_transform - no real provider exists for this axis')
   end subroutine quantity_type_build_transform

   ! merge-characteristic-families task 3.4: a stable, value-
   ! distinguishing signature derived from this subclass's own stored
   ! value (design.md Risks: a small, mechanical derivation).
   function quantity_type_get_signature(this) result(signature)
      class(QuantityTypeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'quantity_type=' // this%quantity_type%to_string() // &
           ';basis=' // this%basis%to_string()
   end function quantity_type_get_signature

end module mapl_QuantityTypeCharacteristic_mod
