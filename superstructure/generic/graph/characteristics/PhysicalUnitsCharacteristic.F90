#include "MAPL.h"

!------------------------------------------------------------------------------
! PhysicalUnitsCharacteristic: the "physical units" StateItemCharacteristic
! (REQ-CHAR-001, REQ-CHAR-002a). ValueCharacteristic holds its value
! inline, never shared. needs_extension_for checks string inequality.
! Named to avoid collision with graph/extension-reuse's unrelated
! mapl_UnitsCharacteristic_mod type.
!------------------------------------------------------------------------------
module mapl_PhysicalUnitsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, PHYSICAL_UNITS_CHARACTERISTIC_KIND
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_UnitsConverterTransform_mod, only: UnitsConverterTransform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: PhysicalUnitsCharacteristic

   type, extends(ValueCharacteristic) :: PhysicalUnitsCharacteristic
      private
      character(:), allocatable :: units
   contains
      procedure, nopass :: get_kind => units_get_kind
      procedure :: needs_extension_for => units_needs_extension_for
      procedure :: build_transform => units_build_transform
      procedure :: get_signature => units_get_signature
      procedure :: get_units
   end type PhysicalUnitsCharacteristic

   interface PhysicalUnitsCharacteristic
      module procedure new_PhysicalUnitsCharacteristic
   end interface PhysicalUnitsCharacteristic

contains

   function new_PhysicalUnitsCharacteristic(units) result(characteristic)
      character(*), intent(in) :: units
      type(PhysicalUnitsCharacteristic) :: characteristic

      characteristic%units = units
   end function new_PhysicalUnitsCharacteristic

   function units_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = PHYSICAL_UNITS_CHARACTERISTIC_KIND
   end function units_get_kind

   ! `goal` is guaranteed a PhysicalUnitsCharacteristic for the real call
   ! chain this method is used in: the one production caller,
   ! find_mismatched_state_item_characteristics, looks both sides up by
   ! the same StateItemCharacteristicKind key, and
   ! StateItemDescriptor%set_characteristic (the only insertion path into that
   ! map) asserts a characteristic's own kind matches its map key before
   ! inserting (REQ-CHAR-006/009, design.md D9) - so the class default
   ! branch below should be checked by the calling procedure, matching
   ! this codebase's own established `error stop` precedent for exactly
   ! that situation (DependencyNetwork.F90, ActualConnectionPt.F90).
   logical function units_needs_extension_for(this, goal) result(needs_extension)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (PhysicalUnitsCharacteristic)
         needs_extension = (this%units /= goal%units)
      class default
         error stop 'PhysicalUnitsCharacteristic: needs_extension_for called with a non-PhysicalUnitsCharacteristic goal - should be checked by calling procedure'
      end select
   end function units_needs_extension_for

   function get_units(this) result(units)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: units

      units = this%units
   end function get_units

   ! Read through accessor for symmetry with get_signature.
   subroutine units_build_transform(this, goal, transformer, rc)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      select type (goal)
      class is (PhysicalUnitsCharacteristic)
         allocate(transformer, source=UnitsConverterTransform(this%get_units(), goal%get_units()))
      class default
         _FAIL('PhysicalUnitsCharacteristic: build_transform called with a non-units goal')
      end select

      _RETURN(_SUCCESS)
   end subroutine units_build_transform

   ! (Moved from retired mapl_UnitsCharacteristic_mod)
   function units_get_signature(this) result(signature)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'units=' // this%units
   end function units_get_signature

end module mapl_PhysicalUnitsCharacteristic_mod
