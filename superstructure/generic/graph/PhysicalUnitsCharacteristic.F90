#include "MAPL.h"

!------------------------------------------------------------------------------
! PhysicalUnitsCharacteristic: the "physical units" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ValueCharacteristic (REQ-CHAR-002a: holds its value inline, never
! shared). needs_extension_for is a plain string-inequality check,
! mirroring (never calling) graph/extension-reuse's own
! UnitsCharacteristic%needs_extension_for - deliberately independent per
! openspec/changes/state-item-characteristics design.md D6.
!
! Named PhysicalUnitsCharacteristic (the spec's own suggested name, kept
! as-is - design.md D3) rather than "UnitsCharacteristic", to avoid
! colliding with graph/extension-reuse's existing, unrelated
! mapl_UnitsCharacteristic_mod type.
!
! No build_transform/adaptation method exists at this layer (REQ-CHAR-002:
! a StateItemCharacteristic describes, it does not adapt) - the
! associated Transform (ConvertUnitsTransform per REQ-CHAR-001's table) is
! graph/extension-reuse's existing, separate concern.
!------------------------------------------------------------------------------
module mapl_PhysicalUnitsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, PHYSICAL_UNITS_CHARACTERISTIC_KIND
   implicit none(type, external)
   private

   public :: PhysicalUnitsCharacteristic

   type, extends(ValueCharacteristic) :: PhysicalUnitsCharacteristic
      private
      character(:), allocatable :: units
   contains
      procedure, nopass :: get_kind => units_get_kind
      procedure :: needs_extension_for => units_needs_extension_for
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
   ! GraphStateItem%set_characteristic (the only insertion path into that
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

end module mapl_PhysicalUnitsCharacteristic_mod
