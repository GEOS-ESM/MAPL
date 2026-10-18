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
! openspec/changes/merge-characteristic-families: build_transform/
! get_signature moved verbatim from the now-retired
! mapl_UnitsCharacteristic_mod's own units_build_transform/
! units_get_signature (that module's own real adaptation provider),
! reading this%get_units()/goal%get_units() in place of the original's
! private-component access.
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

   ! Moved verbatim from the retired mapl_UnitsCharacteristic_mod's own
   ! units_build_transform - UnitsConverterTransform(this%units,
   ! goal%units), reading through the get_units() accessor (already
   ! existed) rather than private-component access, since this%units/
   ! goal%units are private within this module to any OTHER module's
   ! subclass, but accessible here, within THIS module, directly - the
   ! accessor form is kept anyway for symmetry with get_signature below
   ! and to make the parallel with the original explicit.
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

   ! Moved verbatim from the retired mapl_UnitsCharacteristic_mod's own
   ! units_get_signature.
   function units_get_signature(this) result(signature)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'units=' // this%units
   end function units_get_signature

end module mapl_PhysicalUnitsCharacteristic_mod
