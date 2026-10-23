#include "MAPL.h"

submodule (mapl_VariableSpec_mod) make_characteristics_smod
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic, UNKNOWN_UNITS
   implicit none(type, external)

   abstract interface
      subroutine factory_interface(spec, descriptor, rc)
         import :: VariableSpec, StateItemDescriptor
         class(VariableSpec), intent(in) :: spec
         type(StateItemDescriptor), intent(inout) :: descriptor
         integer, optional, intent(out) :: rc
      end subroutine factory_interface
   end interface

   type :: Factory
      procedure(factory_interface), pointer, nopass :: make => null()
   end type Factory

contains

   ! Runs every characteristic factory against the descriptor. Each factory
   ! decides for itself whether the item shape carries its characteristic.
   module subroutine make_characteristics(this, descriptor, rc)
      class(VariableSpec), intent(in) :: this
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(Factory) :: factories(1)

      factories(1)%make => make_units_characteristic

      do i = 1, size(factories)
         call factories(i)%make(this, descriptor, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine make_characteristics

   ! Declared units are SPECIFIED. Undeclared (or UNKNOWN_UNITS) units are
   ! UNCHECKED: they match anything and are never converted. Only shapes
   ! that carry units get an entry.
   subroutine make_units_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(PhysicalUnitsCharacteristic) :: units
      logical :: declared

      if (.not. (spec%itemType == MAPL_STATEITEM_FIELD .or. spec%itemType == MAPL_STATEITEM_BRACKET .or. &
           spec%itemType == MAPL_STATEITEM_VECTOR .or. spec%itemType == MAPL_STATEITEM_VECTORBRACKET .or. &
           spec%itemType == MAPL_STATEITEM_FIELDBUNDLE .or. spec%itemType == MAPL_STATEITEM_EXPRESSION)) then
         _RETURN(_SUCCESS)
      end if

      declared = allocated(spec%units)
      if (declared) declared = spec%units /= UNKNOWN_UNITS
      if (declared) then
         units = PhysicalUnitsCharacteristic(spec%units)
      else
         ! A default-initialized characteristic is INVALID and fails freeze.
         call units%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
      end if

      call descriptor%set_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, units, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_units_characteristic

end submodule make_characteristics_smod
