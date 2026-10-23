#include "MAPL.h"

submodule (mapl_VariableSpec_mod) make_characteristics_smod
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic, UNKNOWN_UNITS
   implicit none(type, external)

contains

   ! Declared units are SPECIFIED. Undeclared (or UNKNOWN_UNITS) units are
   ! UNCHECKED: they match anything and are never converted. Shapes that do
   ! not carry units get no entry.
   module subroutine make_units_characteristic(this, descriptor, rc)
      class(VariableSpec), intent(in) :: this
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(PhysicalUnitsCharacteristic) :: units
      logical :: declared

      if (this%itemType == MAPL_STATEITEM_STATE .or. this%itemType == MAPL_STATEITEM_SERVICE .or. &
           this%itemType == MAPL_STATEITEM_WILDCARD) then
         _RETURN(_SUCCESS)
      end if

      declared = allocated(this%units)
      if (declared) declared = this%units /= UNKNOWN_UNITS
      if (declared) then
         units = PhysicalUnitsCharacteristic(this%units)
      else
         ! A default-initialized characteristic is INVALID and fails freeze.
         call units%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
      end if

      call descriptor%set_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, units, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_units_characteristic

end submodule make_characteristics_smod
