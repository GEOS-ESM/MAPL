#include "MAPL.h"

submodule (mapl_VariableSpec_mod) make_characteristics_smod
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        TYPE_KIND_CHARACTERISTIC_KIND, UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_MIRRORED
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic, UNKNOWN_UNITS
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR
   use mapl_UngriddedDimsCharacteristic_mod, only: UngriddedDimsCharacteristic
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
      type(Factory) :: factories(3)

      factories(1)%make => make_units_characteristic
      factories(2)%make => make_typekind_characteristic
      factories(3)%make => make_ungridded_dims_characteristic

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

      _RETURN_UNLESS(carries_field_description(spec%itemType))

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

   ! MAPL_TYPEKIND_MIRROR is MIRRORED (the import adopts the matched
   ! export's). Anything else, including the R4 default, is SPECIFIED; it is
   ! never DEFERRED.
   subroutine make_typekind_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(TypeKindCharacteristic) :: type_kind

      _RETURN_UNLESS(carries_field_description(spec%itemType))

      if (spec%typekind == MAPL_TYPEKIND_MIRROR) then
         call type_kind%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         type_kind = TypeKindCharacteristic(spec%typekind)
      end if

      call descriptor%set_characteristic(TYPE_KIND_CHARACTERISTIC_KIND, type_kind, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_typekind_characteristic

   ! An explicitly mirrored UngriddedDims is MIRRORED (the import adopts the
   ! matched export's). Anything else, including the empty default, is
   ! SPECIFIED; it is never DEFERRED. An EXPRESSION has no ungridded dims of
   ! its own, so only shapes whose payload is a Field or FieldBundle get an
   ! entry.
   subroutine make_ungridded_dims_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(UngriddedDimsCharacteristic) :: ungridded_dims

      _RETURN_UNLESS(carries_field_description(spec%itemType) .and. spec%itemType /= MAPL_STATEITEM_EXPRESSION)

      if (spec%ungridded_dims%is_mirror()) then
         call ungridded_dims%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         ungridded_dims = UngriddedDimsCharacteristic(spec%ungridded_dims)
      end if

      call descriptor%set_characteristic(UNGRIDDED_DIMS_CHARACTERISTIC_KIND, ungridded_dims, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_ungridded_dims_characteristic

   ! Shapes whose payload is a Field or FieldBundle (or an expression that
   ! evaluates to one) and so carry the field-level kinds.
   logical function carries_field_description(item_type) result(carries)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      carries = item_type == MAPL_STATEITEM_FIELD .or. item_type == MAPL_STATEITEM_BRACKET .or. &
           item_type == MAPL_STATEITEM_VECTOR .or. item_type == MAPL_STATEITEM_VECTORBRACKET .or. &
           item_type == MAPL_STATEITEM_FIELDBUNDLE .or. item_type == MAPL_STATEITEM_EXPRESSION
   end function carries_field_description

end submodule make_characteristics_smod
