#include "MAPL.h"

!------------------------------------------------------------------------------
! PhysicalUnitsCharacteristic: the "physical units" StateItemCharacteristic
! (REQ-CHAR-001, REQ-CHAR-002a). ValueCharacteristic holds its value
! inline, never shared. needs_extension_for checks string inequality.
! An UNCHECKED instance holds no value (get_units returns ''); its Info
! value is UNKNOWN_UNITS. Units are never MIRRORED.
! Named to avoid collision with graph/extension-reuse's unrelated
! mapl_UnitsCharacteristic_mod type.
!------------------------------------------------------------------------------
module mapl_PhysicalUnitsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, operator(==), &
        CHARACTERISTIC_STATUS_SPECIFIED, CHARACTERISTIC_STATUS_UNCHECKED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, PHYSICAL_UNITS_CHARACTERISTIC_KIND
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_UnitsConverterTransform_mod, only: UnitsConverterTransform
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: PhysicalUnitsCharacteristic
   public :: UNKNOWN_UNITS

   ! Info value for UNCHECKED units; also the declared value that means
   ! UNCHECKED.
   character(*), parameter :: UNKNOWN_UNITS = '<unknown>'

   type, extends(ValueCharacteristic) :: PhysicalUnitsCharacteristic
      private
      character(:), allocatable :: units
   contains
      procedure, nopass :: get_kind => units_get_kind
      procedure :: needs_extension_for => units_needs_extension_for
      procedure :: build_transform => units_build_transform
      procedure :: get_signature => units_get_signature
      procedure :: write_info => units_write_info
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
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
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
         needs_extension = (this%get_units() /= goal%get_units())
      class default
         error stop 'PhysicalUnitsCharacteristic: needs_extension_for called with a non-PhysicalUnitsCharacteristic goal - should be checked by calling procedure'
      end select
   end function units_needs_extension_for

   function get_units(this) result(units)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: units

      units = ''
      if (allocated(this%units)) units = this%units
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

   ! Written only in the STRUCTURAL phase: units= on a completed field enters
   ! FieldDelta.
   subroutine units_write_info(this, handle, phase, rc)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units
      type(CharacteristicStatus) :: char_status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)

      char_status = this%get_status()
      _ASSERT(char_status == CHARACTERISTIC_STATUS_SPECIFIED .or. char_status == CHARACTERISTIC_STATUS_UNCHECKED, 'PhysicalUnitsCharacteristic: write_info requires SPECIFIED or UNCHECKED')
      units = this%get_units()
      if (char_status == CHARACTERISTIC_STATUS_UNCHECKED) units = UNKNOWN_UNITS

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, units=units, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, units=units, _RC)
      else
         _FAIL('PhysicalUnitsCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine units_write_info

   ! (Moved from retired mapl_UnitsCharacteristic_mod)
   function units_get_signature(this) result(signature)
      class(PhysicalUnitsCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'units=' // this%get_units()
   end function units_get_signature

end module mapl_PhysicalUnitsCharacteristic_mod
