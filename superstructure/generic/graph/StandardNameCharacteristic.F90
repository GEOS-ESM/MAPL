#include "MAPL.h"

!------------------------------------------------------------------------------
! StandardNameCharacteristic: the "standard name" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D5) - a
! ValueCharacteristic storing a plain standard_name string.
!
! SCOPE BOUNDARY (design.md D5): legacy StandardNameAspect%matches() layers
! real GraphBuilder-adjacent production behavior on top of plain string
! equality - a wildcard/is_unchecked() short-circuit, asymmetric
! accept-with-warning when only one side declares a name, and
! ValidationMode-gated strict-vs-permissive severity with pflogger output
! (FieldDictionaryConfig). None of that is replicated here - this type has
! no ValidationMode/FieldDictionaryConfig/pflogger dependency. Instead,
! needs_extension_for uses the base StateItemCharacteristic's own
! CharacteristicStatus (REQ-CHAR-003) as the "accept without comparison"
! escape: if either side's status is UNCHECKED, no extension is needed,
! exactly matching UNCHECKED's own documented meaning. Otherwise, plain
! string equality - no warning, no ValidationMode, no logging.
!
! No build_transform/adaptation method exists at this layer (REQ-CHAR-002);
! legacy StandardNameAspect%make_transform always returns NullTransform.
!------------------------------------------------------------------------------
module mapl_StandardNameCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, STANDARD_NAME_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED, operator(==)
   implicit none(type, external)
   private

   public :: StandardNameCharacteristic

   type, extends(ValueCharacteristic) :: StandardNameCharacteristic
      private
      character(:), allocatable :: standard_name
   contains
      procedure, nopass :: get_kind => standard_name_get_kind
      procedure :: needs_extension_for => standard_name_needs_extension_for
      procedure :: get_standard_name
   end type StandardNameCharacteristic

   interface StandardNameCharacteristic
      module procedure new_StandardNameCharacteristic
   end interface StandardNameCharacteristic

contains

   function new_StandardNameCharacteristic(standard_name) result(characteristic)
      character(*), intent(in) :: standard_name
      type(StandardNameCharacteristic) :: characteristic

      characteristic%standard_name = standard_name
   end function new_StandardNameCharacteristic

   function standard_name_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = STANDARD_NAME_CHARACTERISTIC_KIND
   end function standard_name_get_kind

   ! `goal` is guaranteed a StandardNameCharacteristic for the real call
   ! chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic). Status-based
   ! escape (design.md D5): either side UNCHECKED means no extension is
   ! needed regardless of the stored names.
   logical function standard_name_needs_extension_for(this, goal) result(needs_extension)
      class(StandardNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (StandardNameCharacteristic)
         if (this%get_status() == CHARACTERISTIC_STATUS_UNCHECKED .or. &
             goal%get_status() == CHARACTERISTIC_STATUS_UNCHECKED) then
            needs_extension = .false.
         else
            needs_extension = (this%standard_name /= goal%standard_name)
         end if
      class default
         error stop 'StandardNameCharacteristic: needs_extension_for called with a non-StandardNameCharacteristic goal - should be checked by calling procedure'
      end select
   end function standard_name_needs_extension_for

   function get_standard_name(this) result(standard_name)
      class(StandardNameCharacteristic), intent(in) :: this
      character(:), allocatable :: standard_name

      standard_name = this%standard_name
   end function get_standard_name

end module mapl_StandardNameCharacteristic_mod
