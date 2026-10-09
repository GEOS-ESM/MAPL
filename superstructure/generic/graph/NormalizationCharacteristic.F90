#include "MAPL.h"

!------------------------------------------------------------------------------
! NormalizationCharacteristic: the "normalization" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D3) - a
! ValueCharacteristic wrapping the existing, already-decoupled
! NormalizationMetadata value type (enums/NormalizationMetadata.F90)
! directly: no StateRegistry coupling, and NormalizationMetadata already
! defines operator(==)/operator(/=) with its own mirror-aware semantics
! (both-mirror compares equal unconditionally) matching legacy
! NormalizationAspect%matches(). needs_extension_for is a single `/=` call -
! no hand-written comparison logic.
!
! No build_transform/adaptation method exists at this layer (REQ-CHAR-002);
! legacy NormalizationAspect%make_transform always returns NullTransform
! (no real adaptation exists anywhere for this axis, graph-native or
! legacy).
!------------------------------------------------------------------------------
module mapl_NormalizationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, NORMALIZATION_CHARACTERISTIC_KIND
   use mapl_NormalizationMetadata_mod, only: NormalizationMetadata, operator(==), operator(/=)
   implicit none(type, external)
   private

   public :: NormalizationCharacteristic

   type, extends(ValueCharacteristic) :: NormalizationCharacteristic
      private
      type(NormalizationMetadata) :: metadata
   contains
      procedure, nopass :: get_kind => normalization_get_kind
      procedure :: needs_extension_for => normalization_needs_extension_for
      procedure :: get_metadata
   end type NormalizationCharacteristic

   interface NormalizationCharacteristic
      module procedure new_NormalizationCharacteristic
   end interface NormalizationCharacteristic

contains

   function new_NormalizationCharacteristic(metadata) result(characteristic)
      type(NormalizationMetadata), intent(in) :: metadata
      type(NormalizationCharacteristic) :: characteristic

      characteristic%metadata = metadata
   end function new_NormalizationCharacteristic

   function normalization_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = NORMALIZATION_CHARACTERISTIC_KIND
   end function normalization_get_kind

   ! `goal` is guaranteed a NormalizationCharacteristic for the real call
   ! chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic).
   logical function normalization_needs_extension_for(this, goal) result(needs_extension)
      class(NormalizationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (NormalizationCharacteristic)
         needs_extension = (this%metadata /= goal%metadata)
      class default
         error stop 'NormalizationCharacteristic: needs_extension_for called with a non-NormalizationCharacteristic goal - should be checked by calling procedure'
      end select
   end function normalization_needs_extension_for

   function get_metadata(this) result(metadata)
      class(NormalizationCharacteristic), intent(in) :: this
      type(NormalizationMetadata) :: metadata

      metadata = this%metadata
   end function get_metadata

end module mapl_NormalizationCharacteristic_mod
