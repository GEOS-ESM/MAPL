#include "MAPL.h"

!------------------------------------------------------------------------------
! UngriddedDimsCharacteristic: the "ungridded dims" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D3) - a
! ValueCharacteristic wrapping the existing, already-decoupled UngriddedDims
! value type (infrastructure/esmf/UngriddedDims.F90) directly: no
! StateRegistry/VariableSpec coupling, and UngriddedDims already defines
! operator(==)/operator(/=) with the same comparison semantics legacy
! UngriddedDimsAspect%matches() uses (plain equality). needs_extension_for
! is a single `/=` call - no hand-written comparison logic.
!
! No build_transform/adaptation method exists at this layer (REQ-CHAR-002);
! legacy UngriddedDimsAspect%make_transform always returns NullTransform
! (no real adaptation exists anywhere for this axis, graph-native or
! legacy).
!------------------------------------------------------------------------------
module mapl_UngriddedDimsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   use mapl_UngriddedDims_mod, only: UngriddedDims, operator(==), operator(/=)
   implicit none(type, external)
   private

   public :: UngriddedDimsCharacteristic

   type, extends(ValueCharacteristic) :: UngriddedDimsCharacteristic
      private
      type(UngriddedDims) :: ungridded_dims
   contains
      procedure, nopass :: get_kind => ungridded_dims_get_kind
      procedure :: needs_extension_for => ungridded_dims_needs_extension_for
      procedure :: get_ungridded_dims
   end type UngriddedDimsCharacteristic

   interface UngriddedDimsCharacteristic
      module procedure new_UngriddedDimsCharacteristic
   end interface UngriddedDimsCharacteristic

contains

   function new_UngriddedDimsCharacteristic(ungridded_dims) result(characteristic)
      type(UngriddedDims), intent(in) :: ungridded_dims
      type(UngriddedDimsCharacteristic) :: characteristic

      characteristic%ungridded_dims = ungridded_dims
   end function new_UngriddedDimsCharacteristic

   function ungridded_dims_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   end function ungridded_dims_get_kind

   ! `goal` is guaranteed an UngriddedDimsCharacteristic for the real call
   ! chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic).
   logical function ungridded_dims_needs_extension_for(this, goal) result(needs_extension)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (UngriddedDimsCharacteristic)
         needs_extension = (this%ungridded_dims /= goal%ungridded_dims)
      class default
         error stop 'UngriddedDimsCharacteristic: needs_extension_for called with a non-UngriddedDimsCharacteristic goal - should be checked by calling procedure'
      end select
   end function ungridded_dims_needs_extension_for

   function get_ungridded_dims(this) result(ungridded_dims)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      type(UngriddedDims) :: ungridded_dims

      ungridded_dims = this%ungridded_dims
   end function get_ungridded_dims

end module mapl_UngriddedDimsCharacteristic_mod
