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
! A `build_transform` is required by the common interface but fails
! explicitly here - no real provider exists for this axis
! (openspec/changes/merge-characteristic-families): legacy
! UngriddedDimsAspect%make_transform always returns NullTransform (no
! real adaptation exists anywhere for this axis, graph-native or
! legacy). `get_signature` derives a stable string from
! `ungridded_dims`'s own per-dimension name/extent pairs.
!------------------------------------------------------------------------------
module mapl_UngriddedDimsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   use mapl_UngriddedDims_mod, only: UngriddedDims, operator(==), operator(/=)
   use mapl_UngriddedDim_mod, only: UngriddedDim
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: UngriddedDimsCharacteristic

   type, extends(ValueCharacteristic) :: UngriddedDimsCharacteristic
      private
      type(UngriddedDims) :: ungridded_dims
   contains
      procedure, nopass :: get_kind => ungridded_dims_get_kind
      procedure :: needs_extension_for => ungridded_dims_needs_extension_for
      procedure :: build_transform => ungridded_dims_build_transform
      procedure :: get_signature => ungridded_dims_get_signature
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

   ! merge-characteristic-families task 3.6: no real adaptation exists
   ! for this axis, graph-native or legacy (module header) - fails
   ! explicitly.
   subroutine ungridded_dims_build_transform(this, goal, transformer, rc)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      _FAIL('UngriddedDimsCharacteristic: build_transform - no real provider exists for this axis')
   end subroutine ungridded_dims_build_transform

   ! merge-characteristic-families task 3.6: a stable, value-
   ! distinguishing signature derived from this subclass's own stored
   ! value - each dimension's own name/extent, in declaration order
   ! (design.md Risks: a small, mechanical derivation).
   function ungridded_dims_get_signature(this) result(signature)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      integer :: i, n
      type(UngriddedDim), pointer :: dim_spec
      character(32) :: extent_buffer

      signature = 'ungridded_dims='
      n = this%ungridded_dims%get_num_ungridded()
      do i = 1, n
         dim_spec => this%ungridded_dims%get_ith_dim_spec(i)
         write(extent_buffer, '(I0)') dim_spec%get_extent()
         if (i > 1) signature = signature // ','
         signature = signature // dim_spec%get_name() // ':' // trim(adjustl(extent_buffer))
      end do
   end function ungridded_dims_get_signature

end module mapl_UngriddedDimsCharacteristic_mod
