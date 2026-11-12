#include "MAPL.h"

!------------------------------------------------------------------------------
! UngriddedDimsCharacteristic: the "ungridded dims" StateItemCharacteristic
! ValueCharacteristic wrapping the existing, already-decoupled UngriddedDims
! value type (infrastructure/esmf/UngriddedDims.F90) directly: no
! StateRegistry/VariableSpec coupling, and UngriddedDims already defines
! operator(==)/operator(/=) with the same comparison semantics legacy
! UngriddedDimsAspect%matches() uses (plain equality). needs_extension_for
! is a single `/=` call - no hand-written comparison logic.
!
! A `build_transform` is required by the common interface but reports
! incompatible here: legacy UngriddedDimsAspect never converts a mismatch
! (supports_conversion_* are false), so no Transform can ever exist.
! `get_signature` derives a stable string from
! `ungridded_dims`'s own per-dimension name/extent pairs.
!
! A MIRRORED instance holds no value (get_ungridded_dims returns an empty
! UngriddedDims). UngriddedDims(is_mirror=.true.) is not a value to compare:
! the VariableSpec factory turns it into MIRRORED status.
!------------------------------------------------------------------------------
module mapl_UngriddedDimsCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_incompatible
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   use mapl_UngriddedDims_mod, only: UngriddedDims, operator(==), operator(/=)
   use mapl_UngriddedDim_mod, only: UngriddedDim
   use mapl_TransformBase_mod, only: TransformBase
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==)
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
      procedure :: write_info => ungridded_dims_write_info
      procedure :: is_structural => ungridded_dims_is_structural
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
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
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

   ! A mismatch can never be adapted (module header): reports incompatible.
   subroutine ungridded_dims_build_transform(this, goal, transformer, rc)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine ungridded_dims_build_transform

   ! ungridded_dims= is a creation input: on a completed field the array shape
   ! is already fixed, so it is written only in the STRUCTURAL phase. UNCHECKED
   ! has no Info representation for this key.
   subroutine ungridded_dims_write_info(this, handle, phase, rc)
      class(UngriddedDimsCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _RETURN_UNLESS(phase == SYNC_PHASE_STRUCTURAL)
      _RETURN_UNLESS(this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, ungridded_dims=this%ungridded_dims, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, ungridded_dims=this%ungridded_dims, _RC)
      else
         _FAIL('UngriddedDimsCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine ungridded_dims_write_info

   logical function ungridded_dims_is_structural(this) result(structural)
      class(UngriddedDimsCharacteristic), intent(in) :: this

      structural = .true.
      _UNUSED_DUMMY(this)
   end function ungridded_dims_is_structural

   ! A stable, value-distinguishing signature: each dimension's own
   ! name/extent, in declaration order.
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
