#include "MAPL.h"

!------------------------------------------------------------------------------
! NormalizationCharacteristic: the "normalization" StateItemCharacteristic
! ValueCharacteristic wrapping the existing, already-decoupled
! NormalizationMetadata value type (enums/NormalizationMetadata.F90)
! directly: no StateRegistry coupling, and NormalizationMetadata already
! defines operator(==)/operator(/=) with its own mirror-aware semantics
! (both-mirror compares equal unconditionally) matching legacy
! NormalizationAspect%matches(). needs_extension_for is a single `/=` call -
! no hand-written comparison logic.
!
! A `build_transform` is required by the common interface but reports
! unsupported here - no real provider exists for this axis. Legacy
! NormalizationAspect%make_transform always returns NullTransform
! (no real adaptation exists anywhere for this axis, graph-native or
! legacy). `get_signature` derives a stable string from `metadata`'s
! own `normalization_type%to_string()`/`normalization_scale`.
!------------------------------------------------------------------------------
module mapl_NormalizationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_unsupported
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, NORMALIZATION_CHARACTERISTIC_KIND
   use mapl_NormalizationMetadata_mod, only: NormalizationMetadata, operator(==), operator(/=)
   use mapl_NormalizationType_mod, only: NormalizationType
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: NormalizationCharacteristic

   type, extends(ValueCharacteristic) :: NormalizationCharacteristic
      private
      type(NormalizationMetadata) :: metadata
   contains
      procedure, nopass :: get_kind => normalization_get_kind
      procedure :: needs_extension_for => normalization_needs_extension_for
      procedure :: build_transform => normalization_build_transform
      procedure :: get_signature => normalization_get_signature
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
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
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

   ! No real adaptation exists for this axis (module header): reports
   ! unsupported.
   subroutine normalization_build_transform(this, goal, transformer, rc)
      class(NormalizationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine normalization_build_transform

   ! merge-characteristic-families task 3.3: a stable, value-
   ! distinguishing signature derived from this subclass's own stored
   ! value (design.md Risks: a small, mechanical derivation).
   function normalization_get_signature(this) result(signature)
      class(NormalizationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      character(32) :: scale_buffer
      type(NormalizationType) :: ntype

      if (this%metadata%is_mirror()) then
         signature = 'normalization=<mirror>'
      else
         write(scale_buffer, '(ES12.5)') this%metadata%get_normalization_scale()
         ntype = this%metadata%get_normalization_type()
         signature = 'normalization=' // ntype%to_string() // &
              ';scale=' // trim(adjustl(scale_buffer))
      end if
   end function normalization_get_signature

end module mapl_NormalizationCharacteristic_mod
