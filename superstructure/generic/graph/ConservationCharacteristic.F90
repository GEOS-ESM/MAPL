#include "MAPL.h"

!------------------------------------------------------------------------------
! ConservationCharacteristic: the "conservation" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D3) - a
! ValueCharacteristic wrapping the existing, already-decoupled
! ConservationMetadata value type (enums/ConservationMetadata.F90) directly:
! no StateRegistry coupling, and ConservationMetadata already defines
! operator(==)/operator(/=) with its own mirror-aware semantics (both-mirror
! compares equal unconditionally) matching legacy ConservationAspect%matches().
! needs_extension_for is a single `/=` call - no hand-written comparison
! logic.
!
! A `build_transform` is required by the common interface but fails
! explicitly here - no real provider exists for this axis
! (openspec/changes/merge-characteristic-families): legacy
! ConservationAspect%make_transform unconditionally _FAILs ("should
! not be called") - this axis is detected but never adapted, graph-native
! or legacy. `get_signature` derives a stable string from `metadata`'s
! own `conservation_type%to_string()`.
!------------------------------------------------------------------------------
module mapl_ConservationCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, CONSERVATION_CHARACTERISTIC_KIND
   use mapl_ConservationMetadata_mod, only: ConservationMetadata, operator(==), operator(/=)
   use mapl_ConservationType_mod, only: ConservationType
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ConservationCharacteristic

   type, extends(ValueCharacteristic) :: ConservationCharacteristic
      private
      type(ConservationMetadata) :: metadata
   contains
      procedure, nopass :: get_kind => conservation_get_kind
      procedure :: needs_extension_for => conservation_needs_extension_for
      procedure :: build_transform => conservation_build_transform
      procedure :: get_signature => conservation_get_signature
      procedure :: get_metadata
   end type ConservationCharacteristic

   interface ConservationCharacteristic
      module procedure new_ConservationCharacteristic
   end interface ConservationCharacteristic

contains

   function new_ConservationCharacteristic(metadata) result(characteristic)
      type(ConservationMetadata), intent(in) :: metadata
      type(ConservationCharacteristic) :: characteristic

      characteristic%metadata = metadata
   end function new_ConservationCharacteristic

   function conservation_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = CONSERVATION_CHARACTERISTIC_KIND
   end function conservation_get_kind

   ! `goal` is guaranteed a ConservationCharacteristic for the real call
   ! chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic).
   logical function conservation_needs_extension_for(this, goal) result(needs_extension)
      class(ConservationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (ConservationCharacteristic)
         needs_extension = (this%metadata /= goal%metadata)
      class default
         error stop 'ConservationCharacteristic: needs_extension_for called with a non-ConservationCharacteristic goal - should be checked by calling procedure'
      end select
   end function conservation_needs_extension_for

   function get_metadata(this) result(metadata)
      class(ConservationCharacteristic), intent(in) :: this
      type(ConservationMetadata) :: metadata

      metadata = this%metadata
   end function get_metadata

   ! merge-characteristic-families task 3.2: no real adaptation exists
   ! for this axis, graph-native or legacy (module header) - fails
   ! explicitly.
   subroutine conservation_build_transform(this, goal, transformer, rc)
      class(ConservationCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      _FAIL('ConservationCharacteristic: build_transform - no real provider exists for this axis')
   end subroutine conservation_build_transform

   ! merge-characteristic-families task 3.2: a stable, value-
   ! distinguishing signature derived from this subclass's own stored
   ! value (design.md Risks: a small, mechanical derivation).
   function conservation_get_signature(this) result(signature)
      class(ConservationCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      type(ConservationType) :: ctype

      if (this%metadata%is_mirror()) then
         signature = 'conservation=<mirror>'
      else
         ctype = this%metadata%get_conservation_type()
         signature = 'conservation=' // ctype%to_string()
         if (this%metadata%get_is_conservable()) then
            signature = signature // ';conservable'
         end if
      end if
   end function conservation_get_signature

end module mapl_ConservationCharacteristic_mod
