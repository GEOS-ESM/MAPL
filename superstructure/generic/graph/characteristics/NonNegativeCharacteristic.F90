#include "MAPL.h"

!------------------------------------------------------------------------------
! NonNegativeCharacteristic: the "non-negative" StateItemCharacteristic, a
! ValueCharacteristic holding the fill method an owner declares for a field
! that must stay non-negative (only 'column' exists). It is descriptive only
! and never compared: needs_extension_for is always false, so a connection
! never needs a step for it. The declaration is per-alias metadata of the
! owner, so an import never adopts its export's.
!
! No Transform exists for this axis: build_transform reports unsupported.
! The fill itself is ConservativeColumnFillTransform, placed on the put path
! of a callback argument, not built from this characteristic.
!------------------------------------------------------------------------------
module mapl_NonNegativeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, &
        ValueCharacteristic, report_build_transform_unsupported
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, NON_NEGATIVE_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: NonNegativeCharacteristic
   public :: NON_NEGATIVE_FILL_COLUMN

   character(*), parameter :: NON_NEGATIVE_FILL_COLUMN = 'column'

   type, extends(ValueCharacteristic) :: NonNegativeCharacteristic
      private
      character(:), allocatable :: fill
   contains
      procedure, nopass :: get_kind => non_negative_get_kind
      procedure :: needs_extension_for => non_negative_needs_extension_for
      procedure :: build_transform => non_negative_build_transform
      procedure :: get_signature => non_negative_get_signature
      procedure :: write_info => non_negative_write_info
      procedure :: is_per_alias => non_negative_is_per_alias
      procedure :: get_fill
   end type NonNegativeCharacteristic

   interface NonNegativeCharacteristic
      module procedure new_NonNegativeCharacteristic
   end interface NonNegativeCharacteristic

contains

   function new_NonNegativeCharacteristic(fill) result(characteristic)
      character(*), intent(in) :: fill
      type(NonNegativeCharacteristic) :: characteristic

      characteristic%fill = fill
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_NonNegativeCharacteristic

   function non_negative_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = NON_NEGATIVE_CHARACTERISTIC_KIND
   end function non_negative_get_kind

   ! Descriptive metadata: a different declaration never needs a step.
   logical function non_negative_needs_extension_for(this, goal) result(needs)
      class(NonNegativeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)
      needs = .false.
   end function non_negative_needs_extension_for

   ! A valueless instance returns ''.
   function get_fill(this) result(fill)
      class(NonNegativeCharacteristic), intent(in) :: this
      character(:), allocatable :: fill

      fill = ''
      if (allocated(this%fill)) fill = this%fill
   end function get_fill

   subroutine non_negative_build_transform(this, goal, transformer, rc)
      class(NonNegativeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine non_negative_build_transform

   ! The graph reads the declaration from the descriptor; there is no Info
   ! key for it, so nothing is written.
   subroutine non_negative_write_info(this, handle, phase, rc)
      class(NonNegativeCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(handle)
      _UNUSED_DUMMY(phase)
      _RETURN(_SUCCESS)
   end subroutine non_negative_write_info

   logical function non_negative_is_per_alias(this) result(per_alias)
      class(NonNegativeCharacteristic), intent(in) :: this

      _UNUSED_DUMMY(this)
      per_alias = .true.
   end function non_negative_is_per_alias

   function non_negative_get_signature(this) result(signature)
      class(NonNegativeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'non_negative=' // this%get_fill()
   end function non_negative_get_signature

end module mapl_NonNegativeCharacteristic_mod
