#include "MAPL.h"

!------------------------------------------------------------------------------
! RestartModeCharacteristic: the "restart mode" StateItemCharacteristic, a
! ValueCharacteristic holding a RestartMode. It is descriptive only and
! never compared: needs_extension_for is always false, so a connection
! never needs a step for it. The mode is per-alias metadata, so each item
! (import or export) writes its own and an import never adopts its
! export's.
!
! No Transform exists for this axis: build_transform reports unsupported.
!------------------------------------------------------------------------------
module mapl_RestartModeCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, &
        ValueCharacteristic, report_build_transform_unsupported
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, RESTART_MODE_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_RestartModes_mod, only: RestartMode, RESTART_REQUIRED
   use mapl_field_api, only: MAPL_FieldSet
   use ESMF, only: ESMF_Field, ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, operator(==)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: RestartModeCharacteristic

   type, extends(ValueCharacteristic) :: RestartModeCharacteristic
      private
      type(RestartMode), allocatable :: mode
   contains
      procedure, nopass :: get_kind => restart_mode_get_kind
      procedure :: needs_extension_for => restart_mode_needs_extension_for
      procedure :: build_transform => restart_mode_build_transform
      procedure :: get_signature => restart_mode_get_signature
      procedure :: write_info => restart_mode_write_info
      procedure :: is_per_alias => restart_mode_is_per_alias
      procedure :: get_restart_mode
   end type RestartModeCharacteristic

   interface RestartModeCharacteristic
      module procedure new_RestartModeCharacteristic
   end interface RestartModeCharacteristic

contains

   function new_RestartModeCharacteristic(mode) result(characteristic)
      type(RestartMode), intent(in) :: mode
      type(RestartModeCharacteristic) :: characteristic

      characteristic%mode = mode
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_RestartModeCharacteristic

   function restart_mode_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = RESTART_MODE_CHARACTERISTIC_KIND
   end function restart_mode_get_kind

   ! Descriptive metadata: a different mode never needs a step.
   logical function restart_mode_needs_extension_for(this, goal) result(needs)
      class(RestartModeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)
      needs = .false.
   end function restart_mode_needs_extension_for

   ! A valueless instance returns the default, RESTART_REQUIRED.
   function get_restart_mode(this) result(mode)
      class(RestartModeCharacteristic), intent(in) :: this
      type(RestartMode) :: mode

      mode = RESTART_REQUIRED
      if (allocated(this%mode)) mode = this%mode
   end function get_restart_mode

   subroutine restart_mode_build_transform(this, goal, transformer, rc)
      class(RestartModeCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine restart_mode_build_transform

   ! Valid in either phase: it is not a creation input and not a FieldDelta
   ! trigger. Only a Field carries a restart mode; the bundle setter has no
   ! such keyword.
   subroutine restart_mode_write_info(this, handle, phase, rc)
      class(RestartModeCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field

      _UNUSED_DUMMY(phase)

      item_type = handle%itemType(_RC)
      _ASSERT(item_type == ESMF_STATEITEM_FIELD, 'RestartModeCharacteristic: write_info needs a Field')
      _RETURN_UNLESS(allocated(this%mode))
      field = handle%get_field(_RC)
      call MAPL_FieldSet(field, restart_mode=this%mode, _RC)

      _RETURN(_SUCCESS)
   end subroutine restart_mode_write_info

   logical function restart_mode_is_per_alias(this) result(per_alias)
      class(RestartModeCharacteristic), intent(in) :: this

      _UNUSED_DUMMY(this)
      per_alias = .true.
   end function restart_mode_is_per_alias

   function restart_mode_get_signature(this) result(signature)
      class(RestartModeCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      character(len=16) :: buffer
      type(RestartMode) :: mode

      mode = this%get_restart_mode()
      write(buffer, '(i0)') mode%get_mode()
      signature = 'restart_mode=' // trim(buffer)
   end function restart_mode_get_signature

end module mapl_RestartModeCharacteristic_mod
