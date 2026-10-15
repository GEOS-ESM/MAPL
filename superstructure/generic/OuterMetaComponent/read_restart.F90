#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) read_restart_smod

   use mapl_ErrorHandling_mod
   use mapl_GenericPhases_mod, only: GENERIC_INTERNAL_READ_RESTART
   use mapl_MultiState_mod
   use mapl_RestartHandler_mod, only: get_restart_bundle
   use mapl_field_bundle_api, only: MAPL_FieldBundleCopy

   implicit none(type,external)

contains

   ! Dedicated ESMF_METHOD_READRESTART dispatch for the internal
   ! (in-memory) checkpoint phase.  Existing netCDF restart reads
   ! continue to run under ESMF_METHOD_INITIALIZE phase
   ! GENERIC_INIT_READ_RESTART via initialize_read_restart, which this
   ! procedure does not touch or replace.
   module recursive subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: current_phase

      call ESMF_GridCompGet(this%get_gridcomp(), currentPhase=current_phase, _RC)

      select case (current_phase)
      case (GENERIC_INTERNAL_READ_RESTART)
         call read_memory_checkpoint_(this, _RC)
      case default
         _FAIL('Unknown internal read restart phase.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine read_restart

    ! In-memory checkpoint read: for each enabled import/internal state,
    ! copy field data values from the
   ! stored memory_checkpoint snapshot back into the live state's
   ! restart-eligible fields (data only - live field objects are not
   ! reallocated or replaced). Fails if no snapshot was ever stored
   ! for a requested state.
   subroutine read_memory_checkpoint_(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states

      driver => this%get_user_gc_driver()
      states = driver%get_states()

      if (this%component_spec%misc%restart_controls%get_import()) then
         call copy_checkpoint_into_state_(this, states%importState, ESMF_STATEINTENT_IMPORT, _RC)
      end if

      if (this%component_spec%misc%restart_controls%get_internal()) then
         call copy_checkpoint_into_state_(this, states%internalState, ESMF_STATEINTENT_INTERNAL, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine read_memory_checkpoint_

   ! Copy field data from the memory_checkpoint substate identified by
   ! state_intent into the corresponding fields of live_state. Fails
   ! if no snapshot has ever been written.
   !
   ! Both the live and checkpoint bundles are built with the same
   ! (is_write=.true., completeness-only) field-selection criteria
   ! used when the checkpoint was originally written: checkpoint
   ! fields are plain FieldClone products with no NamedAlias restart-
   ! mode metadata, so the RESTART_SKIP-aware filter (is_write=.false.)
   ! is not applicable here and would fail on them; using the same
   ! selection on both sides also keeps the two bundles' field counts
   ! and names aligned, as required by MAPL_FieldBundleCopy.
   subroutine copy_checkpoint_into_state_(this, live_state, state_intent, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State), intent(inout) :: live_state
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_State) :: checkpoint_state
       type(ESMF_Info) :: checkpoint_info
       type(ESMF_FieldBundle) :: live_bundle, checkpoint_bundle
       integer :: checkpoint_item_count
       logical :: has_memory_checkpoint

       has_memory_checkpoint = .false.
       _ASSERT(ESMF_StateIsCreated(this%memory_checkpoint), 'In-memory checkpoint read requested but no in-memory checkpoint write has occurred.')
       call ESMF_InfoGetFromHost(this%memory_checkpoint, checkpoint_info, _RC)
       call ESMF_InfoGet(checkpoint_info, key=MEMORY_CHECKPOINT_INFO_KEY, value=has_memory_checkpoint, rc=status)
       if (status /= ESMF_SUCCESS) has_memory_checkpoint = .false.
       _ASSERT(has_memory_checkpoint, 'In-memory checkpoint read requested but no in-memory checkpoint write has occurred.')

       call this%get_memory_checkpoint_state_(state_intent, checkpoint_state, _RC)
       call ESMF_StateGet(checkpoint_state, itemCount=checkpoint_item_count, _RC)
      _ASSERT(checkpoint_item_count > 0, 'In-memory checkpoint read requested for a state with no stored snapshot.')

      call get_restart_bundle(live_state, is_write=.true., bundle=live_bundle, _RC)
      call get_restart_bundle(checkpoint_state, is_write=.true., bundle=checkpoint_bundle, _RC)

      call MAPL_FieldBundleCopy(checkpoint_bundle, live_bundle, _RC)

      call ESMF_FieldBundleDestroy(live_bundle, _RC)
      call ESMF_FieldBundleDestroy(checkpoint_bundle, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine copy_checkpoint_into_state_

end submodule read_restart_smod
