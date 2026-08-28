#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) write_restart_smod

   use mapl_MultiState_mod
   use mapl_RestartHandler_mod
   use mapl_os_mod
   use mapl_ErrorHandling_mod
   use mapl_GenericPhases_mod, only: GENERIC_INTERNAL_WRITE_RESTART
   use mapl_field_bundle_api, only: MAPL_FieldBundleClone, MAPL_FieldBundleCopy

   implicit none(type,external)

contains

   module recursive subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      character(*), parameter :: PHASE_NAME = 'GENERIC::WRITE_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      integer :: status
      integer :: current_phase
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(ESMF_Time) :: current_time

      call ESMF_GridCompGet(this%get_gridcomp(), currentPhase=current_phase, _RC)

      if (current_phase == GENERIC_INTERNAL_WRITE_RESTART) then
         call write_memory_checkpoint_(this, _RC)
         _RETURN(ESMF_SUCCESS)
      end if

      call recurse_write_restart_(this, _RC)
      call this%run_custom(ESMF_METHOD_WRITERESTART, PHASE_NAME, _RC)

      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      call ESMF_ClockGet(driver%get_clock(), currTime=current_time, _RC)
      restart_handler = RestartHandler(this%get_geom(), current_time, this%get_logger())
      states = driver%get_states()

      if (this%component_spec%misc%checkpoint_controls%get_import()) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_IMPORT, _RC)
         call this%start_timer("WriteImportCheckpoint", _RC)
         call restart_handler%write(states%importState, filename, _RC)
         call this%stop_timer("WriteImportCheckpoint", _RC)
      end if

      if (this%component_spec%misc%checkpoint_controls%get_internal()) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_INTERNAL, _RC)
         call this%start_timer("WriteInternalCheckpoint", _RC)
         call restart_handler%write(states%internalState, filename, _RC)
         call this%stop_timer("WriteInternalCheckpoint", _RC)
      end if

      if (this%component_spec%misc%checkpoint_controls%get_export()) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_EXPORT, _RC)
         call this%start_timer("WriteExportCheckpoint", _RC)
         call restart_handler%write(states%exportState, filename, _RC)
         call this%stop_timer("WriteExportCheckpoint", _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine write_restart

    ! In-memory checkpoint write: first save clones each enabled state's
    ! restart-eligible fields into corresponding nested state under
    ! this%memory_checkpoint. Later saves only copy field data into that
    ! fixed structure.
    subroutine write_memory_checkpoint_(this, rc)
       class(OuterMetaComponent), target, intent(inout) :: this
       integer, optional, intent(out) :: rc

       integer :: status
       type(GriddedComponentDriver), pointer :: driver
       type(MultiState) :: states
       type(ESMF_Info) :: checkpoint_info
       logical :: wrote_any

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      call this%ensure_memory_checkpoint_(_RC)
      wrote_any = .false.

      if (this%component_spec%misc%checkpoint_controls%get_import()) then
         call save_state_into_checkpoint_(this, states%importState, ESMF_STATEINTENT_IMPORT, _RC)
         wrote_any = .true.
      end if

      if (this%component_spec%misc%checkpoint_controls%get_internal()) then
         call save_state_into_checkpoint_(this, states%internalState, ESMF_STATEINTENT_INTERNAL, _RC)
         wrote_any = .true.
      end if

      if (wrote_any) then
         call ESMF_InfoGetFromHost(this%memory_checkpoint, checkpoint_info, _RC)
         call ESMF_InfoSet(checkpoint_info, key=MEMORY_CHECKPOINT_INFO_KEY, value=.true., _RC)
      end if

      _RETURN(ESMF_SUCCESS)
    end subroutine write_memory_checkpoint_

    ! Build restart-eligible bundle for live_state. First save for given
    ! state clones bundle structure into checkpoint state. Later saves
    ! reuse stored fields and only copy data values.
    subroutine save_state_into_checkpoint_(this, live_state, state_intent, rc)
       class(OuterMetaComponent), target, intent(inout) :: this
       type(ESMF_State), intent(in) :: live_state
       type(ESMF_StateIntent_Flag), intent(in) :: state_intent
       integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_FieldBundle) :: eligible_bundle, checkpoint_bundle
       type(ESMF_State) :: checkpoint_state
       type(ESMF_FieldBundle) :: cloned_bundle
       type(ESMF_Field), allocatable :: cloned_fields(:)
       integer :: checkpoint_item_count, field_count

       call get_restart_bundle(live_state, is_write=.true., bundle=eligible_bundle, _RC)

       call this%get_memory_checkpoint_state_(state_intent, checkpoint_state, _RC)
       call ESMF_StateGet(checkpoint_state, itemCount=checkpoint_item_count, _RC)

       if (checkpoint_item_count == 0) then
          call MAPL_FieldBundleClone(eligible_bundle, cloned_bundle, _RC)
          ! FieldClone only allocates destination fields. Copy live data
          ! after first-time clone so checkpoint stores current values.
          call MAPL_FieldBundleCopy(eligible_bundle, cloned_bundle, _RC)

          call ESMF_FieldBundleGet(cloned_bundle, fieldCount=field_count, _RC)
          if (field_count > 0) then
             allocate(cloned_fields(field_count), _STAT)
             call ESMF_FieldBundleGet(cloned_bundle, itemorderflag=ESMF_ITEMORDER_ABC, fieldList=cloned_fields, _RC)
             call ESMF_StateAdd(checkpoint_state, cloned_fields, _RC)
          end if
          call ESMF_FieldBundleDestroy(cloned_bundle, _RC)
       else
          call get_restart_bundle(checkpoint_state, is_write=.true., bundle=checkpoint_bundle, _RC)
          call MAPL_FieldBundleCopy(eligible_bundle, checkpoint_bundle, _RC)
          call ESMF_FieldBundleDestroy(checkpoint_bundle, _RC)
       end if

       call ESMF_FieldBundleDestroy(eligible_bundle, _RC)

       _RETURN(ESMF_SUCCESS)
    end subroutine save_state_into_checkpoint_

end submodule write_restart_smod
