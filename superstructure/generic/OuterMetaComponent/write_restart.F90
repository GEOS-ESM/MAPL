#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) write_restart_smod

   use mapl_MultiState_mod
   use mapl_RestartHandler_mod
   use mapl_os_mod
   use mapl_ErrorHandling_mod
   use mapl_GenericPhases_mod, only: GENERIC_INTERNAL_WRITE_RESTART
   use mapl_field_bundle_api, only: MAPL_FieldBundleClone, MAPL_FieldBundleCopy
   use mapl_FieldUtils, only: FieldsDestroy

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

   ! In-memory checkpoint write: for each of import/export/internal
   ! enabled via checkpoint_controls, deep-clone the state's
   ! restart-eligible fields (independent allocation, no data copy
   ! aliasing) into the corresponding nested state under
   ! this%memory_checkpoint, replacing any previously stored snapshot
   ! for that state.
   subroutine write_memory_checkpoint_(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      logical :: wrote_any

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      call this%ensure_memory_checkpoint_(_RC)
      wrote_any = .false.

      if (this%component_spec%misc%checkpoint_controls%get_import()) then
         call clone_state_into_checkpoint_(this, states%importState, ESMF_STATEINTENT_IMPORT, _RC)
         wrote_any = .true.
      end if

      if (this%component_spec%misc%checkpoint_controls%get_export()) then
         call clone_state_into_checkpoint_(this, states%exportState, ESMF_STATEINTENT_EXPORT, _RC)
         wrote_any = .true.
      end if

      if (this%component_spec%misc%checkpoint_controls%get_internal()) then
         call clone_state_into_checkpoint_(this, states%internalState, ESMF_STATEINTENT_INTERNAL, _RC)
         wrote_any = .true.
      end if

      if (wrote_any) this%has_memory_checkpoint = .true.

      _RETURN(ESMF_SUCCESS)
   end subroutine write_memory_checkpoint_

   ! Build the restart-eligible bundle for live_state, deep-clone it
   ! (independent allocation via FieldClone, structure only), copy the
   ! live data values into the clone, and store the clone's fields
   ! into the memory_checkpoint substate identified by state_intent,
   ! replacing any previous contents.
   subroutine clone_state_into_checkpoint_(this, live_state, state_intent, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State), intent(in) :: live_state
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldBundle) :: eligible_bundle, cloned_bundle
      type(ESMF_State) :: checkpoint_state
      type(ESMF_Field), allocatable :: cloned_fields(:)
      integer :: field_count

      call get_restart_bundle(live_state, is_write=.true., bundle=eligible_bundle, _RC)
      call MAPL_FieldBundleClone(eligible_bundle, cloned_bundle, _RC)
      ! FieldClone (used by MAPL_FieldBundleClone) only clones field
      ! structure/metadata into freshly-allocated memory; it does NOT
      ! copy data values. Populate the clone's data explicitly so the
      ! stored checkpoint reflects the live state's current values.
      call MAPL_FieldBundleCopy(eligible_bundle, cloned_bundle, _RC)
      call ESMF_FieldBundleDestroy(eligible_bundle, _RC)

      call this%get_memory_checkpoint_state_(state_intent, checkpoint_state, _RC)
      call clear_checkpoint_state_(checkpoint_state, _RC)

      call ESMF_FieldBundleGet(cloned_bundle, fieldCount=field_count, _RC)
      if (field_count > 0) then
         allocate(cloned_fields(field_count), _STAT)
         call ESMF_FieldBundleGet(cloned_bundle, itemorderflag=ESMF_ITEMORDER_ABC, fieldList=cloned_fields, _RC)
         call ESMF_StateAdd(checkpoint_state, cloned_fields, _RC)
      end if
      call ESMF_FieldBundleDestroy(cloned_bundle, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine clone_state_into_checkpoint_

   ! Remove and destroy any fields currently held directly in
   ! checkpoint_state (i.e. the prior in-memory checkpoint snapshot
   ! for this state, if any).
   subroutine clear_checkpoint_state_(checkpoint_state, rc)
      type(ESMF_State), intent(inout) :: checkpoint_state
      integer, optional, intent(out) :: rc

      integer :: status, item_count
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_Field), allocatable :: old_fields(:)
      integer :: idx

      call ESMF_StateGet(checkpoint_state, itemCount=item_count, _RC)
      if (item_count == 0) then
         _RETURN(ESMF_SUCCESS)
      end if

      allocate(item_names(item_count), _STAT)
      allocate(old_fields(item_count), _STAT)
      call ESMF_StateGet(checkpoint_state, itemNameList=item_names, _RC)
      do idx = 1, item_count
         call ESMF_StateGet(checkpoint_state, item_names(idx), old_fields(idx), _RC)
      end do
      call ESMF_StateRemove(checkpoint_state, itemNameList=item_names, _RC)
      call FieldsDestroy(old_fields, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine clear_checkpoint_state_

end submodule write_restart_smod
