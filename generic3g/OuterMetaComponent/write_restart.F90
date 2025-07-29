#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) write_restart_smod
   use mapl3g_MultiState
   use mapl3g_RestartHandler
   use mapl_OS
   use mapl_ErrorHandling
   implicit none (type, external)

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
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      integer :: status
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(esmf_Time) :: currTime

      call recurse_write_restart_(this, _RC)
      _RETURN_UNLESS(this%has_geom())
      
      driver => this%get_user_gc_driver()
      call esmf_ClockGet(driver%get_clock(), currTime=currTime, _RC)

      restart_handler = RestartHandler( &
           this%get_geom(), &
           currTime, &
           this%get_logger())

      states = driver%get_states()
      subdir = get_checkpoint_subdir(this%hconfig, currTime, _RC)

      if (this%component_spec%misc%checkpoint_controls%import) then
         filename = mapl_PathJoin(subdir, this%get_name() // '_import.nc')
         call restart_handler%write(states%importState, filename, _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%internal) then
         filename = mapl_PathJoin(subdir, this%get_name() // '_internal.nc')
         call restart_handler%write(states%internalState, filename, _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%export) then
         filename = mapl_PathJoin(subdir, this%get_name() // '_export.nc')
         call restart_handler%write(states%exportState, filename, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine write_restart

   module function get_checkpoint_subdir(hconfig, currTime, rc) result(subdir)
      character(:), allocatable :: subdir
      type(esmf_HConfig), intent(in) :: hconfig
      type(esmf_Time), intent(in) :: currTime
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(ESMF_MAXSTR) :: iso_time
      logical :: has_checkpointing, has_path
      character(:), allocatable :: checkpoint_dir
      character(:), allocatable :: timestamp_dir
      type(esmf_HConfig) :: checkpointing_cfg

      call esmf_TimeGet(currTime, timeStringISOFrac=iso_time, _RC)
      timestamp_dir = trim(iso_time)
      
      checkpoint_dir = 'checkpoint'
      has_checkpointing = esmf_HConfigIsDefined(hconfig, keystring='checkpointing', _RC)
      if (has_checkpointing) then
         checkpointing_cfg = esmf_HConfigCreateAt(hconfig, keystring='checkpointing', _RC)
         has_path = esmf_HConfigIsDefined(checkpointing_cfg, keystring='path', _RC)
         if (has_path) then
            checkpoint_dir = esmf_HConfigAsString(checkpointing_cfg, keystring='path', _RC)
         end if
         call esmf_HConfigDestroy(checkpointing_cfg, _RC)
      end if
      
      subdir = mapl_PathJoin(checkpoint_dir, iso_time)
      
      _RETURN(_SUCCESS)
   end function get_checkpoint_subdir
   

end submodule write_restart_smod
