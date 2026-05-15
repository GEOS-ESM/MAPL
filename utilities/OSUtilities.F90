#include "MAPL.h"

module mapl3g_OSUtilities

   use ESMF
   use mapl_OS
   use mapl_ErrorHandlingMod
   implicit none
   private

   public get_checkpoint_subdir

contains

   function get_checkpoint_subdir(hconfig, currTime, rc) result(subdir)
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

      subdir = ''

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

end module mapl3g_OSUtilities
