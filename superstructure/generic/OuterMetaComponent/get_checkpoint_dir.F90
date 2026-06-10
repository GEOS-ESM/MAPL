#include "MAPL.h"

submodule (MAPL_OuterMetaComponent_mod) get_checkpoint_dir_smod

   use ESMF
   use mapl_utils_api, only: MAPL_PathJoin
   use mapl_ErrorHandling_mod, only: MAPL_Verify, MAPL_Return, MAPL_Assert

   implicit none(type, external)

contains

   module function get_checkpoint_dir(this, current_time, unusable, is_private, rc) result(dir)
      class(OuterMetaComponent), intent(in) :: this
      type(ESMF_Time), intent(in) :: current_time
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      logical, optional, intent(in) :: is_private
      integer, optional, intent(out) :: rc
      character(:), allocatable :: dir

      character(len=*), parameter :: CHECKPOINTING_KEY = "checkpointing"
      character(len=ESMF_MAXSTR) :: iso_time_str
      logical :: is_private_
      integer :: status

      is_private_ = .false.
      if (present(is_private)) is_private_ = is_private

      dir = get_checkpoint_path_base_(this%hconfig, _RC)
      call ESMF_TimeGet(current_time, timeStringISOFrac=iso_time_str, _RC)
      dir = MAPL_PathJoin(dir, trim(iso_time_str))
      if (is_private_) then
         dir = MAPL_PathJoin(dir, this%get_name() // "_private")
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_checkpoint_dir

   module function get_checkpoint_filename(this, current_time, state_intent, unusable, rc) result(filename)
      class(OuterMetaComponent), target, intent(in) :: this
      type(ESMF_Time), intent(in) :: current_time
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(:), allocatable :: filename

      character(len=:), allocatable :: checkpoint_dir
      type(GriddedComponentDriver), pointer :: driver
      integer :: status

      driver => this%get_user_gc_driver()
      filename = driver%get_name()

      select case (state_intent%state)
      case (ESMF_STATEINTENT_IMPORT%state)
         filename = filename // "_import.nc"
      case (ESMF_STATEINTENT_INTERNAL%state)
         filename = filename // "_internal.nc"
      case (ESMF_STATEINTENT_EXPORT%state)
         filename = filename // "_export.nc"
      case default
         _FAIL("unsupported state intent")
      end select

      checkpoint_dir = this%get_checkpoint_dir(current_time, _RC)
      filename = MAPL_PathJoin(checkpoint_dir, filename)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_checkpoint_filename

   function get_checkpoint_path_base_(hconfig, rc) result(path_base)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      character(:), allocatable :: path_base

      character(len=*), parameter :: CHECKPOINTING_KEY = "checkpointing"
      character(len=*), parameter :: PATH_KEY = "path"
      type(ESMF_HConfig) :: checkpointing_cfg
      logical :: has_checkpointing_section, has_path
      integer :: status

      path_base = "checkpoint" ! default base name
      has_checkpointing_section = ESMF_HConfigIsDefined(hconfig, keystring=CHECKPOINTING_KEY, _RC)
      if (has_checkpointing_section) then
         checkpointing_cfg = ESMF_HConfigCreateAt(hconfig, keystring=CHECKPOINTING_KEY, _RC)
         has_path = ESMF_HConfigIsDefined(checkpointing_cfg, keystring=PATH_KEY, _RC)
         if (has_path) then
            path_base = ESMF_HConfigAsString(checkpointing_cfg, keystring=PATH_KEY, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end function get_checkpoint_path_base_

end submodule get_checkpoint_dir_smod
