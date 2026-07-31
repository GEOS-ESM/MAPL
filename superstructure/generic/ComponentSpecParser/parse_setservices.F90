#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(config, rc) result(user_ss)
      type(DSOSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR), parameter :: dso_keys(*) = [character(len=ESMF_MAXSTR) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
      character(len=ESMF_MAXSTR), parameter :: userProcedure_keys(*) = [character(len=ESMF_MAXSTR) :: 'SetServices', 'setServices', 'setservices', 'userRoutine', 'userProcedure']

      character(:), allocatable :: sharedObj, userRoutine, try_key
      type(ESMF_HConfig) :: mapl_cfg, dso_cfg
      integer :: i, status
      logical :: has_mapl, has_dso, dso_found, userProcedure_found

      character(len=ESMF_MAXSTR), parameter :: ss_dict_keys(*) = [character(len=ESMF_MAXSTR) :: 'setServices', 'SetServices', 'setservices', 'dso', 'DSO']
      character(len=ESMF_MAXSTR), parameter :: shared_obj_keys(*) = [character(len=ESMF_MAXSTR) :: 'sharedObj', 'sharedobj', 'SHAREDOBJ', 'library', 'dso', 'DSO']
      character(len=ESMF_MAXSTR), parameter :: user_routine_keys(*) = [character(len=ESMF_MAXSTR) :: 'userRoutine', 'userroutine', 'USERROUTINE', 'procedure', 'setServices', 'setservices']
      character(:), allocatable :: ss_key

      has_mapl = ESMF_HConfigIsDefined(config, keyString='mapl', _RC)
      if (has_mapl) then
         mapl_cfg = ESMF_HConfigCreateAt(config, keyString='mapl', _RC)
         has_dso = .false.
         do i = 1, size(ss_dict_keys)
            ss_key = trim(ss_dict_keys(i))
            if (ESMF_HConfigIsDefined(mapl_cfg, keyString=ss_key, _RC)) then
               has_dso = .true.
               exit
            end if
         end do

         if (has_dso) then
            dso_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=ss_key, _RC)
            do i = 1, size(shared_obj_keys)
               try_key = trim(shared_obj_keys(i))
               if (ESMF_HConfigIsDefined(dso_cfg, keyString=try_key, _RC)) then
                  sharedObj = ESMF_HConfigAsString(dso_cfg, keyString=try_key, _RC)
                  exit
               end if
            end do

            do i = 1, size(user_routine_keys)
               try_key = trim(user_routine_keys(i))
               if (ESMF_HConfigIsDefined(dso_cfg, keyString=try_key, _RC)) then
                  userRoutine = ESMF_HConfigAsString(dso_cfg, keyString=try_key, _RC)
                  exit
               end if
            end do
            if (.not. allocated(userRoutine)) then
               userRoutine = 'setservices_'
            end if
            call ESMF_HConfigDestroy(dso_cfg, _RC)
         end if
         call ESMF_HConfigDestroy(mapl_cfg, _RC)
      end if

      if (.not. allocated(sharedObj)) then
         dso_found = .false.
         do i = 1, size(dso_keys)
            try_key = trim(dso_keys(i))
            if (ESMF_HConfigIsDefined(config, keyString=try_key, _RC)) then
               dso_found = .true.
               sharedObj = ESMF_HConfigAsString(config, keyString=try_key, _RC)
               exit
            end if
         end do
         _ASSERT(dso_found, 'setServices spec does not specify library/sharedObj/dso under mapl.dso')

         userProcedure_found = .false.
         do i = 1, size(userProcedure_keys)
            try_key = trim(userProcedure_keys(i))
            if (ESMF_HConfigIsDefined(config, keyString=try_key, _RC)) then
               userProcedure_found = .true.
               userRoutine = ESMF_HConfigAsString(config, keyString=try_key, _RC)
               exit
            end if
         end do
         if (.not. userProcedure_found) then
            userRoutine = 'setservices_'
         end if
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

