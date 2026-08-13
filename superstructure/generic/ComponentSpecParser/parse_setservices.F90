#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(config, rc) result(user_ss)
      type(DSOSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc
      character(:), allocatable :: sharedObj, userRoutine
      character(len=*), parameter :: SHARED_OBJ_KEYS(*) = [character(len=9) :: 'sharedObj', 'dso' ]
      character(len=*), parameter :: USER_ROUTINE_KEY = 'userRoutine'
      integer :: status
      integer :: i
      logical :: has_key
      character, allocatable :: key

      do i=1, size(SHARED_OBJ_KEYS)
         key = trim(SHARED_OBJ_KEYS(i))
         has_key = ESMF_HConfigAsString(config, keyString=SHARED_OBJ_KEYS(i))
         if(has_key) then
            sharedObj = ESMF_HConfigAsString(config,keyString=key, rc=status)
            continue
         end if
      end do
      _ASSERT(status == 0, 'setServices spec does not specify sharedObj')

      if (ESMF_HConfigIsDefined(config,keyString=USER_ROUTINE_KEY)) then
         userRoutine = ESMF_HConfigAsString(config,keyString=USER_ROUTINE_KEY, _RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

