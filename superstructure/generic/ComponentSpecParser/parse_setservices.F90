#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(config, rc) result(user_ss)
      type(DSOSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status
      logical :: dso_found

      ! Accept both 'dso' (canonical) and 'sharedObj' (legacy alias)
      dso_found = .false.
      if (ESMF_HConfigIsDefined(config, keyString='dso')) then
        sharedObj = ESMF_HConfigAsString(config, keyString='dso', _RC)
        dso_found = .true.
      end if
      if (ESMF_HConfigIsDefined(config, keyString='sharedObj')) then
        _ASSERT(.not. dso_found, 'setServices spec specifies both dso and sharedObj')
        sharedObj = ESMF_HConfigAsString(config, keyString='sharedObj', _RC)
        dso_found = .true.
      end if
      _ASSERT(dso_found, 'setServices spec must specify dso (or legacy sharedObj)')

      if (ESMF_HConfigIsDefined(config,keyString='userRoutine')) then
         userRoutine = ESMF_HConfigAsString(config,keyString='userRoutine',_RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

