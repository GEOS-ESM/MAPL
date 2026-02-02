#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(hconfig, rc) result(user_ss)
      type(DSOSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status

      sharedObj = ESMF_HConfigAsString(hconfig,keyString='sharedObj',rc=status)
      _ASSERT(status == 0, 'setServices spec does not specify sharedObj')

      if (ESMF_HConfigIsDefined(hconfig,keyString='userRoutine')) then
         userRoutine = ESMF_HConfigAsString(hconfig,keyString='userRoutine',_RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

