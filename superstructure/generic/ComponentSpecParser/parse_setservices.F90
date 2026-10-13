#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(config, rc) result(user_ss)
      type(DsoSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status

      sharedObj = ESMF_HConfigAsString(config,keyString='sharedObj',rc=status)
      _ASSERT(status == 0, 'setServices spec does not specify sharedObj')
      sharedObj = trim(sharedObj)
      _ASSERT(len(sharedObj) > 0, 'setServices spec does not specify sharedObj')

      if (ESMF_HConfigIsDefined(config,keyString='userRoutine')) then
         userRoutine = ESMF_HConfigAsString(config,keyString='userRoutine',_RC)
         userRoutine = trim(userRoutine)
      end if
      if (.not. allocated(userRoutine)) then
         userRoutine = 'setservices_'
      else if (len_trim(userRoutine) == 0) then
         userRoutine = 'setservices_'
      end if

      user_ss = DsoSetServices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod
