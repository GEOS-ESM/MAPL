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

      character(:), allocatable :: sharedObj, userRoutine, try_key
      type(ESMF_HConfig) :: mapl_cfg, dso_cfg
      integer :: i, status
      logical :: has_mapl, has_dso, dso_found, userProcedure_found

      if (ESMF_HConfigIsDefined(config,keyString='userRoutine')) then
         userRoutine = ESMF_HConfigAsString(config,keyString='userRoutine',_RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

