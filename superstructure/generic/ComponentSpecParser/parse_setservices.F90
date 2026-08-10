#include "MAPL.h"
#if defined(_ASSERT_KEY)
#  under _ASSERT_KEY
#endif
#define _ASSERT_KEY(H, K) _ASSERT(H, 'No ' // K // ' found')

submodule (mapl_ComponentSpecParser_mod) parse_setservices_smod

   implicit none(type,external)
   
contains

   
   module function parse_setservices(config, rc) result(user_ss)
      type(DSOSetServices) :: user_ss
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: maplKey = 'mapl'
      character(len=*), parameter :: sharedObjKey = 'sharedObj'
      character(len=*), parameter :: userRoutineKey = 'userRoutine'
      character(len=*), parameter :: setServicesKey = 'setServices'
      character(:), allocatable :: sharedObj, userRoutine
      integer :: status
      logical :: has_mapl, has_setservices, has_sharedObj
      type(ESMF_HConfig) :: mapl_config, ssconfig

      has_mapl = ESMF_HConfigIsDefined(config, keyString=maplKey)
      _ASSERT(has_mapl, 'No '// maplKey //' found.')

      mapl_config = ESMF_HConfigCreateAt(config, keyString=maplKey, _RC)
      has_setservices = ESMF_HConfigIsDefined(mapl_config, keyString=setServicesKey)
      _ASSERT(has_setservices, 'No ' // setServicesKey // ' found.')

      ssconfig = ESMF_HConfigCreateAt(mapl_config, keyString=setServicesKey, _RC)
      has_sharedObj = ESMF_HConfigIsDefined(ssconfig, keyString=sharedObjKey)
      _ASSERT(has_sharedObj, 'No ' // sharedObjKey // ' found.')

      sharedObj = ESMF_HConfigAsString(ssconfig, keyString=sharedObjKey, _RC)

      if (ESMF_HConfigIsDefined(ssconfig, keyString=userRoutineKey)) then
         userRoutine = ESMF_HConfigAsString(ssconfig,keyString='userRoutine',_RC)
      else
         userRoutine = 'setservices_'
      end if

      call ESMF_HConfigDestroy(ssconfig, _RC)
      call ESMF_HConfigDestroy(mapl_config, _RC)

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices

end submodule parse_setservices_smod

