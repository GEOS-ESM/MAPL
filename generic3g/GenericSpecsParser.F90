#include "MAPL_ErrLog.h"

module mapl3g_GenericSpecsParser
   use mapl_ErrorHandling
   use yafyaml
   implicit none

contains

   function parse_setServices(config, rc) result(user_ss)
      use mapl3g_UserSetServices
      type(DSOSetServices) :: user_ss
      class(YAML_Node), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: sharedObj, userRoutine

      call config%get(sharedObj, 'sharedObj', _RC)
      call config%get(userRoutine, 'userRoutine', _RC)
      
      user_ss = user_setservices(sharedObj, userRoutine)

      _RETURN(_SUCCESS)
   end function parse_setServices

   

end module mapl3g_GenericSpecsParser
