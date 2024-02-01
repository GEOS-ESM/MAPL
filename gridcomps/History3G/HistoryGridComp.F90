#include "MAPL_Generic.h"

module mapl3g_HistoryGridComp
   use mapl3g_HistoryGridComp_private, only: setServices
   implicit none
   private

   public :: setServices

end module mapl3g_HistoryGridComp

subroutine setServices(gridcomp,rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_HistoryGridComp, only: History_setServices => SetServices    
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call History_setServices(gridcomp,_RC)
   _RETURN(_SUCCESS)

end subroutine

