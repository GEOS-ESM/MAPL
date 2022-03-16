#include "MAPL_ErrLog.h"

module MockUserGridComp
   implicit none
   private
   
   public :: setServices
   
contains

   subroutine setservices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status
      
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE,   initialize,    _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN,          run,           _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE,     finalize,      _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_READRESTART,  read_restart,  _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_WRITERESTART, write_restart, _RC)
      

      _RETURN(_RC)
   end subroutine setservices

   
end module MockUserGridComp
