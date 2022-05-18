#include "MAPL_ErrLog.h"

module MockUserGridComp
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_METHOD_INITIALIZE
   use esmf, only: ESMF_METHOD_RUN
   use esmf, only: ESMF_METHOD_FINALIZE
   use esmf, only: ESMF_METHOD_READRESTART
   use esmf, only: ESMF_METHOD_WRITERESTART
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling
   implicit none
   private
   
   public :: setServices
   
contains

   subroutine setservices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status
      
!!$      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE,   initialize,    _RC)
!!$      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN,          run,           _RC)
!!$      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE,     finalize,      _RC)
!!$      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_READRESTART,  read_restart,  _RC)
!!$      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_WRITERESTART, write_restart, _RC)
      

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   
end module MockUserGridComp
