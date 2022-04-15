#include "MAPL_ErrLog.h"

! See external setservices() procedure at end of file


module SimpleLeafGridComp
   use mapl_ErrorHandling
   use scratchpad
   use esmf
   implicit none
   private

   public :: setservices

   
contains

   subroutine setservices(gc, rc)
      use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      if (.not. allocated(log)) then
         log = ''
      else
         log = log // ' :: '
      end if
      log = log // 'wasRun'
      

      _RETURN(ESMF_SUCCESS)
   end subroutine
   
end module SimpleLeafGridComp

subroutine setServices(gc, rc)
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling
   use SimpleLeafGridComp, only: inner_setservices => setservices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc

   integer :: status

   call inner_setservices(gc, _RC)

   _RETURN(ESMF_SUCCESS)
end subroutine setServices
