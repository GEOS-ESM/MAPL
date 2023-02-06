#include "MAPL_ErrLog.h"

! See external setservices() procedure at end of file


module SimpleLeafGridComp
   use mapl_ErrorHandling
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
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run_extra, phase_name='extra', _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, finalize, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      call append_message(gc, 'wasRun')
      
      _RETURN(ESMF_SUCCESS)
   end subroutine run
   
   subroutine run_extra(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      call append_message(gc, 'wasRun_extra')

      _RETURN(ESMF_SUCCESS)
   end subroutine run_extra

   subroutine init(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      call append_message(gc, 'wasInit')
      
      _RETURN(ESMF_SUCCESS)
   end subroutine init
   
   subroutine finalize(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc


      call append_message(gc, 'wasFinal')
      
      _RETURN(ESMF_SUCCESS)
   end subroutine finalize

   subroutine append_message(gc, message)
      use scratchpad, only: append_scratchpad_message => append_message
      type(ESMF_GridComp), intent(in) :: gc
      character(*), intent(in) :: message

      character(ESMF_MAXSTR) :: name
      call ESMF_GridCompGet(gc, name=name)

      call append_scratchpad_message(message // '_' // trim(name))
   end subroutine append_message

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
