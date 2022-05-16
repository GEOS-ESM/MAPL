#include "MAPL_ErrLog.h"

! See external setservices() procedure at end of file


module SimpleParentGridComp
   use mapl_ErrorHandling
   use mapl3g_OuterMetaComponent
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

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call append_message('wasRun')
      outer_meta => get_outer_meta(gc, _RC)
      call outer_meta%run_children(clock, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine run
   
   subroutine run_extra(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call append_message('wasRun_extra')
      

      _RETURN(ESMF_SUCCESS)
   end subroutine run_extra

   subroutine init(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call append_message('wasInit')
      
      _RETURN(ESMF_SUCCESS)
   end subroutine init
   
   subroutine finalize(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call append_message('wasFinal')
      
      _RETURN(ESMF_SUCCESS)
   end subroutine finalize

end module SimpleParentGridComp

subroutine setServices(gc, rc)
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling
   use SimpleParentGridComp, only: inner_setservices => setservices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc

   integer :: status

   call inner_setservices(gc, _RC)

   _RETURN(ESMF_SUCCESS)
end subroutine setServices
