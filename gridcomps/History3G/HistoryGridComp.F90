#include "MAPL_Generic.h"

module mapl3g_HistoryGridComp
   use ESMF
   use generic3g
   use MAPL_ErrorHandlingMod
   use pflogger
!# use mapl3g_HistoryCollectionGridComp, only: collection_setServices => setServices
   implicit none
   private

   public :: setServices_

   contains

   subroutine setServices_(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig,collections_config,collection_hconfig
      character(len=:), allocatable :: collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      character(*), parameter :: PRIVATE_STATE = "HistoryGridComp"
      class(logger), pointer :: lgr
      integer :: num_collections, status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER")
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
!#    _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryGridComp, PRIVATE_STATE, history_gridcomp)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      has_active_collections = ESMF_HConfigIsDefined(hconfig, keyString='active_collections', _RC)
      if (.not. has_active_collections) then
         call MAPL_Get(gridcomp,logger=lgr)
         call lgr%warning("no active collection specified in History")
         _RETURN(_SUCCESS)
      end if

      collections_config = ESMF_HConfigCreateAt(hconfig, keystring='active_collections', _RC)
      num_collections = ESMF_HConfigGetSize(collections_config, _RC)
      _RETURN_UNLESS(num_collections > 0)

      iter_begin = ESMF_HConfigIterBegin(collections_config,_RC)
      iter_end = ESMF_HConfigIterEnd(collections_config, _RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         collection_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         collection_hconfig = ESMF_HConfigCreateAtMapVal(iter, _RC)

!#       call MAPL_AddChild(gridcomp, collection_name, collection_setServices, collection_hconfig, _RC)
         call ESMF_HConfigDestroy(collection_hconfig, _RC)
         
      end do
      
      _RETURN(_SUCCESS)
   end subroutine setServices_

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)

      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_Run_Children(gridcomp, phase_name='run', _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_HistoryGridComp

subroutine setServices(gridcomp,rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_HistoryGridComp, only: History_setServices => SetServices_    
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call History_setServices(gridcomp,_RC)
   _RETURN(_SUCCESS)

end subroutine











