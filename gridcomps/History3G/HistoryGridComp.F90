#include "MAPL_Generic.h"

module mapl3g_HistoryGridComp
   use mapl3g_HistoryCollectionGridComp, only: collection_setServices => setServices
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryGridComp
      class(Client), pointer :: client
   end type HistoryGridComp


   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(HistoryGridComp), pointer :: history_gridcomp
      type(ESMF_HConfig) :: hconfig

      ! Set entry points
!#      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name=GENERIC_INIT_USER)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryGridComp, "HistoryGridComp", history_gridcomp)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      has_collections = ESMF_HConfigIsDefined(hconfig, keyString='collections', _RC)
      _RETURN_UNLESS(has_collections)

      collections_config = ESMF_HConfigCreateAt(hconfig, keystring='collections', _RC)
      num_collections = ESMF_HConfigSize(collections_config, _RC)
      _RETURN_UNLESS(num_collections > 0)

      iter_begin = ESMF_HConfigIterBegin(collections_config,_RC)
      iter_end = ESMF_HConfigIterEnd(collections_config, _RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         collection_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         collection_hconfig = ESMF_HConfigCreateAtMapVal(iter, _RC)

         call MAPL_AddChild(gridcomp, collection_name, collection_setServices, collection_hconfig, _RC)
         call ESMF_HConfigDestroy(collection_hconfig, nogarbage=.true, _RC)
         
      end do
      
      _RETURN(_SUCCESS)
   end subroutine setServices

!#   subroutine init(gridcomp, importState, exportState, clock, rc)
!#      type(ESMF_GridComp)   :: gridcomp
!#      type(ESMF_State)      :: importState
!#      type(ESMF_State)      :: exportState
!#      type(ESMF_Clock)      :: clock      
!#      integer, intent(out)  :: rc
!#
!#      integer :: status
!#
!#      ! To Do:
!#      ! - determine run frequencey and offset (save as alarm)
!#
!#      
!#      _RETURN(_SUCCESS)
!#   end subroutine init
!#

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_RunChildren(gridcomp, phase_name='run', _RC)

      _RETURN(_SUCCESS)
   end subroutine run
end module mapl3g_HistoryGridComp
