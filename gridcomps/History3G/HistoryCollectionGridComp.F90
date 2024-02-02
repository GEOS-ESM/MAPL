#include "MAPL_Generic.h"

module mapl3g_HistoryCollectionGridComp
   use mapl_ErrorHandlingMod
   use generic3g

   use esmf
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryCollectionGridComp
!#      class(Client), pointer :: client
   end type HistoryCollectionGridComp


contains
   
   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      type(ESMF_HConfig) :: hconfig
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      integer :: status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, update_geom, phase_name='GENERIC_RUN_UPDATE_GEOM', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

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


   subroutine update_geom(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status


      _RETURN(_SUCCESS)
   end subroutine update_geom

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_HistoryCollectionGridComp
