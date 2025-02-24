#include "MAPL_Generic.h"

module mapl3g_HistoryGridComp
   use mapl3g_HistoryGridComp_private
   use mapl3g_HistoryCollectionGridComp, only: collection_setServices => setServices
   use MAPL_TimeStringConversion
   use mapl3g_ChildSpec
   use generic3g
   use mapl_ErrorHandling
   use pFlogger, only: logger
   use esmf
   use pfio
   implicit none(type,external)
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, collections_config, child_hconfig
      character(len=:), allocatable :: child_name, collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      class(logger), pointer :: lgr
      type(ChildSpec) :: child_spec
      integer :: num_collections, status
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_Time), allocatable :: refTime

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      has_active_collections = ESMF_HConfigIsDefined(hconfig, keyString='active_collections', _RC)
      if (.not. has_active_collections) then
         call MAPL_GridCompGet(gridcomp,logger=lgr, _RC)
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
         collection_name = ESMF_HConfigAsString(iter, _RC)
         child_hconfig = make_child_hconfig(hconfig, collection_name, _RC)
         child_name = make_child_name(collection_name, _RC)

         call get_child_timestep(child_hconfig, timeStep, _RC)
         child_spec = ChildSpec(user_setservices(collection_setServices), child_hconfig, timeStep, refTime)
         call MAPL_GridCompAddChild(gridcomp, child_name, child_spec,_RC)
      end do
      
      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine get_child_timestep(hconfig, timestep, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_TimeInterval), allocatable, intent(out) :: timestep
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_frequency
      type(ESMF_HConfig) :: time_hconfig

      time_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='time_spec', _RC)
      has_frequency = ESMF_HConfigIsDefined(time_hconfig, keyString='frequency', _RC)
      if (has_frequency) then
         timeStep = hconfig_to_esmf_timeinterval(time_hconfig, 'frequency', _RC)
      end if
      call ESMF_HConfigDestroy(time_hconfig)
      
      _RETURN(_SUCCESS)
   end subroutine get_child_timestep

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status
      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_GridCompRunChildren(gridcomp, phase_name='run', _RC)

      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      _RETURN(_SUCCESS)
   end subroutine run

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

