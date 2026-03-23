#include "MAPL.h"

module mapl3g_HistoryGridComp

   use mapl3
   use mapl3g_HistoryGridComp_private
   use mapl3g_HistoryCollectionGridComp, only: collection_setServices => setServices
   use MAPL_TimeStringConversion
   use mapl3g_StatisticsGridComp, only: statistics_setServices => setServices
   use pFlogger, only: logger

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

         call get_child_timespec(child_hconfig, timeStep,  _RC)
         call add_run_next_step(child_hconfig, hconfig, _RC)
         call add_child_ref_datetime(child_hconfig, _RC)

         child_spec = ChildSpec(user_setservices(collection_setServices), hconfig=child_hconfig, timeStep=timeStep)

         call add_stats_gc(gridcomp, child_name, child_hconfig, _RC)
         call MAPL_GridCompAddChild(gridcomp, child_name, child_spec,_RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine setServices
   
   subroutine get_child_timespec(hconfig, timeStep, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_TimeInterval), allocatable, intent(out) :: timeStep
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_HConfig) :: time_hconfig
      logical :: has_frequency

      time_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='time_spec', _RC)

      has_frequency = ESMF_HConfigIsDefined(time_hconfig, keyString='frequency', _RC)
      if (has_frequency) then
         timeStep = mapl_HConfigAsTimeInterval(time_hconfig, keystring='frequency', _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_child_timespec

   subroutine add_run_next_step(child_hconfig, hconfig, rc)
      type(ESMF_HConfig), intent(inout) :: child_hconfig
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_shift, shift

      shift = .true.
      has_shift = ESMF_HConfigIsDefined(hconfig, keystring='run_next_step', _RC)
      if (has_shift) then
         shift = ESMF_HConfigAsLogical(hconfig, keystring='run_next_step', _RC)
      end if
      call ESMF_HConfigAdd(child_hconfig, shift, addKeyString='run_next_step', _RC)
      _RETURN(_SUCCESS)

   end subroutine add_run_next_step

   subroutine add_child_ref_datetime(hconfig, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_HConfig) :: time_hconfig
      logical :: has_ref_datetime
      character(len=:), allocatable :: ref_datetime

      time_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='time_spec', _RC)

      ref_datetime = "'YYYY-MM-DDTHH:NN:SS'"
      has_ref_datetime = ESMF_HConfigIsDefined(time_hconfig, keyString='ref_datetime', _RC)
      if (has_ref_datetime) then
         ref_datetime = ESMF_HConfigAsString(time_hconfig, keystring='ref_datetime', _RC)
         ref_datetime = "'"//ref_datetime//"'"
      end if
      call ESMF_HConfigAdd(hconfig, ref_datetime, addKeyString='ref_datetime', _RC)
      _RETURN(_SUCCESS)
   end subroutine add_child_ref_datetime

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
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
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
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

