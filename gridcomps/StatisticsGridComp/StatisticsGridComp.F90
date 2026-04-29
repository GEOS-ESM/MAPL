#include "MAPL.h"

module mapl3g_StatisticsGridComp

   use MAPL
   use ESMF
   use mapl3g_RestartHandler
   use mapl3g_ESMF_Time_Utilities, only: sub_time_in_datetime
   ! local modules
   use mapl3g_AbstractTimeStatistic
   use mapl3g_StatisticsVector
   use mapl3g_NullStatistic
   use mapl3g_TimeAverage
   use mapl3g_TimeMin
   use mapl3g_TimeMax
   use mapl3g_State_API
   use pflogger, only: Logger
   use mapl_OS
   use mapl3g_Utilities, only: MAPL_GetCheckpointSubdir
   use mapl3g_SimpleAlarm, only: SimpleAlarm

   implicit none(type,external)
   private

   public :: setServices

   type :: Statistics ! private state
      integer :: item_count = 0
      type(StatisticsVector) :: items
   end type Statistics

   character(*), parameter :: PRIVATE_STATE = 'Statistics'

contains

   subroutine setServices(gridComp, rc)
      type(esmf_GridComp) :: gridComp
      integer, intent(out) :: rc

      integer :: status
      type(Statistics), pointer :: stats
      type(esmf_HConfig) :: hconfig, items_hconfig
      type(esmf_HConfigIter) :: iter, b, e

      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, modify_advertise, phase_name='GENERIC::INIT_MODIFY_ADVERTISED', _RC)
      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, realize, phase_name='GENERIC::INIT_REALIZE', _RC)
      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_RUN, run, phase_name='run', _RC)
      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_WRITERESTART, custom_write_restart, phase_name='GENERIC::WRITE_RESTART', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE)
      _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)

      call mapl_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      items_hconfig = esmf_HConfigCreateAt(hconfig, keystring='stats', _RC)
      stats%item_count = esmf_HConfigGetSize(items_hconfig, _RC)

      b = esmf_HConfigIterBegin(items_hconfig)
      e = esmf_HConfigIterEnd(items_hconfig)
      iter = b
      do while (esmf_HConfigIterLoop(iter,b,e))
         call advertise_item(gridcomp, iter, _RC)
      enddo

      call esmf_HConfigdestroy(items_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine advertise_item(gridcomp, iter, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_HConfigIter), intent(in) :: iter
      integer, optional, intent(out) :: rc

      type(esmf_TimeInterval) :: period
      character(:), allocatable :: action, name
      type(esmf_StateItem_Flag) :: itemtype
      integer :: status
      type(esmf_HConfig) :: hconfig
      type(VariableSpec) :: varspec

      hconfig = esmf_HConfigCreateAt(iter, _RC)
      action = esmf_HConfigAsString(hconfig, keystring='action', _RC)
      name = esmf_HConfigAsString(hconfig, keystring='name', _RC)

      varspec = make_VariableSpec(ESMF_STATEINTENT_IMPORT, name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
       select case (action)
       case ('average')
          period = mapl_HConfigAsTimeInterval(hconfig, keystring='period', _RC)
          varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, name, timestep=period, &
               has_deferred_aspects=.true., _RC)
          call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
          call advertise_time_average_internal_fields(gridcomp, name, _RC)
       case ('min')
          period = mapl_HConfigAsTimeInterval(hconfig, keystring='period', _RC)
          varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, name, timestep=period, &
               has_deferred_aspects=.true., _RC)
          call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
          call advertise_time_min_internal_fields(gridcomp, name, _RC)
       case ('max')
          period = mapl_HConfigAsTimeInterval(hconfig, keystring='period', _RC)
          varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, name, timestep=period, &
               has_deferred_aspects=.true., _RC)
          call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
          call advertise_time_max_internal_fields(gridcomp, name, _RC)
       case default
          _FAIL('unsupported action: '//action)
      end select

      call esmf_HConfigDestroy(hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine advertise_item

   subroutine modify_advertise(gridcomp, importState, exportState, clock, rc)

      type(esmf_GridComp) :: gridcomp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(esmf_HConfigIter) :: iter, b, e
      type(Statistics), pointer :: stats
      type(esmf_HConfig) :: hconfig, items_hconfig
      class(AbstractTimeStatistic), allocatable :: item

      _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)
      call mapl_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      items_hconfig = esmf_HConfigCreateAt(hconfig, keystring='stats', _RC)

      b = esmf_HConfigIterBegin(items_hconfig)
      e = esmf_HConfigIterEnd(items_hconfig)
      iter = b
      do while (esmf_HConfigIterLoop(iter,b,e))
         call modify_advertise_item(iter, _RC)
      enddo

      call esmf_HConfigdestroy(items_hconfig, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine modify_advertise_item(iter, rc)
         type(esmf_HConfigIter), intent(in) :: iter
         integer, optional, intent(out) :: rc

          integer :: status
          character(:), allocatable :: name
          type(esmf_Field) :: f_in
          type(StateItemAllocation) :: allocation_status
          type(esmf_StateItem_Flag) :: itemtype

         name = esmf_HConfigAsString(iter, keystring='name', _RC)

         call mapl_StateGet(importState, itemName=name, itemtype=itemtype, _RC)
         _RETURN_IF(itemtype == ESMF_STATEITEM_NOTFOUND)

          call mapl_StateGet(importState, itemName=name, field=f_in, _RC)
          call mapl_FieldGet(f_in, allocation_status=allocation_status, _RC)
          _RETURN_UNLESS(allocation_status == STATEITEM_ALLOCATION_CONNECTED)

          item = make_item(name, iter, clock, _RC)
         call stats%items%push_back(item)

        _RETURN(_SUCCESS)
      end subroutine modify_advertise_item

      function make_item(name, iter, clock, rc) result(stat)
         class(AbstractTimeStatistic), allocatable :: stat
         character(*), intent(in) :: name
         type(esmf_HConfigIter), intent(in) :: iter
         type(esmf_Clock), intent(in) :: clock
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: action
         type(SimpleAlarm) :: alarm

         stat = NullStatistic() ! just in case
         action = esmf_HConfigAsString(iter, keystring='action', _RC)
         alarm = make_alarm(clock, iter, _RC)

          select case (action)
          case ('average')
             deallocate(stat) ! gfortran workaround
             stat = make_average_stat(name, iter, alarm, _RC)
          case ('min')
             deallocate(stat) ! gfortran workaround
             stat = make_min_stat(name, iter, alarm, _RC)
          case ('max')
             deallocate(stat) ! gfortran workaround
             stat = make_max_stat(name, iter, alarm, _RC)
          case default
             _FAIL('unsupported statistics class: '//action)
          end select

         _RETURN(_SUCCESS)
      end function make_item

      function make_average_stat(name, iter, alarm, rc) result(average)
         type(TimeAverage) :: average
         character(*), intent(in) :: name
          type(esmf_HConfigIter), intent(in) :: iter
          type(SimpleAlarm), intent(in) :: alarm
          integer, optional, intent(out) :: rc

          integer :: status
          type(esmf_Field) :: f_in, f_out

          call esmf_StateGet(importState, itemName=name, field=f_in, _RC)
          call esmf_StateGet(exportState, itemName=name, field=f_out, _RC)

          average = TimeAverage(gridcomp=gridcomp, f=f_in, avg_f=f_out, alarm=alarm, _RC)

         _RETURN(_SUCCESS)
       end function make_average_stat

       function make_min_stat(name, iter, alarm, rc) result(min_stat)
           type(TimeMin) :: min_stat
           character(*), intent(in) :: name
           type(esmf_HConfigIter), intent(in) :: iter
           type(SimpleAlarm), intent(in) :: alarm
           integer, optional, intent(out) :: rc

           integer :: status
           type(esmf_Field) :: f_in, f_out

           call esmf_StateGet(importState, itemName=name, field=f_in, _RC)
           call esmf_StateGet(exportState, itemName=name, field=f_out, _RC)

           min_stat = TimeMin(gridcomp=gridcomp, f=f_in, min_f=f_out, alarm=alarm, _RC)

           _RETURN(_SUCCESS)
        end function make_min_stat

       function make_max_stat(name, iter, alarm, rc) result(max_stat)
           type(TimeMax) :: max_stat
           character(*), intent(in) :: name
           type(esmf_HConfigIter), intent(in) :: iter
           type(SimpleAlarm), intent(in) :: alarm
           integer, optional, intent(out) :: rc

           integer :: status
           type(esmf_Field) :: f_in, f_out

           call esmf_StateGet(importState, itemName=name, field=f_in, _RC)
           call esmf_StateGet(exportState, itemName=name, field=f_out, _RC)

           max_stat = TimeMax(gridcomp=gridcomp, f=f_in, max_f=f_out, alarm=alarm, _RC)

           _RETURN(_SUCCESS)
        end function make_max_stat

       function make_alarm(clock, iter, rc) result(alarm)
           type(SimpleAlarm) :: alarm
           type(esmf_Clock), intent(in) :: clock
           type(esmf_HConfigIter), intent(in) :: iter
           integer, optional, intent(out) :: rc

           integer :: status
           type(esmf_TimeInterval) :: period
           type(esmf_Time) :: ringTime, currTime
           character(:), allocatable :: ref_datetime

           period = mapl_HConfigAsTimeInterval(iter, keystring='period', _RC)
           ref_datetime = esmf_HConfigAsString(iter, keystring='ref_datetime', _RC)

           call esmf_ClockGet(clock, currTime=currTime, _RC)
           ringTime = sub_time_in_datetime(currTime, ref_datetime, _RC)

           alarm = SimpleAlarm(initial_ring_time=ringTime, ring_interval=period, _RC)
            _RETURN(_SUCCESS)
         end function make_alarm

   end subroutine modify_advertise

   subroutine realize(gridcomp, importState, exportState, clock, rc)

      type(esmf_GridComp) :: gridcomp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      call MAPL_StateGetGeom(importState, geom, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC) 
      _RETURN(_SUCCESS)

   end subroutine realize

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridcomp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      type(Statistics), pointer :: stats
      class(AbstractTimeStatistic), pointer :: stat
      integer :: status

      type(StatisticsVectorIterator) :: iter

      _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)

      iter = stats%items%ftn_begin()
      associate (e => stats%items%ftn_end())
        do while (iter /= e)
           call iter%next()
           stat => iter%of()
             call stat%update(gridcomp, clock, _RC)
         end do
       end associate

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(importState)
       _UNUSED_DUMMY(exportState)
   end subroutine run

   subroutine custom_write_restart(gridcomp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridComp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(esmf_State) :: state
      type(RestartHandler) :: restart_handler
      type(esmf_Time) :: currTime
      class(Logger), pointer :: lgr
      type(esmf_Geom) :: geom
      character(:), allocatable :: subdir, filename, name
      type(ESMF_HConfig) :: hconfig
      type(Statistics), pointer :: stats
      type(StatisticsVectorIterator) :: iter
      class(AbstractTimeStatistic), pointer :: stat
       type(SimpleAlarm) :: alarm
       logical :: is_ringing, first_ringing
       logical :: first_item

       ! Verify all alarms share the same ringing status; skip write if all are ringing
       _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)

       call esmf_ClockGet(clock, currTime=currTime, _RC)

       first_item = .true.
       first_ringing = .false.
       iter = stats%items%ftn_begin()
       associate (e => stats%items%ftn_end())
          do while (iter /= e)
             call iter%next()
             stat => iter%of()
             alarm = stat%get_alarm()
             is_ringing = alarm%is_ringing(currTime, _RC)
            if (first_item) then
               first_ringing = is_ringing
               first_item = .false.
               cycle
            endif
            _FAIL(is_ringing .eqv. first_ringing, 'Inconsistent alarm state: not all statistics alarms have the same ringing status')
         end do
      end associate

      if (first_ringing) then
         _RETURN(_SUCCESS)
      end if

      call MAPL_GridCompGetInternalState(gridcomp, state, _RC)
      call mapl_GridCompGet(gridcomp, logger=lgr, name=name, hconfig=hconfig, _RC)

       call MAPL_StateGetGeom(state, geom, _RC)
       restart_handler = RestartHandler(geom, currTime, lgr)

      subdir = MAPL_GetCheckpointSubdir(hconfig, currTime, _RC)
      filename = mapl_PathJoin(subdir, name//'_internal.nc')

      call restart_handler%write(state, filename, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
   end subroutine custom_write_restart

end module mapl3g_StatisticsGridComp

subroutine setServices(gridComp, rc)
   use MAPL
   use mapl3g_StatisticsGridComp, only: StatisticsSetServices => setServices
   implicit none(type,external)
   type(esmf_GridComp), intent(inout) :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call StatisticsSetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices

