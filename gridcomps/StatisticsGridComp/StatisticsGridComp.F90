#include "MAPL.h"
module mapl3g_StatisticsGridComp
   use mapl3
   use mapl3g_RestartHandler
   ! local modules
   use mapl3g_AbstractTimeStatistic
   use mapl3g_StatisticsVector
   use mapl3g_NullStatistic
   use mapl3g_TimeAverage
   use pflogger
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
      type(esmf_GridComp), intent(inout) :: gridComp
      integer, optional, intent(out) :: rc

      integer :: status
      type(Statistics), pointer :: stats
      type(esmf_HConfig) :: hconfig, items_hconfig
      type(esmf_HConfigIter) :: iter, b, e

      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, modify_advertise, phase_name='GENERIC::INIT_MODIFY_ADVERTISED', _RC)
      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_RUN, run, phase_name='run', _RC)
      call mapl_GridCompSetEntryPoint(gridComp, ESMF_METHOD_READRESTART, custom_read_restart, phase_name='GENERIC:READ_RESTART', _RC)
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

      type(esmf_TimeInterval) :: period, offset
      character(:), allocatable :: action, name
      type(esmf_StateItem_Flag) :: itemtype
      integer :: status
      type(esmf_HConfig) :: hconfig
      type(VariableSpec) :: varspec

      hconfig = esmf_HConfigCreateAt(iter, _RC)
      action = esmf_HConfigAsString(hconfig, keystring='action', _RC)
      name = esmf_HConfigAsString(hconfig, keystring='name', _RC)
      itemtype = mapl_HConfigAsItemType(hconfig, keystring='itemtype', _RC)

      varspec = make_VariableSpec(ESMF_STATEINTENT_IMPORT, name, _RC)
      call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
      select case (action)
      case ('average')
         period = mapl_HConfigAsTimeInterval(hconfig, keystring='period', _RC)
         offset= mapl_HConfigAsTimeInterval(hconfig, keystring='offset', _RC)
         varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, name, timestep=period, offset=offset, &
              has_deferred_aspects=.true., _RC)
         call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
!#         call mapl_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_EXPORT, name, dims='XY', vstagger=VERTICAL_STAGGER_NONE, &
!#              itemtype=itemtype, _RC)
!#              standard_name='<unknown>', timestep=period, &
!#              refTime_offset=offset, &
!#              has_deferred_aspects=.true., _RC)
!#         call mapl_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_EXPORT, name, dims='XY', vstagger=VERTICAL_STAGGER_NONE, &
!#              itemtype=itemtype, &
!#              standard_name='<unknown>', timestep=period, &
!#              refTime_offset=offset, &
!#              has_deferred_aspects=.true., _RC)
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

      _HERE
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
         character(:), allocatable :: action, name
         type(esmf_Field) :: f_in, f_out
         logical :: is_connected
         class(AbstractTimeStatistic), allocatable :: stat
         type(StateItemAllocation) :: allocation_status
         type(esmf_HConfig) :: hconfig

         type(esmf_Geom), allocatable :: geom
         character(:), allocatable :: units
         character(:), allocatable :: standard_name, long_name
         type(esmf_TypeKind_Flag) :: typekind
         class(VerticalGrid), allocatable :: vertical_grid
         type(UngriddedDims) :: ungridded_dims
         type(esmf_StateItem_Flag) :: itemtype

         _HERE
         _HERE, importState
         action = esmf_HConfigAsString(iter, keystring='action', _RC)
         name = esmf_HConfigAsString(iter, keystring='name', _RC)

         call mapl_StateGet(importState, itemName=name, itemtype=itemtype, _RC)
         _RETURN_IF(itemtype == ESMF_STATEITEM_NOTFOUND) 

         _HERE
         call mapl_StateGet(importState, itemName=name, field=f_in, _RC)
         _HERE
         call mapl_FieldGet(f_in, allocation_status=allocation_status, _RC)
         _HERE, allocation_status%to_string()
         _RETURN_UNLESS(allocation_status == STATEITEM_ALLOCATION_CONNECTED)

         _HERE,' woo hoo - connected now !!!'

         call mapl_FieldGet(f_in, &
              geom=geom, &
              ungridded_dims=ungridded_dims, &
              units=units, &
              typekind=typekind, &
              _RC)
         call mapl_FieldGetVerticalGrid(f_in, vertical_grid=vertical_grid, _RC)

         _HERE
         call mapl_StateGet(exportState, itemName=name, field=f_out, _RC)
         call mapl_FieldModify(f_out, &
              has_deferred_aspects=.false., &
              geom=geom, &
              ungridded_dims=ungridded_dims, &
              units=units, &
              typekind=typekind, &
              vertical_grid=vertical_grid, &
              _RC)

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
         type(esmf_Alarm) :: alarm

         stat = NullStatistic() ! just in case
         action = esmf_HConfigAsString(iter, keystring='action', _RC)
         alarm = make_alarm(clock, iter, _RC)

         select case (action)
         case ('average')
            deallocate(stat) ! gfortran workaround
            stat = make_average_stat(name, iter, alarm, _RC)
         case default
            _FAIL('unsupported statistics class: '//action)
         end select

         _RETURN(_SUCCESS)
      end function make_item

      function make_average_stat(name, iter, alarm, rc) result(average)
         type(TimeAverage) :: average
         character(*), intent(in) :: name
         type(esmf_HConfigIter), intent(in) :: iter
         type(esmf_Alarm), intent(in) :: alarm
         integer, optional, intent(out) :: rc

         integer :: status
         type(esmf_Field) :: f_in, f_out
         
         call esmf_StateGet(importState, itemName=name, field=f_in, _RC)
         call esmf_StateGet(exportState, itemName=name, field=f_out, _RC)

         average = TimeAverage(f=f_in, avg_f=f_out, alarm=alarm)

         _RETURN(_SUCCESS)
      end function make_average_stat

      function make_alarm(clock, iter, rc) result(alarm)
         type(esmf_Alarm) :: alarm
         type(esmf_Clock), intent(in) :: clock
         type(esmf_HConfigIter), intent(in) :: iter
         integer, optional, intent(out) :: rc

         integer :: status
         type(esmf_TimeInterval) :: period, offset
         type(esmf_Time) :: ringTime, refTime
         character(:), allocatable :: iso_timeinterval

         period = mapl_HConfigAsTimeInterval(iter, keystring='period', _RC)
         offset = mapl_HConfigAsTimeInterval(iter, keystring='offset', _RC)
!#         refTime=
         ringTime = refTime + offset

         alarm = esmf_AlarmCreate(clock, ringTime=ringTime, ringInterval=period, _RC)
         _RETURN(_SUCCESS)
      end function make_alarm

   end subroutine modify_advertise


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

      iter = stats%items%ftn_begin()
      associate (e => stats%items%ftn_end())
        do while (iter /= e)
           call iter%next()
           stat => iter%of()
           call stat%update(_RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run

   subroutine custom_read_restart(gridComp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridComp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(Statistics), pointer :: stats
      type(esmf_State) :: state
      type(StatisticsVectorIterator) :: iter
      type(RestartHandler) :: restart_handler
      class(AbstractTimeStatistic), pointer :: stat
      type(esmf_Time) :: currTime
      class(Logger), pointer :: lgr
      type(esmf_Geom) :: geom
      character(:), allocatable :: name, filename

      _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)
      state = esmf_StateCreate(stateIntent=ESMF_STATEINTENT_UNSPECIFIED, _RC)
      call mapl_GridCompGet(gridcomp, logger=lgr, geom=geom, name=name, _RC)

      iter = stats%items%ftn_begin()
      associate (e => stats%items%ftn_end())
        do while (iter /= e)
           call iter%next()
           stat => iter%of()
           call stat%add_to_state(state, _RC)
        end do
      end associate

      call esmf_ClockGet(clock, currTime=currTime, _RC)
      restart_handler = RestartHandler(geom, currTime, lgr)

      filename = name // '_custom_import.nc'
      call restart_handler%read(state, filename, _RC)

      call esmf_StateDestroy(state, _RC)
      _RETURN(_SUCCESS)

   end subroutine custom_read_restart

   subroutine custom_write_restart(gridcomp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridComp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(Statistics), pointer :: stats
      type(esmf_State) :: state
      type(StatisticsVectorIterator) :: iter
      type(RestartHandler) :: restart_handler
      class(AbstractTimeStatistic), pointer :: stat
      type(esmf_Time) :: currTime
      class(Logger), pointer :: lgr
      type(esmf_Geom) :: geom
      character(:), allocatable :: name, filename

      _GET_NAMED_PRIVATE_STATE(gridcomp, Statistics, PRIVATE_STATE, stats)
      state = esmf_StateCreate(stateIntent=ESMF_STATEINTENT_UNSPECIFIED, _RC)
      call mapl_GridCompGet(gridcomp, logger=lgr, geom=geom, name=name,  _RC)

      iter = stats%items%ftn_begin()
      associate (e => stats%items%ftn_end())
        do while (iter /= e)
           call iter%next()
           stat => iter%of()
           call stat%add_to_state(state, _RC)
        end do
      end associate

      call esmf_ClockGet(clock, currTime=currTime, _RC)
      restart_handler = RestartHandler(geom, currTime, lgr)

       filename = name // '_custom_import.nc'
     call restart_handler%write(state, filename, _RC)

      call esmf_StateDestroy(state, _RC)

      _RETURN(_SUCCESS)
   end subroutine custom_write_restart

end module mapl3g_StatisticsGridComp

subroutine setServices(gridComp, rc)
   use mapl3
   use mapl3g_StatisticsGridComp, only: StatisticsSetServices => setServices
   implicit none(type,external)
   type(esmf_GridComp), intent(inout) :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call StatisticsSetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices

