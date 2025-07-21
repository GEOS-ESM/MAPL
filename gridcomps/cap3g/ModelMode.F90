#include "MAPL.h"
module mapl3g_ModelMode
   use mapl3g_ApplicationMode
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: ModelMode

   type, extends(ApplicationMode) :: ModelMode
   contains
      procedure :: run
      procedure :: init_gc
      procedure :: run_gc
      procedure :: finalize_gc
   end type ModelMode

contains
xo
   subroutine run(this, config, rc)
      class(ModelMode), intent(inout) :: this
      type(ESMF_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      type(ESMF_HConfig) :: config
      type(ESMF_GridComp) :: gridcomp

      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      
      importState = ESMF_StateCreate(_RC)
      exportState = ESMF_StateCreate(_RC)
      clock = create_clock(config, _RC)

      call this%init_gc(gridcomp, importState=importState, exportState=exportState, clock=clock, _RC)
      call this%run_gc(gridcomp, importState=importState, exportState=exportState, clock=clock, _RC)
      call this%finalize_gc(gridcomp, importState=importState, exportState=exportState, clock=clock, _RC)

      call ESMF_GridCompFinalize(cap_gc, importState=importState, exportState=exportState, clock=clock, &
           userRC=user_status, _RC); _VERIFY(user_status)

      call ESMF_GridCompDestroy(cap_gc, nogarbage=.true., _RC)
       
      _RETURN(_SUCCESS)
   end subroutine run
   
   function create_clock(config, rc)
      type(ESMF_Clock) :: create_clock
      type(ESMF_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: start_time, end_time, time_step
      type(ESMF_HConfig) :: clock_config
      
      clock_config = ESMF_HConfigCreateAt(hconfig, keystring='clock', _RC)

      call set_time_interval(start_time, 'start', clock_config, _RC)
      call set_time(end_time, 'end', clock_config, _RC)
      call set_time(time_step, 'dt', clock_config, _RC)
      clock = ESMF_ClockCreate(timestep=dt, startTime=t_begin, endTime=t_end, _RC)
      
      _RETURN(_SUCCESS)
   end function create_clock

   subroutine set_time_interval(interval, key, hconfig, rc)
      type(ESMF_TimeInterval), intent(out) :: interval
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_duration
      
      iso_duration = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      call ESMF_TimeIntervalSet(interval, timeString=iso_time, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine set_time(time, key, hconfig, rc)
      type(ESMF_Time), intent(out) :: time
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_time
      
      iso_time = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      call ESMF_TimeSet(time, timeString=iso_time, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine init_gc(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc
      integer :: i

      do i = 1, size(GENERIC_INIT_PHASE_SEQUENCE)
         associate (phase => GENERIC_INIT_PHASE_SEQUENCE(i))
           call ESMF_GridCompInitialize(cap_gc, &
                importState=importState, exportState=exportState, clock=clock, &
                phase=phase, userRC=user_status, _RC)
           _VERIFY(user_status)
         end associate
      end do
   end subroutine initialize

      subroutine run_gc(gridcomp, importState, exportState, clock, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         type(ESMF_State), intent(inout) :: importState, exportState
         type(ESMF_Clock), intent(inout) :: clock
         integer, optional, intent(out) :: rc

         call ESMF_GridCompRun(gridcomp_gc, & 
              importState=importState, exportState=exportState, clock=clock, &
              userRC=user_status, _RC); _VERIFY(user_status)

         _RETURN(_SUCCESS)
      end subroutine run_gc

      subroutine finalize_gc(gridcomp, importState, exportState, clock, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         type(ESMF_State), intent(inout) :: importState, exportState
         type(ESMF_Clock), intent(inout) :: clock
         integer, optional, intent(out) :: rc

         call ESMF_GridCompRun(gridcomp_gc, & 
              importState=importState, exportState=exportState, clock=clock, &
              userRC=user_status, _RC); _VERIFY(user_status)

         _RETURN(_SUCCESS)
      end subroutine finalize_gc

 end module mapl3g_ModelMode
