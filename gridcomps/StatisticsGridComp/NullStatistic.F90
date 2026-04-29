#include "MAPL.h"

module mapl3g_NullStatistic

   use mapl3g_AbstractTimeStatistic
   use mapl_ErrorHandling
   use esmf, only: esmf_State, esmf_GridComp, esmf_Clock
   use mapl3g_SimpleAlarm, only: SimpleAlarm

   implicit none(type,external)
   private

   public :: NullStatistic

   type, extends(AbstractTimeStatistic) :: NullStatistic
      private
    contains
      procedure :: destroy => noop
       procedure :: update => noop_update
      procedure :: reset => noop_with_gridcomp
      procedure :: compute_result => noop_with_gridcomp
      procedure :: add_to_state
      procedure :: get_alarm => noop_get_alarm
   end type NullStatistic

contains

   subroutine noop(this, rc)
      class(NullStatistic), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _FAIL('NullStatistic does not perform any operations')
      _UNUSED_DUMMY(this)
   end subroutine noop

   subroutine noop_with_gridcomp(this, gridcomp, rc)
      class(NullStatistic), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      _FAIL('NullStatistic does not perform any operations')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(gridcomp)
    end subroutine noop_with_gridcomp

    subroutine noop_update(this, gridcomp, clock, rc)
       class(NullStatistic), intent(inout) :: this
       type(esmf_GridComp), intent(inout) :: gridcomp
       type(esmf_Clock), intent(in) :: clock
       integer, optional, intent(out) :: rc

       _FAIL('NullStatistic does not perform any operations')
       _UNUSED_DUMMY(this)
       _UNUSED_DUMMY(gridcomp)
       _UNUSED_DUMMY(clock)
    end subroutine noop_update

   subroutine add_to_state(this, state, rc)
      class(NullStatistic), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      _FAIL('NullStatistic does not add anything to the state.')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)
   end subroutine add_to_state

    function noop_get_alarm(this) result(alarm)
       class(NullStatistic), intent(in) :: this
       type(SimpleAlarm) :: alarm

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(alarm)
      error stop 'NullStatistic does not have an alarm'
   end function noop_get_alarm

end module mapl3g_NullStatistic
