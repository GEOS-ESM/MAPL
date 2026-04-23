#include "MAPL.h"

module mapl3g_NullStatistic

   use mapl3g_AbstractTimeStatistic
   use mapl_ErrorHandling
   use esmf, only: esmf_State, esmf_GridComp

   implicit none(type,external)
   private

   public :: NullStatistic

   type, extends(AbstractTimeStatistic) :: NullStatistic
      private
    contains
      procedure :: destroy => noop
      procedure :: update => noop_with_gridcomp
      procedure :: reset => noop_with_gridcomp
      procedure :: compute_result => noop_with_gridcomp
      procedure :: add_to_state
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

   subroutine add_to_state(this, state, rc)
      class(NullStatistic), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      _FAIL('NullStatistic does not add anything to the state.')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(state)
   end subroutine add_to_state

end module mapl3g_NullStatistic
