#include "MAPL.h"
module mapl3g_NullStatistic
   use mapl3g_AbstractTimeStatistic
   use mapl_ErrorHandling
   use esmf, only: esmf_State
   implicit none(type,external)
   private

   public :: NullStatistic

   type, extends(AbstractTimeStatistic) :: NullStatistic
      private
    contains
      procedure :: initialize => noop
      procedure :: destroy => noop
      procedure :: update => noop
      procedure :: reset => noop
      procedure :: compute_result => noop
      procedure :: add_to_state
   end type NullStatistic

contains

   subroutine noop(this, rc)
      class(NullStatistic), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _FAIL('NullStatistic does not perform any operations')

   end subroutine noop

    subroutine add_to_state(this, state, rc)
        class(NullStatistic), intent(inout) :: this
        type(esmf_State), intent(inout) :: state
        integer, optional, intent(out) :: rc
  
        _FAIL('NullStatistic does not add anything to the state.')
    end subroutine add_to_state
   
end module mapl3g_NullStatistic
