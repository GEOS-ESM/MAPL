module mapl3g_AbstractTimeStatistic
   use mapl3
   implicit none(type,external)
   private

   public :: AbstractTimeStatistic

   type, abstract :: AbstractTimeStatistic
   contains
      procedure(I_action), deferred :: initialize
      procedure(I_action), deferred :: destroy
      procedure(I_action), deferred :: update
      procedure(I_action), deferred :: reset
      procedure(I_action), deferred :: compute_result
      procedure(I_add_to_state), deferred :: add_to_state
   end type AbstractTimeStatistic

   abstract interface
      subroutine I_action(this, rc)
         import AbstractTimeStatistic
         class(AbstractTimeStatistic), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_action

      subroutine I_add_to_state(this, state, rc)
         import AbstractTimeStatistic
         import esmf_State
         class(AbstractTimeStatistic), intent(inout) :: this
         type(esmf_State), intent(inout) :: state
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_state

   end interface

end module mapl3g_AbstractTimeStatistic
