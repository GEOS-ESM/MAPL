#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_thread_context_smod
   use mapl_OpenMP_Support_mod, only: get_current_thread
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! States of the mini component associated with the calling thread.
   ! When threading is not active the states of the (single) user
   ! component are returned.
   module function get_thread_states(this, rc) result(states)
      type(MultiState) :: states
      class(OuterMetaComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: thread

      if (.not. this%threading_active) then
         states = this%user_gc_driver%get_states()
         _RETURN(_SUCCESS)
      end if

      thread = get_current_thread()
      _ASSERT(thread < size(this%subcomponents), 'thread id exceeds number of sub components')
      states = this%subcomponents(thread+1)%states

      _RETURN(_SUCCESS)
   end function get_thread_states

   ! Gridcomp of the mini component associated with the calling thread.
   ! When threading is not active the (single) user gridcomp is returned.
   module function get_thread_gridcomp(this, rc) result(gridcomp)
      type(ESMF_GridComp) :: gridcomp
      class(OuterMetaComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: thread

      if (.not. this%threading_active) then
         gridcomp = this%user_gc_driver%get_gridcomp()
         _RETURN(_SUCCESS)
      end if

      thread = get_current_thread()
      _ASSERT(thread < size(this%subcomponents), 'thread id exceeds number of sub components')
      gridcomp = this%subcomponents(thread+1)%gridcomp

      _RETURN(_SUCCESS)
   end function get_thread_gridcomp

end submodule get_thread_context_smod
