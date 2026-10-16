#include "MAPL.h"

! Simple accessors for the OpenMP threading parameters of a component.
! The configuration itself lives in the component spec (the `mapl: misc:`
! section of the component hconfig), while `threading_active` is transient
! state that is only true while the component is being run on multiple
! threads.

submodule (mapl_OuterMetaComponent_mod) threading_accessors_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine set_use_threads(this, use_threads)
      class(OuterMetaComponent), intent(inout) :: this
      logical, intent(in) :: use_threads

      this%component_spec%misc%use_threads = use_threads

   end subroutine set_use_threads

   module function get_use_threads(this) result(use_threads)
      logical :: use_threads
      class(OuterMetaComponent), intent(in) :: this

      use_threads = this%component_spec%misc%use_threads

   end function get_use_threads

   module subroutine set_num_threads(this, num_threads, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(in) :: num_threads
      integer, optional, intent(out) :: rc

      _ASSERT(num_threads >= 1, 'num_threads must be at least 1')
      _ASSERT(.not. this%threading_active, 'cannot change num_threads while threading is active')
      this%component_spec%misc%num_threads = num_threads

      _RETURN(_SUCCESS)
   end subroutine set_num_threads

   module function get_num_threads(this) result(num_threads)
      integer :: num_threads
      class(OuterMetaComponent), intent(in) :: this

      num_threads = this%component_spec%misc%num_threads

   end function get_num_threads

   module function is_threading_active(this) result(threading_active)
      logical :: threading_active
      class(OuterMetaComponent), intent(in) :: this

      threading_active = this%threading_active

   end function is_threading_active

end submodule threading_accessors_smod
